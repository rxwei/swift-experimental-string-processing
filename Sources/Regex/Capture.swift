import Util

public enum Capture {
  case atom(Any)
  indirect case tuple([Capture])
  indirect case optional(Capture?)
  indirect case array([Capture])
}

extension Capture {
  static func tupleOrAtom(_ elements: [Capture]) -> Self {
    elements.count == 1 ? elements[0] : .tuple(elements)
  }

  public var value: Any {
    switch self {
    case .atom(let atom):
      return atom
    case .tuple(let elements):
      return Util.tuple(of: elements.map(\.value))
    case .array(let elements):
      guard let first = elements.first else {
        return [Any]()
      }
      // When the array is not empty, infer the concrete `Element `type from the first element.
      func helper<T>(_ first: T) -> Any {
        var castElements = [first]
        for element in elements.dropFirst() {
          castElements.append(element.value as! T)
        }
        return castElements
      }
      return _openExistential(first.value, do: helper)
    case .optional(let subcapture):
      return subcapture?.value as Any
    }
  }
}

extension AST {
  var hasCaptures: Bool {
    switch self {
    case let .alternation(child), let .concatenation(child):
      return child.any(\.hasCaptures)
    case .capturingGroup, .group:
      return true
    case let .many(child),
         let .zeroOrOne(child),
         let .oneOrMore(child):
      return child.hasCaptures
    case .character, .unicodeScalar, .characterClass, .any, .empty:
      return false
    }
  }
}
