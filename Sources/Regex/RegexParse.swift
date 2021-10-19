private let regexLanguageDescription = """
Brief:
    Just a simple, vanilla regular expression languague.
    Supports *, +, ?, |, and non-capturing grouping
    TBD: character classes, ...
"""

extension String: Error {}

/// A parser reads off a lexer and produces an AST
///
/// Syntactic structure of a regular expression:
///
///     RE -> '' | Alternation
///     Alternation -> Concatenation ('|' Concatenation)*
///     Concatenation -> Quantification Quantification*
///     Quantification -> (Group | Atom) <token: qualifier>?
///     Atom -> <token: .character> | <any> | ... character classes ...
///     Group -> '(' Capture? RE ')'
///     Capture -> '?'
///
public enum AST: Hashable {
  indirect case alternation([AST]) // alternation(AST, AST?)
  indirect case concatenation([AST])
  indirect case group(AST)
  indirect case capturingGroup(AST, transform: CaptureTransform? = nil)

  // Post-fix modifiers
  indirect case many(AST)
  indirect case zeroOrOne(AST)
  indirect case oneOrMore(AST)

  case character(Character)
  case unicodeScalar(UnicodeScalar)
  case characterClass(CharacterClass)
  case any
  case empty
}

public struct CaptureTransform: Equatable, Hashable, CustomStringConvertible {
  public let closure: (Substring) -> Any

  public init(_ closure: @escaping (Substring) -> Any) {
    self.closure = closure
  }

  public func callAsFunction(_ input: Substring) -> Any {
    closure(input)
  }

  public static func == (lhs: CaptureTransform, rhs: CaptureTransform) -> Bool {
    unsafeBitCast(lhs.closure, to: (Int, Int).self) ==
      unsafeBitCast(rhs.closure, to: (Int, Int).self)
  }

  public func hash(into hasher: inout Hasher) {
    let (fn, ctx) = unsafeBitCast(closure, to: (Int, Int).self)
    hasher.combine(fn)
    hasher.combine(ctx)
  }

  public var description: String {
    "<transform>"
  }
}

extension AST: CustomStringConvertible {
  public var description: String {
    switch self {
    case .alternation(let rest): return ".alt(\(rest))"
    case .concatenation(let rest): return ".concat(\(rest))"
    case .group(let rest): return ".group(\(rest))"
    case .capturingGroup(let rest, let transform):
      return ".capturingGroup(\(rest), \(String(describing: transform))"
    case .many(let rest): return ".many(\(rest))"
    case .zeroOrOne(let rest): return ".zeroOrOne(\(rest))"
    case .oneOrMore(let rest): return ".oneOrMore(\(rest))"
    case .character(let c): return c.halfWidthCornerQuoted
    case .unicodeScalar(let u): return u.halfWidthCornerQuoted
    case .characterClass(let cc): return ".characterClass(\(cc))"
    case .any: return ".any"
    case .empty: return "".halfWidthCornerQuoted
    }
  }
}

fileprivate struct Parser {
  var lexer: Lexer
  init(_ lexer: Lexer) {
    self.lexer = lexer
  }
}

// Diagnostics
extension Parser {
  mutating func report(
    _ str: String, _ function: String = #function, _ line: Int = #line
  ) throws -> Never {
    throw """
        ERROR: \(str)
        (error in user string evaluating \(
            String(describing: lexer.peek())) prior to: "\(lexer.source)")
        (error detected in parser at \(function):\(line))
        """
  }
}

extension Parser {
  //     RE -> '' | Alternation
  mutating func parse() throws -> AST {
    if lexer.isEmpty { return .empty }
    return try parseAlternation()
  }
  
  //     Alternation -> Concatenation ('|' Concatenation)*
  mutating func parseAlternation() throws -> AST {
    assert(!lexer.isEmpty)
    var result = Array<AST>(try parseConcatenation())
    while lexer.eat(.pipe) {
      result.append(try parseConcatenation())
    }
    return result.count == 1 ? result[0] : .alternation(result)
  }
  
  //     Concatenation -> Quantification Quantification*
  mutating func parseConcatenation() throws -> AST {
    var result = Array<AST>()
    while let quant = try parseQuantification() {
      result.append(quant)
    }
    guard !result.isEmpty else {
      // Happens in `abc|`
      try report("empty concatenation")
    }
    return result.count == 1 ? result[0] : .concatenation(result)
  }
  
  //     Quantification -> (Group | <token: Character>) <token: Quantifier>?
  mutating func parseQuantification() throws -> AST? {
    let partialResult: AST
    switch lexer.peek() {
    case .leftParen?:
      lexer.eat()
      let isCapturing: Bool
      if lexer.eat(.question) {
        isCapturing = true
      } else {
        isCapturing = false
      }
      let child = try parse()
      partialResult = isCapturing ? .capturingGroup(child) : .group(child)
      try lexer.eat(expecting: .rightParen)

    case .character(let c, isEscaped: false):
      lexer.eat()
      partialResult = .character(c)
      
    case .unicodeScalar(let u):
      lexer.eat()
      partialResult = .unicodeScalar(u)
      
    case .character(let c, isEscaped: true):
      lexer.eat()
      if let cc = CharacterClass(c) {
        // Other characters either match a character class...
        partialResult = .characterClass(cc)

      } else {
        // ...or are invalid
        try report("unexpected escape sequence \\\(c)")
      }

    case .dot?:
      lexer.eat()
      partialResult = .characterClass(.any)

    // Correct terminations
    case .rightParen?: fallthrough
    case .pipe?: fallthrough
    case nil:
      return nil
      
    default:
      try report("expected a character or group")
    }

    switch lexer.peek() {
    case .star?:
      lexer.eat()
      return .many(partialResult)
    case .plus?:
      lexer.eat()
      return .oneOrMore(partialResult)
    case .question?:
      lexer.eat()
      return .zeroOrOne(partialResult)
    default:
      return partialResult
    }
  }
}

public func parse(_ regex: String) throws -> AST {
  let lexer = Lexer(Source(regex))
  var parser = Parser(lexer)
  return try parser.parse()
}
