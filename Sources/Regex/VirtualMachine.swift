import Util

/// Object code for a regex program, to be interpreted by a VM
///
/// Consists of an instruction list and metadata tracking:
///   - Locations of labels (branch destinations)
///   - Locations of splits (branches)
///   - Total number of captures
///   - Various options (case-insensitive, etc)
///
public struct RECode {
  public typealias InstructionList = [Instruction]
  var instructions: InstructionList
  var labels: [InstructionAddress]
  var splits: [InstructionAddress]
  var options: Options
}

extension RECode {
  /// A RECode instruction.
  public enum Instruction: Hashable {
    /// NOP (currently unused)
    case nop

    /// Denote a sucessful match. (currently used only at the end of a program)
    case accept

    /// Consume and try to match a unit of input
    case character(Character)

    /// Consume and try to match a unit of input against a character class
    case characterClass(CharacterClass)

    case unicodeScalar(UnicodeScalar)
    
    /// Consume any unit of input
    case any

    /// Split execution. Favored path will fall through while disfavored will branch to `disfavoring`
    case split(disfavoring: LabelId)

    /// Branch to `label`
    case goto(label: LabelId)

    /// The target of a branch, executed as a NOP
    case label(LabelId)

    /// Begin a capture group
    case beginGroup

    /// Ends a capture group
    case endGroup

    /// Begins capturing a portion of the input string
    case beginCapture
    /// Ends capturing a portion of the input string.
    /// The captured portion is mapped using the provided function
    case endCapture(transform: CaptureTransform? = nil)

    /// Convert the capture stack x into [.some(x)]
    case captureSome

    /// Push "nil" onto the capture list
    case captureNil

    /// Convert the current capture stack into an array
    /// of captures and then push
    case captureArray

    var isAccept: Bool {
      switch self {
      case .accept:
        return true
      default:
        return false
      }
    }
    
    // Future instructions
    //    case ratchet
    //    case peekAhead([CharacterClass])
    //    case peekBehind([CharacterClass])
  }
}

// Conveniences
extension RECode.Instruction {
  /// Fetch the label from a label instruction, else `nil`
  var label: LabelId? {
    guard case .label(let id) = self else { return nil }
    return id
  }

  /// Whether this instruction particpcates in matching
  var isMatching: Bool {
    switch self {
    case .accept: return true
    case .character(_): return true
    case .unicodeScalar(_): return true
    case .characterClass(_): return true
    case .any: return true
    default: return false
    }
  }

  /// Whether this instruction consumes the input
  var isConsuming: Bool {
    switch self {
    case .any: return true
    case .character(_): return true
    case .unicodeScalar(_): return true
    case .characterClass(_): return true
    default: return false
    }
  }

  // Convenience constructors
  static func label(_ i: Int) -> Self { .label(LabelId(i)) }
}

public struct Options: OptionSet {
  public let rawValue: Int

  public static var none = Options(rawValue: 0)
  public static var caseInsensitive = Options(rawValue: 1 << 0)
  // Future options
  //    ratcheting
  //    ??? partial
  //    ??? newlineTerminated

  public init() {
    self = .none
  }
  public init(rawValue: Int) {
    self.rawValue = rawValue
  }
}

// RECode as a RAC of instructions. We might want to make this instead be
// `InstructionList` if that graduates from being an array.
extension RECode: RandomAccessCollection {
  public typealias Element = Instruction
  public typealias Index = InstructionAddress

  public var startIndex: Index { return Index(instructions.startIndex) }
  public var endIndex: Index { return Index(instructions.endIndex) }
  public subscript(_ i: Index) -> Element { return instructions[i.rawValue] }

  public func index(after i: Index) -> Index {
    return Index(i.rawValue + 1)
  }
  public func index(before i: Index) -> Index {
    return Index(i.rawValue - 1)
  }
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    return Index(i.rawValue + n)
  }
}

extension RECode {
  public func withMatchLevel(_ level: CharacterClass.MatchLevel) -> RECode {
    var result = self
    result.instructions = result.instructions.map { inst in
      switch inst {
      case .characterClass(var cc):
        cc.matchLevel = level
        return .characterClass(cc)
      default:
        return inst
      }
    }
    return result
  }
}

extension RECode {
  /// Lookup the location of a label
  public func lookup(_ id: LabelId) -> InstructionAddress {
    let result = labels[id.rawValue]
    guard case .label(let lid) = self[result], lid == id else {
      fatalError("malformed program: labels not hooked up correctly")
    }
    return result
  }

  /// A convenient VM thread "core" abstraction
  public struct ThreadCore {
    enum CaptureState {
      case started(String.Index)
      case ended

      var isEnded: Bool {
        guard case .ended = self else {
          return false
        }
        return true
      }

      mutating func start(at index: String.Index) {
        assert(isEnded, "Capture already started")
        self = .started(index)
      }

      mutating func end(at endIndex: String.Index) -> Range<String.Index> {
        guard case let .started(startIndex) = self else {
          fatalError("Capture already ended")
        }
        self = .ended
        return startIndex..<endIndex
      }
    }

    public var pc: InstructionAddress
    public var captureScopes = Stack<[Capture]>()
    public var captures: [Capture] = []
    let input: String
    var captureState: CaptureState = .ended

    public init(startingAt pc: InstructionAddress, input: String) {
      self.pc = pc
      self.input = input
    }

    public mutating func advance() { self.pc = self.pc + 1 }
    public mutating func go(to: InstructionAddress) { self.pc = to }

    public mutating func beginCapture(_ index: String.Index) {
      captureState.start(at: index)
    }

    public mutating func endCapture(_ endIndex: String.Index, transform: CaptureTransform?) {
      let range = captureState.end(at: endIndex)
      let substring = input[range]
      let value = transform?(substring) ?? substring
      captures.append(.atom(value))
    }

    public mutating func beginGroup() {
      captureScopes.push(captures)
      captures = []
    }

    public mutating func endGroup() {
      assert(!captureScopes.isEmpty)
      var top = captureScopes.pop()
      if !captures.isEmpty {
        top.append(singleCapture())
      }
      captures = top
    }

    public mutating func captureNil() {
      captures = [.optional(nil)]
    }

    public mutating func captureSome() {
      captures = [.optional(.tupleOrAtom(captures))]
    }

    public mutating func captureArray() {
      captures = [.array(captures)]
    }

    public func singleCapture() -> Capture {
      .tupleOrAtom(captures)
    }
  }
}

public struct Stack<T> {
  public var stack: Array<T>

  public init() { self.stack = [] }
  public var isEmpty: Bool { stack.isEmpty }

  public mutating func pop() -> T {
    guard !isEmpty else { fatalError("stack is empty") }
    return stack.popLast()!
  }
  public mutating func push(_ t: T) {
    stack.append(t)
  }
  public func peek() -> T {
    guard !isEmpty else { fatalError("stack is empty") }
    return stack.last!
  }
  public var top: T {
    get { peek() }
    _modify {
      guard !isEmpty else { fatalError("stack is empty") }
      yield &stack[stack.endIndex - 1]
    }
  }
}

/// VMs load RECode and run over Strings.
public protocol VirtualMachine {
  /// Declare this VM's motto and general life philosophy
  static var motto: String { get }

  /// Load some RECode and prepare to match
  init(_: RECode)

  /// Match `input`
  func execute(input: String) -> Capture?
}

extension RECode.Instruction: CustomStringConvertible {
  public var description: String {
    switch self {
    case .nop: return "<NOP>"
    case .accept: return "<ACC>"
    case .any: return "<ANY>"
    case .characterClass(let kind): return "<CHAR CLASS \(kind)>"
    case .character(let c): return c.halfWidthCornerQuoted
    case .unicodeScalar(let u): return u.halfWidthCornerQuoted
    case .split(let i): return "<SPLIT disfavoring \(i)>"
    case .goto(let label): return "<GOTO \(label)>"
    case .label(let i): return "<\(i)>"
    case .beginGroup: return "<BEGIN GROUP>"
    case .endGroup: return "<END GROUP>"
    case .beginCapture: return "<BEGIN CAP>"
    case .endCapture: return "<END CAP>"
    case .captureSome: return "<CAP SOME>"
    case .captureNil: return "<CAP NIL>"
    case .captureArray: return "<CAP ARRAY>"
    }
  }
}

