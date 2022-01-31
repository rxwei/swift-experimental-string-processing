//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021-2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import _MatchingEngine

// MARK: - Primitives

extension String: RegexProtocol {
  public typealias Match = Substring

  public var regex: Regex<Match> {
    let atoms = self.map { atom(.char($0)) }
    return .init(ast: concat(atoms))
  }
}

extension Character: RegexProtocol {
  public typealias Match = Substring

  public var regex: Regex<Match> {
    .init(ast: atom(.char(self)))
  }
}

extension CharacterClass: RegexProtocol {
  public typealias Match = Substring

  public var regex: Regex<Match> {
    guard let ast = self.makeAST() else {
      fatalError("FIXME: extended AST?")
    }
    return Regex(ast: ast)
  }
}

// MARK: - Combinators

// MARK: Concatenation

// Note: Concatenation overloads are currently gyb'd.

// TODO: Variadic generics
// struct Concatenation<W0, C0..., R0: RegexProtocol, W1, C1..., R1: RegexProtocol>
// where R0.Match == (W0, C0...), R1.Match == (W1, C1...)
// {
//   typealias Match = (Substring, C0..., C1...)
//   let regex: Regex<Match>
//   init(_ first: R0, _ second: R1) {
//     regex = .init(concat(r0, r1))
//   }
// }

// MARK: Quantification

// Note: Quantifiers are currently gyb'd.

// TODO: Variadic generics
// struct _OneOrMore<W, C..., Component: RegexProtocol>
// where R.Match == (W, C...)
// {
//   typealias Match = (Substring, [(C...)])
//   let regex: Regex<Match>
//   init(_ component: Component) {
//     regex = .init(oneOrMore(r0))
//   }
// }
//
// struct _OneOrMoreNonCapturing<Component: RegexProtocol> {
//   typealias Match = Substring
//   let regex: Regex<Match>
//   init(_ component: Component) {
//     regex = .init(oneOrMore(r0))
//   }
// }
//
// func oneOrMore<W, C..., Component: RegexProtocol>(
//   _ component: Component
// ) -> <R: RegexProtocol where R.Match == (Substring, [(C...)])> R {
//   _OneOrMore(component)
// }
//
// @_disfavoredOverload
// func oneOrMore<Component: RegexProtocol>(
//   _ component: Component
// ) -> <R: RegexProtocol where R.Match == Substring> R {
//   _OneOrMoreNonCapturing(component)
// }

postfix operator .?
postfix operator .*
postfix operator .+

// Overloads for quantifying over a character class.
public func zeroOrOne(_ cc: CharacterClass) -> _ZeroOrOne_0<CharacterClass> {
  .init(component: cc)
}

public func many(_ cc: CharacterClass) -> _ZeroOrMore_0<CharacterClass> {
  .init(component: cc)
}

public func oneOrMore(_ cc: CharacterClass) -> _OneOrMore_0<CharacterClass> {
  .init(component: cc)
}

// MARK: Alternation

// TODO: Support heterogeneous capture alternation.
public struct Alternation<
  Component1: RegexProtocol, Component2: RegexProtocol
>: RegexProtocol {
  public typealias Match = Component1.Match

  public let regex: Regex<Match>

  public init(_ first: Component1, _ second: Component2) {
    regex = .init(ast: alt(
      first.regex.ast.root, second.regex.ast.root
    ))
  }

  public init(
    @RegexBuilder _ content: () -> Alternation<Component1, Component2>
  ) {
    self = content()
  }
}

public func | <Component1, Component2>(
  lhs: Component1, rhs: Component2
) -> Alternation<Component1, Component2> {
  .init(lhs, rhs)
}

// MARK: - Capture

public struct CapturingGroup<Match>: RegexProtocol {
  public let regex: Regex<Match>

  init<Component: RegexProtocol>(
    _ component: Component
  ) {
    self.regex = .init(ast:
      group(.capture, component.regex.ast.root)
    )
  }

  init<Component: RegexProtocol>(
    _ component: Component,
    transform: CaptureTransform
  ) {
    self.regex = .init(
      ast: .groupTransform(
        .init(.init(faking: .capture), component.regex.ast.root, .fake),
        transform: transform))
  }

  init<NewCapture, Component: RegexProtocol>(
    _ component: Component,
    transform: @escaping (Substring) -> NewCapture
  ) {
    self.init(
      component,
      transform: CaptureTransform(resultType: NewCapture.self) {
        transform($0) as Any
      })
  }

  init<NewCapture, Component: RegexProtocol>(
    _ component: Component,
    transform: @escaping (Substring) throws -> NewCapture
  ) {
    self.init(
      component,
      transform: CaptureTransform(resultType: NewCapture.self) {
        try transform($0) as Any
      })
  }

  init<NewCapture, Component: RegexProtocol>(
    _ component: Component,
    transform: @escaping (Substring) -> NewCapture?
  ) {
    self.init(
      component,
      transform: CaptureTransform(resultType: NewCapture.self) {
        transform($0) as Any?
      })
  }
}
