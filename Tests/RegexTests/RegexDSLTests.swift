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

import XCTest
@testable import _StringProcessing

class RegexDSLTests: XCTestCase {
  func testSimpleStrings() throws {
    let regex = Regex {
      "a"
      capture(Character("b")) // Character
      tryCapture("1") { Int($0) } // Int
    }
    // Assert the inferred capture type.
    let _: (Substring, Substring, Int).Type = type(of: regex).Match.self
    let maybeMatch = "ab1".match(regex)
    let match = try XCTUnwrap(maybeMatch)
    XCTAssertTrue(match.match == ("ab1", "b", 1))

    let substring = "ab1"[...]
    let substringMatch = try XCTUnwrap(substring.match(regex))
    XCTAssertTrue(match.match == substringMatch.match)
  }

  func testCharacterClasses() throws {
    let regex = Regex {
      CharacterClass.any
      capture(CharacterClass.whitespace) // Character
      capture("c") // Substring
    }
    // Assert the inferred capture type.
    let _: (Substring, Substring, Substring).Type = type(of: regex).Match.self
    let maybeMatch = "a c".match(regex)
    let match = try XCTUnwrap(maybeMatch)
    XCTAssertTrue(match.match == ("a c", " ", "c"))
  }

  func testAlternation() throws {
    do {
      let regex = oneOf {
        "aaa"
      }
      XCTAssertTrue("aaa".match(regex)?.match == "aaa")
      XCTAssertNil("aab".match(regex)?.match)
    }
    do {
      let regex = oneOf {
        "aaa"
        "bbb"
        "ccc"
      }
      XCTAssertTrue("aaa".match(regex)?.match == "aaa")
      XCTAssertNil("aab".match(regex)?.match)
      XCTAssertTrue("bbb".match(regex)?.match == "bbb")
      XCTAssertTrue("ccc".match(regex)?.match == "ccc")
    }
    do {
      let regex = Regex {
        "ab"
        capture {
          oneOf {
            "c"
            "def"
          }
        }.+
      }
      XCTAssertTrue(
        try XCTUnwrap("abc".match(regex)?.match) == ("abc", ["c"]))
    }
    do {
      let regex = oneOf {
        "aaa"
        "bbb"
        "ccc"
      }
      XCTAssertTrue("aaa".match(regex)?.match == "aaa")
      XCTAssertNil("aab".match(regex)?.match)
      XCTAssertTrue("bbb".match(regex)?.match == "bbb")
      XCTAssertTrue("ccc".match(regex)?.match == "ccc")
    }
    do {
      let regex = oneOf {
        capture("aaa")
      }
      XCTAssertTrue(
        try XCTUnwrap("aaa".match(regex)?.match) == ("aaa", "aaa"))
      XCTAssertNil("aab".match(regex)?.match)
    }
    do {
      let regex = oneOf {
        capture("aaa")
        capture("bbb")
        capture("ccc")
      }
      XCTAssertTrue(
        try XCTUnwrap("aaa".match(regex)?.match) == ("aaa", "aaa", nil, nil))
      XCTAssertTrue(
        try XCTUnwrap("bbb".match(regex)?.match) == ("bbb", nil, "bbb", nil))
      XCTAssertTrue(
        try XCTUnwrap("ccc".match(regex)?.match) == ("ccc", nil, nil, "ccc"))
      XCTAssertNil("aab".match(regex)?.match)
    }
  }

  func testCombinators() throws {
    let regex = Regex {
      "a".+
      capture(oneOrMore(Character("b"))) // Substring
      capture(many("c")) // Substring
      capture(CharacterClass.hexDigit).* // [Substring]
      "e".?
      capture("t" | "k") // Substring
      oneOf { capture("k"); capture("j") } // (Substring?, Substring?)
    }
    // Assert the inferred capture type.
    let _: (Substring, Substring, Substring, [Substring], Substring, Substring?, Substring?).Type
      = type(of: regex).Match.self
    let maybeMatch = "aaaabccccdddkj".match(regex)
    let match = try XCTUnwrap(maybeMatch).match
    XCTAssertEqual(match.0, "aaaabccccdddkj")
    XCTAssertEqual(match.1, "b")
    XCTAssertEqual(match.2, "cccc")
    XCTAssertEqual(match.3, ["d", "d", "d"])
    XCTAssertEqual(match.4, "k")
    XCTAssertEqual(match.5, .none)
    XCTAssertEqual(match.6, .some("j"))
  }

  func testNestedGroups() throws {
    let regex = Regex {
      "a".+
      oneOrMore {
        capture(oneOrMore("b"))
        capture(many("c"))
        capture("d").*
        "e".?
      }
    }
    // Assert the inferred capture type.
    let _: (Substring, [(Substring, Substring, [Substring])]).Type
      = type(of: regex).Match.self
    let maybeMatch = "aaaabccccddd".match(regex)
    let match = try XCTUnwrap(maybeMatch)
    XCTAssertEqual(match.match.1.count, 1)
    XCTAssertEqual(match.match.0, "aaaabccccddd")
    XCTAssertTrue(
      match.match.1[0]
        == ("b", "cccc", ["d", "d", "d"]))
  }

  func testCapturelessQuantification() throws {
    // This test is to make sure that a captureless quantification, when used
    // straight out of the quantifier (without being wrapped in a builder), is
    // able to produce a regex whose `Match` type does not contain any sort of
    // void.
    let regex = many(.digit)
    // Assert the inferred capture type.
    let _: Substring.Type = type(of: regex).Match.self
    let input = "123123"
    let match = try XCTUnwrap(input.match(regex)?.match)
    XCTAssertTrue(match == input)
  }

  func testQuantificationWithTransformedCapture() throws {
    // This test is to make sure transformed capture type information is
    // correctly propagated from the DSL into the bytecode and that the engine
    // is reconstructing the right types upon quantification (both empty and
    // non-empty).
    enum Word: Int32 {
      case apple
      case orange

      init?(_ string: Substring) {
        switch string {
        case "apple": self = .apple
        case "orange": self = .orange
        default: return nil
        }
      }
    }
    let regex = Regex {
      "a".+
      oneOrMore(.whitespace)
      optionally {
        capture(oneOrMore(.digit)) { Int($0)! }
      }
      many {
        oneOrMore(.whitespace)
        capture(oneOrMore(.word)) { Word($0)! }
      }
    }
    // Assert the inferred capture type.
    let _: (Substring, Int?, [Word]).Type = type(of: regex).Match.self
    do {
      let input = "aaa 123 apple orange apple"
      let match = input.match(regex)?.match
      let (whole, number, words) = try XCTUnwrap(match)
      XCTAssertTrue(whole == input)
      XCTAssertEqual(number, 123)
      XCTAssertEqual(words, [.apple, .orange, .apple])
    }
    do {
      let input = "aaa   "
      let match = input.match(regex)?.match
      let (whole, number, words) = try XCTUnwrap(match)
      XCTAssertTrue(whole == input)
      XCTAssertEqual(number, nil)
      XCTAssertTrue(words.isEmpty)
    }
  }

  func testNestedCaptureTypes() throws {
    let regex1 = Regex {
      "a".+
      capture {
        capture(oneOrMore("b"))
        "e".?
      }
    }
    let _: (Substring, Substring, Substring).Type
      = type(of: regex1).Match.self
    let regex2 = Regex {
      "a".+
      capture {
        tryCapture("b") { Int($0) }.*
        "e".?
      }
    }
    let _: (Substring, Substring, [Int]).Type
      = type(of: regex2).Match.self
    let regex3 = Regex {
      "a".+
      capture {
        tryCapture("b") { Int($0) }
        many {
          tryCapture("c") { Double($0) }
        }
        "e".?
      }
    }
    let _: (Substring, Substring, Int, [Double]).Type
      = type(of: regex3).Match.self
    let regex4 = Regex {
      "a".+
      capture {
        oneOrMore {
          capture(oneOrMore("b"))
          capture(many("c"))
          capture("d").*
          "e".?
        }
      }
    }
    let _: (
      Substring, Substring, [(Substring, Substring, [Substring])]).Type
      = type(of: regex4).Match.self
  }

  func testUnicodeScalarPostProcessing() throws {
    let spaces = Regex {
      many {
        CharacterClass.whitespace
      }
    }

    let unicodeScalar = Regex {
      oneOrMore {
        CharacterClass.hexDigit
      }
      spaces
    }

    let unicodeData = Regex {
      unicodeScalar
      optionally {
        ".."
        unicodeScalar
      }

      ";"
      spaces

      capture {
        oneOrMore {
          CharacterClass.word
        }
      }

      many {
        CharacterClass.any
      }
    }

    // Assert the inferred capture type.
    let _: (Substring, Substring).Type = type(of: unicodeData).Match.self

    let unicodeLine =
      "1BCA0..1BCA3  ; Control # Cf   [4] SHORTHAND FORMAT LETTER OVERLAP..SHORTHAND FORMAT UP STEP"
    let match = try XCTUnwrap(unicodeLine.match(unicodeData))
    XCTAssertEqual(match.0, Substring(unicodeLine))
    XCTAssertEqual(match.1, "Control")
  }

  func testGraphemeBreakData() throws {
    let line = """
      A6F0..A6F1    ; Extend # Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM COMBINING MARK TUKWENTIS
      """
    
    let regexWithCapture = Regex {
      capture {
        oneOrMore(CharacterClass.hexDigit)
      } transform: { Unicode.Scalar(hex: $0) }
      optionally {
        ".."
        capture {
          oneOrMore(CharacterClass.hexDigit)
        } transform: { Unicode.Scalar(hex: $0) }
      }
      oneOrMore(CharacterClass.whitespace)
      ";"
      oneOrMore(CharacterClass.whitespace)
      capture(oneOrMore(CharacterClass.word))
      many(CharacterClass.any)
    } // Regex<(Substring, Unicode.Scalar?, Unicode.Scalar??, Substring)>
    do {
      // Assert the inferred capture type.
      typealias ExpectedMatch = (
        Substring, Unicode.Scalar?, Unicode.Scalar??, Substring
      )
      let _: ExpectedMatch.Type = type(of: regexWithCapture).Match.self
      let maybeMatchResult = line.match(regexWithCapture)
      let matchResult = try XCTUnwrap(maybeMatchResult)
      let (wholeMatch, lower, upper, propertyString) = matchResult.match
      XCTAssertEqual(wholeMatch, Substring(line))
      XCTAssertEqual(lower, Unicode.Scalar(0xA6F0))
      XCTAssertEqual(upper, Unicode.Scalar(0xA6F1))
      XCTAssertEqual(propertyString, "Extend")
    }

    let regexWithTryCapture = Regex {
      tryCapture {
        oneOrMore(CharacterClass.hexDigit)
      } transform: {
        Unicode.Scalar(hex: $0)
      }
      optionally {
        ".."
        tryCapture {
          oneOrMore(CharacterClass.hexDigit)
        } transform: {
          Unicode.Scalar(hex: $0)
        }
      }
      oneOrMore(CharacterClass.whitespace)
      ";"
      oneOrMore(CharacterClass.whitespace)
      capture(oneOrMore(CharacterClass.word))
      many(CharacterClass.any)
    } // Regex<(Substring, Unicode.Scalar, Unicode.Scalar?, Substring)>
    do {
      // Assert the inferred capture type.
      typealias ExpectedMatch = (
        Substring, Unicode.Scalar, Unicode.Scalar?, Substring
      )
      let _: ExpectedMatch.Type = type(of: regexWithTryCapture).Match.self
      let maybeMatchResult = line.match(regexWithTryCapture)
      let matchResult = try XCTUnwrap(maybeMatchResult)
      let (wholeMatch, lower, upper, propertyString) = matchResult.match
      XCTAssertEqual(wholeMatch, Substring(line))
      XCTAssertEqual(lower, Unicode.Scalar(0xA6F0))
      XCTAssertEqual(upper, Unicode.Scalar(0xA6F1))
      XCTAssertEqual(propertyString, "Extend")
    }

    do {
      let regexLiteral = try MockRegexLiteral(
        #"([0-9A-F]+)(?:\.\.([0-9A-F]+))?\s+;\s+(\w+).*"#,
        matching: (Substring, Substring, Substring?, Substring).self)
      let maybeMatchResult = line.match(regexLiteral)
      let matchResult = try XCTUnwrap(maybeMatchResult)
      let (wholeMatch, lower, upper, propertyString) = matchResult.match
      XCTAssertEqual(wholeMatch, Substring(line))
      XCTAssertEqual(lower, "A6F0")
      XCTAssertEqual(upper, "A6F1")
      XCTAssertEqual(propertyString, "Extend")
    }
  }

  func testDynamicCaptures() throws {
    do {
      let regex = try Regex("aabcc.")
      let line = "aabccd"
      let captures = try XCTUnwrap(line.match(regex)?.1)
      XCTAssertEqual(captures, .empty)
    }
    do {
      let regex = try Regex(
        #"([0-9A-F]+)(?:\.\.([0-9A-F]+))?\s+;\s+(\w+).*"#)
      let line = """
        A6F0..A6F1    ; Extend # Mn   [2] BAMUM COMBINING MARK KOQNDON..BAMUM \
        COMBINING MARK TUKWENTIS
        """
      let captures = try XCTUnwrap(line.match(regex)?.1)
      XCTAssertEqual(
        captures,
        .tuple([
          .substring("A6F0"),
          .optional(.substring("A6F1")),
          .substring("Extend")]))
    }
  }
}

extension Unicode.Scalar {
  // Convert a hexadecimal string to a scalar
  init?<S: StringProtocol>(hex: S) {
    guard let val = UInt32(hex, radix: 16), let scalar = Self(val) else {
      return nil
    }
    self = scalar
  }
}
