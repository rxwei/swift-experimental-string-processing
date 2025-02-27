<!--
Hello, we want to issue an update to [Regular Expression Literals](https://forums.swift.org/t/pitch-regular-expression-literals/52820) and prepare for a formal proposal. The great delimiter deliberation continues to unfold, so in the meantime, we have a significant amount of surface area to present for review/feedback: the syntax _inside_ a regex literal. Additionally, this is the syntax accepted from a string used for run-time regex construction, so we're devoting an entire pitch/proposal to the topic of _regex syntax_, distinct from the result builder DSL or the choice of delimiters for literals. 
-->

# Regex Syntax

- Authors: Hamish Knight, Michael Ilseman

## Introduction

A regex declares a string processing algorithm using syntax familiar across a variety of languages and tools throughout programming history. Regexes can be created from a string at run time or from a literal at compile time. The contents of that run-time string, or the contents in-between the compile-time literal's delimiters, uses regex syntax. We present a detailed and comprehensive treatment of regex syntax.

This is part of a larger effort in supporting regex literals, which in turn is part of a larger effort towards better string processing using regex. See [Pitch and Proposal Status](https://github.com/apple/swift-experimental-string-processing/issues/107), which tracks each relevant piece. This proposal regards _syntactic_ support, and does not necessarily mean that everything that can be written will be supported by Swift's runtime engine in the initial release. Support for more obscure features may appear over time, see [MatchingEngine Capabilities and Roadmap](https://github.com/apple/swift-experimental-string-processing/issues/99) for status.


## Motivation

Swift aims to be a pragmatic programming language, striking a balance between familiarity, interoperability, and advancing the art. Swift's `String` presents a uniquely Unicode-forward model of string, but currently suffers from limited processing facilities.

The full string processing effort includes a regex type with strongly typed captures, the ability to create a regex from a string at runtime, a compile-time literal, a result builder DSL, protocols for intermixing 3rd party industrial-strength parsers with regex declarations, and a slew of regex-powered algorithms over strings.

This proposal specifically hones in on the _familiarity_ aspect by providing a best-in-class treatment of familiar regex syntax.

## Proposed Solution

We propose accepting a syntactic "superset" of the following existing regular expression engines:

- [PCRE 2][pcre2-syntax], an "industry standard" and a rough superset of Perl, Python, etc.
- [Oniguruma][oniguruma-syntax], a modern engine with additional features.
- [ICU][icu-syntax], used by NSRegularExpression, a Unicode-focused engine.
- [.NET][.net-syntax], which adds delimiter-balancing and some interesting minor details around conditional patterns.

To our knowledge, all other popular regex engines support a subset of the above syntaxes.

We also support [UTS#18][uts18]'s full set of character class operators (to our knowledge no other engine does). Beyond that, UTS#18 deals with semantics rather than syntax, and what syntax it uses is covered by the above list. We also parse Java's properties (e.g. `\p{javaLowerCase}`), meaning we support a superset of Java 8 as well.

Note that there are minor syntactic incompatibilities and ambiguities involved in this approach. Each is addressed in the relevant sections below.

Regex syntax will be part of Swift's source-compatibility story as well as its binary-compatibility story. Thus, we present a detailed and comprehensive design.

## Detailed Design

We propose the following syntax for regex.

<details><summary>Grammar Notation</summary>

For the grammar sections, we use a modified PEG-like notation, in which the grammar also describes an unambiguous top-down parsing algorithm.

- `<Element> -> <Definition>` gives the definition of `Element`
- The `|` operator specifies a choice of alternatives
- `'x'` is the literal character `x`, otherwise it's a reference to x
      + A literal `'` is spelled `"'"`
- Postfix `*` `+` and `?` denote zero-or-more, one-or-more, and zero-or-one
- Range quantifiers, like `{1...4}`, use Swift range syntax as convention.
- Basic custom character classes are written like `[0-9a-zA-Z]`
- Prefix `!` operator means the next element must not appear (a zero-width assertion)
- Parenthesis group for the purposes of quantification
- Builtins use angle brackets:
    - `<Int>` refers to an integer, `<Char>` a character, etc.
    - `<Space>` is any whitespace character
    - `<EOL>` is the end-of-line anchor (e.g. `$` in regex).

For example, `(!'|' !')' ConcatComponent)*` means any number (zero or more) occurrences of `ConcatComponent` so long as the initial character is neither a literal `|` nor a literal `)`.

</details>

### Top-level regular expression

```
Regex         -> GlobalMatchingOptionSequence? RegexNode
RegexNode     -> '' | Alternation
Alternation   -> Concatenation ('|' Concatenation)*
Concatenation -> (!'|' !')' ConcatComponent)*
```

A regex may be prefixed with a sequence of [global matching options](#pcre-global-matching-options). Its contents can be empty or a sequence of alternatives separated by `|`.

Alternatives are a series of expressions concatenated together. The concatentation ends with either a `|` denoting the end of the alternative or a `)` denoting the end of a recursively parsed group.

Alternation has a lower precedence than concatenation or other operations, so e.g `abc|def` matches against `abc` or `def`.

### Concatenated subexpressions

```
ConcatComponent -> Trivia | Quote | Quantification

Trivia  -> Comment | NonSemanticWhitespace
Comment -> '(?#' (!')')* ')' | EndOfLineComment

(extended syntax only) EndOfLineComment      -> '#' (!<EOL> .)* <EOL>
(extended syntax only) NonSemanticWhitespace -> <Space>+

Quote -> '\Q' (!'\E' .)* '\E'

```

Each component of a concatenation may be "trivia" (comments and non-semantic whitespace, if applicable), a quoted run of literal content, or a potentially-quantified subexpression.

In-line comments, similarly to C, are lexical and are not recursively nested like normal groups are. A closing `)` cannot be escaped. Quotes are similarly lexical, non-nested, and the `\` before a `\E` cannot be escaped.

For example, `\Q^[xy]+$\E`, is treated as the literal characters `^[xy]+$` rather than an anchored quantified character class. `\Q\\E` is a literal `\`.

### Quantified subexpressions

```
Quantification -> QuantOperand Quantifier?
Quantifier     -> QuantAmount QuantKind?
QuantAmount    -> '?' | '*' | '+' | '{' Range '}'
QuantKind      -> '?' | '+'
Range          -> ',' <Int> | <Int> ',' <Int>? | <Int>

QuantOperand -> AbsentFunction | Atom | Conditional | CustomCharClass | Group
```

Subexpressions can be quantified, meaning they will be repeated some number of times:

- `?`: 0 or 1 times.
- `*`: 0 or more times.
- `+`: 1 or more times.
- `{n,m}`: Between `n` and `m` (inclusive) times.
- `{n,}`: `n` or more times.
- `{,m}`: Up to `m` times.
- `{n}`: Exactly `n` times.

Behavior can further be refined by a subsequent `?` or `+`:

- `x*` _eager_: consume as much of input as possible.
- `x*?` _reluctant_: consume as little of the input as possible.
- `x*+`: _possessive_: eager and never relinquishes any input consumed.

### Atoms

```
Atom -> Anchor
      | Backreference
      | BacktrackingDirective
      | BuiltinCharacterClass
      | Callout
      | CharacterProperty
      | EscapeSequence
      | NamedScalar
      | Subpattern
      | UnicodeScalar
      | '\K'
      | '\'? <Character>
```

Atoms are the smallest units of regex syntax. They include escape sequences, metacharacters, backreferences, etc. The most basic form of atom is a literal character. A metacharacter may be treated as literal by preceding it with a backslash. Other literal characters may also be preceded with a backslash, but it has no effect if they are unknown escape sequences, e.g `\I` is literal `I`.

#### Anchors

```
Anchor -> '^' | '$' | '\A' | '\b' | '\B' | '\G' | '\y' | '\Y' | '\z' | '\Z'
```

Anchors match against a certain position in the input rather than on a particular character of the input.

- `^`: Matches at the very start of the input string, or the start of a line when in multi-line mode.
- `$`: Matches at the very end of the input string, or the end of a line when in multi-line mode.
- `\A`: Matches at the very start of the input string.
- `\Z`: Matches at the very end of the input string, in addition to before a newline at the very end of the input string.
- `\z`: Like `\Z`, but only matches at the very end of the input string.
- `\G`: Like `\A`, but also matches against the start position of where matching resumes in global matching mode (e.g `\Gab` matches twice in `abab`, `\Aab` would only match once).
- `\b` matches a boundary between a word character and a non-word character. The definitions of which vary depending on matching engine.
- `\B` matches a non-word-boundary.
- `\y` matches a text segment boundary, the definition of which varies based on the `y{w}` and `y{g}` matching option.
- `\Y` matches a non-text-segment-boundary.

#### Escape sequences

```
EscapeSequence -> '\a' | '\b' | '\c' <Char> | '\e' | '\f' | '\n' | '\r' | '\t'
```

These escape sequences each denote a specific scalar value.

- `\a`: The alert (bell) character `U+7`.
- `\b`: The backspace character `U+8`. Note this may only be used in a custom character class, otherwise it represents a word boundary.
- `\c <Char>`: A control character sequence, which denotes a scalar from `U+00` - `U+7F` depending on the ASCII character provided.
- `\e`: The escape character `U+1B`.
- `\f`: The form-feed character `U+C`.
- `\n`: The newline character `U+A`.
- `\r`: The carriage return character `U+D`.
- `\t`: The tab character `U+9`.

#### Builtin character classes

```
BuiltinCharClass -> '.' | '\C' | '\d' | '\D' | '\h' | '\H' | '\N' | '\O' | '\R' | '\s' | '\S' | '\v' | '\V' | '\w' | '\W' | '\X'
```

- `.`: Any character excluding newlines.
- `\C`: A single UTF code unit.
- `\d`: Digit character.
- `\D`: Non-digit character.
- `\h`: Horizontal space character.
- `\H`: Non-horizontal-space character.
- `\N`: Non-newline character.
- `\O`: Any character (including newlines). This is syntax from Oniguruma.
- `\R`: Newline sequence.
- `\s`: Whitespace character.
- `\S`: Non-whitespace character.
- `\v`: Vertical space character.
- `\V`: Non-vertical-space character.
- `\w`: Word character.
- `\W`: Non-word character.
- `\X`: Any extended grapheme cluster.

Precise definitions of character classes is discussed in [Character Classes for String Processing](https://forums.swift.org/t/pitch-character-classes-for-string-processing/52920).

#### Unicode scalars

```
UnicodeScalar -> '\u{' HexDigit{1...} '}'
               | '\u'  HexDigit{4}
               | '\x{' HexDigit{1...} '}'
               | '\x'  HexDigit{0...2}
               | '\U'  HexDigit{8}
               | '\o{' OctalDigit{1...} '}'
               | '\0' OctalDigit{0...3}

HexDigit   -> [0-9a-zA-Z]
OctalDigit -> [0-7]

NamedScalar -> '\N{' ScalarName '}'
ScalarName -> 'U+' HexDigit{1...8} | [\s\w-]+
```

These sequences define a unicode scalar value using hexadecimal or octal notation.

`\x`, when not followed by any hexadecimal digit characters, is treated as `\0`, matching PCRE's behavior.

`\N{...}` allows a specific Unicode scalar to be specified by name or hexadecimal code point.

#### Character properties

```
CharacterProperty      -> '\' ('p' | 'P') '{' PropertyContents '}'
POSIXCharacterProperty -> '[:' PropertyContents ':]'

PropertyContents -> PropertyName ('=' PropertyName)?
PropertyName     -> [\s\w-]+
```

A character property specifies a particular Unicode, POSIX, or PCRE property to match against. We propose supporting:

- The full range of Unicode character properties.
- The POSIX properties `alnum`, `blank`, `graph`, `print`, `word`, `xdigit` (note that `alpha`, `lower`, `upper`, `space`, `punct`, `digit`, and `cntrl` are covered by Unicode properties).
- The UTS#18 special properties `any`, `assigned`, `ascii`.
- The special PCRE2 properties `Xan`, `Xps`, `Xsp`, `Xuc`, `Xwd`.
- The special Java properties `javaLowerCase`, `javaUpperCase`, `javaWhitespace`, `javaMirrored`.

We follow [UTS#18][uts18]'s guidance for character properties, including fuzzy matching for property name parsing, according to rules set out by [UAX44-LM3]. The following property names are equivalent:

- `whitespace`
- `isWhitespace`
- `is-White_Space`
- `iSwHiTeSpaCe`
- `i s w h i t e s p a c e`

Unicode properties consist of both a key and a value, e.g `General_Category=Whitespace`. Each component follows the fuzzy matching rule, and additionally may have an alternative alias spelling, as defined by Unicode in [PropertyAliases.txt][unicode-prop-key-aliases] and [PropertyValueAliases.txt][unicode-prop-value-aliases].

There are some Unicode properties where the key or value may be inferred. These include:

- General category properties e.g `\p{Whitespace}` is inferred as `\p{General_Category=Whitespace}`.
- Script properties e.g `\p{Greek}` is inferred as `\p{Script_Extensions=Greek}`.
- Boolean properties that are inferred to have a `True` value, e.g `\p{Lowercase}` is inferred as `\p{Lowercase=True}`.
- Block properties that begin with the prefix `in`, e.g `\p{inBasicLatin}` is inferred to be `\p{Block=Basic_Latin}`.

Other Unicode properties however must specify both a key and value.

For non-Unicode properties, only a value is required. These include:

- The UTS#18 special properties `any`, `assigned`, `ascii`.
- The POSIX compatibility properties `alnum`, `blank`, `graph`, `print`, `word`, `xdigit`. The remaining POSIX properties are already covered by boolean Unicode property spellings.
- The special PCRE2 properties `Xan`, `Xps`, `Xsp`, `Xuc`, `Xwd`.
- The special Java properties `javaLowerCase`, `javaUpperCase`, `javaWhitespace`, `javaMirrored`.

Note that the internal `PropertyContents` syntax is shared by both the `\p{...}` and POSIX-style `[:...:]` syntax, allowing e.g `[:script=Latin:]` as well as `\p{alnum}`.

#### `\K`

The `\K` escape sequence is used to drop any previously matched characters from the final matching result. It does not affect captures, e.g `a(b)\Kc` when matching against `abc` will return a match of `c`, but with a capture of `b`.

### Groups

```
Group      -> GroupStart RegexNode ')'
GroupStart -> '(' GroupKind | '('
GroupKind  -> '' | '?' BasicGroupKind | '*' PCRE2GroupKind ':'

BasicGroupKind -> ':' | '|' | '>' | '=' | '!' | '*' | '<=' | '<!' | '<*'
                | NamedGroup
                | MatchingOptionSeq (':' | ')')

PCRE2GroupKind -> 'atomic'
                | 'pla' | 'positive_lookahead'
                | 'nla' | 'negative_lookahead'
                | 'plb' | 'positive_lookbehind'
                | 'nlb' | 'negative_lookbehind'
                | 'napla' | 'non_atomic_positive_lookahead'
                | 'naplb' | 'non_atomic_positive_lookbehind'
                | 'sr' | 'script_run'
                | 'asr' | 'atomic_script_run'

NamedGroup -> 'P<' GroupNameBody '>'
            | '<' GroupNameBody '>'
            | "'" GroupNameBody "'"

GroupNameBody -> Identifier | BalancingGroupBody

Identifier -> [\w--\d] \w*
```

Groups define a new scope that contains a recursively nested regex. Groups have different semantics depending on how they are introduced.

Note there are additional constructs that may syntactically appear similar to groups, such as backreferences and PCRE backtracking directives, but are distinct.

#### Basic group kinds

- `()`: A capturing group.
- `(?:)`: A non-capturing group.
- `(?|)`: A group that, for a direct child alternation, resets the numbering of groups at each branch of that alternation. See [Group Numbering](#group-numbering).

Capturing groups produce captures, which remember the range of input matched for the scope of that group.

A capturing group may be named using any of the `NamedGroup` syntax. The characters of the group name may be any letter or number characters or the character `_`. However the name must not start with a number. This restriction follows the behavior of other regex engines and avoids ambiguities when it comes to named and numeric group references.

#### Atomic groups

An atomic group e.g `(?>...)` specifies that its contents should not be re-evaluated for backtracking. This has the same semantics as a possessive quantifier, but applies more generally to any regex pattern.

#### Lookahead and lookbehind

These groups evaluate the input ahead or behind the current matching position, without advancing the input.

- `(?=`: A lookahead, which matches against the input following the current matching position.
- `(?!`: A negative lookahead, which ensures a negative match against the input following the current matching position.
- `(?<=`: A lookbehind, which matches against the input prior to the current matching position.
- `(?<!`: A negative lookbehind, which ensures a negative match against the input prior to the current matching position.

The above groups are all atomic, meaning that they will not be re-evaluated for backtracking. There are however also non-atomic variants:

- `(?*`: A non-atomic lookahead.
- `(?<*`: A non-atomic lookbehind.

PCRE2 also defines explicitly spelled out versions of the above syntax, e.g `(*non_atomic_positive_lookahead` and `(*negative_lookbehind:)`.

#### Script runs

A script run e.g `(*script_run:...)` specifies that the contents must match against a sequence of characters from the same Unicode script, e.g Latin or Greek.

#### Balancing groups

```
BalancingGroupBody -> Identifier? '-' Identifier
```

Introduced by .NET, [balancing groups][balancing-groups] extend the `GroupNameBody` syntax to support the ability to refer to a prior group. Upon matching, the prior group is deleted, and any intermediate matched input becomes the capture of the current group.

#### Group numbering

Capturing groups are implicitly numbered according to the position of their opening `(` in the regex. For example:

```
(a((?:b)(?<c>c)d)(e)f)
^ ^     ^        ^
1 2     3        4
```

Non-capturing groups are skipped over when counting.

Branch reset groups can alter this numbering, as they reset the numbering in the branches of an alternation child. Outside the alternation, numbering resumes at the next available number not used in one of the branches. For example:

```
(a()(?|(b)(c)|(?:d)|(e)))(f)
^ ^    ^  ^         ^    ^
1 2    3  4         3    5
```

### Custom character classes

```
CustomCharClass -> Start Set (SetOp Set)* ']'
Start           -> '[' '^'?
Set             -> Member+
Member          -> CustomCharClass | Quote | Range | Atom
Range           -> RangeElt `-` RangeElt
RangeElt        -> <Char> | UnicodeScalar | EscapeSequence
SetOp           -> '&&' | '--' | '~~' | '-'
```

Custom characters classes introduce their own sublanguage, in which most regular expression metacharacters become literal. The basic element in a custom character class is an `Atom`, though only some atoms are considered valid:

- Builtin character classes, except for `.`, `\R`, `\O`, `\X`, `\C`, and `\N`.
- Escape sequences, including `\b` which becomes the backspace character (rather than a word boundary).
- Unicode scalars.
- Named scalars.
- Character properties.
- Plain literal characters.

Atoms may be used to compose other character class members, including ranges, quoted sequences, and even nested custom character classes `[[ab]c\d]`. Adjacent members form an implicit union of character classes, e.g `[[ab]c\d]` is the union of the characters `a`, `b`, `c`, and digit characters.

Custom character classes may not be empty, e.g `[]` is forbidden. A custom character class may begin with the `]` character, in which case it is treated as literal, e.g `[]a]` is the custom character class of `]` and `a`.

Quoted sequences may be used to escape the contained characters, e.g `[a\Q]\E]` is also the character class of `[` and `a`.

Ranges of characters may be specified with `-`, e.g `[a-z]` matches against the letters from `a` to `z`. Only unicode scalars and literal characters are valid range operands. If `-` cannot be used to form a range, it is interpreted as literal, e.g `[-a-]` is the character class of `-` and `a`. `[a-c-d]` is the character class of `a`...`c`, `-`, and `d`.

Operators may be used to apply set operations to character class members. The operators supported are:

- `&&`: Intersection of the LHS and RHS.
- `--`: Subtraction of the RHS from the LHS.
- `~~`: Symmetric difference of the RHS and LHS.
- `-`: .NET's spelling of subtracting the RHS from the LHS.

These operators have a lower precedence than the implicit union of members, e.g `[ac-d&&a[d]]` is an intersection of the character classes `[ac-d]` and `[ad]`.

To avoid ambiguity between .NET's subtraction syntax and range syntax, .NET specifies that a subtraction will only be parsed if the right-hand-side is a nested custom character class. We propose following this behavior.


### Matching options

```
MatchingOptionSeq -> '^' MatchingOption*
                   | MatchingOption+
                   | MatchingOption* '-' MatchingOption*

MatchingOption -> 'i' | 'J' | 'm' | 'n' | 's' | 'U' | 'x' | 'xx' | 'w' | 'D' | 'P' | 'S' | 'W' | 'y{' ('g' | 'w') '}'
```

A matching option sequence may be used as a group specifier, and denotes a change in matching options for the scope of that group. For example `(?x:a b c)` enables extended syntax for `a b c`. A matching option sequence may be part of an "isolated group" which has an implicit scope that wraps the remaining elements of the current group. For example, `(?x)a b c` also enables extended syntax for `a b c`.

If used in the branch of an alternation, an isolated group affects all the following branches of that alternation. For example, `a(?i)b|c|d` is treated as `a(?i:b)|(?i:c)|(?i:d)`.

We support all the matching options accepted by PCRE, ICU, and Oniguruma. In addition, we accept some matching options unique to our matching engine.

#### PCRE options

- `i`: Case insensitive matching.
- `J`: Allows multiple groups to share the same name, which is otherwise forbidden.
- `m`: Enables `^` and `$` to match against the start and end of a line rather than only the start and end of the entire string.
- `n`: Disables the capturing behavior of `(...)` groups. Named capture groups must be used instead.
- `s`: Changes `.` to match any character, including newlines.
- `U`: Changes quantifiers to be reluctant by default, with the `?` specifier changing to mean greedy.
- `x`, `xx`: Enables extended syntax mode, which allows non-semantic whitespace and end-of-line comments. See [Extended Syntax Modes](#extended-syntax-modes) for more info.

#### ICU options

- `w`: Enables the Unicode interpretation of word boundaries `\b`.

#### Oniguruma options

- `D`: Enables ASCII-only digit matching for `\d`, `\p{Digit}`, `[:digit:]`.
- `S`: Enables ASCII-only space matching for `\s`, `\p{Space}`, `[:space:]`.
- `W`: Enables ASCII-only word matching for `\w`, `\p{Word}`, `[:word:]`, and `\b`.
- `P`: Enables ASCII-only for all POSIX properties (including `digit`, `space`, and `word`).
- `y{g}`, `y{w}`: Changes the meaning of `\X`, `\y`, `\Y`. These are mutually exclusive options, with `y{g}` specifying extended grapheme cluster mode, and `y{w}` specifying word mode.

#### Swift options

These options are specific to the Swift regex matching engine and control the semantic level at which matching takes place.

- `X`: Grapheme cluster matching.
- `u`: Unicode scalar matching.
- `b`: Byte matching.

Further details on these are TBD and outside the scope of this pitch.
      
### References

```
NamedOrNumberRef -> NamedRef | NumberRef
NamedRef         -> Identifier RecursionLevel?
NumberRef        -> ('+' | '-')? <Decimal Number> RecursionLevel?
RecursionLevel   -> '+' <Int> | '-' <Int>
```

A reference is an abstract identifier for a particular capturing group in a regular expression. It can either be named or numbered, and in the latter case may be specified relative to the current group. For example `-2` refers to the capture group `N - 2` where `N` is the number of the next capture group. References may refer to groups ahead of the current position e.g `+3`, or the name of a future group. These may be useful in recursive cases where the group being referenced has been matched in a prior iteration. If a referenced capture does not exist anywhere in the regular expression, the reference is diagnosed as invalid.

A backreference may optionally include a recursion level in certain cases, which is a syntactic element inherited [from Oniguruma][oniguruma-syntax] that allows the reference to specify a capture relative to a given recursion level.

#### Backreferences

```
Backreference -> '\g{' NamedOrNumberRef '}'
               | '\g' NumberRef
               | '\k<' NamedOrNumberRef '>'
               | "\k'" NamedOrNumberRef "'"
               | '\k{' NamedRef '}'
               | '\' [1-9] [0-9]+
               | '(?P=' NamedRef ')'
```

A backreference evaluates to the value last captured by the referenced capturing group. If the referenced capture has not been evaluated yet, the match fails.

#### Subpatterns

```
Subpattern -> '\g<' NamedOrNumberRef '>'
            | "\g'" NamedOrNumberRef "'"
            | '(?' GroupLikeSubpatternBody ')'

GroupLikeSubpatternBody -> 'P>' NamedRef
                         | '&' NamedRef
                         | 'R'
                         | NumberRef
```

A subpattern causes the referenced capture group to be re-evaluated at the current position. The syntax `(?R)` is equivalent to `(?0)`, and causes the entire pattern to be recursed.

### Conditionals

```
Conditional      -> ConditionalStart Concatenation ('|' Concatenation)? ')'
ConditionalStart -> KnownConditionalStart | GroupConditionalStart

KnownConditionalStart -> '(?(' KnownCondition ')'
GroupConditionalStart -> '(?' GroupStart

KnownCondition -> 'R'
                | 'R' NumberRef
                | 'R&' NamedRef
                | '<' NamedOrNumberRef '>'
                | "'" NamedOrNumberRef "'"
                | 'DEFINE'
                | 'VERSION' VersionCheck
                | NumberRef

PCREVersionCheck  -> '>'? '=' PCREVersionNumber
PCREVersionNumber -> <Int> '.' <Int>
```

A conditional evaluates a particular condition, and chooses a branch to match against accordingly. 1 or 2 branches may be specified. If 1 branch is specified e.g `(?(...)x)`, it is treated as the true branch. Note this includes an empty true branch, e.g `(?(...))` which is the null pattern as described in the [Top-Level Regular Expression](#top-level-regular-expression) section. If 2 branches are specified, e.g `(?(...)x|y)`, the first is treated as the true branch, the second being the false branch.

A condition may be:

- A numeric or delimited named reference to a capture group, which checks whether the group matched successfully.
- A recursion check on either a particular group or the entire regex. In the former case, this checks to see if the last recursive call is through that group. In the latter case, it checks if the match is currently taking place in any kind of recursive call.
- A PCRE version check.

If the condition does not syntactically match any of the above, it is treated as an arbitrary recursive regular expression. This will be matched against, and evaluates to true if the match is successful. It may contain capture groups that add captures to the match.

The `DEFINE` keyword is not used as a condition, but rather a way in which to define a group which is not evaluated, but may be referenced by a subpattern.

### PCRE backtracking directives

```
BacktrackingDirective     -> '(*' BacktrackingDirectiveKind (':' <String>)? ')'
BacktrackingDirectiveKind -> 'ACCEPT' | 'FAIL' | 'F' | 'MARK' | '' | 'COMMIT' | 'PRUNE' | 'SKIP' | 'THEN'
```

This is syntax specific to PCRE, and is used to control backtracking behavior. Any of the directives may include an optional tag, however `MARK` must have a tag. The empty directive is treated as `MARK`. Only the `ACCEPT` directive may be quantified, as it can use the backtracking behavior of the engine to be evaluated only if needed by a reluctant quantification.

- `ACCEPT`: Causes matching to terminate immediately as a successful match. If used within a subpattern, only that level of recursion is terminated.
- `FAIL`, `F`: Causes matching to fail, forcing backtracking to occur if possible.
- `MARK`: Assigns a label to the current matching path, which is passed back to the caller on success. Subsequent `MARK` directives overwrite the label assigned, so only the last is passed back.
- `COMMIT`: Prevents backtracking from reaching any point prior to this directive, causing the match to fail. This does not allow advancing the input to try a different starting match position.
- `PRUNE`: Similar to `COMMIT`, but allows advancing the input to try and find a different starting match position.
- `SKIP`: Similar to `PRUNE`, but skips ahead to the position of `SKIP` to try again as the starting position.
- `THEN`: Similar to `PRUNE`, but when used inside an alternation will try to match in the subsequent branch before attempting to advance the input to find a different starting position.

### PCRE global matching options

```
GlobalMatchingOptionSequence -> GlobalMatchingOption+
GlobalMatchingOption -> '(*' GlobalMatchingOptionKind ')'

GlobalMatchingOptionKind -> LimitOptionKind '=' <Int>
                          | NewlineKind | NewlineSequenceKind
                          | 'NOTEMPTY_ATSTART' | 'NOTEMPTY'
                          | 'NO_AUTO_POSSESS' | 'NO_DOTSTAR_ANCHOR'
                          | 'NO_JIT' | 'NO_START_OPT' | 'UTF' | 'UCP'

LimitOptionKind     -> 'LIMIT_DEPTH' | 'LIMIT_HEAP' | 'LIMIT_MATCH'
NewlineKind         -> 'CRLF' | 'CR' | 'ANYCRLF' | 'ANY' | 'LF' | 'NUL'
NewlineSequenceKind -> 'BSR_ANYCRLF' | 'BSR_UNICODE'
```

This is syntax specific to PCRE, and allows a set of global options to appear at the start of a regular expression. They may not appear at any other position.

- `LIMIT_DEPTH`, `LIMIT_HEAP`, `LIMIT_MATCH`: These place certain limits on the resources the matching engine may consume, and matches it may make.
- `CRLF`, `CR`, `ANYCRLF`, `ANY`, `LF`, `NUL`: These control the definition of a newline character, which is used when matching e.g the `.` character class, and evaluating where a line ends in multi-line mode.
- `BSR_ANYCRLF`, `BSR_UNICODE`: These change the definition of `\R`.
- `NOTEMPTY`: Does not consider the empty string to be a valid match.
- `NOTEMPTY_ATSTART`: Like `NOT_EMPTY`, but only applies to the first matching position in the input.
- `NO_AUTO_POSSESS`: Disables an optimization that treats a quantifier as possessive if the following construct clearly cannot be part of the match. In other words, disables the short-circuiting of backtracks in cases where the engine knows it will not produce a match. This is useful for debugging, or for ensuring a callout gets invoked.
- `NO_DOTSTAR_ANCHOR`: Disables an optimization that tries to automatically anchor `.*` at the start of a regex. Like `NO_AUTO_POSSESS`, this is mainly used for debugging or ensuring a callout gets invoked.
- `NO_JIT`: Disables JIT compilation
- `NO_START_OPT`: Disables various optimizations performed at the start of matching. Like `NO_DOTSTAR_ANCHOR`, is mainly used for debugging or ensuring a callout gets invoked.
- `UTF`: Enables UTF pattern support.
- `UCP`: Enables Unicode property support.

### Callouts

```
Callout -> PCRECallout | NamedCallout | InterpolatedCallout

PCRECallout -> '(?C' CalloutBody ')'
PCRECalloutBody -> '' | <Number>
                 | '`' <String> '`'
                 | "'" <String> "'"
                 | '"' <String> '"'
                 | '^' <String> '^'
                 | '%' <String> '%'
                 | '#' <String> '#'
                 | '$' <String> '$'
                 | '{' <String> '}'

NamedCallout   -> '(*' Identifier CalloutTag? CalloutArgs? ')'
CalloutArgs    -> '{' CalloutArgList '}'
CalloutArgList -> CalloutArg (',' CalloutArgList)*
CalloutArg     -> [^,}]+
CalloutTag     -> '[' Identifier ']'

InterpolatedCallout -> '(?' '{' Interpolation '}' CalloutTag? CalloutDirection? ')'
Interpolation       -> <String> | '{' Interpolation '}'
CalloutDirection    -> 'X' | '<' | '>'
```

A callout is a feature that allows a user-supplied function to be called when matching reaches that point in the pattern. We supported parsing 3 types of callout:

- PCRE callout syntax, which accepts a string or numeric argument that is passed to the function.
- Oniguruma named callout syntax, which accepts an identifier with an optional tag and argument list.
- Interpolated callout syntax, which is equivalent to Oniguruma's "callout of contents". This callout accepts an arbitrary interpolated program. This is an expanded version of Perl's interpolation syntax, and allows an arbitrary nesting of delimiters in addition to an optional tag and direction.

While we propose parsing these for the purposes of issuing helpful diagnostics, we are deferring full support for the interpolated syntax for the future.

### Absent functions

```
AbsentFunction -> '(?~' RegexNode ')'
                | '(?~|' Concatenation '|' Concatenation ')'
                | '(?~|' Concatenation ')'
                | '(?~|)'
```

An absent function is an [Oniguruma][oniguruma-syntax] feature that allows for the easy inversion of a given pattern. There are 4 variants of the syntax:

- `(?~|absent|expr)`: Absent expression, which attempts to match against `expr`, but is limited by the range that is not matched by `absent`.
- `(?~absent)`: Absent repeater, which matches against any input not matched by `absent`. Equivalent to `(?~|absent|\O*)`.
- `(?~|absent)`: Absent stopper, which limits any subsequent matching to not include `absent`.
- `(?~|)`: Absent clearer, which undoes the effects of the absent stopper.


## Syntactic differences between engines

The proposed "syntactic superset" introduces some minor ambiguities, as each engine supports a slightly different set of features. When a particular engine's parser sees a feature it doesn't support, it typically has a fall-back behavior, such as treating the unknown feature as literal contents.

Explicit compatibility modes, i.e. precisely mimicking emergent behavior from a specific engine's parser, is deferred as future work from this proposal. Conversion from this "syntactic superset" to a particular engine's syntax (e.g. as an AST "pretty printer") is deferred as future work from this proposal.

Below is an exhaustive treatment of every syntactic ambiguity we have encountered.

### Character class set operations

In a custom character class, some engines allow for binary set operations that take two character class inputs, and produce a new character class output. However which set operations are supported and the spellings used differ by engine.

| PCRE | ICU | UTS#18 | Oniguruma | .NET | Java |
|------|-----|--------|-----------|------|------|
| ❌ | Intersection `&&`, Subtraction `--` | Intersection, Subtraction | Intersection `&&` | Subtraction via `-` | Intersection  `&&` |


[UTS#18][uts18] requires intersection and subtraction, and uses the operation spellings `&&` and `--` in its examples, though it doesn't mandate a particular spelling. In particular, conforming implementations could spell the subtraction `[[x]--[y]]` as `[[x]&&[^y]]`. UTS#18 also suggests a symmetric difference operator `~~`, and uses an explicit `||` operator in examples, though doesn't require either.

Engines that don't support a particular operator fallback to treating it as literal, e.g `[x&&y]` in PCRE is the character class of `["x", "&", "y"]` rather than an intersection.

Unlike other engines, .NET supports the use of `-` to denote both a range as well as a set subtraction. .NET disambiguates this by only permitting its use as a subtraction if the right hand operand is a nested custom character class, otherwise it is a range operator. This conflicts with e.g ICU where `[x-[y]]`, in which the `-` is treated as literal.

We propose supporting the operators `&&`, `--`, and `~~`. This means that any regex literal containing these sequences in a custom character class while being written for an engine not supporting that operation will have a different semantic meaning in our engine. However this ought not to be a common occurrence, as specifying a character multiple times in a custom character class is redundant.

In the interests of compatibility, we also propose supporting the `-` operator, though we will likely want to emit a warning and encourage users to switch to `--`.

### Nested custom character classes

This allows e.g `[[a]b[c]]`, which is interpreted the same as `[abc]`. It also allows for more complex set operations with custom character classes as the operands.

| PCRE | ICU | UTS#18 | Oniguruma | .NET | Java |
|------|-----|--------|-----------|------|------|
| ❌ | ✅ | 💡 | ✅ | ❌ | ✅ |


UTS#18 doesn't require this, though it does suggest it as a way to clarify precedence for chains of character class set operations e.g `[\w--\d&&\s]`, which the user could write as `[[\w--\d]&&\s]`.

PCRE does not support this feature, and as such treats `]` as the closing character of the custom character class. Therefore `[[a]b[c]]` is interpreted as the character class `["[", "a"]`, followed by literal `b`, and then the character class `["c"]`, followed by literal `]`.

.NET does not support nested character classes in general, although allows them as the right-hand side of a subtraction operation.

We propose allowing nested custom character classes.

### `\U`

In PCRE, if `PCRE2_ALT_BSUX` or `PCRE2_EXTRA_ALT_BSUX` are specified, `\U` matches literal `U`. However in ICU, `\Uhhhhhhhh` matches a hex sequence. We propose following the ICU behavior.

### `{,n}`

This quantifier is supported by Oniguruma, but in PCRE it matches the literal characters `{`, `,`, `n`, and `}` in sequence. We propose supporting it as a quantifier.

### `\DDD`

This syntax is implemented in a variety of different ways depending on the engine. In ICU and Java, it is always a backreference unless prefixed with `0`, in which case it is an octal sequence.

In PCRE, Oniguruma, and .NET, it is also always an octal sequence if prefixed with `0`, however there are other cases where it may be treated as octal. These cases vary slightly between the engines. In PCRE, it will be treated as backreference if any of the following hold:

- Its value is `0 < n < 10`.
- Its first digit is `8` or `9`.
- Its value corresponds to a valid *prior* group number.

Otherwise it is treated as an octal sequence.

Oniguruma follows all of these except the second. If the first digit is `8` or `9`, it is instead treated as the literal number, e.g `\81` is `81`. .NET also follows this behavior, but additionally has the last condition consider *all* groups, not just prior ones (as backreferences can refer to future groups in recursive cases).

We propose a simpler behavior more inline with ICU and Java. A `\DDD` sequence that does not start with a `0` will be treated as a backreference, otherwise it will be treated as an octal sequence. If an invalid backreference is formed with this syntax, we will suggest prefixing with a `0` if an octal sequence is desired.

One further difference exists between engines in the octal sequence case. In ICU, up to 3 additional digits are read after the `0`. In PCRE, only 2 additional digits may be interpreted as octal, the last is literal. We will follow the ICU behavior, as it is necessary when requiring a `0` prefix.

### `\x`

In PCRE, a bare `\x` denotes the NUL character (`U+00`). In Oniguruma, it denotes literal `x`. We propose following the PCRE behavior.

### Whitespace in ranges

In PCRE, `x{2,4}` is a range quantifier meaning that `x` can be matched from 2 to 4 times. However if any whitespace is introduced within the braces e.g `x{2, 4}`, it becomes an invalid range and is then treated as the literal characters instead. We find this behavior to be unintuitive, and therefore propose parsing any intermixed whitespace in the range.

### Implicitly-scoped matching option scopes

PCRE and Oniguruma both support changing the active matching options through an isolated group e.g `(?i)`. However, they have differing semantics when it comes to their scoping. In Oniguruma, it is treated as an implicit new scope that wraps everything until the end of the current group. In PCRE, it is treated as changing the matching option for all the following expressions until the end of the group.

These sound similar, but have different semantics around alternations, e.g for `a(?i)b|c|d`, in Oniguruma this becomes `a(?i:b|c|d)`, where `a` is no longer part of the alternation. However in PCRE it becomes `a(?i:b)|(?i:c)|(?i:d)`, where `a` remains a child of the alternation.

We propose matching the PCRE behavior.

### Backreference condition kinds

PCRE and .NET allow for conditional patterns to reference a group by its name without any form of delimiter, e.g:

```
(?<group1>x)?(?(group1)y)
```

where `y` will only be matched if `(?<group1>x)` was matched. PCRE will always treat such syntax as a backreference condition, however .NET will only treat it as such if a group with that name exists somewhere in the regex (including after the conditional). Otherwise, .NET interprets `group1` as an arbitrary regular expression condition to try match against. Oniguruma on the other hand will always treat `group1` as an regex condition to match against.

We propose parsing such conditions as an arbitrary regular expression condition, as long as they do not conflict with other known condition spellings such as `R&name`. If the condition has a name that matches a named group in the regex, we will emit a warning asking users to explicitly use the syntax `(?(<group1>)y)` if they want a backreference condition. This more explicit syntax is supported by both PCRE and Oniguruma.

### `\N`

PCRE supports `\N` meaning "not a newline", however there are engines that treat it as a literal `N`. We propose supporting the PCRE behavior.

### Extended character property syntax

ICU unifies the character property syntax `\p{...}` with the syntax for POSIX character classes `[:...:]`, such that they follow the same internal grammar, which allows referencing any Unicode character property in addition to the POSIX properties. We propose supporting this, though it is a purely additive feature, and therefore should not conflict with regex engines that implement a more limited POSIX syntax.

### Script properties

Shorthand script property syntax e.g `\p{Latin}` is treated as `\p{Script=Latin}` by PCRE, ICU, Oniguruma, and Java. These use [the Unicode Script property][unicode-scripts], which assigns each scalar a particular script value. However, there are scalars that may appear in multiple scripts, e.g U+3003 DITTO MARK. These are often assigned to the `Common` script to reflect this fact, which is not particularly useful for matching purposes. To provide more fine-grained script matching, Unicode provides [the Script Extension property][unicode-script-extensions], which exposes the set of scripts that a scalar appears in.

As such we feel that the more desirable default behavior of shorthand script property syntax e.g `\p{Latin}` is for it to be treated as `\p{Script_Extension=Latin}`. This matches Perl's default behavior. Plain script properties may still be written using the more explicit syntax e.g `\p{Script=Latin}` and `\p{sc=Latin}`.

### Extended syntax modes

Various regex engines offer an "extended syntax" where whitespace is treated as non-semantic (e.g `a b c` is equivalent to `abc`), in addition to allowing end-of-line comments `# comment`. In both PCRE and Perl, this is enabled through the `(?x)`, and in later versions, `(?xx)` matching options. The former allows non-semantic whitespace outside of character classes, and the latter also allows non-semantic whitespace in custom character classes.

Oniguruma, Java, and ICU however enable the more broad behavior under `(?x)`. We therefore propose following this behavior, with `(?x)` and `(?xx)` being treated the same.

Different regex engines also have different rules around what characters are considered non-semantic whitespace. When compiled with Unicode support, PCRE considers the following whitespace:

- The space character `U+20`
- Whitespace characters `U+9...U+D`
- Next line `U+85`
- Left-to-right mark `U+200E`
- Right-to-left mark `U+200F`
- Line separator `U+2028`
- Paragraph separator `U+2029`

This is a subset of the scalars matched by `UnicodeScalar.isWhitespace`. Additionally, in a custom character class, PCRE only considers the space and tab characters as whitespace. Other engines do not differentiate between whitespace characters inside and outside custom character classes, and appear to follow a subset of this list. Therefore we propose supporting exactly the characters in this list for the purposes of non-semantic whitespace parsing.

### Group numbering

In PCRE, groups are numbered according to the position of their opening parenthesis. .NET also follows this rule, with the exception that named groups are numbered after unnamed groups. For example:

```
(a(?<x>x)b)(?<y>y)(z)
^ ^        ^      ^
1 3        4      2
```

The `(z)` group gets numbered before the named groups get numbered.

We propose matching the PCRE behavior where groups are numbered purely based on order.


## Swift canonical syntax

The proposed syntactic superset means there will be multiple ways to write the same thing. Below we discuss what Swift's preferred spelling could be, a "Swift canonical syntax".

We are not formally proposing this as a distinct syntax or concept, rather it is useful for considering compiler features such as fixits, pretty-printing, and refactoring actions. We're hoping for further discussion with the community here. Useful criteria include how well the choice fits in with the rest of Swift, whether there's an existing common practice, and whether one choice is less confusing in the context of others.

[Unicode scalar literals](#unicode-scalars) can be spelled in many ways. We propose treating Swift's string literal syntax of `\u{HexDigit{1...}}` as the preferred spelling.

Character properties can be spelled `\p{...}` or `[:...:]`. We recommend preferring `\p{...}` as the bracket syntax historically meant POSIX-defined character classes, and still has that connotation in some engines. The [spelling of properties themselves can be fuzzy](#character-properties) and we (weakly) recommend the shortest spelling (no opinion on casing yet). For script extensions, we (weakly) recommend e.g. `\p{Greek}` instead of `\p{Script_Extensions=Greek}`. We would like more discussion with the community here.

[Lookaround assertions](#lookahead-and-lookbehind) have common shorthand spellings, while PCRE2 introduced longer more explicit spellings. We are (very weakly) recommending the common short-hand syntax of e.g. `(?=...)` as that's wider spread. We are interested in more discussion with the community here.

Named groups may be specified with a few different delimiters: `(?<name>...)`, `(?P<name>...)`, `(?'name'...)`. We (weakly) recommend `(?<name>...)`, but the final preference may be influenced by choice of delimiter for the regex itself. We'd appreciate any insight from the community.

[Backreferences](#backreferences) have multiple spellings. For absolute numeric references, `\DDD` seems to be a strong candidate for the preferred syntax due to its familiarity. For relative numbered references, as well as named references, either `\k<...>` or `\k'...'` seem like the better choice, depending on the syntax chosen for named groups. This avoids the confusion between `\g{...}` and `\g<...>` referring to a backreferences and subpatterns respectively, as well as any confusion with group syntax. 

For [subpatterns](#subpatterns), we recommend either `\g<...>` or `\g'...'` depending on the choice for named group syntax. We're unsure if we should prefer `(?R)` as a spelling for e.g. `\g<0>` or not, as it is more widely used and understood, but less consistent with other subpatterns.

[Conditional references](#conditionals) have a choice between `(?('name'))` and `(?(<name>))`. The preferred syntax in this case would likely reflect the syntax chosen for named groups.

We are deferring runtime support for callouts from regex literals as future work, though we will correctly parse their contents. We have no current recommendation for a preference of PCRE-style [callout syntax](#callouts), and would like to discuss with the community whether we should have one.

## Alternatives Considered


### Skip the syntax

The top alternative is to just skip regex syntax altogether by only shipping the result builder DSL and forbidding run-time regex construction from strings. However, doing so would miss out on the familiarity benefits of existing regex syntax. Additionally, without support for run-time strings containing regex syntax, important domains would be closed off from better string processing, such as command-line tools and user-input searches. This would land us in a confusing world where NSRegularExpression, even though it operates over a fundamentally different model of string than Swift's `String` and exhibits different behavior than Swift regexes, is still used for these purposes.

We consider our proposed direction to be more compelling, especially when coupled with refactoring actions to convert literals into regex DSLs.

### Introduce a novel regex syntax

Another alternative is to invent a new syntax for regex. This would similarly lose out on the familiarity benefit, though a few simple adjustments could aid readability.

We are prototyping an "experimental" Swift extended syntax, which is future work and outside the scope of this proposal. Every syntactic extension, while individually compelling, does introduce incompatibilities and can lead to an "uncanny valley" effect. Further investigation is needed and such support can be built on top of what is presented here.

### Support a minimal syntactic subset

Regex syntax will become part of Swift's source and binary-compatibility story, so a reasonable alternative is to support the absolute minimal syntactic subset available. However, we would need to ensure that such a minimal approach is extensible far into the future. Because syntax decisions can impact each other, we would want to consider the ramifications of this full syntactic superset ahead of time anyways.

Even though it is more work up-front and creates a longer proposal, it is less risky to support the full intended syntax. The proposed superset maximizes the familiarity benefit of regex syntax.


[pcre2-syntax]: https://www.pcre.org/current/doc/html/pcre2syntax.html
[oniguruma-syntax]: https://github.com/kkos/oniguruma/blob/master/doc/RE
[icu-syntax]: https://unicode-org.github.io/icu/userguide/strings/regexp.html
[uts18]: https://www.unicode.org/reports/tr18/
[.net-syntax]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions
[UAX44-LM3]: https://www.unicode.org/reports/tr44/#UAX44-LM3
[unicode-prop-key-aliases]: https://www.unicode.org/Public/UCD/latest/ucd/PropertyAliases.txt
[unicode-prop-value-aliases]: https://www.unicode.org/Public/UCD/latest/ucd/PropertyValueAliases.txt
[unicode-scripts]: https://www.unicode.org/reports/tr24/#Script
[unicode-script-extensions]: https://www.unicode.org/reports/tr24/#Script_Extensions
[balancing-groups]: https://docs.microsoft.com/en-us/dotnet/standard/base-types/grouping-constructs-in-regular-expressions#balancing-group-definitions
