import std/strutils, std/strformat

# Lexer
type
  TokenKind* = enum
    tokenLeftParen = "LeftParen",
    tokenRightParen = "RightParen",
    tokenIdentifier = "Identifier",
    tokenIntLiteral = "IntegerLiteral",
    tokenFloatLiteral = "FloatLiteral",
    tokenStringLiteral = "StringLiteral"
    tokenEndOfFile = "EndOfFile"

type
  Token* = object
    beg, size: int
    kind: TokenKind

type
  Lexer* = object
    originalInput: string
    position: int
    tokens: seq[Token]
  LexerException* = object of CatchableError

proc calculateLoc(lex: Lexer, beg: int): (int, int) =
  var
    line = 1
    col = 1
  for i in 0 ..< beg:
    if lex.originalInput[i] == '\n':
      inc line
      col = 1
    inc col
  return (line, col)

proc error(lex: Lexer, beg: int, msg: string) =
  var (line, col) = calculateLoc(lex, beg)
  raise LexerException.newException(fmt"error:{line}:{col}: {msg}")

proc iseof(lex: Lexer): bool =
  return lex.position >= len(lex.originalInput) - 1

proc peek(lex: Lexer): char =
  if lex.iseof():
    return '\0'
  return lex.originalInput[lex.position]

proc consume(lex: var Lexer) =
  inc lex.position

proc checkConsume(lex: var Lexer, c: char) =
  if peek(lex) != c:
     error(lex, lex.position, fmt"check failed: '{peek(lex)}' != '{c}'")
  inc lex.position

proc skipWhitespaces(lex: var Lexer) =
  while isSpaceAscii(lex.peek):
    inc lex.position

proc parseSingleCharToken(lex: var Lexer, kind: TokenKind) =
  lex.tokens.add(
    Token(beg: lex.position, size: 1, kind: kind))
  lex.consume()

proc parseIdentifier(lex: var Lexer) =
  var beg = lex.position
  lex.consume()
  var size = 1
  while lex.peek in IdentChars:
    lex.consume()
    inc size
  lex.tokens.add(
    Token(beg: beg, size: size, kind: tokenIdentifier))

proc parseLiteral(lex: var Lexer) =
  var beg = lex.position
  var size = 0
  var kind = tokenIntLiteral
  while lex.peek.isDigit:
    lex.consume()
    inc size
    if lex.peek() == '.':
      kind = tokenFloatLiteral
      lex.checkConsume('.')
      inc size
  lex.tokens.add(
    Token(beg: beg, size: size, kind: kind))

proc parseStringLiteral(lex: var Lexer) =
  var beg = lex.position
  var size = 0
  lex.checkConsume('"')
  inc size
  while lex.peek != '"':
    if lex.iseof():
      error(lex, beg, "unclosed string")
    lex.consume()
    inc size
  lex.checkConsume('"')
  inc size
  lex.tokens.add(
    Token(beg: beg, size: size, kind: tokenStringLiteral))

proc parseCurrentToken(lex: var Lexer) =
  lex.skipWhitespaces()
  case lex.peek():
    of '(':
      parseSingleCharToken(lex, tokenLeftParen)
    of ')':
      parseSingleCharToken(lex, tokenRightParen)
    of IdentStartChars:
      parseIdentifier(lex)
    of Digits:
      parseLiteral(lex)
    of '\"':
      parseStringLiteral(lex)
    of '\0':
      lex.tokens.add(
        Token(beg: lex.position, size: 1, kind: tokenEndOfFile))
    else:
      error(lex, lex.position, fmt"could not parse '{lex.peek}'")

proc initLexer(input: string): Lexer =
  result = Lexer(originalInput: input, position: 0, tokens: @[])
  while true:
    if len(result.tokens) > 0:
      if result.tokens[^1].kind == tokenEndOfFile:
        break
    result.parseCurrentToken()

# AST
type ASTKind* = enum
  AST_Integer = "Integer"
  AST_Nil = "Nil"

type AST* = ref object
  case kind*: ASTKind
  of AST_Integer:
    intValue*: Token
  of AST_Nil:
    nilValue*: ref AST

type Parser* = object
  lex: Lexer
  position: int

proc peek(parser: Parser): Token =
  return parser.lex.tokens[parser.position]

proc skip(parser: var Parser) =
  inc parser.position

proc parseHead(parser: var Parser): AST =
  case parser.peek().kind:
    of tokenIntLiteral:
      return AST(kind: intValue, intValue: parser.peek())

proc parse(parser: var Parser): AST =
  var
    head = parseHead(lex)
   # tail = parseTail(lex, head)

when isMainModule:
  let code = "(define (f x))"
  var lex = initLexer(code)
  echo lex
