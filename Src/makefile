all: SXLexer
	@echo "compiled"

Token:
	scalac src/Token.scala

LiveStream:
	scalac src/LiveStream.scala

Lexer: Token LiveStream
	scalac src/Lexer.scala

SXLexer: Lexer
	scalac src/SXLexer.scala

run:
	@scala SXLexerDoit
