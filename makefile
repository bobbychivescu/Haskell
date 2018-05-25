HS = ghc 
HFLAGS = -o
EXECNAME = myinterpreter
LEXER = alex
GRAMMAR = happy

all: compileInterpreter

compileInterpreter: Tokens.hs Grammar.hs
	$(HS) $(HFLAGS) $(EXECNAME) interpreter.hs

Tokens.hs: Tokens.x
	$(LEXER) Tokens.x

Grammar.hs: Grammar.y Tokens.hs
	$(GRAMMAR) Grammar.y

