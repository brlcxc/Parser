File01.txt: Valid, should parse. There's a logic error (infinite loop) but no syntax issue. 
File02.txt: Undeclared variable, but that's a semantic issue. Should parse OK. 
File03.txt: Syntax error (mismatched parens) 
File04.txt: Scan error, illegal character $ 
File05.txt: Should parse OK. Note that this grammar doesn't specify order of operations, it's just left-to-right. Also note addition of -1, and label that is similar to keyword ('endpoint'). 
