A Lexer Toolkit

In this project, we implement a lexer toolkit based on generalized regular state machines (GRSM). To make this project feasible, we use derivatives of regular expressions.We use this toolkit to implement a lexer for S-Expressions. 
The basic working code was given by Prof.Matt Might.
I wrote a match case for space,newline and tab.Changed chartoken class to Longchar token class to handle '#\newline" and "#\tab".
