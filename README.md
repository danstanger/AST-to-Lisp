# AST-to-Lisp
Update of jnil to use antlr, and other ast generators.

Here is a link to the original project: https://github.com/francogrex/jnil .
Attached is a paper which describes something about it: https://dl.acm.org/doi/pdf/10.1145/1622123.1622147. Here is a link to the java to python converter whose antlr3 parser I modified slightly
https://github.com/natural/java2python .
If you look at the folder https://github.com/francogrex/jnil/tree/master/downloads/jnil/lisp/jnil/ast you will find lisp files which correspond to eclipse classes which are created by parsing a java file. What I did so far is to use the parser to output an ast tree in sexpression format which I read in and started creating the lisp eclipse classes from them. I used methods which used eql specializers corresponding to the tokens in the sexpression. I went through a few iterations of deciding how to write the processing and I have something that produces some (incorrect) output. My motivation for the project was to translate Mathics Python pattern matching code to lisp for the maxima project.
