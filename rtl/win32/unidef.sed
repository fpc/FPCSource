s/function \([^(]*\)W *(/function \1(/
s/procedure \([^(]*\)W *(/procedure \1(/
s/function \([^:(]*\)W *: */function \1 : /
s/procedure \([^;(]*\)W *;/procedure \1;/
s/\([^ \t]*\)W *:=/\1:=/
s/unifun;/unidef;/
s/unifun.pp,v/unidef.pp,v/
s/UNICODEFUNCTIONS/UNICODEFUNCFIONSDEFAULT/

