s/function \([^(]*\)A *(/function \1(/
s/procedure \([^(]*\)A *(/procedure \1(/
s/function \([^:(]*\)A *: */function \1 : /
s/procedure \([^;(]*\)A *;/procedure \1;/
s/\([^ \t]*\)A *:=/\1:=/
s/ascfun;/ascdef;/
s/ascfun.pp,v/ascdef.pp,v/
s/ASCIIFUNCTIONS/ASCIIFUNCFIONSDEFAULT/

