# function names with parameters
s/function \([^(]*\)A *(/function \1(/
# procedure names with parameters
s/procedure \([^(]*\)A *(/procedure \1(/
# function names without parameters
s/function \([^:(]*\)A *: */function \1 : /
# procedure names without parameters
s/procedure \([^;(]*\)A *;/procedure \1;/
# function return value
s/\([^ \t]*\)A *:=/\1:=/
# function call with parameters
s/\:=\(.*\)A(/:=\1(/
# function call without parameters
s/\:=\(.*\)A *;/:=\1;/
# unit name
s/ascfun;/ascdef;/
# cvs name
s/ascfun.inc,v/ascdef.inc,v/
# unit conditionnal
s/ASCIIFUNCTIONS/ASCIIFUNCTIONSDEFAULT/

