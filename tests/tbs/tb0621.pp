PROGRAM compbug300;

VAR x1, x2 : comp;

(* Dividing 8 / 2 doesn't work with fpc 3.0.0
   but works for example with fpc 2.6.4
   Markus Greim / 29.jun.2016 *)

BEGIN

x1 := 8;
writeln('x1 : ',x1);
x2 := x1 / 2;
writeln('x2 = x1/2 should be 4 but is : ', x2);
if x2<>4 then
  halt(1);
x2 := x1 / 4;
writeln('x2 = x1/4 should be 2 but is : ', x2);
if x2<>2 then
  halt(2);
x2 := x1 / 8.0;
writeln('x2 = x1/8.0 should be 1 and is : ', x2);
if x2<>1 then
  halt(3);


END. 
