Program Example14;

{ Program to demonstrate the frexp function. }

Uses math;

Procedure dofrexp(Const X : extended);

var man : extended;
    exp: longint;

begin
  man:=0;
  exp:=0;
  frexp(x,man,exp);
  write(x,' has ');
  Writeln('mantissa ',man,' and exponent ',exp);
end;


begin
//   dofrexp(1.00);
   dofrexp(1.02e-1);
   dofrexp(1.03e-2);
   dofrexp(1.02e1);
   dofrexp(1.03e2);
end.
