Program Example5;

Uses strings;

{ Program to demonstrate the StrLCopy function. }

Const P : PCHar = '123456789ABCDEF';

var PP : PCHar;

begin
  PP:=StrAlloc(11);
  Writeln ('First 10 characters of P : ',StrLCopy (PP,P,10));
end.
