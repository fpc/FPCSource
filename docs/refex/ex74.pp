Program Example74;

{ Program to demonstrate the Val function. }
Var I, Code : Integer;

begin
  Val (ParamStr (1),I,Code);
  If Code<>0 then
    Writeln ('Error at position ',code,' : ',Paramstr(1)[Code])
  else
    Writeln ('Value : ',I);
end.
