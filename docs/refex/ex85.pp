Program Example85;

{ Program to demonstrate the SetLength function. }

Var S : String;

begin
  FillChar(S[1],100,#32);
  Setlength(S,100);
  Writeln ('"',S,'"');
end.
