Program Example48;

{ Program to demonstrate the Pos function. }

Var
  S : String;

begin
  S:='The first space in this sentence is at position : ';
  Writeln (S,pos(' ',S));
  S:='The last letter of the alphabet doesn''t appear in this sentence ';
  If (Pos ('Z',S)=0) and (Pos('z',S)=0) then
    Writeln (S);
end.
