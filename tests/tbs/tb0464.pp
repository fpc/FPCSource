{ %version=1.1 }
{$mode delphi}

var
  a1 : Array of Byte;

begin
  SetLength(a1,2);
  a1[0]:=65;
  a1[1]:=66;
  WriteLn(AnsiString(a1));
end.
