Program Example86;

{ Program to demonstrate the Continue function. }

Var I : longint;

begin
  I:=0;
  While I<10 Do
    begin
    Inc(I);
    If I<5 Then
      Continue;
    Writeln (i);
    end;
  I:=0;
  Repeat
    Inc(I);
    If I<5 Then
      Continue;
    Writeln (i);
  Until I>=10;
  For I:=1 to 10 do
    begin
    If I<5 Then
      Continue;
    Writeln (i);
    end;
end.
