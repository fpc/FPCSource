Program Example7;

{ This program demonstrates the IsValidDateTime function }

Uses SysUtils,DateUtils;

Var
  Y,Mo,D : Word;
  H,M,S,MS : Word;
  I : Integer;

Begin
  For I:=1 to 10 do
    begin
    Y:=2000+Random(5);
    Mo:=Random(15);
    D:=Random(40);
    H:=Random(32);
    M:=Random(90);
    S:=Random(90);
    MS:=Random(1500);
    If Not IsValidDateTime(Y,Mo,D,H,M,S,MS) then
      Writeln(Y,'-',Mo,'-',D,' ',H,':',M,':',S,'.',MS,' is not a valid date/time.');
    end;
End.