Program Example6;

{ This program demonstrates the IsValidTime function }

Uses SysUtils,DateUtils;

Var
  H,M,S,MS : Word;
  I : Integer;

Begin
  For I:=1 to 10 do
    begin
    H:=Random(48);
    M:=Random(120);
    S:=Random(120);
    MS:=Random(2000);
    If Not IsValidTime(H,M,S,MS) then
      Writeln(H,':',M,':',S,'.',MS,' is not a valid time.');
    end;
End.