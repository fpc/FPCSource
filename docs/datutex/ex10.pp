Program Example10;

{ This program demonstrates the IsValidDateWeek function }

Uses SysUtils,DateUtils;

Var
  Y,W,D : Word;
  B : Boolean;

Begin
  For Y:=2000 to 2004 do
    begin
    B:=True;
    For W:=51 to 54 do
      For D:=1 to 7 do
        If B then
          begin
          B:=IsValidDateWeek(Y,W,D);
          If Not B then
            if (D=1) then
              Writeln(Y,' has exactly ',W,' weeks.')
            else
              Writeln(Y,' has ',W,' weeks and ',D-1,' days.');
          end;
    end;
End.