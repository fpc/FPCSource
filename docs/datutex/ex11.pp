Program Example11;

{ This program demonstrates the IsValidDateMonthWeek function }

Uses SysUtils,DateUtils;

Var
  Y,W,D : Word;
  B : Boolean;

Begin
  For Y:=2000 to 2004 do
    begin
    B:=True;
    For W:=4 to 6 do
      For D:=1 to 7 do
        If B then
          begin
          B:=IsValidDateMonthWeek(Y,12,W,D);
          If Not B then
            if (D=1) then
              Writeln('December ',Y,' has exactly ',W,' weeks.')
            else
              Writeln('December ',Y,' has ',W,' weeks and ',D-1,' days.');
          end;
    end;
End.