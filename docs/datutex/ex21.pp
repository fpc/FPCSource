Program Example21;

{ This program demonstrates the IsSameDay function }

Uses SysUtils,DateUtils;

Var
  I : Integer;
  D : TDateTime;

Begin
  For I:=1 to 3 do
    begin
    D:=Today+Random(3)-1;
    Write(FormatDateTime('dd mmmm yyyy "is today : "',D));
    Writeln(IsSameDay(D,Today));
    end;
End.