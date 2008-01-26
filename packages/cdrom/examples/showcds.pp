program showcds;

{$mode objfpc}
{$h+}

uses cdrom,sysutils;

Var
  Drives : Array[1..10] of String;
  I,Count : Integer;

begin
  Try
    Count:=GetCDRomDevices(Drives);
    Writeln('This PC has ',count,' CD-ROM drives');
    For I:=1 to count do
      Writeln('Drive ',i,' on device: ',Drives[i]);
  Except
    On E : exception do
      Writeln(E.ClassName,' exception caught with message: ',E.Message);
  end;
end.
