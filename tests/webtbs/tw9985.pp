{ %opt=-gh }

{$mode objfpc}

uses SysUtils;

var
  DiskNum: Byte;
begin
  HaltOnNotReleased := true;
  Writeln(DiskFree(3), '/', DiskSize(3));

  { Now get disk / by AddDisk. DiskFree and DiskSize below should return
    the same (well, assuming that nothing was writeen to disk between
    calls...). }
{$ifdef unix}
  DiskNum := AddDisk('/');
{$else}
  { dos/windows/os/2 ... Still needs other cases for other OSes }
  DiskNum := 3;
{$endif}
  Writeln(DiskFree(DiskNum), '/', DiskSize(DiskNum));
  if (disksize(3)<>disksize(disknum)) then
    halt(1);
end.
