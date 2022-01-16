{$mode objfpc}
uses
  sysutils;

procedure testff(const s: string);
var
  sr: tsearchrec;
  i : integer;
begin
  i:=0;
  if findfirst(s,faAnyFile,sr)=0 then
   repeat
     writeln(sr.name);
     inc(i);
   until findnext(sr)<>0;
  findclose(sr);
  if (i=0) then
   halt(1);
end;

begin
{$ifdef UNIX}
  {$ifdef haiku}
    testff('/boot/system/lib/libroot.*');
  {$else}
    testff('/etc/host*');
  {$endif}
{$else}
  {$ifdef wince}
    testff('\windows\calc.*');
  {$else}
    testff(ChangeFileExt(GetEnvironmentVariable('comspec'), '.*'));
  {$endif wince}
{$endif}
end.
