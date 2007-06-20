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
  testff('/etc/host*');
{$else}
  {$ifdef wince}
    testff('\windows\calc.*');
  {$else}
    testff(ChangeFileExt(GetEnvironmentVariable('comspec'), '.*'));
  {$endif wince}
{$endif}
end.
