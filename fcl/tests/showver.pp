program showversion;

{$mode fpc}
{$R showver.res}

uses sysutils,fileinfo;

Var version : TFileVersionInfo;
    I : longint;

begin
  if Paramcount<1 then
    begin
      Writeln('Usage: showver <exefile>');
      halt(1);
    end;
  Version:=TFileVErsionInfo.create(Nil);
  Version.FileName:=paramstr(1);
  With Version do
    begin
    if Not FileExists(Paramstr(1)) then
      begin
      Writeln (Format('%s : No such file or directory',[Paramstr(1)]));
      Halt(1);
      end;
    FileName:=Paramstr(1);
    If VersionStrings.Count=0 then
      begin
      Writeln (Format('%s : No version information found.',[paramstr(1)]));
      Halt(2);
      end;
    For i:=0 to VersionStrings.Count-1 do
      Writeln (VersionCategories[I],'=',VersionStrings[i]);
    free;
    end;
end.
