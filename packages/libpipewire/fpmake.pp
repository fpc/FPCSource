{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} sysutils, classes, fpmkunit;
{$ENDIF ALLPACKAGES}

Type
  THackPackage = class(TPackage)
    property Dictionary;
  end;

procedure BeforeInstall_LibPipeWire(Sender: TObject);

Var
  P : TPackage;
  lInstDir,lDest : string;
begin
  P:=Sender as TPackage;
  lDest:=P.GetUnitsOutputDir(Defaults.BuildTarget);
  if P.Directory<>'' then
    lDest:=IncludeTrailingPathDelimiter(P.Directory)+lDest;
  lDest:=IncludeTrailingPathDelimiter(lDest)+'spabridge.o';
  if FileExists(lDest) then
    begin
    // no prefix this time
    lDest:=P.GetUnitsOutputDir(Defaults.BuildTarget);
    lDest:=IncludeTrailingPathDelimiter(lDest)+'spabridge.o';
    // not clear why packagename does not exist at this point?
    THackPackage(P).Dictionary.AddVariable('packagename',P.Name);
    Writeln('Defaults ',Defaults.UnitInstallDir);
    lInstDir:=THackPackage(P).Dictionary.ReplaceStrings(Defaults.UnitInstallDir);
    P.InstallFiles.Add(lDest,lInstDir);
    end;
end;

procedure BeforeCompile_LibPipeWire(Sender: TObject);

var
  lGCC : string;
  lArgs : TStrings;
  lDest,lSrc : String;
  P : TPackage;

begin
  P:=Sender as TPackage;
  lDest:=P.GetUnitsOutputDir(Defaults.BuildTarget);
  if P.Directory<>'' then
    begin
    lSrc:=IncludeTrailingPathDelimiter(P.Directory);
    lDest:=IncludeTrailingPathDelimiter(P.Directory)+lDest;
    end
  else
    lSrc:=IncludeTrailingPathDelimiter(Installer.BuildEngine.StartDir);
  lSrc:=IncludeTrailingPathDelimiter(LSrc+'src')+'spabridge.c';
  lDest:=IncludeTrailingPathDelimiter(lDest)+'spabridge.o';
  lgcc:=ExeSearch('gcc',GetEnvironmentVariable('PATH'));
  lArgs:=TStringList.Create();
  try
    lArgs.Add('-c');
    lArgs.Add('-I/usr/include/spa-0.2/');
    lArgs.Add(lSrc);
    lArgs.Add('-o'+lDest);
    try
      Installer.BuildEngine.ExecuteCommand(lgcc,lArgs);
    except
      On E : Exception do
        Installer.BuildEngine.Log(vlWarning,'Could not compile spabridge.c, it will need to be compiled manually. install libpipewire-0.3-dev package');
    end;
  finally
    lArgs.Free;
  end;
end;

procedure add_libpipewire(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;
begin
  With Installer do
    begin
    P:=AddPackage('libpipewire');
    P.ShortName:='libpw';
    P.Directory:=ADirectory;
    P.Version:='3.3.1';
    P.SourcePath.Add('src');
    P.IncludePath.Add('src');
    { only enable for darwin after testing }
    P.OSes := [linux];
    if Defaults.CPU=jvm then
      P.OSes := [];
    P.SupportBuildModes:= [bmOneByOne];
    P.BeforeCompileProc:=@BeforeCompile_LibPipeWire;
    P.BeforeInstallProc:=@BeforeInstall_LibPipeWire;

    T:=P.Targets.AddUnit('libspa.pp');
    With T.Dependencies do
      begin
      AddInclude('spabridge.inc');
      end;
    T:=P.Targets.AddUnit('libpipewire.pp');
    with T.Dependencies do
      begin
      AddUnit('libspa');
      AddInclude('array.inc');
      AddInclude('permission.inc');
      AddInclude('client.inc');
      AddInclude('loop.inc');
      AddInclude('properties.inc');
      AddInclude('work_queue.inc');
      AddInclude('context.inc');
      AddInclude('device.inc');
      AddInclude('mem.inc');
      AddInclude('buffers.inc');
      AddInclude('core.inc');
      AddInclude('factory.inc');
      AddInclude('keys.inc');
      AddInclude('log.inc');
      AddInclude('link.inc');
      AddInclude('main_loop.inc');
      AddInclude('map.inc');
      AddInclude('module.inc');
      AddInclude('node.inc');
      AddInclude('protocol.inc');
      AddInclude('proxy.inc');
      AddInclude('port.inc');
      AddInclude('stream.inc');
      AddInclude('filter.inc');
      AddInclude('data_loop.inc');
      AddInclude('type.inc');
      AddInclude('utils.inc');
      AddInclude('version.inc');
      AddInclude('thread_loop.inc');
      AddInclude('core_impl.inc');
      AddInclude('loop_impl.inc');
      end;
    P.NamespaceMap:='namespaces.lst';
    end;
end;


{$ifndef ALLPACKAGES}
begin
  add_libpipewire('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
