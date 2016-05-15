unit pkgfpmake;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,DateUtils,
  pkghandler, fpmkunit;

implementation

uses
  fprepos,
  pkgoptions,
  pkgglobals,
  pkgmessages,
  pkgrepos,
  fpxmlrep;

type
  { TFPMakeCompiler }

  TFPMakeCompiler = Class(TPackagehandler)
  Public
    Procedure Execute;override;
  end;


  { TFPMakeRunner }

  TFPMakeRunner = Class(TPackagehandler)
  Protected
    Function RunFPMake(const Command:string):Integer;
  end;


  { TFPMakeRunnerCompile }

  TFPMakeRunnerCompile = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;


  { TFPMakeRunnerBuild }

  TFPMakeRunnerBuild = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;


  { TFPMakeRunnerInstall }

  TFPMakeRunnerInstall = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerUnInstall }

  TFPMakeRunnerUnInstall = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerClean }

  TFPMakeRunnerClean = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerManifest }

  TFPMakeRunnerManifest = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

  { TFPMakeRunnerArchive }

  TFPMakeRunnerArchive = Class(TFPMakeRunner)
  Public
    Procedure Execute;override;
  end;

   TMyMemoryStream=class(TMemoryStream)
   public
     constructor Create(p:pointer;mysize:integer);
   end;

{
  Generated from fpmkunit.pp, using data2inc:
  data2inc -b -s fpmkunit.pp fpmkunitsrc.inc fpmkunitsrc
}
{$i fpmkunitsrc.inc}

procedure CreateFPMKUnitSource(const AFileName:string);
var
  InStream,
  OutStream : TStream;
  pend      : pchar;
begin
  try
    // Don't write trailing #0
    pend:=pchar(@fpmkunitsrc)+sizeof(fpmkunitsrc)-1;
    while pend^=#0 do
      dec(pend);
    InStream:=TMyMemoryStream.Create(@fpmkunitsrc,pend-pchar(@fpmkunitsrc));
    OutStream:=TFileStream.Create(AFileName,fmCreate);
    OutStream.CopyFrom(InStream,InStream.Size);
  finally
    InStream.Destroy;
    OutStream.Destroy;
  end;
end;


{*****************************************************************************
                               TMyMemoryStream
*****************************************************************************}

    constructor TMyMemoryStream.Create(p:pointer;mysize:integer);
      begin
        inherited Create;
        SetPointer(p,mysize);
      end;


{ TFPMakeCompiler }

Procedure TFPMakeCompiler.Execute;
var
  OOptions : string;

  function CheckUnitDir(const AUnitName:string;Out AUnitDir:string):boolean;
  begin
    Result:=false;
    if FPMakeCompilerOptions.LocalUnitDir<>'' then
      begin
        AUnitDir:=IncludeTrailingPathDelimiter(FPMakeCompilerOptions.LocalUnitDir+AUnitName);
        if DirectoryExistsLog(AUnitDir) then
          begin
            Result:=true;
            exit;
          end;
      end;
    AUnitDir:=IncludeTrailingPathDelimiter(FPMakeCompilerOptions.GlobalUnitDir+AUnitName);
    if DirectoryExistsLog(AUnitDir) then
      begin
        Result:=true;
        exit;
      end;
    AUnitDir:='';
  end;

  procedure AddOption(const s:string);
  begin
    if OOptions<>'' then
      OOptions:=OOptions+' ';
    OOptions:=OOptions+maybequoted(s);
  end;

Var
  i : Integer;
  TempBuildDir,
  DepDir,
  FPMakeBin,
  FPMakeSrc : string;
  NeedFPMKUnitSource,
  HaveFpmake : boolean;
  P : TFPPackage;
begin
  P:=AvailableRepository.PackageByName(PackageName);
  NeedFPMKUnitSource:=false;
  OOptions:='';
  SetCurrentDir(PackageBuildPath(P));
  // Generate random name for build path
  TempBuildDir:='build_fpmake_'+HexStr(DateTimeToUnix(Now),8)+HexStr(GetProcessId,4);
  // Check for fpmake source
  FPMakeBin:='fpmake'+ExeExt;
  FPMakeSrc:='fpmake.pp';
  HaveFpmake:=FileExists(FPMakeSrc);
  If Not HaveFPMake then
    begin
      HaveFPMake:=FileExists('fpmake.pas');
      If HaveFPMake then
        FPMakeSrc:='fpmake.pas';
    end;
  // Need to compile fpmake executable?
  if not FileExists(FPMakeBin) or
     (FileAge(FPMakeBin)<FileAge(FPMakeSrc)) then
    begin
      if Not HaveFPMake then
        Error(SErrMissingFPMake);
      AddOption('-n');
      AddOption('-dCOMPILED_BY_FPPKG');
      for i:=0 to high(FPMKUnitDeps) do
        begin
          if FPMKUnitDeps[i].available then
            begin
              if CheckUnitDir(FPMKUnitDeps[i].package,DepDir) then
                AddOption('-Fu'+DepDir)
              else
                Error(SErrMissingInstallPackage,[FPMKUnitDeps[i].package]);
              if FPMKUnitDeps[i].def<>'' then
                AddOption('-d'+FPMKUnitDeps[i].def);
            end
          else
            begin
              // If fpmkunit is not installed, we use the internal fpmkunit source
              if FPMKUnitDeps[i].package='fpmkunit' then
                begin
                  NeedFPMKUnitSource:=true;
                  AddOption('-Fu'+TempBuildDir);
                end;
              if FPMKUnitDeps[i].undef<>'' then
                AddOption('-d'+FPMKUnitDeps[i].undef);
            end;
        end;
      // Add RTL unit dir
      if not CheckUnitDir('rtl',DepDir) then
        Error(SErrMissingInstallPackage,['rtl']);
      AddOption('-Fu'+DepDir);
      // Units in a directory for easy cleaning
      DeleteDir(TempBuildDir);
      ForceDirectories(TempBuildDir);
      AddOption('-FU'+TempBuildDir);
      // Compile options
      //   -- default is to optimize, smartlink and strip to reduce
      //      the executable size (there can be 100's of fpmake's on a system)
      if llInfo in LogLevels then
        AddOption('-vi');
      AddOption('-O2');
      AddOption('-XXs');
      // Create fpmkunit.pp if needed
      if NeedFPMKUnitSource then
        CreateFPMKUnitSource(TempBuildDir+PathDelim+'fpmkunit.pp');
      // Call compiler
      If ExecuteProcess(FPMakeCompilerOptions.Compiler,OOptions+' '+FPmakeSrc)<>0 then
        begin
          if not GlobalOptions.RecoveryMode then
            Error(SErrCompileFailureFPMakeTryRecovery)
          else
            Error(SErrCompileFailureFPMake);
        end;
      // Cleanup units
      DeleteDir(TempBuildDir);
    end
  else
    Log(llCommands,SLogNotCompilingFPMake);
end;


{ TFPMakeRunner }

Function TFPMakeRunner.RunFPMake(const Command:string) : Integer;
Var
  ManifestPackage,
  P : TFPPackage;
  FPMakeBin,
  OOptions : string;

  procedure AddOption(const s:string);
  begin
    if OOptions<>'' then
      OOptions:=OOptions+' ';
    OOptions:=OOptions+maybequoted(s);
  end;

  procedure CondAddOption(const Name,Value:string);
  begin
    if Value<>'' then
      AddOption(Name+'='+Value);
  end;

  procedure ObtainSupportedTargetsFromManifest(p:TFPPackage);
  var
    X : TFPXMLRepositoryHandler;
    ManifestPackages : TFPPackages;
    i: integer;
  begin
    p.OSes:=[];
    p.CPUs:=[];
    ManifestPackages:=TFPPackages.Create(TFPPackage);
    X:=TFPXMLRepositoryHandler.Create;
    try
      X.LoadFromXml(ManifestPackages,ManifestFileName);
      for i := 0 to ManifestPackages.Count-1 do
        begin
          p.OSes:=p.OSes+ManifestPackages[i].OSes;
          p.CPUs:=p.CPUs+ManifestPackages[i].CPUs;
        end;
    finally
      X.Free;
      ManifestPackages.Free;
    end;
  end;

begin
  OOptions:='';
  // Does the current package support this CPU-OS?
  if PackageName<>'' then
    begin
      P:=AvailableRepository.PackageByName(PackageName);
      if (PackageName=CurrentDirPackageName) and (FileExists(ManifestFileName)) then
        ObtainSupportedTargetsFromManifest(p);
    end
  else
    P:=nil;
  if assigned(P) then
    begin
      if (command<>'archive') and (command<>'manifest') and
         (not(CompilerOptions.CompilerOS in P.OSes) or
          not(CompilerOptions.CompilerCPU in P.CPUs)) then
        Error(SErrPackageDoesNotSupportTarget,[P.Name,MakeTargetString(CompilerOptions.CompilerCPU,CompilerOptions.CompilerOS)]);
    end;
  { Maybe compile fpmake executable? }
  ExecuteAction(PackageName,'compilefpmake');
  { Create options }
  if llDebug in LogLevels then
    AddOption('--debug')
  else if llInfo in LogLevels then
    AddOption('--verbose');
  if P.RecompileBroken and
     (P.FPMakeOptionsString<>'') then // Check for a empty FPMakeOptionString for packages being installed with an old fpmkunit
    begin
      // When the package is being reinstalled because of broken dependencies, use the same fpmake-options
      // as were used to compile the package in the first place.
      OOptions:=P.FPMakeOptionsString;
    end
  else
    begin
      if CompilerOptions.HasOptions then
        AddOption('--options='+CompilerOptions.Options.DelimitedText);

      if GlobalOptions.CustomFPMakeOptions<>'' then
        begin
        AddOption('--ignoreinvalidoption');
        AddOption(GlobalOptions.CustomFPMakeOptions);
        end;
    end;

  AddOption('--nofpccfg');
  AddOption('--compiler='+CompilerOptions.Compiler);
  AddOption('--cpu='+CPUToString(CompilerOptions.CompilerCPU));
  AddOption('--os='+OSToString(CompilerOptions.CompilerOS));
  if IsSuperUser or GlobalOptions.InstallGlobal then
    begin
      CondAddOption('--prefix',CompilerOptions.GlobalPrefix);
      CondAddOption('--baseinstalldir',CompilerOptions.GlobalInstallDir);
    end
  else
    begin
      CondAddOption('--prefix',CompilerOptions.LocalPrefix);
      CondAddOption('--baseinstalldir',CompilerOptions.LocalInstallDir);
    end;
  CondAddOption('--localunitdir',CompilerOptions.LocalInstallDir);
  CondAddOption('--globalunitdir',CompilerOptions.GlobalInstallDir);

  { Run FPMake }
  FPMakeBin:='fpmake'+ExeExt;
  SetCurrentDir(PackageBuildPath(P));
  Result:=ExecuteProcess(FPMakeBin,Command+' '+OOptions);
  if Result<>0 then
    Error(SErrExecutionFPMake,[Command]);
end;


procedure TFPMakeRunnerCompile.Execute;
begin
  RunFPMake('compile');
end;


procedure TFPMakeRunnerBuild.Execute;
begin
  RunFPMake('build');
end;


procedure TFPMakeRunnerInstall.Execute;
begin
  RunFPMake('install');
end;


procedure TFPMakeRunnerUnInstall.Execute;
begin
  RunFPMake('uninstall');
end;


procedure TFPMakeRunnerClean.Execute;
begin
  RunFPMake('clean');
end;


procedure TFPMakeRunnerManifest.Execute;
begin
  RunFPMake('manifest');
end;


procedure TFPMakeRunnerArchive.Execute;
begin
  RunFPMake('archive');
end;


initialization
  RegisterPkgHandler('compilefpmake',TFPMakeCompiler);
  RegisterPkgHandler('fpmakecompile',TFPMakeRunnerCompile);
  RegisterPkgHandler('fpmakebuild',TFPMakeRunnerBuild);
  RegisterPkgHandler('fpmakeinstall',TFPMakeRunnerInstall);
  RegisterPkgHandler('fpmakeuninstall',TFPMakeRunnerUnInstall);
  RegisterPkgHandler('fpmakeclean',TFPMakeRunnerClean);
  RegisterPkgHandler('fpmakemanifest',TFPMakeRunnerManifest);
  RegisterPkgHandler('fpmakearchive',TFPMakeRunnerArchive);
end.
