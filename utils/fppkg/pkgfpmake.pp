unit pkgfpmake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

implementation

uses
  fprepos,
  pkgoptions,
  pkgglobals,
  pkgmessages,
  pkgrepos;

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

const
  TempBuildDir = 'build-fpmake';
Var
  i : Integer;
  DepDir,
  FPMakeBin,
  FPMakeSrc : string;
  NeedFPMKUnitSource,
  HaveFpmake : boolean;
  P : TFPPackage;
begin
  P:=InstalledRepository.PackageByName(PackageName);
  OOptions:='';
  SetCurrentDir(PackageBuildPath(P));
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
      for i:=1 to FPMKUnitDepCount do
        begin
          if FPMKUnitDepAvailable[i] then
            begin
              if CheckUnitDir(FPMKUnitDeps[i].package,DepDir) then
                AddOption(maybequoted('-Fu'+DepDir))
              else
                Error(SErrMissingInstallPackage,[FPMKUnitDeps[i].package]);
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
      if vlInfo in LogLevels then
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
    Log(vlCommands,SLogNotCompilingFPMake);
end;


{ TFPMakeRunner }

Function TFPMakeRunner.RunFPMake(const Command:string) : Integer;
Var
  P : TFPPackage;
  FPMakeBin,
  OOptions : string;

  procedure AddOption(const s:string);
  begin
    if OOptions<>'' then
      OOptions:=OOptions+' ';
    OOptions:=OOptions+maybequoted(s);
  end;

begin
  OOptions:='';
  // Does the current package support this CPU-OS?
  if PackageName<>'' then
    P:=InstalledRepository.PackageByName(PackageName)
  else
    P:=nil;
  if assigned(P) then
    begin
      if not(CompilerOptions.CompilerOS in P.OSes) or
         not(CompilerOptions.CompilerCPU in P.CPUs) then
        Error(SErrPackageDoesNotSupportTarget,[P.Name,MakeTargetString(CompilerOptions.CompilerCPU,CompilerOptions.CompilerOS)]);
    end;
  { Maybe compile fpmake executable? }
  ExecuteAction(PackageName,'compilefpmake');
  { Create options }
  AddOption('--nofpccfg');
  if vlInfo in LogLevels then
    AddOption('--verbose');
  AddOption('--compiler='+CompilerOptions.Compiler);
  AddOption('--cpu='+CPUToString(CompilerOptions.CompilerCPU));
  AddOption('--os='+OSToString(CompilerOptions.CompilerOS));
  if IsSuperUser or GlobalOptions.InstallGlobal then
    AddOption('--baseinstalldir='+CompilerOptions.GlobalInstallDir)
  else
    AddOption('--baseinstalldir='+CompilerOptions.LocalInstallDir);
  if CompilerOptions.LocalInstallDir<>'' then
    AddOption('--localunitdir='+CompilerOptions.LocalUnitDir);
  AddOption('--globalunitdir='+CompilerOptions.GlobalUnitDir);
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
  RegisterPkgHandler('fpmakeclean',TFPMakeRunnerClean);
  RegisterPkgHandler('fpmakemanifest',TFPMakeRunnerManifest);
  RegisterPkgHandler('fpmakearchive',TFPMakeRunnerArchive);
end.
