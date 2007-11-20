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
  pkgmessages;

type
  { TFPMakeCompiler }

  TFPMakeCompiler = Class(TPackagehandler)
  Private
    Procedure CompileFPMake;
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TFPMakeRunner }

  TFPMakeRunner = Class(TPackagehandler)
  Protected
    Function RunFPMake(const Command:string):Integer;
  end;


  { TFPMakeRunnerCompile }

  TFPMakeRunnerCompile = Class(TFPMakeRunner)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TFPMakeRunnerBuild }

  TFPMakeRunnerBuild = Class(TFPMakeRunner)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TFPMakeRunnerInstall }

  TFPMakeRunnerInstall = Class(TFPMakeRunner)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TFPMakeRunnerManifest }

  TFPMakeRunnerManifest = Class(TFPMakeRunner)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TFPMakeRunnerArchive }

  TFPMakeRunnerArchive = Class(TFPMakeRunner)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
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

Procedure TFPMakeCompiler.CompileFPMake;

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

const
  TempBuildDir = 'build-fpmake';
Var
  i : Integer;
  OOptions,
  DepDir,
  FPMakeBin,
  FPMakeSrc : string;
  NeedFPMKUnitSource,
  HaveFpmake : boolean;
begin
  SetCurrentDir(PackageBuildPath);
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
      OOptions:='-n';
      for i:=1 to FPMKUnitDepCount do
        begin
          if FPMKUnitDepAvailable[i] then
            begin
              if CheckUnitDir(FPMKUnitDeps[i].package,DepDir) then
                OOptions:=OOptions+' -Fu'+DepDir
              else
                Error(SErrMissingInstallPackage,[FPMKUnitDeps[i].package]);
            end
          else
            begin
              // If fpmkunit is not installed, we use the internal fpmkunit source
              if FPMKUnitDeps[i].package='fpmkunit' then
                begin
                  NeedFPMKUnitSource:=true;
                  OOptions:=OOptions+' -Fu'+TempBuildDir;
                end;
              if FPMKUnitDeps[i].undef<>'' then
                OOptions:=OOptions+' -d'+FPMKUnitDeps[i].undef;
            end;
        end;
      // Add RTL unit dir
      if not CheckUnitDir('rtl',DepDir) then
        Error(SErrMissingInstallPackage,['rtl']);
      OOptions:=OOptions+' -Fu'+DepDir;
      // Units in a directory for easy cleaning
      DeleteDir(TempBuildDir);
      ForceDirectories(TempBuildDir);
      OOptions:=OOptions+' -FU'+TempBuildDir;
      // Compile options
      //   -- default is to optimize, smartlink and strip to reduce
      //      the executable size (there can be 100's of fpmake's on a system)
      if vInfo in Verbosity then
        OOptions:=OOptions+' -vi';
      OOptions:=OOptions+' -O2 -XXs';
      // Create fpmkunit.pp if needed
      if NeedFPMKUnitSource then
        CreateFPMKUnitSource(TempBuildDir+PathDelim+'fpmkunit.pp');
      // Call compiler
      If ExecuteProcess(FPMakeCompilerOptions.Compiler,OOptions+' '+FPmakeSrc)<>0 then
        Error(SErrFailedToCompileFPCMake);
      // Cleanup units
      DeleteDir(TempBuildDir);
    end
  else
    Log(vCommands,SLogNotCompilingFPMake);
end;


function TFPMakeCompiler.Execute(const Args:TActionArgs):boolean;
begin
{$warning TODO Check arguments}
  CompileFPMake;
  result:=true;
end;


{ TFPMakeRunner }

Function TFPMakeRunner.RunFPMake(const Command:string) : Integer;
Var
  FPMakeBin,
  OOptions : string;
begin
  { Maybe compile fpmake executable? }
  ExecuteAction(CurrentPackage,'compilefpmake');
  { Create options }
  OOptions:=' --nofpccfg';
  if vInfo in Verbosity then
    OOptions:=OOptions+' --verbose';
  OOptions:=OOptions+' --compiler='+CompilerOptions.Compiler;
  OOptions:=OOptions+' --CPU='+CPUToString(CompilerOptions.CompilerCPU);
  OOptions:=OOptions+' --OS='+OSToString(CompilerOptions.CompilerOS);
  if IsSuperUser or GlobalOptions.InstallGlobal then
    OOptions:=OOptions+' --baseinstalldir='+CompilerOptions.GlobalInstallDir
  else
    OOptions:=OOptions+' --baseinstalldir='+CompilerOptions.LocalInstallDir;
  if CompilerOptions.LocalInstallDir<>'' then
    OOptions:=OOptions+' --localunitdir='+CompilerOptions.LocalUnitDir;
  OOptions:=OOptions+' --globalunitdir='+CompilerOptions.GlobalUnitDir;
  { Run FPMake }
  FPMakeBin:='fpmake'+ExeExt;
  SetCurrentDir(PackageBuildPath);
  Result:=ExecuteProcess(FPMakeBin,Command+OOptions);

end;


function TFPMakeRunnerCompile.Execute(const Args:TActionArgs):boolean;
begin
  result:=(RunFPMake('compile')=0);
end;


function TFPMakeRunnerBuild.Execute(const Args:TActionArgs):boolean;
begin
  result:=(RunFPMake('build')=0);
end;


function TFPMakeRunnerInstall.Execute(const Args:TActionArgs):boolean;
begin
  result:=(RunFPMake('install')=0);
end;


function TFPMakeRunnerManifest.Execute(const Args:TActionArgs):boolean;
begin
  result:=(RunFPMake('manifest')=0);
end;


function TFPMakeRunnerArchive.Execute(const Args:TActionArgs):boolean;
begin
  result:=(RunFPMake('archive')=0);
end;




initialization
  RegisterPkgHandler('compilefpmake',TFPMakeCompiler);
  RegisterPkgHandler('fpmakecompile',TFPMakeRunnerCompile);
  RegisterPkgHandler('fpmakebuild',TFPMakeRunnerBuild);
  RegisterPkgHandler('fpmakeinstall',TFPMakeRunnerInstall);
  RegisterPkgHandler('fpmakemanifest',TFPMakeRunnerManifest);
  RegisterPkgHandler('fpmakearchive',TFPMakeRunnerArchive);
end.
