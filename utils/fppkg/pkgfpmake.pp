unit pkgfpmake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

implementation

uses
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


{ TFPMakeCompiler }

Procedure TFPMakeCompiler.CompileFPMake;
const
  TempBuildDir = 'build-fpmake';
Var
  i : Integer;
  OOptions,
  BaseDir,
  DepDir,
  FPMakeBin,
  FPMakeSrc : string;
  DoBootStrap,
  HaveFpmake : boolean;
begin
  SetCurrentDir(PackageBuildPath);
  { Check for fpmake source }
  FPMakeBin:='fpmake'+ExeExt;
  FPMakeSrc:='fpmake.pp';
  HaveFpmake:=FileExists(FPMakeSrc);
  If Not HaveFPMake then
    begin
      HaveFPMake:=FileExists('fpmake.pas');
      If HaveFPMake then
        FPMakeSrc:='fpmake.pas';
    end;
  { Need to compile fpmake executable? }
  if not FileExists(FPMakeBin) or
     (FileAge(FPMakeBin)<FileAge(FPMakeSrc)) then
    begin
      if Not HaveFPMake then
        Error(SErrMissingFPMake);
      // Special bootstrapping mode to compile fpmake?
      DoBootStrap:=False;
      if Options.BootStrap then
        begin
{$ifdef check_bootstrap_names}
          for i:=low(FPMKUnitDeps) to high(FPMKUnitDeps) do
            if CurrentPackage.Name=FPMKUnitDeps[i] then
              begin
                DoBootStrap:=True;
                break;
              end;
{$else check_bootstrap_names}
          DoBootStrap:=True;
{$endif check_bootstrap_names}
        end;
      // Compile options
      //   -- bootstrapping compile with -g
      //   -- default is to optimize, smartlink and strip to reduce
      //      the executable size (there can be 100's of fpmake's on a system)
      OOptions:='-n';
      if vInfo in Verbosity then
        OOptions:=OOptions+' -vi';
      if DoBootStrap then
        OOptions:=OOptions+' -g'
      else
        OOptions:=OOptions+' -O2 -XXs';
      // Find required units directories
      if DoBootStrap then
        BaseDir:='../'
      else
        BaseDir:=Options.FPMakeUnitDir;
      if not DirectoryExists(BaseDir) then
        Error(SErrMissingDirectory,[BaseDir]);
      for i:=high(FPMKUnitDeps) downto low(FPMKUnitDeps) do
        begin
          // RTL is always take from the installed compiler
          if FPMKUnitDeps[i]='rtl' then
            DepDir:=IncludeTrailingPathDelimiter(Options.FPMakeUnitDir+FPMKUnitDeps[i])
          else
            begin
              if DoBootStrap then
                DepDir:=IncludeTrailingPathDelimiter(BaseDir+FPMKUnitDeps[i]+PathDelim+'src')
              else
                DepDir:=IncludeTrailingPathDelimiter(BaseDir+FPMKUnitDeps[i]);
            end;
          if not DirectoryExists(DepDir) then
            Error(SErrMissingDirectory,[DepDir]);
          OOptions:=OOptions+' -Fu'+DepDir;
        end;
      // Units in a directory for easy cleaning
      DeleteDir(TempBuildDir);
      ForceDirectories(TempBuildDir);
      OOptions:=OOptions+' -FU'+TempBuildDir;
      // Call compiler
      If ExecuteProcess(Options.FPMakeCompiler,OOptions+' '+FPmakeSrc)<>0 then
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
  FPMakeBin : string;
begin
  { Maybe compile fpmake executable? }
  ExecuteAction(CurrentPackage,'compilefpmake');
  { Run FPMake }
  FPMakeBin:='fpmake'+ExeExt;
  SetCurrentDir(PackageBuildPath);
  Result:=ExecuteProcess(FPMakeBin,Command);
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




initialization
  RegisterPkgHandler('compilefpmake',TFPMakeCompiler);
  RegisterPkgHandler('fpmakebuild',TFPMakeRunnerBuild);
  RegisterPkgHandler('fpmakeinstall',TFPMakeRunnerInstall);
  RegisterPkgHandler('fpmakemanifest',TFPMakeRunnerManifest);
end.
