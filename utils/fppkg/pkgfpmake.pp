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
const
  TempBuildDir = 'build-fpmake';
Var
  OOptions,
  DepDir,
  DepDir2,
  FPMakeBin,
  FPMakeSrc : string;
  NeedFPMKUnitSource,
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
      NeedFPMKUnitSource:=False;
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
      // Check overall unit dir, this must exist at least for RTL
      if not DirectoryExists(Options.FPMakeUnitDir) then
        Error(SErrMissingDirectory,[Options.FPMakeUnitDir]);
      // Add FPMKUnit unit dir, if not found we use the internal fpmkunit source
      DepDir:=IncludeTrailingPathDelimiter(Options.FPMakeUnitDir+'fpmkunit');
      if DirectoryExists(DepDir) then
        OOptions:=OOptions+' -Fu'+DepDir
      else
        begin
          if DoBootStrap then
            begin
              NeedFPMKUnitSource:=true;
              OOptions:=OOptions+' -Fu'+TempBuildDir;
            end
          else
            Error(SErrMissingDirectory,[DepDir]);
        end;
      // Add PaszLib and Hash units dir
      DepDir:=IncludeTrailingPathDelimiter(Options.FPMakeUnitDir+'hash');
      if not DirectoryExists(DepDir) then
        begin
          if DoBootStrap then
            DepDir:=''
          else
            Error(SErrMissingDirectory,[DepDir]);
        end;
      DepDir2:=IncludeTrailingPathDelimiter(Options.FPMakeUnitDir+'paszlib');
      if not FileExists(DepDir2+'zipper.ppu') then
        begin
          if DoBootStrap then
            DepDir2:=''
          else
            Error(SErrMissingDirectory,[DepDir2]);
        end;
      if (DepDir<>'') and (DepDir2<>'') then
        OOptions:=OOptions+' -Fu'+DepDir+' -Fu'+DepDir2
      else
        OOptions:=OOptions+' -dNO_UNIT_ZIPPER';
      // Add Process unit
      DepDir:=IncludeTrailingPathDelimiter(Options.FPMakeUnitDir+'fcl-process');
      if DirectoryExists(DepDir) then
        OOptions:=OOptions+' -Fu'+DepDir
      else
        begin
          if DoBootStrap then
            OOptions:=OOptions+' -dNO_UNIT_PROCESS'
          else
            Error(SErrMissingDirectory,[DepDir]);
        end;
      // Add RTL unit dir
      DepDir:=IncludeTrailingPathDelimiter(Options.FPMakeUnitDir+'rtl');
      if not DirectoryExists(DepDir) then
        Error(SErrMissingDirectory,[DepDir]);
      OOptions:=OOptions+' -Fu'+DepDir;
      // Units in a directory for easy cleaning
      DeleteDir(TempBuildDir);
      ForceDirectories(TempBuildDir);
      OOptions:=OOptions+' -FU'+TempBuildDir;
      // Create fpmkunit.pp if needed
      if NeedFPMKUnitSource then
        CreateFPMKUnitSource(TempBuildDir+PathDelim+'fpmkunit.pp');
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
