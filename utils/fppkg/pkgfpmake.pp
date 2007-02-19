unit pkgfpmake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

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


implementation

uses
  pkgoptions,
  pkgglobals,
  pkgmessages;

{ TFPMakeCompiler }

Procedure TFPMakeCompiler.CompileFPMake;
Var
  O,C : String;
  FPMakeBin,
  FPMakeSrc : string;
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
      { Call compiler }
      C:=Defaults.FPMakeCompiler;
      O:='-vi -n -Fu'+Defaults.FPMakeUnitDir+' -Fu'+Defaults.FPMakeUnitDir+'..'+PathDelim+'rtl'+PathDelim+' '+FPmakeSrc;
      If ExecuteProcess(C,O)<>0 then
        Error(SErrFailedToCompileFPCMake)
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
  I : integer;
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



initialization
  RegisterPkgHandler('compilefpmake',TFPMakeCompiler);
  RegisterPkgHandler('fpmakebuild',TFPMakeRunnerBuild);
  RegisterPkgHandler('fpmakeinstall',TFPMakeRunnerInstall);
end.
