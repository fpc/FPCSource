unit pkgfpmake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

type
  { TFPMakeCompiler }

  TFPMakeCompiler = Class(TPackagehandler)
  Private
    Procedure CompileFPMake(const ADir:string);
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TFPMakeRunner }

  TFPMakeRunner = Class(TPackagehandler)
  Private
    Function RunFPMake(const ADir:string) : Integer;
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TFPMakeRunner }

  TCommandBuild = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


implementation

uses
  pkgmessages;
  
{ TFPMakeCompiler }

Procedure TFPMakeCompiler.CompileFPMake(const ADir:string);
Var
  D,O,C : String;
  FPMakeBin,
  FPMakeSrc : string;
  HaveFpmake : boolean;
begin
  D:=IncludeTrailingPathDelimiter(ADir);
  { Check for fpmake source }
  FPMakeBin:=D+'fpmake'+ExeExt;
  FPMakeSrc:=D+'fpmake.pp';
  HaveFpmake:=FileExists(FPMakeSrc);
  If Not HaveFPMake then
    begin
      HaveFPMake:=FileExists(D+'fpmake.pas');
      If HaveFPMake then
        FPMakeSrc:=D+'fpmake.pas';
    end;
  { Need to compile fpmake executable? }
  if FileAge(FPMakeBin)<FileAge(FPMakeSrc) then
    begin
      if Not HaveFPMake then
        Error(SErrMissingFPMake);
      { Call compiler }
      C:=Defaults.Compiler;
      O:=FPmakeSrc;
      Log(vCommands,SLogCompilingFPMake+C+' '+O);
      If ExecuteProcess(C,O)<>0 then
        Error(SErrFailedToCompileFPCMake)
    end
  else
    Log(vCommands,SLogNotCompilingFPMake);
end;


function TFPMakeCompiler.Execute(const Args:TActionArgs):boolean;
begin
{$warning TODO Check arguments}
  CompileFPMake(Args[0]);
  result:=true;
end;


{ TFPMakeRunner }

Function TFPMakeRunner.RunFPMake(const ADir:string) : Integer;

  Function MaybeQuote(Const S : String) : String;
  begin
    If Pos(' ',S)=0 then
      Result:=S
    else
      Result:='"'+S+'"';
  end;

Var
  I : integer;
  FPMakeBin,
  D,O : String;
begin
  D:=IncludeTrailingPathDelimiter(ADir);
  FPMakeBin:=D+'fpmake'+ExeExt;
  O:='';
  For I:=1 to ParamCount do
    begin
      If (O<>'') then
        O:=O+' ';
      O:=O+MaybeQuote(ParamStr(I));
    end;
  Log(vCommands,SLogRunningFPMake+FPMakeBin+' '+O);
  Result:=ExecuteProcess(FPMakeBin,O);
end;


function TFPMakeRunner.Execute(const Args:TActionArgs):boolean;
begin
{$warning TODO Check arguments}
  result:=(RunFPMake(Args[0])=0);
end;



function TCommandBuild.Execute(const Args:TActionArgs):boolean;
begin
  ActionStack.Push('fpmakebuild',[]);
  ActionStack.Push('compilefpmake',[]);
end;


initialization
  RegisterPkgHandler('compilefpmake',TFPMakeCompiler);
  RegisterPkgHandler('fpmakebuild',TFPMakeRunner);
  RegisterPkgHandler('build',TCommandBuild);
end.
