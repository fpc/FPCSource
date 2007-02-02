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
    Function Execute(const Args:array of string):boolean;override;
  end;


  { TFPMakeRunner }

  TFPMakeRunner = Class(TPackagehandler)
  Private
    Function RunFPMake : Integer;
  Public
    Function Execute(const Args:array of string):boolean;override;
  end;


implementation

uses
  pkgmessages;
  
{ TFPMakeCompiler }

Procedure TFPMakeCompiler.CompileFPMake;
Var
  O,C : String;
  FPMakeSrc : string;
  HaveFpmake : boolean;
begin
  { Check for fpmake source }
  FPMakeSrc:='fpmake.pp';
  HaveFpmake:=FileExists(FPMakeSrc);
  If Not HaveFPMake then
    begin
      HaveFPMake:=FileExists('fpmake.pas');
      If HaveFPMake then
        FPMakeSrc:='fpmake.pas';
    end;
  if Not HaveFPMake then
    Error(SErrMissingFPMake);
  { Call compiler }
  C:=Defaults.Compiler;
  O:=FPmakeSrc;
  Log(vCommands,SLogCompilingFPMake+C+' '+O);
  If ExecuteProcess(C,O)<>0 then
    Error(SErrFailedToCompileFPCMake)
end;


function TFPMakeCompiler.Execute(const Args:array of string):boolean;
begin
{$warning TODO Check arguments}
  CompileFPMake;
  result:=true;
end;


{ TFPMakeRunner }

Function TFPMakeRunner.RunFPMake : Integer;

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
  FPMakeBin:='fpmake'+ExeExt;
  D:=IncludeTrailingPathDelimiter(GetCurrentDir);
  O:='';
  For I:=1 to ParamCount do
    begin
    If (O<>'') then
      O:=O+' ';
    O:=O+MaybeQuote(ParamStr(I));
    end;
  Log(vCommands,SLogRunningFPMake+D+FPMakeBin+' '+O);
  Result:=ExecuteProcess(D+FPMakeBin,O);
end;


function TFPMakeRunner.Execute(const Args:array of string):boolean;
begin
{$warning TODO Check arguments}
  result:=(RunFPMake=0);
end;


end.
