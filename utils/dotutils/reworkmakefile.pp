{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Rework makefile rules: 
    Replace hardcoded unit names xyz in a rule with variable XYZUNIT.
    (see genunitnames for how to create the variables)
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program reworkmakefile;

uses strutils, regexpr, sysutils, classes, types, namespacetool, prefixer,
  custapp, rewritemakefile;


Type

  { TRewriteMakeFileApp }

  TRewriteMakeFileApp = Class(TCustomApplication)
  Private
    FTool : TRewriteMakeFile;
    FFilenames : TStringArray;
    procedure ToolLog(Sender: TObject; EventType: TEventType; const Msg: String
      );
  Protected
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure Usage (aMsg : string);
    function ProcessOptions : Boolean;
    Procedure DoRun; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
  end;

{ TRewriteMakeFileApp }

procedure TRewriteMakeFileApp.ToolLog(Sender: TObject; EventType: TEventType;
  const Msg: String);
begin
  DoLog(EventType,Msg);
end;

procedure TRewriteMakeFileApp.DoLog(EventType: TEventType; const Msg: String);
begin
  Writeln('[',EventType,'] ',Msg);
end;

procedure TRewriteMakeFileApp.Usage(aMsg: string);
begin
  if aMsg<>'' then
    Writeln('Error: ',aMsg);
  Writeln('Usage : ',ExtractFileName(ParamStr(0)),' [options] File1 [File2..FileN]');
  Writeln('-a --aliases=FILE       Load aliases from FILE');
  Writeln('-c --common=FILE        Load names of units that must be in $(NSINC) from FILE');
  Writeln('-s --skip=FILE          Load names of units for which no rule must be made. ');
  ExitCode:=Ord(AMsg<>'');
end;


function TRewriteMakeFileApp.ProcessOptions: Boolean;

Const
  ShortOpts = 'hc:a:s:';
  LongOpts : Array of string = ('help','common:','aliases:','skip:');

Var
  S : String;

begin
  Result:=False;
  S:=CheckOptions(ShortOpts,LongOpts);
  if (S<>'') or HasOPtion('h','help') then
    begin
    Usage(S);
    exit;
    end;
  FTool.AliasesFileName:=GetOptionValue('a','aliases');
  FTool.CommonUnitsFileName:=GetOptionValue('c','common');
  FTool.SkipUnitsFileName:=GetOptionValue('s','skip');
  FFilenames:=GetNonOptions(ShortOpts,LongOpts);
  Result:=(FTool.AliasesFileName<>'');
  if Not Result then
    begin
    Usage('Need aliases file');
    exit;
    end;
  Result:=Length(FFilenames)>0;
  if Not Result then
    begin
    Usage('Need file list');
    exit;
    end;
end;

procedure TRewriteMakeFileApp.DoRun;

var
  aFile : String;

begin
  StopOnException:=True;
  Terminate;
  if not ProcessOptions then
    exit;
  For aFile in FFilenames do
    FTool.HandleMakeFile(aFile);
end;

constructor TRewriteMakeFileApp.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTool:=TRewriteMakeFile.Create(Self);
  FTool.OnLog:=@ToolLog;
end;

destructor TRewriteMakeFileApp.Destroy;
begin
  FreeAndNil(FTool);
  inherited Destroy;
end;

begin
  With TRewriteMakeFileApp.Create(nil) do
    try
      Initialize;
      Run;
    finally
      Free;
    end;
end.

