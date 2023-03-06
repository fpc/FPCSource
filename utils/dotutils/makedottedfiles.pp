{
    This file is part of the Free Component Library
    Copyright (c) 2022 by Michael Van Canneyt, michael@freepascal.org

    Application to Prefix units in uses clause of a list of programs. 
    Optionally adapts an fpmake file.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program makedottedfiles;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  types, Classes, SysUtils, CustApp, Prefixer, namespacetool;


type

  { TNamespaceCreation }

  TNamespaceCreation = class(TCustomApplication)
  Private
    FTool : TNamespaceTool;
    FListFileName: string;
    FQuiet : Boolean;
    FVerbose : Boolean;
    procedure DoLogTool(Sender: TObject; EventType: TEventType; const Msg: String);
    function ProcessOptions: Boolean;
  protected
    procedure DoLog(EventType: TEventType; const Msg: String); override;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(const aMsg : String);
  end;

{ TNamespaceCreation }

procedure TNamespaceCreation.DoLog(EventType: TEventType; const Msg: String);
begin
  Writeln('[',EventType,'] ',Msg);
end;


function TNamespaceCreation.ProcessOptions : Boolean;

const
  Short = 'qDhbncarl:k:d:p:s:e:m:vu';
  Long : Array of string = ('help','list:','known-prefixes:','dirmap:','backup','prefix:','subdir:','dry-run','cased','apply-rule','restart','replace-subdir','ext','fpmake-file:','quiet','verbose','update');

var
  aExt,ErrorMsg: String;
  NonOpts : Array of string;

begin
  Result:=False;
  ErrorMsg:=CheckOptions(Short,Long);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  // Options for tool class
  aExt:=GetOptionValue('e','ext');
  FTool.ForcedExt:=aExt;
  FTool.DirMapFileName:=GetOptionValue('d','dirmap');
  FTool.PrefixesFileName:=GetOptionValue('k','known-prefixes');
  FTool.DefaultPrefix:=GetOptionValue('p','prefix');
  FTool.Subdir:=GetOptionValue('s','subdir');
  if FTool.SubDir='' then
    FTool.SubDir:=DefaultSubdir;
  FTool.SubdirMode:=sdmAppend;
  if HasOption('D','replace-subdir') then
    FTool.SubDirMode:=sdmReplace;
  FTool.Backup:=HasOption('b','backup');
  FTool.Update:=HasOption('u','update');
  FTool.DryRun:=HasOption('n','dry-run');
  FTool.Restart:=HasOption('r','restart');
  FTool.CasedFiles:=HasOption('c','cased');
  FTool.FPMakeNameSpaceFile:=GetoptionValue('m','fpmake-file');
  FQuiet:=HasOption('q','quiet');
  FVerbose:=HasOption('v','verbose');
  if FVerbose then
    FQuiet:=False;
  FListFileName:=GetOptionValue('l','list');
  NonOpts:=GetNonOptions(Short,Long);
  if (FListFileName='') and (Length(NonOpts)=1) then
    FListFileName:=NonOpts[0];
  If (FListFileName='') then
    begin
    Usage('Need file list filename');
    exit;
    end;
  If (FTool.PrefixesFileName='') and (FTool.DefaultPrefix='') and not HasOption('a','apply-rule') then
    begin
    Usage('Need prefixes filename or default prefix');
    exit;
    end;
  Result:=True;
end;

procedure TNamespaceCreation.DoLogTool(Sender: TObject; EventType : TEventType; const Msg: String);

var
  CanLog : Boolean;
begin
  Case EventType of
    etDebug : CanLog:=FVerbose;
    etError : CanLog:=True;
  else
    CanLog:=Not FQuiet;
  end;
  if CanLog then
    Log(EventType,Msg);
end;

procedure TNamespaceCreation.DoRun;


begin
  Terminate;
  if not ProcessOptions then
    exit;
  FTool.Init;
  if HasOption('a','apply-rule') then
    FTool.CreateKnown(FListFileName)
  else
    FTool.HandleFileList(FListFileName);
end;

constructor TNamespaceCreation.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FTool:=TNamespaceTool.Create(Self);
  FTool.OnLog:=@DoLogTool;
end;

destructor TNamespaceCreation.Destroy;

begin
  FreeAndNil(FTool);
  Inherited;
end;

procedure TNamespaceCreation.Usage(const aMsg: String);

begin
  if aMsg<>'' then
    Writeln('Error : ',aMsg);
  { add your help code here }
  Writeln('Usage: ', ExeName, ' [options] list');
  Writeln('Where options is one or more of');
  Writeln('-h --help                  This help');
  Writeln('-a --apply-rule            Apply rule in filelist to construct known file list.');
  Writeln('                           if -k is not specified, then map file is list file with extension changed to FILE.map)');
  Writeln('-b --backup                Create backup of files that are written');
  Writeln('-c --cased                 First letter of name is uppercased when creating alias');
  Writeln('-d --dirmap=FILE           Directory mapping. Old=New, new is relative to subdir');
  Writeln('-e --ext=EXT               Force extension of created dotted units to EXT. If not set, original extension is taken');
  Writeln('-D --replace-subdir        Directory mapping. Completely replace dir with mapping from dirmap.');
  Writeln('-k --known-prefixes=FILE   Namespace mapping. Unit=Namespace');
  Writeln('-l --list=FILE             Files to handle. One file per line');
  Writeln('-m --fpmake-file=FILE      Write namespace unit mappping to FILE and add as NameSpaceMap to package definition in fpmake.pp');
  Writeln('-n --dry-run               Do not execute commands, only write what would be done.');
  Writeln('-r --restart               Do not load done.lst. Default is to load done.lst and skip files listed in it.');
  Writeln('                           The file is always updated when done.');
  Writeln('-q --quiet                 Do not produce any output.');
  Writeln('-s --subdir=DIRECTORY      Directory in which to write files. Default: '+FTool.DefaultPrefix);
  Writeln('-u --update                Write updated known prefixes file.');
  Writeln('-v --verbose               Produce debug output (reports on uses clause manipulations).');
end;

var
  Application: TNamespaceCreation;
begin
  Application:=TNamespaceCreation.Create(nil);
  Application.Title:='Namespaced files creation tool';
  Application.Run;
  Application.Free;
end.

