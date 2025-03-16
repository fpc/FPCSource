{
    This file is part of the Free Component Library

    WEBIDL to pascal code converter program
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program webidl2pas;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, webidlscanner, webidltopas, pascodegen, typinfo,
  webidltopas2js, webidltowasmjob, webidltowasmstub;

type
  TWebIDLToPasFormat = (
    wifPas2js,
    wifWasmJob,
    wifWasmJobStub
    );
const
  WebIDLToPasFormatNames: array[TWebIDLToPasFormat] of string = (
    'pas2js',
    'wasmjob',
    'wasmjobstub'
    );

type

  { TWebIDLToPasApplication }

  TWebIDLToPasApplication = class(TCustomApplication)
  private
    FOutputFormat: TWebIDLToPasFormat;
    FWebIDLToPas: TBaseWebIDLToPas;
    function CheckBaseOption(C: TBaseConversionOption;
      const AShort: Char; const aLong: String): Boolean;
    function CheckPas2jsOption(C: TPas2jsConversionOption;
      const AShort: Char; const aLong: String): Boolean;
    function ConfigWebIDLToPas: Boolean;
    procedure DoConvertLog(Sender: TObject; {%H-}LogType: TCodegenLogType; const Msg: String);
    function GetInputFileName: String;
    function GetOutputFileName: String;
    function GetUnitName: String;
    procedure SetinputFileName(AValue: String);
    procedure SetOutputFileName(AValue: String);
    procedure SetunitName(AValue: String);
  protected
    procedure DoRun; override;
    procedure InitWebIDLToPas; virtual;
  Protected
    Property WebIDLToPas : TBaseWebIDLToPas Read FWebIDLToPas;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Const Msg : string); virtual;
    Property UnitName : String Read GetUnitName Write SetunitName;
    property InputFileName : String Read GetInputFileName Write SetinputFileName;
    property OutputFileName : String Read GetOutputFileName Write SetOutputFileName;
    property OutputFormat: TWebIDLToPasFormat read FOutputFormat write FOutputFormat;
  end;

{ TWebIDLToPasApplication }

function TWebIDLToPasApplication.GetInputFileName: String;
begin
  Result:=FWebIDLToPas.InputFileName;
end;

procedure TWebIDLToPasApplication.DoConvertLog(Sender: TObject;
  LogType: TCodegenLogType; const Msg: String);
begin
  {AllowWriteln}
  Writeln(Msg);
  {AllowWriteln-}
end;

function TWebIDLToPasApplication.GetOutputFileName: String;
begin
  Result:=FWebIDLToPas.OutputFileName
end;

function TWebIDLToPasApplication.GetUnitName: String;
begin
  Result:=FWebIDLToPas.OutputUnitName;
end;

procedure TWebIDLToPasApplication.SetinputFileName(AValue: String);
begin
  FWebIDLToPas.InputFileName:=aValue;
end;

procedure TWebIDLToPasApplication.SetOutputFileName(AValue: String);
begin
  FWebIDLToPas.OutputFileName:=aValue;
end;

procedure TWebIDLToPasApplication.SetunitName(AValue: String);
begin
  FWebIDLToPas.OutputUnitName:=aValue;
end;

function TWebIDLToPasApplication.CheckBaseOption(C: TBaseConversionOption;
  const AShort: Char; const aLong: String): Boolean;
begin
  Result:=HasOption(aShort,ALong);
  if Result then
    FWebIDLToPas.BaseOptions:=FWebIDLToPas.BaseOptions+[C];
end;

function TWebIDLToPasApplication.CheckPas2jsOption(C: TPas2jsConversionOption;
  const AShort: Char; const aLong: String): Boolean;

begin
  if not (FWebIDLToPas is TWebIDLToPas2js) then exit;
  Result:=HasOption(aShort,ALong);
  if Result then
    TWebIDLToPas2js(FWebIDLToPas).Pas2jsOptions:=TWebIDLToPas2js(FWebIDLToPas).Pas2jsOptions+[C];
end;

// Return true if the configuration was OK.
function TWebIDLToPasApplication.ConfigWebIDLToPas : Boolean;

var
  A: String;
  I : Integer;
  L : TStrings;

begin
  Result:=True;
  // set verbosity
  FWebIDLToPas.Verbose:=HasOption('v','verbose');

  // read other options
  CheckPas2jsOption(p2jcoExternalConst,'c','constexternal');

  if CheckBaseOption(coDictionaryAsClass,'d','dicttoclass') then
    TWebIDLToPas2js(FWebIDLToPas).DictionaryClassParent:=GetOptionValue('d','dicttoclass');

  CheckBaseOption(coExpandUnionTypeArgs,'e','expandunionargs');
  CheckBaseOption(coChromeWindow,'r','chrome');
  CheckBaseOption(coPrivateMethods,'a','private');
  // -f ?

  A:=GetOptionValue('g','globals');
  if (Copy(A,1,1)='@') then
    begin
    Delete(A,1,1);
    FWebIDLToPas.GlobalVars.LoadFromFile(A);
    end
  else
    FWebIDLToPas.GlobalVars.CommaText:=A;

  A:=GetOptionValue('b','banned');
  if (Copy(A,1,1)='@') then
    begin
    Delete(A,1,1);
    FWebIDLToPas.Banned.LoadFromFile(A);
    end
  else
    FWebIDLToPas.Banned.CommaText:=A;


  if HasOption('l','list') then
    begin
    L:=TStringList.Create;
    try
      A:=GetOptionValue('l','list');
      if (Copy(A,1,1)='@') then
        begin
        Delete(A,1,1);
        L.LoadFromFile(A);
        end
      else
        L.CommaText:=A;
      FWebIDLToPas.SetUsedList(L);
    finally
      L.free;
    end;
    end;
  InputFileName:=GetOptionValue('i','input');
  if (InputFileName='') then
  begin
    WriteHelp('Missing input filename');
    Exit(False);
  end;

  if HasOption('m','implementation') then
    FWebIDLToPas.IncludeImplementationCode.LoadFromFile(GetOptionValue('m','implementation'));

  if HasOption('n','include') then
    FWebIDLToPas.IncludeInterfaceCode.LoadFromFile(GetOptionValue('n','include'));

  OutputFileName:=GetOptionValue('o','output');

  CheckBaseOption(coAddOptionsToHeader,'p','optionsinheader');

  A:=GetOptionValue('t','typealiases');
  if (Copy(A,1,1)='@') then
    begin
    Delete(A,1,1);
    FWebIDLToPas.TypeAliases.LoadFromFile(A);
    end
  else
    FWebIDLToPas.TypeAliases.CommaText:=A;

  UnitName:=GetOptionValue('u','unitname');
  if UnitName='' then
    UnitName:=ChangeFileExt(ExtractFileName(InputFileName),'');
  if OutputFileName='' then
    begin
    if (UnitName<>'') then
      OutputFileName:=ExtractFilePath(InputFileName)+UnitName+'.pas';
    end;

  if HasOption('w','webidlversion') then
    begin
    A:=GetOptionValue('w','webidlversion');
    I:=GetEnumValue(TypeInfo(TWebIDLVersion),A);
    if (I<>-1) then
      FWebIDLToPas.WebIDLVersion:=TWebIDLVersion(I)
    else
      begin
      WriteHelp('Invalid webidl version: "'+A+'"');
      Exit(False);
      end;
    end;

  FWebIDLToPas.ExtraUnits:=GetOptionValue('x','extra');
end;

procedure TWebIDLToPasApplication.DoRun;

const
  Short = 'ced::f:g:hi:m:n:o:pt:u:vw:x:rl:ab';
  Long : Array of string = (
    'help',
    'constexternal',
    'dicttoclass::',
    'expandunionargs',
    'outputformat:',
    'globals:',
    'input:',
    'implementation:',
    'include:',
    'output:',
    'optionsinheader',
    'typealiases:',
    'unitname:',
    'verbose',
    'webidlversion:',
    'extra:',
    'chrome',
    'list:',
    'private',
    'banned:'
    );


var
  A,ErrorMsg: String;
  ok: Boolean;
  f: TWebIDLToPasFormat;

begin
  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions(Short,Long);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    ErrorMsg:='Missing input filename';
    WriteHelp(ErrorMsg);
    Exit();
    end;

  // first read outputformat and create FWebIDLToPas
  if HasOption('f','outputformat') then
    begin
    A:=GetOptionValue('f','outputformat');
    ok:=false;
    for f in TWebIDLToPasFormat do
      begin
      if SameText(A,WebIDLToPasFormatNames[f]) then
        begin
        OutputFormat:=f;
        ok:=true;
        end;
      end;
    if not ok then
      begin
      WriteHelp('unknown outputformat "'+A+'"');
      exit;
      end;
    end;
  InitWebIDLToPas;
  if ConfigWebIDLToPas then
    FWebIDLToPas.Execute;
end;

procedure TWebIDLToPasApplication.InitWebIDLToPas;
begin
  case OutputFormat of
  wifWasmJob:
    FWebIDLToPas:=TWebIDLToPasWasmJob.Create(Self);
  wifWasmJobStub:
    FWebIDLToPas:=TWebIDLToPasWasmJobStub.Create(Self);
  else
    FWebIDLToPas:=TWebIDLToPas2js.Create(Self);
  end;
  FWebIDLToPas.OnLog:=@DoConvertLog;
  FWebIDLToPas.ClassPrefix:='TJS';
  FWebIDLToPas.ClassSuffix:='';
  FWebIDLToPas.KeywordSuffix:='_';
  FWebIDLToPas.KeywordPrefix:='';
  FWebIDLToPas.DottedUnitsSupport:=dusFull;
end;

constructor TWebIDLToPasApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  ExceptionExitCode:=1;
end;

destructor TWebIDLToPasApplication.Destroy;
begin
  FreeAndNil(FWebIDLToPas);
  inherited Destroy;
end;

procedure TWebIDLToPasApplication.WriteHelp(const Msg: string);
begin
  {AllowWriteln}
  if (Msg<>'') then
    Writeln(StdErr,'Error : ',Msg);
  writeln(StdErr,'Usage: ', ExeName, ' [options]');
  Writeln(StdErr,'Where option is one or more of');
  Writeln(StdErr,'-h  --help                 This help text.');
  Writeln(StdErr,'-a  --private              Write getters/setters as private methods. Default is protected.');
  Writeln(StdErr,'-c  --constexternal        Write consts as external const (no value).');
  Writeln(StdErr,'-d  --dicttoclass[=Parent] Write dictionaries as classes.');
  Writeln(StdErr,'-e  --expandunionargs      Add overloads for all Union typed function arguments.');
  Writeln(StdErr,'-f  --outputformat=[pas2js|wasmjob] Output format, default ',WebIDLToPasFormatNames[OutputFormat],'.');
  Writeln(StdErr,'-g  --globals=list         A comma separated list of global vars.');
  Writeln(StdErr,'                           Use @filename to load the globals from file.');
  Writeln(StdErr,'                           wasmjob: PasVarName=JSClassName,JOBRegisterName');
  Writeln(StdErr,'-i  --input=FileName       Input webidl file.');
  Writeln(StdErr,'-m  --implementation=Filename include file as implementation.');
  Writeln(StdErr,'-n  --include=Filename     Include file at end of interface.');
  Writeln(StdErr,'-o  --output=FileName      Output file. Defaults to unit name with .pas extension appended.');
  Writeln(StdErr,'-p  --optionsinheader      Add options to header of generated file.');
  Writeln(StdErr,'-l  --used=types           A comma separated list of used IDL types. Only these types and necessary dependent types will be converted.');
  Writeln(StdErr,'                           use @filename to load the globals from file.');
  Writeln(StdErr,'-t  --typealiases=alias    A comma separated list of type aliases in Alias=Name form.');
  Writeln(StdErr,'                           use @filename to load the aliases from file.');
  Writeln(StdErr,'-u  --unitname=Name        name for unit. Defaults to input file without extension.');
  Writeln(StdErr,'-v  --verbose              Output some diagnostic information.');
  Writeln(StdErr,'-w  --webidlversion=V      Set web IDL version. Allowed values: v1 or v2.');
  Writeln(StdErr,'-x  --extra=units          Extra units to put in uses clause (comma separated list).');
  Writeln(StdErr,'-b  --banned=list          List of classes that may not be added to the final file (exclude e.g. window classes for workers)');
  ExitCode:=Ord(Msg<>'');
  {AllowWriteln-}
end;

var
  Application: TWebIDLToPasApplication;
begin
  Application:=TWebIDLToPasApplication.Create(nil);
  Application.Title:='WebIDL To Pascal converter Application';
  Application.Run;
  Application.Free;
end.

