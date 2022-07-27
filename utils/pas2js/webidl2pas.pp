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
  webidltopas2js, webidltowasmjob;

type
  TWebIDLToPasFormat = (
    wifPas2js,
    wifWasmJob
    );
const
  WebIDLToPasFormatNames: array[TWebIDLToPasFormat] of string = (
    'pas2js',
    'wasmjob'
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

procedure TWebIDLToPasApplication.DoRun;

  procedure E(const Msg: string);
  begin
    writeln('Error: ',Msg);
    Halt(1);
  end;

var
  A,ErrorMsg: String;
  I : Integer;
  ok: Boolean;
  f: TWebIDLToPasFormat;

begin
  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions('ced::f:g:hi:m:n:o:pt:u:vw:x:', [
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
    'extra:'
    ]);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    WriteHelp(ErrorMsg);
    if ErrorMsg<>'' then
      Halt(1)
    else
      Exit;
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
      E('unknown outputformat "'+A+'"');
    end;
  InitWebIDLToPas;

  // then set verbosity
  FWebIDLToPas.Verbose:=HasOption('v','verbose');

  // read other options
  CheckPas2jsOption(p2jcoExternalConst,'c','constexternal');

  if CheckBaseOption(coDictionaryAsClass,'d','dicttoclass') then
    TWebIDLToPas2js(FWebIDLToPas).DictionaryClassParent:=GetOptionValue('d','dicttoclass');

  CheckBaseOption(coExpandUnionTypeArgs,'e','expandunionargs');

  // -f ?

  A:=GetOptionValue('g','globals');
  if (Copy(A,1,1)='@') then
    begin
    Delete(A,1,1);
    FWebIDLToPas.GlobalVars.LoadFromFile(A);
    end
  else
    FWebIDLToPas.GlobalVars.CommaText:=A;

  InputFileName:=GetOptionValue('i','input');

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
      E('Invalid webidl version: "'+A+'"');
    end;

  FWebIDLToPas.ExtraUnits:=GetOptionValue('x','extra');

  FWebIDLToPas.Execute;
  // stop program loop
  Terminate;
end;

procedure TWebIDLToPasApplication.InitWebIDLToPas;
begin
  case OutputFormat of
  wifWasmJob:
    FWebIDLToPas:=TWebIDLToPasWasmJob.Create(Self);
  else
    FWebIDLToPas:=TWebIDLToPas2js.Create(Self);
  end;
  FWebIDLToPas.OnLog:=@DoConvertLog;
  FWebIDLToPas.ClassPrefix:='TJS';
  FWebIDLToPas.ClassSuffix:='';
  FWebIDLToPas.KeywordSuffix:='_';
  FWebIDLToPas.KeywordPrefix:='';
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
  Writeln(StdErr,'-h  --help                 this help text');
  Writeln(StdErr,'-c  --constexternal        Write consts as external const (no value)');
  Writeln(StdErr,'-d  --dicttoclass[=Parent] Write dictionaries as classes');
  Writeln(StdErr,'-e  --expandunionargs      Add overloads for all Union typed function arguments');
  Writeln(StdErr,'-f  --outputformat=[pas2js|wasmjob] Output format, default ',WebIDLToPasFormatNames[OutputFormat]);
  Writeln(StdErr,'-g  --globals=list         A comma separated list of global vars');
  Writeln(StdErr,'                           use @filename to load the globals from file.');
  Writeln(StdErr,'                           wasmjob: PasVarName=JSClassName,JOBRegisterName');
  Writeln(StdErr,'-i  --input=FileName       input webidl file');
  Writeln(StdErr,'-m  --implementation=Filename include file as implementation');
  Writeln(StdErr,'-n  --include=Filename     include file at end of interface');
  Writeln(StdErr,'-o  --output=FileName      output file. Defaults to unit name with .pas extension appended.');
  Writeln(StdErr,'-p  --optionsinheader      add options to header of generated file');

  Writeln(StdErr,'-t  --typealiases=alias    A comma separated list of type aliases in Alias=Name form');
  Writeln(StdErr,'                           use @filename to load the aliases from file.');
  Writeln(StdErr,'-u  --unitname=Name        name for unit. Defaults to input file without extension.');
  Writeln(StdErr,'-v  --verbose              Output some diagnostic information');
  Writeln(StdErr,'-w  --webidlversion=V      Set web IDL version. Allowed values: v1 or v2');
  Writeln(StdErr,'-x  --extra=units          Extra units to put in uses clause (comma separated list)');
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

