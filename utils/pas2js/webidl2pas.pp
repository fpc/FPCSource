{
    This file is part of the Free Component Library

    WEBIDL to pascal code converter program
    Copyright (c) 2018 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program webidl2pas;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, webidlscanner, webidltopas, pascodegen, typinfo;

type

  { TWebIDLToPasApplication }

  TWebIDLToPasApplication = class(TCustomApplication)
  private
    FWebIDLToPas: TWebIDLToPas;
    function Checkoption(Var O: TCOnversionOPtions; C: TCOnversionOPtion;
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
  Protected
    Property WebIDLToPas : TWebIDLToPas Read FWebIDLToPas;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp(Const Msg : string); virtual;
    Property UnitName : String Read GetUnitName Write SetunitName;
    property InputFileName : String Read GetInputFileName Write SetinputFileName;
    property OutputFileName : String Read GetOutputFileName Write SetOutputFileName;
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

Function TWebIDLToPasApplication.Checkoption(Var O : TCOnversionOPtions;C : TCOnversionOPtion; Const AShort : Char; Const aLong : String) : Boolean;

begin
  Result:=HasOption(aShort,ALong);
  if Result then
    Include(O,C);
end;

procedure TWebIDLToPasApplication.DoRun;

var
  A,ErrorMsg: String;
  O : TConversionOptions;
  I : Integer;

begin

  Terminate;
  // quick check parameters
  ErrorMsg:=CheckOptions('hi:o:u:m:n:vx:t:ced::pw:', ['help','input:','output:','unitname:','include:','implementation:','verbose','extra:','typealiases:','constexternal','expandunionargs','dicttoclass::','optionsinheader','webidlversion:']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    WriteHelp(ErrorMsg);
    Exit;
    end;
  O:=[];
  Checkoption(O,coExternalConst,'c','constexternal');
  Checkoption(O,coExpandUnionTypeArgs,'e','expandunionargs');
  CheckOption(O,coaddOptionsToheader,'p','optionsinheader');
  if Checkoption(O,coDictionaryAsClass,'d','dicttoclass') then
    FWebIDLToPas.DictionaryClassParent:=GetOptionValue('d','dicttoclass');
  FWebIDLToPas.Options:=O;
  InputFileName:=GetOptionValue('i','input');
  OutputFileName:=GetOptionValue('o','output');
  UnitName:=GetOptionValue('u','unitname');
  FWebIDLToPas.Verbose:=HasOption('v','verbose');
  if HasOption('w','webidlversion') then
    begin
    A:=GetOptionValue('w','webidlversion');
    I:=GetEnumValue(TypeInfo(TWebIDLVersion),A);
    if (I<>-1) then
      FWebIDLToPas.WebIDLVersion:=TWebIDLVersion(I)
    else
      Raise EConvertError.CreateFmt('Invalid webidl version: %s',[A]);
    end;
  if hasoption('n','include') then
    FWebIDLToPas.IncludeInterfaceCode.LoadFromFile(GetOptionValue('n','include'));
  if hasoption('m','implementation') then
    FWebIDLToPas.IncludeImplementationCode.LoadFromFile(GetOptionValue('m','implementation'));
  FWebIDLToPas.ExtraUnits:=GetOPtionValue('x','extra');
  A:=GetOptionValue('t','typealiases');
  if (Copy(A,1,1)='@') then
    begin
    Delete(A,1,1);
    FWebIDLToPas.TypeAliases.LoadFromFile(A);
    end
  else
    FWebIDLToPas.TypeAliases.CommaText:=A;
  if UnitName='' then
    UnitName:=ChangeFileExt(ExtractFileName(InputFileName),'');
  if OutputFileName='' then
    begin
    if (UnitName<>'') then
      OutputFileName:=ExtractFilePath(InputFileName)+UnitName+'.pas';
    end;
  FWebIDLToPas.Execute;
  // stop program loop
  Terminate;
end;

constructor TWebIDLToPasApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FWebIDLToPas:=TWebIDLToPas.Create(Self);
  FWebIDLToPas.OnLog:=@DoConvertLog;
  FWebIDLToPas.ClassPrefix:='TJS';
  FWebIDLToPas.ClassSuffix:='';
  FWebIDLToPas.KeywordSuffix:='_';
  FWebIDLToPas.KeywordPrefix:='';
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
  Writeln(StdErr,'-e  --expandunionargs      Add overloads for all Union typed function arguments');
  Writeln(StdErr,'-d  --dicttoclass[=Parent] Write dictionaries as classes');
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

