{ *********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2021 Michael Van Canneyt.

    Typescript declaration module conversion to pascal program.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}

program dts2pas;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, CustApp, pascodegen, tstopas;

type
  { TParseTSApplication }

  TParseTSApplication = class(TCustomApplication)
  private
    FVerbose,
    FWeb : Boolean;
    FLinks,
    FUnits,
    FAliases : TStringArray;

    procedure AddAliases(Converter: TTypescriptToPas; aAlias: String);
    procedure AddWebAliases(S: Tstrings);
    procedure AddJSAliases(S: Tstrings);
    procedure DoLog(Sender: TObject; LogType: TCodegenLogType; const Msg: String);
    function ParseFile(const aInputFileName, aOutputFileName, aUnitName: string): Boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Usage(Msg : string); virtual;
  end;

{ TParseTSApplication }

procedure TParseTSApplication.DoRun;

var
  ErrorMsg: String;
  aUnitName,InputFile,OutputFile : String;

begin
  Terminate;
  ErrorMsg:=CheckOptions('hi:o:a:wx:u:vl:', ['help','input:','output:','alias:','web','extra-units:','unitname:','verbose','link:']);
  if (ErrorMsg<>'') or HasOption('h','help') then
    begin
    Usage(ErrorMsg);
    Exit;
    end;
  InputFile:=GetOptionValue('i','input');
  OutputFile:=GetOptionValue('o','output');
  FAliases:=GetOptionValues('a','alias');
  FLinks:=GetOptionValues('l','link');
  FUnits:=GetOptionValues('x','extra-units');
  FWeb:=HasOption('w','web');
  FVerbose:=HasOption('v','verbose');
  If OutputFile='' then
    if InputFile.EndsWith('d.ts') then
      OutputFile:=ChangeFileExt(ChangeFileExt(InputFile,''),'.pp')
    else
      OutputFile:=ChangeFileExt(InputFile,'.pp');
  aUnitName:=GetOptionValue('u','unitname');
  if aUnitName='' then
    aUnitName:=ChangeFileExt(ExtractFileName(outputFile),'');
  if not ParseFile(InputFIle,OutputFile,aUnitName) then
    ExitCode:=1;
end;

procedure TParseTSApplication.AddAliases(Converter : TTypescriptToPas; aAlias : String);

Var
  aList : TStringList;
  S : String;

begin
  if (aAlias='') then
    exit;
  if aAlias[1]='@' then
    begin
    AList:=TStringList.Create;
    try
      aList.LoadFromFile(Copy(aAlias,2,Length(aAlias)-1));
      Converter.TypeAliases.AddStrings(AList);
    finally
      AList.Free;
    end;
    end
  else
    For S in SplitString(aAlias,',;') do
      if Pos('=',S)<>0 then
        Converter.TypeAliases.Add(S);
end;

Function TParseTSApplication.ParseFile(const aInputFileName,aOutputFileName,aUnitName : string) : Boolean;

Var
  Converter : TTypescriptToPas;
  A, S,U,U1,U2 : String;
  L : TStringArray;
begin
  Result:=False;
  try
    Converter:=TTypescriptToPas.Create(Self);
    try
      AddJSAliases(Converter.TypeAliases);
      For A in FAliases do
        AddAliases(Converter,A);
      if FWeb then
        begin
        AddWebAliases(Converter.TypeAliases);
        Funits:=Concat(Funits, [ 'web' ]);
        end;
      U:='';
      For S in FUnits do
        begin
        L:=SplitString(S,',');
        For U1 in L do
          begin
          U2:=Trim(U1);
          if U2<>'' then
            begin
            if U<>'' then
              U:=U+', ';
            U:=U+U2;
            end;
          end;
        end;
      For S in Flinks do
        Converter.LinkStatements.Add(S);
      Converter.Verbose:=FVerbose;
      Converter.Options:=Converter.Options+[coInterfaceAsClass];
      Converter.ExtraUnits:=U;
      Converter.InputFileName:=aInputFileName;
      Converter.OutputFileName:=aOutputFileName;
      Converter.OutputUnitName:=aUnitName;
      Converter.Execute;
      Converter.OnLog:=@DoLog;
      Result:=True;
    finally
      Converter.Free;
    end;
  except
    on E : Exception do
      Writeln('Conversion error ',E.ClassName,' : ',E.Message);
  end;
end;

constructor TParseTSApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TParseTSApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TParseTSApplication.AddWebAliases(S : Tstrings);

begin
  With S do
    begin
    {$i web.inc}
    end;
end;

procedure TParseTSApplication.AddJSAliases(S: Tstrings);
begin
  With S do
    begin
    Add('Object=TJSObject');
    Add('Function=TJSFunction');
    Add('RegExp=TJSRegexp');
    Add('Promise=TJSPromise');
    Add('Date=TJSDate');
    Add('Array=TJSArray');
    Add('Iterator=TJSIterator');
    Add('IteratorResult=TJSIteratorResult');
    Add('AsyncIterator=TJSAsyncIterator');
    Add('ArrayBuffer=TJSArrayBuffer');
    Add('Set=TJSSet');
    Add('Map=TJSMap');
    Add('BufferSource=TJSBufferSource');
    Add('DataView=TJSDataView');
    Add('Int8Array=TJSInt8Array');
    Add('Int8ClampedArray=TJSInt8ClampedArray');
    Add('Int16Array=TJSInt16Array');
    Add('Int32Array=TJSInt32Array');
    Add('Uint8Array=TJSUInt8Array');
    Add('Uint8ClampedArray=TJSUInt8ClampedArray');
    Add('Uint16Array=TJSUInt16Array');
    Add('Uint32Array=TJSUInt32Array');
    Add('Float32Array=TJSFloat32Array');
    Add('Float64Array=TJSFloat64Array');
    Add('JSON=TJSJSON');
    Add('TextDecoder=TJSTextDecoder');
    Add('TextEncoder=TJSTextEncoder');
    Add('SyntaxError=TJSSyntaxError');
    Add('Error=TJSError');
    end;
end;

procedure TParseTSApplication.DoLog(Sender: TObject; LogType: TCodegenLogType; const Msg: String);
begin
  Writeln('[',LogType,'] : ',Msg);
end;


procedure TParseTSApplication.Usage(Msg: string);
begin
  if Msg<>'' then
    Writeln('Error : ',Msg);
  writeln('Usage: ', ExeName, ' [options]');
  Writeln('Where options is one or mote of:');
  Writeln('-a --alias=ALIAS      Define type aliases (option can be speficied multiple times)');
  Writeln('                      where ALIAS is one of');
  Writeln('                      a comma-separated list of Alias=TypeName values');
  Writeln('                      a @FILE : list is read from FILENAME, one line per alias');
  Writeln('-h --help             Display this help text');
  Writeln('-i --input=FILENAME   Parse .d.ts file FILENAME');
  Writeln('-l --link=FILENAME    add {$linklib FILENAME} statement. (option can be specified multiple times)');
  Writeln('-o --output=FILENAME  Output unit in file FILENAME');
  Writeln('-u --unit=NAME        Set output unitname');
  Writeln('-w --web              Add web unit to uses, define type aliases for web unit');
  Writeln('-x --extra-units=UNITLIST   Add units (comma-separated list of unit names) to uses clause.');
  Writeln('                      This option can be specified multiple times.');
end;

var
  Application: TParseTSApplication;
begin
  Application:=TParseTSApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

