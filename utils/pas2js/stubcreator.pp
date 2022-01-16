{
    Copyright (C) 2017 - 2020 by Michael Van Canneyt michael@freepascal.org

    pas2js Delphi stub generator  - component

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit stubcreator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, inifiles, pscanner, pparser, pastree, iostream, paswrite;

type
  { We have to override abstract TPasTreeContainer methods  }

  TSimpleEngine = class(TPasTreeContainer)
  public
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      override;
    function FindElement(const AName: String): TPasElement; override;
  end;

  TWriteCallBack = Procedure (Data : Pointer; AFileData : PAnsiChar; AFileDataLen: Int32); stdcall;
  TWriteEvent = Procedure(AFileData : String) of object;
  TUnitAliasCallBack = Function (Data: Pointer; AUnitName: PAnsiChar;
    var AUnitNameMaxLen: Int32): boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

  { TStubCreator }

  TStubCreator = Class(TComponent)
  private
    FConfigFile: String;
    FHeaderStream: TStream;
    FIncludePaths: TStrings;
    FInputFile: String;
    FOnUnitAliasData: Pointer;
    FOnWrite: TWriteEvent;
    FOnWriteCallBack: TWriteCallBack;
    FOutputFile: String;
    FDefines : TStrings;
    FOptions: TPasWriterOptions;
    FLineNumberWidth,
    FIndentSize : Integer;
    FExtraUnits : String;
    FForwardClasses : String;
    FHeaderFile : String;
    FOutputStream: TStream;
    FWriteStream : TStringStream;
    FCallBackData : Pointer;
    FLastErrorClass : String;
    FLastError : String;
    FOnUnitAlias : TUnitAliasCallBack;
    procedure SetDefines(AValue: TStrings);
    procedure SetIncludePaths(AValue: TStrings);
    procedure SetOnWrite(AValue: TWriteEvent);
    procedure SetWriteCallback(AValue: TWriteCallBack);
    function CheckUnitAlias(const AUnitName: String): String;
  Protected
    procedure DoExecute;virtual;
    Procedure DoWriteEvent; virtual;
    procedure ReadConfig(const aFileName: String); virtual;
    procedure ReadConfig(const aIni: TIniFile); virtual;
    procedure WriteModule(M: TPasModule); virtual;
    function GetModule: TPasModule; virtual;
    Function MaybeGetFileStream(AStream : TStream; Const AFileName : String; aFileMode : Word) : TStream; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function Execute: Boolean;
    Procedure GetLastError(Out AError,AErrorClass : String);
    // Streams take precedence over filenames. They will be freed on destroy!
    // OutputStream can be used combined with write callbacks.
    Property OutputStream : TStream Read FOutputStream Write FOutputStream;
    Property HeaderStream : TStream Read FHeaderStream Write FHeaderStream;
    Property OnUnitAlias: TUnitAliasCallBack read FOnUnitAlias Write FOnUnitAlias;
    Property OnUnitAliasData : Pointer Read FOnUnitAliasData Write FOnUnitAliasData;
    Property OnWriteCallBack : TWriteCallBack Read FOnWriteCallBack Write SetWriteCallback;
    Property CallbackData : Pointer Read FCallBackData Write FCallBackData;
    Property ExtraUnits : String Read FExtraUnits write FExtraUnits;
  Published
    Property Defines : TStrings Read FDefines Write SetDefines;
    Property ConfigFileName : String Read FConfigFile Write FConfigFile;
    Property InputFileName : String Read FInputFile write FInputFile;
    Property OutputFileName : String Read FOutputFile write FOutputFile;
    Property HeaderFileName : String Read FHeaderFile write FHeaderFile;
    Property ForwardClasses : String Read FForwardClasses write FForwardClasses;
    Property IncludePaths : TStrings Read FIncludePaths Write SetIncludePaths;
    Property OnWrite : TWriteEvent Read FOnWrite Write SetOnWrite;
  end;

Implementation

uses Math;

ResourceString
  SErrNoDestGiven = 'No destination file specified.';
  SErrNoSourceParsed = 'Parsing produced no file.';

procedure TStubCreator.SetDefines(AValue: TStrings);
begin
  if FDefines=AValue then Exit;
  FDefines.Assign(AValue);
end;

procedure TStubCreator.SetIncludePaths(AValue: TStrings);
begin
  if FIncludePaths=AValue then Exit;
  FIncludePaths.Assign(AValue);
end;

procedure TStubCreator.SetOnWrite(AValue: TWriteEvent);
begin
  if FOnWrite=AValue then Exit;
  FOnWrite:=AValue;
  FreeAndNil(FWriteStream);
  if Assigned(AValue) then
    FWriteStream:=TStringStream.Create('');
end;

procedure TStubCreator.SetWriteCallback(AValue: TWriteCallBack);
begin
  if FOnWriteCallBack=AValue then Exit;
  FOnWriteCallBack:=AValue;
  FreeAndNil(FWriteStream);
  if Assigned(AValue) then
    FWriteStream:=TStringStream.Create('');
end;

function TStubCreator.CheckUnitAlias(const AUnitName: String): String;
const
  MAX_UNIT_NAME_LENGTH = 255;

var
   UnitMaxLenthName: Integer;

begin
  Result := AUnitName;
  UnitMaxLenthName := Max(MAX_UNIT_NAME_LENGTH, Result.Length);

  SetLength(Result, UnitMaxLenthName);

  if FOnUnitAlias(OnUnitAliasData, @Result[1], UnitMaxLenthName) then
    Result := LeftStr(PChar(Result), UnitMaxLenthName);
end;

procedure TStubCreator.DoWriteEvent;

Var
  S : String;

begin
  If Assigned(FOnWrite) then
    FOnWrite(FWriteStream.DataString);
  if Assigned(FOnWriteCallBack) then
    begin
    S:=FWriteStream.DataString;
    FOnWriteCallBack(FCallBackData,PChar(S),Length(S));
    end;
end;

{ TStubCreator }

procedure TStubCreator.ReadConfig(const aFileName: String);

Var
  ini : TMemIniFile;

begin
  ini:=TMemIniFile.Create(AFileName);
  try
    ReadConfig(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TStubCreator.ReadConfig(const aIni: TIniFile);

Const
  DelChars = [',',' '];

Var
  O : TPaswriterOptions;
  S : String;
  I : Integer;


begin
  O:=[];
  With aIni do
    begin
    if ReadBool('Config','addlinenumber',False) then
       Include(O,woAddLineNumber);
    if ReadBool('Config','addsourcelinenumber',False) then
      Include(O,woAddLineNumber);
    FOptions:=FOptions+O;
    InputFilename:=ReadString('config','input',InputFilename);
    OutputFilename:=ReadString('config','output',OutputFilename);
    HeaderFilename:=ReadString('config','header',HeaderFilename);
    FIndentSize:=ReadInteger('config','indentsize',FIndentSize);
    FLineNumberWidth:=ReadInteger('config','linenumberwidth',FLineNumberWidth);
    FExtraUnits:=ReadString('config','extra',FExtraUnits);
    FForwardClasses:=ReadString('config','forwardclasses',FForwardClasses);
    S:=ReadString('config','defines','');
    if (S<>'') then
      For I:=1 to WordCount(S,DelChars) do
        FDefines.Add(UpperCase(ExtractWord(I,S,DelChars)));
    S:=ReadString('config','includepaths','');
    if (S<>'') then
      For I:=1 to WordCount(S,[',',';']) do
        FIncludePaths.Add(ExtractWord(I,S,[',',';']));
    end;
  if (FForwardClasses<>'') or (FForwardClasses='all') then
    Include(O,woForwardClasses);
end;

function TStubCreator.Execute: Boolean;
begin
  FLastErrorClass:='';
  FLastError:='';
  Result := False;
  if Defines.IndexOf('MakeStub')=-1 then

  Try
    DoExecute;

    Result := True;
  except
    On E : Exception do
      begin
      FLastErrorClass:=E.Classname;
      FLastError:=E.Message;
      end;
  end;
end;

procedure TStubCreator.GetLastError(out AError, AErrorClass: String);
begin
  AError:=FLastError;
  AErrorClass:=FLastErrorClass;
end;

procedure TStubCreator.DoExecute;

Var
  M : TPasModule;

begin
  If (ConfigFileName<>'') then
    ReadConfig(ConfigFileName);
  if InputFilename = '' then
    raise Exception.Create(SErrNoSourceGiven);
  if (OutputFilename = '') and (FoutputStream=Nil) and (FWriteStream=Nil) then
    raise Exception.Create(SErrNoDestGiven);
  if CompareText(ForwardClasses,'all')=0 then
    begin
    Include(Foptions,woForwardClasses);
    ForwardClasses:='';
    end
  else if (ForwardClasses<>'') then
    Include(Foptions,woForwardClasses);
  Include(Foptions,woForceOverload);
  M:=GetModule;
  if M=Nil then
    raise Exception.Create(SErrNoSourceParsed);
  try
    WriteModule(M);
  finally
    M.Free;
  end;
end;

{ TSimpleEngine }

function TSimpleEngine.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
begin
  Result := AClass.Create(AName, AParent);
  Result.Visibility := AVisibility;
  Result.SourceFilename := ASourceFilename;
  Result.SourceLinenumber := ASourceLinenumber;
end;

function TSimpleEngine.FindElement(const AName: String): TPasElement;
begin
  { dummy implementation, see TFPDocEngine.FindElement for a real example }
  Result := nil;
  if AName<>'' then ; // Keep compiler happy
end;



function TStubCreator.GetModule: TPasModule;

Var
  SE : TSimpleEngine;
  FileResolver: TFileResolver;
  Parser: TPasParser;
  Scanner: TPascalScanner;

var
  s: String;

begin
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  Parser := nil;
  SE:=TSimpleEngine.Create;
  try
    // File resolver
    FileResolver := TFileResolver.Create;
    FileResolver.UseStreams:=True;
    FileResolver.AddIncludePath(ExtractFilePath(InputFileName));
    For S in FIncludePaths do
      FileResolver.AddIncludePath(S);
    // Scanner
    Scanner := TPascalScanner.Create(FileResolver);
    Scanner.Options:=[po_AsmWhole,po_KeepClassForward,po_ExtConstWithoutExpr];
    SCanner.LogEvents:=SE.ScannerLogEvents;
    SCanner.OnLog:=SE.Onlog;
    For S in FDefines do
      Scanner.AddDefine(S);
    if FDefines.IndexOf('MAKESTUB')=-1 then
      Scanner.AddDefine('MAKESTUB');
    Scanner.OpenFile(InputFilename);
    // Parser
    Parser:=TPasParser.Create(Scanner, FileResolver, SE);
    Parser.LogEvents:=SE.ParserLogEvents;
    Parser.OnLog:=SE.Onlog;
    Parser.Options:=Parser.Options+[po_AsmWhole,po_delphi,po_KeepClassForward,po_ExtConstWithoutExpr,po_AsyncProcs];
    Parser.ParseMain(Result);
  finally
    Parser.Free;
    Scanner.Free;
    FileResolver.Free;
    SE.Free;
  end;
end;

function TStubCreator.MaybeGetFileStream(AStream: TStream;
  const AFileName: String; aFileMode: Word): TStream;
begin
  If Assigned(AStream) then
    Result:=AStream
  else if (AFileName<>'') then
    Result:=TFileStream.Create(AFileName,aFileMode)
  else
    Result:=Nil;
end;

constructor TStubCreator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefines:=TStringList.Create;
  FIncludePaths:=TStringList.Create;
  FLineNumberWidth:=4;
  FIndentSize:=2;
  FExtraUnits:='';
  FOptions:=[woNoImplementation,woNoExternalClass,woNoExternalVar,woNoExternalFunc,woNoAsm,woSkipPrivateExternals,woAlwaysRecordHelper,woSkipHints];
end;

destructor TStubCreator.Destroy;
begin
  FreeAndNil(FWriteStream);
  FreeAndNil(FOutputStream);
  FreeAndNil(FHeaderStream);
  FreeAndNil(FIncludePaths);
  FreeAndNil(FDefines);
  inherited Destroy;
end;


procedure TStubCreator.WriteModule(M: TPasModule);

Var
  F,H : TStream;
  W : TPasWriter;

begin
  W:=Nil;
  F:=MaybeGetFileStream(OutputStream,FOutputFile,fmCreate);
  if (F=Nil) then
    if FWriteStream<>nil then
      F:=FWriteStream
    else
      F:=TIOStream.Create(iosOutPut);
  try
     H:=MaybeGetFileStream(HeaderStream,FHeaderFile,fmOpenRead or fmShareDenyWrite);
     if Assigned(h) then
       try
         F.CopyFrom(H,H.Size);
       finally
         if H<>HeaderStream then
           H.Free;
       end;
     W:=TPasWriter.Create(F);
     W.Options:=FOptions;
     W.ExtraUnits:=FExtraUnits;

     if Assigned(FOnUnitAlias) then
       W.OnUnitAlias:=@CheckUnitAlias;

     if FIndentSize<>-1 then
       W.IndentSize:=FIndentSize;
     if FLineNumberWidth>0 then
       W.LineNumberWidth:=FLineNumberWidth;

     W.ForwardClasses.CommaText:=FForwardClasses;
     W.WriteModule(M);
     if Assigned(FWriteStream) then
       DoWriteEvent;
  finally
    W.Free;
    if (F<>OutputStream) and (F<>FWriteStream) then
      F.Free;
  end;
end;

end.

