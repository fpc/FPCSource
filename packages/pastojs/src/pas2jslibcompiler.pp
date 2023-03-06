{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Michael Van Canneyt

    Pascal to Javascript converter class. Library version

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit pas2jslibcompiler;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

{$IFDEF darwin}
{$DEFINE UseCDecl}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes, System.Math,
  Pas2Js.SrcMap, Pas2Js.Files.Cache, Pas2Js.Compiler.Base, Pas2Js.Compiler.Pcu,
  Pas2Js.Compiler.Config, Pas2Js.Compiler.PostProcess;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes, Math,
  FPPJsSrcMap, Pas2jsFileCache, Pas2JSCompiler, Pas2jsPCUCompiler,
  Pas2JSCompilerCfg, Pas2JSCompilerPP;
{$ENDIF FPC_DOTTEDUNITS}

{ ---------------------------------------------------------------------
  Compiler descendant, usable in library
  ---------------------------------------------------------------------}
Const
  DefaultReadBufferSize = 32*1024; // 32kb buffer

Type
  PDirectoryCache = Pointer;

  TLibLogCallBack = Procedure (Data : Pointer; Msg : PAnsiChar; MsgLen : Integer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TWriteJSCallBack = Procedure (Data : Pointer;
    AFileName: PAnsiChar; AFileNameLen : Integer;
    AFileData : PAnsiChar; AFileDataLen: Int32); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TReadPasCallBack = Procedure (Data : Pointer;
    AFileName: PAnsiChar; AFileNameLen : Integer;
    AFileData : PAnsiChar; Var AFileDataLen: Int32); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TReadDirCallBack = Function (Data : Pointer;
    P : PDirectoryCache; ADirPath: PAnsiChar): boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TUnitAliasCallBack = Function (Data: Pointer;
    AUnitName: PAnsiChar; AUnitNameMaxLen: Integer): boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TGetFileSrcAttrCallBack = procedure(AData: Pointer; AFileName: PAnsiChar; AFileNameLen: Integer; AFileInfo: PPas2jsFileSrcAttr); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

  { TLibraryPas2JSCompiler }

  TLibraryPas2JSCompiler = Class(TPas2JSPCUCompiler)
  private
    FLastError: String;
    FLastErrorClass: String;
    FOnLibLogCallBack: TLibLogCallBack;
    FOnLibLogData: Pointer;
    FOnReadDir: TReadDirCallBack;
    FOnReadDirData: Pointer;
    FOnReadPasData: Pointer;
    FOnReadPasFile: TReadPasCallBack;
    FOnUnitAlias: TUnitAliasCallBack;
    FOnUnitAliasData: Pointer;
    FOnWriteJSCallBack: TWriteJSCallBack;
    FOnWriteJSData: Pointer;
    FReadBufferLen: Cardinal;
    FOnGetFileSrcAttr: TGetFileSrcAttrCallBack;
    FOnGetFileSrcAttrData: Pointer;
    function GetLogEncoding: String;
    procedure SetLogEncoding(AValue: String);
  Protected
    Function DoWriteJSFile(const DestFilename, MapFilename: String; aWriter: TPas2JSMapper): Boolean; override;
    Procedure GetLastError(AError : PAnsiChar; Var AErrorLength : Longint;
      AErrorClass : PAnsiChar; Var AErrorClassLength : Longint);
    Function ReadFile(aFilename: string; var aSource: string): boolean; virtual;
    Function ReadDirectory(Dir: TPas2jsCachedDirectory): boolean; virtual;
    procedure GetFileSrcAttr(AFilename: string; var AAttr: TPas2jsFileSrcAttr);
    {$IFDEF DebugLib}
    procedure LibDbgLog(Msg: string);
    {$ENDIF}
  Public
    Constructor Create; override;
    procedure CheckUnitAlias(var UseUnitName: string); override;
    Procedure DoLibraryLog(Sender : TObject; Const Msg : String);
    Function LibraryRun(ACompilerExe, AWorkingDir : PAnsiChar; CommandLine : PPAnsiChar; DoReset : Boolean) :Boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
    Property LastError : String Read FLastError Write FLastError;
    Property LastErrorClass : String Read FLastErrorClass Write FLastErrorClass;
    property LogEncoding: String  read GetLogEncoding write SetLogEncoding;
    Property OnLibLogCallBack : TLibLogCallBack Read FOnLibLogCallBack Write FOnLibLogCallBack;
    Property OnLibLogData : Pointer Read FOnLibLogData Write FOnLibLogData;
    Property OnWriteJSCallBack : TWriteJSCallBack Read FOnWriteJSCallBack Write FOnWriteJSCallBack;
    Property OnWriteJSData : Pointer Read FOnWriteJSData Write FOnWriteJSData;
    Property OnReadPasFile : TReadPasCallBack Read FOnReadPasFile Write FOnReadPasFile;
    Property OnReadPasData : Pointer Read FOnReadPasData Write FOnReadPasData;
    Property ReadBufferLen : Cardinal Read FReadBufferLen Write FReadBufferLen;
    Property OnReadDir: TReadDirCallBack read FOnReadDir write FOnReadDir;
    Property OnReadDirData: Pointer read FOnReadDirData write FOnReadDirData;
    Property OnUnitAlias: TUnitAliasCallBack read FOnUnitAlias write FOnUnitAlias;
    Property OnUnitAliasData: Pointer read FOnUnitAliasData write FOnUnitAliasData;
    property OnGetFileSrcAttr: TGetFileSrcAttrCallBack read FOnGetFileSrcAttr write FOnGetFileSrcAttr;
    property OnGetFileSrcAttrData: Pointer read FOnGetFileSrcAttrData write FOnGetFileSrcAttrData;
  end;

Type
  PPas2JSCompiler = Pointer;

Procedure SetPas2JSWriteJSCallBack(P : PPas2JSCompiler; ACallBack : TWriteJSCallBack; CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure SetPas2JSCompilerLogCallBack(P : PPas2JSCompiler; ACallBack : TLibLogCallBack; CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure SetPas2JSReadPasCallBack(P : PPas2JSCompiler; ACallBack : TReadPasCallBack; CallBackData : Pointer; ABufferSize : Cardinal); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure SetPas2JSReadDirCallBack(P : PPas2JSCompiler; ACallBack : TReadDirCallBack; CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure AddPas2JSDirectoryEntry(P: PDirectoryCache; AFilename: PAnsiChar;
  AAge: TPas2jsFileAgeTime; AAttr: TPas2jsFileAttr; ASize: TPas2jsFileSize); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure SetPas2JSUnitAliasCallBack(P : PPas2JSCompiler; ACallBack : TUnitAliasCallBack; CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Function RunPas2JSCompiler(P : PPas2JSCompiler; ACompilerExe, AWorkingDir : PAnsiChar; CommandLine : PPAnsiChar; DoReset : Boolean) : Boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure FreePas2JSCompiler(P : PPas2JSCompiler); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Function GetPas2JSCompiler : PPas2JSCompiler; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Procedure GetPas2JSCompilerLastError(P : PPas2JSCompiler; AError : PAnsiChar; Var AErrorLength : Longint; AErrorClass : PAnsiChar; Var AErrorClassLength : Longint); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
procedure SetPas2JSLogEncoding(P : PPas2JSCompiler; Enconding: PAnsiChar); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
procedure SetPas2JSGetFileSrcAttrCallBack(P: PPas2JSCompiler; ACallBack: TGetFileSrcAttrCallBack; ACallBackData: Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

implementation

{ TLibraryPas2JSCompiler }

function TLibraryPas2JSCompiler.GetLogEncoding: String;
begin
  Result := Log.Encoding;
end;

procedure TLibraryPas2JSCompiler.SetLogEncoding(AValue: String);
begin
  Log.Encoding := AValue;
end;

function TLibraryPas2JSCompiler.DoWriteJSFile(const DestFilename,
  MapFilename: String; aWriter: TPas2JSMapper): Boolean;
var
  WithUTF8BOM: Boolean;
  ms: TMemoryStream;
begin
  Result:=Assigned(OnWriteJSCallBack);
  if Result then
    begin
    ms:=TMemoryStream.Create;
    try
      try
        WithUTF8BOM:=(Log.Encoding='') or (Log.Encoding='utf8');
        aWriter.SaveJSToStream(WithUTF8BOM,ExtractFilename(MapFilename),ms);
        {$IFDEF DebugLib}
        LibDbgLog('DoWriteJSFile: DestFile="'+DestFilename+'" Map="'+MapFilename+'" '+IntToStr(ms.Position));
        {$ENDIF}
        OnWriteJSCallBack(OnWriteJSData,PAnsiChar(DestFileName),Length(DestFileName),PAnsiChar(ms.Memory),ms.Position);
      except
        Result:=False;
      end;
    finally
      ms.Free;
    end;
    end;
end;

procedure TLibraryPas2JSCompiler.GetLastError(AError: PAnsiChar;
  var AErrorLength: Longint; AErrorClass: PAnsiChar;
  var AErrorClassLength: Longint);

Var
  L : Integer;

begin
  L:=Length(LastError);
  if (L>AErrorLength) then
    L:=AErrorLength;
  if (L>0) then
    Move(FLastError[1],AError^,L);
  AErrorLength:=L;
  L:=Length(LastErrorClass);
  if L>AErrorClassLength then
    L:=AErrorClassLength;
  if (L>0) then
    Move(FLastErrorClass[1],AErrorClass^,L);
  AErrorClassLength:=L;
end;

function TLibraryPas2JSCompiler.ReadFile(aFilename: string; var aSource: string): boolean;

Var
  Buf : Array of AnsiChar;
  S : TStringStream;
  BytesRead : Cardinal;

begin
  if Not Assigned(OnReadPasFile) then
    Exit(False);
  S:=nil;
  try
    if ReadBufferLen=0 then
      ReadBufferLen:=DefaultReadBufferSize;
    SetLength(Buf{%H-},ReadBufferLen);
    S:=TStringStream.Create(''{$IF FPC_FULLVERSION>=30101},CP_ACP{$ENDIF});
    Repeat
      BytesRead:=ReadBufferLen;
      {$IFDEF DebugLib}
      LibDbgLog('ReadFile File="'+aFilename+'" BufferLen='+IntToStr(ReadBufferLen));
      {$ENDIF}
      FOnReadPasFile(OnReadPasData,PAnsiChar(aFileName),Length(aFileName),@Buf[0],BytesRead);
      If BytesRead>0 then
        S.Write(Buf[0],BytesRead);
    Until (BytesRead<ReadBufferLen);
    Result:=S.Size<>0;
    if Result then
      aSource:=S.DataString;
  finally
    SetLength(Buf,0);
    S.Free;
  end;
end;

function TLibraryPas2JSCompiler.ReadDirectory(Dir: TPas2jsCachedDirectory
  ): boolean;
begin
  Result:=false; // return false to call the default TPas2jsCachedDirectory.DoReadDir
  if not Assigned(OnReadDir) then exit;
  {$IFDEF DebugLib}
  LibDbgLog('ReadDirectory Dir="'+Dir.Path+'"...');
  {$ENDIF}
  Result:=OnReadDir(FOnReadDirData,Dir,PAnsiChar(Dir.Path));
  {$IFDEF DebugLib}
  LibDbgLog('ReadDirectory Dir="'+Dir.Path+'" '+IntToStr(Dir.Count));
  {$ENDIF}
end;

procedure TLibraryPas2JSCompiler.GetFileSrcAttr(AFilename: string; var AAttr: TPas2jsFileSrcAttr);
begin
  AAttr.AllowSrcMap := True;
  if Assigned(OnGetFileSrcAttr) then
    begin
    {$IFDEF DebugLib}
    LibDbgLog('GetFileSrcAttr: File="'+AFilename+'"...');
    {$ENDIF}
    OnGetFileSrcAttr(FOnGetFileSrcAttrData, PAnsiChar(AFilename), Length(AFileName), @AAttr);
    end;
end;

{$IFDEF DebugLib}
procedure TLibraryPas2JSCompiler.LibDbgLog(Msg: string);
const
  LogFilename = 'pas2jslib-log.txt';
var
  s: TFileStream;
begin
  if Msg='' then exit;
  Msg:=Msg+sLineBreak;
  if FileExists(LogFilename) then
    s:=TFileStream.Create(LogFilename,fmOpenWrite or fmShareDenyNone)
  else
    s:=TFileStream.Create(LogFilename,fmCreate or fmShareDenyNone);
  try
    s.Seek(0,soEnd);
    s.Write(Msg[1],length(Msg));
  finally
    s.Free;
  end;
end;
{$ENDIF}

constructor TLibraryPas2JSCompiler.Create;
begin
  inherited Create;
  Log.OnLog:=@DoLibraryLog;
  FileCache.OnReadFile:=@ReadFile;
  FReadBufferLen:=DefaultReadBufferSize;
  FileCache.OnReadDirectory:=@ReadDirectory;
  ConfigSupport:=TPas2JSFileConfigSupport.Create(Self);
  PostProcessorSupport:=TPas2JSFSPostProcessorSupport.Create(Self);
  FileCache.OnGetFileSrcAttr:=@GetFileSrcAttr;
end;

procedure TLibraryPas2JSCompiler.CheckUnitAlias(var UseUnitName: string);
var
  UnitNameLen, UnitNameMaxLen: Integer;
  s: String;
  {$IFDEF DebugLib}
  OldUnitName: string;
  {$ENDIF}
begin
  inherited CheckUnitAlias(UseUnitName);
  UnitNameLen:=length(UseUnitName);
  if (UnitNameLen>0) and Assigned(OnUnitAlias) then
    begin
    UnitNameMaxLen:=Max(UnitNameLen,255);
    s:=UseUnitName+StringOfChar(#0,UnitNameMaxLen-UnitNameLen);
    {$IFDEF DebugLib}
    OldUnitName:=UseUnitName;
    LibDbgLog('CheckUnitAlias "'+UseUnitName+'"...');
    {$ENDIF}
    if OnUnitAlias(OnUnitAliasData,Pointer(s),UnitNameMaxLen) then
      begin
      UseUnitName:=PAnsiChar(s);
      {$IFDEF DebugLib}
      if UseUnitName<>OldUnitName then
        LibDbgLog('CheckUnitAlias New="'+UseUnitName+'"');
      {$ENDIF}
      end;
    end;
end;

procedure TLibraryPas2JSCompiler.DoLibraryLog(Sender: TObject; const Msg: String);
begin
  if Assigned(FOnLibLogCallBack) then
    FOnLibLogCallBack(FOnLibLogData,PAnsiChar(Msg),Length(Msg))
  else if isConsole then
    {AllowWriteln}Writeln(Msg);{AllowWriteln-}
end;

function TLibraryPas2JSCompiler.LibraryRun(ACompilerExe, AWorkingDir: PAnsiChar;
  CommandLine: PPAnsiChar; DoReset: Boolean): Boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

Var
  C,W : AnsiString;
  CmdLine : TStrings;
  PP : PPAnsiChar;

begin
  Result:=False;
  C:=ACompilerExe;
  W:=AWorkingDir;
  {$IFDEF DebugLib}
  LibDbgLog('LibraryRun Exe="'+C+'" WorkDir="'+W+'"');
  {$ENDIF}
  CmdLine:=TStringList.Create;
  try
    PP:=CommandLine;
    While (PP^<>Nil) do
      begin
      CmdLine.Add(pp^);
      {$IFDEF DebugLib}
      LibDbgLog('LibraryRun Param['+IntToStr(CmdLine.Count-1)+']="'+CmdLine[CmdLine.Count-1]+'"');
      {$ENDIF}
      Inc(PP);
      end;
    try
      Run(C,W,CmdLine,DoReset);
      Result:=(ExitCode=0);
      if Not Result then
        begin
        LastError:=Format('Compiler exited with exit code %d',[ExitCode]);
        LastErrorClass:='';
        {$IFDEF DebugLib}
        LibDbgLog('LibraryRun Error "'+LastError+'"');
        {$ENDIF}
        end;
    except
      On E : Exception do
        begin
        LastError:=E.Message;
        LastErrorClass:=E.ClassName;
        {$IFDEF DebugLib}
        LibDbgLog('LibraryRun Exception '+LastErrorClass+' "'+LastError+'"');
        {$ENDIF}
        end;
    end;
  finally
    CmdLine.free;
  end;
end;

{ ---------------------------------------------------------------------
  Flat interface
  ---------------------------------------------------------------------}

procedure SetPas2JSWriteJSCallBack(P: PPas2JSCompiler;
  ACallBack: TWriteJSCallBack; CallBackData: Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

begin
  TLibraryPas2JSCompiler(P).OnWriteJSCallBack:=ACallBack;
  TLibraryPas2JSCompiler(P).OnWriteJSData:=CallBackData;
end;

procedure SetPas2JSCompilerLogCallBack(P: PPas2JSCompiler;
  ACallBack: TLibLogCallBack; CallBackData: Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

begin
  TLibraryPas2JSCompiler(P).OnLibLogCallBack:=ACallBack;
  TLibraryPas2JSCompiler(P).OnLibLogData:=CallBackData;
end;

procedure SetPas2JSReadPasCallBack(P: PPas2JSCompiler;
  ACallBack: TReadPasCallBack; CallBackData: Pointer; ABufferSize: Cardinal);
  {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

begin
  TLibraryPas2JSCompiler(P).OnReadPasData:=CallBackData;
  TLibraryPas2JSCompiler(P).OnReadPasFile:=ACallback;
  if (ABufferSize=0) then
    ABufferSize:=DefaultReadBufferSize;
  TLibraryPas2JSCompiler(P).ReadBufferLen:=ABufferSize;
end;

procedure SetPas2JSReadDirCallBack(P: PPas2JSCompiler;
  ACallBack: TReadDirCallBack; CallBackData: Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  TLibraryPas2JSCompiler(P).OnReadDir:=ACallBack;
  TLibraryPas2JSCompiler(P).OnReadDirData:=CallBackData;
end;

procedure AddPas2JSDirectoryEntry(P: PDirectoryCache; AFilename: PAnsiChar;
  AAge: TPas2jsFileAgeTime; AAttr: TPas2jsFileAttr; ASize: TPas2jsFileSize);
  {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  TPas2jsCachedDirectory(P).Add(AFilename,AAge,AAttr,ASize);
end;

procedure SetPas2JSUnitAliasCallBack(P: PPas2JSCompiler;
  ACallBack: TUnitAliasCallBack; CallBackData: Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  TLibraryPas2JSCompiler(P).OnUnitAlias:=ACallBack;
  TLibraryPas2JSCompiler(P).OnUnitAliasData:=CallBackData;
end;

function RunPas2JSCompiler(P: PPas2JSCompiler; ACompilerExe,
  AWorkingDir: PAnsiChar; CommandLine: PPAnsiChar; DoReset: Boolean): Boolean;
  {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=TLibraryPas2JSCompiler(P).LibraryRun(ACompilerExe,AWorkingDir,CommandLine,DoReset)
end;

procedure FreePas2JSCompiler(P: PPas2JSCompiler); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

begin
  TLibraryPas2JSCompiler(P).Free;
end;

function GetPas2JSCompiler: PPas2JSCompiler; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

begin
  Result:=TLibraryPas2JSCompiler.Create;
end;

procedure GetPas2JSCompilerLastError(P: PPas2JSCompiler; AError: PAnsiChar;
  Var AErrorLength: Longint; AErrorClass: PAnsiChar;
  Var AErrorClassLength: Longint); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

begin
  TLibraryPas2JSCompiler(P).GetLastError(AError,AErrorLength,AErrorClass,AErrorClassLength);
end;

procedure SetPas2JSLogEncoding(P : PPas2JSCompiler; Enconding: PAnsiChar); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  TLibraryPas2JSCompiler(P).LogEncoding := Enconding;
end;

procedure SetPas2JSGetFileSrcAttrCallBack(P: PPas2JSCompiler; ACallBack: TGetFileSrcAttrCallBack; ACallBackData: Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
begin
  TLibraryPas2JSCompiler(P).OnGetFileSrcAttr := ACallBack;
  TLibraryPas2JSCompiler(P).OnGetFileSrcAttrData := ACallBackData;
end;

end.

