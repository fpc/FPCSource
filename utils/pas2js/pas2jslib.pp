library pas2jslib;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes, FPPJsSrcMap, Pas2jsFileCache, Pas2jsCompiler;

{ ---------------------------------------------------------------------
  Compiler descendant, usable in library
  ---------------------------------------------------------------------}

Type
  TLibLogCallBack = Procedure (Data : Pointer; Msg : PansiChar; MsgLen : Integer); stdcall;
  TWriteJSCallBack = Procedure (Data : Pointer;
    AFileName: PAnsiChar; AFileNameLen : Integer;
    AFileData : PAnsiChar; AFileDataLen: Int32); stdcall;

  { TLibraryPas2JSCompiler }

  TLibraryPas2JSCompiler = Class(TPas2JSCompiler)
  private
    FLastError: String;
    FLastErrorClass: String;
    FOnLibLogCallBack: TLibLogCallBack;
    FOnLibLogData: Pointer;
    FOnWriteJSCallBack: TWriteJSCallBack;
    FOnWriteJSData: Pointer;
  Protected
    Function DoWriteJSFile(const DestFilename: String; aWriter: TPas2JSMapper): Boolean; override;
    Procedure GetLastError(AError : PAnsiChar; Var AErrorLength : Longint;
      AErrorClass : PAnsiChar; Var AErrorClassLength : Longint);
  Public
    Constructor Create; override;
    Procedure DoLibraryLog(Sender : TObject; Const Msg : String);
    Function LibraryRun(ACompilerExe, AWorkingDir : PAnsiChar; CommandLine : PPAnsiChar; DoReset : Boolean) :Boolean; stdcall;
    Property LastError : String Read FLastError Write FLastError;
    Property LastErrorClass : String Read FLastErrorClass Write FLastErrorClass;
    Property OnLibLogCallBack : TLibLogCallBack Read FOnLibLogCallBack Write FOnLibLogCallBack;
    Property OnLibLogData : Pointer Read FOnLibLogData Write FOnLibLogData;
    Property OnWriteJSCallBack : TWriteJSCallBack Read FOnWriteJSCallBack Write FOnWriteJSCallBack;
    Property OnWriteJSData : Pointer Read FOnWriteJSData Write FOnWriteJSData;
  end;

{ TLibraryPas2JSCompiler }

function TLibraryPas2JSCompiler.DoWriteJSFile(const DestFilename: String; aWriter: TPas2JSMapper): Boolean;

Var
  Src : string;

begin
  Result:=Assigned(OnWriteJSCallBack);
  if Result then
    try
      Src:=aWriter.AsAnsistring;
      OnWriteJSCallBack(OnWriteJSData,PAnsiChar(DestFileName),Length(DestFileName),PAnsiChar(Src),Length(Src));
    except
      Result:=False;
    end;
end;

procedure TLibraryPas2JSCompiler.GetLastError(AError: PAnsiChar; var AErrorLength : Longint;
  AErrorClass: PAnsiChar; var AErrorClassLength : Longint);

Var
  L : Integer;

begin
  L:=Length(LastError);
  if (L>AErrorLength) then
    L:=AErrorLength;
  if (L>0) then
    Move(FLastError[1],AError^,L);
  L:=Length(LastErrorClass);
  if L>AErrorClassLength then
    L:=AErrorClassLength;
  if (L>0) then
    Move(FLastErrorClass[1],AErrorClass^,L);
end;

constructor TLibraryPas2JSCompiler.Create;
begin
  inherited Create;
  Log.OnLog:=@DoLibraryLog;
end;

procedure TLibraryPas2JSCompiler.DoLibraryLog(Sender: TObject; const Msg: String);
begin
  if Assigned(FOnLibLogCallBack) then
    FOnLibLogCallBack(FOnLibLogData,PAnsiChar(Msg),Length(Msg))
  else if isConsole then
    Writeln(Msg);
end;

function TLibraryPas2JSCompiler.LibraryRun(ACompilerExe, AWorkingDir: PAnsiChar;
  CommandLine: PPAnsiChar; DoReset: Boolean): Boolean; stdcall;

Var
  C,W : AnsiString;
  CmdLine : TStrings;
  PP : PPAnsiChar;

begin
  Result:=False;
  C:=ACompilerExe;
  W:=AWorkingDir;
  CmdLine:=TStringList.Create;
  try
    PP:=CommandLine;
    While (PP^<>Nil) do
      begin
      CmdLine.Add(pp^);
      Inc(PP);
      end;
    try
      Run(C,W,CmdLine,DoReset);
      Result:=(ExitCode=0);
      if Not Result then
        begin
        LastError:=Format('Compiler exited with exit code %d',[ExitCode]);
        LastErrorClass:=ECompilerTerminate.ClassName;
        end;
    except
      On E : Exception do
        begin
        LastError:=E.Message;
        LastErrorClass:=E.ClassName;
        end;
    end;
  finally
    CmdLine.free;
  end;
end;

{ ---------------------------------------------------------------------
  Flat interface
  ---------------------------------------------------------------------}

Type
  PPas2JSCompiler = Pointer;
  PStubCreator = Pointer;


Procedure SetPas2JSWriteJSCallBack(P : PPas2JSCompiler; ACallBack : TWriteJSCallBack;
  CallBackData : Pointer); stdcall;

begin
  TLibraryPas2JSCompiler(P).OnWriteJSCallBack:=ACallBack;
  TLibraryPas2JSCompiler(P).OnWriteJSData:=CallBackData;
end;

Procedure SetPas2JSCompilerLogCallBack(P : PPas2JSCompiler; ACallBack : TLibLogCallBack;
  CallBackData : Pointer); stdcall;

begin
  TLibraryPas2JSCompiler(P).OnLibLogCallBack:=ACallBack;
  TLibraryPas2JSCompiler(P).OnLibLogData:=CallBackData;
end;

Function RunPas2JSCompiler(P : PPas2JSCompiler; ACompilerExe, AWorkingDir : PAnsiChar;
  CommandLine : PPAnsiChar; DoReset : Boolean) : Boolean; stdcall;

begin
  Result:=TLibraryPas2JSCompiler(P).LibraryRun(ACompilerExe,AWorkingDir,CommandLine,DoReset)
end;

Procedure FreePas2JSCompiler(P : PPas2JSCompiler); stdcall;

begin
  TLibraryPas2JSCompiler(P).Free;
end;

Function GetPas2JSCompiler : PPas2JSCompiler; stdcall;

begin
  Result:=TLibraryPas2JSCompiler.Create;
end;

Procedure GetPas2JSCompilerLastError(P : PPas2JSCompiler; AError : PAnsiChar;
  Var AErrorLength : Longint; AErrorClass : PAnsiChar; Var AErrorClassLength : Longint); stdcall;

begin
  TLibraryPas2JSCompiler(P).GetLastError(AError,AErrorLength,AErrorClass,AErrorClassLength);
end;

exports
  GetPas2JSCompiler,
  FreePas2JSCompiler,
  RunPas2JSCompiler,
  SetPas2JSWriteJSCallBack,
  SetPas2JSCompilerLogCallBack,
  GetPas2JSCompilerLastError;

end.

