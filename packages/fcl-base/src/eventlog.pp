{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Cross-platform event logging facility.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
unit eventlog;

interface

uses SysUtils,Classes;

Type
  TEventLog = Class;
  TEventType = (etCustom,etInfo,etWarning,etError,etDebug);
  TLogType = (ltSystem,ltFile);
  TLogCodeEvent = Procedure (Sender : TObject; Var Code : DWord) of Object;
  TLogCategoryEvent = Procedure (Sender : TObject; Var Code : Word) of Object;

  TEventLog = Class(TComponent)
  Private
    FEventIDOffset : DWord;
    FLogHandle : Pointer;
    FStream : TFileStream;
    FActive: Boolean;
    FRaiseExceptionOnError: Boolean;
    FIdentification: String;
    FDefaultEventType: TEventType;
    FLogtype: TLogType;
    FFileName: String;
    FTimeStampFormat: String;
    FCustomLogType: Word;
    FOnGetCustomCategory : TLogCategoryEvent;
    FOnGetCustomEventID : TLogCodeEvent;
    FOnGetCustomEvent : TLogCodeEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetIdentification(const Value: String);
    procedure SetlogType(const Value: TLogType);
    procedure ActivateLog;
    procedure DeActivateLog;
    procedure ActivateFileLog;
    procedure SetFileName(const Value: String);
    procedure ActivateSystemLog;
    function DefaultFileName: String;
    procedure WriteFileLog(EventType : TEventType; Msg: String);
    procedure WriteSystemLog(EventType: TEventType; Msg: String);
    procedure DeActivateFileLog;
    procedure DeActivateSystemLog;
    procedure CheckIdentification;
    Procedure DoGetCustomEventID(Var Code : DWord);
    Procedure DoGetCustomEventCategory(Var Code : Word);
    Procedure DoGetCustomEvent(Var Code : DWord);
  Protected
    Procedure CheckInactive;
    Procedure EnsureActive;
    function MapTypeToEvent(EventType: TEventType): DWord;
    Function MapTypeToCategory(EventType : TEventType) : Word;
    Function MapTypeToEventID(EventType : TEventType) : DWord;
  Public
    Destructor Destroy; override;
    Function EventTypeToString(E : TEventType) : String;
    Function RegisterMessageFile(AFileName : String) : Boolean; virtual;
    Procedure Log (EventType : TEventType; Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Log (EventType : TEventType; Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Log (Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Log (Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Warning (Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Warning (Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Error (Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Error (Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Debug (Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Debug (Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
    Procedure Info (Msg : String); {$ifndef fpc }Overload;{$endif}
    Procedure Info (Fmt : String; Args : Array of const); {$ifndef fpc }Overload;{$endif}
  Published
    Property Identification : String Read FIdentification Write SetIdentification;
    Property LogType : TLogType Read Flogtype Write SetlogType;
    Property Active : Boolean Read FActive write SetActive;
    Property RaiseExceptionOnError : Boolean Read FRaiseExceptionOnError Write FRaiseExceptionOnError;
    Property DefaultEventType : TEventType Read FDEfaultEventType Write FDefaultEventType;
    Property FileName : String Read FFileName Write SetFileName;
    Property TimeStampFormat : String Read FTimeStampFormat Write FTimeStampFormat;
    Property CustomLogType : Word Read FCustomLogType Write FCustomLogType;
    Property EventIDOffset : DWord Read FEventIDOffset Write FEventIDOffset;
    Property OnGetCustomCategory : TLogCategoryEvent Read FOnGetCustomCategory Write FOnGetCustomCategory;
    Property OnGetCustomEventID : TLogCodeEvent Read FOnGetCustomEventID Write FOnGetCustomEventID;
    Property OnGetCustomEvent : TLogCodeEvent Read FOnGetCustomEvent Write FOnGetCustomEvent;
  End;

  ELogError = Class(Exception);

Resourcestring

  SLogInfo      = 'Info';
  SLogWarning   = 'Warning';
  SLogError     = 'Error';
  SLogDebug     = 'Debug';
  SLogCustom    = 'Custom (%d)';
  SErrLogFailedMsg = 'Failed to log entry (Error: %s)';
  
implementation

{$i eventlog.inc}

{ TEventLog }

Resourcestring
  SErrOperationNotAllowed = 'Operation not allowed when eventlog is active.';

procedure TEventLog.CheckInactive;
begin
  If Active then
    Raise ELogError.Create(SErrOperationNotAllowed);
end;

procedure TEventLog.Debug(Fmt: String; Args: array of const);
begin
   Debug(Format(Fmt,Args));
end;

procedure TEventLog.Debug(Msg: String);
begin
  Log(etDebug,Msg);
end;

procedure TEventLog.EnsureActive;
begin
  If Not Active then
    Active:=True;
end;

procedure TEventLog.Error(Fmt: String; Args: array of const);
begin
  Error(Format(Fmt,Args));
end;

procedure TEventLog.Error(Msg: String);
begin
  Log(etError,Msg);
end;

procedure TEventLog.Info(Fmt: String; Args: array of const);
begin
  Info(Format(Fmt,Args));
end;

procedure TEventLog.Info(Msg: String);
begin
  Log(etInfo,Msg);
end;

procedure TEventLog.Log(Msg: String);
begin
  Log(DefaultEventType,msg);
end;

procedure TEventLog.Log(EventType: TEventType; Fmt: String;
  Args: array of const);
begin
  Log(EventType,Format(Fmt,Args));
end;

procedure TEventLog.Log(EventType: TEventType; Msg: String);
begin
  EnsureActive;
  Case FlogType of
    ltFile   : WriteFileLog(EventType,Msg);
    ltSystem : WriteSystemLog(EventType,Msg);
  end;
end;

procedure TEventLog.WriteFileLog(EventType : TEventType; Msg : String);

Var
  S,TS,T : String;

begin
  If FTimeStampFormat='' then
    FTimeStampFormat:='yyyy-mm-dd hh:nn:ss.zzz';
  TS:=FormatDateTime(FTimeStampFormat,Now);
  T:=EventTypeToString(EventType);
  S:=Format('%s [%s %s] %s%s',[Identification,TS,T,Msg,LineEnding]);
  try
    FStream.WriteBuffer(S[1],Length(S));
    S:='';
  except
    On E : Exception do
      S:=E.Message;
  end;  
  If (S<>'') and RaiseExceptionOnError then
    Raise ELogError.CreateFmt(SErrLogFailedMsg,[S]);
end;

procedure TEventLog.Log(Fmt: String; Args: array of const);
begin
  Log(Format(Fmt,Args));
end;

procedure TEventLog.SetActive(const Value: Boolean);
begin
  If Value<>FActive then
    begin
    If Value then
      ActivateLog
    else
      DeActivateLog;
    FActive:=Value;
    end;
end;

Procedure TEventLog.ActivateLog;

begin
  Case FLogType of
    ltFile : ActivateFileLog;
    ltSystem : ActivateSystemLog;
  end;
end;

Procedure TEventLog.DeActivateLog;

begin
  Case FLogType of
    ltFile : DeActivateFileLog;
    ltSystem : DeActivateSystemLog;
  end;
end;

Procedure TEventLog.ActivateFileLog;

begin
  If (FFileName='') then
    FFileName:=DefaultFileName;
  // This will raise an exception if the file cannot be opened for writing !
  FStream:=TFileStream.Create(FFileName,fmCreate or fmShareDenyWrite);
end;

Procedure TEventLog.DeActivateFileLog;

begin
  FStream.Free;
  FStream:=Nil;
end;


procedure TEventLog.SetIdentification(const Value: String);
begin
  FIdentification := Value;
end;

procedure TEventLog.SetlogType(const Value: TLogType);
begin
  CheckInactive;
  Flogtype := Value;
end;

procedure TEventLog.Warning(Fmt: String; Args: array of const);
begin
  Warning(Format(Fmt,Args));
end;

procedure TEventLog.Warning(Msg: String);
begin
  Log(etWarning,Msg);
end;

procedure TEventLog.SetFileName(const Value: String);
begin
  CheckInactive;
  FFileName := Value;
end;

Procedure TEventLog.CheckIdentification;

begin
  If (Identification='') then
    Identification:=ChangeFileExt(ExtractFileName(Paramstr(0)),'');
end;

Function TEventLog.EventTypeToString(E : TEventType) : String;

begin
  Case E of
    etInfo    : Result:=SLogInfo;
    etWarning : Result:=SLogWarning;
    etError   : Result:=SLogError;
    etDebug   : Result:=SLogDebug;
    etCustom  : Result:=Format(SLogCustom,[CustomLogType]);
  end;
end;

Procedure TEventLog.DoGetCustomEventID(Var Code : DWord);

begin
  If Assigned(FOnGetCustomEventID) then
    FOnGetCustomEventID(Self,Code);
end;

Procedure TEventLog.DoGetCustomEventCategory(Var Code : Word);

begin
  If Assigned(FOnGetCustomCategory) then
    FOnGetCustomCategory(Self,Code);
end;

Procedure TEventLog.DoGetCustomEvent(Var Code : DWord);

begin
  If Assigned(FOnGetCustomEvent) then
    FOnGetCustomEvent(Self,Code);
end;


destructor TEventLog.Destroy;
begin
  Active:=False;
  inherited;
end;

end.
