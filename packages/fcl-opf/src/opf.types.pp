{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Basic types for FCL-OPF

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Types;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db;
{$ELSE}
  Classes, SysUtils, db;
{$ENDIF}

Type

  { Exception hierarchy - all OPF exceptions descend from EOPF }

  { EOPF: base of all OPF exceptions.
    When the error was caused by a database error, ErrorCode and SQLState
    are those of the database (ESQLDatabaseError), so callers can tell a
    unique violation from a deadlock. OriginalClassName is the class of the
    exception that caused it, empty when there was none. }

  EOPF = Class(EDatabaseError)
  Private
    FErrorCode: Integer;
    FSQLState: String;
    FOriginalClassName: String;
  Public
    // Copy the details of the exception that caused this one
    procedure TakeDetails(aCause: Exception);
    // The error code reported by the database, if any
    Property ErrorCode: Integer Read FErrorCode Write FErrorCode;
    // The SQL state code reported by the database, if any
    Property SQLState: String Read FSQLState Write FSQLState;
    // The class name of the exception that caused this one, empty if there was none
    Property OriginalClassName: String Read FOriginalClassName Write FOriginalClassName;
  end;
  EOPFSQL = Class(EOPF);
  EOPFDDL = Class(EOPF);
  EOPFExecutor = Class(EOPF);
  EOPFTransaction = Class(EOPF);
  // Commit succeeded on some connections but failed on a later one.
  // Connections are named by Name, or DatabaseName when Name is empty.
  EOPFPartialCommit = Class(EOPFTransaction)
  Private
    FCommittedConnections: TStringArray;
    FFailedConnection: String;
    FNotCommittedConnections: TStringArray;
    FOriginalErrorClass: String;
    FOriginalMessage: String;
  Public
    // Committed: their changes are in the database
    Property CommittedConnections: TStringArray Read FCommittedConnections Write FCommittedConnections;
    // The connection whose commit failed: rolled back
    Property FailedConnection: String Read FFailedConnection Write FFailedConnection;
    // Connections after the failed one: rolled back, never committed
    Property NotCommittedConnections: TStringArray Read FNotCommittedConnections Write FNotCommittedConnections;
    // Class name and message of the exception raised by the failed commit
    Property OriginalErrorClass: String Read FOriginalErrorClass Write FOriginalErrorClass;
    // The message of the exception raised by the failed commit
    Property OriginalMessage: String Read FOriginalMessage Write FOriginalMessage;
  end;
  EOPFRegistry = Class(EOPF);
  EFieldMap = Class(EOPF);
  EOPFRelation = Class(EOPF);
  EOPFListReg = Class(EOPF);
  EOPFPersisterIntf = Class(EOPF);
  EOPFJson = Class(EOPF);
  EOPFManyToMany = Class(EOPF);
  EOPFLogging = Class(EOPF);
  EOPFChangeTracking = Class(EOPF);
  EOPFMemento = Class(EOPF);

  EOPFClass = Class of EOPF;

  TClassArray = Array of TClass;

  { IOPPasswordCipher:
    The mechanism for encrypting/decrypting passwords during
    connection list serialization. 
    Inject into TSQLDBConnectionList.PasswordCipher. }

  IOPPasswordCipher = Interface
    // Returns the encrypted form of a plain text password
    Function Encrypt(const aPlain: UTF8String): UTF8String;
    // Returns the plain text password from its encrypted form
    Function Decrypt(const aCipher: UTF8String): UTF8String;
  end;

  { TOPNoCipher:
    passthrough implementation, returns input unchanged. }

  TOPNoCipher = Class(TObject, IOPPasswordCipher)
    // Returns the password unchanged
    Function Encrypt(const aPlain: UTF8String): UTF8String;
    // Returns the password unchanged
    Function Decrypt(const aCipher: UTF8String): UTF8String;
  end;

  { TOPXorCipher:
    XOR-based obfuscation using strutils XorEncode/XorDecode.
    Not cryptographically secure, but prevents plaintext passwords on disk. }

  TOPXorCipher = Class(TObject, IOPPasswordCipher)
  private
    FKey: AnsiString;
  public
    // Creates a cipher that uses the given key
    Constructor Create(const aKey: AnsiString);
    // Returns the password scrambled with the key
    Function Encrypt(const aPlain: UTF8String): UTF8String;
    // Returns the original password from its scrambled form
    Function Decrypt(const aCipher: UTF8String): UTF8String;
  end;

  { TOPBaseComponent: 
    The base class for all OPF TComponent descendants.
    Provides unified RaiseError using the exception class returned by GetExceptionClass. 
    }

  TOPBaseComponent = Class(TComponent)
  Private
    FLogger: TComponent;
    procedure SetLogger(AValue: TComponent);
  Protected
    class function GetExceptionClass: EOPFClass; virtual; abstract;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Public
    // The logger of this component, or the shared logger when none is set
    function GetLogger: TComponent;
    // Raises an error of this component's exception class with the given message
    procedure RaiseError(const aOperation, aReason: string); virtual; overload;
    // Raises an error with a message built from a format string and arguments
    procedure RaiseError(const aOperation, aFmt: string; const aArgs: array of const); virtual; overload;
    // RaiseError for an error caused by aCause: the database error code and
    // SQL state are passed on (EOPF.ErrorCode, EOPF.SQLState)
    procedure RaiseDBError(const aOperation, aFmt: string; const aArgs: array of const; aCause: Exception); virtual;
  Published
    // The logger that receives this component's messages
    Property Logger: TComponent Read GetLogger Write SetLogger;
  end;

{ Error details of database exceptions that do not descend from
  ESQLDatabaseError (e.g. PostgreSQL's EPQDatabaseError: add unit
  Opf.PQErrors). A handler returns True when it recognised aCause. }
Type
  TOPFErrorDetailsHandler = function(aCause: Exception; var aErrorCode: Integer; var aSQLState: String): Boolean;

procedure OPFRegisterErrorDetailsHandler(aHandler: TOPFErrorDetailsHandler);

{ Standalone RaiseError for classes that cannot inherit TOPBaseComponent }
var
  // Guards the creation of the singletons (registries, builders, executor,
  // logger, factory, serializer): two threads asking for one at the same
  // time get the same instance
  OPFSingletonLock: TRTLCriticalSection;

procedure OPFRaiseError(aExcClass: EOPFClass; const aClassName, aOperation, aReason: string); overload;
procedure OPFRaiseError(aExcClass: EOPFClass; const aClassName, aOperation, aFmt: string; const aArgs: array of const); overload;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.StrUtils, Data.Sqldb,
{$ELSE}
  strutils, sqldb,
{$ENDIF}
  Opf.Logging;

resourcestring
  SErrNotALogger = 'Logger must be a TOPLogger, not a %s';

{ TOPNoCipher }

function TOPNoCipher.Encrypt(const aPlain: UTF8String): UTF8String;
begin
  Result:=aPlain;
end;

function TOPNoCipher.Decrypt(const aCipher: UTF8String): UTF8String;
begin
  Result:=aCipher;
end;

{ TOPXorCipher }

constructor TOPXorCipher.Create(const aKey: AnsiString);
begin
  inherited Create;
  FKey:=aKey;
end;

function TOPXorCipher.Encrypt(const aPlain: UTF8String): UTF8String;
begin
  if aPlain='' then
    Result:=''
  else
    Result:=XorEncode(FKey, aPlain);
end;

function TOPXorCipher.Decrypt(const aCipher: UTF8String): UTF8String;
begin
  if aCipher='' then
    Result:=''
  else
    Result:=XorDecode(FKey, aCipher);
end;

{ TOPBaseComponent }

function TOPBaseComponent.GetLogger: TComponent;
begin
  Result:=FLogger;
  if Result=nil then
    Result:=TOPLogger.Instance;
end;

procedure TOPBaseComponent.SetLogger(AValue: TComponent);
begin
  if FLogger=AValue then Exit;
  // The logger is used as a TOPLogger everywhere
  if Assigned(AValue) and not (AValue is TOPLogger) then
    raise EOPF.CreateFmt(SErrNotALogger, [AValue.ClassName]);
  if Assigned(FLogger) then
    FLogger.RemoveFreeNotification(Self);
  FLogger:=AValue;
  if Assigned(FLogger) then
    FLogger.FreeNotification(Self);
end;

procedure TOPBaseComponent.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FLogger) then
    FLogger:=Nil;
end;

procedure TOPBaseComponent.RaiseDBError(const aOperation, aFmt: string; const aArgs: array of const;
  aCause: Exception);
var
  Msg: string;
  E: Exception;
begin
  Msg:=Format('%s.%s: %s', [ClassName, aOperation, Format(aFmt, aArgs)]);
  TOPLogger(GetLogger).LogTrace(Self, Msg);
  E:=GetExceptionClass.Create(Msg);
  if E is EOPF then
    EOPF(E).TakeDetails(aCause);
  raise E;
end;

{ EOPF }

var
  ErrorDetailsHandlers: array of TOPFErrorDetailsHandler;

procedure OPFRegisterErrorDetailsHandler(aHandler: TOPFErrorDetailsHandler);
begin
  SetLength(ErrorDetailsHandlers, Length(ErrorDetailsHandlers)+1);
  ErrorDetailsHandlers[High(ErrorDetailsHandlers)]:=aHandler;
end;

procedure EOPF.TakeDetails(aCause: Exception);
var
  I: Integer;
begin
  if aCause=Nil then
    Exit;
  FOriginalClassName:=aCause.ClassName;
  for I:=0 to High(ErrorDetailsHandlers) do
    if ErrorDetailsHandlers[I](aCause, FErrorCode, FSQLState) then
      Exit;
  if aCause is ESQLDatabaseError then
  begin
    FErrorCode:=ESQLDatabaseError(aCause).ErrorCode;
    FSQLState:=ESQLDatabaseError(aCause).SQLState;
  end
  else if aCause is EOPF then
  begin
    // Wrapped twice: keep the database details
    FErrorCode:=EOPF(aCause).ErrorCode;
    FSQLState:=EOPF(aCause).SQLState;
    if EOPF(aCause).OriginalClassName<>'' then
      FOriginalClassName:=EOPF(aCause).OriginalClassName;
  end;
end;

procedure TOPBaseComponent.RaiseError(const aOperation, aReason: string);
var
  Msg: string;
begin
  Msg:=Format('%s.%s: %s', [ClassName, aOperation, aReason]);
  // Errors are logged where they are handled; tracing shows where they originate
  TOPLogger(GetLogger).LogTrace(Self, Msg);
  raise GetExceptionClass.Create(Msg);
end;

procedure TOPBaseComponent.RaiseError(const aOperation, aFmt: string; const aArgs: array of const);
var
  Msg: string;
begin
  Msg:=Format('%s.%s: %s', [ClassName, aOperation, Format(aFmt, aArgs)]);
  // Errors are logged where they are handled; tracing shows where they originate
  TOPLogger(GetLogger).LogTrace(Self, Msg);
  raise GetExceptionClass.Create(Msg);
end;

{ Standalone helpers }

procedure OPFRaiseError(aExcClass: EOPFClass; const aClassName, aOperation, aReason: string);
var
  Msg: string;
begin
  Msg:=Format('%s.%s: %s', [aClassName, aOperation, aReason]);
  // Errors are logged where they are handled; tracing shows where they originate
  if OPLogger.IsKindActive(lkTrace) then
    OPLogger.LogTrace(Nil, Msg);
  raise aExcClass.Create(Msg);
end;

procedure OPFRaiseError(aExcClass: EOPFClass; const aClassName, aOperation, aFmt: string; const aArgs: array of const);
begin
  OPFRaiseError(aExcClass, aClassName, aOperation, Format(aFmt, aArgs));
end;

initialization
  InitCriticalSection(OPFSingletonLock);

finalization
  DoneCriticalSection(OPFSingletonLock);

end.
