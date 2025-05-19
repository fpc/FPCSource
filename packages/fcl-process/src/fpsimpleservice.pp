{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by the Free Pascal development team
      
    Simple service application class for windows.  

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsimpleservice;

{
  Application checks following command-line options to determine behaviour:
  -r run service
  -i install service
  -u uninstall service.
  
  When the service is run, a descendent of TFPServiceThread is created and executed.
  You must set the descendant class to use in Application.ServiceClass before calling initialize.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, custapp, windows, eventlog, jwawinsvc;

Type
  { TFPServiceThread }

  TFPServiceThread = Class (TThread)
  Private
    FPaused : Boolean;
  protected
    Procedure HandleControlCode(Command :Integer); virtual;
    Procedure Log(EventType : TEventType; const Msg : String);
    Procedure PauseService; virtual;
    Procedure StopService; virtual;
    Procedure RunService; virtual;
    Procedure ContinueService; virtual;
    Procedure Execute; override;
    property Paused : Boolean Read FPaused;
  end;
  TFPServiceThreadClass = Class of TFPServiceThread;

  { TFPServiceApplication }

  TFPServiceApplication = Class(TCustomApplication)
  private
    FAllowServicePause: Boolean;
    FServiceClass: TFPServiceThreadClass;
    FServiceParamStr   : string;
    FTimeout,
    FExitCode,
    FServiceParamCount : integer;
    FStatus : TServiceStatus;
    FStopEvent : THandle;
    FReportStartStop : boolean;
    FStatusHandle : Service_Status_Handle;
    FServiceThread : TFPServiceThread;
    FChkPoint : Integer;
    FEventLog : TEventLog;
    procedure ServiceController(Command: Integer);
    procedure ServiceMain(ArgC: integer; ArgV: ppchar);
    procedure StopNow;
    function ReportNoError(AState : integer) : boolean;
    function ReportServiceStatus(CurrentState, Win32ExitCode, CheckPoint, WaitHint: integer): boolean;
  Protected
    function ConnectToServiceManager: SC_Handle;
    procedure DoRun; override;
    Procedure RunService;
    Procedure InstallService;
    Procedure UninstallService;
    Procedure DoLog(EventType : TEventType; const Msg : String); override;
  public
    Constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    // Return service error code.
    function GetServiceError: integer;
    // Return service error message.
    function GetServiceErrorMessage: string;
    // Report that a start is pending. Return true on success.
    function ReportStartPending: boolean;
    // Report that a stop is pending. Return true on success.
    function ReportStopPending: boolean;
    // Application start
    procedure Initialize; override;
    // Thread class to use when starting the service.
    Property ServiceClass : TFPServiceThreadClass Read FServiceClass Write FServiceClass;
    // Time before to generate an error. Default 20000 milliseconds
    property Timeout : integer read FTimeout write FTimeout; 
    // Exit code to return to Service Manager
    property ExitCode : integer read FExitCode write FExitCode;
    // Parameter list passed when the service was started
    property ServiceParamStr : string read FServiceParamStr; 
    // Number of parameters passed when the service was started
    property ServiceParamCount : integer read FServiceParamCount; 
    // Does the service accept pause/continue commands ?
    Property AllowServicePause : Boolean Read FAllowServicePause Write FAllowServicePause; 
  end;

  EFPService = Class(Exception);

function Application: TFPServiceApplication;

implementation


Resourcestring
  SErrNoServiceClass   = 'Cannot run: No service thread class registered.';
  SErrRunNeedsOverride = 'Cannot run: Runservice must be overridden';
  SErrRunningService   = 'Error running service: %s';
  SControlCodeReceived = 'Service: Recived control code %d';
  SServicePaused       = 'Service received pause command.';
  SServiceContinued    = 'Service received continue command.';

procedure ServiceControllerEntry(Command : DWord); stdcall;

begin
  Application.ServiceController(Command);
end;

procedure ServiceMainEntry(ArgC : DWord; ArgV : pchar); stdcall;

begin
  Application.ServiceMain(ArgC,PPchar(ArgV));
end;

Var
  App : TFPServiceApplication;

function Application: TFPServiceApplication;
begin
  If (App=Nil) then
    App:=TFPServiceApplication.Create(Nil);
  Result:=App;
end;

{ TFPServiceThread }

procedure TFPServiceThread.HandleControlCode(Command: Integer);
begin
  Log(etInfo,Format(SControlCodeReceived,[Command]));
end;

procedure TFPServiceThread.Log(EventType: TEventType; const Msg: String);
begin
  If Assigned(App) then
    App.Log(EventType,Msg);
end;

procedure TFPServiceThread.PauseService;
begin
  Log(etInfo,SServicePaused);
  Suspend;
end;

procedure TFPServiceThread.ContinueService;
begin
  Resume;
  Log(etInfo,SServiceContinued);
end;

procedure TFPServiceThread.StopService;
begin
  Terminate;
end;

procedure TFPServiceThread.RunService;
begin
  Raise EFPService.Create(SErrRunNeedsOverride);
end;



{ TFPServiceApplication }

function TFPServiceApplication.ReportServiceStatus(CurrentState,
  Win32ExitCode, CheckPoint, WaitHint: integer): boolean;

begin
  SetLastError(0);
  With FStatus do
    begin
    dwServiceType := SERVICE_WIN32_OWN_PROCESS;
    dwServiceSpecificExitCode := 0;
    if CurrentState = SERVICE_START_PENDING then
      dwControlsAccepted := 0
    else
      begin
      dwControlsAccepted := SERVICE_ACCEPT_STOP;
      if FAllowServicePause then
        dwControlsAccepted:=dwControlsAccepted or SERVICE_ACCEPT_PAUSE_CONTINUE;
      end;
    dwCurrentState:=CurrentState;
    dwCheckPoint:=CheckPoint;
    dwWaitHint:=WaitHint;
    if (ExitCode=0) then
      dwWin32ExitCode := Win32ExitCode
    else
      begin
      dwWin32ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
      dwServiceSpecificExitCode := ExitCode;
      end;
    end;
  Result:=SetServiceStatus(FStatusHandle, FStatus);
  if not Result then
    StopNow;
end;

procedure TFPServiceApplication.DoRun;
begin
  If HasOption('r','run') then
    RunService
  else if HasOption('i','install') then
    InstallService
  else if HasOption('u','uninstall') then
    UninstallService
  else
    Inherited;
  Terminate;
end;


procedure TFPServiceApplication.InstallService;

Var
  S : string;
  FManager,
  FService : SC_Handle;

begin
  S:=ParamStr(0)+' -r';
  If HasOption('c','config') then
    S:=S+' -c '+self.GetOptionValue('c','config');
  try
    FManager:=ConnectToServiceManager;
    try
      FService := CreateService(FManager, PChar(Name), Pchar(Title), SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_AUTO_START,
                                SERVICE_ERROR_NORMAL, pchar(S), nil, nil, nil, nil, nil);
      if (FService=0) then
         RaiseLastOSError;
      CloseServiceHandle(FService);
    finally
      CloseServiceHandle(FManager);
    end;
  finally
    Terminate;
  end;
end;

Function TFPServiceApplication.ConnectToServiceManager : SC_Handle;

begin
  Result:=OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if (Result=0) then
    RaiseLastOSError;
end;

procedure TFPServiceApplication.UninstallService;
Var
  FManager,
  FService : SC_Handle;

begin
  try
    FManager:=ConnectToServiceManager;
    try
      FService:=OpenService(FManager, Pchar(Name), SERVICE_ALL_ACCESS);
      if (FService=0) then
         RaiseLastOSError;
      if not DeleteService(FService) then
         RaiseLastOSError;
      CloseServiceHandle(FService);
    finally
      CloseServiceHandle(FManager);
    end;
  finally
    Terminate;
  end;
end;

procedure TFPServiceApplication.DoLog(EventType: TEventType; const Msg: String);
begin

  FeventLog.Log(EventType,Msg);
end;

constructor TFPServiceApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEventLog:=TEventLog.Create(Self);
  FEventLog.RegisterMessageFile(ParamStr(0));
  StopOnException:=True;
end;


function TFPServiceApplication.GetServiceError : integer;
begin
  Result := GetLastError;
  if Result = 0 then
    Result := -1
end;

// Returns last error message
function TFPServiceApplication.GetServiceErrorMessage : string;
begin
  Result := SysErrorMessage(GetServiceError)
end;

procedure TFPServiceApplication.StopNow;
begin
  SetLastError(0);
  SetEvent(FStopEvent)
end;

function TFPServiceApplication.ReportNoError(AState : integer) : boolean;
begin
  Result:=ReportServiceStatus(AState, NO_ERROR, 0, 0)
end;

function TFPServiceApplication.ReportStartPending : boolean;

begin
  Inc(FChkPoint);
  Result := ReportServiceStatus(SERVICE_START_PENDING, NO_ERROR, FChkPoint, Timeout);
end;

function TFPServiceApplication.ReportStopPending : boolean;
begin
  Inc(FChkPoint);
  Result := ReportServiceStatus(SERVICE_STOP_PENDING, NO_ERROR, FChkPoint, Timeout);
end;

Procedure TFPServiceApplication.ServiceController(Command :Integer);

begin
  case Command of
    SERVICE_CONTROL_PAUSE:
      if FStatus.dwCurrentState = SERVICE_RUNNING then
        begin
        FServiceThread.FPaused:=True;
        FServiceThread.PauseService;
        ReportNoError(SERVICE_PAUSED);
        end;
    SERVICE_CONTROL_CONTINUE:
      if FStatus.dwCurrentState = SERVICE_PAUSED then
        begin
        FServiceThread.FPaused:=False;
        FServiceThread.ContinueService;
        ReportNoError(SERVICE_RUNNING);
        end;
    SERVICE_CONTROL_STOP:
      begin
      ReportStopPending;
      If Assigned(FServiceThread) then
        begin
        FServiceThread.StopService;
        ReportStopPending;
        end;
      ReportStopPending;
      StopNow;
      end;
    SERVICE_CONTROL_INTERROGATE:
      ReportNoError(SERVICE_RUNNING);
  else
    FServiceThread.HandleControlCode(Command);
  end;
end;

procedure TFPServiceApplication.ServiceMain(ArgC : integer; ArgV : ppchar);

begin
  FServiceParamCount := ArgC;
  if (ArgV<>Nil) then
    FServiceParamStr := strpas(ArgV^);
  SetLastError(0);
  FStatusHandle := RegisterServiceCtrlHandlerA(PChar(Name),@ServiceControllerEntry);
  if FStatusHandle <> 0 then
    begin
    if ReportStartPending then
      begin
      SetLastError(0);
      FStopEvent := CreateEvent(nil, true, false, nil);
      if FStopEvent <> 0 then
        begin
        ReportStartPending;
        FServiceThread:=FServiceClass.Create(False);
        // Wait for stop signal
        if ReportNoError(SERVICE_RUNNING) then
          begin
          {$ifdef svcdebug}  DebugLog('Starting wait for stop event');{$endif svcdebug}
          WaitForSingleObject(FStopEvent, INFINITE);
          {$ifdef svcdebug}  DebugLog('End wait for stop event');{$endif svcdebug}
          end;
        ReportStopPending;
        SetLastError(0);
        CloseHandle(FStopEvent);
        end;
      end;
    end;
  ReportServiceStatus(SERVICE_STOPPED, GetLastError, 0, 0);
end;


Procedure TFPServiceApplication.RunService;

var
  SvcTbl : array[0..1] of TServiceTableEntry;

begin
  if (FServiceClass=nil) then
    Raise EFPService.Create(SErrNoServiceClass);
  FEventLog.Identification:='Service '+Name;
  FeventLog.Active:=True;
  fillchar(SvcTbl, sizeof(SvcTbl),0);
  SvcTbl[0].lpServiceName := Pchar(Name);
  SvcTbl[0].lpServiceProc := @ServiceMainEntry;
  SetLastError(0);
  // Returns only when the service stops
  StartServiceCtrlDispatcher(@SvcTbl[0]);
end;

procedure TFPServiceApplication.Initialize;
begin
  FTimeout  := 20000;
  FReportStartStop := true;
end;


destructor TFPServiceApplication.Destroy;

begin
  FreeAndNil(FEventLog);
  Inherited;
end;

procedure TFPServiceThread.Execute;

begin
  try
    try
      RunService;
    finally
      Terminate;
    end;
  except
    On E : Exception do
      Log(etError,Format(SerrRunningService,[E.Message]));
  end;
end;

initialization

Finalization
  FreeAndNil(App);

end.

