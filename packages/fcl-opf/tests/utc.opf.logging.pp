unit utc.opf.logging;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils,
  FpcUnit.Test, FpcUnit.TestRegistry,
{$ELSE}
  Classes, SysUtils,
  fpcunit, testregistry,
{$ENDIF}
  Opf.Types, Opf.Logging;

Type

  { TTestOPLogger }

  TTestOPLogger = class(TTestCase)
  private
    FLogger: TOPLogger;
    FLastSender: TObject;
    FLastLevel: TLogKind;
    FLastMessage: String;
    FEventCount: Integer;
    procedure HandleLog(Sender: TObject; aLevel: TLogKind; const aMsg: string);
    function GetTempFileName: String;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSingletonPattern;
    procedure TestIsActiveNoChannels;
    procedure TestIsActiveConsole;
    procedure TestIsActiveFile;
    procedure TestIsActiveEvent;
    procedure TestIsKindActive;
    procedure TestLogKindsDefault;
    procedure TestLogToFile;
    procedure TestLogOnEvent;
    procedure TestAddTimeStamp;
    procedure TestSenderFormatNil;
    procedure TestSenderFormatObject;
    procedure TestLevelFiltering;
    procedure TestConvenienceMethods;
    procedure TestFileChannelToggle;
  end;

implementation

{ TTestOPLogger }

procedure TTestOPLogger.HandleLog(Sender: TObject; aLevel: TLogKind; const aMsg: string);
begin
  FLastSender:=Sender;
  FLastLevel:=aLevel;
  FLastMessage:=aMsg;
  Inc(FEventCount);
end;

function TTestOPLogger.GetTempFileName: String;
begin
  Result:=GetTempDir + 'opf_test_log_' + IntToStr(GetProcessID) + '.log';
end;

procedure TTestOPLogger.SetUp;
begin
  inherited SetUp;
  FLogger:=TOPLogger.Create(Nil);
  FLastSender:=Nil;
  FLastLevel:=lkInfo;
  FLastMessage:='';
  FEventCount:=0;
end;

procedure TTestOPLogger.TearDown;
var
  FN: String;
begin
  FN:=FLogger.LogFileName;
  FreeAndNil(FLogger);
  if (FN <> '') and FileExists(FN) then
    DeleteFile(FN);
  inherited TearDown;
end;

procedure TTestOPLogger.TestSingletonPattern;
var
  Inst1, Inst2: TOPLogger;
begin
  TOPLogger.ClearInstance;
  Inst1:=TOPLogger.Instance;
  AssertNotNull('Instance should not be nil', Inst1);
  Inst2:=TOPLogger.Instance;
  AssertTrue('Instance should return same object', Inst1=Inst2);
  TOPLogger.ClearInstance;
  Inst2:=TOPLogger.Instance;
  AssertNotNull('New instance after clear should not be nil', Inst2);
  TOPLogger.ClearInstance;
end;

procedure TTestOPLogger.TestIsActiveNoChannels;
begin
  FLogger.LogToConsole:=False;
  FLogger.LogFileName:='';
  FLogger.OnLog:=Nil;
  AssertFalse('IsActive should be False when no channels enabled', FLogger.IsActive);
end;

procedure TTestOPLogger.TestIsActiveConsole;
begin
  FLogger.LogToConsole:=True;
  AssertTrue('IsActive should be True when console enabled', FLogger.IsActive);
end;

procedure TTestOPLogger.TestIsActiveFile;
begin
  FLogger.LogFileName:=GetTempFileName;
  AssertTrue('IsActive should be True when filename set', FLogger.IsActive);
end;

procedure TTestOPLogger.TestIsActiveEvent;
begin
  FLogger.OnLog:=@HandleLog;
  AssertTrue('IsActive should be True when OnLog assigned', FLogger.IsActive);
end;

procedure TTestOPLogger.TestIsKindActive;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.LogKinds:=[lkWarning, lkError, lkInfo];
  AssertTrue('Warning should be active', FLogger.IsKindActive(lkWarning));
  AssertTrue('Error should be active', FLogger.IsKindActive(lkError));
  AssertTrue('Info should be active', FLogger.IsKindActive(lkInfo));
  AssertFalse('Trace should not be active', FLogger.IsKindActive(lkTrace));
  AssertFalse('SQL should not be active', FLogger.IsKindActive(lkSQL));
end;

procedure TTestOPLogger.TestLogKindsDefault;
begin
  AssertTrue('Default includes lkWarning', lkWarning in FLogger.LogKinds);
  AssertTrue('Default includes lkError', lkError in FLogger.LogKinds);
  AssertTrue('Default includes lkInfo', lkInfo in FLogger.LogKinds);
  AssertFalse('Default excludes lkTrace', lkTrace in FLogger.LogKinds);
  AssertFalse('Default excludes lkSQL', lkSQL in FLogger.LogKinds);
end;

procedure TTestOPLogger.TestLogToFile;
var
  FN, Line: String;
  F: TextFile;
begin
  FN:=GetTempFileName;
  FLogger.LogFileName:=FN;
  FLogger.Log(nil, lkInfo, 'test file message');
  // Close file by clearing filename
  FLogger.LogFileName:='';
  // Read file and verify
  AssertTrue('Log file should exist', FileExists(FN));
  AssignFile(F, FN);
  Reset(F);
  try
    ReadLn(F, Line);
  finally
    CloseFile(F);
  end;
  AssertTrue('Line should contain Info', Pos('[Info]', Line) > 0);
  AssertTrue('Line should contain message', Pos('test file message', Line) > 0);
  DeleteFile(FN);
end;

procedure TTestOPLogger.TestLogOnEvent;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.Log(Self, lkWarning, 'event test');
  AssertEquals('Event should fire once', 1, FEventCount);
  AssertTrue('Sender should be Self', FLastSender=Self);
  AssertEquals('Level should be lkWarning', Ord(lkWarning), Ord(FLastLevel));
  AssertTrue('Message should contain event test', Pos('event test', FLastMessage) > 0);
end;

procedure TTestOPLogger.TestAddTimeStamp;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.AddTimeStamp:=False;
  FLogger.Log(nil, lkInfo, 'no ts');
  // Without timestamp, message starts with '['
  AssertTrue('Without timestamp, starts with [', FLastMessage[1] = '[');

  FLogger.AddTimeStamp:=True;
  FLogger.Log(nil, lkInfo, 'with ts');
  // With timestamp, message starts with a digit (year)
  AssertTrue('With timestamp, starts with digit', FLastMessage[1] in ['0'..'9']);
  // Should contain the T separator from ISO8601 format
  AssertTrue('With timestamp, contains date-time T separator', Pos('T', Copy(FLastMessage, 1, 20)) > 0);
end;

procedure TTestOPLogger.TestSenderFormatNil;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.Log(nil, lkInfo, 'nil sender');
  AssertTrue('Should contain (nil)', Pos('(nil)', FLastMessage) > 0);
end;

procedure TTestOPLogger.TestSenderFormatObject;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.Log(Self, lkInfo, 'obj sender');
  AssertTrue('Should contain class name', Pos('TTestOPLogger(', FLastMessage) > 0);
end;

procedure TTestOPLogger.TestLevelFiltering;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.LogKinds:=[lkWarning, lkError, lkInfo];
  FLogger.Log(nil, lkTrace, 'should not appear');
  AssertEquals('Trace filtered out, event count should be 0', 0, FEventCount);
  FLogger.Log(nil, lkInfo, 'should appear');
  AssertEquals('Info not filtered, event count should be 1', 1, FEventCount);
end;

procedure TTestOPLogger.TestConvenienceMethods;
begin
  FLogger.OnLog:=@HandleLog;
  FLogger.LogKinds:=[lkWarning, lkError, lkInfo, lkTrace];

  FLogger.LogInfo(Self, 'info msg');
  AssertEquals('LogInfo level', Ord(lkInfo), Ord(FLastLevel));

  FLogger.LogError(Self, 'error msg');
  AssertEquals('LogError level', Ord(lkError), Ord(FLastLevel));

  FLogger.LogWarning(Self, 'warn msg');
  AssertEquals('LogWarning level', Ord(lkWarning), Ord(FLastLevel));

  FLogger.LogTrace(Self, 'trace msg');
  AssertEquals('LogTrace level', Ord(lkTrace), Ord(FLastLevel));

  AssertEquals('Total events', 4, FEventCount);
end;

procedure TTestOPLogger.TestFileChannelToggle;
var
  FN1, FN2, Line: String;
  F: TextFile;
begin
  FN1:=GetTempFileName;
  FN2:=GetTempDir + 'opf_test_log2_' + IntToStr(GetProcessID) + '.log';
  // Write to first file
  FLogger.LogFileName:=FN1;
  FLogger.Log(nil, lkInfo, 'first file');
  // Switch to second file
  FLogger.LogFileName:=FN2;
  FLogger.Log(nil, lkInfo, 'second file');
  FLogger.LogFileName:='';
  // Verify first file
  AssertTrue('First log file should exist', FileExists(FN1));
  AssignFile(F, FN1);
  Reset(F);
  try
    ReadLn(F, Line);
  finally
    CloseFile(F);
  end;
  AssertTrue('First file should contain first file', Pos('first file', Line) > 0);
  // Verify second file
  AssertTrue('Second log file should exist', FileExists(FN2));
  AssignFile(F, FN2);
  Reset(F);
  try
    ReadLn(F, Line);
  finally
    CloseFile(F);
  end;
  AssertTrue('Second file should contain second file', Pos('second file', Line) > 0);
  DeleteFile(FN1);
  DeleteFile(FN2);
end;

initialization
  RegisterTest(TTestOPLogger);

end.
