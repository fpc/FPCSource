{ Tests of the event type names and formatted log lines of TEventLog from eventlog }
unit utcEventLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, punit;

// Registers the TEventLog suite
procedure RegisterTests;

implementation

type

  { TLogRecorder }

  TLogRecorder = class
  public
    Lines: TStringList;
    Messages: TStringList;
    constructor Create;
    destructor Destroy; override;
    // Record aLine from OnFormattedLogMessage
    procedure DoFormatted(Sender: TObject; EventType: TEventType; const aLine: String);
    // Record Msg from OnLogMessage
    procedure DoMessage(Sender: TObject; EventType: TEventType; const Msg: String);
  end;

  { TBracketEventLog }

  TBracketEventLog = class(TEventLog)
  protected
    function FormatLogMessage(EventType: TEventType; const Msg: String): String; override;
  end;


constructor TLogRecorder.Create;

begin
  Lines:=TStringList.Create;
  Messages:=TStringList.Create;
end;


destructor TLogRecorder.Destroy;

begin
  Lines.Free;
  Messages.Free;
  inherited Destroy;
end;


procedure TLogRecorder.DoFormatted(Sender: TObject; EventType: TEventType; const aLine: String);

begin
  Lines.Add(aLine);
end;


procedure TLogRecorder.DoMessage(Sender: TObject; EventType: TEventType; const Msg: String);

begin
  Messages.Add(Msg);
end;


function TBracketEventLog.FormatLogMessage(EventType: TEventType; const Msg: String): String;

begin
  Result:='<'+EventTypeToString(EventType)+'> '+Msg;
end;


// Name of a file in the temporary directory, unique for this process
function TempLogName: String;

begin
  Result:=IncludeTrailingPathDelimiter(GetTempDir(False))+'utceventlog'+IntToStr(GetProcessID)+'.log';
end;


// Lines of file aFileName
function FileLines(const aFileName: String): TStringList;

begin
  Result:=TStringList.Create;
  Result.LoadFromFile(aFileName);
end;



function TestEventTypeName: TTestString;

begin
  Result:='';
  AssertEquals('Name of etInfo',SLogInfo,TEventLog.EventTypeName(etInfo));
  AssertEquals('Name of etWarning',SLogWarning,TEventLog.EventTypeName(etWarning));
  AssertEquals('Name of etError',SLogError,TEventLog.EventTypeName(etError));
  AssertEquals('Name of etDebug',SLogDebug,TEventLog.EventTypeName(etDebug));
  AssertEquals('Name of etCustom with custom type 42',Format(SLogCustom,[42]),TEventLog.EventTypeName(etCustom,42));
  AssertEquals('Name of etCustom without custom type',Format(SLogCustom,[0]),TEventLog.EventTypeName(etCustom));
end;


function TestEventTypeToString: TTestString;

var
  lLog: TEventLog;
  lType: TEventType;

begin
  Result:='';
  lLog:=TEventLog.Create(nil);
  try
    lLog.CustomLogType:=7;
    for lType:=Low(TEventType) to High(TEventType) do
      AssertEquals('EventTypeToString and EventTypeName agree for '+TEventLog.EventTypeName(lType,7),
        TEventLog.EventTypeName(lType,7),lLog.EventTypeToString(lType));
    AssertEquals('EventTypeToString uses CustomLogType for etCustom',Format(SLogCustom,[7]),lLog.EventTypeToString(etCustom));
  finally
    lLog.Free;
  end;
end;


function TestFormattedFileLog: TTestString;

var
  lLog: TEventLog;
  lRecorder: TLogRecorder;
  lFile: TStringList;
  lName: String;

begin
  Result:='';
  lName:=TempLogName;
  lFile:=nil;
  lRecorder:=TLogRecorder.Create;
  lLog:=TEventLog.Create(nil);
  try
    lLog.LogType:=ltFile;
    lLog.FileName:=lName;
    lLog.Identification:='utest';
    lLog.TimeStampFormat:='"TS"';
    lLog.OnFormattedLogMessage:=@lRecorder.DoFormatted;
    lLog.OnLogMessage:=@lRecorder.DoMessage;
    lLog.Info('first');
    lLog.Error('second %d',[2]);
    lLog.Active:=False;
    lFile:=FileLines(lName);
    AssertEquals('Two formatted events',2,lRecorder.Lines.Count);
    AssertEquals('Two lines in the file',2,lFile.Count);
    AssertEquals('First formatted line','utest [TS '+SLogInfo+'] first',lRecorder.Lines[0]);
    AssertEquals('First event line equals the first file line',lFile[0],lRecorder.Lines[0]);
    AssertEquals('Second event line equals the second file line',lFile[1],lRecorder.Lines[1]);
    AssertEquals('OnLogMessage still receives the message only','second 2',lRecorder.Messages[1]);
  finally
    lLog.Free;
    lRecorder.Free;
    lFile.Free;
    DeleteFile(lName);
  end;
end;


function TestOverriddenFormat: TTestString;

var
  lLog: TBracketEventLog;
  lRecorder: TLogRecorder;
  lFile: TStringList;
  lName: String;

begin
  Result:='';
  lName:=TempLogName;
  lFile:=nil;
  lRecorder:=TLogRecorder.Create;
  lLog:=TBracketEventLog.Create(nil);
  try
    lLog.LogType:=ltFile;
    lLog.FileName:=lName;
    lLog.OnFormattedLogMessage:=@lRecorder.DoFormatted;
    lLog.Warning('careful');
    lLog.Active:=False;
    lFile:=FileLines(lName);
    AssertEquals('Overridden format in the file','<'+SLogWarning+'> careful',lFile[0]);
    AssertEquals('Overridden format in the event','<'+SLogWarning+'> careful',lRecorder.Lines[0]);
  finally
    lLog.Free;
    lRecorder.Free;
    lFile.Free;
    DeleteFile(lName);
  end;
end;


function TestFormattedStdOutLog: TTestString;

var
  lLog: TEventLog;
  lRecorder: TLogRecorder;
  lFile: TStringList;
  lName: String;
  lSaved: Text;

begin
  Result:='';
  lName:=TempLogName;
  lFile:=nil;
  lSaved:=StdOut;
  lRecorder:=TLogRecorder.Create;
  lLog:=TEventLog.Create(nil);
  try
    AssignFile(StdOut,lName);
    Rewrite(StdOut);
    try
      lLog.LogType:=ltStdOut;
      lLog.Identification:='utest';
      lLog.TimeStampFormat:='"TS"';
      lLog.OnFormattedLogMessage:=@lRecorder.DoFormatted;
      lLog.Debug('to stdout');
      lLog.Active:=False;
    finally
      CloseFile(StdOut);
      StdOut:=lSaved;
    end;
    lFile:=FileLines(lName);
    AssertEquals('One line written to standard output',1,lFile.Count);
    AssertEquals('Event line equals the standard output line',lFile[0],lRecorder.Lines[0]);
    AssertEquals('Standard output line','utest [TS '+SLogDebug+'] to stdout',lFile[0]);
  finally
    lLog.Free;
    lRecorder.Free;
    lFile.Free;
    DeleteFile(lName);
  end;
end;


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TEventLog');
  AddTest('EventTypeName', @TestEventTypeName, lSuite);
  AddTest('EventTypeToString', @TestEventTypeToString, lSuite);
  AddTest('FormattedFileLog', @TestFormattedFileLog, lSuite);
  AddTest('OverriddenFormat', @TestOverriddenFormat, lSuite);
  AddTest('FormattedStdOutLog', @TestFormattedStdOutLog, lSuite);
end;

end.
