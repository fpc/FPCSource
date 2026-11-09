{ Tests of the event type names of TEventLog from eventlog }
unit utcEventLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, eventlog, punit;

// Registers the TEventLog suite
procedure RegisterTests;

implementation


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


procedure RegisterTests;

var
  lSuite: PSuite;

begin
  lSuite:=AddSuite('TEventLog');
  AddTest('EventTypeName', @TestEventTypeName, lSuite);
  AddTest('EventTypeToString', @TestEventTypeToString, lSuite);
end;

end.
