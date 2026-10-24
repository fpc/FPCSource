program tw41950;

{$mode objfpc}{$h+}

uses
  SysUtils;

type
  TCountedException = class(Exception)
  public
    // Creates the exception and counts it as live
    constructor Create(const aMsg: string);
    // Removes the exception from the live count
    destructor Destroy; override;
  end;

const
  cScenarioCount = 8;
  cScenarioNames: array[1..cScenarioCount] of string = (
    'raise in finally',
    'raise in except',
    'leftover from handled finally raise',
    'raise in finally inside except',
    'acquired object below unhandled one',
    'handled raise in finally',
    'handled raise in finally inside except',
    'exception caught inside finally keeps the original');
  cExpectedExitCodes: array[1..cScenarioCount] of Integer = (217, 217, 217, 217, 217, 0, 0, 0);

var
  gLive: Integer = 0;
  gHeld: TObject = nil;
  gOldExitProc: CodePointer;

constructor TCountedException.Create(const aMsg: string);

begin
  inherited Create(aMsg);
  Inc(gLive);
end;


destructor TCountedException.Destroy;

begin
  Dec(gLive);
  inherited Destroy;
end;


// Reports the exception objects left at exit and sets the exit code to 1 when any remain
procedure CheckLive;

begin
  ExitProc:=gOldExitProc;
  if gHeld<>nil then
    begin
    gHeld.Free;
    gHeld:=nil;
    end;
  Writeln(StdOut,'live exception objects at exit: ',gLive);
  if gLive<>0 then
    ExitCode:=1;
end;


// Raises a counted exception with message aMsg
procedure RaiseCounted(const aMsg: string);

begin
  raise TCountedException.Create(aMsg);
end;


// Runs scenario aScenario, which ends with an unhandled exception for scenarios 1 to 5
procedure RunScenario(aScenario: Integer);

begin
  gOldExitProc:=ExitProc;
  ExitProc:=@CheckLive;
  case aScenario of
    1:
      try
        RaiseCounted('first');
      finally
        RaiseCounted('second');
      end;
    2:
      try
        RaiseCounted('first');
      except
        RaiseCounted('second');
      end;
    3:
      begin
      try
        try
          RaiseCounted('first');
        finally
          RaiseCounted('second');
        end;
      except
        on E: TCountedException do
          ;
      end;
      RaiseCounted('third');
      end;
    4:
      try
        RaiseCounted('first');
      except
        try
          RaiseCounted('second');
        finally
          RaiseCounted('third');
        end;
      end;
    5:
      try
        RaiseCounted('first');
      except
        gHeld:=TObject(AcquireExceptionObject);
        try
          RaiseCounted('second');
        finally
          RaiseCounted('third');
        end;
      end;
    6:
      try
        try
          RaiseCounted('first');
        finally
          RaiseCounted('second');
        end;
      except
        on E: TCountedException do
          Writeln(StdOut,'handled ',E.Message);
      end;
    7:
      try
        try
          RaiseCounted('first');
        except
          try
            RaiseCounted('second');
          finally
            RaiseCounted('third');
          end;
        end;
      except
        on E: TCountedException do
          Writeln(StdOut,'handled ',E.Message);
      end;
    8:
      try
        try
          RaiseCounted('first');
        finally
          try
            RaiseCounted('inner');
          except
            on E: TCountedException do
              Writeln(StdOut,'handled ',E.Message);
          end;
        end;
      except
        on E: TCountedException do
          begin
          Writeln(StdOut,'handled ',E.Message);
          if (E.Message<>'first') or (gLive<>1) then
            ExitCode:=2;
          end;
      end;
  end;
end;


// Runs every scenario in a child process and halts with 1 when one of them leaves exception objects behind
procedure RunAll;

var
  lCode, i: Integer;
  lFailed: Boolean;

begin
  lFailed:=False;
  for i:=1 to cScenarioCount do
    begin
    Writeln('=== scenario ',i,': ',cScenarioNames[i]);
    Flush(Output);
    lCode:=ExecuteProcess(ParamStr(0),IntToStr(i));
    Writeln('exit code ',lCode,', expected ',cExpectedExitCodes[i]);
    if lCode<>cExpectedExitCodes[i] then
      lFailed:=True;
    end;
  if lFailed then
    begin
    Writeln('a scenario did not end with the expected exit code');
    Halt(1);
    end;
  Writeln('ok');
end;


begin
  if ParamCount>0 then
    RunScenario(StrToInt(ParamStr(1)))
  else
    RunAll;
end.
