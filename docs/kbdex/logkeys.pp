unit logkeys;

interface

Procedure StartKeyLogging;
Procedure StopKeyLogging;
Function  IsKeyLogging : Boolean;
Procedure  SetKeyLogFileName(FileName : String);


implementation

uses sysutils,keyboard;

var
  NewKeyBoardDriver,
  OldKeyBoardDriver : TKeyboardDriver;
  Active,Logging : Boolean;
  LogFileName : String;
  KeyLog : Text;

Function TimeStamp : String;

begin
  TimeStamp:=FormatDateTime('hh:nn:ss',Time());
end;

Procedure StartKeyLogging;

begin
  Logging:=True;
  Writeln(KeyLog,'Start logging keystrokes at: ',TimeStamp);
end;

Procedure StopKeyLogging;

begin
  Writeln(KeyLog,'Stop logging keystrokes at: ',TimeStamp);
  Logging:=False;
end;

Function IsKeyLogging : Boolean;

begin
  IsKeyLogging:=Logging;
end;

Function LogGetKeyEvent : TKeyEvent;

Var
  K : TKeyEvent;

begin
  K:=OldkeyboardDriver.GetKeyEvent();
  If Logging then
    begin
    Write(KeyLog,TimeStamp,': Key event: ');
    Writeln(KeyLog,KeyEventToString(TranslateKeyEvent(K)));
    end;
  LogGetKeyEvent:=K;
end;

Procedure LogInitKeyBoard;

begin
  OldKeyBoardDriver.InitDriver();
  Assign(KeyLog,logFileName);
  Rewrite(KeyLog);
  Active:=True;
  StartKeyLogging;
end;

Procedure LogDoneKeyBoard;

begin
  StopKeyLogging;
  Close(KeyLog);
  Active:=False;
  OldKeyBoardDriver.DoneDriver();
end;

Procedure SetKeyLogFileName(FileName : String);

begin
  If Not Active then
    LogFileName:=FileName;
end;

Initialization
  GetKeyBoardDriver(OldKeyBoardDriver);
  NewKeyBoardDriver:=OldKeyBoardDriver;
  NewKeyBoardDriver.GetKeyEvent:=@LogGetKeyEvent;
  NewKeyBoardDriver.InitDriver:=@LogInitKeyboard;
  NewKeyBoardDriver.DoneDriver:=@LogDoneKeyboard;
  LogFileName:='keyboard.log';
  Logging:=False;
  SetKeyboardDriver(NewKeyBoardDriver);
end.  