unit logmouse;

interface

Procedure StartMouseLogging;
Procedure StopMouseLogging;
Function  IsMouseLogging : Boolean;
Procedure SetMouseLogFileName(FileName : String);


implementation

uses sysutils,Mouse;

var
  NewMouseDriver,
  OldMouseDriver : TMouseDriver;
  Active,Logging : Boolean;
  LogFileName : String;
  MouseLog : Text;

Function TimeStamp : String;

begin
  TimeStamp:=FormatDateTime('hh:nn:ss',Time());
end;

Procedure StartMouseLogging;

begin
  Logging:=True;
  Writeln(MouseLog,'Start logging mouse events at: ',TimeStamp);
end;

Procedure StopMouseLogging;

begin
  Writeln(MouseLog,'Stop logging mouse events at: ',TimeStamp);
  Logging:=False;
end;

Function IsMouseLogging : Boolean;

begin
  IsMouseLogging:=Logging;
end;

Procedure LogGetMouseEvent(Var Event : TMouseEvent);

Var
  M : TMouseEvent;

begin
  OldMouseDriver.GetMouseEvent(M);
  If Logging then
    begin
    Write(MouseLog,TimeStamp,': Mouse ');
    With M do
      begin
      Case Action of
        MouseActionDown : Write(MouseLog,'down');
        MouseActionUp   : Write(MouseLog,'up');
        MouseActionMove : Write(MouseLog,'move');
      end;
      Write(MouseLog,' event at ',X,',',Y);
      If (Buttons<>0) then
        begin
        Write(MouseLog,' for buttons: ');
        If (Buttons and MouseLeftbutton)<>0 then
          Write(MouseLog,'Left ');
        If (Buttons and MouseRightbutton)<>0 then
          Write(MouseLog,'Right ');
        If (Buttons and MouseMiddlebutton)<>0 then
          Write(MouseLog,'Middle ');
        end;
      Writeln(MouseLog);
      end;
    end;
end;

Procedure LogInitMouse;

begin
  OldMouseDriver.InitDriver();
  Assign(MouseLog,logFileName);
  Rewrite(MouseLog);
  Active:=True;
  StartMouseLogging;
end;

Procedure LogDoneMouse;

begin
  StopMouseLogging;
  Close(MouseLog);
  Active:=False;
  OldMouseDriver.DoneDriver();
end;

Procedure SetMouseLogFileName(FileName : String);

begin
  If Not Active then
    LogFileName:=FileName;
end;

Initialization
  GetMouseDriver(OldMouseDriver);
  NewMouseDriver:=OldMouseDriver;
  NewMouseDriver.GetMouseEvent:=@LogGetMouseEvent;
  NewMouseDriver.InitDriver:=@LogInitMouse;
  NewMouseDriver.DoneDriver:=@LogDoneMouse;
  LogFileName:='Mouse.log';
  Logging:=False;
  SetMouseDriver(NewMouseDriver);
end.  