{ Source provided for Free Pascal Bug Report 3657 }
{ Submitted by "Sergey@michint" on  2005-02-15 }
{ e-mail:  }
// Title: Crash when text of tlabel is empty 
{$mode objfpc}
{$C+}
PROGRAM TestApp;
USES SysUtils, Objects, Video, Drivers, fvcommon, Views, App, Dialogs, FVConsts;

TYPE
   PTVDemo = ^TTVDemo;
   TTVDemo = OBJECT (TApplication)
      pc: Integer; // "program counter"
      l: PLabel;
      PROCEDURE Idle; virtual;
    End;


PROCEDURE TTVDemo.Idle; 
var
  Event: TEvent;
  R: TRect;
begin
  inherited;
  inc(pc); 
  case pc of  // test script
    1: begin
      R.Assign(5, 5, 10, 1);
      New(L, Init(R, '', nil));
      Desktop^.Insert(L); 
      Event.What:=evKeyDown; 
      Event.KeyCode:=kbAltC;
      Event.InfoPtr := Nil;
      PutEvent(Event);
    end;
    2: begin
      Event.What:=evCommand;
      Event.Command:=cmQuit;
      Event.InfoPtr := Nil;
      PutEvent(Event);
    end;
    3: Assert(False, 'Quit fail');
  end;
end;

var
  MyApp: TTVDemo;

BEGIN
  try  Assert(False, 'assert test');
    raise Exception.Create('assertions don''t not work');
  except on E: EAssertionFailed do {nothing} else raise; end;
  try
    MyApp.Init;                                        { Initialize app }
    MyApp.pc:=0;
    try
      MyApp.Run;                                         { Run the app }
    finally
      MyApp.Done;                                        { Dispose of app }
    end;
    Writeln('fv_label.pas - Ok');
  except
    Writeln('fv_label.pas - Error');
    raise;
  end;
END.