program demostatuses;
{$codepage utf8}
uses
  {$ifdef UNIX}cwstring,{$endif}
  //Objects, Drivers, Views, Menus, Dialogs, App, Gadgets, Statuses, Time, fvconsts, FVCommon; { for legacy uncomment this line and comment next line }
  Objects, UDrivers, uViews, uMenus, uDialogs, uApp, uGadgets, uStatuses, Time, fvconsts, UFVCommon; { for unicode support uncomment this line and comment previous line }

const cmOpenGaugeWindow          =22351;
      cmOpenArrowGaugeWindow     =22352;
      cmOpenArrowBackGaugeWindow =22353;
      cmOpenSpinnerGaugeWindow   =22354;
      cmOpenPercentGaugeWindow   =22355;
      cmOpenBarGaugeWindow       =22356;
      cmStatusUp = 19883;

type
  PClockViewCount= ^TClockViewCount;
  TClockViewCount = object(TClockView)
    PROCEDURE Update; Virtual;
  end;

  PStatusesApp = ^TStatusesApp;
  TStatusesApp = object(TApplication)
    Clock: PClockViewCount;
    Heap: PHeapView;
    constructor Init;
    procedure Idle; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
    procedure InitMenuBar; virtual;
  end;

PROCEDURE TClockViewCount.Update;
VAR Hour, Min, Sec, Sec100: Word;
begin
  GetTime(Hour, Min, Sec, Sec100);                   { Get current time }
  If (Abs(Sec - LastTime) >= Refresh) Then Begin     { Refresh time elapsed }
     inherited;
     Message(Owner,evStatus,cmStatusUpdate, pointer(ptruint(cmStatusUp)));   { tell the world that second has passed }
  end;
end;

constructor TStatusesApp.Init;
var
  R: TRect;
begin
  Inherited Init;
  { Initialize demo Statuses }

  GetExtent(R);
  R.A.X := R.B.X - 9; R.B.Y := R.A.Y + 1;
  Clock := New(PClockViewCount, Init(R));
  Clock^.GrowMode:=gfGrowLoX+gfGrowHiX;
  Insert(Clock);

  GetExtent(R);
  Dec(R.B.X);
  R.A.X := R.B.X - 9; R.A.Y := R.B.Y - 1;
  Heap := New(PHeapView, Init(R));
  Heap^.GrowMode:=gfGrowAll;
  Insert(Heap);
end;

procedure TStatusesApp.Idle;
begin
  TApplication.Idle;
  Clock^.Update;
  Heap^.Update;
end;

procedure TStatusesApp.InitMenuBar;
var
  R: TRect;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  MenuBar := New(PMenubar, Init(R, NewMenu(NewSubMenu('~M~enu',  hcNoContext,
    NewMenu(
    NewItem('~G~auge', 'F2', kbF2, cmOpenGaugeWindow, hcNoContext,
    NewItem('~A~rrow gauge', 'F3', kbF3, cmOpenArrowGaugeWindow, hcNoContext,
    NewItem('Back a~r~row gauge', 'F4', kbF4, cmOpenArrowBackGaugeWindow, hcNoContext,
    NewItem('~S~pinner gauge', 'F5', kbF5, cmOpenSpinnerGaugeWindow, hcNoContext,
    NewItem('~P~ercente gauge', 'F6', kbF6, cmOpenPercentGaugeWindow, hcNoContext,
    NewItem('~B~ar gauge', 'F7', kbF7, cmOpenBarGaugeWindow, hcNoContext,
    nil)))))))
    ,
    nil))));
end;

procedure TStatusesApp.HandleEvent(var Event: TEvent);

procedure StatusesDlg( aChoice : byte);
var
  D: PDialog;
  G: PStatus;
  R : TRect;
  Title: Sw_String;
begin
  R.Assign(13, 7, 55, 8);
  case aChoice of
    1: G := new(PGauge,Init(R,cmStatusUp,1,12));
    2: G := new(PArrowGauge,Init(R,cmStatusUp,1,12,true));
    3: G := new(PArrowGauge,Init(R,cmStatusUp,1,12,false));
    4: G := new(PSpinnerGauge,Init(13,7,cmStatusUp));
    5: G := new(PPercentGauge,Init(R,cmStatusUp,1,12));
    6: G := new(PBarGauge,Init(R,cmStatusUp,1,12));
    else
      exit; { none chosen }
  end;
  Title:='12 seconds to pass';
  if aChoice = 4 then
     Title:='Infinite seconds to pass';

{$if sizeof(sw_string)<=8}
  D := New(PStatusMessageDlg,Init(Title, G , sdPauseButton or sdCancelButton,#3'Unicode symbols ◀ ◌ ◂ ◃ ◄ ◅ ◆ ◇ ◈ ◉ ◊'));
{$else}
  D := New(PStatusMessageDlg,Init(Title, G , sdPauseButton or sdCancelButton,#3'Legacy gauge'));
{$endif}
  if ExecuteDialog(D, nil) <> cmCancel then
  begin
    { task complete }

  end;
end;

begin
  inherited HandleEvent(Event);
  case Event.What of
    evCommand:
      begin
        case Event.Command of
          cmOpenGaugeWindow          : StatusesDlg(1);
          cmOpenArrowGaugeWindow     : StatusesDlg(2);
          cmOpenArrowBackGaugeWindow : StatusesDlg(3);
          cmOpenSpinnerGaugeWindow   : StatusesDlg(4);
          cmOpenPercentGaugeWindow   : StatusesDlg(5);
          cmOpenBarGaugeWindow       : StatusesDlg(6);
        else
          Exit;
        end;
        ClearEvent(Event);
      end;
  end;
end;

var
  StatusesApp: TStatusesApp;
begin
  StatusesApp.Init;
  StatusesApp.Run;
  StatusesApp.Done;
end.
