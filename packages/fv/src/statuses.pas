{$V-}
unit Statuses;

{$CODEPAGE cp437}

{#Z+}
{  Free Vision Status Objects Unit
   Free VIsion
   Written by : Brad Williams, DVM

Revision History

1.2.3   (96/04/13)
  - moved Pause and Resume to methods of TStatus leaving TStatus Pause and
    Resume "aware"
  - eliminated many bugs
  - moved Pause, Resume and Cancel from TStatusDlg to TStatus

1.2.1    (95/12/6)
   - minor typo corrections in opening unit documentation
   - F+ to Z+ around stream registration records
   - removed redundant sentence in TAppStatus definition
   - updated CBarStatus documentation and constant
   - removed TGauge.Init cross-reference from TSpinner.Init
   - added THeapMemAvail and RegistertvStatus documentation
   - numerous other documentation updates
   - changed all calls to Send to Message

1.2.0    (95/11/24)
   - conversion to Bsd format

1.1.0    (05/01/94)
   - initial WVS release


Known Bugs

ScanHelp Errors
   - sdXXXX constants help documentation doesn't show TStatusDlg and
     TMessageStatusDlg
   - ScanHelp produces garbage in evStatus help context

tvStatus Bugs
   - CAppStatus may not be correct }
{#Z-}

{ The tvStatus unit implements several views for providing information to
the user which needs to be updated during program execution, such as a
progress indicator, clock, heap viewer, gauges, etc.  All tvStatus views
respond to a new message event class, evStatus.  An individual status view
only processes an event with its associated command. }

interface

{$i platform.inc}

{$ifdef PPC_FPC}
  {$H-}
{$else}
  {$F+,O+,E+,N+}
{$endif}
{$X+,R-,I-,Q-,V-}
{$ifndef OS_UNIX}
  {$S-}
{$endif}

uses

  FVCommon, FVConsts, Objects, Drivers, Views, Dialogs;
{  Resource;}

const

  evStatus = $8000;
    { evStatus represents the event class all status views know how to
      respond to. }
    {#X Statuses }


  CStatus    =  #1#2#3;
{$ifndef cdPrintDoc}
{#F+}
{ÝTStatus.CStatus palette
ßßßßßßßßßßßßßßßßßßßßßßßßß}
{#F-}
{$endif cdPrintDoc}
{ Status views use the default palette, CStatus, to map onto the first three
entries in the standard window palette. }
{#F+}
{              1    2    3
           ÉÍÍÍÍÑÍÍÍÍÑÍÍÍÍ»
 CStatus   º  1 ³  2 ³  3 º
           ÈÍÍÑÍÏÍÍÑÍÏÍÍÑÍ¼
Normal TextÄÄÄÙ    ³    ³
OtherÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ³
Highlighted TextÄÄÄÄÄÄÄÄÙ }
{#F-}
{#X TStatus }

  CAppStatus =  #2#5#4;
{$ifndef cdPrintDoc}
{#F+}
{ÝTAppStatus.CAppStatus palette
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß}
{#F-}
{$endif cdPrintDoc}
{ Status views which are inserted into the application rather than a dialog
or window use the default palette, CAppStatus, to map onto the application
object's palette. }
{#F+}
{                 1    2    3
              ÉÍÍÍÍÑÍÍÍÍÑÍÍÍÍ»
 CAppStatus   º  2 ³  5 ³  4 º
              ÈÍÍÑÍÏÍÍÑÍÏÍÍÑÍ¼
Normal TextÄÄÄÄÄÄÙ    ³    ³
OtherÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ³
Highlighted TextÄÄÄÄÄÄÄÄÄÄÄÙ }
{#F-}
    {#X tvStatus TAppStatus }


  CBarGauge = CStatus + #16#19;
{$ifndef cdPrintDoc}
{#F+}
{ÝTBarGauge.CBarGauge palette
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßß}
{#F-}
{$endif cdPrintDoc}
{ TBarGauge's use the default palette, CBarGauge, to map onto the dialog or
window owner's palette. }
{#F+}
{                 1    2    3   4    5
              ÉÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍÑÍÍÍÍ»
 CAppStatus   º  2 ³  5 ³  4 ³ 16 ³ 19 º
              ÈÍÍÑÍÏÍÍÑÍÏÍÍÑÍÏÍÍÑÍÏÍÍÑÍ¼
Normal TextÄÄÄÄÄÄÙ    ³    ³    ³    ÀÄÄÄÄ filled in bar
OtherÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    ³    ÀÄÄÄÄÄÄÄÄÄ empty bar
Highlighted TextÄÄÄÄÄÄÄÄÄÄÄÙ }
{#F-}
    {#X tvStatus TBarGauge }


{#T sdXXXX }
{$ifndef cdPrintDoc}
{#F+}
{ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
Ý sdXXXX constants   (STDDLG unit) Þ
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß}
{#F-}
{$endif cdNoPrintDoc}
{ sdXXXX constants are used to determine the types of buttons displayed in a
#TStatusDlg# or #TStatusMessageDlg#. }
{#F+}
{    Constant      ³ Value ³ Meaning
ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ
  sdNone          ³ $0000 ³ no buttons
  sdCancelButton  ³ $0001 ³ show Cancel button
  sdPauseButton   ³ $0002 ³ show Pause button
  sdResumeButton  ³ $0004 ³ show Resume button
  sdAllButtons    ³ $0008 ³ show Cancel, Pause and Resume
                  ³       ³   buttons }
{#Z+}
  sdNone                 = $0000;
  sdCancelButton         = $0001;
  sdPauseButton          = $0002;
  sdResumeButton         = $0004;
  sdAllButtons           = sdCancelButton or sdPauseButton or sdResumeButton;
{#Z-}
  {#X tvStatus TStatusDlg TStatusMessageDlg }

  SpinChars : String[4] = '³/Ä\';
    { SpinChars are the characters used by a #TSpinnerGauge# when it is drawn.
      Only one character is displayed at a time.  The string is cycled
      through then started over again until the view is disposed. }
    {#X tvStatus }

  sfPause = $F000;
    { sfPause is an additional state flag used internally by status views to
      indicate they are in a paused state and should not respond to their
      command. }

type
  {#Z+}
  PStatus = ^TStatus;
  {#Z-}
  TStatus = Object(TParamText)
    { TStatus is the base object type from which all status views descend.
      Status views are used to display information that will change at
      run-time based upon some state or process in the application, such as
      printing.

      All status views that are to be inserted into the application should
      descend from #TAppStatus# for proper color mapping. }
    Command : Word;
      { Command is the only command the status view will respond to.  When
        the status view receives an evStatus event it checks the value of the
        Event.Command field against Command before handling the event. }
      {#X HandleEvent }
    constructor Init (R : TRect; ACommand : Word; AText : String;
                      AParamCount : Integer);
      { Init calls the inherited constructor then sets #Command# to ACommand.

        If an error occurs Init fails. }
      {#X Load }
    constructor Load (var S : TStream);
      { Load calls the inherited constructor then reads #Command# from the
        stream.

        If an error occurs Load fails. }
      {#X Store Init }
    function Cancel : Boolean; virtual;
      { Cancel should prompt the user when necessary for validation of
        canceling the process which the status view is displaying.  If the
        user elects to continue the process Cancel must return False,
        otherwise Cancel must return True. }
      {#X Pause Resume }
    function GetPalette : PPalette; virtual;
      { GetPalette returns a pointer to the default status view palette,
        #CStatus#. }
      {#X TAppStatus CAppStatus }
    procedure HandleEvent (var Event : TEvent); virtual;
      { HandleEvent captures any #evStatus# messages with its command value
        equal to #Command#, then calls #Update# with Data set to
        Event.InfoPtr.  If the State field has its #sfPause# bit set, the
        view ignores the event. }
    procedure Pause; virtual;
      { Pause sends an evStatus message to the application with Event.Command
        set to cmStatusPause and Event.InfoPtr set to #Status#^.Command.  The
        #Status# view's sfPause bit of the State flag is set by calling
        SetState.  In the paused state, the status view does not respond to
        its associated command. }
      {#X Resume sdXXXX Cancel }
    procedure Reset; virtual;
      { Reset causes the status view to be reset to its beginning or default
        value, then be redrawn.  Reset is used after an event is aborted
        which can only be performed in its entirety. }
    procedure Resume; virtual;
      { Resume is called in response to pressing the Resume button.  Resume
        sends an evStatus message to the application with Event.Command set
        to cmStatusPause and Event.InfoPtr set to #Status#^.Command.  The
        Status view's sfPause bit is turned off by calling SetState. }
      {#X Pause sdXXXX Cancel }
    procedure Store (var S : TStream); { store should never be virtual;}
      { Store calls the inherited Store method then writes #Command# to the
        stream. }
      {#X Load }
    procedure Update (Data : Pointer); virtual;
      { Update changes the status' displayed text as necessary based on
        Data. }
      {#X Command HandleEvent }
  end;  { of TStatus }


  {#Z+}
  PStatusDlg = ^TStatusDlg;
  {#Z-}
  TStatusDlg = Object(TDialog)
    { A TStatusDlg displays a status view and optional buttons.  It may be
      used to display any status message and optionally provide end user
      cancelation or pausing of an ongoing operation, such as printing.

      All status views that are to be inserted into a window or dialog should
      descend from #TStatus# for proper color mapping. }
    Status : PStatus;
      { Status is the key status view for the dialog.  When a cmStatusPause
        command is broadcast in response to pressing the pause button,
        Event.InfoPtr is set to point to the command associated with Status. }
      {#X TStatus cmXXXX }
    constructor Init (ATitle : TTitleStr; AStatus : PStatus; AFlags : Word);
      { Init calls the inherited constructor to create the dialog and sets
        the EventMask to handle #evStatus# events.  AStatus is assigned to
        #Status# and inserted into the dialog at position 2,2.

        The dialog is anchored at AStatus^.Origin and its size is at least
        AStatus^.Size + 2 in both dimensions.  The actual size is determined
        by the AFlags byte.  The #sdXXXX# constants should be used to signify
        which buttons to display.

        If an error occurs Init fails. }
      {#X TStatus.Pause TStatus.Resume }
    constructor Load (var S : TStream);
      { Load calls the inherited constructor then loads #Status#.

        If an error occurs Load fails. }
      {#X Store }
    procedure Cancel (ACommand : Word); virtual;
      { Cancel sends an evStatus message to the Application object with
        command set to cmCancel and InfoPtr set to the calling status view's
        command, then calls the inherited Cancel method. }
      {#X TBSDDialog.Cancel }
    procedure HandleEvent (var Event : TEvent); virtual;
      { All evStatus events are accepted by the dialog and sent to each
        subview in Z-order until cleared.

        If the dialog recieves an evCommand or evBroadcast event with the
        Command parameter set to cmCancel, HandleEvent sends an #evStatus#
        message to the Application variable with Event.Command set to the
        cmStatusCancel and Event.InfoPtr set to the #Status#.Command and
        disposes of itself.

        When a pause button is included, a cmStatusPause broadcast event is
        associated with the button.  When the button is pressed a call to
        #TStatus.Pause# results.  The status view is inactivated until it
        receives an evStatus event with a commond of cmStatusResume and
        Event.InfoPtr set to the status view's Command value.  When a pause
        button is used, the application should respond to the evStatus event
        (with Event.Command of cmStatusPause) appropriately, then dispatch a
        cmStatusResume evStatus event when ready to resume activity. }
      {#X TStatus.Command }
    procedure InsertButtons (AFlags : Word); virtual;
      { InsertButtons enlarges the dialog to the necessary size and inserts
        the buttons specified in AFlags into the last row of the dialog. }
    procedure Store (var S : TStream); { store should never be virtual;}
      { Store calls the inherited Store method then writes #Status# to the
        stream. }
      {#X Load }
  end;  { of TStatusDlg }


  {#Z+}
  PStatusMessageDlg = ^TStatusMessageDlg;
  {#Z-}
  TStatusMessageDlg = Object(TStatusDlg)
    { A TStatusMessageDlg displays a message as static text with a status
      view on the line below it.

      All status views that are to be inserted into a window or dialog should
      descend from #TStatus# for proper color mapping. }
    constructor Init (ATitle : TTitleStr; AStatus : PStatus; AFlags : Word;
                      AMessage : String);
      { Init calls the inherited constructor then inserts a TStaticText view
        containing AMessage at the top line of the dialog.

        The size of the dialog is determined by the size of the AStatus.  The
        dialog is anchored at AStatus^.Origin and is of at least
        AStatus^.Size + 2 in heighth and width.  The exact width and heighth
        are determined by AOptions.

        AFlags contains flags which determine the buttons to be displayed
        in the dialog.

        If an error occurs Init fails. }
  end;  { of TStatusMessageDlg }


  {#Z+}
  PGauge = ^TGauge;
  {#Z-}
  TGauge = Object(TStatus)
    { A gauge is used to represent the current numerical position within a
      range of values.  When Current equals Max a gauge dispatches an
      #evStatus# event with the command set to cmStatusDone to the
      Application object. }
    Min : LongInt;
      { Min is the minimum value which #Current# may be set to. }
      {#X Max }
    Max : LongInt;
      { Max is the maximum value which #Current# may be set to. }
      {#X Min }
    Current : LongInt;
      { Current is the current value represented in the gauge. }
      {#X Max Min }
    constructor Init (R : TRect; ACommand : Word; AMin, AMax : LongInt);
      { Init calls the inherited constructor then sets #Min# and #Max# to
        AMin and AMax, respectively.  #Current# is set to AMin.

        If an error occurs Init fails. }
      {#X Load }
    constructor Load (var S : TStream);
      { Load calls the inherited constructor then reads #Min#, #Max# and
        #Current# from the stream.

        If an error occurs Load fails. }
      {#X Init Store }
    procedure Draw; virtual;
      { Draw writes the following to the screen: }
{#F+}
{
Min = XXX  Max = XXX  Current = XXX }
{#F-}
      { where XXX are the current values of the corresponding variables. }
    procedure GetData (var Rec); virtual;
      { GetData assumes Rec is a #TGaugeRec# and returns the current settings
        of the gauge. }
      {#X SetData }
    procedure Reset; virtual;
      { Reset sets #Current# to #Min# then redraws the status view. }
      {#X TStatus.Reset }
    procedure SetData (var Rec); virtual;
      { SetData assumes Rec is a #TGaugeRec# and sets the gauge's variables
        accordingly. }
      {#X GetData }
    procedure Store (var S : TStream); { store should never be virtual;}
      { Store calls the inherited Store method then writes #Min#, #Max# and
        #Current# to the stream. }
      {#X Load }
    procedure Update (Data : Pointer); virtual;
      { Update increments #Current#. }
  end;  { of TGauge }


  {#Z+}
  PGaugeRec = ^TGaugeRec;
  {#Z-}
  TGaugeRec = record
    { A TGaugeRec is used to set and get a #TGauge#'s variables. }
    {#X TGauge.GetData TGauge.SetData }
    Min, Max, Current : LongInt;
  end;  { of TGaugeRec }


  {#Z+}
  PArrowGauge = ^TArrowGauge;
  {#Z-}
  TArrowGauge = Object(TGauge)
    { An arrow gauge draws a progressively larger series of arrows across the
      view.  If Right is True, the arrows are right facing, '>', and are
      drawn from left to right.  If Right is False, the arrows are left
      facing, '<', and are drawn from right to left. }
    Right : Boolean;
      { Right determines the direction of arrow used and the direction which
        the status view is filled.  If Right is True, the arrows are right
        facing, '>', and are drawn from left to right.  If Right is False,
        the arrows are left facing, '<', and are drawn from right to left. }
      {#X Draw }
    constructor Init (R : TRect; ACommand : Word; AMin, AMax : Word;
                      RightArrow : Boolean);
      { Init calls the inherited constructor then sets #Right# to RightArrow.

        If an error occurs Init fails. }
      {#X Load }
    constructor Load (var S : TStream);
      { Load calls the inherited constructor then reads #Right# from the
        stream.

        If an error occurs Load fails. }
      {#X Init Store }
    procedure Draw; virtual;
      { Draw fills the Current / Max percent of the view with arrows. }
      {#X Right }
    procedure GetData (var Rec); virtual;
      { GetData assumes Rec is a #TArrowGaugeRec# and returns the current
        settings of the views variables. }
      {#X SetData }
    procedure SetData (var Rec); virtual;
      { SetData assumes Rec is a #TArrowGaugeRec# and sets the view's
        variables accordingly. }
      {#X GetData }
    procedure Store (var S : TStream); { store should never be virtual;}
      { Store calls the inherited Store method then writes #Right# to the
        stream. }
      {#X Load }
  end;  { of TArrowGauge }


  {#Z+}
  PArrowGaugeRec = ^TArrowGaugeRec;
  {#Z-}
  TArrowGaugeRec = record
    { A TArrowGaugeRec is used to set and get the variables of a
      #TArrowGauge#. }
    {#X TArrowGauge.GetData TArrowGauge.SetData }
    Min, Max, Count : LongInt;
    Right : Boolean;
  end;  { of TGaugeRec }


  {#Z+}
  PPercentGauge = ^TPercentGauge;
  {#Z-}
  TPercentGauge = Object(TGauge)
    { A TPercentGauge displays a numerical percentage as returned by
      #Percent# followed by a '%' sign. }
    function Percent : Integer; virtual;
      { Percent returns the whole number value of (Current / Max) * 100. }
      {#X TGauge.Current TGauge.Max }
    procedure Draw; virtual;
      { Draw writes the current percentage to the screen. }
      {#X Percent }
  end;  { of TPercentGauge }


  {#Z+}
  PBarGauge = ^TBarGauge;
  {#Z-}
  TBarGauge = Object(TPercentGauge)
    { A TBarGauge displays a bar which increases in size from the left to
      the right of the view as Current increases.  A numeric percentage
      representing the value of (Current / Max) * 100 is displayed in the
      center of the bar. }
    {#x TPercentGauge.Percent }
    procedure Draw; virtual;
      { Draw draws the bar and percentage to the screen representing the
        current status of the view's variables. }
      {#X TGauge.Update }
    function GetPalette : PPalette; virtual;
      { GetPalette returns a pointer to the default status view palette,
        #CBarStatus#. }
  end;  { of TBarGauge }


  {#Z+}
  PSpinnerGauge = ^TSpinnerGauge;
  {#Z-}
  TSpinnerGauge = Object(TGauge)
    { A TSpinnerGauge displays a series of characters in one spot on the
      screen giving the illusion of a spinning line. }
    constructor Init (X, Y : Integer; ACommand : Word);
      { Init calls the inherited constructor with AMin set to 0 and AMax set
        to 4. }
    procedure Draw; virtual;
      { Draw uses the #SpinChars# variable to draw the view's Current
        character. }
      {#X Update }
    procedure HandleEvent (var Event : TEvent); virtual;
      { HandleEvent calls TStatus.HandleEvent so that a cmStatusDone event
        is not generated when Current equals Max. }
      {#X TGauge.Current TGauge.Max }
    procedure Update (Data : Pointer); virtual;
      { Update increments Current until Current equals Max, when it resets
        Current to Min. }
      {#X Draw HandleEvent }
  end;  { of TSpinnerGauge }


  {#Z+}
  PAppStatus = ^TAppStatus;
  {#Z-}
  TAppStatus = Object(TStatus)
    { TAppStatus is a base object which implements color control for status
      views that are normally inserted in the Application object. }
    {#X TStatus }
    function GetPalette : PPalette; virtual;
      { GetPalette returns a pointer to the default application status view
        palette, #CAppStatus#. }
      {#X TStatus CStatus }
  end;  { of TAppStatus }


  {#Z+}
  PHeapMaxAvail = ^THeapMaxAvail;
  {#Z-}
  THeapMaxAvail = Object(TAppStatus)
    { A THeapMaxAvail displays the largest available contiguous area of heap
      memory.  It responds to a cmStatusUpdate event by calling MaxAvail and
      comparing the result to #Max#, then updating the view if necessary. }
    {#X THeapMemAvail }
    constructor Init (X, Y : Integer);
      { Init creates the view with the following text:

        MaxAvail = xxxx

        where xxxx is the result returned by MaxAvail. }
    procedure Update (Data : Pointer); virtual;
      { Update changes #Mem# to the current MemAvail and redraws the status
        if necessary. }
      private
    Max : LongInt;
      { Max is the last reported value from MaxAvail. }
      {#X Update }
  end;  { of THeapMaxAvail }


  {#Z+}
  PHeapMemAvail = ^THeapMemAvail;
  {#Z-}
  THeapMemAvail = Object(TAppStatus)
    { A THeapMemAvail displays the total amount of heap memory available to
      the application.  It responds to a cmStatusUpdate event by calling
      MemAvail and comparing the result to #Max#, then updating the view if
      necessary. }
    {#X THeapMaxAvail }
    constructor Init (X, Y : Integer);
      { Init creates the view with the following text:

        MemAvail = xxxx

        where xxxx is the result returned by MemAvail. }
      {#X Load }
    procedure Update (Data : Pointer); virtual;
      { Update changes #Mem# to the current MemAvail and redraws the status
        if necessary. }
      private
    Mem : LongInt;
      { Mem is the last available value reported by MemAvail. }
      {#X Update }
  end;  { of THeapMemAvail }


{$ifndef cdPrintDoc}
{#Z+}
{$endif cdPrintDoc}
const
  RStatus    : TStreamRec = (
     ObjType : idStatus;
     VmtLink : Ofs(TypeOf(TStatus)^);
     Load    : @TStatus.Load;
     Store   : @TStatus.Store);

  RStatusDlg : TStreamRec = (
     ObjType : idStatusDlg;
     VmtLink : Ofs(TypeOf(TStatusDlg)^);
     Load    : @TStatusDlg.Load;
     Store   : @TStatusDlg.Store);

  RStatusMessageDlg : TStreamRec = (
     ObjType : idStatusMessageDlg;
     VmtLink : Ofs(TypeOf(TStatusMessageDlg)^);
     Load    : @TStatusMessageDlg.Load;
     Store   : @TStatusMessageDlg.Store);

  RGauge  : TStreamRec = (
     ObjType : idGauge;
     VmtLink : Ofs(TypeOf(TGauge)^);
     Load    : @TGauge.Load;
     Store   : @TGauge.Store);

  RArrowGauge  : TStreamRec = (
     ObjType : idArrowGauge;
     VmtLink : Ofs(TypeOf(TArrowGauge)^);
     Load    : @TArrowGauge.Load;
     Store   : @TArrowGauge.Store);

  RBarGauge  : TStreamRec = (
     ObjType : idBarGauge;
     VmtLink : Ofs(TypeOf(TBarGauge)^);
     Load    : @TBarGauge.Load;
     Store   : @TBarGauge.Store);

  RPercentGauge  : TStreamRec = (
     ObjType : idPercentGauge;
     VmtLink : Ofs(TypeOf(TPercentGauge)^);
     Load    : @TPercentGauge.Load;
     Store   : @TPercentGauge.Store);

  RSpinnerGauge  : TStreamRec = (
     ObjType : idSpinnerGauge;
     VmtLink : Ofs(TypeOf(TSpinnerGauge)^);
     Load    : @TSpinnerGauge.Load;
     Store   : @TSpinnerGauge.Store);

  RAppStatus  : TStreamRec = (
     ObjType : idAppStatus;
     VmtLink : Ofs(TypeOf(TAppStatus)^);
     Load    : @TAppStatus.Load;
     Store   : @TAppStatus.Store);

  RHeapMinAvail  : TStreamRec = (
     ObjType : idHeapMinAvail;
     VmtLink : Ofs(TypeOf(THeapMaxAvail)^);
     Load    : @THeapMaxAvail.Load;
     Store   : @THeapMaxAvail.Store);

  RHeapMemAvail  : TStreamRec = (
     ObjType : idHeapMemAvail;
     VmtLink : Ofs(TypeOf(THeapMemAvail)^);
     Load    : @THeapMemAvail.Load;
     Store   : @THeapMemAvail.Store);
{$ifndef cdPrintDoc}
{#Z-}
{$endif cdPrintDoc}

procedure RegisterStatuses;
{$ifndef cdPrintDoc}
{#F+}
{ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ÝRegisterStatuses procedure   (Statuses unit)Þ
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß}
{#F-}
{$endif cdPrintDoc}
  { RegisterStatuses calls RegisterType for each of the status view and
    status dialog object types defined in the tvStatus unit.  After calling
    RegisterStatuses, your application can read or write any of those types
    with streams. }


implementation

uses
  MsgBox, App;

{****************************************************************************}
{                    Local procedures and functions                          }
{****************************************************************************}

{****************************************************************************}
{ TAppStatus Object                                                          }
{****************************************************************************}
{****************************************************************************}
{ TAppStatus.GetPalette                                                      }
{****************************************************************************}
function TAppStatus.GetPalette : PPalette;
const P : String[Length(CAppStatus)] = CAppStatus;
begin
  GetPalette := PPalette(@P);
end;

{****************************************************************************}
{ TArrowGauge Object                                                         }
{****************************************************************************}
{****************************************************************************}
{ TArrowGauge.Init                                                           }
{****************************************************************************}
constructor TArrowGauge.Init (R : TRect; ACommand : Word; AMin, AMax : Word;
                              RightArrow : Boolean);
begin
  if not TGauge.Init(R,ACommand,AMin,AMax) then
    Fail;
  Right := RightArrow;
end;

{****************************************************************************}
{ TArrowGauge.Load                                                           }
{****************************************************************************}
constructor TArrowGauge.Load (var S : TStream);
begin
  if not TGauge.Load(S) then
    Fail;
  S.Read(Right,SizeOf(Right));
  if (S.Status <> stOk) then
  begin
    TGauge.Done;
    Fail;
  end;
end;

{****************************************************************************}
{ TArrowGauge.Draw                                                           }
{****************************************************************************}
procedure TArrowGauge.Draw;
const Arrows : array[0..1] of Char = '<>';
var
  B : TDrawBuffer;
  C : Word;
  Len : Byte;
begin
  C := GetColor(1);
  Len := Round(Size.X * Current/(Max - Min));
  MoveChar(B,' ',C,Size.X);
  if Right then
    MoveChar(B,Arrows[Byte(Right)],C,Len)
  else MoveChar(B[Size.X - Len],Arrows[Byte(Right)],C,Len);
  WriteLine(0,0,Size.X,1,B);
end;

{****************************************************************************}
{ TArrowGauge.GetData                                                        }
{****************************************************************************}
procedure TArrowGauge.GetData (var Rec);
begin
  PArrowGaugeRec(Rec)^.Min := Min;
  PArrowGaugeRec(Rec)^.Max := Max;
  PArrowGaugeRec(Rec)^.Count := Current;
  PArrowGaugeRec(Rec)^.Right := Right;
end;

{****************************************************************************}
{ TArrowGauge.SetData                                                        }
{****************************************************************************}
procedure TArrowGauge.SetData (var Rec);
begin
  Min := PArrowGaugeRec(Rec)^.Min;
  Max := PArrowGaugeRec(Rec)^.Max;
  Current := PArrowGaugeRec(Rec)^.Count;
  Right := PArrowGaugeRec(Rec)^.Right;
end;

{****************************************************************************}
{ TArrowGauge.Store                                                          }
{****************************************************************************}
procedure TArrowGauge.Store (var S : TStream);
begin
  TGauge.Store(S);
  S.Write(Right,SizeOf(Right));
end;

{****************************************************************************}
{ TBarGauge Object                                                           }
{****************************************************************************}
{****************************************************************************}
{ TBarGauge.Draw                                                             }
{****************************************************************************}
procedure TBarGauge.Draw;
var
  B : TDrawBuffer;
  C : Word;
  FillSize : Word;
  PercentDone : LongInt;
  S : String[4];
begin
  { fill entire view }
  MoveChar(B,' ',GetColor(4),Size.X);
  { make progress bar }
  C := GetColor(5);
  FillSize := Round(Size.X * (Current / Max));
  MoveChar(B,' ',C,FillSize);
  { display percent done }
  PercentDone := Percent;
  FormatStr(S,'%d%%',PercentDone);
  if PercentDone < 50 then
    C := GetColor(4);
  FillSize := (Size.X - Length(S)) div 2;
  MoveStr(B[FillSize],S,C);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

{****************************************************************************}
{ TBarGauge.GetPalette                                                       }
{****************************************************************************}
function TBarGauge.GetPalette : PPalette;
const
  S : String[Length(CBarGauge)] = CBarGauge;
begin
  GetPalette := PPalette(@S);
end;

{****************************************************************************}
{ TGauge Object                                                              }
{****************************************************************************}
{****************************************************************************}
{ TGauge.Init                                                                }
{****************************************************************************}
constructor TGauge.Init (R : TRect; ACommand : Word; AMin, AMax : LongInt);
begin
  if not TStatus.Init(R,ACommand,'',1) then
    Fail;
  Min := AMin;
  Max := AMax;
  Current := Min;
end;

{****************************************************************************}
{ TGauge.Load                                                                }
{****************************************************************************}
constructor TGauge.Load (var S : TStream);
begin
  if not TStatus.Load(S) then
    Fail;
  S.Read(Min,SizeOf(Min));
  S.Read(Max,SizeOf(Max));
  S.Read(Current,SizeOf(Current));
  if S.Status <> stOk then
  begin
    TStatus.Done;
    Fail;
  end;
end;

{****************************************************************************}
{ TGauge.Draw                                                                }
{****************************************************************************}
procedure TGauge.Draw;
var
  S : String;
  B : TDrawBuffer;
begin
  { Blank the gauge }
  MoveChar(B,' ',GetColor(1),Size.X);
  WriteBuf(0,0,Size.X,Size.Y,B);
  { write current status }
  FormatStr(S,'%d',Current);
  MoveStr(B,S,GetColor(1));
  WriteBuf(0,0,Size.X,Size.Y,B);
end;

{****************************************************************************}
{ TGauge.GetData                                                             }
{****************************************************************************}
procedure TGauge.GetData (var Rec);
begin
  TGaugeRec(Rec).Min := Min;
  TGaugeRec(Rec).Max := Max;
  TGaugeRec(Rec).Current := Current;
end;

{****************************************************************************}
{ TGauge.Reset                                                               }
{****************************************************************************}
procedure TGauge.Reset;
begin
  Current := Min;
  DrawView;
end;

{****************************************************************************}
{ TGauge.SetData                                                             }
{****************************************************************************}
procedure TGauge.SetData (var Rec);
begin
  Min := TGaugeRec(Rec).Min;
  Max := TGaugeRec(Rec).Max;
  Current := TGaugeRec(Rec).Current;
end;

{****************************************************************************}
{ TGauge.Store                                                               }
{****************************************************************************}
procedure TGauge.Store (var S : TStream);
begin
  TStatus.Store(S);
  S.Write(Min,SizeOf(Min));
  S.Write(Max,SizeOf(Max));
  S.Write(Current,SizeOf(Current));
end;

{****************************************************************************}
{ TGauge.Update                                                              }
{****************************************************************************}
procedure TGauge.Update (Data : Pointer);
begin
  if Current < Max then
  begin
    Inc(Current);
    DrawView;
  end
  else Message(@Self,evStatus,cmStatusDone,@Self);
end;

{****************************************************************************}
{ THeapMaxAvail Object                                                       }
{****************************************************************************}
{****************************************************************************}
{ THeapMaxAvail.Init                                                         }
{****************************************************************************}
constructor THeapMaxAvail.Init (X, Y : Integer);
var
  R : TRect;
begin
  R.Assign(X,Y,X+20,Y+1);
  if not TAppStatus.Init(R,cmStatusUpdate,' MaxAvail = %d',1) then
    Fail;
  Max := -1;
end;

{****************************************************************************}
{ THeapMaxAvail.Update                                                       }
{****************************************************************************}
procedure THeapMaxAvail.Update (Data : Pointer);
var
  M : LongInt;
begin
  M := MaxAvail;
  if (Max <> M) then
  begin
    Max := MaxAvail;
    SetData(Max);
  end;
end;

{****************************************************************************}
{ THeapMemAvail Object                                                       }
{****************************************************************************}
{****************************************************************************}
{ THeapMemAvail.Init                                                         }
{****************************************************************************}
constructor THeapMemAvail.Init (X, Y : Integer);
var
  R : TRect;
begin
  R.Assign(X,Y,X+20,Y+1);
  if not TAppStatus.Init(R,cmStatusUpdate,' MemAvail = %d',1) then
    Fail;
  Mem := -1;
end;

{****************************************************************************}
{ THeapMemAvail.Update                                                       }
{****************************************************************************}
procedure THeapMemAvail.Update (Data : Pointer);
  { Total bytes available on the heap.  May not be contiguous. }
var
  M : LongInt;
begin
  M := MemAvail;
  if (Mem <> M) then
  begin
    Mem := M;
    SetData(Mem);
  end;
end;

{****************************************************************************}
{ TPercentGauge Object                                                       }
{****************************************************************************}
{****************************************************************************}
{ TPercentGauge.Draw                                                         }
{****************************************************************************}
procedure TPercentGauge.Draw;
var
  B : TDrawBuffer;
  C : Word;
  S : String;
  PercentDone : LongInt;
  FillSize : Integer;
begin
  C := GetColor(1);
  MoveChar(B,' ',C,Size.X);
  WriteLine(0,0,Size.X,Size.Y,B);
  PercentDone := Percent;
  FormatStr(S,'%d%%',PercentDone);
  MoveStr(B[(Size.X - Byte(S[0])) div 2],S,C);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

{****************************************************************************}
{ TPercentGauge.Percent                                                      }
{****************************************************************************}
function TPercentGauge.Percent : Integer;
  { Returns percent as a whole integer Current of Max }
begin
  Percent := Round((Current/Max) * 100);
end;

{****************************************************************************}
{ TSpinnerGauge Object                                                       }
{****************************************************************************}

{****************************************************************************}
{ TSpinnerGauge.Init                                                         }
{****************************************************************************}
constructor TSpinnerGauge.Init (X, Y : Integer; ACommand : Word);
var R : TRect;
begin
  R.Assign(X,Y,X+1,Y+1);
  if not TGauge.Init(R,ACommand,1,4) then
    Fail;
end;

{****************************************************************************}
{ TSpinnerGauge.Draw                                                         }
{****************************************************************************}
procedure TSpinnerGauge.Draw;
var
  B : TDrawBuffer;
  C : Word;
begin
  C := GetColor(1);
  MoveChar(B,' ',C,Size.X);
  WriteLine(0,0,Size.X,Size.Y,B);
  MoveChar(B[Size.X div 2],SpinChars[Current],C,1);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

{****************************************************************************}
{ TSpinnerGauge.HandleEvent                                                  }
{****************************************************************************}
procedure TSpinnerGauge.HandleEvent (var Event : TEvent);
begin
  TStatus.HandleEvent(Event);
end;

{****************************************************************************}
{ TSpinnerGauge.Update                                                       }
{****************************************************************************}
procedure TSpinnerGauge.Update (Data : Pointer);
begin
  if Current = Max then
    Current := Min
  else Inc(Current);
  DrawView;
end;

{****************************************************************************}
{ TStatus Object                                                             }
{****************************************************************************}
{****************************************************************************}
{ TStatus.Init                                                               }
{****************************************************************************}
constructor TStatus.Init (R : TRect; ACommand : Word; AText : String;
                          AParamCount : Integer);
begin
  if (not TParamText.Init(R,AText,AParamCount)) then
    Fail;
  EventMask := EventMask or evStatus;
  Command := ACommand;
end;

{****************************************************************************}
{ TStatus.Load                                                               }
{****************************************************************************}
constructor TStatus.Load (var S : TStream);
begin
  if not TParamText.Load(S) then
    Fail;
  S.Read(Command,SizeOf(Command));
  if (S.Status <> stOk) then
  begin
    TParamText.Done;
    Fail;
  end;
end;

{****************************************************************************}
{ TStatus.Cancel                                                             }
{****************************************************************************}
function TStatus.Cancel : Boolean;
begin
  Cancel := True;
end;

{****************************************************************************}
{ TStatus.GetPalette                                                         }
{****************************************************************************}
function TStatus.GetPalette : PPalette;
const
  P : String[Length(CStatus)] = CStatus;
begin
  GetPalette := PPalette(@P);
end;

{****************************************************************************}
{ TStatus.HandleEvent                                                        }
{****************************************************************************}
procedure TStatus.HandleEvent (var Event : TEvent);
begin
  if (Event.What = evCommand) and (Event.Command = cmStatusPause) then
  begin
    Pause;
    ClearEvent(Event);
  end;
  case Event.What of
    evStatus :
      case Event.Command of
        cmStatusDone :
          if (Event.InfoPtr = @Self) then
          begin
            Message(Owner,evStatus,cmStatusDone,@Self);
            ClearEvent(Event);
          end;
        cmStatusUpdate :
          if (Event.InfoWord = Command) and ((State and sfPause) = 0) then
          begin
            Update(Event.InfoPtr);
            { ClearEvent(Event); } { don't clear the event so multiple }
                            { status views can respond to the same event }
          end;
        cmStatusResume :
          if (Event.InfoWord = Command) and
             ((State and sfPause) = sfPause) then
          begin
            Resume;
            ClearEvent(Event);
          end;
        cmStatusPause :
          if (Event.InfoWord = Command) and ((State and sfPause) = 0) then
          begin
            Pause;
            ClearEvent(Event);
          end;
      end;
  end;
  TParamText.HandleEvent(Event);
end;

{****************************************************************************}
{ TStatus.Pause                                                              }
{****************************************************************************}
procedure TStatus.Pause;
begin
  SetState(sfPause,True);
end;

{****************************************************************************}
{ TStatus.Reset                                                              }
{****************************************************************************}
procedure TStatus.Reset;
begin
  DrawView;
end;

{****************************************************************************}
{ TStatus.Resume                                                             }
{****************************************************************************}
procedure TStatus.Resume;
begin
  SetState(sfPause,False);
end;

{****************************************************************************}
{ TStatus.Store                                                              }
{****************************************************************************}
procedure TStatus.Store (var S : TStream);
begin
  TParamText.Store(S);
  S.Write(Command,SizeOf(Command));
end;

{****************************************************************************}
{ TStatus.Update                                                             }
{****************************************************************************}
procedure TStatus.Update (Data : Pointer);
begin
  DisposeStr(Text);
  Text := NewStr(String(Data^));
  DrawView;
end;

{****************************************************************************}
{ TStatusDlg Object                                                          }
{****************************************************************************}
{****************************************************************************}
{ TStatusDlg.Init                                                            }
{****************************************************************************}
constructor TStatusDlg.Init (ATitle : TTitleStr; AStatus : PStatus;
                             AFlags : Word);
var
  R : TRect;
  i : LongInt;
  Buttons : Byte;
begin
  if (AStatus = nil) then
    Fail;
  R.A := AStatus^.Origin;
  R.B := AStatus^.Size;
  Inc(R.B.Y,R.A.Y+4);
  Inc(R.B.X,R.A.X+5);
  if not TDialog.Init(R,ATitle) then
    Fail;
  EventMask := EventMask or evStatus;
  Status := AStatus;
  Status^.MoveTo(2,2);
  Insert(Status);
  InsertButtons(AFlags);
end;

{****************************************************************************}
{ TStatusDlg.Load                                                            }
{****************************************************************************}
constructor TStatusDlg.Load (var S : TStream);
begin
  if not TDialog.Load(S) then
    Fail;
  GetSubViewPtr(S,Status);
  if (S.Status <> stOk) then
  begin
    if (Status <> nil) then
      Dispose(Status,Done);
    TDialog.Done;
    Fail;
  end;
end;

{****************************************************************************}
{ TStatusDlg.Cancel                                                          }
{****************************************************************************}
procedure TStatusDlg.Cancel (ACommand : Word);
begin
  if Status^.Cancel then
    TDialog.Cancel(ACommand);
end;

{****************************************************************************}
{ TStatusDlg.HandleEvent                                                     }
{****************************************************************************}
procedure TStatusDlg.HandleEvent (var Event : TEvent);
begin
  case Event.What of
    evStatus :
      case Event.Command of
        cmStatusDone :
          if Event.InfoPtr = Status then
          begin
            TDialog.Cancel(cmOk);
            ClearEvent(Event);
          end;
      end;
      { else let TDialog.HandleEvent send to all subviews for handling }
    evBroadcast, evCommand :
      case Event.Command of
        cmCancel, cmClose :
          begin
            Cancel(cmCancel);
            ClearEvent(Event);
          end;
        cmStatusPause :
          begin
            Status^.Pause;
            ClearEvent(Event);
          end;
        cmStatusResume :
          begin
            Status^.Resume;
            ClearEvent(Event);
          end;
      end;
  end;
  TDialog.HandleEvent(Event);
end;

{****************************************************************************}
{ TStatusDlg.InsertButtons                                                   }
{****************************************************************************}
procedure TStatusDlg.InsertButtons (AFlags : Word);
var
  R : TRect;
  P : PButton;
  Buttons : Byte;
  X, Y, Gap : Integer;
  i : Word;
begin
  Buttons := Byte(((AFlags and sdCancelButton) = sdCancelButton));
  { do this Inc twice, once for Pause and once for Resume buttons }
  Inc(Buttons,2 * Byte(((AFlags and sdPauseButton) = sdPauseButton)));
  if Buttons > 0 then
  begin
    Status^.GrowMode := gfGrowHiX;
    { resize dialog to hold all requested buttons }
    if Size.X < ((Buttons * 12) + 2) then
      GrowTo((Buttons * 12) + 2,Size.Y + 2)
    else GrowTo(Size.X,Size.Y + 2);
    { find correct starting position for first button }
    Gap := Size.X - (Buttons * 10) - 2;
    Gap := Gap div Succ(Buttons);
    X := Gap;
    if X < 2 then
      X := 2;
    Y := Size.Y - 3;
    { insert buttons }
    if ((AFlags and sdCancelButton) = sdCancelButton) then
    begin
      P := NewButton(X,Y,10,2,'Cancel',cmCancel,hcCancel,bfDefault);
      P^.GrowMode := gfGrowHiY or gfGrowLoY;
      Inc(X,12 + Gap);
    end;
    if ((AFlags and sdPauseButton) = sdPauseButton) then
    begin
      P := NewButton(X,Y,10,2,'~P~ause',cmStatusPause,hcStatusPause,bfNormal);
      P^.GrowMode := gfGrowHiY or gfGrowLoY;
      Inc(X,12 + Gap);
      P := NewButton(X,Y,10,2,'~R~esume',cmStatusResume,hcStatusResume,
                     bfBroadcast);
      P^.GrowMode := gfGrowHiY or gfGrowLoY;
    end;
  end;  { of if }
  SelectNext(False);
end;

{****************************************************************************}
{ TStatusDlg.Store                                                           }
{****************************************************************************}
procedure TStatusDlg.Store (var S : TStream);
begin
  TDialog.Store(S);
  PutSubViewPtr(S,Status);
end;

{****************************************************************************}
{ TStatusMessageDlg Object                                                   }
{****************************************************************************}
{****************************************************************************}
{ TStatusMessageDlg.Init                                                     }
{****************************************************************************}
constructor TStatusMessageDlg.Init (ATitle : TTitleStr; AStatus : PStatus;
                                    AFlags : Word; AMessage : String);
var
  P : PStaticText;
  X, Y : Integer;
  R : TRect;
begin
  if not TStatusDlg.Init(ATitle,AStatus,AFlags) then
    Fail;
  Status^.GrowMode := gfGrowLoY or gfGrowHiY;
  GetExtent(R);
  X := R.B.X - R.A.X;
  if X < Size.X then
    X := Size.X;
  Y := R.B.Y - R.A.Y;
  if Y < Size.Y then
    Y := Size.Y;
  GrowTo(X,Y);
  R.Assign(2,2,Size.X-2,Size.Y-3);
  P := New(PStaticText,Init(R,AMessage));
  if (P = nil) then
  begin
    TStatusDlg.Done;
    Fail;
  end;
  GrowTo(Size.X,Size.Y + P^.Size.Y + 1);
  Insert(P);
end;

{****************************************************************************}
{                    Global procedures and functions                         }
{****************************************************************************}

{****************************************************************************}
{ RegisterStatuses                                                           }
{****************************************************************************}
procedure RegisterStatuses;
begin
{  RegisterType(RStatus);
  RegisterType(RStatusDlg);
  RegisterType(RGauge);
  RegisterType(RArrowGauge);
  RegisterType(RPercentGauge);
  RegisterType(RBarGauge);
  RegisterType(RSpinnerGauge); }
end;

{****************************************************************************}
{                            Unit Initialization                             }
{****************************************************************************}
begin
end.
