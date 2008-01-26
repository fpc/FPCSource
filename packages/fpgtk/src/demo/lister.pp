{$mode objfpc}{$h+}
unit lister;

interface

uses glib, gdk, gtk, FPgtk, classes, fpgtkext, sysutils;

type

  TListWindow = class (TFPgtkWindow)
  private
    pb : TFPgtkProgressBar;
    spin : TFPgtkSpinButton;
    notebook : TFPgtkNotebook;
    bar : TFPgtkStatusbar;
    EContext, EMessage : TFPgtkEntry;
    IDContext, IDMessage : TFPgtkSpinButton;
    List : TFPgtkScrollList;
    ListText : TFPgtkScrollText;
    CList : TFPgtkScrollCList;
    CListText : TFPgtkScrollText;
    Key : TFPgtkSpinButton;
    ModCtrl, ModAlt, ModShift : TFPgtkToggleButton;
    KeyName : TFPgtkEntry;
    AKeyName : TFPgtkAccelLabel;
    TheAG : guint;
    AccelBut : TFPgtkButton;
    VButtons, HButtons : TFPgtkButtonBox;
    TheLayout : TFPgtkLayout;
    LayoutX, LayoutY : integer;
    cal : TFPgtkCalendar;
    MessageButtons : TMsgDlgButtons;
    Msg : TFPgtkEntry;
    DialogType : TFPgtkMenu;
    procedure AddRemoveButton (Sender:TFPgtkObject; data:pointer);
    procedure ShowMessageDialog (Sender:TFPgtkObject; data:pointer);
    procedure Calendar_ShowDate (Sender:TFPgtkObject; data:pointer);
    procedure Layout_runaway (Sender:TFPgtkObject; data:pointer);
    procedure ButBox_AddThem (Sender:TFPgtkObject; data:pointer);
    procedure Accel_Parse (Sender : TFPgtkObject; data : pointer);
    procedure Accel_AddAccel (Sender : TFPgtkObject; data : pointer);
    procedure Accel_ConvertToName (Sender : TFPgtkObject; data : pointer);
    procedure ProgressBar_ShowItToMe (Sender : TFPgtkObject; data : pointer);
    procedure ProgressBar_SetActivityMode (Sender : TFPgtkObject; data : pointer);
    procedure ProgressBar_FormatString (Sender : TFPgtkObject; data : pointer);
    procedure Spinbutton_ClimbRate (Sender : TFPgtkObject; data : pointer);
    procedure Spinbutton_Digits (Sender : TFPgtkObject; data : pointer);
    procedure Spinbutton_UpdatePolicy (Sender : TFPgtkObject; data : pointer);
    procedure Spinbutton_Numeric (Sender : TFPgtkObject; data : pointer);
    procedure Spinbutton_Wrap (Sender : TFPgtkObject; data : pointer);
    procedure Spinbutton_SnapToTicks (Sender : TFPgtkObject; data : pointer);
    procedure MemoMessage_Show (Sender : TFPgtkObject; data : pointer);
    procedure StatusBar_GetContext (Sender : TFPgtkObject; data : pointer);
    procedure StatusBar_Push (Sender : TFPgtkObject; data : pointer);
    procedure StatusBar_Pop (Sender : TFPgtkObject; data : pointer);
    procedure StatusBar_Remove (Sender : TFPgtkObject; data : pointer);
    procedure List_ShowSignal (Sender : TFPgtkObject; data : pointer);
    procedure List_ShowWidgetSignal (Sender : TFPgtkObject; Widget:TFPgtkWidget; data : pointer);
    procedure List_AddToList (Sender : TFPgtkObject; data : pointer);
    procedure List_SelectionMode (Sender : TFPgtkObject; data : pointer);
    procedure List_ClearAll (Sender : TFPgtkObject; data : pointer);
    procedure List_Clear1_5 (Sender : TFPgtkObject; data : pointer);
    procedure List_AddCount (Sender : TFPgtkObject; data : pointer);
    procedure CList_AddToList (Sender : TFPgtkObject; data : pointer);
    procedure CList_SelectionMode (Sender : TFPgtkObject; data : pointer);
  public
    constructor create;
  end;


implementation

const
  Init_ProgressBar_FormatString = '%p%% (%v of %u)';

constructor TListWindow.Create;
var b, h : TFPgtkBox;
    but : TFPgtkButton;
    tbut : TFPgtkToggleButton;
    e : TFPgtkEntry;
    sp : TFPgtkSpinButton;
    t : TFPgtkScrollText;
    f : TFPgtkFrame;
    p : TFPgtkPaned;
    om : TFPgtkOptionMenu;
    bb : TFPgtkButtonBox;
begin
  inherited create (gtk_window_dialog);
  border := 10;
  modal := TRUE;
  Position := gtk_win_pos_mouse;

  Notebook := TFPgtkNotebook.Create;
  Notebook.homogenous := false;
  Notebook.scrollable := true;
  Add (Notebook);

  // ******* MessageDialog

  writeln ('  MessageDialog');
  b := TFPgtkVBox.Create;
  Notebook.AppendPage (b, TFPgtkLabel.Create('MessageDialog'));

  Msg := TFpgtkEntry.Create;
  Msg.Text := 'This is normal message text';
  b.packstart (msg, false, false, 0);

  om := TFPgtkOptionMenu.Create;
  DialogType := NewMenu ('DialogType', [NewMenuItem ('Warning'),NewMenuItem ('Error'),
           NewMenuItem ('Information'),NewMenuItem ('Confirmation'),NewMenuItem ('Custom')]);
  om.Menu := DialogType;
  om.SetHistory (0);
  b.Packstart (om, false, false, 0);

  bb := TFPgtkHButtonBox.create;
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Yes');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbYes)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('No');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbNo)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Ok');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbOk)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Cancel');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbCancel)));
  bb.PackStart (tbut);
  b.packstart (bb, false, false, 0);

  bb := TFPgtkHButtonBox.create;
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Abort');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbAbort)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Retry');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbRetry)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Ignore');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbIgnore)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('Help');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbHelp)));
  bb.PackStart (tbut);
  b.packstart (bb, false, false, 0);

  bb := TFPgtkHButtonBox.create;
  tbut := TFPgtkCheckedButton.CreateWithLabel ('All');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbAll)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('NoToALl');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbNoToAll)));
  bb.PackStart (tbut);
  tbut := TFPgtkCheckedButton.CreateWithLabel ('YesToAll');
  tbut.connectClicked (@AddRemoveButton, inttopointer(ord(mbYesToAll)));
  bb.PackStart (tbut);
  b.packstart (bb, false, false, 0);

  bb := TFPgtkHButtonBox.create;
  but := TFPgtkButton.CreateWithLabel ('Show Message Dialog !!');
  but.ConnectClicked (@ShowMessageDialog, nil);
  bb.Packstart (but);
  b.packstart (bb, false, false, 0);

  // ******* Calendar

  writeln ('  Calendar');
  b := TFPgtkVBox.Create;
  Notebook.AppendPage (b, TFPgtkLabel.Create('Calendar'));

  cal := TFPgtkCalendar.Create;
  cal.date := encodeDate (2000,1,1);
  b.PackStart (Cal);

  but := TFPgtkButton.CreateWithLabel ('Show Date');
  but.ConnectClicked (@Calendar_ShowDate, nil);
  b.PackEnd (but, false, false, 0);

  // ******* Layout

  writeln ('  Layout');
  TheLayout := TFPgtkLayout.Create;
  Notebook.AppendPage (TheLayout, TFPgtkLabel.Create('Layout'));
  TheLayout.Freeze;
  e := TFPgtkEntry.Create;
    e.Text := 'Entry on 25,10 to clearly show where the edges end';
    TheLayout.Put (e, 25,10);
  e := TFPgtkEntry.Create;
    e.Text := 'Entry on 5,50';
    e.SetUSize (300, 24);
    TheLayout.Put (e, 5,30);
  but := TFPgtkButton.CreateWithLabel ('Catch me with doubleclick !!');
    but.ConnectClicked (@Layout_runaway, nil);
    but.CanDefault:= true;
    but.SetUSize (200, 50);
    TheLayout.put (but, 75, 75);
    LayoutY := 75;
    LayoutX := 75;
  TheLayout.SetSize (350, 400);
  TheLayout.Thaw;

  // ******* ButtonBox

  writeln ('  ButtonBox');
  b := TFPgtkVBox.Create;
  Notebook.AppendPage (b, TFPgtkLabel.Create('Buttonboxes'));

  HButtons := TFPgtkHButtonBox.Create;
  HButtons.Layout := GTK_BUTTONBOX_end;
  HButtons.Spacing := 3;
  b.PackEnd (HButtons, false, false, 0);

  h := TFPgtkHBox.Create;
  b.PackStart (h, true, true, 0);

  but := TFPgtkButton.CreateWithLabel ('Add a button');
  but.ConnectClicked (@ButBox_AddThem, nil);
  h.PackStart (but, false, false, 3);

  VButtons := TFPgtkVButtonBox.Create;
  VButtons.Layout := GTK_Buttonbox_Start;
  VButtons.Spacing := 15;
  h.PackEnd (VButtons, false, false,0);

  // ******* Accelerator

  writeln ('  Accelerator');
  b := TFPgtkVBox.Create;
  Notebook.AppendPage (b, TFPgtkLabel.Create('Accelorators'));

  TheAG := AccelGroupNew;

  h := TFPgtkHBox.Create;
  b.PackStart (h, false, false, 0);
  but := TFPgtkButton.CreateWithLabel('Parse');
  but.ConnectClicked (@Accel_Parse, nil);
  h.PackStart (but, false, false, 0);
  Key := TFPgtkSpinButton.Create;
  h.PackStart (Key);
  Key.adjustment.configure (0, $FFFF, GDK_Delete, 1, 256, 0);
  Key.Configure (nil, 100, 0);

  h := TFPgtkHBox.Create;
  b.PackStart (h, false, false, 0);
  h.homogeneous := true;
  ModShift := TFPgtkCheckedButton.CreateWithLabel('Shift');
  h.PackStart (ModShift);
  ModCtrl := TFPgtkCheckedButton.CreateWithLabel('Ctrl');
  ModCtrl.active := true;
  h.Packstart(ModCtrl);
  ModAlt := TFPgtkCheckedButton.CreateWithLabel('Alt');
  h.Packstart (ModAlt);

  h := TFPgtkHBox.create;
  b.PackStart (h, false, false, 0);
  AccelBut := TFPgtkButton.createwithLabel ('Show');
  Accelbut.ConnectClicked (@Accel_ConvertToName, nil);
  //AcceleratorAdd (TheAG, Accelbut, sgClicked, Gdk_S, Gdk_Control_mask, GTK_ACCEL_VISIBLE);
  h.PackStart (Accelbut,false,false,0);
  KeyName := TFPgtkEntry.Create;
  h.Packstart (KeyName);

  h := TFPgtkHBox.create;
  but := TFPgtkButton.CreateWithLabel ('Add');
  but.ConnectClicked (@Accel_AddAccel, nil);
  h.PackStart (but, false, false, 0);
  but := TFPgtkButton.Create;
  AKeyName := TFPgtkAccelLabel.create ('Accellerators');
  but.Add (AKeyName);
  AKeyName.accelwidget := AccelBut;
  AKeyname.refetch;
  h.PackStart (but, true, true, 0);
  b.PackStart (h, false, false, 0);

  // ******* CList

  writeln ('  CList');
  p := TFPgtkVPaned.Create;
  Notebook.AppendPage (p, TFPgtkLabel.Create('CList'));

  CList := TFPgtkScrollCList.Create (1);
  p.Pack1 (CList, true, true);
  b := TFPgtkVBox.Create;
  p.Pack2 (b, false, true);
  p.handleSize := 5;
  p.ComputePosition (100, 60, 50);

  CListText := TFPgtkScrollText.create;
  b.Packstart (CListText);

  h := TFPgtkHBox.Create;
  b.PackStart (h, false, false, 0);

  but := TFPGtkButton.CreateWithLabel('Add');
  but.ConnectClicked (@CList_AddToList, CListText.TheText);
  h.PackStart (but, false, false, 0);

  h := TFPgtkHBox.Create;
  h.PackStart (TFPGtkLabel.Create('Selection Mode'), false, false, 2);
  om := TFPgtkOptionMenu.Create;
  om.Menu := NewMenu ('Selection Mode', [
                 NewMenuItem ('Single',@CList_SelectionMode, inttopointer(0)),
                 NewMenuItem ('Browse',@CList_SelectionMode, inttopointer(1)),
                 NewMenuItem ('Mutiple',@CList_SelectionMode, inttopointer(2)),
                 NewMenuItem ('Extended',@CList_SelectionMode, inttopointer(3))]);
  om.SetHistory (0);
  CList.CList.SelectionMode := GTK_SELECTION_SINGLE;
  h.Packstart (om, true, true, 0);

  b.PackStart (h, false, false, 0);

  // ******* List

  writeln ('  List');
  p := TFPgtkVPaned.Create;
  Notebook.AppendPage (p, TFPgtkLabel.Create('List'));

  List := TFPgtkScrollList.Create;
  p.Pack1 (List, true, true);
  b := TFPgtkVBox.Create;
  p.Pack2 (b, false, true);
  p.handleSize := 5;
  p.ComputePosition (100, 60, 50);

  ListText := TFPgtkScrollText.create;
  b.Packstart (ListText);

  h := TFPgtkHBox.Create;
  b.PackStart (h, false, false, 0);

  but := TFPGtkButton.CreateWithLabel('Add');
  but.ConnectClicked (@List_AddToList, ListText.TheText);
  h.PackStart (but, false, false, 0);

  but := TFPGtkButton.CreateWithLabel('Add Count');
  but.ConnectClicked (@List_AddCount, ListText.TheText);
  h.PackStart (but, false, false, 0);

  but := TFPGtkButton.CreateWithLabel('Clear 1-5');
  but.ConnectClicked (@List_Clear1_5, ListText.TheText);
  h.PackStart (but, false, false, 0);

  but := TFPGtkButton.CreateWithLabel('Clear all');
  but.ConnectClicked (@List_ClearAll, ListText.TheText);
  h.PackStart (but, false, false, 0);

  h := TFPgtkHBox.Create;
  h.PackStart (TFPGtkLabel.Create('Selection Mode'), false, false, 2);
  om := TFPgtkOptionMenu.Create;
  om.Menu := NewMenu ('Selection Mode', [
                 NewMenuItem ('Single','','',@List_SelectionMode, inttopointer(0)),
                 NewMenuItem ('Browse','','',@List_SelectionMode, inttopointer(1)),
                 NewMenuItem ('Mutiple','','',@List_SelectionMode, inttopointer(2)),
                 NewMenuItem ('Extended','','',@List_SelectionMode, inttopointer(3))]);
  om.SetHistory (0);
  List.List.SelectionMode := GTK_SELECTION_SINGLE;
  h.Packstart (om, true, true, 0);

  b.PackStart (h, false, false, 0);

  // ******* Statusbar

  writeln ('  Statusbar');
  b := TFPgtkVBox.Create;
  Notebook.appendPage (b, TFPgtkLabel.Create('Statusbar'));

  bar := TFPgtkStatusbar.Create;
  b.Packend (bar, false, true, 0);

  f := TFPgtkFrame.Create;
  f.Text := 'Context';
  h := TFPgtkHbox.Create;
  IDContext := TFPgtkSpinbutton.Create;
  IDContext.configure (nil, 1000, 0);
  IDContext.adjustment.configure (-maxint, maxint, 0, 1, 100,0);
  EContext := TFPgtkEntry.Create;
  EContext.ConnectChanged (@StatusBar_GetContext, IDContext);
  h.Packstart (EContext, true, true, 0);
  h.packstart (TFPgtkLabel.Create('ID'), false, false, 2);
  h.packstart (IDContext, false, false, 0);
  f.add (h);
  b.Packstart (f, false, true, 0);

  f := TFPgtkFrame.Create;
  f.Text := 'Message';
  h := TFPgtkHbox.Create;
  IDMessage := TFPgtkSpinbutton.Create;
  IDMessage.configure (nil, 1000, 0);
  IDMessage.adjustment.configure (-maxint, maxint, 0, 1, 100,0);
  EMessage := TFPgtkEntry.Create;
  h.Packstart (EMessage, true, true, 0);
  h.packstart (TFPgtkLabel.Create('ID'), false, false, 2);
  h.packstart (IDMessage, false, false, 0);
  f.add (h);
  b.Packstart (f, false, true, 0);

  h := TFPgtkHBox.Create;
  h.homogeneous := true;
  b.Packstart (h, false, false, 2);

  but := TFPgtkButton.CreateWithLabel ('Push');
  but.ConnectClicked (@statusbar_Push, EMessage);
  h.Packstart (but, false, true, 2);

  but := TFPgtkButton.CreateWithLabel ('Pop');
  but.ConnectClicked (@statusbar_Pop, EMessage);
  h.Packstart (but, false, true, 2);

  but := TFPgtkButton.CreateWithLabel ('Remove');
  but.ConnectClicked (@statusbar_Remove, EMessage);
  h.Packstart (but, false, true, 2);

  // ******* Memo and ShowMessage

  writeln ('  Memo / ShowMessage');
  b := TFPgtkVBox.Create;
  Notebook.appendPageFull (b, TFPgtkLabel.Create('Memo Message'), TFPgtkLabel.Create('Memo and ShowMessage'),true);

  t := TFPgtkScrollText.Create;
  b.PackStart (t, true, true, 10);

  but := TFPgtkButton.CreateWithLabel ('Show text');
  but.ConnectClicked (@MemoMessage_Show, t);
  b.PackStart (but, false, false, 2);

  // ******* File Selection

  writeln ('  File selection');
  b := TFPgtkVBox.Create;
  Notebook.appendPageFull (b, TFPgtkLabel.Create('File selection'), TFPgtkLabel.Create('File selection dialog'),true);

  b.PackStart (TFPgtkFileEntry.Create, false, true, 10);

  // ******* Progress bar page

  b := TFPgtkVBox.Create;
  Notebook.appendPageFull (b, TFPgtkLabel.Create('Progress'), TFPgtkLabel.Create('The Progressbar'),true);

  pb := TFPgtkProgressBar.Create (nil);
  with pb do
    begin
    formatstring := Init_ProgressBar_FormatString;
    showText := true;
    configure (50.0, 0.0, 500.0);
    end;
  b.Packstart (pb, false, false, 3);

  but := TFPgtkButton.createWithLabel ('Run the bar');
  but.ConnectClicked (@ProgressBar_ShowItToMe, nil);
  b.PackStart (but, false, false, 3);

  but := TFPgtkCheckbutton.CreateWithLabel ('Activity mode');
  but.ConnectClicked (@ProgressBar_SetActivityMode, pb);
  b.PackStart (but, false, false, 3);

  b.Packstart (TFPgtkLabel.Create ('Format text'), false, false, 10);

  e := TFPgtkEntry.Create;
  e.Text := Init_ProgressBar_FormatString;
  e.ConnectChanged (@ProgressBar_FormatString, pb);
  b.Packstart(e, false, false, 0);

  // ******* Spinbutton
  writeln ('  page creation');
  b := TFPgtkVBox.Create;
  Notebook.appendPageFull (b, TFPgtkLabel.Create('SpinButton'), TFPgtkLabel.Create('Spinbuttons'),true);

  Spin := TFPgtkSPinButton.Create;
  Spin.Configure (nil,0.01,3);
  Spin.Adjustment.configure (-100.0, 100.0, 10.25, 0.01, 0.1, 1.0);
  Spin.digits := 2;
  Spin.numeric := false;
  Spin.Wrap := false;
  Spin.SnapToTicks := false;
  b.PackStart (spin, false, false, 0);

  b.PackStart (TFPgtkLabel.Create('Climb rate'), false, false, 0);
  sp := TFPgtkSpinButton.Create;
  with sp do
    begin
    with Adjustment do
      begin
      Configure (0.0, 1.0, 0.01, 0.01, 0.20, 0.0);
      ConnectValueChanged (@Spinbutton_ClimbRate, sp);
      end;
    digits := 3;
    ClimbRate := 0.01;
    SnapToTicks := False;
    end;
  b.PackStart (sp, false, false, 0);

  b.PackStart (TFPgtkLabel.Create('Digits'), false, false, 0);
  sp := TFPgtkSpinButton.Create;
  with sp do
    begin
    with adjustment do
      begin
      Configure (0.0, 5.0, 2.0, 1.0, 1.0, 0.0);
      ConnectValueChanged (@Spinbutton_Digits, sp);
      end;
    Configure (nil,1,0);
    SnapToTicks := True;
    end;
  b.PackStart (sp, false, false, 0);

  tbut := TFPgtkToggleButton.CreateWithLabel ('Numeric');
  tbut.ConnectToggled (@SpinButton_numeric, Spin);
  b.PackStart (tbut, false, false, 0);

  tbut := TFPgtkToggleButton.CreateWithLabel ('Wrap');
  tbut.ConnectToggled (@SpinButton_Wrap, Spin);
  b.PackStart (tbut, false, false, 0);

  tbut := TFPgtkToggleButton.CreateWithLabel ('Snap to ticks');
  tbut.ConnectToggled (@SpinButton_SnapToTicks, Spin);
  b.PackStart (tbut, false, false, 0);

  // ******* Last empty page

  writeln ('  Empty page');
  Notebook.appendPageFull (TFPgtkLabel.Create('This page is left intentionally blank'), TFPgtkLabel.Create('Empty'), nil, true);

  Notebook.enablePopup;

end;

// ******* Progressbar

procedure TListWindow.ProgressBar_ShowItToMe (Sender : TFPgtkObject; data : pointer);
var k, r, t : integer;
begin
  for r := 0 to 500 do
    begin
    pb.CurrentValue := (r + 0.1);
    for t := 0 to random(5) do
      k := random(1000)*5 div 2542 + 15;
    end;
end;

procedure TListWindow.ProgressBar_SetActivityMode (Sender : TFPgtkObject; data : pointer);
begin
  TFPgtkProgressBar(data).ActivityMode := TFPgtkCheckButton(Sender).Active;
end;

procedure TListWindow.ProgressBar_FormatString (Sender : TFPgtkObject; data : pointer);
begin
  TFPgtkProgressBar(data).Formatstring := TFPgtkEntry(Sender).Text;
end;

// ******* Spinbutton

procedure TListWindow.Spinbutton_ClimbRate (Sender : TFPgtkObject; data : pointer);
begin
  spin.climbrate := TFPgtkSpinbutton(data).asFloat;
end;

procedure TListWindow.Spinbutton_Digits (Sender : TFPgtkObject; data : pointer);
begin
  spin.digits := TFPgtkSpinbutton(data).asinteger;
end;

procedure TListWindow.Spinbutton_UpdatePolicy (Sender : TFPgtkObject; data : pointer);
begin
end;

procedure TListWindow.Spinbutton_Numeric (Sender : TFPgtkObject; data : pointer);
begin
  TFPgtkSpinbutton(data).Numeric := (Sender as TFPgtkToggleButton).Active;
end;

procedure TListWindow.Spinbutton_Wrap (Sender : TFPgtkObject; data : pointer);
begin
  TFPgtkSpinbutton(data).Wrap := (Sender as TFPgtkToggleButton).Active;
end;

procedure TListWindow.Spinbutton_SnapToTicks (Sender : TFPgtkObject; data : pointer);
begin
  TFPgtkSpinbutton(data).SnapToTicks := (Sender as TFPgtkToggleButton).Active;
end;

{ Memo and ShowMessage }

procedure TListWindow.MemoMessage_Show (Sender : TFPgtkObject; data : pointer);
begin
  ShowMessage ('You typed:', TFPgtkScrollText(data).Text);
end;

{ Statusbar }

procedure TListWindow.StatusBar_Push (Sender : TFPgtkObject; data : pointer);
begin
  IDMessage.asinteger := bar.push (IDContext.asinteger, EMessage.Text);
end;

procedure TListWindow.StatusBar_Pop (Sender : TFPgtkObject; data : pointer);
begin
  bar.pop (IDContext.asinteger);
end;

procedure TListWindow.StatusBar_Remove (Sender : TFPgtkObject; data : pointer);
begin
  bar.Remove (IDContext.asinteger, IDMessage.asinteger);
end;

procedure TListWindow.StatusBar_GetContext (Sender : TFPgtkObject; data : pointer);
begin
  IDContext.asinteger := bar.GetContextID (EContext.Text);
end;

{ List }

const ListSignalNames : array[0..15] of string =
        (sgSelectionChanged,sgSelectChild,sgUnselectChild,
         sgToggleFocusRow,sgSelectAll,sgUnselectAll,sgUndoSelection,
         sgStartSelection,sgEndSelection,sgToggleAddMode,
         sgExtendSelection,sgScrollVertical,sgScrollHorizontal,
         sgSelect,sgDeselect,sgToggle);

procedure TListWindow.List_ShowSignal (Sender : TFPgtkObject; data : pointer);
var r : integer;
begin
  r := PointerToInt (data);
  writeln (Sender.Classname, ' emitted signal ',ListSignalNames[r]);
end;

procedure TListWindow.List_ShowWidgetSignal (Sender : TFPgtkObject; widget:TFPgtkWidget; data : pointer);
var r : integer;
begin
  r := PointerToInt (data);
  writeln (Sender.Classname, ' emitted signal ',ListSignalNames[r]);
end;

procedure TListWindow.List_AddToList (Sender : TFPgtkObject; data : pointer);
var t : TFPgtkText;
    l : TFPgtkListItemGroup;
begin
  l := TFPgtkListItemGroup.Create;
  try
    t := TFPgtkText(data);
    l.FillFromList (t.Lines);
    List.List.AppendItems (l);
  finally
    l.free;
  end;
end;

procedure TListWindow.List_SelectionMode (Sender : TFPgtkObject; data : pointer);
var r : integer;
begin
  r := pointertoint(data);
  List.List.SelectionMode := TGtkSelectionMode(r);
end;

procedure TListWindow.List_ClearAll (Sender : TFPgtkObject; data : pointer);
begin
  List.List.ClearAll;
end;

procedure TListWindow.List_Clear1_5 (Sender : TFPgtkObject; data : pointer);
begin
  List.List.ClearItems(1,5);
end;

procedure TListWindow.List_AddCount (Sender : TFPgtkObject; data : pointer);
var li : TFPgtkListItem;
begin
  li := TFPgtkListItem.CreateWithLabel ('Count');
  List.List.Add (li);
end;

{ CList }

procedure TListWindow.CList_AddToList (Sender : TFPgtkObject; data : pointer);
var t : TFPgtkText;
    l : TStrings;
    r : integer;
begin
  t := TFPgtkText(data);
  l := t.Lines;
  writeln ('Going to add... (',l.commatext,')');
  with l do
    for r := 0 to count-1 do
      CList.CList.Append (l[r],'');
end;

procedure TListWindow.CList_SelectionMode (Sender : TFPgtkObject; data : pointer);
var r : integer;
begin
  r := pointertoint(data);
  CList.CList.SelectionMode := TGtkSelectionMode(r);
end;

{ Accelerators }

procedure Tlistwindow.Accel_ConvertToName (Sender : TFPgtkObject; data : pointer);
var Mods : TGdkModifierType;
begin
  Mods := 0;
  if ModCtrl.active then
    Mods := Mods + Gdk_Control_Mask;
  if ModShift.active then
    Mods := Mods + Gdk_Shift_Mask;
  if ModAlt.active then
    Mods := Mods + Gdk_Mod1_Mask;
  KeyName.Text := AccelKeyName (Key.asinteger, mods);
end;

procedure TListWindow.Accel_Parse (Sender : TFPgtkObject; data : pointer);
var Mods : TGdkModifierType;
    K : guint;
begin
  AccelKeyParse (KeyName.Text, K, Mods);
  ModCtrl.active := (Mods and Gdk_Control_mask) <> 0;
  ModShift.active := (Mods and Gdk_Shift_mask) <> 0;
  ModAlt.active := (Mods and Gdk_Mod1_mask) <> 0;
  Key.Asinteger := k;
end;

procedure TListWindow.Accel_AddAccel (Sender : TFPgtkObject; data : pointer);
var Mods : TGdkModifierType;
begin
  Mods := 0;
  if ModCtrl.active then
    Mods := Mods + Gdk_Control_Mask;
  if ModShift.active then
    Mods := Mods + Gdk_Shift_Mask;
  if ModAlt.active then
    Mods := Mods + Gdk_Mod1_Mask;
  AcceleratorAdd (TheAG, AccelBut, sgClicked, Key.Asinteger, mods, GTK_ACCEL_VISIBLE);
  AKeyName.refetch;
end;

const ButBoxCount : integer = 0;

procedure TListWindow.ButBox_AddThem (Sender:TFPgtkObject; data:pointer);
begin
  inc (ButBoxCount);
  VButtons.PackStart (TFPgtkButton.CreateWithLabel (format ('But %d',[ButBoxCount])),false,false,1);
  HButtons.PackStart (TFPgtkButton.CreateWithLabel (format ('But %d',[ButBoxCount])),false,false,1);
end;

procedure TListWindow.Layout_runaway (Sender:TFPgtkObject; data:pointer);
begin
  randomize;
  LayoutX := LayoutX + 100 - random(200);
  if LayoutX < 0 then
    LayoutX := random(200);
  LayoutY := LayoutY + 100 - random(200);
  if LayoutY < 0 then
    LayoutY := random(200);
  TheLayout.move (Sender as TFPgtkWidget, LayoutX, LayoutY);
end;

procedure TListWindow.Calendar_ShowDate (Sender:TFPgtkObject; data:pointer);
begin
  ShowMessage ('Calendar', 'Date selected: ' + formatdatetime ('dd/mm/yyyy', cal.date));
end;

procedure TlistWindow.AddRemoveButton (Sender:TFPgtkObject; data:pointer);
var mdb : TMsgDlgBtn;
begin
  mdb := TMsgDlgBtn(data);
  if (Sender as TFPgtkToggleButton).Active then
    MessageButtons := MessageButtons + [mdb]
  else
    MessageButtons := MessageButtons - [mdb];
end;

procedure TlistWindow.ShowMessageDialog (Sender:TFPgtkObject; data:pointer);
begin
  MessageDlg (Msg.Text, TMsgDlgType(DialogType.ActiveIndex), MessageButtons, 0);
end;

end.
