{$mode objfpc}{$h+}
unit FPgtkExt;

interface

uses FPgtk, gtk, gdk, glib, sysutils, classes;

{ ==== Application object ==== }

type
  TFPgtkApplication = class
  Private
    FMainwindow : TFPgtkWindow;
    FMainDestroysignal : guint;
    procedure SetMainWindow (Value:TFPgtkWindow);
    procedure MainDestroyed (Sender:TFPgtkObject; data:pointer);
  Public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
    property Mainwindow : TFPgtkWindow read FMainwindow write SetMainwindow;
  end;

var
  Application : TFPgtkApplication;

{ ==== Extra Widgets ==== }

type

  TFPgtkFileEntry = class (TFPgtkHBox)
  private
    FEdit : TFPgtkEntry;
    FButton : TFPgtkButton;
    FImage : TFPgtkPixmap;
    procedure OpenFileSelection (Sender : TFPgtkObject; data : pointer);
    procedure CloseFileSelection (Sender:TFPgtkWindow; DialogResult:pointer;
                                    Action:integer; initiator:TFPgtkObject);
    procedure SetFilename (Value : string);
    function GetFilename : string;
  public
    constructor create;
    property Edit : TFPgtkEntry read FEdit;
    property Button : TFPgtkButton read FButton;
    property Image : TFPgtkPixmap read FImage;
    property Filename : string read GetFilename write SetFilename;
  end;

  TFPgtkCheckedButton = class (TFPgtkToggleButton)
  private
    FChecked, FUnchecked : TFPgtkPixmap;
    procedure ChangeCheck (Sender:TFPgtkObject; data:pointer);
  public
    constructor Create;
    constructor CreateWithLabel (aText:string);
    constructor CreateWithLabel (aText:string; AccelGroup : PGtkAccelGroup);
  end;

{ ==== Widget who needs a scrollwindow ==== }

type

  TFPgtkScrollText = class (TFPgtkScrolledWindow)
  private
    FText : TFPgtkText;
    procedure SetTooltip (Value : string);
    function GetTooltip : string;
    function GetUdpatePolicy : TGtkUpdateType;
    procedure SetUpdatePolicy (Value : TGtkUpdateType);
    function GetText : string;
    procedure SetText (Value : string);
    function GetLines : TStrings;
  public
    constructor create;
    procedure Clear;
    property TheText : TFPgtkText read FText;
    property Tooltip : string read GetTooltip write SetTooltip;
    property UpdatePolicy : TGtkUpdateType read GetUdpatePolicy write SetUpdatePolicy;
    property Text : string read GetText write SetText;
    property Lines : TStrings read GetLines;
  end;

  TFPgtkScrollList = class (TFPgtkScrolledWindow)
  private
    FList : TFPgtkList;
  public
    constructor create;
    property List : TFPgtkList read FList;
  end;

  TFPgtkScrollCList = class (TFPgtkScrolledWindow)
  private
    FCList : TFPgtkCList;
  public
    constructor create (CountColumns : integer);
    property CList : TFPgtkCList read FCList;
  end;

  TFPgtkScrollTree = class (TFPgtkScrolledWindow)
  private
    FTree : TFPgtkTree;
  public
    constructor create;
    property Tree : TFPgtkTree read FTree;
  end;

{ ==== Message dialogs ==== }

type
  TModalResult = Low(Integer)..High(Integer);

  TMsgDlgType = (mtWarning, mtError, mtInformation, mtConfirmation, mtCustom);
  TMsgDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                mbAll, mbNoToAll, mbYesToAll, mbHelp);
  TMsgDlgButtons = set of TMsgDlgBtn;

const
  mbYesNo            = [mbYes,mbNo];
  mbYesNoCancel      = [mbYes, mbNo, mbCancel];
  mbOKCancel         = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];

  mrNone = 0;
  mrOK = mrNone + 1;
  mrCancel = mrNone + 2;
  mrAbort = mrNone + 3;
  mrRetry = mrNone + 4;
  mrIgnore = mrNone + 5;
  mrYes = mrNone + 6;
  mrNo = mrNone + 7;
  mrAll = mrNone + 8;
  mrNoToAll = mrNone + 9;
  mrYesToAll = mrNone + 10;

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
                    Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;


function MessageDlg(const Fmt: string; Args : Array of const; DlgType: TMsgDlgType;
                    Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

procedure ShowMessage (const aTitle, aMessage : string);

{ ==== Menu handling ==== }

type
  TAccelKeyDef = record
    Key : guint;
    Mods : TGdkModifierType;
    AG : PGtkAccelGroup;
  end;
  PAccelKeyDef = ^TAccelKeyDef;

  TAccelModifier = (amShift, amLock, amControl, amMod1, amMod2, amMod3, amMod4,
                    amMod5, amButton1, amButton2, amButton3, amButton4, amButton5,
                    amRelease);
  TAccelModifiersSet = set of TAccelModifier;

const
  amAlt = amMod1;
  gdk_Alt_mask = gdk_mod1_Mask;
  DefaultAccelFlags : TGtkAccelFlags = GTK_ACCEL_VISIBLE;

function RemoveUnderscore (s : string) : string;
function ConvertAccelModifier (amSet : TAccelModifiersSet) : TGdkModifierType;
function ConvertModifierType (Mods : TGdkModifierType) : TAccelModifiersSet;
function MakeAccelKeyDef (aWindow : TFPgtkWindow; anAG : integer; aKey : guint; aMods : TGdkModifierType) : PAccelKeyDef; overload;
function MakeAccelKeyDef (aWindow : TFPgtkWindow; anAG : integer; aKey : guint; aMods : TAccelModifiersSet) : PAccelKeyDef; overload;
function MakeAccelKeyDef (anAG : PGtkAccelGroup; aKey : guint; aMods : TGdkModifierType) : PAccelKeyDef; overload;
function MakeAccelKeyDef (anAG : PGtkAccelGroup; aKey : guint; aMods : TAccelModifiersSet) : PAccelKeyDef; overload;

function NewMenuBar (items : array of TFPgtkMenuItem) : TFPgtkMenuBar;
function NewMenu (ATitle : string; items : array of TFPgtkMenuItem) : TFPgtkMenu;

function NewMenuItem (ACaption, AToolTip, AprivText : string; Accelerator : PAccelKeyDef;
                      ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem; overload;
function NewMenuItem (ACaption, AToolTip, AprivText : string;
                      ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem; overload;
function NewMenuItem (ACaption : string; Accelerator : PAccelKeyDef;
                      ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem; overload;
function NewMenuItem (ACaption : string; ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem; overload;
function NewMenuItem (ACaption : string) : TFPgtkMenuItem; overload;

function NewLine : TFPgtkMenuItem;
function NewTearOffMenu : TFPgtkTearOffMenuItem;

function NewSubMenu (ACaption, ATooltip, AprivText : string; Accelerator : PAccelKeyDef;
                     Items : array of TFPgtkMenuItem) : TFPgtkMenuItem; Overload;
function NewSubMenu (ACaption, ATooltip, AprivText : string;
                     Items : array of TFPgtkMenuItem) : TFPgtkMenuItem; Overload;
function NewSubMenu (ACaption : string; Accelerator : PAccelKeyDef;
                     Items : array of TFPgtkMenuItem) : TFPgtkMenuItem; Overload;
function NewSubMenu (ACaption : string; Items : array of TFPgtkMenuItem) : TFPgtkMenuItem; Overload;

function NewCheckMenuItem (ACaption, AToolTip, AprivText : string; Accelerator : PAccelKeyDef;
                      ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem; Overload;
function NewCheckMenuItem (ACaption, AToolTip, AprivText : string;
                      ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem; Overload;
function NewCheckMenuItem (ACaption : string; Accelerator : PAccelKeyDef;
                      ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem; Overload;
function NewCheckMenuItem (ACaption : string; ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem; Overload;

procedure InsertMenuItemGroup (InMenu : TFPgtkMenuShell; position : integer; MenuItems : TFPgtkItemGroup); Overload;
procedure InsertMenuItemGroup (InMenu : TFPgtkMenuShell; position : integer;
                 MenuItems : TFPgtkItemGroup; ActivateProc : TFPgtkSignalFunction; ActivateData : pointer); Overload;
procedure AppendMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup); Overload;
procedure AppendMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup;
                               ActivateProc : TFPgtkSignalFunction; ActivateData : pointer); Overload;
procedure PrependMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup); Overload;
procedure PrependMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup;
                                ActivateProc : TFPgtkSignalFunction; ActivateData : pointer); Overload;

implementation

resourcestring
  rsNothingToRun = 'No main window defined, nothing to do...';
  rsErrorTitle = 'Error occured';
  rsMessageTitle = 'Message';
  sErrWrongItemType = 'Items in list are not from TFPgtkMenuItem class.';

 { TFPgtkApplication }

constructor TFPgtkApplication.Create;
begin
  gtk_init (@argc, @argv);
  inherited create;
  FMainWindow := nil;
end;

destructor TFPgtkApplication.Destroy;
begin
  if assigned (FMainWindow) then
    FMainWindow.Free;
  gtk_Exit (0);
  inherited;
end;

procedure TFPgtkApplication.SetMainWindow (Value : TFPgtkWindow);
begin
  if FMainWindow <> Value then
    begin
    if assigned (FMainWindow) and (FMainDestroySignal > 0) then
      FMainWindow.signalDisconnect (FMainDestroySignal);
    FMainWindow := Value;
    if Assigned (Value) then
      FMainDestroySignal := FMainWindow.ConnectDestroy (@MainDestroyed, nil);
    end;
end;

procedure TFPgtkApplication.MainDestroyed (Sender:TFPgtkObject; data:pointer);
begin
  FMainWindow := nil;
  FMainDestroySignal := 0;
  gtk_main_quit;
end;

procedure TFPgtkApplication.Run;
begin
  if assigned (FMainWindow) then
    while assigned (FMainWindow) do
      try
        FMainWindow.execute (nil, nil, nil);
        //gtk_main;
        FreeFPgtkObjects (nil);
      except
        on e : exception do
          ShowMessage (rsErrorTitle, e.message);
      end
  else
    ShowMessage (rsMessageTitle, rsNothingToRun);
end;

{ TFPgtkScrollText }

constructor TFPgtkScrollText.create;
begin
  inherited create (nil,nil);
  FText := TFPgtkText.Create;
  Add (FText);
  HPolicy := Gtk_Policy_Never;
end;

function TFPgtkScrollText.GetTooltip : string;
begin
  result := inherited Tooltip;
end;

procedure TFPgtkScrollText.SetTooltip (Value : string);
begin
  TheText.Tooltip := Value;
  inherited Tooltip := Value;
end;

function TFPgtkScrollText.GetUdpatePolicy : TGtkUpdateType;
begin
  result := VScrollbar.UpdatePolicy;
end;

procedure TFPgtkScrollText.Clear;
begin
  if assigned(TheText) then
    TheText.Clear;
end;

procedure TFPgtkScrollText.SetUpdatePolicy (Value : TGtkUpdateType);
begin
  VScrollbar.UpdatePolicy := Value;
  {$ifndef gtkwin}
  HScrollbar.UpdatePolicy := Value;
  {$endif}
end;

function TFPgtkScrollText.GetText : string;
begin
  if assigned(TheText) then
    begin
    result := TheText.Text;
    end
  else
    begin
    result := '';
    end;
end;

procedure TFPgtkScrollText.SetText (Value : string);
begin
  if assigned (TheText) then
    TheText.Text := Value;
end;

function TFPgtkScrollText.GetLines : TStrings;
begin
  if assigned (TheText) then
    result := TheText.Lines
  else
    result := nil;
end;

{ TFPgtkScrollList }

constructor TFPgtkScrollList.create;
begin
  inherited create (nil, nil);
  setusize (100, 40);
  FList := TFPgtkList.Create;
  AddWithViewport (FList);
end;

{ TFPgtkScrollCList }

constructor TFPgtkScrollCList.create (CountColumns : integer);
begin
  inherited create (nil, nil);
  setusize (100, 40);
  FCList := TFPgtkCList.Create (CountColumns);
  Add (FCList);
end;

{ TFPgtkScrollTree }

constructor TFPgtkScrollTree.create;
begin
  inherited create (nil, nil);
  FTree := TFPgtkTree.Create;
  AddWithViewport (FTree);
  FTree.Show;
end;

{ Menu functions }

function RemoveUnderscore (s : string) : string;
begin
  result := stringreplace (s, '_', '', [rfReplaceAll]);
end;

type
  TFPgtkMenuItemType = class of TFPgtkMenuItem;

function MakeAccelKeyDef (aWindow : TFPgtkWindow; anAG : integer; aKey : guint; aMods : TGdkModifierType) : PAccelKeyDef;
begin
  new (result);
  with result^ do
    begin
    AG := aWindow.AccelGroups[anAG];
    Key := aKey;
    Mods := aMods;
    end;
end;

function MakeAccelKeyDef (aWindow : TFPgtkWindow; anAG : integer; aKey : guint; aMods : TAccelModifiersSet) : PAccelKeyDef;
begin
  new (result);
  with result^ do
    begin
    AG := aWindow.AccelGroups[anAG];
    Key := aKey;
    Mods := ConvertAccelModifier (aMods);
    end;
end;

function MakeAccelKeyDef (anAG : PGtkAccelGroup; aKey : guint; aMods : TGdkModifierType) : PAccelKeyDef;
begin
  new (result);
  with result^ do
    begin
    AG := anAG;
    Key := aKey;
    Mods := aMods;
    end;
end;

function MakeAccelKeyDef (anAG : PGtkAccelGroup; aKey : guint; aMods : TAccelModifiersSet) : PAccelKeyDef;
begin
  new (result);
  with result^ do
    begin
    AG := anAG;
    Key := aKey;
    Mods := ConvertAccelModifier (aMods);
    end;
end;

function ConvertAccelModifier (amSet : TAccelModifiersSet) : TGdkModifierType;
var am : TAccelModifier;
begin
  result := 0;
  for am := low(TAccelModifier) to high (TAccelModifier) do
    if am in amSet then
      result := result + (1 shl ord(am));
end;

function ConvertModifierType (Mods : TGdkModifierType) : TAccelModifiersSet;
var am : TAccelModifier;
begin
  result := [];
  for am := low(TAccelModifier) to high (TAccelModifier) do
    if (Mods and (1 shl ord(am))) <> 0 then
      result := result + [am];
end;

function NewMenuBar (items : array of TFPgtkMenuItem) : TFPgtkMenuBar;
var r : integer;
begin
  result := TFPgtkMenuBar.Create;
  with result do
    for r := low(items) to high (items) do
      append (items[r]);
end;

function NewMenu (ATitle : string; items : array of TFPgtkMenuItem) : TFPgtkMenu;
var r : integer;
    AG : PGtkAccelGroup;
    m : TFPgtkMenuItem;
begin
  result := TFPgtkMenu.Create;
  with result do
    begin
    Title := ATitle;
    ag := AccelGroup;
    for r := low(items) to high(items) do
      begin
      m := items[r];
      Append (m);
      if m.AccelKey <> 0 then
        m.AcceleratorAdd (AG, sgActivateItem, m.AccelKey, 0, TGtkAccelFlags(0));
      end;
    end;
end;

function CreateMenuItem (Atype : TFPgtkMenuItemType; ACaption, ATooltip,
                         APrivText : string; Accelerator : PAccelKeyDef) : TFPgtkMenuItem;
begin
  result := AType.CreateWithLabel (ACaption);
  if (ATooltip <> '') or (APrivText <> '') then
    result.Tooltip := ComposeTooltip (ATooltip, APrivText);
  if assigned(accelerator) then
    begin
    with Accelerator^ do
      result.AcceleratorAdd (AG, sgActivateItem, Key, Mods, DefaultAccelFlags);
    dispose (Accelerator);
    end;
end;

function NewMenuItem (ACaption, AToolTip, AprivText : string; Accelerator : PAccelKeyDef;
                      ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem;
begin
  result := CreateMenuItem (TFPgtkMenuItem, ACaption, ATooltip, APrivtext, Accelerator);
  if assigned (ActivateFunc) then
    result.ConnectActivate (ActivateFunc, AData);
end;

function NewMenuItem (ACaption, AToolTip, AprivText : string;
                      ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem;
begin
  result := NewMenuItem (aCaption, aTooltip, aPrivText, nil, ActivateFunc, aData);
end;

function NewMenuItem (ACaption : string; Accelerator : PAccelKeyDef;
                      ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem;
begin
  result := NewMenuItem (aCaption, '', '', Accelerator, ActivateFunc, aData);
end;

function NewMenuItem (ACaption : string; ActivateFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkMenuItem;
begin
  result := NewMenuItem (aCaption, '', '', nil, ActivateFunc, aData);
end;

function NewMenuItem (ACaption : string) : TFPgtkMenuItem;
begin
  result := NewMenuItem (aCaption, '', '', nil, nil, nil);
end;

function NewLine : TFPgtkMenuItem;
begin
  result := TFPgtkMenuItem.Create;
end;

function NewTearOffMenu : TFPgtkTearOffMenuItem;
begin
  result := TFPgtkTearOffMenuItem.create;
end;

function NewSubMenu (ACaption, ATooltip, AprivText : string; Accelerator : PAccelKeyDef;
                     Items : array of TFPgtkMenuItem) : TFPgtkMenuItem;
begin
  result := CreateMenuItem (TFPgtkMenuItem, ACaption, ATooltip, APrivText, Accelerator);
  result.SetSubmenu (NewMenu ('', Items));
end;

function NewSubMenu (ACaption, ATooltip, AprivText : string;
                     Items : array of TFPgtkMenuItem) : TFPgtkMenuItem;
begin
  result := NewSubMenu (aCaption, aTooltip, aPrivText, nil, Items);
end;

function NewSubMenu (ACaption : string; Accelerator : PAccelKeyDef;
                     Items : array of TFPgtkMenuItem) : TFPgtkMenuItem;
begin
  result := NewSubMenu (aCaption, '', '', Accelerator, Items);
end;

function NewSubMenu (ACaption : string; Items : array of TFPgtkMenuItem) : TFPgtkMenuItem;
begin
  result := NewSubMenu (aCaption, '', '', nil, Items);
end;

function NewCheckMenuItem (ACaption, AToolTip, AprivText : string; Accelerator : PAccelKeyDef;
                      ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem;
begin
  result := TFPgtkCheckMenuItem(CreateMenuItem (TFPgtkCheckMenuItem, ACaption, ATooltip, APrivText, Accelerator));
  if assigned (ToggledFunc) then
    Result.ConnectToggled (ToggledFunc, AData);
end;

function NewCheckMenuItem (ACaption, AToolTip, AprivText : string;
                      ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem;
begin
  result := NewCheckMenuItem (aCaption, aToolTip, aPrivText, nil, ToggledFunc, AData);
end;

function NewCheckMenuItem (ACaption : string; Accelerator : PAccelKeyDef;
                      ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem;
begin
  result := NewCheckMenuItem (aCaption, '', '', Accelerator, ToggledFunc, AData);
end;

function NewCheckMenuItem (ACaption : string; ToggledFunc : TFPgtkSignalFunction; AData : pointer) : TFPgtkCheckMenuItem;
begin
  result := NewCheckMenuItem (aCaption, '', '', nil, ToggledFunc, AData);
end;

procedure InsertMenuItemGroup (InMenu : TFPgtkMenuShell; position : integer; MenuItems : TFPgtkItemGroup);
begin
  InsertMenuItemGroup (InMenu, position, MenuItems, nil, nil);
end;

procedure InsertMenuItemGroup (InMenu : TFPgtkMenuShell; position : integer;
                 MenuItems : TFPgtkItemGroup; ActivateProc : TFPgtkSignalFunction; ActivateData : pointer);
var r : integer;
begin
  if (MenuItems.count > 0) then
    if (MenuItems.items[0] is TFPgtkMenuItem) then
      with InMenu do
        for r := MenuItems.count-1 downto 0 do
          begin
          if assigned(ActivateProc) then
            if assigned (ActivateData) then
              TFPgtkMenuItem(MenuItems.items[r]).ConnectActivate (ActivateProc, ActivateData)
            else
              TFPgtkMenuItem(MenuItems.items[r]).ConnectActivate (ActivateProc, inttopointer(r));
          Insert (TFPgtkMenuItem(MenuItems.items[r]), position);
          end
    else
      raise FPgtkException.Create (sErrWrongItemType);
end;

procedure AppendMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup);
begin
  AppendMenuItemGroup (InMenu, MenuItems, nil, nil);
end;

procedure AppendMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup;
                                ActivateProc : TFPgtkSignalFunction; ActivateData : pointer);
var r : integer;
begin
  if (MenuItems.count > 0) then
    if MenuItems.items[0] is TFPgtkMenuItem then
      with InMenu do
        for r := 0 to MenuItems.count-1 do
          begin
          if assigned(ActivateProc) then
            if assigned (ActivateData) then
              TFPgtkMenuItem(MenuItems.items[r]).ConnectActivate (ActivateProc, ActivateData)
            else
              TFPgtkMenuItem(MenuItems.items[r]).ConnectActivate (ActivateProc, inttopointer(r));
          Append (TFPgtkMenuItem(MenuItems.items[r]));
          end
    else
      raise FPgtkException.Create (sErrWrongItemType);
end;

procedure PrependMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup);
begin
  PrependMenuItemGroup (InMenu, MenuItems, nil, nil);
end;

procedure PrependMenuItemGroup (InMenu : TFPgtkmenuShell; MenuItems : TFPgtkItemGroup;
                                ActivateProc : TFPgtkSignalFunction; ActivateData : pointer);
var r : integer;
begin
  if (MenuItems.count > 0) then
    if MenuItems.items[0] is TFPgtkMenuItem then
      with InMenu do
        for r := MenuItems.count-1 downto 0 do
          begin
          if assigned(ActivateProc) then
            if assigned (ActivateData) then
              TFPgtkMenuItem(MenuItems.items[r]).ConnectActivate (ActivateProc, ActivateData)
            else
              TFPgtkMenuItem(MenuItems.items[r]).ConnectActivate (ActivateProc, inttopointer(r));
          Prepend (TFPgtkMenuItem(MenuItems.items[r]));
          end
    else
      raise FPgtkException.Create (sErrWrongItemType);
end;

{ TFileEntryDialog }

type
  TFileEntryDialog = class (TFPgtkFileSelection)
  public
    constructor create (AType:TGtkWindowType);
    procedure DoDialogInit (InitData : pointer); override;
  end;

  PFileEntryData = ^TFileEntryData;
  TFileEntryData = record
    aFilename : string;
  end;

constructor TFileEntryDialog.Create (AType:TGtkWindowType);
begin
  inherited;
  OKButton.ConnectClicked (@CloseWithResult, inttopointer(drOk));
  CancelButton.ConnectClicked (@CloseWindow, nil);
end;

procedure TFileEntryDialog.DoDialogInit (InitData : pointer);
begin
  with PFileEntryData(InitData)^ do
    Filename := aFilename;
end;

{ TFPgtkFileEntry }

const
  FileEntryXPM =
      '16 13 4 1'#13#10+
      '. c None'#13#10+     // no color
      '# c #000000'#13#10+  // black
      'y c #ffff00'#13#10+  // yellow
      'g c #AFAF00'#13#10+  // grayed yellow
      '.......#####....'#13#10+
      '............#.#.'#13#10+
      '.............##.'#13#10+
      '..####......###.'#13#10+
      '##yyyy#####.....'#13#10+
      '#yyyyyyyyy#.....'#13#10+
      '#yyyyyyyyy#.....'#13#10+
      '#yyyy###########'#13#10+
      '#yyy#ggggggggg#.'#13#10+
      '#yy#ggggggggg#..'#13#10+
      '#y#ggggggggg#...'#13#10+
      '##ggggggggg#....'#13#10+
      '###########.....';

var
  DefFileEntryPixmap : PGdkPixmap;
  DefFileEntryBitmask : PGdkBitmap;

constructor TFPgtkFileEntry.create;
begin
  inherited;
  FEdit := TFPgtkEntry.Create;
  FButton := TFPgtkButton.Create;
  FImage := TFPgtkPixMap.Create;
  with FImage do
    if assigned (DefFileEntryPixmap) then
      SetPixmap (DefFileEntryPixmap, DefFileEntryBitmask)
    else
      begin
      loadfromtext (FileEntryXPM);
      GetPixmap (DefFileEntryPixmap, DefFileEntryBitmask);
      end;
  with FButton do
    begin
    Add (FImage);
    ConnectClicked (@OpenFileSelection, self);
    end;
  PackStart (FEdit, true, true, 0);
  PackStart (FButton, false, true, 0);
end;

procedure TFPgtkFileEntry.SetFilename (Value : string);
begin
  FEdit.Text := Value;
end;

function TFPgtkFileEntry.GetFilename : string;
begin
  result := FEdit.Text;
end;

procedure TFPgtkFileEntry.OpenFileSelection (Sender : TFPgtkObject; data : pointer);
var d : TFileEntryData;
begin
  d.aFilename := Filename;
  with TFileEntryDialog.Create(gtk_window_dialog) do
    Execute (nil, @d, @CloseFileSelection);
end;

procedure TFPgtkFileEntry.CloseFileSelection (Sender:TFPgtkWindow; DialogResult:pointer;
                                    Action:integer; initiator:TFPgtkObject);
begin
  if action = drOk then
    Filename := (Sender as TFileEntryDialog).Filename;
end;

{ TFPgtkCheckedButton }

const
  XPMChecked : array [0..17] of ansistring = (
          '15 13 4 1',
          '. c None',      // None
          '# c #000000',   // Black
          '- c #FFFFFF',   // White
          'o c #0000FF',   // Blue
          '..............o',
          '.............o-',
          '............o-.',
          '..########.o-..',
          '..#......#o-...',
          '..#......o-....',
          '..o-....oo-....',
          '.ooo-..oo-.....',
          '..ooo-oo-#.....',
          '..#oooo-.#.....',
          '..##ooo-##.....',
          '.....o-........',
          '...............');

  XPMUnChecked : array [0..17] of ansistring = (
          '15 13 4 1',
          '. c None',      // None
          '# c #000000',   // Black
          '- c #FFFFFF',   // White
          'o c #0000FF',   // Blue
          '...............',
          '...............',
          '...............',
          '..########.....',
          '..#......#.....',
          '..#......#.....',
          '..#......#.....',
          '..#......#.....',
          '..#......#.....',
          '..#......#.....',
          '..########.....',
          '...............',
          '...............');

var
  DefChecked, DefUnchecked : PGdkPixmap;
  DefCheckedBM, DefUncheckedBM : PGdkBitmap;

procedure TFPgtkCheckedButton.ChangeCheck (Sender:TFPgtkObject; data:pointer);
var b : boolean;
begin
  b := Active;
  FChecked.visible := b;
  FUnchecked.visible := not b;
end;

constructor TFPgtkCheckedButton.CreateWithLabel (aText:string);
begin
  create;
  Text := aText;
end;

constructor TFPgtkCheckedButton.CreateWithLabel (aText:string; AccelGroup : PGtkAccelGroup);
begin
  create;
  Text := aText;
  if (AccelKey <> 0) and assigned(AccelGroup) then
    AcceleratorAdd (AccelGroup, sgClicked, AccelKey, DefaultButtonModifiers, GTK_ACCEL_Visible);
end;

constructor TFPgtkCheckedButton.create;
begin
  inherited;
  DrawIndicator := False;
  AddContainer := TFPgtkHBox.Create;
  Add (AddContainer);
  FChecked := TFPgtkPixMap.Create;
  with FChecked do
    if assigned (DefChecked) then
      SetPixmap (DefChecked, DefCheckedBM)
    else
      begin
      loadfromArray (XPMChecked);
      GetPixmap (DefChecked, DefCheckedBM);
      end;
  FUnchecked := TFPgtkPixMap.Create;
  with FUnchecked do
    if assigned (DefUnchecked) then
      SetPixmap (DefUnchecked, DefUncheckedBM)
    else
      begin
      loadfromArray (XPMUnchecked);
      GetPixmap (DefUnchecked, DefUncheckedBM);
      end;
  with TFPgtkBox(AddContainer) do
    begin
    PackStart (FChecked, false, false, 0);
    PackStart (FUnChecked, false, false, 0);
    end;
  ChangeCheck (self, nil);
  ConnectToggled (@ChangeCheck, nil);
end;

{ ShowMessage }

resourcestring
  rsOk = '   Ok   ';

function MessageWindow (aTitle, aMessage : string) : TFPgtkWindow;
var b : TFPgtkBox;
    but : TFPgtkButton;
    l : TFPgtkLabel;
    AG : integer;
    bb : TFPgtkButtonBox;
begin
  result := TFPgtkWindow.create (gtk_window_dialog);

  result.setDefaultSize (200,25);
  result.title := aTitle;

  AG := result.AccelGroupNew;

  b := TFPgtkVBox.create;
  b.Homogeneous := false;
  b.border := 15;
  b.spacing := 15;

  l := TFPgtkLabel.Create (aMessage);
  b.Packstart (l, true, true, 0); // Text to show

  bb := TFPgtkHButtonBox.create;
  bb.Layout := GTK_BUTTONBOX_DEFAULT_STYLE;
  b.PackEnd (bb, false, false, 0);

  but := TFPgtkButton.CreateWithLabel (rsOk);                 // Ok button to close
  but.ConnectClicked (@(result.CloseWindow), nil);
  result.AcceleratorAdd (AG, but, sgClicked, gdk_Cancel, 0, TGTKAccelFlags(0));
  result.AcceleratorAdd (AG, but, sgClicked, gdk_Return, 0, TGTKAccelFlags(0));
  bb.add (but);

  result.Add (b);
end;

procedure ShowMessage (const aTitle, aMessage : string);
begin
  with MessageWindow (aTitle, aMessage) do
    Execute (nil, nil, nil);
end;

{ MessageDialog }

type
  TMessageDialogWindow = Class(TFPgtkWindow)
    FImage : TFPGtkPixMap;
    FLabel : TFPGtkLabel;
    FLTable : TFPgtkTable;
    FVBox : TFPgtkVBox;
    FButtonBox: TFPgtkButtonBox;
    Constructor Create(AMsg:String; DlgType:TMsgDlgType; Buttons: TMsgDlgButtons);
    Procedure CreateButtons(Buttons: TMsgDlgButtons);
  end;

const

IMGInfo : Array[1..37] of string = ('32 32 4 1',
  '. c None',
  '  c None',
  'a c #ffffff', //#c3c3c3',
  '# c #0000ff',
  '............#######.............',
  '.........###aaaaaaa###..........',
  '.......##aaaaaaaaaaaaa##........',
  '......#aaaaaaa###aaaaaaa#.......',
  '.....#aaaaaaa#####aaaaaaa#......',
  '....#aaaaaaa#######aaaaaaa#.....',
  '...#aaaaaaaa#######aaaaaaaa#....',
  '..#aaaaaaaaa#######aaaaaaaaa#...',
  '..#aaaaaaaaaa#####aaaaaaaaaa#...',
  '.#aaaaaaaaaaaa###aaaaaaaaaaaa#..',
  '.#aaaaaaaaaaaaaaaaaaaaaaaaaaa#..',
  '.#aaaaaaaaaaa#####aaaaaaaaaaa#..',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '#aaaaaaaaaaaa#####aaaaaaaaaaaa#.',
  '.#aaaaaaaaaaa#####aaaaaaaaaaa#..',
  '.#aaaaaaaaaaa#####aaaaaaaaaaa#..',
  '.#aaaaaaaaaa#######aaaaaaaaaa#..',
  '..#aaaaaaaaa#######aaaaaaaaa#...',
  '..#aaaaaaaaa#######aaaaaaaaa#...',
  '...#aaaaaaaaaaaaaaaaaaaaaaa#....',
  '....#aaaaaaaaaaaaaaaaaaaaa#.....',
  '.....#aaaaaaaaaaaaaaaaaaa#......',
  '......#aaaaaaaaaaaaaaaaa#.......',
  '.......##aaaaaaaaaaaaa##........',
  '.........###aaaaaaa###..........',
  '............#######.............',
  '................................');

IMGWarning :Array[1..37] of string = ('32 32 4 1',
  '# c #000000',
  'b c #9c999c',
  '. c None',
  'a c #ffff00',
  '.............###................',
  '............#aaa#...............',
  '...........#aaaaa#b.............',
  '...........#aaaaa#bb............',
  '..........#aaaaaaa#bb...........',
  '..........#aaaaaaa#bb...........',
  '.........#aaaaaaaaa#bb..........',
  '.........#aaaaaaaaa#bb..........',
  '........#aaaaaaaaaaa#bb.........',
  '........#aaaa###aaaa#bb.........',
  '.......#aaaa#####aaaa#bb........',
  '.......#aaaa#####aaaa#bb........',
  '......#aaaaa#####aaaaa#bb.......',
  '......#aaaaa#####aaaaa#bb.......',
  '.....#aaaaaa#####aaaaaa#bb......',
  '.....#aaaaaa#####aaaaaa#bb......',
  '....#aaaaaaaa###aaaaaaaa#bb.....',
  '....#aaaaaaaa###aaaaaaaa#bb.....',
  '...#aaaaaaaaa###aaaaaaaaa#bb....',
  '...#aaaaaaaaaa#aaaaaaaaaa#bb....',
  '..#aaaaaaaaaaa#aaaaaaaaaaa#bb...',
  '..#aaaaaaaaaaaaaaaaaaaaaaa#bb...',
  '.#aaaaaaaaaaaa##aaaaaaaaaaa#bb..',
  '.#aaaaaaaaaaa####aaaaaaaaaa#bb..',
  '#aaaaaaaaaaaa####aaaaaaaaaaa#bb.',
  '#aaaaaaaaaaaaa##aaaaaaaaaaaa#bb.',
  '#aaaaaaaaaaaaaaaaaaaaaaaaaaa#bbb',
  '#aaaaaaaaaaaaaaaaaaaaaaaaaaa#bbb',
  '.#aaaaaaaaaaaaaaaaaaaaaaaaa#bbbb',
  '..#########################bbbbb',
  '....bbbbbbbbbbbbbbbbbbbbbbbbbbb.',
  '.....bbbbbbbbbbbbbbbbbbbbbbbbb..');

IMGError : Array[1..37] of string = ('32 32 4 1',
  '. c None',
  'b c #808080',
  '# c #c00000',
  'a c #ffffff',
  '................................',
  '................................',
  '................................',
  '............#######.............',
  '...........###########..........',
  '........###############.........',
  '.......##################.......',
  '......####################......',
  '.....###aa############aa###.....',
  '.....###aaa##########aaa###.....',
  '....#####aaa########aaa#####....',
  '....######aaa######aaa######....',
  '...########aaa####aaa########...',
  '...#########aaa##aaa#########b..',
  '...##########aaaaaa##########b..',
  '...###########aaaa###########b..',
  '...###########aaaa###########b..',
  '...##########aaaaaa##########b..',
  '...#########aaa##aaa#########b..',
  '...########aaa####aaa#######bb..',
  '....######aaa######aaa######bb..',
  '.....####aaa########aaa#####bb..',
  '.....###aaa##########aaa###bbb..',
  '.....###aa############aa##bbb...',
  '......####################bb....',
  '.......##################bb.....',
  '.........###############bb......',
  '..........###########bbbb.......',
  '.............#######bbb.........',
  '................................',
  '................................',
  '................................');

IMGConfirmation : Array[1..37] of string = ('32 32 4 1',
  '. c None',
  'b c #808080',
  'a c #c00000',
  '# c #ffffff',
  '................................',
  '................................',
  '................................',
  '................................',
  '.............######.............',
  '..........###########...........',
  '.........##############.........',
  '........################........',
  '.......##################.......',
  '......########aaaaa#######......',
  '.....########aaaaaaa#######.....',
  '.....#######aa#####aa######.....',
  '.....#######a######aa#######....',
  '....###############aa#######b...',
  '....###############aa#######bb..',
  '....##############aa########bb..',
  '....#############aa#########bb..',
  '....############aa##########bb..',
  '....###########aa###########bb..',
  '.....##########aa##########bbb..',
  '.....##########aa##########bbb..',
  '.....##########aa##########bb...',
  '......#########aa#########bb....',
  '.......##################bbb....',
  '........#######aa#######bbb.....',
  '.........######aa######bbb......',
  '...........###########bbb.......',
  '.............######bbbbb........',
  '................................',
  '................................',
  '................................',
  '................................');


Constructor TMessageDialogWindow.Create(AMsg : String;DlgType:TMsgDlgType;Buttons: TMsgDlgButtons);
const
  OH = GTK_FILL OR GTK_EXPAND;
begin
  Inherited Create(GTK_WINDOW_DIALOG);
  FVBox:=TFPGtkVBox.Create;
  FVBox.Spacing:=4;
  FVBox.Border:=8;
  Add(FVBox);
  FLTable:=TFpgtkTable.Create(10,1);
  if DlgType <> mtCustom then
    begin
    FImage:=TFPGtkPixMap.Create;
    With FImage do
      Case DlgType of
        mtInformation  : LoadFromArray(Imginfo);
        mtWarning      : LoadFromArray(imgWarning);
        mtConfirmation : LoadFromArray(imgConfirmation);
        mtError        : LoadFromArray(imgError);
      end;
    FLTable.Attach(FImage,1,2,0,1,OH,OH,0,0);
    end;
  FLabel:=TFPGtkLabel.Create(Amsg);
  FLTable.Attach(FLabel,4,9,0,1,OH,OH,0,0);
  FButtonBox:=TFPgtkHButtonBox.Create;
  with FButtonBox do
    begin
    Layout := GTK_BUTTONBOX_DEFAULT_STYLE;
    spacing := 4;
    end;
  CreateButtons(Buttons);
  FVBox.PackStart(FLTable,false,False,8);
  FVBox.PackStart(FButtonBox,false,False,8);
end;

Const
  ButtonText : Array[TMsgDlgBtn] of string  =
       ('Yes', 'No', 'OK', 'Cancel','Abort', 'Retry', 'Ignore',
        'All', 'NoToAll', 'YesToAll', 'Help');
  ButtonResult : array [TMsgDlgbtn] of TModalResult =
       (mrYes, mrNo, mrOK, mrCAncel, mrAbort, mrRetry, mrIgnore,
        mrAll, mrNoToAll, mrYesToAll, 0);

Procedure TMessageDialogWindow.CreateButtons(Buttons: TMsgDlgButtons);
Var
  b : TMsgDlgBtn;
  bw : TFPGtkButton;
begin
  For B:=Low(TMsgDlgBtn) to high(TMsgDlgBtn) do
    If b in Buttons then
      begin
      BW:=TFPGtkButton.CreateWithLabel(ButtonText[b]);
      BW.ConnectClicked(@CloseWithResult,IntToPointer(ButtonResult[b]));
      BW.Setusize(50,25);
      FButtonBox.PackStart(BW,False,False,4);
      end;
end;

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
                    Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  With TMessageDialogWindow.Create(AMsg,DlgType,Buttons) do
    Result:=Execute(Nil,Nil,Nil);
end;

function MessageDlg(const Fmt: string; Args : Array of const; DlgType: TMsgDlgType;
                    Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
begin
  Result:=MessageDlg(Format(Fmt,Args),Dlgtype,Buttons,HelpCtx);
end;

end.
