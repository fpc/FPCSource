{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Views and view-related functions for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPViews;

interface

uses
  Dos,Objects,Drivers,Commands,HelpCtx,Views,Menus,Dialogs,App,
{$ifdef EDITORS}
  Editors,
{$else}
  WEditor,
{$endif}
  WHlpView,
  Comphook,
  FPConst;

type
{$IFNDEF EDITORS}
    TEditor = TCodeEditor; PEditor = PCodeEditor;
{$ENDIF}

    PCenterDialog = ^TCenterDialog;
    TCenterDialog = object(TDialog)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr);
    end;

    PIntegerLine = ^TIntegerLine;
    TIntegerLine = object(TInputLine)
      constructor Init(var Bounds: TRect; AMin, AMax: longint);
    end;

    PIDEHelpWindow = ^TIDEHelpWindow;
    TIDEHelpWindow = object(THelpWindow)
      procedure HandleEvent(var Event: TEvent); virtual;
      function  GetPalette: PPalette; virtual;
    end;

    PSourceEditor = ^TSourceEditor;
    TSourceEditor = object(TFileEditor)
{$ifndef EDITORS}
      function  IsReservedWord(S: string): boolean; virtual;
      function  GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer; virtual;
      function  GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string; virtual;
{$endif}
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   LocalMenu(P: TPoint); virtual;
      function    GetLocalMenu: PMenu; virtual;
      function    GetCommandTarget: PView; virtual;
    private
      LastLocalCmd : word;
    end;

    PSourceWindow = ^TSourceWindow;
    TSourceWindow = object(TWindow)
      Editor    : PSourceEditor;
      Indicator : PIndicator;
      constructor Init(var Bounds: TRect; AFileName: PathStr);
      procedure   SetTitle(ATitle: string); virtual;
      procedure   UpdateTitle; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   Update; virtual;
      procedure   UpdateCommands; virtual;
      function    GetPalette: PPalette; virtual;
      destructor  Done; virtual;
    end;

    PClipboardWindow = ^TClipboardWindow;
    TClipboardWindow = object(TSourceWindow)
      constructor Init;
      procedure   Close; virtual;
      destructor  Done; virtual;
    end;

    PAdvancedMenuBox = ^TAdvancedMenuBox;
    TAdvancedMenuBox = object(TMenuBox)
      function NewSubView(var Bounds: TRect; AMenu: PMenu;
                 AParentMenu: PMenuView): PMenuView; virtual;
      function Execute: Word; virtual;
    end;

    PAdvancedMenuPopUp = ^TAdvancedMenuPopup;
    TAdvancedMenuPopUp = object(TMenuPopup)
      function NewSubView(var Bounds: TRect; AMenu: PMenu;
                 AParentMenu: PMenuView): PMenuView; virtual;
      function Execute: Word; virtual;
    end;

    PAdvancedMenuBar = ^TAdvancedMenuBar;
    TAdvancedMenuBar = object(TMenuBar)
      constructor Init(var Bounds: TRect; AMenu: PMenu);
      function  NewSubView(var Bounds: TRect; AMenu: PMenu;
                  AParentMenu: PMenuView): PMenuView; virtual;
      procedure Update; virtual;
      procedure HandleEvent(var Event: TEvent); virtual;
      function  Execute: Word; virtual;
    end;

    PAdvancedStaticText = ^TAdvancedStaticText;
    TAdvancedStaticText = object(TStaticText)
      procedure SetText(S: string); virtual;
    end;

    PAdvancedListBox = ^TAdvancedListBox;
    TAdvancedListBox = object(TListBox)
      Default: boolean;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;

    PColorStaticText = ^TColorStaticText;
    TColorStaticText = object(TAdvancedStaticText)
      Color: word;
      DontWrap: boolean;
      Delta: TPoint;
      constructor Init(var Bounds: TRect; AText: String; AColor: word);
      procedure   Draw; virtual;
    end;

    PUnsortedStringCollection = ^TUnsortedStringCollection;
    TUnsortedStringCollection = object(TCollection)
      procedure FreeItem(Item: Pointer); virtual;
    end;

    PHSListBox = ^THSListBox;
    THSListBox = object(TListBox)
      constructor Init(var Bounds: TRect; ANumCols: Word; AHScrollBar, AVScrollBar: PScrollBar);
    end;

    PDlgWindow = ^TDlgWindow;
    TDlgWindow = object(TDialog)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Integer);
    end;

    PAdvancedStatusLine = ^TAdvancedStatusLine;
    TAdvancedStatusLine = object(TStatusLine)
      StatusText: PString;
      function  GetStatusText: string; virtual;
      procedure SetStatusText(S: string); virtual;
      procedure ClearStatusText; virtual;
      procedure Draw; virtual;
    end;

    PMessageItem = ^TMessageItem;
    TMessageItem = object(TObject)
      TClass    : longint;
      Text      : PString;
      Module    : PString;
      ID        : longint;
      constructor Init(AClass: longint; AText, AModule: string; AID: longint);
      function    GetText(MaxLen: integer): string; virtual;
      procedure   Selected; virtual;
      destructor  Done; virtual;
    end;

    PMessageListBox = ^TMessageListBox;
    TMessageListBox = object(THSListBox)
      Transparent: boolean;
      NoSelection: boolean;
      MaxWidth: integer;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   AddItem(P: PMessageItem); virtual;
      procedure   Clear; virtual;
      function    GetText(Item: Integer; MaxLen: Integer): String; virtual;
      procedure   Draw; virtual;
      destructor  Done; virtual;
    end;

    PCompilerMessage = ^TCompilerMessage;
    TCompilerMessage = object(TMessageItem)
      function    GetText(MaxLen: Integer): String; virtual;
    end;

    PProgramInfoWindow = ^TProgramInfoWindow;
    TProgramInfoWindow = object(TDlgWindow)
      InfoST: PStaticText;
      LogLB : PMessageListBox;
      constructor Init;
      procedure   AddMessage(AClass: longint; Msg, Module: string; Line: longint);
      procedure   SizeLimits(var Min, Max: TPoint); virtual;
      procedure   Close; virtual;
    end;

    PTabItem = ^TTabItem;
    TTabItem = record
      Next : PTabItem;
      View : PView;
      Dis  : boolean;
    end;

    PTabDef = ^TTabDef;
    TTabDef = record
      Next     : PTabDef;
      Name     : PString;
      Items    : PTabItem;
      DefItem  : PView;
      ShortCut : char;
    end;

    PTab = ^TTab;
    TTab = object(TGroup)
      TabDefs   : PTabDef;
      ActiveDef : integer;
      DefCount  : word;
      constructor Init(var Bounds: TRect; ATabDef: PTabDef);
      function    AtTab(Index: integer): PTabDef; virtual;
      procedure   SelectTab(Index: integer); virtual;
      function    TabCount: integer;
      function    Valid(Command: Word): Boolean; virtual;
      procedure   ChangeBounds(var Bounds: TRect); virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      procedure   Draw; virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      destructor  Done; virtual;
    private
      InDraw: boolean;
    end;

function  SearchFreeWindowNo: integer;

procedure InsertOK(ADialog: PDialog);
procedure InsertButtons(ADialog: PDialog);

procedure ErrorBox(S: string; Params: pointer);
procedure InformationBox(S: string; Params: pointer);

function IsThereAnyEditor: boolean;
function IsThereAnyWindow: boolean;

function  SearchMenuItem(Menu: PMenu; Cmd: word): PMenuItem;
procedure SetMenuItemParam(Menu: PMenuItem; Param: string);
function  IsSubMenu(P: PMenuItem): boolean;
function  IsSeparator(P: PMenuItem): boolean;
function  UpdateMenu(M: PMenu): boolean;
function  SearchSubMenu(M: PMenu; Index: integer): PMenuItem;
procedure AppendMenuItem(M: PMenu; I: PMenuItem);
procedure RemoveMenuItem(Menu: PMenu; I: PMenuItem);
function  GetMenuItemBefore(Menu:PMenu; BeforeOf: PMenuItem): PMenuItem;

function  NewTabItem(AView: PView; ANext: PTabItem): PTabItem;
procedure DisposeTabItem(P: PTabItem);
function  NewTabDef(AName: string; ADefItem: PView; AItems: PTabItem; ANext: PTabDef): PTabDef;
procedure DisposeTabDef(P: PTabDef);

function  GetEditorCurWord(Editor: PEditor): string;
procedure InitReservedWords;

const
      SourceCmds  : TCommandSet =
        ([cmSave,cmSaveAs,cmCompile]);
      EditorCmds  : TCommandSet =
        ([cmFind,cmReplace,cmSearchAgain,cmJumpLine,cmHelpTopicSearch]);
      CompileCmds : TCommandSet =
        ([cmMake,cmBuild,cmRun]);

      CalcClipboard  : extended = 0;

      OpenFileName   : string = '';


implementation

uses
  Memory,MsgBox,Validate,
  Tokens,
  FPUtils,FPHelp;

const
  NoNameCount    : integer = 0;
  ReservedWords  : PStringCollection = nil;

function IsThereAnyEditor: boolean;
begin
  IsThereAnyEditor:=Message(Desktop,evBroadcast,cmSearchWindow,nil)<>nil;
end;

function IsThereAnyHelpWindow: boolean;
begin
  IsThereAnyHelpWindow:=(HelpWindow<>nil) and (HelpWindow^.GetState(sfVisible));
end;

function IsThereAnyWindow: boolean;
begin
  IsThereAnyWindow:=IsThereAnyEditor or IsThereAnyHelpWindow;
end;

procedure InsertButtons(ADialog: PDialog);
var R   : TRect;
    W,H : integer;
    X   : integer;
    X1,X2: Sw_integer;
begin
  with ADialog^ do
  begin
    GetExtent(R);
    W:=R.B.X-R.A.X; H:=(R.B.Y-R.A.Y);
    R.Assign(0,0,W,H+3); ChangeBounds(R);
    X:=W div 2; X1:=X div 2+1; X2:=X+X1-1;
    R.Assign(X1-3,H,X1+7,H+2);
    Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
    R.Assign(X2-7,H,X2+3,H+2);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
    SelectNext(true);
  end;
end;

procedure InsertOK(ADialog: PDialog);
var BW: Sw_integer;
    R: TRect;
begin
  with ADialog^ do
  begin
    GetBounds(R); R.Grow(0,1); Inc(R.B.Y);
    ChangeBounds(R);
    BW:=10;
    R.A.Y:=R.B.Y-2; R.B.Y:=R.A.Y+2;
    R.A.X:=R.A.X+(R.B.X-R.A.X-BW) div 2; R.B.X:=R.A.X+BW;
    Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
    SelectNext(true);
  end;
end;


function GetEditorCurWord(Editor: PEditor): string;
var S: string;
    PS,PE: byte;
function Trim(S: string): string;
const TrimChars : set of char = [#0,#9,' ',#255];
begin
  while (length(S)>0) and (S[1] in TrimChars) do Delete(S,1,1);
  while (length(S)>0) and (S[length(S)] in TrimChars) do Delete(S,length(S),1);
  Trim:=S;
end;
const AlphaNum : set of char = ['A'..'Z','0'..'9','_'];
begin
  with Editor^ do
  begin
{$ifdef EDITORS}
    S:='';
{$else}
    S:=GetLineText(CurPos.Y);
    PS:=CurPos.X; while (PS>0) and (Upcase(S[PS]) in AlphaNum) do Dec(PS);
    PE:=CurPos.X; while (PE<length(S)) and (Upcase(S[PE+1]) in AlphaNum) do Inc(PE);
    S:=Trim(copy(S,PS+1,PE-PS));
{$endif}
  end;
  GetEditorCurWord:=S;
end;


{*****************************************************************************
                                   Tab
*****************************************************************************}

function NewTabItem(AView: PView; ANext: PTabItem): PTabItem;
var P: PTabItem;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  P^.Next:=ANext; P^.View:=AView;
  NewTabItem:=P;
end;

procedure DisposeTabItem(P: PTabItem);
begin
  if P<>nil then
  begin
    if P^.View<>nil then Dispose(P^.View, Done);
    Dispose(P);
  end;
end;

function NewTabDef(AName: string; ADefItem: PView; AItems: PTabItem; ANext: PTabDef): PTabDef;
var P: PTabDef;
    x: byte;
begin
  New(P);
  P^.Next:=ANext; P^.Name:=NewStr(AName); P^.Items:=AItems;
  x:=pos('~',AName);
  if (x<>0) and (x<length(AName)) then P^.ShortCut:=Upcase(AName[x+1])
                                  else P^.ShortCut:=#0;
  P^.DefItem:=ADefItem;
  NewTabDef:=P;
end;

procedure DisposeTabDef(P: PTabDef);
var PI,X: PTabItem;
begin
  DisposeStr(P^.Name);
  PI:=P^.Items;
  while PI<>nil do
    begin
      X:=PI^.Next;
      DisposeTabItem(PI);
      PI:=X;
    end;
  Dispose(P);
end;


{*****************************************************************************
                               Reserved Words
*****************************************************************************}

function GetReservedWordCount: integer;
var
  Count,I: integer;
begin
  Count:=0;
  for I:=ord(Low(TokenInfo)) to ord(High(TokenInfo)) do
   with TokenInfo[TToken(I)] do
     if (str<>'') and (str[1] in['A'..'Z']) then
       Inc(Count);
  GetReservedWordCount:=Count;
end;

function GetReservedWord(Index: integer): string;
var
  Count,Idx,I: integer;
  S: string;
begin
  Idx:=-1;
  Count:=-1;
  I:=ord(Low(TokenInfo));
  while (I<=ord(High(TokenInfo))) and (Idx=-1) do
   with TokenInfo[TToken(I)] do
    begin
      if (str<>'') and (str[1] in['A'..'Z']) then
        begin
          Inc(Count);
          if Count=Index then
           Idx:=I;
        end;
      Inc(I);
    end;
  if Idx=-1 then
    S:=''
  else
    S:=TokenInfo[TToken(Idx)].str;
  GetReservedWord:=S;
end;

procedure InitReservedWords;
var
  I,Count: integer;
begin
  Count:=GetReservedWordCount;
  New(ReservedWords, Init(Count,100));
  for I:=1 to Count do
    ReservedWords^.Insert(NewStr(UpcaseStr(GetReservedWord(I-1))));
end;


{*****************************************************************************
                               SearchWindow
*****************************************************************************}

function SearchWindowWithNo(No: integer): PSourceWindow;
var P: PSourceWindow;
begin
  P:=Message(Desktop,evBroadcast,cmSearchWindow+No,nil);
  if pointer(P)=pointer(Desktop) then P:=nil;
  SearchWindowWithNo:=P;
end;

function SearchFreeWindowNo: integer;
var No: integer;
begin
  No:=1;
  while (No<10) and (SearchWindowWithNo(No)<>nil) do
    Inc(No);
  if No=10 then No:=0;
  SearchFreeWindowNo:=No;
end;


{*****************************************************************************
                              TCenterDialog
*****************************************************************************}

constructor TCenterDialog.Init(var Bounds: TRect; ATitle: TTitleStr);
begin
  inherited Init(Bounds,ATitle);
  Options:=Options or ofCentered;
end;


{*****************************************************************************
                              TIntegerLine
*****************************************************************************}

constructor TIntegerLine.Init(var Bounds: TRect; AMin, AMax: longint);
begin
  inherited Init(Bounds, Bounds.B.X-Bounds.A.X-1);
  Validator:=New(PRangeValidator, Init(AMin, AMax));
end;


{*****************************************************************************
                               SourceEditor
*****************************************************************************}

{$ifndef EDITORS}
function TSourceEditor.GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer;
var Count: integer;
begin
  case SpecClass of
    ssCommentPrefix   : Count:=3;
    ssCommentSuffix   : Count:=2;
    ssStringPrefix    : Count:=1;
    ssStringSuffix    : Count:=1;
    ssAsmPrefix       : Count:=1;
    ssAsmSuffix       : Count:=1;
    ssDirectivePrefix : Count:=1;
    ssDirectiveSuffix : Count:=1;
  end;
  GetSpecSymbolCount:=Count;
end;

function TSourceEditor.GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string;
var S: string[20];
begin
  case SpecClass of
    ssCommentPrefix :
      case Index of
        0 : S:='{';
        1 : S:='(*';
        2 : S:='//';
      end;
    ssCommentSuffix :
      case Index of
        0 : S:='}';
        1 : S:='*)';
      end;
    ssStringPrefix :
      S:='''';
    ssStringSuffix :
      S:='''';
    ssAsmPrefix :
      S:='asm';
    ssAsmSuffix :
      S:='end';
    ssDirectivePrefix :
      S:='{$';
    ssDirectiveSuffix :
      S:='}';
  end;
  GetSpecSymbol:=S;
end;

function TSourceEditor.IsReservedWord(S: string): boolean;
var I: Sw_integer;
begin
  S:=UpcaseStr(S);
  IsReservedWord:=ReservedWords^.Search(@S,I);
end;
{$endif EDITORS}

procedure TSourceEditor.LocalMenu(P: TPoint);
var M: PMenu;
    MV: PAdvancedMenuPopUp;
    R: TRect;
    Re: word;
begin
  M:=GetLocalMenu;
  if M=nil then Exit;
  if LastLocalCmd<>0 then
     M^.Default:=SearchMenuItem(M,LastLocalCmd);
  Desktop^.GetExtent(R);
  MakeGlobal(P,R.A); {Desktop^.MakeLocal(R.A,R.A);}
  New(MV, Init(R, M));
  Re:=Application^.ExecView(MV);
  if M^.Default=nil then LastLocalCmd:=0
     else LastLocalCmd:=M^.Default^.Command;
  Dispose(MV, Done);
  if Re<>0 then
    Message(GetCommandTarget,evCommand,Re,@Self);
end;

function TSourceEditor.GetLocalMenu: PMenu;
var M: PMenu;
begin
  M:=NewMenu(
    NewItem('Cu~t~','Shift+Del',kbShiftDel,cmCut,hcCut,
    NewItem('~C~opy','Ctrl+Ins',kbCtrlIns,cmCopy,hcCopy,
    NewItem('~P~aste','Shift+Ins',kbShiftIns,cmPaste,hcPaste,
    NewItem('C~l~ear','Ctrl+Del',kbCtrlDel,cmClear,hcClear,
    NewLine(
    NewItem('Open ~f~ile at cursor','',kbNoKey,cmOpenAtCursor,hcOpenAtCursor,
    NewItem('~B~rowse symbol at cursor','',kbNoKey,cmBrowseAtCursor,hcBrowseAtCursor,
    NewItem('Topic ~s~earch','Ctrl+F1',kbCtrlF1,cmHelpTopicSearch,hcHelpTopicSearch,
    NewLine(
    NewItem('~O~ptions...','',kbNoKey,cmEditorOptions,hcEditorOptions,
    nil)))))))))));
  GetLocalMenu:=M;
end;

function TSourceEditor.GetCommandTarget: PView;
begin
  GetCommandTarget:=@Self;
end;

procedure TSourceEditor.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    P: TPoint;
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) and (Event.Buttons=mbRightButton) then
        begin
          MakeLocal(Event.Where,P); Inc(P.X); Inc(P.Y);
          LocalMenu(P);
          ClearEvent(Event);
        end;
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbAltF10 : Message(@Self,evCommand,cmLocalMenu,@Self);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmLocalMenu :
            begin
              P:=CurPos; Inc(P.X); Inc(P.Y);
              LocalMenu(P);
            end;
          cmOpenAtCursor :
            begin
              OpenFileName:=LowerCaseStr(GetEditorCurWord(@Self))+'.pas';
              Message(Application,evCommand,cmOpen,nil);
            end;
          cmHelp :
            Message(@Self,evCommand,cmHelpTopicSearch,@Self);
          cmHelpTopicSearch :
            HelpTopicSearch(@Self);
        else DontClear:=true;
        end;
        if not DontClear then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TIDEHelpWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate : ReDraw;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TIDEHelpWindow.GetPalette: PPalette;
const P: string[length(CIDEHelpDialog)] = CIDEHelpDialog;
begin
  GetPalette:=@P;
end;

constructor TSourceWindow.Init(var Bounds: TRect; AFileName: PathStr);
var HSB,VSB: PScrollBar;
    R: TRect;
    LoadFile: boolean;
begin
  inherited Init(Bounds,AFileName,SearchFreeWindowNo);
  GetExtent(R); R.A.Y:=R.B.Y-1; R.Grow(-1,0); R.A.X:=14;
  New(HSB, Init(R)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY; Insert(HSB);
  GetExtent(R); R.A.X:=R.B.X-1; R.Grow(0,-1);
  New(VSB, Init(R)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  GetExtent(R); R.A.X:=3; R.B.X:=14; R.A.Y:=R.B.Y-1;
  New(Indicator, Init(R));
  Indicator^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Indicator);
  GetExtent(R); R.Grow(-1,-1);
  LoadFile:=AFileName<>'';
  if not LoadFile then
     begin SetTitle('noname'+IntToStrZ(NonameCount,2)+'.pas'); Inc(NonameCount); end;
  New(Editor, Init(R, HSB, VSB, Indicator,AFileName));
  Editor^.GrowMode:=gfGrowHiX+gfGrowHiY;
  if LoadFile then
    if Editor^.LoadFile=false then
       ErrorBox('Error reading file.',nil);
  Insert(Editor);
  UpdateTitle;
end;

procedure TSourceWindow.UpdateTitle;
var Name: string;
begin
  if Editor^.FileName<>'' then
  begin Name:=SmartPath(Editor^.FileName); SetTitle(Name); end;
end;

procedure TSourceWindow.SetTitle(ATitle: string);
begin
  if Title<>nil then DisposeStr(Title);
  Title:=NewStr(ATitle);
  Frame^.DrawView;
end;

procedure TSourceWindow.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate :
          Update;
        cmUpdateTitle :
          UpdateTitle;
        cmSearchWindow :
          if Editor^.IsClipboard=false then
          ClearEvent(Event);
        else
          begin
            if (Event.Command>cmSearchWindow) and (Event.Command<=cmSearchWindow+100) and
               (Event.Command-cmSearchWindow=Number) then
            if Editor^.IsClipboard=false then
              ClearEvent(Event);
          end;
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmSave :
            if Editor^.IsClipboard=false then
            Editor^.Save;
          cmSaveAs :
            if Editor^.IsClipboard=false then
            Editor^.SaveAs;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TSourceWindow.SetState(AState: Word; Enable: Boolean);
var OldState: word;
begin
  OldState:=State;
  inherited SetState(AState,Enable);
  if ((AState xor State) and sfActive)<>0 then
  UpdateCommands;
end;

procedure TSourceWindow.UpdateCommands;
var Active: boolean;
begin
  Active:=GetState(sfActive);
  if Editor^.IsClipboard=false then
  begin
    SetCmdState(SourceCmds+CompileCmds,Active);
    SetCmdState(EditorCmds,Active);
  end;
  if Active=false then
     SetCmdState(ToClipCmds+FromClipCmds+UndoCmds,false);
end;

procedure TSourceWindow.Update;
begin
  ReDraw;
end;

function TSourceWindow.GetPalette: PPalette;
const P: string[length(CSourceWindow)] = CSourceWindow;
begin
  GetPalette:=@P;
end;

destructor TSourceWindow.Done;
begin
  Message(Application,evBroadcast,cmSourceWindowClosing,@Self);
  inherited Done;
  Message(Application,evBroadcast,cmUpdate,@Self);
end;

constructor TClipboardWindow.Init;
var R: TRect;
    HSB,VSB: PScrollBar;
begin
  Desktop^.GetExtent(R);
  inherited Init(R, '');
  SetTitle('Clipboard');
  HelpCtx:=hcClipboardWindow;
  Number:=wnNoNumber;

  GetExtent(R); R.A.Y:=R.B.Y-1; R.Grow(-1,0); R.A.X:=14;
  New(HSB, Init(R)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY; Insert(HSB);
  GetExtent(R); R.A.X:=R.B.X-1; R.Grow(0,-1);
  New(VSB, Init(R)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  GetExtent(R); R.A.X:=3; R.B.X:=14; R.A.Y:=R.B.Y-1;
  New(Indicator, Init(R));
  Indicator^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Indicator);
  GetExtent(R); R.Grow(-1,-1);
  New(Editor, Init(R, HSB, VSB, Indicator, ''));
  Editor^.GrowMode:=gfGrowHiX+gfGrowHiY;
  Insert(Editor);

  Hide;

  Clipboard:=Editor;
end;

procedure TClipboardWindow.Close;
begin
  Hide;
end;

destructor TClipboardWindow.Done;
begin
  inherited Done;
  Clipboard:=nil;
end;

function TAdvancedMenuBox.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PAdvancedMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

function TAdvancedMenuBox.Execute: word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
function IsDisabled(Item: PMenuItem): boolean;
var Found: boolean;
begin
  Found:=Item^.Disabled or IsSeparator(Item);
  if (Found=false) and (IsSubMenu(Item)=false) then
     Found:=CommandEnabled(Item^.Command)=false;
  IsDisabled:=Found;
end;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
  OldC: PMenuItem;
begin
  MakeLocal(E.Where, Mouse);
  OldC:=Current;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Break;
    end;
    Current := Current^.Next;
  end;
  if (Current<>nil) and IsDisabled(Current) then
  begin
     Current:={OldC}nil;
     MouseActive:=false;
  end;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until (Current^.Name <> nil) and (IsDisabled(Current)=false);
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and (P^.MouseInView(E.Where)=false) do
        P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False; E.What:=evNothing;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  if UpdateMenu(Menu) then
 begin
  if Current<>nil then
    if Current^.Disabled then
       TrackKey(true);
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
 end;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  Execute := Result;
end;

function TAdvancedMenuPopup.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PAdvancedMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

function TAdvancedMenuPopup.Execute: word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
function IsDisabled(Item: PMenuItem): boolean;
var Found: boolean;
begin
  Found:=Item^.Disabled or IsSeparator(Item);
  if (Found=false) and (IsSubMenu(Item)=false) then
     Found:=CommandEnabled(Item^.Command)=false;
  IsDisabled:=Found;
end;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
  OldC: PMenuItem;
begin
  MakeLocal(E.Where, Mouse);
  OldC:=Current;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Break;
    end;
    Current := Current^.Next;
  end;
  if (Current<>nil) and IsDisabled(Current) then
  begin
     Current:={OldC}nil;
     MouseActive:=false;
  end;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until (Current^.Name <> nil) and (IsDisabled(Current)=false);
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and (P^.MouseInView(E.Where)=false) do
        P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False; E.What:=evNothing;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  if UpdateMenu(Menu) then
 begin
  if Current<>nil then
    if Current^.Disabled then
       TrackKey(true);
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
 end;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  Execute := Result;
end;

constructor TAdvancedMenuBar.Init(var Bounds: TRect; AMenu: PMenu);
begin
  inherited Init(Bounds, AMenu);
  EventMask:=EventMask or evBroadcast;
end;

function TAdvancedMenuBar.NewSubView(var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
begin
  NewSubView := New(PAdvancedMenuBox, Init(Bounds, AMenu, AParentMenu));
end;

procedure TAdvancedMenuBar.Update;
begin
  UpdateMenu(Menu);
  DrawView;
end;

procedure TAdvancedMenuBar.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate   : Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TAdvancedMenuBar.Execute: word;
type
  MenuAction = (DoNothing, DoSelect, DoReturn);
var
  AutoSelect: Boolean;
  Action: MenuAction;
  Ch: Char;
  Result: Word;
  ItemShown, P: PMenuItem;
  Target: PMenuView;
  R: TRect;
  E: TEvent;
  MouseActive: Boolean;
function IsDisabled(Item: PMenuItem): boolean;
var Dis : boolean;
begin
  Dis:=Item^.Disabled or IsSeparator(Item);
  if (Dis=false) and (IsSubMenu(Item)=false) then
     Dis:=CommandEnabled(Item^.Command)=false;
  IsDisabled:=Dis;
end;

procedure TrackMouse;
var
  Mouse: TPoint;
  R: TRect;
  OldC: PMenuItem;
begin
  MakeLocal(E.Where, Mouse);
  OldC:=Current;
  Current := Menu^.Items;
  while Current <> nil do
  begin
    GetItemRect(Current, R);
    if R.Contains(Mouse) then
    begin
      MouseActive := True;
      Break;
    end;
    Current := Current^.Next;
  end;
  if (Current<>nil) and IsDisabled(Current) then
    Current:=nil;
end;

procedure TrackKey(FindNext: Boolean);

procedure NextItem;
begin
  Current := Current^.Next;
  if Current = nil then Current := Menu^.Items;
end;

procedure PrevItem;
var
  P: PMenuItem;
begin
  P := Current;
  if P = Menu^.Items then P := nil;
  repeat NextItem until Current^.Next = P;
end;

begin
  if Current <> nil then
    repeat
      if FindNext then NextItem else PrevItem;
    until (Current^.Name <> nil) and (IsDisabled(Current)=false);
end;

function MouseInOwner: Boolean;
var
  Mouse: TPoint;
  R: TRect;
begin
  MouseInOwner := False;
  if (ParentMenu <> nil) and (ParentMenu^.Size.Y = 1) then
  begin
    ParentMenu^.MakeLocal(E.Where, Mouse);
    ParentMenu^.GetItemRect(ParentMenu^.Current, R);
    MouseInOwner := R.Contains(Mouse);
  end;
end;

function MouseInMenus: Boolean;
var
  P: PMenuView;
begin
  P := ParentMenu;
  while (P <> nil) and not P^.MouseInView(E.Where) do P := P^.ParentMenu;
  MouseInMenus := P <> nil;
end;

function TopMenu: PMenuView;
var
  P: PMenuView;
begin
  P := @Self;
  while P^.ParentMenu <> nil do P := P^.ParentMenu;
  TopMenu := P;
end;

begin
  AutoSelect := False; E.What:=evNothing;
  Result := 0;
  ItemShown := nil;
  Current := Menu^.Default;
  MouseActive := False;
  if UpdateMenu(Menu) then
 begin
  if Current<>nil then
    if Current^.Disabled then
       TrackKey(true);
  repeat
    Action := DoNothing;
    GetEvent(E);
    case E.What of
      evMouseDown:
        if MouseInView(E.Where) or MouseInOwner then
        begin
          TrackMouse;
          if Size.Y = 1 then AutoSelect := True;
        end else Action := DoReturn;
      evMouseUp:
        begin
          TrackMouse;
          if MouseInOwner then
            Current := Menu^.Default
          else
            if (Current <> nil) and (Current^.Name <> nil) then
              Action := DoSelect
            else
              if MouseActive or MouseInView(E.Where) then Action := DoReturn
              else
              begin
                Current := Menu^.Default;
                if Current = nil then Current := Menu^.Items;
                Action := DoNothing;
              end;
        end;
      evMouseMove:
        if E.Buttons <> 0 then
        begin
          TrackMouse;
          if not (MouseInView(E.Where) or MouseInOwner) and
            MouseInMenus then Action := DoReturn;
        end;
      evKeyDown:
        case CtrlToArrow(E.KeyCode) of
          kbUp, kbDown:
            if Size.Y <> 1 then
              TrackKey(CtrlToArrow(E.KeyCode) = kbDown) else
              if E.KeyCode = kbDown then AutoSelect := True;
          kbLeft, kbRight:
            if ParentMenu = nil then
              TrackKey(CtrlToArrow(E.KeyCode) = kbRight) else
              Action := DoReturn;
          kbHome, kbEnd:
            if Size.Y <> 1 then
            begin
              Current := Menu^.Items;
              if E.KeyCode = kbEnd then TrackKey(False);
            end;
          kbEnter:
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
            end;
          kbEsc:
            begin
              Action := DoReturn;
              if (ParentMenu = nil) or (ParentMenu^.Size.Y <> 1) then
                ClearEvent(E);
            end;
        else
          Target := @Self;
          Ch := GetAltChar(E.KeyCode);
          if Ch = #0 then Ch := E.CharCode else Target := TopMenu;
          P := Target^.FindItem(Ch);
          if P = nil then
          begin
            P := TopMenu^.HotKey(E.KeyCode);
            if (P <> nil) and CommandEnabled(P^.Command) then
            begin
              Result := P^.Command;
              Action := DoReturn;
            end
          end else
            if Target = @Self then
            begin
              if Size.Y = 1 then AutoSelect := True;
              Action := DoSelect;
              Current := P;
            end else
              if (ParentMenu <> Target) or (ParentMenu^.Current <> P) then
                Action := DoReturn;
        end;
      evCommand:
        if E.Command = cmMenu then
        begin
          AutoSelect := False;
          if ParentMenu <> nil then Action := DoReturn;
        end else Action := DoReturn;
    end;
    if ItemShown <> Current then
    begin
      ItemShown := Current;
      DrawView;
    end;
    if (Action = DoSelect) or ((Action = DoNothing) and AutoSelect) then
      if Current <> nil then with Current^ do if Name <> nil then
        if Command = 0 then
        begin
          if E.What and (evMouseDown + evMouseMove) <> 0 then PutEvent(E);
          GetItemRect(Current, R);
          R.A.X := R.A.X + Origin.X;
          R.A.Y := R.B.Y + Origin.Y;
          R.B := Owner^.Size;
          if Size.Y = 1 then Dec(R.A.X);
          Target := TopMenu^.NewSubView(R, SubMenu, @Self);
          Result := Owner^.ExecView(Target);
          Dispose(Target, Done);
        end else if Action = DoSelect then Result := Command;
    if (Result <> 0) and CommandEnabled(Result) then
    begin
      Action := DoReturn;
      ClearEvent(E);
    end
    else
      Result := 0;
  until Action = DoReturn;
 end;
  if E.What <> evNothing then
    if (ParentMenu <> nil) or (E.What = evCommand) then PutEvent(E);
  if Current <> nil then
  begin
    Menu^.Default := Current;
    Current := nil;
    DrawView;
  end;
  Execute := Result;
end;

procedure ErrorBox(S: string; Params: pointer);
begin
  MessageBox(S,Params,mfError+mfInsertInApp+mfOKButton);
end;

procedure InformationBox(S: string; Params: pointer);
begin
  MessageBox(S,Params,mfInformation+mfInsertInApp+mfOKButton);
end;

function IsSeparator(P: PMenuItem): boolean;
begin
  IsSeparator:=(P<>nil) and (P^.Name=nil) and (P^.HelpCtx=hcNoContext);
end;

function IsSubMenu(P: PMenuItem): boolean;
begin
  IsSubMenu:=(P<>nil) and (P^.Name<>nil) and (P^.Command=0) and (P^.SubMenu<>nil);
end;

function SearchMenuItem(Menu: PMenu; Cmd: word): PMenuItem;
var P,I: PMenuItem;
begin
  I:=nil;
  if Menu=nil then P:=nil else P:=Menu^.Items;
  while (P<>nil) and (I=nil) do
  begin
    if IsSubMenu(P) then
       I:=SearchMenuItem(P^.SubMenu,Cmd);
    if I=nil then
    if P^.Command=Cmd then I:=P else
    P:=P^.Next;
  end;
  SearchMenuItem:=I;
end;

procedure SetMenuItemParam(Menu: PMenuItem; Param: string);
begin
  if Menu=nil then Exit;
  if Menu^.Param<>nil then DisposeStr(Menu^.Param);
  Menu^.Param:=NewStr(Param);
end;

function UpdateMenu(M: PMenu): boolean;
var P: PMenuItem;
    IsEnabled: boolean;
begin
  if M=nil then begin UpdateMenu:=false; Exit; end;
  P:=M^.Items; IsEnabled:=false;
  while (P<>nil) do
  begin
    if IsSubMenu(P) then
       P^.Disabled:=not UpdateMenu(P^.SubMenu);
    if (IsSeparator(P)=false) and (P^.Disabled=false) and (Application^.CommandEnabled(P^.Command)=true) then
       IsEnabled:=true;
    P:=P^.Next;
  end;
  UpdateMenu:=IsEnabled;
end;

function SearchSubMenu(M: PMenu; Index: integer): PMenuItem;
var P,C: PMenuItem;
    Count: integer;
begin
  P:=nil; Count:=-1;
  if M<>nil then C:=M^.Items else C:=nil;
  while (C<>nil) and (P=nil) do
  begin
    if IsSubMenu(C) then
     begin
       Inc(Count);
       if Count=Index then P:=C;
     end;
    C:=C^.Next;
  end;
  SearchSubMenu:=P;
end;

procedure AppendMenuItem(M: PMenu; I: PMenuItem);
var P: PMenuItem;
begin
  if (M=nil) or (I=nil) then Exit;
  I^.Next:=nil;
  if M^.Items=nil then M^.Items:=I else
  begin
    P:=M^.Items;
    while (P^.Next<>nil) do P:=P^.Next;
    P^.Next:=I;
  end;
end;

procedure DisposeMenuItem(P: PMenuItem);
begin
  if P<>nil then
  begin
    if IsSubMenu(P) then DisposeMenu(P^.SubMenu) else
      if IsSeparator(P)=false then
       if P^.Param<>nil then DisposeStr(P^.Param);
    if P^.Name<>nil then DisposeStr(P^.Name);
    Dispose(P);
  end;
end;

procedure RemoveMenuItem(Menu: PMenu; I: PMenuItem);
var P,PrevP: PMenuItem;
begin
  if (Menu=nil) or (I=nil) then Exit;
  P:=Menu^.Items; PrevP:=nil;
  while (P<>nil) do
  begin
    if P=I then
      begin
        if Menu^.Items<>I then PrevP^.Next:=P^.Next
                          else Menu^.Items:=P^.Next;
        DisposeMenuItem(P);
        Break;
      end;
    PrevP:=P; P:=P^.Next;
  end;
end;

function GetMenuItemBefore(Menu: PMenu; BeforeOf: PMenuItem): PMenuItem;
var P,C: PMenuItem;
begin
  P:=nil;
  if Menu<>nil then C:=Menu^.Items else C:=nil;
  while (C<>nil) do
    begin
      if C^.Next=BeforeOf then begin P:=C; Break; end;
      C:=C^.Next;
    end;
  GetMenuItemBefore:=P;
end;

procedure TAdvancedStaticText.SetText(S: string);
begin
  if Text<>nil then DisposeStr(Text);
  Text:=NewStr(S);
  DrawView;
end;

procedure TAdvancedListBox.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) and (Event.Double) then
      begin
        inherited HandleEvent(Event);
        if Range>Focused then SelectItem(Focused);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          Message(Owner,evBroadcast,cmDefault,nil);
      end;
  end;
  inherited HandleEvent(Event);
end;

constructor TColorStaticText.Init(var Bounds: TRect; AText: String; AColor: word);
begin
  inherited Init(Bounds,AText);
  Color:=AColor;
end;

procedure TColorStaticText.Draw;
var
  C: word;
  Center: Boolean;
  I, J, L, P, Y: Integer;
  B: TDrawBuffer;
  S: String;
  T: string;
  CurS: string;
  TildeCount,Po: integer;
  TempS: string;
begin
  if Size.X=0 then Exit;
  if DontWrap=false then
 begin
  C:=Color;
  GetText(S);
  L := Length(S);
  P := 1;
  Y := 0;
  Center := False;
  while Y < Size.Y do
  begin
    MoveChar(B, ' ', Lo(C), Size.X);
    if P <= L then
    begin
      if S[P] = #3 then
      begin
        Center := True;
        Inc(P);
      end;
      I := P;
      repeat
        J := P;
        while (P <= L) and (S[P] = ' ') do Inc(P);
        while (P <= L) and (S[P] <> ' ') and (S[P] <> #13) do Inc(P);
      until (P > L) or (P >= I + Size.X) or (S[P] = #13);
      TildeCount:=0; TempS:=copy(S,I,P-I);
      repeat
        Po:=Pos('~',TempS);
        if Po>0 then begin Inc(TildeCount); Delete(TempS,1,Po); end;
      until Po=0;
      if P > I + Size.X + TildeCount then
        if J > I then P := J else P := I + Size.X;
      T:=copy(S,I,P-I);
      if Center then J := (Size.X - {P + I}CStrLen(T)) div 2 else J := 0;
      MoveCStr(B[J],T,C);
      while (P <= L) and (S[P] = ' ') do Inc(P);
      if (P <= L) and (S[P] = #13) then
      begin
        Center := False;
        Inc(P);
        if (P <= L) and (S[P] = #10) then Inc(P);
      end;
    end;
    WriteLine(0, Y, Size.X, 1, B);
    Inc(Y);
  end;
 end { Wrap=false } else
 begin
  C := Color;
  GetText(S);
  I:=1;
  for Y:=0 to Size.Y-1 do
  begin
    MoveChar(B, ' ', C, Size.X);
    CurS:='';
    if S<>'' then
    begin
    P:=Pos(#13,S);
    if P=0 then P:=length(S)+1;
    CurS:=copy(S,1,P-1);
    CurS:=copy(CurS,Delta.X+1,255);
    CurS:=copy(CurS,1,MaxViewWidth);
    Delete(S,1,P);
    end;
    if CurS<>'' then MoveCStr(B,CurS,C);
    WriteLine(0,Y,Size.X,1,B);
  end;
 end;
end;

procedure TUnsortedStringCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeStr(Item);
end;

constructor THSListBox.Init(var Bounds: TRect; ANumCols: Word; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,ANumCols,AVScrollBar);
  HScrollBar:=AHScrollBar;
end;

constructor TDlgWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Integer);
begin
  inherited Init(Bounds,ATitle);
  Number:=ANumber;
  Flags:=Flags or (wfMove + wfGrow + wfClose + wfZoom);
end;

constructor TMessageListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AHScrollBar, AVScrollBar);
  NoSelection:=true;
end;

procedure TMessageListBox.AddItem(P: PMessageItem);
var W: integer;
begin
  if List=nil then New(List, Init(500,500));
  W:=length(P^.GetText(255));
  if W>MaxWidth then
  begin
    MaxWidth:=W;
    if HScrollBar<>nil then
       HScrollBar^.SetRange(0,MaxWidth);
  end;
  List^.Insert(P);
  SetRange(List^.Count);
  if Focused=List^.Count-1-1 then
     FocusItem(List^.Count-1);
  DrawView;
end;

function TMessageListBox.GetText(Item: Integer; MaxLen: Integer): String;
var P: PMessageItem;
    S: string;
begin
  P:=List^.At(Item);
  S:=P^.GetText(MaxLen);
  GetText:=copy(S,1,MaxLen);
end;

procedure TMessageListBox.Clear;
begin
  if List<>nil then Dispose(List, Done); List:=nil; MaxWidth:=0;
  SetRange(0); DrawView;
end;

procedure TMessageListBox.Draw;
var
  I, J, Item: Integer;
  NormalColor, SelectedColor, FocusedColor, Color: Word;
  ColWidth, CurCol, Indent: Integer;
  B: TDrawBuffer;
  Text: String;
  SCOff: Byte;
  TC: byte;
procedure MT(var C: word); begin if TC<>0 then C:=(C and $ff0f) or (TC and $f0); end;
begin
  if (Owner<>nil) then TC:=ord(Owner^.GetColor(6)) else TC:=0;
  if State and (sfSelected + sfActive) = (sfSelected + sfActive) then
  begin
    NormalColor := GetColor(1);
    FocusedColor := GetColor(3);
    SelectedColor := GetColor(4);
  end else
  begin
    NormalColor := GetColor(2);
    SelectedColor := GetColor(4);
  end;
  if Transparent then
    begin MT(NormalColor); MT(SelectedColor); end;
  if NoSelection then
     SelectedColor:=NormalColor;
  if HScrollBar <> nil then Indent := HScrollBar^.Value
  else Indent := 0;
  ColWidth := Size.X div NumCols + 1;
  for I := 0 to Size.Y - 1 do
  begin
    for J := 0 to NumCols-1 do
    begin
      Item := J*Size.Y + I + TopItem;
      CurCol := J*ColWidth;
      if (State and (sfSelected + sfActive) = (sfSelected + sfActive)) and
        (Focused = Item) and (Range > 0) then
      begin
        Color := FocusedColor;
        SetCursor(CurCol+1,I);
        SCOff := 0;
      end
      else if (Item < Range) and IsSelected(Item) then
      begin
        Color := SelectedColor;
        SCOff := 2;
      end
      else
      begin
        Color := NormalColor;
        SCOff := 4;
      end;
      MoveChar(B[CurCol], ' ', Color, ColWidth);
      if Item < Range then
      begin
        Text := GetText(Item, ColWidth + Indent);
        Text := Copy(Text,Indent,ColWidth);
        MoveStr(B[CurCol+1], Text, Color);
        if ShowMarkers then
        begin
          WordRec(B[CurCol]).Lo := Byte(SpecialChars[SCOff]);
          WordRec(B[CurCol+ColWidth-2]).Lo := Byte(SpecialChars[SCOff+1]);
        end;
      end;
      MoveChar(B[CurCol+ColWidth-1], #179, GetColor(5), 1);
    end;
    WriteLine(0, I, Size.X, 1, B);
  end;
end;

destructor TMessageListBox.Done;
begin
  inherited Done;
  if List<>nil then Dispose(List, Done);
end;

constructor TMessageItem.Init(AClass: longint; AText, AModule: string; AID: longint);
begin
  inherited Init;
  TClass:=AClass;
  Text:=NewStr(AText); Module:=NewStr(AModule); ID:=AID;
end;

function TMessageItem.GetText(MaxLen: integer): string;
var S: string;
begin
  if Text=nil then S:='' else S:=Text^;
  if length(S)>MaxLen then S:=copy(S,1,MaxLen-2)+'..';
  GetText:=S;
end;

procedure TMessageItem.Selected;
begin
end;

destructor TMessageItem.Done;
begin
  inherited Done;
  if Text<>nil then DisposeStr(Text);
end;

function TCompilerMessage.GetText(MaxLen: Integer): String;
var ClassS: string[20];
    S: string;
begin
  if TClass=
    V_Fatal       then ClassS:='Fatal'       else if TClass =
    V_Error       then ClassS:='Error'       else if TClass =
    V_Normal      then ClassS:=''            else if TClass =
    V_Warning     then ClassS:='Warning'     else if TClass =
    V_Note        then ClassS:='Note'        else if TClass =
    V_Hint        then ClassS:='Hint'        else if TClass =
    V_Macro       then ClassS:='Macro'       else if TClass =
    V_Procedure   then ClassS:='Procedure'   else if TClass =
    V_Conditional then ClassS:='Conditional' else if TClass =
    V_Info        then ClassS:='Info'        else if TClass =
    V_Status      then ClassS:='Status'      else if TClass =
    V_Used        then ClassS:='Used'        else if TClass =
    V_Tried       then ClassS:='Tried'       else if TClass =
    V_Debug       then ClassS:='Debug'
  else ClassS:='???';
  if ClassS<>'' then ClassS:=RExpand(ClassS,0)+': ';
  S:=ClassS;
  if (Module<>nil) and (ID<>0) then
     S:=S+Module^+' ('+IntToStr(ID)+'): ';
  if Text<>nil then S:=ClassS+Text^;
  if length(S)>MaxLen then S:=copy(S,1,MaxLen-2)+'..';
  GetText:=S;
end;

constructor TProgramInfoWindow.Init;
var R,R2: TRect;
    HSB,VSB: PScrollBar;
    ST: PStaticText;
begin
  Desktop^.GetExtent(R); R.A.Y:=R.B.Y-13;
  inherited Init(R, 'Program Information', wnNoNumber);

  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,4); R.B.Y:=R.A.Y+1;
  New(ST, Init(R, CharStr('Ä', MaxViewWidth))); ST^.GrowMode:=gfGrowHiX; Insert(ST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,5);
  R2.Copy(R); Inc(R2.B.Y); R2.A.Y:=R2.B.Y-1;
  New(HSB, Init(R2)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX; Insert(HSB);
  R2.Copy(R); Inc(R2.B.X); R2.A.X:=R2.B.X-1;
  New(VSB, Init(R2)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  New(LogLB, Init(R,HSB,VSB));
  LogLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
  LogLB^.Transparent:=true;
  Insert(LogLB);
end;

procedure TProgramInfoWindow.AddMessage(AClass: longint; Msg, Module: string; Line: longint);
begin
  if AClass>=V_Info then Line:=0;
  LogLB^.AddItem(New(PCompilerMessage, Init(AClass, Msg, Module, Line)));
end;

procedure TProgramInfoWindow.SizeLimits(var Min, Max: TPoint);
begin
  inherited SizeLimits(Min,Max);
  Min.Y:=9;
end;

procedure TProgramInfoWindow.Close;
begin
  Hide;
end;

function TAdvancedStatusLine.GetStatusText: string;
var S: string;
begin
  if StatusText=nil then S:='' else S:=StatusText^;
  GetStatusText:=S;
end;

procedure TAdvancedStatusLine.SetStatusText(S: string);
begin
  if StatusText<>nil then DisposeStr(StatusText);
  StatusText:=NewStr(S);
  DrawView;
end;

procedure TAdvancedStatusLine.ClearStatusText;
begin
  SetStatusText('');
end;

procedure TAdvancedStatusLine.Draw;
var B: TDrawBuffer;
    C: word;
    S: string;
begin
  S:=GetStatusText;
  if S='' then inherited Draw else
  begin
    C:=GetColor(1);
    MoveChar(B,' ',C,Size.X);
    MoveStr(B[1],S,C);
    WriteLine(0,0,Size.X,Size.Y,B);
  end;
end;

constructor TTab.Init(var Bounds: TRect; ATabDef: PTabDef);
begin
  inherited Init(Bounds);
  Options:=Options or ofSelectable or ofFirstClick or ofPreProcess or ofPostProcess;
  GrowMode:=gfGrowHiX+gfGrowHiY+gfGrowRel;
  TabDefs:=ATabDef;
  ActiveDef:=-1;
  SelectTab(0);
  ReDraw;
end;

function TTab.TabCount: integer;
var i: integer;
    P: PTabDef;
begin
  I:=0; P:=TabDefs;
  while (P<>nil) do
    begin
      Inc(I);
      P:=P^.Next;
    end;
  TabCount:=I;
end;

function TTab.AtTab(Index: integer): PTabDef;
var i: integer;
    P: PTabDef;
begin
  i:=0; P:=TabDefs;
  while (I<Index) do
    begin
      if P=nil then RunError($AA);
      P:=P^.Next;
      Inc(i);
    end;
  AtTab:=P;
end;

procedure TTab.SelectTab(Index: integer);
var P: PTabItem;
    V: PView;
begin
  if ActiveDef<>Index then
  begin
    if Owner<>nil then Owner^.Lock;
    Lock;
    { --- Update --- }
    if TabDefs<>nil then
       begin
         DefCount:=1;
         while AtTab(DefCount-1)^.Next<>nil do Inc(DefCount);
       end
       else DefCount:=0;
    if ActiveDef<>-1 then
    begin
      P:=AtTab(ActiveDef)^.Items;
      while P<>nil do
        begin
          if P^.View<>nil then Delete(P^.View);
          P:=P^.Next;
        end;
    end;
    ActiveDef:=Index;
    P:=AtTab(ActiveDef)^.Items;
    while P<>nil do
      begin
        if P^.View<>nil then Insert(P^.View);
        P:=P^.Next;
      end;
    V:=AtTab(ActiveDef)^.DefItem;
    if V<>nil then V^.Select;
    ReDraw;
    { --- Update --- }
    UnLock;
    if Owner<>nil then Owner^.UnLock;
    DrawView;
  end;
end;

procedure TTab.ChangeBounds(var Bounds: TRect);
var D: TPoint;
procedure DoCalcChange(P: PView); {$ifndef FPC}far;{$endif}
var
  R: TRect;
begin
  if P^.Owner=nil then Exit; { it think this is a bug in TV }
  P^.CalcBounds(R, D);
  P^.ChangeBounds(R);
end;
var
    P: PTabItem;
    I: integer;
begin
  D.X := Bounds.B.X - Bounds.A.X - Size.X;
  D.Y := Bounds.B.Y - Bounds.A.Y - Size.Y;
  inherited ChangeBounds(Bounds);
  for I:=0 to TabCount-1 do
  if I<>ActiveDef then
    begin
      P:=AtTab(I)^.Items;
      while P<>nil do
        begin
          if P^.View<>nil then DoCalcChange(P^.View);
          P:=P^.Next;
        end;
    end;
end;

procedure TTab.HandleEvent(var Event: TEvent);
var Index : integer;
    I     : integer;
    X     : integer;
    Len   : byte;
    P     : TPoint;
    V     : PView;
    CallOrig: boolean;
    LastV : PView;
    FirstV: PView;
function FirstSelectable: PView;
var
    FV : PView;
begin
  FV := First;
  while (FV<>nil) and ((FV^.Options and ofSelectable)=0) and (FV<>Last) do
        FV:=FV^.Next;
  if FV<>nil then
    if (FV^.Options and ofSelectable)=0 then FV:=nil;
  FirstSelectable:=FV;
end;
function LastSelectable: PView;
var
    LV : PView;
begin
  LV := Last;
  while (LV<>nil) and ((LV^.Options and ofSelectable)=0) and (LV<>First) do
        LV:=LV^.Prev;
  if LV<>nil then
    if (LV^.Options and ofSelectable)=0 then LV:=nil;
  LastSelectable:=LV;
end;
begin
  if (Event.What and evMouseDown)<>0 then
     begin
       MakeLocal(Event.Where,P);
       if P.Y<3 then
          begin
            Index:=-1; X:=1;
            for i:=0 to DefCount-1 do
                begin
                  Len:=CStrLen(AtTab(i)^.Name^);
                  if (P.X>=X) and (P.X<=X+Len+1) then Index:=i;
                  X:=X+Len+3;
                end;
            if Index<>-1 then
               SelectTab(Index);
          end;
     end;
  if Event.What=evKeyDown then
     begin
       Index:=-1;
       case Event.KeyCode of
            kbTab,kbShiftTab  :
              if GetState(sfSelected) then
                 begin
                   if Current<>nil then
                   begin
                   LastV:=LastSelectable; FirstV:=FirstSelectable;
                   if ((Current=LastV) or (Current=PLabel(LastV)^.Link)) and (Event.KeyCode=kbShiftTab) then
                      begin
                        if Owner<>nil then Owner^.SelectNext(true);
                      end else
                   if ((Current=FirstV) or (Current=PLabel(FirstV)^.Link)) and (Event.KeyCode=kbTab) then
                      begin
                        Lock;
                        if Owner<>nil then Owner^.SelectNext(false);
                        UnLock;
                      end else
                   SelectNext(Event.KeyCode=kbShiftTab);
                   ClearEvent(Event);
                   end;
                 end;
       else
       for I:=0 to DefCount-1 do
           begin
             if Upcase(GetAltChar(Event.KeyCode))=AtTab(I)^.ShortCut
                then begin
                       Index:=I;
                       ClearEvent(Event);
                       Break;
                     end;
           end;
       end;
       if Index<>-1 then
          begin
            Select;
            SelectTab(Index);
            V:=AtTab(ActiveDef)^.DefItem;
            if V<>nil then V^.Focus;
          end;
     end;
  CallOrig:=true;
  if Event.What=evKeyDown then
     begin
     if ((Owner<>nil) and (Owner^.Phase=phPostProcess) and (GetAltChar(Event.KeyCode)<>#0)) or GetState(sfFocused)
        then
        else CallOrig:=false;
     end;
  if CallOrig then inherited HandleEvent(Event);
end;

function TTab.GetPalette: PPalette;
begin
  GetPalette:=nil;
end;

procedure TTab.Draw;
var B          : TDrawBuffer;
    i          : integer;
    C1,C2,C3,C : word;
    HeaderLen  : integer;
    X,X2       : integer;
    Name       : PString;
    ActiveKPos : integer;
    ActiveVPos : integer;
    FC         : char;
    ClipR      : TRect;
procedure SWriteBuf(X,Y,W,H: integer; var Buf);
var i: integer;
begin
  if Y+H>Size.Y then H:=Size.Y-Y;
  if X+W>Size.X then W:=Size.X-X;
  if Buffer=nil then WriteBuf(X,Y,W,H,Buf)
                else for i:=1 to H do
                         Move(Buf,Buffer^[X+(Y+i-1)*Size.X],W*2);
end;
procedure ClearBuf;
begin
  MoveChar(B,' ',C1,Size.X);
end;
begin
  if InDraw then Exit;
  InDraw:=true;
  { - Start of TGroup.Draw - }
  if Buffer = nil then
  begin
    GetBuffer;
  end;
  { - Start of TGroup.Draw - }

  C1:=GetColor(1); C2:=(GetColor(7) and $f0 or $08)+GetColor(9)*256; C3:=GetColor(8)+GetColor({9}8)*256;
  HeaderLen:=0; for i:=0 to DefCount-1 do HeaderLen:=HeaderLen+CStrLen(AtTab(i)^.Name^)+3; Dec(HeaderLen);
  if HeaderLen>Size.X-2 then HeaderLen:=Size.X-2;

  { --- 1. sor --- }
  ClearBuf; MoveChar(B[0],'³',C1,1); MoveChar(B[HeaderLen+1],'³',C1,1);
  X:=1;
  for i:=0 to DefCount-1 do
      begin
        Name:=AtTab(i)^.Name; X2:=CStrLen(Name^);
        if i=ActiveDef
           then begin
                  ActiveKPos:=X-1;
                  ActiveVPos:=X+X2+2;
                  if GetState(sfFocused) then C:=C3 else C:=C2;
                end
           else C:=C2;
        MoveCStr(B[X],' '+Name^+' ',C); X:=X+X2+3;
        MoveChar(B[X-1],'³',C1,1);
      end;
  SWriteBuf(0,1,Size.X,1,B);

  { --- 0. sor --- }
  ClearBuf; MoveChar(B[0],'Ú',C1,1);
  X:=1;
  for i:=0 to DefCount-1 do
      begin
        if I<ActiveDef then FC:='Ú'
                       else FC:='¿';
        X2:=CStrLen(AtTab(i)^.Name^)+2;
        MoveChar(B[X+X2],{'Â'}FC,C1,1);
        if i=DefCount-1 then X2:=X2+1;
        if X2>0 then
        MoveChar(B[X],'Ä',C1,X2);
        X:=X+X2+1;
      end;
  MoveChar(B[HeaderLen+1],'¿',C1,1);
  MoveChar(B[ActiveKPos],'Ú',C1,1); MoveChar(B[ActiveVPos],'¿',C1,1);
  SWriteBuf(0,0,Size.X,1,B);

  { --- 2. sor --- }
  MoveChar(B[1],'Ä',C1,Max(HeaderLen,0)); MoveChar(B[HeaderLen+2],'Ä',C1,Max(Size.X-HeaderLen-3,0));
  MoveChar(B[Size.X-1],'¿',C1,1);
  MoveChar(B[ActiveKPos],'Ù',C1,1);
  if ActiveDef=0 then MoveChar(B[0],'³',C1,1)
                 else MoveChar(B[0],{'Ã'}'Ú',C1,1);
  MoveChar(B[HeaderLen+1],'Ä'{'Á'},C1,1); MoveChar(B[ActiveVPos],'À',C1,1);
  MoveChar(B[ActiveKPos+1],' ',C1,Max(ActiveVPos-ActiveKPos-1,0));
  SWriteBuf(0,2,Size.X,1,B);

  { --- marad‚k sor --- }
  ClearBuf; MoveChar(B[0],'³',C1,1); MoveChar(B[Size.X-1],'³',C1,1);
  SWriteBuf(0,3,Size.X,Size.Y-4,B);

  { --- Size.X . sor --- }
  MoveChar(B[0],'À',C1,1); MoveChar(B[1],'Ä',C1,Max(Size.X-2,0)); MoveChar(B[Size.X-1],'Ù',C1,1);
  SWriteBuf(0,Size.Y-1,Size.X,1,B);

  { - End of TGroup.Draw - }
  if Buffer <> nil then
  begin
    Lock;
    Redraw;
    UnLock;
  end;
  if Buffer <> nil then WriteBuf(0, 0, Size.X, Size.Y, Buffer^) else
  begin
    GetClipRect(ClipR);
    Redraw;
    GetExtent(ClipR);
  end;
  { - End of TGroup.Draw - }
  InDraw:=false;
end;

function TTab.Valid(Command: Word): Boolean;
var PT : PTabDef;
    PI : PTabItem;
    OK : boolean;
begin
  OK:=true;
  PT:=TabDefs;
  while (PT<>nil) and (OK=true) do
        begin
          PI:=PT^.Items;
          while (PI<>nil) and (OK=true) do
                begin
                  if PI^.View<>nil then OK:=OK and PI^.View^.Valid(Command);
                  PI:=PI^.Next;
                end;
          PT:=PT^.Next;
        end;
  Valid:=OK;
end;

procedure TTab.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState,Enable);
  if (AState and sfFocused)<>0 then DrawView;
end;

destructor TTab.Done;
var P,X: PTabDef;
procedure DeleteViews(P: PView); {$ifndef FPC}far;{$endif}
begin
  if P<>nil then Delete(P);
end;
begin
  ForEach(@DeleteViews);
  inherited Done;
  P:=TabDefs;
  while P<>nil do
        begin
          X:=P^.Next;
          DisposeTabDef(P);
          P:=X;
        end;
end;


END.
{
  $Log$
  Revision 1.1  1998-12-22 14:27:54  peter
    * moved

  Revision 1.4  1998/12/22 10:39:53  peter
    + options are now written/read
    + find and replace routines

}
