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

{$i globdir.inc}

interface

uses
  Dos,Objects,Drivers,
{$ifdef FVISION}
  FVConsts,
{$else FVISION}
  Commands,HelpCtx,
{$endif FVISION}
  Views,Menus,Dialogs,App,Gadgets,
  ASCIITAB,
  WEditor,WCEdit,
  WUtils,WHelp,WHlpView,WViews,WANSI,
  Comphook,
  FPConst,FPUsrScr;

type
    TEditor = TCodeEditor;
    PEditor = PCodeEditor;

    PStoreCollection = ^TStoreCollection;
    TStoreCollection = object(TStringCollection)
      function Add(const S: string): PString;
    end;

    PIntegerLine = ^TIntegerLine;
    TIntegerLine = object(TInputLine)
      constructor Init(var Bounds: TRect; AMin, AMax: longint);
    end;

    PFPHeapView = ^TFPHeapView;
    TFPHeapView = object(THeapView)
      constructor Init(var Bounds: TRect);
      constructor InitKb(var Bounds: TRect);
      procedure   HandleEvent(var Event: TEvent); virtual;
    end;

    PFPClockView = ^TFPClockView;
    TFPClockView = object(TClockView)
      constructor Init(var Bounds: TRect);
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
    end;

    PFPWindow = ^TFPWindow;
    TFPWindow = object(TWindow)
      AutoNumber: boolean;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   UpdateCommands; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Update; virtual;
      procedure   SelectInDebugSession;
    end;

    PFPHelpViewer = ^TFPHelpViewer;
    TFPHelpViewer = object(THelpViewer)
      function    GetLocalMenu: PMenu; virtual;
      function    GetCommandTarget: PView; virtual;
    end;

    PFPHelpWindow = ^TFPHelpWindow;
    TFPHelpWindow = object(THelpWindow)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ASourceFileID: word; AContext: THelpCtx; ANumber: Integer);
      destructor  Done;virtual;
      procedure   InitHelpView; virtual;
      procedure   Show; {virtual;}
      procedure   Hide; {virtual;}
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetPalette: PPalette; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
    end;

    PTextScroller = ^TTextScroller;
    TTextScroller = object(TStaticText)
      TopLine: integer;
      Speed  : integer;
      Lines  : PUnsortedStringCollection;
      constructor Init(var Bounds: TRect; ASpeed: integer; AText: PUnsortedStringCollection);
      function    GetLineCount: integer; virtual;
      function    GetLine(I: integer): string; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Update; virtual;
      procedure   Reset; virtual;
      procedure   Scroll; virtual;
      procedure   Draw; virtual;
      destructor  Done; virtual;
    private
      LastTT: longint;
    end;

    TAlign = (alLeft,alCenter,alRight);

    PFPToolTip = ^TFPToolTip;
    TFPToolTip = object(TView)
      constructor Init(var Bounds: TRect; const AText: string; AAlign: TAlign);
      procedure   Draw; virtual;
      function    GetText: string;
      procedure   SetText(const AText: string);
      function    GetAlign: TAlign;
      procedure   SetAlign(AAlign: TAlign);
      function    GetPalette: PPalette; virtual;
      destructor  Done; virtual;
    private
      Text: PString;
      Align: TAlign;
    end;

    PSourceEditor = ^TSourceEditor;
    TSourceEditor = object(TFileEditor)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator;const AFileName: string);
    public
      CompileStamp : longint;
    public
      CodeCompleteTip: PFPToolTip;
      { Syntax highlight }
      function  IsReservedWord(const S: string): boolean; virtual;
      function  IsAsmReservedWord(const S: string): boolean; virtual;
      function  GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer; virtual;
      function  GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string; virtual;
      { CodeTemplates }
      function    TranslateCodeTemplate(const Shortcut: string; ALines: PUnsortedStringCollection): boolean; virtual;
      function    SelectCodeTemplate(var ShortCut: string): boolean; virtual;
      { CodeComplete }
      function    CompleteCodeWord(const WordS: string; var Text: string): boolean; virtual;
      procedure   SetCodeCompleteWord(const S: string); virtual;
      procedure   AlignCodeCompleteTip;
      procedure   HandleEvent(var Event: TEvent); virtual;
{$ifdef DebugUndo}
      procedure   DumpUndo;
      procedure   UndoAll;
      procedure   RedoAll;
{$endif DebugUndo}
      function    Valid(Command: Word): Boolean;virtual;
      function    GetLocalMenu: PMenu; virtual;
      function    GetCommandTarget: PView; virtual;
      function    CreateLocalMenuView(var Bounds: TRect; M: PMenu): PMenuPopup; virtual;
      procedure   ModifiedChanged; virtual;
      procedure   InsertOptions; virtual;
      procedure   PushInfo(Const st : string);virtual;
      procedure   PopInfo;virtual;
    end;

    PSourceWindow = ^TSourceWindow;
    TSourceWindow = object(TFPWindow)
      Editor    : PSourceEditor;
      Indicator : PIndicator;
      constructor Init(var Bounds: TRect; AFileName: string);
      function    GetTitle(MaxSize: sw_Integer): TTitleStr; virtual;
      procedure   SetTitle(ATitle: string); virtual;
      procedure   UpdateTitle; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Update; virtual;
      procedure   UpdateCommands; virtual;
      function    GetPalette: PPalette; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   Close; virtual;
      destructor  Done; virtual;
    end;

    PGDBSourceEditor = ^TGDBSourceEditor;
    TGDBSourceEditor = object(TSourceEditor)
      function   InsertNewLine : Sw_integer;virtual;
      function   Valid(Command: Word): Boolean; virtual;
      procedure  AddLine(const S: string); virtual;
      procedure  AddErrorLine(const S: string); virtual;
      { Syntax highlight }
      function  IsReservedWord(const S: string): boolean; virtual;
    private
      Silent,
      AutoRepeat,
      IgnoreStringAtEnd : boolean;
      LastCommand : String;
      end;

    PGDBWindow = ^TGDBWindow;
    TGDBWindow = object(TFPWindow)
      Editor    : PGDBSourceEditor;
      Indicator : PIndicator;
      constructor Init(var Bounds: TRect);
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   WriteText(Buf : pchar;IsError : boolean);
      procedure   WriteString(Const S : string);
      procedure   WriteErrorString(Const S : string);
      procedure   WriteOutputText(Buf : pchar);
      procedure   WriteErrorText(Buf : pchar);
      function    GetPalette: PPalette;virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   UpdateCommands; virtual;
      destructor  Done; virtual;
    end;

    PDisasLine = ^TDisasLine;
    TDisasLine = object(TLine)
      address : cardinal;{ should be target size of address for cross debuggers }
    end;

    PDisasLineCollection = ^TDisasLineCollection;
    TDisasLineCollection = object(TLineCollection)
      function  At(Index: sw_Integer): PDisasLine;
    end;

    PDisassemblyEditor = ^TDisassemblyEditor;
    TDisassemblyEditor = object(TSourceEditor)
      CurrentSource : String;
      CurrentLine : longint;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator;const AFileName: string);
      procedure  ReleaseSource;
      destructor Done;virtual;
      procedure  AddSourceLine(const AFileName: string;line : longint); virtual;
      procedure  AddAssemblyLine(const S: string;AAddress : cardinal); virtual;
      function   GetCurrentLine(address : cardinal) : PDisasLine;
      private
        Source : PSourceWindow;
        OwnsSource : Boolean;
        DisasLines : PDisasLineCollection;
        MinAddress,MaxAddress : cardinal;
        CurL : PDisasLine;
      end;

    PDisassemblyWindow = ^TDisassemblyWindow;
    TDisassemblyWindow = object(TFPWindow)
      Editor    : PDisassemblyEditor;
      Indicator : PIndicator;
      constructor Init(var Bounds: TRect);
      procedure   LoadFunction(Const FuncName : string);
      procedure   LoadAddress(Addr : cardinal);
      function    ProcessPChar(p : pchar) : boolean;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   WriteSourceString(Const S : string;line : longint);
      procedure   WriteDisassemblyString(Const S : string;address : cardinal);
      procedure   SetCurAddress(address : cardinal);
      procedure   UpdateCommands; virtual;
      function    GetPalette: PPalette;virtual;
      destructor  Done; virtual;
    end;

    PClipboardWindow = ^TClipboardWindow;
    TClipboardWindow = object(TSourceWindow)
      constructor Init;
      procedure   Close; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;

    PMessageItem = ^TMessageItem;
    TMessageItem = object(TObject)
      TClass    : longint;
      Text      : PString;
      Module    : PString;
      Row,Col   : sw_integer;
      constructor Init(AClass: longint; const AText: string; AModule: PString; ARow, ACol: sw_integer);
      function    GetText(MaxLen: Sw_integer): string; virtual;
      procedure   Selected; virtual;
      function    GetModuleName: string; virtual;
      destructor  Done; virtual;
    end;

    PMessageListBox = ^TMessageListBox;
    TMessageListBox = object(THSListBox)
      Transparent : boolean;
      NoSelection : boolean;
      MaxWidth    : Sw_integer;
      ModuleNames : PStoreCollection;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   AddItem(P: PMessageItem); virtual;
      function    AddModuleName(const Name: string): PString; virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   Clear; virtual;
      procedure   TrackSource; virtual;
      procedure   GotoSource; virtual;
      procedure   Draw; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetLocalMenu: PMenu; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    end;


    PFPDlgWindow = ^TFPDlgWindow;
    TFPDlgWindow = object(TDlgWindow)
      procedure   HandleEvent(var Event: TEvent); virtual;
    end;

    PProgramInfoWindow = ^TProgramInfoWindow;
    TProgramInfoWindow = object(TFPDlgWindow)
      InfoST: PColorStaticText;
      LogLB : PMessageListBox;
      constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   AddMessage(AClass: longint; Msg, Module: string; Line, Column: longint);
      procedure   ClearMessages;
      procedure   SizeLimits(var Min, Max: TPoint); virtual;
      procedure   Close; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Update; virtual;
      destructor  Done; virtual;
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
      procedure   SelectNextTab(Forwards: boolean);
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

    PScreenView = ^TScreenView;
    TScreenView = object(TScroller)
      Screen: PScreen;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
                    AScreen: PScreen);
      procedure   Draw; virtual;
      procedure   Update; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
    end;

    PScreenWindow = ^TScreenWindow;
    TScreenWindow = object(TFPWindow)
      ScreenView : PScreenView;
      constructor Init(AScreen: PScreen; ANumber: integer);
      destructor  Done; virtual;
    end;

    PFPAboutDialog = ^TFPAboutDialog;
    TFPAboutDialog = object(TCenterDialog)
      constructor Init;
      procedure   ToggleInfo;
      procedure   HandleEvent(var Event: TEvent); virtual;
    private
      Scroller: PTextScroller;
      TitleST : PStaticText;
    end;

    PFPASCIIChart = ^TFPASCIIChart;
    TFPASCIIChart = object(TASCIIChart)
      constructor Init;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    end;

    PVideoModeListBox = ^TVideoModeListBox;
    TVideoModeListBox = object(TDropDownListBox)
      function    GetText(Item: pointer; MaxLen: sw_integer): string; virtual;
    end;

    PFPDesktop = ^TFPDesktop;
    TFPDesktop = object(TDesktop)
      constructor Init(var Bounds: TRect);
      procedure   InitBackground; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
    end;

    PFPMemo = ^TFPMemo;
    TFPMemo = object(TCodeEditor)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
                    PScrollBar; AIndicator: PIndicator);
      function    IsReservedWord(const S: string): boolean; virtual;
      function    GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer; virtual;
      function    GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string; virtual;
      function    GetPalette: PPalette; virtual;
    end;

    PFPCodeMemo = ^TFPCodeMemo;
    TFPCodeMemo = object(TFPMemo)
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
                    PScrollBar; AIndicator: PIndicator);
      function    IsReservedWord(const S: string): boolean; virtual;
      function    GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer; virtual;
      function    GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string; virtual;
    end;

function  SearchFreeWindowNo: integer;

function IsWindow(P: PView): boolean;
function IsThereAnyEditor: boolean;
function IsThereAnyWindow: boolean;
function IsThereAnyVisibleWindow: boolean;
function IsThereAnyNumberedWindow: boolean;
function FirstEditorWindow: PSourceWindow;
function EditorWindowFile(const Name : String): PSourceWindow;
procedure AskToReloadAllModifiedFiles;

function InDisassemblyWindow :boolean;

function  NewTabItem(AView: PView; ANext: PTabItem): PTabItem;
procedure DisposeTabItem(P: PTabItem);
function  NewTabDef(AName: string; ADefItem: PView; AItems: PTabItem; ANext: PTabDef): PTabDef;
procedure DisposeTabDef(P: PTabDef);

function GetEditorCurWord(Editor: PEditor; ValidSpecChars: TCharSet): string;
procedure InitReservedWords;
procedure DoneReservedWords;
function GetReservedWordCount: integer;
function GetReservedWord(Index: integer): string;
function GetAsmReservedWordCount: integer;
function GetAsmReservedWord(Index: integer): string;

procedure TranslateMouseClick(View: PView; var Event: TEvent);

function GetNextEditorBounds(var Bounds: TRect): boolean;
function OpenEditorWindow(Bounds: PRect; FileName: string; CurX,CurY: sw_integer): PSourceWindow;
function IOpenEditorWindow(Bounds: PRect; FileName: string; CurX,CurY: sw_integer; ShowIt: boolean): PSourceWindow;
function SearchOnDesktop(FileName : string;tryexts:boolean) : PSourceWindow;
function TryToOpenFile(Bounds: PRect; FileName: string; CurX,CurY: sw_integer;tryexts: boolean): PSourceWindow;
function ITryToOpenFile(Bounds: PRect; FileName: string; CurX,CurY: sw_integer;tryexts, ShowIt,
         ForceNewWindow:boolean): PSourceWindow;
function LocateSourceFile(const FileName: string; tryexts: boolean): string;

function SearchWindow(const Title: string): PWindow;

function StartEditor(Editor: PCodeEditor; FileName: string): boolean;

{$ifdef VESA}
procedure InitVESAScreenModes;
procedure DoneVESAScreenModes;
{$endif}

procedure NoDebugger;

const
      SourceCmds  : TCommandSet =
        ([cmSave,cmSaveAs,cmCompile,cmHide]);
      EditorCmds  : TCommandSet =
        ([cmFind,cmReplace,cmSearchAgain,cmJumpLine,cmHelpTopicSearch]);
      CompileCmds : TCommandSet =
        ([cmMake,cmBuild,cmRun]);

      CalcClipboard   : extended = 0;

      OpenFileName    : string{$ifdef GABOR}[50]{$endif} = '';
      OpenFileLastExt : string[12] = '*.pas';
      NewEditorOpened : boolean = false;

var  MsgParms : array[1..10] of
         record
           case byte of
             0 : (Ptr : pointer);
             1 : (Long: longint);
         end;

procedure RegisterFPViews;

implementation

uses
  Video,Strings,Keyboard,Validate,
  globtype,Tokens,Version,
  cpubase,ra386,
{$ifdef USE_EXTERNAL_COMPILER}
   fpintf, { superseeds version_string of version unit }
{$endif USE_EXTERNAL_COMPILER}
{$ifndef NODEBUG}
  gdbint,
{$endif NODEBUG}
  {$ifdef VESA}Vesa,{$endif}
  FPString,FPSwitch,FPSymbol,FPDebug,FPVars,FPUtils,FPCompil,FPHelp,
  FPTools,FPIDE,FPCodTmp,FPCodCmp;

const
  RSourceEditor: TStreamRec = (
     ObjType: 1500;
     VmtLink: Ofs(TypeOf(TSourceEditor)^);
     Load:    @TSourceEditor.Load;
     Store:   @TSourceEditor.Store
  );
  RSourceWindow: TStreamRec = (
     ObjType: 1501;
     VmtLink: Ofs(TypeOf(TSourceWindow)^);
     Load:    @TSourceWindow.Load;
     Store:   @TSourceWindow.Store
  );
  RFPHelpViewer: TStreamRec = (
     ObjType: 1502;
     VmtLink: Ofs(TypeOf(TFPHelpViewer)^);
     Load:    @TFPHelpViewer.Load;
     Store:   @TFPHelpViewer.Store
  );
  RFPHelpWindow: TStreamRec = (
     ObjType: 1503;
     VmtLink: Ofs(TypeOf(TFPHelpWindow)^);
     Load:    @TFPHelpWindow.Load;
     Store:   @TFPHelpWindow.Store
  );
  RClipboardWindow: TStreamRec = (
     ObjType: 1504;
     VmtLink: Ofs(TypeOf(TClipboardWindow)^);
     Load:    @TClipboardWindow.Load;
     Store:   @TClipboardWindow.Store
  );
  RMessageListBox: TStreamRec = (
     ObjType: 1505;
     VmtLink: Ofs(TypeOf(TMessageListBox)^);
     Load:    @TMessageListBox.Load;
     Store:   @TMessageListBox.Store
  );
  RFPDesktop: TStreamRec = (
     ObjType: 1506;
     VmtLink: Ofs(TypeOf(TFPDesktop)^);
     Load:    @TFPDesktop.Load;
     Store:   @TFPDesktop.Store
  );

  RGDBSourceEditor: TStreamRec = (
     ObjType: 1507;
     VmtLink: Ofs(TypeOf(TGDBSourceEditor)^);
     Load:    @TGDBSourceEditor.Load;
     Store:   @TGDBSourceEditor.Store
  );
  RGDBWindow: TStreamRec = (
     ObjType: 1508;
     VmtLink: Ofs(TypeOf(TGDBWindow)^);
     Load:    @TGDBWindow.Load;
     Store:   @TGDBWindow.Store
  );
  RFPASCIIChart: TStreamRec = (
     ObjType: 1509;
     VmtLink: Ofs(TypeOf(TFPASCIIChart)^);
     Load:    @TFPASCIIChart.Load;
     Store:   @TFPASCIIChart.Store
  );
  RProgramInfoWindow: TStreamRec = (
     ObjType: 1510;
     VmtLink: Ofs(TypeOf(TProgramInfoWindow)^);
     Load:    @TProgramInfoWindow.Load;
     Store:   @TProgramInfoWindow.Store
  );
  RFPDlgWindow: TStreamRec = (
     ObjType: 1511;
     VmtLink: Ofs(TypeOf(TFPDlgWindow)^);
     Load:    @TFPDlgWindow.Load;
     Store:   @TFPDlgWindow.Store
  );
  RDisassemblyEditor: TStreamRec = (
     ObjType: 1512;
     VmtLink: Ofs(TypeOf(TDisassemblyEditor)^);
     Load:    @TDisassemblyEditor.Load;
     Store:   @TDisassemblyEditor.Store
  );
  RDisassemblyWindow: TStreamRec = (
     ObjType: 1513;
     VmtLink: Ofs(TypeOf(TDisassemblyWindow)^);
     Load:    @TDisassemblyWindow.Load;
     Store:   @TDisassemblyWindow.Store
  );
const
  NoNameCount    : integer = 0;
var
  ReservedWords  : array[1..ReservedWordMaxLen] of PStringCollection;
  AsmReservedWords  : array[1..ReservedWordMaxLen] of PStringCollection;

{****************************************************************************
                                TStoreCollection
****************************************************************************}

function TStoreCollection.Add(const S: string): PString;
var P: PString;
    Index: Sw_integer;
begin
  if S='' then P:=nil else
  if Search(@S,Index) then P:=At(Index) else
    begin
      P:=NewStr(S);
      Insert(P);
    end;
  Add:=P;
end;


function IsThereAnyEditor: boolean;
function EditorWindow(P: PView): boolean; {$ifndef FPC}far;{$endif}
begin
  EditorWindow:=(P^.HelpCtx=hcSourceWindow);
end;
begin
  IsThereAnyEditor:=Desktop^.FirstThat(@EditorWindow)<>nil;
end;

procedure AskToReloadAllModifiedFiles;
  procedure EditorWindowModifiedOnDisk(P: PView); {$ifndef FPC}far;{$endif}
begin
  if (P^.HelpCtx=hcSourceWindow) then
    PSourceWindow(P)^.Editor^.ReloadFile;
end;
begin
  Desktop^.ForEach(@EditorWindowModifiedOnDisk);
end;

function IsThereAnyHelpWindow: boolean;
begin
  IsThereAnyHelpWindow:=(HelpWindow<>nil) and (HelpWindow^.GetState(sfVisible));
end;

function IsThereAnyNumberedWindow: boolean;
var _Is: boolean;
begin
  _Is:=Message(Desktop,evBroadcast,cmSearchWindow,nil)<>nil;
  _Is:=_Is or ( (ClipboardWindow<>nil) and ClipboardWindow^.GetState(sfVisible));
  IsThereAnyNumberedWindow:=_Is;
end;

function IsWindow(P: PView): boolean;
var OK: boolean;
begin
  OK:=false;
  if (P^.HelpCtx=hcSourceWindow) or
     (P^.HelpCtx=hcHelpWindow) or
     (P^.HelpCtx=hcClipboardWindow) or
     (P^.HelpCtx=hcCalcWindow) or
     (P^.HelpCtx=hcInfoWindow) or
     (P^.HelpCtx=hcBrowserWindow) or
     (P^.HelpCtx=hcMessagesWindow) or
     (P^.HelpCtx=hcCompilerMessagesWindow) or
     (P^.HelpCtx=hcGDBWindow) or
     (P^.HelpCtx=hcdisassemblyWindow) or
     (P^.HelpCtx=hcWatchesWindow) or
     (P^.HelpCtx=hcRegistersWindow) or
     (P^.HelpCtx=hcFPURegisters) or
     (P^.HelpCtx=hcStackWindow) or
     (P^.HelpCtx=hcBreakpointListWindow) or
     (P^.HelpCtx=hcASCIITableWindow)
   then
     OK:=true;
   IsWindow:=OK;
end;

function IsThereAnyWindow: boolean;
function CheckIt(P: PView): boolean; {$ifndef FPC}far;{$endif}
begin
  CheckIt:=IsWindow(P);
end;
begin
  IsThereAnyWindow:=Desktop^.FirstThat(@CheckIt)<>nil;
end;

function IsThereAnyVisibleWindow: boolean;
function CheckIt(P: PView): boolean; {$ifndef FPC}far;{$endif}
begin
  CheckIt:=IsWindow(P) and P^.GetState(sfVisible);
end;
begin
  IsThereAnyVisibleWindow:=Desktop^.FirstThat(@CheckIt)<>nil;
end;

function FirstEditorWindow: PSourceWindow;
function EditorWindow(P: PView): boolean; {$ifndef FPC}far;{$endif}
begin
  EditorWindow:=(P^.HelpCtx=hcSourceWindow);
end;
begin
  FirstEditorWindow:=pointer(Desktop^.FirstThat(@EditorWindow));
end;

function EditorWindowFile(const Name : String): PSourceWindow;
var
  SName : string;

  function EditorWindow(P: PView): boolean; {$ifndef FPC}far;{$endif}
  begin
    EditorWindow:=(TypeOf(P^)=TypeOf(TSourceWindow)) and
                  (FixFileName(PSourceWindow(P)^.Editor^.FileName)=SName);
  end;

begin
  SName:=FixFileName(FExpand(Name));
  EditorWindowFile:=pointer(Desktop^.FirstThat(@EditorWindow));
end;

function InDisassemblyWindow :boolean;
var
  PW : PWindow;

function CheckIt(P: PView): boolean; {$ifndef FPC}far;{$endif}
begin
  CheckIt:=IsWindow(P) and P^.GetState(sfVisible) and
     (P^.HelpCtx <> hcWatchesWindow) and
     (P^.HelpCtx <> hcStackWindow) and
     (P^.HelpCtx <> hcRegistersWindow) and
     (P^.HelpCtx <> hcFPURegisters);
end;
begin
  PW:=PWindow(Desktop^.FirstThat(@CheckIt));
  InDisassemblyWindow:=Assigned(PW) and
    (TypeOf(PW^)=TypeOf(TDisassemblyWindow));
end;

function GetEditorCurWord(Editor: PEditor; ValidSpecChars: TCharSet): string;
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
    S:=GetDisplayText(CurPos.Y);
    PS:=CurPos.X; while (PS>0) and (Upcase(S[PS]) in AlphaNum) do Dec(PS);
    PE:=CurPos.X; while (PE<length(S)) and (Upcase(S[PE+1]) in (AlphaNum+ValidSpecChars)) do Inc(PE);
    S:=Trim(copy(S,PS+1,PE-PS));
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
  for I:=ord(Low(tToken)) to ord(High(tToken)) do
  with TokenInfo^[TToken(I)] do
     if (str<>'') and (str[1] in['A'..'Z']) and (keyword=m_all) then
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
  I:=ord(Low(tToken));
  while (I<=ord(High(tToken))) and (Idx=-1) do
   with TokenInfo^[TToken(I)] do
    begin
      if (str<>'') and (str[1] in['A'..'Z']) and (keyword=m_all) then
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
    S:=TokenInfo^[TToken(Idx)].str;
  GetReservedWord:=S;
end;

function GetAsmReservedWordCount: integer;
begin
  GetAsmReservedWordCount:=ord(lastop) - ord(firstop)
   + CondAsmOps*(ord(high(TasmCond))-ord(low(TasmCond)));
end;


function GetAsmReservedWord(Index: integer): string;
var
  CondNum,CondOpNum : integer;
begin
{$ifdef I386}
  if index <= ord(lastop) - ord(firstop) then
{$ifdef COMPILER_1_0}
    GetAsmReservedWord:=att_op2str[tasmop(Index+ord(firstop))]
{$else}
    GetAsmReservedWord:=std_op2str[tasmop(Index+ord(firstop))]
{$endif}
  else
    begin
      index:=index - (ord(lastop) - ord(firstop) );
      CondOpNum:= index div (ord(high(TasmCond))-ord(low(TasmCond)));
      CondNum:=index - (CondOpNum * (ord(high(TasmCond))-ord(low(TasmCond))));
      GetAsmReservedWord:=CondAsmOpStr[CondOpNum]+cond2str[TasmCond(CondNum+ord(low(TAsmCond))+1)];
    end;
{$else not I386}
{$ifdef m68k}
  if index <= ord(lastop) - ord(firstop) then
    GetAsmReservedWord:=mot_op2str[tasmop(Index+ord(firstop))]
  else
    begin
      index:=index - (ord(lastop) - ord(firstop) );
      CondOpNum:= index div (ord(high(TasmCond))-ord(low(TasmCond)));
      CondNum:=index - (CondOpNum * (ord(high(TasmCond))-ord(low(TasmCond))));
      GetAsmReservedWord:=CondAsmOpStr[CondOpNum]+cond2str[TasmCond(CondNum+ord(low(TAsmCond))+1)];
    end;
{$else not m68k}
  GetAsmReservedWord:='';
{$endif m68k}
{$endif I386}
end;

procedure InitReservedWords;
var WordS: string;
    Idx,I,J : sw_integer;
begin
  InitTokens;
  for I:=Low(ReservedWords) to High(ReservedWords) do
    New(ReservedWords[I], Init(50,10));
  for I:=1 to GetReservedWordCount do
    begin
      WordS:=GetReservedWord(I-1); Idx:=length(WordS);
      if (Idx>=Low(ReservedWords)) and (Idx<=High(ReservedWords)) then
        ReservedWords[Idx]^.Insert(NewStr(WordS));
    end;
  for I:=Low(AsmReservedWords) to High(AsmReservedWords) do
    New(AsmReservedWords[I], Init(50,10));
  for I:=1 to GetAsmReservedWordCount do
    begin
      WordS:=UpcaseStr(GetAsmReservedWord(I-1)); Idx:=length(WordS);
      if (Idx>=Low(AsmReservedWords)) and (Idx<=High(AsmReservedWords)) then
        begin
          if not AsmReservedWords[Idx]^.Search(@WordS, J) then
            AsmReservedWords[Idx]^.Insert(NewStr(WordS));
        end;
    end;
end;

procedure DoneReservedWords;
var I: integer;
begin
  for I:=Low(ReservedWords) to High(ReservedWords) do
    if assigned(ReservedWords[I]) then
      begin
        dispose(ReservedWords[I],done);
        ReservedWords[I]:=nil;
      end;
  for I:=Low(AsmReservedWords) to High(AsmReservedWords) do
    if assigned(AsmReservedWords[I]) then
      begin
        dispose(AsmReservedWords[I],done);
        ReservedWords[I]:=nil;
      end;
  DoneTokens;
end;

function IsFPReservedWord(S: string): boolean;
var _Is: boolean;
    Idx,Item: sw_integer;
begin
  Idx:=length(S); _Is:=false;
  if (Low(ReservedWords)<=Idx) and (Idx<=High(ReservedWords)) and
     (ReservedWords[Idx]<>nil) and (ReservedWords[Idx]^.Count<>0) then
    begin
      S:=UpcaseStr(S);
      _Is:=ReservedWords[Idx]^.Search(@S,Item);
    end;
  IsFPReservedWord:=_Is;
end;

function IsFPAsmReservedWord(S: string): boolean;
var _Is: boolean;
    Idx,Item,Len: sw_integer;
    LastC : Char;
    LastTwo : String[2];
begin
  Idx:=length(S); _Is:=false;
  if (Low(AsmReservedWords)<=Idx) and (Idx<=High(AsmReservedWords)) and
     (AsmReservedWords[Idx]<>nil) and (AsmReservedWords[Idx]^.Count<>0) then
    begin
      S:=UpcaseStr(S);
      _Is:=AsmReservedWords[Idx]^.Search(@S,Item);
{$ifdef i386}
      if not _Is and (Length(S)>1) then
        begin
          LastC:=S[Length(S)];
          if LastC in ['B','D','L','Q','S','T','V','W'] then
            begin
              Delete(S,Length(S),1);
              Dec(Idx);
              if (AsmReservedWords[Idx]<>nil) and (AsmReservedWords[Idx]^.Count<>0) then
                _Is:=AsmReservedWords[Idx]^.Search(@S,Item);
              if not _Is and (Length(S)>1) then
                begin
                  LastTwo:=S[Length(S)]+LastC;
                  if (LastTwo='BL') or
                     (LastTwo='WL') or
                     (LastTwo='BW') then
                    begin
                      Delete(S,Length(S),1);
                      Dec(Idx);
                      if (AsmReservedWords[Idx]<>nil) and (AsmReservedWords[Idx]^.Count<>0) then
                        _Is:=AsmReservedWords[Idx]^.Search(@S,Item);
                    end;
                end;
            end;
        end;
{$endif i386}
    end;
  IsFPAsmReservedWord:=_Is;
end;


{*****************************************************************************
                               SearchWindow
*****************************************************************************}

function SearchWindowWithNo(No: integer): PWindow;
var P: PWindow;
begin
  P:=Message(Desktop,evBroadcast,cmSearchWindow+No,nil);
  if pointer(P)=pointer(Desktop) then P:=nil;
  SearchWindowWithNo:=P;
end;

function SearchWindow(const Title: string): PWindow;
function Match(P: PView): boolean; {$ifndef FPC}far;{$endif}
var W: PWindow;
    OK: boolean;
begin
  W:=nil;
  { we have a crash here because of the TStatusLine
    that can also have one of these values
    but is not a Window object PM }
  if P<>pointer(StatusLine) then
  if IsWindow(P) then
    W:=PWindow(P);
  OK:=(W<>nil);
  if OK then
  begin
    OK:=CompareText(W^.GetTitle(255),Title)=0;
  end;
  Match:=OK;
end;
var W: PView;
begin
  W:=Application^.FirstThat(@Match);
{    This is wrong because TStatusLine is also considered PM }
  if not Assigned(W) then W:=Desktop^.FirstThat(@Match);
  { But why do we need to check all ??
    Probably because of the ones which were not inserted into
    Desktop as the Messages view

    Exactly. Some windows are inserted directly in the Application and not
    in the Desktop. btw. Does TStatusLine.HelpCtx really change? Why?
    Only GetHelpCtx should return different values depending on the
    focused view (and it's helpctx), but TStatusLine's HelpCtx field
    shouldn't change...  Gabor

  if Assigned(W)=false then W:=Desktop^.FirstThat(@Match);}
  SearchWindow:=PWindow(W);
end;

function SearchFreeWindowNo: integer;
var No: integer;
begin
  No:=1;
  while (No<100) and (SearchWindowWithNo(No)<>nil) do
    Inc(No);
  if No=100 then No:=0;
  SearchFreeWindowNo:=No;
end;


{*****************************************************************************
                              TIntegerLine
 *****************************************************************************}

constructor TIntegerLine.Init(var Bounds: TRect; AMin, AMax: longint);
begin
  if inherited Init(Bounds, Bounds.B.X-Bounds.A.X-1)=false then
    Fail;
  Validator:=New(PRangeValidator, Init(AMin, AMax));
end;


{*****************************************************************************
                               SourceEditor
*****************************************************************************}

function SearchCoreForFileName(AFileName: string): PCodeEditorCore;
var EC: PCodeEditorCore;
function Check(P: PView): boolean; {$ifndef FPC}far;{$endif}
var OK: boolean;
begin
  OK:=P^.HelpCtx=hcSourceWindow;
  if OK then
    with PSourceWindow(P)^ do
     if FixFileName(Editor^.FileName)=AFileName then
       begin
         EC:=Editor^.Core;
         OK:=true;
       end
     else
       OK:=false;
  Check:=OK;
end;
begin
  EC:=nil;
  AFileName:=FixFileName(AFileName);
  { do not use the same core for all new files }
  if AFileName<>'' then
    Desktop^.FirstThat(@Check);
  SearchCoreForFileName:=EC;
end;

constructor TSourceEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator;const AFileName: string);
var EC: PCodeEditorCore;
begin
  EC:=SearchCoreForFileName(AFileName);
  inherited Init(Bounds,AHScrollBar,AVScrollBar,AIndicator,EC,AFileName);
  SetStoreUndo(true);
  CompileStamp:=0;
end;

function TSourceEditor.GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer;
var Count: integer;
begin
  case SpecClass of
    ssCommentPrefix   : Count:=3;
    ssCommentSingleLinePrefix   : Count:=1;
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
    ssCommentSingleLinePrefix :
      case Index of
        0 : S:='//';
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
    else
      S:='';
  end;
  GetSpecSymbol:=S;
end;

function TSourceEditor.IsReservedWord(const S: string): boolean;
begin
  IsReservedWord:=IsFPReservedWord(S);
end;

function TSourceEditor.IsAsmReservedWord(const S: string): boolean;
begin
  IsAsmReservedWord:=IsFPAsmReservedWord(S);
end;

function TSourceEditor.TranslateCodeTemplate(const Shortcut: string; ALines: PUnsortedStringCollection): boolean;
begin
  TranslateCodeTemplate:=FPTranslateCodeTemplate(ShortCut,ALines);
end;

function TSourceEditor.SelectCodeTemplate(var ShortCut: string): boolean;
var D: PCodeTemplatesDialog;
    OK: boolean;
begin
  New(D, Init(true));
  OK:=Desktop^.ExecView(D)=cmOK;
  if OK then ShortCut:=D^.GetSelectedShortCut;
  Dispose(D, Done);
  SelectCodeTemplate:=OK;
end;

function TSourceEditor.CompleteCodeWord(const WordS: string; var Text: string): boolean;
begin
  CompleteCodeWord:=FPCompleteCodeWord(WordS,Text);
end;

procedure TSourceEditor.SetCodeCompleteWord(const S: string);
var R: TRect;
begin
  inherited SetCodeCompleteWord(S);
  if S='' then
    begin
      if Assigned(CodeCompleteTip) then Dispose(CodeCompleteTip, Done);
      CodeCompleteTip:=nil;
    end
  else
    begin
      R.Assign(0,0,20,1);
      if Assigned(CodeCompleteTip)=false then
        begin
          New(CodeCompleteTip, Init(R, S, alCenter));
          CodeCompleteTip^.Hide;
          Application^.Insert(CodeCompleteTip);
        end
      else
        CodeCompleteTip^.SetText(S);
      AlignCodeCompleteTip;
    end;
end;

procedure TSourceEditor.AlignCodeCompleteTip;
var P: TPoint;
    S: string;
    R: TRect;
begin
  if Assigned(CodeCompleteTip)=false then Exit;
  S:=CodeCompleteTip^.GetText;
  P.Y:=CurPos.Y;
  { determine the center of current word fragment }
  P.X:=CurPos.X-(length(GetCodeCompleteFrag) div 2);
  { calculate position for centering the complete word over/below the current }
  P.X:=P.X-(length(S) div 2);

  P.X:=P.X-Delta.X;
  P.Y:=P.Y-Delta.Y;
  MakeGlobal(P,P);
  if Assigned(CodeCompleteTip^.Owner) then
    CodeCompleteTip^.Owner^.MakeLocal(P,P);

  { ensure that the tooltip stays in screen }
  P.X:=Min(Max(0,P.X),ScreenWidth-length(S)-2-1);
  { align it vertically }
  if P.Y>round(ScreenHeight*3/4) then
    Dec(P.Y)
  else
    Inc(P.Y);
  R.Assign(P.X,P.Y,P.X+1+length(S)+1,P.Y+1);
  CodeCompleteTip^.Locate(R);
  if CodeCompleteTip^.GetState(sfVisible)=false then
    CodeCompleteTip^.Show;
end;

procedure TSourceEditor.ModifiedChanged;
begin
  inherited ModifiedChanged;
  if (@Self<>Clipboard) and GetModified then
    begin
      { global flags }
      EditorModified:=true;
      { reset compile flags as the file is
      not the same as at the compilation anymore }
      CompileStamp:=-1;
    end;
end;

procedure TSourceEditor.InsertOptions;
var C: PUnsortedStringCollection;
    Y: sw_integer;
    S: string;
begin
  Lock;
  New(C, Init(10,10));
  GetCompilerOptionLines(C);
  if C^.Count>0 then
  begin
    for Y:=0 to C^.Count-1 do
    begin
      S:=C^.At(Y)^;
      InsertLine(Y,S);
    end;
    AdjustSelectionPos(0,0,0,C^.Count);
    UpdateAttrs(0,attrAll);
    DrawLines(0);
    SetModified(true);
  end;
  Dispose(C, Done);
  UnLock;
end;

procedure  TSourceEditor.PushInfo(Const st : string);
begin
  PushStatus(st);
end;

procedure  TSourceEditor.PopInfo;
begin
  PopStatus;
end;

function TSourceEditor.GetLocalMenu: PMenu;
var M: PMenu;
begin
  M:=NewMenu(
    NewItem(menu_edit_cut,menu_key_edit_cut,kbShiftDel,cmCut,hcCut,
    NewItem(menu_edit_copy,menu_key_edit_copy,kbCtrlIns,cmCopy,hcCopy,
    NewItem(menu_edit_paste,menu_key_edit_paste,kbShiftIns,cmPaste,hcPaste,
    NewItem(menu_edit_clear,menu_key_edit_clear,kbCtrlDel,cmClear,hcClear,
    NewLine(
    NewItem(menu_srclocal_openfileatcursor,'',kbNoKey,cmOpenAtCursor,hcOpenAtCursor,
    NewItem(menu_srclocal_browseatcursor,'',kbNoKey,cmBrowseAtCursor,hcBrowseAtCursor,
    NewItem(menu_srclocal_topicsearch,menu_key_help_topicsearch,kbCtrlF1,cmHelpTopicSearch,hcHelpTopicSearch,
    NewLine(
    NewItem(menu_srclocal_options,'',kbNoKey,cmEditorOptions,hcEditorOptions,
    nil)))))))))));
  GetLocalMenu:=M;
end;

function TSourceEditor.GetCommandTarget: PView;
begin
  GetCommandTarget:=@Self;
end;

function TSourceEditor.CreateLocalMenuView(var Bounds: TRect; M: PMenu): PMenuPopup;
var MV: PAdvancedMenuPopup;
begin
  New(MV, Init(Bounds,M));
  CreateLocalMenuView:=MV;
end;

{$ifdef DebugUndo}
procedure TSourceEditor.DumpUndo;
var
  i : sw_integer;
begin
  ClearToolMessages;
  AddToolCommand('UndoList Dump');
  for i:=0 to Core^.UndoList^.count-1 do
    with Core^.UndoList^.At(i)^ do
      begin
       if is_grouped_action then
         AddToolMessage('','Group '+ActionString[action]+' '+IntToStr(ActionCount)+' elementary actions',0,0)
       else
         AddToolMessage('',ActionString[action]+' '+IntToStr(StartPos.Y+1)+':'+IntToStr(StartPos.X+1)+
           ' '+IntToStr(EndPos.Y+1)+':'+IntToStr(EndPos.X+1)+' "'+GetStr(Text)+'"',0,0);
      end;
  if Core^.RedoList^.count>0 then
    AddToolCommand('RedoList Dump');
  for i:=0 to Core^.RedoList^.count-1 do
    with Core^.RedoList^.At(i)^ do
      begin
       if is_grouped_action then
         AddToolMessage('','Group '+ActionString[action]+' '+IntToStr(ActionCount)+' elementary actions',0,0)
       else
         AddToolMessage('',ActionString[action]+' '+IntToStr(StartPos.Y+1)+':'+IntToStr(StartPos.X+1)+
         ' '+IntToStr(EndPos.Y+1)+':'+IntToStr(EndPos.X+1)+' "'+GetStr(Text)+'"',0,0);
      end;
  UpdateToolMessages;
  if Assigned(MessagesWindow) then
    MessagesWindow^.Focus;
end;

procedure TSourceEditor.UndoAll;
begin
  While Core^.UndoList^.count>0 do
    Undo;
end;

procedure TSourceEditor.RedoAll;
begin
  While Core^.RedoList^.count>0 do
    Redo;
end;

{$endif DebugUndo}

function TSourceEditor.Valid(Command: Word): Boolean;
var OK: boolean;
begin
  OK:=inherited Valid(Command);
  if OK and ({(Command=cmClose) or already handled in TFileEditor.Valid PM }
     (Command=cmAskSaveAll)) then
    if IsClipboard=false then
      OK:=SaveAsk(false);
  Valid:=OK;
end;


procedure TSourceEditor.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    S: string;
begin
  TranslateMouseClick(@Self,Event);
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbCtrlEnter :
            Message(@Self,evCommand,cmOpenAtCursor,nil);
        else DontClear:=true;
        end;
        if not DontClear then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
  case Event.What of
    evBroadcast :
      case Event.Command of
          cmCalculatorPaste :
            begin
              InsertText(FloatToStr(CalcClipboard,0));
              ClearEvent(Event);
            end;
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
{$ifdef DebugUndo}
          cmDumpUndo    : DumpUndo;
          cmUndoAll     : UndoAll;
          cmRedoAll     : RedoAll;
{$endif DebugUndo}
          cmBrowseAtCursor:
            begin
              S:=LowerCaseStr(GetEditorCurWord(@Self,[]));
              OpenOneSymbolBrowser(S);
            end;
          cmOpenAtCursor :
            begin
              S:=LowerCaseStr(GetEditorCurWord(@Self,['.']));
              if Pos('.',S)<>0 then
                OpenFileName:=S else
              OpenFileName:=S+'.pp'+ListSeparator+
                            S+'.pas'+ListSeparator+
                            S+'.inc';
              Message(Application,evCommand,cmOpen,nil);
            end;
          cmEditorOptions :
            Message(Application,evCommand,cmEditorOptions,@Self);
          cmHelp :
            Message(@Self,evCommand,cmHelpTopicSearch,@Self);
          cmHelpTopicSearch :
            HelpTopicSearch(@Self);
        else DontClear:=true;
        end;
        if not DontClear then ClearEvent(Event);
      end;
  end;
end;

constructor TFPHeapView.Init(var Bounds: TRect);
begin
  if inherited Init(Bounds)=false then Fail;
  Options:=Options or gfGrowHiX or gfGrowHiY;
  EventMask:=EventMask or evIdle;
  GrowMode:=gfGrowAll;
end;

constructor TFPHeapView.InitKb(var Bounds: TRect);
begin
  if inherited InitKb(Bounds)=false then Fail;
  Options:=Options or gfGrowHiX or gfGrowHiY;
  EventMask:=EventMask or evIdle;
  GrowMode:=gfGrowAll;
end;

procedure TFPHeapView.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evIdle :
      Update;
  end;
  inherited HandleEvent(Event);
end;

constructor TFPClockView.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  EventMask:=EventMask or evIdle;
end;

procedure TFPClockView.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evIdle :
      Update;
  end;
  inherited HandleEvent(Event);
end;

function TFPClockView.GetPalette: PPalette;
const P: string[length(CFPClockView)] = CFPClockView;
begin
  GetPalette:=@P;
end;

procedure TFPWindow.SetState(AState: Word; Enable: Boolean);
var OldState: word;
begin
  OldState:=State;
  inherited SetState(AState,Enable);
  if AutoNumber then
    if (AState and (sfVisible+sfExposed))<>0 then
      if GetState(sfVisible+sfExposed) then
        begin
          if Number=0 then
            Number:=SearchFreeWindowNo;
          ReDraw;
        end
      else
        Number:=0;
  if ((AState and sfActive)<>0) and (((OldState xor State) and sfActive)<>0) then
    UpdateCommands;
end;

procedure TFPWindow.UpdateCommands;
begin
end;

procedure TFPWindow.Update;
begin
  ReDraw;
end;

procedure   TFPWindow.SelectInDebugSession;
var
  F,PrevCurrent : PView;
begin
  DeskTop^.Lock;
  PrevCurrent:=Desktop^.Current;
  F:=PrevCurrent;
  While assigned(F) and
    ((F^.HelpCtx = hcGDBWindow) or
     (F^.HelpCtx = hcdisassemblyWindow) or
     (F^.HelpCtx = hcWatchesWindow) or
     (F^.HelpCtx = hcStackWindow) or
     (F^.HelpCtx = hcRegistersWindow) or
     (F^.HelpCtx = hcFPURegisters)) do
    F:=F^.NextView;
  if F<>@Self then
    Select;
  if PrevCurrent<>F then
    Begin
      Desktop^.InsertBefore(@self,F);
      PrevCurrent^.Select;
    End;
  DeskTop^.Unlock;
end;

procedure TFPWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate :
          Update;
        cmSearchWindow+1..cmSearchWindow+99 :
          if (Event.Command-cmSearchWindow=Number) then
              ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;


constructor TFPWindow.Load(var S: TStream);
begin
  inherited Load(S);
  S.Read(AutoNumber,SizeOf(AutoNumber));
end;

procedure TFPWindow.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(AutoNumber,SizeOf(AutoNumber));
end;

function TFPHelpViewer.GetLocalMenu: PMenu;
var M: PMenu;
begin
  M:=NewMenu(
    NewItem(menu_hlplocal_contents,'',kbNoKey,cmHelpContents,hcHelpContents,
    NewItem(menu_hlplocal_index,menu_key_hlplocal_index,kbShiftF1,cmHelpIndex,hcHelpIndex,
    NewItem(menu_hlplocal_topicsearch,menu_key_hlplocal_topicsearch,kbCtrlF1,cmHelpTopicSearch,hcHelpTopicSearch,
    NewItem(menu_hlplocal_prevtopic,menu_key_hlplocal_prevtopic,kbAltF1,cmHelpPrevTopic,hcHelpPrevTopic,
    NewLine(
    NewItem(menu_hlplocal_copy,menu_key_hlplocal_copy,kbCtrlIns,cmCopy,hcCopy,
    nil)))))));
  GetLocalMenu:=M;
end;

function TFPHelpViewer.GetCommandTarget: PView;
begin
  GetCommandTarget:=Application;
end;

constructor TFPHelpWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ASourceFileID: word;
  AContext: THelpCtx; ANumber: Integer);
begin
  inherited Init(Bounds,ATitle,ASourceFileID,AContext,ANumber);
  HelpCtx:=hcHelpWindow;
  HideOnClose:=true;
end;

destructor TFPHelpWindow.Done;
begin
  if HelpWindow=@Self then
    HelpWindow:=nil;
  Inherited Done;
end;

procedure TFPHelpWindow.InitHelpView;
var R: TRect;
begin
  GetExtent(R); R.Grow(-1,-1);
  HelpView:=New(PFPHelpViewer, Init(R, HSB, VSB));
  HelpView^.GrowMode:=gfGrowHiX+gfGrowHiY;
end;

procedure TFPHelpWindow.Show;
begin
  inherited Show;
  if GetState(sfVisible) and (Number=0) then
    begin
      Number:=SearchFreeWindowNo;
      ReDraw;
    end;
end;

procedure TFPHelpWindow.Hide;
begin
  inherited Hide;
  if GetState(sfVisible)=false then
    Number:=0;
end;

procedure TFPHelpWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate :
          ReDraw;
        cmSearchWindow+1..cmSearchWindow+99 :
          if (Event.Command-cmSearchWindow=Number) then
              ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TFPHelpWindow.GetPalette: PPalette;
const P: string[length(CIDEHelpDialog)] = CIDEHelpDialog;
begin
  GetPalette:=@P;
end;

constructor TFPHelpWindow.Load(var S: TStream);
begin
  Abstract;
end;

procedure TFPHelpWindow.Store(var S: TStream);
begin
  Abstract;
end;

constructor TSourceWindow.Init(var Bounds: TRect; AFileName: string);
var HSB,VSB: PScrollBar;
    R: TRect;
    PA : Array[1..2] of pointer;
    LoadFile: boolean;
begin
  inherited Init(Bounds,AFileName,{SearchFreeWindowNo}0);
  AutoNumber:=true;
  Options:=Options or ofTileAble;
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
    begin
      if Editor^.LoadFile=false then
        ErrorBox(FormatStrStr(msg_errorreadingfile,AFileName),nil)
      { warn if modified, but not if modified in another
        already open window PM }
      else if Editor^.GetModified and (Editor^.Core^.GetBindingCount=1) then
        begin
          PA[1]:=@AFileName;
          longint(PA[2]):={Editor^.ChangedLine}-1;
          EditorDialog(edChangedOnloading,@PA);
        end;
   end;
  Insert(Editor);
  If assigned(BreakpointsCollection) then
    BreakpointsCollection^.ShowBreakpoints(@Self);
  UpdateTitle;
end;

procedure TSourceWindow.UpdateTitle;
var Name: string;
    Count: sw_integer;
begin
  if Editor^.FileName<>'' then
  begin
    Name:=SmartPath(Editor^.FileName);
    Count:=Editor^.Core^.GetBindingCount;
    if Count>1 then
    begin
      Name:=Name+':'+IntToStr(Editor^.Core^.GetBindingIndex(Editor)+1);
    end;
    SetTitle(Name);
  end;
end;

function TSourceWindow.GetTitle(MaxSize: sw_Integer): TTitleStr;
begin
  GetTitle:=OptimizePath(inherited GetTitle(255),MaxSize);
end;

procedure TSourceWindow.SetTitle(ATitle: string);
begin
  if Title<>nil then DisposeStr(Title);
  Title:=NewStr(ATitle);
{$ifdef FVISION}
  If assigned(Owner) then
    DrawBorder;
{$else}
  Frame^.DrawView;
{$endif}
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
          if @Self<>ClipboardWindow then
            ClearEvent(Event);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmHide :
            Hide;
          cmSave :
            if Editor^.IsClipboard=false then
             if (Editor^.FileName='') and Editor^.GetModified then
              Editor^.SaveAs
             else
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

procedure TSourceWindow.UpdateCommands;
var Active: boolean;
begin
  Active:=GetState(sfActive);
  if Editor^.IsClipboard=false then
  begin
    SetCmdState(SourceCmds+CompileCmds,Active);
    SetCmdState(EditorCmds,Active);
  end;
  SetCmdState(ToClipCmds+FromClipCmds+NulClipCmds+UndoCmd+RedoCmd,Active);
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
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

constructor TSourceWindow.Load(var S: TStream);
begin
  Title:=S.ReadStr;
  PushStatus(FormatStrStr(msg_loadingfile,GetStr(Title)));
  inherited Load(S);
  GetSubViewPtr(S,Indicator);
  GetSubViewPtr(S,Editor);
  If assigned(BreakpointsCollection) then
    BreakpointsCollection^.ShowBreakpoints(@Self);
  PopStatus;
end;

procedure TSourceWindow.Store(var S: TStream);
begin
  S.WriteStr(Title);
  PushStatus(FormatStrStr(msg_storingfile,GetStr(Title)));
  inherited Store(S);

  PutSubViewPtr(S,Indicator);
  PutSubViewPtr(S,Editor);
  PopStatus;
end;

procedure TSourceWindow.Close;
begin
  inherited Close;
end;

destructor TSourceWindow.Done;
begin
  PushStatus(FormatStrStr(msg_closingfile,GetStr(Title)));
  if not IDEApp.IsClosing then
    Message(Application,evBroadcast,cmSourceWndClosing,@Self);
  inherited Done;
  IDEApp.SourceWindowClosed;
{  if not IDEApp.IsClosing then
    Message(Application,evBroadcast,cmUpdate,@Self);}
  PopStatus;
end;

function TGDBSourceEditor.Valid(Command: Word): Boolean;
var OK: boolean;
begin
  OK:=TCodeEditor.Valid(Command);
  { do NOT ask for save !!
  if OK and ((Command=cmClose) or (Command=cmQuit)) then
     if IsClipboard=false then
    OK:=SaveAsk;  }
  Valid:=OK;
end;

procedure  TGDBSourceEditor.AddLine(const S: string);
begin
   if Silent or (IgnoreStringAtEnd and (S=LastCommand)) then exit;
   inherited AddLine(S);
   LimitsChanged;
end;

procedure  TGDBSourceEditor.AddErrorLine(const S: string);
begin
   if Silent then exit;
   inherited AddLine(S);
   { display like breakpoints in red }
   SetLineFlagState(GetLineCount-1,lfBreakpoint,true);
   LimitsChanged;
end;

const
  GDBReservedCount = 6;
  GDBReservedLongest = 3;
  GDBReserved : array[1..GDBReservedCount] of String[GDBReservedLongest] =
  ('gdb','b','n','s','f','bt');

function IsGDBReservedWord(const S : string) : boolean;
var
  i : longint;
begin
  for i:=1 to GDBReservedCount do
    if (S=GDBReserved[i]) then
      begin
        IsGDBReservedWord:=true;
        exit;
      end;
  IsGDBReservedWord:=false;
end;

function TGDBSourceEditor.IsReservedWord(const S: string): boolean;
begin
  IsReservedWord:=IsGDBReservedWord(S);
end;

function TGDBSourceEditor.InsertNewLine: Sw_integer;
Var
  S : string;
  CommandCalled : boolean;

begin
  if IsReadOnly then begin InsertNewLine:=-1; Exit; end;
  if CurPos.Y<GetLineCount then S:=GetDisplayText(CurPos.Y) else S:='';
  s:=Copy(S,1,CurPos.X);
  CommandCalled:=false;
  if Pos(GDBPrompt,S)=1 then
    Delete(S,1,length(GDBPrompt));
  if assigned(Debugger) then
    if S<>'' then
      begin
        LastCommand:=S;
        { should be true only if we are at the end ! }
        IgnoreStringAtEnd:=(CurPos.Y=GetLineCount-1) and
          (CurPos.X>=length(RTrim(GetDisplayText(GetLineCount-1))));
        Debugger^.Command(S);
        CommandCalled:=true;
        IgnoreStringAtEnd:=false;
      end
    else if AutoRepeat and (CurPos.Y=GetLineCount-1) then
      begin
        Debugger^.Command(LastCommand);
        CommandCalled:=true;
      end;
  InsertNewLine:=inherited InsertNewLine;
  If CommandCalled then
    InsertText(GDBPrompt);
end;


constructor TGDBWindow.Init(var Bounds: TRect);
var HSB,VSB: PScrollBar;
    R: TRect;
begin
  inherited Init(Bounds,dialog_gdbwindow,0);
  Options:=Options or ofTileAble;
  AutoNumber:=true;
  HelpCtx:=hcGDBWindow;
  GetExtent(R); R.A.Y:=R.B.Y-1; R.Grow(-1,0); R.A.X:=14;
  New(HSB, Init(R)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY; Insert(HSB);
  GetExtent(R); R.A.X:=R.B.X-1; R.Grow(0,-1);
  New(VSB, Init(R)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  GetExtent(R); R.A.X:=3; R.B.X:=14; R.A.Y:=R.B.Y-1;
  New(Indicator, Init(R));
  Indicator^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Indicator);
  GetExtent(R); R.Grow(-1,-1);
  New(Editor, Init(R, HSB, VSB, Indicator, GDBOutputFile));
  Editor^.GrowMode:=gfGrowHiX+gfGrowHiY;
  Editor^.SetFlags(efInsertMode+efSyntaxHighlight+efNoIndent+efExpandAllTabs);
  if ExistsFile(GDBOutputFile) then
    begin
      if Editor^.LoadFile=false then
        ErrorBox(FormatStrStr(msg_errorreadingfile,GDBOutputFile),nil);
    end
  else
  { Empty files are buggy !! }
    Editor^.AddLine('');
  Insert(Editor);
  if assigned(Debugger) then
    Debugger^.SetWidth(Size.X-1);
  Editor^.silent:=false;
  Editor^.AutoRepeat:=true;
  Editor^.InsertText(GDBPrompt);
end;

procedure TGDBWindow.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmSaveAs :
              Editor^.SaveAs;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

destructor TGDBWindow.Done;
begin
  if @Self=GDBWindow then
    GDBWindow:=nil;
  inherited Done;
end;

constructor TGDBWindow.Load(var S: TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S,Indicator);
  GetSubViewPtr(S,Editor);
  GDBWindow:=@self;
end;

procedure TGDBWindow.Store(var S: TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S,Indicator);
  PutSubViewPtr(S,Editor);
end;

function TGDBWindow.GetPalette: PPalette;
const P: string[length(CSourceWindow)] = CSourceWindow;
begin
  GetPalette:=@P;
end;

procedure TGDBWindow.WriteOutputText(Buf : pchar);
begin
  {selected normal color ?}
  WriteText(Buf,false);
end;

procedure TGDBWindow.WriteErrorText(Buf : pchar);
begin
  {selected normal color ?}
  WriteText(Buf,true);
end;

procedure TGDBWindow.WriteString(Const S : string);
begin
  Editor^.AddLine(S);
end;

procedure TGDBWindow.WriteErrorString(Const S : string);
begin
  Editor^.AddErrorLine(S);
end;

procedure TGDBWindow.WriteText(Buf : pchar;IsError : boolean);
  var p,pe : pchar;
      s : string;
begin
  p:=buf;
  DeskTop^.Lock;
  While assigned(p) and (p^<>#0) do
    begin
       pe:=strscan(p,#10);
       if pe<>nil then
         pe^:=#0;
       s:=strpas(p);
       If IsError then
         Editor^.AddErrorLine(S)
       else
         Editor^.AddLine(S);
       { restore for dispose }
       if pe<>nil then
         pe^:=#10;
       if pe=nil then
         p:=nil
       else
         begin
           if pe-p > High(s) then
             p:=p+High(s)-1
           else
             begin
               p:=pe;
               inc(p);
             end;
         end;
    end;
  DeskTop^.Unlock;
  Editor^.Draw;
end;

procedure TGDBWindow.UpdateCommands;
var Active: boolean;
begin
  Active:=GetState(sfActive);
  SetCmdState([cmSaveAs,cmHide,cmRun],Active);
  SetCmdState(EditorCmds,Active);
  SetCmdState(ToClipCmds+FromClipCmds+NulClipCmds+UndoCmd+RedoCmd,Active);
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
end;


function  TDisasLineCollection.At(Index: sw_Integer): PDisasLine;
begin
  At := PDisasLine(Inherited At(Index));
end;

constructor TDisassemblyEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator;const AFileName: string);
begin
  Inherited Init(Bounds,AHScrollBar,AVScrollBar,AIndicator,AFileName);
  GrowMode:=gfGrowHiX+gfGrowHiY;
  SetFlags(efInsertMode+efSyntaxHighlight+efNoIndent+efExpandAllTabs{+efHighlightRow});
  New(DisasLines,Init(500,1000));
  Core^.ChangeLinesTo(DisasLines);
  { do not allow to write into that window }
  ReadOnly:=true;
  AddLine('');
  MinAddress:=0;
  MaxAddress:=0;
  CurL:=nil;
  OwnsSource:=false;
  Source:=nil;
end;

destructor TDisassemblyEditor.Done;
begin
  ReleaseSource;
  Inherited Done;
end;

procedure TDisassemblyEditor.ReleaseSource;
begin
  if OwnsSource and assigned(source) then
    begin
      Desktop^.Delete(Source);
      Dispose(Source,Done);
    end;
  OwnsSource:=false;
  Source:=nil;
  CurrentSource:='';
end;

procedure  TDisassemblyEditor.AddSourceLine(const AFileName: string;line : longint);
var
  S : String;
begin
   if AFileName<>CurrentSource then
     begin
       ReleaseSource;
       Source:=SearchOnDesktop(FileName,false);
       if not assigned(Source) then
         begin
           Source:=ITryToOpenFile(nil,AFileName,0,line,false,false,true);
           OwnsSource:=true;
         end
       else
         OwnsSource:=false;
       CurrentSource:=AFileName;
     end;
   if Assigned(Source) and (line>0) then
     S:=Trim(Source^.Editor^.GetLineText(line-1))
   else
     S:='<source not found>';
   CurrentLine:=Line;
   inherited AddLine(AFileName+':'+IntToStr(line)+' '+S);
   { display differently }
   SetLineFlagState(GetLineCount-1,lfSpecialRow,true);
   LimitsChanged;
end;

procedure  TDisassemblyEditor.AddAssemblyLine(const S: string;AAddress : cardinal);
var
  PL : PDisasLine;
  LI : PEditorLineInfo;
begin
   if AAddress<>0 then
     inherited AddLine('$'+hexstr(AAddress,8)+S)
   else
     inherited AddLine(S);
   PL:=DisasLines^.At(DisasLines^.count-1);
   PL^.Address:=AAddress;
   LI:=PL^.GetEditorInfo(@Self);
   if AAddress<>0 then
     LI^.BeginsWithAsm:=true;
   LimitsChanged;
   if ((AAddress<minaddress) or (minaddress=0)) and (AAddress<>0) then
     MinAddress:=AAddress;
   if (AAddress>maxaddress) or (maxaddress=0) then
     MaxAddress:=AAddress;
end;

function   TDisassemblyEditor.GetCurrentLine(address : cardinal) : PDisasLine;

  function IsCorrectLine(PL : PDisasLine) : boolean;
    begin
      IsCorrectLine:=PL^.Address=Address;
    end;
  Var
    PL : PDisasLine;
begin
  PL:=DisasLines^.FirstThat(@IsCorrectLine);
  if Assigned(PL) then
    begin
      if assigned(CurL) then
        CurL^.SetFlagState(lfDebuggerRow,false);
      SetCurPtr(0,DisasLines^.IndexOf(PL));
      PL^.SetFlags(lfDebuggerRow);
      CurL:=PL;
      TrackCursor(false);
    end;
  GetCurrentLine:=PL;
end;

    { PDisassemblyWindow = ^TDisassemblyWindow;
    TDisassemblyWindow = object(TFPWindow)
      Editor    : PDisassemblyEditor;
      Indicator : PIndicator; }
constructor TDisassemblyWindow.Init(var Bounds: TRect);
var HSB,VSB: PScrollBar;
    R: TRect;
begin
  inherited Init(Bounds,dialog_disaswindow,0);
  Options:=Options or ofTileAble;
  AutoNumber:=true;
  HelpCtx:=hcDisassemblyWindow;
  GetExtent(R); R.A.Y:=R.B.Y-1; R.Grow(-1,0); R.A.X:=14;
  New(HSB, Init(R)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY; Insert(HSB);
  GetExtent(R); R.A.X:=R.B.X-1; R.Grow(0,-1);
  New(VSB, Init(R)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  GetExtent(R); R.A.X:=3; R.B.X:=14; R.A.Y:=R.B.Y-1;
  New(Indicator, Init(R));
  Indicator^.GrowMode:=gfGrowLoY+gfGrowHiY;
  Insert(Indicator);
  GetExtent(R); R.Grow(-1,-1);
  New(Editor, Init(R, HSB, VSB, nil, GDBOutputFile));
  Insert(Editor);
  DisassemblyWindow:=@Self;
end;

procedure   TDisassemblyWindow.LoadFunction(Const FuncName : string);
var
   p : pchar;
begin
  If not assigned(Debugger) then Exit;
  Debugger^.Command('set print sym on');
  Debugger^.Command('set width 0xffffffff');
  Debugger^.Command('disas '+FuncName);
  p:=StrNew(Debugger^.GetOutput);
  ProcessPChar(p);
  if (Debugger^.IsRunning) and (FuncName='') then
    Editor^.GetCurrentLine(Debugger^.current_pc);
end;

procedure   TDisassemblyWindow.LoadAddress(Addr : cardinal);
var
   p : pchar;
begin
  If not assigned(Debugger) then Exit;
  Debugger^.Command('set print sym on');
  Debugger^.Command('set width 0xffffffff');
  Debugger^.Command('disas 0x'+HexStr(Addr,8));
  p:=StrNew(Debugger^.GetOutput);
  ProcessPChar(p);
  if Debugger^.IsRunning and
     (Debugger^.current_pc>=Editor^.MinAddress) and
     (Debugger^.current_pc<=Editor^.MaxAddress) then
    Editor^.GetCurrentLine(Debugger^.current_pc);
end;


function TDisassemblyWindow.ProcessPChar(p : pchar) : boolean;
var
  p1: pchar;
  pline : pchar;
  pos1, pos2, CurLine, PrevLine : longint;
  CurAddr : cardinal;
  err : word;
  curaddress, cursymofs, CurFile,
  PrevFile, line : string;
begin
  ProcessPChar:=true;
  Lock;
  Editor^.DisasLines^.FreeAll;
  Editor^.SetFlags(Editor^.GetFlags or efSyntaxHighlight or efKeepLineAttr);

  Editor^.MinAddress:=0;
  Editor^.MaxAddress:=0;
  Editor^.CurL:=nil;
  p1:=p;
  PrevFile:='';
  PrevLine:=0;
  while assigned(p) do
    begin
      pline:=strscan(p,#10);
      if assigned(pline) then
        pline^:=#0;
      line:=strpas(p);
      CurAddr:=0;
      if assigned(pline) then
        begin
          pline^:=#10;
          p:=pline+1;
        end
      else
        p:=nil;
      { now process the line }
      { line is hexaddr <symbol+sym_offset at filename:line> assembly }
      pos1:=pos('<',line);
      if pos1>0 then
        begin
          curaddress:=copy(line,1,pos1-1);
          val(curaddress,CurAddr,err);
          if err>0 then
            val(copy(curaddress,1,err-1),CurAddr,err);
          system.delete(line,1,pos1);
        end;
      pos1:=pos(' at ',line);
      pos2:=pos('>',line);
      if (pos1>0) and (pos1 < pos2) then
        begin
          cursymofs:=copy(line,1,pos1-1);
          CurFile:=copy(line,pos1+4,pos2-pos1-4);
          pos1:=pos(':',CurFile);
          if pos1>0 then
            begin
              val(copy(CurFile,pos1+1,high(CurFile)),CurLine,err);
              system.delete(CurFile,pos1,high(CurFile));
            end
          else
            CurLine:=0;
          system.delete(line,1,pos2);
        end
      else    { no ' at ' found before '>' }
        begin
          cursymofs:=copy(line,1,pos2-1);
          CurFile:='';
          system.delete(line,1,pos2);
        end;
      if (CurFile<>'') and ((CurFile<>PrevFile) or (CurLine<>PrevLine)) then
        begin
          WriteSourceString(CurFile,CurLine);
          PrevLine:=CurLine;
          PrevFile:=CurFile;
        end;
      WriteDisassemblyString(line,curaddr);
    end;
  StrDispose(p1);
  Editor^.ReleaseSource;
  Editor^.UpdateAttrs(0,attrForceFull);
  If assigned(BreakpointsCollection) then
    BreakpointsCollection^.ShowBreakpoints(@Self);
  Unlock;
  ReDraw;
end;

procedure   TDisassemblyWindow.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
end;

procedure   TDisassemblyWindow.WriteSourceString(Const S : string;line : longint);
begin
  Editor^.AddSourceLine(S,line);
end;

procedure   TDisassemblyWindow.WriteDisassemblyString(Const S : string;address : cardinal);
begin
  Editor^.AddAssemblyLine(S,address);
end;

procedure   TDisassemblyWindow.SetCurAddress(address : cardinal);
begin
  if (address<Editor^.MinAddress) or (address>Editor^.MaxAddress) then
    LoadAddress(address);
  Editor^.GetCurrentLine(address);
end;

procedure TDisassemblyWindow.UpdateCommands;
var Active: boolean;
begin
  Active:=GetState(sfActive);
  SetCmdState(SourceCmds+CompileCmds,Active);
  SetCmdState(EditorCmds,Active);
  SetCmdState(ToClipCmds+FromClipCmds+NulClipCmds+UndoCmd+RedoCmd,false);
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
end;


function    TDisassemblyWindow.GetPalette: PPalette;
const P: string[length(CSourceWindow)] = CSourceWindow;
begin
  GetPalette:=@P;
end;

destructor  TDisassemblyWindow.Done;
begin
  if @Self=DisassemblyWindow then
    DisassemblyWindow:=nil;
  inherited Done;
end;



constructor TClipboardWindow.Init;
var R: TRect;
    HSB,VSB: PScrollBar;
begin
  Desktop^.GetExtent(R);
  inherited Init(R, '');
  SetTitle(dialog_clipboard);
  HelpCtx:=hcClipboardWindow;
  Number:=wnNoNumber;
  AutoNumber:=true;

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

  Editor^.SetFlags(Editor^.GetFlags or efUseTabCharacters);
  Hide;

  Clipboard:=Editor;
end;

procedure TClipboardWindow.Close;
begin
  Hide;
end;

constructor TClipboardWindow.Load(var S: TStream);
begin
  inherited Load(S);

  Clipboard:=Editor;
end;

procedure TClipboardWindow.Store(var S: TStream);
begin
  inherited Store(S);
end;

destructor TClipboardWindow.Done;
begin
  inherited Done;
  Clipboard:=nil;
  ClipboardWindow:=nil;
end;


constructor TMessageListBox.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AHScrollBar, AVScrollBar);
  GrowMode:=gfGrowHiX+gfGrowHiY;
  New(ModuleNames, Init(50,100));
  NoSelection:=true;
end;


function TMessageListBox.GetLocalMenu: PMenu;
var M: PMenu;
begin
  if (Owner<>nil) and (Owner^.GetState(sfModal)) then M:=nil else
  M:=NewMenu(
    NewItem(menu_msglocal_clear,'',kbNoKey,cmMsgClear,hcMsgClear,
    NewLine(
    NewItem(menu_msglocal_gotosource,'',kbNoKey,cmMsgGotoSource,hcMsgGotoSource,
    NewItem(menu_msglocal_tracksource,'',kbNoKey,cmMsgTrackSource,hcMsgTrackSource,
    NewLine(
    NewItem(menu_msglocal_saveas,'',kbNoKey,cmSaveAs,hcSaveAs,
    nil)))))));
  GetLocalMenu:=M;
end;

procedure TMessageListBox.SetState(AState: Word; Enable: Boolean);
var OldState: word;
begin
  OldState:=State;
  inherited SetState(AState,Enable);
  if ((AState and sfActive)<>0) and (((OldState xor State) and sfActive)<>0) then
    SetCmdState([cmSaveAs],Enable);
end;


procedure TMessageListBox.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            begin
              Message(@Self,evCommand,cmMsgGotoSource,nil);
              ClearEvent(Event);
              exit;
            end;
        else
          DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListItemSelected :
          if Event.InfoPtr=@Self then
            Message(@Self,evCommand,cmMsgTrackSource,nil);
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmMsgGotoSource :
            if Range>0 then
              begin
                GotoSource;
                ClearEvent(Event);
                exit;
              end;
          cmMsgTrackSource :
            if Range>0 then
              TrackSource;
          cmMsgClear :
            Clear;
          cmSaveAs :
            SaveAs;
          else
            DontClear:=true;
        end;
        if not DontClear then
          ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TMessageListBox.AddItem(P: PMessageItem);
var W : integer;
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

function TMessageListBox.AddModuleName(const Name: string): PString;
var P: PString;
begin
  if ModuleNames<>nil then
    P:=ModuleNames^.Add(Name)
  else
    P:=nil;
  AddModuleName:=P;
end;

function TMessageListBox.GetText(Item,MaxLen: Sw_Integer): String;
var P: PMessageItem;
    S: string;
begin
  P:=List^.At(Item);
  S:=P^.GetText(MaxLen);
  GetText:=copy(S,1,MaxLen);
end;

procedure TMessageListBox.Clear;
begin
  if assigned(List) then
    Dispose(List, Done);
  List:=nil;
  MaxWidth:=0;
  if assigned(ModuleNames) then
    ModuleNames^.FreeAll;
  SetRange(0); DrawView;
  Message(Application,evBroadcast,cmClearLineHighlights,@Self);
end;

procedure TMessageListBox.TrackSource;
var W: PSourceWindow;
    P: PMessageItem;
    R: TRect;
    Row,Col: sw_integer;
    Found : boolean;
begin
  Message(Application,evBroadcast,cmClearLineHighlights,@Self);
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P^.Row=0 then Exit;
  Desktop^.Lock;
  GetNextEditorBounds(R);
  R.B.Y:=Owner^.Origin.Y;
  if P^.Row>0 then Row:=P^.Row-1 else Row:=0;
  if P^.Col>0 then Col:=P^.Col-1 else Col:=0;
  W:=EditorWindowFile(P^.GetModuleName);
  if assigned(W) then
    begin
      W^.GetExtent(R);
      R.B.Y:=Owner^.Origin.Y;
      W^.ChangeBounds(R);
      W^.Editor^.SetCurPtr(Col,Row);
    end
  else
    W:=TryToOpenFile(@R,P^.GetModuleName,Col,Row,true);
  { Try to find it by browsing }
  if W=nil then
    begin
      Desktop^.UnLock;
      Found:=IDEApp.OpenSearch(P^.GetModuleName+'*');
      if found then
        W:=TryToOpenFile(nil,P^.GetModuleName,Col,Row,true);
      Desktop^.Lock;
    end;
  if W<>nil then
    begin
      W^.Select;
      W^.Editor^.TrackCursor(true);
      W^.Editor^.SetLineFlagExclusive(lfHighlightRow,Row);
    end;
  if Assigned(Owner) then
    Owner^.Select;
  Desktop^.UnLock;
end;

procedure TMessageListBox.GotoSource;
var W: PSourceWindow;
    P: PMessageItem;
    R:TRect;
    Row,Col: sw_integer;
    Found : boolean;
begin
  Message(Application,evBroadcast,cmClearLineHighlights,@Self);
  if Range=0 then Exit;
  P:=List^.At(Focused);
  if P^.Row=0 then Exit;
  Desktop^.Lock;
  if P^.Row>0 then Row:=P^.Row-1 else Row:=0;
  if P^.Col>0 then Col:=P^.Col-1 else Col:=0;
  W:=EditorWindowFile(P^.GetModuleName);
  if assigned(W) then
    begin
      W^.GetExtent(R);
      if Owner^.Origin.Y>R.A.Y+4 then
        R.B.Y:=Owner^.Origin.Y;
      W^.ChangeBounds(R);
      W^.Editor^.SetCurPtr(Col,Row);
    end
  else
   W:=TryToOpenFile(nil,P^.GetModuleName,Col,Row,true);
  { Try to find it by browsing }
  if W=nil then
    begin
      Desktop^.UnLock;
      Found:=IDEApp.OpenSearch(P^.GetModuleName+'*');
      if found then
        W:=TryToOpenFile(nil,P^.GetModuleName,Col,Row,true);
      Desktop^.Lock;
    end;
  if assigned(W) then
    begin
      { Message(Owner,evCommand,cmClose,nil);
        This calls close on StackWindow
        rendering P invalid
        so postpone it PM }
      W^.GetExtent(R);
      if (P^.TClass<>0) then
        W^.Editor^.SetErrorMessage(P^.GetText(R.B.X-R.A.X));
      W^.Select;
      Message(Owner,evCommand,cmClose,nil);
    end;
  Desktop^.UnLock;
end;

procedure TMessageListBox.Draw;
var
  I, J, Item: Sw_Integer;
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

constructor TMessageListBox.Load(var S: TStream);
begin
  inherited Load(S);
  New(ModuleNames, Init(50,100));
  NoSelection:=true;
end;

procedure TMessageListBox.Store(var S: TStream);
var OL: PCollection;
    ORV: sw_integer;
begin
  OL:=List; ORV:=Range;

  New(List, Init(1,1)); Range:=0;

  inherited Store(S);

  Dispose(List, Done);
  List:=OL; Range:=ORV;
  { ^^^ nasty trick - has anyone a better idea how to avoid storing the
    collection? Pasting here a modified version of TListBox.Store+
    TAdvancedListBox.Store isn't a better solution, since by eventually
    changing the obj-hierarchy you'll always have to modify this, too - BG }
end;

destructor TMessageListBox.Done;
begin
  inherited Done;
  if List<>nil then Dispose(List, Done);
  if ModuleNames<>nil then Dispose(ModuleNames, Done);
end;

constructor TMessageItem.Init(AClass: longint; const AText: string; AModule: PString; ARow, ACol: sw_integer);
begin
  inherited Init;
  TClass:=AClass;
  Text:=NewStr(AText);
  Module:=AModule;
  Row:=ARow; Col:=ACol;
end;

function TMessageItem.GetText(MaxLen: Sw_integer): string;
var S: string;
begin
  if Text=nil then S:='' else S:=Text^;
  if (Module<>nil) then
     S:=NameAndExtOf(Module^)+'('+IntToStr(Row)+') '+S;
  if length(S)>MaxLen then S:=copy(S,1,MaxLen-2)+'..';
  GetText:=S;
end;

procedure TMessageItem.Selected;
begin
end;

function TMessageItem.GetModuleName: string;
begin
  GetModuleName:=GetStr(Module);
end;

destructor TMessageItem.Done;
begin
  inherited Done;
  if Text<>nil then DisposeStr(Text);
{  if Module<>nil then DisposeStr(Module);}
end;


procedure  TFPDlgWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmSearchWindow+1..cmSearchWindow+99 :
          if (Event.Command-cmSearchWindow=Number) then
              ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;


constructor TProgramInfoWindow.Init;
var R,R2: TRect;
    HSB,VSB: PScrollBar;
    ST: PStaticText;
    C: word;
const White = 15;
begin
  Desktop^.GetExtent(R); R.A.Y:=R.B.Y-13;
  inherited Init(R, dialog_programinformation, wnNoNumber);

  HelpCtx:=hcInfoWindow;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+3;
  C:=((Desktop^.GetColor(32+6) and $f0) or White)*256+Desktop^.GetColor(32+6);
  New(InfoST, Init(R,'', C, false)); InfoST^.GrowMode:=gfGrowHiX;
  Insert(InfoST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,3); R.B.Y:=R.A.Y+1;
  New(ST, Init(R, CharStr('Ä', MaxViewWidth))); ST^.GrowMode:=gfGrowHiX; Insert(ST);
  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,4);
  R2.Copy(R); Inc(R2.B.Y); R2.A.Y:=R2.B.Y-1;
  New(HSB, Init(R2)); HSB^.GrowMode:=gfGrowLoY+gfGrowHiY+gfGrowHiX; Insert(HSB);
  R2.Copy(R); Inc(R2.B.X); R2.A.X:=R2.B.X-1;
  New(VSB, Init(R2)); VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  New(LogLB, Init(R,HSB,VSB));
  LogLB^.GrowMode:=gfGrowHiX+gfGrowHiY;
  LogLB^.Transparent:=true;
  Insert(LogLB);
  Update;
end;

constructor TProgramInfoWindow.Load(var S : TStream);
begin
  inherited Load(S);
  GetSubViewPtr(S,InfoST);
  GetSubViewPtr(S,LogLB);
end;

procedure TProgramInfoWindow.Store(var S : TStream);
begin
  inherited Store(S);
  PutSubViewPtr(S,InfoST);
  PutSubViewPtr(S,LogLB);
end;

procedure TProgramInfoWindow.AddMessage(AClass: longint; Msg, Module: string; Line, Column: longint);
begin
  if AClass>=V_Info then Line:=0;
  LogLB^.AddItem(New(PCompilerMessage, Init(AClass, Msg, LogLB^.AddModuleName(Module), Line, Column)));
end;

procedure TProgramInfoWindow.ClearMessages;
begin
  LogLB^.Clear;
  ReDraw;
end;

procedure TProgramInfoWindow.SizeLimits(var Min, Max: TPoint);
begin
  inherited SizeLimits(Min,Max);
  Min.X:=30; Min.Y:=9;
end;

procedure TProgramInfoWindow.Close;
begin
  Hide;
end;

procedure TProgramInfoWindow.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate :
          Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TProgramInfoWindow.Update;
begin
  ClearFormatParams;
  AddFormatParamStr(label_proginfo_currentmodule);
  AddFormatParamStr(MainFile);
  AddFormatParamStr(label_proginfo_lastexitcode);
  AddFormatParamInt(LastExitCode);
  AddFormatParamStr(label_proginfo_availablememory);
  AddFormatParamInt(MemAvail div 1024);
  InfoST^.SetText(
   FormatStrF(
    {#13+ }
    '%24s : %s'#13+
    '%24s : %d'#13+
    '%24s : %5d'+'K'+#13+
    '',
   FormatParams)
  );
end;

destructor TProgramInfoWindow.Done;
begin
  inherited Done;
  ProgramInfoWindow:=nil;
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

procedure TTab.SelectNextTab(Forwards: boolean);
var Index: integer;
begin
  Index:=ActiveDef;
  if Index=-1 then Exit;
  if Forwards then Inc(Index) else Dec(Index);
  if Index<0 then Index:=DefCount-1 else
  if Index>DefCount-1 then Index:=0;
  SelectTab(Index);
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
            kbCtrlTab :
              begin
                SelectNextTab((Event.KeyShift and kbShift)=0);
                ClearEvent(Event);
              end;
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
var B     : TDrawBuffer;
    i     : integer;
    C1,C2,C3,C : word;
    HeaderLen  : integer;
    X,X2       : integer;
    Name       : PString;
    ActiveKPos : integer;
    ActiveVPos : integer;
    FC   : char;
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
{  if Buffer = nil then
  begin
    GetBuffer;
  end; }
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
  for i:=3 to Size.Y-1 do
    SWriteBuf(0,i,Size.X,1,B);
  { SWriteBuf(0,3,Size.X,Size.Y-4,B); this was wrong
    because WriteBuf then expect a buffer of size size.x*(size.y-4)*2 PM }

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

constructor TScreenView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar;
              AScreen: PScreen);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
  Screen:=AScreen;
  if Screen=nil then
   Fail;
  SetState(sfCursorVis,true);
  Update;
end;

procedure TScreenView.Update;
begin
  SetLimit(UserScreen^.GetWidth,UserScreen^.GetHeight);
  DrawView;
end;

procedure TScreenView.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmUpdate  : Update;
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TScreenView.Draw;
var B: TDrawBuffer;
    X,Y: integer;
    Text,Attr: string;
    P: TPoint;
begin
  Screen^.GetCursorPos(P);
  for Y:=Delta.Y to Delta.Y+Size.Y-1 do
  begin
    if Y<Screen^.GetHeight then
      Screen^.GetLine(Y,Text,Attr)
    else
       begin Text:=''; Attr:=''; end;
    Text:=copy(Text,Delta.X+1,255); Attr:=copy(Attr,Delta.X+1,255);
    MoveChar(B,' ',GetColor(1),Size.X);
    for X:=1 to length(Text) do
      MoveChar(B[X-1],Text[X],ord(Attr[X]),1);
    WriteLine(0,Y-Delta.Y,Size.X,1,B);
  end;
  SetCursor(P.X-Delta.X,P.Y-Delta.Y);
end;

constructor TScreenWindow.Init(AScreen: PScreen; ANumber: integer);
var R: TRect;
    VSB,HSB: PScrollBar;
begin
  Desktop^.GetExtent(R);
  inherited Init(R, dialog_userscreen, ANumber);
  Options:=Options or ofTileAble;
  GetExtent(R); R.Grow(-1,-1); R.Move(1,0); R.A.X:=R.B.X-1;
  New(VSB, Init(R)); VSB^.Options:=VSB^.Options or ofPostProcess;
  VSB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY; Insert(VSB);
  GetExtent(R); R.Grow(-1,-1); R.Move(0,1); R.A.Y:=R.B.Y-1;
  New(HSB, Init(R)); HSB^.Options:=HSB^.Options or ofPostProcess;
  HSB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY; Insert(HSB);
  GetExtent(R); R.Grow(-1,-1);
  New(ScreenView, Init(R, HSB, VSB, AScreen));
  ScreenView^.GrowMode:=gfGrowHiX+gfGrowHiY;
  Insert(ScreenView);
  UserScreenWindow:=@Self;
end;

destructor TScreenWindow.Done;
begin
  inherited Done;
  UserScreenWindow:=nil;
end;

const InTranslate : boolean = false;

procedure TranslateMouseClick(View: PView; var Event: TEvent);
procedure TranslateAction(Action: integer);
var E: TEvent;
begin
  if Action<>acNone then
  begin
    E:=Event;
    E.What:=evMouseDown; E.Buttons:=mbLeftButton;
    View^.HandleEvent(E);
    Event.What:=evCommand;
    Event.Command:=ActionCommands[Action];
  end;
end;
begin
  if InTranslate then Exit;
  InTranslate:=true;
  case Event.What of
    evMouseDown :
      if (GetShiftState and kbAlt)<>0 then
        TranslateAction(AltMouseAction) else
      if (GetShiftState and kbCtrl)<>0 then
        TranslateAction(CtrlMouseAction);
  end;
  InTranslate:=false;
end;

function GetNextEditorBounds(var Bounds: TRect): boolean;
var P: PView;
begin
  P:=Desktop^.Current;
  while P<>nil do
  begin
    if P^.HelpCtx=hcSourceWindow then Break;
    P:=P^.NextView;
    if P=Desktop^.Current then
      begin
        P:=nil;
        break;
      end;
  end;
  if P=nil then Desktop^.GetExtent(Bounds) else
     begin
       P^.GetBounds(Bounds);
       Inc(Bounds.A.X); Inc(Bounds.A.Y);
     end;
  GetNextEditorBounds:=P<>nil;
end;

function IOpenEditorWindow(Bounds: PRect; FileName: string; CurX,CurY: sw_integer; ShowIt: boolean): PSourceWindow;
var R: TRect;
    W: PSourceWindow;
begin
  if Assigned(Bounds) then R.Copy(Bounds^) else
    GetNextEditorBounds(R);
  PushStatus(FormatStrStr(msg_openingsourcefile,SmartPath(FileName)));
  New(W, Init(R, FileName));
  if ShowIt=false then
    W^.Hide;
  if W<>nil then
  begin
    if (CurX<>0) or (CurY<>0) then
       with W^.Editor^ do
       begin
         SetCurPtr(CurX,CurY);
         TrackCursor(true);
       end;
    W^.HelpCtx:=hcSourceWindow;
    Desktop^.Insert(W);
    Message(Application,evBroadcast,cmUpdate,nil);
  end;
  PopStatus;
  IOpenEditorWindow:=W;
end;

function OpenEditorWindow(Bounds: PRect; FileName: string; CurX,CurY: sw_integer): PSourceWindow;
begin
  OpenEditorWindow:=IOpenEditorWindow(Bounds,FileName,CurX,CurY,true);
end;

function SearchOnDesktop(FileName : string;tryexts:boolean) : PSourceWindow;
var
    D,DS : DirStr;
    N,NS : NameStr;
    E,ES : ExtStr;
    SName : string;

function IsSearchedFile(W : PSourceWindow) : boolean;
  var Found: boolean;
  begin
    Found:=false;
    if (W<>nil) and (W^.HelpCtx=hcSourceWindow) then
      begin
        if (D='') then
          SName:=NameAndExtOf(PSourceWindow(W)^.Editor^.FileName)
        else
          SName:=PSourceWindow(W)^.Editor^.FileName;
        FSplit(SName,DS,NS,ES);
        SName:=UpcaseStr(NS+ES);

        if (E<>'') or (not tryexts) then
          begin
            if D<>'' then
              Found:=UpCaseStr(DS)+SName=UpcaseStr(D+N+E)
            else
              Found:=SName=UpcaseStr(N+E);
          end
        else
          begin
            Found:=SName=UpcaseStr(N+'.pp');
            if Found=false then
              Found:=SName=UpcaseStr(N+'.pas');
          end;
      end;
    IsSearchedFile:=found;
  end;
function IsSearchedSource(P: PView) : boolean; {$ifndef FPC}far;{$endif}
begin
  if assigned(P) and
     (TypeOf(P^)=TypeOf(TSourceWindow)) then
       IsSearchedSource:=IsSearchedFile(PSourceWindow(P))
     else
       IsSearchedSource:=false;
end;

begin
  FSplit(FileName,D,N,E);
  SearchOnDesktop:=PSourceWindow(Desktop^.FirstThat(@IsSearchedSource));
end;

function TryToOpenFile(Bounds: PRect; FileName: string; CurX,CurY: sw_integer;tryexts:boolean): PSourceWindow;
begin
  TryToOpenFile:=ITryToOpenFile(Bounds,FileName,CurX,CurY,tryexts,true,false);
end;

function LocateSingleSourceFile(const FileName: string; tryexts: boolean): string;
var D : DirStr;
    N : NameStr;
    E : ExtStr;

  function CheckDir(NewDir: DirStr; NewName: NameStr; NewExt: ExtStr): boolean;
  var OK: boolean;
  begin
    NewDir:=CompleteDir(NewDir);
    OK:=ExistsFile(NewDir+NewName+NewExt);
    if OK then begin D:=NewDir; N:=NewName; E:=NewExt; end;
    CheckDir:=OK;
  end;

  function CheckExt(NewExt: ExtStr): boolean;
  var OK: boolean;
  begin
    OK:=false;
    if D<>'' then OK:=CheckDir(D,N,NewExt) else
      if CheckDir('.'+DirSep,N,NewExt) then OK:=true;
    CheckExt:=OK;
  end;

  function TryToLocateIn(const DD : dirstr): boolean;
  var Found: boolean;
  begin
    D:=CompleteDir(DD);
    Found:=true;
    if (E<>'') or (not tryexts) then
     Found:=CheckExt(E)
    else
     if CheckExt('.pp') then
      Found:=true
    else
     if CheckExt('.pas') then
      Found:=true
    else
     if CheckExt('.inc') then
      Found:=true
    { try also without extension if no other exist }
    else
     if CheckExt('') then
      Found:=true
    else
      Found:=false;
    TryToLocateIn:=Found;
  end;
var Path,DrStr: string;
    Found: boolean;
begin
  FSplit(FileName,D,N,E);
  Found:=CheckDir(D,N,E);
  if not found then
    Found:=TryToLocateIn('.');
  DrStr:=GetSourceDirectories;
  if not Found then
   While pos(ListSeparator,DrStr)>0 do
    Begin
      Found:=TryToLocateIn(Copy(DrStr,1,pos(ListSeparator,DrStr)-1));
      if Found then
        break;
      DrStr:=Copy(DrStr,pos(ListSeparator,DrStr)+1,High(DrStr));
    End;
  if Found then Path:=FExpand(D+N+E) else Path:='';
  LocateSingleSourceFile:=Path;
end;

function LocateSourceFile(const FileName: string; tryexts: boolean): string;
var P: integer;
    FN,S: string;
    FFN: string;
begin
  FN:=FileName;
  repeat
    P:=Pos(ListSeparator,FN); if P=0 then P:=length(FN)+1;
    S:=copy(FN,1,P-1); Delete(FN,1,P);
    FFN:=LocateSingleSourceFile(S,tryexts);
  until (FFN<>'') or (FN='');
  LocateSourceFile:=FFN;
end;

function ITryToOpenFile(Bounds: PRect; FileName: string; CurX,CurY: sw_integer;tryexts:boolean;
         ShowIt,ForceNewWindow: boolean): PSourceWindow;
var
  W : PSourceWindow;
  DrStr: string;
begin
  W:=nil;
  if ForceNewWindow then
    W:=nil
  else
    W:=SearchOnDesktop(FileName,tryexts);
  if W<>nil then
    begin
      NewEditorOpened:=false;
{      if assigned(Bounds) then
        W^.ChangeBounds(Bounds^);}
      W^.Editor^.SetCurPtr(CurX,CurY);
    end
  else
    begin
      DrStr:=LocateSourceFile(FileName,tryexts);
      if DrStr<>'' then
        W:=IOpenEditorWindow(Bounds,DrStr,CurX,CurY,ShowIt);
      NewEditorOpened:=W<>nil;
      if assigned(W) then
        W^.Editor^.SetCurPtr(CurX,CurY);
    end;
  ITryToOpenFile:=W;
end;

function StartEditor(Editor: PCodeEditor; FileName: string): boolean;
var OK: boolean;
    E: PFileEditor;
    R: TRect;
begin
  R.Assign(0,0,0,0);
  New(E, Init(R,nil,nil,nil,nil,FileName));
  OK:=E<>nil;
  if OK then
  begin
    PushStatus(FormatStrStr(msg_readingfileineditor,FileName));
    OK:=E^.LoadFile;
    PopStatus;
   end;
  if OK then
    begin
      Editor^.Lock;
      E^.SelectAll(true);
      Editor^.InsertFrom(E);
      Editor^.SetCurPtr(0,0);
      Editor^.SelectAll(false);
      Editor^.UnLock;
      Dispose(E, Done);
    end;
  StartEditor:=OK;
end;

constructor TTextScroller.Init(var Bounds: TRect; ASpeed: integer; AText: PUnsortedStringCollection);
begin
  inherited Init(Bounds,'');
  EventMask:=EventMask or evIdle;
  Speed:=ASpeed; Lines:=AText;
end;

function TTextScroller.GetLineCount: integer;
var Count: integer;
begin
  if Lines=nil then Count:=0 else
    Count:=Lines^.Count;
  GetLineCount:=Count;
end;

function TTextScroller.GetLine(I: integer): string;
var S: string;
begin
  if I<Lines^.Count then
    S:=GetStr(Lines^.At(I))
  else
    S:='';
  GetLine:=S;
end;

procedure TTextScroller.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evIdle :
      Update;
  end;
  inherited HandleEvent(Event);
end;

procedure TTextScroller.Update;
begin
  if abs(GetDosTicks-LastTT)<Speed then Exit;
  Scroll;
  LastTT:=GetDosTicks;
end;

procedure TTextScroller.Reset;
begin
  TopLine:=0;
  LastTT:=GetDosTicks;
  DrawView;
end;

procedure TTextScroller.Scroll;
begin
  Inc(TopLine);
  if TopLine>=GetLineCount then
    Reset;
  DrawView;
end;

procedure TTextScroller.Draw;
var B: TDrawBuffer;
    C: word;
    Count,Y: integer;
    S: string;
begin
  C:=GetColor(1);
  Count:=GetLineCount;
  for Y:=0 to Size.Y-1 do
    begin
      if Count=0 then S:='' else
        S:=GetLine((TopLine+Y) mod Count);
      if copy(S,1,1)=^C then
        S:=CharStr(' ',Max(0,(Size.X-(length(S)-1)) div 2))+copy(S,2,255);
      MoveChar(B,' ',C,Size.X);
      MoveStr(B,S,C);
      WriteLine(0,Y,Size.X,1,B);
    end;
end;

destructor TTextScroller.Done;
begin
  inherited Done;
  if Lines<>nil then Dispose(Lines, Done);
end;

constructor TFPAboutDialog.Init;
var R,R2: TRect;
    C: PUnsortedStringCollection;
    I: integer;
    OSStr: string;
procedure AddLine(S: string);
begin
  C^.Insert(NewStr(S));
end;
begin
  OSStr:='';
{$ifdef go32v2}
  OSStr:='Dos';
{$endif}
{$ifdef tp}
  OSStr:='Dos';
{$endif}
{$ifdef linux}
  OSStr:='Linux';
{$endif}
{$ifdef win32}
  OSStr:='Win32';
{$endif}
{$ifdef os2}
  OSStr:='OS/2';
{$endif}
{$ifdef FreeBSD}
  OSStr:='FreeBSD';
{$endif}
{$ifdef NetBSD}
  OSStr:='NetBSD';
{$endif}

  R.Assign(0,0,38,14{$ifdef NODEBUG}-1{$endif});
  inherited Init(R, dialog_about);
  HelpCtx:=hcAbout;
  GetExtent(R); R.Grow(-3,-2);
  R2.Copy(R); R2.B.Y:=R2.A.Y+1;
  Insert(New(PStaticText, Init(R2, ^C'FreePascal IDE for '+OSStr)));
  R2.Move(0,1);
  Insert(New(PStaticText, Init(R2, ^C'Version '+VersionStr
    {$ifdef FPC}+' '+{$i %date%}{$endif}
    {$ifdef FVISION}+' FV'{$endif}
    )));
  R2.Move(0,1);
  Insert(New(PStaticText, Init(R2, FormatStrStr2(^C'(%s %s)',label_about_compilerversion,Version_String))));
{$ifndef NODEBUG}
  if pos('Fake',GDBVersion)=0 then
    begin
      R2.Move(0,1);
      Insert(New(PStaticText, Init(R2, FormatStrStr2(^C'(%s %s)',label_about_debugger,GDBVersion))));
      R2.Move(0,1);
    end
  else
{$endif NODEBUG}
    R2.Move(0,2);
  Insert(New(PStaticText, Init(R2, ^C'Copyright (C) 1998-2001 by')));
  R2.Move(0,2);
  Insert(New(PStaticText, Init(R2, ^C'B‚rczi G bor')));
  R2.Move(0,1);
  Insert(New(PStaticText, Init(R2, ^C'Pierre Muller')));
  R2.Move(0,1);
  Insert(New(PStaticText, Init(R2, ^C'and')));
  R2.Move(0,1);
  Insert(New(PStaticText, Init(R2, ^C'Peter Vreman')));
  New(C, Init(50,10));
  for I:=1 to 7 do
  AddLine('');
  AddLine(^C'< Original concept >');
  AddLine(^C'Borland International, Inc.');
  AddLine('');
  AddLine(^C'< Compiler development >');
  AddLine(^C'Carl-Eric Codere');
  AddLine(^C'Daniel Mantione');
  AddLine(^C'Florian Kl„mpfl');
  AddLine(^C'Jonas Maebe');
  AddLine(^C'Mich„el Van Canneyt');
  AddLine(^C'Peter Vreman');
  AddLine(^C'Pierre Muller');
  AddLine('');
  AddLine(^C'< IDE development >');
  AddLine(^C'B‚rczi G bor');
  AddLine(^C'Peter Vreman');
  AddLine(^C'Pierre Muller');
  AddLine('');

  GetExtent(R);
  R.Grow(-1,-1); Inc(R.A.Y,3);
  New(Scroller, Init(R, 10, C));
  Scroller^.Hide;
  Insert(Scroller);
  R.Move(0,-1); R.B.Y:=R.A.Y+1;
  New(TitleST, Init(R, ^C'Team'));
  TitleST^.Hide;
  Insert(TitleST);

  InsertOK(@Self);
end;

procedure TFPAboutDialog.ToggleInfo;
begin
  if Scroller=nil then Exit;
  if Scroller^.GetState(sfVisible) then
    begin
      Scroller^.Hide;
      TitleST^.Hide;
    end
  else
    begin
      Scroller^.Reset;
      Scroller^.Show;
      TitleST^.Show;
    end;
end;

procedure TFPAboutDialog.HandleEvent(var Event: TEvent);
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbAltI : { just like in BP }
          begin
            ToggleInfo;
            ClearEvent(Event);
          end;
      end;
  end;
  inherited HandleEvent(Event);
end;

constructor TFPASCIIChart.Init;
begin
  inherited Init;
  HelpCtx:=hcASCIITableWindow;
  Number:=SearchFreeWindowNo;
  ASCIIChart:=@Self;
end;

procedure TFPASCIIChart.Store(var S: TStream);
begin
  inherited Store(S);
end;

constructor TFPASCIIChart.Load(var S: TStream);
begin
  inherited Load(S);
end;

procedure TFPASCIIChart.HandleEvent(var Event: TEvent);
var W: PSourceWindow;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbEsc :
          begin
            Close;
            ClearEvent(Event);
          end;
      end;
    evCommand :
      case Event.Command of
        cmTransfer :
          begin
            W:=FirstEditorWindow;
            if Assigned(W) and Assigned(Report) then
              Message(W,evCommand,cmAddChar,pointer(ord(Report^.AsciiChar)));
            ClearEvent(Event);
          end;
        cmSearchWindow+1..cmSearchWindow+99 :
          if (Event.Command-cmSearchWindow=Number) then
              ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

destructor TFPASCIIChart.Done;
begin
  ASCIIChart:=nil;
  inherited Done;
end;

function TVideoModeListBox.GetText(Item: pointer; MaxLen: sw_integer): string;
var P: PVideoMode;
    S: string;
begin
  P:=Item;
  S:=IntToStr(P^.Col)+'x'+IntToStr(P^.Row)+' ';
  if P^.Color then
    S:=S+'color'
  else
    S:=S+'mono';
  GetText:=copy(S,1,MaxLen);
end;

constructor TFPDesktop.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
end;

procedure TFPDesktop.InitBackground;
var AV: PANSIBackground;
    FileName: string;
    R: TRect;
begin
  AV:=nil;
  FileName:=LocateFile(BackgroundPath);
  if FileName<>'' then
  begin
    GetExtent(R);
    New(AV, Init(R));
    AV^.GrowMode:=gfGrowHiX+gfGrowHiY;
    if AV^.LoadFile(FileName)=false then
    begin
      Dispose(AV, Done); AV:=nil;
    end;
    if Assigned(AV) then
      Insert(AV);
  end;
  Background:=AV;
  if Assigned(Background)=false then
    inherited InitBackground;
end;

constructor TFPDesktop.Load(var S: TStream);
begin
  inherited Load(S);
end;

procedure TFPDesktop.Store(var S: TStream);
begin
  inherited Store(S);
end;

constructor TFPToolTip.Init(var Bounds: TRect; const AText: string; AAlign: TAlign);
begin
  inherited Init(Bounds);
  SetAlign(AAlign);
  SetText(AText);
end;

procedure TFPToolTip.Draw;
var C: word;
procedure DrawLine(Y: integer; S: string);
var B: TDrawBuffer;
begin
  S:=copy(S,1,Size.X-2);
  case Align of
    alLeft   : S:=' '+S;
    alRight  : S:=LExpand(' '+S,Size.X);
    alCenter : S:=Center(S,Size.X);
  end;
  MoveChar(B,' ',C,Size.X);
  MoveStr(B,S,C);
  WriteLine(0,Y,Size.X,1,B);
end;
var S: string;
    Y: integer;
begin
  C:=GetColor(1);
  S:=GetText;
  for Y:=0 to Size.Y-1 do
    DrawLine(Y,S);
end;

function TFPToolTip.GetText: string;
begin
  GetText:=GetStr(Text);
end;

procedure TFPToolTip.SetText(const AText: string);
begin
  if AText<>GetText then
  begin
    if Assigned(Text) then DisposeStr(Text);
    Text:=NewStr(AText);
    DrawView;
  end;
end;

function TFPToolTip.GetAlign: TAlign;
begin
  GetAlign:=Align;
end;

procedure TFPToolTip.SetAlign(AAlign: TAlign);
begin
  if AAlign<>Align then
  begin
    Align:=AAlign;
    DrawView;
  end;
end;

destructor TFPToolTip.Done;
begin
  if Assigned(Text) then DisposeStr(Text); Text:=nil;
  inherited Done;
end;

function TFPToolTip.GetPalette: PPalette;
const S: string[length(CFPToolTip)] = CFPToolTip;
begin
  GetPalette:=@S;
end;

constructor TFPMemo.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar,AIndicator,nil);
  SetFlags(Flags and not (efPersistentBlocks) or efSyntaxHighlight);
end;

function TFPMemo.GetPalette: PPalette;
const P: string[length(CFPMemo)] = CFPMemo;
begin
  GetPalette:=@P;
end;

function TFPMemo.GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer;
begin
  GetSpecSymbolCount:=0;
end;

function TFPMemo.GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string;
begin
  Abstract;
  GetSpecSymbol:='';
end;

function TFPMemo.IsReservedWord(const S: string): boolean;
begin
  IsReservedWord:=false;
end;

constructor TFPCodeMemo.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar,AIndicator);
end;

function TFPCodeMemo.GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer;
var Count: integer;
begin
  case SpecClass of
    ssCommentPrefix   : Count:=3;
    ssCommentSingleLinePrefix   : Count:=1;
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

function TFPCodeMemo.GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string;
var S: string[20];
begin
  case SpecClass of
    ssCommentPrefix :
      case Index of
        0 : S:='{';
        1 : S:='(*';
        2 : S:='//';
      end;
    ssCommentSingleLinePrefix :
      case Index of
        0 : S:='//';
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

function TFPCodeMemo.IsReservedWord(const S: string): boolean;
begin
  IsReservedWord:=IsFPReservedWord(S);
end;


{$ifdef VESA}
function VESASetVideoModeProc(const VideoMode: TVideoMode; Params: Longint): Boolean; {$ifndef FPC}far;{$endif}
begin
  VESASetVideoModeProc:=VESASetMode(Params);
end;

procedure InitVESAScreenModes;
var ML: TVESAModeList;
    MI: TVESAModeInfoBlock;
    I: integer;
begin
  if VESAInit=false then Exit;
  if VESAGetModeList(ML)=false then Exit;
  for I:=1 to ML.Count do
    begin
      if VESAGetModeInfo(ML.Modes[I],MI) then
      with MI do
{$ifndef DEBUG}
        if (Attributes and vesa_vma_GraphicsMode)=0 then
{$else DEBUG}
        if ((Attributes and vesa_vma_GraphicsMode)=0) or
        { only allow 4 bit i.e. 16 color modes }
          (((Attributes and vesa_vma_CanBeSetInCurrentConfig)<>0) and
           (BitsPerPixel=8)) then
{$endif DEBUG}
          RegisterVesaVideoMode(ML.Modes[I]);
    end;
end;

procedure DoneVESAScreenModes;
begin
  FreeVesaModes;
end;
{$endif}

procedure NoDebugger;
begin
  InformationBox(msg_nodebuggersupportavailable,nil);
end;

procedure RegisterFPViews;
begin
  RegisterType(RSourceEditor);
  RegisterType(RSourceWindow);
  RegisterType(RFPHelpViewer);
  RegisterType(RFPHelpWindow);
  RegisterType(RClipboardWindow);
  RegisterType(RMessageListBox);
  RegisterType(RFPDesktop);
  RegisterType(RGDBSourceEditor);
  RegisterType(RGDBWindow);
  RegisterType(RFPASCIIChart);
  RegisterType(RProgramInfoWindow);
  RegisterType(RFPDlgWindow);
end;


END.
{
  $Log$
  Revision 1.22  2002-06-13 10:54:54  pierre
   * avoid random colors in Screen view

  Revision 1.21  2002/06/06 08:15:29  pierre
   * fix GDBwindow indicator bug

  Revision 1.20  2002/06/01 20:08:42  marco
   * Renamefest

  Revision 1.19  2002/05/31 12:37:10  pierre
   + register asciitable char

  Revision 1.18  2002/05/30 15:02:39  pierre
   * avoid ugly border draw on windows without owners in fvision

  Revision 1.17  2002/05/29 22:38:13  pierre
   Asciitab now in fvision

  Revision 1.16  2002/05/24 21:15:31  pierre
   * add FV suffix in About dialog if using FVision library

  Revision 1.15  2002/04/17 11:10:13  pierre
   * fix last commit for corss compilation fir 1.1 IDE from 1.0.6

  Revision 1.14  2002/04/16 18:12:35  carl
  + compilation problems bugfixes

  Revision 1.13  2002/04/02 11:17:40  pierre
   * Use new SetWidth method for GDB window

  Revision 1.12  2002/01/09 09:48:00  pierre
   try to fix bug 1732

  Revision 1.11  2001/12/19 10:59:18  pierre
   * attempt to fix web bug 1730

  Revision 1.10  2001/11/07 00:28:53  pierre
   + Disassembly window made public

  Revision 1.9  2001/10/11 23:45:28  pierre
   + some preliminary code for graph use

  Revision 1.8  2001/10/11 11:36:30  pierre
   * adapt to new video unit layout

  Revision 1.7  2001/09/27 22:29:12  pierre
   * avoid to give the same core to all new files

  Revision 1.6  2001/09/25 22:46:50  pierre
   highlight i386 movw in asm code correctly

  Revision 1.5  2001/08/29 23:28:20  pierre
   * fix the tab garbage

  Revision 1.4  2001/08/09 23:17:50  pierre
   * keep tabs in Clipboard

  Revision 1.3  2001/08/05 12:23:01  peter
    * Automatically support for fvision or old fv

  Revision 1.2  2001/08/05 02:01:48  peter
    * FVISION define to compile with fvision units

  Revision 1.1  2001/08/04 11:30:24  peter
    * ide works now with both compiler versions

  Revision 1.1.2.34  2001/07/30 20:31:25  pierre
   + support for m68k assembler nstructions

  Revision 1.1.2.33  2001/05/07 23:41:37  carl
  * corrected range check error when initializing asm reserved words

  Revision 1.1.2.32  2001/04/06 20:39:36  pierre
   + open dialog box if TrackSource or GotoSource can't find the file

  Revision 1.1.2.31  2001/04/03 22:01:47  pierre
   * avoid double asking when closing a modified file

  Revision 1.1.2.30  2001/03/20 00:20:42  pierre
   * fix some memory leaks + several small enhancements

  Revision 1.1.2.29  2001/03/13 16:19:15  pierre
   + syntax highligh in disassembly window

  Revision 1.1.2.28  2001/03/13 00:36:44  pierre
   * small DisassemblyWindow fixes

  Revision 1.1.2.27  2001/03/12 17:34:56  pierre
   + Disassembly window started

  Revision 1.1.2.26  2001/03/09 15:01:55  pierre
   + recognize FPU window

  Revision 1.1.2.25  2001/03/08 16:36:34  pierre
   * get version from external compiler if using it

  Revision 1.1.2.24  2001/03/06 22:02:33  pierre
    * fix problems with open file not found correctly if case was different
      in path.
    + PushInfo/PopInfo procedures

  Revision 1.1.2.23  2001/02/19 10:40:50  pierre
   * Check for changed files after Running tool or shell

  Revision 1.1.2.22  2001/01/07 22:38:32  peter
    * fixed source tracking for files not in the current dir
    * start gdbwindow with gdb> prompt

  Revision 1.1.2.21  2000/12/30 22:52:27  peter
    * check modified while in debug mode. But placed it between a
      conditional again as it reports also if the file was already modified
      before the first compile.
    * remove unsaved file checks when compiling without primary file so it
      works the same as with a primary file set.

  Revision 1.1.2.20  2000/11/29 18:28:52  pierre
   + add save to file capability for list boxes

  Revision 1.1.2.19  2000/11/29 11:47:34  pierre
   * fix special window numbering problem

  Revision 1.1.2.18  2000/11/29 11:26:00  pierre
   + TFPDlgWindow that handles cmSearchWindow

  Revision 1.1.2.17  2000/11/29 00:54:45  pierre
   + preserve window number and save special windows

  Revision 1.1.2.16  2000/11/27 12:06:49  pierre
   New bunch of Gabor fixes

  Revision 1.1.2.15  2000/11/23 16:33:32  pierre
   * fix Alt-X problem and set HelpCtx for most dialogs

  Revision 1.1.2.14  2000/11/14 09:08:49  marco
   * First batch IDE renamefest

  Revision 1.1.2.13  2000/11/06 16:55:48  pierre
   * fix failure to recompile when file changed

  Revision 1.1.2.12  2000/10/31 07:54:24  pierre
   enhance GDB Window

  Revision 1.1.2.11  2000/10/26 00:04:36  pierre
   + gdb prompt and FPC_BREAK_ERROR stop

  Revision 1.1.2.10  2000/10/24 00:21:59  pierre
   * fix the greyed save after window list box

  Revision 1.1.2.9  2000/10/20 13:29:29  pierre
   * fix bug 1184, only keyowrd for all mode are highlighted

  Revision 1.1.2.8  2000/10/20 09:55:00  pierre
   * fix GetEditorCurWord if tabs present

  Revision 1.1.2.7  2000/10/18 21:53:27  pierre
   * several Gabor fixes

  Revision 1.1.2.6  2000/08/21 21:23:27  pierre
   * fix loading problem for sources in other dirs

  Revision 1.1.2.5  2000/08/15 03:40:54  peter
   [*] no more fatal exits when the IDE can't find the error file (containing
       the redirected assembler/linker output) after compilation
   [*] hidden windows are now added always at the end of the Window List
   [*] TINIFile parsed entries encapsulated in string delimiters incorrectly
   [*] selection was incorrectly adjusted when typing in overwrite mode
   [*] the line wasn't expanded when it's end was reached in overw. mode
   [*] the IDE now tries to locate source files also in the user specified
       unit dirs (for ex. as a response to 'Open at cursor' (Ctrl+Enter) )
   [*] 'Open at cursor' is now aware of the extension (if specified)

  Revision 1.1.2.4  2000/08/04 14:05:19  michael
  * Fixes from Gabor:
   [*] the IDE now doesn't disable Compile|Make & Build when all windows
       are closed, but there's still a primary file set
       (set bug 1059 to fixed!)

   [*] the IDE didn't read some compiler options correctly back from the
       FP.CFG file, for ex. the linker options. Now it read everything
       correctly, and also automatically handles smartlinking option synch-
       ronization.
       (set bug 1048 to fixed!)

  Revision 1.1.2.3  2000/07/20 11:02:15  michael
  + Fixes from gabor. See fixes.txt

  Revision 1.1.2.2  2000/07/15 21:35:32  pierre
   * Avoid asking twice for Unsaved New File at exit
   * Load files without extensions at startup

  Revision 1.1.2.1  2000/07/15 21:30:06  pierre
  * Wrong commit text

  Revision 1.1  2000/07/13 09:48:36  michael
  + Initial import

  Revision 1.73  2000/06/22 09:07:13  pierre
   * Gabor changes: see fixes.txt

  Revision 1.72  2000/06/16 08:50:42  pierre
   + new bunch of Gabor's changes

  Revision 1.71  2000/05/29 10:44:57  pierre
   + New bunch of Gabor's changes: see fixes.txt

  Revision 1.70  2000/05/16 21:50:53  pierre
   * avoid to typecast the status line to a TWindow

  Revision 1.69  2000/05/02 08:42:29  pierre
   * new set of Gabor changes: see fixes.txt

  Revision 1.68  2000/04/25 08:42:34  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.67  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.66  2000/03/23 22:22:25  pierre
   * file loading problem fixed

  Revision 1.65  2000/03/21 23:25:16  pierre
   adapted to wcedit addition

  Revision 1.64  2000/03/14 13:59:41  pierre
   + add a warning if Changed on loading

  Revision 1.63  2000/03/13 20:39:25  pierre
    * one more try to get the menu update to work correctly
    * breakpoint in red at loading

  Revision 1.62  2000/03/07 21:50:38  pierre
   * UpdateCommands changed again, still not correct :(

  Revision 1.61  2000/03/01 22:32:48  pierre
   * hopfully the bug on wrong Menu config fixed

  Revision 1.60  2000/02/07 23:40:38  pierre
   * avoid closing the StackWindow too early

  Revision 1.59  2000/02/07 10:36:43  michael
  + Something went wrong when unzipping

  Revision 1.58  2000/02/06 23:42:47  pierre
   + Use ErrorLine on GotoSource

  Revision 1.57  2000/02/04 00:03:30  pierre
   + SelectInDebugSession lets CPU and watches in front

  Revision 1.56  2000/02/02 22:51:49  pierre
   * use desktop^.current for GetNextEditorBounds

  Revision 1.55  2000/02/01 10:58:41  pierre
   * avoid Search sometimes disabled for Editor Windows

  Revision 1.54  2000/01/10 14:59:50  pierre
   * TProgramInfo was not registered

  Revision 1.53  2000/01/07 14:02:52  pierre
    + date string added

  Revision 1.52  2000/01/03 11:38:34  michael
  Changes from Gabor

  Revision 1.51  1999/12/20 14:23:17  pierre
    * MyApp renamed IDEApp
    * TDebugController.ResetDebuggerRows added to
      get resetting of debugger rows

  Revision 1.50  1999/12/16 16:55:52  pierre
   * fix of web bug 756

  Revision 1.49  1999/11/25 00:25:43  pierre
   * add Status when loading/saving files

  Revision 1.48  1999/11/22 16:02:12  pierre
   * TryToOpenFile failed tofind a sourcewindow if it has no number

  Revision 1.47  1999/11/18 13:39:24  pierre
   * Better info for Undo debugging

  Revision 1.46  1999/11/10 00:44:12  pierre
   * Grouped Undo action signaled in 'Dump Undo'

  Revision 1.45  1999/10/29 14:50:07  pierre
   * About dialog changes

  Revision 1.44  1999/10/27 12:10:42  pierre
    + With DebugUndo added 3 menu items
      "Dump Undo" "Undo All" and "Redo All"
      for Undo checks

  Revision 1.43  1999/10/25 16:55:13  pierre
   * adapted to a small weditor change

  Revision 1.42  1999/09/16 14:34:59  pierre
    + TBreakpoint and TWatch registering
    + WatchesCollection and BreakpointsCollection stored in desk file
    * Syntax highlighting was broken

  Revision 1.41  1999/09/13 16:24:43  peter
    + clock
    * backspace unident like tp7

  Revision 1.40  1999/09/09 16:30:37  pierre
   * ModuleNames was not created in TMessageListBox.Load

  Revision 1.39  1999/09/03 12:54:07  pierre
    * adapted to modified tokens unit
    * TryToOpen works better

  Revision 1.38  1999/08/31 16:18:33  pierre
   + TGDBWindow.Load and Store + Registration

  Revision 1.37  1999/08/16 18:25:26  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.36  1999/08/03 20:22:39  peter
    + TTab acts now on Ctrl+Tab and Ctrl+Shift+Tab...
    + Desktop saving should work now
       - History saved
       - Clipboard content saved
       - Desktop saved
       - Symbol info saved
    * syntax-highlight bug fixed, which compared special keywords case sensitive
      (for ex. 'asm' caused asm-highlighting, while 'ASM' didn't)
    * with 'whole words only' set, the editor didn't found occourences of the
      searched text, if the text appeared previously in the same line, but didn't
      satisfied the 'whole-word' condition
    * ^QB jumped to (SelStart.X,SelEnd.X) instead of (SelStart.X,SelStart.Y)
      (ie. the beginning of the selection)
    * when started typing in a new line, but not at the start (X=0) of it,
      the editor inserted the text one character more to left as it should...
    * TCodeEditor.HideSelection (Ctrl-K+H) didn't update the screen
    * Shift shouldn't cause so much trouble in TCodeEditor now...
    * Syntax highlight had problems recognizing a special symbol if it was
      prefixed by another symbol character in the source text
    * Auto-save also occours at Dos shell, Tool execution, etc. now...

  Revision 1.35  1999/07/12 13:14:22  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.34  1999/06/30 23:58:20  pierre
    + BreakpointsList Window implemented
      with Edit/New/Delete functions
    + Individual breakpoint dialog with support for all types
      ignorecount and conditions
      (commands are not yet implemented, don't know if this wolud be useful)
      awatch and rwatch have problems because GDB does not annotate them
      I fixed v4.16 for this

  Revision 1.33  1999/06/28 19:32:28  peter
    * fixes from gabor

  Revision 1.32  1999/06/21 23:37:08  pierre
   * VESASetVideoModeProc return value was not set

  Revision 1.31  1999/06/02 11:19:13  pierre
   * @ is now required for FPC for procedure address passing in functions

  Revision 1.30  1999/05/22 13:44:33  peter
    * fixed couple of bugs

  Revision 1.29  1999/04/15 08:58:08  peter
    * syntax highlight fixes
    * browser updates

  Revision 1.28  1999/04/07 21:55:56  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.27  1999/04/01 10:27:06  pierre
   + file(line) in start of message added

  Revision 1.26  1999/03/23 16:16:41  peter
    * linux fixes

  Revision 1.25  1999/03/23 15:11:37  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.24  1999/03/21 22:51:37  florian
    + functional screen mode switching added

  Revision 1.23  1999/03/19 16:04:33  peter
    * new compiler dialog

  Revision 1.22  1999/03/16 00:44:45  peter
    * forgotten in last commit :(

  Revision 1.21  1999/03/08 14:58:16  peter
    + prompt with dialogs for tools

  Revision 1.20  1999/03/01 15:42:08  peter
    + Added dummy entries for functions not yet implemented
    * MenuBar didn't update itself automatically on command-set changes
    * Fixed Debugging/Profiling options dialog
    * TCodeEditor converts spaces to tabs at save only if efUseTabChars is set
    * efBackSpaceUnindents works correctly
    + 'Messages' window implemented
    + Added '$CAP MSG()' and '$CAP EDIT' to available tool-macros
    + Added TP message-filter support (for ex. you can call GREP thru
      GREP2MSG and view the result in the messages window - just like in TP)
    * A 'var' was missing from the param-list of THelpFacility.TopicSearch,
      so topic search didn't work...
    * In FPHELP.PAS there were still context-variables defined as word instead
      of THelpCtx
    * StdStatusKeys() was missing from the statusdef for help windows
    + Topic-title for index-table can be specified when adding a HTML-files

  Revision 1.19  1999/02/22 11:51:39  peter
    * browser updates from gabor

  Revision 1.18  1999/02/22 11:29:38  pierre
    + added col info in MessageItem
    + grep uses HighLightExts and should work for linux

  Revision 1.17  1999/02/22 02:15:22  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.16  1999/02/19 18:43:49  peter
    + open dialog supports mask list

  Revision 1.15  1999/02/17 15:04:02  pierre
   + file(line) added in TProgramInfo message list

  Revision 1.14  1999/02/16 12:45:18  pierre
   * GDBWindow size and grow corrected

  Revision 1.13  1999/02/15 09:36:06  pierre
    * // comment ends at end of line !
      GDB window changed !
      now all is in a normal text editor, but pressing
      Enter key will send part of line before cursor to GDB !

  Revision 1.12  1999/02/11 19:07:25  pierre
    * GDBWindow redesigned :
      normal editor apart from
      that any kbEnter will send the line (for begin to cursor)
      to GDB command !
      GDBWindow opened in Debugger Menu
       still buggy :
       -echo should not be present if at end of text
       -GDBWindow becomes First after each step (I don't know why !)

  Revision 1.11  1999/02/11 13:08:39  pierre
   + TGDBWindow : direct gdb input/output

  Revision 1.10  1999/02/10 09:42:52  pierre
    + DoneReservedWords to avoid memory leaks
    * TMessageItem Module field was not disposed

  Revision 1.9  1999/02/05 12:12:02  pierre
    + SourceDir that stores directories for sources that the
      compiler should not know about
      Automatically asked for addition when a new file that
      needed filedialog to be found is in an unknown directory
      Stored and retrieved from INIFile
    + Breakpoints conditions added to INIFile
    * Breakpoints insterted and removed at debin and end of debug session

  Revision 1.8  1999/02/04 17:45:23  pierre
    + BrowserAtCursor started
    * bug in TryToOpenFile removed

  Revision 1.7  1999/02/04 13:32:11  pierre
    * Several things added (I cannot commit them independently !)
    + added TBreakpoint and TBreakpointCollection
    + added cmResetDebugger,cmGrep,CmToggleBreakpoint
    + Breakpoint list in INIFile
    * Select items now also depend of SwitchMode
    * Reading of option '-g' was not possible !
    + added search for -Fu args pathes in TryToOpen
    + added code for automatic opening of FileDialog
      if source not found

  Revision 1.6  1999/01/21 11:54:27  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.5  1999/01/14 21:42:25  peter
    * source tracking from Gabor

  Revision 1.4  1999/01/12 14:29:42  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.3  1999/01/04 11:49:53  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:54  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.4  1998/12/22 10:39:53  peter
    + options are now written/read
    + find and replace routines

}
