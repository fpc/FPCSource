{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Code editor template objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$I globdir.inc}
unit WEditor;

interface

uses
  Dos,Objects,Drivers,Views,Menus,Commands,
  WUtils;


{ try to only do syntax on part of file until current position
  does not work correctly yet PM }
{.$define TEST_PARTIAL_SYNTAX}

const
      cmFileNameChanged      = 51234;
      cmASCIIChar            = 51235;
      cmClearLineHighlights  = 51236;
      cmSaveCancelled        = 51237;
      cmBreakLine            = 51238;
      cmSelStart             = 51239;
      cmSelEnd               = 51240;
      cmLastCursorPos        = 51241;
      cmIndentBlock          = 51242;
      cmUnIndentBlock        = 51243;
      cmSelectLine           = 51244;
      cmWriteBlock           = 51245;
      cmReadBlock            = 51246;
      cmPrintBlock           = 51247;
      cmResetDebuggerRow     = 51248;
      cmAddChar              = 51249;
      cmExpandCodeTemplate   = 51250;

      EditorTextBufSize = {$ifdef FPC}32768{$else} 4096{$endif};
      MaxLineLength     = {$ifdef FPC}  255{$else}  255{$endif};
      MaxLineCount      = {$ifdef FPC}16380{$else}16380{$endif};

      CodeCompleteMinLen = 4; { minimum length of text to try to complete }

      efBackupFiles         = $00000001;
      efInsertMode          = $00000002;
      efAutoIndent          = $00000004;
      efUseTabCharacters    = $00000008;
      efBackSpaceUnindents  = $00000010;
      efPersistentBlocks    = $00000020;
      efSyntaxHighlight     = $00000040;
      efBlockInsCursor      = $00000080;
      efVerticalBlocks      = $00000100;
      efHighlightColumn     = $00000200;
      efHighlightRow        = $00000400;
      efAutoBrackets        = $00000800;
      efExpandAllTabs       = $00001000;
      efKeepTrailingSpaces  = $00002000;
      efCodeComplete        = $00004000;
      efStoreContent        = $80000000;

      attrAsm       = 1;
      attrComment   = 2;
      attrForceFull = 128;
      attrAll       = attrAsm+attrComment;

      edOutOfMemory   = 0;
      edReadError     = 1;
      edWriteError    = 2;
      edCreateError   = 3;
      edSaveModify    = 4;
      edSaveUntitled  = 5;
      edSaveAs        = 6;
      edFind          = 7;
      edSearchFailed  = 8;
      edReplace       = 9;
      edReplacePrompt = 10;
      edTooManyLines  = 11;
      edGotoLine      = 12;
      edReplaceFile   = 13;
      edWriteBlock    = 14;
      edReadBlock     = 15;
      edFileOnDiskChanged = 16;

      ffmOptions      = $0007; ffsOptions     = 0;
      ffmDirection    = $0008; ffsDirection   = 3;
      ffmScope        = $0010; ffsScope       = 4;
      ffmOrigin       = $0020; ffsOrigin      = 5;
      ffDoReplace     = $0040;
      ffReplaceAll    = $0080;


      ffCaseSensitive    = $0001;
      ffWholeWordsOnly   = $0002;
      ffPromptOnReplace  = $0004;

      ffForward          = $0000;
      ffBackward         = $0008;

      ffGlobal           = $0000;
      ffSelectedText     = $0010;

      ffFromCursor       = $0000;
      ffEntireScope      = $0020;

      coTextColor         = 0;
      coWhiteSpaceColor   = 1;
      coCommentColor      = 2;
      coReservedWordColor = 3;
      coIdentifierColor   = 4;
      coStringColor       = 5;
      coNumberColor       = 6;
      coAssemblerColor    = 7;
      coSymbolColor       = 8;
      coDirectiveColor    = 9;
      coHexNumberColor    = 10;
      coTabColor          = 11;
      coBreakColor        = 12;
      coFirstColor        = 0;
      coLastColor         = coBreakColor;

      eaMoveCursor        = 1;
      eaInsertLine        = 2;
      eaInsertText        = 3;
      eaDeleteLine        = 4;
      eaDeleteText        = 5;
      eaSelectionChanged  = 6;
      eaCut               = 7;
      eaPaste             = 8;
      eaPasteWin          = 9;
      eaClear             = 10;
      LastAction          = eaClear;

      ActionString : array [0..LastAction] of string[8] =
        ('','Move','InsLine','InsText','DelLine','DelText',
         'SelCh','Cut','Paste','PasteWin','Clear');

      CIndicator    = #2#3#1;
      CEditor       = #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#48#49;

      TAB      = #9;
      FindStrSize = 79;

type
    PLine = ^TLine;
    TLine = record
      Text   : PString;
      Format : PString;
      BeginsWithAsm,
      EndsWithAsm   : boolean;
      IsBreakpoint  : boolean;
      BeginsWithComment,
      EndsInSingleLineComment,
      EndsWithComment : boolean;
      BeginsWithDirective,
      EndsWithDirective : boolean;
      {BeginCommentType,}EndCommentType : byte;
    end;

    PLineCollection = ^TLineCollection;
    TLineCollection = object(TCollection)
      function  At(Index: sw_Integer): PLine;
      procedure FreeItem(Item: Pointer); virtual;
    end;

    PIndicator = ^TIndicator;
    TIndicator = object(TView)
      Location: TPoint;
      Modified : Boolean;
{$ifdef debug}
      StoreUndo : Boolean;
      SyntaxComplete : boolean;
      UseTabs : Boolean;
{$endif debug}
      constructor Init(var Bounds: TRect);
      procedure   Draw; virtual;
      function    GetPalette: PPalette; virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   SetValue(ALocation: TPoint; AModified: Boolean);
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
    end;

{$ifdef Undo}
    PEditorAction = ^TEditorAction;
    TEditorAction = object(TObject)
      StartPos  : TPoint;
      EndPos    : TPoint;
      Text      : PString;
      ActionCount : longint;
      Action    : byte;
      TimeStamp : longint; { this is needed to keep track of line number &
                             position changes (for ex. for symbol browser)
                             the line&pos references (eg. symbol info) should
                             also contain such a timestamp. this will enable
                             to determine which changes have been made since
                             storage of the information and thus calculate
                             the (probably) changed line & position information,
                             so, we can still jump to the right position in the
                             editor even when it is heavily modified - Gabor }
      constructor init(act:byte; StartP,EndP:TPoint;Txt:String);
      constructor init_group(act:byte);
      function is_grouped_action : boolean;
      destructor done; virtual;
    end;

    PEditorActionCollection = ^TEditorActionCollection;
    TEditorActionCollection = object(TCollection)
      CurrentGroupedAction : PEditorAction;
      function At(Idx : sw_integer) : PEditorAction;
    end;
{$else}
    PEditorAction = ^TEditorAction;
    TEditorAction = packed record
      StartPos  : TPoint;
      EndPos    : TPoint;
      Text      : PString;
      ActionCount : longint;
      Action    : byte;
      TimeStamp : longint; { see above! }
    end;

    PEditorActionCollection = ^TEditorActionCollection;
    TEditorActionCollection = object(TCollection)
      function At(Idx : sw_integer) : PEditorAction;
      procedure FreeItem(Item: Pointer); virtual;
    end;
{$endif Undo}

    TSpecSymbolClass =
      (ssCommentPrefix,ssCommentSingleLinePrefix,ssCommentSuffix,ssStringPrefix,ssStringSuffix,
       ssDirectivePrefix,ssDirectiveSuffix,ssAsmPrefix,ssAsmSuffix);

    TEditorBookMark = record
      Valid  : boolean;
      Pos    : TPoint;
    end;

    TCompleteState = (csInactive,csOffering,csDenied);

    PCodeEditor = ^TCodeEditor;
    TCodeEditor = object(TScroller)
      Indicator  : PIndicator;
      Lines      : PLineCollection;
      SelStart   : TPoint;
      SelEnd     : TPoint;
      Highlight  : TRect;
      CurPos     : TPoint;
      CanUndo    : Boolean;
      StoreUndo  : boolean;
      Modified   : Boolean;
      IsReadOnly : Boolean;
      NoSelect   : Boolean;
      Flags      : longint;
      TabSize    : integer;
      HighlightRow: sw_integer;
      DebuggerRow: sw_integer;
      UndoList    : PEditorActionCollection;
      RedoList    : PEditorActionCollection;
      CompleteState: TCompleteState;
      CodeCompleteFrag: PString;
      CodeCompleteWord: PString;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator; AbufSize:Sw_Word);
      procedure   SetFlags(AFlags: longint); virtual;
      procedure   ConvertEvent(var Event: TEvent); virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   LocalMenu(P: TPoint); virtual;
      function    GetLocalMenu: PMenu; virtual;
      function    GetCommandTarget: PView; virtual;
      function    CreateLocalMenuView(var Bounds: TRect; M: PMenu): PMenuPopup; virtual;
      procedure   Draw; virtual;
      procedure   DrawCursor; virtual;
      procedure   TrackCursor(Center: boolean); virtual;
      procedure   UpdateIndicator; virtual;
      procedure   LimitsChanged; virtual;
      procedure   SelectionChanged; virtual;
      procedure   HighlightChanged; virtual;
      procedure   ModifiedChanged; virtual;
      procedure   Update; virtual;
      procedure   ScrollTo(X, Y: sw_Integer);
      procedure   SetModified(AModified: boolean); virtual;
      procedure   SetInsertMode(InsertMode: boolean); virtual;
      procedure   SetCurPtr(X,Y: sw_integer); virtual;
      procedure   SetSelection(A, B: TPoint); virtual;
      procedure   SetHighlight(A, B: TPoint); virtual;
      procedure   SetHighlightRow(Row: sw_integer); virtual;
      procedure   SetDebuggerRow(Row: sw_integer); virtual;
      procedure   SetCompleteState(AState: TCompleteState); virtual;
      function    GetCodeCompleteFrag: string;
      procedure   SetCodeCompleteFrag(const S: string);
      procedure   SelectAll(Enable: boolean); virtual;
      function    InsertFrom(Editor: PCodeEditor): Boolean; virtual;
      function    InsertText(const S: string): Boolean; virtual;
      function    GetPalette: PPalette; virtual;
      function    IsClipboard: Boolean;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      function    LoadFromStream(Stream: PStream): boolean; virtual;
      function    SaveToStream(Stream: PStream): boolean; virtual;
      function    SaveAreaToStream(Stream: PStream; StartP,EndP: TPoint): boolean;
      destructor  Done; virtual;
    public
      { Text & info storage abstraction }
      function    GetLineCount: sw_integer; virtual;
      function    CharIdxToLinePos(Line,CharIdx: sw_integer): sw_integer;
      function    LinePosToCharIdx(Line,X: sw_integer): sw_integer;
      function    GetLineText(I: sw_integer): string; virtual;
      procedure   SetDisplayText(I: sw_integer;const S: string); virtual;
      function    GetDisplayText(I: sw_integer): string; virtual;
      procedure   SetLineText(I: sw_integer;const S: string); virtual;
      procedure   SetLineBreakState(I : sw_integer;b : boolean);
      procedure   GetDisplayTextFormat(I: sw_integer;var DT,DF:string); virtual;
      function    GetLineFormat(I: sw_integer): string; virtual;
      procedure   SetLineFormat(I: sw_integer;const S: string); virtual;
      procedure   DeleteAllLines; virtual;
      procedure   DeleteLine(I: sw_integer); virtual;
      procedure   AddLine(const S: string); virtual;
      function    GetErrorMessage: string; virtual;
      procedure   SetErrorMessage(const S: string); virtual;
      procedure   AdjustSelection(DeltaX, DeltaY: sw_integer);
      procedure   AdjustSelectionPos(CurPosX, CurPosY: sw_integer; DeltaX, DeltaY: sw_integer);
      procedure   Lock;
      procedure   UnLock;
    private
      LastLocalCmd: word;
      KeyState    : Integer;
{$ifdef TEST_PARTIAL_SYNTAX}
      LastSyntaxedLine : sw_integer;
      SyntaxComplete : boolean;
{$endif TEST_PARTIAL_SYNTAX}
      ErrorMessage: PString;
      Bookmarks   : array[0..9] of TEditorBookmark;
      LockFlag    : integer;
      DrawCalled,
      DrawCursorCalled,
      IndicatorDrawCalled  : boolean;
      CurEvent    : PEvent;
      function    Overwrite: boolean;
      function    GetLine(I: sw_integer): PLine;
      procedure   CheckSels;
      procedure   CodeCompleteCheck;
      procedure   CodeCompleteApply;
      procedure   CodeCompleteCancel;
      procedure   UpdateUndoRedo(cm : word; action : byte);
      function    UpdateAttrs(FromLine: sw_integer; Attrs: byte): sw_integer;
      function    UpdateAttrsRange(FromLine, ToLine: sw_integer; Attrs: byte): sw_integer;
      procedure   DrawLines(FirstLine: sw_integer);
      procedure   HideHighlight;
      procedure   AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string);
      procedure   AddGroupedAction(AAction : byte);
      procedure   CloseGroupedAction(AAction : byte);
      function    ShouldExtend: boolean;
      function    ValidBlock: boolean;
    public
     { Syntax highlight support }
      function    GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer; virtual;
      function    GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string; virtual;
      function    IsReservedWord(const S: string): boolean; virtual;
     { CodeTemplate support }
      function    TranslateCodeTemplate(const Shortcut: string; ALines: PUnsortedStringCollection): boolean; virtual;
     { CodeComplete support }
      function    CompleteCodeWord(const WordS: string; var Text: string): boolean; virtual;
      function    GetCodeCompleteWord: string;
      procedure   SetCodeCompleteWord(const S: string); virtual;
      procedure   ClearCodeCompleteWord; virtual;
    public
      SearchRunCount: integer;
      InASCIIMode: boolean;
      procedure Indent; virtual;
      procedure CharLeft; virtual;
      procedure CharRight; virtual;
      procedure WordLeft; virtual;
      procedure WordRight; virtual;
      procedure LineStart; virtual;
      procedure LineEnd; virtual;
      procedure LineUp; virtual;
      procedure LineDown; virtual;
      procedure PageUp; virtual;
      procedure PageDown; virtual;
      procedure TextStart; virtual;
      procedure TextEnd; virtual;
      procedure JumpSelStart; virtual;
      procedure JumpSelEnd; virtual;
      procedure JumpMark(MarkIdx: integer); virtual;
      procedure DefineMark(MarkIdx: integer); virtual;
      procedure JumpToLastCursorPos; virtual;
      function  InsertLine: Sw_integer; virtual;
      procedure BreakLine; virtual;
      procedure BackSpace; virtual;
      procedure DelChar; virtual;
      procedure DelWord; virtual;
      procedure DelStart; virtual;
      procedure DelEnd; virtual;
      procedure DelLine; virtual;
      procedure InsMode; virtual;
      procedure StartSelect; virtual;
      procedure EndSelect; virtual;
      procedure DelSelect; virtual;
      procedure HideSelect; virtual;
      procedure CopyBlock; virtual;
      procedure MoveBlock; virtual;
      procedure IndentBlock; virtual;
      procedure UnindentBlock; virtual;
      procedure SelectWord; virtual;
      procedure SelectLine; virtual;
      procedure WriteBlock; virtual;
      procedure ReadBlock; virtual;
      procedure PrintBlock; virtual;
      procedure ExpandCodeTemplate; virtual;
      procedure AddChar(C: char); virtual;
{$ifdef WinClipSupported}
      function  ClipCopyWin: Boolean; virtual;
      function  ClipPasteWin: Boolean; virtual;
{$endif WinClipSupported}
      function  ClipCopy: Boolean; virtual;
      procedure ClipCut; virtual;
      procedure ClipPaste; virtual;
      function  GetCurrentWord : string;
      procedure Undo; virtual;
      procedure Redo; virtual;
      procedure Find; virtual;
      procedure Replace; virtual;
      procedure DoSearchReplace; virtual;
      procedure GotoLine; virtual;
    end;

    PFileEditor = ^TFileEditor;
    TFileEditor = object(TCodeEditor)
      FileName: string;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator;const AFileName: string);
      function    Save: Boolean; virtual;
      function    SaveAs: Boolean; virtual;
      function    SaveAsk: Boolean; virtual;
      function    LoadFile: boolean; virtual;
      function    SaveFile: boolean; virtual;
      function    Valid(Command: Word): Boolean; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    ShouldSave: boolean; virtual;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      function    IsChangedOnDisk : boolean;
      private
        OnDiskLoadTime : longint;
    end;

    TCodeEditorDialog = function(Dialog: Integer; Info: Pointer): Word;

function DefUseSyntaxHighlight(Editor: PFileEditor): boolean;
function DefUseTabsPattern(Editor: PFileEditor): boolean;

const
     DefaultCodeEditorFlags : longint =
       efBackupFiles+efInsertMode+efAutoIndent+efPersistentBlocks+
       {efUseTabCharacters+}efBackSpaceUnindents+efSyntaxHighlight+
       efExpandAllTabs+efCodeComplete;
     DefaultTabSize     : integer = 8;
     EOL : String[2] = {$ifdef Linux}#10;{$else}#13#10;{$endif}

     { used for ShiftDel and ShiftIns to avoid
       GetShiftState to be considered for extending
       selection (PM) }

     cmCopyWin = 240;
     cmPasteWin = 241;

     { History ID }
     FileId        = 101;
     TextFindId    = 105;
     TextReplaceID = 106;
     GotoID        = 107;
     TextGrepId    = 108;

     DontConsiderShiftState: boolean  = false;
     ToClipCmds         : TCommandSet = ([cmCut,cmCopy,cmCopyWin]);
     FromClipCmds       : TCommandSet = ([cmPaste]);
     FromWinClipCmds    : TCommandSet = ([cmPasteWin]);
     NulClipCmds        : TCommandSet = ([cmClear]);
     UndoCmd            : TCommandSet = ([cmUndo]);
     RedoCmd            : TCommandSet = ([cmRedo]);

function StdEditorDialog(Dialog: Integer; Info: Pointer): word;

const
     EditorDialog       : TCodeEditorDialog = StdEditorDialog;
     Clipboard          : PCodeEditor = nil;
     FindStr            : String[FindStrSize] = '';
     ReplaceStr         : String[FindStrSize] = '';
     FindFlags          : word = ffPromptOnReplace;
     WhiteSpaceChars    : set of char = [#0,#32,#255];
     TabChars           : set of char = [#9];
     HashChars          : set of char = ['#'];
     AlphaChars         : set of char = ['A'..'Z','a'..'z','_'];
     NumberChars        : set of char = ['0'..'9'];
     RealNumberChars    : set of char = ['E','e','.'{,'+','-'}];
     DefaultSaveExt     : string[12] = '.pas';
     FileDir            : DirStr = '';

     UseSyntaxHighlight : function(Editor: PFileEditor): boolean = DefUseSyntaxHighlight;
     UseTabsPattern     : function(Editor: PFileEditor): boolean = DefUseTabsPattern;

procedure RegisterCodeEditors;

implementation

uses
  MsgBox,Dialogs,App,StdDlg,HistList,Validate,
{$ifdef WinClipSupported}
  Strings,WinClip,
{$endif WinClipSupported}
  WViews;

{$ifndef NOOBJREG}
const
  RIndicator: TStreamRec = (
     ObjType: 1100;
     VmtLink: Ofs(TypeOf(TIndicator)^);
     Load:    @TIndicator.Load;
     Store:   @TIndicator.Store
  );
  RCodeEditor: TStreamRec = (
     ObjType: 1101;
     VmtLink: Ofs(TypeOf(TCodeEditor)^);
     Load:    @TCodeEditor.Load;
     Store:   @TCodeEditor.Store
  );
  RFileEditor: TStreamRec = (
     ObjType: 1102;
     VmtLink: Ofs(TypeOf(TFileEditor)^);
     Load:    @TFileEditor.Load;
     Store:   @TFileEditor.Store
  );
{$endif}

type
     TFindDialogRec = packed record
       Find     : String[FindStrSize];
       Options  : Word{longint};
       { checkboxes need 32  bits PM  }
       { reverted to word in dialogs.TCluster for TP compatibility (PM) }
       { anyhow its complete nonsense : you can only have 16 fields
         but use a longint to store it !! }
       Direction: word;{ and tcluster has word size }
       Scope    : word;
       Origin   : word;
     end;

     TReplaceDialogRec = packed record
       Find     : String[FindStrSize];
       Replace  : String[FindStrSize];
       Options  : Word{longint};
       Direction: word;
       Scope    : word;
       Origin   : word;
     end;

     TGotoLineDialogRec = packed record
       LineNo  : string[5];
       Lines   : sw_integer;
     end;

const
     kbShift = kbLeftShift+kbRightShift;

const
  FirstKeyCount = 40;
  FirstKeys: array[0..FirstKeyCount * 2] of Word = (FirstKeyCount,
    Ord(^A), cmWordLeft, Ord(^B), cmJumpLine, Ord(^C), cmPageDown,
    Ord(^D), cmCharRight, Ord(^E), cmLineUp,
    Ord(^F), cmWordRight, Ord(^G), cmDelChar,
    Ord(^H), cmBackSpace, Ord(^J), cmExpandCodeTemplate,
    Ord(^K), $FF02, Ord(^L), cmSearchAgain,
    Ord(^M), cmNewLine, Ord(^N), cmBreakLine,
    Ord(^P), cmASCIIChar, Ord(^Q), $FF01,
    Ord(^R), cmPageUp, Ord(^S), cmCharLeft,
    Ord(^T), cmDelWord, Ord(^U), cmUndo,
    Ord(^V), cmInsMode, Ord(^X), cmLineDown,
    Ord(^Y), cmDelLine, kbLeft, cmCharLeft,
    kbRight, cmCharRight, kbCtrlLeft, cmWordLeft,
    kbCtrlRight, cmWordRight, kbHome, cmLineStart,
    kbEnd, cmLineEnd, kbUp, cmLineUp,
    kbDown, cmLineDown, kbPgUp, cmPageUp,
    kbPgDn, cmPageDown, kbCtrlPgUp, cmTextStart,
    kbCtrlPgDn, cmTextEnd, kbIns, cmInsMode,
    kbDel, cmDelChar, kbShiftIns, cmPaste,
    kbShiftDel, cmCut, kbCtrlIns, cmCopy,
    kbCtrlDel, cmClear);
  QuickKeyCount = 23;
  QuickKeys: array[0..QuickKeyCount * 2] of Word = (QuickKeyCount,
    Ord('A'), cmReplace, Ord('C'), cmTextEnd,
    Ord('D'), cmLineEnd, Ord('F'), cmFind,
    Ord('H'), cmDelStart, Ord('R'), cmTextStart,
    Ord('S'), cmLineStart, Ord('Y'), cmDelEnd,
    Ord('G'), cmJumpLine, Ord('A'), cmReplace,
    Ord('B'), cmSelStart, Ord('K'), cmSelEnd,
    Ord('P'), cmLastCursorPos,
    Ord('0'), cmJumpMark0, Ord('1'), cmJumpMark1, Ord('2'), cmJumpMark2,
    Ord('3'), cmJumpMark3, Ord('4'), cmJumpMark4, Ord('5'), cmJumpMark5,
    Ord('6'), cmJumpMark6, Ord('7'), cmJumpMark7, Ord('8'), cmJumpMark8,
    Ord('9'), cmJumpMark9);
  BlockKeyCount = 23;
  BlockKeys: array[0..BlockKeyCount * 2] of Word = (BlockKeyCount,
    Ord('B'), cmStartSelect, Ord('C'), cmCopyBlock,
    Ord('H'), cmHideSelect, Ord('K'), cmEndSelect,
    Ord('Y'), cmDelSelect, Ord('V'), cmMoveBlock,
    Ord('I'), cmIndentBlock, Ord('U'), cmUnindentBlock,
    Ord('T'), cmSelectWord, Ord('L'), cmSelectLine,
    Ord('W'), cmWriteBlock, Ord('R'), cmReadBlock,
    Ord('P'), cmPrintBlock,
    Ord('0'), cmSetMark0, Ord('1'), cmSetMark1, Ord('2'), cmSetMark2,
    Ord('3'), cmSetMark3, Ord('4'), cmSetMark4, Ord('5'), cmSetMark5,
    Ord('6'), cmSetMark6, Ord('7'), cmSetMark7, Ord('8'), cmSetMark8,
    Ord('9'), cmSetMark9);
  KeyMap: array[0..2] of Pointer = (@FirstKeys, @QuickKeys, @BlockKeys);

function ScanKeyMap(KeyMap: Pointer; KeyCode: Word): Word;
type
  pword = ^word;
var
  p : pword;
  count : sw_word;
begin
  p:=keymap;
  count:=p^;
  inc(p);
  while (count>0) do
   begin
     if (lo(p^)=lo(keycode)) and
        ((hi(p^)=0) or (hi(p^)=hi(keycode))) then
      begin
        inc(p);
        scankeymap:=p^;
        Exit;
      end;
     inc(p,2);
     dec(count);
   end;
  scankeymap:=0;
end;

function IsWordSeparator(C: char): boolean;
begin
  IsWordSeparator:=C in[' ',#0,#255,':','=','''','"','.',',','/',';','$','#','(',')','<','>','^','*','+','-','?','&'];
end;

function IsSpace(C: char): boolean;
begin
  IsSpace:=C in[' ',#0,#255];
end;

function LTrim(S: string): string;
begin
  while (length(S)>0) and (S[1] in [#0,TAB,#32]) do
    Delete(S,1,1);
  LTrim:=S;
end;

function RTrim(S: string): string;
begin
  while (length(S)>0) and (S[length(S)] in [#0,TAB,#32]) do
    Delete(S,length(S),1);
  RTrim:=S;
end;

function Trim(S: string): string;
begin
  Trim:=RTrim(LTrim(S));
end;

function EatIO: integer;
begin
  EatIO:=IOResult;
end;

function ExistsFile(const FileName: string): boolean;
var f: file;
    Exists: boolean;
begin
  if FileName='' then Exists:=false else
 begin
  {$I-}
  Assign(f,FileName);
  Reset(f,1);
  Exists:=EatIO=0;
  Close(f);
  EatIO;
  {$I+}
 end;
  ExistsFile:=Exists;
end;

function Max(A,B: longint): longint;
begin
  if A>B then Max:=A else Max:=B;
end;

function Min(A,B: longint): longint;
begin
  if A<B then Min:=A else Min:=B;
end;

function StrToInt(const S: string): longint;
var L: longint;
    C: integer;
begin
  Val(S,L,C); if C<>0 then L:=-1;
  StrToInt:=L;
end;

function RExpand(const S: string; MinLen: byte): string;
begin
  if length(S)<MinLen then
   RExpand:=S+CharStr(' ',MinLen-length(S))
  else
   RExpand:=S;
end;

function upper(const s : string) : string;
var
  i  : Sw_word;
begin
  for i:=1 to length(s) do
   if s[i] in ['a'..'z'] then
    upper[i]:=char(byte(s[i])-32)
   else
    upper[i]:=s[i];
  upper[0]:=s[0];
end;

function DirAndNameOf(const Path: string): string;
var D: DirStr; N: NameStr; E: ExtStr;
begin
  FSplit(Path,D,N,E);
  DirAndNameOf:=D+N;
end;

type TPosOfs = {$ifdef TP}longint{$endif}{$ifdef FPC}comp{$endif};

function PosToOfs(const X,Y: sw_integer): TPosOfs;
type TPosRec = record LoI, HiI: sw_integer; end;
var C: TPosRec;
begin
  C.LoI:=X; C.HiI:=Y;
  PosToOfs:=TPosOfs(C);
end;

function PosToOfsP(const P: TPoint): TPosOfs;
begin
  PosToOfsP:=PosToOfs(P.X,P.Y);
end;

function PointOfs(P: TPoint): TPosOfs;
begin
  PointOfs:={longint(P.Y)*MaxLineLength+P.X}PosToOfsP(P);
end;

{$ifndef Undo}
function NewEditorAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string): PEditorAction;
var P: PEditorAction;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  with P^ do
  begin
    Action:=AAction;
    StartPos:=AStartPos; EndPos:=AEndPos;
    Text:=NewStr(AText);
  end;
  NewEditorAction:=P;
end;

procedure DisposeEditorAction(P: PEditorAction);
begin
  if P<>nil then
  begin
    if P^.Text<>nil then DisposeStr(P^.Text); P^.Text:=nil;
    Dispose(P);
  end;
end;
{$endif ndef Undo}

function ExtractTabs(S: string; TabSize: Sw_integer): string;
var
  P,PAdd: Sw_Word;
begin
  p:=0;
  while p<length(s) do
   begin
     inc(p);
     if s[p]=#9 then
      begin
        PAdd:=TabSize-((p-1) mod TabSize);
        s:=copy(S,1,P-1)+CharStr(' ',PAdd)+copy(S,P+1,255);
        inc(P,PAdd-1);
      end;
   end;
  ExtractTabs:=S;
end;

function CompressUsingTabs(S: string; TabSize: byte): string;
var TabS: string;
    P: byte;
begin
  TabS:=CharStr(' ',TabSize);
  repeat
    P:=Pos(TabS,S);
    if P>0 then
      S:=copy(S,1,P-1)+TAB+copy(S,P+TabSize,255);
  until P=0;
  CompressUsingTabs:=S;
end;


{*****************************************************************************
                           Forward/Backward Scanning
*****************************************************************************}

Const
{$ifndef FPC}
  MaxBufLength   = $7f00;
  NotFoundValue  = -1;
{$else}
  MaxBufLength   = $7fffff00;
  NotFoundValue  = -1;
{$endif}

Type
  Btable = Array[0..255] of Byte;
Procedure BMFMakeTable(const s:string; Var t : Btable);
Var
  x : sw_integer;
begin
  FillChar(t,sizeof(t),length(s));
  For x := length(s) downto 1 do
   if (t[ord(s[x])] = length(s)) then
    t[ord(s[x])] := length(s) - x;
end;


function BMFScan(var Block; Size: Sw_Word;const Str: String;const bt:BTable): Sw_Integer;
Var
  buffer : Array[0..MaxBufLength-1] of Byte Absolute block;
  s2     : String;
  len,
  numb   : Sw_Word;
  found  : Boolean;
begin
  len:=length(str);
  if len>size then
   begin
     BMFScan := NotFoundValue;
     exit;
   end;
  s2[0]:=chr(len);       { sets the length to that of the search String }
  found:=False;
  numb:=pred(len);
  While (not found) and (numb<size) do
   begin
     { partial match }
     if buffer[numb] = ord(str[len]) then
      begin
        { less partial! }
        if buffer[numb-pred(len)] = ord(str[1]) then
         begin
           move(buffer[numb-pred(len)],s2[1],len);
           if (str=s2) then
            begin
              found:=true;
              break;
            end;
         end;
        inc(numb);
     end
    else
     inc(numb,Bt[buffer[numb]]);
  end;
  if not found then
    BMFScan := NotFoundValue
  else
    BMFScan := numb - pred(len);
end;


function BMFIScan(var Block; Size: Sw_Word;const Str: String;const bt:BTable): Sw_Integer;
Var
  buffer : Array[0..MaxBufLength-1] of Char Absolute block;
  len,
  numb,
  x      : Sw_Word;
  found  : Boolean;
  p      : pchar;
  c      : char;
begin
  len:=length(str);
  if (len=0) or (len>size) then
   begin
     BMFIScan := NotFoundValue;
     exit;
   end;
  found:=False;
  numb:=pred(len);
  While (not found) and (numb<size) do
   begin
     { partial match }
     c:=buffer[numb];
     if c in ['a'..'z'] then
      c:=chr(ord(c)-32);
     if (c=str[len]) then
      begin
        { less partial! }
        p:=@buffer[numb-pred(len)];
        x:=1;
        while (x<=len) do
         begin
           if not(((p^ in ['a'..'z']) and (chr(ord(p^)-32)=str[x])) or
             (p^=str[x])) then
            break;
           inc(p);
           inc(x);
         end;
        if (x>len) then
         begin
           found:=true;
           break;
         end;
        inc(numb);
      end
     else
      inc(numb,Bt[ord(c)]);
   end;
  if not found then
    BMFIScan := NotFoundValue
  else
    BMFIScan := numb - pred(len);
end;


Procedure BMBMakeTable(const s:string; Var t : Btable);
Var
  x : sw_integer;
begin
  FillChar(t,sizeof(t),length(s));
  For x := 1 to length(s)do
   if (t[ord(s[x])] = length(s)) then
    t[ord(s[x])] := x-1;
end;


function BMBScan(var Block; Size: Sw_Word;const Str: String;const bt:BTable): Sw_Integer;
Var
  buffer : Array[0..MaxBufLength-1] of Byte Absolute block;
  s2     : String;
  len,
  numb   : Sw_integer;
  found  : Boolean;
begin
  len:=length(str);
  if len>size then
   begin
     BMBScan := NotFoundValue;
     exit;
   end;
  s2[0]:=chr(len);       { sets the length to that of the search String }
  found:=False;
  numb:=size-pred(len);
  While (not found) and (numb>0) do
   begin
     { partial match }
     if buffer[numb] = ord(str[1]) then
      begin
        { less partial! }
        if buffer[numb+pred(len)] = ord(str[len]) then
         begin
           move(buffer[numb],s2[1],len);
           if (str=s2) then
            begin
              found:=true;
              break;
            end;
         end;
        dec(numb);
     end
    else
     dec(numb,Bt[buffer[numb]]);
  end;
  if not found then
    BMBScan := NotFoundValue
  else
    BMBScan := numb;
end;


function BMBIScan(var Block; Size: Sw_Word;const Str: String;const bt:BTable): Sw_Integer;
Var
  buffer : Array[0..MaxBufLength-1] of Char Absolute block;
  len,
  numb,
  x      : Sw_integer;
  found  : Boolean;
  p      : pchar;
  c      : char;
begin
  len:=length(str);
  if (len=0) or (len>size) then
   begin
     BMBIScan := NotFoundValue;
     exit;
   end;
  found:=False;
  numb:=size-len;
  While (not found) and (numb>0) do
   begin
     { partial match }
     c:=buffer[numb];
     if c in ['a'..'z'] then
      c:=chr(ord(c)-32);
     if (c=str[1]) then
      begin
        { less partial! }
        p:=@buffer[numb];
        x:=1;
        while (x<=len) do
         begin
           if not(((p^ in ['a'..'z']) and (chr(ord(p^)-32)=str[x])) or
             (p^=str[x])) then
            break;
           inc(p);
           inc(x);
         end;
        if (x>len) then
         begin
           found:=true;
           break;
         end;
        dec(numb);
      end
     else
      dec(numb,Bt[ord(c)]);
   end;
  if not found then
    BMBIScan := NotFoundValue
  else
    BMBIScan := numb;
end;


{*****************************************************************************
                            PLine,TLineCollection
*****************************************************************************}

function NewLine(S: string): PLine;
var P: PLine;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  P^.Text:=NewStr(S);
  NewLine:=P;
end;


procedure DisposeLine(P: PLine);
begin
  if P<>nil then
  begin
    if P^.Text<>nil then DisposeStr(P^.Text);
    if P^.Format<>nil then DisposeStr(P^.Format);
    Dispose(P);
  end;
end;

function TLineCollection.At(Index: sw_Integer): PLine;
begin
  At:=inherited At(Index);
end;

procedure TLineCollection.FreeItem(Item: Pointer);
begin
  if Item<>nil then DisposeLine(Item);
end;


constructor TIndicator.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  GrowMode := gfGrowLoY + gfGrowHiY;
end;

procedure TIndicator.Draw;
var
  Color: Byte;
  Frame: Char;
  L: array[0..1] of Longint;
  S: String[15];
  B: TDrawBuffer;
begin
  if (State and sfDragging = 0) and (State and sfActive <> 0) then
   begin
     Color := GetColor(1);
     Frame := #205;
   end
  else
   begin
     if (State and sfDragging)<>0 then
      Color := GetColor(2)
     else
      Color := GetColor(3);
     Frame := #196;
   end;
  MoveChar(B, Frame, Color, Size.X);
  if State and sfActive<>0 then
   begin
     if Modified then
       WordRec (B[0]).Lo := ord('*');
{$ifdef debug}
     if StoreUndo then
       WordRec (B[1]).Lo := ord('S');
     if SyntaxComplete then
       WordRec(B[2]).lo := ord('C');
     if UseTabs then
       WordRec(B[3]).lo := ord('T');
{$endif debug}
     L[0] := Location.Y + 1;
     L[1] := Location.X + 1;
     FormatStr(S, '%d:%d ', L);
     MoveStr(B[8 - Pos(':', S)], S, Color);
   end;
  WriteBuf(0, 0, Size.X, 1, B);
end;

function TIndicator.GetPalette: PPalette;
const
  P: string[Length(CIndicator)] = CIndicator;
begin
  GetPalette := @P;
end;

procedure TIndicator.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState, Enable);
  if (AState = sfDragging) or (AState=sfActive) then
   DrawView;
end;

procedure TIndicator.SetValue(ALocation: TPoint; AModified: Boolean);
begin
  if (Location.X<>ALocation.X) or
     (Location.Y<>ALocation.Y) or
     (Modified <> AModified) then
  begin
    Location := ALocation;
    Modified := AModified;
    DrawView;
  end;
end;

constructor TIndicator.Load(var S: TStream);
begin
  inherited Load(S);
  S.Read(Location,SizeOf(Location));
  S.Read(Modified,SizeOf(Modified));
end;

procedure TIndicator.Store(var S: TStream);
begin
  inherited Store(S);
  S.Write(Location,SizeOf(Location));
  S.Write(Modified,SizeOf(Modified));
end;


{*****************************************************************************
                TCodeEditor
*****************************************************************************}

constructor TCodeEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator; ABufSize:Sw_Word);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
{$ifndef Undo}
  StoreUndo:=false;
{$else Undo}
  StoreUndo:=true;
{$endif def Undo}
  new(UndoList,init(500,1000));
  new(RedoList,init(500,1000));
  New(Lines, Init(500,1000));
  { we have always need at least 1 line }
  Lines^.Insert(NewLine(''));
  { ^^^ why? setlinetext() inserts automatically if neccessary and
    getlinetext() checks whether you're in range...
    because otherwise you search for line with index -1 (PM)
    Then I think the algorithm should be changed to handle this special case,
    instead of applying this "work-around" - Gabor
  }
  SetState(sfCursorVis,true);
  SetFlags(DefaultCodeEditorFlags); TabSize:=DefaultTabSize;
  SetHighlightRow(-1);
  SetDebuggerRow(-1);
  SetCurPtr(0,0);
  Indicator:=AIndicator;
{$ifdef TEST_PARTIAL_SYNTAX}
  SyntaxComplete:=true;
{$endif TEST_PARTIAL_SYNTAX}
  UpdateIndicator;
  LimitsChanged;
end;

procedure TCodeEditor.SetFlags(AFlags: longint);
var I: sw_integer;
    OldFlags: longint;
begin
  OldFlags:=Flags;
  Flags:=AFlags;
  if ((OldFlags xor Flags) and efCodeComplete)<>0 then
    ClearCodeCompleteWord;
  SetInsertMode((Flags and efInsertMode)<>0);
  if (Flags and efSyntaxHighlight)<>0 then
    UpdateAttrs(0,attrAll) else
  for I:=0 to GetLineCount-1 do
    SetLineFormat(I,'');
  UpdateIndicator;
  DrawView;
end;

function TCodeEditor.GetErrorMessage: string;
var S: string;
begin
  if ErrorMessage=nil then S:='' else S:=ErrorMessage^;
  GetErrorMessage:=S;
end;

procedure TCodeEditor.SetErrorMessage(const S: string);
begin
  if ErrorMessage<>nil then DisposeStr(ErrorMessage);
  ErrorMessage:=NewStr(S);
  DrawView;
end;

procedure TCodeEditor.Lock;
begin
  Inc(LockFlag);
end;

procedure TCodeEditor.UnLock;
begin
{$ifdef DEBUG}
  if lockflag=0 then
    Bug('negative lockflag',nil)
  else
{$endif DEBUG}
    Dec(LockFlag);
  if (LockFlag>0) then
    exit;
  if DrawCalled then
    DrawView;
  If IndicatorDrawCalled and
    assigned(Indicator) then
      begin
        Indicator^.DrawView;
        IndicatorDrawCalled:=false;
      end;
  If DrawCursorCalled then
    Begin
      DrawCursor;
      DrawCursorCalled:=false;
    End;
end;

procedure TCodeEditor.AdjustSelectionPos(CurPosX, CurPosY: sw_integer; DeltaX, DeltaY: sw_integer);
var CP: TPoint;
begin
  if ValidBlock=false then Exit;

  CP.X:=CurPosX; CP.Y:=CurPosY;
  if (PosToOfsP(SelStart)<=PosToOfsP(CP)) and (PosToOfsP(CP)<PosToOfsP(SelEnd)) then
    begin
      { CurPos is IN selection }
      Inc(SelEnd.Y,DeltaY);
      if (CP.Y=SelEnd.Y) and
         ((SelStart.Y<>SelEnd.Y) or (SelStart.X<=CP.X)) and
         (CP.X<=SelEnd.X) then
       Inc(SelEnd.X,DeltaX);
      SelectionChanged;
    end
  else
  if (PosToOfsP(CP)<=PosToOfsP(SelStart)) then
    begin
      { CurPos is BEFORE selection }
      if (CP.Y=SelStart.Y) and (CP.Y=SelEnd.Y) and (DeltaY<0) then
        begin
          SelStart:=CurPos; SelEnd:=CurPos;
        end
      else
      if (CP.Y=SelStart.Y) then
        begin
          if CP.X<SelStart.X then
            Inc(SelStart.X,DeltaX);
        end;
{      else}
        begin
          Inc(SelStart.Y,DeltaY);
          Inc(SelEnd.Y,DeltaY);
        end;
      if SelEnd.Y=CurPos.Y then Inc(SelEnd.X,DeltaX);
      SelectionChanged;
    end
  else
    begin
      { CurPos is AFTER selection }
      { actually we don't have to do anything here }
    end;
end;

procedure TCodeEditor.AdjustSelection(DeltaX, DeltaY: sw_integer);
begin
  AdjustSelectionPos(CurPos.X,CurPos.Y,DeltaX,DeltaY);
end;

procedure TCodeEditor.TrackCursor(Center: boolean);
var D: TPoint;
begin
  D:=Delta;
  if CurPos.Y<Delta.Y then D.Y:=CurPos.Y else
   if CurPos.Y>Delta.Y+Size.Y-1 then D.Y:=CurPos.Y-Size.Y+1;
  if CurPos.X<Delta.X then D.X:=CurPos.X else
   if CurPos.X>Delta.X+Size.X-1 then D.X:=CurPos.X-Size.X+1;
  if {((Delta.X<>D.X) or (Delta.Y<>D.Y)) and }Center then
  begin
     { loose centering for debugger PM }
     while (CurPos.Y-D.Y)<(Size.Y div 3) do Dec(D.Y);
     while (CurPos.Y-D.Y)>2*(Size.Y div 3) do Inc(D.Y);
  end;
  if (Delta.X<>D.X) or (Delta.Y<>D.Y) then
    ScrollTo(D.X,D.Y);
  DrawCursor;
  UpdateIndicator;
end;

procedure TCodeEditor.ScrollTo(X, Y: sw_Integer);
begin
  inherited ScrollTo(X,Y);
  if (HScrollBar=nil) or (VScrollBar=nil) then
     begin Delta.X:=X; Delta.Y:=Y; end;
  DrawView;
end;

procedure TCodeEditor.UpdateIndicator;
begin
  if Indicator<>nil then
  begin
    Indicator^.Location:=CurPos;
    Indicator^.Modified:=Modified;
{$ifdef debug}
    Indicator^.StoreUndo:=StoreUndo;
{$ifdef TEST_PARTIAL_SYNTAX}
    Indicator^.SyntaxComplete:=SyntaxComplete and ((Flags and efSyntaxHighlight)<>0);
{$endif TEST_PARTIAL_SYNTAX}
    Indicator^.UseTabs:=((Flags and efUseTabCharacters)<>0);
{$endif debug}
    if lockflag>0 then
      IndicatorDrawCalled:=true
    else
      Indicator^.DrawView;
  end;
end;

procedure TCodeEditor.LimitsChanged;
begin
  SetLimit(MaxLineLength+1,GetLineCount);
end;

procedure TCodeEditor.ConvertEvent(var Event: TEvent);
var
  Key: Word;
begin
  if Event.What = evKeyDown then
  begin
    if (Event.KeyShift and kbShift <> 0) and
      (Event.ScanCode >= $47) and (Event.ScanCode <= $51) then
      Event.CharCode := #0;
    Key := Event.KeyCode;
    if KeyState <> 0 then
    begin
      if (Lo(Key) >= $01) and (Lo(Key) <= $1A) then Inc(Key, $40);
      if (Lo(Key) >= $61) and (Lo(Key) <= $7A) then Dec(Key, $20);
    end;
    Key := ScanKeyMap(KeyMap[KeyState], Key);
    if (KeyState<>0) and (Key=0) then
      ClearEvent(Event); { eat second key if unrecognized after ^Q or ^K }
    KeyState := 0;
    if Key <> 0 then
      if Hi(Key) = $FF then
        begin
          KeyState := Lo(Key);
          ClearEvent(Event);
        end
      else
        begin
          Event.What := evCommand;
          Event.Command := Key;
        end;
  end;
end;

procedure TCodeEditor.HandleEvent(var Event: TEvent);
var DontClear : boolean;

  procedure CheckScrollBar(P: PScrollBar; var D: Sw_Integer);
  begin
    if (Event.InfoPtr = P) and (P^.Value <> D) then
    begin
      D := P^.Value;
      DrawView;
    end;
  end;

  procedure GetMousePos(var P: TPoint);
  begin
    MakeLocal(Event.Where,P);
    Inc(P.X,Delta.X); Inc(P.Y,Delta.Y);
  end;
type TCCAction = (ccCheck,ccClear,ccDontCare);
var
  StartP,P: TPoint;
  E: TEvent;
  OldEvent : PEvent;
  CCAction: TCCAction;
begin
  CCAction:=ccClear;
  E:=Event;
  OldEvent:=CurEvent;
  if (E.What and (evMouse or evKeyboard))<>0 then
    CurEvent:=@E;
  if (InASCIIMode=false) or (Event.What<>evKeyDown) then
   if (Event.What<>evKeyDown) or
      ((Event.KeyCode<>kbEnter) and (Event.KeyCode<>kbEsc)) or
      (CompleteState<>csOffering) then
    ConvertEvent(Event);
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) then
       if (Event.Buttons=mbRightButton) then
         begin
           MakeLocal(Event.Where,P); Inc(P.X); Inc(P.Y);
           LocalMenu(P);
           ClearEvent(Event);
         end else
       if Event.Buttons=mbLeftButton then
        begin
          GetMousePos(P);
          StartP:=P;
          SetCurPtr(P.X,P.Y);
          repeat
            GetMousePos(P);
            if PointOfs(P)<PointOfs(StartP)
               then SetSelection(P,StartP)
               else SetSelection(StartP,P);
            SetCurPtr(P.X,P.Y);
            DrawView;
          until not MouseEvent(Event, evMouseMove+evMouseAuto);
          DrawView;
        end;
    evKeyDown :
      begin
        { Scancode is almost never zero PM }
        { this is supposed to enable entering of ASCII chars below 32,
          which are normally interpreted as control chars. So, when you enter
          Alt+24 (on the numeric pad) then this will normally move the cursor
          one line down, but if you do it in ASCII mode (also after Ctrl+B)
          then this will insert the ASCII #24 char (upper arrow) in the
          source code. - Gabor }
        if InASCIIMode {and (Event.CharCode<>0)} then
          begin
            AddChar(Event.CharCode);
            if (CompleteState<>csDenied) or (Event.CharCode=#32) then
              CCAction:=ccCheck
            else
              CCAction:=ccClear;
          end
        else
          begin
           DontClear:=false;
           case Event.KeyCode of
             kbAltF10 :
               Message(@Self,evCommand,cmLocalMenu,@Self);
             kbEnter  :
               if CompleteState=csOffering then
                 CodeCompleteApply
               else
                 Message(@Self,evCommand,cmNewLine,nil);
             kbEsc :
               if CompleteState=csOffering then
                 CodeCompleteCancel;
           else
            case Event.CharCode of
             #9,#32..#255 :
               begin
                 NoSelect:=true;
                 AddChar(Event.CharCode);
                 NoSelect:=false;
                 if (CompleteState<>csDenied) or (Event.CharCode=#32) then
                   CCAction:=ccCheck
                 else
                   CCAction:=ccClear;
               end;
            else
              DontClear:=true;
            end; { case Event.CharCode .. }
           end; { case Event.KeyCode .. }
            if not DontClear then
             ClearEvent(Event);
          end;
        InASCIIMode:=false;
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmASCIIChar   : InASCIIMode:=not InASCIIMode;
          cmAddChar     : AddChar(chr(longint(Event.InfoPtr)));
          cmCharLeft    : CharLeft;
          cmCharRight   : CharRight;
          cmWordLeft    : WordLeft;
          cmWordRight   : WordRight;
          cmLineStart   : LineStart;
          cmLineEnd     : LineEnd;
          cmLineUp      : LineUp;
          cmLineDown    : LineDown;
          cmPageUp      : PageUp;
          cmPageDown    : PageDown;
          cmTextStart   : TextStart;
          cmTextEnd     : TextEnd;
          cmNewLine     : InsertLine;
          cmBreakLine   : BreakLine;
          cmBackSpace   : BackSpace;
          cmDelChar     : DelChar;
          cmDelWord     : DelWord;
          cmDelStart    : DelStart;
          cmDelEnd      : DelEnd;
          cmDelLine     : DelLine;
          cmInsMode     : InsMode;
          cmStartSelect : StartSelect;
          cmHideSelect  : HideSelect;
          cmUpdateTitle : ;
          cmEndSelect   : EndSelect;
          cmDelSelect   : DelSelect;
          cmCopyBlock   : CopyBlock;
          cmMoveBlock   : MoveBlock;
          cmIndentBlock   : IndentBlock;
          cmUnindentBlock : UnindentBlock;
          cmSelStart    : JumpSelStart;
          cmSelEnd      : JumpSelEnd;
          cmLastCursorPos : JumpToLastCursorPos;
          cmJumpMark0..cmJumpMark9 : JumpMark(Event.Command-cmJumpMark0);
          cmSetMark0..cmSetMark9 : DefineMark(Event.Command-cmSetMark0);
          cmSelectWord  : SelectWord;
          cmSelectLine  : SelectLine;
          cmWriteBlock  : WriteBlock;
          cmReadBlock   : ReadBlock;
          cmPrintBlock  : PrintBlock;
        { ------ }
          cmFind        : Find;
          cmReplace     : Replace;
          cmSearchAgain : DoSearchReplace;
          cmJumpLine    : GotoLine;
        { ------ }
          cmCut         : ClipCut;
          cmCopy        : ClipCopy;
          cmPaste       : ClipPaste;
{$ifdef WinClipSupported}
          cmCopyWin     : ClipCopyWin;
          cmPasteWin    : ClipPasteWin;
{$endif WinClipSupported}
          cmUndo        : Undo;
          cmRedo        : Redo;
          cmClear       : DelSelect;
          cmExpandCodeTemplate: ExpandCodeTemplate;
          cmLocalMenu :
            begin
              P:=CurPos; Inc(P.X); Inc(P.Y);
              LocalMenu(P);
            end;
        else
          begin
            DontClear:=true;
            CCAction:=ccDontCare;
          end;
        end;
        if DontClear=false then
          ClearEvent(Event);
      end;
    evBroadcast :
      begin
        CCAction:=ccDontCare;
        case Event.Command of
          cmUpdate :
            Update;
          cmClearLineHighlights :
            SetHighlightRow(-1);
          cmResetDebuggerRow :
            SetDebuggerRow(-1);
          cmScrollBarChanged:
            if (Event.InfoPtr = HScrollBar) or
               (Event.InfoPtr = VScrollBar) then
              begin
                CheckScrollBar(HScrollBar, Delta.X);
                CheckScrollBar(VScrollBar, Delta.Y);
              end;
        end;
      end;
  else CCAction:=ccDontCare;
  end;
  inherited HandleEvent(Event);
  CurEvent:=OldEvent;
  case CCAction of
    ccCheck : CodeCompleteCheck;
    ccClear : ClearCodeCompleteWord;
  end;
end;

procedure TCodeEditor.UpdateUndoRedo(cm : word; action : byte);
var UndoMenu : PMenuItem;
begin
  UndoMenu:=PAdvancedMenuBar(MenuBar)^.GetMenuItem(cm);
  if assigned(UndoMenu) then
    begin
      If assigned(UndoMenu^.Param) then
        DisposeStr(UndoMenu^.Param);
      if action<lastaction then
        UndoMenu^.Param:=NewStr(ActionString[action]);
    end;
end;


procedure TCodeEditor.Update;
begin
  LimitsChanged;
  SelectionChanged; HighlightChanged;
  UpdateIndicator;
  DrawView;
end;

function TCodeEditor.GetLocalMenu: PMenu;
begin
  GetLocalMenu:=nil;
end;

function TCodeEditor.GetCommandTarget: PView;
begin
  GetCommandTarget:=@Self;
end;

function TCodeEditor.CreateLocalMenuView(var Bounds: TRect; M: PMenu): PMenuPopup;
var MV: PMenuPopup;
begin
  New(MV, Init(Bounds, M));
  CreateLocalMenuView:=MV;
end;

procedure TCodeEditor.LocalMenu(P: TPoint);
var M: PMenu;
    MV: PMenuPopUp;
    R: TRect;
    Re: word;
begin
  M:=GetLocalMenu;
  if M=nil then Exit;
  if LastLocalCmd<>0 then
     M^.Default:=SearchMenuItem(M,LastLocalCmd);
  Desktop^.GetExtent(R);
  MakeGlobal(P,R.A); {Desktop^.MakeLocal(R.A,R.A);}
  MV:=CreateLocalMenuView(R,M);
  Re:=Application^.ExecView(MV);
  if M^.Default=nil then LastLocalCmd:=0
     else LastLocalCmd:=M^.Default^.Command;
  Dispose(MV, Done);
  if Re<>0 then
    Message(GetCommandTarget,evCommand,Re,@Self);
end;


procedure TCodeEditor.Draw;
var SelectColor,
    HighlightColColor,
    HighlightRowColor,
    ErrorMessageColor  : word;
    B: TDrawBuffer;
    X,Y,AX,AY,MaxX: sw_integer;
    PX: TPoint;
    LineCount: sw_integer;
    Line: PLine;
    LineText,Format: string;
    isBreak : boolean;
    C: char;
    FreeFormat: array[0..255] of boolean;
    Color: word;
    ColorTab: array[coFirstColor..coLastColor] of word;
    ErrorLine: integer;
    ErrorMsg: string[MaxViewWidth];
function CombineColors(Orig,Modifier: byte): byte;
var Color: byte;
begin
  if (Modifier and $0f)=0 then
    Color:=(Orig and $0f) or (Modifier and $f0)
  else
    Color:=(Orig and $f0) or (Modifier and $0f);
  { do not allow invisible }
  { use white as foreground in this case }
  if (Color and $f) = ((Color div $10) and $7) then
    Color:=(Color and $F0) or $F;
  CombineColors:=Color;
end;
const NulLine : TLine = (Text: nil; Format: nil);
begin

  if LockFlag>0 then
    begin
      DrawCalled:=true;
      Exit;
    end;
  DrawCalled:=false;

  ErrorMsg:=copy(GetErrorMessage,1,MaxViewWidth);
  if ErrorMsg='' then ErrorLine:=-1 else
  if (CurPos.Y-Delta.Y)<(Size.Y div 2) then ErrorLine:=Size.Y-1
     else ErrorLine:=0;
  LineCount:=GetLineCount;
  ColorTab[coTextColor]:=GetColor(1);
  ColorTab[coWhiteSpaceColor]:=GetColor(2);
  ColorTab[coCommentColor]:=GetColor(3);
  ColorTab[coReservedWordColor]:=GetColor(4);
  ColorTab[coIdentifierColor]:=GetColor(5);
  ColorTab[coStringColor]:=GetColor(6);
  ColorTab[coNumberColor]:=GetColor(7);
  ColorTab[coAssemblerColor]:=GetColor(8);
  ColorTab[coSymbolColor]:=GetColor(9);
  ColorTab[coDirectiveColor]:=GetColor(13);
  ColorTab[coHexNumberColor]:=GetColor(14);
  ColorTab[coTabColor]:=GetColor(15);
  { break same as error }
  ColorTab[coBreakColor]:=GetColor(16);
  SelectColor:=GetColor(10);
  HighlightColColor:=GetColor(11);
  HighlightRowColor:=GetColor(12);
  ErrorMessageColor:=GetColor(16);
{$ifdef TEST_PARTIAL_SYNTAX}
  If LastSyntaxedLine<Delta.Y+Size.Y then
    UpdateAttrsRange(LastSyntaxedLine,Delta.Y+Size.Y,AttrAll);
{$endif TEST_PARTIAL_SYNTAX}
  for Y:=0 to Size.Y-1 do
  if Y=ErrorLine then
  begin
    MoveChar(B,' ',ErrorMessageColor,Size.X);
    MoveStr(B,ErrorMsg,ErrorMessageColor);
    WriteLine(0,Y,Size.X,1,B);
  end else
  begin
    AY:=Delta.Y+Y;
    Color:=ColorTab[coTextColor];
    FillChar(FreeFormat,SizeOf(FreeFormat),1);
    MoveChar(B,' ',Color,Size.X);
    if AY<LineCount then
     begin
       Line:=GetLine(AY);
       IsBreak:=Lines^.at(AY)^.isBreakpoint;
     end
    else
     begin
       Line:=@NulLine;
       IsBreak:=false;
     end;
    GetDisplayTextFormat(AY,LineText,Format);

{    if (Flags and efSyntaxHighlight)<>0 then MaxX:=length(LineText)+1
       else }MaxX:=Size.X+Delta.X;
    for X:=1 to Min(MaxX,255) do
    begin
      AX:=Delta.X+X-1;
      if X<=length(LineText) then C:=LineText[X] else C:=' ';

      PX.X:=AX-Delta.X; PX.Y:=AY;
      if (Highlight.A.X<>Highlight.B.X) or (Highlight.A.Y<>Highlight.B.Y) then
      begin
    if (PointOfs(Highlight.A)<=PointOfs(PX)) and (PointOfs(PX)<PointOfs(Highlight.B)) then
    begin
       Color:=SelectColor;
       FreeFormat[X]:=false;
    end;
      end else
      { no highlight }
      begin
   if (Flags and efVerticalBlocks<>0) then
      begin
        if (SelStart.X<=AX) and (AX<=SelEnd.X) and
      (SelStart.Y<=AY) and (AY<=SelEnd.Y) then
      begin Color:=SelectColor; FreeFormat[X]:=false; end;
      end else
    if PointOfs(SelStart)<>PointOfs(SelEnd) then
     if (PointOfs(SelStart)<=PointOfs(PX)) and (PointOfs(PX)<PointOfs(SelEnd)) then
        begin Color:=SelectColor; FreeFormat[X]:=false; end;
      end;
    if FreeFormat[X] then
     if X<=length(Format) then
       {Color:=ColorTab[ord(Format[X])] else Color:=ColorTab[coTextColor];
         this give BoundsCheckError with -Cr quite often PM }
       Color:=ColorTab[ord(Format[X]) mod (coLastColor + 1)] else Color:=ColorTab[coTextColor];

    if ( ((Flags and efHighlightRow)   <>0) and
       (PX.Y=CurPos.Y) ) and (HighlightRow=-1) then
      begin
        Color:=CombineColors(Color,HighlightRowColor);
        FreeFormat[X]:=false;
      end;
    if ( ((Flags and efHighlightColumn)<>0) and (PX.X=CurPos.X) ) then
      begin
        Color:=CombineColors(Color,HighlightColColor);
        FreeFormat[X]:=false;
      end;

    if HighlightRow=AY then
      begin
        Color:=CombineColors(Color,HighlightRowColor);
        FreeFormat[X]:=false;
      end;
    if DebuggerRow=AY then
      begin
        Color:=CombineColors(Color,HighlightRowColor);
        FreeFormat[X]:=false;
      end;
    if isbreak then
      begin
        Color:=ColorTab[coBreakColor];
        FreeFormat[X]:=false;
      end;

      if (0<=X-1-Delta.X) and (X-1-Delta.X<MaxViewWidth) then
      MoveChar(B[X-1-Delta.X],C,Color,1);
    end;
    WriteLine(0,Y,Size.X,1,B);
  end;
  DrawCursor;
end;

procedure TCodeEditor.DrawCursor;
begin
  if lockflag>0 then
    DrawCursorCalled:=true
  else
    SetCursor(CurPos.X-Delta.X,CurPos.Y-Delta.Y);
  SetState(sfCursorIns,Overwrite);
end;

function TCodeEditor.Overwrite: boolean;
begin
  Overwrite:=(Flags and efInsertMode)=0;
end;

function TCodeEditor.GetLineCount: sw_integer;
begin
  GetLineCount:=Lines^.Count;
end;

function TCodeEditor.GetLine(I: sw_integer): PLine;
begin
  GetLine:=Lines^.At(I);
end;

function TCodeEditor.CharIdxToLinePos(Line,CharIdx: sw_integer): sw_integer;
var S: string;
    CP,RX: sw_integer;
begin
  S:=GetLineText(Line);
  CP:=1; RX:=0;
  while (CP<=length(S)) and (CP<CharIdx) do
   begin
     if S[CP]=TAB then
       Inc(RX,TabSize-(RX mod TabSize))
     else
       Inc(RX);
     Inc(CP);
   end;
  CharIdxToLinePos:=RX;
end;

function TCodeEditor.LinePosToCharIdx(Line,X: sw_integer): sw_integer;
var S: string;
    CP,RX: sw_integer;
begin
  S:=GetLineText(Line);
  if S='' then
    CP:=0
  else
    begin
     CP:=0; RX:=0;
     while (RX<=X) and (CP<=length(S)) do
      begin
        Inc(CP);
        if S[CP]=TAB then
          Inc(RX,TabSize-(RX mod TabSize))
        else
          Inc(RX);
      end;
    end;
  LinePosToCharIdx:=CP;
end;

{function TCodeEditor.GetLineTextPos(Line,X: integer): integer;
var
  S: string;
  rx,i : Sw_integer;
begin
  S:=GetLineText(Line);
  i:=0; rx:=0;
  while (RX<X) and (i<Length(s)) do
   begin
     inc(i);
     inc(rx);
     if s[i]=#9 then
      inc(rx,TabSize-(rx mod tabsize));
   end;
  if RX<X then Inc(I,X-RX);
  GetLineTextPos:=i;
end;

function TCodeEditor.GetDisplayTextPos(Line,X: integer): integer;
var
  S: string;
  L: PLine;
  rx,i : Sw_integer;
begin
  S:='';
  if Line<Lines^.Count then
   begin
     L:=Lines^.At(Line);
     if assigned(L^.Text) then
      S:=L^.Text^;
   end;
  i:=0;
  rx:=0;
  while (i<X) and (i<Length(s)) do
   begin
     inc(i);
     inc(rx);
     if s[i]=#9 then
      inc(rx,TabSize-(rx mod tabsize));
   end;
  GetDisplayTextPos:=rx;
end;}

function TCodeEditor.GetLineText(I: sw_integer): string;
var
  L : PLine;
begin
  GetLineText:='';
  if I<Lines^.Count then
   begin
     L:=Lines^.At(I);
     if assigned(L^.Text) then
      GetLineText:=L^.Text^;
   end;
end;

procedure TCodeEditor.SetLineText(I: sw_integer;const S: string);
var
  L : PLine;
  AddCount : Sw_Integer;
begin
  AddCount:=0;
  while (Lines^.Count<I+1) do
   begin
     Lines^.Insert(NewLine(''));
     Inc(AddCount);
   end;
  if AddCount>0 then
   LimitsChanged;
  L:=Lines^.At(I);
  if assigned(L^.Text) then
   DisposeStr(L^.Text);
  L^.Text:=NewStr(S);
end;

procedure TCodeEditor.SetLineBreakState(I : sw_integer;b : boolean);
var PL : PLine;
begin
   if (i>0) and (i<=Lines^.Count) then
     PL:=Lines^.At(i-1)
   else
     exit;
   if assigned(PL) then
     PL^.isbreakpoint:=b;
   DrawView;
end;

function TCodeEditor.GetDisplayText(I: sw_integer): string;
begin
  GetDisplayText:=ExtractTabs(GetLineText(I),TabSize);
end;

procedure TCodeEditor.SetDisplayText(I: sw_integer;const S: string);
begin
  if ((Flags and efUseTabCharacters)<>0) and (TabSize>0) then
   SetLineText(I,CompressUsingTabs(S,TabSize))
  else
   SetLineText(I,S);
end;

procedure TCodeEditor.GetDisplayTextFormat(I: sw_integer;var DT,DF:string);
var
  L : PLine;
  P,PAdd : SW_Integer;
begin
  DF:='';
  DT:='';
  if I<Lines^.Count then
   begin
     L:=Lines^.At(I);
     if assigned(L^.Text) then
      begin
   if assigned(L^.Format)=false then DF:='' else
     DF:=L^.Format^;
   DT:=L^.Text^;
   p:=0;
   while p<length(DT) do
    begin
      inc(p);
      if DT[p]=#9 then
       begin
         PAdd:=TabSize-((p-1) mod TabSize);
         if DF<>'' then
          DF:=copy(DF,1,P-1)+CharStr(DF[p],PAdd)+copy(DF,P+1,255);
         DT:=copy(DT,1,P-1)+CharStr(' ',PAdd)+copy(DT,P+1,255);
         inc(P,PAdd-1);
       end;
    end;
      end;
   end;
end;

function TCodeEditor.GetLineFormat(I: sw_integer): string;
var P: PLine;
    S: string;
begin
  if I<GetLineCount then P:=Lines^.At(I) else P:=nil;
  if (P=nil) or (P^.Format=nil) then S:='' else
     S:=P^.Format^;
  GetLineFormat:=S;
end;

procedure TCodeEditor.SetLineFormat(I: sw_integer;const S: string);
var P: PLine;
begin
  if I<GetLineCount then
  begin
    P:=Lines^.At(I);
    if P^.Format<>nil then DisposeStr(P^.Format);
    P^.Format:=NewStr(S);
  end;
end;

procedure TCodeEditor.DeleteAllLines;
begin
  if Assigned(Lines) then
    Lines^.FreeAll;
end;

procedure TCodeEditor.DeleteLine(I: sw_integer);
var
 CP : Tpoint;
begin
  if I<Lines^.Count then
    begin
      if StoreUndo then
        begin
          CP.X:=0;CP.Y:=I;
          AddAction(eaDeleteLine,CP,CP,GetLineText(I));
       end;
      Lines^.AtFree(I);
    end;
end;

procedure TCodeEditor.AddLine(const S: string);
begin
  Lines^.Insert(NewLine(S));
end;

function TCodeEditor.GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer;
begin
  GetSpecSymbolCount:=0;
end;

function TCodeEditor.GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string;
begin
  GetSpecSymbol:='';
  Abstract;
end;

function TCodeEditor.IsReservedWord(const S: string): boolean;
begin
  IsReservedWord:=false;
end;

function TCodeEditor.TranslateCodeTemplate(const Shortcut: string; ALines: PUnsortedStringCollection): boolean;
begin
  TranslateCodeTemplate:=false;
end;

function TCodeEditor.CompleteCodeWord(const WordS: string; var Text: string): boolean;
begin
  CompleteCodeWord:=false;
end;

function TCodeEditor.GetCodeCompleteWord: string;
begin
  GetCodeCompleteWord:=GetStr(CodeCompleteWord);
end;

procedure TCodeEditor.SetCodeCompleteWord(const S: string);
begin
  if Assigned(CodeCompleteWord) then DisposeStr(CodeCompleteWord);
  CodeCompleteWord:=NewStr(S);
  if S<>'' then
    SetCompleteState(csOffering)
  else
    SetCompleteState(csInactive);
end;

procedure TCodeEditor.ClearCodeCompleteWord;
begin
  SetCodeCompleteWord('');
  SetCompleteState(csInactive);
end;

procedure TCodeEditor.Indent;
var S, PreS: string;
    Shift: integer;
begin
  S:=GetLineText(CurPos.Y);
  if CurPos.Y>0 then
    PreS:=RTrim(GetLineText(CurPos.Y-1))
  else
    PreS:='';
  if CurPos.X>=length(PreS) then
    Shift:=TabSize
  else
    begin
      Shift:=1;
      while (CurPos.X+Shift<length(PreS)) and (PreS[CurPos.X+Shift]<>' ') do
       Inc(Shift);
    end;
  SetLineText(CurPos.Y,RExpand(copy(S,1,CurPos.X+1),CurPos.X+1)+CharStr(' ',Shift)+copy(S,CurPos.X+2,255));
  SetCurPtr(CurPos.X+Shift,CurPos.Y);
  UpdateAttrs(CurPos.Y,attrAll);
  DrawLines(CurPos.Y);
  SetModified(true);
end;

procedure TCodeEditor.CharLeft;
begin
  if CurPos.X=0 then Exit;

  SetCurPtr(CurPos.X-1,CurPos.Y);
end;

procedure TCodeEditor.CharRight;
begin
  if CurPos.X>=MaxLineLength then
    Exit;
  SetCurPtr(CurPos.X+1,CurPos.Y);
end;

procedure TCodeEditor.WordLeft;
var X, Y: sw_integer;
    Line: string;
    GotIt,FoundNonSeparator: boolean;
begin
  X:=CurPos.X;
  Y:=CurPos.Y;
  GotIt:=false;
  FoundNonSeparator:=false;
  while (Y>=0) do
   begin
     if Y=CurPos.Y then
      begin
   X:=length(GetDisplayText(Y));
   if CurPos.X<X then
     X:=CurPos.X; Dec(X);
   if (X=-1) then
     begin
       Dec(Y);
       if Y>=0 then
        X:=length(GetDisplayText(Y));
       Break;
     end;
      end
     else
      X:=length(GetDisplayText(Y))-1;
     Line:=GetDisplayText(Y);
     while (X>=0) and (GotIt=false) do
      begin
   if FoundNonSeparator then
    begin
      if IsWordSeparator(Line[X+1]) then
       begin
         Inc(X);
         GotIt:=true;
         Break;
       end;
    end
   else
    if not IsWordSeparator(Line[X+1]) then
     FoundNonSeparator:=true;
   Dec(X);
   if (X=0) and (IsWordSeparator(Line[1])=false) then
    begin
      GotIt:=true;
      Break;
    end;
      end;
     if GotIt then
      Break;
     X:=0;
     Dec(Y);
     if Y>=0 then
      begin
   X:=length(GetDisplayText(Y));
   Break;
      end;
   end;
  if Y<0 then Y:=0; if X<0 then X:=0;
  SetCurPtr(X,Y);
end;

procedure TCodeEditor.WordRight;
var X, Y: sw_integer;
    Line: string;
    GotIt: boolean;
begin
  X:=CurPos.X; Y:=CurPos.Y; GotIt:=false;
  while (Y<GetLineCount) do
  begin
    if Y=CurPos.Y then
       begin
    X:=CurPos.X; Inc(X);
    if (X>length(GetDisplayText(Y))-1) then
       begin Inc(Y); X:=0; end;
       end else X:=0;
    Line:=GetDisplayText(Y);
    while (X<=length(Line)+1) and (GotIt=false) and (Line<>'') do
    begin
      if X=length(Line)+1 then begin GotIt:=true; Dec(X); Break end;
      if IsWordSeparator(Line[X]) then
    begin
      while (Y<GetLineCount) and
       (X<=length(Line)) and (IsWordSeparator(Line[X])) do
       begin
         Inc(X);
         if X>=length(Line) then
            begin GotIt:=true; Dec(X); Break; end;
       end;
      if (GotIt=false) and (X<length(Line)) then
      begin
        Dec(X);
        GotIt:=true;
        Break;
      end;
    end;
      Inc(X);
    end;
    if GotIt then Break;
    X:=0;
    Inc(Y);
    if (Y<GetLineCount) then
    begin
      Line:=GetDisplayText(Y);
      if (Line<>'') and (IsWordSeparator(Line[1])=false) then Break;
    end;
  end;
  if Y=GetLineCount then Y:=GetLineCount-1;
  SetCurPtr(X,Y);
end;

procedure TCodeEditor.LineStart;
begin
  SetCurPtr(0,CurPos.Y);
end;

procedure TCodeEditor.LineEnd;
var
  s : string;
  i : longint;
begin
  if CurPos.Y<GetLineCount then
    begin
      s:=GetDisplayText(CurPos.Y);
      i:=length(s);
      while (i>0) and (s[i]=' ') do
        dec(i);
      SetCurPtr(i,CurPos.Y);
    end
  else
    SetCurPtr(0,CurPos.Y);
end;

procedure TCodeEditor.LineUp;
begin
  if CurPos.Y>0 then
     SetCurPtr(CurPos.X,CurPos.Y-1);
end;

procedure TCodeEditor.LineDown;
begin
  if CurPos.Y<GetLineCount-1 then
     SetCurPtr(CurPos.X,CurPos.Y+1);
end;

procedure TCodeEditor.PageUp;
begin
  ScrollTo(Delta.X,Max(Delta.Y-Size.Y,0));
  SetCurPtr(CurPos.X,Max(0,CurPos.Y-(Size.Y)));
end;

procedure TCodeEditor.PageDown;
begin
  ScrollTo(Delta.X,Min(Delta.Y+Size.Y,GetLineCount-1));
  SetCurPtr(CurPos.X,Min(GetLineCount-1,CurPos.Y+(Size.Y{-1})));
end;

procedure TCodeEditor.TextStart;
begin
  SetCurPtr(0,0);
end;

procedure TCodeEditor.TextEnd;
var s : string;
    i : longint;
begin
  s:=GetDisplayText(GetLineCount-1);
  i:=length(s);
  while (i>0) and (s[i]=' ') do
    dec(i);
  SetCurPtr(i,GetLineCount-1);
end;

procedure TCodeEditor.JumpSelStart;
begin
  if ValidBlock then
    SetCurPtr(SelStart.X,SelStart.Y);
end;

procedure TCodeEditor.JumpSelEnd;
begin
  if ValidBlock then
  SetCurPtr(SelEnd.X,SelEnd.Y);
end;

procedure TCodeEditor.JumpMark(MarkIdx: integer);
begin
  if (MarkIdx<Low(Bookmarks)) or (MarkIdx>High(Bookmarks)) then
    begin ErrorBox('Invalid mark index ('+IntToStr(MarkIdx)+')',nil); Exit; end;

  with Bookmarks[MarkIdx] do
  if Valid=false then
    InformationBox('Mark '+IntToStr(MarkIdx)+' not set.',nil)
  else
    SetCurPtr(Pos.X,Pos.Y);
end;

procedure TCodeEditor.DefineMark(MarkIdx: integer);
begin
  if (MarkIdx<Low(Bookmarks)) or (MarkIdx>High(Bookmarks)) then
    begin
      ErrorBox('Invalid mark index ('+IntToStr(MarkIdx)+')',nil);
      Exit;
    end;
  with Bookmarks[MarkIdx] do
   begin
     Pos:=CurPos;
     Valid:=true;
   end;
end;

procedure TCodeEditor.JumpToLastCursorPos;
{$ifdef Undo}
var
  pa : PEditorAction;
{$endif Undo}
begin
{$ifdef Undo}
  if (UndoList^.count>0) and (RedoList^.count=0) then
    begin
      { Or should we just call Undo ?? PM }
      pa:=UndoList^.At(UndoList^.count-1);
      if (pa^.action=eaMoveCursor) then
        SetCurPtr(pa^.StartPos.X,pa^.StartPos.Y);
    end;
{$else not Undo}
  NotImplemented;
{$endif Undo}
end;

function TCodeEditor.InsertLine: Sw_integer;
var Ind: Sw_integer;
    S,IndentStr: string;
procedure CalcIndent(LineOver: Sw_integer);
begin
  if (LineOver<0) or (LineOver>GetLineCount) then Ind:=0 else
  begin
    IndentStr:=GetLineText(LineOver);
    Ind:=0;
    while (Ind<length(IndentStr)) and (IndentStr[Ind+1]=' ') do
     Inc(Ind);
  end;
  IndentStr:=CharStr(' ',Ind);
end;
var SelBack: sw_integer;
    SCP: TPoint;
    HoldUndo : Boolean;
begin
  if IsReadOnly then begin InsertLine:=-1; Exit; end;
  Lock;
  SCP:=CurPos;
  HoldUndo:=StoreUndo;
  StoreUndo:=false;
  if CurPos.Y<GetLineCount then S:=GetLineText(CurPos.Y) else S:='';
  if Overwrite=false then
  begin
    SelBack:=0;
    if GetLineCount>0 then
    begin
      S:=GetDisplayText(CurPos.Y);
      SelBack:=length(S)-SelEnd.X;
      SetDisplayText(CurPos.Y,RTrim(S));
    end;
    SetDisplayText(CurPos.Y,copy(S,1,CurPos.X-1+1));
    CalcIndent(CurPos.Y);
    Lines^.AtInsert(CurPos.Y+1,NewLine(IndentStr+copy(S,CurPos.X+1,255)));
    LimitsChanged;
(*    if PointOfs(SelStart)<>PointOfs(SelEnd) then { !!! check it - it's buggy !!! }
      begin SelEnd.Y:=CurPos.Y+1; SelEnd.X:=length(GetLineText(CurPos.Y+1))-SelBack; end;*)
    UpdateAttrs(CurPos.Y,attrAll);
    SetCurPtr(Ind,CurPos.Y+1);
{$ifdef Undo}
     StoreUndo:=HoldUndo;
     Addaction(eaInsertLine,SCP,CurPos,IndentStr);
     StoreUndo:=false;
{$endif Undo}
    AdjustSelection(CurPos.X-SCP.X,CurPos.Y-SCP.Y);
  end else
  begin
    CalcIndent(CurPos.Y);
    if CurPos.Y=GetLineCount-1 then
    begin
      Lines^.Insert(NewLine(IndentStr));
      AdjustSelection(0,1);
      LimitsChanged;
{$ifdef Undo}
      StoreUndo:=HoldUndo;
      UpdateAttrs(CurPos.Y,attrAll);
      SetCurPtr(Ind,CurPos.Y+1);
      Addaction(eaInsertLine,SCP,CurPos,IndentStr);
      StoreUndo:=false;
{$endif Undo}
    end
    else
    begin
      UpdateAttrs(CurPos.Y,attrAll);
      StoreUndo:=HoldUndo;
      SetCurPtr(Ind,CurPos.Y+1);
      StoreUndo:=false;
    end;
  end;
  DrawLines(CurPos.Y);
  StoreUndo:=HoldUndo;
  SetModified(true);
  Unlock;
end;

procedure TCodeEditor.BreakLine;
begin
  NotImplemented; Exit;
end;

procedure TCodeEditor.BackSpace;
var S,PreS: string;
    OI,CI,CP,Y,TX: Sw_integer;
    SCP,SC1 : TPoint;
    HoldUndo : Boolean;
begin
  if IsReadOnly then Exit;
  Lock;
  SCP:=CurPos;
  HoldUndo:=StoreUndo;
  StoreUndo:=false;
  if CurPos.X=0 then
   begin
     if CurPos.Y>0 then
      begin
        S:=GetLineText(CurPos.Y-1);
        SetLineText(CurPos.Y-1,S+GetLineText(CurPos.Y));
        SC1.X:=Length(S);SC1.Y:=CurPOS.Y-1;
        StoreUndo:=HoldUndo;
        AddAction(eaDeleteLine,SCP,SC1,GetLineText(CurPos.Y));
        StoreUndo:=false;
        DeleteLine(CurPos.Y);
        LimitsChanged;
        SetCurPtr(length(S),CurPos.Y-1);
      end;
   end
  else
   begin
     S:=GetDisplayText(CurPos.Y);
     CP:=CurPos.X-1;
     if (Flags and efBackspaceUnindents)<>0 then
      if Trim(copy(S,1,CP+1))='' then
      begin
        Y:=CurPos.Y;
        while (Y>0) do
          begin
            Dec(Y);
            PreS:=GetDisplayText(Y);
            if Trim(copy(PreS,1,CP+1))<>'' then Break;
          end;
        if Y<0 then PreS:='';
{        while (CP>0) and
              ( (CP>length(S))    or (S[CP]=' ')     ) and
              ( (CP>length(PreS)) or (PreS[CP]<>' ') ) do
          Dec(CP);}
        TX:=0;
        while (TX<length(PreS)) and (PreS[TX+1]=' ') do
          Inc(TX);
        if TX<CP then CP:=TX;
      end;
     S:=GetLineText(CurPos.Y);
     OI:=LinePosToCharIdx(CurPos.Y,CurPos.X);
     CI:=LinePosToCharIdx(CurPos.Y,CP);
     SetLineText(CurPos.Y,copy(S,1,CI-1)+copy(S,OI,255));
     SetCurPtr(CP,CurPos.Y);
{$ifdef Undo}
     StoreUndo:=HoldUndo;
     Addaction(eaDeleteText,SCP,CurPos,Copy(S,CI,OI-CI));
     StoreUndo:=false;
{$endif Undo}
   end;
  UpdateAttrs(CurPos.Y,attrAll);
  AdjustSelection(CurPos.X-SCP.X,CurPos.Y-SCP.Y);
  DrawLines(CurPos.Y);
  StoreUndo:=HoldUndo;
  SetModified(true);
  Unlock;
end;

procedure TCodeEditor.DelChar;
var S: string;
    SDX,SDY,CI : sw_integer;
    HoldUndo : boolean;
    SCP : TPoint;
begin
  if IsReadOnly then Exit;
  Lock;
  HoldUndo:=StoreUndo;
  StoreUndo:=false;
  S:=GetLineText(CurPos.Y);
  if CurPos.X=length(S) then
   begin
     if CurPos.Y<GetLineCount-1 then
      begin
        SetLineText(CurPos.Y,S+GetLineText(CurPos.Y+1));
        StoreUndo:=HoldUndo;
        SCP.X:=0;SCP.Y:=CurPos.Y+1;
        AddAction(eaDeleteLine,SCP,CurPos,GetLineText(CurPos.Y+1));
        StoreUndo:=false;
        DeleteLine(CurPos.Y+1);
        LimitsChanged;
        SDX:=0; SDY:=-1;
       end;
   end
  else
   begin
     { Problem if S[CurPos.X+1]=TAB !! PM }
     CI:=LinePosToCharIdx(CurPos.Y,CurPos.X);
     if S[CI]=TAB then
       begin
         S:=Copy(S,1,CI-1)+CharStr(' ',TabSize-1)+Copy(S,CI+1,255);
{$ifdef Undo}
         StoreUndo:=HoldUndo;
         Addaction(eaDeleteText,CurPos,CurPos,' ');
         StoreUndo:=false;
{$endif Undo}
       end
     else
       begin
{$ifdef Undo}
         StoreUndo:=HoldUndo;
         Addaction(eaDeleteText,CurPos,CurPos,S[CI]);
         StoreUndo:=false;
{$endif Undo}
         Delete(S,CI,1);
       end;
     SetLineText(CurPos.Y,S);
     SDX:=-1; SDY:=0;
   end;
  SetCurPtr(CurPos.X,CurPos.Y);
  UpdateAttrs(CurPos.Y,attrAll);
  AdjustSelection(SDX,SDY);
  DrawLines(CurPos.Y);
  StoreUndo:=HoldUndo;
  SetModified(true);
  Unlock;
end;

procedure TCodeEditor.DelWord;
var
  SP,EP : TPoint;
  SelSize : sw_integer;
begin
  if IsReadOnly then Exit;
  Lock;
  SP:=SelStart;
  EP:=SelEnd;
  SelectWord;
  SelSize:=SelEnd.X-SelStart.X;
  DelSelect;
  SetSelection(SP,EP);
  AdjustSelectionPos(CurPos.X,CurPos.Y,SelSize,0);
  SetModified(true);
  Unlock;
end;

procedure TCodeEditor.DelStart;
var S: string;
begin
  if IsReadOnly then Exit;
  Lock;
  S:=GetLineText(CurPos.Y);
  if (S<>'') and (CurPos.X<>0) then
  begin
    SetLineText(CurPos.Y,copy(S,LinePosToCharIdx(CurPos.Y,CurPos.X),255));
    SetCurPtr(0,CurPos.Y);
    UpdateAttrs(CurPos.Y,attrAll);
    DrawLines(CurPos.Y);
    SetModified(true);
  end;
  Unlock;
end;

procedure TCodeEditor.DelEnd;
var S: string;
begin
  if IsReadOnly then Exit;
  Lock;
  S:=GetLineText(CurPos.Y);
  if (S<>'') and (CurPos.X<>length(S)) then
  begin
    SetLineText(CurPos.Y,copy(S,1,LinePosToCharIdx(CurPos.Y,CurPos.X)-1));
    SetCurPtr(CurPos.X,CurPos.Y);
    UpdateAttrs(CurPos.Y,attrAll);
    DrawLines(CurPos.Y);
    SetModified(true);
  end;
  Unlock;
end;

procedure TCodeEditor.DelLine;
var
  HoldUndo : boolean;
  SP : TPoint;
begin
  if IsReadOnly then Exit;
  Lock;
  if GetLineCount>0 then
  begin
    SP:=CurPos;
    DeleteLine(CurPos.Y);
    HoldUndo:=StoreUndo;
    StoreUndo:=false;
    LimitsChanged;
    AdjustSelection(0,-1);
    SetCurPtr(0,CurPos.Y);
    UpdateAttrs(Max(0,CurPos.Y-1),attrAll);
    DrawLines(CurPos.Y);
    If HoldUndo then
      with UndoList^.At(UndoList^.count-1)^ do
        begin
          EndPos:=CurPos;
          StartPos:=SP;
        end;
    StoreUndo:=HoldUndo;
    SetModified(true);
  end;
  Unlock;
end;

procedure TCodeEditor.InsMode;
begin
  SetInsertMode(Overwrite);
end;

function  TCodeEditor.GetCurrentWord : string;
const WordChars = ['A'..'Z','a'..'z','0'..'9','_'];
var P : TPoint;
    S : String;
    StartPos,EndPos : byte;
begin
  P:=CurPos;
  S:=GetLineText(P.Y);
  StartPos:=P.X+1;
  EndPos:=StartPos;
  if not (S[StartPos] in WordChars) then
    GetCurrentWord:=''
  else
    begin
       While (StartPos>0) and (S[StartPos-1] in WordChars) do
    Dec(StartPos);
       While (EndPos<Length(S)) and (S[EndPos+1] in WordChars) do
    Inc(EndPos);
       GetCurrentWord:=Copy(S,StartPos,EndPos-StartPos+1);
    end;
end;

procedure TCodeEditor.StartSelect;
var P1,P2: TPoint;
begin
  if ValidBlock=false then
    begin
{      SetSelection(SelStart,Limit);}
      P1:=CurPos; P1.X:=0; P2:=CurPos; {P2.X:=length(GetLineText(P2.Y))+1;}
      SetSelection(P1,P2);
    end
  else
    SetSelection(CurPos,SelEnd);
  if PointOfs(SelEnd)<PointOfs(SelStart) then
     SetSelection(SelStart,SelStart);
  CheckSels;
  DrawView;
end;

procedure TCodeEditor.EndSelect;
var P: TPoint;
    LS: sw_integer;
begin
  P:=CurPos;
{  P.X:=Min(SelEnd.X,length(GetLineText(SelEnd.Y)));}
  LS:=length(GetLineText(SelEnd.Y));
  if LS<P.X then P.X:=LS;
  CheckSels;
  SetSelection(SelStart,P);
  DrawView;
end;

procedure TCodeEditor.DelSelect;
var LineDelta, LineCount, CurLine: Sw_integer;
    StartX,EndX,LastX: Sw_integer;
    S: string;
    SPos : TPoint;
begin
  if IsReadOnly or (ValidBlock=false) then Exit;

  Lock;
  LineCount:=(SelEnd.Y-SelStart.Y)+1;
  LineDelta:=0; LastX:=CurPos.X;
  CurLine:=SelStart.Y;
  while (LineDelta<LineCount) do
  begin
    S:=GetDisplayText(CurLine);
    if LineDelta=0 then StartX:=SelStart.X else StartX:=0;
    if LineDelta=LineCount-1 then EndX:=SelEnd.X else EndX:=length(S);
    if (LineDelta<LineCount-1) and ((StartX=0) and (EndX>=length(S))) then
      begin
      { delete the complete line }
        DeleteLine(CurLine);
        if CurLine>0 then
          LastX:=length(GetDisplayText(CurLine-1))
        else
          LastX:=0;
      end
    else
      begin
        if StoreUndo then
          begin
            SPos.X:=StartX;
            SPos.Y:=CurLine;
            AddAction(eaDeleteText,SPos,SPos,Copy(S,StartX+1,EndX-StartX));
          end;
        SetDisplayText(CurLine,RExpand(copy(S,1,StartX),StartX)+copy(S,EndX+1,255));
        LastX:=StartX;
        if (StartX=0) and (0<LineDelta) and
           not(((LineDelta=LineCount-1) and (StartX=0) and (StartX=EndX))) then
          begin
            S:=GetDisplayText(CurLine-1);
            SetDisplayText(CurLine-1,S+GetLineText(CurLine));
            DeleteLine(CurLine);
            LastX:=length(S);
          end
        else
         Inc(CurLine);
      end;
    Inc(LineDelta);
  end;
  HideSelect;
  SetCurPtr(LastX,CurLine-1);
  UpdateAttrs(CurPos.Y,attrAll);
  DrawLines(CurPos.Y);
  SetModified(true);
  UnLock;
end;

procedure TCodeEditor.HideSelect;
begin
  SetSelection(CurPos,CurPos);
  DrawLines(Delta.Y);
end;

procedure TCodeEditor.CopyBlock;
var Temp: PCodeEditor;
    R: TRect;
begin
  if IsReadOnly or (ValidBlock=false) then Exit;

  Lock;
  GetExtent(R);
  New(Temp, Init(R, nil, nil, nil,0));
  Temp^.InsertFrom(@Self);
  Temp^.SelectAll(true);
  { this selects one line too much because
    we have a empty line at creation to avoid
    negative line problems so we need to decrease SelEnd.Y }
  Dec(Temp^.SelEnd.Y);


  InsertFrom(Temp);
  Dispose(Temp, Done);
  UnLock;
end;

procedure TCodeEditor.MoveBlock;
var Temp: PCodeEditor;
    R: TRect;
    OldPos: TPoint;
begin
  if IsReadOnly then Exit;
  if (SelStart.X=SelEnd.X) and (SelStart.Y=SelEnd.Y) then Exit;
  Lock;
  GetExtent(R);
  New(Temp, Init(R, nil, nil, nil,0));
  Temp^.InsertFrom(@Self);
  OldPos:=CurPos;
  if CurPos.Y>SelStart.Y then
    Dec(OldPos.Y,Temp^.GetLineCount-1);
  DelSelect;
  SetCurPtr(OldPos.X,OldPos.Y);
  InsertFrom(Temp);
  Dispose(Temp, Done);
  UnLock;
end;

procedure TCodeEditor.IndentBlock;
var
  ey,i : Sw_integer;
  S : String;
begin
  if IsReadOnly then Exit;
  if (SelStart.X=SelEnd.X) and (SelStart.Y=SelEnd.Y) then Exit;
  Lock;
  ey:=selend.y;
  if selend.x=0 then
   dec(ey);
  for i:=selstart.y to ey do
   begin
     S:=GetLineText(i);
     SetLineText(i,' '+S);
   end;
  SetCurPtr(CurPos.X,CurPos.Y);
  UpdateAttrsRange(SelStart.Y,SelEnd.Y,attrAll);
  DrawLines(CurPos.Y);
  SetModified(true);
  UnLock;
end;

procedure TCodeEditor.UnindentBlock;
var
  ey,i : Sw_integer;
  S : String;
begin
  if IsReadOnly then Exit;
  if (SelStart.X=SelEnd.X) and (SelStart.Y=SelEnd.Y) then Exit;
  Lock;
  ey:=selend.y;
  if selend.x=0 then
   dec(ey);
  for i:=selstart.y to ey do
   begin
     S:=GetLineText(i);
     if (length(s)>1) and (S[1]=' ') then
      Delete(s,1,1);
     SetLineText(i,S);
   end;
  SetCurPtr(CurPos.X,CurPos.Y);
  UpdateAttrsRange(SelStart.Y,SelEnd.Y,attrAll);
  DrawLines(CurPos.Y);
  SetModified(true);
  UnLock;
end;

procedure TCodeEditor.SelectWord;
const WordChars = ['A'..'Z','a'..'z','0'..'9','_'];
var S : String;
    StartPos,EndPos : byte;
    A,B: TPoint;
begin
  A:=CurPos;
  B:=CurPos;
  S:=GetLineText(A.Y);
  StartPos:=A.X+1;
  EndPos:=StartPos;
  if not (S[StartPos] in WordChars) then
    exit
  else
    begin
       While (StartPos>0) and (S[StartPos-1] in WordChars) do
         Dec(StartPos);
       While (EndPos<Length(S)) and (S[EndPos+1] in WordChars) do
         Inc(EndPos);
       A.X:=StartPos-1;
       B.X:=EndPos;
       SetSelection(A,B);
    end;
end;

procedure TCodeEditor.SelectLine;
var A,B: TPoint;
begin
  if CurPos.Y<GetLineCount then
    begin
      A.Y:=CurPos.Y; A.X:=0;
      B.Y:=CurPos.Y+1; B.X:=0;
      SetSelection(A,B);
    end;
end;

procedure TCodeEditor.WriteBlock;
var FileName: string;
    S: PBufStream;
begin
  if ValidBlock=false then Exit;

  FileName:='';
  if EditorDialog(edWriteBlock, @FileName) <> cmCancel then
  begin
    FileName := FExpand(FileName);

    New(S, Init(FileName, stCreate, 4096));
    if (S=nil) or (S^.Status<>stOK) then
      EditorDialog(edCreateError,@FileName)
    else
      if SaveAreaToStream(S,SelStart,SelEnd)=false then
        EditorDialog(edWriteError,@FileName);
    if Assigned(S) then Dispose(S, Done);
  end;
end;

procedure TCodeEditor.ReadBlock;
var FileName: string;
    S: PBufStream;
    E: PCodeEditor;
    R: TRect;
begin
  FileName:='';
  if EditorDialog(edReadBlock, @FileName) <> cmCancel then
  begin
    FileName := FExpand(FileName);

    New(S, Init(FileName, stOpenRead, 4096));
    if (S=nil) or (S^.Status<>stOK) then
      EditorDialog(edReadError,@FileName)
    else
      begin
        R.Assign(0,0,0,0);
        New(E, Init(R,nil,nil,nil,0));
        if E^.LoadFromStream(S)=false then
          EditorDialog(edReadError,@FileName)
        else
          begin
            E^.SelectAll(true);
            Self.InsertFrom(E);
          end;
        Dispose(E, Done);
      end;
    if Assigned(S) then Dispose(S, Done);
  end;
end;


procedure TCodeEditor.PrintBlock;
begin
  NotImplemented; Exit;
end;

procedure TCodeEditor.ExpandCodeTemplate;
var OSS,OSE: TPoint;
    Line,ShortCut: string;
    X,Y,I,LineIndent: sw_integer;
    CodeLines: PUnsortedStringCollection;
begin
  {
    The usage of editing primitives in this routine make it pretty slow, but
    its speed is still acceptable and they make the implementation of Undo
    much easier... - Gabor
  }
  if IsReadOnly then Exit;

  Lock;

  Line:=GetDisplayText(CurPos.Y);
  X:=CurPos.X; ShortCut:='';
  if X<=length(Line) then
  while (X>0) and (Line[X] in (NumberChars+AlphaChars)) do
  begin
    ShortCut:=Line[X]+ShortCut;
    Dec(X);
  end;

  if ShortCut<>'' then
  begin
    New(CodeLines, Init(10,10));
    if TranslateCodeTemplate(ShortCut,CodeLines) then
    begin
      LineIndent:=X;
      SetCurPtr(X,CurPos.Y);
      for I:=1 to length(ShortCut) do
        DelChar;
      for Y:=0 to CodeLines^.Count-1 do
      begin
        if Y>0 then
          for X:=1 to LineIndent do  { indent template lines to align }
            AddChar(' ');            { them to the first line         }
        Line:=CodeLines^.At(Y)^;
        for X:=1 to length(Line) do
          AddChar(Line[X]);
        if Y<CodeLines^.Count-1 then
          begin
            InsertLine;               { line break }
            while CurPos.X>0 do       { unindent }
            begin
              SetCurPtr(CurPos.X-1,CurPos.Y);
              DelChar;
            end;
          end;
      end;
    end;
    Dispose(CodeLines, Done);
  end;

  UnLock;
end;

procedure TCodeEditor.AddChar(C: char);
const OpenBrackets  : string[10] = '[({';
      CloseBrackets : string[10] = '])}';
var S,SC,TabS: string;
    BI: byte;
    CI,TabStart,LocTabSize : Sw_integer;
    SP: TPoint;
    HoldUndo : boolean;
begin
  if IsReadOnly then Exit;

  Lock;
  SP:=CurPos;
  HoldUndo:=StoreUndo;
  StoreUndo:=false;
  if (C<>TAB) or ((Flags and efUseTabCharacters)<>0) then
    SC:=C
  else
    begin
      LocTabSize:=TabSize - (CurPos.X mod TabSize);
      if (CurPos.Y<=1) or ((Flags and efAutoIndent)=0) then
        SC:=CharStr(' ',LocTabSize)
      else
        begin
          S:=GetLineText(CurPos.Y-1);
          BI:=CurPos.X+1;
          while (BI<=Length(S)) and (S[BI]=' ') do
            inc(BI);
          if (BI=CurPos.X+1) or (BI>Length(S)) then
            SC:=CharStr(' ',LocTabSize)
          else
            SC:=CharStr(' ',BI-CurPos.X-1);
        end;
    end;
  S:=GetLineText(CurPos.Y);
  if CharIdxToLinePos(CurPos.Y,length(S))<CurPos.X then
    begin
      S:=S+CharStr(' ',CurPos.X-CharIdxToLinePos(CurPos.Y,length(S)){-1});
      SetLineText(CurPos.Y,S);
    end;
  CI:=LinePosToCharIdx(CurPos.Y,CurPos.X);
  if (S[CI]=TAB) then
    begin
      TabStart:=CharIdxToLinePos(CurPos.Y,CI);
      if SC=Tab then TabS:=Tab else
        TabS:=CharStr(' ',CurPos.X-TabStart);
      SetLineText(CurPos.Y,copy(S,1,CI-1)+TabS+SC+copy(S,CI+1,255));
      SetCurPtr(CharIdxToLinePos(CurPos.Y,CI+length(TabS)+length(SC)),CurPos.Y);
    end
  else
    begin
      if Overwrite and (CI<=length(S)) then
        SetLineText(CurPos.Y,copy(S,1,CI-1)+SC+copy(S,CI+length(SC),255))
      else
        SetLineText(CurPos.Y,copy(S,1,CI-1)+SC+copy(S,CI,255));
      SetCurPtr(CurPos.X+length(SC),CurPos.Y);
    end;
{$ifdef Undo}
 { must be before CloseBrackets !! }
  StoreUndo:=HoldUndo;
  Addaction(eaInsertText,SP,CurPos,C);
  StoreUndo:=false;
{$endif Undo}
  if ((Flags and efAutoBrackets)<>0) then
    begin
      BI:=Pos(C,OpenBrackets);
      if (BI>0) then
        begin
          StoreUndo:=HoldUndo;
          AddChar(CloseBrackets[BI]);
          StoreUndo:=false;
          SetCurPtr(CurPos.X-1,CurPos.Y);
        end;
    end;
  UpdateAttrs(CurPos.Y,attrAll);
  AdjustSelection(CurPos.X-SP.X,CurPos.Y-SP.Y);
  DrawLines(CurPos.Y);
  StoreUndo:=HoldUndo;
  SetModified(true);
  UnLock;
end;

function TCodeEditor.ClipCopy: Boolean;
var OK: boolean;
begin
  Lock;
  {AddGroupedAction(eaCopy);
   can we undo a copy ??
   maybe as an Undo Paste in Clipboard !! }
  OK:=Clipboard<>nil;
  if OK then OK:=Clipboard^.InsertFrom(@Self);
  ClipCopy:=OK;
  UnLock;
end;

procedure TCodeEditor.ClipCut;
begin
  if IsReadOnly then Exit;
  Lock;
  AddGroupedAction(eaCut);
  DontConsiderShiftState:=true;
  if Clipboard<>nil then
   if Clipboard^.InsertFrom(@Self) then
    begin
      if not IsClipBoard then
       DelSelect;
      SetModified(true);
    end;
  CloseGroupedAction(eaCut);
  UnLock;
  DontConsiderShiftState:=false;
end;

procedure TCodeEditor.ClipPaste;
begin
  if IsReadOnly then Exit;
  DontConsiderShiftState:=true;
  Lock;
  AddGroupedAction(eaPaste);
  if Clipboard<>nil then
   begin
     InsertFrom(Clipboard);
     SetModified(true);
   end;
  CloseGroupedAction(eaPaste);
  UnLock;
  DontConsiderShiftState:=false;
end;

{$ifdef WinClipSupported}
function TCodeEditor.ClipPasteWin: Boolean;
var OK: boolean;
    l,i : longint;
    p,p10,p2,p13 : pchar;
    s : string;
    BPos,EPos,StorePos : TPoint;
    first : boolean;
begin
  Lock;
  OK:=WinClipboardSupported;
  if OK then
    begin
      first:=true;
      StorePos:=CurPos;
      i:=CurPos.Y;
      l:=GetTextWinClipboardSize;
      if l=0 then
        OK:=false
      else
        OK:=GetTextWinClipBoardData(p,l);
      if OK then
        begin
          AddGroupedAction(eaPasteWin);
          p2:=p;
          p13:=strpos(p,#13);
          p10:=strpos(p,#10);
          while assigned(p10) do
            begin
              if p13+1=p10 then
                p13[0]:=#0
              else
                p10[0]:=#0;
              s:=strpas(p2);
              if first then
                begin
                  { we need to cut the line in two
                    if not at end of line PM }
                  InsertLine;
                  SetCurPtr(StorePos.X,StorePos.Y);
                  InsertText(s);
                  first:=false;
                end
              else
                begin
                  Inc(i);
                  Lines^.AtInsert(i,NewLine(s));
                  BPos.X:=0;BPos.Y:=i;
                  EPOS.X:=Length(s);EPos.Y:=i;
                  AddAction(eaInsertLine,BPos,EPos,S);
                end;
              if p13+1=p10 then
                p13[0]:=#13
              else
                p10[0]:=#10;
              p2:=@p10[1];
              p13:=strpos(p2,#13);
              p10:=strpos(p2,#10);
            end;
          if strlen(p2)>0 then
            begin
              s:=strpas(p2);
              if not first then
                SetCurPtr(0,i+1);
              InsertText(s);
            end;
          SetCurPtr(StorePos.X,StorePos.Y);
          SetModified(true);
          UpdateAttrs(StorePos.Y,attrAll);
          CloseGroupedAction(eaPasteWin);
          Update;
          { we must free the allocated memory }
          freemem(p,l);
        end;
    end;
  ClipPasteWin:=OK;
  UnLock;
end;

function TCodeEditor.ClipCopyWin: Boolean;
var OK: boolean;
    p,p2 : pchar;
    s : string;
    i,str_begin,str_end,NumLines,PcLength : longint;
begin
  NumLines:=SelEnd.Y-SelStart.Y;
  if (NumLines>0) or (SelEnd.X>SelStart.X) then
    Inc(NumLines);
  if NumLines=0 then
    exit;
  Lock;
  { First calculate needed size }
  { for newlines first + 1 for terminal #0 }
  PcLength:=Length(EOL)*(NumLines-1)+1;

  { overestimated but can not be that big PM }
  for i:=SelStart.Y to SelEnd.Y do
    PCLength:=PCLength+Length(GetLineText(i));
  getmem(p,PCLength);
  i:=SelStart.Y;
  s:=GetLineText(i);
  str_begin:=LinePosToCharIdx(i,SelStart.X);
  if SelEnd.Y>SelStart.Y then
    str_end:=255
  else
    str_end:=LinePosToCharIdx(i,SelEnd.X)-1;
  s:=copy(s,str_begin,str_end-str_begin+1);
  strpcopy(p,s);
  p2:=strend(p);
  inc(i);
  while i<SelEnd.Y do
    begin
      strpcopy(p2,EOL+GetLineText(i));
      p2:=strend(p2);
      Inc(i);
    end;
  if SelEnd.Y>SelStart.Y then
    begin
      s:=copy(GetLineText(i),1,LinePosToCharIdx(i,SelEnd.X)-1);
      strpcopy(p2,EOL+s);
    end;
  OK:=WinClipboardSupported;
  if OK then
    begin
      OK:=SetTextWinClipBoardData(p,strlen(p));
    end;
  ClipCopyWin:=OK;
  Freemem(p,PCLength);
  UnLock;
end;
{$endif WinClipSupported}

procedure TCodeEditor.Undo;
{$ifdef Undo}
var
  Temp,Idx,Last,Count : Longint;
  Is_grouped : boolean;
{$endif Undo}
begin
{$ifdef Undo}
  StoreUndo := False;
  Lock;
  if UndoList^.count > 0 then
  begin
    Last:=UndoList^.count-1;
    if UndoList^.At(Last)^.Is_grouped_action then
      begin
        Count:=UndoList^.At(Last)^.ActionCount;
        Dec(Last);
        Is_grouped:=true;
      end
    else
      begin
        Count:=1;
        Is_grouped:=false;
      end;
    for Idx:=Last downto Last-Count+1 do
      with UndoList^.At(Idx)^ do
        begin
          case action of
            eaMoveCursor :
              begin
                { move cursor back to original position }
                SetCurPtr(startpos.x,startpos.y);
              end;
            eaInsertText :
              begin
                SetCurPtr(StartPos.X,StartPos.Y);
                if assigned(text) then
                  for Temp := 1 to length(Text^) do
                    DelChar;
              end;
            eaDeleteText :
              begin
                { reinsert deleted text }
                SetCurPtr(EndPos.X,EndPos.Y);
                if assigned(text) then
                  for Temp := 1 to length(Text^) do
                    AddChar(Text^[Temp]);
                SetCurPtr(StartPos.X,StartPos.Y);
              end;
            eaInsertLine :
              begin
                SetCurPtr(EndPos.X,EndPos.Y);
                SetDisplayText(EndPos.Y,Copy(GetDisplayText(EndPos.Y),EndPos.X+1,255));
                BackSpace;
                SetCurPtr(StartPos.X,StartPos.Y);
              end;
            eaDeleteLine :
              begin
                SetCurPtr(EndPos.X,EndPos.Y);
                DelEnd;
                InsertLine;
                SetCurPtr(StartPos.X,StartPos.Y);
                SetLineText(StartPos.Y,GetStr(Text));
              end;
            eaSelectionChanged :
              begin
                { move cursor to end of last set selection }
              end;
          else
            { what the 'ell's an undefined action doing round 'ere mate! }
          end; { once this lot is done paste into redo and modify to suit needs }
          { move item to redo stack }
          RedoList^.Insert(UndoList^.At(Idx));
          UpdateUndoRedo(cmRedo,UndoList^.At(Idx)^.Action);
          UndoList^.atDelete(Idx);
          If Idx>0 then
            UpdateUndoRedo(cmUndo,UndoList^.At(Idx-1)^.Action)
          else
            UpdateUndoRedo(cmUndo,0);
        end;{Idx loop for grouped actions }
      if is_grouped then
        begin
          Idx:=UndoList^.Count-1;
          RedoList^.Insert(UndoList^.At(Idx));
          UpdateUndoRedo(cmRedo,UndoList^.At(Idx)^.Action);
          UndoList^.atDelete(Idx);
          If Idx>0 then
            UpdateUndoRedo(cmUndo,UndoList^.At(Idx-1)^.Action)
          else
            UpdateUndoRedo(cmUndo,0);
        end;
      if UndoList^.count=0 then
        SetCmdState(UndoCmd,false);
      SetCmdState(RedoCmd,true);
      Message(Application,evBroadcast,cmCommandSetChanged,nil);
      DrawView;
    end;
  StoreUndo := True;
  Unlock;
{$else}
  NotImplemented; Exit;
{$endif Undo}
end;

procedure TCodeEditor.Redo;
{$ifdef Undo}
var
  Temp,Idx,Last,Count : Longint;
  Is_grouped : boolean;
{$endif Undo}
begin
{$ifdef Undo}
  StoreUndo := False;
  Lock;
  if RedoList^.count <> 0 then
   begin
    Last:=RedoList^.count-1;
    if RedoList^.At(Last)^.Is_grouped_action then
      begin
        Count:=RedoList^.At(Last)^.ActionCount;
        Dec(Last);
        Is_grouped:=true;
      end
    else
      begin
        Count:=1;
        Is_grouped:=false;
      end;
    for Idx:=Last downto Last-Count+1 do
    with RedoList^.At(Idx)^ do
    begin
      case action of
        eaMoveCursor :
          begin
            { move cursor back to original position }
            SetCurPtr(EndPos.X,EndPos.Y);
          end;
        eaInsertText :
          begin
            SetCurPtr(startpos.x,startpos.y);
            InsertText(GetStr(Text));
          end;
        eaDeleteText :
          begin
            SetCurPtr(EndPos.X,EndPos.Y);
            for Temp := 1 to length(GetStr(Text)) do
              DelChar;
          end;
        eaInsertLine :
          begin
            SetCurPtr(StartPos.X,StartPos.Y);
            InsertLine;
            SetCurPtr(StartPos.X,StartPos.Y);
            InsertText(GetStr(Text));
            SetCurPtr(EndPos.X,EndPos.Y);
          end;
        eaDeleteLine :
          begin
            SetCurPtr(StartPos.X,StartPos.Y);
            DeleteLine(StartPos.Y);
            { SetCurPtr(EndPos.X,EndPos.Y);
            for Temp := 1 to length(GetStr(Text)) do
              DelChar;}
            SetCurPtr(EndPos.X,EndPos.Y);
          end;
        eaSelectionChanged :
          begin
            { move cursor to end of last set test selection }
          end;
      else
        { what the 'ell's an undefined action doing round 'ere mate! }
      end; { once this lot is done paste back into undo and modify to suit needs }
    { move item to undo stack }
      UndoList^.Insert(RedoList^.At(Idx));
      UpdateUndoRedo(cmUndo,RedoList^.At(Idx)^.Action);
      If Idx>0 then
        UpdateUndoRedo(cmRedo,RedoList^.At(Idx-1)^.Action)
      else
        UpdateUndoRedo(cmRedo,0);
      RedoList^.atDelete(Idx);
      end;{ Idx loop for grouped action }
      If is_grouped then
        begin
          Idx:=RedoList^.count-1;
          UndoList^.Insert(RedoList^.At(Idx));
          UpdateUndoRedo(cmUndo,RedoList^.At(Idx)^.Action);
          If Idx>0 then
            UpdateUndoRedo(cmRedo,RedoList^.At(Idx-1)^.Action)
          else
            UpdateUndoRedo(cmRedo,0);
          RedoList^.atDelete(Idx);
        end;
      if RedoList^.count=0 then
        SetCmdState(RedoCmd,false);
      SetCmdState(UndoCmd,true);
      DrawView;
      Message(Application,evBroadcast,cmCommandSetChanged,nil);
    end;
  StoreUndo := True;
  Unlock;
{$else}
  NotImplemented; Exit;
{$endif Undo}
end;

procedure TCodeEditor.GotoLine;
var
  GotoRec: TGotoLineDialogRec;
begin
  with GotoRec do
  begin
    LineNo:='1';
    Lines:=GetLineCount;
    if EditorDialog(edGotoLine, @GotoRec) <> cmCancel then
    begin
      SetCurPtr(0,StrToInt(LineNo)-1);
      TrackCursor(true);
    end;
  end;
end;

procedure TCodeEditor.Find;
var
  FindRec: TFindDialogRec;
  DoConf: boolean;
begin
  with FindRec do
  begin
    Find := FindStr;
    if GetCurrentWord<>'' then
      Find:=GetCurrentWord;
    Options := (FindFlags and ffmOptions) shr ffsOptions;
    Direction := (FindFlags and ffmDirection) shr ffsDirection;
    Scope := (FindFlags and ffmScope) shr ffsScope;
    Origin := (FindFlags and ffmOrigin) shr ffsOrigin;
    DoConf:= (FindFlags and ffPromptOnReplace)<>0;
    if EditorDialog(edFind, @FindRec) <> cmCancel then
    begin
      FindStr := Find;
      FindFlags := (Options shl ffsOptions) or (Direction shl ffsDirection) or
         (Scope shl ffsScope) or (Origin shl ffsOrigin);
      FindFlags := FindFlags and not ffDoReplace;
      if DoConf then
   FindFlags := (FindFlags or ffPromptOnReplace);
      SearchRunCount:=0;
      DoSearchReplace;
    end;
  end;
end;

procedure TCodeEditor.Replace;
var
  ReplaceRec: TReplaceDialogRec;
  Re: word;
begin
  if IsReadOnly then Exit;
  with ReplaceRec do
  begin
    Find := FindStr;
    if GetCurrentWord<>'' then
      Find:=GetCurrentWord;
    Replace := ReplaceStr;
    Options := (FindFlags and ffmOptions) shr ffsOptions;
    Direction := (FindFlags and ffmDirection) shr ffsDirection;
    Scope := (FindFlags and ffmScope) shr ffsScope;
    Origin := (FindFlags and ffmOrigin) shr ffsOrigin;
    Re:=EditorDialog(edReplace, @ReplaceRec);
    if Re <> cmCancel then
    begin
      FindStr := Find;
      ReplaceStr := Replace;
      FindFlags := (Options shl ffsOptions) or (Direction shl ffsDirection) or
         (Scope shl ffsScope) or (Origin shl ffsOrigin);
      FindFlags := FindFlags or ffDoReplace;
      if Re = cmYes then
        FindFlags := FindFlags or ffReplaceAll;
      SearchRunCount:=0;
      DoSearchReplace;
    end;
  end;
end;

procedure TCodeEditor.DoSearchReplace;
var S: string;
    DX,DY,P,Y,X: sw_integer;
    Count: sw_integer;
    Found,CanExit: boolean;
    SForward,DoReplace,DoReplaceAll: boolean;
    LeftOK,RightOK: boolean;
    FoundCount: sw_integer;
    A,B: TPoint;
    AreaStart,AreaEnd: TPoint;
    CanReplace,Confirm: boolean;
    Re: word;
    IFindStr : string;
    BT : BTable;

  function ContainsText(const SubS:string;var S: string; Start: Sw_word): Sw_integer;
  var
    P: Sw_Integer;
  begin
    if Start<=0 then
     P:=0
    else
     begin
       if SForward then
        begin
          if Start>length(s) then
           P:=0
          else if FindFlags and ffCaseSensitive<>0 then
           P:=BMFScan(S[Start],length(s)+1-Start,FindStr,Bt)+1
          else
           P:=BMFIScan(S[Start],length(s)+1-Start,IFindStr,Bt)+1;
          if P>0 then
           Inc(P,Start-1);
        end
       else
        begin
          if start>length(s) then
           start:=length(s);
          if FindFlags and ffCaseSensitive<>0 then
           P:=BMBScan(S[1],Start,FindStr,Bt)+1
          else
           P:=BMBIScan(S[1],Start,IFindStr,Bt)+1;
        end;
     end;
    ContainsText:=P;
  end;

  function InArea(X,Y: sw_integer): boolean;
  begin
    InArea:=((AreaStart.Y=Y) and (AreaStart.X<=X)) or
       ((AreaStart.Y<Y) and (Y<AreaEnd.Y)) or
       ((AreaEnd.Y=Y) and (X<=AreaEnd.X));
  end;
var CurDY: sw_integer;
begin
  Inc(SearchRunCount);

  SForward:=(FindFlags and ffmDirection)=ffForward;
  DoReplace:=(FindFlags and ffDoReplace)<>0;
  Confirm:=(FindFlags and ffPromptOnReplace)<>0;
  DoReplaceAll:=(FindFlags and ffReplaceAll)<>0;
  Count:=GetLineCount; FoundCount:=0;

  if SForward then
    DY:=1
  else
    DY:=-1;
  DX:=DY;

  if (FindFlags and ffmScope)=ffGlobal then
   begin
     AreaStart.X:=0;
     AreaStart.Y:=0;
     AreaEnd.X:=length(GetDisplayText(Count-1));
     AreaEnd.Y:=Count-1;
   end
  else
   begin
     AreaStart:=SelStart;
     AreaEnd:=SelEnd;
   end;

  X:=CurPos.X-DX;
  Y:=CurPos.Y;;
  if SearchRunCount=1 then
    if (FindFlags and ffmOrigin)=ffEntireScope then
      if SForward then
        begin
          X:=AreaStart.X-1;
          Y:=AreaStart.Y;
        end
       else
        begin
          X:=AreaEnd.X+1;
          Y:=AreaEnd.Y;
        end;

  if FindFlags and ffCaseSensitive<>0 then
   begin
     if SForward then
      BMFMakeTable(FindStr,bt)
     else
      BMBMakeTable(FindStr,bt);
   end
  else
   begin
     IFindStr:=Upper(FindStr);
     if SForward then
      BMFMakeTable(IFindStr,bt)
     else
      BMBMakeTable(IFindStr,bt);
   end;

  inc(X,DX);
  CanExit:=false;
  if (DoReplace=false) or ((Confirm=false) and (Owner<>nil)) then
    Owner^.Lock;
  if InArea(X,Y) then
  repeat
    CurDY:=DY;
    S:=GetDisplayText(Y);
    P:=ContainsText(FindStr,S,X+1);
    Found:=P<>0;
    if Found then
      begin
        A.X:=P-1;
        A.Y:=Y;
        B.Y:=Y;
        B.X:=A.X+length(FindStr);
      end;
    Found:=Found and InArea(A.X,A.Y);

    if Found and ((FindFlags and ffWholeWordsOnly)<>0) then
     begin
       LeftOK:=(A.X<=0) or (not( (S[A.X] in AlphaChars) or (S[A.X] in NumberChars) ));
       RightOK:=(B.X>=length(S)) or (not( (S[B.X+1] in AlphaChars) or (S[B.X+1] in NumberChars) ));
       Found:=LeftOK and RightOK;
       if Found=false then
         begin
           CurDY:=0;
           X:=B.X+1;
         end;
     end;

    if Found then
      Inc(FoundCount);

    if Found then
      begin
        Lock;
        if SForward then
         SetCurPtr(B.X,B.Y)
        else
         SetCurPtr(A.X,A.Y);
        TrackCursor(true);
        SetHighlight(A,B);
        UnLock;
        CurDY:=0;
        if (DoReplace=false) then
          begin
            CanExit:=true;
            If SForward then
              begin
                X:=B.X;
                Y:=B.Y;
              end
            else
              begin
                X:=A.X;
                Y:=A.Y;
              end;
          end
        else
          begin
            if Confirm=false then CanReplace:=true else
              begin
                Re:=EditorDialog(edReplacePrompt,@CurPos);
                case Re of
                  cmYes :
                    CanReplace:=true;
                  cmNo :
                    CanReplace:=false;
                  else {cmCancel}
                    begin
                      CanReplace:=false;
                      CanExit:=true;
                    end;
                end;
              end;
            if CanReplace then
              begin
                Lock;
                SetSelection(A,B);
                DelSelect;
                InsertText(ReplaceStr);
                if SForward then
                  begin
                    X:=CurPos.X;
                    Y:=CurPos.Y;
                  end
                else
                  begin
                    X:=A.X;
                    Y:=A.Y;
                  end;
                UnLock;
              end
            else
              begin
                If SForward then
                  begin
                    X:=B.X;
                    Y:=B.Y;
                  end
                else
                  begin
                    X:=A.X;
                    Y:=A.Y;
                  end;
              end;
            if (DoReplaceAll=false) then
              CanExit:=true;
          end;
      end;

    if (CanExit=false) and (CurDY<>0) then
      begin
        inc(Y,CurDY);
        if SForward then
          X:=0
        else
          X:=254;
        CanExit:=(Y>=Count) or (Y<0);
      end;
    if not CanExit then
      CanExit:=not InArea(X,Y);
  until CanExit;
  if (FoundCount=0) or (DoReplace) then
    SetHighlight(CurPos,CurPos);
  if (DoReplace=false) or ((Confirm=false) and (Owner<>nil)) then
    Owner^.UnLock;
  {if (DoReplace=false) or (Confirm=false) then
    UnLock;}
  if (FoundCount=0) then
    EditorDialog(edSearchFailed,nil);
  if (FindFlags and ffmScope)=ffSelectedText then
    { restore selection PM }
    begin
      SetSelection(AreaStart,AreaEnd);
    end;
end;

procedure TCodeEditor.SetInsertMode(InsertMode: boolean);
begin
  if InsertMode then
    Flags:=(Flags or efInsertMode)
  else
    Flags:=(Flags and (not efInsertMode));
  DrawCursor;
end;

procedure TCodeEditor.SetModified(AModified: boolean);
begin
  if AModified<>Modified then
  begin
    Modified:=AModified;
    ModifiedChanged;
  end;
end;

{ there is a problem with ShiftDel here
  because GetShitState tells to extend the
  selection which gives wrong results (PM) }

function TCodeEditor.ShouldExtend: boolean;
var ShiftInEvent: boolean;
begin
  ShiftInEvent:=false;
  if Assigned(CurEvent) then
    if CurEvent^.What=evKeyDown then
      ShiftInEvent:=((CurEvent^.KeyShift and kbShift)<>0);
  ShouldExtend:=ShiftInEvent and
    not DontConsiderShiftState;
end;

procedure TCodeEditor.SetCurPtr(X,Y: sw_integer);
var OldPos,OldSEnd,OldSStart: TPoint;
    Extended: boolean;
begin
  Lock;
  X:=Max(0,Min(MaxLineLength+1,X));
  Y:=Max(0,Min(GetLineCount-1,Y));
  OldPos:=CurPos;
  OldSEnd:=SelEnd;
  OldSStart:=SelStart;
  CurPos.X:=X;
  CurPos.Y:=Y;
  TrackCursor(false);
  if (NoSelect=false) and (ShouldExtend) then
  begin
    CheckSels;
    Extended:=false;
    if PointOfs(OldPos)=PointOfs(SelStart) then
      begin SetSelection(CurPos,SelEnd); Extended:=true; end;
    CheckSels;
    if Extended=false then
     if PointOfs(OldPos)=PointOfs(SelEnd) then
       begin
         if ValidBlock=false then
           SetSelection(CurPos,CurPos);
         SetSelection(SelStart,CurPos); Extended:=true;
       end;
    CheckSels;
    if (Extended=false) then
       if PointOfs(OldPos)<=PointOfs(CurPos)
     then begin SetSelection(OldPos,CurPos); Extended:=true; end
     else begin SetSelection(CurPos,OldPos); Extended:=true; end;
    DrawView;
  end else
   if (Flags and efPersistentBlocks)=0 then
      begin HideSelect; DrawView; end;
{  if PointOfs(SelStart)=PointOfs(SelEnd) then
     SetSelection(CurPos,CurPos);}
  if (Flags and (efHighlightColumn+efHighlightRow))<>0 then
     DrawView;
  if ((CurPos.X<>OldPos.X) or (CurPos.Y<>OldPos.Y)) and
     ((Highlight.A.X<>HighLight.B.X) or (Highlight.A.Y<>HighLight.B.Y)) then
     HideHighlight;
  if (OldPos.Y<>CurPos.Y) and (0<=OldPos.Y) and (OldPos.Y<GetLineCount) then
     SetLineText(OldPos.Y,RTrim(GetLineText(OldPos.Y)));
  if ((CurPos.X<>OldPos.X) or (CurPos.Y<>OldPos.Y)) and (GetErrorMessage<>'') then
    SetErrorMessage('');
  if ((CurPos.X<>OldPos.X) or (CurPos.Y<>OldPos.Y)) and (HighlightRow<>-1) then
    SetHighlightRow(-1);
  if ((CurPos.X<>OldPos.X) or (CurPos.Y<>OldPos.Y)) then
    AddAction(eaMoveCursor,OldPos,CurPos,'');
  if ((CurPos.X<>OldPos.X) or (CurPos.Y<>OldPos.Y)) then
     UpdateIndicator;
  UnLock;
end;

procedure TCodeEditor.CheckSels;
begin
  if (SelStart.Y>SelEnd.Y) or
     ( (SelStart.Y=SelEnd.Y) and (SelStart.X>SelEnd.X) ) then
       SetSelection(SelEnd,SelStart);
end;

procedure TCodeEditor.CodeCompleteApply;
var S: string;
    I: integer;
begin
  Lock;

  { here should be some kind or "mark" or "break" inserted in the Undo
    information, so activating it "undoes" only the completition first and
    doesn't delete the complete word at once... - Gabor }

  S:=GetCodeCompleteFrag;
  SetCurPtr(CurPos.X-length(S),CurPos.Y);
  for I:=1 to length(S) do
    DelChar;
  S:=GetCodeCompleteWord;
  for I:=1 to length(S) do
    AddChar(S[I]);

  UnLock;
  SetCompleteState(csInactive);
end;

procedure TCodeEditor.CodeCompleteCancel;
begin
  SetCompleteState(csDenied);
end;

procedure TCodeEditor.CodeCompleteCheck;
var Line: string;
    X,Y,I: sw_integer;
    CurWord,NewWord: string;
begin
  SetCodeCompleteFrag('');
  if ((Flags and efCodeComplete)=0) or (IsReadOnly=true) then Exit;

  Lock;

  Line:=GetDisplayText(CurPos.Y);
  X:=CurPos.X; CurWord:='';
  if X<=length(Line) then
  while (X>0) and (Line[X] in (NumberChars+AlphaChars)) do
  begin
    CurWord:=Line[X]+CurWord;
    Dec(X);
  end;

  if (length(CurWord)>=CodeCompleteMinLen) and CompleteCodeWord(CurWord,NewWord) then
    begin
      SetCodeCompleteFrag(CurWord);
      SetCodeCompleteWord(NewWord);
    end
  else
    ClearCodeCompleteWord;

  UnLock;
end;

function TCodeEditor.GetCodeCompleteFrag: string;
begin
  GetCodeCompleteFrag:=GetStr(CodeCompleteFrag);
end;

procedure TCodeEditor.SetCodeCompleteFrag(const S: string);
begin
  if Assigned(CodeCompleteFrag) then DisposeStr(CodeCompleteFrag);
  CodeCompleteFrag:=NewStr(S);
end;

function TCodeEditor.UpdateAttrs(FromLine: sw_integer; Attrs: byte): sw_integer;
type
    TCharClass = (ccWhiteSpace,ccTab,ccAlpha,ccNumber,ccRealNumber,ccHash,ccSymbol);
var
  SymbolIndex: Sw_integer;
  CurrentCommentType : Byte;
  FirstCC,LastCC: TCharClass;
  InAsm,InComment,InSingleLineComment,InDirective,InString: boolean;
  X,ClassStart: Sw_integer;
  SymbolConcat: string;
  LineText,Format: string;

  function MatchSymbol(const What, S: string): boolean;
  var Match: boolean;
  begin
    Match:=false;
    if length(What)>=length(S) then
      if copy(What,1+length(What)-length(S),length(S))=S then
    Match:=true;
    MatchSymbol:=Match;
  end;

  var MatchedSymbol: boolean;
      MatchingSymbol: string;
  type TPartialType = (pmNone,pmLeft,pmRight,pmAny);
  function MatchesAnySpecSymbol(What: string; SClass: TSpecSymbolClass; PartialMatch: TPartialType;
           CaseInsensitive: boolean): boolean;
  var S: string;
      I: Sw_integer;
      Match,Found: boolean;
  begin
    Found:=false;
    if CaseInsensitive then
      What:=UpcaseStr(What);
    if What<>'' then
    for I:=1 to GetSpecSymbolCount(SClass) do
    begin
      SymbolIndex:=I;
      S:=GetSpecSymbol(SClass,I-1);
      if (length(What)<length(S)) or
         ((PartialMatch=pmNone) and (length(S)<>length(What)))
          then
        Match:=false
      else
        begin
          if CaseInsensitive then
            S:=UpcaseStr(S);
          case PartialMatch of
            pmNone : Match:=What=S;
            pmRight:
              Match:=copy(What,length(What)-length(S)+1,length(S))=S;
          else Match:=MatchSymbol(What,S);
          end;
        end;
      if Match then
      begin
        MatchingSymbol:=S; Found:=true; Break;
      end;
    end;
    MatchedSymbol:=MatchedSymbol or Found;
    MatchesAnySpecSymbol:=Found;
  end;

  function IsCommentPrefix: boolean;
  begin
    IsCommentPrefix:=MatchesAnySpecSymbol(SymbolConcat,ssCommentPrefix,pmLeft,false);
  end;

  function IsSingleLineCommentPrefix: boolean;
  begin
    IsSingleLineCommentPrefix:=MatchesAnySpecSymbol(SymbolConcat,ssCommentSingleLinePrefix,pmLeft,false);
  end;

  function IsCommentSuffix: boolean;
  begin
    IsCommentSuffix:=(MatchesAnySpecSymbol(SymbolConcat,ssCommentSuffix,pmRight,false))
      and (CurrentCommentType=SymbolIndex);
  end;

  function IsStringPrefix: boolean;
  begin
    IsStringPrefix:=MatchesAnySpecSymbol(SymbolConcat,ssStringPrefix,pmLeft,false);
  end;

  function IsStringSuffix: boolean;
  begin
    IsStringSuffix:=MatchesAnySpecSymbol(SymbolConcat,ssStringSuffix,pmRight,false);
  end;

  function IsDirectivePrefix: boolean;
  begin
    IsDirectivePrefix:=MatchesAnySpecSymbol(SymbolConcat,ssDirectivePrefix,pmLeft,false);
  end;

  function IsDirectiveSuffix: boolean;
  begin
    IsDirectiveSuffix:=MatchesAnySpecSymbol(SymbolConcat,ssDirectiveSuffix,pmRight,false);
  end;

  function IsAsmPrefix(const WordS: string): boolean;
  begin
    IsAsmPrefix:=MatchesAnySpecSymbol(WordS,ssAsmPrefix,pmNone,true);
  end;

  function IsAsmSuffix(const WordS: string): boolean;
  begin
    IsAsmSuffix:=MatchesAnySpecSymbol(WordS,ssAsmSuffix,pmNone,true);
  end;

  function GetCharClass(C: char): TCharClass;
  var CC: TCharClass;
  begin
    if C in WhiteSpaceChars then CC:=ccWhiteSpace else
    if C in TabChars then CC:=ccTab else
    if C in HashChars then CC:=ccHash else
    if C in AlphaChars then CC:=ccAlpha else
    if C in NumberChars then CC:=ccNumber else
    if (LastCC=ccNumber) and (C in RealNumberChars) then
      begin
        if (C='.') then
          begin
            if (LineText[X+1]='.') then
              cc:=ccSymbol
            else
              CC:=ccRealNumber
          end
        else
          cc:=ccrealnumber;
      end else
    CC:=ccSymbol;
    GetCharClass:=CC;
  end;

  procedure FormatWord(SClass: TCharClass; StartX:Sw_integer;EndX: Sw_integer);
  var
      C: byte;
      WordS: string;
  begin
    C:=0;
    WordS:=copy(LineText,StartX,EndX-StartX+1);
    if IsAsmSuffix(WordS) and (InAsm=true) and (InComment=false) and
       (InString=false) and (InDirective=false) then InAsm:=false;
    if InDirective then C:=coDirectiveColor else
    if InComment then C:=coCommentColor else
    if InString then C:=coStringColor else
    if InAsm then C:=coAssemblerColor else
    case SClass of
      ccWhiteSpace : C:=coWhiteSpaceColor;
      ccTab : C:=coTabColor;
      ccNumber :
        if copy(WordS,1,1)='$' then
          C:=coHexNumberColor
        else
          C:=coNumberColor;
      ccHash :
        C:=coStringColor;
      ccSymbol :
        C:=coSymbolColor;
      ccAlpha :
        begin
          if IsReservedWord(WordS) then
            C:=coReservedWordColor
          else
            C:=coIdentifierColor;
        end;
    end;
    if EndX+1>=StartX then
      FillChar(Format[StartX],EndX+1-StartX,C);
    if IsAsmPrefix(WordS) and
       (InAsm=false) and (InComment=false) and (InDirective=false) then
      InAsm:=true;
  end;

  procedure ProcessChar(C: char);
  var CC: TCharClass;
      EX: Sw_integer;
  begin
    CC:=GetCharClass(C);
    if ClassStart=X then
      FirstCC:=CC;
    if ( (CC<>LastCC) and
        (
         ((FirstCC=ccNumber) and (CC<>ccRealNumber)) or
        (((CC<>ccAlpha) or (LastCC<>ccNumber) ) and
          ( (CC<>ccNumber) or (LastCC<>ccAlpha) ) and
          ( (CC<>ccNumber) or (LastCC<>ccHash) ) and
          ( (CC<>ccRealNumber) or (LastCC<>ccNumber))
         ))) or

       (X>length(LineText)) or (CC=ccSymbol) then
    begin
      MatchedSymbol:=false;
      EX:=X-1;
      if (CC=ccSymbol) then
       begin
         if length(SymbolConcat)>=High(SymbolConcat) then
           Delete(SymbolConcat,1,1);
         SymbolConcat:=SymbolConcat+C;
       end;
      case CC of
        ccSymbol :
          if IsCommentSuffix and (InComment) then
             Inc(EX) else
          if IsStringSuffix and (InString) then
             Inc(EX) else
          if IsDirectiveSuffix and (InDirective) then
             Inc(EX);
      end;
      if (C='$') and (MatchedSymbol=false) and (IsDirectivePrefix=false) then
        CC:=ccNumber;
      if CC<>ccSymbol then SymbolConcat:='';
      FormatWord(LastCC,ClassStart,EX);
      ClassStart:=EX+1;
      case CC of
        ccAlpha  : ;
        ccNumber :
          if (LastCC<>ccAlpha) then;
        ccSymbol :
            if IsDirectivePrefix and {(InComment=false) and }(InDirective=false) then
               begin InDirective:=true; InComment:=false; Dec(ClassStart,length(MatchingSymbol)-1); end else
            if IsDirectiveSuffix and (InComment=false) and (InDirective=true) then
               InDirective:=false else
            if IsCommentPrefix and (InComment=false) and (InString=false) then
              begin
                InComment:=true;
                CurrentCommentType:=SymbolIndex;
                InSingleLineComment:=IsSingleLineCommentPrefix;
                {InString:=false; }
                Dec(ClassStart,length(MatchingSymbol)-1);
              end
            else
           if IsCommentSuffix and (InComment) then
             begin InComment:=false; InString:=false; end else
           if IsStringPrefix and (InComment=false) and (InString=false) then
             begin InString:=true; Dec(ClassStart,length(MatchingSymbol)-1); end else
           if IsStringSuffix and (InComment=false) and (InString=true) then
             InString:=false;
      end;
      if MatchedSymbol and (InComment=false) then
        SymbolConcat:='';
      LastCC:=CC;
    end;
  end;

var CurLine: Sw_integer;
    Line,NextLine,PrevLine,OldLine: PLine;
begin
{$ifdef TEST_PARTIAL_SYNTAX}
  If ((Flags and efSyntaxHighlight)<>0) and (LastSyntaxedLine<FromLine)
     and (FromLine<GetLineCount) then
    FromLine:=UpdateAttrsRange(LastSyntaxedLine,FromLine,Attrs);
{$endif TEST_PARTIAL_SYNTAX}
  if ((Flags and efSyntaxHighlight)=0) or (FromLine>=GetLineCount) then
  begin
    SetLineFormat(FromLine,'');
    UpdateAttrs:=GetLineCount;
{$ifdef TEST_PARTIAL_SYNTAX}
    LastSyntaxedLine:=GetLineCount;
    SyntaxComplete:=true;
{$endif TEST_PARTIAL_SYNTAX}
    UpdateIndicator;
    Exit;
  end;
  CurLine:=FromLine;
  if CurLine>0 then PrevLine:=Lines^.At(CurLine-1) else PrevLine:=nil;
  repeat
    Line:=Lines^.At(CurLine);
    InSingleLineComment:=false;
    if PrevLine<>nil then
     begin
       InAsm:=PrevLine^.EndsWithAsm;
       InComment:=PrevLine^.EndsWithComment and not PrevLine^.EndsInSingleLineComment;
       CurrentCommentType:=PrevLine^.EndCommentType;
       InDirective:=PrevLine^.EndsWithDirective;
     end
    else
     begin
       InAsm:=false;
       InComment:=false;
       CurrentCommentType:=0;
       InDirective:=false;
     end;
    OldLine:=Line;
    Line^.BeginsWithAsm:=InAsm;
    Line^.BeginsWithComment:=InComment;
    Line^.BeginsWithDirective:=InDirective;
    LineText:=GetLineText(CurLine);
    Format:=CharStr(chr(coTextColor),length(LineText));
    LastCC:=ccWhiteSpace;
    ClassStart:=1;
    SymbolConcat:='';
    InString:=false;
    if LineText<>'' then
     begin
       for X:=1 to length(LineText) do
         ProcessChar(LineText[X]);
       Inc(X);
       ProcessChar(' ');
     end;
    SetLineFormat(CurLine,Format);
    Line^.EndsWithAsm:=InAsm;
    Line^.EndsWithComment:=InComment;
    Line^.EndsInSingleLineComment:=InSingleLineComment;
    Line^.EndCommentType:=CurrentCommentType;
    Line^.EndsWithDirective:=InDirective;
    Inc(CurLine);
    if CurLine>=GetLineCount then
     Break;
    NextLine:=Lines^.At(CurLine);
    if (Attrs and attrForceFull)=0 then
      if (InAsm=false) and (NextLine^.BeginsWithAsm=false) and
         (InComment=false) and (NextLine^.BeginsWithComment=false) and
         (InDirective=false) and (NextLine^.BeginsWithDirective=false) and
         (OldLine^.EndsWithComment=Line^.EndsWithComment) and
         (OldLine^.EndsWithAsm=Line^.EndsWithAsm) and
         (OldLine^.EndsWithDirective=Line^.EndsWithDirective) and
         (NextLine^.BeginsWithAsm=Line^.EndsWithAsm) and
         (NextLine^.BeginsWithComment=Line^.EndsWithComment) and
         (NextLine^.BeginsWithDirective=Line^.EndsWithDirective) and
         (NextLine^.Format<>nil) then
       Break;
{$ifdef TEST_PARTIAL_SYNTAX}
    if not SyntaxComplete then
      if ((Attrs and attrForceFull)=0) and
         (CurLine>Delta.Y+Size.Y) then
        break;
{$endif TEST_PARTIAL_SYNTAX}
    PrevLine:=Line;
  until false;
  UpdateAttrs:=CurLine;
{$ifdef TEST_PARTIAL_SYNTAX}
  If LastSyntaxedLine<CurLine-1 then
    LastSyntaxedLine:=CurLine-1;
  if CurLine=GetLineCount then
    begin
      SyntaxComplete:=true;
      UpdateIndicator;
    end;
{$endif TEST_PARTIAL_SYNTAX}
end;


function TCodeEditor.UpdateAttrsRange(FromLine, ToLine: sw_integer; Attrs: byte): sw_integer;
var Line: Sw_integer;
begin
  Lock;
  Line:=FromLine;
  repeat
    Line:=UpdateAttrs(Line,Attrs);
  until (Line>=GetLineCount) or (Line>ToLine);
  UpdateAttrsRange:=Line;
  Unlock;
end;


procedure TCodeEditor.DrawLines(FirstLine: sw_integer);
begin
  if FirstLine>=(Delta.Y+Size.Y) then Exit; { falls outside of the screen }
  DrawView;
end;

function TCodeEditor.InsertText(const S: string): Boolean;
var I: sw_integer;
    OldPos: TPoint;
    HoldUndo : boolean;
begin
  Lock;
  OldPos:=CurPos;
  HoldUndo:=StoreUndo;
  StoreUndo:=false;
  for I:=1 to length(S) do
    AddChar(S[I]);
  InsertText:=true;
  StoreUndo:=HoldUndo;
  AddAction(eaInsertText,OldPos,CurPos,S);
  UnLock;
end;

function TCodeEditor.InsertFrom(Editor: PCodeEditor): Boolean;
var OK: boolean;
    LineDelta,LineCount: Sw_integer;
    StartPos,DestPos,BPos,EPos: TPoint;
    LineStartX,LineEndX: Sw_integer;
    S,OrigS,AfterS: string;
    VerticalBlock: boolean;
    SEnd: TPoint;
begin
  if (Editor^.Flags and efVerticalBlocks)<>0 then
    begin
      NotImplemented;
      Exit;
    end;
  Lock;
  OK:=(Editor^.SelStart.X<>Editor^.SelEnd.X) or (Editor^.SelStart.Y<>Editor^.SelEnd.Y);
  if OK then
  begin
    StartPos:=CurPos; DestPos:=CurPos;
    EPos:=CurPos;
    VerticalBlock:=(Editor^.Flags and efVerticalBlocks)<>0;
    LineDelta:=0; LineCount:=(Editor^.SelEnd.Y-Editor^.SelStart.Y)+1;
    OK:=GetLineCount<MaxLineCount;
    OrigS:=GetDisplayText(DestPos.Y);
    AfterS:=Copy(OrigS,DestPos.X+1,255);

    while OK and (LineDelta<LineCount) do
    begin
      if (LineDelta<LineCount-1) and (VerticalBlock=false) then
      if (LineDelta<>0) or (Editor^.SelEnd.X=0) then
        begin
          Lines^.AtInsert(DestPos.Y,NewLine(''));
          BPos.X:=0;BPos.Y:=DestPos.Y;
          EPOS.X:=0;EPos.Y:=DestPos.Y;
          AddAction(eaInsertLine,BPos,EPos,'');
          LimitsChanged;
        end;

      if (LineDelta=0) or VerticalBlock then
        LineStartX:=Editor^.SelStart.X
      else
        LineStartX:=0;

      if (LineDelta=LineCount-1) or VerticalBlock then
        LineEndX:=Editor^.SelEnd.X-1
      else
        LineEndX:=255;

      if LineEndX<LineStartX then
        S:=''
      else if VerticalBlock then
        S:=RExpand(copy(Editor^.GetLineText(Editor^.SelStart.Y+LineDelta),LineStartX+1,LineEndX-LineStartX+1),
                   Min(LineEndX-LineStartX+1,255))
      else
        S:=copy(Editor^.GetLineText(Editor^.SelStart.Y+LineDelta),LineStartX+1,LineEndX-LineStartX+1);
      if VerticalBlock=false then
        begin
          If LineDelta>0 then
            OrigS:='';
          if LineDelta=LineCount-1 then
            begin
              SetLineText(DestPos.Y,RExpand(copy(OrigS,1,DestPos.X),DestPos.X)+S+AfterS);
              BPos.X:=DestPos.X;BPos.Y:=DestPos.Y;
              EPOS.X:=DestPos.X+Length(S);EPos.Y:=DestPos.Y;
              AddAction(eaInsertText,BPos,EPos,S);
            end
          else
            begin
              SetLineText(DestPos.Y,RExpand(copy(OrigS,1,DestPos.X),DestPos.X)+S);
              BPos.X:=DestPos.X;BPos.Y:=DestPos.Y;
              EPOS.X:=DestPos.X+Length(S);EPos.Y:=DestPos.Y;
              AddAction(eaInsertText,BPos,EPos,S);
            end;
          if LineDelta=LineCount-1 then
            begin
              SEnd.Y:=DestPos.Y;
              SEnd.X:=DestPos.X+length(S);
            end
          else
           begin
             Inc(DestPos.Y);
             DestPos.X:=0;
           end;
        end
      else { if VerticalBlock=false then .. else }
        begin
          { this is not yet implemented !! PM }
          S:=RExpand(S,LineEndX-LineStartX+1);
        end;
      Inc(LineDelta);
      OK:=GetLineCount<MaxLineCount;
    end;
    if OK=false then EditorDialog(edTooManyLines,nil);
    { mainly to force eaMove insertion }
    if not IsClipboard then
      SetCurPtr(EPos.X,EPos.Y);
    SetCurPtr(StartPos.X,StartPos.Y);
    UpdateAttrs(StartPos.Y,attrAll);
    SetModified(true);
    LimitsChanged;
    SetSelection(CurPos,SEnd);
    if IsClipboard then
     begin
       Inc(DestPos.X,length(S));
       SetCurPtr(DestPos.X,DestPos.Y);
     end;
    DrawView;
  end;
  UnLock;
  InsertFrom:=OK;
end;

function TCodeEditor.IsClipboard: Boolean;
begin
  IsClipboard:=(Clipboard=@Self);
end;

procedure TCodeEditor.HideHighlight;
begin
  SetHighlight(CurPos,CurPos);
end;

procedure TCodeEditor.AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string);
{$ifdef Undo}
var
  ActionIntegrated : boolean;
  pa : PEditorAction;
  S : String;
{$endif Undo}
begin
{$ifdef Undo}
  if (UndoList=nil) or (not StoreUndo) then Exit;
  ActionIntegrated:=false;
  if UndoList^.count>0 then
    begin
      pa:=UndoList^.At(UndoList^.count-1);
      if (pa^.action=AAction) and
         (pa^.EndPos.X=AStartPos.X) and
         (pa^.EndPos.Y=AStartPos.Y) and
         { do not group InsertLine and DeleteLine !! }
         ((AAction=eaMoveCursor) or
          (AAction=eaInsertText) or
          (AAction=eaDeleteText))
         then
        begin
          pa^.EndPos:=AEndPos;
          S:=GetStr(pa^.text);
          if S<>'' then
           DisposeStr(pa^.text);
          pa^.text:=NewStr(S+AText);
          ActionIntegrated:=true;
        end;
    end;
  if not ActionIntegrated then
    begin
      UndoList^.Insert(New(PEditorAction,Init(AAction,AStartPos,AEndPos,AText)));
      if assigned(UndoList^.CurrentGroupedAction) then
        Inc(UndoList^.CurrentGroupedAction^.actionCount);
      UpdateUndoRedo(cmUndo,AAction);
    end;
  if UndoList^.count>0 then
  begin
    SetCmdState(UndoCmd,true);
    SetCmdState(RedoCmd,false);
    Message(Application,evBroadcast,cmCommandSetChanged,nil);
    UpdateUndoRedo(cmRedo,0);
    RedoList^.FreeAll;
  end;
{$endif Undo}
end;

procedure TCodeEditor.AddGroupedAction(AAction : byte);
begin
{$ifdef Undo}
  UndoList^.CurrentGroupedAction:=New(PEditorAction,Init_group(AAction));
{$endif Undo}
end;

procedure TCodeEditor.CloseGroupedAction(AAction : byte);
begin
{$ifdef Undo}
  UndoList^.Insert(UndoList^.CurrentGroupedAction);
  UndoList^.CurrentGroupedAction:=nil;
  UpdateUndoRedo(cmUndo,AAction);
{$endif Undo}
end;


function TCodeEditor.ValidBlock: boolean;
begin
  ValidBlock:=(SelStart.X<>SelEnd.X) or (SelStart.Y<>SelEnd.Y);
end;

procedure TCodeEditor.SetSelection(A, B: TPoint);
var WV: boolean;
    OS,OE: TPoint;
begin
  WV:=ValidBlock;
  OS:=SelStart; OE:=SelEnd;
  SelStart:=A; SelEnd:=B;
  if (WV=false) and (ValidBlock=false) then { do nothing } else
    if (OS.X<>SelStart.X) or (OS.Y<>SelStart.Y) or
       (OE.X<>SelEnd.X) or (OE.Y<>SelEnd.Y) then
     SelectionChanged;
end;

procedure TCodeEditor.SetHighlight(A, B: TPoint);
begin
  Highlight.A:=A; Highlight.B:=B;
  HighlightChanged;
end;

procedure TCodeEditor.SetHighlightRow(Row: sw_integer);
begin
  HighlightRow:=Row;
  DrawView;
end;

procedure TCodeEditor.SetDebuggerRow(Row: sw_integer);
begin
  DebuggerRow:=Row;
  DrawView;
end;

procedure TCodeEditor.SetCompleteState(AState: TCompleteState);
begin
  if AState<>CompleteState then
  begin
    CompleteState:=AState;
    if CompleteState<>csOffering then
      ClearCodeCompleteWord;
  end;
end;

procedure TCodeEditor.SelectAll(Enable: boolean);
var A,B: TPoint;
begin
  if (Enable=false) or (GetLineCount=0) then
     begin A:=CurPos; B:=CurPos end
  else
     begin
       A.X:=0; A.Y:=0;
{       B.Y:=GetLineCount-1;
       B.X:=length(GetLineText(B.Y));}
       B.Y:=GetLineCount; B.X:=0;
     end;
  SetSelection(A,B);
  DrawView;
end;

procedure TCodeEditor.SelectionChanged;
var Enable,CanPaste: boolean;
begin
  if GetLineCount=0 then
    begin
      SelStart.X:=0; SelStart.Y:=0; SelEnd:=SelStart;
    end
  else
    if SelEnd.Y>GetLineCount-1 then
     if (SelEnd.Y<>GetLineCount) or (SelEnd.X<>0) then
      begin
        SelEnd.Y:=GetLineCount-1;
        SelEnd.X:=length(GetDisplayText(SelEnd.Y));
      end;

  Enable:=((SelStart.X<>SelEnd.X) or (SelStart.Y<>SelEnd.Y)) and (Clipboard<>nil);
  SetCmdState(ToClipCmds,Enable and (Clipboard<>@Self));
  SetCmdState(NulClipCmds,Enable);
  CanPaste:=(Clipboard<>nil) and ((Clipboard^.SelStart.X<>Clipboard^.SelEnd.X) or
       (Clipboard^.SelStart.Y<>Clipboard^.SelEnd.Y));
  SetCmdState(FromClipCmds,CanPaste  and (Clipboard<>@Self));
{$ifdef WinClipSupported}
  SetCmdState(FromWinClipCmds,GetTextWinClipboardSize>0);
{$endif WinClipSupported}
  SetCmdState(UndoCmd,(UndoList^.count>0));
  SetCmdState(RedoCmd,(RedoList^.count>0));
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
  DrawView;
end;

procedure TCodeEditor.HighlightChanged;
begin
  DrawView;
end;

procedure TCodeEditor.ModifiedChanged;
begin
  UpdateIndicator;
end;

procedure TCodeEditor.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState,Enable);
  if (AState and (sfActive+sfSelected+sfFocused))<>0 then
    begin
      SelectionChanged;
      if ((State and sfFocused)=0) and (CompleteState=csOffering) then
        ClearCodeCompleteWord;
    end;
end;

function TCodeEditor.GetPalette: PPalette;
const P: string[length(CEditor)] = CEditor;
begin
  GetPalette:=@P;
end;

constructor TCodeEditor.Load(var S: TStream);
var TS: PSubStream;
    TSize: longint;
begin
  inherited Load(S);

  New(UndoList,init(500,1000));
  New(RedoList,init(500,1000));

  New(Lines, Init(500,1000));
  { we have always need at least 1 line }
  Lines^.Insert(NewLine(''));

  GetPeerViewPtr(S,Indicator);
  S.Read(Flags,SizeOf(Flags));
  S.Read(TabSize,SizeOf(TabSize));

  if (Flags and efStoreContent)<>0 then
    begin
      S.Read(TSize,SizeOf(TSize));
      New(TS, Init(@S,S.GetPos,TSize));
{$ifdef TEST_PARTIAL_SYNTAX}
      SyntaxComplete:=false;
{$endif TEST_PARTIAL_SYNTAX}
      LoadFromStream(TS);
      Dispose(TS, Done);
    end;

  S.Read(SelStart,SizeOf(SelStart));
  S.Read(SelEnd,SizeOf(SelEnd));
  S.Read(Highlight,SizeOf(Highlight));
  S.Read(CurPos,SizeOf(CurPos));
  S.Read(StoreUndo,SizeOf(StoreUndo));
  S.Read(IsReadOnly,SizeOf(IsReadOnly));
  S.Read(NoSelect,SizeOf(NoSelect));
  S.Read(HighlightRow,SizeOf(HighlightRow));
  SetDebuggerRow(-1);

  LimitsChanged;
  SelectionChanged; HighlightChanged;
  UpdateIndicator;
end;

procedure TCodeEditor.Store(var S: TStream);
var NS: TNulStream;
    TSize: longint;
begin
  inherited Store(S);

  PutPeerViewPtr(S,Indicator);
  S.Write(Flags,SizeOf(Flags));
  S.Write(TabSize,SizeOf(TabSize));

  if (Flags and efStoreContent)<>0 then
    begin
      NS.Init;
      SaveToStream(@NS);
      TSize:=NS.GetSize;
      NS.Done;

      S.Write(TSize,SizeOf(TSize));
      SaveToStream(@S);
    end;

  S.Write(SelStart,SizeOf(SelStart));
  S.Write(SelEnd,SizeOf(SelEnd));
  S.Write(Highlight,SizeOf(Highlight));
  S.Write(CurPos,SizeOf(CurPos));
  S.Write(StoreUndo,SizeOf(StoreUndo));
  S.Write(IsReadOnly,SizeOf(IsReadOnly));
  S.Write(NoSelect,SizeOf(NoSelect));
  S.Write(HighlightRow,SizeOf(HighlightRow));
end;

function TCodeEditor.LoadFromStream(Stream: PStream): boolean;
var S: string;
    OK: boolean;
begin
  DeleteAllLines;
  OK:=(Stream^.Status=stOK);
  if eofstream(Stream) then
   AddLine('')
  else
   while OK and (eofstream(Stream)=false) and (GetLineCount<MaxLineCount) do
   begin
     readlnfromstream(Stream,S);
     OK:=OK and (Stream^.Status=stOK);
     if OK then AddLine(S);
   end;
  LimitsChanged;
  if (Flags and efSyntaxHighlight)<>0 then
    UpdateAttrsRange(0,Min(Delta.Y+Size.Y,GetLineCount-1),
      attrAll
{$ifndef TEST_PARTIAL_SYNTAX}
      +attrForceFull
{$endif TEST_PARTIAL_SYNTAX}
      );
  TextStart;
  LoadFromStream:=OK;
end;

function TCodeEditor.SaveToStream(Stream: PStream): boolean;
var A,B: TPoint;
begin
  A.Y:=0; A.X:=0;
  B.Y:=GetLineCount-1;
  if GetLineCount>0 then
    B.X:=length(GetDisplayText(B.Y))
  else
    B.X:=0;
  SaveToStream:=SaveAreaToStream(Stream,A,B);
end;

function TCodeEditor.SaveAreaToStream(Stream: PStream; StartP,EndP: TPoint): boolean;
var S: string;
    OK: boolean;
    Line: Sw_integer;
    P: PLine;
begin
  if EndP.X=0 then
    begin
      if EndP.Y>0 then
        begin
          EndP.X:=length(GetDisplayText(EndP.Y));
        end
      else
        EndP.X:=0;
    end
  else
    Dec(EndP.X);
  OK:=(Stream^.Status=stOK); Line:=StartP.Y;
  while OK and (Line<=EndP.Y) and (Line<GetLineCount) do
  begin
    P:=Lines^.At(Line);
    if P^.Text=nil then S:='' else
      begin
        S:=P^.Text^;
        if Line=EndP.Y then S:=copy(S,1,EndP.X+1);
        if Line=StartP.Y then S:=copy(S,StartP.Y+1,255);
      end;
    { Remove all traling spaces PM }
    if (Flags and efKeepTrailingSpaces)=0 then
      While (Length(S)>0) and (S[Length(S)]=' ') do
       Dec(S[0]);
    if (Flags and efUseTabCharacters)<>0 then
      S:=CompressUsingTabs(S,TabSize);
    Stream^.Write(S[1],length(S));
    Stream^.Write(EOL[1],length(EOL));
    Inc(Line);
    OK:=OK and (Stream^.Status=stOK);
  end;
  SaveAreaToStream:=OK;
end;

destructor TCodeEditor.Done;
begin
  inherited Done;
  if assigned(Lines) then
    Dispose(Lines, Done);
  If assigned(RedoList) then
      Dispose(RedoList,done);
  If assigned(UndoList) then
      Dispose(UndoList,done);
  if Assigned(CodeCompleteFrag) then
    DisposeStr(CodeCompleteFrag);
  if Assigned(CodeCompleteWord) then
    DisposeStr(CodeCompleteWord);
end;

{$ifdef Undo}

constructor TEditorAction.init(act:byte; StartP,EndP:TPoint;Txt:String);
begin
  Action:=act;
  StartPos:=StartP;
  EndPos:=EndP;
  Text:=NewStr(txt);
  ActionCount:=0;
end;

constructor TEditorAction.init_group(act:byte);
begin
  Action:=act;
  ActionCount:=0;
end;

function TEditorAction.Is_grouped_action : boolean;
begin
  Is_grouped_action:=Action in [eaCut,eaPaste,eaPasteWin,eaClear];
end;

destructor TEditorAction.done;
begin
  DisposeStr(Text);
end;

{$else}
procedure TEditorActionCollection.FreeItem(Item: Pointer);
begin
  if assigned(Item) then
    freemem(Item,Sizeof(TEditorAction));
end;
{$endif Undo}

function TEditorActionCollection.At(Idx : sw_integer) : PEditorAction;
begin
  At:=PEditorAction(Inherited At(Idx));
end;

constructor TFileEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
       PScrollBar; AIndicator: PIndicator;const AFileName: string);
begin
  inherited Init(Bounds,AHScrollBAr,AVScrollBAr,AIndicator,0);
  FileName:=AFileName;
  UpdateIndicator;
  Message(@Self,evBroadcast,cmFileNameChanged,@Self);
  OnDiskLoadTime:=-1;
end;

function TFileEditor.LoadFile: boolean;
var S: PBufStream;
    OK: boolean;
begin
  New(S, Init(FileName,stOpenRead,EditorTextBufSize));
  OK:=Assigned(S);
{$ifdef TEST_PARTIAL_SYNTAX}
  SyntaxComplete:=false;
{$endif TEST_PARTIAL_SYNTAX}
  if OK then OK:=LoadFromStream(S);
  if Assigned(S) then Dispose(S, Done);
  OnDiskLoadTime:=GetFileTime(FileName);
  LoadFile:=OK;
end;

function TFileEditor.IsChangedOnDisk : boolean;
begin
  IsChangedOnDisk:=(OnDiskLoadTime<>GetFileTime(FileName)) and (OnDiskLoadTime<>-1);
end;

function TFileEditor.SaveFile: boolean;
var OK: boolean;
    BAKName: string;
    S: PBufStream;
    f: text;
begin
  If IsChangedOnDisk then
    begin
      if EditorDialog(edFileOnDiskChanged, @FileName) <> cmYes then
        begin
          SaveFile:=false;
          exit;
        end;
    end;
{$I-}
  if (Flags and efBackupFiles)<>0 then
  begin
     BAKName:=DirAndNameOf(FileName)+'.bak';
     Assign(f,BAKName);
     Erase(f);
     EatIO;
     Assign(f,FileName);
     Rename(F,BAKName);
     EatIO;
  end;
{$I+}
  New(S, Init(FileName,stCreate,EditorTextBufSize));
  OK:=Assigned(S);
  if OK then OK:=SaveToStream(S);
  if Assigned(S) then Dispose(S, Done);
  if OK then SetModified(false);
  { don't forget to update the OnDiskLoadTime value }
  OnDiskLoadTime:=GetFileTime(FileName);
  SaveFile:=OK;
end;

function TFileEditor.ShouldSave: boolean;
begin
  ShouldSave:=Modified{ or (FileName='')};
end;

function TFileEditor.Save: Boolean;
begin
  if ShouldSave=false then begin Save:=true; Exit; end;
  if FileName = '' then Save := SaveAs else Save := SaveFile;
end;

function TFileEditor.SaveAs: Boolean;
begin
  SaveAs := False;
  if EditorDialog(edSaveAs, @FileName) <> cmCancel then
  begin
    FileName := FExpand(FileName);
    Message(Owner, evBroadcast, cmUpdateTitle, @Self);
    { if we rename the file the OnDiskLoadTime is wrong so we reset it }
    OnDiskLoadTime:=-1;
    SaveAs := SaveFile;
    if IsClipboard then FileName := '';
    Message(Application,evBroadcast,cmFileNameChanged,@Self);
  end;
end;

function TFileEditor.SaveAsk: boolean;
var OK: boolean;
    D: Sw_integer;
begin
  OK:=Modified=false;
  if OK=false then
  begin
    if FileName = '' then D := edSaveUntitled else D := edSaveModify;
    case EditorDialog(D, @FileName) of
      cmYes    : OK := Save;
      cmNo     : begin Modified := False; OK:=true; end;
      cmCancel : begin
                   OK := False;
                   Message(Application,evBroadcast,cmSaveCancelled,@Self);
                 end;
    end;
  end;
  SaveAsk:=OK;
end;

procedure TFileEditor.HandleEvent(var Event: TEvent);
var SH,B: boolean;
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
   cmFileNameChanged :
     if (Event.InfoPtr=nil) or (Event.InfoPtr=@Self) then
     begin
       B:=(Flags and efSyntaxHighlight)<>0;
       SH:=UseSyntaxHighlight(@Self);
       if SH<>B then
         if SH then
           SetFlags(Flags or efSyntaxHighlight)
         else
           SetFlags(Flags and not efSyntaxHighlight);
       if UseTabsPattern(@Self) then
         SetFlags(Flags or efUseTabCharacters);
     end;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TFileEditor.Valid(Command: Word): Boolean;
var OK: boolean;
begin
  OK:=inherited Valid(Command);
  if OK and ((Command=cmClose) or (Command=cmQuit)) then
     if IsClipboard=false then
    OK:=SaveAsk;
  Valid:=OK;
end;

constructor TFileEditor.Load(var S: TStream);
var P: PString;
    SSP,SEP,CP,DP: TPoint;
    HR: TRect;
    HoldUndo : boolean;
begin
  inherited Load(S);
  HoldUndo:=StoreUndo;
  StoreUndo:=False;
  P:=S.ReadStr;
  FileName:=GetStr(P);
  if P<>nil then DisposeStr(P);

  UpdateIndicator;
{  Message(@Self,evBroadcast,cmFileNameChanged,@Self);}

  SSP:=SelStart; SEP:=SelEnd;
  CP:=CurPos;
  HR:=Highlight;
  DP:=Delta;

  if FileName<>'' then
    LoadFile;

  SetHighlight(HR.A,HR.B);
  SetSelection(SSP,SEP);
  SetCurPtr(CP.X,CP.Y);
  ScrollTo(DP.X,DP.Y);
  SetModified(false);

  LimitsChanged;
  StoreUndo:=HoldUndo;
end;

procedure TFileEditor.Store(var S: TStream);
begin
  inherited Store(S);
  S.WriteStr(@FileName);
end;

function CreateFindDialog: PDialog;
var R,R1,R2: TRect;
    D: PDialog;
    IL1: PInputLine;
    Control : PView;
    CB1: PCheckBoxes;
    RB1,RB2,RB3: PRadioButtons;
begin
  R.Assign(0,0,56,15);
  New(D, Init(R, 'Find'));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2);
    R1.Copy(R); R1.B.X:=17; R1.B.Y:=R1.A.Y+1;
    R2.Copy(R); R2.B.X:=R2.B.X-3;R2.A.X:=17; R2.B.Y:=R2.A.Y+1;
    New(IL1, Init(R2, FindStrSize));
    IL1^.Data^:=FindStr;
    Insert(IL1);
    Insert(New(PLabel, Init(R1, '~T~ext to find', IL1)));
    R1.Assign(R2.B.X, R2.A.Y, R2.B.X+3, R2.B.Y);
    Control := New(PHistory, Init(R1, IL1, TextFindId));
    Insert(Control);

    R1.Copy(R); Inc(R1.A.Y,2); R1.B.Y:=R1.A.Y+1; R1.B.X:=R1.A.X+(R1.B.X-R1.A.X) div 2-1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(CB1, Init(R2,
      NewSItem('~C~ase sensitive',
      NewSItem('~W~hole words only',
      nil))));
    Insert(CB1);
    Insert(New(PLabel, Init(R1, 'Options', CB1)));

    R1.Copy(R); Inc(R1.A.Y,2); R1.B.Y:=R1.A.Y+1; R1.A.X:=R1.B.X-(R1.B.X-R1.A.X) div 2+1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(RB1, Init(R2,
      NewSItem('Forwar~d~',
      NewSItem('~B~ackward',
      nil))));
    Insert(RB1);
    Insert(New(PLabel, Init(R1, 'Direction', RB1)));

    R1.Copy(R); Inc(R1.A.Y,6); R1.B.Y:=R1.A.Y+1; R1.B.X:=R1.A.X+(R1.B.X-R1.A.X) div 2-1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(RB2, Init(R2,
      NewSItem('~G~lobal',
      NewSItem('~S~elected text',
      nil))));
    Insert(RB2);
    Insert(New(PLabel, Init(R1, 'Scope', RB2)));

    R1.Copy(R); Inc(R1.A.Y,6); R1.B.Y:=R1.A.Y+1; R1.A.X:=R1.B.X-(R1.B.X-R1.A.X) div 2+1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(RB3, Init(R2,
      NewSItem('~F~rom cursor',
      NewSItem('~E~ntire scope',
      nil))));
    Insert(RB3);
    Insert(New(PLabel, Init(R1, 'Origin', RB3)));

    GetExtent(R); R.Grow(-13,-1); R.A.Y:=R.B.Y-2; R.B.X:=R.A.X+10;
    Insert(New(PButton, Init(R, 'O~K', cmOK, bfDefault)));
    R.Move(19,0);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  end;
  IL1^.Select;
  CreateFindDialog := D;
end;

function CreateReplaceDialog: PDialog;
var R,R1,R2: TRect;
    D: PDialog;
    Control : PView;
    IL1,IL2: PInputLine;
    CB1: PCheckBoxes;
    RB1,RB2,RB3: PRadioButtons;
begin
  R.Assign(0,0,56,18);
  New(D, Init(R, 'Replace'));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2);
    R1.Copy(R); R1.B.X:=17; R1.B.Y:=R1.A.Y+1;
    R2.Copy(R); R2.B.X:=R2.B.X-3;R2.A.X:=17; R2.B.Y:=R2.A.Y+1;
    New(IL1, Init(R2, FindStrSize));
    IL1^.Data^:=FindStr;
    Insert(IL1);
    Insert(New(PLabel, Init(R1, '~T~ext to find', IL1)));
    R1.Assign(R2.B.X, R2.A.Y, R2.B.X+3, R2.B.Y);
    Control := New(PHistory, Init(R1, IL1, TextFindId));
    Insert(Control);

    R1.Copy(R); R1.Move(0,2); R1.B.X:=17; R1.B.Y:=R1.A.Y+1;
    R2.Copy(R); R2.Move(0,2);R2.B.X:=R2.B.X-3;
    R2.A.X:=17; R2.B.Y:=R2.A.Y+1;
    New(IL2, Init(R2, FindStrSize));
    IL2^.Data^:=ReplaceStr;
    Insert(IL2);
    Insert(New(PLabel, Init(R1, '    ~N~ew text', IL2)));
    R1.Assign(R2.B.X, R2.A.Y, R2.B.X+3, R2.B.Y);
    Control := New(PHistory, Init(R1, IL2, TextReplaceId));
    Insert(Control);

    R1.Copy(R); Inc(R1.A.Y,4); R1.B.Y:=R1.A.Y+1; R1.B.X:=R1.A.X+(R1.B.X-R1.A.X) div 2-1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+3;
    New(CB1, Init(R2,
      NewSItem('~C~ase sensitive',
      NewSItem('~W~hole words only',
      NewSItem('~P~rompt on replace',
      nil)))));
    Insert(CB1);
    Insert(New(PLabel, Init(R1, 'Options', CB1)));

    R1.Copy(R); Inc(R1.A.Y,4); R1.B.Y:=R1.A.Y+1; R1.A.X:=R1.B.X-(R1.B.X-R1.A.X) div 2+1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(RB1, Init(R2,
      NewSItem('Forwar~d~',
      NewSItem('~B~ackward',
      nil))));
    Insert(RB1);
    Insert(New(PLabel, Init(R1, 'Direction', RB1)));

    R1.Copy(R); Inc(R1.A.Y,9); R1.B.Y:=R1.A.Y+1; R1.B.X:=R1.A.X+(R1.B.X-R1.A.X) div 2-1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(RB2, Init(R2,
      NewSItem('~G~lobal',
      NewSItem('~S~elected text',
      nil))));
    Insert(RB2);
    Insert(New(PLabel, Init(R1, 'Scope', RB2)));

    R1.Copy(R); Inc(R1.A.Y,9); R1.B.Y:=R1.A.Y+1; R1.A.X:=R1.B.X-(R1.B.X-R1.A.X) div 2+1;
    R2.Copy(R1); R2.Move(0,1); R2.B.Y:=R2.A.Y+2;
    New(RB3, Init(R2,
      NewSItem('~F~rom cursor',
      NewSItem('~E~ntire scope',
      nil))));
    Insert(RB3);
    Insert(New(PLabel, Init(R1, 'Origin', RB3)));

    GetExtent(R); R.Grow(-13,-1); R.A.Y:=R.B.Y-2; R.B.X:=R.A.X+10; R.Move(-10,0);
    Insert(New(PButton, Init(R, 'O~K~', cmOK, bfDefault)));
    R.Move(11,0); R.B.X:=R.A.X+14;
    Insert(New(PButton, Init(R, 'Change ~a~ll', cmYes, bfNormal)));
    R.Move(15,0); R.B.X:=R.A.X+10;
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  end;
  IL1^.Select;
  CreateReplaceDialog := D;
end;

function CreateGotoLineDialog(Info: pointer): PDialog;
var D: PDialog;
    R,R1,R2: TRect;
    Control : PView;
    IL: PInputLine;
begin
  R.Assign(0,0,40,7);
  New(D, Init(R, 'Goto line'));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2); R.B.Y:=R.A.Y+1;
    R1.Copy(R); R1.B.X:=27; R2.Copy(R);
    R2.B.X:=R2.B.X-3;R2.A.X:=27;
    New(IL, Init(R2,5));
    with TGotoLineDialogRec(Info^) do
    IL^.SetValidator(New(PRangeValidator, Init(1, Lines)));
    Insert(IL);
    Insert(New(PLabel, Init(R1, 'Enter new line ~n~umber', IL)));
    R1.Assign(R2.B.X, R2.A.Y, R2.B.X+3, R2.B.Y);
    Control := New(PHistory, Init(R1, IL, GotoId));
    Insert(Control);

    GetExtent(R); R.Grow(-8,-1); R.A.Y:=R.B.Y-2; R.B.X:=R.A.X+10;
    Insert(New(PButton, Init(R, 'O~K', cmOK, bfDefault)));
    R.Move(15,0);
    Insert(New(PButton, Init(R, 'Cancel', cmCancel, bfNormal)));
  end;
  IL^.Select;
  CreateGotoLineDialog:=D;
end;

function StdEditorDialog(Dialog: Integer; Info: Pointer): Word;
var
  R: TRect;
  T: TPoint;
  Re: word;
  Name: string;
  DriveNumber : byte;
  StoreDir,StoreDir2 : DirStr;
  Title,DefExt: string;
  AskOW: boolean;
begin
  case Dialog of
    edOutOfMemory:
      StdEditorDialog := MessageBox('Not enough memory for this operation.',
   nil, mfInsertInApp+ mfError + mfOkButton);
    edReadError:
      StdEditorDialog := MessageBox('Error reading file %s.',
   @Info, mfInsertInApp+ mfError + mfOkButton);
    edWriteError:
      StdEditorDialog := MessageBox('Error writing file %s.',
   @Info, mfInsertInApp+ mfError + mfOkButton);
    edCreateError:
      StdEditorDialog := MessageBox('Error creating file %s.',
   @Info, mfInsertInApp+ mfError + mfOkButton);
    edSaveModify:
      StdEditorDialog := MessageBox('%s has been modified. Save?',
   @Info, mfInsertInApp+ mfInformation + mfYesNoCancel);
    edSaveUntitled:
      StdEditorDialog := MessageBox('Save untitled file?',
   nil, mfInsertInApp+ mfInformation + mfYesNoCancel);
    edFileOnDiskChanged:
      StdEditorDialog := MessageBox(#3'File %s '#13#3+
        'was modified by another program.'#13#3'Overwrite new version?',
   @info, mfInsertInApp+ mfInformation + mfYesNoCancel);
    edSaveAs,edWriteBlock,edReadBlock:
      begin
        Name:=PString(Info)^;
        GetDir(0,StoreDir);
        DriveNumber:=0;
        if (Length(FileDir)>1) and (FileDir[2]=':') then
          begin
            { does not assume that lowercase are greater then uppercase ! }
            if (FileDir[1]>='a') and (FileDir[1]>='z') then
              DriveNumber:=Ord(FileDir[1])-ord('a')+1
            else
              DriveNumber:=Ord(FileDir[1])-ord('A')+1;
            GetDir(DriveNumber,StoreDir2);
{$ifndef FPC}
            ChDir(Copy(FileDir,1,2));
            { this sets InOutRes in win32 PM }
            { is this bad? What about an EatIO? Gabor }
{$endif not FPC}
          end;
        if FileDir<>'' then
          ChDir(FileDir);
        case Dialog of
          edSaveAs     :
            begin
              Title:='Save File As';
              DefExt:='*'+DefaultSaveExt;
            end;
          edWriteBlock :
            begin
              Title:='Write Block to File';
              DefExt:='';
            end;
          edReadBlock  :
            begin
              Title:='Read Block from File';
              DefExt:='';
            end;
        else begin Title:='???'; DefExt:=''; end;
        end;
        Re:=Application^.ExecuteDialog(New(PFileDialog, Init(DefExt,
          Title, '~N~ame', fdOkButton, FileId)), @Name);
        case Dialog of
          edSaveAs     : AskOW:=(Name<>PString(Info)^);
          edWriteBlock : AskOW:=true;
          edReadBlock  : AskOW:=false;
        else AskOW:=true;
        end;
        if (Re<>cmCancel) and AskOW then
          begin
            FileDir:=DirOf(FExpand(Name));
            if ExistsFile(Name) then
              if EditorDialog(edReplaceFile,@Name)<>cmYes then
                Re:=cmCancel;
          end;
        if DriveNumber<>0 then
          ChDir(StoreDir2);
{$ifndef FPC}
        if (Length(StoreDir)>1) and (StoreDir[2]=':') then
          ChDir(Copy(StoreDir,1,2));
{$endif not FPC}
        if StoreDir<>'' then
          ChDir(StoreDir);

        if Re<>cmCancel then
          PString(Info)^:=Name;
        StdEditorDialog := Re;
      end;
    edGotoLine:
      StdEditorDialog :=
   Application^.ExecuteDialog(CreateGotoLineDialog(Info), Info);
    edFind:
      StdEditorDialog :=
   Application^.ExecuteDialog(CreateFindDialog, Info);
    edSearchFailed:
      StdEditorDialog := MessageBox('Search string not found.',
   nil, mfInsertInApp+ mfError + mfOkButton);
    edReplace:
      StdEditorDialog :=
   Application^.ExecuteDialog(CreateReplaceDialog, Info);
    edReplacePrompt:
      begin
   { Avoid placing the dialog on the same line as the cursor }
   R.Assign(0, 1, 40, 8);
   R.Move((Desktop^.Size.X - R.B.X) div 2, 0);
   Desktop^.MakeGlobal(R.B, T);
   Inc(T.Y);
   if PPoint(Info)^.Y <= T.Y then
     R.Move(0, Desktop^.Size.Y - R.B.Y - 2);
   StdEditorDialog := MessageBoxRect(R, 'Replace this occurence?',
     nil, mfInsertInApp+ mfYesNoCancel + mfInformation);
      end;
    edReplaceFile :
      StdEditorDialog :=
   MessageBox('File %s already exists. Overwrite?',@Info,mfInsertInApp+mfConfirmation+
     mfYesButton+mfNoButton);
  end;
end;

function DefUseSyntaxHighlight(Editor: PFileEditor): boolean;
begin
  DefUseSyntaxHighlight:=(Editor^.Flags and efSyntaxHighlight)<>0;
end;

function DefUseTabsPattern(Editor: PFileEditor): boolean;
begin
  DefUseTabsPattern:=(Editor^.Flags and efUseTabCharacters)<>0;
end;

procedure RegisterCodeEditors;
begin
{$ifndef NOOBJREG}
  RegisterType(RIndicator);
  RegisterType(RCodeEditor);
  RegisterType(RFileEditor);
{$endif}
end;

END.
{
  $Log$
  Revision 1.69  2000-01-05 00:37:34  pierre
    * ^KC fix
    *  better Tab handling

  Revision 1.68  2000/01/04 12:33:08  pierre
    * reinserted version 1.66 lost changes
    + CtrlT Undo works now !

  Revision 1.67  2000/01/03 11:38:35  michael
  Changes from Gabor

  Revision 1.65  1999/12/08 16:02:46  pierre
   * fix for bugs 746,748 and 750

  Revision 1.64  1999/12/01 17:25:00  pierre
   + check if file on disk was changed since load before overwriting

  Revision 1.63  1999/11/22 17:34:08  pierre
   * fix for form bug 634

  Revision 1.62  1999/11/18 13:42:06  pierre
   * Some more Undo stuff

  Revision 1.61  1999/11/10 00:45:30  pierre
   + groupd action started, not yet working

  Revision 1.60  1999/11/05 13:49:13  pierre
   * WinPaste depends on avalaible Clipboard data

  Revision 1.59  1999/11/03 09:39:23  peter
    * fixed uppercase filenames
    * savetostream did twice a -1 on the linecount, so the lastline of a
      file wasn't saved correctly

  Revision 1.58  1999/10/28 15:14:22  pierre
   * get it to compile with debug conditional

  Revision 1.56  1999/10/27 13:32:58  pierre
   * some more Undo Fixes

  Revision 1.55  1999/10/27 10:46:19  pierre
   * More Undo/Redo stuff

  Revision 1.54  1999/10/25 16:49:05  pierre
    + Undo/Redo by Visa Harvey (great thanks) inserted
      (with some modifications)
      Moves work correctly
      Text insertion/deletion are still buggy !
    * LinePosToCharIndex and reverse function changed to get more
      sensible results, dependant code adapted
    * several bug fixes

  Revision 1.53  1999/10/14 10:21:48  pierre
   * more tabs related problems fiwes

  Revision 1.52  1999/10/12 23:35:18  pierre
    + DelStart and SelectWord implemented
    * AddChar(tab) now reacts correctly if efAutoIndent is set

  Revision 1.51  1999/10/08 15:24:50  pierre
   * InsertFrom bug (end of line wasdiscarded)

  Revision 1.50  1999/09/28 23:44:13  pierre
   * text insertion in middle of line was buggy

  Revision 1.49  1999/09/23 16:33:30  pierre
    * ^B^A now prints out the ascii 1 char
    * In SearchReplace Several occurence of a pattern in the same line
      should now be found correctly

  Revision 1.48  1999/09/22 16:16:26  pierre
   + added HistLists for several dialogs

  Revision 1.47  1999/09/21 17:08:59  pierre
   + Windows clipboard for win32

  Revision 1.46  1999/09/13 16:24:44  peter
    + clock
    * backspace unident like tp7

  Revision 1.45  1999/09/09 12:05:33  pierre
    + Copy/Paste to Windows Clipboard
    + efLeaveTrailingSpaces added to editor flags
      (if not set then spaces at the end of a line are
      removed on writing the file)

  Revision 1.44  1999/08/27 15:07:44  pierre
   + cmResetDebuggerRow

  Revision 1.43  1999/08/24 22:04:35  pierre
    + TCodeEditor.SetDebuggerRow
      works like SetHighlightRow but is only disposed by a SetDebuggerRow(-1)
      so the current stop point in debugging is not lost if
      we move the cursor

  Revision 1.42  1999/08/22 22:20:30  pierre
   * selection extension bug removed, via oldEvent pointer in TCodeEditor.HandleEvent

  Revision 1.41  1999/08/16 18:25:28  peter
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

  Revision 1.40  1999/08/03 20:22:42  peter
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

  Revision 1.39  1999/07/28 23:11:26  peter
    * fixes from gabor

  Revision 1.38  1999/07/12 13:14:24  pierre
    * LineEnd bug corrected, now goes end of text even if selected
    + Until Return for debugger
    + Code for Quit inside GDB Window

  Revision 1.37  1999/06/29 22:50:16  peter
    * more fixes from gabor

  Revision 1.36  1999/06/29 08:51:34  pierre
   * lockflag problems fixed

  Revision 1.35  1999/06/28 19:32:32  peter
    * fixes from gabor

  Revision 1.34  1999/06/28 15:58:07  pierre
   * ShiftDel problem solved

  Revision 1.33  1999/06/25 00:31:51  pierre
   + FileDir remembers the last directory for Open and Save

  Revision 1.32  1999/06/21 23:36:12  pierre
   * Size for Cluster is word (TP compatibility)

  Revision 1.31  1999/05/22 13:44:35  peter
    * fixed couple of bugs

  Revision 1.30  1999/04/15 08:58:10  peter
    * syntax highlight fixes
    * browser updates

  Revision 1.29  1999/04/07 21:55:59  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.28  1999/03/23 15:11:39  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

  Revision 1.27  1999/03/08 14:58:17  peter
    + prompt with dialogs for tools

  Revision 1.26  1999/03/07 22:58:57  pierre
   * FindRec needs longint for CheckBoxes

  Revision 1.25  1999/03/05 17:39:39  pierre
   * Actions item freeing

  Revision 1.24  1999/03/03 16:45:07  pierre
   * Actions were not dispose in TCodeEditor.Done

  Revision 1.23  1999/03/01 15:42:10  peter
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

  Revision 1.22  1999/02/22 02:15:25  peter
    + default extension for save in the editor
    + Separate Text to Find for the grep dialog
    * fixed redir crash with tp7

  Revision 1.21  1999/02/20 15:18:33  peter
    + ctrl-c capture with confirm dialog
    + ascii table in the tools menu
    + heapviewer
    * empty file fixed
    * fixed callback routines in fpdebug to have far for tp7

  Revision 1.20  1999/02/18 17:27:57  pierre
   * find/replace dialogs need packed records !!

  Revision 1.19  1999/02/18 13:44:36  peter
    * search fixed
    + backward search
    * help fixes
    * browser updates

  Revision 1.18  1999/02/15 15:12:25  pierre
   + TLine remembers Comment type

  Revision 1.17  1999/02/15 09:32:58  pierre
   * single line comment // fix : comments intermix still wrong !!

  Revision 1.16  1999/02/11 19:07:26  pierre
    * GDBWindow redesigned :
      normal editor apart from
      that any kbEnter will send the line (for begin to cursor)
      to GDB command !
      GDBWindow opened in Debugger Menu
       still buggy :
       -echo should not be present if at end of text
       -GDBWindow becomes First after each step (I don't know why !)

  Revision 1.15  1999/02/09 09:29:59  pierre
   * avoid invisible characters in CombineColors

  Revision 1.14  1999/02/05 13:51:45  peter
    * unit name of FPSwitches -> FPSwitch which is easier to use
    * some fixes for tp7 compiling

  Revision 1.13  1999/02/05 13:22:43  pierre
   * bug that caused crash for empty files

  Revision 1.12  1999/02/05 12:04:56  pierre
   + 'loose' centering for debugger

  Revision 1.11  1999/02/04 17:19:26  peter
    * linux fixes

  Revision 1.10  1999/02/04 10:13:00  pierre
    + GetCurrentWord (used in Find/Replace)
    + DefUseTabsPattern (pattern forcing tabs to be kept)
      used for all makefiles !!

  Revision 1.9  1999/01/29 10:34:33  peter
    + needobjdir,needlibdir

  Revision 1.8  1999/01/21 11:54:31  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.7  1999/01/14 21:41:17  peter
    * use * as modified indicator
    * fixed syntax highlighting

  Revision 1.6  1999/01/12 14:29:44  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.5  1999/01/07 15:02:40  peter
    * better tab support

  Revision 1.4  1999/01/04 11:49:55  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.2  1998/12/28 15:47:55  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.4  1998/12/27 12:01:23  gabor
    * efXXXX constants revised for BP compatibility
    * fixed column and row highlighting (needs to rewrite default palette in the INI)

  Revision 1.3  1998/12/22 10:39:54  peter
    + options are now written/read
    + find and replace routines

}
