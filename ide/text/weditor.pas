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
unit WEditor;

interface

uses
  Objects,Drivers,Views,Menus,Commands;

const
      cmFileNameChanged      = 51234;
      cmASCIIChar            = 51235;
      cmClearLineHighlights  = 51236;
      cmSaveCancelled        = 51237;

{$ifdef FPC}
      EditorTextBufSize = 32768;
      MaxLineLength = 255;
      MaxLineCount  = 16380;
{$else}
      EditorTextBufSize = 4096;
      MaxLineLength = 255;
      MaxLineCount  = 16380;
{$endif}

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

      CIndicator    = #2#3#1;
      CEditor       = #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#48#49;

      TAB      = #9;

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
      function  At(Index: Integer): PLine;
      procedure FreeItem(Item: Pointer); virtual;
    end;

    PIndicator = ^TIndicator;
    TIndicator = object(TView)
      Location: TPoint;
      Modified: Boolean;
      constructor Init(var Bounds: TRect);
      procedure   Draw; virtual;
      function    GetPalette: PPalette; virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   SetValue(ALocation: TPoint; AModified: Boolean);
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
    end;

    PEditorAction = ^TEditorAction;
    TEditorAction = packed record
      Action    : byte;
      StartPos  : TPoint;
      EndPos    : TPoint;
      Text      : PString;
    end;

    PEditorActionCollection = ^TEditorActionCollection;
    TEditorActionCollection = object(TCollection)
      procedure FreeItem(Item: Pointer); virtual;
    end;

    TSpecSymbolClass =
      (ssCommentPrefix,ssCommentSingleLinePrefix,ssCommentSuffix,ssStringPrefix,ssStringSuffix,
       ssDirectivePrefix,ssDirectiveSuffix,ssAsmPrefix,ssAsmSuffix);

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
      HighlightRow: integer;
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
      procedure   ScrollTo(X, Y: Integer); virtual;
      procedure   SetInsertMode(InsertMode: boolean); virtual;
      procedure   SetCurPtr(X, Y: Integer); virtual;
      procedure   SetSelection(A, B: TPoint); virtual;
      procedure   SetHighlight(A, B: TPoint); virtual;
      procedure   SetHighlightRow(Row: integer); virtual;
      procedure   SelectAll(Enable: boolean); virtual;
      function    InsertFrom(Editor: PCodeEditor): Boolean; virtual;
      function    InsertText(const S: string): Boolean; virtual;
      function    GetPalette: PPalette; virtual;
      function    IsClipboard: Boolean;
      constructor Load(var S: TStream);
      procedure   Store(var S: TStream);
      destructor  Done; virtual;
    public
      { Text & info storage abstraction }
      function    GetLineCount: integer; virtual;
      function    CharIdxToLinePos(Line,CharIdx: integer): integer;
      function    LinePosToCharIdx(Line,X: integer): integer;
      function    GetLineText(I: integer): string; virtual;
      procedure   SetDisplayText(I: integer;const S: string); virtual;
      function    GetDisplayText(I: integer): string; virtual;
      procedure   SetLineText(I: integer;const S: string); virtual;
      procedure   SetLineBreakState(I : integer;b : boolean);
      procedure   GetDisplayTextFormat(I: integer;var DT,DF:string); virtual;
      function    GetLineFormat(I: integer): string; virtual;
      procedure   SetLineFormat(I: integer;const S: string); virtual;
      procedure   DeleteAllLines; virtual;
      procedure   DeleteLine(I: integer); virtual;
      procedure   AddLine(const S: string); virtual;
      function    GetErrorMessage: string; virtual;
      procedure   SetErrorMessage(const S: string); virtual;
    private
      LastLocalCmd: word;
      KeyState    : Integer;
      ErrorMessage: PString;
      Actions     : PEditorActionCollection;
      function    Overwrite: boolean;
      function    GetLine(I: integer): PLine;
      procedure   CheckSels;
      function    UpdateAttrs(FromLine: integer; Attrs: byte): integer;
      procedure   DrawLines(FirstLine: integer);
      procedure   HideHighlight;
      procedure   AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string);
    public
     { Syntax highlight support }
      function    GetSpecSymbolCount(SpecClass: TSpecSymbolClass): integer; virtual;
      function    GetSpecSymbol(SpecClass: TSpecSymbolClass; Index: integer): string; virtual;
      function    IsReservedWord(const S: string): boolean; virtual;
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
      function  InsertLine: Sw_integer; virtual;
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
      procedure AddChar(C: char); virtual;
      function  ClipCopy: Boolean; virtual;
      procedure ClipCut; virtual;
      procedure ClipPaste; virtual;
      function  GetCurrentWord : string;
      procedure Undo; virtual;
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
    end;

    TCodeEditorDialog = function(Dialog: Integer; Info: Pointer): Word;

function DefUseSyntaxHighlight(Editor: PFileEditor): boolean;
function DefUseTabsPattern(Editor: PFileEditor): boolean;

const
     DefaultCodeEditorFlags : longint =
       efBackupFiles+efInsertMode+efAutoIndent+efPersistentBlocks+
       {efUseTabCharacters+}efBackSpaceUnindents+efSyntaxHighlight;
     DefaultTabSize     : integer = 8;

     ToClipCmds         : TCommandSet = ([cmCut,cmCopy]);
     FromClipCmds       : TCommandSet = ([cmPaste]);
     NulClipCmds        : TCommandSet = ([cmClear]);
     UndoCmds           : TCommandSet = ([cmUndo,cmRedo]);

function StdEditorDialog(Dialog: Integer; Info: Pointer): word;

const
     EditorDialog       : TCodeEditorDialog = StdEditorDialog;
     Clipboard          : PCodeEditor = nil;
     FindStr            : String[80] = '';
     ReplaceStr         : String[80] = '';
     FindFlags          : word = ffPromptOnReplace;
     WhiteSpaceChars    : set of char = [#0,#32,#255];
     TabChars           : set of char = [#9];
     AlphaChars         : set of char = ['A'..'Z','a'..'z','_'];
     NumberChars        : set of char = ['0'..'9'];
     DefaultSaveExt     : string[12] = '.pas';

     UseSyntaxHighlight : function(Editor: PFileEditor): boolean = DefUseSyntaxHighlight;
     UseTabsPattern     : function(Editor: PFileEditor): boolean = DefUseTabsPattern;

procedure RegisterCodeEditors;

implementation

uses
  Dos,
  MsgBox,Dialogs,App,StdDlg,HistList,Validate,
  WUtils,WViews;

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

type
     TFindDialogRec = packed record
       Find     : String[80];
       Options  : {Word}longint;{checkboxes need 32  bits PM  }
       Direction: word;{ and tcluster has word size }
       Scope    : word;
       Origin   : word;
     end;

     TReplaceDialogRec = packed record
       Find     : String[80];
       Replace  : String[80];
       Options  : {Word}longint;{checkboxes need 32  bits PM  }
       Direction: word;
       Scope    : word;
       Origin   : word;
     end;

     TGotoLineDialogRec = packed record
       LineNo  : string[5];
       Lines   : integer;
     end;

const
     kbShift = kbLeftShift+kbRightShift;

const
  FirstKeyCount = 38;
  FirstKeys: array[0..FirstKeyCount * 2] of Word = (FirstKeyCount,
    Ord(^A), cmWordLeft, Ord(^B), cmASCIIChar, Ord(^C), cmPageDown,
    Ord(^D), cmCharRight, Ord(^E), cmLineUp,
    Ord(^F), cmWordRight, Ord(^G), cmDelChar,
    Ord(^H), cmBackSpace, Ord(^J), cmJumpLine,
    Ord(^K), $FF02, Ord(^L), cmSearchAgain,
    Ord(^M), cmNewLine, Ord(^Q), $FF01,
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
  QuickKeyCount = 10;
  QuickKeys: array[0..QuickKeyCount * 2] of Word = (QuickKeyCount,
    Ord('A'), cmReplace, Ord('C'), cmTextEnd,
    Ord('D'), cmLineEnd, Ord('F'), cmFind,
    Ord('H'), cmDelStart, Ord('R'), cmTextStart,
    Ord('S'), cmLineStart, Ord('Y'), cmDelEnd,
    Ord('G'), cmJumpLine, Ord('P'), cmReplace );
  BlockKeyCount = 6;
  BlockKeys: array[0..BlockKeyCount * 2] of Word = (BlockKeyCount,
    Ord('B'), cmStartSelect, Ord('C'), cmCopyBlock,
    Ord('H'), cmHideSelect, Ord('K'), cmEndSelect,
    Ord('Y'), cmDelSelect, Ord('V'), cmMoveBlock);
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
   exit;
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

function PointOfs(P: TPoint): longint;
begin
  PointOfs:=longint(P.Y)*MaxLineLength+P.X;
end;

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

function TLineCollection.At(Index: Integer): PLine;
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
     L[0] := Location.Y + 1;
     L[1] := Location.X + 1;
     FormatStr(S, ' %d:%d ', L);
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
          PScrollBar; AIndicator: PIndicator; AbufSize:Sw_Word);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
  StoreUndo:=false;
  New(Actions, Init(500,1000));
  New(Lines, Init(500,1000));
  { we have always need at least 1 line }
  Lines^.Insert(NewLine(''));
  { ^^^ why? setlinetext() inserts automatically if neccessary and
    getlinetext() checks whether you're in range...
    because otherwise you search for line with index -1 (PM) }
  SetState(sfCursorVis,true);
  SetFlags(DefaultCodeEditorFlags); TabSize:=DefaultTabSize;
  SetHighlightRow(-1);
  SetCurPtr(0,0);
  Indicator:=AIndicator;
  UpdateIndicator; LimitsChanged;
end;

procedure TCodeEditor.SetFlags(AFlags: longint);
var I: integer;
begin
  Flags:=AFlags;
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

procedure TCodeEditor.ScrollTo(X, Y: Integer);
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
    if (GetShiftState and kbShift <> 0) and
      (Event.ScanCode >= $47) and (Event.ScanCode <= $51) then
      Event.CharCode := #0;
    Key := Event.KeyCode;
    if KeyState <> 0 then
    begin
      if (Lo(Key) >= $01) and (Lo(Key) <= $1A) then Inc(Key, $40);
      if (Lo(Key) >= $61) and (Lo(Key) <= $7A) then Dec(Key, $20);
    end;
    Key := ScanKeyMap(KeyMap[KeyState], Key);
    KeyState := 0;
    if Key <> 0 then
      if Hi(Key) = $FF then
      begin
   KeyState := Lo(Key);
   ClearEvent(Event);
      end else
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

var
  StartP,P: TPoint;
begin
  if (InASCIIMode=false) or (Event.What<>evKeyDown) then
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
        if InASCIIMode and (Event.ScanCode=0) then
          AddChar(Event.CharCode)
        else
          begin
           DontClear:=false;
           case Event.KeyCode of
             kbAltF10 : Message(@Self,evCommand,cmLocalMenu,@Self);
           else
            case Event.CharCode of
             #9,#32..#255 :
               begin
                 NoSelect:=true;
                 AddChar(Event.CharCode);
                 NoSelect:=false;
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
        { ------ }
          cmFind        : Find;
          cmReplace     : Replace;
          cmSearchAgain : DoSearchReplace;
          cmJumpLine    : GotoLine;
        { ------ }
          cmCut         : ClipCut;
          cmCopy        : ClipCopy;
          cmPaste       : ClipPaste;
          cmUndo        : Undo;
          cmClear       : DelSelect;
          cmLocalMenu :
            begin
              P:=CurPos; Inc(P.X); Inc(P.Y);
              LocalMenu(P);
            end;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmClearLineHighlights :
          SetHighlightRow(-1);
        cmScrollBarChanged:
          if (Event.InfoPtr = HScrollBar) or
             (Event.InfoPtr = VScrollBar) then
            begin
              CheckScrollBar(HScrollBar, Delta.X);
              CheckScrollBar(VScrollBar, Delta.Y);
            end;
      end;
  end;
  inherited HandleEvent(Event);
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
    X,Y,AX,AY,MaxX: integer;
    PX: TPoint;
    LineCount: integer;
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
  SetCursor(CurPos.X-Delta.X,CurPos.Y-Delta.Y);
  SetState(sfCursorIns,Overwrite);
end;

function TCodeEditor.Overwrite: boolean;
begin
  Overwrite:=(Flags and efInsertMode)=0;
end;

function TCodeEditor.GetLineCount: integer;
begin
  GetLineCount:=Lines^.Count;
end;

function TCodeEditor.GetLine(I: integer): PLine;
begin
  GetLine:=Lines^.At(I);
end;

function TCodeEditor.CharIdxToLinePos(Line,CharIdx: integer): integer;
var S: string;
    CP,RX: integer;
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

function TCodeEditor.LinePosToCharIdx(Line,X: integer): integer;
var S: string;
    CP,RX: integer;
begin
  S:=GetLineText(Line);
  if S='' then
    CP:=0
  else
    begin
     CP:=0; RX:=0;
     while (RX<X) and (CP<length(S)) do
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

function TCodeEditor.GetLineText(I: integer): string;
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

procedure TCodeEditor.SetLineText(I: integer;const S: string);
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

procedure TCodeEditor.SetLineBreakState(I : integer;b : boolean);
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

function TCodeEditor.GetDisplayText(I: integer): string;
begin
  GetDisplayText:=ExtractTabs(GetLineText(I),TabSize);
end;

procedure TCodeEditor.SetDisplayText(I: integer;const S: string);
begin
  if ((Flags and efUseTabCharacters)<>0) and (TabSize>0) then
   SetLineText(I,CompressUsingTabs(S,TabSize))
  else
   SetLineText(I,S);
end;

procedure TCodeEditor.GetDisplayTextFormat(I: integer;var DT,DF:string);
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
         DF:=copy(DF,1,P-1)+CharStr(DF[p],PAdd)+copy(DF,P+1,255);
         DT:=copy(DT,1,P-1)+CharStr(' ',PAdd)+copy(DT,P+1,255);
         inc(P,PAdd-1);
       end;
    end;
      end;
   end;
end;

function TCodeEditor.GetLineFormat(I: integer): string;
var P: PLine;
    S: string;
begin
  if I<GetLineCount then P:=Lines^.At(I) else P:=nil;
  if (P=nil) or (P^.Format=nil) then S:='' else
     S:=P^.Format^;
  GetLineFormat:=S;
end;

procedure TCodeEditor.SetLineFormat(I: integer;const S: string);
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

procedure TCodeEditor.DeleteLine(I: integer);
begin
  if I<Lines^.Count then
    Lines^.AtFree(I);
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
  Modified:=true;
  UpdateIndicator;
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
var X, Y: integer;
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
var X, Y: integer;
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
begin
  if CurPos.Y<GetLineCount then
    SetCurPtr(length(GetDisplayText(CurPos.Y)),CurPos.Y)
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
begin
  SetCurPtr(length(GetDisplayText(GetLineCount-1)),GetLineCount-1);
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
var SelBack: integer;
begin
  if IsReadOnly then begin InsertLine:=-1; Exit; end;
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
    CalcIndent(CurPos.Y);
    Lines^.AtInsert(CurPos.Y+1,NewLine(IndentStr+copy(S,CurPos.X+1,255)));
    LimitsChanged;
    SetDisplayText(CurPos.Y,copy(S,1,CurPos.X-1+1));
    if PointOfs(SelStart)<>PointOfs(SelEnd) then { !!! check it - it's buggy !!! }
      begin SelEnd.Y:=CurPos.Y+1; SelEnd.X:=length(GetLineText(CurPos.Y+1))-SelBack; end;
    UpdateAttrs(CurPos.Y,attrAll);
    SetCurPtr(Ind,CurPos.Y+1);
  end else
  begin
    if CurPos.Y=GetLineCount-1 then
    CalcIndent(CurPos.Y);
    begin
      Lines^.Insert(NewLine(IndentStr));
      LimitsChanged;
    end;
    SetCurPtr(Ind,CurPos.Y+1);
  end;
  DrawLines(CurPos.Y);
end;

procedure TCodeEditor.BackSpace;
var S,PreS: string;
    OI,CI,CP,Y: Sw_integer;
begin
  if IsReadOnly then Exit;
  if CurPos.X=0 then
   begin
     if CurPos.Y>0 then
      begin
        S:=GetLineText(CurPos.Y-1);
        SetLineText(CurPos.Y-1,S+GetLineText(CurPos.Y));
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
        while (CP>0) and
              ( (CP>length(S))    or (S[CP]=' ')     ) and
              ( (CP>length(PreS)) or (PreS[CP]<>' ') ) do
          Dec(CP);
      end;
     S:=GetLineText(CurPos.Y);
     OI:=LinePosToCharIdx(CurPos.Y,CurPos.X);
     CI:=LinePosToCharIdx(CurPos.Y,CP);
     SetLineText(CurPos.Y,copy(S,1,CI+1-1)+copy(S,OI+1,255));
     SetCurPtr(CP,CurPos.Y);
   end;
  UpdateAttrs(CurPos.Y,attrAll);
  DrawLines(CurPos.Y);
  Modified:=true;
  UpdateIndicator;
end;

procedure TCodeEditor.DelChar;
var S: string;
begin
  if IsReadOnly then Exit;
  S:=GetLineText(CurPos.Y);
  if CurPos.X=length(S) then
   begin
     if CurPos.Y<GetLineCount-1 then
      begin
   SetLineText(CurPos.Y,S+GetLineText(CurPos.Y+1));
   DeleteLine(CurPos.Y+1);
   LimitsChanged;
      end;
   end
  else
   begin
     Delete(S,CurPos.X+1,1);
     SetLineText(CurPos.Y,S);
   end;
  UpdateAttrs(CurPos.Y,attrAll);
  DrawLines(CurPos.Y);
  Modified:=true;
  UpdateIndicator;
end;

procedure TCodeEditor.DelWord;
begin
  if IsReadOnly then Exit;
  Modified:=true;
  UpdateIndicator;
end;

procedure TCodeEditor.DelStart;
begin
  if IsReadOnly then Exit;
  Modified:=true;
  UpdateIndicator;
end;

procedure TCodeEditor.DelEnd;
var S: string;
begin
  if IsReadOnly then Exit;
  S:=GetLineText(CurPos.Y);
  if (S<>'') and (CurPos.X<>length(S)) then
  begin
    SetLineText(CurPos.Y,copy(S,1,CurPos.X));
    UpdateAttrs(CurPos.Y,attrAll);
    DrawLines(CurPos.Y);
    Modified:=true;
    UpdateIndicator;
  end;
end;

procedure TCodeEditor.DelLine;
begin
  if IsReadOnly then Exit;
  if GetLineCount>0 then
  begin
    DeleteLine(CurPos.Y);
    LimitsChanged;
    SetCurPtr(0,CurPos.Y);
    UpdateAttrs(Max(0,CurPos.Y-1),attrAll);
    DrawLines(CurPos.Y);
    Modified:=true;
    UpdateIndicator;
  end;
end;

procedure TCodeEditor.InsMode;
begin
  SetInsertMode(Overwrite);
end;

procedure TCodeEditor.StartSelect;
begin
  if (PointOfs(SelStart)=PointOfs(SelEnd)) then
     SetSelection(SelStart,Limit);
  SetSelection(CurPos,SelEnd);
  if PointOfs(SelEnd)<PointOfs(SelStart) then
     SetSelection(SelStart,SelStart);
  CheckSels;
  DrawView;
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

procedure TCodeEditor.EndSelect;
var P: TPoint;
begin
  P:=CurPos; P.X:=Min(SelEnd.X,length(GetLineText(SelEnd.Y))); CheckSels;
  SetSelection(SelStart,P);
  DrawView;
end;

procedure TCodeEditor.DelSelect;
var LineDelta, LineCount, CurLine: Sw_integer;
    StartX,EndX,LastX: Sw_integer;
    S: string;
begin
  if IsReadOnly then Exit;
  if (SelStart.X=SelEnd.X) and (SelStart.Y=SelEnd.Y) then Exit;
  LineCount:=(SelEnd.Y-SelStart.Y)+1;
  LineDelta:=0; LastX:=CurPos.X;
  CurLine:=SelStart.Y;
  while (LineDelta<LineCount) do
  begin
    S:=GetDisplayText(CurLine);
    if LineDelta=0 then StartX:=SelStart.X else StartX:=0;
    if LineDelta=LineCount-1 then EndX:=SelEnd.X else EndX:=length(S);
    if (LineDelta<LineCount-1) and
       ( (StartX=0) and (EndX>=length(S)) )
       then begin
         DeleteLine(CurLine);
         if CurLine>0 then LastX:=length(GetDisplayText(CurLine-1))
            else LastX:=0;
       end
       else begin
         SetDisplayText(CurLine,copy(S,1,StartX)+copy(S,EndX+1,255));
         LastX:=StartX;
         if (StartX=0) and (0<LineDelta) and
       not(((LineDelta=LineCount-1) and (StartX=0) and (StartX=EndX))) then
         begin
      S:=GetDisplayText(CurLine-1);
      SetDisplayText(CurLine-1,S+GetLineText(CurLine));
      DeleteLine(CurLine);
      LastX:=length(S);
         end else
         Inc(CurLine);
       end;
    Inc(LineDelta);
  end;
  SetCurPtr(LastX,CurLine-1);
  HideSelect;
  UpdateAttrs(CurPos.Y,attrAll);
  DrawLines(CurPos.Y);
  Modified:=true;
  UpdateIndicator;
end;

procedure TCodeEditor.HideSelect;
begin
  SetSelection(CurPos,CurPos);
end;

procedure TCodeEditor.CopyBlock;
var Temp: PCodeEditor;
    R: TRect;
begin
  if IsReadOnly then Exit;
  if (SelStart.X=SelEnd.X) and (SelStart.Y=SelEnd.Y) then Exit;
  GetExtent(R);
  New(Temp, Init(R, nil, nil, nil,0));
  Temp^.InsertFrom(@Self);
  InsertFrom(Temp);
  Dispose(Temp, Done);
end;

procedure TCodeEditor.MoveBlock;
var Temp: PCodeEditor;
    R: TRect;
    OldPos: TPoint;
begin
  if IsReadOnly then Exit;
  if (SelStart.X=SelEnd.X) and (SelStart.Y=SelEnd.Y) then Exit;
  GetExtent(R);
  New(Temp, Init(R, nil, nil, nil,0));
  Temp^.InsertFrom(@Self);
  OldPos:=CurPos; Dec(OldPos.Y,Temp^.GetLineCount-1);
  DelSelect;
  SetCurPtr(OldPos.X,OldPos.Y);
  InsertFrom(Temp);
  Dispose(Temp, Done);
end;

procedure TCodeEditor.AddChar(C: char);
const OpenBrackets  : string[10] = '[({';
      CloseBrackets : string[10] = '])}';
var S,SC,TabS: string;
    BI: byte;
    CI,TabStart : Sw_integer;
begin
  if IsReadOnly then Exit;
  if (C<>TAB) or ((Flags and efUseTabCharacters)<>0) then
    SC:=C
  else
    SC:=CharStr(' ',TabSize);
  S:=GetLineText(CurPos.Y);
  if CharIdxToLinePos(CurPos.Y,length(S))<CurPos.X then
    begin
      S:=S+CharStr(' ',CurPos.X-CharIdxToLinePos(CurPos.Y,length(S)));
      SetLineText(CurPos.Y,S);
    end;
  CI:=LinePosToCharIdx(CurPos.Y,CurPos.X);
  if (S[CI]=TAB) then
    begin
      TabStart:=CharIdxToLinePos(CurPos.Y,CI);
      if SC=Tab then TabS:='' else
        TabS:=CharStr(' ',CurPos.X-TabStart);
      SetLineText(CurPos.Y,copy(S,1,CI-1)+TabS+SC+copy(S,CI,255));
      SetCurPtr(CharIdxToLinePos(CurPos.Y,CI+length(TabS)+length(SC)),CurPos.Y);
    end
  else
    begin
      if Overwrite and (CI<length(S)) then
        SetLineText(CurPos.Y,copy(S,1,CI)+SC+copy(S,CI+2,255))
      else
        SetLineText(CurPos.Y,copy(S,1,CI)+SC+copy(S,CI+1,255));
      SetCurPtr(CharIdxToLinePos(CurPos.Y,CI+length(SC)+1),CurPos.Y);
{      if PointOfs(SelStart)<>PointOfs(SelEnd) then
        if (CurPos.Y=SelEnd.Y) and (CurPos.X<SelEnd.X) then
          Inc(SelEnd.X);
      CharRight;}
    end;
  BI:=Pos(C,OpenBrackets);
  if ((Flags and efAutoBrackets)<>0) and (BI>0) then
   begin
     AddChar(CloseBrackets[BI]);
     SetCurPtr(CurPos.X-1,CurPos.Y);
   end;
  UpdateAttrs(CurPos.Y,attrAll);
  DrawLines(CurPos.Y);
  Modified:=true;
  UpdateIndicator;
end;

function TCodeEditor.ClipCopy: Boolean;
var OK: boolean;
begin
  OK:=Clipboard<>nil;
  if OK then OK:=Clipboard^.InsertFrom(@Self);
  ClipCopy:=OK;
end;

procedure TCodeEditor.ClipCut;
begin
  if IsReadOnly then Exit;
  if Clipboard<>nil then
     if Clipboard^.InsertFrom(@Self) then
     begin
       DelSelect;
       Modified:=true;
       UpdateIndicator;
     end;
end;

procedure TCodeEditor.ClipPaste;
begin
  if IsReadOnly then Exit;
  if Clipboard<>nil then
     begin
       InsertFrom(Clipboard);
       Modified:=true;
       UpdateIndicator;
     end;
end;

procedure TCodeEditor.Undo;
begin
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
    DX,DY,P,Y,X: integer;
    Count: integer;
    Found,CanExit: boolean;
    SForward,DoReplace,DoReplaceAll: boolean;
    LeftOK,RightOK: boolean;
    FoundCount: integer;
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
          if FindFlags and ffCaseSensitive<>0 then
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

  function InArea(X,Y: integer): boolean;
  begin
    InArea:=((AreaStart.Y=Y) and (AreaStart.X<=X)) or
       ((AreaStart.Y<Y) and (Y<AreaEnd.Y)) or
       ((AreaEnd.Y=Y) and (X<=AreaEnd.X));
  end;

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
     end;

    if Found then
      Inc(FoundCount);

    if Found then
      begin
        if Owner<>nil then Owner^.Lock;
        if SForward then
         SetCurPtr(B.X,B.Y)
        else
         SetCurPtr(A.X,A.Y);
        TrackCursor(true);
        SetHighlight(A,B);
        if Owner<>nil then Owner^.UnLock;
        if (DoReplace=false) then CanExit:=true else
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
                if Owner<>nil then
                  Owner^.Lock;
                SetSelection(A,B);
                DelSelect;
                InsertText(ReplaceStr);
                if Owner<>nil then
                  Owner^.UnLock;
              end;
            if (DoReplaceAll=false) then
              CanExit:=true;
          end;
      end;

    if CanExit=false then
      begin
        inc(Y,DY);
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
  if (FoundCount=0) then
    EditorDialog(edSearchFailed,nil);
end;

procedure TCodeEditor.SetInsertMode(InsertMode: boolean);
begin
  if InsertMode then Flags:=Flags or efInsertMode
      else Flags:=Flags and (not efInsertMode);
  DrawCursor;
end;

procedure TCodeEditor.SetCurPtr(X,Y: integer);
var OldPos,OldSEnd,OldSStart: TPoint;
    Extended: boolean;
begin
  X:=Max(0,Min(MaxLineLength+1,X));
  Y:=Max(0,Min(GetLineCount-1,Y));
  OldPos:=CurPos;
  OldSEnd:=SelEnd;
  OldSStart:=SelStart;
  CurPos.X:=X;
  CurPos.Y:=Y;
  TrackCursor(false);
  if (NoSelect=false) and ((GetShiftState and kbShift)<>0) then
  begin
    CheckSels;
    Extended:=false;
    if PointOfs(OldPos)=PointOfs(SelStart) then
      begin SetSelection(CurPos,SelEnd); Extended:=true; end;
    CheckSels;
    if Extended=false then
     if PointOfs(OldPos)=PointOfs(SelEnd) then
   begin SetSelection(SelStart,CurPos); Extended:=true; end;
    CheckSels;
    if (Extended=false) then
       if PointOfs(OldPos)<=PointOfs(CurPos)
     then begin SetSelection(OldPos,CurPos); Extended:=true; end
     else begin SetSelection(CurPos,OldPos); Extended:=true; end;
    DrawView;
  end else
   if (Flags and efPersistentBlocks)=0 then
      begin HideSelect; DrawView; end;
  if PointOfs(SelStart)=PointOfs(SelEnd) then
     SetSelection(CurPos,CurPos);
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
end;

procedure TCodeEditor.CheckSels;
begin
  if (SelStart.Y>SelEnd.Y) or
     ( (SelStart.Y=SelEnd.Y) and (SelStart.X>SelEnd.X) ) then
       SetSelection(SelEnd,SelStart);
end;

function TCodeEditor.UpdateAttrs(FromLine: integer; Attrs: byte): integer;
type
    TCharClass = (ccWhiteSpace,ccTab,ccAlpha,ccNumber,ccSymbol);
var
  SymbolIndex: Sw_integer;
  CurrentCommentType : Byte;
  LastCC: TCharClass;
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
  function MatchesAnySpecSymbol(const What: string; SClass: TSpecSymbolClass; PartialMatch: boolean): boolean;
  var S: string;
      I: Sw_integer;
      Match,Found: boolean;
  begin
    Found:=false;
    if What<>'' then
    for I:=1 to GetSpecSymbolCount(SClass) do
    begin
      SymbolIndex:=I;
      S:=GetSpecSymbol(SClass,I-1);
      if PartialMatch then Match:=MatchSymbol(What,S)
            else Match:=What=S;
      if Match then
    begin MatchingSymbol:=S; Found:=true; Break; end;
    end;
    MatchedSymbol:=MatchedSymbol or Found;
    MatchesAnySpecSymbol:=Found;
  end;

  function IsCommentPrefix: boolean;
  begin
    IsCommentPrefix:=MatchesAnySpecSymbol(SymbolConcat,ssCommentPrefix,true);
  end;

  function IsSingleLineCommentPrefix: boolean;
  begin
    IsSingleLineCommentPrefix:=MatchesAnySpecSymbol(SymbolConcat,ssCommentSingleLinePrefix,true);
  end;

  function IsCommentSuffix: boolean;
  begin
    IsCommentSuffix:=(MatchesAnySpecSymbol(SymbolConcat,ssCommentSuffix,true))
      and (CurrentCommentType=SymbolIndex);
  end;

  function IsStringPrefix: boolean;
  begin
    IsStringPrefix:=MatchesAnySpecSymbol(SymbolConcat,ssStringPrefix,true);
  end;

  function IsStringSuffix: boolean;
  begin
    IsStringSuffix:=MatchesAnySpecSymbol(SymbolConcat,ssStringSuffix,true);
  end;

  function IsDirectivePrefix: boolean;
  begin
    IsDirectivePrefix:=MatchesAnySpecSymbol(SymbolConcat,ssDirectivePrefix,true);
  end;

  function IsDirectiveSuffix: boolean;
  begin
    IsDirectiveSuffix:=MatchesAnySpecSymbol(SymbolConcat,ssDirectiveSuffix,true);
  end;

  function IsAsmPrefix(const WordS: string): boolean;
  begin
    IsAsmPrefix:=MatchesAnySpecSymbol(WordS,ssAsmPrefix,false);
  end;

  function IsAsmSuffix(const WordS: string): boolean;
  begin
    IsAsmSuffix:=MatchesAnySpecSymbol(WordS,ssAsmSuffix,false);
  end;

  function GetCharClass(C: char): TCharClass;
  var CC: TCharClass;
  begin
    if C in WhiteSpaceChars then CC:=ccWhiteSpace else
    if C in TabChars then CC:=ccTab else
    if C in AlphaChars      then CC:=ccAlpha else
    if C in NumberChars     then CC:=ccNumber else
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
      ccNumber     : if copy(WordS,1,1)='$' then
             C:=coHexNumberColor
           else
             C:=coNumberColor;
      ccSymbol     : C:=coSymbolColor;
      ccAlpha      :
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
    if ( (CC<>LastCC) and
         ( (CC<>ccAlpha) or (LastCC<>ccNumber) ) and
         ( (CC<>ccNumber) or (LastCC<>ccAlpha) )
       ) or
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
  if ((Flags and efSyntaxHighlight)=0) or (FromLine>=GetLineCount) then
  begin
    SetLineFormat(FromLine,'');
    UpdateAttrs:=GetLineCount-1;
    Exit;
  end;
  CurLine:=FromLine;
  if CurLine>0 then PrevLine:=Lines^.At(CurLine-1) else PrevLine:=nil;
  repeat
    Line:=Lines^.At(CurLine);
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
    PrevLine:=Line;
  until false;
  UpdateAttrs:=CurLine;
end;

procedure TCodeEditor.DrawLines(FirstLine: integer);
begin
  DrawView;
end;

function TCodeEditor.InsertText(const S: string): Boolean;
var I: integer;
    OldPos: TPoint;
begin
  OldPos:=CurPos;
  for I:=1 to length(S) do
    AddChar(S[I]);
  AddAction(eaInsertText,OldPos,CurPos,S);
  InsertText:=true;
end;

function TCodeEditor.InsertFrom(Editor: PCodeEditor): Boolean;
var OK: boolean;
    LineDelta,LineCount: Sw_integer;
    StartPos,DestPos: TPoint;
    LineStartX,LineEndX: Sw_integer;
    S,OrigS: string;
    VerticalBlock: boolean;
    SEnd: TPoint;
begin
  OK:=(Editor^.SelStart.X<>Editor^.SelEnd.X) or (Editor^.SelStart.Y<>Editor^.SelEnd.Y);
  if OK then
  begin
    StartPos:=CurPos; DestPos:=CurPos;
    VerticalBlock:=(Editor^.Flags and efVerticalBlocks)<>0;
    LineDelta:=0; LineCount:=(Editor^.SelEnd.Y-Editor^.SelStart.Y)+1;
    OK:=GetLineCount<MaxLineCount;
    while OK and (LineDelta<LineCount) do
    begin
      if (LineDelta<LineCount-1) and (VerticalBlock=false) then
      if (LineDelta<>0) or (Editor^.SelEnd.X=0) then
        begin
          Lines^.AtInsert(DestPos.Y,NewLine(''));
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

      if LineEndX<=LineStartX then
        S:=''
      else
        S:=RExpand(copy(Editor^.GetLineText(Editor^.SelStart.Y+LineDelta),LineStartX+1,LineEndX-LineStartX+1),
                   Min(LineEndX-LineStartX+1,255));
      if VerticalBlock=false then
        begin
          OrigS:=GetDisplayText(DestPos.Y);
          SetLineText(DestPos.Y,RExpand(copy(OrigS,1,DestPos.X),DestPos.X)+S+copy(OrigS,DestPos.X+1,255));
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
        S:=RExpand(S,LineEndX-LineStartX+1);
      Inc(LineDelta);
      OK:=GetLineCount<MaxLineCount;
    end;
    if OK=false then EditorDialog(edTooManyLines,nil);
    UpdateAttrs(StartPos.Y,attrAll);
    LimitsChanged;
    SetSelection(CurPos,SEnd);
    if IsClipboard then
       begin Inc(DestPos.X,length(S)); SetCurPtr(DestPos.X,DestPos.Y); end;
    DrawView;
  end;
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
begin
  if (Actions=nil) or (not StoreUndo) then Exit;
  Actions^.Insert(NewEditorAction(AAction,AStartPos,AEndPos,AText));
end;

procedure TCodeEditor.SetSelection(A, B: TPoint);
begin
  SelStart:=A; SelEnd:=B;
  SelectionChanged;
end;

procedure TCodeEditor.SetHighlight(A, B: TPoint);
begin
  Highlight.A:=A; Highlight.B:=B;
  HighlightChanged;
end;

procedure TCodeEditor.SetHighlightRow(Row: integer);
begin
  HighlightRow:=Row;
  DrawView;
end;

procedure TCodeEditor.SelectAll(Enable: boolean);
var A,B: TPoint;
begin
  if (Enable=false) or (GetLineCount=0) then
     begin A:=CurPos; B:=CurPos end
  else
     begin A.X:=0; A.Y:=0; B.Y:=GetLineCount-1; B.X:=length(GetLineText(B.Y)); end;
  SetSelection(A,B);
  DrawView;
end;

procedure TCodeEditor.SelectionChanged;
var Enable,CanPaste: boolean;
begin
  Enable:=((SelStart.X<>SelEnd.X) or (SelStart.Y<>SelEnd.Y)) and (Clipboard<>nil);
  SetCmdState(ToClipCmds,Enable and (Clipboard<>@Self));
  SetCmdState(NulClipCmds,Enable);
  CanPaste:=(Clipboard<>nil) and ((Clipboard^.SelStart.X<>Clipboard^.SelEnd.X) or
       (Clipboard^.SelStart.Y<>Clipboard^.SelEnd.Y));
  SetCmdState(FromClipCmds,CanPaste  and (Clipboard<>@Self));
  Message(Application,evBroadcast,cmCommandSetChanged,nil);
end;

procedure TCodeEditor.HighlightChanged;
begin
  DrawView;
end;

procedure TCodeEditor.SetState(AState: Word; Enable: Boolean);
begin
  inherited SetState(AState,Enable);
  if (AState and (sfActive+sfSelected+sfFocused))<>0 then
     SelectionChanged;
end;

function TCodeEditor.GetPalette: PPalette;
const P: string[length(CEditor)] = CEditor;
begin
  GetPalette:=@P;
end;

constructor TCodeEditor.Load(var S: TStream);
begin
  inherited Load(S);

  New(Actions, Init(500,1000));
  New(Lines, Init(500,1000));
  { we have always need at least 1 line }
  Lines^.Insert(NewLine(''));

  GetPeerViewPtr(S,Indicator);
  S.Read(SelStart,SizeOf(SelStart));
  S.Read(SelEnd,SizeOf(SelEnd));
  S.Read(Highlight,SizeOf(Highlight));
  S.Read(CurPos,SizeOf(CurPos));
  S.Read(StoreUndo,SizeOf(StoreUndo));
  S.Read(IsReadOnly,SizeOf(IsReadOnly));
  S.Read(NoSelect,SizeOf(NoSelect));
  S.Read(Flags,SizeOf(Flags));
  S.Read(TabSize,SizeOf(TabSize));
  S.Read(HighlightRow,SizeOf(HighlightRow));

  UpdateIndicator; LimitsChanged;
end;

procedure TCodeEditor.Store(var S: TStream);
begin
  inherited Store(S);
  PutPeerViewPtr(S,Indicator);
  S.Write(SelStart,SizeOf(SelStart));
  S.Write(SelEnd,SizeOf(SelEnd));
  S.Write(Highlight,SizeOf(Highlight));
  S.Write(CurPos,SizeOf(CurPos));
  S.Write(StoreUndo,SizeOf(StoreUndo));
  S.Write(IsReadOnly,SizeOf(IsReadOnly));
  S.Write(NoSelect,SizeOf(NoSelect));
  S.Write(Flags,SizeOf(Flags));
  S.Write(TabSize,SizeOf(TabSize));
  S.Write(HighlightRow,SizeOf(HighlightRow));
end;

destructor TCodeEditor.Done;
begin
  inherited Done;
  if assigned(Lines) then
    Dispose(Lines, Done);
  If assigned(Actions) then
    Dispose(Actions, Done);
end;

procedure TEditorActionCollection.FreeItem(Item: Pointer);
begin
  if assigned(Item) then
    freemem(Item,Sizeof(TEditorAction));
end;

constructor TFileEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
       PScrollBar; AIndicator: PIndicator;const AFileName: string);
begin
  inherited Init(Bounds,AHScrollBAr,AVScrollBAr,AIndicator,0);
  FileName:=AFileName;
  UpdateIndicator;
  Message(@Self,evBroadcast,cmFileNameChanged,@Self);
end;

function TFileEditor.LoadFile: boolean;
var S: string;
    OK: boolean;
    f: text;
    FM,Line: Sw_integer;
    Buf : Pointer;
begin
  DeleteAllLines;
  GetMem(Buf,EditorTextBufSize);
{$I-}
  FM:=FileMode; FileMode:=0;
  Assign(f,FileName);
  SetTextBuf(f,Buf^,EditorTextBufSize);
  Reset(f);
  OK:=(IOResult=0);
  if Eof(f) then
   AddLine('')
  else
   while OK and (Eof(f)=false) and (GetLineCount<MaxLineCount) do
   begin
     readln(f,S);
     OK:=OK and (IOResult=0);
     if OK then AddLine(S);
   end;
  FileMode:=FM;
  Close(F);
  EatIO;
{$I+}
  LimitsChanged;
  Line:=-1;
  repeat
    Line:=UpdateAttrs(Line+1,attrAll+attrForceFull);
  until Line>=GetLineCount-1;
  TextStart;
  LoadFile:=OK;
  FreeMem(Buf,EditorTextBufSize);
end;

function TFileEditor.SaveFile: boolean;
var S: string;
    OK: boolean;
    f: text;
    Line: Sw_integer;
    P: PLine;
    BAKName: string;
    Buf : Pointer;
begin
  GetMem(Buf,EditorTextBufSize);
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
  Assign(f,FileName);
  Rewrite(f);
  SetTextBuf(f,Buf^,EditorTextBufSize);
  OK:=(IOResult=0); Line:=0;
  while OK and (Line<GetLineCount) do
  begin
    P:=Lines^.At(Line);
    if P^.Text=nil then S:='' else S:=P^.Text^;
    if (Flags and efUseTabCharacters)<>0 then
      S:=CompressUsingTabs(S,TabSize);
    writeln(f,S);
    Inc(Line);
    OK:=OK and (IOResult=0);
  end;
  Close(F);
  EatIO;
{$I+}
  if OK then begin Modified:=false; UpdateIndicator; end;
  SaveFile:=OK;
  FreeMem(Buf,EditorTextBufSize);
end;

function TFileEditor.ShouldSave: boolean;
begin
  ShouldSave:=Modified or (FileName='');
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
begin
  inherited Load(S);
  P:=S.ReadStr;
  FileName:=GetStr(P);
  if P<>nil then DisposeStr(P);

  UpdateIndicator;
  Message(@Self,evBroadcast,cmFileNameChanged,@Self);
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
    CB1: PCheckBoxes;
    RB1,RB2,RB3: PRadioButtons;
begin
  R.Assign(0,0,56,15);
  New(D, Init(R, 'Find'));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2);
    R1.Copy(R); R1.B.X:=17; R1.B.Y:=R1.A.Y+1; R2.Copy(R); R2.A.X:=17; R2.B.Y:=R2.A.Y+1;
    New(IL1, Init(R2, 80));
    IL1^.Data^:=FindStr;
    Insert(IL1);
    Insert(New(PLabel, Init(R1, '~T~ext to find', IL1)));

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
    R1.Copy(R); R1.B.X:=17; R1.B.Y:=R1.A.Y+1; R2.Copy(R); R2.A.X:=17; R2.B.Y:=R2.A.Y+1;
    New(IL1, Init(R2, 80));
    IL1^.Data^:=FindStr;
    Insert(IL1);
    Insert(New(PLabel, Init(R1, '~T~ext to find', IL1)));

    R1.Copy(R); R1.Move(0,2); R1.B.X:=17; R1.B.Y:=R1.A.Y+1;
    R2.Copy(R); R2.Move(0,2); R2.A.X:=17; R2.B.Y:=R2.A.Y+1;
    New(IL2, Init(R2, 80));
    IL2^.Data^:=ReplaceStr;
    Insert(IL2);
    Insert(New(PLabel, Init(R1, '    ~N~ew text', IL2)));

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
    IL: PInputLine;
begin
  R.Assign(0,0,40,7);
  New(D, Init(R, 'Goto line'));
  with D^ do
  begin
    Options:=Options or ofCentered;
    GetExtent(R); R.Grow(-3,-2); R.B.Y:=R.A.Y+1;
    R1.Copy(R); R1.B.X:=27; R2.Copy(R); R2.A.X:=27;
    New(IL, Init(R2,5));
    with TGotoLineDialogRec(Info^) do
    IL^.SetValidator(New(PRangeValidator, Init(1, Lines)));
    Insert(IL);
    Insert(New(PLabel, Init(R1, 'Enter new line ~n~umber', IL)));

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
    edSaveAs:
      begin
   Name:=PString(Info)^;
   Re:=Application^.ExecuteDialog(New(PFileDialog, Init('*'+DefaultSaveExt,
   'Save file as', '~N~ame', fdOkButton, 101)), @Name);
   if (Re<>cmCancel) and (Name<>PString(Info)^) then
     if ExistsFile(Name) then
       if EditorDialog(edReplaceFile,@Name)<>cmYes then
         Re:=cmCancel;
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
  RegisterType(RIndicator);
  RegisterType(RCodeEditor);
  RegisterType(RFileEditor);
end;

END.
{
  $Log$
  Revision 1.29  1999-04-07 21:55:59  peter
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
