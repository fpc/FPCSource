{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998-2000 by Berczi Gabor

    Code editor template objects

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
unit WCEdit;

interface

uses Objects,Drivers,Views,
     WUtils,WEditor;

type
    PCodeEditor = ^TCodeEditor;

    PIndicator = ^TIndicator;
    TIndicator = object(TView)
      Location: TPoint;
      Modified : Boolean;
      CodeOwner : PCodeEditor;
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
    end;

    PLine = ^TLine;
    TLine = object(TCustomLine)
    public { internal use only! }
      Text        : PString;
      DefaultEditorInfo : PEditorLineInfo;
      EditorInfos : PEditorLineInfoCollection;
      Flags       : longint;
      Owner       : PCustomCodeEditorCore;
      procedure AddEditorInfo(Index: sw_integer; AEditor: PCustomCodeEditor); virtual;
      procedure RemoveEditorInfo(AEditor: PCustomCodeEditor); virtual;
    public
      constructor Init(AOwner: PCustomCodeEditorCore; const AText: string; AFlags: longint);
      function    GetText: string; virtual;
      procedure   SetText(const AText: string); virtual;
      function    GetEditorInfo(Editor: PCustomCodeEditor): PEditorLineInfo; virtual;
      function    GetFlags: longint; virtual;
      procedure   SetFlags(AFlags: longint); virtual;
      destructor  Done; virtual;
    end;

    PCodeEditorCore = ^TCodeEditorCore;
    TCodeEditorCore = object(TCustomCodeEditorCore)
    {$ifdef TP}public{$else}protected{$endif}
      Lines      : PLineCollection;
      CanUndo    : Boolean;
      StoreUndo  : boolean;
      Modified   : Boolean;
      ReadOnly   : Boolean;
      TabSize    : integer;
      IndentSize  : integer;
      ModifiedTime : cardinal;
    public
      UndoList   : PEditorActionCollection;
      RedoList   : PEditorActionCollection;
      constructor Init;
      destructor  Done; virtual;
      procedure   ChangeLinesTo(ALines : PLineCollection); virtual;
      function    GetModified: boolean; virtual;
      procedure   SetModified(AModified: boolean); virtual;
      function    GetModifyTime: cardinal; virtual;
      function    GetTabSize: integer; virtual;
      procedure   SetTabSize(ATabSize: integer); virtual;
      function    GetIndentSize: integer; virtual;
      procedure   SetIndentSize(AIndentSize: integer); virtual;
      function    GetStoreUndo: boolean; virtual;
      procedure   SetStoreUndo(AStore: boolean); virtual;
      function    GetSyntaxCompleted: boolean; virtual;
      procedure   SetSyntaxCompleted(SC : boolean); virtual;
      function    GetLastSyntaxedLine: sw_integer; virtual;
      procedure   SetLastSyntaxedLine(ALine: sw_integer); virtual;
      { Storage }
    {$ifdef TP}public{$else}protected{$endif}
      { Text & info storage abstraction }
      procedure   ISetLineFlagState(Binding: PEditorBinding; LineNo: sw_integer; Flag: longint; ASet: boolean); virtual;
      procedure   IGetDisplayTextFormat(Binding: PEditorBinding; LineNo: sw_integer;var DT,DF:string); virtual;
      function    IGetLineFormat(Binding: PEditorBinding; LineNo: sw_integer): string; virtual;
      procedure   ISetLineFormat(Binding: PEditorBinding; LineNo: sw_integer;const S: string); virtual;
    public
      { Text & info storage abstraction }
      function    GetLineCount: sw_integer; virtual;
      function    GetLine(LineNo: sw_integer): PCustomLine; virtual;
      function    GetLineText(LineNo: sw_integer): string; virtual;
      procedure   SetDisplayText(I: sw_integer;const S: string); virtual;
      function    GetDisplayText(I: sw_integer): string; virtual;
      procedure   SetLineText(I: sw_integer;const S: string); virtual;
      procedure   DeleteAllLines; virtual;
      procedure   DeleteLine(I: sw_integer); virtual;
      function    InsertLine(LineNo: sw_integer; const S: string): PCustomLine; virtual;
      procedure   AddLine(const S: string); virtual;
      procedure   GetContent(ALines: PUnsortedStringCollection); virtual;
      procedure   SetContent(ALines: PUnsortedStringCollection); virtual;
   public
     { Undo info storage }
      procedure   AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string;AFlags : longint); virtual;
      procedure   AddGroupedAction(AAction : byte); virtual;
      procedure   CloseGroupedAction(AAction : byte); virtual;
      function    GetUndoActionCount: sw_integer; virtual;
      function    GetRedoActionCount: sw_integer; virtual;
    private
      OnDiskLoadTime : cardinal;
      SystemLoadTime : cardinal;
      procedure LinesInsert(Idx: sw_integer; Line: PLine);
    end;

    TCodeEditor = object(TCustomCodeEditor)
      Core       : PCodeEditorCore;
      Flags      : longint;
      Indicator  : PIndicator;
      HighlightRow: sw_integer;
      DebuggerRow: sw_integer;
      CodeCompleteFrag: PString;
      CodeCompleteWord: PString;
      ReadOnly   : boolean;
      CompleteState: TCompleteState;
      ErrorMessage: PString;
      IndicatorDrawCalled  : boolean;
      Folds      : PFoldCollection;
      MaxFoldLevel: sw_integer;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator; ACore: PCodeEditorCore);
    public
      procedure   DrawIndicator; virtual;
    public
      function    GetFlags: longint; virtual;
      procedure   SetFlags(AFlags: longint); virtual;
      function    GetModified: boolean; virtual;
      procedure   SetModified(AModified: boolean); virtual;
      function    GetStoreUndo: boolean; virtual;
      procedure   SetStoreUndo(AStore: boolean); virtual;
      procedure   ClearUndoList;
      function    GetSyntaxCompleted: boolean; virtual;
      procedure   SetSyntaxCompleted(SC : boolean); virtual;
      function    GetLastSyntaxedLine: sw_integer; virtual;
      procedure   SetLastSyntaxedLine(ALine: sw_integer); virtual;
      function    GetTabSize: integer; virtual;
      procedure   SetTabSize(ATabSize: integer); virtual;
      function    GetIndentSize: integer; virtual;
      procedure   SetIndentSize(AIndentSize: integer); virtual;
      function    IsReadOnly: boolean; virtual;
    public
      procedure   UpdateIndicator; virtual;
      procedure   ModifiedChanged; virtual;
      procedure   PositionChanged; virtual;
      procedure   LimitsChanged; virtual;
      function    IsClipboard: Boolean; virtual;
      function    LoadFromStream(Stream: PFastBufStream): boolean; virtual;
      function    SaveToStream(Stream: PStream): boolean; virtual;
      function    SaveAreaToStream(Stream: PStream; StartP,EndP: TPoint): boolean;virtual;
      destructor  Done; virtual;
    public
{      ChangedLine : sw_integer;}
      { Text & info storage abstraction }
      function    GetLineCount: sw_integer; virtual;
      function    GetLine(LineNo: sw_integer): PCustomLine; virtual;
      function    CharIdxToLinePos(Line,CharIdx: sw_integer): sw_integer; virtual;
      function    LinePosToCharIdx(Line,X: sw_integer): sw_integer; virtual;
      function    GetLineText(I: sw_integer): string; virtual;
      procedure   SetDisplayText(I: sw_integer;const S: string); virtual;
      function    GetDisplayText(I: sw_integer): string; virtual;
      procedure   SetLineText(I: sw_integer;const S: string); virtual;
      procedure   GetDisplayTextFormat(I: sw_integer;var DT,DF:string); virtual;
      function    GetLineFormat(I: sw_integer): string; virtual;
      procedure   SetLineFormat(I: sw_integer;const S: string); virtual;
      procedure   DeleteAllLines; virtual;
      procedure   DeleteLine(I: sw_integer); virtual;
      function    InsertLine(LineNo: sw_integer; const S: string): PCustomLine; virtual;
      procedure   AddLine(const S: string); virtual;
      function    GetErrorMessage: string; virtual;
      procedure   SetErrorMessage(const S: string); virtual;
      procedure   GetContent(ALines: PUnsortedStringCollection); virtual;
      procedure   SetContent(ALines: PUnsortedStringCollection); virtual;
      procedure   Lock; virtual;
      procedure   UnLock; virtual;
    public
     { CodeComplete support }
      function    GetCodeCompleteWord: string; virtual;
      procedure   SetCodeCompleteWord(const S: string); virtual;
      function    GetCodeCompleteFrag: string; virtual;
      procedure   SetCodeCompleteFrag(const S: string); virtual;
      function    GetCompleteState: TCompleteState; virtual;
      procedure   SetCompleteState(AState: TCompleteState); virtual;
   public
      { Syntax highlight }
   {a}function    UpdateAttrs(FromLine: sw_integer; Attrs: byte): sw_integer; virtual;
   {a}function    UpdateAttrsRange(FromLine, ToLine: sw_integer; Attrs: byte): sw_integer; virtual;
   public
     { Undo info storage }
      procedure   AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string;AFlags : longint); virtual;
      procedure   AddGroupedAction(AAction : byte); virtual;
      procedure   CloseGroupedAction(AAction : byte); virtual;
      function    GetUndoActionCount: sw_integer; virtual;
      function    GetRedoActionCount: sw_integer; virtual;
      procedure   JumpToLastCursorPos; virtual;
      procedure   Undo; virtual;
      procedure   Redo; virtual;
     { Fold support }
      function    GetMaxFoldLevel: sw_integer; virtual;
      function    GetFoldCount: sw_integer; virtual;
      function    GetFold(Index: sw_integer): PFold; virtual;
      procedure   RegisterFold(AFold: PFold); virtual;
      procedure   UnRegisterFold(AFold: PFold); virtual;
    end;

    PFileEditor = ^TFileEditor;
    TFileEditor = object(TCodeEditor)
      FileName: string;
      constructor Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator; ACore: PCodeEditorCore; const AFileName: string);
      function    Save: Boolean; virtual;
      function    SaveAs: Boolean; virtual;
      function    SaveAsk(Force: boolean): Boolean; virtual;
      function    LoadFile: boolean; virtual;
      function    ReloadFile: boolean; virtual;
      function    SaveFile: boolean; virtual;
      function    Valid(Command: Word): Boolean; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    ShouldSave: boolean; virtual;
      function    IsChangedOnDisk : boolean;
    public
      procedure   BindingsChanged; virtual;
    end;

function DefUseSyntaxHighlight(Editor: PFileEditor): boolean;
function DefUseTabsPattern(Editor: PFileEditor): boolean;

const
     DefaultCodeEditorFlags : longint =
       efBackupFiles+efInsertMode+efAutoIndent+efPersistentBlocks+
       {efUseTabCharacters+}efBackSpaceUnindents+efSyntaxHighlight+
       efExpandAllTabs+efCodeComplete{+efFolds};
     DefaultTabSize     : integer = 8;
     DefaultIndentSize   : integer = 1;
     UseSyntaxHighlight : function(Editor: PFileEditor): boolean = {$ifdef fpc}@{$endif}DefUseSyntaxHighlight;
     UseTabsPattern     : function(Editor: PFileEditor): boolean = {$ifdef fpc}@{$endif}DefUseTabsPattern;

procedure RegisterWCEdit;

implementation

uses Dos,
     WConsts,
     FVConsts,
     App,WViews;

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

constructor TLine.Init(AOwner: PCustomCodeEditorCore; const AText: string; AFlags: longint);
begin
  inherited Init(AText,AFlags);
  // New(EditorInfos, Init(10,10));
  Owner:=AOwner;
end;

procedure TLine.AddEditorInfo(Index: sw_integer; AEditor: PCustomCodeEditor);
begin
  if Index=0 then
    begin
      DefaultEditorInfo:=New(PEditorLineInfo, Init(AEditor));
      exit;
    end;
  if not assigned(EditorInfos) then
    begin
      New(EditorInfos, Init(10,10));
      EditorInfos^.AtInsert(0,DefaultEditorInfo);
      DefaultEditorInfo:=nil;
    end;
  EditorInfos^.AtInsert(Index,New(PEditorLineInfo, Init(AEditor)));
end;

procedure TLine.RemoveEditorInfo(AEditor: PCustomCodeEditor);
var E: PEditorLineInfo;
begin
  E:=GetEditorInfo(AEditor);
  if Assigned(EditorInfos) then
    EditorInfos^.Free(E);
end;

function TLine.GetText: string;
begin
  GetText:=GetStr(Text);
end;

procedure TLine.SetText(const AText: string);
begin
  SetStr(Text,AText);
end;

function TLine.GetEditorInfo(Editor: PCustomCodeEditor): PEditorLineInfo;
function Match(P: PEditorLineInfo): boolean; {$ifdef TP}far;{$endif}
begin
  Match:=P^.Editor=Editor;
end;
begin
  if not assigned(EditorInfos) then
    GetEditorInfo:=DefaultEditorInfo
  else
    GetEditorInfo:=EditorInfos^.FirstThat(@Match);
end;

function TLine.GetFlags: longint;
begin
  GetFlags:=Flags;
end;

procedure TLine.SetFlags(AFlags: longint);
begin
  Flags:=AFlags;
  if Assigned(Owner) then
    Owner^.ContentsChanged;
end;

destructor TLine.Done;
begin
  if Assigned(Text) then
    DisposeStr(Text);
  Text:=nil;
  if Assigned(EditorInfos) then
    Dispose(EditorInfos, Done);
  EditorInfos:=nil;
  if Assigned(DefaultEditorInfo) then
    Dispose(DefaultEditorInfo, Done);
  DefaultEditorInfo:=nil;
  inherited Done;
end;

constructor TCodeEditorCore.Init;
begin
  inherited Init;
  StoreUndo:=true;
  new(UndoList,init(500,1000));
  new(RedoList,init(500,1000));
  New(Lines, Init(500,1000));
  TabSize:=DefaultTabSize;
  IndentSize:=DefaultIndentSize;
  OnDiskLoadTime:=0;
  SystemLoadTime:=0;
end;

procedure TCodeEditorCore.ChangeLinesTo(ALines : PLineCollection);
begin
  if assigned(lines) then
    Dispose(Lines,Done);
  Lines:=ALines;
end;

function TCodeEditorCore.GetLineCount: sw_integer;
begin
  GetLineCount:=Lines^.Count;
end;

function TCodeEditorCore.GetLine(LineNo: sw_integer): PCustomLine;
begin
  GetLine:=Lines^.At(LineNo);
end;

function TCodeEditorCore.GetModified: boolean;
begin
  GetModified:=Modified;
end;

function TCodeEditorCore.GetModifyTime: cardinal;
begin
  GetModifyTime:=ModifiedTime;
end;

procedure TCodeEditorCore.SetModified(AModified: boolean);
begin
  if AModified<>Modified then
  begin
    Modified:=AModified;
    ModifiedChanged;
  end;
  ModifiedTime:=cardinal(Now);
end;

function TCodeEditorCore.GetStoreUndo: boolean;
begin
  GetStoreUndo:=StoreUndo;
end;

procedure TCodeEditorCore.SetStoreUndo(AStore: boolean);
begin
  if StoreUndo<>AStore then
  begin
    StoreUndo:=AStore;
    StoreUndoChanged;
  end;
end;

function TCodeEditorCore.GetSyntaxCompleted: boolean;
begin
{$ifdef TEST_PARTIAL_SYNTAX}
  GetSyntaxCompleted:=SyntaxComplete;
{$else}
  GetSyntaxCompleted:=true;
{$endif}
end;

procedure TCodeEditorCore.SetSyntaxCompleted(SC: boolean);
begin
{$ifdef TEST_PARTIAL_SYNTAX}
  if SC<>SyntaxComplete then
    begin
      SyntaxComplete:=SC;
    end;
{$endif}
end;
function TCodeEditorCore.GetLastSyntaxedLine: sw_integer;
begin
  GetLastSyntaxedLine:=LastSyntaxedLine;
end;

procedure TCodeEditorCore.SetLastSyntaxedLine(ALine: sw_integer);
begin
  LastSyntaxedLine:=ALine;
end;


procedure TCodeEditorCore.ISetLineFlagState(Binding: PEditorBinding; LineNo: sw_integer; Flag: longint; ASet: boolean);
var P: PCustomLine;
begin
  if LineNo<GetLineCount then
  begin
    P:=GetLine(LineNo);
    if assigned(P) then
      P^.SetFlagState(Flag,ASet);
  end;
end;

procedure TCodeEditorCore.GetContent(ALines: PUnsortedStringCollection);
procedure AddIt(P: PCustomLine); {$ifndef FPC}far;{$endif}
begin
  if Assigned(P) then
    ALines^.Insert(NewStr(P^.GetText));
end;
begin
  if Assigned(Lines) then
    Lines^.ForEach(@AddIt);
end;

procedure TCodeEditorCore.SetContent(ALines: PUnsortedStringCollection);
procedure AddIt(P: PString); {$ifndef FPC}far;{$endif}
begin
  AddLine(GetStr(P));
end;
begin
  DeleteAllLines;
  if Assigned(ALines) then
    ALines^.ForEach(@AddIt);
  LimitsChanged;
end;

function TCodeEditorCore.GetTabSize: integer;
begin
  GetTabSize:=TabSize;
end;

procedure TCodeEditorCore.SetTabSize(ATabSize: integer);
begin
  if ATabSize<>TabSize then
  begin
    TabSize:=ATabSize;
    TabSizeChanged;
  end;
end;

function TCodeEditorCore.GetIndentSize: integer;
begin
  GetIndentSize:=IndentSize;
end;

procedure TCodeEditorCore.SetIndentSize(AIndentSize: integer);
begin
  if AIndentSize<>IndentSize then
  begin
    IndentSize:=AIndentSize;
  end;
end;

function TCodeEditorCore.GetLineText(LineNo: sw_integer): string;
var
  L : PCustomLine;
begin
  GetLineText:='';
  if LineNo<Lines^.Count then
   begin
     L:=Lines^.At(LineNo);
     GetLineText:=L^.GetText;
   end;
end;

procedure TCodeEditorCore.LinesInsert(Idx: sw_integer; Line: PLine);
var I: sw_integer;
procedure RegLine(P: PEditorBinding); {$ifndef FPC}far;{$endif}
begin
  Line^.AddEditorInfo(I,P^.Editor);
  Inc(I);
end;
begin
  if Idx=-1 then Idx:=Lines^.Count;
  I:=0;
  Bindings^.ForEach(@RegLine);
  Lines^.AtInsert(Idx,Line);
end;

procedure TCodeEditorCore.SetLineText(I: sw_integer;const S: string);
var
  L : PCustomLine;
  AddCount : Sw_Integer;
begin
  AddCount:=0;
  while (Lines^.Count<I+1) do
   begin
     LinesInsert(-1,New(PLine, Init(@Self,'',0)));
     Inc(AddCount);
   end;
  if AddCount>0 then
   LimitsChanged;
  L:=Lines^.At(I);
  L^.SetText(S);
  ContentsChanged;
end;

function TCodeEditorCore.GetDisplayText(I: sw_integer): string;
begin
  GetDisplayText:=ExtractTabs(GetLineText(I),GetTabSize);
end;

procedure TCodeEditorCore.SetDisplayText(I: sw_integer;const S: string);
begin
  { I disagree here
    I don't want the editor to change the position of the tabs
    in my makefiles !! PM
  if FlagSet(efUseTabCharacters) and (TabSize>0) then
   SetLineText(I,CompressUsingTabs(S,TabSize))
  else                  }
  { ... then you better make this optional - Gabor }
   SetLineText(I,S);
end;

procedure TCodeEditorCore.IGetDisplayTextFormat(Binding: PEditorBinding; LineNo: sw_integer;var DT,DF:string);
var
  L : PCustomLine;
  P,PAdd : SW_Integer;
begin
  DF:='';
  DT:='';
  if (0<=LineNo) and (LineNo<GetLineCount) then
   begin
     L:=GetLine(LineNo);
     if not assigned(L) then
       exit;
     DF:=IGetLineFormat(Binding,LineNo);
     DT:=L^.GetText;
     p:=0;
     while p<length(DT) do
      begin
        inc(p);
        if DT[p]=#9 then
         begin
           PAdd:=TabSize-((p-1) mod TabSize);
           if DF<>'' then
            DF:=copy(DF,1,P-1)+CharStr(DF[p],PAdd)+copy(DF,P+1,High(DF));
           DT:=copy(DT,1,P-1)+CharStr(' ',PAdd)+copy(DT,P+1,High(DF));
           inc(P,PAdd-1);
         end;
      end;
   end;
end;

function TCodeEditorCore.IGetLineFormat(Binding: PEditorBinding; LineNo: sw_integer): string;
var P: PCustomLine;
    LI: PEditorLineInfo;
    S: string;
begin
  if (0<=LineNo) and (LineNo<GetLineCount) then
    P:=GetLine(LineNo)
  else
    P:=nil;
  if P=nil then LI:=nil else
    LI:=P^.GetEditorInfo(Binding^.Editor);
  if LI=nil then S:='' else S:=LI^.GetFormat;
  IGetLineFormat:=S;
end;

procedure TCodeEditorCore.ISetLineFormat(Binding: PEditorBinding; LineNo: sw_integer;const S: string);
var P: PCustomLine;
    LI: PEditorLineInfo;
begin
  if (LineNo<GetLineCount) then
  begin
    P:=GetLine(LineNo);
    if P=nil then LI:=nil else LI:=P^.GetEditorInfo(Binding^.Editor);
    if Assigned(LI) then LI^.SetFormat(S);
  end;
end;

procedure TCodeEditorCore.DeleteAllLines;
begin
  if Assigned(Lines) then
    Lines^.FreeAll;
end;

procedure TCodeEditorCore.DeleteLine(I: sw_integer);
var
 CP : Tpoint;
begin
  if I<Lines^.Count then
    begin
      if StoreUndo then
        begin
          CP.X:=0;CP.Y:=I;
          AddAction(eaDeleteLine,CP,CP,GetLineText(I),0);
       end;
      Lines^.AtFree(I);
    end;
end;

function TCodeEditorCore.InsertLine(LineNo: sw_integer; const S: string): PCustomLine;
var L: PLine;
begin
  L:=New(PLine, Init(@Self,S,0));
  LinesInsert(LineNo, L);
  InsertLine:=L;
end;

procedure TCodeEditorCore.AddLine(const S: string);
begin
  LinesInsert(-1,New(PLine, Init(@Self,S,0)));
end;

procedure TCodeEditorCore.AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string;AFlags : longint);
var
  ActionIntegrated : boolean;
  pa : PEditorAction;
  S : String;
begin
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
          (AAction=eaOverwriteText) or
          (AAction=eaDeleteText)) and
         { do not group if a new grouped_action started }
          (not assigned(UndoList^.CurrentGroupedAction) or
           (UndoList^.CurrentGroupedAction^.ActionCount>0))
         then
        begin
          pa^.EndPos:=AEndPos;
          S:=GetStr(pa^.text);
          if S<>'' then
           DisposeStr(pa^.text);
          if (AAction=eaDeleteText) and
             (AStartPos.X>AEndPos.X) then
            pa^.text:=NewStr(AText+S)
          else
            pa^.text:=NewStr(S+AText);
          ActionIntegrated:=true;
        end;
    end;
  if not ActionIntegrated then
    begin
      UndoList^.Insert(New(PEditorAction,Init(AAction,AStartPos,AEndPos,AText,AFlags)));
      if assigned(UndoList^.CurrentGroupedAction) then
        Inc(UndoList^.CurrentGroupedAction^.actionCount);
      UpdateUndoRedo(cmUndo,AAction);
    end;
  if UndoList^.count>0 then
  begin
    UpdateUndoRedo(cmRedo,0);
    RedoList^.FreeAll;
  end;
end;

procedure TCodeEditorCore.AddGroupedAction(AAction : byte);
begin
  if (UndoList=nil) or (not StoreUndo) then Exit;
  if Assigned(UndoList^.CurrentGroupedAction) then
    inc(UndoList^.GroupLevel)
  else
    begin
      UndoList^.CurrentGroupedAction:=New(PEditorAction,Init_group(AAction));
      UndoList^.GroupLevel:=1;
    end;
end;

procedure TCodeEditorCore.CloseGroupedAction(AAction : byte);
begin
  if (UndoList=nil) or (not StoreUndo) then Exit;
  dec(UndoList^.GroupLevel);
  if UndoList^.GroupLevel=0 then
    begin
      UndoList^.CurrentGroupedAction^.TimeStamp:=now;
      UndoList^.Insert(UndoList^.CurrentGroupedAction);
      UndoList^.CurrentGroupedAction:=nil;
      UpdateUndoRedo(cmUndo,AAction);
    end;
end;

function TCodeEditorCore.GetUndoActionCount: sw_integer;
begin
  GetUndoActionCount:=UndoList^.Count;
end;

function TCodeEditorCore.GetRedoActionCount: sw_integer;
begin
  GetRedoActionCount:=RedoList^.Count;
end;

destructor TCodeEditorCore.Done;
begin
  inherited Done;
  if Assigned(Lines) then Dispose(Lines, Done); Lines:=nil;
  if Assigned(RedoList) then Dispose(RedoList, Done); RedoList:=nil;
  if Assigned(UndoList) then Dispose(UndoList, Done); UndoList:=nil;
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
  if assigned(CodeOwner) and
     (CodeOwner^.ELockFlag>0) then
    begin
      CodeOwner^.IndicatorDrawCalled:=true;
      exit;
    end;
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

{constructor TIndicator.Load(var S: TStream);
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
end;}

{*****************************************************************************
                TCodeEditor
*****************************************************************************}

constructor TCodeEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
          PScrollBar; AIndicator: PIndicator; ACore: PCodeEditorCore);
begin
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
  New(Folds, Init(100,100));
  if ACore=nil then ACore:=New(PCodeEditorCore, Init);
  Core:=ACore;
  Core^.BindEditor(@Self);
  SetState(sfCursorVis,true);
  SetFlags(DefaultCodeEditorFlags);
  SetCurPtr(0,0);
  Indicator:=AIndicator;
  if assigned(Indicator) then
    Indicator^.CodeOwner:=@Self;
  UpdateIndicator;
  LimitsChanged;
end;

function TCodeEditor.GetFlags: longint;
begin
  GetFlags:=Flags;
end;

procedure TCodeEditor.SetFlags(AFlags: longint);
var OFlags: longint;
begin
  if AFlags<>Flags then
  begin
    OFlags:=Flags;
    Flags:=AFlags;
    FlagsChanged(OFlags);
  end;
end;

function TCodeEditor.GetModified: boolean;
begin
  GetModified:=Core^.GetModified;
end;

procedure TCodeEditor.SetModified(AModified: boolean);
begin
  Core^.SetModified(AModified);
end;

function TCodeEditor.GetStoreUndo: boolean;
begin
  GetStoreUndo:=Core^.GetStoreUndo;
end;

procedure TCodeEditor.SetStoreUndo(AStore: boolean);
begin
  Core^.SetStoreUndo(AStore);
end;

procedure TCodeEditor.ClearUndoList;
begin
  Core^.UndoList^.FreeAll;
  Core^.RedoList^.FreeAll;
end;

function TCodeEditor.GetSyntaxCompleted: boolean;
begin
  GetSyntaxCompleted:=Core^.GetSyntaxCompleted;
end;

procedure TCodeEditor.SetSyntaxCompleted(SC : boolean);
begin
  Core^.SetSyntaxCompleted(SC);
  UpdateIndicator;
end;

function TCodeEditor.GetLastSyntaxedLine: sw_integer;
begin
  GetLastSyntaxedLine:=Core^.GetLastSyntaxedLine;
end;

procedure TCodeEditor.SetLastSyntaxedLine(ALine: sw_integer);
begin
  Core^.SetLastSyntaxedLine(ALine);
end;

function TCodeEditor.GetTabSize: integer;
begin
  GetTabSize:=Core^.GetTabSize;
end;

procedure TCodeEditor.SetTabSize(ATabSize: integer);
begin
  Core^.SetTabSize(ATabSize);
end;

function TCodeEditor.GetIndentSize: integer;
begin
  GetIndentSize:=Core^.GetIndentSize;
end;

procedure TCodeEditor.SetIndentSize(AIndentSize: integer);
begin
  Core^.SetIndentSize(AIndentSize);
end;

function TCodeEditor.IsReadOnly: boolean;
begin
  IsReadOnly:=ReadOnly or (Core^.ReadOnly);
end;

function TCodeEditor.IsClipboard: Boolean;
begin
  IsClipboard:=Core^.IsClipboard;
end;

function TCodeEditor.GetErrorMessage: string;
begin
  GetErrorMessage:=GetStr(ErrorMessage);
end;

procedure TCodeEditor.SetErrorMessage(const S: string);
begin
  SetStr(ErrorMessage,S);
  DrawView;
end;

function TCodeEditor.GetLineCount: sw_integer;
begin
  GetLineCount:=Core^.GetLineCount;
end;

function TCodeEditor.GetLine(LineNo: sw_integer): PCustomLine;
begin
  GetLine:=Core^.GetLine(LineNo);
end;

function TCodeEditor.CharIdxToLinePos(Line,CharIdx: sw_integer): sw_integer;
begin
  CharIdxToLinePos:=Core^.CharIdxToLinePos(Line,CharIdx);
end;

function TCodeEditor.LinePosToCharIdx(Line,X: sw_integer): sw_integer;
begin
  LinePosToCharIdx:=Core^.LinePosToCharIdx(Line,X);
end;

function TCodeEditor.GetLineText(I: sw_integer): string;
begin
  GetLineText:=Core^.GetLineText(I);
end;

procedure TCodeEditor.SetDisplayText(I: sw_integer;const S: string);
begin
  Core^.SetDisplayText(I,S);
end;

function TCodeEditor.GetDisplayText(I: sw_integer): string;
begin
  GetDisplayText:=Core^.GetDisplayText(I);
end;

procedure TCodeEditor.SetLineText(I: sw_integer;const S: string);
begin
  Core^.SetLineText(I,S);
end;

procedure TCodeEditor.GetDisplayTextFormat(I: sw_integer;var DT,DF:string);
begin
  Core^.GetDisplayTextFormat(@Self,I,DT,DF);
end;

function TCodeEditor.GetLineFormat(I: sw_integer): string;
begin
  GetLineFormat:=Core^.GetLineFormat(@Self,I);
end;

procedure TCodeEditor.SetLineFormat(I: sw_integer;const S: string);
begin
  Core^.SetLineFormat(@Self,I,S);
end;

procedure TCodeEditor.DeleteAllLines;
begin
  Core^.DeleteAllLines;
end;

procedure TCodeEditor.DeleteLine(I: sw_integer);
begin
  Core^.DeleteLine(I);
end;

function TCodeEditor.InsertLine(LineNo: sw_integer; const S: string): PCustomLine;
begin
  InsertLine:=Core^.InsertLine(LineNo,S);
end;

procedure TCodeEditor.AddLine(const S: string);
begin
  Core^.AddLine(S);
end;

function TCodeEditor.GetMaxFoldLevel: sw_integer;
begin
  GetMaxFoldLevel:=MaxFoldLevel;
end;

procedure TCodeEditor.RegisterFold(AFold: PFold);
var L: sw_integer;
begin
  if Assigned(Folds) then
  begin
    Folds^.Insert(AFold);
    L:=AFold^.GetLevel+1;
    if L>MaxFoldLevel then MaxFoldLevel:=L;
  end;
end;

procedure TCodeEditor.UnRegisterFold(AFold: PFold);
begin
  if Assigned(Folds) then
  begin
    Folds^.Delete(AFold);
    if Folds^.Count=0 then
      MaxFoldLevel:=0
    else
      MaxFoldLevel:=inherited GetMaxFoldLevel+1;
  end;
end;

function TCodeEditor.GetFoldCount: sw_integer;
begin
  GetFoldCount:=Folds^.Count;
end;

function TCodeEditor.GetFold(Index: sw_integer): PFold;
begin
  GetFold:=Folds^.At(Index);
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
  L: PCustomLine;
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

procedure TCodeEditor.GetContent(ALines: PUnsortedStringCollection);
begin
  Core^.GetContent(ALines);
end;

procedure TCodeEditor.SetContent(ALines: PUnsortedStringCollection);
begin
  Lock;
  TextStart; HideSelect;
  Core^.SetContent(ALines);
  LimitsChanged;
  if IsFlagSet(efSyntaxHighlight) then
    Core^.UpdateAttrsRange(0,Min(Delta.Y+Size.Y,GetLineCount-1),
      attrAll
{$ifndef TEST_PARTIAL_SYNTAX}
      +attrForceFull
{$endif TEST_PARTIAL_SYNTAX}
      );
  TextStart;
  UnLock;
end;

function TCodeEditor.GetCodeCompleteFrag: string;
begin
  GetCodeCompleteFrag:=GetStr(CodeCompleteFrag);
end;

procedure TCodeEditor.SetCodeCompleteFrag(const S: string);
begin
  SetStr(CodeCompleteFrag,S);
end;

function TCodeEditor.GetCompleteState: TCompleteState;
begin
  GetCompleteState:=CompleteState;
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

function TCodeEditor.GetCodeCompleteWord: string;
begin
  GetCodeCompleteWord:=GetStr(CodeCompleteWord);
end;

procedure TCodeEditor.SetCodeCompleteWord(const S: string);
begin
  if Assigned(CodeCompleteWord) then DisposeStr(CodeCompleteWord);
  CodeCompleteWord:=NewStr(S);
  inherited SetCodeCompleteWord(S);
end;

procedure TCodeEditor.DrawIndicator;
begin
  if Assigned(Indicator) then
    Indicator^.DrawView;
end;

procedure TCodeEditor.Lock;
begin
  inherited Lock;
  Core^.Lock(@Self);
end;

procedure TCodeEditor.UnLock;
begin
  Core^.UnLock(@Self);
  inherited UnLock;
  If (ELockFlag=0) and IndicatorDrawCalled then
    begin
      DrawIndicator;
      IndicatorDrawCalled:=false;
    end;
end;

procedure TCodeEditor.UpdateIndicator;
begin
  if Indicator<>nil then
  begin
    Indicator^.Location:=CurPos;
    Indicator^.Modified:=GetModified;
{$ifdef debug}
    Indicator^.StoreUndo:=GetStoreUndo;
{$ifdef TEST_PARTIAL_SYNTAX}
    Indicator^.SyntaxComplete:=GetSyntaxCompleted and IsFlagSet(efSyntaxHighlight);
{$endif TEST_PARTIAL_SYNTAX}
    Indicator^.UseTabs:=IsFlagSet(efUseTabCharacters);
{$endif debug}
    if Elockflag>0 then
      IndicatorDrawCalled:=true
    else
      Indicator^.DrawView;
  end;
end;

procedure TCodeEditor.LimitsChanged;
begin
  Core^.LimitsChanged;
end;

procedure TCodeEditor.ModifiedChanged;
begin
  UpdateIndicator;
end;

procedure TCodeEditor.PositionChanged;
begin
  UpdateIndicator;
end;

procedure TCodeEditor.JumpToLastCursorPos;
var
  pa : PEditorAction;
begin
  if (Core^.UndoList^.count>0) and (Core^.RedoList^.count=0) then
    begin
      { Or should we just call Undo ?? PM }
      pa:=Core^.UndoList^.At(Core^.UndoList^.count-1);
      if (pa^.action=eaMoveCursor) then
        SetCurPtr(pa^.StartPos.X,pa^.StartPos.Y);
    end;
end;

procedure TCodeEditor.Undo;
var
  Temp,Idx,Last,Count : Longint;
  StoredFlags : longint;
  UndoTime : longint;
  WasInserting,IsGrouped,HadefNoIndent : boolean;
  MaxY,MinY : sw_integer;
  Line : String;

  procedure SetMinMax(y : sw_integer);
    begin
      if MinY=-1 then
        MinY:=Y;
      if Y<MinY then
        MinY:=Y;
      if MaxY=-1 then
        MaxY:=Y;
      if Y>MaxY then
        MaxY:=Y;
    end;
begin
  Core^.SetStoreUndo(False);
  Lock;
  MinY:=-1;
  MaxY:=-1;
  if Core^.UndoList^.count > 0 then
  begin
    Last:=Core^.UndoList^.count-1;
    if Core^.UndoList^.At(Last)^.Is_grouped_action then
      begin
        Count:=Core^.UndoList^.At(Last)^.ActionCount;
        UndoTime:=Core^.UndoList^.At(Last)^.TimeStamp;
        Dec(Last);
        IsGrouped:=true;
      end
    else
      begin
        Count:=1;
        IsGrouped:=false;
      end;
    for Idx:=Last downto Last-Count+1 do
      with Core^.UndoList^.At(Idx)^ do
        begin
          if not IsGrouped then
            UndoTime:=TimeStamp;
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
                SetMinMax(StartPos.Y);
              end;
            eaDeleteText :
              begin
                { reinsert deleted text }
                SetCurPtr(EndPos.X,EndPos.Y);
                WasInserting:=GetInsertMode;
                SetInsertMode(true);
                if assigned(text) then
                  for Temp := 1 to length(Text^) do
                    AddChar(Text^[Temp]);
                SetInsertMode(WasInserting);
                SetMinMax(EndPos.Y);
                SetCurPtr(StartPos.X,StartPos.Y);
              end;
            eaOverwriteText :
              begin
                SetCurPtr(StartPos.X,StartPos.Y);
                Line:=GetDisplayText(StartPos.Y);
                WasInserting:=GetInsertMode;
                SetInsertMode(false);
                if assigned(text) then
                  for Temp := 1 to length(Text^) do
                    begin
                      AddChar(Text^[Temp]);
                      if StartPos.X+Temp>Length(Line) then
                        Text^[Temp]:=' '
                      else
                        Text^[Temp]:=Line[StartPos.X+Temp];
                    end;
                SetInsertMode(WasInserting);
                SetMinMax(EndPos.Y);
                SetCurPtr(StartPos.X,StartPos.Y);
              end;
            eaInsertLine :
              begin
                SetCurPtr(EndPos.X,EndPos.Y);
                Line:=Copy(GetDisplayText(StartPos.Y),1,StartPos.X);
                If Length(Line)<StartPos.X then
                  Line:=Line+CharStr(' ',StartPos.X-length(Line))+GetStr(Text);
                SetDisplayText(StartPos.Y,Line+Copy(GetDisplayText(EndPos.Y),EndPos.X+1,255));
                SetMinMax(EndPos.Y);
                SetCurPtr(0,EndPos.Y);
                DeleteLine(EndPos.Y);
                SetCurPtr(StartPos.X,StartPos.Y);
                SetMinMax(StartPos.Y);
              end;
            eaDeleteLine :
              begin
                HadefNoIndent:=(GetFlags and efNoIndent)<>0;
                WasInserting:=GetInsertMode;
                SetInsertMode(true);
                SetFlags(GetFlags or efNoIndent);
                InsertLine(StartPos.Y,'');
                SetInsertMode(WasInserting);
                if not HadefNoIndent then
                  SetFlags(GetFlags and not efNoIndent);
                {DelEnd; wrong for eaCut at least }
                SetCurPtr(StartPos.X,StartPos.Y);
                SetLineText(StartPos.Y,Copy(GetDisplayText(StartPos.Y),1,StartPos.X)+GetStr(Text));
                SetMinMax(StartPos.Y);
              end;
            eaSelectionChanged :
              begin
                { move cursor to end of last set selection }
              end;
          else
            { what the 'ell's an undefined action doing round 'ere mate! }
            ;
          end; { once this lot is done paste into redo and modify to suit needs }
          { move item to redo stack }
          Core^.RedoList^.Insert(Core^.UndoList^.At(Idx));
          UpdateUndoRedo(cmRedo,Core^.UndoList^.At(Idx)^.Action);
          Core^.UndoList^.atDelete(Idx);
          If Idx>0 then
            UpdateUndoRedo(cmUndo,Core^.UndoList^.At(Idx-1)^.Action)
          else
            UpdateUndoRedo(cmUndo,0);
        end;{Idx loop for grouped actions }
      if IsGrouped then
        begin
          Idx:=Core^.UndoList^.Count-1;
          Core^.RedoList^.Insert(Core^.UndoList^.At(Idx));
          UpdateUndoRedo(cmRedo,Core^.UndoList^.At(Idx)^.Action);
          Core^.UndoList^.atDelete(Idx);
          If Idx>0 then
            UpdateUndoRedo(cmUndo,Core^.UndoList^.At(Idx-1)^.Action)
          else
            UpdateUndoRedo(cmUndo,0);
        end;
      if Core^.UndoList^.count=0 then
        SetCmdState(UndoCmd,false);
      if (Core^.UndoList^.count=0) or
         ((Core^.UndoList^.count=1) and
          (Core^.UndoList^.At(0)^.Action=eaMoveCursor)) then
        begin
          SetCmdState(UndoCmd,false);
          if (UndoTime>=Core^.SystemLoadTime) or (Core^.SystemLoadTime=0) then
            SetModified(false);
        end;
      SetCmdState(RedoCmd,true);
      Message(Application,evBroadcast,cmCommandSetChanged,nil);
      if MinY<>-1 then
        UpdateAttrsRange(MinY,MaxY,attrAll);
      DrawView;
    end;
  Core^.SetStoreUndo(True);
  Unlock;
end;

procedure TCodeEditor.Redo;
var
  Temp,Idx,i,Last,Count : Longint;
  StoredFlags : longint;
  WasInserting,IsGrouped,ShouldInsertText : boolean;
  Line : String;
  MaxY,MinY : sw_integer;
  procedure SetMinMax(y : sw_integer);
    begin
      if MinY=-1 then
        MinY:=Y;
      if Y<MinY then
        MinY:=Y;
      if MaxY=-1 then
        MaxY:=Y;
      if Y>MaxY then
        MaxY:=Y;
    end;
begin
  Core^.SetStoreUndo(False);
  Lock;
  MinY:=-1;
  MaxY:=-1;
  if Core^.RedoList^.count <> 0 then
   begin
    Last:=Core^.RedoList^.count-1;
    if Core^.RedoList^.At(Last)^.Is_grouped_action then
      begin
        Count:=Core^.RedoList^.At(Last)^.ActionCount;
        Dec(Last);
        IsGrouped:=true;
      end
    else
      begin
        Count:=1;
        IsGrouped:=false;
      end;
    for Idx:=Last downto Last-Count+1 do
    with Core^.RedoList^.At(Idx)^ do
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
            SetMinMax(StartPos.Y);
          end;
        eaDeleteText :
          begin
            SetCurPtr(EndPos.X,EndPos.Y);
            for Temp := 1 to length(GetStr(Text)) do
              DelChar;
            SetMinMax(EndPos.Y);
          end;
        eaOverwriteText :
          begin
            SetCurPtr(StartPos.X,StartPos.Y);
            Line:=GetDisplayText(StartPos.Y);
            WasInserting:=GetInsertMode;
            SetInsertMode(false);
            if assigned(text) then
              for Temp := 1 to length(Text^) do
                begin
                  AddChar(Text^[Temp]);
                  if StartPos.X+Temp>Length(Line) then
                    Text^[Temp]:=' '
                  else
                    Text^[Temp]:=Line[StartPos.X+Temp];
                end;
            SetInsertMode(WasInserting);
            SetCurPtr(EndPos.X,EndPos.Y);
            SetMinMax(StartPos.Y);
          end;
        eaInsertLine :
          begin
            SetCurPtr(StartPos.X,StartPos.Y);
            StoredFlags:=GetFlags;
            SetFlags(Flags);
            InsertNewLine;
            SetCurPtr(0,EndPos.Y);
            Line:=GetStr(Text);
            ShouldInsertText:=false;
            for I:=1 to Length(Line) do
              if Line[I]<>' ' then
                ShouldInsertText:=true;
            If ShouldInsertText then
              InsertText(Line);
            SetFlags(StoredFlags);
            SetCurPtr(EndPos.X,EndPos.Y);
            SetMinMax(StartPos.Y);
          end;
        eaDeleteLine :
          begin
            SetCurPtr(StartPos.X,StartPos.Y);
            DeleteLine(StartPos.Y);
            SetCurPtr(EndPos.X,EndPos.Y);
            if EndPos.Y=StartPos.Y-1 then
            SetDisplayText(EndPos.Y,RExpand(
              copy(GetDisplayText(EndPos.Y),1,EndPos.X),EndPos.X)
              +GetStr(Text));
            SetCurPtr(EndPos.X,EndPos.Y);
            SetMinMax(StartPos.Y);
            SetMinMax(EndPos.Y);
          end;
        eaSelectionChanged :
          begin
            { move cursor to end of last set test selection }
          end;
      else
        { what the 'ell's an undefined action doing round 'ere mate! }
        ;
      end; { once this lot is done paste back into undo and modify to suit needs }
    { move item to undo stack }
      Core^.UndoList^.Insert(Core^.RedoList^.At(Idx));
      UpdateUndoRedo(cmUndo,Core^.RedoList^.At(Idx)^.Action);
      If Idx>0 then
        UpdateUndoRedo(cmRedo,Core^.RedoList^.At(Idx-1)^.Action)
      else
        UpdateUndoRedo(cmRedo,0);
      Core^.RedoList^.atDelete(Idx);
      end;{ Idx loop for grouped action }
      If IsGrouped then
        begin
          Idx:=Core^.RedoList^.count-1;
          Core^.UndoList^.Insert(Core^.RedoList^.At(Idx));
          UpdateUndoRedo(cmUndo,Core^.RedoList^.At(Idx)^.Action);
          If Idx>0 then
            UpdateUndoRedo(cmRedo,Core^.RedoList^.At(Idx-1)^.Action)
          else
            UpdateUndoRedo(cmRedo,0);
          Core^.RedoList^.atDelete(Idx);
        end;
      if Core^.RedoList^.count=0 then
        SetCmdState(RedoCmd,false);
      SetCmdState(UndoCmd,true);
      Message(Application,evBroadcast,cmCommandSetChanged,nil);
      if MinY<>-1 then
        UpdateAttrsRange(MinY,MaxY,attrAll);
      DrawView;
    end;
  Core^.SetStoreUndo(True);
  Unlock;
end;

(*constructor TCodeEditor.Load(var S: TStream);
var TS: PSubStream;
    TSize: longint;
begin
  inherited Load(S);

  New(UndoList,init(500,1000));
  New(RedoList,init(500,1000));

  New(Lines, Init(500,1000));
  { we have always need at least 1 line }
  LinesInsert(New(PLine, Init('',0)));

  GetPeerViewPtr(S,Indicator);
  S.Read(Flags,SizeOf(Flags));
  S.Read(TabSize,SizeOf(TabSize));

  if IsFlagSet(efStoreContent) then
    begin
      S.Read(TSize,SizeOf(TSize));
      New(TS, Init(@S,S.GetPos,TSize));
{$ifdef TEST_PARTIAL_SYNTAX}
      Core^.SearchBinding(Editor)^.SyntaxComplete:=false;
      { Idle necessary }
      EventMask:=EventMask or evIdle;
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
var {NS: TNulStream;}
    TSizePos,TSize,EndPos: longint;
begin
  inherited Store(S);

  PutPeerViewPtr(S,Indicator);
  S.Write(Flags,SizeOf(Flags));
  S.Write(TabSize,SizeOf(TabSize));

  if IsFlagSet(efStoreContent) then
    begin
      { NS.Init;
      SaveToStream(@NS);
      TSize:=NS.GetSize;
      NS.Done;
        This is waste of time PM
        use Seek instead !! }
      { yep. and this won't work for serial streams. - Gabor }
      TSize:=0;
      TSizePos:=S.GetPos;
      S.Write(TSize,SizeOf(TSize));
      SaveToStream(@S);
      EndPos:=S.GetPos;
      TSize:=EndPos-TSizePos-SizeOf(TSize);
      S.Seek(TSizePos);
      S.Write(TSize,SizeOf(TSize));
      S.Seek(EndPos);
    end;

  S.Write(SelStart,SizeOf(SelStart));
  S.Write(SelEnd,SizeOf(SelEnd));
  S.Write(Highlight,SizeOf(Highlight));
  S.Write(CurPos,SizeOf(CurPos));
  S.Write(StoreUndo,SizeOf(StoreUndo));
  S.Write(IsReadOnly,SizeOf(IsReadOnly));
  S.Write(NoSelect,SizeOf(NoSelect));
  S.Write(HighlightRow,SizeOf(HighlightRow));
end;*)

function TCodeEditor.LoadFromStream(Stream: PFastBufStream): boolean;
var OK: boolean;
begin
  OK:=Core^.LoadFromStream(@Self,Stream);
  if IsFlagSet(efSyntaxHighlight) then
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
begin
  SaveToStream:=Core^.SaveToStream(@Self,Stream);
end;

function TCodeEditor.SaveAreaToStream(Stream: PStream; StartP,EndP: TPoint): boolean;
begin
  SaveAreaToStream:=Core^.SaveAreaToStream(@Self,Stream,StartP,EndP);
end;

function TCodeEditor.UpdateAttrs(FromLine: sw_integer; Attrs: byte): sw_integer;
begin
  UpdateAttrs:=Core^.UpdateAttrs(FromLine,Attrs);
end;

function TCodeEditor.UpdateAttrsRange(FromLine, ToLine: sw_integer; Attrs: byte): sw_integer;
begin
  UpdateAttrsRange:=Core^.UpdateAttrsRange(FromLine,ToLine,Attrs);
end;

procedure TCodeEditor.AddAction(AAction: byte; AStartPos, AEndPos: TPoint; AText: string;AFlags : longint);
begin
  Core^.AddAction(AAction,AStartPos,AEndPos,AText,AFlags);
end;

procedure TCodeEditor.AddGroupedAction(AAction : byte);
begin
  Core^.AddGroupedAction(AAction);
end;

procedure TCodeEditor.CloseGroupedAction(AAction : byte);
begin
  Core^.CloseGroupedAction(AAction);
end;

function TCodeEditor.GetUndoActionCount: sw_integer;
begin
  GetUndoActionCount:=Core^.GetUndoActionCount;
end;

function TCodeEditor.GetRedoActionCount: sw_integer;
begin
  GetRedoActionCount:=Core^.GetRedoActionCount;
end;

destructor TCodeEditor.Done;
begin
  inherited Done;
  if Assigned(Core) then
  begin
    Core^.UnBindEditor(@Self);
    if Core^.CanDispose then
      Dispose(Core, Done);
  end;
  Core:=nil;
  if Assigned(CodeCompleteFrag) then
    DisposeStr(CodeCompleteFrag);
  if Assigned(CodeCompleteWord) then
    DisposeStr(CodeCompleteWord);
  if Assigned(ErrorMessage) then
    DisposeStr(ErrorMessage);
  if Assigned(Folds) then
    Dispose(Folds, Done);
  Folds:=nil;
end;

constructor TFileEditor.Init(var Bounds: TRect; AHScrollBar, AVScrollBar:
       PScrollBar; AIndicator: PIndicator;ACore: PCodeEditorCore; const AFileName: string);
begin
  inherited Init(Bounds,AHScrollBAr,AVScrollBAr,AIndicator,ACore);
  FileName:=AFileName;
  UpdateIndicator;
  Message(@Self,evBroadcast,cmFileNameChanged,@Self);
end;

function TFileEditor.LoadFile: boolean;
var OK: boolean;
    PA : Array[1..2] of pointer;
begin
  OK:=LoadFromFile(FileName);
  if GetModified and (Core^.GetBindingCount=1) then
    begin
      PA[1]:=@FileName;
      Ptrint(PA[2]):=Core^.GetChangedLine;
      EditorDialog(edChangedOnloading,@PA);
    end;
  Core^.OnDiskLoadTime:=Cardinal(GetFileTime(FileName));
  Core^.SystemLoadTime:=Core^.OnDiskLoadTime;
  LoadFile:=OK;
end;

function TFileEditor.IsChangedOnDisk : boolean;
begin
  IsChangedOnDisk:=(Core^.OnDiskLoadTime<>Cardinal(GetFileTime(FileName))) and
    (Core^.OnDiskLoadTime<>0);
end;

function TFileEditor.SaveFile: boolean;
var OK: boolean;
    BAKName: string;
    f: text;
    SaveTime : cardinal;
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
  if IsFlagSet(efBackupFiles) and ExistsFile(FileName) then
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
  SaveTime:=cardinal(now);
  OK:=SaveToFile(FileName);
  if OK then
    SetModified(false)
  { Restore the original }
  else if IsFlagSet(efBackupFiles) and ExistsFile(BakName) then
    begin
{$I-}
     Assign(f,BakName);
     Rename(F,FileName);
     EatIO;
{$I+}
    end;
  { don't forget to update the OnDiskLoadTime value }
  if OK then
    begin
      Core^.OnDiskLoadTime:=Cardinal(GetFileTime(FileName));
      Core^.SystemLoadTime:=SaveTime;
    end;
  if not OK then
    EditorDialog(edSaveError,@FileName);
  SaveFile:=OK;
end;

function TFileEditor.ReloadFile: boolean;
var OK,WasModified: boolean;
    BAKName: string;
    f: text;
begin
  If not IsChangedOnDisk then
    begin
      ReloadFile:=false;
      exit;
    end;
  WasModified:=GetModified;
  if not WasModified then
    OK:=EditorDialog(edreloaddiskmodifiedfile, @FileName)=cmYes
  else
    OK:=EditorDialog(edreloaddiskandidemodifiedfile, @FileName)=cmYes;
  if not OK then
    begin
      ReloadFile:=false;
      exit;
    end;
  { avoid wrong message }
  if WasModified then
    SetModified(false);
  OK:=LoadFile;
  if OK then
    begin
      SetModified(false);
      ClearUndoList;
      { don't forget to update the OnDiskLoadTime value }
      Core^.OnDiskLoadTime:=Cardinal(GetFileTime(FileName));
      Core^.SystemLoadTime:=Core^.OnDiskLoadTime;
      DrawView;
    end
  else
    begin
      if WasModified then
        SetModified(true);
      EditorDialog(edReadError,@FileName);
    end;
  ReloadFile:=OK;
end;

function TFileEditor.ShouldSave: boolean;
begin
  ShouldSave:=GetModified{ or (FileName='')};
end;

function TFileEditor.Save: Boolean;
begin
  if ShouldSave=false then begin Save:=true; Exit; end;
  if FileName = '' then Save := SaveAs else Save := SaveFile;
end;

function TFileEditor.SaveAs: Boolean;
var
  SavedName : String;
  SavedDiskLoadTime : cardinal;
begin
  SaveAs := False;
  SavedName:=FileName;
  SavedDiskLoadTime:=Core^.OnDiskLoadTime;
  if EditorDialog(edSaveAs, @FileName) <> cmCancel then
  begin
    FileName:=FExpand(FileName);
    Message(Owner, evBroadcast, cmUpdateTitle, @Self);
    { if we rename the file the OnDiskLoadTime is wrong so we reset it }
    Core^.OnDiskLoadTime:=0;
    if SaveFile then
      begin
        SaveAs := true;
      end
    else
      begin
        FileName:=SavedName;
        Core^.OnDiskLoadTime:=SavedDiskLoadTime;
        Message(Owner, evBroadcast, cmUpdateTitle, @Self);
      end;
    if IsClipboard then FileName := '';
    Message(Application,evBroadcast,cmFileNameChanged,@Self);
  end;
end;

function TFileEditor.SaveAsk(Force: boolean): boolean;
var OK: boolean;
    D: Sw_integer;
begin
  if Force then
   begin
     if GetModified then
      OK:=Save
     else
      OK:=true;
   end
  else
   begin
     OK:=(GetModified=false);
     if (OK=false) and (Core^.GetBindingCount>1) then
      OK:=true;
     if OK=false then
      begin
        if FileName = '' then D := edSaveUntitled else D := edSaveModify;
        case EditorDialog(D, @FileName) of
          cmYes    : OK := Save;
          cmNo     : begin
                     { the file should be still marked as modified! (FK) }
                     {   SetModified(False);                               }
                     OK:=true;
                    end;
         cmCancel : begin
                      OK := False;
                      Message(Application,evBroadcast,cmSaveCancelled,@Self);
                    end;
        end;
      end;
   end;
  SaveAsk:=OK;
end;

procedure TFileEditor.BindingsChanged;
begin
  Message(Application,evBroadcast,cmUpdateTitle,@Self);
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
       B:=IsFlagSet(efSyntaxHighlight);
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
  if OK and (Command=cmClose) then
    if IsClipboard=false then
      OK:=SaveAsk(false);
  Valid:=OK;
end;

(* constructor TFileEditor.Load(var S: TStream);
var P: PString;
    SSP,SEP,CP,DP: TPoint;
    HR: TRect;
    PA : Array[1..2] of pointer;
    HoldUndo : boolean;
begin
  inherited Load(S);
  HoldUndo:=GetStoreUndo;
  SetStoreUndo(False);
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

{  if GetModified then
    begin
      PA[1]:=@FileName;
      longint(PA[2]):=ChangedLine;
      EditorDialog(edChangedOnloading,@PA);
    end;}

  SetHighlight(HR.A,HR.B);
  SetSelection(SSP,SEP);
  SetCurPtr(CP.X,CP.Y);
  ScrollTo(DP.X,DP.Y);
  SetModified(false);

  LimitsChanged;
  SetStoreUndo(HoldUndo);
end;

procedure TFileEditor.Store(var S: TStream);
begin
  inherited Store(S);
  S.WriteStr(@FileName);
end;
*)

function DefUseSyntaxHighlight(Editor: PFileEditor): boolean;
begin
  DefUseSyntaxHighlight:=Editor^.IsFlagSet(efSyntaxHighlight);
end;

function DefUseTabsPattern(Editor: PFileEditor): boolean;
begin
  DefUseTabsPattern:=Editor^.IsFlagSet(efUseTabCharacters);
end;

procedure RegisterWCEdit;
begin
{$ifndef NOOBJREG}
  RegisterType(RIndicator);
  RegisterType(RCodeEditor);
  RegisterType(RFileEditor);
{$endif}
end;

end.
