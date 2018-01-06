{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Symbol browse support routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
unit FPSymbol;

interface

uses Objects,Drivers,Views,Menus,Dialogs,
{$ifdef HASOUTLINE}
     Outline,
{$endif HASOUTLINE}
     BrowCol,
     WViews,
     FPViews;

const
      { Browser tab constants }
      btScope       = 0;
      btReferences  = 1;
      btInheritance = 2;
      btMemInfo     = 3;
      btUnitInfo    = 4;
      btBreakWatch  = 7;

type
    PBrowserWindow = ^TBrowserWindow;

    PGDBValueCollection = ^TGDBValueCollection;

    PGDBValue = ^TGDBValue;
    TGDBValue = Object(TObject)
      constructor Init(Const AExpr : String;ASym : PSymbol);
      procedure GetValue;
      function  GetText : String;
      destructor Done;virtual;
    private
      expr : Pstring;
      St   : Pstring;
      S    : PSymbol;
      GDBI : longint;
      end;

    TGDBValueCollection = Object(TCollection)
      function  At(Index: sw_Integer): PGDBValue;
      end;


    PSymbolView = ^TSymbolView;
    TSymbolView = object(TLocalMenuListBox)
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      destructor   Done;virtual;
      procedure    HandleEvent(var Event: TEvent); virtual;
      procedure    SetState(AState: Word; Enable: Boolean); virtual;
      function     GotoItem(Item: sw_integer): boolean; virtual;
      function     TrackItem(Item: sw_integer; AutoTrack: boolean): boolean; virtual;
      function     GetPalette: PPalette; virtual;
      function     GetLocalMenu: PMenu; virtual;
      procedure    ClearHighlights;
      procedure    AutoTrackSource; virtual;
      procedure    Browse; virtual;
      procedure    GotoSource; virtual;
      procedure    TrackSource; virtual;
      procedure    OptionsDlg; virtual;
    private
      MyBW         : PBrowserWindow;
      function     TrackReference(R: PReference; AutoTrack: boolean): boolean; virtual;
      function     GotoReference(R: PReference): boolean; virtual;
    end;

    PSymbolScopeView = ^TSymbolScopeView;
    TSymbolScopeView = object(TSymbolView)
      constructor Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
      destructor  Done; virtual;
      procedure   SetGDBCol;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Draw; virtual;
      procedure   LookUp(S: string); virtual;
      function    GotoItem(Item: sw_integer): boolean; virtual;
      function    TrackItem(Item: sw_integer; AutoTrack: boolean): boolean; virtual;
    private
      Symbols: PSymbolCollection;
      SymbolsValue : PGDBValueCollection;
      LookupStr: string;
    end;

    PSymbolReferenceView = ^TSymbolReferenceView;
    TSymbolReferenceView = object(TSymbolView)
      constructor Init(var Bounds: TRect; AReferences: PReferenceCollection; AHScrollBar, AVScrollBar: PScrollBar);
      destructor  Done; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   SelectItem(Item: Sw_Integer); virtual;
      function    GotoItem(Item: sw_integer): boolean; virtual;
      function    TrackItem(Item: sw_integer; AutoTrack: boolean): boolean; virtual;
      procedure   Browse; virtual;
    private
      References: PReferenceCollection;
    end;

    PSymbolMemInfoView = ^TSymbolMemInfoView;
    TSymbolMemInfoView = object(TStaticText)
      constructor  Init(var Bounds: TRect; AMemInfo: PSymbolMemInfo);
      destructor  Done; virtual;
      procedure    GetText(var S: String); virtual;
      function     GetPalette: PPalette; virtual;
    private
      MemInfo: PSymbolMemInfo;
      MyBW   : PBrowserWindow;
    end;

    PSymbolMemoView = ^TSymbolMemoView;
    TSymbolMemoView = object(TFPMemo)
      function    GetPalette: PPalette; virtual;
    end;

    PSymbolInheritanceView = ^TSymbolInheritanceView;
{$ifdef HASOUTLINE}
    TSymbolInheritanceView = object(TOutlineViewer)
{$else notHASOUTLINE}
    TSymbolInheritanceView = object(TLocalMenuListBox)
{$endif HASOUTLINE}
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar; ARoot: PObjectSymbol);
      destructor   Done; virtual;
      function     GetRoot: Pointer; virtual;
      function     HasChildren(Node: Pointer): Boolean; virtual;
      function     GetChild(Node: Pointer; I: sw_Integer): Pointer; virtual;
      function     GetNumChildren(Node: Pointer): sw_Integer; virtual;
      function     GetNumChildrenExposed(Node: Pointer) : sw_Integer; virtual;
      procedure    Adjust(Node: Pointer; Expand: Boolean); virtual;
      function     IsExpanded(Node: Pointer): Boolean; virtual;
{$ifdef HASOUTLINE}
      function     GetText(Node: Pointer): String; virtual;
{$else not HASOUTLINE}
      procedure    ExpandAll(Node: Pointer);
      function     GetNode(I : sw_Integer) : Pointer; virtual;
      function     GetLineNode(Item : sw_Integer) : Pointer; virtual;
      function     GetText(Item,MaxLen: Sw_Integer): String; virtual;
{$endif HASOUTLINE}
      procedure    NodeSelected(P: pointer); virtual;
      procedure    Selected(I: sw_Integer); virtual;
      procedure    HandleEvent(var Event: TEvent); virtual;
      function     GetPalette: PPalette; virtual;
    private
      Root         : PObjectSymbol;
      MyBW         : PBrowserWindow;
    end;

    PBrowserTabItem = ^TBrowserTabItem;
    TBrowserTabItem = record
      Sign  : char;
      Link  : PView;
      Next  : PBrowserTabItem;
    end;

    PBrowserTab = ^TBrowserTab;
    TBrowserTab = object(TView)
      Items: PBrowserTabItem;
      constructor Init(var Bounds: TRect; AItems: PBrowserTabItem);
      function    GetItemCount: sw_integer; virtual;
      function    GetItem(Index: sw_integer): PBrowserTabItem; virtual;
      procedure   SetParams(AFlags: word; ACurrent: Sw_integer); virtual;
      procedure   SelectItem(Index: Sw_integer); virtual;
      procedure   Draw; virtual;
      function    GetPalette: PPalette; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      destructor  Done; virtual;
    private
      Flags   : word;
      Current : Sw_integer;
    end;

    PUnitInfoPanel = ^TUnitInfoPanel;
    TUnitInfoPanel = object(TPanel)
      InOwnerCall: boolean;
      procedure HandleEvent(var Event: TEvent); virtual;
    end;

    TBrowserWindow = object(TFPWindow)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
                    const AName,APrefix: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
                    AInheritance: PObjectSymbol; AMemInfo: PSymbolMemInfo);
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   Close; virtual;
      procedure   SelectTab(BrowserTab: Sw_integer); virtual;
      function    GetPalette: PPalette; virtual;
      function    Disassemble : boolean;
      destructor  Done;virtual;
    private
      PageTab       : PBrowserTab;
      ST            : PStaticText;
      Sym           : PSymbol;
      ScopeView     : PSymbolScopeView;
      ReferenceView : PSymbolReferenceView;
      InheritanceView: PSymbolInheritanceView;
      MemInfoView   : PSymbolMemInfoView;
      UnitInfoText  : PSymbolMemoView;
      UnitInfoUsed  : PSymbolScopeView;
      UnitInfoDependent : PSymbolScopeView;
      UnitInfo      : PUnitInfoPanel;
      Prefix        : PString;
      IsValid       : boolean;
      DebuggerValue : PGDBValue;
    end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;S : PSymbol;
            ParentBrowser : PBrowserWindow;
            Symbols: PSymbolCollection; References: PReferenceCollection;
            Inheritance: PObjectSymbol; MemInfo: PSymbolMemInfo);

function IsSymbolInfoAvailable: boolean;

procedure OpenOneSymbolBrowser(Name : String);

procedure CloseAllBrowsers;

procedure RemoveBrowsersCollection;

const
   GlobalsCollection : PSortedCollection = nil;
   ProcedureCollection : PSortedCollection = nil;
   ModulesCollection : PSortedCollection = nil;

implementation

uses App,Strings,
     FVConsts,
{$ifdef BROWSERCOL}
     symconst,
{$endif BROWSERCOL}
     WUtils,WEditor,
     FPConst,FPUtils,FPVars,{$ifndef FPDEBUG}FPDebug{$endif},FPIDE;

{$ifdef USERESSTRINGS}
resourcestring
{$else}
const
{$endif}
                msg_symbolnotfound = #3'Symbol %s not found';
                msg_nobrowserinfoavailable = 'No Browser info available';
                msg_cantfindfile = 'Can''t find %s';

                menu_local_gotosource = '~G~oto source';
                menu_local_tracksource = '~T~rack source';
                menu_local_options = '~O~ptions...';
                menu_local_clear = '~C~lear';
                menu_local_saveas = 'Save ~a~s';

                { Symbol view local menu items }
                menu_symlocal_browse = '~B~rowse';
                menu_symlocal_gotosource = '~G~oto source';
                menu_symlocal_tracksource = '~T~rack source';
                menu_symlocal_options = '~O~ptions...';

                { Symbol browser meminfo page }
                msg_sizeinmemory = 'Size in memory';
                msg_sizeonstack = 'Size on stack';

                msg_usedfirstin = 'Used first in';
                msg_mainsource = 'Main source';
                msg_sourcefiles = 'Source files';

                dialog_browse = 'Browse: %s';

const           { Symbol browser tabs }
                { must be char constants (so cannot be resourcestring)}
                label_browsertab_scope = 'S';
                label_browsertab_reference = 'R';
                label_browsertab_inheritance = 'I';
                label_browsertab_memory = 'M';
                label_browsertab_unit = 'U';

procedure CloseAllBrowsers;
  procedure SendCloseIfBrowser(P: PView);
  begin
    if assigned(P) and
       ((TypeOf(P^)=TypeOf(TBrowserWindow)) or
       (TypeOf(P^)=TypeOf(TSymbolView)) or
       (TypeOf(P^)=TypeOf(TSymbolScopeView)) or
       (TypeOf(P^)=TypeOf(TSymbolReferenceView)) or
       (TypeOf(P^)=TypeOf(TSymbolMemInfoView)) or
       (TypeOf(P^)=TypeOf(TSymbolInheritanceView)) or
       (TypeOf(P^)=TypeOf(TSymbolMemoView))) then
      Message(P,evCommand,cmClose,nil);
  end;

begin
  Desktop^.ForEach(@SendCloseIfBrowser);
end;

procedure RemoveBrowsersCollection;
begin
  if assigned(GlobalsCollection) then
    begin
      GlobalsCollection^.deleteAll;
      Dispose(GlobalsCollection,done);
      GlobalsCollection:=nil;
    end;
  if assigned(ProcedureCollection) then
    begin
      ProcedureCollection^.deleteAll;
      Dispose(ProcedureCollection,done);
      ProcedureCollection:=nil;
    end;
  if assigned(ModulesCollection) then
    begin
      ModulesCollection^.deleteAll;
      Dispose(ModulesCollection,done);
      ModulesCollection:=nil;
    end;
end;

function NewBrowserTabItem(ASign: char; ALink: PView; ANext: PBrowserTabItem): PBrowserTabItem;
var P: PBrowserTabItem;
begin
  New(P); FillChar(P^,SizeOf(P^),0);
  with P^ do begin Sign:=ASign; Link:=ALink; Next:=ANext; end;
  NewBrowserTabItem:=P;
end;

procedure DisposeBrowserTabItem(P: PBrowserTabItem);
begin
  if P<>nil then Dispose(P);
end;

procedure DisposeBrowserTabList(P: PBrowserTabItem);
begin
  if P<>nil then
  begin
    if P^.Next<>nil then DisposeBrowserTabList(P^.Next);
    DisposeBrowserTabItem(P);
  end;
end;

function IsSymbolInfoAvailable: boolean;
begin
  IsSymbolInfoAvailable:=BrowCol.Modules<>nil;
end;

procedure OpenOneSymbolBrowser(Name : String);

var Index : sw_integer;
    PS,S : PSymbol;
    Anc : PObjectSymbol;
    P : Pstring;
    Symbols: PSymbolCollection;

  function Search(P : PSymbol) : boolean;
  begin
    Search:=UpcaseStr(P^.Items^.LookUp(Name,Index))=Name;
  end;

begin
   Name:=UpcaseStr(Name);
   If BrowCol.Modules<>nil then
     begin
       PS:=BrowCol.Modules^.FirstThat(@Search);
       If assigned(PS) then
         begin
           S:=PS^.Items^.At(Index);
           Symbols:=S^.Items;
           if (not assigned(symbols) or (symbols^.count=0)) and
              assigned(S^.Ancestor) then
             Symbols:=S^.Ancestor^.Items;
           if (S^.Flags and (sfObject or sfClass))=0 then
             Anc:=nil
           else if S^.Ancestor=nil then
             Anc:=ObjectTree
           else
             Anc:=SearchObjectForSymbol(S^.Ancestor);
           OpenSymbolBrowser(0,20,
                PS^.Items^.At(Index)^.GetName,
                PS^.Items^.At(Index)^.GetText,
                PS^.Items^.At(Index),nil,
                Symbols,PS^.Items^.At(Index)^.References,Anc,PS^.MemInfo);
         end
       else
         begin
           P:=@Name;
           ErrorBox(msg_symbolnotfound,@P);
         end;
     end
   else
     ErrorBox(msg_nobrowserinfoavailable,nil);
end;

(*procedure ReadBrowseLog(FileName: string);
var f: text;
    IOOK,EndOfFile: boolean;
    Line: string;
procedure NextLine;
begin
  readln(f,Line);
  EndOfFile:=Eof(f);
end;
var Level: integer;
procedure ProcessSymTable(Indent: integer; Owner: PSymbolCollection);
var IndentS,S,Source: string;
    Sym: PSymbol;
    Ref: PSymbolReference;
    P: byte;
    PX: TPoint;
    PS: PString;
    PCount: integer;
    Params: array[0..30] of PString;
    Typ: tsymtyp;
    ExitBack: boolean;
begin
  Inc(Level);
  IndentS:=CharStr(' ',Indent); ExitBack:=false;
  Sym:=nil;
  repeat
    if copy(Line,1,length(IndentS))<>IndentS then ExitBack:=true else
    if copy(Line,Indent+1,3)='***' then
      { new symbol }
      begin
        S:=copy(Line,Indent+1+3,255);
        P:=Pos('***',S); if P=0 then P:=length(S)+1;
        S:=Trim(copy(S,1,P-1));
        if (copy(S,1,1)='_') and (Pos('$$',S)>0) then
          begin
            repeat
              P:=Pos('$$',S);
              if P>0 then Delete(S,1,P+1);
            until P=0;
            P:=Pos('$',S);
            Delete(S,1,P);
            PCount:=0;
            repeat
              P:=Pos('$',S); if P=0 then P:=length(S)+1;
              Params[PCount]:=TypeNames^.Add(copy(S,1,P-1));
              Inc(PCount);
              Delete(S,1,P);
            until S='';
            Sym^.Typ:=procsym;
            Sym^.SetParams(PCount,@Params);
          end
        else
          New(Sym, Init(S, varsym, 0, nil));
        Owner^.Insert(Sym);
        NextLine;
      end else
    if copy(Line,Indent+1,3)='---' then
      { child symtable }
      begin
        S:=Trim(copy(Line,Indent+1+12,255));
        if Level=1 then Typ:=unitsym else
          Typ:=typesym;
        if (Sym<>nil) and (Sym^.GetName=S) then
        else
          begin
            New(Sym, Init(S, Typ, 0, nil));
            Owner^.Insert(Sym);
          end;
        Sym^.Typ:=Typ;
        NextLine;
        New(Sym^.Items, Init(0,50));
        ProcessSymTable(Indent+2,Sym^.Items);
      end else
{    if Sym<>nil then}
    if copy(Line,Indent+1,1)=' ' then
      { reference }
      begin
        S:=copy(Line,Indent+1+2,255);
        P:=Pos('(',S); if P=0 then P:=length(S)+1;
        Source:=Trim(copy(S,1,P-1)); Delete(S,1,P);
        P:=Pos(',',S); if P=0 then P:=length(S)+1;
        PX.Y:=StrToInt(copy(S,1,P-1)); Delete(S,1,P);
        P:=Pos(')',S); if P=0 then P:=length(S)+1;
        PX.X:=StrToInt(copy(S,1,P-1)); Delete(S,1,P);
        PS:=ModuleNames^.Add(Source);
        New(Ref, Init(PS, PX));
        if Sym^.References=nil then
          New(Sym^.References, Init(10,50));
        Sym^.References^.Insert(Ref);
      end;
    if ExitBack=false then
      NextLine;
  until EndOfFile or ExitBack;
  Dec(Level);
end;
begin
  DoneSymbolBrowser;
  InitSymbolBrowser;

{$I-}
  Assign(f,FileName);
  Reset(f);
  Level:=0;
  NextLine;
  while (IOResult=0) and (EndOfFile=false) do
    ProcessSymTable(0,Modules);
  Close(f);
  EatIO;
{$I+}
end;*)


{****************************************************************************
                               TGDBValue
****************************************************************************}

constructor TGDBValue.Init(Const AExpr : String;ASym : PSymbol);
begin
  St := nil;
  S := ASym;
  Expr:=NewStr(AExpr);
  GDBI:=-1;
end;

destructor TGDBValue.Done;
begin
  If Assigned(St) then
    begin
      DisposeStr(St);
      st:=nil;
    end;
  If Assigned(Expr) then
    begin
      DisposeStr(Expr);
      Expr:=nil;
    end;
end;

procedure TGDBValue.GetValue;
var
  p : pchar;
begin
{$ifdef BROWSERCOL}
{$ifndef NODEBUG}
  if not assigned(Debugger) then
    exit;
  if not Debugger^.IsRunning then
    exit;
  if (S^.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym]) or (GDBI=Debugger^.RunCount) then
    exit;
  If Assigned(St) then
    DisposeStr(St);
  if assigned(Expr) then
    begin
      { avoid infinite recursion here }
      GDBI:=Debugger^.RunCount;
      p:=Debugger^.GetValue(Expr^);
      St:=NewStr(GetPChar(p));
      if assigned(p) then
        StrDispose(p);
    end;
{$endif ndef NODEBUG}
{$endif BROWSERCOL}
end;

function TGDBValue.GetText : String;
begin
  GetValue;
  if assigned(St) then
    GetText:=S^.GetText+' = '+GetStr(St)
  else
    GetText:=S^.GetText;
end;

{****************************************************************************
                               TGDBValueCollection
****************************************************************************}
function  TGDBValueCollection.At(Index: sw_Integer): PGDBValue;
begin
  At:= Inherited At(Index);
end;
{****************************************************************************
                               TSymbolView
****************************************************************************}

constructor TSymbolView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AVScrollBar);
  HScrollBar:=AHScrollBar;
  MyBW:=nil;
  if assigned(HScrollBar) then
    begin
      HScrollBar^.SetRange(1,80);
    end;
  Options:=Options or (ofSelectable+ofTopSelect);
  EventMask:=EventMask or evBroadcast;
end;

procedure TSymbolView.ClearHighlights;
begin
  Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

procedure TSymbolView.AutoTrackSource;
begin
  if Range>0 then
    TrackSource;
end;

procedure TSymbolView.OptionsDlg;
begin
  { Abstract }
end;

destructor TSymbolView.Done;
begin
  EventMask:=EventMask and not evBroadcast;
  Inherited Done;
end;

procedure TSymbolView.SetState(AState: Word; Enable: Boolean);
var OState: longint;
begin
  OState:=State;
  inherited SetState(AState,Enable);
  if ((OState xor State) and sfFocused)<>0 then
    if GetState(sfFocused) then
      begin
        if (MiscOptions and moAutoTrackSource)<>0 then
          AutoTrackSource;
      end
    else
      Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

procedure TSymbolView.Browse;
begin
  SelectItem(Focused);
end;

procedure TSymbolView.GotoSource;
begin
  if GotoItem(Focused) then
    PutCommand(Owner,evCommand,cmClose,nil);
end;

procedure TSymbolView.TrackSource;
begin
  TrackItem(Focused,false);
end;

procedure TSymbolView.HandleEvent(var Event: TEvent);
var DontClear: boolean;
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            Browse;
          kbCtrlEnter :
            GotoSource;
          kbSpaceBar :
            TrackSource;
          kbRight,kbLeft :
            if HScrollBar<>nil then
              HScrollBar^.HandleEvent(Event);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evMouseDown :
      begin
        if Event.double then
          begin
            Browse;
            ClearEvent(Event);
          end;
      end;
    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
          cmSymBrowse :
            Browse;
          cmSymGotoSource :
            GotoSource;
          cmSymTrackSource :
            TrackSource;
          cmSymOptions :
            OptionsDlg;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evBroadcast :
      case Event.Command of
        cmListFocusChanged :
         if Event.InfoPtr=@Self then
          if (MiscOptions and moAutoTrackSource)<>0 then
            if GetState(sfFocused) then
              AutoTrackSource;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TSymbolView.GetPalette: PPalette;
const
  P: string[length(CBrowserListBox)] = CBrowserListBox;
begin
  GetPalette:=@P;
end;

function TSymbolView.GetLocalMenu: PMenu;
begin
  GetLocalMenu:=NewMenu(
    NewItem(menu_symlocal_browse,'',kbNoKey,cmSymBrowse,hcSymBrowse,
    NewItem(menu_symlocal_gotosource,'',kbNoKey,cmSymGotoSource,hcSymGotoSource,
    NewItem(menu_symlocal_tracksource,'',kbNoKey,cmSymTrackSource,hcSymTrackSource,
    NewLine(
    NewItem(menu_symlocal_options,'',kbNoKey,cmSymOptions,hcSymOptions,
    nil))))));
end;

function TSymbolView.GotoItem(Item: sw_integer): boolean;
begin
  SelectItem(Item);
  GotoItem:=true;
end;

function TSymbolView.TrackItem(Item: sw_integer; AutoTrack: boolean): boolean;
begin
  SelectItem(Item);
  TrackItem:=true;
end;

function LastBrowserWindow: PBrowserWindow;
var BW: PBrowserWindow;
procedure IsBW(P: PView);
begin
  if (P^.HelpCtx=hcBrowserWindow) then
    BW:=pointer(P);
end;
begin
  BW:=nil;
  Desktop^.ForEach(@IsBW);
  LastBrowserWindow:=BW;
end;

function TSymbolView.TrackReference(R: PReference; AutoTrack: boolean): boolean;
var W: PSourceWindow;
    BW: PBrowserWindow;
    P: TPoint;
begin
  ClearHighlights;
  Desktop^.Lock;
  P.X:=R^.Position.X-1; P.Y:=R^.Position.Y-1;
  if AutoTrack then
    W:=SearchOnDesktop(R^.GetFileName,false)
  else
    W:=TryToOpenFile(nil,R^.GetFileName,P.X,P.Y,true);
  if not assigned(W) then
    begin
      Desktop^.Unlock;
      if IDEApp.OpenSearch(R^.GetFileName+'*') then
        begin
          W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
          if Assigned(W) then
            W^.Select;
        end;
      Desktop^.Lock;
    end;
  if W<>nil then
  begin
    BW:=LastBrowserWindow;
    if BW=nil then
      W^.Select
    else
      begin
        Desktop^.Delete(W);
        Desktop^.InsertBefore(W,BW^.NextView);
      end;
    W^.Editor^.SetLineFlagExclusive(lfHighlightRow,P.Y);
  end;
  Desktop^.UnLock;
  if Assigned(W)=false then
    ErrorBox(FormatStrStr(msg_cantfindfile,R^.GetFileName),nil);

  TrackReference:=W<>nil;
end;

function TSymbolView.GotoReference(R: PReference): boolean;
var W: PSourceWindow;
begin
  Desktop^.Lock;
  W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
  if Assigned(W) then
    W^.Select
  else
    begin
      Desktop^.Unlock;
      if IDEApp.OpenSearch(R^.GetFileName+'*') then
        begin
          W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1,true);
          if Assigned(W) then
            W^.Select;
        end;
      Desktop^.Lock;
    end;
  Desktop^.UnLock;
  if Assigned(W)=false then
    ErrorBox(FormatStrStr(msg_cantfindfile,R^.GetFileName),nil);
  GotoReference:=W<>nil;
end;

{****************************************************************************
                               TSymbolScopeView
****************************************************************************}

constructor TSymbolScopeView.Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  Symbols:=ASymbols;
  NewList(ASymbols);
  New(SymbolsValue,Init(50,50));
  SetRange(Symbols^.Count);
end;

destructor TSymbolScopeView.Done;
begin
  {if assigned(Symbols) then
    begin
       the elements belong to other lists
       Symbols^.DeleteAll;
       dispose(Symbols,done);
    end;}
  if Assigned(SymbolsValue) then
    begin
      Dispose(SymbolsValue,Done);
      SymbolsValue:=nil;
    end;
  Inherited Done;
end;

procedure TSymbolScopeView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
begin
  case Event.What of
    evKeyDown :
      case Event.KeyCode of
        kbBack :
          begin
            LookUp(copy(LookUpStr,1,length(LookUpStr)-1));
            ClearEvent(Event);
          end;
      else
        if Event.CharCode in[#33..#255] then
          begin
            LookUp(LookUpStr+Event.CharCode);
            ClearEvent(Event);
          end;
      end;
  end;
  OldFocus:=Focused;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
    Lookup('');
end;

procedure TSymbolScopeView.Draw;
var DeltaX: sw_integer;
begin
  inherited Draw;
  if Assigned(HScrollBar)=false then DeltaX:=0 else
    DeltaX:=HScrollBar^.Value-HScrollBar^.Min;
  SetCursor(2+SymbolTypLen+length(LookUpStr)-DeltaX,Focused-TopItem);
end;

procedure TSymbolScopeView.LookUp(S: string);
var Idx,Slength: Sw_integer;
    NS: string;
begin
  NS:=LookUpStr;
  Slength:=Length(S);
  if (Symbols=nil) or (S='') then NS:='' else
    begin
      S:=Symbols^.LookUp(S,Idx);
      if Idx<>-1 then
        begin
          NS:=S;
          FocusItem(Idx);
        end;
    end;
  LookUpStr:=Copy(NS,1,Slength);
  SetState(sfCursorVis,LookUpStr<>'');
  DrawView;
end;

function TSymbolScopeView.GotoItem(Item: sw_integer): boolean;
var S: PSymbol;
    OK: boolean;
begin
  OK:=Range>0;
  if OK then
  begin
    S:=List^.At(Item);
    OK:=(S^.References<>nil) and (S^.References^.Count>0);
    if OK then
      OK:=GotoReference(S^.References^.At(0));
  end;
  GotoItem:=OK;
end;

function TSymbolScopeView.TrackItem(Item: sw_integer; AutoTrack: boolean): boolean;
var S: PSymbol;
    OK: boolean;
begin
  OK:=Range>0;
  if OK then
  begin
    S:=List^.At(Item);
    OK:=(S^.References<>nil) and (S^.References^.Count>0);
    if OK then
      OK:=TrackReference(S^.References^.At(0),AutoTrack);
  end;
  TrackItem:=OK;
end;

procedure TSymbolScopeView.SetGDBCol;
var S : PSymbol;
    I : sw_integer;
begin
  if assigned(MyBW) and (SymbolsValue^.Count=0) then
    begin
      For i:=0 to Symbols^.Count-1 do
        begin
          S:=Symbols^.At(I);
          SymbolsValue^.Insert(New(PGDBValue,Init(GetStr(MyBW^.Prefix)+S^.GetName,S)));
        end;
    end;
end;

function TSymbolScopeView.GetText(Item,MaxLen: Sw_Integer): String;
var S1: string;
    S : PSymbol;
    SG : PGDBValue;
begin
  S:=Symbols^.At(Item);
  if Assigned(SymbolsValue) and (SymbolsValue^.Count>Item) then
    SG:=SymbolsValue^.At(Item)
  else
    SG:=nil;
  if assigned(SG) then
    S1:=SG^.getText
  else
    S1:=S^.GetText;
  GetText:=copy(S1,1,MaxLen);
end;


{****************************************************************************
                             TSymbolReferenceView
****************************************************************************}

constructor TSymbolReferenceView.Init(var Bounds: TRect; AReferences: PReferenceCollection;
              AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  References:=AReferences;
  NewList(AReferences);
  SetRange(References^.Count);
end;

destructor TSymbolReferenceView.Done;
begin
  Inherited Done;
end;

procedure TSymbolReferenceView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
    DontClear: boolean;
begin
  OldFocus:=Focused;
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEnter :
            TrackItem(Focused,false);
          kbCtrlEnter :
            GotoItem(Focused);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
   if (MiscOptions and moAutoTrackSource)=0 then
    ClearHighlights;
end;

procedure TSymbolReferenceView.Browse;
begin
  { do nothing here }
end;

function TSymbolReferenceView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
    P: PReference;
begin
  P:=References^.At(Item);
  S:=P^.GetFileName+'('+IntToStr(P^.Position.Y)+','+IntToStr(P^.Position.X)+')';
  GetText:=copy(S,1,MaxLen);
end;

function TSymbolReferenceView.GotoItem(Item: sw_integer): boolean;
var OK: boolean;
begin
  OK:=Range>0;
  if OK then
    OK:=GotoReference(List^.At(Item));
  GotoItem:=OK;
end;

function TSymbolReferenceView.TrackItem(Item: sw_integer; AutoTrack: boolean): boolean;
var OK: boolean;
begin
  OK:=Range>0;
  if OK then
    OK:=TrackReference(List^.At(Item),AutoTrack);
  TrackItem:=OK;
end;

procedure TSymbolReferenceView.SelectItem(Item: Sw_Integer);
begin
  GotoItem(Item);
end;


constructor TSymbolMemInfoView.Init(var Bounds: TRect; AMemInfo: PSymbolMemInfo);
begin
  inherited Init(Bounds,'');
  Options:=Options or (ofSelectable+ofTopSelect);
  MemInfo:=AMemInfo;
  MyBW:=nil;
end;

destructor TSymbolMemInfoView.Done;
begin
{  if assigned(MemInfo) then
    dispose(MemInfo);}
  Inherited Done;
end;

procedure TSymbolMemInfoView.GetText(var S: String);
function SizeStr(Size: longint): string;
var S: string[40];
begin
  S:=IntToStrL(Size,7);
  S:=S+' byte';
  if Size>1 then S:=S+'s';
  if Size=-1 then
    SizeStr:='variable'
  else
    SizeStr:=S;
end;
function AddrStr(Addr: longint): string;
{ Warning this is endian specific code !! (PM) }
type TLongint = record LoW,HiW: word; end;
begin
  with TLongint(Addr) do
  AddrStr:='$'+hexstr(HiW,4)+hexstr(LoW,4);
end;
begin
  ClearFormatParams;
  AddFormatParamStr(msg_sizeinmemory);
  AddFormatParamStr(msg_sizeonstack);
  S:=
  FormatStrF(
   #13+
{  ' Memory location: '+AddrStr(MemInfo^.Addr)+#13+
  '   Local address: '+AddrStr(MemInfo^.LocalAddr)+#13+}

  { ??? internal linker ??? }

  '%18s: '+SizeStr(MemInfo^.Size)+#13+
  '%18s: '+SizeStr(MemInfo^.PushSize)+#13+
  '',
  FormatParams);
end;

function TSymbolMemInfoView.GetPalette: PPalette;
begin
  GetPalette:=inherited GetPalette;
end;

function TSymbolMemoView.GetPalette: PPalette;
const P: string[length(CFPSymbolMemo)] = CFPSymbolMemo;
begin
  GetPalette:=@P;
end;

{****************************************************************************
                          TSymbolInheritanceView
****************************************************************************}

constructor TSymbolInheritanceView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar; ARoot: PObjectSymbol);
begin
{$ifdef HASOUTLINE}
  inherited Init(Bounds,AHScrollBar,AVScrollBar);
{$else not HASOUTLINE}
  inherited Init(Bounds,1,AVScrollBar);
  HScrollBar:=AHScrollBar;
{$endif not HASOUTLINE}
  Options:=Options or (ofSelectable+ofTopSelect);
  Root:=ARoot;
  MyBW:=nil;
  ExpandAll(Root);
{$ifdef HASOUTLINE}
  Update;
{$else not HASOUTLINE}
  SetRange(GetNumChildrenExposed(Root));
{$endif not HASOUTLINE}
end;

destructor TSymbolInheritanceView.Done;
begin
  { do not dispose,
    belongs to a symbolcollection (PM)
  if assigned(Root) then
    dispose(Root,done); }
  Inherited Done;
end;

function TSymbolInheritanceView.GetRoot: Pointer;
begin
  GetRoot:=Root;
end;

function TSymbolInheritanceView.HasChildren(Node: Pointer): Boolean;
begin
  HasChildren:=GetNumChildren(Node)>0;
end;

function TSymbolInheritanceView.GetChild(Node: Pointer; I: sw_Integer): Pointer;
begin
  GetChild:=PObjectSymbol(Node)^.GetDescendant(I);
end;

function TSymbolInheritanceView.GetNumChildren(Node: Pointer): sw_Integer;
begin
  GetNumChildren:=PObjectSymbol(Node)^.GetDescendantCount;
end;

function TSymbolInheritanceView.GetNumChildrenExposed(Node: Pointer) : sw_Integer;
var
  Nb : integer;
  P : PObjectSymbol;
    Procedure AddCount(P : PObjectSymbol);
    var
      i,count : integer;
      D : PObjectSymbol;
    begin
      if not assigned(P) then
        exit;
      Count:=P^.GetDescendantCount;
      Inc(Nb,Count);
      for I:=0 to Count-1 do
        begin
          D:=P^.GetDescendant(I);
          AddCount(D);
        end;
    end;
begin
  Nb:=0;
  AddCount(Node);
  GetNumChildrenExposed:=Nb;
end;


procedure TSymbolInheritanceView.Adjust(Node: Pointer; Expand: Boolean);
begin
  PObjectSymbol(Node)^.Expanded:=Expand;
end;

function TSymbolInheritanceView.IsExpanded(Node: Pointer): Boolean;
begin
  IsExpanded:=PObjectSymbol(Node)^.Expanded;
end;

procedure TSymbolInheritanceView.HandleEvent(var Event: TEvent);
var DontClear: boolean;
{$ifndef HASOUTLINE}
        P: TPoint;
{$endif HASOUTLINE}
begin
  case Event.What of
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
{$ifndef HASOUTLINE}
          kbEnter:
            NodeSelected(GetLineNode(Cursor.Y-Origin.Y));
{$endif HASOUTLINE}
          kbLeft,kbRight,
          kbCtrlLeft,kbCtrlRight :
            if Assigned(HScrollBar) then
              HScrollBar^.HandleEvent(Event)
            else
              DontClear:=true;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evMouseDown :
      begin
{$ifndef HASOUTLINE}
        MakeLocal(Event.Where,P);
        SetCursor(P.X,P.Y);
{$endif HASOUTLINE}
        if Event.double then
          begin
            Message(@Self,evKeyDown,kbEnter,nil);
            ClearEvent(Event);
          end;
      end;
  end;
  inherited HandleEvent(Event);
end;

function TSymbolInheritanceView.GetPalette: PPalette;
const P: string[length(CBrowserOutline)] = CBrowserOutline;
begin
  GetPalette:=@P;
end;

{$ifdef HASOUTLINE}
function TSymbolInheritanceView.GetText(Node: Pointer): String;
begin
  GetText:=PObjectSymbol(Node)^.GetName;
end;

{$else not HASOUTLINE}
function TSymbolInheritanceView.GetNode(I : sw_Integer) : Pointer;
var
  P : PObjectSymbol;
begin
  P:=Root;
  If Assigned(P) then
    P:=P^.GetDescendant(I);
  GetNode:=Pointer(P);
end;

procedure TSymbolInheritanceView.ExpandAll(Node: Pointer);
var
  i : integer;
  P : Pointer;
begin
  Adjust(Node,true);
  For i:=0 to GetNumChildren(Node)-1 do
    begin
      P:=GetChild(Node,I);
      if Assigned(P) then
        ExpandAll(P);
    end;
end;

function TSymbolInheritanceView.GetLineNode(Item : sw_Integer) : Pointer;
var
  P : PObjectSymbol;
  NT: Integer;
    procedure FindSymbol(var P:PObjectSymbol);
    var
      Q : PObjectSymbol;
      Nc,Des : integer;
    begin
      if not assigned(P) then
         exit;
      Des:=0;
      While (NT<Item) and (Des<GetNumChildren(P)) do
        begin
          Q:=P^.GetDescendant(Des);
          Inc(NT);
          if NT=Item then
            begin
              P:=Q;
              exit;
            end;
          Nc:=GetNumChildrenExposed(Q);
          If NT+Nc<Item then
            Inc(NT,Nc)
          else
            begin
              FindSymbol(Q);
              P:=Q;
              exit;
            end;
          Inc(Des);
        end;
    end;

begin
  P:=Root;
  NT:=0;
  FindSymbol(P);
  GetLineNode:=P;
end;

function TSymbolInheritanceView.GetText(Item,MaxLen: Sw_Integer): String;
var
  P,Ans : PObjectSymbol;
  NC,NT,NumParents : Integer;
  S : String;
    procedure FindSymbol(var P:PObjectSymbol);
    var
      Q : PObjectSymbol;
      Des : integer;
    begin
      if not assigned(P) then
         exit;
      Des:=0;
      While (NT<Item) and (Des<GetNumChildren(P)) do
        begin
          Q:=P^.GetDescendant(Des);
          Inc(NT);
          if NT=Item then
            begin
              P:=Q;
              exit;
            end;
          Nc:=GetNumChildrenExposed(Q);
          If NT+Nc<Item then
            Inc(NT,Nc)
          else
            begin
              FindSymbol(Q);
              P:=Q;
              exit;
            end;
          Inc(Des);
        end;
    end;

begin
  P:=Root;
  NT:=0;
  FindSymbol(P);

  if assigned(P) then
    begin
      S:=P^.GetName;
      Ans:=P^.Parent;
      NumParents:=0;
      While Assigned(Ans) do
        begin
          Inc(NumParents);
          Ans:=Ans^.Parent;
        end;
      S:=CharStr('-',NumParents)+S;
      GetText:=Copy(S,1,MaxLen);
    end
  else
    GetText:='';
end;

{$endif HASOUTLINE}


procedure TSymbolInheritanceView.Selected(I: sw_Integer);
var P: pointer;
begin
  P:=GetNode(I);
  NodeSelected(P);
end;

procedure TSymbolInheritanceView.NodeSelected(P: pointer);
var
    S: PSymbol;
    St : String;
    Anc: PObjectSymbol;
begin
  if P=nil then Exit;

  S:=PObjectSymbol(P)^.Symbol;

  { this happens for the top objects view (PM) }
  if S=nil then exit;

  st:=S^.GetName;
  if S^.Ancestor=nil then
    Anc:=ObjectTree
  else
    Anc:=SearchObjectForSymbol(S^.Ancestor);
  OpenSymbolBrowser(Origin.X-1,
{$ifdef HASOUTLINE}
    FOC-Delta.Y+1,
{$else not HASOUTLINE}
    Origin.Y+1,
{$endif not HASOUTLINE}
    st,
    S^.GetText,S,nil,
    S^.Items,S^.References,Anc,S^.MemInfo);
end;


{****************************************************************************
                               TBrowserTab
****************************************************************************}

constructor TBrowserTab.Init(var Bounds: TRect; AItems: PBrowserTabItem);
begin
  inherited Init(Bounds);
  Options:=Options or ofPreProcess;
  Items:=AItems;
  SetParams(0,0);
end;

procedure TBrowserTab.SetParams(AFlags: word; ACurrent: Sw_integer);
begin
  Flags:=AFlags;
  SelectItem(ACurrent);
end;

procedure TBrowserTab.SelectItem(Index: Sw_integer);
var P: PBrowserTabItem;
begin
  Current:=Index;
  P:=GetItem(Current);
  if (P<>nil) and (P^.Link<>nil) then
    P^.Link^.Focus;
  DrawView;
end;

function TBrowserTab.GetItemCount: sw_integer;
var Count: integer;
    P: PBrowserTabItem;
begin
  Count:=0; P:=Items;
  while (P<>nil) do
    begin
      Inc(Count);
      P:=P^.Next;
    end;
  GetItemCount:=Count;
end;

function TBrowserTab.GetItem(Index: sw_integer): PBrowserTabItem;
var Counter: integer;
    P: PBrowserTabItem;
begin
  P:=Items;
  Counter:=0;
  while (P<>nil) and (Counter<Index) do
    begin
      P:=P^.Next;
      Inc(Counter);
    end;
  GetItem:=P;
end;

procedure TBrowserTab.Draw;
var B: TDrawBuffer;
    SelColor, NormColor, C: word;
    I,CurX,Count: Sw_integer;
function Names(Idx: integer): char;
begin
  Names:=GetItem(Idx)^.Sign;
end;
begin
  NormColor:=GetColor(1); SelColor:=GetColor(2);
  MoveChar(B,'Ä',SelColor,Size.X);
  CurX:=0; Count:=0;
  for I:=0 to GetItemCount-1 do
    if (Flags and (1 shl I))<>0 then
    begin
      Inc(Count);
      if Current=I then C:=SelColor
                   else C:=NormColor;
      if Count=1 then MoveChar(B[CurX],'´',SelColor,1)
                 else MoveChar(B[CurX],'³',SelColor,1);
      MoveCStr(B[CurX+1],' '+Names(I)+' ',C);
      Inc(CurX,4);
    end;
  if Count>0 then
    MoveChar(B[CurX],'Ã',SelColor,1);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

procedure TBrowserTab.HandleEvent(var Event: TEvent);
var I,Idx: integer;
    DontClear: boolean;
    P: TPoint;
function GetItemForCoord(X: integer): integer;
var I,CurX,Idx: integer;
begin
  CurX:=0; Idx:=-1;
  for I:=0 to GetItemCount-1 do
    if (Flags and (1 shl I))<>0 then
    begin
      if (CurX+1<=X) and (X<=CurX+3) then
        begin Idx:=I; Break; end;
      Inc(CurX,4);
    end;
  GetItemForCoord:=Idx;
end;
begin
  case Event.What of
    evMouseDown :
      if MouseInView(Event.Where) then
        begin
          repeat
            MakeLocal(Event.Where,P);
            Idx:=GetItemForCoord(P.X);
            if Idx<>-1 then
              SelectItem(Idx);
          until not MouseEvent(Event, evMouseMove);
          ClearEvent(Event);
        end;
    evKeyDown :
      begin
        DontClear:=false; Idx:=-1;
        for I:=0 to GetItemCount-1 do
          if (GetCtrlCode(GetItem(I)^.Sign)=Event.KeyCode){ or
             (GetItem(I)^.Sign=UpCase(Event.CharCode))}  then
           if (Flags and (1 shl I))<>0 then
            begin
              Idx:=I;
              Break;
            end;
        if Idx=-1 then
          DontClear:=true
        else
          SelectItem(Idx);
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TBrowserTab.GetPalette: PPalette;
const P: string[length(CBrowserTab)] = CBrowserTab;
begin
  GetPalette:=@P;
end;

destructor TBrowserTab.Done;
begin
  if Items<>nil then DisposeBrowserTabList(Items);
  inherited Done;
end;

procedure TUnitInfoPanel.HandleEvent(var Event: TEvent);
begin
  if (Event.What=evBroadcast) and (Event.Command=cmListItemSelected) and
     (InOwnerCall=false) then
    begin
      InOwnerCall:=true;
      if Assigned(Owner) then
        Owner^.HandleEvent(Event);
      InOwnerCall:=false;
    end;
  inherited HandleEvent(Event);
end;

constructor TBrowserWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
             const AName,APrefix: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection;
             AInheritance: PObjectSymbol; AMemInfo: PSymbolMemINfo);
var R,R2,R3: TRect;
    HSB,VSB: PScrollBar;
    CST: PColorStaticText;
    I: sw_integer;
function CreateVSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  R2.Copy(R); R2.Move(1,0); R2.A.X:=R2.B.X-1;
  New(SB, Init(R2)); SB^.GrowMode:=gfGrowLoX+gfGrowHiX+gfGrowHiY;
  CreateVSB:=SB;
end;
function CreateHSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  R2.Copy(R); R2.Move(0,1); R2.A.Y:=R2.B.Y-1;
  New(SB, Init(R2)); SB^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
  CreateHSB:=SB;
end;
begin
  inherited Init(Bounds, FormatStrStr(dialog_browse,ATitle), ANumber);
  HelpCtx:=hcBrowserWindow;
  Sym:=ASym;
  Prefix:=NewStr(APrefix);

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
{$ifndef NODEBUG}
  if {assigned(Debugger) and Debugger^.IsRunning and}
     assigned(Sym) and (Sym^.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym]) then
    begin
      New(DebuggerValue,Init(ATitle,Sym));
      New(ST, Init(R, ' '+DebuggerValue^.GetText));
    end
  else
{$endif NODEBUG}
    begin
      New(ST, Init(R, ' '+AName));
      DebuggerValue:=nil;
    end;
  ST^.GrowMode:=gfGrowHiX;
  Insert(ST);

  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,2);
  if assigned(ASymbols) and (ASymbols^.Count>0) then
    begin
      HSB:=CreateHSB(R);
      Insert(HSB);
      VSB:=CreateVSB(R);
      Insert(VSB);
      New(ScopeView, Init(R, ASymbols, HSB, VSB));
      ScopeView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ScopeView);
      ScopeView^.MyBW:=@Self;
      ScopeView^.SetGDBCol;
    end;
  if assigned(AReferences) and (AReferences^.Count>0) then
    begin
      HSB:=CreateHSB(R);
      Insert(HSB);
      VSB:=CreateVSB(R);
      Insert(VSB);
      New(ReferenceView, Init(R, AReferences, HSB, VSB));
      ReferenceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ReferenceView);
      ReferenceView^.MyBW:=@Self;
    end;
  if assigned(AInheritance) then
    begin
      HSB:=CreateHSB(R);
      Insert(HSB);
      VSB:=CreateVSB(R);
      Insert(VSB);
      New(InheritanceView, Init(R, HSB,VSB, AInheritance));
      InheritanceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(InheritanceView);
      InheritanceView^.MyBW:=@Self;
    end;
  if assigned(AMemInfo) then
    begin
      New(MemInfoView, Init(R, AMemInfo));
      MemInfoView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(MemInfoView);
      MemInfoView^.MyBW:=@Self;
    end;
  if Assigned(Asym) and (TypeOf(ASym^)=TypeOf(TModuleSymbol)) then
  with PModuleSymbol(Sym)^ do
    begin
      New(UnitInfo, Init(R));
      UnitInfo^.GetExtent(R3);

      R2.Copy(R3);
      R2.B.Y:=R2.A.Y+3;
      if (Assigned(UsedUnits) or Assigned(DependentUnits))=false then
        R2.B.Y:=R3.B.Y;
      HSB:=CreateHSB(R2); {UnitInfo^.Insert(HSB); HSB:=nil;}
      VSB:=CreateVSB(R2);
      {UnitInfo^.Insert(VSB);
       VSB will be owned by UnitInfoText PM }
      New(UnitInfoText, Init(R2,HSB,VSB, nil));
      with UnitInfoText^ do
      begin
        GrowMode:=gfGrowHiX;
        if Assigned(LoadedFrom) then
        begin
          AddLine(FormatStrStr2('%s : %s',msg_usedfirstin,GetStr(LoadedFrom)));
          AddLine(FormatStrStr('%s : ',msg_mainsource));
          AddLine(FormatStrStr('  %s',GetStr(MainSource)));
          if Assigned(SourceFiles) and (SourceFiles^.Count>1) then
          begin
            AddLine(FormatStrStr('%s : ',msg_sourcefiles));
            for I:=0 to SourceFiles^.Count-1 do
              AddLine(FormatStrStr('  %s',GetStr(SourceFiles^.At(I))));
          end;
        end;
      end;
      UnitInfo^.Insert(UnitInfoText);

      if Assigned(UsedUnits) then
      begin
        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R2.A.Y+1;
        New(CST, Init(R2,'´ Used units Ã'+CharStr('Ä',255),ColorIndex(12),false));
        CST^.GrowMode:=gfGrowHiX;
        UnitInfo^.Insert(CST);

        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R2.A.Y+4;
        if Assigned(DependentUnits)=false then R2.B.Y:=R3.B.Y;
        {HSB:=CreateHSB(R2); UnitInfo^.Insert(HSB); }
        HSB:=nil;
        VSB:=CreateVSB(R2);
        {UnitInfo^.Insert(VSB);  this created crashes,
        that were difficult to findout PM }
        New(UnitInfoUsed, Init(R2,UsedUnits,HSB,VSB));
        UnitInfoUsed^.GrowMode:=gfGrowHiY+gfGrowHiX;
        UnitInfoUsed^.MyBW:=@Self;
        UnitInfo^.Insert(UnitInfoUsed);
      end;

      if Assigned(DependentUnits) then
      begin
        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R2.A.Y+1;
        New(CST, Init(R2,'´ Dependent units Ã'+CharStr('Ä',255),ColorIndex(12),false));
        CST^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
        UnitInfo^.Insert(CST);

        Inc(R2.A.Y,R2.B.Y-R2.A.Y); R2.B.Y:=R3.B.Y;
        {HSB:=CreateHSB(R2); UnitInfo^.Insert(HSB); }
        HSB:=nil;
        VSB:=CreateVSB(R2);
        { UnitInfo^.Insert(VSB);  this created crashes,
        that were difficult to findout PM }
        New(UnitInfoDependent, Init(R2,DependentUnits,HSB,VSB));
        UnitInfoDependent^.GrowMode:=gfGrowLoY+gfGrowHiX+gfGrowHiY;
        UnitInfoDependent^.MyBW:=@Self;
        UnitInfo^.Insert(UnitInfoDependent);
      end;

      if Assigned(UnitInfoText) then
        UnitInfoText^.Select;

      Insert(UnitInfo);
    end;

  GetExtent(R); R.Grow(-1,-1); R.Move(0,1); R.B.Y:=R.A.Y+1;
  New(PageTab, Init(R,
    NewBrowserTabItem(label_browsertab_scope,ScopeView,
    NewBrowserTabItem(label_browsertab_reference,ReferenceView,
    NewBrowserTabItem(label_browsertab_inheritance,InheritanceView,
    NewBrowserTabItem(label_browsertab_memory,MemInfoView,
    NewBrowserTabItem(label_browsertab_unit,UnitInfo,
    nil)))))));
  PageTab^.GrowMode:=gfGrowHiX;
  Insert(PageTab);

  if assigned(ScopeView) then
   SelectTab(btScope)
  else if assigned(ReferenceView) then
    SelectTab(btReferences)
  else if assigned(MemInfoView) then
    SelectTab(btMemInfo)
  else
   if assigned(InheritanceView) then
    SelectTab(btInheritance);
end;

destructor  TBrowserWindow.Done;
begin
  { UnitInfoText needs to be removed first
    to avoid crashes within the UnitInfo destructor PM }
  if Assigned(UnitInfoText) then
    begin
      UnitInfo^.Delete(UnitInfoText);
      Dispose(UnitInfoText,Done);
      UnitInfoText:=nil;
    end;
  if assigned(DebuggerValue) then
    begin
      Dispose(DebuggerValue,Done);
      DebuggerValue:=nil;
    end;
  if assigned(Prefix) then
    begin
      DisposeStr(Prefix);
      Prefix:=nil;
    end;
  inherited Done;
end;

procedure TBrowserWindow.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    S: PSymbol;
    Symbols: PSymbolCollection;
    Anc: PObjectSymbol;
    P: TPoint;
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmDebuggerStopped :
          begin
            if Assigned(DebuggerValue) and
               (DebuggerValue^.GDBI<>PtrInt(Event.InfoPtr)) then
              begin
                If Assigned(ST^.Text) then
                  DisposeStr(ST^.Text);
                ST^.Text:=NewStr(DebuggerValue^.GetText);
                ST^.DrawView;
              end;
          end;
        cmSearchWindow :
          ClearEvent(Event);
        cmListItemSelected :
          begin
            S:=nil;
            if (Event.InfoPtr=ScopeView) then
              begin
                S:=ScopeView^.Symbols^.At(ScopeView^.Focused);
                MakeGlobal(ScopeView^.Origin,P);
                Desktop^.MakeLocal(P,P); Inc(P.Y,ScopeView^.Focused-ScopeView^.TopItem);
                Inc(P.Y);
              end;
            if (Event.InfoPtr=UnitInfoUsed) then
              begin
                S:=UnitInfoUsed^.Symbols^.At(UnitInfoUsed^.Focused);
                MakeGlobal(UnitInfoUsed^.Origin,P);
                Desktop^.MakeLocal(P,P); Inc(P.Y,UnitInfoUsed^.Focused-UnitInfoUsed^.TopItem);
                Inc(P.Y);
              end;
            if (Event.InfoPtr=UnitInfoDependent) then
              begin
                S:=UnitInfoDependent^.Symbols^.At(UnitInfoDependent^.Focused);
                MakeGlobal(UnitInfoDependent^.Origin,P);
                Desktop^.MakeLocal(P,P); Inc(P.Y,UnitInfoDependent^.Focused-UnitInfoDependent^.TopItem);
                Inc(P.Y);
              end;
            if Assigned(S) then
              begin
                if S^.Ancestor=nil then Anc:=nil else
                  Anc:=SearchObjectForSymbol(S^.Ancestor);
                Symbols:=S^.Items;
                if (not assigned(Symbols)  or (symbols^.count=0)) then
                  if assigned(S^.Ancestor) then
                    Symbols:=S^.Ancestor^.Items;
                if (S^.GetReferenceCount>0) or (assigned(Symbols) and (Symbols^.Count>0)) or (Anc<>nil) then
                 OpenSymbolBrowser(Origin.X-1,P.Y,
                   S^.GetName,
                   ScopeView^.GetText(ScopeView^.Focused,255),
                   S,@self,
                   Symbols,S^.References,Anc,S^.MemInfo);
              end;
            end;
      end;
{    evCommand :
      begin
        DontClear:=false;
        case Event.Command of
        cmGotoSymbol :
          if Event.InfoPtr=ScopeView then
           if ReferenceView<>nil then
            if ReferenceView^.Range>0 then
              ReferenceView^.GotoItem(0);
        cmTrackSymbol :
          if Event.InfoPtr=ScopeView then
            if (ScopeView<>nil) and (ScopeView^.Range>0) then
              begin
                S:=ScopeView^.At(ScopeView^.Focused);
                if (S^.References<>nil) and (S^.References^.Count>0) then
                  TrackItem(S^.References^.At(0));
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;}
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEsc :
            Close;
          kbAltI :
            If not Disassemble then
              DontClear:=true;
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

function TBrowserWindow.Disassemble : boolean;
begin
  Disassemble:=false;
  if not assigned(sym) or (sym^.typ<>procsym) then
    exit;
  { We need to load exefile }
{$ifndef NODEBUG}
  InitGDBWindow;
  if not assigned(Debugger) then
    begin
      new(Debugger,Init);
      if assigned(Debugger) then
        Debugger^.SetExe(ExeFile);
    end;
  if not assigned(Debugger) or not Debugger^.HasExe then
    exit;
  { goto source/assembly mixture }
  InitDisassemblyWindow;
  DisassemblyWindow^.LoadFunction(Sym^.GetName);
  DisassemblyWindow^.SelectInDebugSession;
  Disassemble:=true;
{$else NODEBUG}
  NoDebugger;
{$endif NODEBUG}
end;

procedure TBrowserWindow.SetState(AState: Word; Enable: Boolean);
{var OldState: word;}
begin
{  OldState:=State;}
  inherited SetState(AState,Enable);
{  if ((State xor OldState) and sfActive)<>0 then
    if GetState(sfActive)=false then
      Message(Desktop,evBroadcast,cmClearLineHighlights,nil);}
end;

procedure TBrowserWindow.Close;
begin
  inherited Close;
end;

procedure TBrowserWindow.SelectTab(BrowserTab: Sw_integer);
var Tabs: Sw_integer;
{$ifndef NODEBUG}
    PB : PBreakpoint;
{$endif}
    PS :PString;
    l : longint;
begin
  case BrowserTab of
    btScope :
      if assigned(ScopeView) then
        ScopeView^.Select;
    btReferences :
      if assigned(ReferenceView) then
        ReferenceView^.Select;
    btMemInfo:
      if assigned(MemInfoView) then
        MemInfoView^.Select;
{$ifndef NODEBUG}
    btBreakWatch :
      begin
        if Assigned(Sym) then
          begin
            if Pos('proc',Sym^.GetText)>0 then
          { insert function breakpoint }
            begin
               { make it visible }
               PS:=Sym^.Name;
               l:=Length(PS^);
               If PS^[l]='*' then
                 begin
                   PB:=BreakpointsCollection^.GetType(bt_function,copy(GetStr(PS),1,l-1));
                   If Assigned(PB) then
                     BreakpointsCollection^.Delete(PB);
                   Sym^.Name:=NewStr(copy(GetStr(PS),1,l-1));
                   DrawView;
                   DisposeStr(PS);
                 end
               else
                 begin
                   Sym^.Name:=NewStr(GetStr(PS)+'*');
                   DrawView;
                   New(PB,init_function(GetStr(PS)));
                   DisposeStr(PS);
                   BreakpointsCollection^.Insert(PB);
                   BreakpointsCollection^.Update;
                 end;
            end
          else if pos('var',Sym^.GetText)>0 then
            { insert watch point }
            begin
               { make it visible }
               PS:=Sym^.Name;
               l:=Length(PS^);
               If PS^[l]='*' then
                 begin
                   PB:=BreakpointsCollection^.GetType(bt_awatch,copy(PS^,1,l-1));
                   If Assigned(PB) then
                     BreakpointsCollection^.Delete(PB);
                   Sym^.Name:=NewStr(copy(PS^,1,l-1));
                   DrawView;
                   DisposeStr(PS);
                 end
               else
                 begin
                   Sym^.Name:=NewStr(GetStr(PS)+'*');
                   DrawView;
                   New(PB,init_type(bt_awatch,GetStr(PS)));
                   DisposeStr(PS);
                   BreakpointsCollection^.Insert(PB);
                   BreakpointsCollection^.Update;
                 end;
            end;
        end;
      end;
{$endif NODEBUG}
  end;
  Tabs:=0;
  if assigned(ScopeView) then
    Tabs:=Tabs or (1 shl btScope);
  if assigned(ReferenceView) then
    Tabs:=Tabs or (1 shl btReferences);
  if assigned(InheritanceView) then
    Tabs:=Tabs or (1 shl btInheritance);
  if assigned(MemInfoView) then
    Tabs:=Tabs or (1 shl btMemInfo);
{$ifndef NODEBUG}
  if Assigned(Sym) then
    if (Pos('proc',Sym^.GetText)>0) or (Pos('var',Sym^.GetText)>0) then
      Tabs:=Tabs or (1 shl btBreakWatch);
{$endif NODEBUG}
  if assigned(UnitInfo) then
    Tabs:=Tabs or (1 shl btUnitInfo);
  if PageTab<>nil then PageTab^.SetParams(Tabs,BrowserTab);
end;

function TBrowserWindow.GetPalette: PPalette;
const S: string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;S : PSymbol;
            ParentBrowser : PBrowserWindow;
            Symbols: PSymbolCollection; References: PReferenceCollection;
            Inheritance: PObjectSymbol; MemInfo: PSymbolMemInfo);
var R: TRect;
    PB : PBrowserWindow;
    St,st2 : string;
begin
  if X=0 then X:=Desktop^.Size.X-35;
  R.A.X:=X; R.A.Y:=Y;
  R.B.X:=R.A.X+35; R.B.Y:=R.A.Y+15;
  while (R.B.Y>Desktop^.Size.Y) do R.Move(0,-1);
  if assigned(ParentBrowser) and assigned(ParentBrowser^.Prefix) and
     assigned(ParentBrowser^.sym) and
     (ParentBrowser^.sym^.typ<>unitsym)
     then
    begin
      st:=GetStr(ParentBrowser^.Prefix)+' '+Name;
    end
  else
    st:=Name;
  st2:=st;
  if assigned(S) and ((S^.Flags and sfPointer)<>0) then
    begin
      st:=st+'^';
      if assigned(S^.Ancestor) and
         ((S^.Ancestor^.Flags and sfRecord)<>0) then
        st:=st+'.';
    end
  else if assigned(S) and ((S^.Flags and sfRecord)<>0) then
    st:=st+'.';

  PB:=New(PBrowserWindow, Init(R,
    st2,SearchFreeWindowNo,S,Line,st,
    Symbols,References,Inheritance,MemInfo));
  if (assigned(S) and (S^.typ in [fieldvarsym,staticvarsym,localvarsym,paravarsym])) or
     (assigned(ParentBrowser) and ParentBrowser^.IsValid) then
    PB^.IsValid:=true;

  Desktop^.Insert(PB);
end;

END.
