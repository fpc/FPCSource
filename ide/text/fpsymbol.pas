{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    Symbol browse support routines for the IDE

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPSymbol;

interface

uses Objects,Drivers,Views,Dialogs,
     BrowCol,
     FPViews;

const
      { Browser tab constants }
      btScope       = 1;
      btReferences  = 2;
      btInheritance = 4;
      btBreakWatch  = 8;
      
type
    PSymbolView = ^TSymbolView;
    TSymbolView = object(TListBox)
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure    HandleEvent(var Event: TEvent); virtual;
      procedure    GotoItem(Item: sw_integer); virtual;
      function     GetPalette: PPalette; virtual;
    end;

    PSymbolScopeView = ^TSymbolScopeView;
    TSymbolScopeView = object(TSymbolView)
      constructor Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   Draw; virtual;
      procedure   LookUp(S: string); virtual;
    private
      Symbols: PSymbolCollection;
      LookupStr: string;
    end;

    PSymbolReferenceView = ^TSymbolReferenceView;
    TSymbolReferenceView = object(TSymbolView)
      constructor Init(var Bounds: TRect; AReferences: PReferenceCollection; AHScrollBar, AVScrollBar: PScrollBar);
      procedure   HandleEvent(var Event: TEvent); virtual;
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   SelectItem(Item: Sw_Integer); virtual;
      procedure   GotoItem(Item: sw_integer); virtual;
      procedure   GotoSource; virtual;
      procedure   TrackSource; virtual;
    private
      References: PReferenceCollection;
    end;

    PBrowserTab = ^TBrowserTab;
    TBrowserTab = object(TView)
      constructor Init(var Bounds: TRect);
      procedure   SetParams(AFlags: word; ACurrent: Sw_integer); virtual;
      procedure   Draw; virtual;
      function    GetPalette: PPalette; virtual;
    private
      Flags   : word;
      Current : Sw_integer;
    end;

    PBrowserWindow = ^TBrowserWindow;
    TBrowserWindow = object(TFPWindow)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
                    const AName: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection);
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SetState(AState: Word; Enable: Boolean); virtual;
      procedure   Close; virtual;
      procedure   SelectTab(BrowserTab: Sw_integer); virtual;
      function    GetPalette: PPalette; virtual;
    private
      PageTab       : PBrowserTab;
      Sym           : PSymbol;
      ScopeView     : PSymbolScopeView;
      ReferenceView : PSymbolReferenceView;
    end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;S : PSymbol;
            Symbols: PSymbolCollection; References: PReferenceCollection);

function IsSymbolInfoAvailable: boolean;

procedure OpenOneSymbolBrowser(Name : String);

implementation

uses Commands,App,
     WEditor,FPDebug,
     FPConst,FPUtils,FPVars;

function IsSymbolInfoAvailable: boolean;
begin
  IsSymbolInfoAvailable:=BrowCol.Modules<>nil;
end;

procedure OpenOneSymbolBrowser(Name : String);

var Index : sw_integer;
    PS : PSymbol;
    P : Pstring;

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
         OpenSymbolBrowser(0,20,
                PS^.Items^.At(Index)^.GetName,'',PS^.Items^.At(Index),
                PS^.Items^.At(Index)^.Items,PS^.Items^.At(Index)^.References)
       else
         begin
           P:=@Name;
           ErrorBox(#3'Symbol %s not found',@P);
         end;
     end
   else
     ErrorBox('No Browser info available',nil);
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
                               TSymbolView
****************************************************************************}

constructor TSymbolView.Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,1,AVScrollBar);
  HScrollBar:=AHScrollBar;
  if assigned(HScrollBar) then
    HScrollBar^.SetRange(1,80);
  Options:=Options or (ofSelectable+ofTopSelect);
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
            GotoItem(Focused);
          kbRight,kbLeft :
            if HScrollBar<>nil then
              HScrollBar^.HandleEvent(Event);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
    evMouse :
      if Event.double then
        GotoItem(Focused);
  end;
  inherited HandleEvent(Event);
end;

function TSymbolView.GetPalette: PPalette;
const
  P: string[length(CBrowserListBox)] = CBrowserListBox;
begin
  GetPalette:=@P;
end;


procedure TSymbolView.GotoItem(Item: sw_integer);
begin
  SelectItem(Item);
end;

{****************************************************************************
                               TSymbolScopeView
****************************************************************************}

constructor TSymbolScopeView.Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
begin
  inherited Init(Bounds,AHScrollBar, AVScrollBar);
  Symbols:=ASymbols;
  NewList(ASymbols);
  SetRange(Symbols^.Count);
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
        if Event.CharCode in[#32..#255] then
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
begin
  inherited Draw;
  SetCursor(2+SymbolTypLen+length(LookUpStr),Focused-TopItem);
end;

procedure TSymbolScopeView.LookUp(S: string);
var Idx: Sw_integer;
    NS: string;
begin
  NS:=LookUpStr;
  if (Symbols=nil) or (S='') then NS:='' else
    begin
      S:=Symbols^.LookUp(S,Idx);
      if Idx<>-1 then
        begin
          NS:=S;
          FocusItem(Idx);
        end;
    end;
  LookUpStr:=NS;
  SetState(sfCursorVis,LookUpStr<>'');
  DrawView;
end;

function TSymbolScopeView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
begin
  S:=Symbols^.At(Item)^.GetText;
  GetText:=copy(S,1,MaxLen);
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

procedure TSymbolReferenceView.HandleEvent(var Event: TEvent);
var OldFocus: sw_integer;
begin
  OldFocus:=Focused;
  inherited HandleEvent(Event);
  if OldFocus<>Focused then
    Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

function TSymbolReferenceView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
    P: PReference;
begin
  P:=References^.At(Item);
  S:=P^.GetFileName+'('+IntToStr(P^.Position.Y)+','+IntToStr(P^.Position.X)+')';
  GetText:=copy(S,1,MaxLen);
end;

procedure TSymbolReferenceView.GotoSource;
var R: PReference;
    W: PSourceWindow;
begin
  if Range=0 then Exit;
  R:=References^.At(Focused);
  Desktop^.Lock;
  W:=TryToOpenFile(nil,R^.GetFileName,R^.Position.X-1,R^.Position.Y-1);
  if W<>nil then W^.Select;
  Desktop^.UnLock;
end;

function LastBrowserWindow: PBrowserWindow;
var BW: PBrowserWindow;
procedure IsBW(P: PView); {$ifndef FPC}far;{$endif}
begin
  if (P^.HelpCtx=hcBrowserWindow) then
    BW:=pointer(P);
end;
begin
  BW:=nil;
  Desktop^.ForEach(@IsBW);
  LastBrowserWindow:=BW;
end;

procedure TSymbolReferenceView.TrackSource;
var R: PReference;
    W: PSourceWindow;
    BW: PBrowserWindow;
    P: TPoint;
begin
  Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
  if Range=0 then Exit;
  R:=References^.At(Focused);
  Desktop^.Lock;
  P.X:=R^.Position.X-1; P.Y:=R^.Position.Y-1;
  W:=TryToOpenFile(nil,R^.GetFileName,P.X,P.Y);
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
    W^.Editor^.SetHighlightRow(P.Y);
  end;
  Desktop^.UnLock;
end;

procedure TSymbolReferenceView.GotoItem(Item: sw_integer);
begin
  GotoSource;
end;

procedure TSymbolReferenceView.SelectItem(Item: Sw_Integer);
begin
  TrackSource;
end;


{****************************************************************************
                               TBrowserTab
****************************************************************************}

constructor TBrowserTab.Init(var Bounds: TRect);
begin
  inherited Init(Bounds);
  SetParams(0,0);
end;

procedure TBrowserTab.SetParams(AFlags: word; ACurrent: Sw_integer);
begin
  Flags:=AFlags; Current:=ACurrent;
  DrawView;
end;

procedure TBrowserTab.Draw;
var B: TDrawBuffer;
    SelColor, NormColor, C: word;
    I,CurX,Count: Sw_integer;
const
    Names: string[4] = 'SRIB';
begin
  NormColor:=GetColor(1); SelColor:=GetColor(2);
  MoveChar(B,'Ä',SelColor,Size.X);
  CurX:=0; Count:=0;
  for I:=0 to 3 do
    if (Flags and (1 shl I))<>0 then
    begin
      Inc(Count);
      if Current=(1 shl I) then C:=SelColor
                           else C:=NormColor;
      if Count=1 then MoveChar(B[CurX],'´',SelColor,1)
                 else MoveChar(B[CurX],'³',SelColor,1);
      MoveCStr(B[CurX+1],' '+Names[I+1]+' ',C);
      Inc(CurX,4);
    end;
  if Count>0 then
    MoveChar(B[CurX],'Ã',SelColor,1);
  WriteLine(0,0,Size.X,Size.Y,B);
end;

function TBrowserTab.GetPalette: PPalette;
const P: string[length(CBrowserTab)] = CBrowserTab;
begin
  GetPalette:=@P;
end;

constructor TBrowserWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;ASym : PSymbol;
             const AName: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection);
var R: TRect;
    ST: PStaticText;
    HSB,VSB: PScrollBar;
function CreateVSB(R: TRect): PScrollBar;
var R2: TRect;
    SB: PScrollBar;
begin
  Sym:=ASym;
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
  inherited Init(Bounds, ATitle, ANumber);
  HelpCtx:=hcBrowserWindow;

  GetExtent(R); R.Grow(-1,-1); R.B.Y:=R.A.Y+1;
  New(ST, Init(R, ' '+AName)); ST^.GrowMode:=gfGrowHiX;
  Insert(ST);

  GetExtent(R); R.Grow(-1,-1); R.Move(0,1); R.B.Y:=R.A.Y+1;
  New(PageTab, Init(R));
  PageTab^.GrowMode:=gfGrowHiX;
  Insert(PageTab);

  GetExtent(R); R.Grow(-1,-1); Inc(R.A.Y,2);
  if assigned(ASymbols) and (ASymbols^.Count>0) then
    begin
      HSB:=CreateHSB(R); Insert(HSB);
      VSB:=CreateVSB(R); Insert(VSB);
      New(ScopeView, Init(R, ASymbols, HSB, VSB));
      ScopeView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ScopeView);
    end;
  if assigned(AReferences) and (AReferences^.Count>0) then
    begin
      HSB:=CreateHSB(R); Insert(HSB);
      VSB:=CreateVSB(R); Insert(VSB);
      New(ReferenceView, Init(R, AReferences, HSB, VSB));
      ReferenceView^.GrowMode:=gfGrowHiX+gfGrowHiY;
      Insert(ReferenceView);
    end;
  if assigned(ScopeView) then
   SelectTab(btScope)
  else
   if assigned(ReferenceView) then
    SelectTab(btReferences);
end;

procedure TBrowserWindow.HandleEvent(var Event: TEvent);
var DontClear: boolean;
    S: PSymbol;
    P: TPoint;
begin
  case Event.What of
    evBroadcast :
      case Event.Command of
        cmSearchWindow :
          ClearEvent(Event);
        cmListItemSelected :
          if Event.InfoPtr=ScopeView then
            begin
              S:=ScopeView^.Symbols^.At(ScopeView^.Focused);
              MakeGlobal(ScopeView^.Origin,P);
              Desktop^.MakeLocal(P,P); Inc(P.Y,ScopeView^.Focused-ScopeView^.TopItem);
              Inc(P.Y);
              if (S^.GetReferenceCount>0) or (S^.GetItemCount>0) then
              OpenSymbolBrowser(Origin.X-1,P.Y,
                S^.GetName,
                ScopeView^.GetText(ScopeView^.Focused,255),S,
                S^.Items,S^.References);
            end;
      end;
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEsc :
            Close;
          kbCtrlB :
            SelectTab(btBreakWatch);
          kbCtrlS :
            SelectTab(btScope);
          kbCtrlR :
            SelectTab(btReferences);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
      end;
  end;
  inherited HandleEvent(Event);
end;

procedure TBrowserWindow.SetState(AState: Word; Enable: Boolean);
var OldState: word;
begin
  OldState:=State;
  inherited SetState(AState,Enable);
  if ((State xor OldState) and sfActive)<>0 then
    if GetState(sfActive)=false then
      Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
end;

procedure TBrowserWindow.Close;
begin
  Message(Desktop,evBroadcast,cmClearLineHighlights,nil);
  inherited Close;
end;

procedure TBrowserWindow.SelectTab(BrowserTab: Sw_integer);
var Tabs: Sw_integer;
    PB : PBreakpoint;
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
                   PB:=BreakpointCollection^.GetType(bt_function,copy(GetStr(PS),1,l-1));
                   If Assigned(PB) then
                     BreakpointCollection^.Delete(PB);
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
                   BreakpointCollection^.Insert(PB);
                   BreakpointCollection^.Update;
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
                   PB:=BreakpointCollection^.GetType(bt_awatch,copy(PS^,1,l-1));
                   If Assigned(PB) then
                     BreakpointCollection^.Delete(PB);
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
                   BreakpointCollection^.Insert(PB);
                   BreakpointCollection^.Update;
                 end;
            end;
        end;
      end;

  end;
  Tabs:=0;
  if assigned(ScopeView) then
    Tabs:=Tabs or btScope;
  if assigned(ReferenceView) then
    Tabs:=Tabs or btReferences;
  if Assigned(Sym) then
    if (Pos('proc',Sym^.GetText)>0) or (Pos('var',Sym^.GetText)>0) then
      Tabs:=Tabs or btBreakWatch;
  if (Tabs and BrowserTab)=0 then
    if (Tabs and btScope)<>0 then BrowserTab:=btScope else
    if (Tabs and btReferences)<>0 then BrowserTab:=btReferences else
    if (Tabs and btInheritance)<>0 then BrowserTab:=btInheritance else
      BrowserTab:=btBreakWatch;
  if PageTab<>nil then PageTab^.SetParams(Tabs,BrowserTab);
end;

function TBrowserWindow.GetPalette: PPalette;
const S: string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;S : PSymbol;
            Symbols: PSymbolCollection; References: PReferenceCollection);
var R: TRect;
begin
  if X=0 then X:=Desktop^.Size.X-35;
  R.A.X:=X; R.A.Y:=Y;
  R.B.X:=R.A.X+35; R.B.Y:=R.A.Y+15;
  while (R.B.Y>Desktop^.Size.Y) do R.Move(0,-1);
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    'Browse: '+Name,SearchFreeWindowNo,S,Line,Symbols,References)));
end;


END.
{
  $Log$
  Revision 1.6  1999-02-10 09:44:59  pierre
    + added B tab for functions and vars for break/watch
      TBrowserWindow also stores the symbol itself for break/watchpoints

  Revision 1.5  1999/02/04 17:53:47  pierre
   + OpenOneSymbolBrowser

  Revision 1.4  1999/02/04 13:16:14  pierre
   + column info added

  Revision 1.3  1999/01/21 11:54:23  peter
    + tools menu
    + speedsearch in symbolbrowser
    * working run command

  Revision 1.2  1999/01/14 21:42:24  peter
    * source tracking from Gabor

  Revision 1.1  1999/01/12 14:29:40  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.0  1999/01/09 11:49:41  gabor
     Original implementation
}
