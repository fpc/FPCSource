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

type
    PSymbolView = ^TSymbolView;
    TSymbolView = object(TListBox)
      constructor  Init(var Bounds: TRect; AHScrollBar, AVScrollBar: PScrollBar);
      procedure    HandleEvent(var Event: TEvent); virtual;
      function     GetPalette: PPalette; virtual;
    end;

    PSymbolScopeView = ^TSymbolScopeView;
    TSymbolScopeView = object(TSymbolView)
      constructor Init(var Bounds: TRect; ASymbols: PSymbolCollection; AHScrollBar, AVScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
    private
      Symbols: PSymbolCollection;
    end;

    PSymbolReferenceView = ^TSymbolReferenceView;
    TSymbolReferenceView = object(TSymbolView)
      constructor Init(var Bounds: TRect; AReferences: PReferenceCollection; AHScrollBar, AVScrollBar: PScrollBar);
      function    GetText(Item,MaxLen: Sw_Integer): String; virtual;
      procedure   SelectItem(Item: Sw_Integer); virtual;
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
    TBrowserWindow = object(TWindow)
      constructor Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;
                    const AName: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection);
      procedure   HandleEvent(var Event: TEvent); virtual;
      procedure   SelectTab(BrowserTab: Sw_integer); virtual;
      function    GetPalette: PPalette; virtual;
    private
      PageTab       : PBrowserTab;
      ScopeView     : PSymbolScopeView;
      ReferenceView : PSymbolReferenceView;
    end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;
            Symbols: PSymbolCollection; References: PReferenceCollection);

implementation

uses Commands,App,
     FPConst,FPUtils,FPVars;

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
            SelectItem(Focused);
          kbRight,kbLeft :
            if HScrollBar<>nil then
              HScrollBar^.HandleEvent(Event);
        else DontClear:=true;
        end;
        if DontClear=false then ClearEvent(Event);
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

function TSymbolReferenceView.GetText(Item,MaxLen: Sw_Integer): String;
var S: string;
    P: PReference;
begin
  P:=References^.At(Item);
  S:=P^.GetFileName+'('+IntToStr(P^.Position.Y)+')';
  GetText:=copy(S,1,MaxLen);
end;

procedure TSymbolReferenceView.SelectItem(Item: Sw_Integer);
begin
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
    Names: string[3] = 'SRI';
begin
  NormColor:=GetColor(1); SelColor:=GetColor(2);
  MoveChar(B,'Ä',SelColor,Size.X);
  CurX:=0; Count:=0;
  for I:=0 to 2 do
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

constructor TBrowserWindow.Init(var Bounds: TRect; ATitle: TTitleStr; ANumber: Sw_Integer;
             const AName: string; ASymbols: PSymbolCollection; AReferences: PReferenceCollection);
var R: TRect;
    ST: PStaticText;
    HSB,VSB: PScrollBar;
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
        cmSearchWindow+1..cmSearchWindow+100 :
          if (Event.Command-cmSearchWindow=Number) then
              ClearEvent(Event);
        cmListItemSelected :
          if Event.InfoPtr=ScopeView then
            begin
              S:=ScopeView^.Symbols^.At(ScopeView^.Focused);
              MakeGlobal(ScopeView^.Origin,P);
              Desktop^.MakeLocal(P,P); Inc(P.Y,ScopeView^.Focused-ScopeView^.TopItem);
              Inc(P.Y);
              OpenSymbolBrowser(Origin.X-1,P.Y,
                S^.GetName,
                ScopeView^.GetText(ScopeView^.Focused,255),
                S^.Items,S^.References);
            end else
          if Event.InfoPtr=ReferenceView then
            begin
            end;
      end;
    evKeyDown :
      begin
        DontClear:=false;
        case Event.KeyCode of
          kbEsc :
            Close;
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

procedure TBrowserWindow.SelectTab(BrowserTab: Sw_integer);
var Tabs: Sw_integer;
begin
  case BrowserTab of
    btScope :
      if assigned(ScopeView) then
        ScopeView^.Select;
    btReferences :
      if assigned(ReferenceView) then
        ReferenceView^.Select;
  end;
  Tabs:=0;
  if assigned(ScopeView) then
    Tabs:=Tabs or btScope;
  if assigned(ReferenceView) then
    Tabs:=Tabs or btReferences;
  if (Tabs and BrowserTab)=0 then
    if (Tabs and btScope)<>0 then BrowserTab:=btScope else
    if (Tabs and btReferences)<>0 then BrowserTab:=btReferences else
      BrowserTab:=btInheritance;
  if PageTab<>nil then PageTab^.SetParams(Tabs,BrowserTab);
end;

function TBrowserWindow.GetPalette: PPalette;
const S: string[length(CBrowserWindow)] = CBrowserWindow;
begin
  GetPalette:=@S;
end;

procedure OpenSymbolBrowser(X,Y: Sw_integer;const Name,Line: string;
            Symbols: PSymbolCollection; References: PReferenceCollection);
var R: TRect;
begin
  if X=0 then X:=Desktop^.Size.X-35;
  R.A.X:=X; R.A.Y:=Y;
  R.B.X:=R.A.X+35; R.B.Y:=R.A.Y+15;
  while (R.B.Y>Desktop^.Size.Y) do R.Move(0,-1);
  Desktop^.Insert(New(PBrowserWindow, Init(R,
    'Browse: '+Name,SearchFreeWindowNo,Line,Symbols,References)));
end;


END.
{
  $Log$
  Revision 1.1  1999-01-12 14:29:40  peter
    + Implemented still missing 'switch' entries in Options menu
    + Pressing Ctrl-B sets ASCII mode in editor, after which keypresses (even
      ones with ASCII < 32 ; entered with Alt+<###>) are interpreted always as
      ASCII chars and inserted directly in the text.
    + Added symbol browser
    * splitted fp.pas to fpide.pas

  Revision 1.0  1999/01/09 11:49:41  gabor
     Original implementation
}
