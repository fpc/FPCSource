{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown ANSI terminal renderer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.ANSIRender;

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, System.Contnrs, System.Console.Ansi,
{$ELSE}
  Classes, SysUtils, Contnrs, FPAnsi,
{$ENDIF}
  Markdown.Elements,
  Markdown.Render,
  Markdown.HTMLEntities;

type
  // Style flags collected for a text run.
  TRunStyle = record
    Bold, Italic, Underline, Strike, Inverse, Faint : Boolean;
    Fg : Integer;     // 256-color code, or -1 for the terminal default
    Href : string;    // when set (and Hyperlinks is on) emitted as an OSC 8 link
  end;

  TLinePrefix = record
    Vis : Integer;    // visible width of the prefix
    Styled : string;  // styled text emitted at the start of every line
  end;

  { TMarkDownANSIRenderer }

  TMarkDownANSIRenderer = class(TMarkdownRenderer)
  private
    FOutput : TStrings;
    FResult : TStringList; 
    FWidth : Integer;
    FUseColor : Boolean;
    FHyperlinks : Boolean;
    // line layout state
    FLine : string;
    FCol : Integer;
    FLineStartCol : Integer; 
    FStarted : Boolean;   
    FPendingSpace : Boolean;
    // line-start prefixes (indents, quote bars, ...)
    FPrefix : array of TLinePrefix;
    FMarker : string;
    FMarkerVis : Integer;
    // inherited inline style (set by heading/quote block renderers)
    FBaseStyle : TRunStyle;
    FInListItem : Boolean; 
    FHTMLEntities : TFPStringHashTable;
    procedure CollectEntities;
    procedure StartLine;
    procedure PlaceStyled(const aVisible, aStyled : string);
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    // Rendering
    procedure RenderDocument(aDocument : TMarkdownDocument); override; overload;
    procedure RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings); overload;
    function RenderToString(aDocument : TMarkdownDocument) : string;
    // Helpers used by the block/text renderers
    function VisibleWidth(const aText : string) : Integer;
    function ResolveEntities(const aText : string) : string;
    function StyleText(const aText : string; const aStyle : TRunStyle) : string;
    function DefaultStyle : TRunStyle;
    // Writer primitives
    procedure NewLine;
    procedure FlushLine;
    procedure BlankLine;
    procedure AddRun(const aText : string; const aStyle : TRunStyle; aWordWrap : Boolean);
    procedure AddRaw(const aVisible, aStyled : string);
    procedure PushPrefix(aVis : Integer; const aStyled : string);
    procedure PopPrefix;
    procedure SetMarker(const aStyled : string; aVis : Integer);
    function PrefixWidth : Integer;
    procedure RenderInline(aNode : TMarkdownTextNode);
    // Theme/state accessed by block renderers
    property BaseStyle : TRunStyle read FBaseStyle write FBaseStyle;
    property InListItem : Boolean read FInListItem write FInListItem;
    property Output : TStrings read FOutput;
    // Result of the parameterless RenderDocument
    property Lines : TStringList read FResult;
  published
    property Width : Integer read FWidth write FWidth;
    property UseColor : Boolean read FUseColor write FUseColor;
    property Hyperlinks : Boolean read FHyperlinks write FHyperlinks;
  end;

  { TANSIBlockRenderer }

  TANSIBlockRenderer = class(TMarkdownBlockRenderer)
  protected
    function ANSI : TMarkDownANSIRenderer; inline;
  end;

implementation

const
  // 256-color palette codes (0..15 = the terminal's themed 16 colors)
  clHeading = 12; // bright blue
  clLink    = 14; // bright cyan
  clCodeInl = 11; // bright yellow
  clCodeBlk = 10; // bright green
  clBullet  = 4;  // blue
  clQuote   = 2;  // green
  clRule    = 8;  // bright black / grey

type
  { Block renderers }

  TANSIParagraphRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSITextBlockRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIHeadingRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIQuoteRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIListRenderer = class(TANSIBlockRenderer)
  private
    FItemNumber : Integer;
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    property ItemNumber : Integer read FItemNumber;
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIListItemRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSICodeRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSIThematicBreakRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

  TANSITableRenderer = class(TANSIBlockRenderer)
  protected
    procedure DoRender(aBlock : TMarkdownBlock); override;
  public
    class function BlockClass : TMarkdownBlockClass; override;
  end;

function Spaces(aCount : Integer) : string;
begin
  if aCount<=0 then
    Result:=''
  else
    Result:=StringOfChar(' ',aCount);
end;

{ TMarkDownANSIRenderer }

constructor TMarkDownANSIRenderer.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  FWidth:=80;
  FUseColor:=True;
  FHyperlinks:=True;
  FHTMLEntities:=TFPStringHashTable.Create;
  FResult:=TStringList.Create;
  FBaseStyle:=DefaultStyle;
end;


destructor TMarkDownANSIRenderer.Destroy;
begin
  FreeAndNil(FResult);
  FreeAndNil(FHTMLEntities);
  inherited Destroy;
end;


function TMarkDownANSIRenderer.DefaultStyle : TRunStyle;
begin
  Result:=Default(TRunStyle);
  Result.Fg:=-1;
end;


function TMarkDownANSIRenderer.VisibleWidth(const aText : string) : Integer;
var
  i : Integer;
begin
  // Count UTF-8 codepoints (ignore continuation bytes). ANSI escapes are never
  // passed here; callers always measure the plain visible text.
  Result:=0;
  for i:=1 to Length(aText) do
    if (Ord(aText[i]) and $C0)<>$80 then
      Inc(Result);
end;


procedure TMarkDownANSIRenderer.CollectEntities;
var
  Ent : THTMLEntityDef;
  lKey : string;
begin
  for Ent in EntityDefList do
    begin
    lKey:=LowerCase(Ent.e);
    if FHTMLEntities.Items[lKey]='' then
      FHTMLEntities.Add(lKey,Utf8Encode(Ent.u));
    end;
end;


function TMarkDownANSIRenderer.ResolveEntities(const aText : string) : string;

  procedure CopyToResult(aEnd, aStart : Integer);
  begin
    Result:=Result+Copy(aText,aStart,aEnd-aStart+1);
  end;

var
  lEnd, lPrev, lNext, lUnicode : Integer;
  lUChar : UnicodeChar;
  lEnt, lUTF8 : string;
begin
  if FHTMLEntities.Count=0 then
    CollectEntities;
  lPrev:=1;
  lNext:=Pos('&',aText,1);
  if lNext=0 then
    Exit(aText);
  Result:='';
  while lNext>0 do
    begin
    lUTF8:='';
    CopyToResult(lNext-1,lPrev);
    lEnd:=Pos(';',aText,lNext+1);
    if lEnd=0 then
      begin
      Result:=Result+'&';
      lPrev:=lNext+1;
      lNext:=Pos('&',aText,lPrev);
      Continue;
      end;
    lEnt:=Copy(aText,lNext+1,lEnd-lNext-1);
    if lEnt<>'' then
      begin
      if lEnt[1]<>'#' then
        lUTF8:=FHTMLEntities.Items[LowerCase(lEnt)]
      else if TryStrToInt(StringReplace(Copy(lEnt,2,Length(lEnt)-1),'x','$',[rfIgnoreCase]),lUnicode) then
        begin
        if (lUnicode<=0) or (lUnicode>$FFFF) then
          lUChar:=#$FFFD
        else
          lUChar:=UnicodeChar(lUnicode);
        lUTF8:=Utf8Encode(lUChar);
        end;
      end;
    if lUTF8='' then
      Result:=Result+'&'+lEnt+';'
    else
      Result:=Result+lUTF8;
    lPrev:=lEnd+1;
    lNext:=Pos('&',aText,lPrev);
    end;
  CopyToResult(Length(aText),lPrev);
end;


function TMarkDownANSIRenderer.StyleText(const aText : string; const aStyle : TRunStyle) : string;
var
  a : TAnsi;
  lVis : string;
begin
  lVis:=aText;
  if (aStyle.Href<>'') and FHyperlinks then
    lVis:=TAnsi.Hyperlink(aStyle.Href,aText);
  if not FUseColor then
    Exit(lVis);
  if not (aStyle.Bold or aStyle.Italic or aStyle.Underline or aStyle.Strike
          or aStyle.Inverse or aStyle.Faint) and (aStyle.Fg<0) then
    Exit(lVis);
  a:=lVis;
  if aStyle.Bold then a:=a.Bold;
  if aStyle.Faint then a:=a.Faint;
  if aStyle.Italic then a:=a.Italic;
  if aStyle.Underline then a:=a.Underline;
  if aStyle.Inverse then a:=a.Inverse;
  if aStyle.Strike then a:=a.Strikethrough;
  if aStyle.Fg>=0 then a:=a.Fg(aStyle.Fg);
  Result:=a.ToString;
end;


function TMarkDownANSIRenderer.PrefixWidth : Integer;
var
  i : Integer;
begin
  Result:=0;
  for i:=0 to High(FPrefix) do
    Inc(Result,FPrefix[i].Vis);
end;


procedure TMarkDownANSIRenderer.StartLine;
var
  i : Integer;
begin
  FLine:='';
  FCol:=0;
  for i:=0 to High(FPrefix) do
    if (i=High(FPrefix)) and (FMarker<>'') then
      begin
      FLine:=FLine+FMarker;
      Inc(FCol,FMarkerVis);
      end
    else
      begin
      FLine:=FLine+FPrefix[i].Styled;
      Inc(FCol,FPrefix[i].Vis);
      end;
  FMarker:='';
  FLineStartCol:=FCol;
  FStarted:=True;
end;


procedure TMarkDownANSIRenderer.NewLine;
begin
  if not FStarted then
    StartLine;
  FOutput.Add(FLine);
  FLine:='';
  FCol:=0;
  FStarted:=False;
  FPendingSpace:=False;
end;


procedure TMarkDownANSIRenderer.FlushLine;
begin
  if FStarted then
    NewLine;
end;


procedure TMarkDownANSIRenderer.BlankLine;
begin
  FlushLine;
  if (FOutput.Count>0) and (FOutput[FOutput.Count-1]<>'') then
    FOutput.Add('');
end;


procedure TMarkDownANSIRenderer.PlaceStyled(const aVisible, aStyled : string);
var
  w : Integer;
begin
  w:=VisibleWidth(aVisible);
  if not FStarted then
    StartLine;
  if FCol>FLineStartCol then
    begin
    if FPendingSpace then
      begin
      if FCol+1+w>FWidth then
        begin NewLine; StartLine; end
      else
        begin FLine:=FLine+' '; Inc(FCol); end;
      end
    else if FCol+w>FWidth then
      begin NewLine; StartLine; end;
    end;
  FLine:=FLine+aStyled;
  Inc(FCol,w);
  FPendingSpace:=False;
end;


procedure TMarkDownANSIRenderer.AddRun(const aText : string; const aStyle : TRunStyle; aWordWrap : Boolean);
var
  i : Integer;
  c : Char;
  lWord : string;
begin
  if aText='' then
    exit;
  if not aWordWrap then
    begin
    PlaceStyled(aText,StyleText(aText,aStyle));
    exit;
    end;
  lWord:='';
  for i:=1 to Length(aText) do
    begin
    c:=aText[i];
    if (c=' ') or (c=#9) or (c=#10) or (c=#13) then
      begin
      if lWord<>'' then
        begin
        PlaceStyled(lWord,StyleText(lWord,aStyle));
        lWord:='';
        end;
      FPendingSpace:=True;
      end
    else
      lWord:=lWord+c;
    end;
  if lWord<>'' then
    PlaceStyled(lWord,StyleText(lWord,aStyle));
end;


procedure TMarkDownANSIRenderer.AddRaw(const aVisible, aStyled : string);
begin
  if not FStarted then
    StartLine;
  FLine:=FLine+aStyled;
  Inc(FCol,VisibleWidth(aVisible));
end;


procedure TMarkDownANSIRenderer.PushPrefix(aVis : Integer; const aStyled : string);
begin
  SetLength(FPrefix,Length(FPrefix)+1);
  FPrefix[High(FPrefix)].Vis:=aVis;
  FPrefix[High(FPrefix)].Styled:=aStyled;
end;


procedure TMarkDownANSIRenderer.PopPrefix;
begin
  if Length(FPrefix)>0 then
    SetLength(FPrefix,Length(FPrefix)-1);
end;


procedure TMarkDownANSIRenderer.SetMarker(const aStyled : string; aVis : Integer);
begin
  FMarker:=aStyled;
  FMarkerVis:=aVis;
end;


procedure TMarkDownANSIRenderer.RenderInline(aNode : TMarkdownTextNode);
var
  lStyle : TRunStyle;
  lText : string;
begin
  if not assigned(aNode) then
    exit;
  lStyle:=FBaseStyle;
  if nsStrong in aNode.Styles then lStyle.Bold:=True;
  if nsEmph in aNode.Styles then lStyle.Italic:=True;
  if nsDelete in aNode.Styles then lStyle.Strike:=True;
  case aNode.Kind of
    nkText:
      AddRun(ResolveEntities(aNode.NodeText),lStyle,True);
    nkCode:
      begin
      lStyle.Fg:=clCodeInl;
      AddRun(ResolveEntities(aNode.NodeText),lStyle,False);
      end;
    nkURI,nkEmail:
      begin
      lStyle.Underline:=True;
      lStyle.Fg:=clLink;
      lStyle.Href:=aNode.Attrs['href'];
      AddRun(ResolveEntities(aNode.NodeText),lStyle,False);
      end;
    nkImg:
      begin
      lText:=ResolveEntities(aNode.Attrs['alt']);
      if lText='' then
        lText:=aNode.Attrs['src'];
      lStyle.Faint:=True;
      AddRun('[image: '+lText+']',lStyle,True);
      end;
    nkLineBreak:
      NewLine;
  end;
end;


procedure TMarkDownANSIRenderer.RenderDocument(aDocument : TMarkdownDocument; aDest : TStrings);
begin
  FOutput:=aDest;
  SetLength(FPrefix,0);
  FMarker:='';
  FLine:='';
  FCol:=0;
  FStarted:=False;
  FPendingSpace:=False;
  FBaseStyle:=DefaultStyle;
  RenderChildren(aDocument);
  FlushLine;
  // drop a trailing blank line if present
  while (FOutput.Count>0) and (FOutput[FOutput.Count-1]='') do
    FOutput.Delete(FOutput.Count-1);
end;


procedure TMarkDownANSIRenderer.RenderDocument(aDocument : TMarkdownDocument);
begin
  FResult.Clear;
  RenderDocument(aDocument,FResult);
end;


function TMarkDownANSIRenderer.RenderToString(aDocument : TMarkdownDocument) : string;
var
  L : TStringList;
begin
  L:=TStringList.Create;
  try
    RenderDocument(aDocument,L);
    Result:=L.Text;
  finally
    L.Free;
  end;
end;


{ TANSIBlockRenderer }

function TANSIBlockRenderer.ANSI : TMarkDownANSIRenderer;
begin
  Result:=TMarkDownANSIRenderer(Renderer);
end;


{ TANSIParagraphRenderer }

procedure TANSIParagraphRenderer.DoRender(aBlock : TMarkdownBlock);
begin
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.FlushLine;
  if not ANSI.InListItem then
    ANSI.BlankLine;
end;

class function TANSIParagraphRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownParagraphBlock;
end;


{ TANSITextBlockRenderer }

procedure TANSITextBlockRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lTextBlock : TMarkdownTextBlock;
  i : Integer;
begin
  if not (aBlock is TMarkdownTextBlock) then
    exit;
  lTextBlock:=TMarkdownTextBlock(aBlock);
  if not assigned(lTextBlock.Nodes) then
    exit;
  for i:=0 to lTextBlock.Nodes.Count-1 do
    ANSI.RenderInline(lTextBlock.Nodes[i]);
end;

class function TANSITextBlockRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownTextBlock;
end;


{ TANSIHeadingRenderer }

procedure TANSIHeadingRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lSaved, lStyle : TRunStyle;
begin
  ANSI.BlankLine;
  lSaved:=ANSI.BaseStyle;
  lStyle:=lSaved;
  lStyle.Bold:=True;
  lStyle.Fg:=clHeading;
  ANSI.BaseStyle:=lStyle;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.BaseStyle:=lSaved;
  ANSI.FlushLine;
  ANSI.BlankLine;
end;

class function TANSIHeadingRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownHeadingBlock;
end;


{ TANSIQuoteRenderer }

procedure TANSIQuoteRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lSaved : TRunStyle;
  lBarStyle : TRunStyle;
begin
  ANSI.BlankLine;
  lBarStyle:=ANSI.DefaultStyle;
  lBarStyle.Fg:=clQuote;
  ANSI.PushPrefix(2,ANSI.StyleText('| ',lBarStyle));
  lSaved:=ANSI.BaseStyle;
  lBarStyle:=lSaved;
  lBarStyle.Faint:=True;
  lBarStyle.Fg:=clQuote;
  ANSI.BaseStyle:=lBarStyle;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.BaseStyle:=lSaved;
  ANSI.PopPrefix;
  ANSI.BlankLine;
end;

class function TANSIQuoteRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownQuoteBlock;
end;


{ TANSIListRenderer }

procedure TANSIListRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lList : TMarkdownListBlock;
  i : Integer;
begin
  lList:=aBlock as TMarkdownListBlock;
  ANSI.BlankLine;
  FItemNumber:=0;
  for i:=0 to aBlock.ChildCount-1 do
    begin
    if lList.Ordered then
      FItemNumber:=lList.Start+i;
    Renderer.RenderBlock(aBlock.Children[i]);
    end;
  ANSI.BlankLine;
end;

class function TANSIListRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownListBlock;
end;


{ TANSIListItemRenderer }

procedure TANSIListItemRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lList : TANSIListRenderer;
  lMarker : string;
  lMarkerVis : Integer;
  lStyle : TRunStyle;
  lWasInItem : Boolean;
begin
  lList:=TANSIListRenderer(GetFirstParentWithClass(TANSIListRenderer));
  if Assigned(lList) and (lList.ItemNumber>0) then
    lMarker:=IntToStr(lList.ItemNumber)+'. '
  else
    lMarker:='* ';
  lMarkerVis:=ANSI.VisibleWidth(lMarker);
  lStyle:=ANSI.DefaultStyle;
  lStyle.Fg:=clBullet;
  lStyle.Bold:=True;
  ANSI.PushPrefix(lMarkerVis,Spaces(lMarkerVis));
  ANSI.SetMarker(ANSI.StyleText(lMarker,lStyle),lMarkerVis);
  lWasInItem:=ANSI.InListItem;
  ANSI.InListItem:=True;
  Renderer.RenderChildren(aBlock as TMarkdownContainerBlock);
  ANSI.InListItem:=lWasInItem;
  ANSI.FlushLine;
  ANSI.PopPrefix;
end;

class function TANSIListItemRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownListItemBlock;
end;


{ TANSICodeRenderer }

procedure TANSICodeRenderer.DoRender(aBlock : TMarkdownBlock);
var
  i,j : Integer;
  lChild : TMarkdownBlock;
  lLine : string;
  lStyle : TRunStyle;
begin
  ANSI.BlankLine;
  ANSI.PushPrefix(4,Spaces(4));
  lStyle:=ANSI.DefaultStyle;
  lStyle.Fg:=clCodeBlk;
  for i:=0 to aBlock.ChildCount-1 do
    begin
    lChild:=aBlock.Children[i];
    lLine:='';
    if lChild is TMarkdownTextBlock then
      for j:=0 to TMarkdownTextBlock(lChild).Nodes.Count-1 do
        lLine:=lLine+TMarkdownTextBlock(lChild).Nodes[j].NodeText;
    // verbatim: no wrapping, no entity resolution, exact whitespace
    ANSI.AddRun(lLine,lStyle,False);
    ANSI.NewLine;
    end;
  ANSI.PopPrefix;
  ANSI.BlankLine;
end;

class function TANSICodeRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownCodeBlock;
end;


{ TANSIThematicBreakRenderer }

procedure TANSIThematicBreakRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lStyle : TRunStyle;
  lWidth : Integer;
  lRule : string;
begin
  if aBlock=nil then ;
  ANSI.BlankLine;
  lStyle:=ANSI.DefaultStyle;
  lStyle.Faint:=True;
  lStyle.Fg:=clRule;
  lWidth:=ANSI.Width-ANSI.PrefixWidth;
  if lWidth<1 then
    lWidth:=1;
  lRule:=StringOfChar('-',lWidth);
  ANSI.AddRaw(lRule,ANSI.StyleText(lRule,lStyle));
  ANSI.NewLine;
  ANSI.BlankLine;
end;

class function TANSIThematicBreakRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownThematicBreakBlock;
end;


{ TANSITableRenderer }

procedure TANSITableRenderer.DoRender(aBlock : TMarkdownBlock);
var
  lTable : TMarkdownTableBlock;
  lCols, lRows, r, c : Integer;
  lColW : array of Integer;
  lCellText : array of array of string;
  lRow, lCell : TMarkdownBlock;
  lText, lLine, lSep : string;
  lStyle : TRunStyle;

  function CellText(aCell : TMarkdownBlock) : string;
  var n : Integer;
  begin
    Result:='';
    if aCell is TMarkdownTextBlock then
      for n:=0 to TMarkdownTextBlock(aCell).Nodes.Count-1 do
        Result:=Result+TMarkdownTextBlock(aCell).Nodes[n].NodeText;
    Result:=ANSI.ResolveEntities(Result);
  end;

begin
  lTable:=aBlock as TMarkdownTableBlock;
  lRows:=lTable.ChildCount;
  if lRows=0 then
    exit;
  lCols:=Length(lTable.Columns);
  if lCols=0 then
    exit;
  SetLength(lColW,lCols);
  SetLength(lCellText,lRows,lCols);
  for r:=0 to lRows-1 do
    begin
    lRow:=lTable.Children[r];
    for c:=0 to lCols-1 do
      begin
      if c<lRow.ChildCount then
        lCell:=lRow.Children[c]
      else
        lCell:=nil;
      if lCell<>nil then
        lText:=CellText(lCell)
      else
        lText:='';
      lCellText[r][c]:=lText;
      if ANSI.VisibleWidth(lText)>lColW[c] then
        lColW[c]:=ANSI.VisibleWidth(lText);
      end;
    end;
  ANSI.BlankLine;
  // separator line "+----+----+"
  lSep:='+';
  for c:=0 to lCols-1 do
    lSep:=lSep+StringOfChar('-',lColW[c]+2)+'+';
  ANSI.AddRaw(lSep,lSep);
  ANSI.NewLine;
  for r:=0 to lRows-1 do
    begin
    lStyle:=ANSI.DefaultStyle;
    if r=0 then
      lStyle.Bold:=True; // header row
    lLine:='|';
    for c:=0 to lCols-1 do
      begin
      lText:=lCellText[r][c];
      lText:=lText+Spaces(lColW[c]-ANSI.VisibleWidth(lText));
      lLine:=lLine+' '+ANSI.StyleText(lText,lStyle)+' |';
      end;
    // styled line: pass empty visible (column position is irrelevant before NewLine)
    ANSI.AddRaw('',lLine);
    ANSI.NewLine;
    if r=0 then
      begin
      ANSI.AddRaw(lSep,lSep);
      ANSI.NewLine;
      end;
    end;
  ANSI.AddRaw(lSep,lSep);
  ANSI.NewLine;
  ANSI.BlankLine;
end;

class function TANSITableRenderer.BlockClass : TMarkdownBlockClass;
begin
  Result:=TMarkdownTableBlock;
end;


initialization
  TANSIParagraphRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSITextBlockRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIHeadingRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIQuoteRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIListRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIListItemRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSICodeRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSIThematicBreakRenderer.RegisterRenderer(TMarkDownANSIRenderer);
  TANSITableRenderer.RegisterRenderer(TMarkDownANSIRenderer);
end.
