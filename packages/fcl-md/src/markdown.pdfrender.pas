{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Markdown PDF renderer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  Text Layout routine was inspired on the algorithm found in https://github.com/BeRo1985/pashtmldown
}
unit markdown.pdfrender;
{$mode objfpc}
{$h+}
{$codepage UTF8}
{$modeswitch advancedrecords}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Classes, System.Contnrs, System.Types,MarkDown.Elements, 
  MarkDown.Render, Markdown.HTMLEntities, fpimage, FpPdf.Pdf, FpPdf.Ttf;
{$ELSE}
  SysUtils, Classes, Contnrs, Types, MarkDown.Elements, MarkDown.Render,
  fpimage, Markdown.HTMLEntities, fppdf, fpttf;
{$ENDIF}

const
  clNone = $FFFFFF;
  clInfoText = 0;
  clWindow = 0;
  clWindowText = 0;
  clWindowFrame = 0;
  clHighlight = 0;

type
  TLayoutItemKind = (
    likText,
    likHR,
    likRect,
    likLine,
    likImage,
    likBackground
  );
  TFontStyle = (fsBold,fsItalic,fsUnderline);
  TFontStyles = set of TFontStyle;
  TFontContextItem = (fcMono,fcCode,fcFencedCode,fcQuote,fcHyperLink);
  TFontContext = set of TFontContextItem;


  { TLayoutItem }
  TARGBColorKind = (ckForeground,ckBackground);
  TLayoutItem = class
  private
    FBGColor: TARGBColor;
    FColor: TARGBColor;
    FContext: TFontContext;
    FDeltaX: TPDFFloat;
    FDeltaY: TPDFFloat;
    FKind: TLayoutItemKind;
    FX: TPDFFLoat;
    FY: TPDFFLoat;
    FWidth: TPDFFloat;
    FHeight: TPDFFloat;
    FText: utf8string;
    FFontSize: LongInt;
    FFontStyle: TFontStyles;
    FURL: utf8string;
    FPageIndex: Integer;  // New field to track which page this item belongs to
    FImageIndex: Integer; // index into TPDFDocument.Images for likImage items
  protected
    function GeTARGBColor(aKind : TARGBColorKind; aDefault : TARGBColor) : TARGBColor;
  public
    constructor Create(aKind : TLayoutItemKind; aX,aY : TPDFFloat);
    property Kind: TLayoutItemKind read FKind;
    property X: TPDFFLoat read FX write FX;
    property Y: TPDFFLoat read FY write FY;
    property BGColor : TARGBColor Read FBGColor Write FBGColor;
    property Color : TARGBColor Read FColor Write FColor;
    property DeltaX: TPDFFloat read FDeltaX write FDeltaX;
    property DeltaY: TPDFFloat read FDeltaY write FDeltaY;
    property Width: TPDFFloat read FWidth write FWidth;
    property Height: TPDFFloat read FHeight write FHeight;
    property Text: utF8string read FText write FText;
    property FontSize: LongInt read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property URL : utF8string read FURL write FURL;
    property Context: TFontContext Read FContext Write FContext;
    property PageIndex: Integer read FPageIndex write FPageIndex;  // New property
    property ImageIndex: Integer read FImageIndex write FImageIndex;
  end;

  { TLayoutItemList }

  TLayoutItemList = class(TFPObjectList)
  private
    function GetItem(const aIndex: LongInt): TLayoutItem;
  public
    function NewItem: TLayoutItem;
    function NewItem(aKind: TLayoutItemKind; aX, aY: TPDFFLoat): TLayoutItem;
    property Items[const Index: LongInt]: TLayoutItem read GetItem; default;
  end;

  { TLinkHitRect }

  TLinkHitRect = class
  private
    FX: TPDFFLoat;
    FY: TPDFFLoat;
    FWidth: TPDFFLoat;
    FHeight: TPDFFLoat;
    FHREF: utf8string;
    FPageIndex: Integer;  // New field to track which page this link belongs to
  public
    constructor create (aX,aY,aWidth,aHeight : TPDFFLoat; aHREF : UTF8String; aPageIndex: Integer = 0);
    function HasPoint(aX,aY : Longint) : Boolean;
    property X: TPDFFLoat read FX;
    property Y: TPDFFLoat read FY;
    property Width: TPDFFLoat read FWidth;
    property Height: TPDFFLoat read FHeight;
    property HREF: utf8string read FHREF;
    property PageIndex: Integer read FPageIndex write FPageIndex;  // New property
  end;

  { TLinkHitRectList }

  TLinkHitRectList = class (TFPObjectList)
  private
    function GetItem(const aIndex: LongInt): TLinkHitRect;
  public
    function Add(const aX, aY, aWidth, aHeight: TPDFFLoat; const aHref: utf8string; const aPageIndex: Integer = 0): TLinkHitRect; reintroduce;
    property Items[const Index: LongInt]: TLinkHitRect read GetItem; default;
  end;

  TLayoutData = record
    MaxWidth : TPDFFloat;
    CurrentIndent : TPDFFloat;
    LineX : TPDFFloat;
    LineY : TPDFFloat;
    LineHeight : TPDFFloat;
    LineAboveExtra : TPDFFloat;
    LineBelowExtra : TPDFFloat;
    BaselineShiftCurrent : TPDFFloat;
    BaselineFromIndex : TPDFFloat;
    NeedSpaceBeforeNextText : Boolean;
    // PDF-specific fields
    CurrentPageIndex : Integer;
  end;

  TLayoutLists = record
    Items: TLayoutItemList;
    LinkRects: TLinkHitRectList;
    class function Create : TLayoutLists; Static;
    procedure Clear;
    procedure Free;
  end;

  TMarkdownImageEvent = procedure(Sender : TObject; const aURL : string; var aImage : TFPCustomImage) of object;

  { TMarkDownPDFRenderer }

  TMarkDownPDFRenderer = class(TMarkDownRenderer)
  private
    FCalculatedBlockQuoteIndent: Integer;
    const BulletCount = 3;
  private
    FBlockQuoteIndent: Integer;
    FBottomMargin: TPDFFloat;
    FBulletLevel: Integer;
    FBullets: Array[1..BulletCount] of string;
    FLeftMargin: TPDFFloat;
    FPDFDocument: TPDFDocument;
    FDocument: TMarkDownDocument;
    FHyperLinkColor: TARGBColor;
    FImageMargin: integer;
    FImageBaseDir: string;
    FFallbackFont: TFPFontCacheItem; // cached font used for glyphs the main font lacks
    FOnGetImage: TMarkdownImageEvent;
    FLists : TLayoutLists;

    FLineWidth: TPDFFloat;
    FLayout : TLayoutData;
    FParagraphSpacing: LongInt;
    FExtraIndent: LongInt;
    FFontName: utf8string;
    FMonoFontName: utf8string;
    FBaseFontSize: LongInt;
    FRightMargin: TPDFFloat;
    FTargetDPI: LongInt;

    // Colors
    FBGCodeColor: TARGBColor;
    FFontCodeColor: TARGBColor;
    FBGMarkColor: TARGBColor;
    FFontMarkColor: TARGBColor;
    FBGColor: TARGBColor;
    FFonTARGBColor: TARGBColor;
    FFontQuoteColor: TARGBColor;
    FNextLineIndent : Integer;
    FSuppressParagraphBreak : Boolean;
    FImageCache : TFPObjectHashTable;
    FHTMLEntities : TFPStringHashTable;

    // PDF-specific fields
    FCurrentPage: TPDFPage;
    FPageList: TList;  // List of TPDFPage objects
    FTopMargin: TPDFFloat;
  Protected

    // Layout management
    procedure Clear;
    procedure BeginLayout;
    procedure EndLayout;
    function GetBulletChar(AIndex: Integer): string;
    procedure NewLine;
    procedure ParagraphBreak;
    procedure MaybeStartParagraph;
    // Keep the next block on the current line (used to put list item text next to the bullet)
    procedure SuppressNextParagraphBreak;
    function SwapLayout(const aData : TLayoutData) : TLayoutData;
    function SwapLayoutLists(const aData : TLayoutLists) : TLayoutLists;
    procedure SetCurrentY(aValue: TPDFFloat);

    // PDF-specific page management
    procedure CheckPageBreak(const aHeight: TPDFFloat);
    procedure CreateNewPage;
    function GetCurrentPageY: TPDFFLoat;
    function GetEffectivePageHeight: TPDFFLoat;

    // Font and measurement
    function DIP(const aValue: LongInt): LongInt;
    // Resolve the font for a run, falling back to a font that covers the text's
    // glyphs when the configured font does not (e.g. CJK characters).
    function ResolveFontForText(const aText: utf8string; const aFontName: string;
      const aFontStyle: TFontStyles): TFPFontCacheItem;
    procedure ApplyFont(aPage: TPDFPage; const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext);
    procedure MeasureText(const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext; out aWidth, aHeight: LongInt);
    function MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
    function MeasureTextHeight(const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext : TFontContext): LongInt;
    function GetTexTARGBColor(aContext: TFontContext): TARGBColor; virtual;
    function GetTextBGColor(aContext: TFontContext): TARGBColor; virtual;

    // Text layout
    procedure CollectHTMLEntities;
    function ResolveEntities(const aText: String): String; virtual;
    procedure FlushTextRun(const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
      const aDryRun: boolean); virtual;
    procedure LayoutTextWrapped(const aText: utf8string; const aFontSize: LongInt;
      const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
      const aPreserveWhitespace, aDryRun: boolean); virtual;
    function LayoutImage(aImageURL: UTF8String): Boolean; virtual;
    procedure RenderTextNode(aTextNode: TMarkDownTextNode; aFontSize: LongInt; aFontStyle: TFontStyles; const aContext : TFontContext);

    // Actual drawing to PDF
    procedure DrawLayoutItem(aItem: TLayoutItem);
    procedure DrawLayout;

    // Utility
    function GetNodeFontStyle(aTextNode: TMarkDownTextNode): TFontStyles;
    // indent
    Procedure Indent(aSize : integer);
    procedure SetBulletChar(AIndex: Integer; AValue: string);
    Procedure Undent(aSize : integer);
    procedure IncBulletLevel;
    Procedure DecBulletLevel;
    function GetBullet : String;
    function GetBulletAt(aLevel : integer) : string;
    function CreateLayoutItem(aKind : TLayoutItemKind; aX,aY : TPDFFloat) : TLayoutItem; inline;
    Property BulletLevel : Integer Read FBulletLevel;
    Property CalculatedBlockQuoteIndent : Integer Read FCalculatedBlockQuoteIndent;
    Property Layout : TLayoutData read FLayout;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Rendering interface. The markdown document is supplied by the caller (who
    // retains ownership) and rendered into aPDFDocument.
    procedure RenderDocument(aDocument: TMarkDownDocument; aPDFDocument: TPDFDocument);

    function HitTestLink(const aX, aY: LongInt; const aPageIndex: Integer; out aHref: utf8string): boolean;

    // Properties
    Property Document : TMarkdownDocument Read FDocument;
    property TargetDPI: LongInt read FTargetDPI write FTargetDPI;
    property FontName: utf8string read FFontName write FFontName;
    property MonoFontName: utf8string read FMonoFontName write FMonoFontName;

    property FontHyperLinkColor: TARGBColor read FHyperLinkColor write FHyperLinkColor;
    property BGCodeColor: TARGBColor read FBGCodeColor write FBGCodeColor;
    property FontCodeColor: TARGBColor read FFontCodeColor write FFontCodeColor;
    property BGMarkColor: TARGBColor read FBGMarkColor write FBGMarkColor;
    property FontMarkColor: TARGBColor read FFontMarkColor write FFontMarkColor;

    property BGColor: TARGBColor read FBGColor write FBGColor;
    property FonTARGBColor: TARGBColor read FFonTARGBColor write FFonTARGBColor;
    property FontQuoteColor: TARGBColor read FFontQuoteColor write FFontQuoteColor;
    property BaseFontSize: LongInt read FBaseFontSize write FBaseFontSize;
    property BulletChar1 : string Index 1 Read GetBulletChar Write SetBulletChar;
    property BulletChar2 : string Index 2 read GetBulletChar Write SetBulletChar;
    property BulletChar3 : string Index 3 read GetBulletChar Write SetBulletChar;
    Property BlockQuoteIndent : Integer Read FBlockQuoteIndent Write FBlockQuoteIndent;
    Property ParagraphSpacing : Integer Read FParagraphSpacing Write FParagraphSpacing;
    Property ExtraIndent : Integer Read FExtraIndent Write FExtraIndent;
    Property ImageMargin : integer read FImageMargin Write FImageMargin;
    // Base directory used to resolve relative image URLs
    Property ImageBaseDir : string read FImageBaseDir Write FImageBaseDir;
    Property OnGetImage : TMarkdownImageEvent Read FOnGetImage Write FOnGetImage;

    // PDF-specific properties
    property PDFDocument: TPDFDocument read FPDFDocument write FPDFDocument;
    property PageList: TList read FPageList;
    property TopMargin : TPDFFloat read FTopMargin Write FTopMargin;
    property BottomMargin : TPDFFloat read FBottomMargin Write FBottomMargin;
    property LeftMargin : TPDFFloat read FLeftMargin Write FLeftMargin;
    property RightMargin : TPDFFloat Read FRightMargin Write FRightMargin;

  end;

  { TPDFMarkDownBlockRenderer }

  TPDFMarkDownBlockRenderer = class(TMarkDownBlockRenderer)
  private
    function GetPDFRenderer: TMarkDownPDFRenderer;
  protected
    // Helper methods for PDF rendering
    function DIP(aSize : integer) : Integer;
    procedure Indent(aSize : Integer);
    procedure Undent(aSize : Integer);
    function LayoutData : TLayoutData;
    function CreateLayoutItem(aKind : TLayoutItemKind; aX,aY : TPDFFloat) : TLayoutItem; inline;
    function MeasureTextHeight(const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
    function MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      aFontContext: TFontContext): LongInt;
    procedure LayoutText(const aText: utf8string; const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string;
      const aContext: TFontContext); inline;
    procedure SetCurrentY(aValue: TPDFFloat);
    function GetCurrentY : TPDFFloat;
    procedure NewLine; inline;
    procedure ParagraphBreak; inline;
    procedure MaybeStartParagraph; inline;
  public
    property PDFRenderer: TMarkDownPDFRenderer read GetPDFRenderer;
  end;

  TPDFMarkDownBlockRendererClass = class of TPDFMarkDownBlockRenderer;

  { TPDFMarkDownTextRenderer }

  TPDFMarkDownTextRenderer = class(TMarkDownTextRenderer)
  private
    function GetPDFRenderer: TMarkDownPDFRenderer;
  protected
    procedure DoRender(aElement: TMarkDownTextNode); override;
  public
    property PDFRenderer: TMarkDownPDFRenderer read GetPDFRenderer;
  end;

  { Individual PDF Block Renderers }

  { TPDFMarkDownParagraphBlockRenderer }
  TPDFMarkDownParagraphBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownHeadingBlockRenderer }
  TPDFMarkDownHeadingBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    fontSize: LongInt;
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownQuoteBlockRenderer }
  TPDFMarkDownQuoteBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownListBlockRenderer }
  TPDFMarkDownListBlockRenderer = class(TPDFMarkDownBlockRenderer)
  Private
    FItemNumber : Integer;
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
    property ItemNumber : Integer Read FItemNumber;
  public
    procedure reset; override;
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownListItemBlockRenderer }
  TPDFMarkDownListItemBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownCodeBlockRenderer }
  TPDFMarkDownCodeBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownThematicBreakBlockRenderer }
  TPDFMarkDownThematicBreakBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownTextBlockRenderer }
  TPDFMarkDownTextBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownTableBlockRenderer }
  TPDFMarkDownTableBlockRenderer = class(TPDFMarkDownBlockRenderer)
  protected
    Type
      TColData = record
        Width, Min, Pref : LongInt;
      end;
      TColDataArray = array of TColData;
      TLongIntArray = array of LongInt;
  private
    FColLayout : TColDataArray;
    FAvailableWidth : Integer;
    FGridSize : Integer;
    FCellPadding : Integer;
    FStartX, FStartY : Integer;
    FRowHeights : TLongIntArray;
    FCurrentRow : Integer;
    procedure CalcRowHeights(aTable: TMarkDownTableBlock; const aFontSize: LongInt; const aFontStyle: TFontStyles;
      const aContext: TFontContext);
    procedure DistributeColumns(const aAvailable: LongInt);
    procedure DrawBorders;
    procedure GetCellTexts(aCell: TMarkDownBlock; aTexts: TStrings);
    function GetTotalColumns: Integer;
    function GetTotalHeight: Integer;
    procedure MeasureTableColumns(const aTableModel: TMarkDownTableBlock);
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
    // Column layout, calculated at start of DoRender
    property ColLayout : TColDataArray read FColLayout;
    // Number of columns
    property TotalColumns : Integer read GetTotalColumns;
    // Row heights, calculated at start of DoRender
    property RowHeights : TLongIntArray read FRowHeights;
    // Row currently being rendered
    property CurrentRow : Integer read FCurrentRow;
    // Width of the grid lines between cells
    property GridSize : Integer read FGridSize;
    // Padding inside each cell
    property CellPadding : Integer read FCellPadding;
    // Total table width, calculated in DoRender
    property AvailableWidth : Integer read FAvailableWidth;
    // Sum of all row heights
    property TotalHeight : Integer read GetTotalHeight;
  public
    procedure Reset; override;
    class function BlockClass: TMarkDownBlockClass; override;
  end;

  { TPDFMarkDownTableRowBlockRenderer }
  TPDFMarkDownTableRowBlockRenderer = class(TPDFMarkDownBlockRenderer)
  private
    FTableRenderer : TPDFMarkDownTableBlockRenderer;
    procedure GetTableRenderer;
  protected
    procedure DoRender(aBlock: TMarkDownBlock); override;
    // The enclosing table renderer, located through the parent renderer chain
    property TableRenderer : TPDFMarkDownTableBlockRenderer read FTableRenderer;
  public
    class function BlockClass: TMarkDownBlockClass; override;
  end;

implementation

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Math;
  {$ELSE}
  Math;
  {$ENDIF}

const
  ExtendHeight = UTF8String('Áy');
  cPDFPointsPerInch = 72; // PDF user space is 72 points per inch by definition
  cLineSpacingFactor = 1.2; // baseline-to-baseline distance as a multiple of the font size

function RGBToColor(aRed,aGreen,aBlue : Byte) : TARGBColor;

begin
  Result:=aBlue or (aGreen shl 8) or (aRed shl 16);
end;

function GetFontCacheItem(const aFontName: string; const aFontStyle: TFontStyles): TFPFontCacheItem;

  function TryFind(const aName: string; var aResult: TFPFontCacheItem; aBold, aItalic: Boolean): Boolean;
  begin
    if aResult=nil then
      aResult := gTTFontCache.Find(aName, aBold, aItalic);
    Result := aResult<>nil;
  end;

var
  IsBold, IsItalic, WantMono: Boolean;
  lFamilyMatch: string;
  i: Integer;
  lItem: TFPFontCacheItem;
begin
  IsBold := fsBold in aFontStyle;
  IsItalic := fsItalic in aFontStyle;
  WantMono := Pos('mono', LowerCase(aFontName))>0;
  Result := nil;
  // Try the requested font first, then a series of common fallbacks. Note that
  // names like 'Sans Serif'/'Monospace' rarely resolve on Linux, so we must
  // always end up at a real TrueType font: its metrics are used both to lay out
  // (MeasureText) and to render (ApplyFont), and they only match when identical.
  TryFind(aFontName, Result, IsBold, IsItalic);
  if WantMono then
    begin
    TryFind('Liberation Mono', Result, IsBold, IsItalic);
    TryFind('DejaVu Sans Mono', Result, IsBold, IsItalic);
    TryFind('Courier New', Result, IsBold, IsItalic);
    lFamilyMatch := 'mono';
    end
  else
    begin
    TryFind('Arial', Result, IsBold, IsItalic);
    TryFind('Liberation Sans', Result, IsBold, IsItalic);
    TryFind('DejaVu Sans', Result, IsBold, IsItalic);
    lFamilyMatch := 'sans';
    end;
  // Last resort: any cached font of the right kind matching the requested style
  if Result=nil then
    for i := 0 to gTTFontCache.Count-1 do
      begin
      lItem := gTTFontCache.Items[i];
      if (Pos(lFamilyMatch, LowerCase(lItem.FamilyName))>0)
         and (lItem.IsBold=IsBold) and (lItem.IsItalic=IsItalic) then
        begin
        Result := lItem;
        Break;
        end;
      end;
end;


// True if aFont has a glyph for every non-ASCII codepoint in aText
function FontCoversText(aFont: TFPFontCacheItem; const aText: utf8string): Boolean;
var
  lUni: UnicodeString;
  i: Integer;
begin
  Result := True;
  if aFont=nil then
    Exit(False);
  lUni := UTF8Decode(aText);
  for i := 1 to Length(lUni) do
    if Ord(lUni[i]) > 127 then
      if aFont.FontData.GetGlyphIndex(Ord(lUni[i]))=0 then
        Exit(False);
end;


// Split aText into consecutive pieces that are each either fully covered or fully
// not covered by aBaseFont, so each piece can be drawn with a single font (the base
// font, or a fallback for glyphs the base lacks, e.g. CJK).
function SplitByCoverage(const aText: utf8string; aBaseFont: TFPFontCacheItem): TStringDynArray;
var
  lUni: UnicodeString;
  i, lStart: Integer;
  lCur, lCovered: Boolean;

  function CoveredAt(aIndex: Integer): Boolean;
  begin
    if Ord(lUni[aIndex]) <= 127 then
      Result := True // the base font is always assumed to provide ASCII
    else
      Result := (aBaseFont<>nil) and (aBaseFont.FontData.GetGlyphIndex(Ord(lUni[aIndex]))<>0);
  end;

begin
  Result := nil;
  lUni := UTF8Decode(aText);
  if lUni='' then
    exit;
  lStart := 1;
  lCur := CoveredAt(1);
  for i := 2 to Length(lUni) do
    begin
    lCovered := CoveredAt(i);
    if lCovered<>lCur then
      begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := UTF8Encode(Copy(lUni, lStart, i-lStart));
      lStart := i;
      lCur := lCovered;
      end;
    end;
  SetLength(Result, Length(Result)+1);
  Result[High(Result)] := UTF8Encode(Copy(lUni, lStart, Length(lUni)-lStart+1));
end;

{ TPDFMarkDownBlockRenderer }

function TPDFMarkDownBlockRenderer.GetPDFRenderer: TMarkDownPDFRenderer;
begin
  Result := TMarkDownPDFRenderer(Renderer);
end;

function TPDFMarkDownBlockRenderer.DIP(aSize: integer): Integer;
begin
  Result := PDFRenderer.DIP(aSize);
end;

procedure TPDFMarkDownBlockRenderer.Indent(aSize: Integer);
begin
  PDFRenderer.Indent(aSize);
end;

procedure TPDFMarkDownBlockRenderer.Undent(aSize: Integer);
begin
  PDFRenderer.Undent(aSize);
end;

function TPDFMarkDownBlockRenderer.LayoutData: TLayoutData;
begin
  Result := PDFRenderer.Layout;
end;

function TPDFMarkDownBlockRenderer.CreateLayoutItem(aKind: TLayoutItemKind; aX, aY: TPDFFloat): TLayoutItem;
begin
  Result := PDFRenderer.CreateLayoutItem(aKind, aX, aY);
end;

function TPDFMarkDownBlockRenderer.MeasureTextHeight(const aFontSize: LongInt; const aFontStyle: TFontStyles; const aContext: TFontContext): LongInt;
begin
  Result := PDFRenderer.MeasureTextHeight(aFontSize, aFontStyle, aContext);
end;

function TPDFMarkDownBlockRenderer.MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt; const aFontStyle: TFontStyles; aFontContext: TFontContext): LongInt;
begin
  Result := PDFRenderer.MeasureTextWidth(aText, aFontSize, aFontStyle, aFontContext);
end;

procedure TPDFMarkDownBlockRenderer.LayoutText(const aText: utf8string; const aFontSize: LongInt; const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext: TFontContext);
begin
  PDFRenderer.LayoutTextWrapped(aText, aFontSize, aFontStyle, aLinkHref, aContext, False, False);
end;

procedure TPDFMarkDownBlockRenderer.SetCurrentY(aValue: TPDFFloat);
begin
  PDFRenderer.SetCurrentY(aValue);
end;

function TPDFMarkDownBlockRenderer.GetCurrentY: TPDFFloat;
begin
  Result := PDFRenderer.Layout.LineY;
end;

procedure TPDFMarkDownBlockRenderer.NewLine;
begin
  PDFRenderer.NewLine;
end;

procedure TPDFMarkDownBlockRenderer.ParagraphBreak;
begin
  PDFRenderer.ParagraphBreak;
end;

procedure TPDFMarkDownBlockRenderer.MaybeStartParagraph;
begin
  PDFRenderer.MaybeStartParagraph;
end;

{ TPDFMarkDownTextRenderer }

function TPDFMarkDownTextRenderer.GetPDFRenderer: TMarkDownPDFRenderer;
begin
  Result := TMarkDownPDFRenderer(Renderer);
end;

procedure TPDFMarkDownTextRenderer.DoRender(aElement: TMarkDownTextNode);
begin
  PDFRenderer.RenderTextNode(aElement, PDFRenderer.BaseFontSize, [], []);
end;

{ TPDFMarkDownParagraphBlockRenderer }

procedure TPDFMarkDownParagraphBlockRenderer.DoRender(aBlock: TMarkDownBlock);
begin
  MaybeStartParagraph;
  Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
end;

class function TPDFMarkDownParagraphBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownParagraphBlock;
end;

{ TPDFMarkDownHeadingBlockRenderer }

procedure TPDFMarkDownHeadingBlockRenderer.DoRender(aBlock: TMarkDownBlock);
const
  // Per-level point increment over the base size (matches the canvas renderer)
  HeadingSizes : array[1..5] of Integer = (10,8,6,4,2);
var
  lHeadingBlock: TMarkDownHeadingBlock;
begin
  lHeadingBlock := TMarkDownHeadingBlock(aBlock);

  // The computed size is read back by the text block renderer through the parent
  // renderer chain (see TPDFMarkDownTextBlockRenderer.DoRender).
  fontSize := PDFRenderer.BaseFontSize;
  if lHeadingBlock.Level in [1..5] then
    Inc(fontSize, HeadingSizes[lHeadingBlock.Level]);

  MaybeStartParagraph;
  Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
  ParagraphBreak;
end;

class function TPDFMarkDownHeadingBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownHeadingBlock;
end;

{ TPDFMarkDownQuoteBlockRenderer }

procedure TPDFMarkDownQuoteBlockRenderer.DoRender(aBlock: TMarkDownBlock);
begin
  MaybeStartParagraph;
  Indent(PDFRenderer.BlockQuoteIndent);
  try
    Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
  finally
    Undent(PDFRenderer.BlockQuoteIndent);
  end;
  ParagraphBreak;
end;

class function TPDFMarkDownQuoteBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownQuoteBlock;
end;

{ TPDFMarkDownListBlockRenderer }

procedure TPDFMarkDownListBlockRenderer.reset;
begin
  inherited reset;
  FItemNumber := 0;
end;

procedure TPDFMarkDownListBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lListBlock: TMarkDownListBlock;
  i: Integer;
begin
  if not assigned(aBlock) then
    exit;
  lListBlock := TMarkDownListBlock(aBlock);

  MaybeStartParagraph;

  // The bullet level (and its bullet character) only applies to unordered lists
  if not lListBlock.Ordered then
    PDFRenderer.IncBulletLevel;
  try
    // For ordered lists, ItemNumber is set before each item so the item
    // renderer can show a number; for unordered lists it stays 0 (a bullet).
    FItemNumber := 0;
    for i := 0 to aBlock.ChildCount-1 do
      begin
      if lListBlock.Ordered then
        FItemNumber := lListBlock.Start + i;
      Renderer.RenderBlock(aBlock.Children[i]);
      end;
  finally
    if not lListBlock.Ordered then
      PDFRenderer.DecBulletLevel;
  end;

  ParagraphBreak;
end;

class function TPDFMarkDownListBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownListBlock;
end;

{ TPDFMarkDownListItemBlockRenderer }

procedure TPDFMarkDownListItemBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lListRenderer: TPDFMarkDownListBlockRenderer;
  lPrefix: string;
  lIndent: Integer;
begin
  if not assigned(aBlock) then
    exit;

  // A numbered prefix for ordered lists (ItemNumber>0), a bullet otherwise
  lListRenderer := TPDFMarkDownListBlockRenderer(GetFirstParentWithClass(TPDFMarkDownListBlockRenderer));
  if Assigned(lListRenderer) and (lListRenderer.ItemNumber>0) then
    lPrefix := IntToStr(lListRenderer.ItemNumber) + '.'
  else
    lPrefix := PDFRenderer.GetBullet;

  // Render bullet/number, then hang-indent so wrapped lines align after it
  LayoutText(lPrefix+' ', PDFRenderer.BaseFontSize, [], '', []);
  lIndent := MeasureTextWidth(lPrefix+'_', PDFRenderer.BaseFontSize, [], []);
  PDFRenderer.Indent(lIndent);

  // Keep the item text on the same line as the bullet
  PDFRenderer.SuppressNextParagraphBreak;
  // Render list item content
  Renderer.RenderChildren(aBlock as TMarkDownContainerBlock);
  // Clear the suppression in case the content produced no paragraph to consume it
  PDFRenderer.FSuppressParagraphBreak:=False;
  PDFRenderer.Undent(lIndent);
  NewLine;
end;

class function TPDFMarkDownListItemBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownListItemBlock;
end;

{ TPDFMarkDownCodeBlockRenderer }

procedure TPDFMarkDownCodeBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  i, lPad, lLineHeight: Integer;
  lStartX, lStartY: TPDFFloat;
  lBG: TLayoutItem;
begin
  if not assigned(aBlock) or (aBlock.ChildCount=0) then
    exit;
  MaybeStartParagraph;
  lPad := DIP(4);
  lLineHeight := MeasureTextHeight(PDFRenderer.BaseFontSize, [], [fcMono]);
  lStartX := LayoutData.CurrentIndent;
  lStartY := LayoutData.LineY;
  // Filled background spanning the block, created first so it draws behind the code.
  lBG := CreateLayoutItem(likBackground, lStartX, lStartY);
  lBG.Width := LayoutData.MaxWidth - lStartX;
  lBG.Height := aBlock.ChildCount*lLineHeight + 2*lPad;
  lBG.BGColor := PDFRenderer.BGCodeColor;
  // Inset the code by the padding (top and left)
  SetCurrentY(lStartY + lPad);
  Indent(lPad);
  // Each child text block is one code line; the monospace/code context is picked up
  // from this renderer by the text block renderer's parent walk.
  for i := 0 to aBlock.ChildCount-1 do
    begin
    Renderer.RenderBlock(aBlock.Children[i]);
    NewLine;
    end;
  Undent(lPad);
  ParagraphBreak;
end;

class function TPDFMarkDownCodeBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownCodeBlock;
end;

{ TPDFMarkDownThematicBreakBlockRenderer }

procedure TPDFMarkDownThematicBreakBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  HRItem: TLayoutItem;
begin
  MaybeStartParagraph;

  // Create horizontal rule
  HRItem := CreateLayoutItem(likHR, LayoutData.CurrentIndent, LayoutData.LineY);
  HRItem.Width := LayoutData.MaxWidth - LayoutData.CurrentIndent;
  HRItem.Height := 2;

  ParagraphBreak;
end;

class function TPDFMarkDownThematicBreakBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownThematicBreakBlock;
end;

{ TPDFMarkDownTextBlockRenderer }

procedure TPDFMarkDownTextBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lTextBlock: TMarkDownTextBlock;
  i: Integer;
  FontStyles: TFontStyles;
  FontSize: integer;
  FontContext: TFontContext;
  Parents: TMarkDownElementRendererArray;
  Parent: TMarkDownElementRenderer;
begin
  if not assigned(aBlock) then exit;
  lTextBlock := TMarkDownTextBlock(aBlock);
  if not assigned(lTextBlock.Nodes) then exit;

  FontStyles := [];
  FontContext := [];
  FontSize := PDFRenderer.BaseFontSize;

  // Inherit styling from the enclosing block renderers: a heading sets the font
  // size, a quote/code block sets the font context, and the table header row is
  // rendered bold.
  Parents := GetParentRenderers;
  for i := Length(Parents)-1 downto 0 do
    begin
    Parent := Parents[i];
    if Parent is TPDFMarkDownHeadingBlockRenderer then
      FontSize := TPDFMarkDownHeadingBlockRenderer(Parent).fontSize;
    if Parent is TPDFMarkDownQuoteBlockRenderer then
      Include(FontContext, fcQuote);
    if Parent is TPDFMarkDownCodeBlockRenderer then
      FontContext := FontContext + [fcMono, fcFencedCode];
    if Parent is TPDFMarkDownTableBlockRenderer then
      if TPDFMarkDownTableBlockRenderer(Parent).CurrentRow = 0 then
        Include(FontStyles, fsBold);
    end;

  for i := 0 to lTextBlock.Nodes.Count - 1 do
    PDFRenderer.RenderTextNode(lTextBlock.Nodes[i], FontSize, FontStyles, FontContext);
end;

class function TPDFMarkDownTextBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result := TMarkDownTextBlock;
end;

{ TPDFMarkDownTableBlockRenderer }

// Collect the plain text of all nodes in a cell (used to measure column widths)
procedure TPDFMarkDownTableBlockRenderer.GetCellTexts(aCell: TMarkDownBlock; aTexts: TStrings);
var
  lCont : TMarkDownContainerBlock absolute aCell;
  lText : TMarkDownTextBlock absolute aCell;
  i : Integer;
begin
  if aCell is TMarkDownTextBlock then
    begin
    for i:=0 to lText.Nodes.Count-1 do
      aTexts.Add(lText.Nodes[i].NodeText);
    end
  else if aCell is TMarkDownContainerBlock then
    for i:=0 to lCont.ChildCount-1 do
      GetCellTexts(lCont.Children[i], aTexts);
end;


function TPDFMarkDownTableBlockRenderer.GetTotalColumns: Integer;
begin
  Result:=Length(FColLayout);
end;


function TPDFMarkDownTableBlockRenderer.GetTotalHeight: Integer;
var
  i : Integer;
begin
  Result:=0;
  for i:=0 to Length(FRowHeights)-1 do
    Result:=Result+FRowHeights[i];
  // Grid lines sit between the rows
  if Length(FRowHeights)>1 then
    Result:=Result+(Length(FRowHeights)-1)*FGridSize;
end;


// Determine the height of each row by laying out every cell into a throw-away
// layout and taking the tallest result.
procedure TPDFMarkDownTableBlockRenderer.CalcRowHeights(aTable: TMarkDownTableBlock; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext: TFontContext);
var
  lRow, lColumn, lLast : LongInt;
  CellWidth, TextWidth : LongInt;
  Saved, Temp : TLayoutData;
  SavedItems, TempItems : TLayoutLists;
  Cell : TMarkDownBlock;
begin
  SetLength(FRowHeights, aTable.ChildCount);
  for lRow:=0 to aTable.ChildCount-1 do
    begin
    FRowHeights[lRow]:=0;
    lLast:=Min(aTable.Children[lRow].ChildCount-1, TotalColumns-1);
    for lColumn:=0 to lLast do
      begin
      Cell:=aTable.Children[lRow].Children[lColumn];
      CellWidth:=FColLayout[lColumn].Width;
      TextWidth:=CellWidth-(FCellPadding*2);
      if TextWidth<1 then
        TextWidth:=1;
      Temp:=Default(TLayoutData);
      Temp.MaxWidth:=TextWidth;
      Temp.CurrentPageIndex:=PDFRenderer.Layout.CurrentPageIndex;
      Saved:=PDFRenderer.SwapLayout(Temp);
      // Measure into a temporary item list so nothing is actually drawn
      TempItems:=TLayoutLists.Create;
      SavedItems:=PDFRenderer.SwapLayoutLists(TempItems);
      try
        PDFRenderer.RenderBlock(Cell);
      finally
        PDFRenderer.SwapLayoutLists(SavedItems);
        TempItems.Free;
      end;
      Temp:=PDFRenderer.SwapLayout(Saved);
      if Round(Temp.LineY+Temp.LineHeight)>FRowHeights[lRow] then
        FRowHeights[lRow]:=Round(Temp.LineY+Temp.LineHeight);
      end;
    if FRowHeights[lRow]<MeasureTextHeight(aFontSize,aFontStyle,aContext) then
      FRowHeights[lRow]:=MeasureTextHeight(aFontSize,aFontStyle,aContext);
    FRowHeights[lRow]:=FRowHeights[lRow]+(FCellPadding*2);
    end;
end;


// Compute the minimum (widest word) and preferred (no-wrap) width per column
procedure TPDFMarkDownTableBlockRenderer.MeasureTableColumns(const aTableModel: TMarkDownTableBlock);

  function MaxWordWidth(aTexts: TStrings; aFontSize: Integer): LongInt;
  var
    ScanIndex : LongInt;
    WordBuffer : UTF8String;
    BestWidth, Current : LongInt;
    Text : string;

    procedure UpdateBest;
    begin
      if WordBuffer='' then
        exit;
      Current:=MeasureTextWidth(WordBuffer, aFontSize, [], [fcQuote]);
      if Current>BestWidth then
        BestWidth:=Current;
      WordBuffer:='';
    end;

  begin
    BestWidth:=0;
    WordBuffer:='';
    for Text in aTexts do
      begin
      ScanIndex:=1;
      while ScanIndex<=Length(Text) do
        begin
        if (Text[ScanIndex]=' ') or (Text[ScanIndex]=#9) or (Text[ScanIndex]=#10) or (Text[ScanIndex]=#13) then
          UpdateBest
        else
          WordBuffer:=WordBuffer+Text[ScanIndex];
        Inc(ScanIndex);
        end;
      UpdateBest;
      end;
    Result:=BestWidth;
  end;

  function NoWrapWidth(aTexts: TStrings; aFontSize: Integer): LongInt;
  begin
    Result:=MeasureTextWidth(aTexts.DelimitedText, aFontSize, [], [fcQuote]);
  end;

var
  lColumn, lRow, lLast : LongInt;
  MinimumWord, PreferredNoWrap : LongInt;
  Cell : TMarkDownBlock;
  lRowBlock : TMarkDownTableRowBlock;
  lFontSize : Integer;
  Texts : TStrings;
begin
  lFontSize:=PDFRenderer.BaseFontSize;
  for lColumn:=0 to TotalColumns-1 do
    begin
    FColLayout[lColumn].Min:=DIP(8);
    FColLayout[lColumn].Pref:=DIP(16);
    end;
  Texts:=TStringList.Create;
  try
    Texts.Delimiter:=' ';
    for lRow:=0 to aTableModel.ChildCount-1 do
      begin
      lRowBlock:=aTableModel.Children[lRow] as TMarkDownTableRowBlock;
      lLast:=Min(lRowBlock.ChildCount-1, TotalColumns-1);
      for lColumn:=0 to lLast do
        begin
        Cell:=lRowBlock.Children[lColumn];
        Texts.Clear;
        GetCellTexts(Cell, Texts);
        MinimumWord:=MaxWordWidth(Texts, lFontSize)+DIP(8);
        PreferredNoWrap:=NoWrapWidth(Texts, lFontSize)+DIP(8);
        with FColLayout[lColumn] do
          begin
          if MinimumWord>Min then
            Min:=MinimumWord;
          if PreferredNoWrap>Pref then
            Pref:=PreferredNoWrap;
          end;
        end;
      end;
  finally
    Texts.Free;
  end;
end;


// Allocate the available width across the columns between their min and preferred
procedure TPDFMarkDownTableBlockRenderer.DistributeColumns(const aAvailable: LongInt);
var
  i, TotalMin, TotalPref, Extra, Remain, Room, Denom, SpanVal : LongInt;
begin
  TotalMin:=0;
  TotalPref:=0;
  for i:=0 to TotalColumns-1 do
    with FColLayout[i] do
      begin
      TotalMin:=TotalMin+Min;
      TotalPref:=TotalPref+Pref;
      end;
  if aAvailable<=TotalMin then
    begin
    // Less room than the minimum - allocate the minimum
    for i:=0 to TotalColumns-1 do
      FColLayout[i].Width:=FColLayout[i].Min;
    end
  else if aAvailable>=TotalPref then
    begin
    // More room than needed - give every column its preferred width plus a share
    Extra:=aAvailable-TotalPref;
    for i:=0 to TotalColumns-1 do
      FColLayout[i].Width:=FColLayout[i].Pref+(Extra div TotalColumns);
    Remain:=Extra mod TotalColumns;
    for i:=0 to Remain-1 do
      Inc(FColLayout[i].Width);
    end
  else
    begin
    // Between minimum and preferred - distribute proportionally
    Room:=aAvailable-TotalMin;
    Denom:=TotalPref-TotalMin;
    for i:=0 to TotalColumns-1 do
      with FColLayout[i] do
        begin
        SpanVal:=Pref-Min;
        if Denom>0 then
          Width:=Min+((SpanVal*Room) div Denom)
        else
          Width:=Min;
        end;
    end;
end;


// Draw the outer rectangle and the vertical separators between columns
procedure TPDFMarkDownTableBlockRenderer.DrawBorders;
var
  i, ColX : Integer;
  Itm : TLayoutItem;
begin
  Itm:=CreateLayoutItem(likRect, FStartX, FStartY);
  Itm.Width:=AvailableWidth;
  Itm.Height:=TotalHeight;
  ColX:=FStartX;
  for i:=0 to Length(FColLayout)-2 do
    begin
    ColX:=ColX+FColLayout[i].Width+FGridSize;
    Itm:=CreateLayoutItem(likLine, ColX, FStartY);
    Itm.DeltaX:=0;
    Itm.DeltaY:=TotalHeight;
    end;
end;


procedure TPDFMarkDownTableBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lTable : TMarkDownTableBlock absolute aBlock;
  i : Integer;
begin
  if not assigned(aBlock) then
    exit;
  MaybeStartParagraph;
  SetLength(FColLayout, Length(lTable.Columns));
  if TotalColumns=0 then
    exit;
  FGridSize:=DIP(1);
  FCellPadding:=DIP(4);
  FAvailableWidth:=Round(LayoutData.MaxWidth-LayoutData.CurrentIndent)-(TotalColumns-1)*FGridSize;
  if FAvailableWidth<1 then
    FAvailableWidth:=1;
  MeasureTableColumns(lTable);
  DistributeColumns(AvailableWidth);
  CalcRowHeights(lTable, PDFRenderer.BaseFontSize, [], []);
  // Move the whole table to the next page if it does not fit on the current one
  PDFRenderer.CheckPageBreak(TotalHeight);
  // CurrentIndent already includes the left margin, so it is the table's left edge.
  FStartX:=Round(LayoutData.CurrentIndent);
  FStartY:=Round(LayoutData.LineY);
  DrawBorders;
  // Render the rows; each advances the running start position
  for i:=0 to aBlock.ChildCount-1 do
    begin
    FCurrentRow:=i;
    PDFRenderer.RenderBlock(aBlock.Children[i]);
    FStartY:=FStartY+RowHeights[i]+GridSize;
    SetCurrentY(FStartY);
    end;
  FCurrentRow:=-1;
  ParagraphBreak;
end;


procedure TPDFMarkDownTableBlockRenderer.Reset;
begin
  inherited Reset;
  FCurrentRow:=-1;
  SetLength(FColLayout,0);
end;


class function TPDFMarkDownTableBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableBlock;
end;


{ TPDFMarkDownTableRowBlockRenderer }

// Find the enclosing table renderer through the parent renderer chain
procedure TPDFMarkDownTableRowBlockRenderer.GetTableRenderer;
var
  A : TMarkDownElementRendererArray;
  i : Integer;
begin
  A:=GetParentRenderers;
  FTableRenderer:=nil;
  i:=Length(A)-1;
  while (i>=0) and not Assigned(FTableRenderer) do
    begin
    if A[i] is TPDFMarkDownTableBlockRenderer then
      FTableRenderer:=TPDFMarkDownTableBlockRenderer(A[i]);
    Dec(i);
    end;
  if FTableRenderer=nil then
    Raise EMarkDown.Create('Could not find markdown table renderer');
end;


procedure TPDFMarkDownTableRowBlockRenderer.DoRender(aBlock: TMarkDownBlock);
var
  lColumn, lLast : LongInt;
  CellWidth, CurrentY, CurrentX : LongInt;
  Saved, Temp : TLayoutData;
  Cell : TMarkDownBlock;
  Itm : TLayoutItem;
begin
  if not assigned(aBlock) then
    exit;
  GetTableRenderer;
  CurrentY:=TableRenderer.FStartY;
  CurrentX:=TableRenderer.FStartX;
  // Horizontal separator above every row except the first
  if TableRenderer.CurrentRow>0 then
    begin
    Itm:=CreateLayoutItem(likLine, CurrentX, CurrentY);
    Itm.DeltaX:=TableRenderer.AvailableWidth;
    Itm.DeltaY:=0;
    end;
  lLast:=Min(aBlock.ChildCount-1, Length(TableRenderer.ColLayout)-1);
  for lColumn:=0 to lLast do
    begin
    Cell:=aBlock.Children[lColumn];
    CellWidth:=TableRenderer.ColLayout[lColumn].Width;
    Temp:=Default(TLayoutData);
    Temp.MaxWidth:=CurrentX+CellWidth-TableRenderer.CellPadding;
    Temp.CurrentIndent:=CurrentX+TableRenderer.CellPadding;
    // LineY is the line-box top; FlushTextRun adds the ascent for the baseline.
    Temp.LineY:=CurrentY+TableRenderer.CellPadding;
    Temp.CurrentPageIndex:=PDFRenderer.Layout.CurrentPageIndex;
    Saved:=PDFRenderer.SwapLayout(Temp);
    PDFRenderer.RenderBlock(Cell);
    PDFRenderer.SwapLayout(Saved);
    CurrentX:=CurrentX+TableRenderer.ColLayout[lColumn].Width+TableRenderer.GridSize;
    end;
end;


class function TPDFMarkDownTableRowBlockRenderer.BlockClass: TMarkDownBlockClass;
begin
  Result:=TMarkDownTableRowBlock;
end;

{ TLayoutItem }

function TLayoutItem.GeTARGBColor(aKind: TARGBColorKind; aDefault: TARGBColor): TARGBColor;
begin
  case aKind of
    ckBackground : Result:=FBGColor;
    ckForeground : Result:=FColor;
  else
    Result:=clNone;
  end;
  if Result=clNone then
    Result:=aDefault;
end;

constructor TLayoutItem.Create(aKind: TLayoutItemKind; aX, aY: TPDFFloat);
begin
  FKind:=aKind;
  FColor:=clNone;
  FBGColor:=clNone;

  FX:=aX;
  FY:=aY;
  FPageIndex:=0;  // Initialize page index
end;

function TLayoutItemList.NewItem: TLayoutItem;
begin
  Result:=NewItem(likText,0,0)
end;

function TLayoutItemList.NewItem(aKind: TLayoutItemKind; aX, aY: TPDFFLoat): TLayoutItem;
begin
  Result:=TLayoutItem.Create(aKind,aX,aY);
  Add(Result);
end;

function TLayoutItemList.GetItem(const aIndex: LongInt): TLayoutItem;
begin
  Result:=TLayoutItem(Inherited GetItem(aIndex));
end;

{ TLinkHitRect }

constructor TLinkHitRect.create(aX, aY, aWidth, aHeight: TPDFFLoat; aHREF: UTF8String; aPageIndex: Integer);
begin
  FX:=aX;
  FY:=aY;
  FWidth:=aWidth;
  FHeight:=aHeight;
  FHREF:=aHREF;
  FPageIndex:=aPageIndex;
end;

function TLinkHitRect.HasPoint(aX, aY: Longint): Boolean;
begin
  Result:= ((aX>=X) and (aX<(X+Width))) and
           ((aY>=Y) and (aY<(Y+Height)));
end;

{=== TLinkHitRectList ======================================================}

function TLinkHitRectList.Add(const aX, aY, aWidth, aHeight: TPDFFLoat; const aHref: utf8string; const aPageIndex: Integer
  ): TLinkHitRect;
begin
  Result:=TLinkHitRect.Create(ax,ay,aWidth,aHeight,aHREF,aPageIndex);
  Inherited add(Result);
end;

function TLinkHitRectList.GetItem(const aIndex: LongInt): TLinkHitRect;
begin
  Result:=TLinkHitRect(Inherited GetItem(aIndex));
end;

{ TMarkDownPDFRenderer }

constructor TMarkDownPDFRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FHTMLEntities:=TFPStringHashTable.Create;
  FImageCache:=TFPObjectHashTable.Create(True);
  FPageList:=TList.Create;
  FBullets[1]:='•';
  FBullets[2]:='◦';
  FBullets[3]:='▪';
  FBulletLevel:=0;
  FImageMargin:=2;
  FLists.Items:=TLayoutItemList.Create;
  FLists.LinkRects:=TLinkHitRectList.Create;

  // Initialize default values
  fParagraphSpacing:=10;
  fExtraIndent:=0;
  fTargetDPI:=96;
  fFontName:='Sans Serif';
  fMonoFontName:='Monospace';

  BGCodeColor:=RGBToColor(253,245,227);
  fFontCodeColor:=clInfoText;
  fBGMarkColor:=clYellow;
  fFontMarkColor:=clBlack;
  fHyperLinkColor:=RGBToColor(17,85,204);
  fBGColor:=clWindow;
  fFonTARGBColor:=clWindowText;
  // A muted green-grey for quoted text (clWindowFrame is black in this unit)
  fFontQuoteColor:=RGBToColor(106,153,85);

  fBaseFontSize:=10;
  FLayout.BaselineShiftCurrent:=0;

  // Initialize PDF-specific defaults
  FLayout.CurrentPageIndex:=-1;
  TopMargin:=72;    // 1 inch margin
  BottomMargin:=72;
  LeftMargin:=72;
  RightMargin:=72;
  FBlockQuoteIndent:=DIP(24); // indent for block quotes (was uninitialised)
end;

destructor TMarkDownPDFRenderer.Destroy;
begin
  FreeAndNil(FHTMLEntities);
  FreeAndNil(FImageCache);
  FreeAndNil(FPageList);
  FreeAndNil(FLists.Items);
  FreeAndNil(FLists.LinkRects);
  // FDocument is owned by the caller, not freed here.
  inherited Destroy;
end;

{ TLayoutLists }

procedure TLayoutLists.Clear;
begin
  Items.Clear;
  LinkRects.Clear;
end;

class function TLayoutLists.Create: TLayoutLists;
begin
  Result.Items:=TLayoutItemList.Create;
  Result.LinkRects:=TLinkHitRectList.Create;
end;

procedure TLayoutLists.Free;
begin
  FreeAndNil(Items);
  FreeAndNil(LinkRects);
end;

{ PDF-specific layout and page management methods }

procedure TMarkDownPDFRenderer.CheckPageBreak(const aHeight: TPDFFloat);
var
  CurrentPageHeight, AvailableHeight: TPDFFLoat;
begin
  CurrentPageHeight := GetCurrentPageY;
  AvailableHeight := GetEffectivePageHeight;

  if CurrentPageHeight + aHeight > AvailableHeight then
  begin
    CreateNewPage;
  end;
end;

procedure TMarkDownPDFRenderer.CreateNewPage;
var
  FontIndex: Integer;
begin
  if Not Assigned(FPDFDocument) then
    Raise Exception.Create('No document');
  FCurrentPage := FPDFDocument.Pages.AddPage;
  FPDFDocument.Sections[0].AddPage(FCurrentPage);
  FPageList.Add(FCurrentPage);
  Inc(FLayout.CurrentPageIndex);
  // LineX is 0 at the start of a line; CurrentIndent (set in BeginLayout) carries
  // the left margin. CurrentIndent is preserved across page breaks.
  FLayout.LineX:=0;
  FLayout.LineY:=TopMargin;
  // Set default font for the new page
  FontIndex := FPDFDocument.AddFont('Helvetica');
  FCurrentPage.SetFont(FontIndex, FBaseFontSize);
end;

function TMarkDownPDFRenderer.GetCurrentPageY: TPDFFloat;
begin
  Result := FLayout.LineY;
end;

function TMarkDownPDFRenderer.GetEffectivePageHeight: TPDFFloat;
begin
  Result := TPDFPage(FPageList[FLayout.CurrentPageIndex]).Paper.H - TopMargin - BottomMargin;
end;

{ Layout management methods }

procedure TMarkDownPDFRenderer.Clear;
begin
  FLists.Items.Clear;
  FLists.LinkRects.Clear;
  FLayout:=Default(TLayoutData);
  fLineWidth:=0;
  FLayout.NeedSpaceBeforeNextText:=False;
  FLayout.CurrentPageIndex:=-1;
  FPageList.Clear;
end;

procedure TMarkDownPDFRenderer.BeginLayout;
begin
  Clear;
  CreateNewPage;
  // The left margin lives in CurrentIndent so that line starts (LineX=0) and the
  // wrap test (CurrentIndent+LineX+w <= MaxWidth) are uniform with table cells.
  FLayout.CurrentIndent:=LeftMargin;
end;

procedure TMarkDownPDFRenderer.EndLayout;
begin
  // Reserved for future batching hooks
end;

function TMarkDownPDFRenderer.GetBulletChar(AIndex: Integer): string;
begin
  result:=FBullets[aIndex];
end;

function TMarkDownPDFRenderer.DIP(const aValue: LongInt): LongInt;
begin
  Result:=(aValue * fTargetDPI + 48) div 96;
end;

procedure TMarkDownPDFRenderer.NewLine;
begin
  FLayout.LineX:=0;
  if FNextLineIndent<>0 then
    begin
    FLayout.CurrentIndent:=FLayout.CurrentIndent+FNextLineIndent;
    FNextLineIndent:=0;
    end;
  FLayout.LineY:=FLayout.LineY + (FLayout.LineAboveExtra + FLayout.LineHeight + FLayout.LineBelowExtra);

  // Check if we need a new page
  CheckPageBreak(FLayout.LineHeight);

  FLayout.LineHeight:=0;
  FLayout.LineAboveExtra:=0;
  FLayout.LineBelowExtra:=0;
  FLayout.NeedSpaceBeforeNextText:=False;
  FLayout.BaselineFromIndex:=FLists.Items.Count-1;
  FLayout.BaselineShiftCurrent:=0;
end;

procedure TMarkDownPDFRenderer.ParagraphBreak;
begin
  if FLayout.LineX<>0 then
    NewLine;
  FLayout.LineY:=FLayout.LineY + DIP(fParagraphSpacing);
  CheckPageBreak(DIP(fParagraphSpacing));
  FLayout.LineAboveExtra:=0;
  FLayout.LineBelowExtra:=0;
end;

procedure TMarkDownPDFRenderer.MaybeStartParagraph;
begin
  // A list item suppresses the break of its first content block so the text
  // ends up next to the bullet instead of on the following line.
  if FSuppressParagraphBreak then
    begin
    FSuppressParagraphBreak:=False;
    Exit;
    end;
  if FLayout.LineX<>0 then
   ParagraphBreak;
end;


procedure TMarkDownPDFRenderer.SuppressNextParagraphBreak;
begin
  FSuppressParagraphBreak:=True;
end;

function TMarkDownPDFRenderer.SwapLayout(const aData: TLayoutData): TLayoutData;
begin
  Result:=FLayout;
  FLayout:=aData;
end;

function TMarkDownPDFRenderer.SwapLayoutLists(const aData: TLayoutLists): TLayoutLists;
begin
  Result:=FLists;
  FLists:=aData;
end;

procedure TMarkDownPDFRenderer.SetCurrentY(aValue: TPDFFLoat);
begin
  FLayout.LineY:=aValue;
end;

{ Font and measurement methods - adapted for PDF }

function TMarkDownPDFRenderer.ResolveFontForText(const aText: utf8string; const aFontName: string;
  const aFontStyle: TFontStyles): TFPFontCacheItem;
var
  i: Integer;
  lItem: TFPFontCacheItem;
begin
  Result := GetFontCacheItem(aFontName, aFontStyle);
  if (Result=nil) or FontCoversText(Result, aText) then
    exit;
  // The configured font lacks some glyphs (e.g. CJK). Reuse the last fallback if
  // it covers this run, otherwise scan the cache once for a font that does.
  if FontCoversText(FFallbackFont, aText) then
    Exit(FFallbackFont);
  for i := 0 to gTTFontCache.Count-1 do
    begin
    lItem := gTTFontCache.Items[i];
    if FontCoversText(lItem, aText) then
      begin
      FFallbackFont := lItem;
      Exit(lItem);
      end;
    end;
end;


procedure TMarkDownPDFRenderer.ApplyFont(aPage: TPDFPage; const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext : TFontContext);
var
  lFontName: string;
  FontItem: TFPFontCacheItem;
  FontIndex: Integer;
  FallbackFontName: string;
begin
  if fcMono in aContext then
    lFontName := fMonoFontName
  else
    lFontName := fFontName;

  FontItem := ResolveFontForText(aText, lFontName, aFontStyle);

  if Assigned(aPage) and Assigned(FPDFDocument) then
  begin
    if Assigned(FontItem) then
    begin
      // Embed the actual TrueType font, so the glyphs (and, crucially, their
      // advance widths) drawn here match what MeasureText computed from the very
      // same font cache item. Registering by name only (AddFont(PostScriptName))
      // would fall back to a standard PDF font with different metrics.
      if FontItem.FileName<>'' then
        FontIndex := FPDFDocument.AddFont(FontItem.FileName, FontItem.PostScriptName)
      else
        FontIndex := FPDFDocument.AddFont(FontItem.PostScriptName);
      aPage.SetFont(FontIndex, aFontSize);
    end
    else
    begin
      // Fallback to standard PDF fonts
      if fcMono in aContext then
        FallbackFontName := 'Courier'
      else if fsBold in aFontStyle then
        FallbackFontName := 'Helvetica-Bold'
      else if fsItalic in aFontStyle then
        FallbackFontName := 'Helvetica-Oblique'
      else
        FallbackFontName := 'Helvetica';

      FontIndex := FPDFDocument.AddFont(FallbackFontName);
      aPage.SetFont(FontIndex, aFontSize);
    end;
  end;
end;

procedure TMarkDownPDFRenderer.MeasureText(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext : TFontContext; out aWidth, aHeight: LongInt);
var
  lFontName: string;
  BaseFont, SegFont: TFPFontCacheItem;
  CharWidth: LongInt;
  lSeg: utf8string;
  lSegs: TStringDynArray;
begin
  if length(aText)>0 then
  begin
    if fcMono in aContext then
      lFontName := fMonoFontName
    else
      lFontName := fFontName;

    BaseFont := GetFontCacheItem(lFontName, aFontStyle);
    if Assigned(BaseFont) then
    begin
      // Measure each coverage segment with the font that will actually draw it, so
      // the width matches the per-segment font fallback used in FlushTextRun.
      // The font cache returns the width in pixels at gTTFontCache.DPI (typically
      // 96), but layout happens in PDF points (72 per inch, since the document
      // uses uomPixels where 1 unit = 1 point); convert, or runs drift right.
      aWidth := 0;
      for lSeg in SplitByCoverage(aText, BaseFont) do
        begin
        SegFont := ResolveFontForText(lSeg, lFontName, aFontStyle);
        if SegFont=nil then
          SegFont := BaseFont;
        aWidth := aWidth + Round(SegFont.TextWidth(lSeg, aFontSize) * cPDFPointsPerInch / gTTFontCache.DPI);
        end;
    end else begin
      // Improved fallback estimation
      if fcMono in aContext then
        CharWidth := Round(aFontSize * 0.6)  // Monospace is typically 0.6 width
      else
        CharWidth := Round(aFontSize * 0.5); // Proportional font average

      aWidth := Length(aText) * CharWidth;
    end;
    // Line height is the rendered font size plus normal leading. The font cache
    // would only report the cap height (no ascenders/descenders/leading), which
    // packs lines too tightly, so derive it from the point size instead.
    aHeight := Round(aFontSize * cLineSpacingFactor);
  end
  else
  begin
    aWidth:=0;
    aHeight:=0;
  end;
end;

function TMarkDownPDFRenderer.MeasureTextWidth(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext : TFontContext): LongInt;
var
  Width, Height: LongInt;
begin
  MeasureText(aText, aFontSize, aFontStyle, aContext, Width, Height);
  Result:=Width;
end;

function TMarkDownPDFRenderer.MeasureTextHeight(const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aContext: TFontContext): LongInt;
var
  Width, Height: LongInt;
begin
  MeasureText(ExtendHeight, aFontSize, aFontStyle, aContext, Width, Height);
  Result:=Height;
end;

function TMarkDownPDFRenderer.GetTexTARGBColor(aContext : TFontContext) : TARGBColor;
begin
  if fcHyperLink in aContext then
    result:=FontHyperLinkColor
  else if fcQuote in aContext then
    Result:=fFontQuoteColor
  else if ([fcCode,fcFencedCode]*aContext)<>[] then
    Result:=FontCodeColor
  else
    Result:=FonTARGBColor;
end;

function TMarkDownPDFRenderer.GetTextBGColor(aContext : TFontContext) : TARGBColor;
begin
  if fcFencedCode in aContext then
    Result:=BGCodeColor
  else
    Result:=BGColor;
end;

{ Text rendering methods }

procedure TMarkDownPDFRenderer.FlushTextRun(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
  const aDryRun: boolean);
var
  Item: TLayoutItem;
  lX,lY : TPDFFLoat;
  lNewWidth, lHeight, lWidth, BaselineShift: TPDFFLoat;
  lFontName: string;
  BaseFont: TFPFontCacheItem;
  lSeg: utf8string;
begin
  if length(aText)=0 then
    exit;

  lHeight:=MeasureTextHeight(aFontSize, aFontStyle, aContext);
  BaselineShift:=FLayout.BaselineShiftCurrent;
  if FLayout.LineHeight<(lHeight) then
    FLayout.LineHeight:=lHeight;
  // LineY is the top of the line box; PDF text is positioned on its baseline, so
  // drop down by the ascent (~0.78 of the run's own height) to keep the glyphs
  // inside the box. Using the run's height (not the line max) keeps every run on
  // the same baseline even when a taller item (e.g. an inline image) shares the line.
  lY:=FLayout.LineY + FLayout.LineAboveExtra + Round(lHeight*0.78) + BaselineShift;

  if fcMono in aContext then
    lFontName := fMonoFontName
  else
    lFontName := fFontName;
  BaseFont := GetFontCacheItem(lFontName, aFontStyle);

  // Emit one item per coverage segment so each is drawn with a font that has its
  // glyphs (the base font, or a fallback for e.g. CJK). ApplyFont re-derives the
  // same per-segment font at draw time from the item text.
  for lSeg in SplitByCoverage(aText, BaseFont) do
    begin
    lWidth:=MeasureTextWidth(lSeg, aFontSize, aFontStyle, aContext);
    if not aDryRun then
      begin
      lX:=FLayout.CurrentIndent + FLayout.LineX;
      Item:=FLists.Items.NewItem(TLayoutItemKind.likText,lX,lY);
      item.BGColor:=GetTextBGColor(aContext);
      item.color:=GetTexTARGBColor(aContext);
      Item.Width:=lWidth;
      Item.Height:=lHeight;
      Item.Text:=lSeg;
      Item.FontSize:=aFontSize;
      Item.FontStyle:=aFontStyle;
      Item.URL:=aLinkHref;
      Item.Context:=aContext;
      Item.PageIndex:=FLayout.CurrentPageIndex;
      if length(aLinkHref)<>0 then
        FLists.LinkRects.Add(lX, lY, lWidth, lHeight, aLinkHref, FLayout.CurrentPageIndex);
      end;
    FLayout.LineX:=FLayout.LineX + lWidth;
    end;

  lNewWidth:=FLayout.CurrentIndent + FLayout.LineX;
  if fLineWidth<lNewWidth then
    fLineWidth:=lNewWidth;
end;

procedure TMarkDownPDFRenderer.RenderDocument(aDocument : TMarkDownDocument; aPDFDocument : TPDFDocument);
begin
  FDocument:=aDocument; // referenced only; the caller owns it
  FPDFDocument:=aPDFDocument;
  // We embed TrueType fonts (see ApplyFont); subsetting is required for the
  // embedded glyphs - including non-ASCII ones such as list bullets - to render.
  FPDFDocument.Options:=FPDFDocument.Options+[poSubsetFont];
  FPDFDocument.StartDocument;
  if FPDFDocument.Sections.Count=0 then
    FPDFDocument.Sections.AddSection; // need at least one section
  BeginLayout;
  // MaxWidth is the absolute right-edge X (page width minus the right margin), so
  // the wrap test CurrentIndent+LineX+w <= MaxWidth holds in both flow and cells.
  FLayout.MaxWidth:=FPDFDocument.Pages[0].Paper.W-RightMargin;
  // Render all child blocks
  RenderChildren(FDocument);
  EndLayout;
  DrawLayout;
end;

procedure TMarkDownPDFRenderer.DrawLayout;
var
  Index: LongInt;
  Item: TLayoutItem;
begin
  Index:=0;
  while Index<FLists.Items.Count do
    begin
    Item:=FLists.Items[Index];
    if Assigned(Item) then
      DrawLayoutItem(Item);
    Inc(Index);
    end;
end;

procedure TMarkDownPDFRenderer.DrawLayoutItem(aItem: TLayoutItem);
var
  TargetPage: TPDFPage;
begin
  if not Assigned(FPDFDocument) or not Assigned(aItem) then
    Exit;

  // Get the appropriate page for this item
  if (aItem.PageIndex < FPageList.Count) and (aItem.PageIndex >= 0) then
    TargetPage := TPDFPage(FPageList[aItem.PageIndex])
  else
    TargetPage := FCurrentPage;

  if not Assigned(TargetPage) then
    Exit;

  case aItem.Kind of
   TLayoutItemKind.likText:
     begin
     // Apply font and draw text
     ApplyFont(TargetPage, aItem.Text, aItem.FontSize, aItem.FontStyle, aItem.Context);

     // Convert TARGBColor to PDF color if needed
     TargetPage.SetColor(aItem.Color, False);
     TargetPage.WriteText(aItem.X, aItem.Y, aItem.Text);

     // Add clickable link if URL is present
     if aItem.URL <> '' then
       TargetPage.AddExternalLink(aItem.X, aItem.Y, aItem.Width, aItem.Height, aItem.URL, False);
     end;
   TLayoutItemKind.likRect:
     begin
     // DrawRect grows upward from its Y corner (origin is at the top), so pass the
     // bottom edge to make the item's Y the visual top of the rectangle.
     TargetPage.DrawRect(aItem.X, aItem.Y + aItem.Height, aItem.Width, aItem.Height, 1.0, False, True);
     end;
   TLayoutItemKind.likLine:
     begin
     TargetPage.DrawLine(aItem.X, aItem.Y, aItem.X + aItem.DeltaX, aItem.Y + aItem.DeltaY, 1.0);
     end;
   TLayoutItemKind.likImage:
     begin
     // Like DrawRect, the image grows up from its Y corner, so pass the bottom edge.
     TargetPage.DrawImage(aItem.X, aItem.Y + aItem.Height, aItem.Width, aItem.Height, aItem.ImageIndex);
     end;
   TLayoutItemKind.likBackground:
     begin
     // A filled rectangle (e.g. the code block background), drawn before the text.
     TargetPage.SetColor(aItem.BGColor, False);
     TargetPage.DrawRect(aItem.X, aItem.Y + aItem.Height, aItem.Width, aItem.Height, 0, True, False);
     end;
   // Additional cases for other item kinds as needed
  end;
end;


function TMarkDownPDFRenderer.HitTestLink(const aX, aY: LongInt; const aPageIndex: Integer; out aHref: utf8string): boolean;
var
  I: LongInt;
  Itm: TLinkHitRect;
begin
  aHref:='';
  Result:=False;
  I:=0;
  While Not Result and (I<FLists.LinkRects.Count) do
    begin
    Itm:=FLists.LinkRects[I];
    if (Itm.PageIndex = aPageIndex) and Itm.HasPoint(aX,aY) then
      begin
      Result:=True;
      aHref:=Itm.Href;
      Exit;
      end;
    Inc(I);
    end;
end;

function TMarkDownPDFRenderer.CreateLayoutItem(aKind: TLayoutItemKind; aX, aY: TPDFFloat): TLayoutItem;
begin
  Result:=FLists.Items.NewItem(aKind,aX,aY);
  Result.PageIndex := FLayout.CurrentPageIndex;
end;


function TMarkDownPDFRenderer.GetNodeFontStyle(aTextNode: TMarkDownTextNode): TFontStyles;
begin
  Result:=[];
  if nsStrong in aTextNode.Styles then
    Include(Result, fsBold);
  if nsEmph in aTextNode.Styles then
    Include(Result, fsItalic);
  if nsDelete in aTextNode.Styles then
    Include(Result, fsUnderline); // Using underline for strikeout as approximation
end;

procedure TMarkDownPDFRenderer.Indent(aSize: integer);
begin
  inc(aSize,fExtraIndent);
  if FLayout.LineX>0 then
    FNextLineIndent:=FNextLineIndent+aSize
  else
    FLayout.CurrentIndent:=FLayout.CurrentIndent+aSize;
end;

procedure TMarkDownPDFRenderer.SetBulletChar(AIndex: Integer; AValue: string);
begin
  FBullets[aIndex]:=aValue;
end;

procedure TMarkDownPDFRenderer.Undent(aSize: integer);
begin
  inc(aSize,fExtraIndent);
  // If the matching Indent was deferred to the next line and never applied (the
  // content did not wrap), just drop the pending amount.
  if FNextLineIndent >= aSize then
    begin
    Dec(FNextLineIndent, aSize);
    Exit;
    end;
  Dec(aSize, FNextLineIndent);
  FNextLineIndent := 0;
  FLayout.CurrentIndent:=FLayout.CurrentIndent-aSize;
  if FLayout.CurrentIndent<0 then
    FLayout.CurrentIndent:=0;
end;

procedure TMarkDownPDFRenderer.IncBulletLevel;
begin
  Inc(FBulletLevel);
end;

procedure TMarkDownPDFRenderer.DecBulletLevel;
begin
  Dec(FBulletLevel);
  if FBulletLevel<0 then
    FBulletLevel:=0;
end;

function TMarkDownPDFRenderer.GetBullet: String;
begin
  Result:=GetBulletAt(BulletLevel)
end;

function TMarkDownPDFRenderer.GetBulletAt(aLevel: integer): string;
begin
  // FBullets is 1-based; keep aLevel in range so a stray 0/level can never read
  // out of bounds.
  if aLevel<1 then
    aLevel:=1;
  While aLevel>BulletCount do
    aLevel:=aLevel-BulletCount;
  Result:=FBullets[aLevel];
end;

procedure TMarkDownPDFRenderer.LayoutTextWrapped(const aText: utf8string; const aFontSize: LongInt;
  const aFontStyle: TFontStyles; const aLinkHref: utf8string; const aContext : TFontContext;
  const aPreserveWhitespace, aDryRun: boolean);
Const
  WhiteSpace = [#32, #9, #13, #10];

var
  LineBuffer: AnsiString;
  NeedSpace: boolean;

  procedure FlushCurrentLine;
  begin
    if length(LineBuffer)>0 then
      begin
      FlushTextRun(TrimRight(LineBuffer), aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
      LineBuffer:='';
      end;
  end;

  function NextUTF8CharLenAt(const aString: utf8string; const aPosition: integer): integer;
  var
    ByteValue: byte;
  begin
    if (aPosition<1) or (aPosition>length(aString)) then
      Exit(0);
    ByteValue:=byte(aString[aPosition]);
    if ByteValue<$80 then
      Result:=1
    else if (ByteValue and $e0) = $c0 then
      Result:=2
    else if (ByteValue and $f0) = $e0 then
      Result:=3
    else
      Result:=4;
  end;

  // Break a word that is wider than the whole line, character by character
  procedure BreakLongWord(const aWord: utf8string);
  var
    CharPos, WordLen, CharLen: LongInt;
    CharBuffer, CurrentChar: utf8string;
    CharWidth: LongInt;
  begin
    WordLen:=length(aWord);
    CharBuffer:='';
    CharPos:=1;
    while CharPos <= WordLen do
      begin
      CharLen:=NextUTF8CharLenAt(aWord, CharPos);
      if CharLen <= 0 then
        CharLen:=1;
      CurrentChar:=copy(aWord, CharPos, CharLen);
      CharWidth:=MeasureTextWidth(CharBuffer + CurrentChar, aFontSize, aFontStyle, aContext);
      if (FLayout.CurrentIndent + FLayout.LineX + CharWidth)<=FLayout.MaxWidth then
        CharBuffer:=CharBuffer + CurrentChar
      else
        begin
        if length(CharBuffer)>0 then
          begin
          FlushTextRun(CharBuffer, aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
          CharBuffer:='';
          end;
        NewLine;
        CharBuffer:=CurrentChar;
        end;
      Inc(CharPos, CharLen);
      end;
    if length(CharBuffer)>0 then
      FlushTextRun(CharBuffer, aFontSize, aFontStyle, aLinkHref, aContext, aDryRun);
    NeedSpace:=True;
  end;

  // Add a word to the current line, wrapping to the next line when it does not fit
  procedure AddWordToLine(const aWord: utf8string);
  var
    lLine: utf8string;
    lWidth: LongInt;
  begin
    if length(aWord)=0 then
      exit;
    lLine:=LineBuffer;
    if NeedSpace and (length(LineBuffer)>0) then
      lLine:=lLine + ' ';
    lLine:=lLine + aWord;
    lWidth:=MeasureTextWidth(lLine, aFontSize, aFontStyle, aContext);
    if (FLayout.CurrentIndent+FLayout.LineX+lWidth)<=FLayout.MaxWidth then
      begin
      // Word fits on the current line
      if NeedSpace and (length(LineBuffer)>0) then
        LineBuffer:=LineBuffer + ' '
      else if NeedSpace and (length(LineBuffer) = 0) and (FLayout.LineX>0) then
        LineBuffer:=' ';
      LineBuffer:=LineBuffer + aWord;
      NeedSpace:=True;
      end
    else
      begin
      // Word does not fit: flush the line so far and continue on a new line
      FlushCurrentLine;
      NewLine;
      NeedSpace:=False;
      lWidth:=MeasureTextWidth(aWord, aFontSize, aFontStyle, aContext);
      if (FLayout.CurrentIndent + FLayout.LineX + lWidth) <= FLayout.MaxWidth then
        begin
        LineBuffer:=aWord;
        NeedSpace:=True;
        end
      else
        BreakLongWord(aWord);
      end;
  end;

var
  Index, lLength: LongInt;
  lChar: ansichar;
  WordBuffer, lText: utf8string;
  TextHeight: LongInt;
  FirstIndex, LastIndex: LongInt;

begin
  if aText='' then
    exit;
  lText:=aText;
  TextHeight:=MeasureTextHeight(aFontSize, aFontStyle, aContext);
  if TextHeight>FLayout.LineHeight then
    FLayout.LineHeight:=TextHeight;
  lLength:=length(lText);
  Index:=1;
  WordBuffer:='';
  LineBuffer:='';
  NeedSpace:=FLayout.NeedSpaceBeforeNextText;
  FirstIndex:=-1;
  LastIndex:=-1;

  while Index <= lLength do
    begin
    lChar:=lText[Index];
    if Not (lChar in WhiteSpace) then
      begin
      if FirstIndex<0 then
        FirstIndex:=Index;
      LastIndex:=Index;
      end
    else
      begin
      // Whitespace ends the current word
      if FirstIndex >= 0 then
        begin
        WordBuffer:=copy(lText, FirstIndex, (LastIndex - FirstIndex) + 1);
        FirstIndex:=-1;
        LastIndex:=-1;
        end;
      if length(WordBuffer)>0 then
        begin
        AddWordToLine(WordBuffer);
        WordBuffer:='';
        end;
      NeedSpace:=(lChar in [#13, #10]) or aPreserveWhitespace;
      if Not NeedSpace then
        begin
        AddWordToLine(lChar);
        NeedSpace:=False;
        end;
      if (lChar = #13) and (Index<lLength) and (lText[Index+1]=#10) then
        Inc(Index);
      end;
    Inc(Index);
    end;

  if FirstIndex >= 0 then
    begin
    WordBuffer:=copy(lText, FirstIndex, (LastIndex - FirstIndex)+1);
    FirstIndex:=-1;
    LastIndex:=-1;
    end;
  if length(WordBuffer)>0 then
    AddWordToLine(WordBuffer);
  FlushCurrentLine;
  FLayout.NeedSpaceBeforeNextText:=(lLength>0) and (lText[lLength] in WhiteSpace);
end;

function TMarkDownPDFRenderer.LayoutImage(aImageURL: UTF8String): Boolean;
var
  lPath: string;
  lIdx, pxW, pxH: Integer;
  dispW, dispH, maxW, scale: TPDFFloat;
  Item: TLayoutItem;
begin
  Result := False;
  if not Assigned(FPDFDocument) then
    exit;
  // Resolve a relative URL against the configured base directory
  lPath := aImageURL;
  if not FileExists(lPath) and (FImageBaseDir<>'') then
    lPath := IncludeTrailingPathDelimiter(FImageBaseDir) + aImageURL;
  if not FileExists(lPath) then
    exit;
  try
    lIdx := FPDFDocument.Images.AddFromFile(lPath, False);
  except
    // Unsupported/unreadable image: fall back to the alt text
    Exit(False);
  end;
  if lIdx < 0 then
    exit;
  pxW := FPDFDocument.Images[lIdx].Width;
  pxH := FPDFDocument.Images[lIdx].Height;
  if (pxW <= 0) or (pxH <= 0) then
    exit;
  // Pixels are at the target DPI; convert to PDF points (72 per inch)
  scale := cPDFPointsPerInch / fTargetDPI;
  dispW := pxW * scale;
  dispH := pxH * scale;
  // Wrap to a new line if the image does not fit in the remaining line space
  if (FLayout.LineX > 0) and (FLayout.CurrentIndent + FLayout.LineX + dispW > FLayout.MaxWidth) then
    NewLine;
  // Scale down proportionally if wider than the available content width
  maxW := FLayout.MaxWidth - FLayout.CurrentIndent;
  if (dispW > maxW) and (maxW > 0) then
    begin
    dispH := dispH * (maxW / dispW);
    dispW := maxW;
    end;
  Item := FLists.Items.NewItem(TLayoutItemKind.likImage, FLayout.CurrentIndent + FLayout.LineX + FImageMargin, FLayout.LineY);
  Item.Width := dispW;
  Item.Height := dispH;
  Item.ImageIndex := lIdx;
  Item.URL := aImageURL;
  Item.PageIndex := FLayout.CurrentPageIndex;
  // Leave a small margin on both sides of the image
  FLayout.LineX := FLayout.LineX + dispW + 2*FImageMargin;
  if FLayout.LineHeight < dispH then
    FLayout.LineHeight := dispH;
  Result := True;
end;

procedure TMarkDownPDFRenderer.RenderTextNode(aTextNode: TMarkDownTextNode; aFontSize: LongInt;
  aFontStyle: TFontStyles; const aContext : TFontContext);
var
  fontStyle: TFontStyles;
  lText,linkHref: utf8string;
  lContext : TFontContext;
begin
  if not assigned(aTextNode) then exit;
  lContext:=aContext;
  fontStyle:=aFontStyle + GetNodeFontStyle(aTextNode);
  lText:=ResolveEntities(aTextNode.NodeText);
  linkHref:='';
  if aTextNode.Kind = nkCode then
    lContext:=lContext+[fcMono,fcCode];
  case aTextNode.Kind of
    nkCode,
    nkText:
      if fcFencedCode in lContext then
        // Code block lines are rendered verbatim (exact whitespace, no wrapping)
        FlushTextRun(lText, aFontSize, fontStyle, linkHref, lContext, False)
      else
        LayoutTextWrapped(lText, aFontSize, fontStyle, linkHref, lContext, false, False);
    nkLineBreak:
      NewLine;
    nkURI, nkEmail:
      begin
      linkHref:=aTextNode.Attrs['href'];
      LayoutTextWrapped(lText, aFontSize, fontStyle + [fsUnderline], linkHref,
        lContext+[fcHyperLink], False, False);
      end;
    nkImg:
      if Not LayoutImage(aTextNode.Attrs['src']) then
        begin
        lText:=ResolveEntities(aTextNode.Attrs['alt']);
        if lText='' then
          lText:='img';
        LayoutTextWrapped('['+lText+']', aFontSize, fontStyle, '', lContext+[fcQuote], False, False);
        end;
  end;
end;

procedure TMarkDownPDFRenderer.CollectHTMLEntities;
var
  Ent : THTMLEntityDef;
  lKey : String;
begin
  // Index by lower-case name for case-insensitive lookup; the table contains
  // several names that only differ in case, so keep the first of each.
  for Ent in EntityDefList do
    begin
    lKey:=LowerCase(Ent.e);
    if FHTMLEntities.Items[lKey]='' then
      FHTMLEntities.Add(lKey, Utf8Encode(Ent.u));
    end;
end;

// Replace HTML named (&quot;) and numeric (&#NNN;) entities with their UTF-8 text
function TMarkDownPDFRenderer.ResolveEntities(const aText: String): String;

  procedure CopyToResult(aEnd, aStart : Integer);
  begin
    Result:=Result+Copy(aText, aStart, aEnd-aStart+1);
  end;

var
  lEnd, lPrev, lNext, lUnicode : Integer;
  lUChar : UnicodeChar;
  lEnt, lUTF8 : String;
begin
  if FHTMLEntities.Count=0 then
    CollectHTMLEntities;
  lPrev:=1;
  lNext:=Pos('&', aText, 1);
  if lNext=0 then
    Exit(aText);
  Result:='';
  while lNext>0 do
    begin
    lUTF8:='';
    CopyToResult(lNext-1, lPrev);
    lEnd:=Pos(';', aText, lNext+1);
    if lEnd=0 then
      begin
      // A bare '&' without a closing ';': emit it literally and move on
      Result:=Result+'&';
      lPrev:=lNext+1;
      lNext:=Pos('&', aText, lPrev);
      Continue;
      end;
    lEnt:=Copy(aText, lNext+1, lEnd-lNext-1);
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
      // Unknown entity: keep the original text unchanged
      Result:=Result+'&'+lEnt+';'
    else
      Result:=Result+lUTF8;
    lPrev:=lEnd+1;
    lNext:=Pos('&', aText, lPrev);
    end;
  CopyToResult(Length(aText), lPrev);
end;

initialization
  // Register all PDF block renderers
  TPDFMarkDownParagraphBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownHeadingBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownQuoteBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownListBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownListItemBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownCodeBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownThematicBreakBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownTextBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownTableBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);
  TPDFMarkDownTableRowBlockRenderer.RegisterRenderer(TMarkDownPDFRenderer);

  // Register text renderer
  TPDFMarkDownTextRenderer.RegisterRenderer(TMarkDownPDFRenderer);

end.
