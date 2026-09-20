{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    A font of square glyphs with exact metrics, for tests that need no face.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit svgstubfont;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, fpsvg.types;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpsvg.types;
{$ENDIF FPC_DOTTEDUNITS}

const
  StubAdvanceRatio = 0.5;
  StubAscentRatio = 0.8;
  StubDescentRatio = 0.2;

type
  { Every glyph is a square half an em wide. A run of n characters
    advances exactly n half sizes. }
  TSVGStubFont = class(TObject, ISVGFont)
  private
    FName: String;
    FSize: Double;
    FLimit: Cardinal;
  public
    constructor Create(const aName: String; aSize: Double;
      aLimit: Cardinal = 0);
    function GetFontName: TSVGString;
    function GetUnitsPerEm: Integer;
    function GetSize: Double;
    function GetAscent: Double;
    function GetDescent: Double;
    function GetUnderlinePosition: Double;
    function GetUnderlineThickness: Double;
    function GetGlyphIndex(aCodePoint: Cardinal): Cardinal;
    function GetGlyphForRun(const aCodes: TSVGCodePointArray; aFrom: Integer;
      out aCount: Integer): Cardinal;
    function GetGlyphNamed(const aName: TSVGString): Cardinal;
    function GetGlyphAdvance(aGlyph: Cardinal): Double;
    function GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;
    procedure GetGlyphVerticalOrigin(aGlyph: Cardinal;
      out aX, aY: Double);
    function GetGlyphKerning(aLeft, aRight: Cardinal): Double;
    function GetGlyphVerticalKerning(aAbove,
      aBelow: Cardinal): Double;
    function GetSmallCaps: Boolean;
    function GetGlyphOutline(aGlyph: Cardinal; aPath: TSVGPath): Boolean;
  end;

  { Returns a stub font for every request, whatever the family. }
  TSVGStubFontProvider = class(TObject, ISVGFontProvider)
  private
    FFonts: array of TSVGStubFont;
    FRequests: Integer;
    FLastRequest: String;
    FLastStretch: TSVGFontStretch;
    FRefuse: Boolean;
    FResources: String;
    FLimit: Cardinal;
    FCoverName: String;
    FCovered: Integer;
  public
    destructor Destroy; override;
    function ResolveFont(const aRequest: TSVGFontRequest): ISVGFont;
    function AddFontResource(const aFamily: TSVGString; aWeight: Integer;
      aStyle: TSVGFontStyle; const aFileName: String): Boolean;
    function ResolveCover(aCodePoint: Cardinal;
      const aRequest: TSVGFontRequest): ISVGFont;
    // Number of requests answered.
    property Requests: Integer read FRequests;
    // The families of the most recent request.
    property LastRequest: String read FLastRequest;
    // The width the most recent request specified.
    property LastStretch: TSVGFontStretch read FLastStretch;
    // When set, every request resolves to nothing.
    property Refuse: Boolean read FRefuse write FRefuse;
    // The families that a font-face rule offered a file for, one per
    // line.
    property Resources: String read FResources;
    // Code points above this have no glyph in the fonts returned. Zero
    // returns fonts that cover everything.
    property Limit: Cardinal read FLimit write FLimit;
    // The name of the face that covers the code points the others do not.
    // Empty covers nothing, like a provider with no coverage source.
    property CoverName: String read FCoverName write FCoverName;
    // Number of characters a cover was requested for.
    property Covered: Integer read FCovered;
  end;

implementation

{ TSVGStubFont }

constructor TSVGStubFont.Create(const aName: String; aSize: Double;
  aLimit: Cardinal = 0);

begin
  inherited Create;
  FName := aName;
  FSize := aSize;
  FLimit := aLimit;
end;


function TSVGStubFont.GetFontName: TSVGString;

begin
  Result := FName;
end;


function TSVGStubFont.GetUnitsPerEm: Integer;

begin
  Result := 1000;
end;


function TSVGStubFont.GetSize: Double;

begin
  Result := FSize;
end;


function TSVGStubFont.GetAscent: Double;

begin
  Result := FSize * StubAscentRatio;
end;


function TSVGStubFont.GetDescent: Double;

begin
  Result := FSize * StubDescentRatio;
end;


// The stub says nothing about its lines, so a caller places them itself.
function TSVGStubFont.GetUnderlinePosition: Double;

begin
  Result := 0;
end;


function TSVGStubFont.GetUnderlineThickness: Double;

begin
  Result := 0;
end;


function TSVGStubFont.GetGlyphIndex(aCodePoint: Cardinal): Cardinal;

begin
  if (FLimit > 0) and (aCodePoint > FLimit) then
    Result := 0
  else if aCodePoint = Ord(' ') then
    Result := 1
  else
    Result := aCodePoint;
end;


// The stub draws one code point at a time and gives its glyphs no
// names.
function TSVGStubFont.GetGlyphForRun(const aCodes: TSVGCodePointArray;
  aFrom: Integer; out aCount: Integer): Cardinal;

begin
  aCount := 1;
  Result := 0;
  if aFrom < Length(aCodes) then
    Result := GetGlyphIndex(aCodes[aFrom]);
end;


function TSVGStubFont.GetGlyphNamed(const aName: TSVGString): Cardinal;

begin
  if aName = '' then ;
  Result := 0;
end;


function TSVGStubFont.GetGlyphAdvance(aGlyph: Cardinal): Double;

begin
  Result := FSize * StubAdvanceRatio;
end;


function TSVGStubFont.GetGlyphVerticalAdvance(aGlyph: Cardinal): Double;

begin
  Result := FSize;
end;


procedure TSVGStubFont.GetGlyphVerticalOrigin(aGlyph: Cardinal;
  out aX, aY: Double);

begin
  aX := -FSize * StubAdvanceRatio / 2;
  aY := FSize * StubAscentRatio;
end;


function TSVGStubFont.GetGlyphKerning(aLeft, aRight: Cardinal): Double;

begin
  Result := 0;
end;


function TSVGStubFont.GetGlyphVerticalKerning(aAbove,
  aBelow: Cardinal): Double;

begin
  Result := 0;
end;


function TSVGStubFont.GetSmallCaps: Boolean;

begin
  Result := False;
end;


function TSVGStubFont.GetGlyphOutline(aGlyph: Cardinal;
  aPath: TSVGPath): Boolean;

var
  lWidth, lHeight: Double;

begin
  Result := aGlyph <> 1;
  if not Result then
    Exit;
  lWidth := FSize * StubAdvanceRatio;
  lHeight := FSize * StubAscentRatio;
  aPath.MoveTo(0, 0);
  aPath.LineTo(lWidth, 0);
  aPath.LineTo(lWidth, -lHeight);
  aPath.LineTo(0, -lHeight);
  aPath.Close;
end;


{ TSVGStubFontProvider }

destructor TSVGStubFontProvider.Destroy;

var
  I: Integer;

begin
  for I := 0 to High(FFonts) do
    FFonts[I].Free;
  FFonts := nil;
  inherited Destroy;
end;


function TSVGStubFontProvider.AddFontResource(const aFamily: TSVGString;
  aWeight: Integer; aStyle: TSVGFontStyle;
  const aFileName: String): Boolean;

begin
  // The stub reads no file. It records the offer so a test can see that
  // the renderer made it.
  Result := aFileName <> '';
  if not Result then
    Exit;
  FResources := FResources + Format('%s %d %d %s', [aFamily, aWeight,
    Ord(aStyle), ExtractFileName(aFileName)]) + LineEnding;
end;


function TSVGStubFontProvider.ResolveFont(
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lFont: TSVGStubFont;

begin
  Inc(FRequests);
  FLastRequest := aRequest.Families;
  FLastStretch := aRequest.Stretch;
  Result := nil;
  if FRefuse or (aRequest.Size <= 0) then
    Exit;
  lFont := TSVGStubFont.Create('stub', aRequest.Size, FLimit);
  SetLength(FFonts, Length(FFonts) + 1);
  FFonts[High(FFonts)] := lFont;
  Result := lFont;
end;


function TSVGStubFontProvider.ResolveCover(aCodePoint: Cardinal;
  const aRequest: TSVGFontRequest): ISVGFont;

var
  lFont: TSVGStubFont;

begin
  Inc(FCovered);
  Result := nil;
  if (FCoverName = '') or (aRequest.Size <= 0) then
    Exit;
  lFont := TSVGStubFont.Create(FCoverName, aRequest.Size);
  SetLength(FFonts, Length(FFonts) + 1);
  FFonts[High(FFonts)] := lFont;
  Result := lFont;
end;


end.
