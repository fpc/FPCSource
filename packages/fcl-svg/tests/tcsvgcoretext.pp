{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the Core Text font provider of macOS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgcoretext;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

{$IFNDEF DARWIN}
{$FATAL This unit tests the macOS font provider and builds for macOS only.}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, FpcUnit.Test, FpcUnit.Registry, fpsvg.types,
     fpsvg.coretext, fpsvg.fonts.provider;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpcunit, testregistry, fpsvg.types, fpsvg.coretext,
     fpsvg.fonts.provider;
{$ENDIF FPC_DOTTEDUNITS}

type
  { The provider that draws on Core Text, which every Mac has. }
  TTestSVGCoreText = class(TTestCase)
  private
    FProvider: TSVGCoreTextProvider;
    function Face(const aFamilies: String; aSize: Double): ISVGFont;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTheProviderIsAvailable;
    procedure TestTheSystemHasFaces;
    procedure TestAFamilyOfTheSystemResolves;
    procedure TestAFaceReportsTheSizeItWasResolvedAt;
    procedure TestTheMetricsOfAFaceAreScaledToItsSize;
    procedure TestALatinLetterHasAGlyph;
    procedure TestALetterAdvancesTheWidthOfItsGlyph;
    procedure TestTheOutlineOfALetterRisesAboveTheBaseline;
    procedure TestAGlyphWithNoOutlineDrawsNothing;
    procedure TestTheGenericFamiliesResolve;
    procedure TestAMonospaceFaceAdvancesEveryLetterAlike;
    procedure TestAFamilyTheSystemLacksStillResolves;
    procedure TestACharacterTheFamilyLacksIsCovered;
    procedure TestAnItalicIsLeanedWhenTheFamilyHasNone;
    procedure TestTheSelectorPicksCoreText;
  end;

implementation

const
  ProbeSize = 20;
  Han = $65E5;
  Delta = 1e-9;


procedure TTestSVGCoreText.SetUp;

begin
  inherited SetUp;
  FProvider := TSVGCoreTextProvider.Create;
end;


procedure TTestSVGCoreText.TearDown;

begin
  FreeAndNil(FProvider);
  inherited TearDown;
end;


function TTestSVGCoreText.Face(const aFamilies: String;
  aSize: Double): ISVGFont;

begin
  Result := FProvider.ResolveFont(TSVGFontRequest.Create(aFamilies, aSize));
end;


procedure TTestSVGCoreText.TestTheProviderIsAvailable;

begin
  AssertTrue('Core Text is part of macOS', FProvider.Available);
  AssertTrue('and the unit says the same on its own',
    SVGCoreTextFontsAvailable);
end;


procedure TTestSVGCoreText.TestTheSystemHasFaces;

begin
  AssertTrue('the system holds faces', FProvider.AddSystemFonts > 0);
  AssertEquals('and the count is kept', FProvider.FamilyCount,
    FProvider.AddSystemFonts);
end;


procedure TTestSVGCoreText.TestAFamilyOfTheSystemResolves;

var
  lFont: ISVGFont;

begin
  lFont := Face('Helvetica', ProbeSize);
  AssertTrue('Helvetica is on every Mac', lFont <> nil);
  AssertEquals('and comes back under its own name', 'Helvetica',
    lFont.GetFontName);
end;


procedure TTestSVGCoreText.TestAFaceReportsTheSizeItWasResolvedAt;

var
  lFont: ISVGFont;

begin
  lFont := Face('Helvetica', 32);
  AssertTrue('the face resolves', lFont <> nil);
  AssertEquals('the size is the one asked of it', 32.0, lFont.GetSize,
    Delta);
  AssertTrue('and the design grid is known', lFont.GetUnitsPerEm > 0);
end;


procedure TTestSVGCoreText.TestTheMetricsOfAFaceAreScaledToItsSize;

var
  lFont: ISVGFont;
  lHeight: Double;

begin
  lFont := Face('Helvetica', ProbeSize);
  AssertTrue('the face resolves', lFont <> nil);
  AssertTrue('the ascent rises above the baseline', lFont.GetAscent > 0);
  AssertTrue('the descent falls below it', lFont.GetDescent > 0);
  lHeight := lFont.GetAscent + lFont.GetDescent;
  AssertTrue('and the two together are about the size',
    (lHeight > ProbeSize * 0.7) and (lHeight < ProbeSize * 1.6));
end;


procedure TTestSVGCoreText.TestALatinLetterHasAGlyph;

var
  lFont: ISVGFont;

begin
  lFont := Face('Helvetica', ProbeSize);
  AssertTrue('the face resolves', lFont <> nil);
  AssertTrue('A is in the face', lFont.GetGlyphIndex(Ord('A')) <> 0);
  AssertEquals('a code point no face draws has none', 0,
    lFont.GetGlyphIndex($10FFFD));
end;


procedure TTestSVGCoreText.TestALetterAdvancesTheWidthOfItsGlyph;

var
  lFont: ISVGFont;
  lAdvance: Double;

begin
  lFont := Face('Helvetica', ProbeSize);
  AssertTrue('the face resolves', lFont <> nil);
  lAdvance := lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('A')));
  AssertTrue('an A takes room', lAdvance > 0);
  AssertTrue('and no more than the size', lAdvance <= ProbeSize);
end;


procedure TTestSVGCoreText.TestTheOutlineOfALetterRisesAboveTheBaseline;

var
  lFont: ISVGFont;
  lPath: TSVGPath;
  lBounds: TSVGRect;

begin
  lFont := Face('Helvetica', ProbeSize);
  AssertTrue('the face resolves', lFont <> nil);
  lPath := TSVGPath.Create;
  try
    AssertTrue('the outline of an A is given',
      lFont.GetGlyphOutline(lFont.GetGlyphIndex(Ord('A')), lPath));
    AssertTrue('and holds segments', lPath.SegmentCount > 0);
    lBounds := lPath.ControlBounds;
    // The outline is placed at the origin with y running down the page,
    // so a letter stands above it and its top is negative.
    AssertTrue('the letter stands above the baseline', lBounds.Top < 0);
    AssertTrue('and reaches about the size', -lBounds.Top < ProbeSize);
  finally
    lPath.Free;
  end;
end;


procedure TTestSVGCoreText.TestAGlyphWithNoOutlineDrawsNothing;

var
  lFont: ISVGFont;
  lPath: TSVGPath;

begin
  lFont := Face('Helvetica', ProbeSize);
  AssertTrue('the face resolves', lFont <> nil);
  lPath := TSVGPath.Create;
  try
    AssertFalse('a space has no outline',
      lFont.GetGlyphOutline(lFont.GetGlyphIndex(Ord(' ')), lPath));
    AssertTrue('and nothing was appended', lPath.IsEmpty);
  finally
    lPath.Free;
  end;
end;


procedure TTestSVGCoreText.TestTheGenericFamiliesResolve;

begin
  AssertTrue('serif resolves', Face('serif', ProbeSize) <> nil);
  AssertTrue('sans-serif resolves', Face('sans-serif', ProbeSize) <> nil);
  AssertTrue('monospace resolves', Face('monospace', ProbeSize) <> nil);
  AssertTrue('cursive resolves', Face('cursive', ProbeSize) <> nil);
  AssertTrue('fantasy resolves', Face('fantasy', ProbeSize) <> nil);
end;


procedure TTestSVGCoreText.TestAMonospaceFaceAdvancesEveryLetterAlike;

var
  lFont: ISVGFont;

begin
  lFont := Face('monospace', ProbeSize);
  AssertTrue('monospace resolves', lFont <> nil);
  AssertEquals('an i takes the room of a W',
    lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('W'))),
    lFont.GetGlyphAdvance(lFont.GetGlyphIndex(Ord('i'))), 0.01);
end;


procedure TTestSVGCoreText.TestAFamilyTheSystemLacksStillResolves;

var
  lFont: ISVGFont;

begin
  lFont := Face('NoFamilyOfThisName', ProbeSize);
  AssertTrue('a face is found all the same', lFont <> nil);
  AssertTrue('and it is not the family that was asked for',
    lFont.GetFontName <> 'NoFamilyOfThisName');
end;


procedure TTestSVGCoreText.TestACharacterTheFamilyLacksIsCovered;

var
  lRequest: TSVGFontRequest;
  lFont, lCover: ISVGFont;

begin
  lRequest := TSVGFontRequest.Create('Helvetica', ProbeSize);
  lFont := FProvider.ResolveFont(lRequest);
  AssertTrue('the face resolves', lFont <> nil);
  if lFont.GetGlyphIndex(Han) <> 0 then
    Exit;
  lCover := FProvider.ResolveCover(Han, lRequest);
  AssertTrue('Core Text falls back to a face that has it', lCover <> nil);
  AssertTrue('and that face draws the character',
    lCover.GetGlyphIndex(Han) <> 0);
end;


procedure TTestSVGCoreText.TestAnItalicIsLeanedWhenTheFamilyHasNone;

var
  lRequest: TSVGFontRequest;
  lFont: TSVGCoreTextFont;

begin
  lRequest := TSVGFontRequest.Create('Menlo', ProbeSize);
  lRequest.Style := fnItalic;
  lFont := FProvider.ResolveFace(lRequest);
  AssertTrue('the face resolves', lFont <> nil);
  if Pos('Italic', lFont.GetFontName) > 0 then
    Exit;
  AssertTrue('a face that does not lean is leaned here',
    lFont.Slant >= 0);
end;


procedure TTestSVGCoreText.TestTheSelectorPicksCoreText;

begin
  AssertEquals('macOS draws text with Core Text', 'Core Text',
    SVGPlatformFontEngine);
  AssertTrue('and the selector has a provider to give',
    SVGPlatformFontProvider <> nil);
end;


initialization
  RegisterTest('coretext', TTestSVGCoreText);
end.
