(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Font.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines font structures and routines.
 *
 * History:
 *    09/13/94 art   Created by Art Lamb.
 *    05/05/98 art   Add structures for font mapping table.
 *    07/03/98 kwk   Added FntWidthToOffset.
 *    10/23/98 kwk   Changed fontMapTable to 0xC000 (was 0xFFFF).
 *    10/20/99 kwk   Moved private values to FontPrv.h
 *    05/12/00 kwk   Added FntWCharWidth.
 *
 *****************************************************************************)
{$MACRO ON}
unit font;

interface

uses palmos, coretraps;

// Pixel width of tab stops in fields
const
  fntTabChrWidth = 20;

// Width of character missing from font.
const
  fntMissingChar = -1;

type
  FontCharInfoType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FONTS} // These fields will not be available in the next OS release!
    offset: Int8;
    width: Int8;
  {$endif}
  end;
  FontCharInfoTag = FontCharInfoType;
  FontCharInfoPtr = ^FontCharInfoType;

type
  FontType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FONTS} // These fields will not be available in the next OS release!
    fontType: Int16;    // font type
    firstChar: Int16;   // ASCII code of first character
    lastChar: Int16;    // ASCII code of last character
    maxWidth: Int16;    // maximum character width
    kernMax: Int16;     // negative of maximum character kern
    nDescent: Int16;    // negative of descent
    fRectWidth: Int16;  // width of font rectangle
    fRectHeight: Int16; // height of font rectangle
    owTLoc: Int16;      // offset to offset/width table
    ascent: Int16;      // ascent
    descent: Int16;     // descent
    leading: Int16;     // leading
    rowWords: Int16;    // row width of bit image / 2
  {$endif}
  end;
  FontTag = FontType;
  FontPtr = ^FontType;
  FontTablePtr = ^FontPtr;

type
  FontID = Enum;

const
  stdFont = $00;                    // Small font used for the user's writing.  Shows a good amount
  boldFont = Succ(stdFont);         // Small font.  Bold for easier reading.  Used often for ui.
  largeFont = Succ(boldFont);       // Larger font for easier reading.  Shows a lot less.
  symbolFont = Succ(largeFont);     // Various ui images like check boxes and arrows
  symbol11Font = Succ(symbolFont);  // Larger various ui images
  symbol7Font = Succ(symbol11Font); // Smaller various ui images
  ledFont = Succ(symbol7Font);      // Calculator specific font
  largeBoldFont = Succ(ledFont);    // A thicker version of the large font.  More readable.
  fntAppFontCustomBase = $80;       // First available application-defined font ID

const
  checkboxFont = symbol11Font;

function FntIsAppDefined(fnt: FontID): Boolean;

//--------------------------------------------------------------------
//
// Font Function
//
//--------------------------------------------------------------------

function FntGetFont: FontID; syscall sysTrapFntGetFont;

function FntSetFont(font: FontID): FontID; syscall sysTrapFntSetFont;

function FntGetFontPtr: FontPtr; syscall sysTrapFntGetFontPtr;

function FntBaseLine: Int16; syscall sysTrapFntBaseLine;

function FntCharHeight: Int16; syscall sysTrapFntCharHeight;

function FntLineHeight: Int16; syscall sysTrapFntLineHeight;

function FntAverageCharWidth: Int16; syscall sysTrapFntAverageCharWidth;

function FntCharWidth(ch: Char): Int16; syscall sysTrapFntCharWidth;

function FntWCharWidth(iChar: WChar): Int16; syscall sysTrapFntWCharWidth;

function FntCharsWidth(const chars: PChar; len: Int16): Int16; syscall sysTrapFntCharsWidth;

function FntWidthToOffset(const pChars: PChar; length: UInt16; pixelWidth: Int16; var leadingEdge: Boolean; var truncWidth: Int16): Int16; syscall sysTrapFntWidthToOffset;

procedure FntCharsInWidth(const AString: PChar; var stringWidthP, stringLengthP: Int16;
                          var fitWithinWidth: Boolean); syscall sysTrapFntCharsInWidth;

function FntDescenderHeight: Int16; syscall sysTrapFntDescenderHeight;

function FntLineWidth(const pChars: PChar; length: UInt16): Int16; syscall sysTrapFntLineWidth;

function FntWordWrap(const chars: PChar; maxWidth: UInt16): UInt16; syscall sysTrapFntWordWrap;

procedure FntWordWrapReverseNLines(const chars: PChar; maxWidth: UInt16; var linesToScrollP, scrollPosP: UInt16); syscall sysTrapFntWordWrapReverseNLines;

procedure FntGetScrollValues(const chars: PChar; width, scrollPos: UInt16; var linesP, topLine: UInt16); syscall sysTrapFntGetScrollValues;

function FntDefineFont(font: FontID; fontP: FontPtr): Err; syscall sysTrapFntDefineFont;

implementation

function FntIsAppDefined(fnt: FontID): Boolean;
begin
  FntIsAppDefined := fnt >= fntAppFontCustomBase;
end;

end.
