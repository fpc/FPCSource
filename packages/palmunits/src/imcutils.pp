{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1997-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: IMCUtils.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *      Routines to handle Internet Mail Consortium specs
 *
 * History:
 *      8/6/97  roger - Created
 *
 **************************************************************************)

unit imcutils;

interface

uses palmos, coretraps;

const
  EOF = $ffff;

// Constants for some common IMC spec values.
  parameterDelimeterChr     = ';';
  valueDelimeterChr         = ':';
  groupDelimeterChr         = '.';
  paramaterNameDelimiterChr = '=';
  endOfLineChr              = $0D;
  imcLineSeparatorString    = '\015\012';
  imcFilenameLength         = 32;
  imcUnlimitedChars         = $FFFE; // 64K, minus 1 character for null

// These are for functions called to handle input and output.  These are currently used
// to allow disk based or obx based transfers

type
  GetCharF = function(const p: Pointer): UInt16;
  PutStringF = procedure(p: Pointer; const stringP: PChar);

// maxChars does NOT include trailing null, buffer may be 1 larger.
// use imcUnlimitedChars if you don't want a max.
function ImcReadFieldNoSemicolon(inputStream: Pointer; inputFunc: GetCharF; var c: UInt16;
                                const maxChars: UInt16): PChar; syscall sysTrapImcReadFieldNoSemicolon;

// maxChars does NOT include trailing null, buffer may be 1 larger.
// use imcUnlimitedChars if you don't want a max.
function ImcReadFieldQuotablePrintable(inputStream: Pointer; inputFunc: GetCharF; var c: UInt16;
                                       const stopAt: Char; const quotedPrintable: Boolean; const maxChars: UInt16): PChar; syscall sysTrapImcReadFieldQuotablePrintable;

procedure ImcReadPropertyParameter(inputStream: Pointer; inputFunc: GetCharF; var cP: UInt16;
                                   nameP, valueP: PChar); syscall sysTrapImcReadPropertyParameter;

procedure ImcSkipAllPropertyParameters(inputStream: Pointer; inputFunc: GetCharF; var cP: UInt16;
                                       identifierP: PChar; var quotedPrintableP: Boolean); syscall sysTrapImcSkipAllPropertyParameters;

procedure ImcReadWhiteSpace(inputStream: Pointer; inputFunc: GetCharF; var c, charAttrP: UInt16); syscall sysTrapImcReadWhiteSpace;

procedure ImcWriteQuotedPrintable(outputStream: Pointer; outputFunc: PutStringF;
                                  const stringP: PChar; const noSemicolons: Boolean); syscall sysTrapImcWriteQuotedPrintable;

procedure ImcWriteNoSemicolon(outputStream: Pointer; outputFunc: PutStringF; const stringP: PChar); syscall sysTrapImcWriteNoSemicolon;

function ImcStringIsAscii(const stringP: PChar): Boolean; syscall sysTrapImcStringIsAscii;

implementation

end.
