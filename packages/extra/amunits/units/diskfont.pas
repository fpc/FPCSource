{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit diskfont;

INTERFACE

uses exec, graphics;

Const

    MAXFONTPATH         = 256;

Type

    pFontContents = ^tFontContents;
    tFontContents = record
        fc_FileName     : Array [0..MAXFONTPATH-1] of Char;
        fc_YSize        : Word;
        fc_Style        : Byte;
        fc_Flags        : Byte;
    end;


   pTFontContents = ^tTFontContents;
   tTFontContents = record
    tfc_FileName  : Array[0..MAXFONTPATH-3] of Char;
    tfc_TagCount  : Word;

    tfc_YSize     : Word;
    tfc_Style,
    tfc_Flags     : Byte;
   END;


Const

    FCH_ID              = $0f00;
    TFCH_ID             = $0f02;
    OFCH_ID             = $0f03;



Type

    pFontContentsHeader = ^tFontContentsHeader;
    tFontContentsHeader = record
        fch_FileID      : Word;
        fch_NumEntries  : Word;
    end;

Const

    DFH_ID              = $0f80;
    MAXFONTNAME         = 32;

Type

    pDiskFontHeader = ^tDiskFontHeader;
    tDiskFontHeader = record
        dfh_DF          : tNode;
        dfh_FileID      : Word;
        dfh_Revision    : Word;
        dfh_Segment     : Longint;
        dfh_Name        : Array [0..MAXFONTNAME-1] of Char;
        dfh_TF          : tTextFont;
    end;

Const

    AFB_MEMORY          = 0;
    AFF_MEMORY          = 1;
    AFB_DISK            = 1;
    AFF_DISK            = 2;
    AFB_SCALED          = 2;
    AFF_SCALED          = $0004;
    AFB_BITMAP          = 3;
    AFF_BITMAP          = $0008;
    AFB_TAGGED          = 16;
    AFF_TAGGED          = $10000;


Type

    pAvailFonts = ^tAvailFonts;
    tAvailFonts = record
        af_Type         : Word;
        af_Attr         : tTextAttr;
    end;

    pTAvailFonts = ^tTAvailFonts;
    tTAvailFonts = record
        taf_Type        : Word;
        taf_Attr        : tTTextAttr;
    END;

    pAvailFontsHeader = ^tAvailFontsHeader;
    tAvailFontsHeader = record
        afh_NumEntries  : Word;
    end;

VAR DiskfontBase : pLibrary;

FUNCTION AvailFonts(buffer : pCHAR; bufBytes : LONGINT; flags : LONGINT) : LONGINT;
PROCEDURE DisposeFontContents(fontContentsHeader : pFontContentsHeader);
FUNCTION NewFontContents(fontsLock : BPTR; fontName : pCHAR) : pFontContentsHeader;
FUNCTION NewScaledDiskFont(sourceFont : pTextFont; destTextAttr : pTextAttr) : pDiskFontHeader;
FUNCTION OpenDiskFont(textAttr : pTextAttr) : pTextFont;

IMPLEMENTATION

FUNCTION AvailFonts(buffer : pCHAR; bufBytes : LONGINT; flags : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L buffer,A0
    MOVE.L  bufBytes,D0
    MOVE.L  flags,D1
    MOVEA.L DiskfontBase,A6
    JSR -036(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE DisposeFontContents(fontContentsHeader : pFontContentsHeader);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L fontContentsHeader,A1
    MOVEA.L DiskfontBase,A6
    JSR -048(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION NewFontContents(fontsLock : BPTR; fontName : pCHAR) : pFontContentsHeader;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L fontsLock,A0
    MOVEA.L fontName,A1
    MOVEA.L DiskfontBase,A6
    JSR -042(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION NewScaledDiskFont(sourceFont : pTextFont; destTextAttr : pTextAttr) : pDiskFontHeader;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sourceFont,A0
    MOVEA.L destTextAttr,A1
    MOVEA.L DiskfontBase,A6
    JSR -054(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION OpenDiskFont(textAttr : pTextAttr) : pTextFont;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L textAttr,A0
    MOVEA.L DiskfontBase,A6
    JSR -030(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

END. (* UNIT DISKFONT *)




