{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    MorphOS version (c) 2015 by Karoly Balogh

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}

unit diskfont;

INTERFACE

uses exec, agraphics, utility;

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

const
    DISKFONTNAME : PChar = 'diskfont.library';

VAR DiskfontBase : pLibrary = nil;

FUNCTION AvailFonts(buffer : pCHAR location 'a0'; bufBytes : LONGINT location 'd0'; flags : LONGINT location 'd1') : LONGINT; syscall DiskfontBase 036;
PROCEDURE DisposeFontContents(fontContentsHeader : pFontContentsHeader location 'a1'); syscall DiskfontBase 048;
FUNCTION NewFontContents(fontsLock : BPTR location 'a0'; fontName : pCHAR location 'a1') : pFontContentsHeader; syscall DiskfontBase 042;
FUNCTION NewScaledDiskFont(sourceFont : pTextFont location 'a0'; destTextAttr : pTextAttr location 'a1') : pDiskFontHeader; syscall DiskfontBase 054;
FUNCTION OpenDiskFont(textAttr : pTextAttr location 'a0') : pTextFont; syscall DiskfontBase 030;
{ MorphOS actually supports these V45+ calls }
FUNCTION GetDiskFontCtrl(tagid : LONGINT location 'd0') : LONGINT; syscall DiskfontBase 060;
PROCEDURE SetDiskFontCtrlA(taglist : pTagItem location 'a0'); syscall DiskfontBase 066;

function InitDISKFONTLibrary: boolean;

IMPLEMENTATION

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

function InitDiskFontLibrary: boolean;
begin
  InitDiskFontLibrary := Assigned(DiskFontBase);
end;

initialization
  DiskFontBase := OpenLibrary(DISKFONTNAME,LIBVERSION);
finalization
  if Assigned(DiskFontBase) then
    CloseLibrary(DiskFontBase);
END.
