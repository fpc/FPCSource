{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    diskfont.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit diskfont;

{$mode objfpc}

INTERFACE

uses exec, agraphics,utility;

const

    MAXFONTPATH         = 256;

type

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


const

    FCH_ID              = $0f00;
    TFCH_ID             = $0f02;
    OFCH_ID             = $0f03;



type

    pFontContentsHeader = ^tFontContentsHeader;
    tFontContentsHeader = record
        fch_FileID      : Word;
        fch_NumEntries  : Word;
    end;

const

    DFH_ID              = $0f80;
    MAXFONTNAME         = 32;

type

    pDiskFontHeader = ^tDiskFontHeader;
    tDiskFontHeader = record
        dfh_DF          : tNode;
        dfh_FileID      : Word;
        dfh_Revision    : Word;
        dfh_Segment     : Longint;
        dfh_Name        : Array [0..MAXFONTNAME-1] of Char;
        dfh_TF          : tTextFont;
    end;

const

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


type

    pAvailFonts = ^tAvailFonts;
    tAvailFonts = record
        af_Type         : Word;
        af_Attr         : tTextAttr;
    end;

    pTAvailFonts = ^tTAvailFonts;
    tTAvailFonts = record
        taf_Type        : Word;
        taf_Attr        : tTTextAttr;
    end;

    pAvailFontsHeader = ^tAvailFontsHeader;
    tAvailFontsHeader = record
        afh_NumEntries  : Word;
    end;

const
    DISKFONTNAME : PChar = 'diskfont.library';

var
  DiskfontBase: PLibrary;

function AvailFonts(Buffer: PChar; BufBytes: LongInt; Flags: LongInt): LongInt; syscall DiskfontBase 6;
procedure DisposeFontContents(FontContentsHeader: PFontContentsHeader); syscall DiskfontBase 8; 
function NewFontContents(FontsLock: BPTR; FontName: PChar): PFontContentsHeader; syscall DiskfontBase 7;
function NewScaledDiskFont(SourceFont: PTextFont; DestTextAttr: PTextAttr): PDiskFontHeader; syscall DiskfontBase 9;
function OpenDiskFont(TextAttr: PTextAttr): PTextFont; syscall DiskfontBase 5;
//function GetDiskFontCtrl(tagid: LongInt): LongInt;
//procedure SetDiskFontCtrlA(taglist: PTagItem);

implementation

initialization
  DiskfontBase := OpenLibrary(DISKFONTNAME, 36);
finalization
  CloseLibrary(DiskfontBase);
end.



