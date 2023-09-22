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

{$IFNDEF FPC_DOTTEDUNITS}
unit diskfont;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses Amiga.Core.Exec, Amiga.Core.Agraphics,Amiga.Core.Utility;
{$ELSE FPC_DOTTEDUNITS}
uses exec, agraphics,utility;
{$ENDIF FPC_DOTTEDUNITS}

const
  MAXFONTPATH = 256;

type
  PFontContents = ^TFontContents;
  TFontContents = record
    fc_FileName: array[0..MAXFONTPATH - 1] of AnsiChar;
    fc_YSize: Word;
    fc_Style: Byte;
    fc_Flags: Byte;
  end;

  PTFontContents = ^TTFontContents;
  TTFontContents = record
    tfc_FileName: array[0..MAXFONTPATH - 3] of AnsiChar;
    tfc_TagCount: Word;
    tfc_YSize: Word;
    tfc_Style,
    tfc_Flags: Byte;
   end;

const
  FCH_ID   = $0f00;
  TFCH_ID  = $0f02;
  OFCH_ID  = $0f03;

type
  PFontContentsHeader = ^TFontContentsHeader;
  TFontContentsHeader = record
    fch_FileID: Word;
    fch_NumEntries: Word;
  end;

const
  DFH_ID = $0f80;
  MAXFONTNAME = 32;

type
  PDiskFontHeader = ^TDiskFontHeader;
  TDiskFontHeader = record
    dfh_DF: TNode;
    dfh_FileID: Word;
    dfh_Revision: Word;
    dfh_Segment: BPTR;
    dfh_Name: array [0..MAXFONTNAME-1] of AnsiChar;
    dfh_TF: TTextFont;
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
  PAvailFonts = ^TAvailFonts;
  TAvailFonts = record
    af_Type: Word;
    af_Attr: TTextAttr;
  end;

  PTAvailFonts = ^TTAvailFonts;
  TTAvailFonts = record
    taf_Type: Word;
    taf_Attr: TTTextAttr;
  end;

  PAvailFontsHeader = ^TAvailFontsHeader;
  TAvailFontsHeader = record
    afh_NumEntries: Word;
  end;

const
  DISKFONTNAME: PAnsiChar = 'diskfont.library';

var
  DiskfontBase: PLibrary;

function AvailFonts(Buffer: PAnsiChar; BufBytes: LongInt; Flags: LongInt): LongInt; syscall DiskfontBase 6;
procedure DisposeFontContents(FontContentsHeader: PFontContentsHeader); syscall DiskfontBase 8;
function NewFontContents(FontsLock: BPTR; FontName: PAnsiChar): PFontContentsHeader; syscall DiskfontBase 7;
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



