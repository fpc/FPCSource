{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:
    
    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening
    of the library.
    13 Jan 2003.
    
    nils.sjoholm@mailbox.swipnet.se Nils Sjoholm
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

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

const
    DISKFONTNAME : PChar = 'diskfont.library';

VAR DiskfontBase : pLibrary;

FUNCTION AvailFonts(buffer : pCHAR; bufBytes : LONGINT; flags : LONGINT) : LONGINT;
PROCEDURE DisposeFontContents(fontContentsHeader : pFontContentsHeader);
FUNCTION NewFontContents(fontsLock : BPTR; fontName : pCHAR) : pFontContentsHeader;
FUNCTION NewScaledDiskFont(sourceFont : pTextFont; destTextAttr : pTextAttr) : pDiskFontHeader;
FUNCTION OpenDiskFont(textAttr : pTextAttr) : pTextFont;

IMPLEMENTATION

uses msgbox;

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

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of diskfont.library}

var
    diskfont_exit : Pointer;

procedure ClosediskfontLibrary;
begin
    ExitProc := diskfont_exit;
    if DiskfontBase <> nil then begin
        CloseLibrary(DiskfontBase);
        DiskfontBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : Cardinal = 0;

begin
    DiskfontBase := nil;
    DiskfontBase := OpenLibrary(DISKFONTNAME,LIBVERSION);
    if DiskfontBase <> nil then begin
        diskfont_exit := ExitProc;
        ExitProc := @ClosediskfontLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open diskfont.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of diskfont.library compiled}
   {$Info Make sure you open diskfont.library yourself}
{$endif use_auto_openlib}

END. (* UNIT DISKFONT *)

{
  $Log$
  Revision 1.3  2003-01-14 18:46:04  nils
  * added defines use_amia_smartlink and use_auto_openlib

  * implemented autoopening of library

  Revision 1.2  2002/11/18 20:52:58  nils
    * added diskfontname

}
  


