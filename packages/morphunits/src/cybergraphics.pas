{
  This file is part of the Free Pascal MorphOS support package

  Copyright (c) 2015 Karoly Balogh
  member of the Free Pascal Development Team

  cybergraphics.library interface unit

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$mode fpc}
{$packrecords 2}

UNIT CYBERGRAPHICS;

INTERFACE

USES
  exec,agraphics,utility;

VAR CyberGfxBase : pLibrary = nil;

const
    CYBERGRAPHICSNAME : PChar = 'cybergraphics.library';

{
    Contents of this file is based on cybergraphics.h from the MorphOS SDK:

        $VER: cybergraphics.h 50.12 (13.08.2008)
        include file for cybergraphics.library
        Copyright (c) 1996-2008 by Vision Factory Development
        All Rights reserved.
}

  const
     CYBERGFXNAME = 'cybergraphics.library';
     CYBERGFX_INCLUDE_VERSION = 41;
  {
      Definition of CyberModeNode (Returned in AllocModeList)
                                                               }
  type
     PCyberModeNode = ^tCyberModeNode;
     tCyberModeNode = record
          Node : tNode;
          ModeText : array[0..(DISPLAYNAMELEN)-1] of char; { name for this mode  }
          DisplayID : ULONG;                               { display id associated with the node  }
          Width : UWORD;                                   { visible width  }
          Height : UWORD;                                  { visible height  }
          Depth : UWORD;                                   { display depth  }
          DisplayTagList : PTagItem;                       { taglist with extended ModeID information  }
       end;

  {
     Parameters for GetCyberMapAttr()
                                       }
  const
  { function returns BytesPerRow if its called with this parameter  }
     CYBRMATTR_XMOD = $80000001;
  { BytesPerPixel shall be returned  }
     CYBRMATTR_BPPIX = $80000002;
  { do not use this ! private tag  }
     CYBRMATTR_DISPADR = $80000003;
  { the pixel format is returned  }
     CYBRMATTR_PIXFMT = $80000004;
  { returns width in pixels  }
     CYBRMATTR_WIDTH = $80000005;
  { returns height in lines  }
     CYBRMATTR_HEIGHT = $80000006;
  { returns bits per pixel  }
     CYBRMATTR_DEPTH = $80000007;
  { returns -1 if supplied bitmap is a cybergfx one  }
     CYBRMATTR_ISCYBERGFX = $80000008;
  { returns -1 if supplied bitmap is linear accessable  }
     CYBRMATTR_ISLINEARMEM = $80000009;
  { returns colormap associated with that bitmap (v50) }
     CYBRMATTR_COLORMAP = $8000000A;
  {
     Parameters for GetCyberIDAttr()
                                      }
  { the pixel format is returned  }
     CYBRIDATTR_PIXFMT = $80000001;
  { returns visible width in pixels  }
     CYBRIDATTR_WIDTH = $80000002;
  { returns visible height in lines  }
     CYBRIDATTR_HEIGHT = $80000003;
  { returns bits per pixel  }
     CYBRIDATTR_DEPTH = $80000004;
  { BytesPerPixel shall be returned  }
     CYBRIDATTR_BPPIX = $80000005;
  {
     Tags for CyberModeRequest()
                                   }
     CYBRMREQ_TB = TAG_USER + $40000;
  {
     FilterTags
                 }
  { Minimum depth for displayed screenmode  }
     CYBRMREQ_MinDepth = CYBRMREQ_TB + 0;
  { Maximum depth  "       "        "  }
     CYBRMREQ_MaxDepth = CYBRMREQ_TB + 1;
  { Minumum width  "       "        "  }
     CYBRMREQ_MinWidth = CYBRMREQ_TB + 2;
  { Maximum width  "       "        "  }
     CYBRMREQ_MaxWidth = CYBRMREQ_TB + 3;
  { Minumum height "       "        "  }
     CYBRMREQ_MinHeight = CYBRMREQ_TB + 4;
  { Minumum height "       "        "  }
     CYBRMREQ_MaxHeight = CYBRMREQ_TB + 5;
     CYBRMREQ_CModelArray = CYBRMREQ_TB + 6;
     CYBRMREQ_WinTitle = CYBRMREQ_TB + 20;
     CYBRMREQ_OKText = CYBRMREQ_TB + 21;
     CYBRMREQ_CancelText = CYBRMREQ_TB + 22;
  { Screen you wish the Requester to open on  }
     CYBRMREQ_Screen = CYBRMREQ_TB + 30;
  {
     Tags for BestCyberModeID()
                                 }
     CYBRBIDTG_TB = TAG_USER + $50000;
  { FilterTags  }
     CYBRBIDTG_Depth = CYBRBIDTG_TB + 0;
     CYBRBIDTG_NominalWidth = CYBRBIDTG_TB + 1;
     CYBRBIDTG_NominalHeight = CYBRBIDTG_TB + 2;
     CYBRBIDTG_MonitorID = CYBRBIDTG_TB + 3;
     CYBRBIDTG_BoardName = CYBRBIDTG_TB + 5;
  {
     definition of divers pixel formats
                                         }
     PIXFMT_LUT8 = 0;
     PIXFMT_RGB15 = 1;
     PIXFMT_RGB15X = 2;
     PIXFMT_BGR15 = 2;
     PIXFMT_RGB15PC = 3;
     PIXFMT_BGR15PC = 4;
     PIXFMT_RGB16 = 5;
     PIXFMT_BGR16 = 6;
     PIXFMT_RGB16PC = 7;
     PIXFMT_BGR16PC = 8;
     PIXFMT_RGB24 = 9;
     PIXFMT_BGR24 = 10;
     PIXFMT_ARGB32 = 11;
     PIXFMT_BGRA32 = 12;
     PIXFMT_RGBA32 = 13;
  {
     SrcRectangle formats defines for xxxPixelArray calls()
                                                             }
     RECTFMT_RGB = 0;
     RECTFMT_RGBA = 1;
     RECTFMT_ARGB = 2;
     RECTFMT_LUT8 = 3;
     RECTFMT_GREY8 = 4;
     RECTFMT_RAW = 5;
  {
     Parameters for CVideoCtrlTagList()
                                         }
     SETVC_DPMSLevel = $88002001;
  { Full operation                              }
     DPMS_ON = 0;
  { Optional state of minimal power reduction   }
     DPMS_STANDBY = 1;
  { Significant reduction of power consumption  }
     DPMS_SUSPEND = 2;
  { Lowest level of power consumption           }
     DPMS_OFF = 3;
  {
     Tags for LockBitMapTagList()
                                   }
     LBMI_WIDTH = $84001001;
     LBMI_HEIGHT = $84001002;
     LBMI_DEPTH = $84001003;
     LBMI_PIXFMT = $84001004;
     LBMI_BYTESPERPIX = $84001005;
     LBMI_BYTESPERROW = $84001006;
     LBMI_BASEADDRESS = $84001007;
  {
     Tags for UnLockBitMapTagList()
                                     }
     UBMI_UPDATERECTS = $85001001;
     UBMI_REALLYUNLOCK = $85001002;
  {
     Message passed to the DoCDrawMethodTagList() hook function
                                                                 }

  type
     PCDrawMsg = ^tCDrawMsg;
     tCDrawMsg = record
          cdm_MemPtr : APTR;
          cdm_offx : ULONG;
          cdm_offy : ULONG;
          cdm_xsize : ULONG;
          cdm_ysize : ULONG;
          cdm_BytesPerRow : UWORD;
          cdm_BytesPerPix : UWORD;
          cdm_ColorModel : UWORD;
       end;

  {
     Colour Table source formats for WriteLUTPixelArray()
                                                         }
  { ULONG [] table  }

  const
     CTABFMT_XRGB8 = 0;
  {
        graphics.library/AllocBitMap() extended flags
                                                         }
     BMB_SPECIALFMT = 7;
     BMF_SPECIALFMT = 1 shl BMB_SPECIALFMT;

     BMF_REQUESTVMEM = (BMF_MINPLANES or BMF_DISPLAYABLE);

     BMB_ROOTMAP = 5;
     BMF_ROOTMAP = 1 shl BMB_ROOTMAP;

     BMB_3DTARGET = 8;
     BMF_3DTARGET = 1  shl BMB_3DTARGET;

{*
 * Operations for ProcessPixelArray() (v50)
 *
 *}
  const
     POP_BRIGHTEN        =    0;
     POP_DARKEN          =    1;
     POP_SETALPHA        =    2;
     POP_TINT            =    3;
     POP_BLUR            =    4;
     POP_COLOR2GREY      =    5;
     POP_NEGATIVE        =    6;
     POP_NEGFADE         =    7;
     POP_TINTFADE        =    8;
     POP_GRADIENT        =    9;
     POP_SHIFTRGB        =    10;

{*
 * Values for POP_SHIFTRGB
 *
 *}
  const
     RGBSHIFT_BGR        =     1;
     RGBSHIFT_BRG        =     2;
     RGBSHIFT_GBR        =     3;
     RGBSHIFT_GRB        =     4;
     RGBSHIFT_RBG        =     5;

{*
 * Tags for ProcessPixelArray() ops
 *
 *}
  const
     PPAOPTAG_FADEFULLSCALE = $85231020;
     PPAOPTAG_FADEOFFSET    = $85231021;

     PPAOPTAG_GRADIENTTYPE  =                $85231022;

     GRADTYPE_HORIZONTAL    =                0;
     GRADTYPE_VERTICAL      =                1;

{* yet unsupported gradient types follow *}
     GRADTYPE_RECTANGLE     =                2;
     GRADTYPE_LINEAR_ANGLE  =                3;
     GRADTYPE_RADIAL        =                4; {* "circle" center-based *}

     GRADIENT_NUMTYPES      =                2;

     PPAOPTAG_GRADCOLOR1    =                $85231023;
     PPAOPTAG_GRADCOLOR2    =                $85231024;

     PPAOPTAG_GRADFULLSCALE =                PPAOPTAG_FADEFULLSCALE;
     PPAOPTAG_GRADOFFSET    =                PPAOPTAG_FADEOFFSET;

     PPAOPTAG_RGBMASK       =                $85231025;

     PPAOPTAG_GRADSYMCENTER =                $85231026;

{*
 * Tags for BltBitMap(RastPort)Alpha() (v50)
 *
 *}
  const
     BLTBMA_MIXLEVEL        = $88802000;     {* from 0(0%) to 0xFFFFFFFF (100%) *}
     BLTBMA_USESOURCEALPHA  = $88802001;
     BLTBMA_GLOBALALPHA     = BLTBMA_MIXLEVEL;
     BLTBMA_DESTALPHAVALUE  = $88802002;

     DESTALPHAVALUE_UNDEFINED  =  0; {* default *}
     DESTALPHAVALUE_ONE        =  1;
     DESTALPHAVALUE_USESOURCE  =  2;
     DESTALPHAVALUE_USEDEST    =  3;



FUNCTION AllocCModeListTagList(ModeListTags : pTagItem location 'a1') : pList; syscall CyberGfxBase 072;
FUNCTION BestCModeIDTagList(BestModeIDTags : pTagItem location 'a0') : longword; syscall CyberGfxBase 060;
FUNCTION CModeRequestTagList(ModeRequest : POINTER location 'a0'; ModeRequestTags : pTagItem location 'a1') : longword; syscall CyberGfxBase 066;
PROCEDURE CVideoCtrlTagList(ViewPort : pViewPort location 'a0'; TagList : pTagItem location 'a1'); syscall CyberGfxBase 162;
PROCEDURE DoCDrawMethodTagList(Hook : pHook location 'a0'; a1arg : pRastPort location 'a1'; TagList : pTagItem location 'a2'); syscall CyberGfxBase 156;
FUNCTION ExtractColor(a0arg : pRastPort location 'a0'; BitMap : pBitMap location 'a1'; Colour : longword location 'd0'; SrcX : longword location 'd1'; SrcY : longword location 'd2'; Width : longword location 'd3'; Height : longword location 'd4') : longword; syscall CyberGfxBase 186;
FUNCTION FillPixelArray(a1arg : pRastPort location 'a1'; DestX : WORD location 'd0'; DestY : WORD location 'd1'; SizeX : WORD location 'd2'; SizeY : WORD location 'd3'; ARGB : longword location 'd4') : longword; syscall CyberGfxBase 150;
PROCEDURE FreeCModeList(ModeList : pList location 'a0'); syscall CyberGfxBase 078;
FUNCTION GetCyberIDAttr(CyberIDAttr : longword location 'd0'; CyberDisplayModeID : longword location 'd1') : longword; syscall CyberGfxBase 102;
FUNCTION GetCyberMapAttr(CyberGfxBitmap : pBitMap location 'a0'; CyberAttrTag : longword location 'd0') : longword; syscall CyberGfxBase 096;
FUNCTION InvertPixelArray(a1arg : pRastPort location 'a1'; DestX : WORD location 'd0'; DestY : WORD location 'd1'; SizeX : WORD location 'd2'; SizeY : WORD location 'd3') : longword; syscall CyberGfxBase 144;
FUNCTION IsCyberModeID(displayID : longword location 'd0') : wordbool; syscall CyberGfxBase 054;
FUNCTION LockBitMapTagList(BitMap : POINTER location 'a0'; TagList : pTagItem location 'a1') : POINTER; syscall CyberGfxBase 168;
FUNCTION MovePixelArray(SrcX : WORD location 'd0'; SrcY : WORD location 'd1'; a1arg : pRastPort location 'a1'; DestX : WORD location 'd2'; DestY : WORD location 'd3'; SizeX : WORD location 'd4'; SizeY : WORD location 'd5') : longword; syscall CyberGfxBase 132;
FUNCTION ReadPixelArray(destRect : POINTER location 'a0'; destX : WORD location 'd0'; destY : WORD location 'd1'; destMod : WORD location 'd2'; a1arg : pRastPort location 'a1'; SrcX : WORD location 'd3'; SrcY : WORD location 'd4'; SizeX : WORD location 'd5'; SizeY : WORD location 'd6'; DestFormat : byte location 'd7') : longword; syscall CyberGfxBase 120;
FUNCTION ReadRGBPixel(a1arg : pRastPort location 'a1'; x : WORD location 'd0'; y : WORD location 'd1') : longword; syscall CyberGfxBase 108;
FUNCTION ScalePixelArray(srcRect : POINTER location 'a0'; SrcW : WORD location 'd0'; SrcH : WORD location 'd1'; SrcMod : WORD location 'd2'; a1arg : pRastPort location 'a1'; DestX : WORD location 'd3'; DestY : WORD location 'd4'; DestW : WORD location 'd5'; DestH : WORD location 'd6'; SrcFormat : byte location 'd7') : LONGINT; syscall CyberGfxBase 090;
PROCEDURE UnLockBitMap(Handle : POINTER location 'a0'); syscall CyberGfxBase 174;
PROCEDURE UnLockBitMapTagList(Handle : POINTER location 'a0'; TagList : pTagItem location 'a1'); syscall CyberGfxBase 180;
FUNCTION WriteLUTPixelArray(srcRect : POINTER location 'a0'; SrcX : WORD location 'd0'; SrcY : WORD location 'd1'; SrcMod : WORD location 'd2'; a1arg : pRastPort location 'a1'; ColorTab : POINTER location 'a2'; DestX : WORD location 'd3'; DestY : WORD location 'd4'; SizeX : WORD location 'd5'; SizeY : WORD location 'd6'; CTFormat : byte location 'd7') : longword; syscall CyberGfxBase 198;
FUNCTION WritePixelArray(srcRect : POINTER location 'a0'; SrcX : WORD location 'd0'; SrcY : WORD location 'd1'; SrcMod : WORD location 'd2'; a1arg : pRastPort location 'a1'; DestX : WORD location 'd3'; DestY : WORD location 'd4'; SizeX : WORD location 'd5'; SizeY : WORD location 'd6'; SrcFormat : byte location 'd7') : longword; syscall CyberGfxBase 126;
FUNCTION WriteRGBPixel(a1arg : pRastPort location 'a1'; x : WORD location 'd0'; y : WORD location 'd1'; argb : longword location 'd2') : LONGINT; syscall CyberGfxBase 114;

{*** v43 ***}
FUNCTION WritePixelArrayAlpha(srcRect : POINTER location 'a0'; SrcX : WORD location 'd0'; SrcY : WORD location 'd1'; SrcMod : WORD location 'd2'; a1arg : pRastPort location 'a1'; DestX : WORD location 'd3'; DestY : WORD location 'd4'; SizeX : WORD location 'd5'; SizeY : WORD location 'd6'; globalAlpha : LongWord location 'd7') : longword; syscall CyberGfxBase 216;
PROCEDURE BltTemplateAlpha(source : pWORD location 'a0'; xSrc : LongInt location 'd0'; srcMod : LongInt location 'd1'; destRP : pRastPort location 'a1'; xDest : LongInt location 'd2'; yDest : LongInt location 'd3'; xSize : LongInt location 'd4'; ySize : LongInt location 'd5'); SysCall CyberGfxBase 222;
PROCEDURE ProcessPixelArray(rastPort: pRastPort location 'a0'; destX: LongWord location 'd0'; destY: LongWord location 'd1'; sizeX: LongWord location 'd2'; sizeY: LongWord location 'd3'; operation: LongWord location 'd4'; value: LongInt location 'd5'; taglist: pTagItem location 'a2'); syscall CyberGfxBase 228;

{*** v50 ***}
FUNCTION BltBitMapAlpha(srcBitMap : pBitMap location 'a0'; xSrc : LongInt location 'd0'; ySrc : LongInt location 'd1'; destBitMap : pBitMap location 'a1'; xDest : LongInt location 'd2'; yDest : LongInt location 'd3'; xSize : LongInt location 'd4'; ySize : LongInt location 'd5'; tagList : pTagItem location 'a2') : LongWord; syscall CyberGfxBase 234;
FUNCTION BltBitMapRastPortAlpha(srcBitMap : pBitMap location 'a0'; xSrc : LongInt location 'd0'; ySrc : LongInt location 'd1'; destRP : pRastPort location 'a1'; xDest : LongInt location 'd2'; yDest : LongInt location 'd3'; xSize : LongInt location 'd4'; ySize : LongInt location 'd5'; tagList : pTagItem location 'd2') : LongWord; SysCall CyberGfxBase 240;
FUNCTION ScalePixelArrayAlpha(srcRect : POINTER location 'a0'; SrcW : WORD location 'd0'; SrcH : WORD location 'd1'; SrcMod : WORD location 'd2'; a1arg : pRastPort location 'a1'; DestX : WORD location 'd3'; DestY : WORD location 'd4'; DestW : WORD location 'd5'; DestH : WORD location 'd6'; globalAlpha : LongWord location 'd7') : LONGINT; syscall CyberGfxBase 252;

{
 Functions and procedures with array of const go here
}
{
FUNCTION AllocCModeListTags(const ModeListTags : Array Of Const) : pList;
FUNCTION BestCModeIDTags(const BestModeIDTags : Array Of Const) : longword;
FUNCTION CModeRequestTags(ModeRequest : POINTER; const ModeRequestTags : Array Of Const) : longword;
PROCEDURE CVideoCtrlTags(ViewPort : pViewPort; const TagList : Array Of Const);
PROCEDURE DoCDrawMethodTags(Hook : pHook; a1arg : pRastPort; const TagList : Array Of Const);
FUNCTION LockBitMapTags(BitMap : POINTER; const TagList : Array Of Const) : POINTER;
PROCEDURE UnLockBitMapTags(Handle : POINTER; const TagList : Array Of Const);
}
function SHIFT_PIXFMT(fmt : longint) : longint;

function InitCyberGfxLibrary: boolean;

IMPLEMENTATION

{uses
tagsarray;}

{
 Functions and procedures with array of const go here
}
{
FUNCTION AllocCModeListTags(const ModeListTags : Array Of Const) : pList;
begin
    AllocCModeListTags := AllocCModeListTagList(readintags(ModeListTags));
end;

FUNCTION BestCModeIDTags(const BestModeIDTags : Array Of Const) : longword;
begin
    BestCModeIDTags := BestCModeIDTagList(readintags(BestModeIDTags));
end;

FUNCTION CModeRequestTags(ModeRequest : POINTER; const ModeRequestTags : Array Of Const) : longword;
begin
    CModeRequestTags := CModeRequestTagList(ModeRequest , readintags(ModeRequestTags));
end;

PROCEDURE CVideoCtrlTags(ViewPort : pViewPort; const TagList : Array Of Const);
begin
    CVideoCtrlTagList(ViewPort , readintags(TagList));
end;

PROCEDURE DoCDrawMethodTags(Hook : pHook; a1arg : pRastPort; const TagList : Array Of Const);
begin
    DoCDrawMethodTagList(Hook , a1arg , readintags(TagList));
end;

FUNCTION LockBitMapTags(BitMap : POINTER; const TagList : Array Of Const) : POINTER;
begin
    LockBitMapTags := LockBitMapTagList(BitMap , readintags(TagList));
end;

PROCEDURE UnLockBitMapTags(Handle : POINTER; const TagList : Array Of Const);
begin
    UnLockBitMapTagList(Handle , readintags(TagList));
end;
}
function SHIFT_PIXFMT(fmt : longint) : longint; inline;
begin
    SHIFT_PIXFMT:=(ULONG(fmt)) shl 24;
end;


const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

function InitCyberGfxLibrary: boolean;
begin
  InitCyberGfxLibrary := Assigned(CyberGfxBase);
end;

initialization
  CyberGfxBase := OpenLibrary(CYBERGFXNAME,LIBVERSION);
finalization
  if Assigned(CyberGfxBase) then
    CloseLibrary(CyberGfxBase);
END. (* UNIT CYBERGRAPHICS *)
