{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    dos.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cybergraphics;
{$mode objfpc}
interface

uses
  Exec, agraphics, utility;

const
  CYBERGFXNAME = 'cybergraphics.library';
  CYBERGFX_INCLUDE_VERSION = 41;
 
 
  // ProcessPixelArray Operations (v50)

  POP_BRIGHTEN                = 0;
  POP_DARKEN                  = 1;
  POP_SETALPHA                = 2;
  POP_TINT                    = 3;
  POP_BLUR                    = 4;
  POP_COLOR2GREY              = 5;
  POP_NEGATIVE                = 6;
  POP_NEGFADE                 = 7;
  POP_TINTFADE                = 8;
  POP_GRADIENT                = 9;
  POP_SHIFTRGB                =10;

  GRADTYPE_HORIZONTAL         = 0;
  GRADTYPE_VERTICAL           = 1;
  GRADTYPE_RECTANGLE          = 2;
  GRADTYPE_LINEAR_ANGLE       = 3;
  GRADTYPE_RADIAL             = 4;

  RGBSHIFT_BGR: LongWord      = 1;
  RGBSHIFT_BRG: LongWord      = 2;
  RGBSHIFT_GBR: LongWord      = 3;
  RGBSHIFT_GRB: LongWord      = 4;
  RGBSHIFT_RBG: LongWord      = 5;

  PPAOPTAG_FADEFULLSCALE      = $85231020;
  PPAOPTAG_FADEOFFSET         = $85231021;

  PPAOPTAG_GRADIENTTYPE       = $85231022;
  PPAOPTAG_GRADCOLOR1         = $85231023;
  PPAOPTAG_GRADCOLOR2         = $85231024;
  PPAOPTAG_GRADFULLSCALE      = PPAOPTAG_FADEFULLSCALE;
  PPAOPTAG_GRADOFFSET         = PPAOPTAG_FADEOFFSET;
  PPAOPTAG_GRADSYMCENTER      = $85231026;

  PPAOPTAG_RGBMASK            = $85231025;
  
  //  Definition of CyberModeNode (Returned in AllocModeList)
  
type
   PCyberModeNode = ^TCyberModeNode;
   TCyberModeNode = record
     Node: TNode;
     ModeText: array[0..DISPLAYNAMELEN - 1] of Char; // name for this mode
     DisplayID: ULONG;                               // display id associated with the node
     Width: UWORD;                                   // visible width
     Height: UWORD;                                  // visible height
     Depth: UWORD;                                   // display depth
     DisplayTagList: PTagItem;                       // taglist with extended ModeID information
   end;


const
// Parameters for GetCyberMapAttr()
  CYBRMATTR_XMOD         = $80000001; // function returns BytesPerRow if its called with this parameter
  CYBRMATTR_BPPIX        = $80000002; // BytesPerPixel shall be returned
  CYBRMATTR_DISPADR      = $80000003; // do not use this ! private tag
  CYBRMATTR_PIXFMT       = $80000004; // the pixel format is returned 
  CYBRMATTR_WIDTH        = $80000005; // returns width in pixels
  CYBRMATTR_HEIGHT       = $80000006; // returns height in lines  
  CYBRMATTR_DEPTH        = $80000007; // returns bits per pixel
  CYBRMATTR_ISCYBERGFX   = $80000008; // returns -1 if supplied bitmap is a cybergfx one
  CYBRMATTR_ISLINEARMEM  = $80000009; // returns -1 if supplied bitmap is linear accessable
  CYBRMATTR_PIXFMT_ALPHA = $8000000A;
// Parameters for GetCyberIDAttr()
  CYBRIDATTR_PIXFMT     = $80000001; // the pixel format is returned
  CYBRIDATTR_WIDTH      = $80000002; // returns visible width in pixels
  CYBRIDATTR_HEIGHT     = $80000003; // returns visible height in lines
  CYBRIDATTR_DEPTH      = $80000004; // returns bits per pixel
  CYBRIDATTR_BPPIX      = $80000005; // BytesPerPixel shall be returned
// Tags for CyberModeRequest()
  CYBRMREQ_TB = TAG_USER + $40000;
  //  FilterTags
  CYBRMREQ_MinDepth    = CYBRMREQ_TB + 0; // Minimum depth for displayed screenmode
  CYBRMREQ_MaxDepth    = CYBRMREQ_TB + 1; // Maximum depth  "       "        "
  CYBRMREQ_MinWidth    = CYBRMREQ_TB + 2; // Minumum width  "       "        "
  CYBRMREQ_MaxWidth    = CYBRMREQ_TB + 3; // Maximum width  "       "        "
  CYBRMREQ_MinHeight   = CYBRMREQ_TB + 4; // Minumum height "       "        "
  CYBRMREQ_MaxHeight   = CYBRMREQ_TB + 5; // Minumum height "       "        "
  CYBRMREQ_CModelArray = CYBRMREQ_TB + 6;
  CYBRMREQ_WinTitle    = CYBRMREQ_TB + 20;
  CYBRMREQ_OKText      = CYBRMREQ_TB + 21;
  CYBRMREQ_CancelText  = CYBRMREQ_TB + 22;
  CYBRMREQ_Screen      = CYBRMREQ_TB + 30; // Screen you wish the Requester to open on 
// Tags for BestCyberModeID()
  CYBRBIDTG_TB = TAG_USER + $50000;
  // FilterTags
  CYBRBIDTG_Depth         = CYBRBIDTG_TB + 0;
  CYBRBIDTG_NominalWidth  = CYBRBIDTG_TB + 1;
  CYBRBIDTG_NominalHeight = CYBRBIDTG_TB + 2;
  CYBRBIDTG_MonitorID     = CYBRBIDTG_TB + 3;
  CYBRBIDTG_BoardName     = CYBRBIDTG_TB + 5;
// definition of divers pixel formats
  PIXFMT_LUT8    = 0;
  PIXFMT_RGB15   = 1;
  PIXFMT_BGR15   = 2;
  PIXFMT_RGB15PC = 3;
  PIXFMT_BGR15PC = 4;
  PIXFMT_RGB16   = 5;
  PIXFMT_BGR16   = 6;
  PIXFMT_RGB16PC = 7;
  PIXFMT_BGR16PC = 8;
  PIXFMT_RGB24   = 9;
  PIXFMT_BGR24   = 10;
  PIXFMT_ARGB32  = 11;
  PIXFMT_BGRA32  = 12;
  PIXFMT_RGBA32  = 13;
// SrcRectangle formats defines for xxxPixelArray calls()
  RECTFMT_RGB   = 0;
  RECTFMT_RGBA  = 1;
  RECTFMT_ARGB  = 2;
  RECTFMT_LUT8  = 3;
  RECTFMT_GREY8 = 4;
  RECTFMT_RAW   = 5;
  // AROS extensions
{$ifdef AROS}
  PIXFMT_ABGR32 = 100;
  PIXFMT_0RGB32 = 101;
  PIXFMT_BGR032 = 102;
  PIXFMT_RGB032 = 103;
  PIXFMT_0BGR32 = 104;

  RECTFMT_RGB15	  = 100;
  RECTFMT_BGR15	  = 101;
  RECTFMT_RGB15PC = 102;
  RECTFMT_BGR15PC = 103;
  RECTFMT_RGB16	  = 104;
  RECTFMT_BGR16	  = 105;
  RECTFMT_RGB16PC = 106;
  RECTFMT_BGR16PC = 107;
  RECTFMT_RGB24	  = RECTFMT_RGB;
  RECTFMT_BGR24	  = 109;
  RECTFMT_ARGB32	= RECTFMT_ARGB;
  RECTFMT_BGRA32	= 111;
  RECTFMT_RGBA32	= RECTFMT_RGBA;
  RECTFMT_ABGR32	= 113;
  RECTFMT_0RGB32  = 114;
  RECTFMT_BGR032  = 115;
  RECTFMT_RGB032  = 116;
  RECTFMT_0BGR32  = 117;
{$endif}
  
// Parameters for CVideoCtrlTagList()
  SETVC_DPMSLevel = $88002001;
  DPMS_ON         = 0; // Full operation
  DPMS_STANDBY    = 1; // Optional state of minimal power reduction
  DPMS_SUSPEND    = 2; // Significant reduction of power consumption
  DPMS_OFF        = 3; // Lowest level of power consumption
// Tags for LockBitMapTagList()
  LBMI_WIDTH       = $84001001;
  LBMI_HEIGHT      = $84001002;
  LBMI_DEPTH       = $84001003;
  LBMI_PIXFMT      = $84001004;
  LBMI_BYTESPERPIX = $84001005;
  LBMI_BYTESPERROW = $84001006;
  LBMI_BASEADDRESS = $84001007;
// Tags for UnLockBitMapTagList()
  UBMI_UPDATERECTS  = $85001001;
  UBMI_REALLYUNLOCK = $85001002;
  
type
// Message passed to the DoCDrawMethodTagList() hook function
  PCDrawMsg = ^TCDrawMsg;
  TCDrawMsg = record
    cdm_MemPtr: APTR;
    cdm_offx: ULONG;
    cdm_offy: ULONG;
    cdm_xsize: ULONG;
    cdm_ysize: ULONG;
    cdm_BytesPerRow: UWORD;
    cdm_BytesPerPix: UWORD;
    cdm_ColorModel: UWORD;
  end;

// Colour Table source formats for WriteLUTPixelArray()  ULONG [] table
const
  CTABFMT_XRGB8 = 0;
// graphics.library/AllocBitMap() extended flags
  BMB_SPECIALFMT = 7;
  BMF_SPECIALFMT = 1 shl BMB_SPECIALFMT;

var
  CyberGfxBase: PLibrary;

function IsCyberModeID(modeID: LongWord): LongBool; syscall CyberGfxBase 9;
function BestCModeIDTagList(tags: PTagItem): LongWord; syscall CyberGfxBase 10;
function AllocCModeListTagList(tags: PTagItem): PList; syscall CyberGfxBase 12;
procedure FreeCModeList(modeList: PList); syscall CyberGfxBase 13;
function ScalePixelArray(srcRect: APTR; SrcW: Word; SrcH: Word; SrcMod: Word; RastPort: PRastPort; DestX: Word; DestY: Word; DestW: Word; DestH: Word; SrcFormat: Byte): LongInt; syscall CyberGfxBase 15;
function GetCyberMapAttr(bitMap: PBitMap; attribute_: LongWord): LongWord; syscall CyberGfxBase 16;
function GetCyberIDAttr(attribute_: LongWord; DisplayModeID: LongWord): LongWord; syscall CyberGfxBase 17;
function ReadRGBPixel(rp: PRastPort; x: Word; y: Word): LongWord; syscall CyberGfxBase 18;
function WriteRGBPixel(rp: PRastPort; x: Word; y: Word; pixel: LongWord): LongInt; syscall CyberGfxBase 19;
function ReadPixelArray(dst: APTR; destx: Word; desty: Word; dstmod: Word; rp: PRastPort; srcx: Word; srcy: Word; width: Word; height: Word; dstformat: Byte): LongWord; syscall CyberGfxBase 20;
function WritePixelArray(src: APTR; srcx: Word; srcy: Word; srcmod: Word; rp: PRastPort; destx: Word; desty: Word; width: Word; height: Word; srcformat: Byte): LongWord; syscall CyberGfxBase 21;
function MovePixelArray(SrcX: Word; SrcY: Word; RastPort: PRastPort; DstX: Word; DstY: Word; SizeX: Word; SizeY: Word): LongWord; syscall CyberGfxBase 22;
function InvertPixelArray(rp: PRastPort; destx: Word; desty: Word; width: Word; height: Word): LongWord; syscall CyberGfxBase 24;
function FillPixelArray(rp: PRastPort; destx: Word; desty: Word; width: Word; height: Word; pixel: LongWord): LongWord; syscall CyberGfxBase 25;
procedure DoCDrawMethodTagList(hook: PHook; rp: PRastPort; tags: PTagItem); syscall CyberGfxBase 26;
procedure CVideoCtrlTagList(vp: PViewPort; tags: PTagItem); syscall CyberGfxBase 27;
function LockBitMapTagList(bitmap: APTR; tags: PTagItem): APTR; syscall CyberGfxBase 28;
procedure UnLockBitMap(Handle: APTR); syscall CyberGfxBase 29;
procedure UnLockBitMapTagList(Handle: APTR; Tags: PTagItem); syscall CyberGfxBase 30;
function ExtractColor(RastPort: PRastPort; SingleMap: PBitMap; Colour: LongWord; sX: LongWord; sY: LongWord; Width: LongWord; Height: LongWord): LongWord; syscall CyberGfxBase 31;
function WriteLUTPixelArray(srcRect: APTR; SrcX: Word; SrcY: Word; SrcMod: Word; rp: PRastPort; CTable: APTR; DestX: Word; DestY: Word; SizeX: Word; SizeY: Word; CTabFormat: Byte): LongInt; syscall CyberGfxBase 33;
function WritePixelArrayAlpha(src: APTR; srcx: Word; srcy: Word; srcmod: Word; rp: PRastPort; destx: Word; desty: Word; width: Word; height: Word; globalalpha: LongWord): LongWord; syscall CyberGfxBase 36;
procedure BltTemplateAlpha(src: APTR; srcx: LongInt; srcmod: LongInt; rp: PRastPort; destx: LongInt; desty: LongInt; width: LongInt; height: LongInt); syscall CyberGfxBase 37;
procedure ProcessPixelArray(rp: PRastPort; destX: LongWord; destY: LongWord; sizeX: LongWord; sizeY: LongWord; operation: LongWord; value: LongInt; taglist: PTagItem); syscall CyberGfxBase 38;

// Functions and procedures with array of const go here
function AllocCModeListTags(const ModeListTags: array of const): PList;
function BestCModeIDTags(const BestModeIDTags: array of const): LongWord;
procedure CVideoCtrlTags(ViewPort: PViewPort; const TagList: array of const);
procedure DoCDrawMethodTags(Hook: PHook; a1arg: PRastPort; const TagList: array of const);
function LockBitMapTags(BitMap: APTR; const TagList: array of const): APTR;
procedure UnLockBitMapTags(Handle: APTR; const TagList: array of const);

function SHIFT_PIXFMT(fmt: LongInt): LongInt;
function DOWNSHIFT_PIXFMT(fmt: LongInt): LongInt;

implementation

uses
  tagsarray;

// Functions and procedures with array of const go here
function AllocCModeListTags(const ModeListTags: array of const): PList;
var
  TagList: TTagsList;
begin
  AddTags(TagList, ModeListTags);
  AllocCModeListTags := AllocCModeListTagList(GetTagPtr(TagList));
end;

function BestCModeIDTags(const BestModeIDTags: array of const): LongWord;
var
  TagList: TTagsList;
begin
  AddTags(TagList, BestModeIDTags);
  BestCModeIDTags := BestCModeIDTagList(GetTagPtr(TagList));
end;

procedure CVideoCtrlTags(ViewPort: PViewPort; const TagList: array of const);
var
  TagsList: TTagsList;
begin
  AddTags(TagsList, TagList);
  CVideoCtrlTagList(ViewPort, GetTagPtr(TagsList));
end;

procedure DoCDrawMethodTags(Hook: PHook; a1arg: PRastPort; const TagList: array of const);
var
  TagsList: TTagsList;
begin
  AddTags(TagsList, TagList);
  DoCDrawMethodTagList(Hook, a1arg, GetTagPtr(TagsList));
end;

function LockBitMapTags(BitMap: APTR; const TagList: array of const): APTR;
var
  TagsList: TTagsList;
begin
  AddTags(TagsList, TagList);
  LockBitMapTags := LockBitMapTagList(BitMap, GetTagPtr(TagsList));
end;

procedure UnLockBitMapTags(Handle: APTR; const TagList: array of const);
var
  TagsList: TTagsList;
begin
  AddTags(TagsList, TagList);
  UnLockBitMapTagList(Handle, GetTagPtr(TagsList));
end;

function SHIFT_PIXFMT(fmt: LongInt): LongInt;
begin
  SHIFT_PIXFMT:=(ULONG(fmt)) shl 24;
end;

function DOWNSHIFT_PIXFMT(fmt: LongInt): LongInt;
begin
  DOWNSHIFT_PIXFMT:=(ULONG(fmt)) shr 24;
end;

initialization
  CyberGfxBase := OpenLibrary(CYBERGFXNAME, 0); 
finalization
  CloseLibrary(CyberGfxBase);
end.



