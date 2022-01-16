{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    Picasso96 functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit picasso96api;

interface

uses
  Exec, utility, agraphics, intuition;

// Picasso96.h -- include File
//  (C) Copyright 1996-98 Alexander Kneer & Tobias Abt
//      All Rights Reserved.
const
  PICASSO96APINAME: PChar = 'Picasso96API.library';

// Types for RGBFormat used
type
  TRGBFTYPE = (
    RGBFB_NONE,     // no valid RGB format (should not happen)
    RGBFB_CLUT,     // palette mode, set colors when opening screen using tags or use SetRGB32/LoadRGB32(...)
    RGBFB_R8G8B8,   // TrueColor RGB (8 bit each)
    RGBFB_B8G8R8,   // TrueColor BGR (8 bit each)
    RGBFB_R5G6B5PC, // HiColor16 (5 bit R, 6 bit G, 5 bit B), format: gggbbbbbrrrrrggg
    RGBFB_R5G5B5PC, // HiColor15 (5 bit each), format: gggbbbbb0rrrrrgg
    RGBFB_A8R8G8B8, // 4 Byte TrueColor ARGB (A unused alpha channel)
    RGBFB_A8B8G8R8, // 4 Byte TrueColor ABGR (A unused alpha channel)
    RGBFB_R8G8B8A8, // 4 Byte TrueColor RGBA (A unused alpha channel)
    RGBFB_B8G8R8A8, // 4 Byte TrueColor BGRA (A unused alpha channel)
    RGBFB_R5G6B5,   // HiColor16 (5 bit R, 6 bit G, 5 bit B), format: rrrrrggggggbbbbb
    RGBFB_R5G5B5,   // HiColor15 (5 bit each), format: 0rrrrrgggggbbbbb
    RGBFB_B5G6R5PC, // HiColor16 (5 bit R, 6 bit G, 5 bit B), format: gggrrrrrbbbbbggg
    RGBFB_B5G5R5PC, //  HiColor15 (5 bit each), format: gggrrrrr0bbbbbbgg
    // By now, the following formats are for use with a hardware window only (bitmap operations may be implemented incompletely)
    RGBFB_YUV422CGX,// 2 Byte TrueColor YUV (CCIR recommendation CCIR601).
    RGBFB_YUV411,   //  1 Byte TrueColor ACCUPAK.
    RGBFB_YUV422PA, // 2 Byte TrueColor CCIR601 for use with YUV12 planar assist mode on Cirrus Logic base graphics chips.
    RGBFB_YUV422,   // 2 Byte TrueColor YUV (CCIR recommendation CCIR601).
    RGBFB_YUV422PC, // 2 Byte TrueColor CCIR601 byte swapped (V0-Y0-U0-Y1)
    RGBFB_YUV420P,  // 12 Bit TrueColor 3-plane YUV
    RGBFB_YUV410P,  // 9 Bit TrueColor 3-plane YUV
    RGBFB_ALPHA8,
    RGBFB_MaxFormats);

const
  RGBFF_NONE      = 1 shl 0;
  RGBFF_CLUT      = 1 shl 1;
  RGBFF_R8G8B8    = 1 shl 2;
  RGBFF_B8G8R8    = 1 shl 3;
  RGBFF_R5G6B5PC  = 1 shl 4;
  RGBFF_R5G5B5PC  = 1 shl 5;
  RGBFF_A8R8G8B8  = 1 shl 6;
  RGBFF_A8B8G8R8  = 1 shl 7;
  RGBFF_R8G8B8A8  = 1 shl 8;
  RGBFF_B8G8R8A8  = 1 shl 9;
  RGBFF_R5G6B5    = 1 shl 10;
  RGBFF_R5G5B5    = 1 shl 11;
  RGBFF_B5G6R5PC  = 1 shl 12;
  RGBFF_B5G5R5PC  = 1 shl 13;
  RGBFF_YUV422CGX = 1 shl 14;
  RGBFF_YUV411    = 1 shl 15;
  RGBFF_YUV422    = 1 shl 16;
  RGBFF_YUV422PC  = 1 shl 17;
  RGBFF_YUV422PA  = 1 shl 18;
  RGBFF_YUV420P   = 1 shl 19;
  RGBFF_YUV410P   = 1 shl 20;
  RGBFF_ALPHA8    = 1 shl 21;

  RGBFF_HICOLOR   = RGBFF_R5G6B5PC or RGBFF_R5G5B5PC or RGBFF_R5G6B5 or RGBFF_R5G5B5 or RGBFF_B5G6R5PC or RGBFF_B5G5R5PC;
  RGBFF_TRUECOLOR = RGBFF_R8G8B8 or RGBFF_B8G8R8;
  RGBFF_TRUEALPHA = RGBFF_A8R8G8B8 or RGBFF_A8B8G8R8 or RGBFF_R8G8B8A8 or RGBFF_B8G8R8A8;

// Flags for p96AllocBitMap
  BMF_USERPRIVATE = $8000; // private user bitmap that will never be put to a board, but may be used as a temporary render buffer and accessed
                           // with OS blit functions, too. Bitmaps allocated with this flag do not need to be locked.

// Attributes for p96GetBitMapAttr
  P96BMA_WIDTH = 0;
  P96BMA_HEIGHT = 1;
  P96BMA_DEPTH = 2;
  P96BMA_MEMORY = 3;
  P96BMA_BYTESPERROW = 4;
  P96BMA_BYTESPERPIXEL = 5;
  P96BMA_BITSPERPIXEL = 6;
  P96BMA_RGBFORMAT = 7;
  P96BMA_ISP96 = 8;
  P96BMA_ISONBOARD = 9;
  P96BMA_BOARDMEMBASE = 10;
  P96BMA_BOARDIOBASE = 11;
  P96BMA_BOARDMEMIOBASE = 12;

// Attributes for p96GetModeIDAttr
  P96IDA_WIDTH = 0;
  P96IDA_HEIGHT = 1;
  P96IDA_DEPTH = 2;
  P96IDA_BYTESPERPIXEL = 3;
  P96IDA_BITSPERPIXEL = 4;
  P96IDA_RGBFORMAT = 5;
  P96IDA_ISP96 = 6;
  P96IDA_BOARDNUMBER = 7;
  P96IDA_STDBYTESPERROW = 8;
  P96IDA_BOARDNAME = 9;
  P96IDA_COMPATIBLEFORMATS = 10;
  P96IDA_VIDEOCOMPATIBLE = 11;
  P96IDA_PABLOIVCOMPATIBLE = 12;
  P96IDA_PALOMAIVCOMPATIBLE = 13;

// Tags for p96BestModeIDTagList
  P96BIDTAG_Dummy              = TAG_USER + 96;
  P96BIDTAG_FormatsAllowed     = P96BIDTAG_Dummy + $0001;
  P96BIDTAG_FormatsForbidden   = P96BIDTAG_Dummy + $0002;
  P96BIDTAG_NominalWidth       = P96BIDTAG_Dummy + $0003;
  P96BIDTAG_NominalHeight      = P96BIDTAG_Dummy + $0004;
  P96BIDTAG_Depth              = P96BIDTAG_Dummy + $0005;
  P96BIDTAG_VideoCompatible    = P96BIDTAG_Dummy + $0006;
  P96BIDTAG_PabloIVCompatible  = P96BIDTAG_Dummy + $0007;
  P96BIDTAG_PalomaIVCompatible = P96BIDTAG_Dummy + $0008;
// Tags for p96RequestModeIDTagList
  P96MA_Dummy              = TAG_USER + $10000 + 96;
  P96MA_MinWidth           = P96MA_Dummy + $0001;
  P96MA_MinHeight          = P96MA_Dummy + $0002;
  P96MA_MinDepth           = P96MA_Dummy + $0003;
  P96MA_MaxWidth           = P96MA_Dummy + $0004;
  P96MA_MaxHeight          = P96MA_Dummy + $0005;
  P96MA_MaxDepth           = P96MA_Dummy + $0006;
  P96MA_DisplayID          = P96MA_Dummy + $0007;
  P96MA_FormatsAllowed     = P96MA_Dummy + $0008;
  P96MA_FormatsForbidden   = P96MA_Dummy + $0009;
  P96MA_WindowTitle        = P96MA_Dummy + $000a;
  P96MA_OKText             = P96MA_Dummy + $000b;
  P96MA_CancelText         = P96MA_Dummy + $000c;
  P96MA_Window             = P96MA_Dummy + $000d;
  P96MA_PubScreenName      = P96MA_Dummy + $000e;
  P96MA_Screen             = P96MA_Dummy + $000f;
  P96MA_VideoCompatible    = P96MA_Dummy + $0010;
  P96MA_PabloIVCompatible  = P96MA_Dummy + $0011;
  P96MA_PalomaIVCompatible = P96MA_Dummy + $0012;
// Tags for p96OpenScreenTagList
  P96SA_Dummy        = TAG_USER + $20000 + 96;
  P96SA_Left         = P96SA_Dummy + $0001;
  P96SA_Top          = P96SA_Dummy + $0002;
  P96SA_Width        = P96SA_Dummy + $0003;
  P96SA_Height       = P96SA_Dummy + $0004;
  P96SA_Depth        = P96SA_Dummy + $0005;
  P96SA_DetailPen    = P96SA_Dummy + $0006;
  P96SA_BlockPen     = P96SA_Dummy + $0007;
  P96SA_Title        = P96SA_Dummy + $0008;
  P96SA_Colors       = P96SA_Dummy + $0009;
  P96SA_ErrorCode    = P96SA_Dummy + $000a;
  P96SA_Font         = P96SA_Dummy + $000b;
  P96SA_SysFont      = P96SA_Dummy + $000c;
  P96SA_Type         = P96SA_Dummy + $000d;
  P96SA_BitMap       = P96SA_Dummy + $000e;
  P96SA_PubName      = P96SA_Dummy + $000f;
  P96SA_PubSig       = P96SA_Dummy + $0010;
  P96SA_PubTask      = P96SA_Dummy + $0011;
  P96SA_DisplayID    = P96SA_Dummy + $0012;
  P96SA_DClip        = P96SA_Dummy + $0013;
  P96SA_ShowTitle    = P96SA_Dummy + $0014;
  P96SA_Behind       = P96SA_Dummy + $0015;
  P96SA_Quiet        = P96SA_Dummy + $0016;
  P96SA_AutoScroll   = P96SA_Dummy + $0017;
  P96SA_Pens         = P96SA_Dummy + $0018;
  P96SA_SharePens    = P96SA_Dummy + $0019;
  P96SA_BackFill     = P96SA_Dummy + $001a;
  P96SA_Colors32     = P96SA_Dummy + $001b;
  P96SA_VideoControl = P96SA_Dummy + $001c;
  P96SA_RGBFormat    = P96SA_Dummy + $001d;
  P96SA_NoSprite     = P96SA_Dummy + $001e;
  P96SA_NoMemory     = P96SA_Dummy + $001f;
  P96SA_RenderFunc   = P96SA_Dummy + $0020;
  P96SA_SaveFunc     = P96SA_Dummy + $0021;
  P96SA_UserData     = P96SA_Dummy + $0022;
  P96SA_Alignment    = P96SA_Dummy + $0023;
  P96SA_FixedScreen  = P96SA_Dummy + $0024;
  P96SA_Exclusive    = P96SA_Dummy + $0025;
  P96SA_ConstantBytesPerRow  = P96SA_Dummy + $0026;
  P96SA_ConstantByteSwapping = P96SA_Dummy + $0027;

  MODENAMELENGTH = 48;
type
  PP96Mode = ^TP96Mode;
  TP96Mode = record
    Node: TNode;
    Description: array[0..MODENAMELENGTH - 1] of Char;
    Width: Word;
    Height: Word;
    Depth: Word;
    DisplayID: LongWord;
  end;
// Structure to describe graphics data
  PRenderInfo = ^TRenderInfo;
  TRenderInfo = record
    Memory : APTR;         // pointer to graphics data
    BytesPerRow : Word;    // distance in bytes between one pixel and its neighbour up or down.
    pad : Word;            // private, not used.
    RGBFormat : TRGBFTYPE; // RGBFormat of the data.
  end;
// used for planar YUV formats
  PYUVRenderInfo = ^TYUVRenderInfo;
  TYUVRenderInfo = record
    RI: TRenderInfo;
    Planes: array[0..2] of APTR;
    BytesPerRow: array[0..2] of Word;
    padWord: Word;
  end;
// Structure for p96WriteTrueColorData() and p96ReadTrueColorData()
  PTrueColorInfo = ^TTrueColorInfo;
  TTrueColorInfo = record
    PixelDistance : LongWord; // distance in bytes between the red (must be the same as for the green or blue)
                              // component of one pixel and its next neighbour to the left or right.
    BytesPerRow : LongWord;   // distance in bytes between the red (must be the same as for the green or blue)
                              // component of one pixel and its next neighbour up or down.
    RedData: PByte;           // pointer to the red component of the upper left pixel.
    GreenData: PByte;         // pointer to the green component of the upper left pixel.
    BlueData: PByte;          // pointer to the blue component of the upper left pixel.
  end;
// Tags for PIPs
const
  P96PIP_Dummy        = TAG_USER + $30000 + 96;
  P96PIP_SourceFormat = P96PIP_Dummy + 1;  // TRGBFTYPE (I)
  P96PIP_SourceBitMap = P96PIP_Dummy + 2;  // struct BitMap   (G)
  P96PIP_SourceRPort  = P96PIP_Dummy + 3;  // struct RastPort   (G)
  P96PIP_SourceWidth  = P96PIP_Dummy + 4;  // LongWord (I)
  P96PIP_SourceHeight = P96PIP_Dummy + 5;  // LongWord (I)
  P96PIP_Type         = P96PIP_Dummy + 6;  // LongWord (I) default: PIPT_MemoryWindow
  P96PIP_ErrorCode    = P96PIP_Dummy + 7;  // LONG  (I)
  P96PIP_Brightness   = P96PIP_Dummy + 8;  // LongWord (IGS) default: 0
  P96PIP_Left         = P96PIP_Dummy + 9;  // LongWord (I) default: 0
  P96PIP_Top          = P96PIP_Dummy + 10; // LongWord (I) default: 0
  P96PIP_Width        = P96PIP_Dummy + 11; // LongWord (I) default: inner width of window
  P96PIP_Height       = P96PIP_Dummy + 12; // LongWord (I) default: inner height of window
  P96PIP_Relativity   = P96PIP_Dummy + 13; // LongWord (I) default: PIPRel_Width|PIPRel_Height
  P96PIP_Colors       = P96PIP_Dummy + 14; // TColorSpec (IS) ti_Data is an array of TColorSpec, terminated by ColorIndex = -1.
  P96PIP_Colors32     = P96PIP_Dummy + 15; // LongWord  (IS) Tag to set the palette colors at 32 bits-per-gun.
  P96PIP_NoMemory     = P96PIP_Dummy + 16;
  P96PIP_RenderFunc   = P96PIP_Dummy + 17;
  P96PIP_SaveFunc     = P96PIP_Dummy + 18;
  P96PIP_UserData     = P96PIP_Dummy + 19;
  P96PIP_Alignment    = P96PIP_Dummy + 20;
  P96PIP_ConstantBytesPerRow  = P96PIP_Dummy + 21;
  P96PIP_AllowCropping        = P96PIP_Dummy + 22;
  P96PIP_InitialIntScaling    = P96PIP_Dummy + 23;
  P96PIP_ClipLeft             = P96PIP_Dummy + 24; // LongWord (IS)
  P96PIP_ClipTop              = P96PIP_Dummy + 25; // LongWord (IS)
  P96PIP_ClipWidth            = P96PIP_Dummy + 26; // LongWord (IS)
  P96PIP_ClipHeight           = P96PIP_Dummy + 27; // LongWord (IS)
  P96PIP_ConstantByteSwapping = P96PIP_Dummy + 28;
  P96PIP_ColorKeyPen          = P96PIP_Dummy + 29;
  P96PIP_NumberOfBuffers      = P96PIP_Dummy + 30;
  P96PIP_VisibleBuffer        = P96PIP_Dummy + 31;
  P96PIP_WorkBuffer           = P96PIP_Dummy + 32;
  P96PIP_DisplayedBuffer      = P96PIP_Dummy + 33;
  P96PIP_ColorKeyARGB         = P96PIP_Dummy + 34;

  PIPT_MemoryWindow = 0; // default
  PIPT_VideoWindow  = 1;
  PIPT_NUMTYPES     = 2;

  P96PIPT_MemoryWindow = PIPT_MemoryWindow;
  P96PIPT_VideoWindow  = PIPT_VideoWindow;

  PIPRel_Right  = 1; // P96PIP_Left is relative to the right side (negative value)
  PIPRel_Bottom = 2; // P96PIP_Top is relative to the bottom (negative value)
  PIPRel_Width  = 4; // P96PIP_Width is amount of pixels not used by PIP at the right side of the window (negative value)
  PIPRel_Height = 8; // P96PIP_Height is amount of pixels not used by PIP at the window bottom (negative value)
  PIPERR_NOMEMORY      = 1; // couldn't get normal memory
  PIPERR_ATTACHFAIL    = 2; // Failed to attach to a screen
  PIPERR_NOTAVAILABLE  = 3; // PIP not available for other reason
  PIPERR_OUTOFPENS     = 4; // couldn't get a free pen for occlusion
  PIPERR_BADDIMENSIONS = 5; // type, width, height or format invalid
  PIPERR_NOWINDOW      = 6; // couldn't open window
  PIPERR_BADALIGNMENT  = 7; // specified alignment is not ok
  PIPERR_CROPPED       = 8; // pip would be cropped, but isn't allowed to
// Tags for P96GetRTGDataTagList
  P96RD_Dummy          = TAG_USER + $40000 + 96;
  P96RD_NumberOfBoards = P96RD_Dummy + 1;
// Tags for P96GetBoardDataTagList
  P96BD_Dummy             = TAG_USER + $50000 + 96;
  P96BD_BoardName         = P96BD_Dummy + 1;
  P96BD_ChipName          = P96BD_Dummy + 2;
  P96BD_TotalMemory       = P96BD_Dummy + 4;
  P96BD_FreeMemory        = P96BD_Dummy + 5;
  P96BD_LargestFreeMemory = P96BD_Dummy + 6;
  P96BD_MonitorSwitch     = P96BD_Dummy + 7;
  P96BD_RGBFormats        = P96BD_Dummy + 8;
  P96BD_MemoryClock       = P96BD_Dummy + 9;
  P96BD_PCIVendorID          = P96BD_Dummy + 10;
  P96BD_PCIProductID         = P96BD_Dummy + 11;
  P96BD_MonitorVendorID      = P96BD_Dummy + 12; // new 2.354
  P96BD_MonitorProduct       = P96BD_Dummy + 13; // new 2.354
  P96BD_MonitorProductID     = P96BD_Dummy + 14; // new 2.354
  P96BD_MonitorHSyncMin      = P96BD_Dummy + 15; // new 2.354
  P96BD_MonitorHSyncMax      = P96BD_Dummy + 16; // new 2.354
  P96BD_MonitorVSyncMin      = P96BD_Dummy + 17; // new 2.354
  P96BD_MonitorVSyncMax      = P96BD_Dummy + 18; // new 2.354
  P96BD_MonitorDotClockMin   = P96BD_Dummy + 19; // new 2.354
  P96BD_MonitorDotClockMax   = P96BD_Dummy + 20; // new 2.354
  P96BD_MonitorInputType     = P96BD_Dummy + 21; // new 2.354
  P96BD_MonitorEDIDVer       = P96BD_Dummy + 22; // new 2.354
  P96BD_MonitorEDIDRev       = P96BD_Dummy + 23; // new 2.354
  P96BD_MonitorDisplayWidth  = P96BD_Dummy + 24; // new 2.354
  P96BD_MonitorDisplayHeight = P96BD_Dummy + 25; // new 2.354

  P96BD_BoardDriver        = P96BD_Dummy + 26; // new 2.359
  P96BD_ChipDriver         = P96BD_Dummy + 27; // new 2.359
  P96BD_InternalMemorySize = P96BD_Dummy + 28; // new 2.359

var
  P96Base: PLibrary = nil;
  IP96: PInterface = nil;

function p96Obtain(): LongWord; syscall IGfx 60;
function p96Release(): LongWord; syscall IGfx 64;
procedure p96Expunge(); syscall IGfx 68;
function p96Clone(): PInterface; syscall IGfx 72;
function p96AllocBitMap(SizeX, SizeY, Depth, Flags: LongWord; Friend: PBitMap; RGBFormat: TRGBFTYPE): pBitMap; syscall IP96 76;
procedure p96FreeBitMap(BitMap: PBitMap); syscall IP96 80;
function p96GetBitMapAttr(BitMap: PBitMap; Attribute: LongWord): LongWord; syscall IP96 84;
function p96LockBitMap(BitMap: PBitMap; Buffer: PByte; Size: LongWord): LongInt; syscall IP96 88;
procedure p96UnlockBitMap(BitMap: PBitMap; Lock: LongInt); syscall IP96 92;
function p96BestModeIDTagList(Tags: PTagItem): LongWord; syscall IP96 96;
// 100 p96BestModeIDTags
function p96RequestModeIDTagList(Tags: PTagItem): LongWord; syscall IP96 104;
// 108 p96RequestModeIDTags
function p96AllocModeListTagList(Tags: PTagItem): PList; syscall IP96 112;
// 116 p96AllocModeListTags
procedure p96FreeModeList(List: PList); syscall IP96 120;
function p96GetModeIDAttr(Mode: LongWord; Attribute: LongWord): LongWord; syscall IP96 124;
function p96OpenScreenTagList(Tags: PTagItem): PScreen; syscall IP96 128;
// 132 p96OpenScreenTags
function p96CloseScreen(Screen: PScreen): Wordbool; syscall IP96 136;
procedure p96WritePixelArray(Ri: PRenderInfo; SrcX, SrcY: LongWord; Rp: PRastPort; DestX, DestY, SizeX, SizeY: LongWord); syscall IP96 140;
procedure p96ReadPixelArray(Ri: PRenderInfo; DestX, DestY: LongWord; Rp: PRastPort; SrcX, SrcY, SizeX, SizeY: LongWord); syscall IP96 144;
function p96WritePixel(Rp: PRastPort; X, Y, Color: LongWord): LongWord; syscall IP96 148;
function p96ReadPixel(Rp: PRastPort; X, Y: LongWord): LongWord; syscall IP96 152;
procedure p96RectFill(Rp: PRastPort; MinX, MinY, MaxX, MaxY, Color: LongWord); syscall IP96 156;
procedure p96WriteTrueColorData(Tci: PTrueColorInfo; SrcX, SrcY: LongWord; Rp: PRastPort; DestX, DestY, SizeX, SizeY: LongWord); syscall IP96 160;
procedure p96ReadTrueColorData(Tci: PTrueColorInfo; DestX, DestY: LongWord; Rp: PRastPort; SrcX, SrcY, SizeX, SizeY: LongWord); syscall IP96 164;
function p96PIP_OpenTagList(Tags: PTagItem): PWindow; syscall IP96 168;
// 172 p96PIP_OpenTags
function p96PIP_Close(Window: PWindow): Wordbool; syscall IP96 176;
function p96PIP_SetTagList(Window: PWindow; Tags: PTagItem): LongInt; syscall IP96 180;
// 184 p96PIP_SetTags
function p96PIP_GetTagList(Window: PWindow; Tags: PTagItem): LongInt; syscall IP96 188;
// 192 p96PIP_GetTags
function p96PIP_GetIMsg(Port: PMsgPort): PIntuiMessage; syscall IP96 196;
procedure p96PIP_ReplyIMsg(IntuiMessage: PIntuiMessage); syscall IP96 200;
function p96GetRTGDataTagList(Tags: PTagItem): LongInt; syscall IP96 204;
// 208 p96GetRTGDataTags
function p96GetBoardDataTagList(Board: LongWord; Tags: PTagItem): LongInt; syscall IP96 212;
// 216 p96GetBoardDataTags
function p96EncodeColor(RGBFormat: TRGBFTYPE; Color: LongWord): LongWord; syscall IP96 220;
function p96LockBitMapToBoard(Bm: PBitmap; Board_Number: LongWord; Buf: PByte; Size: LongWord): WordBool; syscall IP96 224;
procedure p96UnlockBitMapFromBoard(Bm: PBitmap; Modified: LongBool); syscall IP96 228;

// functions and procedures with array of PtrUInt go here
function p96BestModeIDTags(const Tags: array of PtrUInt): LongWord;
function p96RequestModeIDTags(const Tags: array of PtrUInt): LongWord;
function p96AllocModeListTags(const Tags: array of PtrUInt): PList;
function p96OpenScreenTags(const Tags: array of PtrUInt): PScreen;
function p96PIP_OpenTags(const Tags: array of PtrUInt): PWindow;
function p96PIP_SetTags(Window: PWindow; const Tags: array of PtrUInt): LongInt;
function p96PIP_GetTags(Window: PWindow; const Tags: array of PtrUInt): LongInt;
function p96GetRTGDataTags(const Tags: array of PtrUInt): LongInt;
function p96GetBoardDataTags(Board: LongWord; const Tags: array of PtrUInt): LongInt;

implementation

// functions and procedures with array of PtrUInt go here
function p96BestModeIDTags(const Tags: array of PtrUInt): LongWord;
begin
  p96BestModeIDTags := p96BestModeIDTagList(@Tags);
end;

function p96RequestModeIDTags(const Tags: array of PtrUInt): LongWord;
begin
  p96RequestModeIDTags := p96RequestModeIDTagList(@Tags);
end;

function p96AllocModeListTags(const Tags: array of PtrUInt): PList;
begin
  p96AllocModeListTags := p96AllocModeListTagList(@Tags);
end;

function p96OpenScreenTags(const Tags: array of PtrUInt): PScreen;
begin
  p96OpenScreenTags := p96OpenScreenTagList(@Tags);
end;

function p96PIP_OpenTags(const Tags: array of PtrUInt): PWindow;
begin
  p96PIP_OpenTags := p96PIP_OpenTagList(@Tags);
end;

function p96PIP_SetTags(Window: PWindow; const Tags: array of PtrUInt): LongInt;
begin
  p96PIP_SetTags := p96PIP_SetTagList(Window , @Tags);
end;

function p96PIP_GetTags(Window: PWindow; const Tags: array of PtrUInt): LongInt;
begin
  p96PIP_GetTags := p96PIP_GetTagList(Window , @Tags);
end;

function p96GetRTGDataTags(const Tags: array of PtrUInt): LongInt;
begin
  p96GetRTGDataTags := p96GetRTGDataTagList(@Tags);
end;

function p96GetBoardDataTags(Board: LongWord; const Tags: array of PtrUInt): LongInt;
begin
  p96GetBoardDataTags := p96GetBoardDataTagList(Board , @Tags);
end;

const
  LIBVERSION: LongWord = 0;

initialization
  P96Base := OpenLibrary(PICASSO96APINAME, LIBVERSION);
  if Assigned(P96Base) then
    IP96 := GetInterface(P96Base, 'main', 1, nil);
finalization
  if Assigned(IP96) then
    DropInterface(IP96);
  if Assigned(P96Base) then
    CloseLibrary(P96Base);
end.


