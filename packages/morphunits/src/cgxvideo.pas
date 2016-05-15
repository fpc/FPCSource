{
  This file is part of the Free Pascal MorphOS support package

  Copyright (c) 2015 Karoly Balogh
  member of the Free Pascal Development Team

  cgxvideo.library interface unit

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$mode fpc}
{$packrecords 2}
unit cgxvideo;

interface

uses
  exec, intuition, utility;

{
    Contents of this file is based on cgxvideo.h from the MorphOS SDK:

        $VER: cgxvideo.h 43.2 (13.08.2008)
        include file for cgxvideo.library
        Copyright (c) 1996-1998 by phase5 digital products
        Copyright (c) 1999-2008 by Vision Factory Development
        All Rights reserved.
}

const
  CGXVIDEONAME = 'cgxvideo.library';

var
  CGXVideoBase: PLibrary = nil;

type
  TVLayerHandle = Pointer;
  PVLayerHandle = ^TVLayerHandle;

const
  VOA_LeftIndent   = $88000001;
  VOA_RightIndent  = $88000002;
  VOA_TopIndent    = $88000003;
  VOA_BottomIndent = $88000004;

  VOA_SrcType      = $88000005;
  VOA_SrcWidth     = $88000006;
  VOA_SrcHeight    = $88000007;

  VOA_Error        = $88000008;

  VOA_UseColorKey  = $88000009;

  VOA_UseBackfill  = $8800000a;

  VOA_UseFilter    = $8800000c;

  VOA_BaseAddress  = $88000030;
  VOA_ColorKeyPen  = $88000031;
  VOA_ColorKey     = $88000032;

{* v42 additions *}
  VOA_Identifier   = $8800000b;

  VOA_FrameBase0   = $88000033;
  VOA_FrameBase1   = $88000034;
  VOA_FrameType    = $88000035;

  VOA_Width        = $88000036;
  VOA_Height       = $88000037;
  VOA_Modulo       = $88000038;     {* may be specified by CreateVLayerTagList() for v43 *}

{* V43 additions *}

  VOA_DoubleBuffer = $8800000d;
  VOA_InterLaced   = $8800000e;
  VOA_CaptureMode  = $8800000f;

  VOA_BaseOffset   = $88000039;

{* V50 additions *}

  VOA_FrameIndex   = $88000010;
  VOA_MultiBuffer  = $88000011;
  VOA_ZoomRect     = $88000012;

  VOA_BaseOffset0  = $88000040;
  VOA_BaseOffset1  = $88000041;
  VOA_BaseOffset2  = $88000042;
  VOA_BaseOffset3  = $88000043;
  VOA_BaseOffset4  = $88000044;
  VOA_BaseOffset5  = $88000045;


  VOA_Color0SP           = $88000050;
  VOA_Color1SP           = $88000051;
  VOA_Color2SP           = $88000052;
  VOA_Color3SP           = $88000053;
  VOA_Color4SP           = $88000054;
  VOA_Color5SP           = $88000055;
  VOA_Color6SP           = $88000056;
  VOA_Color7SP           = $88000057;
  VOA_Color8SP           = $88000058;
  VOA_Color9SP           = $88000059;
  VOA_Color10SP          = $8800005A;
  VOA_Color11SP          = $8800005B;
  VOA_Color12SP          = $8800005C;
  VOA_Color13SP          = $8800005D;
  VOA_Color14SP          = $8800005E;
  VOA_Color15SP          = $8800005F;

  VOA_SubPicture         = $88000060;
  VOA_EnableSP           = $88000061;
  VOA_StreamRectSP       = $88000062;
  VOA_ColConSP           = $88000063;
  VOA_HLEnableSP         = $88000065;
  VOA_HLRectSP           = $88000064;
  VOA_HLColConSP         = $88000066;
  VOA_SrcWidthSP         = $88000067;
  VOA_SrcHeightSP        = $88000068;

const
  VSQ_Dummy              = (TAG_USER + $A5000);
  VSQ_SupportedFeatures  = (VSQ_Dummy + 1);

  VSQ_FEAT_OVERLAY       = (1 shl 0);
  VSQ_FEAT_DOUBLEBUFFER  = (1 shl 1);
  VSQ_FEAT_MULTIBUFFER   = (1 shl 2);
  VSQ_FEAT_COLORKEYING   = (1 shl 3);
  VSQ_FEAT_FILTERING     = (1 shl 4);
  VSQ_FEAT_CAPTUREMODE   = (1 shl 5);
  VSQ_FEAT_INTERLACE     = (1 shl 6);
  VSQ_FEAT_ZOOMRECT      = (1 shl 7);
  VSQ_FEAT_SUBPICTURE    = (1 shl 8);

  VSQ_SupportedFormats   = (VSQ_Dummy + 2);

  VSQ_FMT_YUYV           = (1 shl 0);
  VSQ_FMT_R5G5B5_LE      = (1 shl 1);
  VSQ_FMT_R5G6B5_LE      = (1 shl 2);
  VSQ_FMT_YUV420_PLANAR  = (1 shl 3);

  VSQ_MaxWidth           = (VSQ_Dummy + 3);       {* will return maximum width for overlay *}

  VSQ_MaxWidthSP         = (VSQ_Dummy + 4);       {* will return maximum width for subpicture *}

{* possible errors returned with VOA_Error tag *}
const
  VOERR_OK               = 0;       {* No error *}
  VOERR_INVSCRMODE       = 1;       {* video overlay not possible for that mode *}
  VOERR_NOOVLMEMORY      = 2;       {* No memory for video overlay *}
  VOERR_INVSRCFMT        = 3;       {* Source format not supported *}
  VOERR_NOMEMORY         = 4;       {* Not enough memory *}

{* Source data types              *}
{* see cgxvideo.doc for more info *}
const
  SRCFMT_YUV16    = 0;       {* obsolete *}
  SRCFMT_YCbCr16  = 1;
  SRCFMT_RGB15    = 2;       {* this format is actually byte swapped *}
  SRCFMT_R5G5B5PC = 2;
  SRCFMT_RGB16    = 3;       {* this format is actually byte swapped *}
  SRCFMT_R5G6B5PC = 3;
  SRCFMT_YCbCr420 = 4;       {* YUV planar *}

function CreateVLayerHandleTagList(screen: PScreen location 'a0'; tags: PTagItem location 'a1'): PVLayerHandle; syscall CGXVideoBase 30;
function DeleteVLayerHandle(VLayerHandle: PVLayerHandle location 'a0'): LongWord; syscall CGXVideoBase 36;
function AttachVLayerTagList(VLayerHandle: PVLayerHandle location 'a0'; window: PWindow location 'a1'; tags: PTagItem location 'a2'): LongWord; syscall CGXVideoBase 42;
function DetachVLayer(VLayerHandle: PVLayerHandle location 'a0'): LongWord; syscall CGXVideoBase 48;
function GetVLayerAttr(VLayerHandle: PVLayerHandle location 'a0'; AttrID: LongWord location 'd0'): LongWord; syscall CGXVideoBase 54;
function LockVLayer(VLayerHandle: PVLayerHandle location 'a0'): LongWord; syscall CGXVideoBase 60;
function UnlockVLayer(VLayerHandle: PVLayerHandle location 'a0'): LongWord; syscall CGXVideoBase 66;
procedure SetVLayerAttrTagList(VLayerHandle: PVLayerHandle location 'a0'; tags: PTagItem location 'a1'); syscall CGXVideoBase 72;
procedure SwapVLayerBuffer(VLayerHandle: PVLayerHandle location 'a0'); syscall CGXVideoBase 96;
function WriteSPLine(VLayerHandle: PVLayerHandle location 'a0'; buffer: PByte location 'a1'; x: LongInt location 'a0'; y: LongInt location 'd1'; w: LongInt location 'd2'): LongWord; syscall CGXVideoBase 102;
function QueryVLayerAttr(screen: PScreen location 'a0'; AttrID: LongWord location 'd0'): LongWord; syscall CGXVideoBase 108;

{
#if !defined(USE_INLINE_STDARG)
ULONG AttachVLayerTags(struct VLayerHandle *,struct Window *,Tag, ...);
#endif

#if !defined(USE_INLINE_STDARG)
struct VLayerHandle *CreateVLayerHandleTags(struct Screen *,Tag, ...);
#endif

#if !defined(USE_INLINE_STDARG)
void SetVLayerAttrTags(struct VLayerHandle *,Tag, ...);
#endif
}

function InitCGXVideoLibrary: boolean;

implementation

const
    { Change VERSION and LIBVERSION to proper values }
    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

function InitCGXVideoLibrary: boolean;
begin
  InitCGXVideoLibrary := Assigned(CGXVideoBase);
end;

initialization
  CGXVideoBase := OpenLibrary(CGXVIDEONAME,LIBVERSION);
finalization
  if Assigned(CGXVideoBase) then
    CloseLibrary(CGXVideoBase);
end.
