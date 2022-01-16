{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Free Pascal development team

    layers.library functions for Amiga OS 4.x

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$PACKRECORDS 2}

unit layers;

interface

uses
  exec, agraphics, utility;

const
  LAYERSIMPLE           = 1;
  LAYERSMART            = 2;
  LAYERSUPER            = 4;
  LAYEROFFSCREEN        = 8;     // New for V53 (system private)
  LAYERUPDATING         = $10;
  LAYERBACKDROP         = $40;
  LAYERREFRESH          = $80;
  LAYER_CLIPRECTS_LOST  = $100;  // during BeginUpdate or during layerop this happens if out of memory
  LAYERIREFRESH         = $200;
  LAYERIREFRESH2        = $400;
  LAYERSAVEBACK         = $800;  // New for V44: Set if clips are saved back
  LAYERHIDDEN           = $1000; // New for V45: Layer is invisible
  LAYERSTAYTOP          = $2000; // New for V45: Layer can't be moved behind other layers
  LAYERMOVECHANGESSHAPE = $4000; // New for V45: Report MoveLayer() to shapechangehook
  LAYERBACKUP           = $8000; // New for V53 (system private)

  ALPHA_PREFIX = $01000000;
type
  PLayer_Info = ^TLayer_Info;
  TLayer_Info = record
    top_layer: PLayer;         // Frontmost layer
    resPtr1: Pointer;          // V45 spare
    resPtr2: Pointer;          // Another V45 spare
    FreeClipRects: PClipRect;  // Implements a backing store of cliprects to avoid frequent re-allocation of cliprects. Private
    Bounds: TRectangle;        // clipping bounds of this layer info. All layers are clipped against this
    Lock: TSignalSemaphore;    // Layer_Info lock
    gs_Head: TMinList;         // linked list of all semaphores of all layers within this layer info
    PrivateReserve3: Smallint; // Private
    ClipRectPool: APTR;        // V45: Pool for cliprects (private use only)
    Flags: Word;
    fatten_count: Shortint;    // V45 spare, no longer used
    LockLayersCount: Shortint; // Counts # times LockLayers has been called
    PrivateReserve5: smallint; // Private
    BlankHook: PHook;          // LayerInfo backfill hook
    LayerInfo_extra: Pointer;  // Private
  end;

const
  NEWLAYERINFO_CALLED = 1;
  LIF_EXTLBOUNDS  = $0200; // System private
  LIF_OFFSCREEN   = $0400; // System private
  LIF_SYNCCOMPOSE = $0800; // System private
  LIF_SYNCALWAYS  = $1000; // System private
  LIF_COLLECTONLY = $2000; // System private
  LIF_IGNORE      = $4000; // System private
  LIF_OPTIMIZE    = $8000; // System private

  // Special backfill hook values you may want to install here.
  LAYERS_NOBACKFILL = PHook(1);  // the value needed to get no backfill hook
  LAYERS_BACKFILL   = PHook(0);  // the value needed to get the default backfill hook

  // Special codes for ShowLayer(): Give this as target layer where to move your layer to.
  LAYER_BACKMOST  = PLayer(0);
  LAYER_FRONTMOST = PLayer(1);

  //CreateBackFillHookA() attributes
  LAYERS_DUMMY = TAG_USER;
  BFHA_APen    = LAYERS_DUMMY + 0; // foreground color (def not 0)
  BFHA_BPen    = LAYERS_DUMMY + 1; // background color (def not 0)
  BFHA_DrMd    = LAYERS_DUMMY + 2; // drawmode (def JAM2)
  BFHA_PatSize = LAYERS_DUMMY + 3; // pattern size, see SetAfPt()
  BFHA_Pattern = LAYERS_DUMMY + 4; // the pattern
  BFHA_BitMap  = LAYERS_DUMMY + 5; // bitmap to use as backfill
  BFHA_Width   = LAYERS_DUMMY + 6; // width of bm
  BFHA_Height  = LAYERS_DUMMY + 7; // height of bm
  BFHA_OffsetX = LAYERS_DUMMY + 8; // x offset into the bm
  BFHA_OffsetY = LAYERS_DUMMY + 9; // y offset into the bm

  //CreateLayerA() attributes
  LAYA_MinX        = LAYERS_DUMMY + 30; // upper left corner
  LAYA_MinY        = LAYERS_DUMMY + 31; // of layer
  LAYA_MaxX        = LAYERS_DUMMY + 32; // lower right corner
  LAYA_MaxY        = LAYERS_DUMMY + 33; // of layer
  LAYA_ShapeRegion = LAYERS_DUMMY + 34; // shape of this layer
  LAYA_ShapeHook   = LAYERS_DUMMY + 35; // hook to create layer shape
  LAYA_InFrontOf   = LAYERS_DUMMY + 36; // create the layer in front of the given one
  LAYA_BitMap        = LAYERS_DUMMY + 37; // common bitmap used by all layers
  LAYA_SuperBitMap   = LAYERS_DUMMY + 38; // the superbitmap, sets LAYERSUPER
  LAYA_SimpleRefresh = LAYERS_DUMMY + 39; // make it a simple refresh layer
  LAYA_SmartRefresh  = LAYERS_DUMMY + 40; // smart refresh layer (default TRUE)
  LAYA_Hidden        = LAYERS_DUMMY + 41; // make it invisible
  LAYA_Backdrop      = LAYERS_DUMMY + 42; // request backdrop layer
  LAYA_Flags         = LAYERS_DUMMY + 43; // layer flags
  LAYA_BackFillHook  = LAYERS_DUMMY + 44; // backfill hook for this layer
  LAYA_Behind        = LAYERS_DUMMY + 45; // create behind layer (default FALSE)
  LAYA_StayTop       = LAYERS_DUMMY + 46; // create a window that stays on top of all other layers
  LAYA_AlphaClips    = LAYERS_DUMMY + 47; // alpha shape of this layer
  LAYA_AlphaHook     = LAYERS_DUMMY + 48; // hook to create layer alpha shape
  LAYA_Opaqueness    = LAYERS_DUMMY + 49; // opaqueness of this layer
  LAYA_Window        = LAYERS_DUMMY + 50; // window associated to this layer (system use only)
  LAYA_Padding       = LAYERS_DUMMY + 51; // extra padding on layer sides, extending screen redraw area (system use only)

  //SetLayerInfoAttrsA()/GetLayerInfoAttrsA() attributes
  LAYERINFO_BackFillHook    = LAYERS_DUMMY + 100; // Backfill hook for this LayerInfo
  LAYERINFO_Bounds          = LAYERS_DUMMY + 101; // Clipping bounds for this LayerInfo
  LAYERINFO_OffScreenBitMap = LAYERS_DUMMY + 102; // Common offscreen bitmap for this LayerInfo's layers
  LAYERINFO_ComposeHook     = LAYERS_DUMMY + 103; // Compositing hook for this LayerInfo
  LAYERINFO_ComposeLock     = LAYERS_DUMMY + 104; // Semaphore protecting compositing hook

// The message a backfill hook receives
type
  PBackFillMessage = ^TBackFillMessage;
  TBackFillMessage = record
    Layer: PLayer;
    Bounds: TRectangle;
    OffsetX: LongInt;
    OffsetY: LongInt;
  end;

// The message a compose hook receives (system private)
  PLayerComposeMsg = ^TLayerComposeMsg;
  TLayerComposeMsg = record
    Action: LongWord;
    LayerInfo: PLayer_Info;
    BackBitMap: PBitMap;
    Bounds: TRectangle;
  end;
const
  LCM_COMPOSE = 0;
  LCM_DAMAGE  = 1;

  LAYERSNAME: PChar = 'layers.library';
var
  LayersBase: PLibrary = nil;
  ILayers: PInterface = nil;

function LayersObtain(): LongWord; syscall ILayers 60;
function LayersRelease(): LongWord; syscall ILayers 64;
procedure LayersExpunge(); syscall ILayers 68;
function LayersClone(): PInterface; syscall ILayers 72;
procedure InitLayers(Li: PLayer_Info); syscall ILayers 76;
function CreateUpfrontLayer(Li: PLayer_Info; Bm: PBitMap; X0, Y0, X1, Y1, Flags: LongInt; Bm2: PBitMap): PLayer; syscall ILayers 80;
function CreateBehindLayer(Li: PLayer_Info; Bm: PBitMap; X0, Y0, X1, Y1, Flags: LongInt; Bm2: PBitMap): PLayer; syscall ILayers 84;
function UpfrontLayer(Dummy: LongInt; Layer: PLayer): LongInt; syscall ILayers 88;
function BehindLayer(Dummy: LongInt; Layer: PLayer): LongInt; syscall ILayers 92;
function MoveLayer(Dummy: LongInt; Layer: PLayer; Dx, Dy: LongInt): LongInt; syscall ILayers 96;
function SizeLayer(Summy: LongInt; Layer: PLayer; Dx, Dy: LongInt): LongInt; syscall ILayers 100;
procedure ScrollLayer(Dummy: LongInt; Layer: PLayer; Dx, Dy: LongInt); syscall ILayers 104;
function BeginUpdate(L: PLayer): LongInt; syscall ILayers 108;
procedure EndUpdate(Layer: PLayer; Flag: LongWord); syscall ILayers 112;
function DeleteLayer(Dummy: LongInt; Layer: PLayer): LongInt; syscall ILayers 116;
procedure LockLayer(Dummy: LongInt; Layer: PLayer); syscall ILayers 120;
procedure UnlockLayer(Layer: PLayer); syscall ILayers 124;
procedure LockLayers(Li: PLayer_Info); syscall ILayers 128;
procedure UnlockLayers(Li: PLayer_Info); syscall ILayers 132;
procedure LockLayerInfo(Li: PLayer_Info); syscall ILayers 136;
procedure SwapBitsRastPortClipRect(Rp: PRastPort; Cr: PClipRect); syscall ILayers 140;
function WhichLayer(Li: PLayer_Info; X, Y: LongInt): PLayer; syscall ILayers 144;
procedure UnlockLayerInfo(Li: PLayer_Info); syscall ILayers 148;
function NewLayerInfo: PLayer_Info; syscall ILayers 152;
procedure DisposeLayerInfo(Li: PLayer_Info); syscall ILayers 156;
function FattenLayerInfo(Li: PLayer_Info): LongInt; syscall ILayers 160;
procedure ThinLayerInfo(Li: PLayer_Info); syscall ILayers 164;
function MoveLayerInFrontOf(LayerToMove: PLayer; OtherLayer: PLayer): LongInt; syscall ILayers 168;
function InstallClipRegion(Layer: PLayer; const Region: PRegion): PRegion; syscall ILayers 172;
function MoveSizeLayer(Layer: PLayer; Dx, Dy, Dw, Dh: LongInt): LongInt; syscall ILayers 176;
function CreateUpfrontHookLayer(Li: PLayer_Info; Bm: PBitMap; X0, Y0, X1, Y1, Flags: LongInt; Hook: PHook; Bm2: PBitMap): PLayer; syscall ILayers 180;
function CreateBehindHookLayer(Li: PLayer_Info; Bm: PBitMap; X0, Y0, X1, Y1, Flags: LongInt; Hook: PHook; Bm2: PBitMap): PLayer; syscall ILayers 184;
function InstallLayerHook(Layer: PLayer; Hook: PHook): PHook; syscall ILayers 188;
function InstallLayerInfoHook(Li: PLayer_Info; const Hook: PHook): PHook; syscall ILayers 192;
procedure SortLayerCR(Layer: PLayer; Dx, Dy: LongInt); syscall ILayers 196;
procedure DoHookClipRects(Hook: PHook; RPort: PRastPort; const Rect: PRectangle); syscall ILayers 200;
function LayerOccluded(Layer: PLayer): LongInt; syscall ILayers 204;
function HideLayer(Layer: PLayer): LongInt; syscall ILayers 208;
function ShowLayer(Layer: PLayer): LongInt; syscall ILayers 212;
function SetLayerInfoBounds(Li: PLayer_Info; Bounds: PRectangle): LongInt; syscall ILayers 216;
function AllocClipRect(Li: PLayer_Info): PClipRect; syscall ILayers 220;
procedure FreeClipRect(Li: PLayer_Info; Cr: PClipRect); syscall ILayers 224;
function CreateLayerA(Li: PLayer_Info; TList: PTagItem): PLayer; syscall ILayers 228;
// 232 CreateLayer
function ChangeLayerShape(Layer: PLayer; Region: PRegion; Hook: PHook): PRegion; syscall ILayers 236;
function CreateBackFillHookA(TList: PTagItem): PHook; syscall ILayers 240;
// 244 CreateBackFillHook
procedure DeleteBackFillHook(Hook: PHook); syscall ILayers 248;
function SetBackFillHookAttrsA(Hook: PHook; Tags: PTagItem): LongWord; syscall ILayers 252;
// 256 SetBackFillHookAttrs
function GetBackFillHookAttrsA(Hook: PHook; Tags: PTagItem): LongWord; syscall ILayers 260;
// 264 GetBackFillHookAttrs
function ComposeLayersA(L: PLayer; Rect: PRectangle; TagList: PTagItem): LongWord; syscall ILayers 268;
// 272 ComposeLayers
function GetOffScreenLayerBitMap(L: PLayer; FallBack: PBitMap; Bounds: PRectangle): LongWord; syscall ILayers 276;
function SetLayerInfoAttrsA(Li: PLayer_Info; TagList: PTagItem): LongWord; syscall ILayers 280;
// 284 SetLayerInfoAttrs
function GetLayerInfoAttrsA(Li: PLayer_Info; TagList: PTagItem): LongWord; syscall ILayers 288;
// 292 GetLayerInfoAttrs
function ChangeLayerAlpha(L: PLayer; Clips: PClipRect; Hook: PHook): PClipRect; syscall ILayers 296;
function SetForegroundAlpha(L: PLayer; TemplatePtr: APTR; TemplateType, BytesPerRow: LongWord; Left, Top: LongInt; Width, Height, Flags: LongWord): LongWord; syscall ILayers 300;
function SetLayerOpaqueness(L: PLayer; Opaqueness: LongWord): LongWord; syscall ILayers 304;
function SetLayerOverlayRect(L: PLayer; Left, Top, Width, Height: LongInt; Flags: LongWord): LongWord; syscall ILayers 308;

implementation

const
  // Change VERSION and LIBVERSION to proper values
  LIBVERSION : longword = 0;

initialization
  LayersBase := OpenLibrary(LAYERSNAME, LIBVERSION);
  if Assigned(LayersBase) then
    ILayers := GetInterface(LayersBase, 'main', 1, nil);
finalization
  if Assigned(ILayers) then
    DropInterface(ILayers);
  if Assigned(LayersBase) then
    CloseLibrary(LayersBase);
end.



