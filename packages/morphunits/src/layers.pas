{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    layers.library interface unit for MorphOS/PowerPC

    Based on work of Nils Sjoholm member of the Amiga RTL
    development team.

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$PACKRECORDS 2}
unit layers;

interface

uses exec, agraphics, utility;

const
  LAYERSIMPLE          = 1;
  LAYERSMART           = 2;
  LAYERSUPER           = 4;
  LAYERUPDATING        = $10;
  LAYERBACKDROP        = $40;
  LAYERREFRESH         = $80;
  LAYER_CLIPRECTS_LOST = $100; // during BeginUpdate or during layerop this happens if out of memory
  LAYERIREFRESH        = $200;
  LAYERIREFRESH2       = $400;

type
  PLayer_Info = ^TLayer_Info;
  TLayer_Info = packed record
    top_layer        : PLayer;
    check_lp         : PLayer;                // Private
    obs              : PClipRect;
    FreeClipRects    : PClipRect;             // Private
    PrivateReserve1,                          // Private
    PrivateReserve2  : LongInt;               // Private
    Lock             : TSignalSemaphore;      // Private
    gs_Head          : TMinList;              // Private
    PrivateReserve3  : SmallInt;              // Private
    PrivateReserve4  : APTR;                  // Private
    Flags            : Word;
    fatten_count     : ShortInt;              // Private
    LockLayersCount  : ShortInt;              // Private
    PrivateReserve5  : SmallInt;              // Private
    BlankHook,                                // Private
    LayerInfo_extra  : APTR;                  // Private
  end;

const
  NEWLAYERINFO_CALLED = 1;

// LAYERS_NEVERBACKFILL, available since v52.22
// unlike with NOBACKFILL, the new layer will not be filled with contents
// of layers underneath it, contents is lost after resize
  LAYERS_NEVERBACKFILL   = 2;
  LAYERS_NOBACKFILL      = 1;
  LAYERS_BACKFILL        = 0;

// Tags for Create#?LayerTagList
  LA_Dummy        = TAG_USER + 1024;
  LA_BackfillHook = LA_Dummy + $0001;
  LA_TransRegion  = LA_Dummy + $0002;
  LA_TransHook    = LA_Dummy + $0003;
  LA_WindowPtr    = LA_Dummy + $0004;
  LA_SuperBitMap  = LA_Dummy + $0005; // replaces bm2 in function call

  LR_Dummy = TAG_USER + 1150;
  LR_Destination_RastPort = LR_Dummy + 1; // PRastPort to render in
  LR_Destination_BitMap   = LR_Dummy + 2; // PBitMap to render in. mutually exclusive with LR_Destination_RastPort. Do note that the destination
    // bitmap (or the rastport's bitmap) MUST be in the same format as the source you want to render from!
  LR_Destination_Bounds   = LR_Dummy + 3; // PRectangle. the graphics will be rendered inside of the given boundaries. if not passed, the call assumes the buffer has at least the same size as LayerInfo
  LR_LayerInfo_Bounds     = LR_Dummy + 4; // PRectangle. limits the portion of a LayerInfo to draw
  LR_Erase                = LR_Dummy + 5; // LongBool. setting to FALSE will make the layers be drawn without the background being cleared with the screen's backfill hook TRUE by default
  LR_RenderList           = LR_Dummy + 6; // PPLayer. a nil terminated list of PLayer pointers to render if they are within given bounds
  LR_IgnoreList           = LR_Dummy + 7; // PPLayer. a nil terminated list of PLayer pointers to ommit when rendering the layerinfo. mutually exclusive with LR_RenderList!

  LAYERSNAME: PChar = 'layers.library';

var
  LayersBase: PLibrary = nil;

procedure InitLayers(Li: PLayer_Info location 'a0'); SysCall LayersBase 030;
function CreateUpfrontLayer(Li: PLayer_Info location 'a0'; Bm: PBitmap location 'a1'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'; Flags: LongInt location 'd4'; Bm2: PBitmap location 'a2'): PLayer; SysCall LayersBase 036;
function CreateBehindLayer(Li: PLayer_Info location 'a0'; Bm: PBitmap location 'a1'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'; Flags: LongInt location 'd4'; Bm2: PBitmap location 'a2'): PLayer; SysCall LayersBase 042;
function UpfrontLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'): LongInt; SysCall LayersBase 048;
function BehindLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'): LongInt; SysCall LayersBase 054;
function MoveLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'; Dx: LongInt location 'd0'; Dy: LongInt location 'd1'): LongInt; SysCall LayersBase 060;
function SizeLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'; Dx: LongInt location 'd0'; Dy: LongInt location 'd1'): LongInt; SysCall LayersBase 066;
procedure ScrollLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'; Dx: LongInt location 'd0'; Dy: LongInt location 'd1'); SysCall LayersBase 072;
function BeginUpdate(L: PLayer location 'a0'): LongInt; SysCall LayersBase 078;
procedure EndUpdate(Layer: PLayer location 'a0'; Flag: LongWord location 'd0'); SysCall LayersBase 084;
function DeleteLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'): LongInt; SysCall LayersBase 090;
procedure LockLayer(Dummy: LongInt location 'a0'; Layer: PLayer location 'a1'); SysCall LayersBase 096;
procedure UnlockLayer(Layer: PLayer location 'a0'); SysCall LayersBase 102;
procedure LockLayers(Li: PLayer_Info location 'a0'); SysCall LayersBase 108;
procedure UnlockLayers(Li: PLayer_Info location 'a0'); SysCall LayersBase 114;
procedure LockLayerInfo(Li: PLayer_Info location 'a0'); SysCall LayersBase 120;
procedure SwapBitsRastPortClipRect(Rp: PRastPort location 'a0'; Cr: PClipRect location 'a1'); SysCall LayersBase 126;
function WhichLayer(Li: PLayer_Info location 'a0'; X: LongInt location 'd0'; Y: LongInt location 'd1'): PLayer; SysCall LayersBase 132;
procedure UnlockLayerInfo(Li: PLayer_Info location 'a0'); SysCall LayersBase 138;
function NewLayerInfo: PLayer_Info; SysCall LayersBase 144;
procedure DisposeLayerInfo(Li: PLayer_Info location 'a0'); SysCall LayersBase 150;
function FattenLayerInfo(Li: PLayer_Info location 'a0'): LongInt; SysCall LayersBase 156;
procedure ThinLayerInfo(Li: PLayer_Info location 'a0'); SysCall LayersBase 162;
function MoveLayerInFrontOf(Layer_To_Move: PLayer location 'a0'; Other_Layer: PLayer location 'a1'): LongInt; SysCall LayersBase 168;
function InstallClipRegion(Layer: PLayer location 'a0'; const Region: PRegion location 'a1'): PRegion; SysCall LayersBase 174;
function MoveSizeLayer(Layer: PLayer location 'a0'; Dx: LongInt location 'd0'; Dy: LongInt location 'd1'; Dw: LongInt location 'd2'; Dh: LongInt location 'd3'): LongInt; SysCall LayersBase 180;
function CreateUpfrontHookLayer(Li: PLayer_Info location 'a0'; Bm: PBitmap location 'a1'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'; Flags: LongInt location 'd4'; Hook: PHook location 'a3'; Bm2: PBitmap location 'a2'): PLayer; SysCall LayersBase 186;
function CreateBehindHookLayer(Li: PLayer_Info location 'a0'; Bm: PBitmap location 'a1'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'; Flags: LongInt location 'd4'; Hook: PHook location 'a3'; Bm2: PBitmap location 'a2'): PLayer; SysCall LayersBase 192;
function InstallLayerHook(Layer: PLayer location 'a0'; Hook: PHook location 'a1'): PHook; SysCall LayersBase 198;
function InstallLayerInfoHook(Li: PLayer_Info location 'a0'; const Hook: PHook location 'a1'): PHook; SysCall LayersBase 204;
procedure SortLayerCR(Layer: PLayer location 'a0'; Dx: LongInt location 'd0'; Dy: LongInt location 'd1'); SysCall LayersBase 210;
procedure DoHookClipRects(Hook: PHook location 'a0'; RPort: PRastPort location 'a1'; const Rect: PRectangle location 'a2'); SysCall LayersBase 216;

// V50 (MorphOS)
function CreateUpfrontLayerTagList(Li: PLayer_Info location 'a0'; Bm: PBitmap location 'a1'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'; Flags: LongInt location 'd4'; Taglist: PTagItem location 'a2'): PLayer; SysCall LayersBase 234;
function CreateBehindLayerTagList(Li: PLayer_Info location 'a0'; Bm: PBitmap location 'a1'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'; Flags: LongInt location 'd4'; Taglist: PTagItem location 'a2'): PLayer; SysCall LayersBase 240;

// V52 (MorphOS)
function WhichLayerBehindLayer(L: PLayer location 'a0'; X: LongInt location 'd0'; Y: LongInt location 'd1'): PLayer; SysCall LayersBase 252;
function IsLayerVisible(L: PLayer location 'a0'): LongBool; SysCall LayersBase 258;
function RenderLayerInfoTagList(Li: PLayer_Info location 'a0'; Tags: PTagItem location 'a1'): LongBool; SysCall LayersBase 282;

procedure LockLayerUpdates(L: PLayer location 'a0'); SysCall LayersBase 288;
procedure UnlockLayerUpdates(L: PLayer location 'a0'); SysCall LayersBase 294;

function IsVisibleInLayer(L: PLayer location 'a0'; X0: LongInt location 'd0'; Y0: LongInt location 'd1'; X1: LongInt location 'd2'; Y1: LongInt location 'd3'): LongBool; SysCall LayersBase 300;
function IsLayerHitable(L: PLayer location 'a0'): LongBool; SysCall LayersBase 306;

// Var args Version
function RenderLayerInfoTags(Li: PLayer_Info; const Tags: array of PtrUInt): LongBool; inline;

{ Helper func }
function InitLayersLibrary : boolean;

implementation

function RenderLayerInfoTags(Li: PLayer_Info; const Tags: array of PtrUInt): LongBool; inline;
begin
  RenderLayerInfoTags := RenderLayerInfoTagList(Li, @Tags);
end;

const
  { Change VERSION and LIBVERSION to proper values }
  VERSION : string[2] = '50';
  LIBVERSION : longword = 50;

function InitLayersLibrary : boolean;
begin
  InitLayersLibrary := Assigned(LayersBase);
end;

initialization
  LayersBase := OpenLibrary(LAYERSNAME,LIBVERSION);
finalization
  if Assigned(LayersBase) then
    CloseLibrary(LayersBase);
end.
