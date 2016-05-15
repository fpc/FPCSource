{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    layers.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit layers;

interface
uses
  exec, agraphics, utility;

const

    LAYERSIMPLE         = 1;
    LAYERSMART          = 2;
    LAYERSUPER          = 4;
    LAYERUPDATING       = $10;
    LAYERBACKDROP       = $40;
    LAYERREFRESH        = $80;
    LAYER_CLIPRECTS_LOST = $100;        { during BeginUpdate }
                                        { or during layerop }
                                        { this happens if out of memory }
    LMN_REGION          = -1;

type
 PLayer_Info = ^TLayer_Info;
 TLayer_Info = record
    top_layer           : PLayer;
    check_lp            : PLayer;              { !! Private !! }
    obs                 : PClipRect;
    FreeClipRects       : PClipRect;              { !! Private !! }
    PrivateReserve1,                            { !! Private !! }
    PrivateReserve2     : LongInt;              { !! Private !! }
    Lock                : TSignalSemaphore;      { !! Private !! }
    gs_Head             : TMinList;              { !! Private !! }
    PrivateReserve3     : SmallInt;                 { !! Private !! }
    PrivateReserve4     : Pointer;              { !! Private !! }
    Flags               : WORD;
    fatten_count        : Shortint;                 { !! Private !! }
    LockLayersCount     : Shortint;                 { !! Private !! }
    PrivateReserve5     : SmallInt;                 { !! Private !! }
    BlankHook,                                  { !! Private !! }
    LayerInfo_extra     : Pointer;              { !! Private !! }
 end;

const
    NEWLAYERINFO_CALLED = 1;

{
 * LAYERS_NOBACKFILL is the value needed to get no backfill hook
 * LAYERS_BACKFILL is the value needed to get the default backfill hook
 }
 LAYERS_NOBACKFILL      = 1;
 LAYERS_BACKFILL        = 0;

 LAYERSNAME : PChar = 'layers.library';

var
  LayersBase : PLibrary;

function BeginUpdate(Layer: PLayer): LongInt; syscall LayersBase 13; 
function BehindLayer(dummy: LongInt; Layer: PLayer): LongInt; syscall LayersBase 9;
function CreateBehindHookLayer(LayerInfo: PLayer_Info; Bitmap1: PBitMap; x0: LongInt; y0: LongInt; x1: LongInt; y1: LongInt; Flags: LongInt; Hook: PHook; SuperBitmap2: PBitMap): PLayer; syscall LayersBase 32;
function CreateBehindLayer(LayerInfo: PLayer_Info; Bitmap1: PBitMap; x0: LongInt; y0: LongInt; x1: LongInt; y1: LongInt; Flags: LongInt; SuperBitmap2: PBitMap): PLayer; syscall LayersBase 7;
function CreateUpfrontHookLayer(LayerInfo: PLayer_Info; Bitmap1: PBitMap; x0: LongInt; y0: LongInt; x1: LongInt; y1: LongInt; Flags: LongInt; Hook: PHook; SuperBitmap2: PBitMap): PLayer; syscall LayersBase 31;
function CreateUpfrontLayer(LayerInfo: PLayer_Info; Bitmap1: PBitMap; x0: LongInt; y0: LongInt; x1: LongInt; y1: LongInt; Flags: LongInt; SuperBitmap2: PBitMap): PLayer; syscall LayersBase 6;
function DeleteLayer(dummy: LongInt; Layer: PLayer): LongInt; syscall LayersBase 15;
procedure DisposeLayerInfo(LayerInfo: PLayer_Info); syscall LayersBase 25;
procedure DoHookClipRects(Hook: PHook; RPort: PRastPort;const Rect: PRectangle); syscall LayersBase 36;
procedure EndUpdate(Layer: PLayer; Flag: LongWord); syscall LayersBase 14;
function FattenLayerInfo(LayerInfo: PLayer_Info): LongInt; syscall LayersBase 26;
procedure InitLayers(LayerInfo: PLayer_Info); syscall LayersBase 5;
function InstallClipRegion(Layer: PLayer; const Region: PRegion): PRegion;  syscall LayersBase 29;
function InstallLayerHook(Layer: PLayer; Hook: PHook): PHook;  syscall LayersBase 33;
function InstallLayerInfoHook(LayerInfo: PLayer_Info; const Hook: PHook): PHook; syscall LayersBase 34;
procedure LockLayer(dummy: LongInt; Layer: PLayer); syscall LayersBase 16;
procedure LockLayerInfo(LayerInfo: PLayer_Info); syscall LayersBase 20;
procedure LockLayers(LayerInfo: PLayer_Info); syscall LayersBase 18;
function MoveLayer(dummy: LongInt; Layer: PLayer; dx: LongInt; dy: LongInt): LongInt; syscall LayersBase 10;
function MoveLayerInFrontOf(layer_to_move: PLayer; other_layer: PLayer): LongInt; syscall LayersBase 28;
function MoveSizeLayer(Layer: PLayer; dx: LongInt; dy: LongInt; dw: LongInt; dh: LongInt): LongInt; syscall LayersBase 30;
function NewLayerInfo: PLayer_Info; syscall LayersBase 24;
procedure ScrollLayer(dummy: LongInt; Layer: PLayer; dx: LongInt; dy: LongInt); syscall LayersBase 12;
function SizeLayer(dummy: LongInt; Layer: PLayer; dx: LongInt; dy: LongInt): LongInt; syscall LayersBase 11; 
procedure SortLayerCR(Layer: PLayer; dx: LongInt; dy: LongInt); syscall LayersBase 35;
procedure SwapBitsRastPortClipRect(rp: PRastPort; cr: PClipRect); syscall LayersBase  21;
procedure ThinLayerInfo(LayerInfo: PLayer_Info); syscall LayersBase 27;
procedure UnlockLayer(Layer: PLayer); syscall LayersBase 17;
procedure UnlockLayerInfo(LayerInfo: PLayer_Info); syscall LayersBase 23;
procedure UnlockLayers(LayerInfo: PLayer_Info); syscall LayersBase 19;
function UpfrontLayer(dummy: LongInt; Layer: PLayer): LongInt; syscall LayersBase 8;
function WhichLayer(LayerInfo: PLayer_Info; x: LongInt; y: LongInt): PLayer; syscall LayersBase 22;

implementation

initialization
  LayersBase := OpenLibrary(LAYERSNAME, 36);

finalization
  CloseLibrary(LayersBase);

eND. (* UNIT LAYERS *)



