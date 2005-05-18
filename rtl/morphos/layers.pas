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

uses exec, graphics, utility;

const
  LAYERSIMPLE         = 1;
  LAYERSMART          = 2;
  LAYERSUPER          = 4;
  LAYERUPDATING       = $10;
  LAYERBACKDROP       = $40;
  LAYERREFRESH        = $80;
  LAYER_CLIPRECTS_LOST = $100;          { during BeginUpdate }
                                        { or during layerop }
                                        { this happens if out of memory }
  LMN_REGION          = -1;

type
  pLayer_Info = ^tLayer_Info;
  tLayer_Info = packed record
    top_layer           : pLayer;
    check_lp            : pLayer;                { !! Private !! }
    obs                 : pClipRect;
    FreeClipRects       : pClipRect;             { !! Private !! }
    PrivateReserve1,                             { !! Private !! }
    PrivateReserve2     : Longint;               { !! Private !! }
    Lock                : tSignalSemaphore;      { !! Private !! }
    gs_Head             : tMinList;              { !! Private !! }
    PrivateReserve3     : smallint;              { !! Private !! }
    PrivateReserve4     : Pointer;               { !! Private !! }
    Flags               : WORD;
    fatten_count        : Shortint;              { !! Private !! }
    LockLayersCount     : Shortint;              { !! Private !! }
    PrivateReserve5     : smallint;              { !! Private !! }
    BlankHook,                                   { !! Private !! }
    LayerInfo_extra     : Pointer;               { !! Private !! }
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

procedure InitLayers(li : pLayer_Info location 'a0');
SysCall LayersBase 030;

function CreateUpfrontLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LongInt location 'd0'; y0 : LongInt location 'd1'; x1 : LongInt location 'd2'; y1 : LongInt location 'd3'; flags : LongInt location 'd4'; bm2 : pBitMap location 'a2') : pLayer;
SysCall LayersBase 036;

function CreateBehindLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LongInt location 'd0'; y0 : LongInt location 'd1'; x1 : LongInt location 'd2'; y1 : LongInt location 'd3'; flags : LongInt location 'd4'; bm2 : pBitMap location 'a2') : pLayer;
SysCall LayersBase 042;

function UpfrontLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1') : LongInt;
SysCall LayersBase 048;

function BehindLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1') : LongInt;
SysCall LayersBase 054;

function MoveLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1'; dx : LongInt location 'd0'; dy : LongInt location 'd1') : LongInt;
SysCall LayersBase 060;

function SizeLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1'; dx : LongInt location 'd0'; dy : LongInt location 'd1') : LongInt;
SysCall LayersBase 066;

procedure ScrollLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1'; dx : LongInt location 'd0'; dy : LongInt location 'd1');
SysCall LayersBase 072;

function BeginUpdate(l : pLayer location 'a0') : LongInt;
SysCall LayersBase 078;

procedure EndUpdate(layer : pLayer location 'a0'; flag : CARDINAL location 'd0');
SysCall LayersBase 084;

function DeleteLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1') : LongInt;
SysCall LayersBase 090;

procedure LockLayer(dummy : LongInt location 'a0'; layer : pLayer location 'a1');
SysCall LayersBase 096;

procedure UnlockLayer(layer : pLayer location 'a0');
SysCall LayersBase 102;

procedure LockLayers(li : pLayer_Info location 'a0');
SysCall LayersBase 108;

procedure UnlockLayers(li : pLayer_Info location 'a0');
SysCall LayersBase 114;

procedure LockLayerInfo(li : pLayer_Info location 'a0');
SysCall LayersBase 120;

procedure SwapBitsRastPortClipRect(rp : pRastPort location 'a0'; cr : pClipRect location 'a1');
SysCall LayersBase 126;

function WhichLayer(li : pLayer_Info location 'a0'; x : LongInt location 'd0'; y : LongInt location 'd1') : pLayer;
SysCall LayersBase 132;

procedure UnlockLayerInfo(li : pLayer_Info location 'a0');
SysCall LayersBase 138;

function NewLayerInfo : pLayer_Info;
SysCall LayersBase 144;

procedure DisposeLayerInfo(li : pLayer_Info location 'a0');
SysCall LayersBase 150;

function FattenLayerInfo(li : pLayer_Info location 'a0') : LongInt;
SysCall LayersBase 156;

procedure ThinLayerInfo(li : pLayer_Info location 'a0');
SysCall LayersBase 162;

function MoveLayerInFrontOf(layer_to_move : pLayer location 'a0'; other_layer : pLayer location 'a1') : LongInt;
SysCall LayersBase 168;

function InstallClipRegion(layer : pLayer location 'a0'; CONST region : pRegion location 'a1') : pRegion;
SysCall LayersBase 174;

function MoveSizeLayer(layer : pLayer location 'a0'; dx : LongInt location 'd0'; dy : LongInt location 'd1'; dw : LongInt location 'd2'; dh : LongInt location 'd3') : LongInt;
SysCall LayersBase 180;

function CreateUpfrontHookLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LongInt location 'd0'; y0 : LongInt location 'd1'; x1 : LongInt location 'd2'; y1 : LongInt location 'd3'; flags : LongInt location 'd4'; hook : pHook location 'a3'; bm2 : pBitMap location 'a2') : pLayer;
SysCall LayersBase 186;

function CreateBehindHookLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LongInt location 'd0'; y0 : LongInt location 'd1'; x1 : LongInt location 'd2'; y1 : LongInt location 'd3'; flags : LongInt location 'd4'; hook : pHook location 'a3'; bm2 : pBitMap location 'a2') : pLayer;
SysCall LayersBase 192;

function InstallLayerHook(layer : pLayer location 'a0'; hook : pHook location 'a1') : pHook;
SysCall LayersBase 198;

function InstallLayerInfoHook(li : pLayer_Info location 'a0'; CONST hook : pHook location 'a1') : pHook;
SysCall LayersBase 204;

procedure SortLayerCR(layer : pLayer location 'a0'; dx : LongInt location 'd0'; dy : LongInt location 'd1');
SysCall LayersBase 210;

procedure DoHookClipRects(hook : pHook location 'a0'; rport : pRastPort location 'a1'; CONST rect : pRectangle location 'a2');
SysCall LayersBase 216;

function InstallTransparentRegion(l : pLayer location 'a0'; r : pRegion location 'a1') : pRegion;
SysCall LayersBase 222;

function InstallTransparentRegionHook(l : pLayer location 'a0'; h : pHook location 'a1') : pHook;
SysCall LayersBase 228;

function CreateUpfrontLayerTagList(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LongInt location 'd0'; y0 : LongInt location 'd1'; x1 : LongInt location 'd2'; y1 : LongInt location 'd3'; flags : LongInt location 'd4'; taglist : pTagItem location 'a2') : pLayer;
SysCall LayersBase 234;

function CreateBehindLayerTagList(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LongInt location 'd0'; y0 : LongInt location 'd1'; x1 : LongInt location 'd2'; y1 : LongInt location 'd3'; flags : LongInt location 'd4'; taglist : pTagItem location 'a2') : pLayer;
SysCall LayersBase 240;

{
 Functions and procedures with array of const go here
}
{
function CreateUpfrontLayerTags(li : pLayer_Info; bm : pBitMap; x0 : LongInt; y0 : LongInt; x1 : LongInt; y1 : LongInt; flags : LongInt; const taglist : Array Of Const) : pLayer;
function CreateBehindLayerTags(li : pLayer_Info; bm : pBitMap; x0 : LongInt; y0 : LongInt; x1 : LongInt; y1 : LongInt; flags : LongInt; const taglist : Array Of Const) : pLayer;
}


{ Helper func }
function InitLayersLibrary : boolean;


implementation

const
  { Change VERSION and LIBVERSION to proper values }
  VERSION : string[2] = '50';
  LIBVERSION : longword = 50;

var
  layers_exit : Pointer;

procedure CloseLayersLibrary;
begin
  ExitProc := layers_exit;
  if LayersBase <> nil then begin
    CloseLibrary(LayersBase);
    LayersBase := nil;
  end;
end;

function InitLayersLibrary : boolean;
begin
  LayersBase := nil;
  LayersBase := OpenLibrary(LAYERSNAME,LIBVERSION);
  if LayersBase <> nil then begin
    layers_exit := ExitProc;
    ExitProc := @CloseLayersLibrary;
    InitLayersLibrary:=True;
  end else begin
    InitLayersLibrary:=False;
  end;
end;


end.
