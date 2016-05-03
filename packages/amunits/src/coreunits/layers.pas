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
    14 Jan 2003.

    Update for AmigaOS 3.9.
    Changed start code for unit.
    06 Feb 2003.

    Changed integer > smallint,
            cardinal > longword.
    09 Feb 2003.

    nils.sjoholm@mailbox.swipnet.se
}
{$PACKRECORDS 2}

UNIT layers;

INTERFACE
USES exec, agraphics, utility;

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
 pLayer_Info = ^tLayer_Info;
 tLayer_Info = record
    top_layer           : pLayer;
    check_lp            : pLayer;              { !! Private !! }
    obs                 : pClipRect;
    FreeClipRects       : pClipRect;              { !! Private !! }
    PrivateReserve1,                            { !! Private !! }
    PrivateReserve2     : Longint;              { !! Private !! }
    Lock                : tSignalSemaphore;      { !! Private !! }
    gs_Head             : tMinList;              { !! Private !! }
    PrivateReserve3     : smallint;                 { !! Private !! }
    PrivateReserve4     : Pointer;              { !! Private !! }
    Flags               : WORD;
    fatten_count        : Shortint;                 { !! Private !! }
    LockLayersCount     : Shortint;                 { !! Private !! }
    PrivateReserve5     : smallint;                 { !! Private !! }
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

VAR LayersBase : pLibrary;

FUNCTION BeginUpdate(l : pLayer location 'a0') : LONGINT; syscall LayersBase 078;
FUNCTION BehindLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1') : LONGINT; syscall LayersBase 054;
FUNCTION CreateBehindHookLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LONGINT location 'd0'; y0 : LONGINT location 'd1'; x1 : LONGINT location 'd2'; y1 : LONGINT location 'd3'; flags : LONGINT location 'd4'; hook : pHook location 'a3'; bm2 : pBitMap location 'a2') : pLayer; syscall LayersBase 192;
FUNCTION CreateBehindLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LONGINT location 'd0'; y0 : LONGINT location 'd1'; x1 : LONGINT location 'd2'; y1 : LONGINT location 'd3'; flags : LONGINT location 'd4'; bm2 : pBitMap location 'a2') : pLayer; syscall LayersBase 042;
FUNCTION CreateUpfrontHookLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LONGINT location 'd0'; y0 : LONGINT location 'd1'; x1 : LONGINT location 'd2'; y1 : LONGINT location 'd3'; flags : LONGINT location 'd4'; hook : pHook location 'a3'; bm2 : pBitMap location 'a2') : pLayer; syscall LayersBase 186;
FUNCTION CreateUpfrontLayer(li : pLayer_Info location 'a0'; bm : pBitMap location 'a1'; x0 : LONGINT location 'd0'; y0 : LONGINT location 'd1'; x1 : LONGINT location 'd2'; y1 : LONGINT location 'd3'; flags : LONGINT location 'd4'; bm2 : pBitMap location 'a2') : pLayer; syscall LayersBase 036;
FUNCTION DeleteLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1') : LONGINT; syscall LayersBase 090;
PROCEDURE DisposeLayerInfo(li : pLayer_Info location 'a0'); syscall LayersBase 150;
PROCEDURE DoHookClipRects(hook : pHook location 'a0'; rport : pRastPort location 'a1'; const rect : pRectangle location 'a2'); syscall LayersBase 216;
PROCEDURE EndUpdate(layer : pLayer location 'a0'; flag : ULONG location 'd0'); syscall LayersBase 084;
FUNCTION FattenLayerInfo(li : pLayer_Info location 'a0') : LONGINT; syscall LayersBase 156;
PROCEDURE InitLayers(li : pLayer_Info location 'a0'); syscall LayersBase 030;
FUNCTION InstallClipRegion(layer : pLayer location 'a0';const region : pRegion location 'a1') : pRegion; syscall LayersBase 174;
FUNCTION InstallLayerHook(layer : pLayer location 'a0'; hook : pHook location 'a1') : pHook; syscall LayersBase 198;
FUNCTION InstallLayerInfoHook(li : pLayer_Info location 'a0'; const hook : pHook location 'a1') : pHook; syscall LayersBase 204;
PROCEDURE LockLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1'); syscall LayersBase 096;
PROCEDURE LockLayerInfo(li : pLayer_Info location 'a0'); syscall LayersBase 120;
PROCEDURE LockLayers(li : pLayer_Info location 'a0'); syscall LayersBase 108;
FUNCTION MoveLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1'; dx : LONGINT location 'd0'; dy : LONGINT location 'd1') : LONGINT; syscall LayersBase 060;
FUNCTION MoveLayerInFrontOf(layer_to_move : pLayer location 'a0'; other_layer : pLayer location 'a1') : LONGINT; syscall LayersBase 168;
FUNCTION MoveSizeLayer(layer : pLayer location 'a0'; dx : LONGINT location 'd0'; dy : LONGINT location 'd1'; dw : LONGINT location 'd2'; dh : LONGINT location 'd3') : LONGINT; syscall LayersBase 180;
FUNCTION NewLayerInfo : pLayer_Info; syscall LayersBase 144;
PROCEDURE ScrollLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1'; dx : LONGINT location 'd0'; dy : LONGINT location 'd1'); syscall LayersBase 072;
FUNCTION SizeLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1'; dx : LONGINT location 'd0'; dy : LONGINT location 'd1') : LONGINT; syscall LayersBase 066;
PROCEDURE SortLayerCR(layer : pLayer location 'a0'; dx : LONGINT location 'd0'; dy : LONGINT location 'd1'); syscall LayersBase 210;
PROCEDURE SwapBitsRastPortClipRect(rp : pRastPort location 'a0'; cr : pClipRect location 'a1'); syscall LayersBase 126;
PROCEDURE ThinLayerInfo(li : pLayer_Info location 'a0'); syscall LayersBase 162;
PROCEDURE UnlockLayer(layer : pLayer location 'a0'); syscall LayersBase 102;
PROCEDURE UnlockLayerInfo(li : pLayer_Info location 'a0'); syscall LayersBase 138;
PROCEDURE UnlockLayers(li : pLayer_Info location 'a0'); syscall LayersBase 114;
FUNCTION UpfrontLayer(dummy : LONGINT location 'a0'; layer : pLayer location 'a1') : LONGINT; syscall LayersBase 048;
FUNCTION WhichLayer(li : pLayer_Info location 'a0'; x : LONGINT location 'd0'; y : LONGINT location 'd1') : pLayer; syscall LayersBase 132;

{Here we read how to compile this unit}
{You can remove this include and use a define instead}
{$I useautoopenlib.inc}
{$ifdef use_init_openlib}
procedure InitLAYERSLibrary;
{$endif use_init_openlib}

{This is a variable that knows how the unit is compiled}
var
    LAYERSIsCompiledHow : longint;

IMPLEMENTATION

uses
{$ifndef dont_use_openlib}
amsgbox;
{$endif dont_use_openlib}

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : longword = 0;

{$ifdef use_init_openlib}
  {$Info Compiling initopening of layers.library}
  {$Info don't forget to use InitLAYERSLibrary in the beginning of your program}

var
    layers_exit : Pointer;

procedure CloselayersLibrary;
begin
    ExitProc := layers_exit;
    if LayersBase <> nil then begin
        CloseLibrary(LayersBase);
        LayersBase := nil;
    end;
end;

procedure InitLAYERSLibrary;
begin
    LayersBase := nil;
    LayersBase := OpenLibrary(LAYERSNAME,LIBVERSION);
    if LayersBase <> nil then begin
        layers_exit := ExitProc;
        ExitProc := @CloselayersLibrary;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open layers.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;
end;

begin
    LAYERSIsCompiledHow := 2;
{$endif use_init_openlib}

{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of layers.library}

var
    layers_exit : Pointer;

procedure CloselayersLibrary;
begin
    ExitProc := layers_exit;
    if LayersBase <> nil then begin
        CloseLibrary(LayersBase);
        LayersBase := nil;
    end;
end;

begin
    LayersBase := nil;
    LayersBase := OpenLibrary(LAYERSNAME,LIBVERSION);
    if LayersBase <> nil then begin
        layers_exit := ExitProc;
        ExitProc := @CloselayersLibrary;
        LAYERSIsCompiledHow := 1;
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open layers.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$endif use_auto_openlib}

{$ifdef dont_use_openlib}
begin
    LAYERSIsCompiledHow := 3;
   {$Warning No autoopening of layers.library compiled}
   {$Warning Make sure you open layers.library yourself}
{$endif dont_use_openlib}


END. (* UNIT LAYERS *)



