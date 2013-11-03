{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit Hermes;

{$MODE objfpc}

{$if defined(darwin) and defined(cpui386)}
{ darwin/i386 requires a 16 byte aligned stack, and inserts code for that on
  entry in assembler routines (unless they are declared with "nostackframe").
  These assembler routines manually create their own stack frame and hardcode
  parameter offsets without using nostackframe, so they can never work on
  Darwin.
}
{$define noassembler}
{$endif}

{$IF defined(cpui386) and not defined(noassembler)}
  {$IF defined(linux) or defined(win32) or defined(go32v2) or defined(freebsd) or defined(haiku) or defined(beos)}
    {$DEFINE I386_ASSEMBLER}
  {$ENDIF}
{$ENDIF}

{$IF defined(cpux86_64) and not defined(noassembler)}
  {$DEFINE X86_64_ASSEMBLER}
{$ENDIF}

interface

const
  HERMES_CONVERT_NORMAL = 0;
  HERMES_CONVERT_DITHER = 1;

type
  THermesConverterHandle = Pointer;
  THermesPaletteHandle = Pointer;
  THermesClearerHandle = Pointer;
  PUint8  = ^Uint8;
  PUint16 = ^Uint16;
  PUint32 = ^Uint32;
  PUint64 = ^Uint64;
  PSint8  = ^Sint8;
  PSint16 = ^Sint16;
  PSint32 = ^Sint32;
  PSint64 = ^Sint64;
  Uint8  = Byte;
  Uint16 = Word;
  Uint32 = DWord;
  Uint64 = QWord;
  Sint8  = ShortInt;
  Sint16 = SmallInt;
  Sint32 = LongInt;
  Sint64 = Int64;
  PHermesFormat = ^THermesFormat;
  THermesFormat = record
    r,g,b,a: Uint32;
    bits: Integer;
    indexed: Boolean;
    has_colorkey: Boolean;
    colorkey: Uint32;
  end;

function Hermes_FormatNewEmpty: PHermesFormat;

{ Create a new format structure, returns nil if failed. }
function Hermes_FormatNew(bits: Integer; r, g, b, a: Uint32;
                          indexed: Boolean): PHermesFormat;

{ Free a format structure }
procedure Hermes_FormatFree(fmt: PHermesFormat);

{ Create a new format structure with colorkey info, returns nil if failed. }
function Hermes_FormatNewEx(bits: Integer; r, g, b, a: Uint32;
                            indexed, has_colorkey: Boolean;
                            colorkey: Uint32): PHermesFormat;

{ Compare two formats. Return true if they are equal, false otherwise }
function Hermes_FormatEquals(op1, op2: PHermesFormat): Boolean;

{ Copy the contents of format 'source' to format 'destination' }
procedure Hermes_FormatCopy(source, dest: PHermesFormat);


{
   Get a converter to work with, specifying a combination of the flags
   above. Returns nil if unsuccessful.
}
function Hermes_ConverterInstance(flags: DWord): THermesConverterHandle;

{
   Return a converter if it is not needed anymore, thus releasing some
   memory.
}
procedure Hermes_ConverterReturn(handle: THermesConverterHandle);

{
   Request a format conversion between two formats. This function returns false
   if the conversion cannot be provided (which should not occur too often :)
   Repeated calls to this function will be cached an terminate almost
   immediately, so don't be ashamed of calling it often.
}
function Hermes_ConverterRequest(handle: THermesConverterHandle;
                                 source, dest: PHermesFormat): Boolean;

{
   Set the palette of the source surface / destination surface for a
   subsequent conversion. At the moment, only sourcepal is used.
   Returns false if unsuccessful (invalid handle!).
}
function Hermes_ConverterPalette(handle: THermesConverterHandle; sourcepal, destpal: THermesPaletteHandle): Boolean;

{
   do a format conversion after calling the setup routines above. This will
   convert (or copy) the pixel data from s_pixels to the data in d_pixels.
   Both source and destination areas/origins can be specified as well as
   the scanline width in bytes of the source/destination.
   Returns false if unsuccessful (invalid handle or request not called before).
}
function Hermes_ConverterCopy(handle: THermesConverterHandle; s_pixels: Pointer;
                              s_x, s_y, s_width, s_height, s_pitch: Integer;
                              d_pixels: Pointer; d_x, d_y, d_width,
                              d_height, d_pitch: Integer): Boolean;

(*
{-----------------H_BLIT---------------}

{
   Get a blitter to work with, specifying a combination of the flags
   in H_Conv. Returns 0 if unsuccessful.
}
function Hermes_BlitterInstance(flags: DWord): THermesHandle;

{
   Return a blitter if it is not needed anymore, thus releasing some
   memory.
}
procedure Hermes_BlitterReturn(handle: THermesHandle);

{
   Request a format blitting between two formats. This function returns false
   if the blitting cannot be provided (which should not occur too often :)
   Repeated calls to this function will be cached an terminate almost
   immediately, so don't be ashamed of calling it often.
}
function Hermes_BlitterRequest(handle: THermesHandle;
                               source, dest: PHermesFormat): Boolean;

{
   Set the palette of the source surface / destination surface for a
   subsequent blitting. At the moment, only sourcepal is used.
   Returns false if unsuccessful (invalid handle!).
}
function Hermes_BlitterPalette(handle, sourcepal, destpal: THermesHandle): Boolean;

{
   do a format blitting after calling the setup routines above. This will
   blit the pixel data from s_pixels to the data in d_pixels.  Both source
   and destination areas/origins can be specified as well as the scanline
   width in bytes of the source/destination.  Returns false if unsuccessful
   (invalid handle or request not called before).
}
function Hermes_BlitterBlit(handle: THermesHandle; s_pixels: Pointer;
                            s_x, s_y, s_width, s_height, s_pitch: Integer;
                            d_pixels: Pointer; d_x, d_y, d_width, d_height,
                            d_pitch: Integer): Boolean;
*)
{-----------------H_PAL---------------}

{ Get a handle for a palette to work with. This allocates memory for an
   internal palette. Returns nil if failed.
}
function Hermes_PaletteInstance: THermesPaletteHandle;

{
   Return a handle for a palette if the palette isn't used anymore. The
   internal palette will be deallocated.
}
procedure Hermes_PaletteReturn(handle: THermesPaletteHandle);

{
   Copy the contents of the palette parameter provided into the internal
   palette. The user palette has to be 256*4 bytes long.
}
procedure Hermes_PaletteSet(handle: THermesPaletteHandle; palette: Pointer);

{
   Return the pointer to the internal palette. The palette is 256*4 bytes
   long.
}
function Hermes_PaletteGet(handle: THermesPaletteHandle): Pointer;

{
   Force invalidation of the palette cache. This will force lookup tables to
   be regenerated and has to be done manually after PaletteGet has been used
   and the data has been modified without the knowledge of Hermes.
}
procedure Hermes_PaletteInvalidateCache(handle: THermesPaletteHandle);


{-----------------H_CLEAR---------------}

{
   Get a handle for a new clearer instance to work with. Returns nil if failed.
}
function Hermes_ClearerInstance: THermesClearerHandle;

{
   Return the clearer instance if it is no longer needed.
}
procedure Hermes_ClearerReturn(handle: THermesClearerHandle);

{
   Request the clearing routines to be set up for clearing to a specific
   format later. Repeated calls to the routine will be cached and terminate
   after a short check.
}
function Hermes_ClearerRequest(handle: THermesClearerHandle; format: PHermesFormat): Boolean;

{
   Clear a surface. pixels points to the pixel data, x1, y1, width, height
   specify the area to clear, pitch is the width of a scanline in bytes,
   the rest are the colour components.
}
function Hermes_ClearerClear(handle: THermesClearerHandle; pixels: Pointer;
                             x1, y1, width, height, pitch: Integer;
                             r, g, b: Uint32; index: Uint8): Boolean;


{ Initialise Hermes, returns false if failed }
function Hermes_Init: Boolean;

{ Deinitialise Hermes, returns false if failed }
function Hermes_Done: Boolean;

implementation

{$I hermdef.inc}

const
  PROC_GENERIC = 1;
  PROC_X86_PENTIUM = 2;
  PROC_MMX_PENTIUM = 4;
  PROC_SSE2 = 8;
  PROC_X86_64 = 16;
  HERMES_CONVERT_GENERIC = 65536;

{$I hermconf.inc}

type
  THermesHandle = Integer;
  PHermesClearInterface = ^THermesClearInterface;
  THermesClearInterface = record
    dest: ^Uint8;
    value: Uint32;
    width, height: Integer;
    add: Integer;
  end;
  THermesClearPtr = procedure(hci: PHermesClearInterface); cdecl;
  PHermesClearer = ^THermesClearer;
  THermesClearer = record
    bits: Integer;
    func: THermesClearPtr;
  end;


{ Structure to hold shift amounts for the generic routines }
  PHermesGenericInfo = ^THermesGenericInfo;
  THermesGenericInfo = record
    r_right, g_right, b_right, a_right: Integer; {Shift amount to the right}
    r_left, g_left, b_left, a_left: Integer; {Shift amount to the right}
  end;


{ Pointer to specialised (one-scanline-only) conversion procedure }
  THermesConverterPtr = procedure(source, dest: PUint8;
                                  count, inc_source: DWord); cdecl;

{ Structure for conversion loop routines, don't be scared, size does NOT
   matter in this case :) }
  PHermesConverterInterface = ^THermesConverterInterface;
  THermesConverterInterface = record
    s_pixels: PUint8;
    s_width,s_height: Integer;
    s_add: Integer;          { Offset to next line from end of line }

    d_pixels: PUint8;
    d_width,d_height: Integer;
    d_add: Integer;

    func: THermesConverterPtr;

    lookup: PUint32;          { Palette lookup table ptr, for 8 bit }

    s_pitch: Integer;        { Source and destination pitch, }
    d_pitch: Integer;        { only used by C routines }

    info: THermesGenericInfo; { Only used by generic converters }
    mask_r, mask_g, mask_b, mask_a: Uint32; { Only used by generic converters }
    s_mask_a: Uint32;

    s_has_colorkey: Boolean;
    s_colorkey: Uint32;

    d_has_colorkey: Boolean;
    d_colorkey: Uint32;
  end;

{ Pointer to loop function (C, assembler main loop, generic routines) }
  THermesConverterLoopPtr = procedure(hci: PHermesConverterInterface); cdecl;

  PHermesConverter = ^THermesConverter;
  THermesConverter = record
    source,dest: THermesFormat;           { Source and destination format }
    lookup: PUint32;                       { Pointer to lookup table (8bit) }

    flags: DWord;                         { Defined in H_Conv.h, DITHER,etc}

    loopnormal: THermesConverterLoopPtr;  { Loop routine for normal conv. }
    loopstretch: THermesConverterLoopPtr;
    normal: THermesConverterPtr;          { One-scanline routine }
    stretch: THermesConverterPtr;

    dither: THermesConverterLoopPtr;        { Dithering routines always }
    ditherstretch: THermesConverterLoopPtr; { convert the whole buffer }
  end;

  PHermesFactoryStruct = ^THermesFactoryStruct;
  THermesFactoryStruct = record
    s_bits: Integer;
    s_idx: Boolean;
    s_r, s_g, s_b, s_a: Uint32;
    d_bits: Integer;
    d_idx: Boolean;
    d_r, d_g, d_b, d_a: Uint32;

    loopnormal, loopstretch: THermesConverterLoopPtr;
    normal, stretch: THermesConverterPtr;
    dither, ditherstretch: THermesConverterLoopPtr;

    processor: Integer;
  end;

{dither types ?}

const
{ p_converters holds a list of formats, for conversion from 32 bit, 24 bit,
  16 bit, muhmu and 8 bit.
  The destination formats are listed in the order of frequency they might
  occur so common formats can be retrieved faster.

  Format of a row:
  source bpp, s. indexed, s. r_mask, s. g_mask, s. b_mask, s. alpha ,dest bpp,
  d.indexed, d. r_mask, d. g_mask, d. b_mask, d. alpha
}
{ I wish I could touch this, but it's used in too many other placed in the code,
  ( at least indirectly), and many of the indicies are hardcoded }
  p_converters: array [0..4, 0..11, 0..11] of DWord =
  (
  ( {From 32 bit RGB 888}
  (32,0,$ff0000,$ff00,$ff,0,16,0,$f800,$7e0,$1f,0),          {16RGB565 }
  (32,0,$ff0000,$ff00,$ff,0, 8,0,$e0,$1c,$3,0),              { 8RGB332 }
  (32,0,$ff0000,$ff00,$ff,0,16,0,$7c00,$3e0,$1f,0),          { 16RGB555 }
  (32,0,$ff0000,$ff00,$ff,0,24,0,$ff0000,$ff00,$ff,0),       { 24RGB888 }
  (32,0,$ff0000,$ff00,$ff,0,32,0,$ff,$ff00,$ff0000,0),       { 32BGR888 }
  (32,0,$ff0000,$ff00,$ff,0,16,0,$1f,$7e0,$f800,0),          { 16BGR565 }
  (32,0,$ff0000,$ff00,$ff,0,16,0,$1f,$3e0,$7c00,0),          { 16BGR555 }
  (32,0,$ff0000,$ff00,$ff,0,32,0,$ff000000,$ff0000,$ff00,$ff), { 32RGBA888 }
  (32,0,$ff0000,$ff00,$ff,0,32,0,$ff00,$ff0000,$ff000000,$ff), { 32BGRA888 }
  (32,0,$ff0000,$ff00,$ff,0,24,0,$ff,$ff00,$ff0000,0),       { 24BGR888 }
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0)
  )
  ,
  ( {From 24 bit RGB 888}
  (24,0,$ff0000,$ff00,$ff,0,32,0,$ff0000,$ff00,$ff,0),       { 32RGB888 }
  (24,0,$ff0000,$ff00,$ff,0,16,0,$f800,$7e0,$1f,0),          { 16RGB565 }
  (24,0,$ff0000,$ff00,$ff,0, 8,0,$e0,$1c,$3,0),              { 8RGB332 }
  (24,0,$ff0000,$ff00,$ff,0,16,0,$7c00,$3e0,$1f,0),          { 16RGB555 }
  (24,0,$ff0000,$ff00,$ff,0,32,0,$ff,$ff00,$ff0000,0),       { 32BGR888 }
  (24,0,$ff0000,$ff00,$ff,0,16,0,$1f,$7e0,$f800,0),          { 16BGR565 }
  (24,0,$ff0000,$ff00,$ff,0,16,0,$1f,$3e0,$7c00,0),          { 16BGR555 }
  (24,0,$ff0000,$ff00,$ff,0,32,0,$ff000000,$ff0000,$ff00,$ff), { 32RGBA888 }
  (24,0,$ff0000,$ff00,$ff,0,32,0,$ff00,$ff0000,$ff000000,$ff), { 32BGRA888 }
  (24,0,$ff0000,$ff00,$ff,0,24,0,$ff,$ff00,$ff0000,0),       { 24BGR888 }
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0)
  )
  ,
  ( {From 16 bit RGB 565}
  (16,0,$f800,$7e0,$1f,0,32,0,$ff0000,$ff00,$ff,0),          { 32RGB888 }
  (16,0,$f800,$7e0,$1f,0, 8,0,$e0,$1c,$3,0),                 { 8RGB332 }
  (16,0,$f800,$7e0,$1f,0,16,0,$7c00,$3e0,$1f,0),             { 16RGB555 }
  (16,0,$f800,$7e0,$1f,0,24,0,$ff0000,$ff00,$ff,0),          { 24RGB888 }
  (16,0,$f800,$7e0,$1f,0,32,0,$ff,$ff00,$ff0000,0),          { 32BGR888 }
  (16,0,$f800,$7e0,$1f,0,16,0,$1f,$7e0,$f800,0),             { 16BGR565 }
  (16,0,$f800,$7e0,$1f,0,16,0,$1f,$3e0,$7c00,0),             { 16BGR555 }
  (16,0,$f800,$7e0,$1f,0,32,0,$ff000000,$ff0000,$ff00,$ff),    { 32RGBA888 }
  (16,0,$f800,$7e0,$1f,0,32,0,$ff00,$ff0000,$ff000000,$ff),    { 32BGRA888 }
  (16,0,$f800,$7e0,$1f,0,24,0,$ff,$ff00,$ff0000,0),          { 24BGR888 }
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0)
  ),
  ( {From 32 bit muhmu}
  (32,0,$ff00000,$3fc00,$ff,0,32,0,$ff0000,$ff00,$ff,0),     { 32RGB888 }
  (32,0,$ff00000,$3fc00,$ff,0,16,0,$f800,$7e0,$1f,0),        { 16RGB565 }
  (32,0,$ff00000,$3fc00,$ff,0, 8,0,$e0,$1c,$3,0),            { 8RGB332 }
  (32,0,$ff00000,$3fc00,$ff,0,16,0,$7c00,$3e0,$1f,0),        { 16RGB555 }
  (32,0,$ff00000,$3fc00,$ff,0,24,0,$ff0000,$ff00,$ff,0),     { 24RGB888 }
  (32,0,$ff00000,$3fc00,$ff,0,32,0,$ff,$ff00,$ff0000,0),     { 32BGR888 }
  (32,0,$ff00000,$3fc00,$ff,0,16,0,$1f,$7e0,$f800,0),        { 16BGR565 }
  (32,0,$ff00000,$3fc00,$ff,0,16,0,$1f,$3e0,$7c00,0),        { 16BGR555 }
  (32,0,$ff00000,$3fc00,$ff,0,32,0,$ff000000,$ff0000,$ff00,$ff), { 32RGBA888 }
  (32,0,$ff00000,$3fc00,$ff,0,32,0,$ff00,$ff0000,$ff000000,$ff), { 32BGRA888 }
  (32,0,$ff00000,$3fc00,$ff,0,24,0,$ff,$ff00,$ff0000,0),     { 24BGR888 }
  (0,0,0,0,0,0,0,0,0,0,0,0)
  ),
  ( {From 8 bit indexed}
  (8,1,0,0,0,0,32,0,0,0,0,0),
  (8,1,0,0,0,0,24,0,0,0,0,0),
  (8,1,0,0,0,0,16,0,0,0,0,0),
  (8,1,0,0,0,0,8,0,0,0,0,0),
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0),
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0),
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0),
  (0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0)
  )
  );
  numConverters: array [0..4] of Integer = (10,10,10,11,4);
  refcount: Integer = 0;

var
  Clearers: array [0..3] of PHermesClearer;
  numClearers: Integer;
  standardConverters: array [0..4] of ^PHermesConverter;
  equalConverters: array [0..3] of PHermesConverter;

{$I hermes_debug.inc}

{$I hermes_dither.inc}
{$I headp.inc}
{$IFDEF I386_ASSEMBLER}
  {$I i386/headi386.inc}
  {$I i386/headmmx.inc}
{$ENDIF I386_ASSEMBLER}
{$IFDEF X86_64_ASSEMBLER}
  {$I x86_64/headx86_64.inc}
{$ENDIF X86_64_ASSEMBLER}
{$I factconv.inc}
{$I hermes_list.inc}
{$I hermes_utility.inc}
{$I hermes_format.inc}
{$I hermes_palette.inc}
{$I hermes_converter.inc}
{$I hermes_clearer.inc}
{$I hermes_factory.inc}

function Hermes_Init: Boolean;
var
  i, j: Integer;
  source, dest: THermesFormat;
begin
  if refcount > 0 then
  begin
    Inc(refcount);
    Result := True;
    exit;
  end;
  { Initialise hermes factory }
  Hermes_Factory_Init;

  { Instruct the factory to return clearing routines }
  Clearers[0] := Hermes_Factory_getClearer(32);
  Clearers[1] := Hermes_Factory_getClearer(24);
  Clearers[2] := Hermes_Factory_getClearer(16);
  Clearers[3] := Hermes_Factory_getClearer(8);
  numClearers := 4;

  { Use factory to obtain specialised converters }
  for j := 0 to 4 do
  begin
    standardConverters[j] := GetMem(SizeOf(PHermesConverter)*numConverters[j]);
    for i := 0 to numConverters[j] - 1 do
    begin
      // xxx jm color keys not taken into consideration here
      FillChar(source, SizeOf(source), 0);
      FillChar(dest, SizeOf(dest), 0);
      source.bits := p_converters[j, i, 0];    dest.bits := p_converters[j, i, 6];
      source.indexed:= p_converters[j,i,1]<>0; dest.indexed:= p_converters[j,i,7]<>0;
      source.r := p_converters[j, i, 2];       dest.r := p_converters[j, i, 8];
      source.g := p_converters[j, i, 3];       dest.g := p_converters[j, i, 9];
      source.b := p_converters[j, i, 4];       dest.b := p_converters[j, i, 10];
      source.a := p_converters[j, i, 5];       dest.a := p_converters[j, i, 11];

      standardConverters[j][i] := Hermes_Factory_getConverter(@source, @dest);
    end;
  end;


  { Set up converters for equal colour formats }
  equalConverters[3] := Hermes_Factory_getEqualConverter(32);
  equalConverters[2] := Hermes_Factory_getEqualConverter(24);
  equalConverters[1] := Hermes_Factory_getEqualConverter(16);
  equalConverters[0] := Hermes_Factory_getEqualConverter(8);

  { Initialise dithering tables }
  Dither_SetupMatrices;

  Inc(refcount);
  Result := True;
end;

function Hermes_Done: Boolean;
var
  i, j: Integer;
begin
  Dec(refcount);
  if refcount < 0 then
  begin
    refcount := 0;
    Result := False;
    exit;
  end;
  if refcount = 0 then
  begin
    for i := 0 to 3 do
    begin
      if Clearers[i] <> nil then
      begin
        Dispose(Clearers[i]);
        Clearers[i] := nil;
      end;
      if equalConverters[i] <> nil then
      begin
        Dispose(equalConverters[i]);
        equalConverters[i] := nil;
      end;
    end;
    for i := 0 to 4 do
    begin
      if standardConverters[i] <> nil then
      begin
        for j := 0 to numConverters[i] - 1 do
          Dispose(standardConverters[i][j]);
        FreeMem(standardConverters[i]);
        standardConverters[i] := nil;
      end;
    end;
  end;
  Result := True;
end;

begin
  DebugInit;
end.
