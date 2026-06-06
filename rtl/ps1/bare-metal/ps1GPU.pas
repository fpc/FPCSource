{$mode objfpc}{$H+}
unit ps1GPU;
interface
uses ps1System;

const
  DMA_MAX_CHUNK_SIZE    =  16;
  CHAIN_BUFFER_SIZE : dword = 1024 * 7;
  ORDERING_TABLE_SIZE : dword = 1024 * 4;


// --- GP0 drawing attributes ---

// Blend modes
const
  GP0_BLEND_SEMITRANS = 0;
  GP0_BLEND_ADD       = 1;
  GP0_BLEND_SUBTRACT  = 2;
  GP0_BLEND_DIV4_ADD  = 3;

// Color depths
const
  GP0_COLOR_4BPP  = 0;
  GP0_COLOR_8BPP  = 1;
  GP0_COLOR_16BPP = 2;


// Horizontal resolution
const
  GP1_HRES_256 = 0; // Dotclock divided by 10
  GP1_HRES_320 = 1; // Dotclock divided by 8
  GP1_HRES_368 = 1 shl 6; // Dotclock divided by 7
  GP1_HRES_512 = 2; // Dotclock divided by 5
  GP1_HRES_640 = 3; // Dotclock divided by 4

// Vertical resolution
const
  GP1_VRES_256 = 0;
  GP1_VRES_512 = 1;

// Video mode
const
  GP1_MODE_NTSC = 0;
  GP1_MODE_PAL  = 1;

// Color depth
const
  GP1_COLOR_16BPP = 0;
  GP1_COLOR_24BPP = 1;

// DMA request mode
const
  GP1_DREQ_NONE       = 0;
  GP1_DREQ_FIFO       = 1;
  GP1_DREQ_GP0_WRITE  = 2;
  GP1_DREQ_GP0_READ   = 3;

// VRAM size
const
  GP1_VRAM_1MB = 0;
  GP1_VRAM_2MB = 1;


const
  // GP0 command bases
  GP0_CMD_MISC       = $00000000;
  GP0_CMD_POLYGON    = $20000000;
  GP0_CMD_LINE       = $40000000;
  GP0_CMD_RECTANGLE  = $60000000;
  GP0_CMD_VRAM_BLIT  = $80000000;
  GP0_CMD_VRAM_WRITE = $A0000000;
  GP0_CMD_VRAM_READ  = $C0000000;
  GP0_CMD_ATTRIBUTE  = $E0000000;

  // GP0 misc/attribute commands
  GP0_CMD_FLUSH_CACHE = GP0_CMD_MISC or ($1 shl 24);
  GP0_CMD_VRAM_FILL   = GP0_CMD_MISC or ($2 shl 24);
  GP0_CMD_IRQ         = GP0_CMD_MISC or ($1F shl 24);
  GP0_CMD_TEXPAGE     = GP0_CMD_ATTRIBUTE or ($1 shl 24);
  GP0_CMD_TEXWINDOW   = GP0_CMD_ATTRIBUTE or ($2 shl 24);
  GP0_CMD_FB_OFFSET1  = GP0_CMD_ATTRIBUTE or ($3 shl 24);
  GP0_CMD_FB_OFFSET2  = GP0_CMD_ATTRIBUTE or ($4 shl 24);
  GP0_CMD_FB_ORIGIN   = GP0_CMD_ATTRIBUTE or ($5 shl 24);
  GP0_CMD_FB_MASK     = GP0_CMD_ATTRIBUTE or ($6 shl 24);

  // GP1 commands
  GP1_CMD_RESET_GPU   = 0 shl 24;
  GP1_CMD_RESET_FIFO  = 1 shl 24;
  GP1_CMD_ACKNOWLEDGE = 2 shl 24;
  GP1_CMD_DISP_BLANK  = 3 shl 24;
  GP1_CMD_DREQ_MODE   = 4 shl 24;
  GP1_CMD_FB_OFFSET   = 5 shl 24;
  GP1_CMD_FB_RANGE_H  = 6 shl 24;
  GP1_CMD_FB_RANGE_V  = 7 shl 24;
  GP1_CMD_FB_MODE     = 8 shl 24;
  GP1_CMD_VRAM_SIZE   = 9 shl 24;
  GP1_CMD_GET_INFO    = 16 shl 24;

{ GP0 inline functions }

function gp0_tag(length: LongWord; next: Pointer): LongWord; inline;
function gp0_endTag(length: LongWord): LongWord; inline;
function gp0_page(x, y: LongWord; blendMode: LongInt; colorDepth: LongInt): Word; inline;
function gp0_clut(x, y: LongWord): Word; inline;
function gp0_xy(x, y: LongInt): LongWord; inline;
function gp0_uv(u, v: LongWord; attr: Word): LongWord; inline;
function gp0_rgb(r, g, b: Byte): LongWord; inline;
function _gp0_polygon(quad, unshaded, gouraud, textured, blend: Boolean): LongWord; inline;
function gp0_triangle(textured, blend: Boolean): LongWord; inline;
function gp0_shadedTriangle(gouraud, textured, blend: Boolean): LongWord; inline;
function gp0_quad(textured, blend: Boolean): LongWord; inline;
function gp0_shadedQuad(gouraud, textured, blend: Boolean): LongWord; inline;
function gp0_line(gouraud, blend: Boolean): LongWord; inline;
function gp0_polyLine(gouraud, blend: Boolean): LongWord; inline;
function _gp0_rectangle(size: Byte; textured, unshaded, blend: Boolean): LongWord; inline;
function gp0_rectangle(textured, unshaded, blend: Boolean): LongWord; inline;
function gp0_rectangle1x1(textured, unshaded, blend: Boolean): LongWord; inline;
function gp0_rectangle8x8(textured, unshaded, blend: Boolean): LongWord; inline;
function gp0_rectangle16x16(textured, unshaded, blend: Boolean): LongWord; inline;
function gp0_vramBlit: LongWord; inline;
function gp0_vramWrite: LongWord; inline;
function gp0_vramRead: LongWord; inline;
function gp0_flushCache: LongWord; inline;
function gp0_vramFill: LongWord; inline;
function gp0_irq: LongWord; inline;
function gp0_texpage(page: Word; dither, unlockFB: Boolean): LongWord; inline;
function gp0_texwindow(baseX, baseY, maskX, maskY: Byte): LongWord; inline;
function gp0_fbOffset1(x, y: LongWord): LongWord; inline;
function gp0_fbOffset2(x, y: LongWord): LongWord; inline;
function gp0_fbOrigin(x, y: LongInt): LongWord; inline;
function gp0_fbMask(setMask, useMask: Boolean): LongWord; inline;

{ GP1 inline functions }

function gp1_clockMultiplierH(horizontalRes: LongInt): LongWord; inline;
function gp1_clockDividerV(verticalRes: LongInt): LongWord; inline;
function gp1_resetGPU: LongWord; inline;
function gp1_resetFIFO: LongWord; inline;
function gp1_acknowledge: LongWord; inline;
function gp1_dispBlank(blank: Boolean): LongWord; inline;
function gp1_dmaRequestMode(mode: LongInt): LongWord; inline;
function gp1_fbOffset(x, y: LongWord): LongWord; inline;
function gp1_fbRangeH(low, high: LongInt): LongWord; inline;
function gp1_fbRangeV(low, high: LongInt): LongWord; inline;
function gp1_fbMode(horizontalRes: LongInt; verticalRes: LongInt; videoMode: LongInt; interlace: Boolean; colorDepth: LongInt): LongWord; inline;
function gp1_vramSize(size: LongInt): LongWord; inline;




type
  DMAChain = packed record
    data: array of LongWord;
    nextPacket: pdword;
    orderingTable: array of LongWord;
  end;
  PDMAChain = ^DMAChain;

  TextureInfo = record
    u, v : byte;
    width, height : word;
    page, clut : word;
  end;

var
  dmaChains: array[0..1] of DMAChain;

function RGB(r, g, b: byte): Word;

procedure setupGPU(mode: LongWord; width, height: Integer);
procedure setupChainOT(chainSize, OTsize: dword);

procedure waitForGP0Ready;
procedure waitForDMADone;
procedure waitForVSync;

procedure sendLinkedList(data: Pointer);

procedure clearOrderingTable(var table: array of LongWord; numEntries: Integer);

function  allocatePacket(var chain: DMAChain; zIndex, numCommands: Integer): pdword;
function  allocatePacket(var chain: DMAChain; numCommands: Integer): PDWord;

procedure sendVRAMData(data: Pointer; x, y, width, height: Integer);

procedure uploadTexture(var info: TextureInfo; const data: Pointer; x, y, width, height: Integer);
procedure uploadIndexedTexture(var info: TextureInfo; const image, palette: Pointer; imageX, imageY, paletteX, paletteY, width, height: Integer; colorDepth: longint);

implementation

{ GP0 implementations }

function gp0_tag(length: LongWord; next: Pointer): LongWord;
begin
  Result := (LongWord(UIntPtr(next)) and $FFFFFF) or ((length and $FF) shl 24);
end;

function gp0_endTag(length: LongWord): LongWord;
begin
  Result := gp0_tag(length, Pointer($FFFFFF));
end;

function gp0_page(x, y: LongWord; blendMode: LongInt; colorDepth: LongInt): Word;
begin
  Result := (x and $F) or ((y and 1) shl 4) or ((Ord(blendMode) and 3) shl 5)
          or ((Ord(colorDepth) and 3) shl 7) or ((y and 2) shl 10);
end;

function gp0_clut(x, y: LongWord): Word;
begin
  Result := ((x and $3F) shl 0) or ((y and $3FF) shl 6);
end;

function gp0_xy(x, y: LongInt): LongWord;
begin
  Result := (LongWord(x) and $FFFF)
         or ((LongWord(y) and $FFFF) shl 16);
end;


function gp0_uv(u, v: LongWord; attr: Word): LongWord;
begin
  Result := (u and $FF)
       or ((v and $FF) shl 8)
       or (LongWord(attr) shl 16);
end;

function gp0_rgb(r, g, b: Byte): LongWord;
begin
  Result := (r shl 0) or (g shl 8) or (b shl 16);
end;

function _gp0_polygon(quad, unshaded, gouraud, textured, blend: Boolean): LongWord;
begin
  Result := GP0_CMD_POLYGON or (LongWord(ord(unshaded) and 1) shl 24) or (LongWord(ord(blend) and 1) shl 25)
          or (LongWord(ord(textured) and 1) shl 26) or (LongWord(ord(quad) and 1) shl 27) or (LongWord(ord(gouraud) and 1) shl 28);
end;

function gp0_triangle(textured, blend: Boolean): LongWord;
begin
  Result := _gp0_polygon(False, True, False, textured, blend);
end;

function gp0_shadedTriangle(gouraud, textured, blend: Boolean): LongWord;
begin
  Result := _gp0_polygon(False, False, gouraud, textured, blend);
end;

function gp0_quad(textured, blend: Boolean): LongWord;
begin
  Result := _gp0_polygon(True, True, False, textured, blend);
end;

function gp0_shadedQuad(gouraud, textured, blend: Boolean): LongWord;
begin
  Result := _gp0_polygon(True, False, gouraud, textured, blend);
end;

function gp0_line(gouraud, blend: Boolean): LongWord;
begin
  Result := GP0_CMD_LINE or ((LongWord(blend) and 1) shl 25) or ((LongWord(gouraud) and 1) shl 28);
end;

function gp0_polyLine(gouraud, blend: Boolean): LongWord;
begin
  Result := GP0_CMD_LINE or ((LongWord(blend) and 1) shl 25) or (1 shl 27) or ((LongWord(gouraud) and 1) shl 28);
end;

function _gp0_rectangle(size: Byte; textured, unshaded, blend: Boolean): LongWord;
begin
  Result := GP0_CMD_RECTANGLE
          or ((Ord(unshaded) and 1) shl 24)
          or ((Ord(blend)    and 1) shl 25)
          or ((Ord(textured) and 1) shl 26)
          or ((size and 3) shl 27);
end;

function gp0_rectangle(textured, unshaded, blend: Boolean): LongWord;
begin
  Result := _gp0_rectangle(0, textured, unshaded, blend);
end;


function gp0_rectangle1x1(textured, unshaded, blend: Boolean): LongWord;
begin
  Result := _gp0_rectangle(1, textured, unshaded, blend);
end;

function gp0_rectangle8x8(textured, unshaded, blend: Boolean): LongWord;
begin
  Result := _gp0_rectangle(2, textured, unshaded, blend);
end;

function gp0_rectangle16x16(textured, unshaded, blend: Boolean): LongWord;
begin
  Result := _gp0_rectangle(3, textured, unshaded, blend);
end;

function gp0_vramBlit: LongWord; inline;
begin
  Result := GP0_CMD_VRAM_BLIT;
end;

function gp0_vramWrite: LongWord; inline;
begin
  Result := GP0_CMD_VRAM_WRITE;
end;

function gp0_vramRead: LongWord; inline;
begin
  Result := GP0_CMD_VRAM_READ;
end;

function gp0_flushCache: LongWord; inline;
begin
  Result := GP0_CMD_FLUSH_CACHE;
end;

function gp0_vramFill: LongWord; inline;
begin
  Result := GP0_CMD_VRAM_FILL;
end;

function gp0_irq: LongWord; inline;
begin
  Result := GP0_CMD_IRQ;
end;

function gp0_texpage(page: Word; dither, unlockFB: Boolean): LongWord;
begin
  Result := GP0_CMD_TEXPAGE or ((page and $9FF) shl 0) or (LongWord(dither) shl 9) or (LongWord(unlockFB) shl 10);
end;

function gp0_texwindow(baseX, baseY, maskX, maskY: Byte): LongWord;
begin
  Result := GP0_CMD_TEXWINDOW or ((maskX and $1F) shl 0) or ((maskY and $1F) shl 5)
          or ((baseX and $1F) shl 10) or ((baseY and $1F) shl 15);
end;

function gp0_fbOffset1(x, y: LongWord): LongWord;
begin
  Result := GP0_CMD_FB_OFFSET1 or ((x and $3FF) shl 0) or ((y and $3FF) shl 10);
end;

function gp0_fbOffset2(x, y: LongWord): LongWord;
begin
  Result := GP0_CMD_FB_OFFSET2 or ((x and $3FF) shl 0) or ((y and $3FF) shl 10);
end;

function gp0_fbOrigin(x, y: LongInt): LongWord;
begin
  Result := GP0_CMD_FB_ORIGIN or ((LongWord(x) and $7FF) shl 0) or ((LongWord(y) and $7FF) shl 11);
end;

function gp0_fbMask(setMask, useMask: Boolean): LongWord;
begin
  Result := GP0_CMD_FB_MASK or (LongWord(setMask) shl 0) or (LongWord(useMask) shl 1);
end;

{ GP1 implementations }

function gp1_clockMultiplierH(horizontalRes: LongInt): LongWord;
begin
  case horizontalRes of
    GP1_HRES_256: Result := 10;
    GP1_HRES_320: Result := 8;
    GP1_HRES_368: Result := 7;
    GP1_HRES_512: Result := 5;
    GP1_HRES_640: Result := 4;
  else
    Result := 0;
  end;
end;

function gp1_clockDividerV(verticalRes: LongInt): LongWord;
begin
  case verticalRes of
    GP1_VRES_256: Result := 1;
    GP1_VRES_512: Result := 2;
  else
    Result := 0;
  end;
end;

function gp1_resetGPU: LongWord; inline;
begin
  Result := GP1_CMD_RESET_GPU;
end;

function gp1_resetFIFO: LongWord; inline;
begin
  Result := GP1_CMD_RESET_FIFO;
end;

function gp1_acknowledge: LongWord; inline;
begin
  Result := GP1_CMD_ACKNOWLEDGE;
end;

function gp1_dispBlank(blank: Boolean): LongWord; inline;
begin
  Result := GP1_CMD_DISP_BLANK or LongWord(blank);
end;

function gp1_dmaRequestMode(mode: LongInt): LongWord; inline;
begin
  Result := GP1_CMD_DREQ_MODE or (LongWord(mode) and 3);
end;

function gp1_fbOffset(x, y: LongWord): LongWord; inline;
begin
  Result := GP1_CMD_FB_OFFSET or ((x and $3FF) shl 0) or ((y and $3FF) shl 10);
end;

function gp1_fbRangeH(low, high: LongInt): LongWord;
begin
  Result := GP1_CMD_FB_RANGE_H
         or ((LongWord(low)  and $FFF) shl 0)
         or ((LongWord(high) and $FFF) shl 12);
end;

function gp1_fbRangeV(low, high: LongInt): LongWord;
begin
  Result := GP1_CMD_FB_RANGE_V
         or ((LongWord(low)  and $3FF) shl 0)
         or ((LongWord(high) and $3FF) shl 10);
end;

function gp1_fbMode(horizontalRes: LongInt; verticalRes: LongInt; videoMode: LongInt; interlace: Boolean; colorDepth: LongInt): LongWord; inline;
begin
  Result := GP1_CMD_FB_MODE or ((LongWord(horizontalRes) and $47) shl 0)
          or ((LongWord(verticalRes) and 1) shl 2)
          or ((LongWord(videoMode) and 1) shl 3)
          or ((LongWord(colorDepth) and 1) shl 4)
          or ((LongWord(interlace) and 1) shl 5);
end;

function gp1_vramSize(size: LongInt): LongWord; inline;
begin
  Result := GP1_CMD_VRAM_SIZE or (LongWord(size) and 1);
end;


function RGB(r, g, b: byte): Word;
var
  R5, G5, B5 : Word;
  T : Word;

begin

  { Convert 0–255 channel to 0–31 and mask to 5 bits }
  R5 := (r * 31 div 255) and $1F;
  G5 := (g * 31 div 255) and $1F;
  B5 := (b * 31 div 255) and $1F;

  { Transparency flag: set to 1 if alpha < 128 (customize if needed) }
  T := 1;

  { Pack into 16-bit PlayStation pixel: TBBBBBGGGGGRRRRR }
  result := (T shl 15) or (B5 shl 10) or (G5 shl 5) or R5;

end;


procedure setupGPU(mode: LongWord; width, height: Integer);
var
  x, y, offsetX, offsetY: Integer;
  horizontalRes: LongWord;
  verticalRes: LongWord;

begin

  x := $760;
  if mode = GP1_MODE_PAL then
    y := $A3
  else
    y := $88;

  horizontalRes := GP1_HRES_320;
  verticalRes := GP1_VRES_256;

  offsetX := (width * gp1_clockMultiplierH(horizontalRes)) div 2;
  offsetY := (height div gp1_clockDividerV(verticalRes)) div 2;

  GPU_GP1 := gp1_resetGPU();
  GPU_GP1 := gp1_fbRangeH(x - offsetX, x + offsetX);
  GPU_GP1 := gp1_fbRangeV(y - offsetY, y + offsetY);
  GPU_GP1 := gp1_fbMode(horizontalRes, verticalRes, mode, False, GP1_COLOR_16BPP); // GP1_COLOR_16BPP ~ 1

end;


procedure setupChainOT(chainSize, OTsize: dword);
begin

  CHAIN_BUFFER_SIZE:= chainSize;
  ORDERING_TABLE_SIZE:= OTsize;

  setlength(dmaChains[0].data, chainSize);
  setlength(dmaChains[0].orderingTable, OTsize);
  setlength(dmaChains[1].data, chainSize);
  setlength(dmaChains[1].orderingTable, OTsize);

  if chainSize > 0 then begin
    dmaChains[0].nextPacket := @dmaChains[0].data[0];
    dmaChains[1].nextPacket := @dmaChains[1].data[0];
  end;

  FillDWord(dmaChains[0].orderingTable[0], OTsize, $00FFFFFF);
  FillDWord(dmaChains[1].orderingTable[0], OTsize, $00FFFFFF);

  DMA_DPCR:= DMA_DPCR or DMA_DPCR_CH_ENABLE(DMA_GPU) or DMA_DPCR_CH_ENABLE(DMA_OTC);

  GPU_GP1:= gp1_dmaRequestMode(GP1_DREQ_GP0_WRITE);
  GPU_GP1:= gp1_dispBlank(False);

end;


procedure waitForGP0Ready;
begin

  while (GPU_GP1 and GP1_STAT_CMD_READY) = 0 do ; // spin

end;


procedure waitForDMADone;
begin

  while (DMA_CHCR(DMA_GPU) and DMA_CHCR_ENABLE) <> 0 do asm nop end;

end;


procedure WaitForVSync;
var
   currentCount : longint;
begin

  // Wait until IRQ vertical blank flag is set.

    if (IRQ_MASK and 1) <> 0 then begin
      
      currentCount:= vblankCount;
      while currentCount = vblankCount do ;

    end else begin

      while (IRQ_STAT and (Word(1) shl IRQ_VSYNC)) = 0 do ;
      IRQ_STAT := Word(not (Word(1) shl IRQ_VSYNC));

      inc(vblankCount);

    end;

end;


procedure sendLinkedList(data: Pointer);
begin

  // pointer must be 4-byte aligned
  // DMA must be idle
  waitForDMADone;


  DMA_MADR_Set(DMA_GPU, LongWord(NativeUInt(data)));
  DMA_CHCR_Set(DMA_GPU, DMA_CHCR_WRITE or DMA_CHCR_MODE_LIST or DMA_CHCR_ENABLE);

end;


procedure sendVRAMData(data: Pointer; x, y, width, height: Integer);
var
  lengthWords: NativeUInt;
  chunkSize, numChunks: NativeUInt;

begin

  waitForDMADone;

  // number of 32-bit words to send: width*height pixels -> half as many words for 16bpp
  lengthWords := (NativeUInt(width) * NativeUInt(height)) div 2;

  if lengthWords < DMA_MAX_CHUNK_SIZE then
  begin
    chunkSize := lengthWords;
    numChunks := 1;
  end
  else
  begin
    chunkSize := DMA_MAX_CHUNK_SIZE;
    numChunks := lengthWords div DMA_MAX_CHUNK_SIZE;
  end;

  waitForGP0Ready;
  GPU_GP0 := gp0_vramWrite();
  GPU_GP0 := gp0_xy(x, y);
  GPU_GP0 := gp0_xy(width, height);

  DMA_MADR_Set(DMA_GPU, LongWord(NativeUInt(data)));
  DMA_BCR_Set(DMA_GPU, LongWord(chunkSize) or (LongWord(numChunks) shl 16));
  DMA_CHCR_Set(DMA_GPU, DMA_CHCR_WRITE or DMA_CHCR_MODE_SLICE or DMA_CHCR_ENABLE);

end;


procedure clearOrderingTable(var table: array of LongWord; numEntries: Integer);
begin

  // Give DMA a pointer to the end of the ordering table (reversed)
  DMA_MADR_Set(DMA_OTC, LongWord(NativeUInt(@table[numEntries - 1])));
  DMA_BCR_Set(DMA_OTC, LongWord(numEntries));
  DMA_CHCR_Set(DMA_OTC, DMA_CHCR_READ or DMA_CHCR_REVERSE or DMA_CHCR_MODE_BURST or DMA_CHCR_ENABLE or DMA_CHCR_TRIGGER);

  // wait until OTC DMA finishes
  while (DMA_CHCR(DMA_OTC) and DMA_CHCR_ENABLE) <> 0 do
    asm nop end;

end;


function allocatePacket(var chain: DMAChain; zIndex, numCommands: Integer): pdword;
var
  ptr: pdword;
  tmp: LongWord;

begin

  ptr := chain.nextPacket;
  Inc(chain.nextPacket, numCommands + 1);

  // create tag word linking to the current head of orderingTable[zIndex]
  tmp := gp0_tag(numCommands, Pointer(chain.orderingTable[zIndex]));
  ptr^ := tmp;

  // update ordering table entry to point to this packet (tag pointing to ptr)
  chain.orderingTable[zIndex] := gp0_tag(0, Pointer(ptr));

  // return pointer to the first command word (after tag)
  Result := @ptr[1];

end;


function allocatePacket(var chain: DMAChain; numCommands: Integer): PDWord;
var
  ptr: PDWord;

begin

  ptr := chain.nextPacket;
  Inc(chain.nextPacket, numCommands + 1);
  ptr^ := gp0_tag(numCommands, chain.nextPacket);
  Result := ptr + 1;

end;


procedure uploadTexture(var info: TextureInfo; const data: Pointer; x, y, width, height: Integer);
begin

  if (x < 0) or (y < 0) then exit;
  if (width < 0) or (height < 0) then exit;
  if (width > 65535) or (height > 65535) then exit;

  sendVRAMData(data, x, y, width, height);
  waitForDMADone;

  info.page   := gp0_page(x div 64, y div 256, 1, GP0_COLOR_16BPP);
  info.clut   := 0;
  info.u      := Byte(x mod 64);
  info.v      := Byte(y mod 256);
  info.width  := Word(width);
  info.height := Word(height);

end;


procedure uploadIndexedTexture(var info: TextureInfo; const image, palette: Pointer; imageX, imageY, paletteX, paletteY, width, height: Integer; colorDepth: longint);
var
  numColors, widthDivider: Integer;

begin

  if (imageX < 0) or (imageY < 0) then exit;
  if (width < 0) or (height < 0) then exit;
  if (width > 65535) or (height > 65535) then exit;

  if colorDepth = GP0_COLOR_8BPP then begin
    numColors := 256;
    widthDivider := 2;
  end else begin
    numColors := 16;
    widthDivider := 4;
  end;

  sendVRAMData(image, imageX, imageY, width div widthDivider, height);
  waitForDMADone;
  sendVRAMData(palette, paletteX, paletteY, numColors, 1);
  waitForDMADone;

  info.page   := gp0_page(imageX div 64, imageY div 256, GP0_BLEND_SEMITRANS, colorDepth);
  info.clut   := gp0_clut(paletteX div 16, paletteY);
  info.u      := Byte((imageX mod 64) * widthDivider);
  info.v      := Byte(imageY mod 256);
  info.width  := Word(width);
  info.height := Word(height);

end;


end.
