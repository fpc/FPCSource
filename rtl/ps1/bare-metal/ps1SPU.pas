{$MODE OBJFPC}{$H+}
unit ps1SPU;
interface
uses ps1System;

type
  Channel     = Integer;
  ChannelMask = Cardinal;

const
  { LoopFlag }
  LOOP_END     = 1 shl 0;
  LOOP_SUSTAIN = 1 shl 1;
  LOOP_START   = 1 shl 2;

  DUMMY_BLOCK_OFFSET = $01010;
  DUMMY_BLOCK_END    = $01010;
  SPU_RAM_END        = $7FFF0;

  NUM_CHANNELS = 24;
  MAX_VOLUME   = $3FFF;

  ALL_CHANNELS : ChannelMask = (1 shl NUM_CHANNELS) - 1;


const
  _DMA_CHUNK_SIZE = 4;
  _DMA_TIMEOUT    = 100000;
  _STATUS_TIMEOUT = 10000;

var
  // Pointer to the next free space in SPU RAM
  spuAllocPtr: Cardinal = $01010;

{ Utilities }

function concat4_8(a,b,c,d: Byte): Cardinal; inline;
function concat4_16(a,b: Word): Cardinal; inline;
function bswap32(num: Cardinal): Cardinal; inline;
function roundup(a,b: SizeUInt): SizeUInt; inline;

{ Basic SPU API }

procedure initSPU;

procedure setMasterVolume(master, reverb: Word); inline;
procedure setChannelVolume(channel: Byte; master: Word); inline;
procedure stopChannel(ch: Channel); inline;

function upload(offset: Cardinal; data: Pointer; length: SizeUInt; wait: Boolean): SizeUInt;

type
  TSound = record
    offset: Cardinal;
    sampleRate, length: Word;
  end;

function sound_playOnChannel(sound: TSound; ch: Channel): Channel;

implementation

function concat4_8(a,b,c,d: Byte): Cardinal; inline;
begin
  Result := Cardinal(a) or (Cardinal(b) shl 8) or
            (Cardinal(c) shl 16) or (Cardinal(d) shl 24);
end;

function concat4_16(a,b: Word): Cardinal; inline;
begin
  Result := Cardinal(a) or (Cardinal(b) shl 16);
end;

function bswap32(num: Cardinal): Cardinal; inline;
begin
  Result := ((num shr 24) and $FF) or
            ((num shr 8)  and $FF00) or
            ((num shl 8)  and $FF0000) or
            ((num shl 24) and $FF000000);
end;

function roundup(a,b: SizeUInt): SizeUInt; inline;
begin
  Result := ((a + b - 1) div b) * b;
end;

procedure setMasterVolume(master, reverb: Word); inline;
begin
  SPU_MASTER_VOL_L := master;
  SPU_MASTER_VOL_R := master;
  SPU_REVERB_VOL_L := reverb;
  SPU_REVERB_VOL_R := reverb;
end;

procedure setChannelVolume(channel: Byte; master: Word); inline;
begin
  SPU_CH_VOL_L_Set(channel, master);
  SPU_CH_VOL_R_Set(channel, master);
end;

procedure stopChannel(ch: Channel); inline;
begin
 // stopChannels(1 shl ch);
end;



// ---------------- Private helpers ----------------
function _waitForStatus(mask, value: Word): Boolean;
var
  timeout: Integer;
begin
  timeout := _STATUS_TIMEOUT;
  while timeout > 0 do
  begin
    if (SPU_STAT and mask) = value then
      Exit(True);
    delayMicroseconds(10);
    Dec(timeout, 10);
  end;
  Result := False;
end;

// ---------------- Public API ---------------------

procedure initSPU;
  var i : longint;
begin
(*
    BIU_DEV4_CTRL := 0
        or( 1 shl  0) // Write delay
        or(14 shl  4) // Read delay
        or BIU_CTRL_RECOVERY
        or BIU_CTRL_WIDTH_16
        or BIU_CTRL_AUTO_INCR
        or ( 9 shl 16) // Number of address lines
        or ( 0 shl 24) // DMA read/write delay
        or BIU_CTRL_DMA_DELAY;
    
    SPU_CTRL_REG:= 0;
    _waitForStatus($3f, 0);
{
    SPU_MASTER_VOL_L:= 0;
    SPU_MASTER_VOL_R:= 0;
    SPU_REVERB_VOL_L:= 0;
    SPU_REVERB_VOL_R:= 0;
 }   SPU_REVERB_ADDR := SPU_RAM_END div 8;

    SPU_FLAG_FM1    := 0;
    SPU_FLAG_FM2    := 0;
    SPU_FLAG_NOISE1 := 0;
    SPU_FLAG_NOISE2 := 0;
    SPU_FLAG_REVERB1:= 0;
    SPU_FLAG_REVERB2:= 0;

    SPU_CTRL_REG:= SPU_CTRL_ENABLE;
  _waitForStatus($3f, 0);

    // Place a dummy (silent) looping block at the beginning of SPU RAM.

    SPU_DMA_CTRL:= 4;
    SPU_ADDR    := DUMMY_BLOCK_OFFSET div 8;

    SPU_DATA:= $0500;
    for i:= 0 to 24 do SPU_DATA:= 0;

    SPU_CTRL_REG:= SPU_CTRL_XFER_WRITE or SPU_CTRL_ENABLE;
    _waitForStatus(SPU_CTRL_XFER_BITMASK or SPU_STAT_BUSY, SPU_CTRL_XFER_WRITE);
    delayMicroseconds(100);

    SPU_CTRL_REG:= SPU_CTRL_UNMUTE or SPU_CTRL_ENABLE;
*)

SPU_CTRL_REG := 0;              // reset
  _waitForStatus($3F, 0);

  // Set master volume full
  SPU_MASTER_VOL_L := MAX_VOLUME;
  SPU_MASTER_VOL_R := MAX_VOLUME;

  SPU_REVERB_VOL_L := 0;
  SPU_REVERB_VOL_R := 0;

  SPU_CTRL_REG:= SPU_CTRL_UNMUTE or SPU_CTRL_ENABLE;
  _waitForStatus($3F, 0);

    // Enable the SPU's DMA channel
    DMA_DPCR:= DMA_DPCR or DMA_DPCR_CH_ENABLE(DMA_SPU);


end;


procedure stopChannels(mask: ChannelMask);
var
  ch: Channel;
  m: ChannelMask;
begin
  mask := mask and ALL_CHANNELS;

   SPU_FLAG_OFF1 := mask and $FFFF;
   SPU_FLAG_OFF2 := mask shr 16;

  m := mask;
  for ch := 0 to NUM_CHANNELS - 1 do
  begin
    if (m and 1) <> 0 then
    begin
       SPU_CH_VOL_L_Set(ch, 0);
       SPU_CH_VOL_R_Set(ch, 0);
       SPU_CH_FREQ_Set(ch, 1 shl 12);
       SPU_CH_ADDR_Set(ch, DUMMY_BLOCK_OFFSET div 8);
    end;
    m := m shr 1;
    if m = 0 then Break;
  end;

   SPU_FLAG_ON1 := mask and $FFFF;
   SPU_FLAG_ON2 := mask shr 16;
end;



function upload(offset: Cardinal; data: Pointer; length: dword; wait: Boolean): SizeUInt;
var
  ctrlReg: Word;
  d : dword;
words, dmaBlocks: Cardinal;
begin
  words := (length + 3) div 4;
  dmaBlocks := (words + _DMA_CHUNK_SIZE - 1) div _DMA_CHUNK_SIZE;
  d:= (_DMA_CHUNK_SIZE shl 16) or dmaBlocks;

  writeln(d);

  if not waitForDMATransfer(DMA_SPU, _DMA_TIMEOUT) then begin
    writeln('waitForDMATransfer');
    Exit(0);
  end;

  ctrlReg := SPU_CTRL_REG and (not SPU_CTRL_XFER_BITMASK);

  SPU_CTRL_REG := ctrlReg;
  _waitForStatus(SPU_CTRL_XFER_BITMASK, 0);

  SPU_DMA_CTRL := 4;
  SPU_ADDR     := offset div 8;
  SPU_CTRL_REG     := ctrlReg or SPU_CTRL_XFER_DMA_WRITE;
  _waitForStatus(SPU_CTRL_XFER_BITMASK, SPU_CTRL_XFER_DMA_WRITE);

  DMA_MADR_Set(DMA_SPU, dword(data));
  DMA_BCR_Set(DMA_SPU, d);
  DMA_CHCR_Set(DMA_SPU, 0
    or DMA_CHCR_WRITE
    or DMA_CHCR_MODE_SLICE
    or DMA_CHCR_ENABLE);

  if wait then
    waitForDMATransfer(DMA_SPU, _DMA_TIMEOUT);

  Result := length * _DMA_CHUNK_SIZE * 4;
end;


function sound_playOnChannel(sound: TSound; ch: Channel): Channel;
begin
  SPU_CTRL_REG := SPU_CTRL_ENABLE or SPU_CTRL_UNMUTE;

  SPU_CH_VOL_L_set(ch, $3FFF); // max volume left
  SPU_CH_VOL_R_set(ch, $3FFF); // max volume right


  SPU_CH_FREQ_Set(ch, (sound.sampleRate shl 12) div 44100);

  SPU_CH_ADDR_Set (ch, spuAllocPtr div 8);
  SPU_CH_ADSR1_Set(ch, $0f00);
  SPU_CH_ADSR2_Set(ch, $f000);
  
  if ch < 16 then SPU_FLAG_ON1 := 1 shl ch else SPU_FLAG_ON2 := 1 shl (ch - 16);

end;


end.
