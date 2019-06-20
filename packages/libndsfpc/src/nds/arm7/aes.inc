{$ifdef NDS_INTERFACE}
type
  _aes_keyslot = packed record
	normalkey: array [0..15] of pcuint8;
	key_x: array [0..15] of pcuint8;
	key_y: array [0..15] of pcuint8;
  end; 
  aes_keyslot_t = _aes_keyslot;
  Taes_keyslot_t = aes_keyslot_t;
  Paes_keyslot_t = ^Taes_keyslot_t;


const 
  REG_AES_CNT: pcuint32  = pointer($04004400);
  AES_WRFIFO_FLUSH = (1 shl 10);
  AES_RDFIFO_FLUSH = (1 shl 11);

function AES_CNT_DMA_WRITE_SIZE(size: cint): cint; inline;
function AES_CNT_DMA_READ_SIZE(size: cint): cint; inline;

function AES_CNT_CCM_SIZE(size: cint): cint; inline;

const
  AES_CCM_PASSTRHOUGH = (1 shl 19);
  AES_CNT_KEY_APPLY = (1 shl 24);

function AES_CNT_KEYSLOT(slot: cint): cint; inline;

function AES_CNT_MODE(mode: cint): cint; inline;

const
  AES_CNT_IRQ       = (1 shl 30);
  AES_CNT_ENABLE    = (1 shl 31);
  
  REG_AES_BLKCNT: pcuint32 = pointer($4004404);

  REG_AES_WRFIFO: pcuint32 = pointer($4004408);
  REG_AES_RDFIFO: pcuint32 = pointer($400440c);

  REG_AES_IV: pcuint8 = pointer($4004420);
  REG_AES_MAC: pcuint8 = pointer($4004430);

  AES_KEYSLOT  :  Paes_keyslot_t = pointer($4004440);
  AES_KEYSLOT0 :  Paes_keyslot_t = pointer($4004440);
  AES_KEYSLOT1 :  Paes_keyslot_t = pointer($4004470);
  AES_KEYSLOT2 :  Paes_keyslot_t = pointer($40044A0);
  AES_KEYSLOT3 :  Paes_keyslot_t = pointer($40044D0);
{$endif NDS_INTERFACE}

{$ifdef NDS_IMPLEMENTATION}
function AES_CNT_DMA_WRITE_SIZE(size: cint): cint; inline; 
begin
  AES_CNT_DMA_WRITE_SIZE := ((size and 3) shl 12);
end;

function AES_CNT_DMA_READ_SIZE(size: cint): cint; inline;  
begin
  AES_CNT_DMA_READ_SIZE := ((size and 3) shl 14);
end;

function AES_CNT_CCM_SIZE(size: cint): cint; inline;
begin
  AES_CNT_CCM_SIZE := ((size and 3) shl 16);
end;

function AES_CNT_KEYSLOT(slot: cint): cint; inline; 
begin
  AES_CNT_KEYSLOT := ((slot and 3) shl 26);
end;

function AES_CNT_MODE(mode: cint): cint; inline;
begin
  AES_CNT_MODE := ((mode and 3) shl 28);
end;

{$endif NDS_IMPLEMENTATION}