{$ifdef NDS_INTERFACE}

//type
//  getByteCallback = function(source: pcuint8): pcuint8;
//  getHeaderCallback = function(source: pcuint8; dest: pcuint16; arg: cuint32): pcint;

type
  DecompressType = integer;
const
  LZ77    : DecompressType = 0; 
  LZ77Vram: DecompressType = 1;
  HUFF    : DecompressType = 2;
  RLE     : DecompressType = 3;
  RLEVram : DecompressType = 4;

procedure decompress(const data: pointer; dst: pointer; atype: DecompressType); cdecl; external;
procedure decompressStream(const data: pointer; dst: pointer; atype: DecompressType; readCB: getByteCallback; getHeaderCB: getHeaderCallback); cdecl; external;


{$endif NDS_INTERFACE}
