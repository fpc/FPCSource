{$ifdef NDS_INTERFACE}

type
  TGetHeaderCallback = function(source: pcuint8;  dest: pcuint16; r2: cuint32): pcint;
  getHeaderCallback = TGetHeaderCallback;
  TGetResultCallback = function(source: pcuint8): pcint;
  getResultCallback = TGetResultCallback; 
  TGetByteCallback = function(source: pcuint8): pcuint8; 
  getByteCallback = TGetByteCallback;

type
  DecompressionStream = packed record
    getSize: TGetHeaderCallback; 
    getResult: TGetResultCallback; 
    readByte: TGetByteCallback; 
  end;
  TDecompressionStream = DecompressionStream;
  PDecompressionStream = ^TDecompressionStream;

  UnpackStruct = packed record
    sourceSize: cuint16; // in bytes
    sourceWidth: cuint8;
    destWidth: cuint8;
    dataOffset: cuint32;
  end;
  TUnpackStruct = UnpackStruct;
  PUnpackStruct = ^TUnpackStruct;


procedure swiSoftReset(); cdecl; external;

procedure swiDelay(duration: cuint32); cdecl; external;

function swiDivide(numerator, divisor: cint): cint; cdecl; external;
function swiRemainder(numerator, divisor: cint): cint; cdecl; external;
procedure swiDivMod(numerator, divisor: cint; var result, remainder: cint); cdecl; external;

const
  COPY_MODE_HWORD = (0);
  COPY_MODE_WORD  = (1 shl 26);
  COPY_MODE_COPY  = (0);
  COPY_MODE_FILL  = (1 shl 24);

procedure swiCopy(const source: pointer; dest: pointer; flags: cint); cdecl; external;
procedure swiFastCopy(const source: pointer; dest: pointer; flags: cint); cdecl; external;

function swiSqrt(value: cint): cint; cdecl; external;

function swiCRC16(crc: cuint16; data: pointer; size: cuint32): cuint16; cdecl; external;

function swiIsDebugger(): cint; cdecl; external;

procedure swiUnpackBits(source: pcuint8; destination: pcuint32; params: PUnpackStruct); cdecl; external;

procedure swiDecompressLZSSWram(source, destination: pointer); cdecl; external;
function swiDecompressLZSSVram(source, destination: pointer; toGetSize: cuint32; stream: PDecompressionStream): cint; cdecl; external;

function swiDecompressLZSSVramNTR(source, destination: pointer; toGetSize: cuint32; stream: PDecompressionStream): cint; cdecl; external;
function swiDecompressLZSSVramTWL(source, destination: pointer; toGetSize: cuint32; stream: PDecompressionStream): cint; cdecl; external;

function swiDecompressHuffman(source, destination: pointer; toGetSize: cuint32; stream: PDecompressionStream): cint; cdecl; external;

procedure swiDecompressRLEWram(source, destination: pointer); cdecl; external;
function swiDecompressRLEVram(source, destination: pointer; toGetSize: cuint32; stream: PDecompressionStream): cint; cdecl; external;

{$ifdef ARM9}
  procedure swiWaitForIRQ(); cdecl; external;
  procedure swiSetHaltCR(data: cuint32); cdecl; external;
  procedure swiDecodeDelta8(source, destination: pointer); cdecl; external;
  procedure swiDecodeDelta16(source, destination: pointer); cdecl; external;
{$endif ARM9}

{$ifdef ARM7}
  procedure swiHalt(); cdecl; external;
  procedure swiSleep(); cdecl; external;
  procedure swiSwitchToGBAMode(); cdecl; external;
  procedure swiSetHaltCR(data: cuint8); cdecl; external;
  function swiGetSineTable(index: cint): cuint16; cdecl; external;
  function swiGetPitchTable(index: cint): cuint16; cdecl; external;
  function swiGetVolumeTable(index: cint): cuint8; cdecl; external;
  procedure swiChangeSoundBias(enabled, delay: cint); cdecl; external;
{$endif ARM7}

{$endif NDS_INTERFACE}
