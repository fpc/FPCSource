unit gccore;
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

{$define HW_RVL}

interface

uses
  cmem, ctypes, math, gctypes;

const
  RNC_FILE_IS_NOT_RNC     = -1;
  RNC_HUF_DECODE_ERROR    = -2;
  RNC_FILE_SIZE_MISMATCH  = -3;
  RNC_PACKED_CRC_ERROR    = -4;
  RNC_UNPACKED_CRC_ERROR  = -5;

function depackrnc1_ulen(packed_: pointer): cint32; cdecl; external;
function depackrnc1(packed_, unpacked: pointer): cint32; cdecl; external;
procedure depackrnc2(packed_, unpacked: pointer); cdecl; external;

{$define OGC_INTERFACE}
{$include gccore.inc}
{$undef OGC_INTERFACE}

implementation

{$define OGC_IMPLEMENTATION}
{$include gccore.inc}
{$undef OGC_IMPLEMENTATION}

initialization

{$linklib wiikeyboard}
{$linklib wiiuse}
{$linklib bte}
{$linklib ogc}
{$linklib m}

{$linklib c} 
{$linklib gcc}  
{$linklib g} 
{$linklib sysbase}

{$linklib iberty}


end.
