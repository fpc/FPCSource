
{$IFDEF OGC_INTERFACE}
const
  MAX_MI_NUM = 1;
  MAX_DI_NUM = 5;
  PAGE_SIZE256 = 256;
  PAGE_SIZE512 = 512;

(* CID Register  *)

{$define MANUFACTURER_ID(drv_no) := (cuint8(g_CID[drv_no][0]))}

(* CSD Register  *)

{$define READ_BL_LEN(drv_no) := (cuint8(g_CSD[drv_no][5] and $0f))}
{$define WRITE_BL_LEN(drv_no) := (cuint8((g_CSD[drv_no][12] and $03) shl 2) or ((g_CSD[drv_no][13] shr 6) and $03))}
{$define C_SIZE(drv_no) := (cuint16(((g_CSD[drv_no][6] and $03) shl 10) or (g_CSD[drv_no][7] shl 2) or ((g_CSD[drv_no][8] shr 6) and $03)))}
{$define C_SIZE_MULT(drv_no) := (cuint8((g_CSD[drv_no][9] and $03) shl 1) or ((g_CSD[drv_no][10] shr 7) and $01))}


var
  g_CSD : array [0..15] of cuint8; external;
  g_CID : array [0..15] of cuint8; external;
  g_mCode : array [0..MAX_MI_NUM-1] of cuint8; external;
  g_dCode : array [0..MAX_DI_NUM-1] of cuint16; external;


procedure sdgecko_initIODefault; cdecl; external;
function sdgecko_initIO(drv_no: cint32): cint32; cdecl; external;
function sdgecko_preIO(drv_no: cint32): cint32; cdecl; external;
function sdgecko_readCID(drv_no: cint32): cint32; cdecl; external;
function sdgecko_readCSD(drv_no: cint32): cint32; cdecl; external;
function sdgecko_readStatus(drv_no: cint32): cint32; cdecl; external;
function sdgecko_readSectors(drv_no: cint32; sector_no, num_sectors: cuint32;
  buf: pointer): cint32; cdecl; external;
function sdgecko_writeSector(drv_no: cint32; sector_no: cuint32; buf: pointer;
  len: cuint32): cint32; cdecl; external;
function sdgecko_writeSectors(drv_no: cint32; sector_no, num_sectors: cuint32;
  buf: pointer): cint32; cdecl; external;
function sdgecko_doUnmount(drv_no: cint32): cint32; cdecl; external;
procedure sdgecko_insertedCB(drv_no: cint32); cdecl; external;
procedure sdgecko_ejectedCB(drv_no: cint32); cdecl; external;
{$ENDIF}




