
{$IFDEF OGC_INTERFACE}
const
  EXI_READ = 0;  (*!< EXI transfer type read  *)
  EXI_WRITE = 1;  (*!< EXI transfer type write  *)
  EXI_READWRITE = 2;  (*!< EXI transfer type read-write  *)

  EXI_CHANNEL_0 = 0;  (*!< EXI channel 0 (memory card slot A)  *)
  EXI_CHANNEL_1 = 1;  (*!< EXI channel 1 (memory card slot B)  *)
  EXI_CHANNEL_2 = 2;  (*!< EXI channel 2 (other EXI devices connected, e.g. BBA)  *)
  EXI_CHANNEL_MAX = 3;  (*!< _Termination  *)

  EXI_DEVICE_0 = 0;  (*!< EXI device 0  *)
  EXI_DEVICE_1 = 1;  (*!< EXI device 1  *)
  EXI_DEVICE_2 = 2;  (*!< EXI device 2  *)
  EXI_DEVICE_MAX = 3;  (*!< _Termination  *)

  EXI_SPEED1MHZ = 0;  (*!< EXI device frequency 1MHz  *)
  EXI_SPEED2MHZ = 1;  (*!< EXI device frequency 2MHz  *)
  EXI_SPEED4MHZ = 2;  (*!< EXI device frequency 4MHz  *)
  EXI_SPEED8MHZ = 3;  (*!< EXI device frequency 8MHz  *)
  EXI_SPEED16MHZ = 4;  (*!< EXI device frequency 16MHz  *)
  EXI_SPEED32MHZ = 5;  (*!< EXI device frequency 32MHz  *)

  EXI_FLAG_DMA = $0001;  (*!< EXI DMA mode transfer in progress  *)
  EXI_FLAG_IMM = $0002;  (*!< EXI immediate mode transfer in progress  *)
  EXI_FLAG_SELECT = $0004;  (*!< EXI channel and device selected  *)
  EXI_FLAG_ATTACH = $0008;  (*!< EXI device on selected channel and device attached  *)
  EXI_FLAG_LOCKED = $0010;  (*!< EXI channel and device locked for device operations  *)
  EXI_MEMCARD59 = $00000004;  (*!< Nintendo memory card:   64/  4/ 0.5 (blocks/Mbits/MB). 3rd party vendors do have the same identification  *)
  EXI_MEMCARD123 = $00000008;  (*!< Nintendo memory card:  128/  8/ 1.0 (blocks/Mbits/MB). 3rd party vendors do have the same identification  *)
  EXI_MEMCARD251 = $00000010;  (*!< Nintendo memory card:  256/ 16/ 2.0 (blocks/Mbits/MB). 3rd party vendors do have the same identification  *)
  EXI_MEMCARD507 = $00000020;  (*!< Nintendo memory card:  512/ 32/ 4.0 (blocks/Mbits/MB). 3rd party vendors do have the same identification  *)
  EXI_MEMCARD1019 = $00000040;  (*!< Nintendo memory card: 1024/ 64/ 8.0 (blocks/Mbits/MB). 3rd party vendors do have the same identification  *)
  EXI_MEMCARD2043 = $00000080;  (*!< Nintendo memory card: 2048/128/16.0 (blocks/Mbits/MB). 3rd party vendors do have the same identification  *)

type
  EXICallback = function(chn, dev: cint32): cint32; cdecl;

function EXI_ProbeEx(nChn: cint32): cint32; cdecl; external;

function EXI_Probe(nChn: cint32): cint32; cdecl; external;

function EXI_Lock(nChn, nDev: cint32; unlockCB: EXICallback): cint32; cdecl; external;

function EXI_Unlock(nChn: cint32): cint32; cdecl; external;

function EXI_Select(nChn, nDev, nFrq: cint32): cint32; cdecl; external;

function EXI_SelectSD(nChn, nDev, nFrq: cint32): cint32; cdecl; external;

function EXI_Deselect(nChn: cint32): cint32; cdecl; external;

function EXI_Sync(nChn: cint32): cint32; cdecl; external;

function EXI_Imm(nChn: cint32; pData: pointer; nLen, nMode: cuint32;
  tc_cb: EXICallback): cint32; cdecl; external;

function EXI_ImmEx(nChn: cint32; pData: pointer; nLen, nMode: cuint32): cint32; cdecl; external;

function EXI_Dma(nChn: cint32; pData: pointer; nLen, nMode: cuint32;
  tc_cb: EXICallback): cint32; cdecl; external;

function EXI_GetState(nChn: cint32): cint32; cdecl; external;

function EXI_GetID(nChn, nDev: cint32; nId: pcuint32): cint32; cdecl; external;

function EXI_Attach(nChn: cint32; ext_cb: EXICallback): cint32; cdecl; external;

function EXI_Detach(nChn: cint32): cint32; cdecl; external;

procedure EXI_ProbeReset; cdecl; external;

function EXI_RegisterEXICallback(nChn: cint32; exi_cb: EXICallback): EXICallback; cdecl; external;
{$ENDIF}


