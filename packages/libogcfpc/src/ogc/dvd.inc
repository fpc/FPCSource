
{$IFDEF OGC_INTERFACE}
const
  DVD_STATE_FATAL_ERROR = - 1;
  DVD_STATE_END = 0;
  DVD_STATE_BUSY = 1;
  DVD_STATE_WAITING = 2;
  DVD_STATE_COVER_CLOSED = 3;
  DVD_STATE_NO_DISK = 4;
  DVD_STATE_COVER_OPEN = 5;
  DVD_STATE_WRONG_DISK = 6;
  DVD_STATE_MOTOR_STOPPED = 7;
  DVD_STATE_IGNORED = 8;
  DVD_STATE_CANCELED = 10;
  DVD_STATE_RETRY = 11;
  DVD_ERROR_OK = 0;
  DVD_ERROR_FATAL = - 1;
  DVD_ERROR_IGNORED = - 2;
  DVD_ERROR_CANCELED = - 3;
  DVD_ERROR_COVER_CLOSED = - 4;
  DVD_RESETHARD = 0;  (*!< Performs a hard reset. Complete new boot of FW.  *)
  DVD_RESETSOFT = 1;  (*!< Performs a soft reset. FW restart and drive spinup  *)
  DVD_RESETNONE = 2;  (*!< Only initiate DI registers  *)
  DVD_SPINMOTOR_DOWN = $00000000;  (*!< Stop DVD drive  *)
  DVD_SPINMOTOR_UP = $00000100;  (*!< Start DVD drive  *)
  DVD_SPINMOTOR_ACCEPT = $00004000;  (*!< Force DVD to accept the disk  *)
  DVD_SPINMOTOR_CHECKDISK = $00008000;  (*!< Force DVD to perform a disc check  *)

type

  _dvddiskid = record
    gamename : array [0..3] of cint8;
    company : array [0..1] of cint8;
    disknum : cuint8;
    gamever : cuint8;
    streaming : cuint8;
    streambufsize : cuint8;
    pad : array [0..21] of cuint8;
  end;
  dvddiskid = _dvddiskid;
  pdvddiskid = ^dvddiskid;


  dvdcbcallback = procedure(result: cint32; block: pointer); cdecl;

  _dvdcmdblk = record
    node : lwp_node;
    cmd : cuint32;
    state : cint32;
    offset : cint64;
    len : cuint32;
    buf : pointer;
    currtxsize : cuint32;
    txdsize : cuint32;
    id : Pdvddiskid;
    cb : dvdcbcallback;
    usrdata : pointer;
  end;
  dvdcmdblk = _dvdcmdblk;
  Pdvdcmdblk = ^dvdcmdblk;


  _dvddrvinfo = record
    rev_level : cuint16;
    dev_code : cuint16;
    rel_date : cuint32;
    pad : array [0..23] of cuint8;
  end;
  dvddrvinfo = _dvddrvinfo;
  Pdvddrvinfo = ^dvddrvinfo;


  dvdcallback = procedure(result: cint32; info: pointer); cdecl;

  _dvdfileinfo = record
    block : dvdcmdblk;
    addr : cuint32;
    len : cuint32;
    cb : dvdcallback;
  end;
  dvdfileinfo = _dvdfileinfo;
  Pdvdfileinfo = ^dvdfileinfo;


procedure DVD_Init; cdecl; external;

procedure DVD_Pause; cdecl; external;


procedure DVD_Reset(reset_mode: cuint32); cdecl; external;


function DVD_Mount: cint32; cdecl; external;

function DVD_GetDriveStatus: cint32; cdecl; external;


function DVD_MountAsync(block: Pdvdcmdblk; cb: dvdcbcallback): cint32; cdecl; external;


function DVD_ControlDrive(block: Pdvdcmdblk; cmd: cuint32): cint32; cdecl; external;


function DVD_ControlDriveAsync(block: Pdvdcmdblk; cmd: cuint32;
  cb: dvdcbcallback): cint32; cdecl; external;


function DVD_SetGCMOffset(block: Pdvdcmdblk; offset: cint64): cint32; cdecl; external;


function DVD_SetGCMOffsetAsync(block: Pdvdcmdblk; offset: cint64;
  cb: dvdcbcallback): cint32; cdecl; external;


function DVD_GetCmdBlockStatus(block: Pdvdcmdblk): cint32; cdecl; external;

function DVD_SpinUpDrive(block: Pdvdcmdblk): cint32; cdecl; external;

function DVD_SpinUpDriveAsync(block: Pdvdcmdblk; cb: dvdcbcallback): cint32; cdecl; external;

function DVD_Inquiry(block: Pdvdcmdblk; info: Pdvddrvinfo): cint32; cdecl; external;

function DVD_InquiryAsync(block: Pdvdcmdblk; info: Pdvddrvinfo;
  cb: dvdcbcallback): cint32; cdecl; external;

function DVD_ReadPrio(block: Pdvdcmdblk; buf: pointer; len: cuint32;
  offset: cint64; prio: cint32): cint32; cdecl; external;

function DVD_ReadAbsAsyncPrio(block: Pdvdcmdblk; buf: pointer; len: cuint32;
  offset: cint64; cb: dvdcbcallback; prio: cint32): cint32; cdecl; external;

function DVD_ReadAbsAsyncForBS(block: Pdvdcmdblk; buf: pointer; len: cuint32;
  offset: cint64; cb: dvdcbcallback): cint32; cdecl; external;

function DVD_SeekPrio(block: Pdvdcmdblk; offset: cint64; prio: cint32): cint32; cdecl; external;

function DVD_SeekAbsAsyncPrio(block: Pdvdcmdblk; offset: cint64;
  cb: dvdcbcallback; prio: cint32): cint32; cdecl; external;

function DVD_CancelAllAsync(cb: dvdcbcallback): cint32; cdecl; external;

function DVD_StopStreamAtEndAsync(block: Pdvdcmdblk; cb: dvdcbcallback): cint32; cdecl; external;

function DVD_StopStreamAtEnd(block: Pdvdcmdblk): cint32; cdecl; external;

function DVD_ReadDiskID(block: Pdvdcmdblk; id: Pdvddiskid; cb: dvdcbcallback)
  : cint32; cdecl; external;

function DVD_SetAutoInvalidation(auto_inv: cuint32): cuint32; cdecl; external;

function DVD_GetCurrentDiskID: Pdvddiskid; cdecl; external;

function DVD_GetDriveInfo: Pdvddrvinfo; cdecl; external;


procedure DVD_SetUserData(block: _dvdcmdblk; data: pointer); inline;
function DVD_GetUserData(block: _dvdcmdblk): pointer; inline;

const
  DEVICE_TYPE_GAMECUBE_DVD = ((Ord('G') shl 24) or (Ord('D') shl 16) or (Ord('V') shl 8) or Ord('D'));

var
  __io_gcdvd : DISC_INTERFACE; cvar; external;
{$ENDIF}


{$IFDEF OGC_IMPLEMENTATION}
procedure DVD_SetUserData(block: _dvdcmdblk; data: pointer); inline;
begin
  block.usrdata := data;
end;

function DVD_GetUserData(block: _dvdcmdblk): pointer; inline;
begin
  DVD_GetUserData := block.usrdata;
end;
{$ENDIF}

