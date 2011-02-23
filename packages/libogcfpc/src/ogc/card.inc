{$IFDEF OGC_INTERFACE}
const
  CARD_SLOTA = 0;  (*!< memory card slot A  *)
  CARD_SLOTB = 1;  (*!< memory card slot B  *)
  CARD_WORKAREA = ( 5 * 8 * 1024 );  (*!< minimum size of the workarea passed to Mount[Async]()  *)
  CARD_READSIZE = 512;  (*!< minimum size of block to read from memory card  *)
  CARD_FILENAMELEN = 32;  (*!< maximum filename length  *)
  CARD_MAXFILES = 128;  (*!< maximum number of files on the memory card  *)
  CARD_ERROR_UNLOCKED = 1;  (*!< card beeing unlocked or allready unlocked.  *)
  CARD_ERROR_READY = 0;  (*!< card is ready.  *)
  CARD_ERROR_BUSY = - 1;  (*!< card is busy.  *)
  CARD_ERROR_WRONGDEVICE = - 2;  (*!< wrong device connected in slot  *)
  CARD_ERROR_NOCARD = - 3;  (*!< no memory card in slot  *)
  CARD_ERROR_NOFILE = - 4;  (*!< specified file not found  *)
  CARD_ERROR_IOERROR = - 5;  (*!< internal EXI I/O error  *)
  CARD_ERROR_BROKEN = - 6;  (*!< directory structure or file entry broken  *)
  CARD_ERROR_EXIST = - 7;  (*!< file allready exists with the specified parameters  *)
  CARD_ERROR_NOENT = - 8;  (*!< found no empty block to create the file  *)
  CARD_ERROR_INSSPACE = - 9;  (*!< not enough space to write file to memory card  *)
  CARD_ERROR_NOPERM = - 10;  (*!< not enough permissions to operate on the file  *)
  CARD_ERROR_LIMIT = - 11;  (*!< card size limit reached  *)
  CARD_ERROR_NAMETOOLONG = - 12;  (*!< filename too long  *)
  CARD_ERROR_ENCODING = - 13;  (*!< font encoding PAL/SJIS mismatch *)
  CARD_ERROR_CANCELED = - 14;  (*!< card operation canceled  *)
  CARD_ERROR_FATAL_ERROR = - 128;  (*!< fatal error, non recoverable  *)


const
  CARD_ATTRIB_PUBLIC = $04;
  CARD_ATTRIB_NOCOPY = $08;
  CARD_ATTRIB_NOMOVE = $10;

  CARD_BANNER_W = 96;
  CARD_BANNER_H = 32;
  CARD_BANNER_NONE = $00;
  CARD_BANNER_CI = $01;
  CARD_BANNER_RGB = $02;
  CARD_BANNER_MASK = $03;

  CARD_MAXICONS = 8;
  CARD_ICON_W = 32;
  CARD_ICON_H = 32;
  CARD_ICON_NONE = $00;
  CARD_ICON_CI = $01;
  CARD_ICON_RGB = $02;
  CARD_ICON_MASK = $03;
  CARD_ANIM_LOOP = $00;
  CARD_ANIM_BOUNCE = $04;
  CARD_ANIM_MASK = $04;
  CARD_SPEED_END = $00;
  CARD_SPEED_FAST = $01;
  CARD_SPEED_MIDDLE = $02;
  CARD_SPEED_SLOW = $03;
  CARD_SPEED_MASK = $03;

type
  _card_file = record
    chn : cint32;
    filenum : cint32;
    offset : cint32;
    len : cint32;
    iblock : cuint16;
  end;
  card_file = _card_file;
  pcard_file = ^card_file;

  _card_dir = record
    chn : cint32;
    fileno : cuint32;
    filelen : cuint32;
    permissions : cuint8;
    filename : array [0..CARD_FILENAMELEN-1] of cuint8;
    gamecode : array [0..3] of cuint8;
    company : array [0..1] of cuint8;
    showall : cbool;
  end;
  card_dir = _card_dir;
  pcard_dir = ^card_dir;

  _card_stat = record
    filename : array [0..CARD_FILENAMELEN-1] of cuint8;
    len : cuint32;
    time : cuint32;  //time since 1970 in seconds
    gamecode : array [0..3] of cuint8;
    company : array [0..1] of cuint8;
    banner_fmt : cuint8;
    icon_addr : cuint32;
    icon_fmt : cuint16;
    iconfmt : array [0..CARD_MAXICONS-1] of cuint16;
    icon_speed : cuint16;
    iconspeed : array [0..CARD_MAXICONS-1] of cuint16;
    comment_addr : cuint32;
    offset_banner : cuint32;
    offset_banner_tlut : cuint32;
    offset_icon : array [0..CARD_MAXICONS-1] of cuint32;
    offset_icon_tlut : array [0..CARD_MAXICONS-1] of cuint32;
    offset_data : cuint32;
  end;
  card_stat = _card_stat;
  pcard_stat = ^card_stat;

function CARD_GetBannerFmt(stat: card_stat): cuint8; inline;
procedure CARD_SetBannerFmt(stat: card_stat; fmt: cuint8); inline;
function CARD_GetIconFmt(stat: card_stat; n: cint): cuint16; inline;
procedure CARD_SetIconFmt(stat: card_stat; n: cint; fmt: cuint8); inline;
function CARD_GetIconSpeed(stat: card_stat; n: cint):	cuint16; inline;
procedure CARD_SetIconSpeed(stat: card_stat; n: cint; speed: cuint16); inline;
procedure CARD_SetIconAddr(stat: card_stat; addr: cuint32); inline;
procedure CARD_SetCommentAddr(stat: card_stat; addr: cuint32); inline;

type
  cardcallback = procedure(chan, result: cint32); cdecl;



function CARD_Init(const gamecode, company: pcchar): cint32; cdecl; external;


function CARD_Probe(chn: cint32): cint32; cdecl; external;



function CARD_ProbeEx(chn: cint32; mem_size, sect_size: pcint32): cint32; cdecl; external;

function CARD_Mount(chn: cint32; workarea: pointer; detach_cb: cardcallback)
  : cint32; cdecl; external;

function CARD_MountAsync(chn: cint32; workarea: pointer;
  detach_cb, attach_cb: cardcallback): cint32; cdecl; external;


function CARD_Unmount(chn: cint32): cint32; cdecl; external;


function CARD_Read(file_: Pcard_file; buffer: pointer; len, offset: cuint32)
  : cint32; cdecl; external;

function CARD_ReadAsync(file_: Pcard_file; buffer: pointer;
  len, offset: cuint32; callback: cardcallback): cint32; cdecl; external;


function CARD_Open(chn: cint32; filename: Pcchar; file_: Pcard_file): cint32; cdecl; external;

function CARD_OpenEntry(chn: cint32; entry: Pcard_dir; file_: Pcard_file)
  : cint32; cdecl; external;



function CARD_Close(file_: Pcard_file): cint32; cdecl; external;


function CARD_Create(chn: cint32; filename: pcchar; size: cuint32;
  file_: Pcard_file): cint32; cdecl; external;

function CARD_CreateAsync(chn: cint32; filename: pcchar; size: cuint32;
  file_: Pcard_file; callback: cardcallback): cint32; cdecl; external;

function CARD_CreateEntry(chn: cint32; direntry: Pcard_dir; file_: Pcard_file)
  : cint32; cdecl; external;

function CARD_CreateEntryAsync(chn: cint32; direntry: Pcard_dir;
  file_: Pcard_file; callback: cardcallback): cint32; cdecl; external;

function CARD_Delete(chn: cint32; filename: pcchar): cint32; cdecl; external;

function CARD_DeleteAsync(chn: cint32; filename: pcchar; callback: cardcallback)
  : cint32; cdecl; external;

function CARD_DeleteEntry(chn: cint32; dir_entry: Pcard_dir): cint32; cdecl; external;

function CARD_DeleteEntryAsync(chn: cint32; dir_entry: Pcard_dir;
  callback: cardcallback): cint32; cdecl; external;

function CARD_Write(file_: Pcard_file; buffer: pointer; len, offset: cuint32)
  : cint32; cdecl; external;

function CARD_WriteAsync(file_: Pcard_file; buffer: pointer;
  len, offset: cuint32; callback: cardcallback): cint32; cdecl; external;

function CARD_GetErrorCode(chn: cint32): cint32; cdecl; external;

function CARD_FindFirst(chn: cint32; dir: Pcard_dir; showall: cbool): cint32; cdecl; external;

function CARD_FindNext(dir: Pcard_dir): cint32; cdecl; external;

function CARD_GetDirectory(chn: cint32; dir_entries: Pcard_dir; count: pcint32;
  showall: cbool): cint32; cdecl; external;

function CARD_GetSectorSize(chn: cint32; sector_size: pcuint32): cint32; cdecl; external;

function CARD_GetBlockCount(chn: cint32; block_count: pcuint32): cint32; cdecl; external;

function CARD_GetStatus(chn, fileno: cint32; stats: Pcard_stat): cint32; cdecl; external;

function CARD_SetStatus(chn, fileno: cint32; stats: Pcard_stat): cint32; cdecl; external;

function CARD_SetStatusAsync(chn, fileno: cint32; stats: Pcard_stat;
  callback: cardcallback): cint32; cdecl; external;

function CARD_GetAttributes(chn, fileno: cint32; attr: pcuint8): cint32; cdecl; external;

function CARD_SetAttributes(chn, fileno: cint32; attr: cuint8): cint32; cdecl; external;

function CARD_SetAttributesAsync(chn, fileno: cint32; attr: cuint8;
  callback: cardcallback): cint32; cdecl; external;

function CARD_Format(chn: cint32): cint32; cdecl; external;

function CARD_FormatAsync(chn: cint32; callback: cardcallback): cint32; cdecl; external;

function CARD_SetCompany(company: pcchar): cint32; cdecl; external;

function CARD_SetGamecode(gamecode: pcchar): cint32; cdecl; external;

{$endif OGC_INTERFACE}

{$IFDEF OGC_IMPLEMENTATION}

function CARD_GetBannerFmt(stat: card_stat): cuint8; inline;
begin
  CARD_GetBannerFmt := stat.banner_fmt and CARD_BANNER_MASK;
end;

procedure CARD_SetBannerFmt(stat: card_stat; fmt: cuint8); inline;
begin
  stat.banner_fmt := (stat.banner_fmt and not CARD_BANNER_MASK) or fmt;
end;

function CARD_GetIconFmt(stat: card_stat; n: cint): cuint16; inline;
begin
  CARD_GetIconFmt := (stat.icon_fmt shr (2 * n)) and CARD_ICON_MASK;
end;

procedure CARD_SetIconFmt(stat: card_stat; n: cint; fmt: cuint8); inline;
begin
  stat.icon_fmt := cuint16((stat.icon_fmt and not (CARD_ICON_MASK shl (2 * n))) or (fmt shl (2 * n)));
end;

function CARD_GetIconSpeed(stat: card_stat; n: cint):	cuint16; inline;
begin
  CARD_GetIconSpeed := (stat.icon_speed shr (2 * n)) and not CARD_SPEED_MASK;
end;

procedure CARD_SetIconSpeed(stat: card_stat; n: cint; speed: cuint16); inline;
begin
  stat.icon_speed := cuint16((stat.icon_fmt and not (CARD_SPEED_MASK shl (2 * n))) or ((speed) shl (2 * n)));
end;

procedure CARD_SetIconAddr(stat: card_stat; addr: cuint32); inline;
begin
  stat.icon_addr := addr;
end;

procedure CARD_SetCommentAddr(stat: card_stat; addr: cuint32); inline;
begin
  stat.comment_addr := addr;
end;

{$ENDIF OGC_IMPLEMENTATION}
