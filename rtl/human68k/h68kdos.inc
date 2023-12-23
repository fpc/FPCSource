{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Free Pascal development team

    DOS related defines for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  DOSE_ILGFNC = -1; { Invalid function code executed }
  DOSE_NOENT = -2; { File not found }
  DOSE_NODIR = -3; { Directory not found }
  DOSE_MFILE = -4; { Too many open files }
  DOSE_ISDIR = -5; { Directory and volume label inaccessible }
  DOSE_BADF = -6; { The specified handle is not open }
  DOSE_BROKNMEM = -7; { The memory management area was destroyed }
  DOSE_NOMEM = -8; { Not enough memory for execution }
  DOSE_ILGMPTR = -9; { Invalid memory management pointer specified }
  DOSE_ILGENV = -10; { Illegal environment specified }
  DOSE_ILGFMT = -11; { Abnormal executable file format }
  DOSE_ILGARG = -12; { Abnormal open access mode }
  DOSE_ILGFNAME = -13; { Invalid file name }
  DOSE_ILGPARM = -14; { Called with invalid parameter }
  DOSE_ILGDRV = -15; { Invalid drive specified }
  DOSE_ISCURDIR = -16; { Current directory can't be deleted }
  DOSE_CANTIOC = -17; { ioctrl can not be used }
  DOSE_NOMORE = -18; { No more files found }
  DOSE_RDONLY = -19; { The file can't be written }
  DOSE_EXISTDIR = -20; { The directory already exists }
  DOSE_NOTEMPTY = -21; { File can't be deleted }
  DOSE_CANTREN = -22; { File can't be renamed }
  DOSE_DISKFULL = -23; { File can't be created because disk is full }
  DOSE_DIRFULL = -24; { File can't be created because folder is full }
  DOSE_CANTSEEK = -25; { Can't seek to the specified position }
  DOSE_SUPER = -26; { Supervisor mode require while in supervisor mode }
  DOSE_DUPTHNAM = -27; { Thread name exists }
  DOSE_CANTSEND = -28; { IPC buffer is write protected }
  DOSE_THFULL = -29; { Can't start any more background processes }
  DOSE_LCKFULL = -32; { Insufficient lock space }
  DOSE_LCKERR = -33; { File is locked and can't be accessed }
  DOSE_BUSYDRV = -34; { The drive has a handler open }
  DOSE_SYMLOOP = -35; { Symbolic link nest exceeded 16 links(lndrv) }
  DOSE_EXISTFILE = -80; { File exists }


type
  Th68kdos_comline = record
    case boolean of
      true: ( len: byte; buffer: array[0..255] of char; );
      false: ( pstr: shortstring; )
  end;
  Ph68kdos_comline = ^Th68kdos_comline;

type
  Th68kdos_psp = record
    env: pchar;
    _exit: pointer;
    ctrlc: pointer;
    errexit: pointer;
    comline: Ph68kdos_comline;
    handle: array[0..11] of byte;
    bss: pointer;
    heap: pointer;
    stack: pointer;
    usp: pointer;
    ssp: pointer;
    sr: word;
    abort_sr: word;
    abort_ssp: pointer;
    trap10: pointer;
    trap11: pointer;
    trap12: pointer;
    trap13: pointer;
    trap14: pointer;
    osflg: dword;
    reserve_1: array[0..27] of byte;
    exe_path: array[0..67] of char;
    exe_name: array[0..23] of char;
    reserve_2: array[0..35] of byte;
  end;
  Ph68kdos_psp = ^Th68kdos_psp;

// register contents on startup, a0-a5 respectively
type
  Th68kdos_startup = record
    mcb: pbyte;
    bss_end: pbyte;
    comm: ph68kdos_comline;
    environ: pbyte;
    entry: pbyte;
    intr: pbyte;
  end;
  Ph68kdos_startup = ^Th68kdos_startup;

type
  Th68kdos_freeinfo = record
    free: word;
    max: word;
    sectors: word;
    bytes: word;
  end;
  Ph68kdos_freeinfo = ^Th68kdos_freeinfo;

type
  Th68kdos_filbuf = record
    searchatr: byte;
    driveno: byte;
    dirsec: dword;
    dirlft: word;
    dirpos: word;
    filename: array[0..7] of char;
    ext: array[0..2] of char;
    atr: byte;
    time: word;
    date: word;
    filelen: dword;
    name: array[0..22] of char;
  end;
  Ph68kdos_filbuf = ^Th68kdos_filbuf;

// as used by seek
const
    SEEK_FROM_START   = 0;
    SEEK_FROM_CURRENT = 1;
    SEEK_FROM_END     = 2;

procedure h68kdos_exit; noreturn; syscall $ff00;
function h68kdos_chgdrv(newdrv: word): longint; syscall $ff0e;
function h68kdos_curdrv: longint; syscall $ff17;
function h68kdos_gettim2: longint; syscall $ff27;
function h68kdos_settim2(time: longint): longint; syscall $ff28;
function h68kdos_getdate: longint; syscall $ff2a;
function h68kdos_setdate(date: word): longint; syscall $ff2b;
function h68kdos_gettime: longint; syscall $ff2c;
function h68kdos_settime(time: word): longint; syscall $ff2d;
function h68kdos_vernum: longint; syscall $ff30;
function h68kdos_dskfre(drive: word; buffer: Ph68kdos_freeinfo): longint; syscall $ff36;
function h68kdos_mkdir(name: pchar): longint; syscall $ff39;
function h68kdos_rmdir(name: pchar): longint; syscall $ff3a;
function h68kdos_chdir(name: pchar): longint; syscall $ff3b;
function h68kdos_create(name: pchar; attr: word): longint; syscall $ff3c;
function h68kdos_open(name: pchar; mode: word): longint; syscall $ff3d;
function h68kdos_close(fileno: word): longint; syscall $ff3e;
function h68kdos_read(fileno: word; buffer: pointer; len: longint): longint; syscall $ff3f;
function h68kdos_write(fileno: word; buffer: pointer; len: longint): longint; syscall $ff40;
function h68kdos_delete(name: pchar): longint; syscall $ff41;
function h68kdos_seek(fileno: word; offset: longint; mode: word): longint; syscall $ff42;
function h68kdos_chmod(name: pchar; atr: word):longint; syscall $ff43;
function h68kdos_curdir(driveno: word; buffer: pointer): longint; syscall $ff47;
function h68kdos_malloc(size: longint): pointer; syscall $ff48;
function h68kdos_mfree(ptr: pointer): longint; syscall $ff49;
function h68kdos_setblock(ptr: pointer; size: longint): longint; syscall $ff4a;
function h68kdos_exec(mode: word; fil: pchar; p1: pointer; p2: pointer): longint; syscall $ff4b;
procedure h68kdos_exit2(code: smallint); noreturn; syscall $ff4c;
function h68kdos_files(filbuf: Ph68kdos_filbuf; name: pchar; atr: word): longint; syscall $ff4e;
function h68kdos_nfiles(filbuf: Ph68kdos_filbuf): longint; syscall $ff4f;

{ * human68k v2 only calls * }
function h68kdos_rename_v2(oldname: PChar; newname: PChar): longint; syscall $ff56;
function h68kdos_filedate_v2(fileno: word; datetime: longint): longint; syscall $ff57;

{ * human68k v3 only calls * }
function h68kdos_rename_v3(oldname: PChar; newname: PChar): longint; syscall $ff86;
function h68kdos_filedate_v3(fileno: word; datetime: longint): longint; syscall $ff87;
