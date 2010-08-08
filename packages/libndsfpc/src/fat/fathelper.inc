{$ifdef NDS_INTERFACE}

(*
  ------------------------------------------------------------------------------
    libc file handling types and routines
  ------------------------------------------------------------------------------
*)

type
  _FILE = record
    firstCluster: cuint;
    length: cuint;
    curPos: cuint;
    curClus: cuint;                       // Current cluster to read from
    curSect: integer;                     // Current sector within cluster
    curByte: integer;                     // Current byte within sector
    readBuffer: array [0..511] of byte;   // Buffer used for unaligned reads
    appClus: cuint;                       // Cluster to append to
    appSect: integer;                     // Sector within cluster for appending
    appByte: integer;                     // Byte within sector for appending
    read: boolean;                        // Can read from file
    write: boolean;                       // Can write to file
    append: boolean;                      // Can append to file
    inUse: boolean;                       // This file is open
    dirEntSector: cuint;                  // The sector where the directory entry is stored
    dirEntOffset: integer;                // The offset within the directory sector
  end;
  P_FILE = ^_FILE;

const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

(*
  ------------------------------------------------------------------------------
    Directory iterator for mantaining state between dir* calls
  ------------------------------------------------------------------------------
*)
type
  DIR_ITER = record
    device: cint;
    dirStruct: pointer;
  end;
  PDIR_ITER = ^DIR_ITER;

  stat = packed record
    st_dev: cint;
    st_ino: cuint;
    st_mode : cuint;
    st_nlink : cushort;
    st_uid : cushort;
    st_gid : cushort;
    st_rdev : cint;
    st_size : clong;
    st_atime : clong;

    st_spare1: clong;
    st_mtime: clong;
    st_spare2: clong;
    st_ctime: clong;
    st_spare3: clong;
    st_blksize: clong;
    st_blocks: clong;
    st_spare4: array [0..1] of clong;
  end;
  TStat = stat;
  PStat = ^stat;

const
  S_IFMT        = $F000;
  S_IFDIR       = $4000;
  S_IFCHR       = $2000;
  S_IFBLK       = $6000;
  S_IFREG       = $8000;
  S_IFIFO       = $1000;
  S_IFLNK       = $A000;
  S_IFSOCK      = $C000;

  S_ISUID       = $800;
  S_ISGID       = $400;
  S_ISVTX       = $200;
  S_IREAD       = $100;
  S_IWRITE      = $80;
  S_IEXEC       = $40;
  NAME_MAX = 767;
  
type
  dirent = record
    d_ino: cint;
    d_name: array [0..NAME_MAX] of char;
  end;
  PDirent = ^dirent;
  PPDirent = ^PDirent;

  DIR = record
    position: cint32;
    dirData: PDIR_ITER;
    fileData: dirent;
  end;
  PDIR = ^DIR;

function diropen(const path: pchar): PDIR_ITER; cdecl; external;
function dirreset(dirState: PDIR_ITER): cint; cdecl; external;
function dirnext(dirState: PDIR_ITER; filename: pchar; filestat: Pstat): cint; cdecl; external;
function dirclose(dirState: PDIR_ITER): cint; cdecl; external;

(* File handling *)
function fopen(filename: Pchar; modes: Pchar): P_FILE; cdecl; external;
function fread(ptr: pointer; size: longint; n: longint; stream: P_FILE): longint; cdecl; external;
function fread(var ptr; size: longint; n: longint; var stream: _FILE): longint; cdecl; external;
function fwrite(ptr: pointer; size: longint; n: longint; s: P_FILE): longint; cdecl; external;
function fwrite(var ptr; size: longint; n: longint; var s: _FILE): longint; cdecl; external;
function ftell(stream: P_FILE): longint; cdecl; external;
function ftell(var stream: _FILE): longint; cdecl; external;
function fseek(stream: P_FILE; off: longint; whence: longint): longint; cdecl; external;
function fseek(var stream: _FILE; off: longint; whence: longint): longint; cdecl; external;
function fclose(stream: P_FILE): longint; cdecl; external;
function fclose(var stream: _FILE): longint; cdecl; external;
function isatty(fildes: longint): longint; cdecl; external;
function fileno(para1: P_FILE): longint; cdecl; external;
function fileno(var para1: _FILE): longint; cdecl; external;
function fstat(fildes: longint; buf: PStat): longint; cdecl; external;
function fstat(fildes: longint; var buf: TStat): longint; cdecl; external;
function _stat(__file:Pchar; var __buf:Tstat):longint; cdecl; external name 'stat';
function ftruncate(fildes: longint; len: longint): longint; cdecl; external;
function unlink(path: Pchar): longint; cdecl; external;
function rename(para1: Pchar; para2: Pchar): longint; cdecl; external;
{$endif NDS_INTERFACE}
