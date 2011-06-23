{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2002 by the Free Pascal development team

    libc unit for Nintendo DS
    Copyright (c) 2006 by Francesco Lombardi

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 *****************************************************************************}

type
  time_t = longint;
  ptime_t = ^time_t;

  Ptm = ^tm;
  tm = record
    tm_sec: longint;
    tm_min: longint;
    tm_hour: longint;
    tm_mday: longint;
    tm_mon: longint;
    tm_year: longint;
    tm_wday: longint;
    tm_yday: longint;
    tm_isdst: longint;
  end;


(* Some libc functions *)
//function printf(format: Pchar; args: array of const): longint; cdecl; external;
function printf(format: Pchar): longint; cdecl; varargs; external;
//function sprintf(s: Pchar; format: Pchar; args: array of const): longint; cdecl; external;
function sprintf(s: Pchar; format: Pchar): longint; varargs; cdecl; external;
//function iprintf(format: Pchar; args: array of const): longint; cdecl; external;
function iprintf(format: Pchar): longint; varargs; cdecl; external;
//function scanf(format: Pchar; args: array of const): longint; cdecl; external;
function scanf(format: Pchar): longint; cdecl; varargs; external;
//function sscanf(s: Pchar; format: Pchar; args: array of const): longint; cdecl; external;
function sscanf(s: Pchar; format: Pchar): longint; cdecl; varargs; external;
function strcmp(s1: Pchar; s2: Pchar): longint; cdecl; external;

function malloc(size: integer): pointer; cdecl; external;
function realloc(ptr: pointer; size: integer): pointer; cdecl; external;
procedure free(ptr: pointer); cdecl; external;
function memcpy(dest: pointer; src: pointer; n: integer): pointer; cdecl; external;

function gmtime(timer: ptime_t): ptm; cdecl; external;
function time(timer: ptime_t): time_t; cdecl; external;

type
  TSort = function (const a, b: pointer): integer;
procedure qsort(__base: pointer; __nmemb: integer; __size: integer; __compar: TSort); cdecl; external;

function __errno: plongint; cdecl; export;

type
  _FILE = record
    firstCluster: longword;
    length: longword;
    curPos: longword;
    curClus: longword;                       // Current cluster to read from
    curSect: integer;                     // Current sector within cluster
    curByte: integer;                     // Current byte within sector
    readBuffer: array [0..511] of byte;   // Buffer used for unaligned reads
    appClus: longword;                       // Cluster to append to
    appSect: integer;                     // Sector within cluster for appending
    appByte: integer;                     // Byte within sector for appending
    read: boolean;                        // Can read from file
    write: boolean;                       // Can write to file
    append: boolean;                      // Can append to file
    inUse: boolean;                       // This file is open
    dirEntSector: longword;                  // The sector where the directory entry is stored
    dirEntOffset: integer;                // The offset within the directory sector
  end;
  P_FILE = ^_FILE;

const
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;
   R_OK = 1;
   W_OK = 2;
   X_OK = 4;
   F_OK = 8;
   L_SET = SEEK_SET;
   L_INCR = SEEK_CUR;
   L_XTND = SEEK_END;
   EFF_ONLY_OK = 8;
   STDIN_FILENO = 0;
   STDOUT_FILENO = 1;
   STDERR_FILENO = 2;

(*
  ------------------------------------------------------------------------------
    Directory iterator for mantaining state between dir* calls
  ------------------------------------------------------------------------------
*)
type
  DIR_ITER = record
    device: longint;
    dirStruct: pointer;
  end;
  PDIR_ITER = ^DIR_ITER;

  stat = record
    st_dev: longint;
    st_ino: longword;
    st_mode : longword;
    st_nlink : word;
    st_uid : word;
    st_gid : word;
    st_rdev : longint;
    st_size : longint;
    st_atime : longint;

    st_spare1: longint;
    st_mtime: longint;
    st_spare2: longint;
    st_ctime: longint;
    st_spare3: longint;
    st_blksize: longint;
    st_blocks: longint;
    st_spare4: array [0..1] of longint;
  end;
  TStat = stat;
  PStat = ^stat;

const
  _IFMT    = 0170000;   // type of file
  _IFDIR   = 0040000;   // directory
  _IFCHR   = 0020000; 	// character special
  _IFBLK   = 0060000; 	// block special
  _IFREG   = 0100000; 	// regular
  _IFLNK   = 0120000; 	// symbolic link
  _IFSOCK  = 0140000; 	// socket
  _IFIFO   = 0010000; 	// fifo

  S_BLKSIZE = 1024;  // size of a block

  S_ISUID = 0004000; // set user id on execution
  S_ISGID = 0002000; // set group id on execution

  NAME_MAX = 767;

function S_ISBLK(m: longint): boolean; inline;
function S_ISCHR(m: longint): boolean; inline;
function S_ISDIR(m: longint): boolean; inline;
function S_ISFIFO(m: longint): boolean; inline;
function S_ISREG(m: longint): boolean; inline;
function S_ISLNK(m: longint): boolean; inline;
function S_ISSOCK(m: longint): boolean; inline;


type
  dirent = record
    d_ino: longint;
    d_name: array [0..NAME_MAX] of char;
  end;
  PDirent = ^dirent;
  PPDirent = ^PDirent;

  DIR = record
    position: longint;
    dirData: PDIR_ITER;
    fileData: dirent;
  end;
  PDIR = ^DIR;

(* DIR handling *)
function closedir(dirp: PDIR): longint; cdecl; external;
function opendir(const dirname: pchar): PDIR; cdecl; external;
function readdir(dirp: PDIR): PDirent; cdecl; external;
function readdir_r(dirp: PDIR; entry: PDirent; result: PPDirent): longint; cdecl; external;
procedure rewinddir(dirp: PDIR); cdecl; external;
procedure seekdir(dirp: PDIR; loc: longint); cdecl; external;
function telldir(dirp: PDIR): longint; cdecl; external;

function diropen(const path: pchar): PDIR_ITER; cdecl; external;
function dirreset(dirState: PDIR_ITER): longint; cdecl; external;
function dirnext(dirState: PDIR_ITER; filename: pchar; filestat: Pstat): longint; cdecl; external;
function dirclose(dirState: PDIR_ITER): longint; cdecl; external;

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

function FpOpen(path: Pchar; oflag: longint): longint; cdecl; external name 'open';
function FpOpen(path: Pchar; oflag, mode: longint): longint; cdecl; external name 'open';
function FpRead(fildes:longint; buf:pointer; nbytes:dword):longint;cdecl;external name 'read';
function FpRead(fildes:longint; var buf; nbytes:dword):longint;cdecl;external name 'read';
function FpWrite(fildes:longint; buf:pointer; nbytes:dword):longint;cdecl;external name 'write';
function FpWrite(fildes:longint; var buf; nbytes:dword):longint;cdecl;external name 'write';
function fplseek(fildes:longint; offset:longint; whence:longint):longint;cdecl;external name 'lseek';
function FpClose(fildes:longint):longint;cdecl;external name 'close';
function FpChsize(fildes:longint; size:dword):longint;cdecl;external name 'chsize';
function FpUnlink(path:Pchar):longint;cdecl;external name 'unlink';
function FpRename(para1: Pchar; para2: Pchar): longint; cdecl; external name 'rename';
function Fpstat(path:Pchar; buf:Pstat):longint;cdecl;external name 'stat';
function Fpstat(path:Pchar; var buf:Tstat):longint;cdecl;external name 'stat';
function FpAccess(path:Pchar; mode:longint):longint;cdecl;external name 'access';



const
 F_GETFL      = 1;        // get file status flags
 F_SETFL      = 2;        // set file status flags
 F_DUPFD      = 3;        // duplicate file descriptor
 F_GETFD      = 4;        // get file descriptor flags
 F_SETFD      = 5;        // set file descriptor flags
 F_SETLK      = 6;        // set record locking info
 F_SETLK64    = 16;       // set record locking info (64-bit)
 F_GETLK      = 7;        // get record locking info
 F_GETLK64    = 17;       // get record locking info (64-bit)
 F_SETLKW     = 8;        // get record locking info; wait if blocked
 F_SETLKW64   = 18;       // get record locking info (64-bit)
 F_CLOEXEC    = 9;        // close on execute

// values for 'l_type' field of 'struct flock'...
 F_RDLCK      = 1;        // shared or read lock
 F_WRLCK      = 2;        // exclusive or write lock
 F_UNLCK      = 3;        // unlock

// values for 'oflag' in open()...
 O_RDONLY     =$00000000; // open for read only
 O_WRONLY     =$00000001; // open for write only
 O_RDWR       =$00000002; // open for read and write
 O_ACCMODE    =$00000003; // access flags mask
 O_reserved1  =$00000004; // reserved
 O_reserved2  =$00000008; // reserved
 O_APPEND     =$00000010; // writes done at end of file
 O_CREAT      =$00000020; // create new file
 O_TRUNC      =$00000040; // truncate existing file
 O_EXCL       =$00000080; // exclusive open
 O_NOCTTY     =$00000100; // no controlling terminal--unsupported
 O_BINARY     =$00000200; // binary file--all files
 O_NDELAY     =$00000400; // nonblocking flag
 O_reserved3  =$00000800; // reserved
 O_SYNC       =$00001000; // synchronized I/O file integrity
 O_DSYNC      =$00002000; // synchronized I/O data integrity
 O_RSYNC      =$00004000; // synchronized read I/O
 O_NONBLOCK   = O_NDELAY; // alias
 FD_CLOEXEC   =$00008000; // parent closes after call to process()
 O_UPDATE     =$00010000; // keep legacy files updated
 O_FIFO       =$00100000; // opening one end of a FIFO [non-standard]

// value for third argument when 'cmd' is F_SETFL in fcntl()...
 FNDELAY      = O_NDELAY;   // fcntl() non-blocking I/O

// 'shflag' values for sopen()...
 SH_DENYRW    = $00000010; // deny read/write mode
 SH_DENYWR    = $00000020; // deny write mode
 SH_DENYRD    = $00000030; // deny read mode
 SH_DENYNO    = $00000040; // deny none mode
 
 