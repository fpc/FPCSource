{
  Netware Server Imports for FreePascal, contains definition from the
  following header files:

  string.h dirent.h errno.h fcntl.h limits.h locale.h nwaudnlm.h
  nwbitops.h nwcntask.h nwconio.h nwconn.h nwdebug.h nwdfs.h nwdos.h
  nwerrno.h nwfattr.h nwfileio.h nwfileng.h nwfinfo.h nwfshook.h
  nwipx.h nwlib.h nwlocale.h nwmalloc.h nwncpx.h nwnspace.h nwproc.h
  nwsemaph.h nwserv.h nwsignal.h nwstring.h nwtoolib.h stdio.h stdlib.h
  unistd.h time.h utime.h nwthread.h nwmediam.h ioctl.h
  sys/socket.h sys/time.h sys/filio.h syys/ioctl.h sys/stat.h
  sys/time.h sys/timeval.h sys/uio.h sys/utsname.h

  Initial Version 2002/02/22 Armin (diehl@nordrhein.de)

  The C-NDK and Documentation can be found here:
    http://developer.novell.com

  This program is distributed in the hope that it will be useful,but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.

  Do not blame Novell if there are errors in this file, instead
  contact me and i will se what i can do.

}

unit nwserv;

interface

{$mode objfpc}

const NULL = 0;
      ThreadsNlm = 'threads';
      Lib0Nlm    = 'lib0';
      NlmLibNlm  = 'nlmlib';
      FIONREAD   = 1;  // get count of bytes to read (readable)
      FIONBIO    = 2;  // set/clear nonblocking I/O
      FIOGETNBIO = 3;  // get nonblocking I/O status

type
   Psize_t = ^Tsize_t;
   Tsize_t = dword;
   PPChar = ^PChar;
   PPPChar= ^PPChar;
   Tsigset_t = longint;
   TNlmHandle = longint;

const
  NullNlmHandle = 0;

{-time.h-----------------------------------------------------------------------}
{$PACKRECORDS C}

const
   CLOCKS_PER_SEC = 100;

type
   Pclock_t = ^Tclock_t;
   Tclock_t = dword;

   Ptime_t = ^Ttime_t;
   Ttime_t = dword;
   Ptm = ^Ttm;
   Ttm = record
        tm_sec   : longint;    { seconds after the minute--range [0, 59]      }
        tm_min   : longint;    { minutes after the hour--range [0, 59]        }
        tm_hour  : longint;    { hours since midnight--range [0, 23]          }
        tm_mday  : longint;    { days of the month--range [1, 31]             }
        tm_mon   : longint;    { months since January--range [0, 11]          }
        tm_year  : longint;    { years since 1900--range [0, 99]              }
        tm_wday  : longint;    { days since Sunday--range [0, 6]              }
        tm_yday  : longint;    { days since first of January--range [0, 365]  }
        tm_isdst : longint;    { Daylight Savings Time flag--set [-, 0, +]:   }
     end;

{ ISO/ANSI C functions...  }
function asctime(para1:Ptm):Pchar;cdecl;external 'clib' name 'asctime';
function clock:Tclock_t;cdecl;external 'clib' name 'clock';
function ctime(para1:Ptime_t):Pchar;cdecl;external 'clib' name 'ctime';
function difftime(para1:Ttime_t; para2:Ttime_t):double;cdecl;external 'clib' name 'difftime';
function gmtime(para1:Ptime_t):Ptm;cdecl;external 'clib' name 'gmtime';
function localtime(para1:Ptime_t):Ptm;cdecl;external 'clib' name 'localtime';
function mktime(para1:Ptm):Ttime_t;cdecl;external 'clib' name 'mktime';
function strftime(para1:Pchar; para2:Tsize_t; para3:Pchar; para4:Ptm):Tsize_t;cdecl;external 'clib' name 'strftime';
function time(para1:Ptime_t):Ttime_t;cdecl;external 'clib' name 'time';
{ POSIX data and functions...  }
{ For extern char tzname[2], see macro below  }
procedure tzset;cdecl;external 'clib' name 'tzset';
function __get_CLK_TCK:Tclock_t;cdecl;external 'clib' name '__get_CLK_TCK';
function __get_tzname:pPchar;cdecl;external 'clib' name '__get_tzname';
{ POSIX-defined additions ...  }
function asctime_r(para1:Ptm; para2:Pchar):Pchar;cdecl;external 'clib' name 'asctime_r';
function ctime_r(para1:Ptime_t; para2:Pchar):Pchar;cdecl;external 'clib' name 'ctime_r';
function gmtime_r(para1:Ptime_t; para2:Ptm):Ptm;cdecl;external 'clib' name 'gmtime_r';
function localtime_r(para1:Ptime_t; para2:Ptm):Ptm;cdecl;external 'clib' name 'localtime_r';
function CLK_TCK : longint;
function tzname : pchar;

{-utime.h----------------------------------------------------------------------}
type
   Putimbuf = ^Tutimbuf;
   Tutimbuf = record
      actime  : Ttime_t;   // access time
      modtime : Ttime_t;   // modification time
   end;


function utime(path:Pchar; times:Putimbuf):longint;cdecl;external 'clib' name 'utime';
function utime(path:Pchar; var times:Tutimbuf):longint;cdecl;external 'clib' name 'utime';

{-string.h---------------------------------------------------------------------}
function memchr(para1:pointer; para2:longint; para3:Tsize_t):pointer;cdecl;external 'clib' name 'memchr';
function memcmp(para1:pointer; para2:pointer; para3:Tsize_t):longint;cdecl;external 'clib' name 'memcmp';
function memcpy(para1:pointer; para2:pointer; para3:Tsize_t):pointer;cdecl;external 'clib' name 'memcpy';
function memmove(para1:pointer; para2:pointer; para3:Tsize_t):pointer;cdecl;external 'clib' name 'memmove';
function memset(para1:pointer; para2:longint; para3:Tsize_t):pointer;cdecl;external 'clib' name 'memset';
function strcpy(para1:Pchar; para2:Pchar):Pchar;cdecl;external 'clib' name 'strcpy';
function strcat(para1:Pchar; para2:Pchar):Pchar;cdecl;external 'clib' name 'strcat';
function strchr(para1:Pchar; para2:longint):Pchar;cdecl;external 'clib' name 'strchr';
function strcmp(para1:Pchar; para2:Pchar):longint;cdecl;external 'clib' name 'strcmp';
function strcoll(para1:Pchar; para2:Pchar):longint;cdecl;external 'clib' name 'strcoll';
function strcspn(para1:Pchar; para2:Pchar):Tsize_t;cdecl;external 'clib' name 'strcspn';
function strerror(para1:longint):Pchar;cdecl;external 'clib' name 'strerror';
function strlen(para1:Pchar):Tsize_t;cdecl;external 'clib' name 'strlen';
function strncat(para1:Pchar; para2:Pchar; para3:Tsize_t):Pchar;cdecl;external 'clib' name 'strncat';
function strncmp(para1:Pchar; para2:Pchar; para3:Tsize_t):longint;cdecl;external 'clib' name 'strncmp';
function strncpy(para1:Pchar; para2:Pchar; para3:Tsize_t):Pchar;cdecl;external 'clib' name 'strncpy';
function strpbrk(para1:Pchar; para2:Pchar):Pchar;cdecl;external 'clib' name 'strpbrk';
function strrchr(para1:Pchar; para2:longint):Pchar;cdecl;external 'clib' name 'strrchr';
function strspn(para1:Pchar; para2:Pchar):Tsize_t;cdecl;external 'clib' name 'strspn';
function strstr(para1:Pchar; para2:Pchar):Pchar;cdecl;external 'clib' name 'strstr';
function strtok(para1:Pchar; para2:Pchar):Pchar;cdecl;external 'clib' name 'strtok';
function strxfrm(para1:Pchar; para2:Pchar; para3:Tsize_t):Tsize_t;cdecl;external 'clib' name 'strxfrm';
function strtok_r(para1:Pchar; para2:Pchar; para3:PPchar):Pchar;cdecl;external 'clib' name 'strtok_r';
function memicmp(para1:pointer; para2:pointer; para3:Tsize_t):longint;cdecl;external 'clib' name 'memicmp';
function strcmpi(para1:Pchar; para2:Pchar):longint;cdecl;external 'clib' name 'strcmpi';
function stricmp(para1:Pchar; para2:Pchar):longint;cdecl;external 'clib' name 'stricmp';
function strdup(para1:Pchar):Pchar;cdecl;external 'clib' name 'strdup';
function strlist(para1,para2:Pchar; args:array of const):Pchar;cdecl;external 'clib' name 'strlist';
function strlist(para1,para2:Pchar):Pchar;cdecl;external 'clib' name 'strlist';
function strlwr(para1:Pchar):Pchar;cdecl;external 'clib' name 'strlwr';
function strnicmp(para1,para2:Pchar; para3:Tsize_t):longint;cdecl;external 'clib' name 'strnicmp';
function strnset(para1:Pchar; para2:longint; para3:Tsize_t):Pchar;cdecl;external 'clib' name 'strnset';
function strrev(para1:Pchar):Pchar;cdecl;external 'clib' name 'strrev';
function strset(para1:Pchar; para2:longint):Pchar;cdecl;external 'clib' name 'strset';
function strupr(para1:Pchar):Pchar;cdecl;external 'clib' name 'strupr';
procedure swab(para1:pointer; para2:pointer; para3:Tsize_t);cdecl;external 'clib' name 'swab';
procedure swaw(para1:pointer; para2:pointer; para3:Tsize_t);cdecl;external 'clib' name 'swaw';
{-dirent.h---------------------------------------------------------------------}
{$I npackon.inc}
type
   Pino_t = ^Tino_t;
   Tino_t = longint;
   Pdev_t = ^Tdev_t;
   Tdev_t = longint;
   Pdirent = ^Tdirent;
   Tdirent =
   record
     d_attr                 : dword;
     d_time                 : word;
     d_date                 : word;
     d_size                 : longint;
     d_ino                  : Tino_t;
     d_dev                  : Tdev_t;
     d_cdatetime            : dword;
     d_adatetime            : dword;
     d_bdatetime            : dword;
     d_uid                  : longint;
     d_archivedID           : dword;
     d_updatedID            : dword;
     d_nameDOS              : array[0..12] of char;
     d_inheritedRightsMask  : word;
     d_originatingNameSpace : byte;
     d_ddatetime            : dword;
     d_deletedID            : dword;
     d_name                 : array[0..255] of char;
   end;
   TDIR = Tdirent;
   PDIR = ^TDIR;
{$I npackoff.inc}

function closedir_old (dirp:PDIR):longint;                            cdecl; external 'clib' name 'closedir';
function closedir     (dirp:PDIR):longint;                            cdecl; external 'clib' name 'closedir_510';
function opendir_old  (pathName:Pchar):PDIR;                          cdecl; external 'clib' name 'opendir';
function opendir      (pathName:Pchar):PDIR;                          cdecl; external 'clib' name 'opendir_411';
function readdir_old  (dirp:PDIR):PDIR;                               cdecl; external 'clib' name 'readdir';
function readdir      (dirp:PDIR):PDIR;                               cdecl; external 'clib' name 'readdir_411';
procedure rewinddir   (dirp:PDIR);                                    cdecl; external 'clib' name 'rewinddir';
function SetReaddirAttribute(dirp:PDIR; newAttribute:dword):longint;  cdecl; external 'clib' name 'SetReaddirAttribute';
{-errno.h----------------------------------------------------------------------}
const
   ENOENT   = 1;
   E2BIG    = 2;
   ENOEXEC  = 3;
   EBADF    = 4;
   ENOMEM   = 5;
   EACCES   = 6;
   EEXIST   = 7;
   EXDEV    = 8;
   EINVAL   = 9;
   ENFILE   = 10;
   EMFILE   = 11;
   ENOSPC   = 12;
   EDOM     = 13;
   ERANGE   = 14;
   EDEADLK  = 15;
   EINUSE   = 16;
   ESERVER  = 17;
   ENOSERVR = 18;
   EWRNGKND = 19;
   ETRNREST = 20;
   ERESOURCE= 21;
   EBADHNDL = 22;
   ENO_SCRNS= 23;
   EAGAIN   = 24;
   ENXIO    = 25;
   EBADMSG  = 26;
   EFAULT   = 27;
   EIO      = 28;
   ENODATA  = 29;
   ENOSTRMS = 30;
   EPROTO   = 31;
   EPIPE    = 32;
   ESPIPE   = 33;
   ETIME    = 34;
   EWOULDBLOCK = 35;
   EINPROGRESS = 36;
   EALREADY = 37;
   ENOTSOCK = 38;
   EDESTADDRREQ = 39;
   EMSGSIZE        = 40;
   EPROTOTYPE      = 41;
   ENOPROTOOPT     = 42;
   EPROTONOSUPPORT = 43;
   ESOCKTNOSUPPORT = 44;
   EOPNOTSUPP = 45;
   EPFNOSUPPORT = 46;
   EAFNOSUPPORT = 47;
   EADDRINUSE = 48;
   EADDRNOTAVAIL = 49;
   ENETDOWN = 50;
   ENETUNREACH = 51;
   ENETRESET = 52;
   ECONNABORTED = 53;
   ECONNRESET = 54;
   ENOBUFS = 55;
   EISCONN = 56;
   ENOTCONN = 57;
   ESHUTDOWN = 58;
   ETOOMANYREFS = 59;
   ETIMEDOUT = 60;
   ECONNREFUSED = 61;

   EBUSY = 62;
   EINTR = 63;
   EISDIR = 64;
   ENAMETOOLONG = 65;
   ENOSYS = 66;
   ENOTDIR = 67;
   ENOTEMPTY = 68;
   EPERM = 69;
   ECHILD = 70;
   EFBIG = 71;
   EMLINK = 72;
   ENODEV = 73;
   ENOLCK = 74;
   ENOTTY = 75;
   EFTYPE = ENOTTY;
   EROFS = 76;
   ESRCH = 77;
   ECANCELED = 78;
   ENOTSUP = 79;

   // CLib-implementation-specific constants
   ECANCELLED = ECANCELED;
   ENLMDATA = 100;
   EILSEQ = 101;
   EINCONSIS = 102;
   EDOSTEXTEOL = 103;
   ENONEXTANT = 104;
   ENOCONTEXT = 105;
   ELASTERR = ENOCONTEXT;
{-nwerrno.h--------------------------------------------------------------------}
{ Multi purpose return values.}
const
   ESUCCESS = 0;
   EFAILURE = -(1);
   ERR_TTS_NOT_AVAILABLE = $00;
   ERR_RECORD_NOT_LOCKED = $01;
   ERR_INSUFFICIENT_SPACE = $01;
   ERR_STRING_EXCEEDS_LENGTH = $01;
   ERR_TTS_AVAILABLE = $01;
   ERR_NOT_AVAILABLE_PROTECTED = $64;
   ERR_NOT_AVAILABLE_ON_3X = $65;
   ERR_BAD_THREAD_ID = $66;
   ERR_BAD_PRTY_CLASS = $67;
   ERR_BAD_PRTY_SCOPE = $68;
   ERR_NOT_A_POPUP_SCREEN = $69;
   ERR_OPEN_SCREEN = $6A;
   ERR_BAD_SHFLAG = $6B;
   ERR_BAD_ACCESS = $6C;
   ERR_BAD_ORIGIN = $6D;
   ERR_BAD_ACTION_CODE = $6E;
   ERR_OUT_OF_TASKS = $6F;
   ERR_BAD_QUERY_TYPE = $70;
   ERR_BAD_LIBRARY_HANDLE = $71;
   ERR_STREAMS = $72;
   ERR_BAD_FILE_SERVER_ID = $73;
   ERR_BAD_CONNECTION_ID = $73;
   ERR_BAD_FLAGS = $74;
   ERR_STRUCT_NOT_FOUND = $C8;
   ERR_NO_ITEMS_FOUND = $79;
   ERR_NCPEXT_TRANSPORT_PROTOCOL_VIOLATION = $7E;
   ERR_FILE_IN_USE = $80;
   ERR_LOCK_FAIL = $80;
   ERR_MAPPED_TO_A_LOCAL_DRIVE = $80;
   ERR_NO_MORE_FILE_HANDLES = $81;
   ERR_NO_OPEN_PRIVILEGE = $82;
   ERR_NETWORK_DISK_IO = $83;
   ERR_NO_CREATE_PRIVILEGE = $84;
   ERR_NO_CREATE_DELETE_PRIVILEGE = $85;
   ERR_R_O_CREATE_FILE = $86;
   ERR_CREATE_FILE_INVALID_NAME = $87;
   ERR_INVALID_FILE_HANDLE = $88;
   ERR_NO_SEARCH_PRIVILEGE = $89;
   ERR_NO_DELETE_PRIVILEGE = $8A;
   ERR_NO_RENAME_PRIVILEGE = $8B;
   ERR_NO_MODIFY_PRIVILEGE = $8C;
   ERR_NO_SET_PRIVILEGE = $8C;
   ERR_SOME_FILES_IN_USE = $8D;
   ERR_ALL_FILES_IN_USE = $8E;
   ERR_SOME_READ_ONLY = $8F;
   ERR_ALL_READ_ONLY = $90;
   ERR_SOME_NAMES_EXIST = $91;
   ERR_ALL_NAMES_EXIST = $92;
   ERR_NO_READ_PRIVILEGE = $93;
   ERR_NO_WRITE_PRIVILEGE_OR_READONLY = $94;
   ERR_FILE_DETACHED = $95;
   ERR_NO_ALLOC_SPACE = $96;
   ERR_SERVER_OUT_OF_MEMORY = $96;
   ERR_TARGET_NOT_A_SUBDIRECTORY = $96;
   ERR_NO_SPOOL_SPACE = $97;
   ERR_INVALID_VOLUME = $98;
   ERR_VOLUME_DOES_NOT_EXIST = $98;
   ERR_DIRECTORY_FULL = $99;
   ERR_RENAME_ACROSS_VOLUME = $9A;
   ERR_BAD_DIR_HANDLE = $9B;
   ERR_HOLE_FOUND = $9C;
   ERR_INVALID_PATH = $9C;
   ERR_NO_SUCH_EXTENSION = $9C;
   ERR_NO_DIR_HANDLES = $9D;
   ERR_BAD_FILE_NAME = $9E;
   ERR_DIRECTORY_ACTIVE = $9F;
   ERR_DIRECTORY_IN_USE = $9F;
   ERR_DIRECTORY_NOT_EMPTY = $A0;
   ERR_DIRECTORY_IO_ERROR = $A1;
   ERR_IO_LOCKED = $A2;
   ERR_TRANSACTION_RESTARTED = $A3;
   ERR_RENAME_DIR_INVALID = $A4;
   ERR_INVALID_OPENCREATE_MODE = $A5;
   ERR_ALREADY_IN_USE = $A6;
   ERR_SEARCH_DRIVE_VECTOR_FULL = $B0;
   ERR_DRIVE_DOES_NOT_EXIST = $B1;
   ERR_DRIVE_IS_NOT_MAPPED = $B1;
   ERR_CANT_MAP_LOCAL_DRIVE = $B2;
   ERR_INVALID_MAP_TYPE = $B3;
   ERR_INVALID_DRIVE_LETTER = $B4;
   ERR_NO_DRIVE_AVAILABLE = $B5;
   ERR_WORKSTATION_OUT_OF_MEMORY = $B6;
   ERR_NO_SUCH_SEARCH_DRIVE = $B7;
   ERR_INVALID_ENVIRON_VARIABLE = $B8;
   ERR_DOES_NOT_RUN_ON_IOENGINE = $B9;
   ERR_PACKET_SIGNATURES_REQURIED = $BC;
   ERR_PACKET_SIGNATURES_REQUIRED = $BC;
   ERR_INVALID_DATA_STREAM = $BE;
   ERR_INVALID_NAME_SPACE = $BF;
   ERR_NO_ACCOUNT_PRIVILEGES = $C0;
   ERR_NO_ACCOUNTING_PRIVILEGES = $C0;
   ERR_NO_ACCOUNT_BALANCE = $C1;
   ERR_CREDIT_LIMIT_EXCEEDED = $C2;
   ERR_LOGIN_DENIED_NO_CREDIT = $C2;
   ERR_TOO_MANY_HOLDS = $C3;
   ERR_ACCOUNTING_DISABLED = $C4;
   ERR_LOGIN_LOCKOUT = $C5;
   ERR_NO_CONSOLE_OPERATOR_RIGHTS = $C6;
   ERR_MISSING_EA_KEY = $C8;
   ERR_EA_NOT_FOUND = $C9;
   ERR_INVALID_EA_HANDLE_TYPE = $CA;
   ERR_EA_NO_KEY_NO_DATA = $CB;
   ERR_EA_NUMBER_MISMATCH = $CC;
   ERR_EXTENT_NUMBER_OUT_OF_RANGE = $CD;
   ERR_EA_BAD_DIR_NUM = $CE;
   ERR_INVALID_EA_HANDLE = $CF;
   ERR_EA_POSITION_OUT_OF_RANGE = $D0;
   ERR_Q_IO_FAILURE = $D0;
   ERR_EA_ACCESS_DENIED = $D1;
   ERR_NO_QUEUE = $D1;
   ERR_DATA_PAGE_ODD_SIZE = $D2;
   ERR_NO_Q_SERVER = $D2;
   ERR_EA_VOLUME_NOT_MOUNTED = $D3;
   ERR_NO_Q_RIGHTS = $D3;
   ERR_BAD_PAGE_BOUNDARY = $D4;
   ERR_Q_FULL = $D4;
   ERR_INSPECT_FAILURE = $D5;
   ERR_NO_Q_JOB = $D5;
   ERR_EA_ALREADY_CLAIMED = $D6;
   ERR_NO_Q_JOB_RIGHTS = $D6;
   ERR_UNENCRYPTED_NOT_ALLOWED = $D6;
   ERR_ODD_BUFFER_SIZE = $D7;
   ERR_DUPLICATE_PASSWORD = $D7;
   ERR_Q_IN_SERVICE = $D7;
   ERR_NO_SCORECARDS = $D8;
   ERR_PASSWORD_TOO_SHORT = $D8;
   ERR_Q_NOT_ACTIVE = $D8;
   ERR_BAD_EDS_SIGNATURE = $D9;
   ERR_MAXIMUM_LOGINS_EXCEEDED = $D9;
   ERR_LOGIN_DENIED_NO_CONNECTION = $D9;
   ERR_Q_STN_NOT_SERVER = $D9;
   ERR_EA_SPACE_LIMIT = $DA;
   ERR_BAD_LOGIN_TIME = $DA;
   ERR_Q_HALTED = $DA;
   ERR_EA_KEY_CORRUPT = $DB;
   ERR_NODE_ADDRESS_VIOLATION = $DB;
   ERR_Q_MAX_SERVERS = $DB;
   ERR_EA_KEY_LIMIT = $DC;
   ERR_LOG_ACCOUNT_EXPIRED = $DC;
   ERR_TALLY_CORRUPT = $DD;
   ERR_BAD_PASSWORD = $DE;
   ERR_PASSWORD_EXPIRED_NO_GRACE = $DE;
   ERR_PASSWORD_EXPIRED = $DF;
   ERR_NOT_ITEM_PROPERTY = $E8;
   ERR_WRITE_TO_GROUP_PROPERTY = $E8;
   ERR_MEMBER_ALREADY_EXISTS = $E9;
   ERR_NO_SUCH_MEMBER = $EA;
   ERR_PROPERTY_NOT_GROUP = $EB;
   ERR_NOT_GROUP_PROPERTY = $EB;
   ERR_NO_SUCH_SEGMENT = $EC;
   ERR_NO_SUCH_VALUE_SET = $EC;
   ERR_SPX_CONNECTION_TERMINATED = $EC;
   ERR_TERMINATED_BY_REMOTE_PARTNER = $EC;
   ERR_PROPERTY_ALREADY_EXISTS = $ED;
   ERR_SPX_CONNECTION_FAILED = $ED;
   ERR_SPX_TERMINATED_POORLY = $ED;
   ERR_SPX_NO_ANSWER_FROM_TARGET = $ED;
   ERR_OBJECT_ALREADY_EXISTS = $EE;
   ERR_SPX_INVALID_CONNECTION = $EE;
   ERR_INVALID_NAME = $EF;
   ERR_SPX_CONNECTION_TABLE_FULL = $EF;
   ERR_IPX_NOT_INSTALLED = $F0;
   ERR_ILLEGAL_WILDCARD = $F0;
   ERR_WILDCARD_NOT_ALLOWED = $F0;
   ERR_SOCKET_NOT_OPEN = $F0;
   ERR_BINDERY_SECURITY = $F1;
   ERR_INVALID_BINDERY_SECURITY = $F1;
   ERR_SOCKET_ALREADY_OPEN = $F1;
   ERR_NO_OBJECT_READ_PRIVILEGE = $F2;
   ERR_NO_OBJECT_READ_RIGHTS = $F2;
   ERR_NO_OBJECT_RENAME_PRIVILEGE = $F3;
   ERR_NO_OBJECT_RENAME_RIGHTS = $F3;
   ERR_NO_OBJECT_DELETE_PRIVILEGE = $F4;
   ERR_NO_OBJECT_DELETE_RIGHTS = $F4;
   ERR_NO_OBJECT_CREATE_PRIVILEGE = $F5;
   ERR_NO_OBJECT_CREATE_RIGHTS = $F5;
   ERR_NO_PROPERTY_DELETE_PRIVILEGE = $F6;
   ERR_NO_PROPERTY_DELETE_RIGHTS = $F6;
   ERR_NO_PROPERTY_CREATE_PRIVILEGE = $F7;
   ERR_NO_PROPERTY_CREATE_RIGHTS = $F7;
   ERR_ALREADY_ATTACHED_TO_SERVER = $F8;
   ERR_NO_PROPERTY_WRITE_PRIVILEGE = $F8;
   ERR_NO_PROPERTY_WRITE_RIGHTS = $F8;
   ERR_NOT_ATTACHED_TO_SERVER = $F8;
   ERR_ECB_CANNOT_BE_CANCELLED = $F9;
   ERR_NO_FREE_CONNECTION_SLOTS = $F9;
   ERR_NO_PROPERTY_READ_PRIVILEGE = $F9;
   ERR_NO_PROPERTY_READ_RIGHTS = $F9;
   ERR_NO_LOCAL_TARGET_IDENTIFIED = $FA;
   ERR_NO_MORE_SERVER_SLOTS = $FA;
   ERR_TEMP_REMAP = $FA;
   ERR_NO_KNOWN_ROUTE_TO_DESTINATION = $FA;
   ERR_INVALID_PARAMETERS = $FB;
   ERR_NO_SUCH_PROPERTY = $FB;
   ERR_UNKNOWN_REQUEST = $FB;
   ERR_EVENT_CANCELLED = $FC;
   ERR_INTERNET_PACKET_REQT_CANCELED = $FC;
   ERR_MESSAGE_QUEUE_FULL = $FC;
   ERR_NO_SUCH_BINDERY_OBJECT = $FC;
   ERR_NO_SUCH_OBJECT = $FC;
   ERR_REQUEST_CANCELLED = $FC;
   ERR_SPX_COMMAND_CANCELLED = $FC;
   ERR_SPX_SOCKET_CLOSED = $FC;
   ERR_UNKNOWN_FILE_SERVER = $FC;
   ERR_TARGET_ALREADY_HAS_MESSAGE = $FC;
   ERR_NCPEXT_SERVICE_PROTOCOL_VIOLATION = $FC;
   ERR_BAD_SERIAL_NUMBER = $FD;
   ERR_INVALID_PACKET_LENGTH = $FD;
   ERR_PACKET_OVERFLOW = $FD;
   ERR_TTS_DISABLED = $FD;
   ERR_FIELD_ALREADY_LOCKED = $FD;
   ERR_FSCOPY_DIFFERENT_NETWORKS = $FD;
   ERR_BAD_STATION_NUMBER = $FD;
   ERR_BAD_PACKET = $FE;
   ERR_SPX_MALFORMED_PACKET = $FE;
   ERR_BINDERY_LOCKED = $FE;
   ERR_DOS_ACCESS_DENIED = $FE;
   ERR_DOS_NO_SEARCH_RIGHTS = $FE;
   ERR_IMPLICIT_TRANSACTION_ACTIVE = $FE;
   ERR_INCORRECT_ACCESS_PRIVILEGES = $FE;
   ERR_INVALID_NAME_LENGTH = $FE;
   ERR_INVALID_SEMAPHORE_NAME_LENGTH = $FE;
   ERR_IO_FAILURE = $FE;
   ERR_PACKET_NOT_DELIVERABLE = $FE;
   ERR_SPOOL_DIRECTORY_ERROR = $FE;
   ERR_SUPERVISOR_HAS_DISABLED_LOGIN = $FE;
   ERR_TRANSACTION_ENDS_RECORDS_LOCKED = $FE;
   ERR_SERVER_BINDERY_LOCKED = $FE;
   ERR_TIMEOUT_FAILURE = $FE;
   ERR_TRUSTEE_NOT_FOUND = $FE;
   ERR_SOCKET_TABLE_FULL = $FE;
   ERR_NCPEXT_NO_HANDLER = $FE;
   ERR_BAD_PARAMETER = $FF;
   ERR_BAD_SPOOL_PRINTER = $FF;
   ERR_RECORD_ALREADY_LOCKED = $FF;
   ERR_BAD_RECORD_OFFSET = $FF;
   ERR_BINDERY_FAILURE = $FF;
   ERR_ECB_NOT_IN_USE = $FF;
   ERR_FAILURE = $FF;
   ERR_FILE_EXTENSION_ERROR = $FF;
   ERR_HARD_FAILURE = $FF;
   ERR_INVALID_INITIAL_SEMAPHORE_VALUE = $FF;
   ERR_INVALID_SEMAPHORE_HANDLE = $FF;
   ERR_DOS_FILE_NOT_FOUND = $FF;
   ERR_EXPLICIT_TRANSACTION_ACTIVE = $FF;
   ERR_FILE_NOT_OPEN = $FF;
   ERR_NO_EXPLICIT_TRANSACTION_ACTIVE = $FF;
   ERR_NO_FILES_FOUND = $FF;
   ERR_NO_RECORD_FOUND = $FF;
   ERR_NO_RESPONSE_FROM_SERVER = $FF;
   ERR_NO_SPOOL_FILE = $FF;
   ERR_NO_SUCH_OBJECT_OR_BAD_PASSWORD = $FF;
   ERR_OPEN_FILES = $FF;
   ERR_PATH_ALREADY_EXISTS = $FF;
   ERR_PATH_NOT_LOCATABLE = $FF;
   ERR_QUEUE_FULL = $FF;
   ERR_REQUEST_NOT_OUTSTANDING = $FF;
   ERR_SOCKET_CLOSED = $FF;
   ERR_SPX_IS_INSTALLED = $FF;
   ERR_SPX_SOCKET_NOT_OPENED = $FF;
   ERR_TARGET_NOT_LOGGED_IN = $FF;
   ERR_TARGET_NOT_ACCEPTING_MESSAGES = $FF;
   ERR_TRANSACTION_NOT_YET_WRITTEN = $FF;
   ERR_NO_TRUSTEE_CHANGE_PRIVILEGE = $FF;
   ERR_CHECKSUMS_REQUIRED = $FF;
   ERR_SERVICE_NOT_LOADED = $101;
   ERR_NO_LIBRARY_CONTEXT = $400;
{-----------------------------------------------------------------------------
   Important Note:
   Additional NetWareErrno values that don't employ a ERR_ prefix have been
   moved from this position into obsolete header niterror.h. Many of these had
   been included for compatibility with the now-obsolete NIT API for DOS
   clients and many conflict with current cross-platform headers.
  -----------------------------------------------------------------------------}
{ NetWare Core Protocol (NCP) error codes. }
   DISKFULL = 1;
   BADNET = 2;
   LISTENERROR = 2;
   BADLADDRESS = 3;
   INVALIDSESSION = 3;
   NOSLOTS = 4;
   SLOTALLOCERR = 4;
   BROADCASTERROR = 5;
   BADSERVERNAME = 6;
   BADUSERNAME = 7;
   BADPASSWORD = 8;
   MEMERROR = 9;
   INVALIDCONNECTION = 10;
   INVALIDHANDLE = 11;
   INVALIDREQUEST = 12;
   SOCKETERROR = 13;
   ALLOCTAGERR = 14;
   CONNECTIONABORTED = 15;
   TIMEOUTERR = 16;
{ frame type: Ethernet 802.3  }
   CHECKSUMS_NOT_SUPPORTED = 17;
   CHECKSUM_FAILURE = 18;
   NO_FRAGMENT_LIST = 19;
{ Values for 'NetWareErrno' as set by spawnlp() and spawnvp().}
   LOAD_COULD_NOT_FIND_FILE = 1;
   LOAD_ERROR_READING_FILE = 2;
   LOAD_NOT_NLM_FILE_FORMAT = 3;
   LOAD_WRONG_NLM_FILE_VERSION = 4;
   LOAD_REENTRANT_INITIALIZE_FAILURE = 5;
   LOAD_CAN_NOT_LOAD_MULTIPLE_COPIES = 6;
   LOAD_ALREADY_IN_PROGRESS = 7;
   LOAD_NOT_ENOUGH_MEMORY = 8;
   LOAD_INITIALIZE_FAILURE = 9;
   LOAD_INCONSISTENT_FILE_FORMAT = 10;
   LOAD_CAN_NOT_LOAD_AT_STARTUP = 11;
   LOAD_AUTO_LOAD_MODULES_NOT_LOADED = 12;
   LOAD_UNRESOLVED_EXTERNAL = 13;
   LOAD_PUBLIC_ALREADY_DEFINED = 14;
 { Values for _msize() error return and NWMemorySizeAddressable(). }
   ERR_HEAP_BAD_PTR = $FFFFFFFF;
   ERR_HEAP_BLOCK_ALREADY_FREE = $FFFFFFFE;
   ERR_INVALID_ADDRESS = $FFFFFFFD;
 { Values for NetWare Virtual Memory (NVM) APIs as returned by GetVMErrno().
   These values should be examined after calling a Win32 VM API without a
   satisfactorily-lucid error in 'errno' or from (Win32) GetLastError(). }
   ERROR_INSUFFICIENT_CONTIGUOUS_MEMORY = $1000;
   ERROR_INSUFFICIENT_DISK_SWAP_SPACE = $1001;
   ERROR_INSUFFICIENT_MEMORY = $1002;
   ERROR_INSUFFICIENT_RESOURCES_TO_COMMIT_MEMORY = $1003;
   ERROR_INVALID_ATTRIBUTE_FLAGS = $1004;
   ERROR_INVALID_ADDRESS = $1005;
   ERROR_INVALID_LOCK_FLAGS = $1006;
   ERROR_INVALID_PAGE_COUNT = $1007;
   ERROR_INVALID_PROTECTION_FLAGS = $1008;
   ERROR_NON_SHARED_MEMORY_ADDRESS = $1009;
   ERROR_SHARED_MEMORY_ADDRESS = $100A;

function GetVMErrno:longint;cdecl;external 'clib' name 'GetVMErrno';
procedure SetVMErrno(para1:longint);cdecl;external 'clib' name 'SetVMErrno';
function __get_NWErrno:longint;cdecl;external 'clib' name '__get_NWErrno';
function __get_NWErrno_ptr:Plongint;cdecl;external 'clib' name '__get_NWErrno_ptr';
function NetWareErrno : longint;
{-fcntl.h----------------------------------------------------------------------}
const
   F_GETFL    = 1;
   F_SETFL    = 2;
   F_DUPFD    = 3;
   F_GETFD    = 4;
   F_SETFD    = 5;
   F_SETLK    = 6;
   F_GETLK    = 7;
   F_SETLKW   = 8;
   F_RDLCK    = 9;
   F_UNLCK    = 10;
   F_WRLCK    = 11;
   F_CLOEXEC  = 12;
   O_RDONLY   = $0000;
   O_WRONLY   = $0001;
   O_RDWR     = $0002;
   O_ACCMODE  = $0003;
   O_APPEND   = $0010;
   O_CREAT    = $0020;
   O_TRUNC    = $0040;
   O_EXCL     = $0080;
   O_TEXT     = $0100;
   O_BINARY   = $0200;
   O_NDELAY   = $0400;
   O_NOCTTY   = $0800;
   O_NONBLOCK = O_NDELAY;
   FNDELAY    = $0004;
type
   Poff_t = ^Toff_t;
   Toff_t = longint;
   Ppid_t = ^Tpid_t;
   Tpid_t = longint;
   Pssize_t = ^Tssize_t;
   Tssize_t = longint;
   Pmode_t = ^Tmode_t;
   Tmode_t = dword;

   Pflock = ^Tflock;
   Tflock = record
        l_type   : smallint;
        l_whence : smallint;
        l_start  : Toff_t;
        l_len    : Toff_t;
        l_pid    : Tpid_t;
     end;


function creat (path:Pchar; mode:Tmode_t):longint;                  cdecl;external 'clib' name 'creat';
function _fcntl (fildes:longint; cmd:longint;
                args:array of const):longint;                       cdecl;external 'clib' name 'fcntl';
function _fcntl (fildes:longint; cmd:longint):longint;               cdecl;external 'clib' name 'fcntl';
function open  (path:Pchar; oflag:longint;
                args:array of const):longint;                       cdecl;external 'clib' name 'open';
function open  (path:Pchar; oflag:longint):longint;                 cdecl;external 'clib' name 'open';
function fpopen(path:Pchar; oflag:longint):longint;                 cdecl;external 'clib' name 'open';
function sopen (path:Pchar; oflag, shflag:longint;
                args:array of const):longint;                       cdecl;external 'clib' name 'sopen';
function sopen (path:Pchar; oflag,shflag:longint):longint;          cdecl;external 'clib' name 'sopen';

{-limits.h---------------------------------------------------------------------}
const
   PAGESIZE  = 4096;
   CHAR_BIT  = 8;
   SCHAR_MIN = -(128);
   SCHAR_MAX = 127;
   UCHAR_MAX = 255;
   CHAR_MIN  = SCHAR_MIN;
   CHAR_MAX  = SCHAR_MAX;

   MB_LEN_MAX = 5;
   SHRT_MIN   = -(32768);
   SHRT_MAX   = 32767;
   USHRT_MAX  = 65535;
   LONG_MIN   = (-(2147483647)) - 1;
   LONG_MAX   = 2147483647;
   ULONG_MAX  = 4294967295;
   INT_MIN    = LONG_MIN;
   INT_MAX    = LONG_MAX;
   UINT_MAX   = ULONG_MAX;
   SSIZE_MAX  = INT_MAX;
   TZNAME_MAX = 8;
   PIPE_BUF   = 512;
{-locale.h---------------------------------------------------------------------}
{$PACKRECORDS C}

const
   LC_CTYPE = 0;
   LC_NUMERIC = 1;
   LC_TIME = 2;
   LC_COLLATE = 3;
   LC_MONETARY = 4;
   LC_MESSAGES = 5;
   LC_ALL = 6;

type
   Plconv = ^Tlconv;
   Tlconv =
   record
     decimal_point       : array [0..3] of char;
     thousands_sep       : array [0..3] of char;
     grouping            : array [0..3] of char;
     int_curr_symbol     : array [0..7] of char;
     currency_symbol     : array [0..3] of char;
     mon_decimal_point   : array [0..3] of char;
     mon_thousands_sep   : array [0..3] of char;
     mon_grouping        : array [0..7] of char;
     positive_sign       : array [0..3] of char;
     negative_sign       : array [0..3] of char;
     int_frac_digits     : char;
     frac_digits         : char;
     p_cs_precedes       : char;
     p_sep_by_space      : char;
     n_cs_precedes       : char;
     n_sep_by_space      : char;
     p_sign_posn         : char;
     n_sign_posn         : char;
     code_page           : word;
     country_id          : word;
     data_list_separator : array[0..1] of char;
     date_separator      : array[0..1] of char;
     time_separator      : array[0..1] of char;
     time_format         : char;
     date_format         : word;
     reserved            : array[0..49] of char;
   end;

// ???? struct lconv  *localeconv( void );

function setlocale_old (p1:longint; p2:Pchar):Pchar;  cdecl; external 'clib' name 'setlocale';
function setlocale     (p1:longint; p2:Pchar):Pchar;  cdecl; external 'clib' name 'setlocale_411';
{-nwlocale.h-------------------------------------------------------------------}
{$PACKRECORDS C}

type
  TNUMBER_TYPE = double;
  TUCHAR = byte;


const
   MERIDLEN = 5;
   L_MB_LEN_MAX = 2;    // multibyte character length maximum is 2
   NWSINGLE_BYTE = 1;   // returned from NWCharType()...
   NWDOUBLE_BYTE = 2;
{ country/language ID definitions (field 'country_id' in struct Llconv)...   }
   ARABIC = 785;
   AUSTRALIA = 61;
   BELGIUM = 32;
   CANADA_ENG = 1;
   CANADA_FR = 2;
   DENMARK = 45;
   FINLAND = 358;
   FRANCE = 33;
   GERMANY = 49;
   HEBREW = 972;
   ITALY = 39;
   LATIN_AMERICA = 003;
   NETHERLANDS = 31;
   NORWAY = 47;
   PORTUGAL = 351;
   SPAIN = 34;
   SWEDEN = 46;
   SWITZERLAND = 41;
   UK = 44;
   USA = 1;
   JAPAN = 81;
   KOREA = 82;
   PRC = 86;
   TAIWAN = 88;
   ASIAN_ENGLISH = 99;


type
   PVECTOR = ^TVECTOR;
   TVECTOR = record
        lowValue : char;
        highValue : char;
     end;
{ extern double-byte table data...  }
//??  var _DBCSVector : array[0..4] of TVECTOR;cvar;external;
{ prototypes...  }

function NWCharType(ch:dword):longint;cdecl;external 'locnlm32' name 'NWCharType';
function NWCharVal(_string:Pchar):longint;cdecl;external 'locnlm32' name 'NWCharVal';
function NWCharUpr(chr:longint):longint;cdecl;external 'locnlm32' name 'NWCharUpr';
function NWcprintf(format:Pchar; args:array of const):longint;cdecl;external 'locnlm32' name 'NWcprintf';
function NWcprintf(format:Pchar):longint;cdecl;external 'locnlm32' name 'NWcprintf';
function NWIncrement(_string:Pchar; numChars:Tsize_t):Pchar;cdecl;external 'locnlm32' name 'NWIncrement';
{
   NWatoi, NWisalnum, NWisalpha, and NWisdigit are preferred over NWLatoi,
   NWisalnum, NWLisalpha, and NWLisdigit respectively.
 }
function NWatoi(_string:Pchar):longint;cdecl;external 'locnlm32' name 'NWatoi';
function NWisalnum(ch:dword):longint;cdecl;external 'locnlm32' name 'NWisalnum';
function NWisalpha(ch:dword):longint;cdecl;external 'locnlm32' name 'NWisalpha';
function NWisdigit(ch:dword):longint;cdecl;external 'locnlm32' name 'NWisdigit';
function NWisxdigit(ch:dword):longint;cdecl;external 'locnlm32' name 'NWisxdigit';
function NWitoa(value:longint; _string:Pchar; radix:longint):longint;cdecl;external 'locnlm32' name 'NWitoa';
function NWutoa(value:dword; _string:Pchar; radix:longint):longint;cdecl;external 'locnlm32' name 'NWutoa';
function NWltoa(value:longint; _string:Pchar; radix:longint):longint;cdecl;external 'locnlm32' name 'NWltoa';
function NWultoa(value:dword; _string:Pchar; radix:longint):longint;cdecl;external 'locnlm32' name 'NWultoa';
(* Const before type ignored *)
function NWLatoi(_string:Pchar):longint;cdecl;external 'locnlm32' name 'NWLatoi';
function NWLisalnum(ch:dword):longint;cdecl;external 'locnlm32' name 'NWLisalnum';
function NWLisalpha(ch:dword):longint;cdecl;external 'locnlm32' name 'NWLisalpha';
function NWLisdigit(ch:dword):longint;cdecl;external 'locnlm32' name 'NWLisdigit';
function NWLlocaleconv(lconvPtr:PLCONV):PLCONV;cdecl;external 'locnlm32' name 'NWLlocaleconv';
function NWLmblen(_string:Pchar; maxBytes:Tsize_t):longint;cdecl;external 'locnlm32' name 'NWLmblen';
function NWLmbslen(_string:Pchar):longint;cdecl;external 'locnlm32' name 'NWLmbslen';
function NWLsetlocale(category:longint; locale:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLsetlocale';
function NWLsetlocale_411(category:longint; locale:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLsetlocale_411';
function NWLstrbcpy(dest:Pchar; src:Pchar; maxlen:Tsize_t):Pchar;cdecl;external 'locnlm32' name 'NWLstrbcpy';
function NWLstrchr(_string:Pchar; find:longint):Pchar;cdecl;external 'locnlm32' name 'NWLstrchr';
function NWLstrcoll(string1:Pchar; string2:Pchar):longint;cdecl;external 'locnlm32' name 'NWLstrcoll';
function NWLstrcspn(string1:Pchar; string2:Pchar):Tsize_t;cdecl;external 'locnlm32' name 'NWLstrcspn';
function NWLstrftime(_string:Pchar; maxSize:Tsize_t; format:Pchar; timePtr:Ptm):Tsize_t;cdecl;external 'locnlm32' name 'NWLstrftime';
function NWLstricmp(str1:Pchar; str2:Pchar):longint;cdecl;external 'locnlm32' name 'NWLstricmp';
function NWLstrlwr(_string:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLstrlwr';
function NWLstrpbrk(string1:Pchar; string2:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLstrpbrk';
function NWLstrrchr(_string:Pchar; find:longint):Pchar;cdecl;external 'locnlm32' name 'NWLstrrchr';
function NWLstrrev(string1:Pchar; string2:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLstrrev';
function NWLstrspn(string1:Pchar; string2:Pchar):Tsize_t;cdecl;external 'locnlm32' name 'NWLstrspn';
function NWLstrstr(_string:Pchar; searchString:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLstrstr';
function NWLstrupr(_string:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWLstrupr';
function NWLstrxfrm(string1:Pchar; string2:Pchar; numChars:Tsize_t):Tsize_t;cdecl;external 'locnlm32' name 'NWLstrxfrm';
function NWPrevChar(_string:Pchar; position:Pchar):Pchar;cdecl;external 'locnlm32' name 'NWPrevChar';
function NWprintf(format:Pchar; args:array of const):longint;cdecl;external 'locnlm32' name 'NWprintf';
function NWprintf(format:Pchar):longint;cdecl;external 'locnlm32' name 'NWprintf';
function NWsprintf(s:Pchar; format:Pchar; args:array of const):longint;cdecl;external 'locnlm32' name 'NWsprintf';
function NWsprintf(s:Pchar; format:Pchar):longint;cdecl;external 'locnlm32' name 'NWsprintf';
function NWstrImoney(buffer:Pchar; Value:TNUMBER_TYPE):Pchar;cdecl;external 'locnlm32' name 'NWstrImoney';
function NWstrmoney(buffer:Pchar; Value:TNUMBER_TYPE):Pchar;cdecl;external 'locnlm32' name 'NWstrmoney';
function NWstrncoll(string1:Pchar; string2:Pchar; maxChars:Tsize_t):longint;cdecl;external 'locnlm32' name 'NWstrncoll';
function NWstrncpy(target_string:Pchar; source_string:Pchar; numChars:longint):Pchar;cdecl;external 'locnlm32' name 'NWstrncpy';
function NWstrnum(buffer:Pchar; Value:TNUMBER_TYPE):Pchar;cdecl;external 'locnlm32' name 'NWstrnum';
//function NWvcprintf(format:Pchar; arg:Tva_list):longint;cdecl;external 'locnlm32' name 'NWvcprintf';
//function NWvprintf(format:Pchar; arg:Tva_list):longint;cdecl;external 'locnlm32' name 'NWvprintf';
//function NWvsprintf(s:Pchar; format:Pchar; arg:Tva_list):longint;cdecl;external 'locnlm32' name 'NWvsprintf';
{-nwaudnlm.h-------------------------------------------------------------------}
{ defined network address types:  }

const
   ASCIIZ_STRING_NET_ADDRESS_TYPE = 0;
   IPX_NET_ADDRESS_TYPE           = 1;

   // special value network address type:
   NO_IDENTITY_HAS_BEEN_SET       = $FF;

function NWAddRecordToAuditingFile
    (volumeNumber,
     recordType,
     stationNumber,
     statusCode        : longint;
     data              : pointer;
     dataSize          : longint):longint;cdecl;external 'clib' name 'NWAddRecordToAuditingFile';
function NWAddRecordToAuditingFile
    (volumeNumber,
     recordType,
     stationNumber,
     statusCode        : longint;
 var data;
     dataSize          : longint):longint;cdecl;external 'clib' name 'NWAddRecordToAuditingFile';


function NWGetAuditingIdentity
    (addressType       : Plongint;
     networkAddress    : pointer;
     identityName      : Pchar):longint;cdecl;external 'clib' name 'NWGetAuditingIdentity';
function NWGetAuditingIdentity
    (var addressType   : longint;
     var networkAddress;
     identityName      : Pchar):longint;cdecl;external 'clib' name 'NWGetAuditingIdentity';
function NWSetAuditingIdentity
    (addressType:longint;
     networkAddress:pointer;
     identityName:Pchar):longint;cdecl;external 'clib' name 'NWSetAuditingIdentity';
function NWSetAuditingIdentity
    (addressType:longint;
 var networkAddress;
     identityName:Pchar):longint;cdecl;external 'clib' name 'NWSetAuditingIdentity';
{-nwbitops.h-------------------------------------------------------------------}
procedure BitClear        (bitArray:pointer; bitNumber:longint);cdecl;external 'clib' name 'BitClear';
procedure BitSet          (bitArray:pointer; bitNumber:longint);cdecl;external 'clib' name 'BitSet';
function  BitTest         (bitArray:pointer; bitNumber:longint):longint;cdecl;external 'clib' name 'BitTest';
function  BitTestAndClear (bitArray:pointer; bitNumber:longint):longint;cdecl;external 'clib' name 'BitTestAndClear';
function  BitTestAndSet   (bitArray:pointer; bitNumber:longint):longint;cdecl;external 'clib' name 'BitTestAndSet';
function  ScanBits        (bitArray:pointer; startingBitNumber,totalBitCount:longint):longint;cdecl;external 'clib' name 'ScanBits';
function  ScanClearedBits (bitArray:pointer; startingBitNumber,totalBitCount:longint):longint;cdecl;external 'clib' name 'ScanClearedBits';

procedure BitClear        (var bitArray; bitNumber:longint);cdecl;external 'clib' name 'BitClear';
procedure BitSet          (var bitArray; bitNumber:longint);cdecl;external 'clib' name 'BitSet';
function  BitTest         (var bitArray; bitNumber:longint):longint;cdecl;external 'clib' name 'BitTest';
function  BitTestAndClear (var bitArray; bitNumber:longint):longint;cdecl;external 'clib' name 'BitTestAndClear';
function  BitTestAndSet   (var bitArray; bitNumber:longint):longint;cdecl;external 'clib' name 'BitTestAndSet';
function  ScanBits        (var bitArray; startingBitNumber,totalBitCount:longint):longint;cdecl;external 'clib' name 'ScanBits';
function  ScanClearedBits (var bitArray; startingBitNumber,totalBitCount:longint):longint;cdecl;external 'clib' name 'ScanClearedBits';
{-nwcntask.h-------------------------------------------------------------------}
{#define LOGIN_WITHOUT_PASSWORD ((char *) N_TRUE) }

function AllocateBlockOfTasks(numberWanted:longint):longint;cdecl;external 'clib' name 'AllocateBlockOfTasks';
function CheckIfConnectionActive(connection:longint):byte;cdecl;external 'clib' name 'CheckIfConnectionActive';
function DisableConnection(connection:longint):longint;cdecl;external 'clib' name 'DisableConnection';
function EnableConnection(connection:longint):longint;cdecl;external 'clib' name 'EnableConnection';
function GetCurrentConnection:longint;cdecl;external 'clib' name 'GetCurrentConnection';
function GetCurrentFileServerID:word;cdecl;external 'clib' name 'GetCurrentFileServerID';
function GetCurrentTask:longint;cdecl;external 'clib' name 'GetCurrentTask';
function LoginObject(connection:longint; objectName:Pchar; objectType:word; password:Pchar):longint;cdecl;external 'clib' name 'LoginObject';
function LogoutObject(connection:longint):longint;cdecl;external 'clib' name 'LogoutObject';
function ReturnBlockOfTasks(startingTask,numberOfTasks:longint):longint;cdecl;external 'clib' name 'ReturnBlockOfTasks';
function ReturnConnection(connection:longint):longint;cdecl;external 'clib' name 'ReturnConnection';
function ReturnLocalConnection(connection:longint):longint;cdecl;external 'clib' name 'ReturnLocalConnection';
function SetCurrentConnection(connectionNumber:longint):longint;cdecl;external 'clib' name 'SetCurrentConnection';
function SetCurrentFileServerID(connectionID:word):word;cdecl;external 'clib' name 'SetCurrentFileServerID';
function SetCurrentTask(taskNumber:longint):longint;cdecl;external 'clib' name 'SetCurrentTask';
{-nwconio.h--------------------------------------------------------------------}
const
   DONT_AUTO_ACTIVATE     = $01;  // avoids autoactivation when screens are
                                  // created, but no other screens exist
   DONT_SWITCH_SCREEN     = $02;  // avoids screen being switched
   DONT_CHECK_CTRL_CHARS  = $10;  // turns off ^C and ^S processing
   AUTO_DESTROY_SCREEN    = $20;  // avoids "Press any key to close screen
   POP_UP_SCREEN          = $40;
   UNCOUPLED_CURSORS      = $80;  // for distinct input & output cursors
{ more screen attribute values returned by GetScreenInfo()  }
   HAS_A_CLIB_HANDLE            = $00000100;
   _KEYBOARD_INPUT_ACTIVE       = $00010000;
   _PROCESS_BLOCKED_ON_KEYBOARD = $00020000;
   _PROCESS_BLOCKED_ON_SCREEN   = $00040000;
   _INPUT_CURSOR_DISABLED       = $00080000;
   _SCREEN_HAS_TITLE_BAR        = $00400000;
   _NON_SWITCHABLE_SCREEN       = $01000000;

   { key types...  }
     NORMAL_KEY       = $00;
     FUNCTION_KEY     = $01;
     ENTER_KEY        = $02;
     ESCAPE_KEY       = $03;
     BACKSPACE_KEY    = $04;
     DELETE_KEY       = $05;
     INSERT_KEY       = $06;
     CURSOR_UP_KEY    = $07;
     CURSOR_DOWN_KEY  = $08;
     CURSOR_RIGHT_KEY = $09;
     CURSOR_LEFT_KEY  = $0A;
     CURSOR_HOME_KEY  = $0B;
     CURSOR_END_KEY   = $0C;
     CURSOR_PUP_KEY   = $0D;
     CURSOR_PDOWN_KEY = $0E;
  { some name equivalents...  }
     ENTER     = $0D;
     ESCAPE    = $1B;
     BACKSPACE = $08;
  { modifier code constituents...  }
     SHIFT_KEY_HELD    = $01;
     CTRL_KEY_HELD     = $04;
     ALT_KEY_HELD      = $08;
     CAPS_LOCK_IS_ON   = $40;
     NUM_LOCK_IS_ON    = $20;
     SCROLL_LOCK_IS_ON = $10;

  { cursor types...  }
     CURSOR_NORMAL = $0C0B;
     CURSOR_THICK  = $0C09;
     CURSOR_BLOCK  = $0C00;
     CURSOR_TOP    = $0400;

type  // libc compatible
   Pscr_t = ^scr_t;
   scr_t = pointer;
   TScr = scr_t;
   PScr = Pscr_t;
   PScreenStruct = PScr;

function getch:longint; cdecl; external 'clib' name 'getch';
function getche:longint; cdecl; external 'clib' name 'getche';
function kbhit:longint; cdecl; external 'clib' name 'kbhit';
function putch(c:longint):longint; cdecl; external 'clib' name 'putch';
function ungetch(c:longint):longint; cdecl; external 'clib' name 'ungetch';
function ungetcharacter(c:longint):longint; cdecl; external 'clib' name 'ungetch';
function cgets(buf:Pchar):Pchar; cdecl; external 'clib' name 'cgets';
function CheckIfScreenDisplayed(screenHandle,waitFlag:longint):longint; cdecl; external 'clib' name 'CheckIfScreenDisplayed';
function CheckIfScreenDisplayed(screenHandle:TScr;waitFlag:longint):longint; cdecl; external 'clib' name 'CheckIfScreenDisplayed';
procedure clrscr; cdecl; external 'clib' name 'clrscr';
procedure ConsolePrintf(format:Pchar; args:array of const); cdecl; external 'clib' name 'ConsolePrintf';
procedure ConsolePrintf(format:Pchar); cdecl; external 'clib' name 'ConsolePrintf';
procedure CopyToScreenMemory(height,width:word; Rect:PBYTE; beg_x,beg_y:word); cdecl; external 'clib' name 'CopyToScreenMemory';
procedure CopyToScreenMemory(height,width:word; var Data; beg_x,beg_y:word); cdecl; external 'clib' name 'CopyToScreenMemory';
procedure CopyFromScreenMemory(height,width:word; Rect:PBYTE; beg_x,beg_y:word); cdecl; external 'clib' name 'CopyFromScreenMemory';
procedure CopyFromScreenMemory(height,width:word; var Data; beg_x,beg_y:word); cdecl; external 'clib' name 'CopyFromScreenMemory';
function CoupleInputOutputCursors:longint; cdecl; external 'clib' name 'CoupleInputOutputCursors';
function cputs(buf:Pchar):longint; cdecl; external 'clib' name 'cputs';
function cprintf(fmt:Pchar; args:array of const):longint; cdecl; external 'clib' name 'cprintf';
function cprintf(fmt:Pchar):longint; cdecl; external 'clib' name 'cprintf';
//function CreateScreen(screenName:Pchar; attr:byte):longint; cdecl; external 'clib' name 'CreateScreen';
function CreateScreen(screenName:Pchar; attr:byte):TScr; cdecl; external 'clib' name 'CreateScreen';
function cscanf(fmt:Pchar; args:array of const):longint; cdecl; external 'clib' name 'cscanf';
function cscanf(fmt:Pchar):longint; cdecl; external 'clib' name 'cscanf';
function DecoupleInputOutputCursors:longint; cdecl; external 'clib' name 'DecoupleInputOutputCursors';
function DestroyScreen(screenHandle:longint):longint; cdecl; external 'clib' name 'DestroyScreen';
function DestroyScreen(screenHandle:TScr):longint; cdecl; external 'clib' name 'DestroyScreen';
function DisplayInputCursor:longint; cdecl; external 'clib' name 'DisplayInputCursor';
function DisplayScreen(screenHandle:longint):longint; cdecl; external 'clib' name 'DisplayScreen';
function DisplayScreen(screenHandle:TScr):longint; cdecl; external 'clib' name 'DisplayScreen';
function DropPopUpScreen(screenHandle:longint):longint; cdecl; external 'clib' name 'DropPopUpScreen';
function DropPopUpScreen(screenHandle:TScr):longint; cdecl; external 'clib' name 'DropPopUpScreen';
//function GetCurrentScreen:longint; cdecl; external 'clib' name 'GetCurrentScreen';
function GetCurrentScreen:TScr; cdecl; external 'clib' name 'GetCurrentScreen';
function GetCursorCouplingMode:byte; cdecl; external 'clib' name 'GetCursorCouplingMode';
function GetCursorShape(startline,endline:PBYTE):word; cdecl; external 'clib' name 'GetCursorShape';
function GetCursorShape(var startline,endline:byte):word; cdecl; external 'clib' name 'GetCursorShape';
function GetCursorSize(firstline,lastline:PBYTE):word; cdecl; external 'clib' name 'GetCursorSize';
function GetCursorSize(var firstline,lastline:byte):word; cdecl; external 'clib' name 'GetCursorSize';
function GetPositionOfOutputCursor(rowP,columnP:PWORD):longint; cdecl; external 'clib' name 'GetPositionOfOutputCursor';
function GetPositionOfOutputCursor(var row,col:word):longint; cdecl; external 'clib' name 'GetPositionOfOutputCursor';
function __GetScreenID(screenHandle:longint):longint; cdecl; external 'clib' name '__GetScreenID';
function __GetScreenID(screenHandle:TScr):longint; cdecl; external 'clib' name '__GetScreenID';
function GetScreenInfo(handle:longint; name:Pchar; attr:plongint):longint; cdecl; external 'clib' name 'GetScreenInfo';
function GetScreenInfo(handle:longint; name:Pchar; var attr:longint):longint; cdecl; external 'clib' name 'GetScreenInfo';
function GetSizeOfScreen(heightP,widthP:PWORD):longint; cdecl; external 'clib' name 'GetSizeOfScreen';
function GetSizeOfScreen(var heightP,widthP:word):longint; cdecl; external 'clib' name 'GetSizeOfScreen';
procedure gotoxy(col,row:word); cdecl; external 'clib' name 'gotoxy';
function HideInputCursor:longint; cdecl; external 'clib' name 'HideInputCursor';
function IsColorMonitor:longint; cdecl; external 'clib' name 'IsColorMonitor';
function PressAnyKeyToContinue:longint; cdecl; external 'clib' name 'PressAnyKeyToContinue';
function PressAnyKey:longint; cdecl; external 'clib' name 'PressAnyKeyToContinue';
function PressEscapeToQuit:longint; cdecl; external 'clib' name 'PressEscapeToQuit';
function PressEscape:longint; cdecl; external 'clib' name 'PressEscapeToQuit';
procedure RingTheBell; cdecl; external 'clib' name 'RingTheBell';
procedure RingBell; cdecl; external 'clib' name 'RingTheBell';

function ScanScreens(LastScreenID:longint; name:Pchar; attr:plongint):longint; cdecl; external 'clib' name 'ScanScreens';
function ScanScreens(LastScreenID:longint; name:Pchar; var attr:longint):longint; cdecl; external 'clib' name 'ScanScreens';
function ScanScreens(LastScreenID:TScr; name:Pchar; attr:plongint):TScr; cdecl; external 'clib' name 'ScanScreens';
function ScanScreens(LastScreenID:TScr; name:Pchar; var attr:longint):TScr; cdecl; external 'clib' name 'ScanScreens';

function ScrollScreenRegionDown(firstLine,numLines:longint):longint; cdecl; external 'clib' name 'ScrollScreenRegionDown';
function ScrollScreenRegionUp(firstLine,numLines:longint):longint; cdecl; external 'clib' name 'ScrollScreenRegionUp';
function SetAutoScreenDestructionMode(newMode:byte):byte; cdecl; external 'clib' name 'SetAutoScreenDestructionMode';
function SetCtrlCharCheckMode(newMode:byte):byte; cdecl; external 'clib' name 'SetCtrlCharCheckMode';
function SetCursorCouplingMode(newMode:byte):byte; cdecl; external 'clib' name 'SetCursorCouplingMode';
function SetCursorShape(startline,endline:byte):word; cdecl; external 'clib' name 'SetCursorShape';
function SetCurrentScreen(screenHandle:longint):longint; cdecl; external 'clib' name 'SetCurrentScreen';
function SetCurrentScreen(screenHandle:TScr):longint; cdecl; external 'clib' name 'SetCurrentScreen';
function SetInputAtOutputCursorPosition:longint; cdecl; external 'clib' name 'SetInputAtOutputCursorPosition';
function SetOutputAtInputCursorPosition:longint; cdecl; external 'clib' name 'SetOutputAtInputCursorPosition';
function SetPositionOfInputCursor(row,col:word):longint; cdecl; external 'clib' name 'SetPositionOfInputCursor';
function SetScreenAreaAttribute(line,col:longint; numLines:longint; numColumns:longint; attr:longint):longint; cdecl; external 'clib' name 'SetScreenAreaAttribute';
function SetScreenAttributes(mask,attr:longint):longint; cdecl; external 'clib' name 'SetScreenAttributes';
function SetScreenCharacterAttribute(line,column,attr:longint):longint; cdecl; external 'clib' name 'SetScreenCharacterAttribute';
function SetScreenRegionAttribute(firstLine,numLines:longint; attr:byte):longint; cdecl; external 'clib' name 'SetScreenRegionAttribute';
function wherex:word; cdecl; external 'clib' name 'wherex';
function wherey:word; cdecl; external 'clib' name 'wherey';

procedure GetKey(scrID:TScr; _type,value,status,scancode:Pbyte;linesToProtect:Longint);cdecl;external 'clib' name 'GetKey';
procedure GetKey(scrID:TScr; var _type,value,status,scancode:byte;linesToProtect:Longint);cdecl;external 'clib' name 'GetKey';
procedure GetKey(scrID:Longint; _type,value,status,scancode:Pbyte;linesToProtect:Longint);cdecl;external 'clib' name 'GetKey';
procedure GetKey(scrID:Longint; var _type,value,status,scancode:byte;linesToProtect:Longint);cdecl;external 'clib' name 'GetKey';

function UngetKey(scrID:TScr; _type,value,status,scancode:byte):longint;cdecl;external 'clib' name 'UngetKey';
function UngetKey(scrID:Longint; _type,value,status,scancode:byte):longint;cdecl;external 'clib' name 'UngetKey';
{-nwconn.h---------------------------------------------------------------------}
{ Structures and typedefs for connection services  }

const
   IPX_TRANSPORT_ADDRESS =  1;
   IPX_TRANSPORT_LENGTH  = 12;
   UDP_TRANSPORT_ADDRESS =  8;
   UDP_TRANSPORT_LENGTH  =  4;
   TCP_TRANSPORT_ADDRESS =  9;
   TCP_TRANSPORT_LENGTH  =  4;
{$include npackon.inc}
type
   PUserNameStruct = ^TUserNameStruct;
   TUserNameStruct = record
        UserName : array[0..47] of char;
        ObjectID : longint;
     end;
   TConnectionCriticalErrorHandler =
      function (fileServerID,connection,err:longint):longint; cdecl;

{$include npackoff.inc}

function AttachByAddress(transType:byte; transLen:longint; transBuf:pointer; fileServerID:PWORD):longint;cdecl;external 'clib' name 'AttachByAddress';
function AttachByAddress(transType:byte; transLen:longint; var transBuf; var fileServerID:word):longint;cdecl;external 'clib' name 'AttachByAddress';

function AttachToFileServer(fileServerName:Pchar; fileServerID:PWORD):longint;cdecl;external 'clib' name 'AttachToFileServer';
function AttachToFileServer(fileServerName:Pchar; var fileServerID:word):longint;cdecl;external 'clib' name 'AttachToFileServer';

function GetConnectionFromID(fileServerID:PWORD):longint;cdecl;external 'clib' name 'GetConnectionFromID';
function GetConnectionFromID(var fileServerID:word):longint;cdecl;external 'clib' name 'GetConnectionFromID';

function GetConnectionInformation (connectionNumber:word;
                                   objectName      :Pchar;
                                   objectType      :PWORD;
                                   objectID        :Plongint;
                                   loginTime       :pointer):longint;cdecl;external 'clib' name 'GetConnectionInformation';
function GetConnectionInformation (connectionNumber:word;
                                   objectName      :Pchar;
                               var objectType      :word;
                               var objectID        :longint;
                               var loginTime):longint;cdecl;external 'clib' name 'GetConnectionInformation';

function GetConnectionList(objectID,lastConnection:longint;
                           numberOfConnections:Plongint;
                           connectionList:pointer;
                           connectionSize:longint):longint;cdecl;external 'clib' name 'GetConnectionList';
function GetConnectionList(objectID,lastConnection:longint;
                       var numberOfConnections:longint;
                       var connectionList;  {array of longint}
                           connectionSize:longint):longint;cdecl;external 'clib' name 'GetConnectionList';
function GetConnectionNumber:word;cdecl;external 'clib' name 'GetConnectionNumber';
function GetDefaultConnectionID:longint;cdecl;external 'clib' name 'GetDefaultConnectionID';
function GetDefaultFileServerID:longint;cdecl;external 'clib' name 'GetDefaultFileServerID';
function GetFileServerID(fileServerName:Pchar; fileServerID:PWORD):longint;cdecl;external 'clib' name 'GetFileServerID';
function GetFileServerID(fileServerName:Pchar; var fileServerID:word):longint;cdecl;external 'clib' name 'GetFileServerID';

function GetInternetAddress(connectionNumber:word;
                            networkNumber:pointer;
                            physicalNodeAddress:pointer):longint;cdecl;external 'clib' name 'GetInternetAddress';
function GetInternetAddress(connectionNumber:word;
                            var networkNumber;  {4 bytes}
                            var physicalNodeAddress {6 bytes}):longint;cdecl;external 'clib' name 'GetInternetAddress';
function GetLANAddress (boardNumber:longint;
                        nodeAddress:pointer):longint;cdecl;external 'clib' name 'GetLANAddress';
function GetLANAddress (boardNumber:longint;
                    var nodeAddress{6 bytes}):longint;cdecl;external 'clib' name 'GetLANAddress';

function GetMaximumNumberOfStations:longint;cdecl;external 'clib' name 'GetMaximumNumberOfStations';
function GetNetNumber(boardNumber:longint):longint;cdecl;external 'clib' name 'GetNetNumber';

function GetObjectConnectionNumbers (objectName:Pchar;
                                     objectType:word;
                                     numberOfConnections:PWORD;
                                     connectionList:PWORD;
                                     maxConnections:word):longint;cdecl;external 'clib' name 'GetObjectConnectionNumbers';
function GetObjectConnectionNumbers (objectName:Pchar;
                                     objectType:word;
                                 var numberOfConnections:word;
                                 var connectionList;  {array of WORD}
                                     maxConnections:word):longint;cdecl;external 'clib' name 'GetObjectConnectionNumbers';

procedure GetStationAddress(physicalNodeAddress:pointer);cdecl;external 'clib' name 'GetStationAddress';
procedure GetStationAddress(var physicalNodeAddress {6 bytes});cdecl;external 'clib' name 'GetStationAddress';

function GetUserNameFromNetAddress (internetAddress:PBYTE;
                                    sequenceNumber:longint;
                                    userNameP:PUserNameStruct):longint;cdecl;external 'clib' name 'GetUserNameFromNetAddress';
function GetUserNameFromNetAddress (var internetAddress;  {10 bytes}
                                    sequenceNumber:longint;
                                    var userName:TUserNameStruct):longint;cdecl;external 'clib' name 'GetUserNameFromNetAddress';

function LoginToFileServer (objectName:Pchar;
                            objectType:word;
                            objectPassword:Pchar):longint;cdecl;external 'clib' name 'LoginToFileServer';
procedure Logout;cdecl;external 'clib' name 'Logout';
procedure LogoutFromFileServer(fileServerID:word);cdecl;external 'clib' name 'LogoutFromFileServer';
function NWDSGetCurrentUser:longint;cdecl;external 'clib' name 'NWDSGetCurrentUser';
function NWDSSetCurrentUser(userHandle:longint):longint;cdecl;external 'clib' name 'NWDSSetCurrentUser';

function NWDSSetPreferredDSTree (len:longint; treeName:Pchar):longint;cdecl;external 'clib' name 'NWDSSetPreferredDSTree';
function NWGetPacketBurstBufferCount:longint;cdecl;external 'clib' name 'NWGetPacketBurstBufferCount';
function NWGetSecurityLevel:longint;cdecl;external 'clib' name 'NWGetSecurityLevel';

function NWNCPSend (functionCode:byte;
                    sendPacket:pointer; sendLen :word;
                    replyBuf  :pointer; replyLen:word):longint;cdecl;external 'clib' name 'NWNCPSend';
function NWNCPSend (functionCode:byte;
                    var sendPacket; sendLen :word;
                    var replyBuf;   replyLen:word):longint;cdecl;external 'clib' name 'NWNCPSend';

function NWSetPacketBurstBufferCount(numberOfBuffers:longint):longint;cdecl;external 'clib' name 'NWSetPacketBurstBufferCount';
function NWSetSecurityLevel(SecurityLevel:longint):longint;cdecl;external 'clib' name 'NWSetSecurityLevel';

function SetConnectionCriticalErrorHandler(func:TConnectionCriticalErrorHandler):longint;cdecl;external 'clib' name 'SetConnectionCriticalErrorHandler';
{-nwdebug.h--------------------------------------------------------------------}
{ library-debug flags  }

const
   CLIB_CONTEXT_CHECK = $002;   { CLib Context  }
   CLIB_MEMCHECK      = $004;   { Memory Overwrites  }
   CLIB_RESOURCECHECK = $020;   { Resource Check  }
   CLIB_THREAD_CHECK  = $200;   { Thread Check  }
   CLIB_SEMCHECK      = $080;   { Semaphore Checking  }
   CLIB_RING_BELL     = $040;
{ dynamic setting and clearing of breakpoints  }
   EXECUTION_BREAKPOINT  = 0;
   WRITE_BREAKPOINT      = 1;
   READ_WRITE_BREAKPOINT = 3;

// most of the functions are in the kernel (system), we define clib here to
// avoid linker errors
procedure NWClearBreakpoint(breakpoint:longint);cdecl;external 'clib' name 'NWClearBreakpoint';
function NWSetBreakpoint(address,breakType:longint):longint;cdecl;external 'clib' name 'NWSetBreakpoint';
function NWDebugPrintf(format:Pchar; args:array of const):longint;cdecl;external 'clib' name 'NWDebugPrintf';
function NWDebugPrintf(format:Pchar):longint;cdecl;external 'clib' name 'NWDebugPrintf';
function NWValidateDebugProfile:longint;cdecl;external 'clib' name 'NWValidateDebugProfile';
procedure NWBumpFunctionCount(name:Pchar);cdecl;external 'clib' name 'NWBumpFunctionCount';
procedure NWDisplayBinaryAtAddr(addr:pointer);cdecl;external 'clib' name 'NWDisplayBinaryAtAddr';
procedure NWDisplayDoubleAtAddr(addr:pointer);cdecl;external 'clib' name 'NWDisplayDoubleAtAddr';
procedure NWDisplayLConvAtAddr(lc:pointer);cdecl;external 'clib' name 'NWDisplayLConvAtAddr';
procedure NWDisplayStringAtAddr(s:Pchar; len:longint);cdecl;external 'clib' name 'NWDisplayStringAtAddr';
procedure NWDisplayTMAtAddr(t:pointer);cdecl;external 'clib' name 'NWDisplayTMAtAddr';
procedure NWDisplayUnicodeAtAddr(s:pointer; len:longint);cdecl;external 'clib' name 'NWDisplayUnicodeAtAddr';
procedure NWEnableDebugProfile(flag:longint);cdecl;external 'clib' name 'NWEnableDebugProfile';
procedure EnterDebugger;cdecl;external 'clib' name 'EnterDebugger';
function GetDebugSettings:longint;cdecl;external 'clib' name 'GetDebugSettings';
procedure SetDebugSettings(Settings:longint);cdecl;external 'clib' name 'SetDebugSettings';
function GetNLMIDFromNLMName(NLMName:PChar):longint;cdecl;external 'clib' name 'GetNLMIDFromNLMName';
function GetDebugErrorMessage:PChar;cdecl;external 'clib' name 'GetDebugErrorMessage';
function GetMemoryUsage(NLMID:longint):longint;cdecl;external 'clib' name 'GetMemoryUsage';
{-nwdfs.h----------------------------------------------------------------------}
// completion codes
const
   DFSFailedCompletion           = -1;
   DFSNormalCompletion           = 0;
   DFSInsufficientSpace          = 1;
   DFSVolumeSegmentDeactivated   = 4;
   DFSTruncationFailure          = 16;
   DFSHoleInFileError            = 17;
   DFSParameterError             = 18;
   DFSOverlapError               = 19;
   DFSSegmentError               = 20;
   DFSBoundryError               = 21;
   DFSInsufficientLimboFileSpace = 22;
   DFSNotInDirectFileMode        = 23;
   DFSOperationBeyondEndOfFile   = 24;
   DFSOutOfHandles               = 129;
   DFSHardIOError                = 131;
   DFSInvalidFileHandle          = 136;
   DFSNoReadPrivilege            = 147;
   DFSNoWritePrivilege           = 148;
   DFSFileDetached               = 149;
   DFSInsufficientMemory         = 150;
   DFSInvalidVolume              = 152;
   DFSIOLockError                = 162;

{$PACKRECORDS C}
type
   PFileMapStructure = ^TFileMapStructure;
   TFileMapStructure = record
     fileBlock      : longint;
     volumeBlock    : longint;
     numberOfBlocks : longint;
   end;

   PVolumeInformationStructure = ^TVolumeInformationStructure;
   TVolumeInformationStructure = record
     VolumeAllocationUnitSizeInBytes           : longint;
     VolumeSizeInAllocationUnits               : longint;
     VolumeSectorSize                          : longint;
     AllocationUnitsUsed                       : longint;
     AllocationUnitsFreelyAvailable            : longint;
     AllocationUnitsInDeletedFilesNotAvailable : longint;
     AllocationUnitsInAvailableDeletedFiles    : longint;
     NumberOfPhysicalSegmentsInVolume          : longint;
     PhysicalSegmentSizeInAllocationUnits      : array[0..63] of longint;
   end;

   PDFSCallBackParameters = ^TDFSCallBackParameters;
   TDFSCallBackParameters = record
     localSemaphoreHandle : longint;
     completionCode       : longint;
   end;

{-------------------------------------------------------------------------
        Definition of setSizeFlags
 ------------------------------------------------------------------------- }

const
   SETSIZE_NON_SPARSE_FILE = $00000001;    // Alloc blocks to extend the file
   SETSIZE_NO_ZERO_FILL    = $00000002;    // Do not zero fill the newly allocated blocks
   SETSIZE_UNDO_ON_ERR     = $00000004;    // In non sparse cases truncate back to original eof if an error occurs
   SETSIZE_PHYSICAL_ONLY   = $00000008;    // Change the physical EOF only, dont change logical EOF. This means non sparse for the expand case
   SETSIZE_LOGICAL_ONLY    = $00000010;    // Change only the logical EOF expand will always be sparse and truncate won't free physical blocks

function DFSclose(fileHandle:longint):longint;cdecl;external 'clib' name 'DFSclose';
function DFScreat(fileName:Pchar; permission,flagBits:longint):longint;cdecl;external 'clib' name 'DFScreat';
function DFSExpandFile(fileHandle,fileBlockNumber,
                       numberOfBlocks,volumeBlockNumber,segmentNumber:longint):longint;cdecl;external 'clib' name 'DFSExpandFile';
function DFSFreeLimboVolumeSpace(volumeNumber,numberOfBlocks:longint):longint;cdecl;external 'clib' name 'DFSFreeLimboVolumeSpace';
function DFSsopen(fileName:PChar; access,share,permission,flagBits,dataStream:longint):longint;cdecl;external 'clib' name 'DFSsopen';
function DFSRead(fileHandle,startingSector,sectorCount:longint; buffer:pointer):longint;cdecl;external 'clib' name 'DFSRead';
function DFSRead(fileHandle,startingSector,sectorCount:longint; var buffer):longint;cdecl;external 'clib' name 'DFSRead';
function DFSReadNoWait(fileHandle,startingSector,sectorCount:longint; buffer:pointer; callBackNode:PDFSCallBackParameters):longint;cdecl;external 'clib' name 'DFSReadNoWait';
function DFSReadNoWait(fileHandle,startingSector,sectorCount:longint; var buffer; var callBackNode:TDFSCallBackParameters):longint;cdecl;external 'clib' name 'DFSReadNoWait';
function DFSReturnFileMappingInformation(fileHandle,startingBlockNumber:longint; numberOfEntries:Plongint; tableSize:longint; table:PFileMapStructure):longint;cdecl;external 'clib' name 'DFSReturnFileMappingInformation';
function DFSReturnFileMappingInformation(fileHandle,startingBlockNumber:longint; var numberOfEntries:longint; tableSize:longint; var table:TFileMapStructure):longint;cdecl;external 'clib' name 'DFSReturnFileMappingInformation';
function DFSReturnVolumeBlockInformation(volumeNumber,startingBlockNumber,numberOfBlocks:longint; buffer:Pointer):longint;cdecl;external 'clib' name 'DFSReturnVolumeBlockInformation';
function DFSReturnVolumeBlockInformation(volumeNumber,startingBlockNumber,numberOfBlocks:longint; var buffer):longint;cdecl;external 'clib' name 'DFSReturnVolumeBlockInformation';
function DFSReturnVolumeMappingInformation(volumeNumber:longint; volumeInformation:PVolumeInformationStructure):longint;cdecl;external 'clib' name 'DFSReturnVolumeMappingInformation';
function DFSSetDataSize (handle:longint; newFileSize:int64; setSizeFlags:longint):longint; cdecl;external 'clib' name 'DFSSetDataSize';
function DFSSetEndOfFile(handle,newFileSize,returnTruncatedBlocksFlag:longint):longint;cdecl;external 'clib' name 'DFSSetEndOfFile';
function DFSWrite(fileHandle,startingSector,sectorCount:longint; buffer:pointer):longint;cdecl;external 'clib' name 'DFSWrite';
function DFSWrite(fileHandle,startingSector,sectorCount:longint; var buffer):longint;cdecl;external 'clib' name 'DFSWrite';
function DFSWriteNoWait(fileHandle,startingSector,sectorCount:longint; buffer:pointer; callBackNode:PDFSCallBackParameters):longint;cdecl;external 'clib' name 'DFSWriteNoWait';
function DFSWriteNoWait(fileHandle,startingSector,sectorCount:longint; var buffer; var callBackNode:TDFSCallBackParameters):longint;cdecl;external 'clib' name 'DFSWriteNoWait';
{-nwdos.h----------------------------------------------------------------------}
{$include npackon.inc}
type
   Pfind_t = ^Tfind_t;
   Tfind_t = record
        reserved : array[0..20] of char;
        attrib : char;
        wr_time : word;
        wr_date : word;
        size : longint;
        name : array[0..12] of char;
     end;

{$include npackoff.inc}

function DOSChangeFileMode(name:Pchar; attributes:Plongint; _function:longint; newAttributes:longint):longint;cdecl;external 'clib' name 'DOSChangeFileMode';
function DOSClose(handle:longint):longint;cdecl;external 'clib' name 'DOSClose';
function DOSCopy(NetWareFileName,DOSFileName:Pchar):longint;cdecl;external 'clib' name 'DOSCopy';
function DOSCreate(fileName:Pchar; handle:Plongint):longint;cdecl;external 'clib' name 'DOSCreate';
function DOSCreate(fileName:Pchar; var handle:longint):longint;cdecl;external 'clib' name 'DOSCreate';
function DOSsopen(filename:Pchar; access,share,permission:longint):longint;cdecl;external 'clib' name 'DOSsopen';
function DOSFindFirstFile(fileName:Pchar; searchAttributes:word; diskTransferAddress:Pfind_t):longint;cdecl;external 'clib' name 'DOSFindFirstFile';
function DOSFindNextFile(diskTransferAddress:Pfind_t):longint;cdecl;external 'clib' name 'DOSFindNextFile';
function DOSMkdir(__dirName:Pchar):longint;cdecl;external 'clib' name 'DOSMkdir';
function DOSOpen(fileName:Pchar; handle:Plongint):longint;cdecl;external 'clib' name 'DOSOpen';
function DOSOpen(fileName:Pchar; var handle:longint):longint;cdecl;external 'clib' name 'DOSOpen';
function DOSPresent:longint;cdecl;external 'clib' name 'DOSPresent';
function DOSRead(handle,fileOffset:longint; buffer:pointer; numberOfBytesToRead:longint; numberOfBytesRead:Plongint):longint;cdecl;external 'clib' name 'DOSRead';
function DOSRead(handle,fileOffset:longint; var buffer; numberOfBytesToRead:longint; var numberOfBytesRead:longint):longint;cdecl;external 'clib' name 'DOSRead';
function DOSRemove(name:Pchar):longint;cdecl;external 'clib' name 'DOSRemove';
function DOSRename(srcName,dstName:Pchar):longint;cdecl;external 'clib' name 'DOSRename';
function DOSRmdir(Name:Pchar):longint;cdecl;external 'clib' name 'DOSRmdir';
function DOSSetDateAndTime(handle,date,time:longint):longint;cdecl;external 'clib' name 'DOSSetDateAndTime';
procedure DOSShutOffFloppyDrive;cdecl;external 'clib' name 'DOSShutOffFloppyDrive';
function DOSUnlink(Name:Pchar):longint;cdecl;external 'clib' name 'DOSUnlink';
function DOSWrite(handle,fileOffset:longint; buffer:pointer; numberOfBytesToWrite:longint; numberOfBytesWritten:Plongint):longint;cdecl;external 'clib' name 'DOSWrite';
function DOSWrite(handle,fileOffset:longint; var buffer; numberOfBytesToWrite:longint; var numberOfBytesWritten:longint):longint;cdecl;external 'clib' name 'DOSWrite';
{-nwfattr.h--------------------------------------------------------------------}
const
   NWSH_PRE_401D_COMPAT = $80000000;  // for multiple thread use, see documentation for sopen()...
{ Attribute values for use with existing files                              }
{ Normal (read/write) file         }
   _A_NORMAL             = $00000000;
   _A_RDONLY             = $00000001;
   _A_HIDDEN             = $00000002;
   _A_SYSTEM             = $00000004;
   _A_EXECUTE            = $00000008;
   _A_VOLID              = $00000008;  // Volume ID entry
   _A_SUBDIR             = $00000010;
   _A_ARCH               = $00000020;
   _A_SHARE              = $00000080;
   _A_NO_SUBALLOC        = $00000800;  // Don't sub alloc. this file
   _A_TRANS              = $00001000;  // Transactional file (TTS usable)
   _A_READAUD            = $00004000;  // Read audit
   _A_WRITAUD            = $00008000;  // Write audit
   _A_IMMPURG            = $00010000;  // Immediate purge
   _A_NORENAM            = $00020000;  // Rename inhibit
   _A_NODELET            = $00040000;  // Delete inhibit
   _A_NOCOPY             = $00080000;  // Copy inhibit
   _A_FILE_MIGRATED      = $00400000;  // File has been migrated
   _A_DONT_MIGRATE       = $00800000;  // Don't migrate this file
   _A_IMMEDIATE_COMPRESS = $02000000;  // Compress this file immediately
   _A_FILE_COMPRESSED    = $04000000;  // File is compressed
   _A_DONT_COMPRESS      = $08000000;  // Don't compress this file
   _A_CANT_COMPRESS      = $20000000;  // Can't compress this file
   _A_ATTR_ARCHIVE       = $40000000;  // Entry has had an EA modified
                                       // an ownerID changed, or trustee
                                       // info changed, etc.

// Attribute values usable during file creation
// Use: OR value with the file mode value to initialize the mode parameter
   FA_NORMAL  = _A_NORMAL shl 16;
   FA_RDONLY  = _A_RDONLY shl 16;
   FA_HIDDEN  = _A_HIDDEN shl 16;
   FA_SYSTEM  = _A_SYSTEM shl 16;
   FA_EXECUTE = _A_EXECUTE shl 16;
   FA_SUBDIR  = _A_SUBDIR shl 16;
   FA_ARCHIVE = _A_ARCH shl 16;
   FA_SHARE   = _A_SHARE shl 16;
{ Extended file attributes values  }
   FA_TRANSAC = _A_TRANS shl 12;
   FA_READAUD = _A_READAUD shl 12;
   FA_WRITEAUD= _A_WRITAUD shl 12;
   FA_IMMPURG = _A_IMMPURG shl 12;
   FA_NORENAM = _A_NORENAM shl 12;
   FA_NODELET = _A_NODELET shl 12;
   FA_NOCOPY  = _A_NOCOPY shl 12;
{ Sharing values for sharable open functions  }
{ compatibility mode    }
   SH_COMPAT = $00;
{ deny read/write mode  }
   SH_DENYRW = $10;
{ deny write mode       }
   SH_DENYWR = $20;
{ deny read mode        }
   SH_DENYRD = $30;
{ deny none mode        }
   SH_DENYNO = $40;
{ FEcreat/FEsopen flagBits parameter values used when creating a file  }
   DELETE_FILE_ON_CREATE_BIT        = $0001;
   NO_RIGHTS_CHECK_ON_CREATE_BIT    = $0002;
{ FEsopen flagBits parameter values used when opening a file  }
   FILE_WRITE_THROUGH_BIT           = $00000040;
   ENABLE_IO_ON_COMPRESSED_DATA_BIT = $00000100;
   LEAVE_FILE_COMPRESSED_DATA_BIT   = $00000200;
   DELETE_FILE_ON_CLOSE_BIT         = $00000400;
   NO_RIGHTS_CHECK_ON_OPEN_BIT      = $00010000;
   OK_TO_OPEN_DOS_FILE              = $80000000;
{ Volume Flags used with NWGetVolumeFlags and NWSetVolumeFlags  }
   SUB_ALLOCATION_FLAG         = $02;  // if set sub allocation units valid on this volume
   FILE_COMPRESSION_FLAG       = $04;  // if set file compression enabled on this volume
   DATA_MIGRATION_FLAG         = $08;  // if set data migration is allowed on this volume
   VOLUME_IMMEDIATE_PURGE_FLAG = $40;  // if set volume is marked as immediate purge
{ Name space values  }
   DOSNameSpace      = 0;
   MACNameSpace      = 1;
   NFSNameSpace      = 2;
   FTAMNameSpace     = 3;
   OS2NameSpace      = 4;
   LONGNameSpace     = 4;
   NTNameSpace       = 5;
   MAX_NAMESPACES    = 6;
   NWDOS_NAME_SPACE  = DOSNameSpace;
   NWMAC_NAME_SPACE  = MACNameSpace;
   NWNFS_NAME_SPACE  = NFSNameSpace;
   NWFTAM_NAME_SPACE = FTAMNameSpace;
   NWOS2_NAME_SPACE  = OS2NameSpace;
   NWLONG_NAME_SPACE = LONGNameSpace;
   NWNT_NAME_SPACE   = NTNameSpace;
{ Data stream values  }
   PrimaryDataStream = 0;
   MACResourceForkDataStream = 1;
   FTAMStructuringDataStream = 2;
{ File path length values  }
   _MAX_PATH = 255;           // maximum length of full pathname
   _MAX_SERVER = 48;          // maximum length of server name
   _MAX_VOLUME = 16;          // maximum length of volume component
   _MAX_DRIVE  = 3;           // maximum length of drive component
   _MAX_DIR    = 255;         // maximum length of path component
   _MAX_FNAME  = 9;           // maximum length of file name component
   _MAX_EXT    = 5;           // maximum length of extension component
   _MAX_NAME   = 13;          // maximum length of file name
   NAME_MAX    = 12;          // maximum length of file name (alternate view)

{ Modify structure mask values  }
   MModifyNameBit = $0001;
   MFileAttributesBit = $0002;
   MCreateDateBit = $0004;
   MCreateTimeBit = $0008;
   MOwnerIDBit = $0010;
   MLastArchivedDateBit = $0020;
   MLastArchivedTimeBit = $0040;
   MLastArchivedIDBit = $0080;
   MLastUpdatedDateBit = $0100;
   MLastUpdatedTimeBit = $0200;
   MLastUpdatedIDBit = $0400;
   MLastAccessedDateBit = $0800;
   MInheritanceRestrictionMaskBit = $1000;
   MMaximumSpaceBit = $2000;
   MLastUpdatedInSecondsBit = $4000;
{$include npackon.inc}
type
   PModifyStructure = ^TModifyStructure;
   TModifyStructure = record
        MModifyName : PBYTE;
        MFileAttributes : longint;
        MFileAttributesMask : longint;
        MCreateDate : word;
        MCreateTime : word;
        MOwnerID : longint;
        MLastArchivedDate : word;
        MLastArchivedTime : word;
        MLastArchivedID : longint;
        MLastUpdatedDate : word;
        MLastUpdatedTime : word;
        MLastUpdatedID : longint;
        MLastAccessedDate : word;
        MInheritanceGrantMask : word;
        MInheritanceRevokeMask : word;
        MMaximumSpace : longint;
        MLastUpdatedInSeconds : longint;
     end;

{$include npackoff.inc}

{-nwadv.h----------------------------------------------------------------------}
{$include npackon.inc}
{ Resource tag signatures for AllocateResourceTag  }

const
   AllocSignature                 = $54524C41;
   AESProcessSignature            = $50534541;
   CacheNonMovableMemorySignature = $544D4E43;
   ConsoleCommandSignature        = $4D4F4343;
   HardwareInterruptSignature     = $50544E49;
   InterruptTimeCallBackSignature = $524D4954;
   SemiPermMemorySignature        = $454D5053;
   DebuggerSignature              = $47554244;
   BreakpointSignature            = $54504B42;

type
   TCommandParserFunc = function (screenID    : scr_t;
                                  commandLine : PChar):longint;cdecl;
   PcommandParserStructure = ^TcommandParserStructure;
   TcommandParserStructure = record                 // Data structure for RegisterConsoleCommand
        Link         : PcommandParserStructure;     // set by RegisterConsoleCommand
        parseRoutine : TCommandParserFunc;          // parsing routing (user defined)
        RTag         : longint;                     // set to resource tag
     end;

{
   Structures and constants for RegisterForEvent function. Unless otherwise
   noted an event does NOT call a Warn routine.
 }

const
   EVENT_VOL_SYS_MOUNT = 0;
{ parameter is undefined. Report Routine will be called immediately
      after vol SYS has been mounted.
     }
   EVENT_VOL_SYS_DISMOUNT = 1;
{ parameter is undefined. Warn Routine and Report Routine will be
      called before vol SYS is dismounted.
     }
   EVENT_ANY_VOL_MOUNT = 2;
{ parameter is volume number. Report Routine will be called immediately
      after any volume is mounted.
     }
   EVENT_ANY_VOL_DISMOUNT = 3;
{ parameter is volume number. Warn Routine and Report Routine will be
      called before any volume is dismounted.
     }
   EVENT_DOWN_SERVER = 4;
{ parameter is undefined. Warn Routine and Report Routine will be
      called before the server is shut down.
     }
   EVENT_EXIT_TO_DOS = 7;
{ parameter is undefined. The Report Routine will be called before the
      server exits to DOS.
     }
   EVENT_MODULE_UNLOAD = 8;
{ parameter is module handle. Warn Routine and Report Routine will be
      called when a module is unloaded from the console command line. Only
      the Report Routine will be called when a module unloads itself.
     }
   EVENT_CLEAR_CONNECTION = 9;
{ parameter is connection number. Report Routine is called before the
      connection is cleared.
     }
   EVENT_LOGIN_USER = 10;
{ parameter is connection number. Report Routine is called after the
      connection has been allocated.
     }
   EVENT_CREATE_BINDERY_OBJ = 11;
{ parameter is object ID. Report Routine is called after the object is
      created and entered in the bindery.
     }
   EVENT_DELETE_BINDERY_OBJ = 12;
{ parameter is object ID. Report Routine is called before the object is
      removed from the bindery.
     }
   EVENT_CHANGE_SECURITY = 13;
{ parameter is a pointer a structure of type EventSecurityChangeStruct.
      Report Routine is called after a security
      equivalence change has occurred.
     }
   EVENT_ACTIVATE_SCREEN = 14;
{ Parameter is screen ID. Report routine is called after the
      screen becomes the active screen.
     }
   EVENT_UPDATE_SCREEN = 15;
{ Parameter is screen ID. Report routine is called after a change is
      made to the screen image.
     }
   EVENT_UPDATE_CURSOR = 16;
{ Parameter is screen ID. Report routine is called after a change to
      the cursor position or state occurs.
     }
   EVENT_KEY_WAS_PRESSED = 17;
{ Parameter is undefined. Report routine is called whenever a
      key on the keyboard is pressed (including shift/alt/control).
      This routine is called at interrupt time.
     }
   EVENT_DEACTIVATE_SCREEN = 18;
{ Parameter is screen ID. Report routine is called when the
      screen becomes inactive.
     }
   EVENT_TRUSTEE_CHANGE = 19;
{ Parameter is a pointer to type struct EventTrusteeChangeStruct. The
      report routine is called everytime there is a change to a trustee in
      the file system. Shouldn't sleep.
     }
   EVENT_OPEN_SCREEN = 20;
{ Parameter is the screen ID for the newly created screen. The report
      routine will be called after the screen is created.
     }
   EVENT_CLOSE_SCREEN = 21;
{ Parameter is the screen ID for the screen that will be closed. The
      report routine will be called before the screen is closed.
     }
   EVENT_MODIFY_DIR_ENTRY = 22;
{ Parameter is a pointer to a structure of type EventModifyDirEntryStruct
      which contains the modify information. The report routine will be
      called right after the entry is changed but before the directory
      entry is unlocked. The report routine must not go to sleep.
     }
   EVENT_NO_RELINQUISH_CONTROL = 23;
{ Parameter is the running process. This will be called when the
      timer detects that a process is hogging the processor. The report
      routine must not sleep.
     }
   EVENT_THREAD_SWITCH = 25;
{ Parameter is the threadID of the thread that was executing when the
      thread switch occurred. The report routine will be called when the
      new thread begins executing. The report routine must not go to sleep.
     }
   EVENT_MODULE_LOAD = 27;
{ parameter is module handle. The report routine will be called
      after a module has loaded.
     }
   EVENT_CREATE_PROCESS = 28;
{ parameter is the PID of the process being created. It is called
      after the process is created. The report routine may not sleep.
     }
   EVENT_DESTROY_PROCESS = 29;
{ parameter is the PID of the process being destroyed. It is called
      before the process is actually destroyed. The report routine may not
      sleep.
     }
   EVENT_NEW_PUBLIC = 32;
{ Parameter is a pointer to a length preceded string which is the name
      of the new public entry point. This event may not sleep.
     }
   EVENT_PROTOCOL_BIND = 33;
{ Parameter is a pointer to a structure of type EventProtocolBindStruct.
      This event is generated every time a board is bound to a protocol.
      This event may sleep.
     }
   EVENT_PROTOCOL_UNBIND = 34;
{ Parameter is a pointer to a structure of type EventProtocolBindStruct.
      This event is generated every time a board is unbound from a protocol.
      This event may sleep.
     }
   EVENT_ALLOCATE_CONNECTION = 37;
{ parameter is connection number. Report Routine is called after the
      connection is allocated.
     }
   EVENT_LOGOUT_CONNECTION = 38;
{ parameter is connection number. Report Routine is called before the
      connection is logged out. The event handler may sleep.
     }
   EVENT_MLID_REGISTER = 39;
{ parameter is board number. Report Routine is called after the MLID
      is registered.
     }
   EVENT_MLID_DEREGISTER = 40;
{ parameter is board number. Report Routine is called before the MLID
      is deregistered.
     }
   EVENT_DATA_MIGRATION = 41;
{ Parameter is a pointer to a structure of type EventDateMigrationInfo.
      This event is generated when a file's data has been migrated.
    }
   EVENT_DATA_DEMIGRATION = 42;
{ Parameter is a pointer to a structure of type EventDateMigrationInfo.
      This event is generated when a file's data has been de-migrated.
    }
   EVENT_QUEUE_ACTION = 43;
{   Parameter is a pointer to a structure of type EventQueueNote.
      This event is generated when a queue is activated, deactivated,
      created, or deleted.
    }
   EVENT_NETWARE_ALERT = 44;
{ Parameter is a pointer to a structure of type EventNetwareAlertStruct.
      This event is generated anytime the following alert calls are
      made:
              NetWareAlert        NW 4.X

        The report routine may sleep.
     }
   EVENT_CREATE_OBJECT = 46;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_DELETE_OBJECT = 47;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_RENAME_OBJECT = 48;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_VALUE_CHANGE = 49;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_CLOSE_FILE = 50;
{ Parameter is a pointer to a structure of type EventCloseFileInfo.  }
   EVENT_CHANGE_TIME = 51;
{ This event is given when the time is changed or when Time
      Synchronization schedules a nonuniform adjustment. The parameter is
      the UTC time (in seconds) before the time change. The current time
      is available from the OS. Since you have no way of knowing the
      magnitudue of the time change, nor whether it has taken place or is
      scheduled for the next clock interrupt, you must detect the time
      change on your own. In general, if current time is less than old
      time, or at least two seconds ahead of old time, then the time change
      has been applied. You must wait for one of those conditions to be
      sure that the time change has "settled down" before you can assume
      that the event has "happened."
     }
   EVENT_MOVE_OBJECT = 52;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_VALUE_ADD = 53;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_VALUE_DEL = 54;
{ Parameter is a pointer to a structure of type EventBinderyObject
      or EventDSObject
     }
   EVENT_DM_KEY_MODIFIED = 55;
{ Parameter is a pointer to a structure of type EventDMKeyModified
     }
   EVENT_MODULE_UNLOADED = 56;
{ Parameter is module handle. Report Routine will be called after the
      NLM's exit routine has been called, after his resources have been
      returned to the OS, and after he has been unlinked from the OS's lists.
      The only thing left of this NLM is the memory for his load definition
      structure, data image, and code image.
     }
   EVENT_REMOVE_PUBLIC = 57;
{ Parameter is the address of the public entry point. This only happens
      on module unload.
     }
   EVENT_DS_EVENT = 58;
{ Parameter is the address of a DS defined event structure   }
   EVENT_UNICODE = 59;
{ Parameter is the address of a UNICODE defined event structure  }
   EVENT_SFT3_SERVER_STATE = 60;
{ Parameter is the ServerState Number
      (Refer to messtype.h, server state codes)
      IOEngineState                          0
      PrimaryNoSecondaryState                1
      PrimarySyncingWithSecondaryState       2
      PrimaryTransferingMemoryImageState     3
      PrimaryWithSecondaryState              4
      SecondaryTransferingMemoryImageState   5
      SecondaryMirroredState                 6
    }
   EVENT_SFT3_IMAGE_STATE = 61;
{ Parameter is memory mirror state  }
{ 0 = Not mirrored  }
{ 1 = Mirrored  }
   EVENT_SFT3_PRESYNC_STATE = 62;
{ called when the primary is about ready to synchronize  }
{ with the secondary   }
{ Parameter is unsed for now.  }
{ This event report is allowed to sleep  }
   EVENT_ALTERNATE_MOUNT_VOLUME = 63;
{ called when NetWare is not aware of the volume name to be mounted,  }
{ Parameter is used to pass a event structure EventAlternateMountVolume. }
{ This event report is allowed to sleep, also the return code is in the  }
{ structre, after it has been processed.  }
   EVENT_CONSOLE_CONFIG_COMMAND = 64;
{ called when the console command CONFIG is typed on the server command  }
{ line. The event report is allowed to sleep.  The console screen handle  }
{ pointer is passed as the only parameter  }
   EVENT_CONSOLE_VERSION_COMMAND = 65;
{ called when the console command VERSION is typed on the server command  }
{ line. The event report is allowed to sleep.  A pointer to the structure  }
{ struct EventConfigVersionCmdInfo to help in the displaying to the screen  }
   EVENT_PRE_LOAD_NLM = 66;
{ called while an NLM is being loaded but before most of the work is
      done. The data and code segments have not been allocated yet. The
      event report is allowed to sleep. The parameter is a pointer to an
      NLM Load File Header structure.
     }
   EVENT_LOW_MEMORY = 67;
{ called when the cache memory allocator tries to allocate a cache block
      and fails; only one event per minute will be generated. It happens
      in conjunction with the netware alert. The event report can block.
      The parameter is a zero. This event is mainly for OS2 based NetWare
      so it can try to borrow memory back from OS2.
     }
{-----------------------------------------------------------
     Flags for the trustee change event (EVENT_TRUSTEE_CHANGE)
     ----------------------------------------------------------- }
   EVENT_NEW_TRUSTEE = 1;
   EVENT_REMOVE_TRUSTEE = 2;
{-------------------------------------------------------------
     Flags for the change security event (EVENT_CHANGE_SECURITY)
     ------------------------------------------------------------- }
   EVENT_ADD_EQUIVALENCE = 1;
   EVENT_REMOVE_EQUIVALENCE = 2;
{----------------------------------------------
     Structure returned for EVENT_TRUSTEE_CHANGE
     ---------------------------------------------- }
{ flags are EVENT_NEW_TRUSTEE and EVENT_REMOVE_TRUSTEE  }
type
   PEventTrusteeChangeStruct = ^TEventTrusteeChangeStruct;
   TEventTrusteeChangeStruct = record
        objectID : longint;
        entryID : longint;
        volumeNumber : longint;
        changeFlags : longint;
        newRights : longint;
     end;

{-----------------------------------------------
     Structure returned for EVENT_CHANGE_SECURITY
     ----------------------------------------------- }
{ EVENT_ADD_EQUIVALENCE and EVENT_REMOVE_EQUIVALENCE  }
   PEventSecurityChangeStruct = ^TEventSecurityChangeStruct;
   TEventSecurityChangeStruct = record
        objectID : longint;
        equivalentID : longint;
        changeFlags : longint;
     end;

{------------------------------------------------
     Structure returned for EVENT_MODIFY_DIR_ENTRY
     ------------------------------------------------ }
   PEventModifyDirEntryStruct = ^TEventModifyDirEntryStruct;
   TEventModifyDirEntryStruct = record
        primaryDirectoryEntry : longint;
        nameSpace : longint;
        modifyBits : longint;
        modifyVector : PModifyStructure;
        volumeNumber : longint;
        directoryEntry : longint;
     end;

{----------------------------------------------------
     Structure returned for EVENT_PROTOCOL_BIND & UNBIND
     ---------------------------------------------------- }
   PEventProtocolBindStruct = ^TEventProtocolBindStruct;
   TEventProtocolBindStruct = record
        boardNumber : longint;
        protocolNumber : longint;
     end;

{----------------------------------------------------------
     Structure returned for EVENT_DATA_MIGRATION & DEMIGRATION
     ---------------------------------------------------------- }
{ 255 + 1 len byte  }
   PEventDateMigrationInfo = ^TEventDateMigrationInfo;
   TEventDateMigrationInfo = record
        FileSystemTypeID : longint;
        Volume : longint;
        DOSDirEntry : longint;
        OwnerDirEntry : longint;
        OwnerNameSpace : longint;
        OwnerFileName : array[0..255] of byte;
     end;

{------------------------------------------------
     Structure returned for EVENT_QUEUE_ACTION
     ------------------------------------------------ }
{ 0=created, 1=deleted, 2 = activated, 3 = deactivated  }
   PEventQueueNote = ^TEventQueueNote;
   TEventQueueNote = record
        QAction : longint;
        QID : longint;
        QName : array[0..49] of byte;
     end;

{------------------------------------------------
     Structure returned for EVENT_NETWARE_ALERT
     ------------------------------------------------ }
   PEventNetwareAlertStruct = ^TEventNetwareAlertStruct;
   TEventNetwareAlertStruct = record
        alertFlags : longint;
        alertId : longint;
        alertLocus : longint;
        alertClass : longint;
        alertSeverity : longint;
        targetStationCount : longint;
        targetStationList : array[0..31] of longint;
        targetNotificationBits : longint;
        alertParmCount : longint;
        alertDataPtr : pointer;
        NetWorkManagementAttributePointer : pointer;
        alertUnused : array[0..1] of longint;
        alertControlStringMessageNumber : longint;
        alertControlString : array[0..255] of byte;
        alertParameters : array[0..(256 + 256)-1] of byte;
        alertModuleName : array[0..35] of byte;
        alertModuleMajorVersion : longint;
        alertModuleMinorVersion : longint;
        alertModuleRevision : longint;
     end;

{ set to 'BIND' for bindery  }
   PEventBinderyObject = ^TEventBinderyObject;
   TEventBinderyObject = record
        EventObjectType : longint;
        ObjectID : longint;
        ObjectType : longint;
     end;

{ 'DNIB'  }

const
   EventBinderySignature = $444e4942;
{ 'CVSD'  }
   EventDSSignature = $43565344;
{ set to 'DSVC' for directory services  }
{ add, delete, etc.  }
{ DS defined entry structure  }
type
   PEventDSObject = ^TEventDSObject;
   TEventDSObject = record
        EventObjectType : longint;
        EventType : longint;
        entry : pointer;
     end;

   PEventCloseFileInfo = ^TEventCloseFileInfo;
   TEventCloseFileInfo = record
        fileHandle : longint;
        station : longint;
        task : longint;
        fileHandleFlags : longint;
        completionCode : longint;
     end;

   TreportProcedure = procedure (parameter:longint; userParameter:longint); cdecl;
   TOutputRoutine   = procedure (controlString:pointer; args:array of const); cdecl;
   TWarnProcedure   = function  (OutputRoutine:TOutputRoutine; parameter,userParameter:longint):longint; cdecl;

{ struct EventCloseFileInfo's fileHandleFlags  }

const
   ECNotReadableBit = $00000001;
   ECNotWriteableBit = $00000002;
   ECWrittenBit = $00000004;
   ECDetachedBit = $00000008;
   ECDirectFileSystemBit = $00000020;
   ECFileWriteThroughBit = $00000040;
   HANDLEDCOMMAND  = 0;
   NOTMYCOMMAND    = 1;
{$include npackoff.inc}

type
  TRtag = longint;
  PRtag = ^TRtag;

function AllocateResourceTag (NLMHandle:TNlmHandle;
                              descriptionString:PChar;
                              resourceType:longint):longint;cdecl;external 'clib' name 'AllocateResourceTag';
function DSAllocateEventTag (DSEventSignature:longint):pointer;cdecl;external 'clib' name 'DSAllocateEventTag';
function GetCurrentOSLanguageID:longint;cdecl;external 'clib' name 'GetCurrentOSLanguageID';
function GetFileHoleMap (handle:longint;
                         startingPosition:longint;
                         numberOfBlocks:longint;
                         replyBitMapP:PBYTE;
                         allocationUnitSizeP:Plongint):longint;cdecl;external 'clib' name 'GetFileHoleMap';
function GetFileHoleMap (handle:longint;
                         startingPosition:longint;
                         numberOfBlocks:longint;
                         var replyBitMapP;
                         var allocationUnitSizeP:longint):longint;cdecl;external 'clib' name 'GetFileHoleMap';
function GetSetableParameterValue (connectionNumber:longint;
                                   setableParameterString:PChar;
                                   returnValue:pointer):longint;cdecl;external 'clib' name 'GetSetableParameterValue';
function GetSettableParameterValue (connectionNumber:longint;
                                    setableParameterString:PBYTE;
                                    returnValue:pointer):longint;cdecl;external 'clib' name 'GetSetableParameterValue';  // use this define if the misspelling is too annoying
function GetThreadDataAreaPtr:pointer;cdecl;external 'clib' name 'GetThreadDataAreaPtr';
function ImportSymbol(NLMHandle:TNlmHandle; symbolName:Pchar):pointer;cdecl;external 'clib' name 'ImportSymbol';
function LoadLanguageMessageTable(messageTable:PPPchar; messageCount:Plongint; languageID:Plongint):longint;cdecl;external 'clib' name 'LoadLanguageMessageTable';
function LoadLanguageMessageTable(var messageTable; var messageCount:longint; languageID:Plongint):longint;cdecl;external 'clib' name 'LoadLanguageMessageTable';
function NWAddSearchPathAtEnd(searchPath:PChar; number:Plongint):longint;cdecl;external 'clib' name 'NWAddSearchPathAtEnd';
function NWAddSearchPathAtEnd(searchPath:PChar; var number:longint):longint;cdecl;external 'clib' name 'NWAddSearchPathAtEnd';
function NWDeleteSearchPath(searchPathNumber:longint):longint;cdecl;external 'clib' name 'NWDeleteSearchPath';
function NWGetSearchPathElement (searchPathNumber:longint;
                                 isDOSSearchPath:Plongint;
                                 searchPath:PChar):longint;cdecl;external 'clib' name 'NWGetSearchPathElement';
function NWGetSearchPathElement (searchPathNumber:longint;
                                 var isDOSSearchPath:boolean;
                                 searchPath:PChar):longint;cdecl;external 'clib' name 'NWGetSearchPathElement';
function NWInsertSearchPath(searchPathNumber:longint; path:PChar):longint;cdecl;external 'clib' name 'NWInsertSearchPath';
function RegisterConsoleCommand(newCommandParser:PcommandParserStructure):longint;cdecl;external 'clib' name 'RegisterConsoleCommand';
function RegisterConsoleCommand(var newCommandParser:TcommandParserStructure):longint;cdecl;external 'clib' name 'RegisterConsoleCommand';
function RegisterForEvent (eventType:longint;
                           reportProcedure:TreportProcedure;
                           warnProcedure:TwarnProcedure):longint;cdecl;external 'clib' name 'RegisterForEvent';
function RenameLanguage (languageID:longint;
                         newLanguageName:PChar;
                         showErrorsToConsole:longint):longint;cdecl;external 'clib' name 'RenameLanguage';
function ReturnLanguageName(languageID:longint; languageName:PChar):longint;cdecl;external 'clib' name 'ReturnLanguageName';
procedure SaveThreadDataAreaPtr(threadDataAreaPtr:pointer);cdecl;external 'clib' name 'SaveThreadDataAreaPtr';
    { -1 for all, COMMUNICATIONS, MEMORY, etc  }
    { 0 for first time  }
    { 0 = number, 1 = boolean, 2 = time ticks., etc  }
    { STARTUP, HIDE, etc  }
    { COMMUNICATIONS, MEMORY, etc  }
    { description string  }
function ScanSetableParameters (scanCategory:longint;
                                scanSequence:Plongint;
                                rParameterName:PBYTE;
                                rType:Plongint;
                                rFlags:Plongint;
                                rCategory:Plongint;
                                rParameterDescription:pointer;
                                rCurrentValue:pointer;
                                rLowerLimit:Plongint;
                                rUpperLimit:Plongint):longint;cdecl;external 'clib' name 'ScanSetableParameters';
function SetCurrentOSLanguageID(newLanguageID:longint):longint;cdecl;external 'clib' name 'SetCurrentOSLanguageID';
function SetSetableParameterValue (connectionNumber:longint;
                                   setableParameterString:PBYTE;
                                   newValue:pointer):longint;cdecl;external 'clib' name 'SetSetableParameterValue';
procedure SynchronizeStart;cdecl;external 'clib' name 'SynchronizeStart';
function UnimportSymbol (NLMHandle:TNlmHandle;
                         symbolName:Pchar):longint;cdecl;external 'clib' name 'UnimportSymbol';
function UnRegisterConsoleCommand (commandParserToDelete:PcommandParserStructure):longint;cdecl;external 'clib' name 'UnRegisterConsoleCommand';
function UnRegisterConsoleCommand (var commandParserToDelete:TcommandParserStructure):longint;cdecl;external 'clib' name 'UnRegisterConsoleCommand';
function UnregisterForEvent (eventHandle:longint):longint;cdecl;external 'clib' name 'UnregisterForEvent';
{-nwfileio.h-------------------------------------------------------------------}
type
   PcacheBufferStructure = ^TcacheBufferStructure;
   TcacheBufferStructure = record
        cacheBufferPointer : Pchar;
        cacheBufferLength  : longint;
        completionCode     : longint;
     end;
   TT_cacheBufferStructure = TcacheBufferStructure;
   PT_cacheBufferStructure = ^TT_cacheBufferStructure;

   PmwriteBufferStructure = ^TmwriteBufferStructure;
   TmwriteBufferStructure = record
        mwriteBufferPointer : Pchar;
        mwriteBufferLength  : longint;
        reserved            : longint;
     end;
   TT_mwriteBufferStructure = TmwriteBufferStructure;
   PT_mwriteBufferStructure = ^TT_mwriteBufferStructure;

{ NetWare additions to POSIX...  }

function filelength(fildes:longint):longint;cdecl;external 'clib' name 'filelength';
function gwrite(fildes:longint; bufferP:PT_mwriteBufferStructure; numberBufs:longint; numberBufsWritten:plongint):longint;cdecl;external 'clib' name 'gwrite';
function gwrite(fildes:longint; var buffer:TT_mwriteBufferStructure; numberBufs:longint; var numberBufsWritten:longint):longint;cdecl;external 'clib' name 'gwrite';
function lock(fildes,offset,nbytes:longint):longint;cdecl;external 'clib' name 'lock';
function qread(fildes:longint; buffer:pointer; len,position:longint):longint;cdecl;external 'clib' name 'qread';
function qread(fildes:longint; var buffer; len,position:longint):longint;cdecl;external 'clib' name 'qread';
function qwrite(fildes:longint; buffer:pointer; len,position:longint):longint;cdecl;external 'clib' name 'qwrite';
function qwrite(fildes:longint; var buffer; len,position:longint):longint;cdecl;external 'clib' name 'qwrite';
function setmode(fildes,mode:longint):longint;cdecl;external 'clib' name 'setmode';
// sopen already in fcntl
//function sopen(path:Pchar; oflag,shflag:longint; args:array of const):longint;cdecl;external 'clib' name 'sopen';
//function sopen(path:Pchar; oflag,shflag:longint):longint;cdecl;external 'clib' name 'sopen';
function tell(fildes:longint):longint;cdecl;external 'clib' name 'tell';
function unlock(fildes,offset,nbytes:longint):longint;cdecl;external 'clib' name 'unlock';
{ other NetWare file I/O utilities...  }
function AsyncRead(handle:longint;
                   startingOffset:longint;
                   numberBytesToRead:longint;
                   numberBytesRead:plongint;
                   localSemaHandle:longint;
                   cacheBufferInfo:PT_cacheBufferStructure;
                   numOfCacheBufs:plongint):longint;cdecl;external 'clib' name 'AsyncRead';
function AsyncRead(handle:longint;
                   startingOffset:longint;
                   numberBytesToRead:longint;
                   var numberBytesRead:longint;
                   localSemaHandle:longint;
                   var cacheBufferInfo:TT_cacheBufferStructure;
                   var numOfCacheBufs:longint):longint;cdecl;external 'clib' name 'AsyncRead';

procedure AsyncRelease(cacheBufferInfo:PT_cacheBufferStructure);cdecl;external 'clib' name 'AsyncRelease';
procedure AsyncRelease(var cacheBufferInfo:TT_cacheBufferStructure);cdecl;external 'clib' name 'AsyncRelease';
function CountComponents(pathString:PChar; len:longint):longint;cdecl;external 'clib' name 'CountComponents';
function GetExtendedFileAttributes(pathName:Pchar; extFileAttrs:PBYTE):longint;cdecl;external 'clib' name 'GetExtendedFileAttributes';
function GetExtendedFileAttributes(pathName:Pchar; var extFileAttrs):longint;cdecl;external 'clib' name 'GetExtendedFileAttributes';
procedure _makepath(path,drive,dir,fname,ext:Pchar);cdecl;external 'clib' name '_makepath';
function NWGetVolumeFlags(volume:longint; flags:plongint):longint;cdecl;external 'clib' name 'NWGetVolumeFlags';
function NWGetVolumeFlags(volume:longint; var flags:longint):longint;cdecl;external 'clib' name 'NWGetVolumeFlags';
function NWSetVolumeFlags(volume,flags:longint):longint;cdecl;external 'clib' name 'NWSetVolumeFlags';
function ParsePath(path,server,volume,directories:Pchar):longint;cdecl;external 'clib' name 'ParsePath';
// SetReaddirAttribute already defined in dirent
//function SetReaddirAttribute(dirP:PDIR; newAttribute:dword):longint;cdecl;external 'clib' name 'SetReaddirAttribute';
procedure _splitpath(path,drive,dir,fname,ext:Pchar);cdecl;external 'clib' name '_splitpath';
procedure UseAccurateCaseForPaths(yesno:longint);cdecl;external 'clib' name 'UseAccurateCaseForPaths';
procedure UnAugmentAsterisk(yesno:longint);cdecl;external 'clib' name 'UnAugmentAsterisk';
{-nwfileeng.h------------------------------------------------------------------}
{ values for flags parameter in FEGetOpenFileInfo()...  }
const
   _NotReadableBit                     = $00000001;
   _NotWriteableBit                    = $00000002;
   _WrittenBit                         = $00000004;
   _DetachedBit                        = $00000008;
   _SwitchingToDirectFileSystemModeBit = $00000010;
   _DirectFileSystemModeBit            = $00000020;
   _FileWriteThroughBit                = $00000040;
{ extra flags  }
   _DiskBlockReturnedBit               = $00010000;
   _IAmOnTheOpenFileListBit            = $00020000;
   _FileReadAuditBit                   = $00040000;
   _FileWriteAuditBit                  = $00080000;
   _FileCloseAuditBit                  = $00100000;
   _DontFileWriteSystemAlertBit        = $00200000;
   _ReadAheadHintBit                   = $00400000;
   _NotifyCompressionOnCloseBit        = $00800000;
{ extra extra flags  }
   _IsWritingCompressedBit             = $01000000;
   _HasTimeDateBit                     = $02000000;
   _DoingDeCompressionBit              = $04000000;
   _NoSubAllocBit                      = $08000000;
   _IsATransactionFileBit              = $10000000;
   _HasFileWritePrivilegeBit           = $20000000;
   _TTSReadAuditBit                    = $40000000;
   _TTSWriteAuditBit                   = $80000000;

type
  TT_PathParseFunc = function (inputPath:Pchar;
                           var connectionIDp:word;
                           var volumeNumber:longint;
                           var directoryNumber:longint;
                               outPathStringP:PChar;
                           var outPathCount:longint):longint;cdecl;
  TVolumeNameString = String [17];

function FEConvertDirectoryNumber(sourceNameSpace:longint;
                                  volumeNumber:longint;
                                  sourceDirectoryNumber:longint;
                                  destinationNameSpace:longint;
                                  destinationDirectoryNumberP:Plongint):longint;cdecl;external 'clib' name 'FEConvertDirectoryNumber';
function FEConvertDirectoryNumber(sourceNameSpace:longint;
                                  volumeNumber:longint;
                                  sourceDirectoryNumber:longint;
                                  destinationNameSpace:longint;
                              var destinationDirectoryNumber:longint):longint;cdecl;external 'clib' name 'FEConvertDirectoryNumber';
function FEcreat(name:Pchar; permission,flagBits:longint):longint;cdecl;external 'clib' name 'FEcreat';
function FEFlushWrite(handle:longint):longint;cdecl;external 'clib' name 'FEFlushWrite';
function FEGetCWDnum:longint;cdecl;external 'clib' name 'FEGetCWDnum';
function FEGetCWVnum:longint;cdecl;external 'clib' name 'FEGetCWVnum';
function FEGetDirectoryEntry(volumeNumber,directoryNumber:longint; pathString:PChar;
                             pathCount,desiredNameSpace:longint;
                             namespaceDirectoryStructPp:Ppointer;
                             DOSdirectoryStructPp:Ppointer):longint;cdecl;external 'clib' name 'FEGetDirectoryEntry';
function FEGetDirectoryEntry(volumeNumber,directoryNumber:longint; pathString:PChar;
                             pathCount,desiredNameSpace:longint;
                             var namespaceDirectoryStructP:Pointer;
                             var DOSdirectoryStructP:Pointer):longint;cdecl;external 'clib' name 'FEGetDirectoryEntry';
function FEGetEntryVersion(volumeNumber,directoryNumber:longint; pathString:PChar; pathCount:longint; version:PWORD):longint;cdecl;external 'clib' name 'FEGetEntryVersion';
function FEGetEntryVersion(volumeNumber,directoryNumber:longint; pathString:PChar; pathCount:longint; var version:word):longint;cdecl;external 'clib' name 'FEGetEntryVersion';
function FEGetOpenFileInfo (connection:longint;
                            handle:longint;
                            volume:Plongint;
                            directoryNumber:Plongint;
                            dataStream:Plongint;
                            flags:Plongint):longint;cdecl;external 'clib' name 'FEGetOpenFileInfo';
function FEGetOpenFileInfo (connection:longint;
                            handle:longint;
                        var volume,directoryNumber,dataStream,flags:longint):longint;cdecl;external 'clib' name 'FEGetOpenFileInfo';
function FEGetOpenFileInfoForNS (connection, handle:longint;
                                 volume,DOSdirectoryNumber,directoryNumber:Plongint;
                                 nameSpace,dataStream,flags:Plongint):longint;cdecl;external 'clib' name 'FEGetOpenFileInfoForNS';
function FEGetOpenFileInfoForNS (connection, handle:longint;
                                 var volume,DOSdirectoryNumber,directoryNumber:longint;
                                 var nameSpace,dataStream,flags:longint):longint;cdecl;external 'clib' name 'FEGetOpenFileInfoForNS';
function FEGetOriginatingNameSpace(volumeNumber,directoryNumber:longint):longint;cdecl;external 'clib' name 'FEGetOriginatingNameSpace';
function FEMapConnsHandleToVolAndDir(connection,handle:longint; volumeNumberP,directoryNumberP:Plongint):longint;cdecl;external 'clib' name 'FEMapConnsHandleToVolAndDir';
function FEMapConnsHandleToVolAndDir(connection,handle:longint; var volumeNumber,directoryNumber:longint):longint;cdecl;external 'clib' name 'FEMapConnsHandleToVolAndDir';
function FEMapHandleToVolumeAndDirectory(handle:longint; volumeNumberP,directoryNumberP:PLongint):longint;cdecl;external 'clib' name 'FEMapHandleToVolumeAndDirectory';
function FEMapHandleToVolumeAndDirectory(handle:longint; var volumeNumberP,directoryNumberP:Longint):longint;cdecl;external 'clib' name 'FEMapHandleToVolumeAndDirectory';
function FEMapPathVolumeDirToVolumeDir(pathName:Pchar; volumeNumber,directoryNumber:longint; newVolumeNumberP,newDirectoryNumberP:Plongint):longint;cdecl;external 'clib' name 'FEMapPathVolumeDirToVolumeDir';
function FEMapPathVolumeDirToVolumeDir(pathName:Pchar; volumeNumber,directoryNumber:longint; var newVolumeNumberP,newDirectoryNumberP:longint):longint;cdecl;external 'clib' name 'FEMapPathVolumeDirToVolumeDir';

function FEMapVolumeAndDirectoryToPath(volumeNumber,directoryNumber:longint; pathString:PChar; pathCount:Plongint):longint;cdecl;external 'clib' name 'FEMapVolumeAndDirectoryToPath';
function FEMapVolumeAndDirectoryToPath(volumeNumber,directoryNumber:longint; pathString:PChar; var pathCount:longint):longint;cdecl;external 'clib' name 'FEMapVolumeAndDirectoryToPath';

function FEMapVolumeAndDirectoryToPathForNS(volumeNumber,directoryNumber:longint; nameSpace:longint; pathString:PBYTE; pathCount:Plongint):longint;cdecl;external 'clib' name 'FEMapVolumeAndDirectoryToPathForNS';
function FEMapVolumeNumberToName(volumeNumber:longint; volumeName:PChar):longint;cdecl;external 'clib' name 'FEMapVolumeNumberToName';
function FEMapVolumeNumberToName(volumeNumber:longint; var volumeName:TVolumeNameString):longint;cdecl;external 'clib' name 'FEMapVolumeNumberToName';
function FEQuickClose(connection,task,fileHandle:longint):longint;cdecl;external 'clib' name 'FEQuickClose';
function FEQuickFileLength(connection,handle:longint; fileSize:Plongint):longint;cdecl;external 'clib' name 'FEQuickFileLength';
function FEQuickFileLength(connection,handle:longint; var fileSize:longint):longint;cdecl;external 'clib' name 'FEQuickFileLength';
function FEQuickOpen (connection,task,volumeNumber,directoryNumber:longint;
                      pathString:PChar;
                      pathCount,nameSpace,attributeMatchBits,requestedAccessRights,dataStreamNumber:longint;
                      fileHandle:Plongint):longint;cdecl;external 'clib' name 'FEQuickOpen';
function FEQuickOpen (connection,task,volumeNumber,directoryNumber:longint;
                      pathString:PChar;
                      pathCount,nameSpace,attributeMatchBits,requestedAccessRights,dataStreamNumber:longint;
                      var fileHandle:longint):longint;cdecl;external 'clib' name 'FEQuickOpen';

function FEQuickRead (connection,handle,postition,bytesToRead:longint;
                      bytesRead:Plongint;
                      buffer:pointer):longint;cdecl;external 'clib' name 'FEQuickRead';
function FEQuickRead (connection,handle,postition,bytesToRead:longint;
                      var bytesRead:longint;
                      var buffer):longint;cdecl;external 'clib' name 'FEQuickRead';

function FEQuickWrite(connection,handle,position,bytesToWrite:longint; buffer:pointer):longint;cdecl;external 'clib' name 'FEQuickWrite';
function FEQuickWrite(connection,handle,position,bytesToWrite:longint; var buffer):longint;cdecl;external 'clib' name 'FEQuickWrite';

function FERegisterNSPathParser(normalFunc:TT_PathParseFunc):longint;cdecl;external 'clib' name 'FERegisterNSPathParser';
function FESetCWDnum(CWDnum:longint):longint;cdecl;external 'clib' name 'FESetCWDnum';
function FESetCWVandCWDnums(CWVnum:longint; CWDnum:longint):longint;cdecl;external 'clib' name 'FESetCWVandCWDnums';
function FESetCWVnum(CWVnum:longint):longint;cdecl;external 'clib' name 'FESetCWVnum';
function FESetOriginatingNameSpace(volumeNumber,directoryNumber,currentNameSpace,newNameSpace:longint):longint;cdecl;external 'clib' name 'FESetOriginatingNameSpace';
function FEsopen(name:Pchar; access,share,permission,flagBits:longint;
           dataStream:byte):longint;cdecl;external 'clib' name 'FEsopen';
function NWGetDirBaseFromPath(path:Pchar; nameSpace:byte; volNum,NSDirBase,DOSDirBase:Plongint):longint;cdecl;external 'clib' name 'NWGetDirBaseFromPath';
function NWGetDirBaseFromPath(path:Pchar; nameSpace:byte; var volNum,NSDirBase,DOSDirBase:longint):longint;cdecl;external 'clib' name 'NWGetDirBaseFromPath';
{-nwfinfo.h--------------------------------------------------------------------}
function FileServerFileCopy (sourceFileHandle,destinationFileHandle:longint;
                             sourceFileOffset,destinationFileOffset,numberOfBytesToCopy:longint;
                             numberOfBytesCopied:Plongint):longint;cdecl;external 'clib' name 'FileServerFileCopy';
function FileServerFileCopy (sourceFileHandle,destinationFileHandle:longint;
                             sourceFileOffset,destinationFileOffset,numberOfBytesToCopy:longint;
                             var numberOfBytesCopied:longint):longint;cdecl;external 'clib' name 'FileServerFileCopy';

function NWGetCompressedFileLengths (handle:longint;
                                     uncompressedLength,
                                     compressedLength:Plongint):longint;cdecl;external 'clib' name 'NWGetCompressedFileLengths';
function NWGetCompressedFileLengths (handle:longint;
                                     var uncompressedLength,
                                         compressedLength:longint):longint;cdecl;external 'clib' name 'NWGetCompressedFileLengths';

function NWGetDiskIOsPending:longint;cdecl;external 'clib' name 'NWGetDiskIOsPending';
function NWSetCompressedFileLengths (handle,
                                     uncompressedLength,
                                     compressedLengt:longint):longint;cdecl;external 'clib' name 'NWSetCompressedFileLengths';

function PurgeErasedFile(pathName:Pchar; sequenceNumber:longint):longint;cdecl;external 'clib' name 'PurgeErasedFile';
function SalvageErasedFile(pathName:Pchar; sequenceNumber:longint; newFileName:Pchar):longint;cdecl;external 'clib' name 'SalvageErasedFile';

function ScanErasedFiles_411 (path           : PChar;
                              nextEntryNumber: Plongint;
                              deletedFileInfo: PDIR):longint; cdecl;external 'clib' name 'ScanErasedFiles_411';
function ScanErasedFiles_411 (path               : PChar;
                              var nextEntryNumber: longint;
                              var deletedFileInfo: TDIR):longint; cdecl;external 'clib' name 'ScanErasedFiles_411';

function SetExtendedFileAttributes(pathName:Pchar; extendedFileAttributes:byte):longint;cdecl;external 'clib' name 'SetExtendedFileAttributes';
function SetFileInfo(pathName:Pchar; searchAttributes:byte; fileAttributes:longint; creationDate:Pchar; lastAccessDate:Pchar;
               lastUpdateDateAndTime:Pchar; lastArchiveDateAndTime:Pchar; fileOwnerID:longint):longint;cdecl;external 'clib' name 'SetFileInfo';
{-nwfshook.h-------------------------------------------------------------------}
{ ------------  File System Monitor Hook Call Back Numbers ------------
   The defined constants below that have _GEN_ in the name represent call back
   numbers that will hook Generic versions of the respective OS routines.
   Namely, routines that support Name Spaces other than DOS.
   --------------------------------------------------------------------- }

const
   FSHOOK_PRE_ERASEFILE            = 0;
   FSHOOK_PRE_OPENFILE             = 1;
   FSHOOK_PRE_CREATEFILE           = 2;
   FSHOOK_PRE_CREATE_OPENFILE      = 3;
   FSHOOK_PRE_RENAME_OR_MOVE       = 4;
   FSHOOK_PRE_CLOSEFILE            = 5;
   FSHOOK_PRE_CREATEDIR            = 6;
   FSHOOK_PRE_DELETEDIR            = 7;
   FSHOOK_PRE_MODIFY_DIRENTRY      = 8;
   FSHOOK_PRE_SALVAGE_DELETED      = 9;
   FSHOOK_PRE_PURGE_DELETED        = 10;
   FSHOOK_PRE_RENAME_NS_ENTRY      = 11;
   FSHOOK_PRE_GEN_SALVAGE_DELETED  = 12;
   FSHOOK_PRE_GEN_PURGE_DELETED    = 13;
   FSHOOK_PRE_GEN_OPEN_CREATE      = 14;
   FSHOOK_PRE_GEN_RENAME           = 15;
   FSHOOK_PRE_GEN_ERASEFILE        = 16;
   FSHOOK_PRE_GEN_MODIFY_DOS_INFO  = 17;
   FSHOOK_PRE_GEN_MODIFY_NS_INFO   = 18;
   FSHOOK_POST_ERASEFILE           = $80000000;
   FSHOOK_POST_OPENFILE            = $80000001;
   FSHOOK_POST_CREATEFILE          = $80000002;
   FSHOOK_POST_CREATE_OPENFILE     = $80000003;
   FSHOOK_POST_RENAME_OR_MOVE      = $80000004;
   FSHOOK_POST_CLOSEFILE           = $80000005;
   FSHOOK_POST_CREATEDIR           = $80000006;
   FSHOOK_POST_DELETEDIR           = $80000007;
   FSHOOK_POST_MODIFY_DIRENTRY     = $80000008;
   FSHOOK_POST_SALVAGE_DELETED     = $80000009;
   FSHOOK_POST_PURGE_DELETED       = $8000000A;
   FSHOOK_POST_RENAME_NS_ENTRY     = $8000000B;
   FSHOOK_POST_GEN_SALVAGE_DELETED = $8000000C;
   FSHOOK_POST_GEN_PURGE_DELETED   = $8000000D;
   FSHOOK_POST_GEN_OPEN_CREATE     = $8000000E;
   FSHOOK_POST_GEN_RENAME          = $8000000F;
   FSHOOK_POST_GEN_ERASEFILE       = $80000010;
   FSHOOK_POST_GEN_MODIFY_DOS_INFO = $80000011;
   FSHOOK_POST_GEN_MODIFY_NS_INFO  = $80000012;

    {--------------------------------------------------------------------
      Structure returned for
      FSHOOK_PRE_ERASEFILE and FSHOOK_POST_ERASEFILE
     -------------------------------------------------------------------- }
type

   PEraseFileCallBackStruct = ^TEraseFileCallBackStruct;
   TEraseFileCallBackStruct = record
     connection,
     task,
     volume,
     dirBase : Longint;
     pathString : Pchar;
     pathComponentCount,
     nameSpace,
     attributeMatchBits : Longint;
   end;
{--------------------------------------------------------------------
      Structure returned for
      FSHOOK_PRE_OPENFILE and FSHOOK_POST_OPENFILE
     -------------------------------------------------------------------- }

   POpenFileCallBackStruct = ^TOpenFileCallBackStruct;
   TOpenFileCallBackStruct = record
     connection,
     task,
     volume,
     dirBase : Longint;
     pathString : Pchar;
     pathComponentCount,
     nameSpace,
     attributeMatchBits,
     requestedAccessRights,
     dataStreamNumber : Longint;
     fileHandle : PLongint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_CREATEFILE and FSHOOK_POST_CREATEFILE
 -------------------------------------------------------------------- }

   PCreateFileCallBackStruct = ^TCreateFileCallBackStruct;
   TCreateFileCallBackStruct = record
     connection,
     task,
     volume,
     dirBase : Longint;
     pathString : Pchar;
     pathComponentCount,
     nameSpace,
     createAttributeBits,
     createFlagBits,
     dataStreamNumber : Longint;
     fileHandle : PLongint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_CREATE_OPENFILE and FSHOOK_POST_CREATE_OPENFILE
 -------------------------------------------------------------------- }

   PCreateAndOpenCallBackStruct = ^TCreateAndOpenCallBackStruct;
   TCreateAndOpenCallBackStruct = record
     connection,
     task,
     volume,
     dirBase : Longint;
     pathString : Pchar;
     pathComponentCount,
     nameSpace,
     createAttributeBits,
     requestedAccessRights,
     createFlagBits,
     dataStreamNumber : Longint;
     fileHandle       : PLongint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_RENAME_OR_MOVE and FSHOOK_POST_RENAME_OR_MOVE
 -------------------------------------------------------------------- }

   PRenameMoveEntryCallBackStruct = ^TRenameMoveEntryCallBackStruct;
   TRenameMoveEntryCallBackStruct = record
     connection,
     task,
     volume,
     dirBase                 : Longint;
     pathString              : Pchar;
     pathComponentCount,
     nameSpace,
     attributeMatchBits,
     subDirsOnlyFlag,
     newDirBase              : Longint;
     newPathString           : Pchar;
     originalNewCount,
     compatibilityFlag,
     allowRenamesToMyselfFlag: Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_CLOSEFILE and FSHOOK_POST_CLOSEFILE
 -------------------------------------------------------------------- }

   PCloseFileCallBackStruct = ^TCloseFileCallBackStruct;
   TCloseFileCallBackStruct = record
     connection,
     task,
     fileHandle : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_CREATEDIR and FSHOOK_POST_CREATEDIR
-------------------------------------------------------------------- }

   PCreateDirCallBackStruct = ^TCreateDirCallBackStruct;
   TCreateDirCallBackStruct = record
     connection,
     volume,
     dirBase    : Longint;
     pathString : Pchar;
     pathComponentCount,
     nameSpace,
     directoryAccessMask : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_DELETEDIR and FSHOOK_POST_DELETEDIR
 -------------------------------------------------------------------- }

   PDeleteDirCallBackStruct = ^TDeleteDirCallBackStruct;
   TDeleteDirCallBackStruct = record
     connection,
     volume,
     dirBase : Longint;
     pathString : PChar;
     pathComponentCount,
     nameSpace : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_MODIFYDIRENTRY and FSHOOK_POST_MODIFYDIRENTRY
 -------------------------------------------------------------------- }

   PModifyDirEntryCallBackStruct = ^TModifyDirEntryCallBackStruct;
   TModifyDirEntryCallBackStruct = record
     connection,
     task,
     volume,
     dirBase            : Longint;
     pathString         : PChar;
     pathComponentCount,
     nameSpace,
     attributeMatchBits,
     targetNameSpace    : Longint;
     modifyVector       : PModifyStructure;
     modifyBits,
     allowWildCardsFlag : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_SALVAGE_DELETED and FSHOOK_POST_SALVAGE_DELETED
 -------------------------------------------------------------------- }

   PSalvageDeletedCallBackStruct = ^TSalvageDeletedCallBackStruct;
   TSalvageDeletedCallBackStruct = record
     connection,
     volume,
     dirBase,
     toBeSalvagedDirBase,
     nameSpace            : Longint;
     newName              : PChar;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_PURGE_DELETED and FSHOOK_POST_PURGE_DELETED
 -------------------------------------------------------------------- }

   PPurgeDeletedCallBackStruct = ^TPurgeDeletedCallBackStruct;
   TPurgeDeletedCallBackStruct = record
     connection,
     volume,
     dirBase,
     toBePurgedDirBase,
     nameSpace          : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_RENAME_NS_ENTRY and FSHOOK_POST_RENAME_NS_ENTRY
 -------------------------------------------------------------------- }

   PRenameNSEntryCallBackStruct = ^TRenameNSEntryCallBackStruct;
   TRenameNSEntryCallBackStruct = record
     connection,
     task,
     volume,
     dirBase       : Longint;
     pathString    : PChar;
     pathComponentCount,
     nameSpace,
     matchBits     : Longint;
     newName       : PChar;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_GEN_SALVAGE_DELETED and FSHOOK_POST_GEN_SALVAGE_DELETED
 -------------------------------------------------------------------- }

   PGenericSalvageDeletedCBStruct = ^TGenericSalvageDeletedCBStruct;
   TGenericSalvageDeletedCBStruct = record
     connection,
     nameSpace,
     sequence,
     volume,
     dirBase : Longint;
     newName : PChar;
  end;
{--------------------------------------------------------------------
      Structure returned for
      FSHOOK_PRE_GEN_PURGE_DELETED and FSHOOK_POST_GEN_PURGE_DELETED
     -------------------------------------------------------------------- }

   PGenericPurgeDeletedCBStruct = ^TGenericPurgeDeletedCBStruct;
   TGenericPurgeDeletedCBStruct = record
     connection,
     nameSpace,
     sequence,
     volume,
     dirBase  : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_GEN_OPEN_CREATE and FSHOOK_POST_GEN_OPEN_CREATE
 -------------------------------------------------------------------- }

   PGenericOpenCreateCBStruct = ^TGenericOpenCreateCBStruct;
   TGenericOpenCreateCBStruct = record
     connection,
     task,
     volume,
     pathComponentCount,
     dirBase               : Longint;
     pathString            : Pchar;
     nameSpace,
     dataStreamNumber,
     openCreateFlags,
     searchAttributes,
     createAttributes,
     requestedAccessRights,
     returnInfoMask        : Longint;
     fileHandle            : PLongint;
     openCreateAction      : Pointer;
   end;
{--------------------------------------------------------------------
 Structure returned for
 FSHOOK_PRE_GEN_RENAME and FSHOOK_POST_GEN_RENAME
 -------------------------------------------------------------------- }

   PGenericRenameCBStruct = ^TGenericRenameCBStruct;
   TGenericRenameCBStruct = record
     connection,
     task,
     nameSpace,
     renameFlag,
     searchAttributes,
     srcVolume,
     srcPathComponentCount,
     srcDirBase            : Longint;
     srcPathString         : Pchar;
     dstVolume,
     dstPathComponentCount,
     dstDirBase            : Longint;
     dstPathString         : Pchar;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_GEN_ERASEFILE and FSHOOK_POST_GEN_ERASEFILE
 -------------------------------------------------------------------- }

   PGenericEraseFileCBStruct = ^TGenericEraseFileCBStruct;
   TGenericEraseFileCBStruct = record
     connection,
     task,
     volume,
     pathComponentCount,
     dirBase          : Longint;
     pathString       : Pchar;
     nameSpace,
     searchAttributes : Longint;
   end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_GEN_MODIFY_DOS_INFO and FSHOOK_POST_GEN_MODIFY_DOS_INFO
 -------------------------------------------------------------------- }

   PGenericModifyDOSInfoCBStruct = ^TGenericModifyDOSInfoCBStruct;
   TGenericModifyDOSInfoCBStruct = record
        connection,
        task,
        volume,
        pathComponentCount,
        dirBase              : Longint;
        pathString           : Pchar;
        nameSpace            : Longint;
        searchAttributes     : Longint;
        modifyMask           : Longint;
        modifyInfo           : pointer;
     end;
{--------------------------------------------------------------------
  Structure returned for
  FSHOOK_PRE_GEN_MODIFY_NS_INFO and FSHOOK_POST_GEN_MODIFY_NS_INFO
-------------------------------------------------------------------- }

   PGenericModifyNSInfoCBStruct = ^TGenericModifyNSInfoCBStruct;
   TGenericModifyNSInfoCBStruct = record
     connection,
     task,
     dataLength,
     srcNameSpace,
     dstNameSpace,
     volume,
     dirBase,
     modifyMask   : Longint;
     modifyInfo   : pointer;
   end;

function NWAddFSMonitorHook (callBackNumber:Longint;
                             callBackFunc:pointer;
                             callBackHandle:PLongint):Longint;cdecl;external 'clib' name 'NWAddFSMonitorHook';
function NWAddFSMonitorHook (callBackNumber:Longint;
                             callBackFunc:pointer;
                         var callBackHandle:Longint):Longint;cdecl;external 'clib' name 'NWAddFSMonitorHook';
function NWRemoveFSMonitorHook (callBackNumber,callBackHandle:Longint):Longint;cdecl;external 'clib' name 'NWRemoveFSMonitorHook';
{-nwipx.h----------------------------------------------------------------------}
{$include npackon.inc}
{ ECB status field completion codes  }

const
   STS_SPX_CONNECTION_TERMINATED = $FFEC;
   STS_SPX_TERMINATED_POORLY = $FFED;
   STS_SPX_INVALID_CONNECTION = $FFEE;
   STS_SPX_CONNECTION_TABLE_FULL = $FFEF;
   STS_SPX_SOCKET_NOT_OPEN = $FFF0;
   STS_SPX_SOCKET_ALREADY_OPEN = $FFF1;
   STS_SPX_ECB_CANNOT_BE_CANCELLED = $FFF9;
   STS_SPX_NO_KNOWN_ROUTE_TO_DESTINATION = $FFFA;
   STS_SPX_EVENT_CANCELLED = $FFFC;
   STS_SPX_PACKET_OVERFLOW = $FFFD;
   STS_SPX_MALFORMED_PACKET = $FFFE;
   STS_SPX_TRANSMIT_FAILURE = $FFFF;

   SPX_SSTATUS_ABORTED = $00;
   SPX_SSTATUS_WAITING = $01;
   SPX_SSTATUS_STARTING = $02;
   SPX_SSTATUS_ESTABLISHED = $03;
   SPX_SSTATUS_TERMINATING = $04;


{
   This define is for the Queued IPX/SPX Calls. The return code passed in will
   be set to this value until the packet is actually sent to IPX/SPX.
 }
   PACKET_IN_QUEUE = $0001;
{---------------------------------------------------------------------------

     IPX_ECB status field busy (in-process) codes:

     0x11 - AES (asynchronous event service) waiting
     0x12 - Holding
     0x13 - Session listen
     0x14 - Processing
     0x15 - Receiving
     0x16 - Sending
     0x17 - Waiting

  -------------------------------------------------------------------------- }
{---------------------------------------------------------------------------
     The comment characters in the IPX_ECB structure have the
     following meanings
     s - this field must be filled in prior to a send
     r - this field must be filled in prior to a receive
     R - this field is reserved
     A - this field may be used when the ECB is not in use by IPX/SPX
     q - the application may read this field
  -------------------------------------------------------------------------- }
{ Packet type codes  }
   UNKNOWN_PACKET_TYPE = 0;
   ROUTING_INFORMATION_PACKET = 1;
   ECHO_PACKET = 2;
   ERROR_PACKET = 3;
   PACKET_EXCHANGE_PACKET = 4;
   SEQUENCED_PACKET_PROTOCOL_PACKET = 5;

   SPX_END_OF_MESSAGE = $10;
   ENABLE_WATCHDOG    = $ff;

{ various SAP definitions  }

   SAP_SOCKET = $0452;
   GENERAL_SERVICE_QUERY = 1;
   GENERAL_SERVICE_RESPONSE = 2;
   NEAREST_SERVICE_QUERY = 3;
   NEAREST_SERVICE_RESPONSE = 4;
   PERIODIC_ID_PACKET = 2;
   NOT_SUPPORTED = 1;
   INVALID_QUERY_TYPE = 2;
   SAP_RESPONSES_PER_PACKET = 8;
{ 'ELRS'  }
   QUERY_LIST_SIGNATURE = $454C5253;
{ type definitions  }
type
   TMisalignedLONG = longint;
   TMisalignedWORD = word;

   PtagECBFrag = ^TtagECBFrag;
   TtagECBFrag = record
        fragAddress : pointer;
        fragSize    : longint;
     end;
   TECBFrag = TtagECBFrag;
   PECBFrag = ^TECBFrag;

   PIPX_ECBStruct = ^TIPX_ECBStruct;
   TIPX_ECBStruct = record
        semHandleSave : longint;
        queueHead : ^PIPX_ECBStruct;
        next : PIPX_ECBStruct;
        prev : PIPX_ECBStruct;
        status : word;
        semHandle : longint;
        lProtID : word;
        protID : array[0..5] of byte;
        boardNumber : longint;
        immediateAddress : array[0..5] of byte;
        driverWS : array[0..3] of byte;
        ESREBXValue : longint;
        socket : word;
        protocolWorkspace : word;
        dataLen : longint;
        fragCount : longint;
        fragList : array[0..1] of TECBFrag;
     end;
   TIPX_ECB  = TIPX_ECBStruct;
   PIPX_ECB  = ^TIPX_ECB;
   PPIPX_ECB = ^PIPX_ECB;

   TSPX_ECB  = TIPX_ECBStruct;
   PSPX_ECB  = PIPX_ECBStruct;


   PtagInternetAddress = ^TtagInternetAddress;
   TtagInternetAddress = record
        network : TMisalignedLONG;
        node    : array[0..5] of byte;
        socket  : TMisalignedWORD;
     end;
   TInternetAddress = TtagInternetAddress;
   PInternetAddress = ^TInternetAddress;

   PtagIPX_HEADER = ^TtagIPX_HEADER;
   TtagIPX_HEADER = record
        checksum    : word;
        packetLen   : word;
        transportCtl: byte;
        packetType  : byte;
        destNet     : TMisalignedLONG;
        destNode    : array[0..5] of byte;
        destSocket  : word;
        sourceNet   : TMisalignedLONG;
        sourceNode  : array[0..5] of byte;
        sourceSocket: word;
     end;
   TIPX_HEADER = TtagIPX_HEADER;
   PIPX_HEADER = ^TIPX_HEADER;
{ included only for compatibility  }

   PtagIPX_STATS = ^TtagIPX_STATS;
   TtagIPX_STATS = record
        dummy : char;
     end;
   TIPX_STATS = TtagIPX_STATS;
   PIPX_STATS = ^TIPX_STATS;

   PtagSPX_HEADER = ^TtagSPX_HEADER;
   TtagSPX_HEADER = record
        checksum : word;
        packetLen : word;
        transportCtl : byte;
        packetType : byte;
        destNet : TMisalignedLONG;
        destNode : array[0..5] of byte;
        destSocket : word;
        sourceNet : TMisalignedLONG;
        sourceNode : array[0..5] of byte;
        sourceSocket : word;
        connectionCtl : byte;
        dataStreamType : byte;
        sourceConnectID : word;
        destConnectID : word;
        sequenceNumber : word;
        ackNumber : word;
        allocNumber : word;
     end;
   TSPX_HEADER = TtagSPX_HEADER;
   PSPX_HEADER = ^TSPX_HEADER;

   PSPX_ConnStruct = ^TSPX_ConnStruct;
   TSPX_ConnStruct = record
        sStatus : byte;
        sFlags : byte;
        sSourceConnectID : word;
        sDestConnectID : word;
        sSequenceNumber : word;
        sAckNumber : word;
        sAllocNumber : word;
        sRemoteAckNumber : word;
        sRemoteAllocNumber : word;
        sLocalSocket : word;
        sImmediateAddress : array[0..5] of byte;
        sRemoteNet : longint;
        sRemoteNode : array[0..5] of byte;
        sRemoteSocket : word;
        sRetransmitCount : byte;
        sRetransmitMax : byte;
        sRoundTripTimer : word;
        sRetransmittedPackets : word;
        sSuppressedPackets : word;
        sLastReceiveTime : word;
        sLastSendTime : word;
        sRoundTripMax : word;
        sWatchdogTimeout : word;
        sSessionXmitQHead : array[0..3] of byte;
        sSessionXmitECBp : array[0..3] of byte;
     end;
   TSPX_SESSION = TSPX_ConnStruct;
   PSPX_SESSION = ^TSPX_SESSION;


type

   PT_SAP_ID_PACKET = ^TT_SAP_ID_PACKET;
   TT_SAP_ID_PACKET = record
     SAPPacketType       : word;                    // 2 or 4
     serverType          : word;                    // assigned by novell
     serverName          : array[0..47] of byte;    // Service name
     serverAddress       : TInternetAddress;
     interveningNetworks : word;                    // # of networks packets must pass
   end;

   PSERVICE_QUERY_PACKET = ^TSERVICE_QUERY_PACKET;
   TSERVICE_QUERY_PACKET = record
     queryType,                                     // 1 or 3
     serverType : word;                             // assigned by novell
   end;

   PSAPResponse = ^TSAPResponse;
   TSAPResponse = record
        SAPPacketType : word;                       // 2 or 4
        responses     : array[0..(SAP_RESPONSES_PER_PACKET)-1] of record
             serverType          : word;
             serverName          : array[0..47] of byte;
             serverAddress       : TInternetAddress;
             interveningNetworks : word;
          end;
        next          : PSAPResponse;
        signature     : longint;
        count         : longint;
     end;
   TSAP_RESPONSE_LIST_ENTRY = TSAPResponse;
   PSAP_RESPONSE_LIST_ENTRY = ^TSAP_RESPONSE_LIST_ENTRY;
{$include npackoff.inc}


{ IPX function prototypes...  }

function IpxCheckSocket(socket:word):longint;cdecl;external 'clib' name 'IpxCheckSocket';
function IpxCancelEvent(ECBp:PIPX_ECB):longint;cdecl;external 'clib' name 'IpxCancelEvent';
function IpxCloseSocket(socket:word):longint;cdecl;external 'clib' name 'IpxCloseSocket';
function IpxConnect(ECBp:PIPX_ECB):longint;cdecl;external 'clib' name 'IpxConnect';
function IpxDisconnect(ECBp:PIPX_ECB):longint;cdecl;external 'clib' name 'IpxDisconnect';
function IpxGetAndClearQ(replyQptr:PPIPX_ECB):PIPX_ECB;cdecl;external 'clib' name 'IpxGetAndClearQ';
function IpxGetInternetworkAddress(address:PBYTE):longint;cdecl;external 'clib' name 'IpxGetInternetworkAddress';
function IpxGetLocalTarget(address:PBYTE; ECBp:PIPX_ECB; timeToNet:Plongint):longint;cdecl;external 'clib' name 'IpxGetLocalTarget';
function IpxGetStatistics(ipxStats:PIPX_STATS):longint;cdecl;external 'clib' name 'IpxGetStatistics';
function IpxGetVersion(majorVersion:PBYTE; minorVersion:PBYTE; revision:PWORD):longint;cdecl;external 'clib' name 'IpxGetVersion';
function IpxGetVersion(var majorVersion,minorVersion:byte; var revision:word):longint;cdecl;external 'clib' name 'IpxGetVersion';
function IpxOpenSocket(socketP:PWORD):longint;cdecl;external 'clib' name 'IpxOpenSocket';
function IpxOpenSocket(var socket:word):longint;cdecl;external 'clib' name 'IpxOpenSocket';
function IpxQueuedSend(socket:word; ECBp:PIPX_ECB; rcode:Plongint):longint;cdecl;external 'clib' name 'IpxQueuedSend';
function IpxQueuedSend(socket:word; ECBp:PIPX_ECB; var rcode:longint):longint;cdecl;external 'clib' name 'IpxQueuedSend';
function IpxQueuedReceive(socket:word; ECBp:PIPX_ECB; rcode:Plongint):longint;cdecl;external 'clib' name 'IpxQueuedReceive';
function IpxQueuedReceive(socket:word; ECBp:PIPX_ECB; var rcode:longint):longint;cdecl;external 'clib' name 'IpxQueuedReceive';
function IpxReceive(socket:word; ECBp:PIPX_ECB):longint;cdecl;external 'clib' name 'IpxReceive';
function IpxResetStatistics:longint;cdecl;external 'clib' name 'IpxResetStatistics';
function IpxSend(socket:word; ECBp:PIPX_ECB):longint;cdecl;external 'clib' name 'IpxSend';
{ SPX function prototypes...  }
function SpxAbortConnection(connection:word):longint;cdecl;external 'clib' name 'SpxAbortConnection';
function SpxCancelEvent(ecb:PSPX_ECB):longint;cdecl;external 'clib' name 'SpxCancelEvent';
function SpxCheckSocket(socket:word):longint;cdecl;external 'clib' name 'SpxCheckSocket';
function SpxCloseSocket(socket:word):longint;cdecl;external 'clib' name 'SpxCloseSocket';
function SpxEstablishConnection(socket:word; ecb:PSPX_ECB; retryCount:byte; watchDogFlag:byte; connection:PWORD):longint;cdecl;external 'clib' name 'SpxEstablishConnection';
function SpxEstablishConnection(socket:word; ecb:PSPX_ECB; retryCount:byte; watchDogFlag:byte; var connection:word):longint;cdecl;external 'clib' name 'SpxEstablishConnection';
function SpxGetConfiguration(maxConn,availConn:Plongint):longint;cdecl;external 'clib' name 'SpxGetConfiguration';
function SpxGetConfiguration(var maxConn,availConn:longint):longint;cdecl;external 'clib' name 'SpxGetConfiguration';
function SpxGetConnectionStatus(connection:word; buffer:PSPX_SESSION):longint;cdecl;external 'clib' name 'SpxGetConnectionStatus';
{ (56 bytes)  }
function SpxGetTime(marker:Plongint):longint;cdecl;external 'clib' name 'SpxGetTime';
function SpxGetTime(var marker:longint):longint;cdecl;external 'clib' name 'SpxGetTime';
function SpxGetVersion(major,minor:PBYTE; revision:PWORD; revDate:Plongint):longint;cdecl;external 'clib' name 'SpxGetVersion';
function SpxGetVersion(var major,minor:byte; var revision:word; var revDate:longint):longint;cdecl;external 'clib' name 'SpxGetVersion';
function SpxListenForConnection(socket:word; ecb:PSPX_ECB; retryCount:byte; watchDogFlag:byte; connection:PWORD):longint;cdecl;external 'clib' name 'SpxListenForConnection';
function SpxListenForConnection(socket:word; ecb:PSPX_ECB; retryCount:byte; watchDogFlag:byte; var connection:word):longint;cdecl;external 'clib' name 'SpxListenForConnection';
function SpxListenForConnectedPacket(socket:word; ecb:PSPX_ECB; connection:word):longint;cdecl;external 'clib' name 'SpxListenForConnectedPacket';
function SpxListenForSequencedPacket(socket:word; ecb:PSPX_ECB):longint;cdecl;external 'clib' name 'SpxListenForSequencedPacket';
function SpxOpenSocket(socket:PWORD):longint;cdecl;external 'clib' name 'SpxOpenSocket';
function SpxOpenSocket(var socket:word):longint;cdecl;external 'clib' name 'SpxOpenSocket';
function SpxQueuedListenForSequencedPacket(socket:word; ecb:PSPX_ECB; rcode:Plongint):longint;cdecl;external 'clib' name 'SpxQueuedListenForSequencedPacket';
function SpxQueuedListenForSequencedPacket(socket:word; ecb:PSPX_ECB; var rcode:longint):longint;cdecl;external 'clib' name 'SpxQueuedListenForSequencedPacket';
function SpxQueuedSendSequencedPacket(connection:word; ecb:PSPX_ECB; rcode:Plongint):longint;cdecl;external 'clib' name 'SpxQueuedSendSequencedPacket';
function SpxQueuedSendSequencedPacket(connection:word; ecb:PSPX_ECB; var rcode:longint):longint;cdecl;external 'clib' name 'SpxQueuedSendSequencedPacket';
function SpxSendSequencedPacket(connection:word; ecb:PSPX_ECB):longint;cdecl;external 'clib' name 'SpxSendSequencedPacket';
function SpxTerminateConnection(connection:word; ecb:PSPX_ECB):longint;cdecl;external 'clib' name 'SpxTerminateConnection';
{ SAP function prototypes...  }
function AdvertiseService(serviceType:word; serviceName:Pchar; serviceSocket:word):longint;cdecl;external 'clib' name 'AdvertiseService';
function FreeQueryServicesList(listP:PSAP_RESPONSE_LIST_ENTRY):longint;cdecl;external 'clib' name 'FreeQueryServicesList';
function QueryServices(queryType,serviceType:word):PSAP_RESPONSE_LIST_ENTRY;cdecl;external 'clib' name 'QueryServices';
function ShutdownAdvertising(advertisingHandle:longint):longint;cdecl;external 'clib' name 'ShutdownAdvertising';
{-nwlib.h----------------------------------------------------------------------}
type
  TLibraryCleanupFunc = function (dataAreaPtr:pointer):longint; cdecl;

function __get_thread_data_area_ptr:Plongint;cdecl;external 'clib' name '__get_thread_data_area_ptr';
function GetDataAreaPtr(libraryHandle:longint):pointer;cdecl;external 'clib' name 'GetDataAreaPtr';
function DeregisterLibrary(libraryHandle:longint):longint;cdecl;external 'clib' name 'DeregisterLibrary';
function RegisterLibrary(cleanupFunc:TLibraryCleanupFunc):longint;cdecl;external 'clib' name 'RegisterLibrary';
function SaveDataAreaPtr(libraryHandle:longint; dataAreaPtr:pointer):longint;cdecl;external 'clib' name 'SaveDataAreaPtr';
// function Thread_Data_Area : pointer;  Thread_Data_Area (*__get_thread_data_area_ptr())
{-nwmalloc.h-------------------------------------------------------------------}
procedure NWGarbageCollect (NLMHandle:TNlmHandle);  cdecl; external 'clib' name 'NWGarbageCollect';
function  NWGetAllocPageOverhead (pageCount:longint):longint;cdecl; external 'clib' name 'NWGetAllocPageOverhead';
function  NWGetAvailableMemory : longint;        cdecl; external 'clib' name 'NWGetAvailableMemory';
function  NWGetPageSize : longint;               cdecl; external 'clib' name 'NWGetPageSize';
function  NWMemorySizeAddressable (addr:pointer; size:longint):longint;cdecl; external 'clib' name 'NWMemorySizeAddressable';
function  alloca (size:longint):pointer;         cdecl; external 'clib' name 'alloca';
function  _msize (buffer:pointer):longint;       cdecl; external 'clib' name '_msize';
function  __qcalloc (num,siz:longint):pointer;   cdecl; external 'clib' name '__qcalloc';
function  __qmalloc (siz:longint):pointer;       cdecl; external 'clib' name '__qmalloc';
function  __qrealloc (old:pointer; siz:longint):pointer;cdecl; external 'clib' name '__qrealloc';
function  stackavail:longint;                    cdecl; external 'clib' name 'stackavail';

function  calloc  (num,siz:longint):pointer;     cdecl; external 'clib' name 'calloc';
procedure free    (p:pointer);                   cdecl; external 'clib' name 'free';
function  malloc  (siz:longint):pointer;         cdecl; external 'clib' name 'malloc';
function  realloc (oldMemP:pointer;
                   newsize:longint):pointer;     cdecl; external 'clib' name 'realloc';
{-nwncpx.h---------------------------------------------------------------------}
const
   MAX_NCP_EXTENSION_NAME_BYTES = 33;
   BEGIN_SCAN_NCP_EXTENSIONS = $FFFFFFFF;
   REPLY_BUFFER_IS_FRAGGED = $FFFFFFFF;
   CONNECTION_BEING_RESTARTED = $01101001;
   CONNECTION_BEING_KILLED = $02202002;
   CONNECTION_BEING_LOGGED_OUT = $03303003;
   CONNECTION_BEING_FREED = $04404004;
type
   PNCPExtensionClient = ^TNCPExtensionClient;
   TNCPExtensionClient = record
        connection : longint;
        task : longint;
     end;

   PFragElement = ^TFragElement;
   TFragElement = record
        ptr : pointer;
        size : longint;
     end;

   PNCPExtensionMessageFrag = ^TNCPExtensionMessageFrag;
   TNCPExtensionMessageFrag = record
        totalMessageSize : longint;
        fragCount : longint;
        fragList : array[0..3] of TFragElement;
     end;

   TNCPExtensionHandlerFunc =
     function (NCPExtensionClient:PNCPExtensionClient;
               requestData:pointer;
               requestDataLen:longint;
               replyData:pointer;
               replyDataLen:Plongint):byte; cdecl;
   TConnectionEventHandlerProc =
     procedure (connection:longint; eventType:longint); cdecl;

   TReplyBufferManagerProc =
     procedure (NCPExtensionClient:PNCPExtensionClient; replyBuffer:pointer); cdecl;

function NWDeRegisterNCPExtension(queryData:pointer):longint;cdecl;external name 'NWDeRegisterNCPExtension';
function NWGetNCPExtensionInfo(NCPExtensionName:Pchar; NCPExtensionID:Plongint; majorVersion:PBYTE; minorVersion:PBYTE; revision:PBYTE;
           queryData:pointer):longint;cdecl;external  name 'NWGetNCPExtensionInfo';
function NWGetNCPExtensionInfoByID(NCPExtensionID:longint; NCPExtensionName:Pchar; majorVersion:PBYTE; minorVersion:PBYTE; revision:PBYTE;
           queryData:pointer):longint;cdecl;external  name 'NWGetNCPExtensionInfoByID';
function NWRegisterNCPExtension(NCPExtensionName:Pchar;
                                NCPExtensionHandler:TNCPExtensionHandlerFunc;
                                ConnectionEventHandler:TConnectionEventHandlerProc;
                                ReplyBufferManager:TReplyBufferManagerProc;
                                majorVersion,minorVersion,revision:byte;
                                queryData:Ppointer):longint;cdecl;external  name 'NWRegisterNCPExtension';

function NWRegisterNCPExtensionByID(NCPExtensionID:longint;
                                    NCPExtensionName:Pchar;
                                    NCPExtensionHandler:TNCPExtensionHandlerFunc;
                                    ConnectionEventHandler:TConnectionEventHandlerProc;
                                    ReplyBufferManager:TReplyBufferManagerProc;
                                    majorVersion,minorVersion,revision:byte; queryData:Ppointer):longint;cdecl;external  name 'NWRegisterNCPExtensionByID';

function NWScanNCPExtensions(NCPExtensionID:Plongint; NCPExtensionName:Pchar; majorVersion:PBYTE; minorVersion:PBYTE; revision:PBYTE;
           queryData:pointer):longint;cdecl;external  name 'NWScanNCPExtensions';
function NWSendNCPExtensionFraggedRequest(NCPExtensionID:longint; requestFrag:PNCPExtensionMessageFrag; replyFrag:PNCPExtensionMessageFrag):longint;cdecl;external  name 'NWSendNCPExtensionFraggedRequest';
function NWSendNCPExtensionRequest(NCPExtensionID:longint; requestData:pointer; requestDataLen:longint; replyData:pointer; replyDataLen:Plongint):longint;cdecl;external  name 'NWSendNCPExtensionRequest';
{-nwnspace.h-------------------------------------------------------------------}
{$include npackon.inc}
type

   PNWNSINFO = ^TNWNSINFO;
   TNWNSINFO = record
     nsInfoBitMask,
     fixedBitMask,
     reservedBitMask,
     extendedBitMask     : longint;
     fixedBitsDefined,
     reservedBitsDefined,
     extendedBitsDefined : word;
     fieldsLenTable      : array[0..31] of longint;
     hugeStateInfo       : array[0..15] of byte;
     hugeDataLength      : longint;
   end;
   TNW_NS_INFO = TNWNSINFO;
   PNW_NS_INFO = ^TNW_NS_INFO;

{$include npackoff.inc}

function GetDataStreamName(volume:longint; dataStream:byte; name:Pchar; numberOfDataStreams:Plongint):longint;cdecl;external 'clib' name 'GetDataStreamName';
function GetDataStreamName(volume:longint; dataStream:byte; name:Pchar; var numberOfDataStreams:longint):longint;cdecl;external 'clib' name 'GetDataStreamName';
function GetNameSpaceName(volume:longint; nameSpace:longint; name:Pchar; numberOfNameSpaces:Plongint):longint;cdecl;external 'clib' name 'GetNameSpaceName';
function GetNameSpaceName(volume:longint; nameSpace:longint; name:Pchar; var numberOfNameSpaces:longint):longint;cdecl;external 'clib' name 'GetNameSpaceName';
function NWGetHugeNSInfo(volNum,nameSpace:byte; dirBase,hugeInfoMask:longint;
                         hugeStateInfo:PBYTE;
                         hugeData:PBYTE;
                         hugeDataLen:PLongint;
                         nextHugeStateInfo:PBYTE):longint;cdecl;external 'clib' name 'NWGetHugeNSInfo';
function NWGetHugeNSInfo(volNum,nameSpace:byte; dirBase,hugeInfoMask:longint;
                         var hugeStateInfo;
                         var hugeData;
                         var hugeDataLen:longint;
                         var nextHugeStateInfo):longint;cdecl;external 'clib' name 'NWGetHugeNSInfo';
function NWGetNameSpaceEntryName(path:PBYTE; nameSpace:longint; maxNameBufferLength:longint; nameSpaceEntryName:PChar):longint;cdecl;external 'clib' name 'NWGetNameSpaceEntryName';
function NWGetNameSpaceEntryName(var path:byte; nameSpace:longint; maxNameBufferLength:longint; nameSpaceEntryName:PChar):longint;cdecl;external 'clib' name 'NWGetNameSpaceEntryName';
function NWGetNSInfo(volNum:byte; srcNameSpace:byte; dstNameSpace:byte; dirBase:longint; nsInfoMask:longint;
           nsSpecificInfo:PBYTE):longint;cdecl;external 'clib' name 'NWGetNSInfo';
function NWGetNSLoadedList(volNum:byte; loadListSize:word; NSLoadedList:PBYTE; returnListSize:PWORD):longint;cdecl;external 'clib' name 'NWGetNSLoadedList';
function NWQueryNSInfoFormat(nameSpace,volNum:byte; nsInfo:PNW_NS_INFO):longint;cdecl;external 'clib' name 'NWQueryNSInfoFormat';
function NWQueryNSInfoFormat(nameSpace,volNum:byte; var nsInfo:TNW_NS_INFO):longint;cdecl;external 'clib' name 'NWQueryNSInfoFormat';
function NWSetHugeNSInfo(volNum,nameSpace:byte; dirBase:longint;
                         hugeInfoMask:longint; hugeStateInfo:PBYTE;
                         hugeDataLen:longint;
                         hugeData:PBYTE;
                         nextHugeStateInfo:PBYTE;
                         hugeDataUsed:PLongint):longint;cdecl;external 'clib' name 'NWSetHugeNSInfo';
function NWSetHugeNSInfo(volNum,nameSpace:byte; dirBase:longint;
                         hugeInfoMask:longint; var hugeStateInfo;
                         hugeDataLen:longint;
                         var hugeData;
                         var nextHugeStateInfo;
                         var hugeDataUsed:longint):longint;cdecl;external 'clib' name 'NWSetHugeNSInfo';

function NWSetNameSpaceEntryName(path:PChar; nameSpace:longint; nameSpaceEntryName:PChar):longint;cdecl;external 'clib' name 'NWSetNameSpaceEntryName';
function NWSetNSInfo(volNum,srcNameSpace,dstNameSpace:byte; dirBase:longint;
                     nsInfoMask:longint;
                     nsSpecificInfoLen:longint;
                     nsSpecificInfo:Pointer):longint;cdecl;external 'clib' name 'NWSetNSInfo';
function NWSetNSInfo(volNum,srcNameSpace,dstNameSpace:byte; dirBase:longint;
                     nsInfoMask:longint;
                     nsSpecificInfoLen:longint;
                     var nsSpecificInfo):longint;cdecl;external 'clib' name 'NWSetNSInfo';
function SetCurrentNameSpace(newNameSpace:byte):byte;cdecl;external 'clib' name 'SetCurrentNameSpace';
function SetTargetNameSpace(newNameSpace:byte):byte;cdecl;external 'clib' name 'SetTargetNameSpace';
{-nwproc.h---------------------------------------------------------------------}
type TStdfds = array[0..2] of longint;

function CreateChildProcess (func:pointer;
                             threadName:PChar;
                             cmdLine:PChar;
                             arg:PChar;
                             stack:pointer;
                             stackSize:longint;
                             stdfds:TStdfds;
                             clearenv:longint;
                             procName:PChar;
                             enableApp:longint):Tpid_t;cdecl;external 'clib' name 'CreateChildProcess';
function KillChildProcess(pid:Tpid_t):longint;cdecl;external 'clib' name 'KillChildProcess';
function WaitOnChildProcess(pid:Tpid_t; statloc:Plongint; options:longint):longint;cdecl;external 'clib' name 'WaitOnChildProcess';
function WaitOnChildProcess(pid:Tpid_t; var statloc:longint; options:longint):longint;cdecl;external 'clib' name 'WaitOnChildProcess';
{-nwsemaph.h-------------------------------------------------------------------}
function CloseLocalSemaphore       (semaHandle : longint)       : longint; cdecl; external 'clib' name 'CloseLocalSemaphore';
function ExamineLocalSemaphore     (semaHandle : longint)       : longint; cdecl; external 'clib' name 'ExamineLocalSemaphore';
function OpenLocalSemaphore        (initValue  : longint)       : longint; cdecl; external 'clib' name 'OpenLocalSemaphore';
function SignalLocalSemaphore      (semaHandle : longint)       : longint; cdecl; external 'clib' name 'SignalLocalSemaphore';
function TimedWaitOnLocalSemaphore (semaHandle,Timeout:longint) : longint; cdecl; external 'clib' name 'TimedWaitOnLocalSemaphore';
function WaitOnLocalSemaphore      (semaHandle : longint)       : longint; cdecl; external 'clib' name 'WaitOnLocalSemaphore';
{-signal.h---------------------------------------------------------------------}
{ #define SIG_IGN            (void (*)(int)) 1 }
{ #define SIG_DFL            (void (*)(int)) 2 }
{ #define SIG_ERR            (void (*)(int)) 3 }
const
    SIGABRT = 1;
    SIGFPE = 2;
    SIGILL = 3;
    SIGINT = 4;
    SIGSEGV = 5;
    SIGTERM = 6;
    SIGPOLL = 7;
 { currently unimplemented POSIX-mandated signals  }
    SIGKILL = 101;
    SA_NOCLDSTOP = 102;
    SIGALRM = 103;
    SIGCHILD = 104;
    SIGCONT = 105;
    SIGHUP = 106;
    SIGPIPE = 107;
    SIGQUIT = 108;
    SIGSTOP = 109;
    SIGTSTP = 110;
    SIGTTIN = 111;
    SIGTTOU = 112;
    SIGUSR1 = 113;
    SIGUSR2 = 114;
    SIG_BLOCK = 115;
    SIG_SETMASK = 116;
    SIG_UNBLOCK = 117;
 { Novell-defined signals  }
    SIG_FINI = 500;
    SIG_IPBIND = 501;
    SIG_IPUNBIND = 502;
    SIG_IPXBIND = 503;
    SIG_IPXUNBIND = 504;
    SIG_IPREGISTER = 505;
    SIG_IPUNREGISTER = 506;
    SIG_IPXREGISTER = 507;
    SIG_IPXUNREGISTER = 508;
    SIG_LOCALECHANGE = 510;

type
   Psig_atomic_t = ^Tsig_atomic_t;
   Tsig_atomic_t = longint;
   TSigHandlerProc = procedure (Sig:longint); cdecl;

function _raise(para1:longint):longint;cdecl;external 'clib' name 'raise';
function signal(sig:longint; func:TSigHandlerProc):longint;cdecl;external 'clib' name 'signal';

function nw_raise(para1:longint):longint;cdecl;external 'clib' name 'raise';
function nw_signal(sig:longint; func:TSigHandlerProc):longint;cdecl;external 'clib' name 'signal';
{-nwstring.h-------------------------------------------------------------------}
function ASCIIZToLenStr (lenString,ASCIIZstring:Pchar) : longint; cdecl;external 'clib' name 'ASCIIZToLenStr';
function ASCIIZToMaxLenStr (lenString,ASCIIZstring:Pchar; maximumLength:longint):longint; cdecl;external 'clib' name 'ASCIIZToMaxLenStr';
function IntSwap (w:word):word; cdecl;external 'clib' name 'IntSwap';
function LenStrCat (destStr,srcStr:Pchar):Pchar; cdecl;external 'clib' name 'LenStrCat';
function LenStrCmp (s1,s2:Pchar):longint; cdecl;external 'clib' name 'LenStrCmp';
function LenStrCpy (dest,src:Pchar):Pchar; cdecl;external 'clib' name 'LenStrCpy';
function LenToASCIIZStr (ASCIIZstring,lenString:Pchar) : longint; cdecl;external 'clib' name 'LenToASCIIZStr';
function LongSwap (l:longint) : longint; cdecl;external 'clib' name 'LongSwap';
{-nwtoolib.h-------------------------------------------------------------------}
type
  TreadFunc           = function :longint; cdecl;
  TNLMBeginFunc       = procedure (NLMID:longint; commandLine:Pchar); cdecl;
  TNLMPreEndFunc      = procedure (NLMID:longint); cdecl;
  TNLMPostEndFunc     = procedure (NLMID:longint); cdecl;
  TNLMEndNoContextFunc= procedure (NLMID:longint); cdecl;
  TthreadGroupBeginFunc=function (threadGroupID:longint; argc:longint; const argv:array of Pchar):longint; cdecl;
  TthreadGroupEndFunc = procedure (threadGroupID:longint); cdecl;
  TthreadBeginFunc    = function (threadID:longint):longint;  cdecl;
  TthreadEndFunc      = procedure (threadID:longint); cdecl;
  TthreadReleaseFileResourcesFunc = procedure (threadID:longint); cdecl;

function _NWGetErrno:longint;cdecl;external 'clib' name '_NWGetErrno';
function _NWGetNWErrno:longint;cdecl;external 'clib' name '_NWGetNWErrno';
function _NWGetNLMLevelLibDataPtr(NLMID:longint):pointer;cdecl;external 'clib' name '_NWGetNLMLevelLibDataPtr';
function _NWGetThreadGroupLevelLibDataPtr(threadGroupID:longint):pointer;cdecl;external 'clib' name '_NWGetThreadGroupLevelLibDataPtr';
function _NWGetThreadLevelLibDataPtr(threadID:longint):pointer;cdecl;external 'clib' name '_NWGetThreadLevelLibDataPtr';
function _NWLoadNLMMessageTable(NLMHandle:TNlmHandle; messageTable:PPPchar; messageCount:Plongint; languageID:Plongint):longint;cdecl;external 'clib' name '_NWLoadNLMMessageTable';
function _NWRegisterNLMLibrary (NLMHandle:TNlmHandle;
                                NLMFileHandle:longint;
                                readFunc:TReadFunc;
                                NLMBegin:TNLMBeginFunc;
                                NLMPreEnd:TNLMPreEndFunc;
                                NLMPostEnd:TNLMPostEndFunc;
                                NLMEndNoContext:TNLMEndNoContextFunc;
                                threadGroupBegin:TthreadGroupBeginFunc;
                                threadGroupEnd:TthreadGroupEndFunc;
                                threadBegin:TthreadBeginFunc;
                                threadEnd:TthreadEndFunc;
                                threadReleaseFileResources:TthreadReleaseFileResourcesFunc ):longint;cdecl;external 'clib' name '_NWRegisterNLMLibrary';
procedure _NWSetErrno   (errnoValue : longint);cdecl;external 'clib' name '_NWSetErrno';
procedure _NWSetNWErrno (NWErrnoValue : longint);cdecl;external 'clib' name '_NWSetNWErrno';
procedure _NWSetNLMLevelLibDataPtr (NLMID : longint; dataPtr : pointer);cdecl;external 'clib' name '_NWSetNLMLevelLibDataPtr';
procedure _NWSetThreadGroupLevelLibDataPtr (threadGroupID : longint; dataPtr : pointer);cdecl;external 'clib' name '_NWSetThreadGroupLevelLibDataPtr';
procedure _NWSetThreadLevelLibDataPtr (threadID : longint; dataPtr : pointer);cdecl;external 'clib' name '_NWSetThreadLevelLibDataPtr';
{-stdio.h----------------------------------------------------------------------}
type
   Tva_list = pointer;  // dont know what this is

   Pfpos_t = ^Tfpos_t;
   Tfpos_t = longint;

   Pwchar_t = ^Twchar_t;
   Twchar_t = word;

   Pwint_t = ^Twint_t;
   Twint_t = longint;
{ values for fseek()' whence argument  }
{ add 'offset' to beginning of file         }

const
   SEEK_SET = 0;
{ add 'offset' to current position in file  }
   SEEK_CUR = 1;
{ add 'offset' to end of file               }
   SEEK_END = 2;
{ miscellaneous definitions  }
{ at least this many FILEs available        }
   FOPEN_MAX = 20;
{ (extreme) default buffer size             }
   BUFSIZ = 1024;
{ max number of characters in a path name   }
   FILENAME_MAX = 1024;
{ definitions for tmpnam() and tmpfil()  }
{ "_T-00000.TMP" to "_T-99999.TMP"          }
   TMP_MAX = 100000;
{ 8 + 1 + 3 + 1 (always DOS namespace)      }
   L_tmpnam = 13;
{ values for field '_flag' in FILE below  }
{ currently reading                         }
   _IOREAD = $0001;
{ currently writing                         }
   _IOWRT = $0002;
{ opened for reading and writing            }
   _IORW = $0004;
{ binary file (O_BINARY)                    }
   _IOBIN = $0008;
{ unbuffered (e.g.: stdout and stderr)      }
   _IONBF = $0010;
{ line buffered (e.g.: stdin)               }
   _IOLBF = $0020;
{ fully buffered (most files)               }
   _IOFBF = $0040;
{ EOF reached on read                       }
   _IOEOF = $0080;
{ I/O error from system                     }
   _IOERR = $0100;
{ stdio code malloc()'d this buffer         }
   _IOBUF = $0200;
{ was a temporary file by tmpfile()         }
   _IOTMP = $0400;
{ file stream structure                     }
type
   Piobuf = ^Tiobuf;
   Tiobuf = record
        _signature : dword;    { identifies this structure                 }
        _avail : longint;      { available (unused/unread) room in buffer  }
        _ptr : Pbyte;          { next character from/to here in buffer     }
        _base : Pbyte;         { the buffer (not really)                   }
        _oflag : dword;        { pre-CLib.NLM v4.11 compatibility          }
        _file : dword;         { file descriptor                           }
        _flag : dword;         { state of stream                           }
        _buf : array[0..3] of
                  byte;        { fake, micro buffer as a fall-back         }
        _env : dword;          { Macintosh or UNIX text file signature     }
     end;
   TFILE = Tiobuf;
   PFILE = ^TFILE;
   PPFILE = ^PFILE;


{ ISO/ANSI C defined functions...  }
procedure clearerr(para1:PFILE);cdecl;external 'clib' name 'clearerr';
procedure clearerr(var para1:TFILE);cdecl;external 'clib' name 'clearerr';
function fclose(para1:PFILE):longint;cdecl;external 'clib' name 'fclose';
function fclose(var para1:TFILE):longint;cdecl;external 'clib' name 'fclose';
function feof(para1:PFILE):longint;cdecl;external 'clib' name 'feof';
function feof(var para1:TFILE):longint;cdecl;external 'clib' name 'feof';
function ferror(para1:PFILE):longint;cdecl;external 'clib' name 'ferror';
function ferror(var para1:TFILE):longint;cdecl;external 'clib' name 'ferror';
function fflush(para1:PFILE):longint;cdecl;external 'clib' name 'fflush';
function fflush(var para1:TFILE):longint;cdecl;external 'clib' name 'fflush';
function fgetc(para1:PFILE):char;cdecl;external 'clib' name 'fgetc';
function fgetc(var para1:TFILE):char;cdecl;external 'clib' name 'fgetc';
function fgetpos(para1:PFILE; para2:Pfpos_t):longint;cdecl;external 'clib' name 'fgetpos';
function fgetpos(var para1:TFILE; var para2:Tfpos_t):longint;cdecl;external 'clib' name 'fgetpos';
function fgets(para1:Pchar; para2:longint; para3:PFILE):Pchar;cdecl;external 'clib' name 'fgets';
function fgets(para1:Pchar; para2:longint; var para3:TFILE):Pchar;cdecl;external 'clib' name 'fgets';
function fopen(para1,para2:Pchar):PFILE;cdecl;external 'clib' name 'fopen';
function fprintf(para1:PFILE; para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'fprintf';
function fprintf(var para1:TFILE; para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'fprintf';
function fprintf(para1:PFILE; para2:Pchar):longint;cdecl;external 'clib' name 'fprintf';
function fprintf(var para1:TFILE; para2:Pchar):longint;cdecl;external 'clib' name 'fprintf';
function fputc(para1:longint; para2:PFILE):longint;cdecl;external 'clib' name 'fputc';
function fputs(para1:Pchar; para2:PFILE):longint;cdecl;external 'clib' name 'fputs';
function fread(para1:pointer; para2:Tsize_t; para3:Tsize_t; para4:PFILE):Tsize_t;cdecl;external 'clib' name 'fread';
function freopen(para1:Pchar; para2:Pchar; para3:PFILE):PFILE;cdecl;external 'clib' name 'freopen';
function fscanf(para1:PFILE; para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'fscanf';
function fscanf(var para1:TFILE; para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'fscanf';
function fscanf(para1:PFILE; para2:Pchar):longint;cdecl;external 'clib' name 'fscanf';
function fscanf(var para1:TFILE; para2:Pchar):longint;cdecl;external 'clib' name 'fscanf';
function fseek(fp:PFILE; offset:longint; whence:longint):longint;cdecl;external 'clib' name 'fseek';
function fseek(var fp:TFILE; offset:longint; whence:longint):longint;cdecl;external 'clib' name 'fseek';
(* Const before type ignored *)
function fsetpos(para1:PFILE; para2:Pfpos_t):longint;cdecl;external 'clib' name 'fsetpos';
function fsetpos(var para1:TFILE; para2:Pfpos_t):longint;cdecl;external 'clib' name 'fsetpos';
function ftell(para1:PFILE):longint;cdecl;external 'clib' name 'ftell';
function ftell(var para1:TFILE):longint;cdecl;external 'clib' name 'ftell';

function fwrite(para1:pointer; para2:Tsize_t; para3:Tsize_t; para4:PFILE):Tsize_t;cdecl;external 'clib' name 'fwrite';

function getc(para1:PFILE):char;cdecl;external 'clib' name 'getc';
function getc(var para1:TFILE):char;cdecl;external 'clib' name 'getc';

function getchar:char;cdecl;external 'clib' name 'getchar';
function gets(para1:Pchar):Pchar;cdecl;external 'clib' name 'gets';
procedure perror(para1:Pchar);cdecl;external 'clib' name 'perror';
function printf(para1:Pchar; args:array of const):longint;cdecl;external 'clib' name 'printf';
function printf(para1:Pchar):longint;cdecl;external 'clib' name 'printf';
function putc(para1:char; para2:PFILE):longint;cdecl;external 'clib' name 'putc';
function putc(para1:char; var para2:TFILE):longint;cdecl;external 'clib' name 'putc';

function putchar(para1:char):longint;cdecl;external 'clib' name 'putchar';

function puts(para1:Pchar):longint;cdecl;external 'clib' name 'puts';
function remove(para1:Pchar):longint;cdecl;external 'clib' name 'remove';
function rename(para1, para2:Pchar):longint;cdecl;external 'clib' name 'rename';
procedure rewind(para1:PFILE);cdecl;external 'clib' name 'rewind';
procedure rewind(var para1:TFILE);cdecl;external 'clib' name 'rewind';

function scanf(para1:Pchar; args:array of const):longint;cdecl;external 'clib' name 'scanf';
function scanf(para1:Pchar):longint;cdecl;external 'clib' name 'scanf';
procedure setbuf(para1:PFILE; para2:Pchar);cdecl;external 'clib' name 'setbuf';
procedure setbuf(var para1:TFILE; para2:Pchar);cdecl;external 'clib' name 'setbuf';
function setvbuf(para1:PFILE; para2:Pchar; para3:longint; para4:Tsize_t):longint;cdecl;external 'clib' name 'setvbuf';
function setvbuf(para1:TFILE; para2:Pchar; para3:longint; para4:Tsize_t):longint;cdecl;external 'clib' name 'setvbuf';

function sprintf(para1,para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'sprintf';
function sprintf(para1,para2:Pchar):longint;cdecl;external 'clib' name 'sprintf';
function sscanf(para1, para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'sscanf';
function sscanf(para1, para2:Pchar):longint;cdecl;external 'clib' name 'sscanf';
function tmpfile:PFILE;cdecl;external 'clib' name 'tmpfile';
function tmpnam(para1:Pchar):Pchar;cdecl;external 'clib' name 'tmpnam';
function ungetc(para1:longint; para2:PFILE):longint;cdecl;external 'clib' name 'ungetc';
function ungetc(para1:longint; var para2:TFILE):longint;cdecl;external 'clib' name 'ungetc';

function vfprintf(para1:PFILE; para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'vfprintf';
function vfprintf(var para1:TFILE; para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'vfprintf';

function vfscanf(para1:PFILE; para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'vfscanf';
function vfscanf(var para1:TFILE; para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'vfscanf';

function vprintf(para1:Pchar; para2:Tva_list):longint;cdecl;external 'clib' name 'vprintf';
function vscanf(para1:Pchar; para2:Tva_list):longint;cdecl;external 'clib' name 'vscanf';
function vsprintf(para1,para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'vsprintf';
function vsscanf(para1, para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'vsscanf';
{ POSIX-defined additions...  }
function fdopen(para1:longint; para2:Pchar):PFILE;cdecl;external 'clib' name 'fdopen';
function fileno(fp:PFILE):longint;cdecl;external 'clib' name 'fileno';
function fileno(var f:TFILE):longint;cdecl;external 'clib' name 'fileno';
//function cgets(para1:Pchar):Pchar;cdecl;external 'clib' name 'cgets';
//function cprintf(para1:Pchar; args:array of const):longint;cdecl;external 'clib' name 'cprintf';
//function cprintf(para1:Pchar):longint;cdecl;external 'clib' name 'cprintf';
//function cputs(para1:Pchar):longint;cdecl;external 'clib' name 'cputs';
//function cscanf(para1:Pchar; args:array of const):longint;cdecl;external 'clib' name 'cscanf';
//function cscanf(para1:Pchar):longint;cdecl;external 'clib' name 'cscanf';
function fcloseall:longint;cdecl;external 'clib' name 'fcloseall';
function fgetchar:longint;cdecl;external 'clib' name 'fgetchar';
function flushall:longint;cdecl;external 'clib' name 'flushall';
function fputchar(para1:longint):longint;cdecl;external 'clib' name 'fputchar';
function vcprintf(para1:Pchar; para2:Tva_list):longint;cdecl;external 'clib' name 'vcprintf';
function vcscanf(para1:Pchar; para2:Tva_list):longint;cdecl;external 'clib' name 'vcscanf';
function NWfprintf(var para1:TFILE; para2:Pchar; args:array of const):longint;cdecl;external 'clib' name 'NWfprintf';
function NWfprintf(var para1:TFILE; para2:Pchar):longint;cdecl;external 'clib' name 'NWfprintf';
function NWvcprintf(para1:Pchar; para2:Tva_list):longint;cdecl;external 'clib' name 'NWvcprintf';
function NWvfprintf(para1:PFILE; para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'NWvfprintf';
function NWvprintf(para1:Pchar; para2:Tva_list):longint;cdecl;external 'clib' name 'NWvprintf';
function NWvsprintf(para1:Pchar; para2:Pchar; para3:Tva_list):longint;cdecl;external 'clib' name 'NWvsprintf';
{
   For the following support, open the file without 'b' in the mode. Additions
   for transparent Macintosh text file support ('\r' on lines) and additions
   for transparent UNIX text file support ('\n' on lines).
 }
function IsMacintoshTextFile(para1:PFILE):longint;cdecl;external 'clib' name 'IsMacintoshTextFile';
function SetMacintoshTextMode(para1:PFILE):longint;cdecl;external 'clib' name 'SetMacintoshTextMode';
function UnsetMacintoshTextMode(para1:PFILE):longint;cdecl;external 'clib' name 'UnsetMacintoshTextMode';

function IsMacintoshTextFile(var para1:TFILE):longint;cdecl;external 'clib' name 'IsMacintoshTextFile';
function SetMacintoshTextMode(var para1:TFILE):longint;cdecl;external 'clib' name 'SetMacintoshTextMode';
function UnsetMacintoshTextMode(var para1:TFILE):longint;cdecl;external 'clib' name 'UnsetMacintoshTextMode';

{ back to '\r\n'  }
function is_unix_text_file(para1:PFILE):longint;cdecl;external 'clib' name 'is_unix_text_file';
function set_unix_text_mode(para1:PFILE):longint;cdecl;external 'clib' name 'set_unix_text_mode';
function unset_unix_text_mode(para1:PFILE):longint;cdecl;external 'clib' name 'unset_unix_text_mode';

function is_unix_text_file(var para1:TFILE):longint;cdecl;external 'clib' name 'is_unix_text_file';
function set_unix_text_mode(var para1:TFILE):longint;cdecl;external 'clib' name 'set_unix_text_mode';
function unset_unix_text_mode(var para1:TFILE):longint;cdecl;external 'clib' name 'unset_unix_text_mode';

{ back to '\r\n'  }
{ functions underlying macro support...  }
function __get_stdin:PPFILE;cdecl;external 'clib' name '__get_stdin';
function __get_stdout:PPFILE;cdecl;external 'clib' name '__get_stdout';
function __get_stderr:PPFILE;cdecl;external 'clib' name '__get_stderr';

function __stdin : PFILE;
function __stdout : PFILE;
function __stderr : PFILE;
{-stdlib.h---------------------------------------------------------------------}
{$PACKRECORDS C}


const
   EXIT_FAILURE = -(1);
   EXIT_SUCCESS = 0;
   RAND_MAX = 32767;
type
   Pdiv_t = ^Tdiv_t;
   Tdiv_t = record
        quot : longint;
        rem : longint;
     end;

   Pldiv_t = ^Tldiv_t;
   Tldiv_t = record
        quot : longint;
        rem : longint;
     end;

   TCdeclProcedure = procedure; cdecl;
   TBSearchFunc = function (para1:pointer; para2:pointer):longint; cdecl;
   TQSortFunc = function (para1:pointer; para2:pointer):longint; cdecl;

//??  var __ctype : array of byte;cvar;external;

procedure abort;cdecl;external 'clib' name 'abort';
function abs(para1:longint):longint;cdecl;external 'clib' name 'abs';
function atexit(proc:TCdeclProcedure):longint;cdecl;external 'clib' name 'atexit';
function atof(para1:Pchar):double;cdecl;external 'clib' name 'atof';
function atoi(para1:Pchar):longint;cdecl;external 'clib' name 'atoi';
function atol(para1:Pchar):longint;cdecl;external 'clib' name 'atol';
function bsearch(para1,para2:pointer; para3,para4:Tsize_t; para5:TBsearchFunc):pointer;cdecl;external 'clib' name 'bsearch';
function calloc(para1:Tsize_t; para2:Tsize_t):pointer;cdecl;external 'clib' name 'calloc';
function _div(para1,para2:longint):Tdiv_t;cdecl;external 'clib' name 'div';
//procedure exit(para1:longint);cdecl;external 'clib' name 'exit';
procedure _exit(para1:longint);cdecl;external 'clib' name '_exit';
function getenv(para1:Pchar):Pchar;cdecl;external 'clib' name 'getenv';
function labs(para1:longint):longint;cdecl;external 'clib' name 'labs';
function ldiv(para1:longint; para2:longint):Tldiv_t;cdecl;external 'clib' name 'ldiv';
function malloc(para1:Tsize_t):pointer;cdecl;external 'clib' name 'malloc';
function mblen(para1:Pchar; para2:Tsize_t):longint;cdecl;external 'clib' name 'mblen';
function mbstowcs(para1:Pwchar_t; para2:Pchar; para3:Tsize_t):Tsize_t;cdecl;external 'clib' name 'mbstowcs';
function mbtowc(para1:Pwchar_t; para2:Pchar; para3:Tsize_t):longint;cdecl;external 'clib' name 'mbtowc';
procedure qsort(para1:pointer; para2,para3:Tsize_t; para4:TQSortFunc);cdecl;external 'clib' name 'qsort';
function rand:longint;cdecl;external 'clib' name 'rand';
function realloc(para1:pointer; para2:Tsize_t):pointer;cdecl;external 'clib' name 'realloc';
procedure srand(para1:dword);cdecl;external 'clib' name 'srand';
function strtod(para1:Pchar; para2:PPchar):double;cdecl;external 'clib' name 'strtod';
function strtol(para1:Pchar; para2:PPchar; para3:longint):longint;cdecl;external 'clib' name 'strtol';
function strtoul(para1:Pchar; para2:PPchar; para3:longint):dword;cdecl;external 'clib' name 'strtoul';
function _system(para1:Pchar):longint;cdecl;external 'clib' name 'system';
function wcstombs(para1:Pchar; para2:Pwchar_t; para3:Tsize_t):Tsize_t;cdecl;external 'clib' name 'wcstombs';
function wctomb(para1:Pchar; para2:Twchar_t):longint;cdecl;external 'clib' name 'wctomb';
function clearenv:longint;cdecl;external 'clib' name 'clearenv';
function ecvt(para1:double; para2:longint; para3:Plongint; para4:Plongint):Pchar;cdecl;external 'clib' name 'ecvt';
function fcvt(para1:double; para2:longint; para3:Plongint; para4:Plongint):Pchar;cdecl;external 'clib' name 'fcvt';
function gcvt(para1:double; para2:longint; para3:Pchar):Pchar;cdecl;external 'clib' name 'gcvt';
function htol(para1:Pchar):dword;cdecl;external 'clib' name 'htol';
function itoa(para1:longint; para2:Pchar; para3:longint):Pchar;cdecl;external 'clib' name 'itoa';
function itoab(para1:dword; para2:Pchar):Pchar;cdecl;external 'clib' name 'itoab';
function ltoa(para1:longint; para2:Pchar; para3:longint):Pchar;cdecl;external 'clib' name 'ltoa';
function max(para1:longint; para2:longint):longint;cdecl;external 'clib' name 'max';
function min(para1:longint; para2:longint):longint;cdecl;external 'clib' name 'min';
function putenv(name:Pchar):longint;cdecl;external 'clib' name 'putenv';
function rand_r(seed:Pdword; result:Plongint):longint;cdecl;external 'clib' name 'rand_r';
function _rotl(para1:dword; para2:dword):dword;cdecl;external 'clib' name '_rotl';
function _rotr(para1:dword; para2:dword):dword;cdecl;external 'clib' name '_rotr';
function scanenv(sequence:Plongint; variable:Pchar; length:Psize_t; value:Pchar):longint;cdecl;external 'clib' name 'scanenv';
function setenv(name:Pchar; value:Pchar; overwrite:longint):longint;cdecl;external 'clib' name 'setenv';
// double   strtod_ld( const char *, char **, long double *);
function strtoi(para1:Pchar; para2:longint):longint;cdecl;external 'clib' name 'strtoi';
function ultoa(para1:dword; para2:Pchar; para3:longint):Pchar;cdecl;external 'clib' name 'ultoa';
function unsetenv(name:Pchar):longint;cdecl;external 'clib' name 'unsetenv';
function utoa(para1:dword; para2:Pchar; para3:longint):Pchar;cdecl;external 'clib' name 'utoa';
function _lrotl(para1:dword; para2:dword):dword;cdecl;external 'clib' name '_lrotl';
function _lrotr(para1:dword; para2:dword):dword;cdecl;external 'clib' name '_lrotr';
{-unistd.h---------------------------------------------------------------------}
const
   F_OK = 0;
   R_OK = 4;
   W_OK = 2;
   X_OK = 1;
{ test using effective ids  }
   EFF_ONLY_OK = 8;
   STDIN_FILENO = 0;
   STDOUT_FILENO = 1;
   STDERR_FILENO = 2;

type TPipeFiledes = array [0..1] of longint;

function access(path:Pchar; mode:longint):longint;cdecl;external 'clib' name 'access';
function _chdir(path:Pchar):longint;cdecl;external 'clib' name 'chdir';
function Fpchdir(path:Pchar):longint;cdecl;external 'clib' name 'chdir';
function chsize(fildes:longint; size:dword):longint;cdecl;external 'clib' name 'chsize';
function _close(fildes:longint):longint;cdecl;external 'clib' name 'close';
function Fpclose(fildes:longint):longint;cdecl;external 'clib' name 'close';
function dup(fildes:longint):longint;cdecl;external 'clib' name 'dup';
function fpdup(fildes:longint):longint;cdecl;external 'clib' name 'dup';
function dup2(fildes1:longint; fildes2:longint):longint;cdecl;external 'clib' name 'dup2';
function fpdup2(fildes1:longint; fildes2:longint):longint;cdecl;external 'clib' name 'dup2';
function _eof(fildes:longint):longint;cdecl;external 'clib' name 'eof';
function Fpeof(fildes:longint):longint;cdecl;external 'clib' name 'eof';
function getcwd(path:Pchar; len:Tsize_t):Pchar;cdecl;external 'clib' name 'getcwd';
function isatty(fildes:longint):longint;cdecl;external 'clib' name 'isatty';
function lseek(fildes:longint; offset:Toff_t; whence:longint):Toff_t;cdecl;external 'clib' name 'lseek';
function pipe(fildes:TPipeFiledes):longint;cdecl;external 'clib' name 'pipe';
function _read(fildes:longint; buf:pointer; nbytes:Tsize_t):Tssize_t;cdecl;external 'clib' name 'read';
function Fpread(fildes:longint; buf:pointer; nbytes:Tsize_t):Tssize_t;cdecl;external 'clib' name 'read';
function rmdir(path:Pchar):longint;cdecl;external 'clib' name 'rmdir';
function unlink(path:Pchar):longint;cdecl;external 'clib' name 'unlink';
function _write(fildes:longint; buf:pointer; nbytes:Tsize_t):Tssize_t;cdecl;external 'clib' name 'write';
function Fpwrite(fildes:longint; buf:pointer; nbytes:Tsize_t):Tssize_t;cdecl;external 'clib' name 'write';
function pread(fildes:longint; buf:pointer; nbytes:Tsize_t; offset:Toff_t):Tssize_t;cdecl;external 'clib' name 'pread';
function pwrite(fildes:longint; buf:pointer; nbytes:Tsize_t; offset:Toff_t):Tssize_t;cdecl;external 'clib' name 'pwrite';

function _write(fildes:longint; var buf; nbytes:Tsize_t):Tssize_t;cdecl;external 'clib' name 'write';
function Fpwrite(fildes:longint; var buf; nbytes:Tsize_t):Tssize_t;cdecl;external 'clib' name 'write';
function pread(fildes:longint; var buf; nbytes:Tsize_t; offset:Toff_t):Tssize_t;cdecl;external 'clib' name 'pread';
function pwrite(fildes:longint; var buf; nbytes:Tsize_t; offset:Toff_t):Tssize_t;cdecl;external 'clib' name 'pwrite';
{-libcclib.h-------------------------------------------------------------------}
{$PACKRECORDS C}

type
   Tstart_routineProc = procedure (arg:pointer); cdecl;
   Pclibctx_t = ^Tclibctx_t;
   Tclibctx_t = record
     ThreadGroupGetID   : function :longint;cdecl;
     ThreadGroupCreate  : function (name:Pchar; threadGroupID:Plongint):longint; cdecl;
     ThreadGroupDispose : function (threadGroupID:longint):longint; cdecl;
     ThreadGroupUnwrap  : function (threadGroupID:longint; restoredThreadGroupID:longint):longint; cdecl;
     ThreadGroupWrap    : function (threadGroupID:longint):longint; cdecl;
     ThreadCreate       : function (threadGroupID:longint;
                              start_routine:Tstart_routineProc;
                              arg:pointer;
                              stackSize:Tsize_t;
                              flags:dword;
                              threadID:Plongint):longint; cdecl;
     __UnloadBroker     : procedure ; cdecl;
     reserved1          : pointer;
     reserved           : array[0..7] of pointer;
   end;

function CLibLoadContextBroker(module:pointer; callback:Pchar):longint;cdecl;external 'clib' name 'CLibLoadContextBroker';
function CLibUnloadContextBroker(broker:Pclibctx_t):longint;cdecl;external 'clib' name 'CLibUnloadContextBroker';
function MyCallBack(broker:Pclibctx_t):longint;cdecl;external 'clib' name 'MyCallBack';
{-nwtime.h---------------------------------------------------------------------}
const
   CLOCK_IS_SYNCHRONIZED = $01;
type

   PclockAndStatus = ^TclockAndStatus;
   TclockAndStatus = longint;

const
   CLOCK_IS_NETWORK_SYNCHRONIZED = $02;
   CLOCK_SYNCHRONIZATION_IS_ACTIVE = $04;

{$include npackon.inc}
type
   PDOSTime = ^TDOSTime;
   TDOSTime = record
        flag0 : word;
     end;

const
   bm_TDOSTime_bisecond = $1F;
   bp_TDOSTime_bisecond = 0;
   bm_TDOSTime_minute = $7E0;
   bp_TDOSTime_minute = 5;
   bm_TDOSTime_hour = $F800;
   bp_TDOSTime_hour = 11;
function bisecond(var a : TDOSTime) : word;
procedure set_bisecond(var a : TDOSTime; __bisecond : word);
function minute(var a : TDOSTime) : word;
procedure set_minute(var a : TDOSTime; __minute : word);
function hour(var a : TDOSTime) : word;
procedure set_hour(var a : TDOSTime; __hour : word);
type
   PDOSDate = ^TDOSDate;
   TDOSDate = record
        flag0 : word;
     end;
   T_DOSDate = TDOSDate;

const
   bm_TDOSDate_day = $1F;
   bp_TDOSDate_day = 0;
   bm_TDOSDate_month = $1E0;
   bp_TDOSDate_month = 5;
   bm_TDOSDate_yearsSince80 = $FE00;
   bp_TDOSDate_yearsSince80 = 9;
function day(var a : TDOSDate) : word;
procedure set_day(var a : TDOSDate; __day : word);
function month(var a : TDOSDate) : word;
procedure set_month(var a : TDOSDate; __month : word);
function yearsSince80(var a : TDOSDate) : word;
procedure set_yearsSince80(var a : TDOSDate; __yearsSince80 : word);
type
   P_DOSTime = ^T_DOSTime;
   T_DOSTime = record
        flag0 : word;
     end;

const
   bm_T_DOSTime_bisecond = $1F;
   bp_T_DOSTime_bisecond = 0;
   bm_T_DOSTime_minute = $7E0;
   bp_T_DOSTime_minute = 5;
   bm_T_DOSTime_hour = $F800;
   bp_T_DOSTime_hour = 11;
function bisecond(var a : T_DOSTime) : word;
procedure set_bisecond(var a : T_DOSTime; __bisecond : word);
function minute(var a : T_DOSTime) : word;
procedure set_minute(var a : T_DOSTime; __minute : word);
function hour(var a : T_DOSTime) : word;
procedure set_hour(var a : T_DOSTime; __hour : word);

const
   bm_T_DOSDate_day = $1F;
   bp_T_DOSDate_day = 0;
   bm_T_DOSDate_month = $1E0;
   bp_T_DOSDate_month = 5;
   bm_T_DOSDate_yearsSince80 = $FE00;
   bp_T_DOSDate_yearsSince80 = 9;


{$include npackoff.inc}

function _ConvertDOSTimeToCalendar(dateTime:longint):Ttime_t;cdecl;external 'clib' name '_ConvertDOSTimeToCalendar';
procedure _ConvertTimeToDOS(calendarTime:Ttime_t; filDatP:PDOSDate; filTimP:PDOSTime);cdecl;external 'clib' name '_ConvertTimeToDOS';
procedure GetClockStatus(_dataPtr:TclockAndStatus);cdecl;external 'clib' name 'GetClockStatus';
function GetCurrentTicks:longint;cdecl;external 'clib' name 'GetCurrentTicks';
function GetHighResolutionTimer:longint;cdecl;external 'clib' name 'GetHighResolutionTimer';
function GetSuperHighResolutionTimer:longint;cdecl;external 'clib' name 'GetSuperHighResolutionTimer';
function NWGetHighResolutionTimer:longint;cdecl;external 'clib' name 'NWGetHighResolutionTimer';
function NWGetSuperHighResolutionTimer:longint;cdecl;external 'clib' name 'NWGetSuperHighResolutionTimer';
function __get_altzone:Ptime_t;cdecl;external 'clib' name '__get_altzone';
function altzone:Ptime_t;cdecl;external 'clib' name '__get_altzone';
function __get_daylight:Plongint;cdecl;external 'clib' name '__get_daylight';
function daylight:Plongint;cdecl;external 'clib' name '__get_daylight';
function __get_daylightOffset:Ptime_t;cdecl;external 'clib' name '__get_daylightOffset';
function daylightOffset:Ptime_t;cdecl;external 'clib' name '__get_daylightOffset';
function __get_daylightOnOff:Plongint;cdecl;external 'clib' name '__get_daylightOnOff';
function daylightOnOff:Plongint;cdecl;external 'clib' name '__get_daylightOnOff';
function __get_timezone:Ptime_t;cdecl;external 'clib' name '__get_timezone';
function timezone:Ptime_t;cdecl;external 'clib' name '__get_timezone';
procedure SecondsToTicks(Seconds:longint; TenthsOfSeconds:longint; Ticks:Plongint);cdecl;external 'clib' name 'SecondsToTicks';
procedure TicksToSeconds(Ticks:longint; Seconds:Plongint; TenthsOfSeconds:Plongint);cdecl;external 'clib' name 'TicksToSeconds';
{-nwthread.h-------------------------------------------------------------------}
  { values for __action_code used with ExitThread()  }

  const
     TSR_THREAD = -1;
     EXIT_THREAD = 0;
     EXIT_NLM = 1;
  { values for __mode used with spawnxx()  }
     P_WAIT = 0;
     P_NOWAIT = 1;
     P_OVERLAY = 2;
     P_NOWAITO = 4;
     P_SPAWN_IN_CURRENT_DOMAIN = 8;
     NO_CONTEXT = 0;
     USE_CURRENT_CONTEXT = 1;
  { stack defines  }
     MIN_STACKSIZE = 16384;
     DEFAULT_STACKSIZE = 16384;

  type

     PWorkToDo = ^TWorkToDo;

     TProcedure    = procedure; cdecl;
     TThreadFunc   = procedure (param1:pointer); cdecl;
     TWorkToDoProc = procedure (data:pointer; workToDo:PWorkToDo); cdecl;
     TCleanup      = procedure (para1:longint); cdecl;


     PAESProcessStructure = ^TAESProcessStructure;
     TAESProcessStructure = record
          ALink              : PAESProcessStructure;
          AWakeUpDelayAmount : longint;
          AWakeUpTime        : longint;
          AProcessToCall     : procedure (para1:pointer);cdecl;
          ARTag              : longint;
          AOldLink           : longint;
       end;


     PWorkToDoStructure = ^TWorkToDoStructure;
     TWorkToDoStructure = record
          Link            : PWorkToDoStructure;
          workProcedure   : TProcedure;
          WorkResourceTag : longint;
          PollCountAmount : longint;
          PollCountWhen   : longint;
          userProcedure   : TProcedure;
          dataPtr         : pointer;
          destThreadGroup : longint;
       end;
     TWorkToDo = TWorkToDoStructure;



  { custom data area variables...  }
{
    var
       threadCustomDataPtr : pointer;cvar;external;
       threadCustomDataSize : longint;cvar;external;
       threadGroupCustomDataPtr : pointer;cvar;external;
       threadGroupCustomDataSize : longint;cvar;external;
 }


  function AtUnload(func:Tprocedure):longint;                       cdecl;external ThreadsNlm name 'AtUnload';
  function BeginThread(func:TThreadFunc;
                       stackP:pointer;
                       stackSize:dword;
                       arg:pointer):longint;                        cdecl;external ThreadsNlm name 'BeginThread';
  function BeginThreadGroup(func:TThreadFunc;
                            stackP:pointer;
                            stackSize:dword;
                            arg:pointer):longint;                   cdecl;external ThreadsNlm name 'BeginThreadGroup';
  function Breakpoint(arg:longint):longint;                         cdecl;external Lib0Nlm name 'Breakpoint';
  procedure CancelNoSleepAESProcessEvent(EventNode:PAESProcessStructure);cdecl;external ThreadsNlm name 'CancelNoSleepAESProcessEvent';
  procedure CancelSleepAESProcessEvent  (EventNode:PAESProcessStructure);cdecl;external ThreadsNlm name 'CancelSleepAESProcessEvent';
  function ClearNLMDontUnloadFlag(NLMID:longint):longint;                cdecl;external ThreadsNlm name 'ClearNLMDontUnloadFlag';
  procedure delay(milliseconds:dword);                                   cdecl;external ThreadsNlm name 'delay';
  function EnterCritSec:longint;                                         cdecl;external ThreadsNlm name 'EnterCritSec';
  function ExitCritSec:longint;                                          cdecl;external ThreadsNlm name 'ExitCritSec';
  procedure ExitThread(action_code     :longint;
                       termination_code:longint);                        cdecl;external ThreadsNlm name 'ExitThread';

  function FindNLMHandle(NLMFileName:Pchar):TNlmHandle;                  cdecl;external ThreadsNlm name 'FindNLMHandle';
  function getcmd(cmdLine:Pchar):Pchar;                                  cdecl;external ThreadsNlm name 'getcmd';
  function GetNLMHandle:TNlmHandle;                                      cdecl;external ThreadsNlm name 'GetNLMHandle';
  function GetNLMID:longint;                                             cdecl;external ThreadsNlm name 'GetNLMID';
  function GetNLMIDFromNLMHandle(NLMHandle:longint):longint;             cdecl;external ThreadsNlm name 'GetNLMIDFromNLMHandle';
  function GetNLMIDFromThreadID(threadID:longint;fileName:Pchar):longint;cdecl;external ThreadsNlm name 'GetNLMIDFromThreadID';
  function GetNLMNameFromNLMID(NLMID:longint;
                               fileName:Pchar;
                               description:Pchar):longint;               cdecl;external ThreadsNlm name 'GetNLMNameFromNLMID';
  function GetNLMNameFromNLMHandle(NLMHandle:TNlmHandle;
                                   LDFileName:Pchar;
                                   LDName:Pchar):longint;                cdecl;external ThreadsNlm name 'GetNLMNameFromNLMHandle';
  function GetThreadContextSpecifier(threadID:longint):longint;          cdecl;external ThreadsNlm name 'GetThreadContextSpecifier';
  function GetThreadGroupID:longint;                                     cdecl;external ThreadsNlm name 'GetThreadGroupID';
  function __GetThreadIDFromPCB(PCB:longint):longint;                    cdecl;external Lib0Nlm name '__GetThreadIDFromPCB';
  function GetThreadHandicap(threadID:longint):longint;                  cdecl;external ThreadsNlm name 'GetThreadHandicap';
  function GetThreadID:longint;                                          cdecl;external ThreadsNlm name 'GetThreadID';
  function GetThreadName(threadID:longint; tName:Pchar):longint;         cdecl;external ThreadsNlm name 'GetThreadName';
  function GetThreadName(threadID:longint; var tName):longint;           cdecl;external ThreadsNlm name 'GetThreadName';
  function MapNLMIDToHandle(NLMID:longint):TNlmHandle;                   cdecl;external ThreadsNlm name 'MapNLMIDToHandle';
  function PopThreadCleanup(execute:longint):TCLEANUP;                   cdecl;external ThreadsNlm name 'PopThreadCleanup';
  function PopThreadGroupCleanup(execute:longint):TCLEANUP;              cdecl;external ThreadsNlm name 'PopThreadGroupCleanup';
  function PushThreadCleanup(func:TCLEANUP):longint;                     cdecl;external ThreadsNlm name 'PushThreadCleanup';
  function PushThreadGroupCleanup(func:TCLEANUP):longint;                cdecl;external ThreadsNlm name 'PushThreadGroupCleanup';
  function RenameThread(threadID:longint; newName:Pchar):longint;        cdecl;external ThreadsNlm name 'RenameThread';
  function ResumeThread(threadID:longint):longint;                       cdecl;external ThreadsNlm name 'ResumeThread';
  function ReturnNLMVersionInfoFromFile(pathName:pchar;
                                        majorVersion:Plongint;
                                        minorVersion:Plongint;
                                        revision:Plongint;
                                        year:Plongint;
                                        month:Plongint;
                                        day:Plongint;
                                        copyrightString:pchar;
                                        description:pchar):longint;      cdecl;external NlmLibNlm name 'ReturnNLMVersionInfoFromFile';
  function ReturnNLMVersionInfoFromFile(pathName:pchar;
                                        var majorVersion,minorVersion,revision:longint;
                                        var year,month,day:longint;
                                        copyrightString:pchar;
                                        description:pchar):longint;      cdecl;external NlmLibNlm name 'ReturnNLMVersionInfoFromFile';

  function ReturnNLMVersionInformation(NLMHandle:TNlmHandle;
                                       majorVersion,minorVersion,revision,year,month,day:Plongint;
                                       copyrightString:pchar; description:pchar):longint;cdecl;external NlmLibNlm name 'ReturnNLMVersionInformation';
  function ReturnNLMVersionInformation(NLMHandle:TNlmHandle;
                                       var majorVersion,minorVersion,revision,year,month,day:longint;
                                       copyrightString:pchar; description:pchar):longint;cdecl;external NlmLibNlm name 'ReturnNLMVersionInformation';

  procedure ScheduleNoSleepAESProcessEvent(EventNode:PAESProcessStructure);cdecl;external ThreadsNlm name 'ScheduleNoSleepAESProcessEvent';
  procedure ScheduleSleepAESProcessEvent(EventNode:PAESProcessStructure);  cdecl;external ThreadsNlm name 'ScheduleSleepAESProcessEvent';


  function ScheduleWorkToDo(ProcedureToCall:TWorkToDoProc;
                            workData       :pointer;
                            workToDo       :PWorkToDo):longint;            cdecl;external ThreadsNlm name 'ScheduleWorkToDo';
  function SetNLMDontUnloadFlag(NLMID:longint):longint;                    cdecl;external ThreadsNlm name 'SetNLMDontUnloadFlag';
  function SetNLMID(newNLMID:longint):longint;                             cdecl;external ThreadsNlm name 'SetNLMID';
  function SetThreadContextSpecifier(threadID,
                                     contextSpecifier:longint):longint;    cdecl;external ThreadsNlm name 'SetThreadContextSpecifier';
  function SetThreadGroupID(newThreadGroupID:longint):longint;             cdecl;external ThreadsNlm name 'SetThreadGroupID';
  procedure SetThreadHandicap(threadID, handicap:longint);                 cdecl;external ThreadsNlm name 'SetThreadHandicap';
  function spawnlp(mode:longint;
                   path,arg0:Pchar;
                   args:array of const):longint;                           cdecl;external ThreadsNlm name 'spawnlp';
  function spawnlp(mode:longint;
                   path,arg0:Pchar):longint;                               cdecl;external ThreadsNlm name 'spawnlp';
  function spawnvp(mode:longint;
                   path,argv:PPchar):longint;                              cdecl;external ThreadsNlm name 'spawnvp';
  function SuspendThread(threadID:longint):longint;                        cdecl;external ThreadsNlm name 'SuspendThread';
  procedure ThreadSwitch;                                                  cdecl;external ThreadsNlm name 'ThreadSwitch';
  procedure ThreadSwitchLowPriority;                                       cdecl;external ThreadsNlm name 'ThreadSwitchLowPriority';
  procedure ThreadSwitchWithDelay;                                         cdecl;external ThreadsNlm name 'ThreadSwitchWithDelay';
{-nwmediam.h-------------------------------------------------------------------}
{$include npackon.inc}

const
   FORMAT_MEDIA = $0000;
   TAPE_CONTROL = $0001;
   ACTIVATE_FUNCTIONS = $0003;
   MOUNT_FUNCTIONS = $0004;
   SELECT_FUNCTIONS = $0005;
   INSERTION_FUNCTIONS = $0006;
   LOCK_FUNCTIONS = $0007;
   MOVE_FUNCTIONS = $0008;
   STAMP_FUNCTIONS = $0009;
   SCAN_FUNCTIONS = $000A;
   MAGAZINE_FUNCTIONS = $000D;
{ IO Functions  }
   RANDOM_READ = $0020;
   RANDOM_WRITE = $0021;
   RANDOM_WRITE_ONCE = $0022;
   SEQUENTIAL_READ = $0023;
   SEQUENTIAL_WRITE = $0024;
   RESET_END_OF_TAPE = $0025;
   SINGLE_FILE_MARK = $0026;
   MULTIPLE_FILE_MARK = $0027;
   SINGLE_SET_MARK = $0028;
   MULTIPLE_SET_MARK = $0029;
   SPACE_DATA_BLOCKS = $002A;
   LOCATE_DATA_BLOCKS = $002B;
   POSITION_PARTITION = $002C;
   POSITION_MEDIA = $002D;
   DEVICE_GENERIC_IOCTL = $003E;
{ Object Types  }
   UNKNOWN_OBJECT = $FFFF;
   ADAPTER_OBJECT = 0;
   CHANGER_OBJECT = 1;
   DEVICE_OBJECT = 2;
   MEDIA_OBJECT = 4;
   PARTITION_OBJECT = 5;
   SLOT_OBJECT = 6;
   HOTFIX_OBJECT = 7;
   MIRROR_OBJECT = 8;
   PARITY_OBJECT = 9;
   VOLUME_SEG_OBJECT = 10;
   VOLUME_OBJECT = 11;
   CLONE_OBJECT = 12;
   MAGAZINE_OBJECT = 14;
   UNIDENTIFIABLE_MEDIA = $00000001;
   HIGH_SIERRA_CDROM_MEDIA = $00000002;
   ISO_CDROM_MEDIA = $00000003;
   MAC_CDROM_MEDIA = $00000004;
   NETWARE_FILE_SYSTEM_MEDIA = $00000005;
   INTERNAL_IDENTIFY_TYPE = $00000007;
   SMS_MEDIA_TYPE = $00000008;
{ Notify Event Bits  }
   NOTIFY_OBJECT_CREATION = $0001;
   NOTIFY_OBJECT_DELETION = $0002;
   NOTIFY_OBJECT_ACTIVATED = $0004;
   NOTIFY_OBJECT_DEACTIVATED = $0008;
   NOTIFY_OBJECT_RESERVATION = $0010;
   NOTIFY_OBJECT_UNRESERVATION = $0020;
{ Object Status Bits  }
   OBJECT_ACTIVATED = $00000001;
   OBJECT_PHANTOM = $00000002;
   OBJECT_ASSIGNABLE = $00000004;
   OBJECT_ASSIGNED = $00000008;
   OBJECT_RESERVED = $00000010;
   OBJECT_BEING_IDENTIFIED = $00000020;
   OBJECT_MAGAZINE_LOADED = $00000040;
   OBJECT_FAILURE = $00000080;
   OBJECT_REMOVABLE = $00000100;
   OBJECT_READ_ONLY = $00000200;
   OBJECT_IN_DEVICE = $00010000;
   OBJECT_ACCEPTS_MAGAZINES = $00020000;
   OBJECT_IS_IN_A_CHANGER = $00040000;
   OBJECT_LOADABLE = $00080000;
   OBJECT_BEING_LOADED = $00080000;
   OBJECT_DEVICE_LOCK = $01000000;
   OBJECT_CHANGER_LOCK = $02000000;
   OBJECT_REMIRRORING = $04000000;
   OBJECT_SELECTED = $08000000;
{ Resource Tag Allocation Signatures  }
{ 'PAMM'  }
   MMApplicationSignature = $50424D4D;
{ 'ONMM'  }
   MMNotifySignature = $4F4E4D4D;
{ 'DIMM'  }
   MMIdentifySignature = $44494D4D;
{ AlertTypes  }
   ALERT_MESSAGE = $00000001;
   ALERT_ACTIVATE = $00000002;
   ALERT_DEACTIVATE = $00000003;
   ALERT_DELETE = $00000004;
{ AlertReasons  }
   ALERT_HOTFIX_ERROR = $00000000;
   ALERT_DRIVER_UNLOAD = $00000001;
   ALERT_DEVICE_FAILURE = $00000002;
   ALERT_PROGRAM_CONTROL = $00000003;
   ALERT_MEDIA_DISMOUNT = $00000004;
   ALERT_MEDIA_EJECT = $00000005;
   ALERT_SERVER_DOWN = $00000006;
   ALERT_SERVER_FAILURE = $00000007;
   ALERT_MEDIA_LOAD = $00000008;
   ALERT_MEDIA_MOUNT = $00000009;
   ALERT_DRIVER_LOAD = $0000000A;
   ALERT_LOST_SOFTWARE_FAULT_TOLERANCE = $0000000B;
   ALERT_INTERNAL_OBJECT_DELETE = $0000000C;
   ALERT_MAGAZINE_LOAD = $0000000D;
   ALERT_MAGAZINE_UNLOAD = $0000000E;
   ALERT_DEVICE_GOING_TO_BE_REMOVED = $0000000F;
   ALERT_CHECK_DEVICE = $00000010;
   ALERT_CONFIGURATION_CHANGE = $00000011;
   ALERT_APPLICATION_UNREGISTER = $00000012;
   ALERT_DAI_EMMULATION = $00000013;
   ALERT_LOST_HARDWARE_FAULT_TOLERANCE = $00000014;
   ALERT_INTERNAL_OBJECT_CREATE = $00000015;
   ALERT_INTERNAL_MANAGER_REMOVE = $00000016;
   ALERT_DEVICE_GOING_TO_BE_DEACTIVATED = $00000017;
   ALERT_DEVICE_END_OF_MEDIA = $00000018;
   ALERT_MEDIA_INSERTED = $00000019;
   ALERT_UNKNOWN_DEVICE_ALERT = $0000001A;
   ALERT_UNKNOWN_ADAPTER_ALERT = $0000001B;
{ Function Control (Priority) Bits  }
   PRIORITY_1 = $0001;
   PRIORITY_2 = $0002;
   ACCELERATED_BIT = $0004;
   ELEVATOR_OFF_BIT = $0008;
   RETURN_RAW_COMPLETION = $0010;
   SCRAMBLE_BIT = $0020;
{ Application Alert Codes  }
   GOING_TO_BE_DEACTIVATED = $0001;
   OBJECT_BEING_DEACTIVATED = $0002;
   OBJECT_SIZE_CHANGED = $0003;
   OBJECT_BEING_ACTIVATED = $0004;
   OBJECT_BEING_DELETED = $0005;
   OBJECT_LOST_FAULT_TOLERANCE = $0006;
{ Initial Completion Codes  }
   MESSAGE_PROCESSED = $00;
   MESSAGE_DATA_MISSING = $01;
   MESSAGE_POSTPONE = $02;
   MESSAGE_ABORTED = $03;
   MESSAGE_INVALID_PARAMETERS = $04;
   MESSAGE_OBJECT_NOT_ACTIVE = $05;
   MESSAGE_INVALID_OJECT = $06;
   MESSAGE_FUNCTION_NOT_SUPPORTED = $07;
   MESSAGE_INVALID_MODE = $08;
   MESSAGE_INTERNAL_ERROR = $09;
{ FinalCompletion Codes  }
   FUNCTION_OK = $00;
   FUNCTION_CORRECTED_MEDIA_ERROR = $10;
   FUNCTION_MEDIA_ERROR = $11;
   FUNCTION_DEVICE_ERROR = $12;
   FUNCTION_ADAPTER_ERROR = $13;
   FUNCTION_NOT_SUPPORTED_BY_DEVICE = $14;
   FUNCTION_NOT_SUPPORTED_BY_DRIVER = $15;
   FUNCTION_PARAMETER_ERROR = $16;
   FUNCTION_MEDIA_NOT_PRESENT = $17;
   FUNCTION_MEDIA_CHANGED = $18;
   FUNCTION_PREVIOUSLY_WRITTEN = $19;
   FUNCTION_MEDIA_NOT_FORMATED = $1A;
   FUNCTION_BLANK_MEDIA = $1B;
{end of partition }
   FUNCTION_END_OF_MEDIA = $1C;
   FUNCTION_FILE_MARK_DETECTED = $1D;
   FUNCTION_SET_MARK_DETECTED = $1E;
   FUNCTION_WRITE_PROTECTED = $1F;
   FUNCTION_OK_EARLY_WARNING = $20;
   FUNCTION_BEGINNING_OF_MEDIA = $21;
   FUNCTION_MEDIA_NOT_FOUND = $22;
   FUNCTION_MEDIA_NOT_REMOVED = $23;
   FUNCTION_UNKNOWN_COMPLETION = $24;
   FUNCTION_DATA_MISSING = $25;
   FUNCTION_HOTFIX_ERROR = $26;
   FUNCTION_HOTFIX_UPDATE_ERROR = $27;
   FUNCTION_IO_ERROR = $28;
   FUNCTION_CHANGER_SOURCE_EMPTY = $29;
   FUNCTION_CHANGER_DEST_FULL = $2A;
   FUNCTION_CHANGER_JAMMED = $2B;
   FUNCTION_MAGAZINE_NOT_PRESENT = $2D;
   FUNCTION_MAGAZINE_SOURCE_EMPTY = $2E;
   FUNCTION_MAGAZINE_DEST_FULL = $2F;
   FUNCTION_MAGAZINE_JAMMED = $30;
   FUNCTION_ABORT_CAUSED_BY_PRIOR_ERROR = $31;
   FUNCTION_CHANGER_ERROR = $32;
   FUNCTION_MAGAZINE_ERROR = $33;
{ ErrorCodes  }
   MM_OK = $00;
   MM_INVALID_OBJECT = $01;
   MM_INVALID_APPLICATION = $02;
   MM_INVALID_RESOURCETAG = $03;
   MM_MEMORY_ALLOCATION_ERROR = $04;
   MM_INVALID_MODE = $05;
   MM_RESERVATION_CONFLICT = $06;
   MM_PARAMETER_ERROR = $07;
   MM_OBJECT_NOT_FOUND = $08;
   MM_ATTRIBUTE_NOT_SETABLE = $09;
   MM_FAILURE = $0A;
{ Console Human Jukebox Definitions  }
   HJ_INSERT_MESSAGE = 0;
   HJ_EJECT_MESSAGE = 1;
   HJ_ACK_MESSAGE = 2;
   HJ_NACK_MESSAGE = 3;
   HJ_ERROR = 4;
{ Media Manager Structures  }
type
   PMM_F1_Structure = ^TMM_F1_Structure;
   TMM_F1_Structure = record
        code : word;
        control : word;
     end;

   PPrivateIOConfigurationStucture = ^TPrivateIOConfigurationStucture;
   TPrivateIOConfigurationStucture = record
        f1 : longint;
        f2 : word;
        f3 : word;
        f4 : array[0..3] of word;
        f5 : longint;
        f6 : word;
        f7 : longint;
        f8 : word;
        f9 : array[0..1] of byte;
        f10 : array[0..1] of byte;
        f11 : longint;
        f12 : longint;
        f13 : longint;
        f14 : array[0..17] of byte;
        f15 : array[0..1] of longint;
        f16 : word;
        f17 : array[0..5] of byte;
     end;

   PAdapterInfoDef = ^TAdapterInfoDef;
   TAdapterInfoDef = record
        systemtype : byte;
        processornumber : byte;
        uniquetag : word;
        systemnumber : longint;
        devices : array[0..31] of longint;
        configinfo : TPrivateIOConfigurationStucture;
        drivername : array[0..35] of byte;
        systemname : array[0..63] of byte;
        numberofdevices : longint;
        reserved : array[0..6] of longint;
     end;

   PAttributeInfoDef = ^TAttributeInfoDef;
   TAttributeInfoDef = record
        name : array[0..63] of byte;
        attributetype : longint;
        nextattributeid : longint;
        attributesize : longint;
     end;

   PChangerInfoDef = ^TChangerInfoDef;
   TChangerInfoDef = record
        numberofdevices : longint;
        numberofslots : longint;
        numberofmailslots : longint;
        reserved : array[0..7] of longint;
        slotmappingtable : array[0..0] of longint;
     end;

   PDeviceInfoDef = ^TDeviceInfoDef;
   TDeviceInfoDef = record
        status : longint;
        controllernumber : byte;
        drivenumber : byte;
        cardnumber : byte;
        systemtype : byte;
        accessflags : byte;
        _type : byte;
        blocksize : byte;
        sectorsize : byte;
        heads : byte;
        sectors : byte;
        cylinders : word;
        capacity : longint;
        mmadapternumber : longint;
        mmmedianumber : longint;
        rawname : array[0..39] of byte;
        reserved : array[0..7] of longint;
     end;

   PPrivateMediaInfoDef = ^TPrivateMediaInfoDef;
   TPrivateMediaInfoDef = record
        f1 : array[0..63] of byte;
        f2 : longint;
        f3 : longint;
     end;

   PGenericInfoDef = ^TGenericInfoDef;
   TGenericInfoDef = record
        mediainfo : TPrivateMediaInfoDef;
        mediatype : longint;
        cartridgetype : longint;
        unitsize : longint;
        blocksize : longint;
        capacity : longint;
        preferredunitsize : longint;
        name : array[0..63] of byte;
        _type : longint;
        status : longint;
        functionmask : longint;
        controlmask : longint;
        parentcount : longint;
        siblingcount : longint;
        childcount : longint;
        specificinfosize : longint;
        objectuniqueid : longint;
        mediaslot : longint;
     end;

   PHotfixInfoDef = ^THotfixInfoDef;
   THotfixInfoDef = record
        hotfixoffset : longint;
        hotfixidentifier : longint;
        numberoftotalblocks : longint;
        numberofusedblocks : longint;
        numberofavailableblocks : longint;
        numberofsystemblocks : longint;
        reserved : array[0..7] of longint;
     end;

   PIdentifierInfoDef = ^TIdentifierInfoDef;
   TIdentifierInfoDef = record
        applicationtype : longint;
        mediatype : longint;
        cartridgetype : longint;
        name : array[0..63] of byte;
        stampflag : longint;
     end;

   PInsertRequestDef = ^TInsertRequestDef;
   TInsertRequestDef = record
        devicenumber : longint;
        mailslot : longint;
        medianumber : longint;
        mediacount : longint;
     end;

   PMagazineInfoDef = ^TMagazineInfoDef;
   TMagazineInfoDef = record
        numberofslots : longint;
        reserved : array[0..7] of longint;
        slotmappingtable : array[0..0] of longint;
     end;

   PMappintInfoHeaderDef = ^TMappintInfoHeaderDef;
   TMappintInfoHeaderDef = record
        parentcount : longint;
        siblingcount : longint;
        childcount : longint;
     end;

   PMediaInfoDef = ^TMediaInfoDef;
   TMediaInfoDef = record
        _label : array[0..63] of byte;
        identificationtype : longint;
        identificationtimestamp : longint;
     end;

   PMediaRequestDef = ^TMediaRequestDef;
   TMediaRequestDef = record
        devicenumber : longint;
        mailslot : longint;
        medianumber : longint;
        mediacount : longint;
     end;

   PMirrorInfoDef = ^TMirrorInfoDef;
   TMirrorInfoDef = record
        mirrorcount : longint;
        mirroridentifier : longint;
        mirrormembers : array[0..7] of longint;
        mirrorsynchflags : array[0..7] of byte;
        reserved : array[0..7] of longint;
     end;

   PPartitionInfoDef = ^TPartitionInfoDef;
   TPartitionInfoDef = record
        partitionertype : longint;
        partitiontype : longint;
        partitionoffset : longint;
        partitionsize : longint;
        reserved : array[0..7] of longint;
     end;

{ these also correspond to offsets in struct  ObjectDef  }
   PResourceTagDef = ^TResourceTagDef;
   TResourceTagDef = record
        reserved : array[0..1] of longint;
        resourcetagtype : longint;
        resourcetagcount : longint;
        resourcenext : PResourceTagDef;
        resourcelast : PResourceTagDef;
     end;

{$include npackoff.inc}

function HJ_Media_Request_Ack(minfo:PInsertRequestDef; ackcode:longint; uniqueid:longint):longint;cdecl;external 'clib' name 'HJ_Media_Request_Ack';
function MM_Abort_Function(messagehandle:longint):longint;cdecl;external 'clib' name 'MM_Abort_Function';
function MM_Check_For_Pending_Aborts(OSRequestHandle:longint):longint;cdecl;external 'clib' name 'MM_Check_For_Pending_Aborts';
function MM_Create_Media_Object(objectnumber:longint; mediainfo:PMediaInfoDef):longint;cdecl;external 'clib' name 'MM_Create_Media_Object';
procedure MM_ExecuteMessages;cdecl;external 'clib' name 'MM_ExecuteMessages';
function MM_Find_Identifier(lastidentifiernumber:Plongint):longint;cdecl;external 'clib' name 'MM_Find_Identifier';
function MM_Find_Identifier(var lastidentifiernumber:longint):longint;cdecl;external 'clib' name 'MM_Find_Identifier';
function MM_Find_Object_Type(typ:longint; nextindicator:Plongint):longint;cdecl;external 'clib' name 'MM_Find_Object_Type';
function MM_Find_Object_Type(typ:longint; var nextindicator:longint):longint;cdecl;external 'clib' name 'MM_Find_Object_Type';
function MM_Object_Blocking_IO (returnparameter:Plongint;
                                objecthandle:longint;
                                _function:TMM_F1_Structure;
                                parameter0:longint;
                                parameter1:longint;
                                parameter2:longint;
                                bufferlength:longint;
                                buffer:pointer):longint;cdecl;external 'clib' name 'MM_Object_Blocking_IO';
function MM_Object_Blocking_IO (var returnparameter:longint;
                                objecthandle:longint;
                                _function:TMM_F1_Structure;
                                parameter0:longint;
                                parameter1:longint;
                                parameter2:longint;
                                bufferlength:longint;
                                var buffer):longint;cdecl;external 'clib' name 'MM_Object_Blocking_IO';
// This call is not handled by the server libraries
//function MM_Object_IO(messagehandle:Plongint; applicationrequesthandle:longint; objecthandle:longint; _function:TMM_F1_Structure; parameter0:longint;
//               parameter1:longint; parameter2:longint; bufferlength:longint; buffer:pointer; callbackroutine:procedure ):longint;cdecl;external 'clib' name 'MM_Object_IO';

type TLongintCDeclFunc = function :longint; cdecl;
     TCdeclProc        = procedure; cdecl;

function MM_Register_Application (applicationhandle:Plongint;
                                  applicationid:longint;
                                  name:PChar;
                                  reserved:longint;
                                  mediaconsoleroutine:TLongintCDeclFunc;
                                  resourcetag:PResourceTagDef):longint;cdecl;external 'clib' name 'MM_Register_Application';

function MM_Register_Identification_Routines (oshandle:Plongint;
                                              applicationhandle:longint;
                                              identifyroutine:TLongintCDeclFunc;
                                              unstamproutine:TLongintCDeclFunc;
                                              stamproutine:TLongintCDeclFunc;
                                              identifiertype:longint;
                                              identifiername:PBYTE;
                                              resourcetag:PResourceTagDef):longint;cdecl;external 'clib' name 'MM_Register_Identification_Routines';
function MM_Register_Notify_Routine (oshandle:Plongint;
                                     applicationhandle:longint;
                                     notifyroutine:TCdeclProc;
                                     objectclass:longint;
                                     eventmask:longint;
                                     resourcetag:PResourceTagDef):longint;cdecl;external 'clib' name 'MM_Register_Notify_Routine';
// This call is not handled by the server libraries
//function MM_Release_Object(objecthandle:longint; applicationhandle:longint):longint;cdecl;external 'clib' name 'MM_Release_Object';

function MM_Release_Unload_Semaphore(currentinstance:longint):longint;cdecl;external 'clib' name 'MM_Release_Unload_Semaphore';
function MM_Rename_Object(objectID:longint; para2:PBYTE):longint;cdecl;external 'clib' name 'MM_Rename_Object';
// This call is not handled by the server libraries
//function MM_Reserve_Object(objecthandle:Plongint; applicationidentifier:longint; objectid:longint; iomode:longint; applicationhandle:longint;
//               notifyroutine:function :longint):longint;cdecl;external 'clib' name 'MM_Reserve_Object';

function MM_Return_Identifier_Info(identifiernumber:longint; info:PIdentifierInfoDef):longint;cdecl;external 'clib' name 'MM_Return_Identifier_Info';
function MM_Return_Object_Attribute(objectid:longint; attributeid:longint; length:longint; info:pointer):longint;cdecl;external 'clib' name 'MM_Return_Object_Attribute';
function MM_Return_Object_Generic_Info(objectid:longint; info:PGenericInfoDef):longint;cdecl;external 'clib' name 'MM_Return_Object_Generic_Info';
function MM_Return_Object_Mapping_Info(objectid:longint; mappinginfolength:longint; mappinginfo:Plongint):longint;cdecl;external 'clib' name 'MM_Return_Object_Mapping_Info';
function MM_Return_Object_Specific_Info(objectid:longint; infolength:longint; info:pointer):longint;cdecl;external 'clib' name 'MM_Return_Object_Specific_Info';
function MM_Return_Object_Table_Size:longint;cdecl;external 'clib' name 'MM_Return_Object_Table_Size';
function MM_Return_Objects_Attributes(objectid:longint; attributeid:longint; info:PAttributeInfoDef):longint;cdecl;external 'clib' name 'MM_Return_Objects_Attributes';
function MM_Set_Object_Attribute(objecthandle:longint; attributeid:longint; length:longint; info:pointer):longint;cdecl;external 'clib' name 'MM_Set_Object_Attribute';
function MM_Set_Unload_Semaphore(currentinstance:Plongint):longint;cdecl;external 'clib' name 'MM_Set_Unload_Semaphore';
function MM_Special_Object_Blocking_IO(returnparameter:Plongint; objectnumber:longint; _function:TMM_F1_Structure; parameter0:longint; parameter1:longint;
               parameter2:longint; bufferlength:longint; buffer:pointer):longint;cdecl;external 'clib' name 'MM_Special_Object_Blocking_IO';
function MM_Unregister_Application(applicationhandle,applicationid:longint):longint;cdecl;external 'clib' name 'MM_Unregister_Application';
function MM_Unregister_Identification_Routines(handle,applicationtype:longint):longint;cdecl;external 'clib' name 'MM_Unregister_Identification_Routines';
function MM_Unregister_Notify_Routine(oshandle, applicationhandle:longint):longint;cdecl;external 'clib' name 'MM_Unregister_Notify_Routine';
{-ioctl.h----------------------------------------------------------------------}
const
   I_NWRITE = 101;
   I_SETBUF = 102;

function ioctl(fd:longint; command:longint; args:array of const):longint;cdecl;external 'clib' name 'ioctl';
function ioctl(fd:longint; command:longint):longint;cdecl;external 'clib' name 'ioctl';
{-sys/socket.h-----------------------------------------------------------------}
{$PACKRECORDS C}

type
   Piovec = ^Tiovec;
   Tiovec = record
        iov_base : Pchar;
        iov_len : longint;
     end;

{ Berkeley Sockets definitions and types  }

const
   FD_SETSIZE = 16;
type

   Pfd_array = ^Tfd_array;
   Tfd_array = longint;

   Pfd_set = ^Tfd_set;
   Tfd_set = record
        fds : Tfd_array;
     end;

   Ptimeval = ^Ttimeval;
   Ttimeval = record
     tv_sec  : longint;
     tv_usec : longint;
   end;

{ definitions related to sockets: types, address families, options  }
{ types  }
{ just NW OFSD, no socket  }

const
   SOCK_NULL = 0;
{ stream socket  }
   SOCK_STREAM = 1;
{ datagram socket  }
   SOCK_DGRAM = 2;
{ raw-protocol interface  }
   SOCK_RAW = 3;
{ reliably-delivered message  }
   SOCK_RDM = 4;
{ sequenced packet stream  }
   SOCK_SEQPACKET = 5;
{ option flags per-socket  }
{ turn on debugging info recording  }
   SO_DEBUG = $0001;
{ socket has had listen()  }
   SO_ACCEPTCONN = $0002;
{ allow local address reuse  }
   SO_REUSEADDR = $0004;
{ keep connections alive  }
   SO_KEEPALIVE = $0008;
{ just use interface addresses  }
   SO_DONTROUTE = $0010;
{ permit sending of broadcast msgs  }
   SO_BROADCAST = $0020;
{ bypass hardware when possible  }
   SO_USELOOPBACK = $0040;
{ linger on close if data present  }
   SO_LINGER = $0080;
{ leave received OOB data in line  }
   SO_OOBINLINE = $0100;
{
   N.B.: The following definition is present only for compatibility with
   release 3.0. It will disappear in later releases.
 }
{ ~SO_LINGER  }
   SO_DONTLINGER =  not (SO_LINGER);
{ additional options, not kept in so_options  }
{ send buffer size  }
   SO_SNDBUF = $1001;
{ receive buffer size  }
   SO_RCVBUF = $1002;
{ send low-water mark  }
   SO_SNDLOWAT = $1003;
{ receive low-water mark  }
   SO_RCVLOWAT = $1004;
{ send timeout  }
   SO_SNDTIMEO = $1005;
{ receive timeout  }
   SO_RCVTIMEO = $1006;
{ get error status and clear  }
   SO_ERROR = $1007;
{ get socket type  }
   SO_TYPE = $1008;
{ additional option to be used with level IPPROTO_TCP  }
{ turn off the Nagle delay algorithm  }
   TCP_NODELAY = 1;

   SIOCATMARK      = 8;              // at oob mark?
   SIOCDGRAMSIZE   = 500;
   IP_INBOUND_IF   = 501;
   IP_OUTBOUND_IF  = 502;


{ structure used for manipulating linger option  }
{ option on/off  }
{ linger time  }

type
   Plinger = ^Tlinger;
   Tlinger = record
        l_onoff : longint;
        l_linger : longint;
     end;

{ level number for get/setsockopt() to apply to socket itself  }
{ options for socket level  }

const
   SOL_SOCKET = $ffff;
{ address families  }
{ unspecified  }
   AF_UNSPEC = 0;
{ local to host (pipes, portals)  }
   AF_UNIX = 1;
{ internetwork: UDP, TCP, etc.  }
   AF_INET = 2;
{ Xerox NS protocols  }
   AF_NS = 6;
{ AppleTalk  }
   AF_APPLETALK = 16;
{ umbrella for all (e.g. protosw lookup)  }
   AF_OSI = 19;
{ U.S. Government OSI  }
   AF_GOSIP = 22;
   AF_MAX = 21;
{ structure used by kernel to store most addresses  }
{ address family  }
{ up to 14 bytes of direct address  }
type
   Psockaddr = ^Tsockaddr;
   Tsockaddr = record
        sa_family : word;
        sa_data : array[0..13] of char;
     end;

{ structure used by kernel to pass protocol information in raw sockets  }
{ address family  }
{ protocol  }
   Psockproto = ^Tsockproto;
   Tsockproto = record
        sp_family : word;
        sp_protocol : word;
     end;

{ protocol families, same as address families for now  }

const
   PF_UNSPEC = AF_UNSPEC;
   PF_UNIX = AF_UNIX;
   PF_INET = AF_INET;
   PF_NS = AF_NS;
   PF_APPLETALK = AF_APPLETALK;
   PF_OSI = AF_OSI;
   PF_GOSIP = AF_GOSIP;
   PF_MAX = AF_MAX;
{ test protocol "numbered pipe"  }
   TSTPROTO_NPIPE = 0;
{ maximum queue length specifiable by listen  }
   SOMAXCONN = 5;
{
   Message header for recvmsg and sendmsg calls.
  }
{ optional address  }
{ size of address  }
{ scatter/gather array  }
{ number of elements in msg_iov  }
{ access rights sent/received  }
type
   Pmsghdr = ^Tmsghdr;
   Tmsghdr = record
        msg_name : Pchar;
        msg_namelen : longint;
        msg_iov : Piovec;
        msg_iovlen : longint;
        msg_accrights : Pchar;
        msg_accrightslen : longint;
     end;

{ process out-of-band data  }

const
   MSG_OOB = $1;
{ peek at incoming message  }
   MSG_PEEK = $2;
{ send without using routing tables  }
   MSG_DONTROUTE = $4;
   MSG_MAXIOVLEN = 16;
{ for NLM clients  }
type TSKT = longint;

function accept(s:TSKT; addr:Psockaddr; addrlen:Plongint):longint;cdecl;external 'clib' name 'accept';
function accept(s:TSKT; var addr:Tsockaddr; var addrlen:longint):longint;cdecl;external 'clib' name 'accept';
function bind(s:TSKT; name:Psockaddr; namelen:longint):longint;cdecl;external 'clib' name 'bind';
function connect(s:TSKT; name:Psockaddr; namelen:longint):longint;cdecl;external 'clib' name 'connect';
function getpeername(s:TSKT; name:Psockaddr; namelen:Plongint):longint;cdecl;external 'clib' name 'getpeername';
function getsockname(s:TSKT; name:Psockaddr; namelen:Plongint):longint;cdecl;external 'clib' name 'getsockname';
function getsockopt(s:TSKT; level:longint; name:longint; val:Pchar; len:Plongint):longint;cdecl;external 'clib' name 'getsockopt';
function getsockopt(s:TSKT; level:longint; name:longint; val:Pchar; var len:longint):longint;cdecl;external 'clib' name 'getsockopt';

function listen(s:TSKT; backlog:longint):longint;cdecl;external 'clib' name 'listen';
function readv(s:TSKT; iov:Piovec; iovcnt:longint):longint;cdecl;external 'clib' name 'readv';
function recv(s:TSKT; msg:Pchar; len:longint; flags:longint):longint;cdecl;external 'clib' name 'recv';
function recv(s:TSKT; var data; len:longint; flags:longint):longint;cdecl;external 'clib' name 'recv';

function recvfrom(s:TSKT; msg:Pchar; len:longint; flags:longint; from:Psockaddr;
           fromlen:Plongint):longint;cdecl;external 'clib' name 'recvfrom';
function recvfrom(s:TSKT; var data; len:longint; flags:longint; from:Psockaddr;
           var fromlen:longint):longint;cdecl;external 'clib' name 'recvfrom';

function recvmsg(s:TSKT; msg:Pmsghdr; flags:longint):longint;cdecl;external 'clib' name 'recvmsg';
function send(s:TSKT; msg:Pchar; len:longint; flags:longint):longint;cdecl;external 'clib' name 'send';
function send(s:TSKT; var data; len:longint; flags:longint):longint;cdecl;external 'clib' name 'send';
function sendto(s:TSKT; msg:Pchar; len:longint; flags:longint; _to:Psockaddr;
           tolen:longint):longint;cdecl;external 'clib' name 'sendto';
function sendto(s:TSKT; var data; len:longint; flags:longint; _to:Psockaddr;
           tolen:longint):longint;cdecl;external 'clib' name 'sendto';

function sendmsg(s:TSKT; msg:Pmsghdr; flags:longint):longint;cdecl;external 'clib' name 'sendmsg';
function setsockopt(s:TSKT; level:longint; name:longint; val:Pchar; len:longint):longint;cdecl;external 'clib' name 'setsockopt';
function setsockopt(s:TSKT; level:longint; name:longint; var value; len:longint):longint;cdecl;external 'clib' name 'setsockopt';
function shutdown(s:TSKT; how:longint):longint;cdecl;external 'clib' name 'shutdown';
function socket(domain:longint; _type:longint; protocol:longint):longint;cdecl;external 'clib' name 'socket';
function writev(s:TSKT; iov:Piovec; iovcnt:longint):longint;cdecl;external 'clib' name 'writev';
function select(width:longint; readfds:Pfd_set; writefds:Pfd_set; exceptfds:Pfd_set; timeout:Ptimeval):longint;cdecl;external 'clib' name 'select';

{-sys/time.h-------------------------------------------------------------------}
{$PACKRECORDS C}

{ commonly-used definitions...  }

const
   SEC = 1;
   MILLISEC = 1000;
   MICROSEC = 1000000;
   NANOSEC = 1000000000;
{ wall clock, bound to LWP  }
   __CLOCK_REALTIME0 = 0;
{ user CPU usage clock  }
   CLOCK_VIRTUAL = 1;
{ user and system CPU usage clock  }
   CLOCK_PROF = 2;
{ wall clock, not bound  }
   __CLOCK_REALTIME3 = 3;
   CLOCK_REALTIME = __CLOCK_REALTIME3;
{ set timer relative  }
   TIMER_RELTIME = $0;
{ set timer absolute  }
   TIMER_ABSTIME = $1;
{ time expressed in seconds and nanoseconds  }
{ seconds  }
{ and nanoseconds  }
type

   Ptimespec = ^Ttimespec;
   Ttimespec = record
        tv_sec : Ttime_t;
        tv_nsec : longint;
     end;
   Ttimespec_t = Ttimespec;
   Ptimespec_t = ^Ttimespec_t;
   Ttimestrc_t = Ttimespec;
   Ptimestrc_t = ^Ttimestrc_t;

{-sys/utsname.h----------------------------------------------------------------}
const
   _SYS_NMLN = 260;

type
   Putsname = ^Tutsname;
   Tutsname = record
     sysname  : array[0..(_SYS_NMLN)-1] of char;  // name of operating system implementation
     release  : array[0..(_SYS_NMLN)-1] of char;
     version  : array[0..(_SYS_NMLN)-1] of char;
     nodename : array[0..(_SYS_NMLN)-1] of char;
     machine  : array[0..(_SYS_NMLN)-1] of char;
     _library : array[0..(_SYS_NMLN)-1] of char;
   end;

function uname(name:Putsname):longint;cdecl;external 'clib' name 'uname';
function uname(var name:Tutsname):longint;cdecl;external 'clib' name 'uname';

{-sys/stat.h-------------------------------------------------------------------}
{$PACKRECORDS C}

{ POSIX file types  }
{ type of file (mask for following)  }

const
   S_IFMT = 0170000;
{ first-in/first-out (pipe)  }
   S_IFIFO = 0010000;
{ character-special file  }
   S_IFCHR = 0020000;
{ directory  }
   S_IFDIR = 0040000;
{ blocking device (not used on NetWare)  }
   S_IFBLK = 0060000;
{ regular  }
   S_IFREG = 0100000;
{ symbolic link (not used on NetWare)  }
   S_IFLNK = 0120000;
{ Berkeley socket  }
   S_IFSOCK = 0140000;

{ POSIX file modes: owner (user) permission  }

const
   S_IRWXU = 0000700;
   S_IRUSR = 0000400;
   S_IWUSR = 0000200;
   S_IXUSR = 0000100;
   S_IREAD = S_IRUSR;
   S_IWRITE = S_IWUSR;
   S_IEXEC = S_IXUSR;
{ POSIX file modes: group permission  }
   S_IRWXG = 0000070;
   S_IRGRP = 0000040;
   S_IWGRP = 0000020;
   S_IXGRP = 0000010;
{ POSIX file modes: other permission  }
   S_IRWXO = 0000007;
   S_IROTH = 0000004;
   S_IWOTH = 0000002;
   S_IXOTH = 0000001;
{ Novell-defined additional directory modes for mkdir()  }
{ system directory  }
   S_DSYSTEM = $00100000;
{ hidden directory  }
   S_DHIDE = $00200000;
{ delete-inhibit  }
   S_DDEL_INH = $00400000;
{ rename-inhibit  }
   S_DREN_INH = $00800000;
{ purge-immediate  }
   S_DPURGE_IMM = $01000000;
{ compress-immediate  }
   S_DCOMP_IMM = $02000000;
{ no compression  }
   S_DCOMP_NO = $04000000;
{ equivalent to mode = 0  }
   S_DALL = $01FB;
{ POSIX setuid, setgid, and sticky  }
   S_ISUID = 0004000;
   S_ISGID = 0002000;
   S_ISVTX = 0001000;

type
   Pstat = ^Tstat;
   Tstat = record
        st_dev : Tdev_t;
        st_ino : Tino_t;
        st_mode : word;
        st_pad1 : word;
        st_nlink : dword;
        st_uid : dword;
        st_gid : dword;
        st_rdev : Tdev_t;
        st_size : Toff_t;
        st_atime : Ttime_t;
        st_mtime : Ttime_t;
        st_ctime : Ttime_t;
        st_btime : Ttime_t;
        st_attr : dword;
        st_archivedID : dword;
        st_updatedID : dword;
        st_inheritedRightsMask : word;
        st_pad2 : word;
        st_originatingNameSpace : dword;
        st_blksize : Tsize_t;
        st_blocks : Tsize_t;
        st_flags : dword;
        st_spare : array[0..3] of dword;
        st_name : array[0..(255 + 1)-1] of byte;
     end;

{ definitions of older structure technology are mostly for reference  }
{ v4.11  }
{----------------- new fields starting in v4.11 -------------------------  }
   Pstat411 = ^Tstat411;
   Tstat411 = record
        st_dev : Tdev_t;
        st_ino : Tino_t;
        st_mode : word;
        st_nlink : smallint;
        st_uid : dword;
        st_gid : smallint;
        st_rdev : Tdev_t;
        st_size : Toff_t;
        st_atime : Ttime_t;
        st_mtime : Ttime_t;
        st_ctime : Ttime_t;
        st_btime : Ttime_t;
        st_attr : dword;
        st_archivedID : dword;
        st_updatedID : dword;
        st_inheritedRightsMask : word;
        st_originatingNameSpace : byte;
        st_name : array[0..(255 + 1)-1] of byte;
        st_blksize : Tsize_t;
        st_blocks : Tsize_t;
        st_flags : dword;
        st_spare : array[0..3] of dword;
     end;

{ v3.12, v4.0, v4.01, v4.02 and v4.10  }
   Pstat410 = ^Tstat410;
   Tstat410 = record
        st_dev : Tdev_t;
        st_ino : Tino_t;
        st_mode : word;
        st_nlink : smallint;
        st_uid : dword;
        st_gid : smallint;
        st_rdev : Tdev_t;
        st_size : Toff_t;
        st_atime : Ttime_t;
        st_mtime : Ttime_t;
        st_ctime : Ttime_t;
        st_btime : Ttime_t;
        st_attr : dword;
        st_archivedID : dword;
        st_updatedID : dword;
        st_inheritedRightsMask : word;
        st_originatingNameSpace : byte;
        st_name : array[0..12] of byte;
     end;

function chmod(path:Pchar; mode:Tmode_t):longint;cdecl;external 'clib' name 'chmod';
function mkdir(path:Pchar):longint;cdecl;external 'clib' name 'mkdir';
function mkdir_510(pathname:Pchar; mode:Tmode_t):longint;cdecl;external 'clib' name 'mkdir_510';
function umask(cmask:Tmode_t):Tmode_t;cdecl;external 'clib' name 'umask';
function fstat_410(fildes:longint; buf:Pstat410):longint;cdecl;external 'clib' name 'fstat_410';
function fstat_410(fildes:longint; var buf:Tstat410):longint;cdecl;external 'clib' name 'fstat_410';
function fstat_411(fildes:longint; buf:Pstat411):longint;cdecl;external 'clib' name 'fstat_411';
function fstat_411(fildes:longint; var buf:Tstat411):longint;cdecl;external 'clib' name 'fstat_411';
function fstat_500(fildes:longint; buf:Pstat):longint;cdecl;external 'clib' name 'fstat_500';
function fstat_500(fildes:longint; var buf:Tstat):longint;cdecl;external 'clib' name 'fstat_500';
function fstat (fildes:longint; buf:Pstat):longint;cdecl;external 'clib' name 'fstat_500';
function fstat (fildes:longint; var buf:Tstat):longint;cdecl;external 'clib' name 'fstat_500';

function stat_410(path:Pchar; buf:Pstat410):longint;cdecl;external 'clib' name 'stat_410';
function stat_410(path:Pchar; var buf:Tstat410):longint;cdecl;external 'clib' name 'stat_410';
function stat_411(path:Pchar; buf:Pstat411):longint;cdecl;external 'clib' name 'stat_411';
function stat_411(path:Pchar; var buf:Tstat411):longint;cdecl;external 'clib' name 'stat_411';
function stat_500(path:Pchar; buf:Pstat):longint;cdecl;external 'clib' name 'stat_500';
function stat_500(path:Pchar; var buf:Tstat):longint;cdecl;external 'clib' name 'stat_500';
function stat (path:Pchar; buf:Pstat):longint;cdecl;external 'clib' name 'stat_500';
function stat (path:Pchar; var buf:Tstat):longint;cdecl;external 'clib' name 'stat_500';
{------------------------------------------------------------------------------}
{definitions for netwareAlert, not documented, found that on the novell developer newsgroup}
const
// ModuleNumbers for 'nwAlertID' in TNetWareAlertStructure
   ALERT_BINDERY            = $01020000;     // Bindery Subject
   ALERT_OS                 = $01030000;     // OS Event Subject
   ALERT_LLC                = $01040000;     // LLC
   ALERT_SDLC               = $01050000;     // SDLC Stack
   ALERT_REMOTE             = $01060000;     // RConsole
   ALERT_MLID               = $01070000;     // MLID LAN Drivers
   ALERT_QLLC               = $01080000;     // QLLC
   ALERT_UPS                = $01090000;     // UPS Monitor
   ALERT_DS                 = $010a0000;     // Directory Service
   ALERT_RSPX               = $010c0000;     // RSPX
   ALERT_R232               = $010d0000;     // R232
   ALERT_TIME_SYNC          = $010e0000;     // TimeSync
   ALERT_CLIB               = $010f0000;     // CLib
   ALERT_PRINT              = $01100000;     // Print
   ALERT_NRS                = $01200000;     // Novell Replication Services
   ALERT_DNS                = $01300000;     // IP/Domain Name Services
   ALERT_DHCP               = $01400000;     // DHCP Services
   ALERT_MM                 = $01500000;     // Media Manager

// OS-defined AlertNumber values for nwAlertID in TNetWareAlertStructure
// starting with NetWare 4...
   nmAllocFailed                           = 1;
   nmErrWrtExtDir                          = 2;
   nmSysErrWrtDSnoFN                       = 3;
   nmStaErrWrtDSnoFN                       = 4;
   nmSysErrWrtDSwithFN                     = 5;
   nmStaErrWrtDSwithFN                     = 6;
   nmSysErrRdDSnoFN                        = 7;
   nmStaErrRdDSnoFN                        = 8;
   nmSysErrRdDSwithFN                      = 9;
   nmStaErrRdDSwithFN                      = 10;
   nmSysWrtPreRDnoFN                       = 11;
   nmStaWrtPreRDnoFN                       = 12;
   nmSysWrtPreRDwithFN                     = 13;
   nmStaWrtPreRDwithFN                     = 14;
   nmCacheMemLimitExceded                  = 15;
   nmCacheMemOutOfMem                      = 16;
   nmCacheBufsGetLo                        = 17;
   nmDskSpcNoDelFiles                      = 18;
   nmDskSpcNoLimbo                         = 19;
   nmVolSpcAlmostGone                      = 20;
   nmFATWrtErr                             = 21;
   nmDirWrtErr                             = 22;
   nmDirCopyRdErr                          = 23;
   nmDirDblRdErr                           = 24;
   nmAllocDirWrtErr                        = 25;
   nmDirExpansionErr                       = 26;
   nmDirTooLarge                           = 27;
   nmErrExpandingDir                       = 28;
   nmErrExpandingMem                       = 29;
   nmErrDirGetTooLarge                     = 30;
   nmDskBottleneck                         = 31;
   nmWDClearedConn                         = 32;
   nmCpyrtViolation                        = 33;
   nmReadFault                             = 35;
   nmPktTooSmall                           = 36;
   nmCreatingVolLog                        = 37;
   nmWrtVolLog                             = 38;
   nmVolDmtDevDeact                        = 39;
   nmLoginDisabled                         = 40;
   nmLoginEnabled                          = 41;
   nmClrSta                                = 42;
   nmClrStaByUsr                           = 43;
   nmFSDownByUser                          = 44;
   nmRIPAlreadyOpn                         = 45;
   nmRouterConfigErr                       = 46;
   nmLANLoopbackErr                        = 47;
   nmRouterConfigErrNoInfo                 = 48;
   nmIPXUnreachable                        = 49;
   nmIPXUnbind                             = 50;
   nmSAPAlreadyOpn                         = 51;
   nmRouterConfigErrNameInfo               = 52;
   nmSpuriousInt                           = 53;
   nmChecksumInvalidAlert                  = 54;
   nmPrimaryPicLostInt                     = 55;
   nmSecondaryPicLostInt                   = 56;
   nmCompErrHoleCountMismatch              = 57;
   nmInvalidScreen                         = 58;
   nmRelinquishControl                     = 59;
   nmFSUserDeleted                         = 60;
   nmAccDelByUser                          = 61;
   nmInvalidRTag                           = 62;
   nmDeactUnknown                          = 63;
   nmDeactDriveUnld                        = 64;
   nmDeactDevFailure                       = 65;
   nmDeactUsrRequest                       = 66;
   nmDeactMediaDismount                    = 67;
   nmDeactMediaEject                       = 68;
   nmDeactServerDown                       = 69;
   nmDeactServerFailure                    = 70;
   nmResourceRelErr                        = 71;
   nmMirrorsNotSync                        = 72;
   nmMirrorsSyncUp                         = 73;
   nmPartMirrorSync                        = 74;
   nmPartMirrorNotSync                     = 75;
   nmReMirroringPart                       = 76;
   nmReMirroringPartAborted                = 77;
   nmLogPartMirrorInconsist                = 78;
   nmSysFileLockThresh                     = 79;
   nmStaFileLockThresh                     = 80;
   nmSysRecLockThresh                      = 81;
   nmStaRecLockThresh                      = 82;
   nmOpnNETACCTFailed                      = 83;
   nmNCPSearchLimitSys                     = 84;
   nmNCPSearchLimitSta                     = 85;
   nmInsMediaAck                           = 86;
   nmInsMediaAborted                       = 87;
   nmRemMediaAck                           = 88;
   nmRemMediaAbort                         = 89;
   nmInsMediaInto                          = 90;
   nmRemMediaFrom                          = 91;
   nmReDirectedBlockPart                   = 92;
   nmReDirectedBlockPartErr                = 93;
   nmOutOfHotFixBlocks                     = 94;
   nmLowWarningHotFixBlocks                = 95;
   nmReDirectInconsistNoFix                = 96;
   nmReDirectInconsistFixed                = 97;
   nmInvalidRTagHOptions                   = 98;
   nmCheckAndAddHWNoGetRTag                = 99;
   nmRemHWBadPtr                           = 100;
   nmErrUnldNLM                            = 101;
   nmIvldRTagCrProc                        = 102;
   nmCrProcStkTooSmall                     = 103;
   nmCrProcNoPCB                           = 104;
   nmDelToLimboFileErr                     = 105;
   nmDelToLimboNoSpace                     = 106;
   nmMLIDResetLanBd                        = 107;
   nmRouterReset                           = 108;
   nmVolWrongDOSType                       = 109;
   nmNoOwnerNSfound                        = 110;
   nmRTDMDefSMchanged                      = 111;
   nmErrOpnTTSLOG                          = 112;
   nmErrWrtTTSLOG                          = 113;
   nmTTSdownVolDismount                    = 114;
   nmTTSdisableByStaUsr                    = 115;
   nmTTSdisByOp                            = 116;
   nmTTSdisErrRdBackFile                   = 117;
   nmTTSdisErrWrBackFile                   = 118;
   nmTTSdisTooManyDefVol                   = 119;
   nmTTSdisWrtVolDefInfo                   = 120;
   nmTTSdisErrRdBkFlRecGen                 = 121;
   nmTTSdisGrowMemTables                   = 122;
   nmTTSdisErrAllDiskSp                    = 123;
   nmTTSdisDirErrOnBkFile                  = 124;
   nmTTSEnableByStaUsr                     = 125;
   nmTTStransAbortedForSta                 = 126;
   nmTTStooManyTransDelaying               = 127;
   nmTTSNoMemForExpTransNodes              = 128;
   nmAuditEvent                            = 129;
   nmAuditDisNoAuditCfg                    = 130;
   nmInvldConnTypeToAllocConn              = 131;
   nmInvldRTagToAllocConn                  = 132;
   nmOutOfServerConns                      = 133;
   nmConnTermAfter5Min                     = 134;
   nmUsrAccDisableBySta                    = 135;
   nmUnEncryptPwdNotAllowed                = 136;
   nmSuperAccLockedByConsole               = 137;
   nmSystemTimeChangedByCon                = 138;
   nmSystemTimeChangedBySta                = 139;
   nmVolStillActWithError                  = 140;
   nmRouterFalsehood                       = 141;
   nmServerAddressChanged                  = 142;
   nmExtFileNoOwnerCharge                  = 143;
   nmRouterConfigErrNode                   = 144;
   nmRouterConfigErrMyAddr                 = 145;
   nmNoMigratorLd                          = 146;
   nmNoSMLd                                = 147;
   nmNotEnoughRamForCompression            = 148;
   nmDiskErrorCompressing                  = 149;
   nmUnknownErrorCompressing               = 150;
   nmInsufficientSpaceForDeCompression     = 151;
   nmDecompressUnknownCompressionVersion   = 152;
   nmUnknownDecompressError                = 153;
   nmInsufficientRAMToDecompress           = 154;
   nmCompressedFileIsCorrupt               = 155;
   nmStaAttemptedToUseBadPckt              = 156;
   nmStaUsedABadPckt                       = 157;
   nmStaAttemptedToUseBadSFL               = 158;
   nmStaUsedABadSFL                        = 159;
   nmCorruptCompFileWithName               = 160;
   nmCorruptCompFileWithNameAndStation     = 161;
   nmLowPriThreadsNotRun                   = 162;
   nmWorkToDoNotRun                        = 163;
   nmCompressErrorTempFileError            = 164;
   nmCompressErrorLengthTotalsMismatch     = 165;
   nmCompressErrorOffsetTotalsMismatch     = 166;
   nmCompressErrorDataCodeCountMismatch    = 167;
   nmCompressErrorLengthCountMismatch      = 168;
   nmCompressErrorLargeLengthCountMismatch = 169;
   nmCompressErrorReadZeroBytesOrg         = 170;
   nmCompressErrorTreeTooBig               = 171;
   nmCompressErrorMatchSizeFail            = 172;
   nmSignatureInvalidAlert                 = 173;
   nmLicenseIsInvalid                      = 174;
   nmDeactHotFixError                      = 175;
   nmUnknownDecompressErrorFN              = 176;
   nmInsufficientRAMToDecompressFN         = 177;
   nmDecompressUnderFreePercentage         = 178;
   nmNegPktTriedLargeBuffer                = 179;
   nmLoginDisabledByConsole                = 180;
   nmLoginEnabledByConsole                 = 181;
   nmGrwStkNotAvail                        = 182;
   nmLicenseFileIsMissing                  = 183;
   nmFailedToDeletedMigratedFile           = 184;
   nmNoMemForAuditing                      = 185;
   nmAuditFileWriteError                   = 186;
   nmAuditFileFull                         = 187;
   nmAuditFileThresholdOverflow            = 188;
   nmCompressErrorReadZeroBytesInt         = 189;
   nmEASpaceLimit                          = 190;
   nmThreadAreaNotEmpty                    = 191;
   nmErrMovingLogToMSEngine                = 192;
   nmFaultInConsoleCmdHandler              = 193;
   nmServerToServerComLinkActivated        = 194;
   nmServerToServerComLinkFailure          = 195;
   nmServerToServerComLinkDeact            = 196;
   nmOtherServerAttemptedToSync            = 197;
   nmServerToServerComLinkBrokeOK          = 198;
   nmServerSyncStartingIAmSecondary        = 199;
   nmBadSvrInitMsgFromOtherSvr             = 200;
   nmSvrToSvrCommLinkInitFailed            = 201;
   nmFailedDuringSyncWithReason            = 202;
   nmCommDrvLdDuringActivateWait           = 203;
   nmErrWritingStatusDump                  = 204;
   nmComDrvFailureOnPrimary                = 205;
   nmComDrvFailureOnSecondary              = 206;
   nmErrFinishingGenStatusDump             = 207;
   nmSFTIIWhatToDoWithReasonString         = 208;
   nmSFTIIErrorUnexpected                  = 209;
   nmSyncErrFromCustomServerNLM            = 210;
   nmSvrLinkHasPluggedPacket               = 211;
   nmSvrToBeRevived                        = 212;
   nmServersAreSyncPri                     = 213;
   nmSvrCantRouteIPXSec                    = 214;
   nmSrvIPXRouteInfoSec                    = 215;
   nmErrGivingRAMtoMS                      = 216;
   nmMoreRAMgivenToMS                      = 217;
   nmServersAreSyncSec                     = 218;
   nmSvrCantRouteIPXPri                    = 219;
   nmSrvIPXRouteInfoPri                    = 220;
   nmPriSvrFailedButSecDown                = 221;
   nmPriSvrFailedNewPri                    = 222;
   nmNumMemSegsExceedLimit                 = 223;
   nmNumScreenExceedsLimit                 = 224;
   nmIOVersionMismatch                     = 225;
   nmOtherSvrProtectLvlNoMatch             = 226;
   nmOtherSvrScrAddrMismatch               = 227;
   nmIOEngNotAtSameAddr                    = 228;
   nmBothSvrHaveMSEng                      = 229;
   nmNoMSEngOnServers                      = 230;
   nmSecSvrMissingRAM                      = 231;
   nmBothSrvHaveSameIPXAddr                = 232;
   nmIOEngIPXAddrMatchMSEng                = 233;
   nmIOEngsMismatchRxSizes                 = 234;
   nmIOEngsHaveSameName                    = 235;
   nmNoMemForIOEngName                     = 236;
   nmSrvToSvrLinkBeginSync                 = 237;
   nmMSEngActivated                        = 238;
   nmMSEngActNowSyncOther                  = 239;
   nmIOtoMSComMisMatchUnload               = 240;
   nmSFTIIIOutOfMsgCodes                   = 241;
   nmErrXferDumpToSystem                   = 242;
   nmFailureChkPrimary                     = 243;
   nmNoMemForOtherIOEngScr                 = 244;
   nmErrStarting2ndProc                    = 245;
   nmSrvFailureMsg                         = 246;
   nmSecIOEngSupModNotLd                   = 247;
   nmMSLBdNumHasConn                       = 248;
   nmSecSvrLANIsBetter                     = 249;
   nmIPXrtnStatusPckts                     = 250;
   nmIPXnotRtnStatChkPckts                 = 251;
   nmIPXnotRtnStatLANJam                   = 252;
   nmFailReasonByOtherSrv                  = 253;
   nmIPXMayBeTooSlowForSecSrv              = 254;
   nmIPXToOtherSrvTooManyHops              = 255;
   nmIPXappearsDown                        = 256;
   nmIPXFoundRouteToOtherSrv               = 257;
   nmIPXLostRoute                          = 258;
   nmSecSrvGoingToDie                      = 259;
   nmPriSrcDyingTimerStart                 = 260;
   nmPriSrvDying                           = 261;
   nmIPXInternetIsJammed                   = 262;
   nmIPXNewRouteToSecSvr                   = 263;
   nmSrvsSyncing                           = 264;
   nmFSHookRegistered                      = 265;
   nmFSHookDeRegistered                    = 266;
   nmIOEngCantBorrowMemory                 = 267;
   nmDecompressNoCompressionOnVolume       = 268;
   nmMkProcessUsingTooSmallStk             = 269;
   nmQueueEventReportNoMemory              = 270;
   nmServerPartMirrorNotSync               = 271;
   nmStaWithoutRightsConsoleRPC            = 272;
   nmAuditOverflowFileThreshold            = 273;
   nmAuditOverflowFileFull                 = 274;
   nmSwitchStacksGrwStk                    = 275;
   nmConsoleCommandProcRestarted           = 276;
   nmGrowableStackGrew                     = 278;
   nmOtherSvrIOLogSpaceNoMatch             = 279;
   nmDFSLogicalStackRead                   = 280;
   nmDFSLogicalStackWrite                  = 281;
   nmSecureEraseFailure                    = 282;
   nmDropBadPktBurstConn                   = 283;
   nmOutOfIPXSockets                       = 284;
   nmVolumeObjectIDChanged                 = 285;
   nmAbendRecovery                         = 286;
   nmOpLockTimeout                         = 287;
   nmAbendRecovered                        = 288;

// starting with NetWare 5...
   nmUnknownSetCmd                         = 289;
   nmAddressSpaceProtectionFault           = 290;
   nmAddressSpaceFailedToRestart           = 291;
   nmAddressSpaceRestarted                 = 292;
   nmCorruptMemoryNodeDetected             = 293;
   nmAddressSpaceCleanupFailure            = 294;
   nmInvalidParameter                      = 295;
   nmInvalidObjectHandle                   = 296;
   nmNullPointer                           = 297;
   nmVolDmtMedDmt                          = 298;
   nmVolDmtmedChgd                         = 299;
   nmAccDelByUsrActConn                    = 300;
   nmResourcesRelErr                       = 301;
   nmDemoVersion                           = 302;
   nmDemoVersionTooLong                    = 303;
   nmLicenseReSellerFileIsMissing          = 304;
   nmLicenseUpgradeIsMissing               = 305;
   nmLicenseVersionInvalid                 = 306;
   nmLicenseProductInvalid                 = 307;
   nmLicenseNoMoreFiles                    = 308;
   nmLicensePIDInvalid                     = 309;
   nmLicenseContentInalid                  = 310;
   nmLicenseBadUpgrade                     = 311;
   nmLicensePrevMaxConnMisMatch            = 312;
   nmLicenseContentResellerBad             = 313;
   nmLicenseSNMisMatch                     = 314;
   nmLicenseUIDMisMatch                    = 315;
   nmLicenseOpenError                      = 316;
   nmLicenseCompanionErr                   = 317;
   nmLicenseSNUpgradeMisMatch              = 318;
   nmLicenseUnableToRemMSL                 = 319;
   nmLicenseUnableToRemULF                 = 320;
   nmLicenseUnableToRemRLF                 = 321;
   nmLicenseUnableToGetFileSize            = 322;
   nmLicenseUnkLicenseType                 = 323;
   nmLicenseReadErr                        = 324;
   nmLicenseFileSizeMisMatch               = 325;
   nmLicenseDupServerLic                   = 326;
   nmLicenseNeedUpgrade                    = 327;
   nmLicenseMirrorNeedUpgrade              = 328;
   nmLicenseDupLicDiscovered               = 329;
   nmLicenseDupLicDiscoveredDel            = 330;
   nmLicenseCpyRightViolated               = 331;
   nmLicenseExpired                        = 332;
   nmVolDmtDevMedChgd                      = 333;
   nmVolDmtDevMedDmt                       = 334;
   nmInsMediaAckDS                         = 335;
   nmInsMediaAckMag                        = 336;
   nmInsMediaAbortedDS                     = 337;
   nmInsMediaAbortedMag                    = 338;
   nmRemMediaAckDS                         = 339;
   nmRemMediaAckMag                        = 340;
   nmRemMediaAbortDS                       = 341;
   nmRemMediaAbortMag                      = 342;
   nmInsMediaIntoDS                        = 343;
   nmInsMediaIntoMag                       = 344;
   nmRemMediaFromDS                        = 345;
   nmRemMediaFromMag                       = 346;
   nmServAddr                              = 347;
   nmSwapInError                           = 348;
   nmSwapOutError                          = 349;
   nmAveragePageInThresholdExceeded        = 350;
   nmIllegalRequest                        = 351;
   nmTTSThrottleDelayError                 = 352;
   nmTTSLackOfResourcesError               = 353;
   nmTTSLackOfResourcesNoReason            = 354;
   nmDelayedWTDNotRunning                  = 355;
   nmInvalidCharacterInName                = 356;

// starting with NetWare 6
   nmMPKBadThreadState                     = 357;
   nmPoolSeriousError                      = 358;
   nmPoolSeriousReadError                  = 359;
   nmVolSeriousError                       = 360;
   nmVolSeriousReadError                   = 361;
   nmVolDeactSeriousIOError                = 362;
   nmVolDeactSeriousNonIOError             = 363;
   nmPoolDeactSeriousIOError               = 364;
   nmPoolDeactSeriousNonIOError            = 365;
   nmTaskZeroCheck                         = 366;

// Values for nwAlertLocus
   LOCUS_UNKNOWN               = 0;
   LOCUS_MEMORY                = 1;
   LOCUS_FILESYSTEM            = 2;
   LOCUS_DISKS                 = 3;
   LOCUS_LANBOARDS             = 4;
   LOCUS_COMSTACKS             = 5;
   LOCUS_TTS                   = 7;
   LOCUS_BINDERY               = 8;
   LOCUS_STATION               = 9;
   LOCUS_ROUTER                = 10;
   LOCUS_LOCKS                 = 11;
   LOCUS_KERNEL                = 12;
   LOCUS_UPS                   = 13;
   LOCUS_SERVICE_PROTOCOL      = 14;
   LOCUS_SFT_III               = 15;
   LOCUS_RESOURCE_TRACKING     = 16;
   LOCUS_NLM                   = 17;
   LOCUS_OS_INFORMATION        = 18;
   LOCUS_CACHE                 = 19;

   AlertIDValidMask         = $00000002;
   AlertLocusValidMask      = $00000004;
   NoDisplayAlertIDBit      = $20000000;

   NOTIFY_CONNECTION_BIT      = $00000001;
   NOTIFY_EVERYONE_BIT        = $00000002;
   NOTIFY_ERROR_LOG_BIT       = $00000004;
   NOTIFY_CONSOLE_BIT         = $00000008;
   NOTIFY_QUEUE_MESSAGE       = $10000000;
   NOTIFY_DONT_NOTIFY_NMAGENT = $80000000;

// ERROR CLASSES
   CLASS_UNKNOWN                 =  0;
   CLASS_OUT_OF_RESOURCE         =  1;
   CLASS_TEMP_SITUATION          =  2;
   CLASS_AUTHORIZATION_FAILURE   =  3;
   CLASS_INTERNAL_ERROR          =  4;
   CLASS_HARDWARE_FAILURE        =  5;
   CLASS_SYSTEM_FAILURE          =  6;
   CLASS_REQUEST_ERROR           =  7;
   CLASS_NOT_FOUND               =  8;
   CLASS_BAD_FORMAT              =  9;
   CLASS_LOCKED                  = 10;
   CLASS_MEDIA_FAILURE           = 11;
   CLASS_ITEM_EXISTS             = 12;
   CLASS_STATION_FAILURE         = 13;
   CLASS_LIMIT_EXCEEDED          = 14;
   CLASS_CONFIGURATION_ERROR     = 15;
   CLASS_LIMIT_ALMOST_EXCEEDED   = 16;
   CLASS_SECURITY_AUDIT_INFO     = 17;
   CLASS_DISK_INFORMATION        = 18;
   CLASS_GENERAL_INFORMATION     = 19;
   CLASS_FILE_COMPRESSION        = 20;
   CLASS_PROTECTION_VIOLATION    = 21;
   CLASS_VIRTUAL_MEMORY          = 22;

   SEVERITY_INFORMATIONAL         = 0;
   SEVERITY_WARNING               = 1;
   SEVERITY_RECOVERABLE           = 2;
   SEVERITY_CRITICAL              = 3;
   SEVERITY_FATAL                 = 4;
   SEVERITY_OPERATION_ABORTED     = 5;
   SEVERITY_NONOS_UNRECOVERABLE   = 6;

type
     TnwAlertDataFreeProc = procedure (nwAlertDataPtr:pointer);cdecl;
     PNetWareAlertStructure  = ^TNetWareAlertStructure;
     TNetWareAlertStructure = record
       pNetworkManagementAttribute  : pointer;
       nwAlertFlags,
       nwTargetStation,
       nwTargetNotificationBits,
       nwAlertID,
       nwAlertLocus,
       nwAlertClass,
       nwAlertSeverity              : longint;
       nwAlertDataPtr               : pointer;
       nwAlertDataFree              : TnwAlertDataFreeProc;
       nwControlString              : pchar;
       nwControlStringMessageNumber : longint;
     end;


procedure NetWareAlert(nlmHandle      : TNlmHandle;
                       nwAlert        : PNetWareAlertStructure;
                       parameterCount : longint;
                       args           : array of const); cdecl; external 'clib';

procedure NetWareAlert(nlmHandle      : TNlmHandle;
                       nwAlert        : PNetWareAlertStructure;
                       parameterCount : longint); cdecl; external 'clib';

{------------------------------------------------------------------------------}

implementation

function CLK_TCK : longint;
begin
   CLK_TCK:=__get_CLK_TCK;
end;

function tzname : pchar;
begin
   tzname:=__get_tzname^;
end;

function NetWareErrno : longint;
begin
  NetWareErrno := __get_NWErrno_ptr()^;
end;

function __stdin : PFILE;
begin
  __stdin := __get_stdin^;
end;

function __stdout : PFILE;
begin
  __stdout := __get_stdout^;
end;

function __stderr : PFILE;
begin
  __stderr := __get_stderr^;
end;

function bisecond(var a : TDOSTime) : word;
begin
   bisecond:=(a.flag0 and bm_TDOSTime_bisecond) shr bp_TDOSTime_bisecond;
end;

procedure set_bisecond(var a : TDOSTime; __bisecond : word);
begin
   a.flag0:=a.flag0 or ((__bisecond shl bp_TDOSTime_bisecond) and bm_TDOSTime_bisecond);
end;

function minute(var a : TDOSTime) : word;
begin
   minute:=(a.flag0 and bm_TDOSTime_minute) shr bp_TDOSTime_minute;
end;

procedure set_minute(var a : TDOSTime; __minute : word);
begin
   a.flag0:=a.flag0 or ((__minute shl bp_TDOSTime_minute) and bm_TDOSTime_minute);
end;

function hour(var a : TDOSTime) : word;
begin
   hour:=(a.flag0 and bm_TDOSTime_hour) shr bp_TDOSTime_hour;
end;

procedure set_hour(var a : TDOSTime; __hour : word);
begin
   a.flag0:=a.flag0 or ((__hour shl bp_TDOSTime_hour) and bm_TDOSTime_hour);
end;

function day(var a : TDOSDate) : word;
begin
   day:=(a.flag0 and bm_TDOSDate_day) shr bp_TDOSDate_day;
end;

procedure set_day(var a : TDOSDate; __day : word);
begin
   a.flag0:=a.flag0 or ((__day shl bp_TDOSDate_day) and bm_TDOSDate_day);
end;

function month(var a : TDOSDate) : word;
begin
   month:=(a.flag0 and bm_TDOSDate_month) shr bp_TDOSDate_month;
end;

procedure set_month(var a : TDOSDate; __month : word);
begin
   a.flag0:=a.flag0 or ((__month shl bp_TDOSDate_month) and bm_TDOSDate_month);
end;

function yearsSince80(var a : TDOSDate) : word;
begin
   yearsSince80:=(a.flag0 and bm_TDOSDate_yearsSince80) shr bp_TDOSDate_yearsSince80;
end;

procedure set_yearsSince80(var a : TDOSDate; __yearsSince80 : word);
begin
   a.flag0:=a.flag0 or ((__yearsSince80 shl bp_TDOSDate_yearsSince80) and bm_TDOSDate_yearsSince80);
end;

function bisecond(var a : T_DOSTime) : word;
begin
   bisecond:=(a.flag0 and bm_T_DOSTime_bisecond) shr bp_T_DOSTime_bisecond;
end;

procedure set_bisecond(var a : T_DOSTime; __bisecond : word);
begin
   a.flag0:=a.flag0 or ((__bisecond shl bp_T_DOSTime_bisecond) and bm_T_DOSTime_bisecond);
end;

function minute(var a : T_DOSTime) : word;
begin
   minute:=(a.flag0 and bm_T_DOSTime_minute) shr bp_T_DOSTime_minute;
end;

procedure set_minute(var a : T_DOSTime; __minute : word);
begin
   a.flag0:=a.flag0 or ((__minute shl bp_T_DOSTime_minute) and bm_T_DOSTime_minute);
end;

function hour(var a : T_DOSTime) : word;
begin
   hour:=(a.flag0 and bm_T_DOSTime_hour) shr bp_T_DOSTime_hour;
end;

procedure set_hour(var a : T_DOSTime; __hour : word);
begin
   a.flag0:=a.flag0 or ((__hour shl bp_T_DOSTime_hour) and bm_T_DOSTime_hour);
end;

end.
