{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team
    Copyright (c) 2001-2004 Armin Diehl

    Interface to netware clib

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


CONST Clib       = 'clib';
      ThreadsNlm = 'threads';

TYPE
  dev_t         = LONGINT;
  ino_t         = LONGINT;
  unsignedshort = WORD;
  unsignedlong  = LONGINT;
  unsignedint   = LONGINT;
  off_t         = LONGINT;
  size_t        = LONGINT;
  time_t        = LONGINT;
  NWStatBufT = PACKED RECORD
   st_dev        : dev_t;         (* volume number *)
   st_ino        : ino_t;         (* directory entry number of the st_name *)
   st_mode       : unsignedshort; (* emulated file mode *)
   st_nlink      : unsignedshort; (* count of hard links (always 1) *)
   st_uid        : unsignedlong;  (* object id of owner *)
   st_gid        : unsignedshort; (* group-id (always 0) *)
   st_rdev       : dev_t;         (* device type (always 0) *)
   st_size       : off_t;         (* total file size--files only *)
   st_atime      : time_t;        (* last access date--files only *)
   st_mtime      : time_t;        (* last modify date and time *)
   st_ctime      : time_t;        (* POSIX: last status change time... *)
                                  (* ...NetWare: creation date/time *)
   st_btime      : time_t;        (* last archived date and time *)
   st_attr       : unsignedlong;  (* file attributes *)
   st_archivedID : unsignedlong;  (* user/object ID of last archive *)
   st_updatedID  : unsignedlong;  (* user/object ID of last update *)
   st_inheritedRightsMask
                 : unsignedshort; (* inherited rights mask *)
   st_originatingNameSpace
                 : BYTE;          (* namespace of creation       *)
   st_name       : ARRAY [0..255] OF CHAR;
                                  (* TARGET_NAMESPACE name *)
   st_blksize    : LONGINT;
   st_blocks     : LONGINT;
   st_flags      : LONGINT;
   st_spare      : ARRAY [0..3] OF LONGINT;
  END;

FUNCTION  _stat  (path : PCHAR; VAR buf : NWStatBufT) : LONGINT; CDECL; EXTERNAL Clib NAME 'stat_411';
FUNCTION  _fstat (Fileno : LONGINT; VAR buf : NWStatBufT) : LONGINT; CDECL; EXTERNAL CLib NAME 'fstat_411';

PROCEDURE NWFree   (P : POINTER); CDECL; EXTERNAL Clib NAME 'free';

PROCEDURE PressAnyKeyToContinue; CDecl; EXTERNAL; // Clib;


PROCEDURE ExitThread (action_code, termination_code : LONGINT); CDecl; EXTERNAL CLib;
FUNCTION _BeginThread (func, stack : pointer; Stacksize : LONGINT; arg : pointer) : LONGINT; Cdecl; EXTERNAL CLib NAME 'BeginThread';
FUNCTION _GetThreadDataAreaPtr : POINTER; CDecl; EXTERNAL CLib NAME 'GetThreadDataAreaPtr';
PROCEDURE _SaveThreadDataAreaPtr (P : POINTER); CDecl; EXTERNAL CLib NAME 'SaveThreadDataAreaPtr';
PROCEDURE _exit (ExitCode : LONGINT); CDecl; EXTERNAL CLib;
function _SuspendThread(threadID:longint):longint; cdecl;external ThreadsNlm name 'SuspendThread';
function _GetThreadID:longint; cdecl;external ThreadsNlm name 'GetThreadID';
procedure _ThreadSwitchWithDelay; cdecl;external ThreadsNlm name 'ThreadSwitchWithDelay';
function _GetThreadName(threadID:longint; var tName):longint; cdecl;external ThreadsNlm name 'GetThreadName';
function GetNLMHandle:dword;  cdecl;external ThreadsNlm name 'GetNLMHandle';
(*
PROCEDURE ConsolePrintf (FormatStr : PCHAR; Param : LONGINT); CDecl; EXTERNAL CLib Name 'ConsolePrintf';
PROCEDURE ConsolePrintf (FormatStr : PCHAR; Param : pchar); CDecl; EXTERNAL CLib Name 'ConsolePrintf';
PROCEDURE ConsolePrintf (FormatStr : PCHAR; P1,P2 : LONGINT); CDecl; EXTERNAL CLib Name 'ConsolePrintf';
PROCEDURE ConsolePrintf (FormatStr : PCHAR; P1,P2,P3 : LONGINT); CDecl; EXTERNAL CLib Name 'ConsolePrintf';
PROCEDURE ConsolePrintf (FormatStr : PCHAR; P1,P2,P3,P4 : LONGINT); CDecl; EXTERNAL CLib Name 'ConsolePrintf';
PROCEDURE ConsolePrintf (FormatStr : PCHAR); CDecl; EXTERNAL CLib Name 'ConsolePrintf';
*)
// this gives internal compiler error 1234124 ??
//PROCEDURE ConsolePrintf (FormatStr : PCHAR; Param : array of const); CDecl; EXTERNAL CLib;

PROCEDURE _printf (FormatStr : PCHAR; Param : LONGINT); CDecl; EXTERNAL CLib;
PROCEDURE _printf (FormatStr : PCHAR); CDecl; EXTERNAL CLib;
// this gives internet compiler error 1234124 ??
// PROCEDURE _printf (FormatStr : PCHAR; Param : array of const); CDecl; EXTERNAL CLib;

// values for __action_code used with ExitThread()
CONST
  TSR_THREAD  = -1; //-Terminate only the current thread.
  EXIT_THREAD = 0;  //-Terminate the current thread; if the current thread is
                    // also the last thread, terminate the NLM.
  EXIT_NLM    = 1;  //-Terminate the entire NLM.

FUNCTION _GetStdIn  : POINTER; CDECL; EXTERNAL Clib NAME '__get_stdin';  // result: **FILE
FUNCTION _GetStdOut : POINTER; CDECL; EXTERNAL Clib NAME '__get_stdout';
FUNCTION _GetStdErr : POINTER; CDECL; EXTERNAL Clib NAME '__get_stderr';

// FileIO by Fileno
FUNCTION _open   (FileName : PCHAR; access, mode : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'open';
FUNCTION _close  (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'close';
FUNCTION _lseek  (FileNo,Pos,whence :LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'lseek';
FUNCTION _chsize (FileNo,Pos : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'chsize';
FUNCTION _tell   (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'tell';
FUNCTION _write  (FileNo : LONGINT; BufP : POINTER; Len : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'write';
FUNCTION _read   (FileNo : LONGINT; BufP : POINTER; Len : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'read';
FUNCTION _filelength (filedes : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'filelength';
FUNCTION _lock (filedes : LONGINT; Offset, Length : Cardinal) : LONGINT; CDECL; EXTERNAL Clib NAME 'lock';
FUNCTION _unlock (filedes : LONGINT; Offset, Length : Cardinal) : LONGINT; CDECL; EXTERNAL Clib NAME 'unlock';

TYPE
  NWModifyStructure =
    RECORD
       MModifyName            : PCHAR;
       MFileAttributes        : LONGINT;
       MFileAttributesMask    : LONGINT;
       MCreateDate            : WORD;
       MCreateTime            : WORD;
       MOwnerID               : LONGINT;
       MLastArchivedDate      : WORD;
       MLastArchivedTime      : WORD;
       MLastArchivedID        : LONGINT;
       MLastUpdatedDate       : WORD;
       MLastUpdatedTime       : WORD;
       MLastUpdatedID         : LONGINT;
       MLastAccessedDate      : WORD;
       MInheritanceGrantMask  : WORD;
       MInheritanceRevokeMask : WORD;
       MMaximumSpace          : LONGINT;
       MLastUpdatedInSeconds  : LONGINT
     END;

CONST MModifyNameBit                 = $0001;
      MFileAtrributesBit             = $0002;
      MCreateDateBit                 = $0004;
      MCreateTimeBit                 = $0008;
      MOwnerIDBit                    = $0010;
      MLastArchivedDateBit           = $0020;
      MLastArchivedTimeBit           = $0040;
      MLastArchivedIDBit             = $0080;
      MLastUpdatedDateBit            = $0100;
      MLastUpdatedTimeBit            = $0200;
      MLastUpdatedIDBit              = $0400;
      MLastAccessedDateBit           = $0800;
      MInheritanceRestrictionMaskBit = $1000;
      MMaximumSpaceBit               = $2000;
      MLastUpdatedInSecondsBit       = $4000;

// Directory
FUNCTION _chdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL CLib NAME 'chdir';
FUNCTION _getcwd (path : PCHAR; pathlen : LONGINT) : PCHAR; CDECL; EXTERNAL CLib NAME 'getcwd';
FUNCTION _mkdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL CLib NAME 'mkdir';
FUNCTION _rmdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL CLib NAME 'rmdir';
FUNCTION _ChangeDirectoryEntry (PathName : PCHAR; VAR ModyStruct : NWModifyStructure; ModifyBits, AllowWildcard : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'ChangeDirectoryEntry';

// get fileno from stream
FUNCTION _fileno (Handle : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'fileno';
FUNCTION _isatty (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'isatty';

(* values for 'o_flag' in open()... *)
CONST O_RDONLY     = $0000;   (* open for read only *)
      O_WRONLY     = $0001;   (* open for write only *)
      O_RDWR       = $0002;   (* open for read and write *)
      O_ACCMODE    = $0003;   (* AND with value to extract access flags *)
      O_APPEND     = $0010;   (* writes done at end of file *)
      O_CREAT      = $0020;   (* create new file *)
      O_TRUNC      = $0040;   (* truncate existing file *)
      O_EXCL       = $0080;   (* exclusive open *)
      O_TEXT       = $0100;   (* text file--unsupported *)
      O_BINARY     = $0200;   (* binary file *)
      O_NDELAY     = $0400;   (* nonblocking flag *)
      O_NOCTTY     = $0800;   (* currently unsupported *)
      O_NONBLOCK   = O_NDELAY;


// File Utils
FUNCTION _unlink (FileName : PCHAR) : LONGINT; CDECL; EXTERNAL CLib NAME 'unlink';
FUNCTION _rename (oldpath, newpath : PCHAR) : LONGINT; CDECL; EXTERNAL Clib NAME 'rename';

// Error
TYPE _PLONGINT = ^LONGINT;
FUNCTION __get_errno_ptr : _PLONGINT; CDECL; EXTERNAL Clib;

// Memory
FUNCTION _malloc (size : LONGINT) : POINTER; CDECL; EXTERNAL CLib NAME 'malloc';
FUNCTION _realloc (p : POINTER; size : LONGINT) : POINTER; CDECL; EXTERNAL CLib NAME 'realloc';
PROCEDURE _free (what : POINTER); CDECL; EXTERNAL CLib NAME 'free';
FUNCTION _stackavail : CARDINAL; CDECL; EXTERNAL CLib NAME 'stackavail';

// Debug
PROCEDURE _EnterDebugger; CDECL; EXTERNAL Clib NAME 'EnterDebugger';

// String
FUNCTION _strlen (P : PCHAR) : LONGINT; CDECL; EXTERNAL Clib NAME 'strlen';

// Time/Date
TYPE NWTM = RECORD
              tm_sec, tm_min, tm_hour,
              tm_mday, tm_mon, tm_year,
              tm_wday, tm_yday, tm_isdst : LONGINT;
            END;
     PNWTM = ^NWTM;
FUNCTION _localtime (VAR time : time_t) : PNWTM; CDECL; EXTERNAL Clib NAME 'localtime';
FUNCTION _time (tloc : POINTER) : LONGINT; CDECL; EXTERNAL Clib NAME 'time';
PROCEDURE _ConvertTimeToDOS (time : time_t; VAR DosDate, DosTime : WORD); CDECL; EXTERNAL Clib NAME '_ConvertTimeToDOS';
PROCEDURE _tzset; CDECL; EXTERNAL Clib NAME 'tzset';


//-----------------------------------------------------------------------

CONST NWDEFCONN_HANDLE = 0;

TYPE NWCONN_HANDLE = LONGINT;
     NWRCODE = LONGINT;
     NWDateAndTime = PACKED RECORD
                       Year,Month,Day,
                       Hour,Minute,Second,DayOfWeek : BYTE;
                     END;

PROCEDURE GetFileServerDateAndTime (VAR TimeBuf : NWDateAndTime); CDECL; EXTERNAL CLib NAME 'GetFileServerDateAndTime';
FUNCTION  SetFileServerDateAndTime(year:WORD; month:WORD; day:WORD; hour:WORD; minute:WORD;
               second:WORD):longint;cdecl; EXTERNAL CLib Name 'SetFileServerDateAndTime';

TYPE   FILE_SERV_INFO = record
            serverName              : array[0..47] of char;
            netwareVersion          : BYTE;
            netwareSubVersion       : BYTE;
            maxConnectionsSupported : WORD;
            connectionsInUse        : WORD;
            maxVolumesSupported     : WORD;
            revisionLevel           : BYTE;
            SFTLevel                : BYTE;
            TTSLevel                : BYTE;
            peakConnectionsUsed     : WORD;
            accountingVersion       : BYTE;
            VAPversion              : BYTE;
            queingVersion           : BYTE;
            printServerVersion      : BYTE;
            virtualConsoleVersion   : BYTE;
            securityRestrictionLevel: BYTE;
            internetBridgeSupport   : BYTE;
            reserved                : array[0..59] of BYTE;
            CLibMajorVersion        : BYTE;
            CLibMinorVersion        : BYTE;
            CLibRevision            : BYTE;
         end;
   pFILE_SERV_INFO = ^FILE_SERV_INFO;

FUNCTION GetServerInformation(returnSize:longint; serverInfo:pFILE_SERV_INFO):longint;cdecl; EXTERNAL CLib NAME 'GetServerInformation';

// Directory
TYPE NWDirEnt =
  PACKED RECORD
    d_attr                : LONGINT;
    d_time                : WORD;           {modification time}
    d_date                : WORD;           {modification date}
    d_size                : LONGINT;        {filesize}
    d_ino                 : LONGINT;        {serial number}
    d_dev                 : LONGINT;        {volume number}
    d_cdatetime           : time_t;         {creation date and time}
    d_adatetime           : time_t;         {last access - files only}
    d_bdatetime           : time_t;         {last archive date and time}
    d_uid                 : LONGINT;        {owner id (object id) }
    d_archivedID          : LONGINT;
    d_updatedID           : LONGINT;
    d_nameDOS             : ARRAY [0..12] OF CHAR;
    d_inheritedRightsMask : WORD;
    d_originatingNameSpace: BYTE;
    d_ddatetime           : time_t;         {deleted date time}
    d_deletedID           : LONGINT;
    {---- new fields starting in v4.11 ----}
    d_name                : ARRAY [0..255] OF CHAR;  { enty's namespace name }
  END;
  PNWDirEnt = ^NWDirEnt;

  FUNCTION _opendir (pathname : PCHAR) : PNWDirEnt; CDECL; EXTERNAL CLib NAME 'opendir_411';
  FUNCTION _closedir (dirH : PNWDirEnt) : LONGINT; CDECL; EXTERNAL CLib NAME 'closedir';
  FUNCTION _readdir  (dirH : PNWDirEnt) : PNWDirEnt; CDECL; EXTERNAL CLib NAME 'readdir';
  FUNCTION _SetReaddirAttribute (dirH : PNWDirEnt; Attribute : LONGINT) : LONGINT; EXTERNAL CLib NAME 'SetReaddirAttribute';

// Environment
  FUNCTION _getenv (name : PCHAR) : PCHAR; CDECL; EXTERNAL CLib NAME 'getenv';

// Volumes
  FUNCTION _GetVolumeName (volumeNumber : LONGINT; volumeName : PCHAR) : LONGINT; CDECL; EXTERNAL CLib NAME 'GetVolumeName';
  FUNCTION _GetVolumeNumber (volumeName : PCHAR; VAR volumeNumber : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'GetVolumeNumber';
  FUNCTION _GetVolumeInfoWithNumber (VolumeNumber : BYTE;
                                     VolumeName   : PCHAR;
                                 VAR TotalBlocks  : WORD;
                                 VAR SectorsPerBlock : WORD;
                                 VAR availableBlocks : WORD;
                                 VAR totalDirectorySlots : WORD;
                                 VAR availableDirSlots   : WORD;
                                 VAR volumeisRemovable   : WORD) : LONGINT; CDECL; EXTERNAL CLib NAME 'GetVolumeInfoWithNumber';
  FUNCTION _GetNumberOfVolumes : LONGINT; CDECL; EXTERNAL CLib NAME 'GetNumberOfVolumes';

// Screen/Keyboad
PROCEDURE _CopyToScreenMemory (Height, Width : WORD; Data : POINTER; x, y : WORD); CDECL; EXTERNAL Clib NAME 'CopyToScreenMemory';
PROCEDURE _CopyFromScreenMemory (Height, Width : WORD; Data : POINTER; x, y : WORD); CDECL; EXTERNAL CLib NAME 'CopyFromScreenMemory';
FUNCTION  _DisplayInputCursor : LONGINT; CDECL; EXTERNAL CLib NAME 'DisplayInputCursor';
FUNCTION  _HideInputCursor : LONGINT; CDECL; EXTERNAL CLib NAME 'HideInputCursor';
FUNCTION  _SetPositionOfInputCursor (row,col : WORD): LONGINT; CDECL; EXTERNAL Clib NAME 'SetPositionOfInputCursor';
PROCEDURE _GotoXY (col, row : WORD); CDECL; EXTERNAL Clib NAME 'gotoxy';
FUNCTION  _GetSizeOfScreen (VAR height,width : WORD): LONGINT; CDECL; EXTERNAL CLib NAME 'GetSizeOfScreen';
FUNCTION  _IsColorMonitor : LONGINT; CDECL; EXTERNAL CLib NAME 'IsColorMonitor';
PROCEDURE _RingTheBell; CDECL; EXTERNAL CLib NAME 'RingTheBell';
FUNCTION  _SetCursorShape (startline,endline : BYTE) : WORD; CDECL; EXTERNAL CLib NAME 'SetCursorShape';
FUNCTION  _GetCursorShape (VAR startline,endline : BYTE) : WORD; CDECL; EXTERNAL CLib NAME 'GetCursorShape';
FUNCTION  _wherex : WORD; CDECL; EXTERNAL CLib NAME 'wherex';
FUNCTION  _wherey : WORD; CDECL; EXTERNAL CLib NAME 'wherey';
PROCEDURE _clrscr; CDECL; EXTERNAL CLib NAME 'clrscr';
FUNCTION  _kbhit : LONGINT; CDECL; EXTERNAL Clib NAME 'kbhit';
FUNCTION  _getch : CHAR; CDECL; EXTERNAL CLib NAME 'getch';
PROCEDURE _delay (miliseconds : longint); CDECL; EXTERNAL Clib NAME 'delay';
FUNCTION  _SetCtrlCharCheckMode (Enabled : BOOLEAN) : BOOLEAN; CDECL; EXTERNAL CLib NAME 'SetCtrlCharCheckMode';
FUNCTION  _SetAutoScreenDestructionMode (Enabled : BOOLEAN) : BOOLEAN; CDECL; EXTERNAL CLib NAME 'SetAutoScreenDestructionMode';

// Misc
FUNCTION _memcpy (Dest, Src : POINTER; Len : LONGINT) : POINTER; CDECL; EXTERNAL Clib NAME 'memcpy';

FUNCTION _OpenLocalSemaphore (InitialValue : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'OpenLocalSemaphore';
FUNCTION _WaitOnLocalSemaphore (semaphoreHandle : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'WaitOnLocalSemaphore';
FUNCTION _SignalLocalSemaphore (semaphoreHandle : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'SignalLocalSemaphore';
FUNCTION _CloseLocalSemaphore (semaphoreHandle : LONGINT) : LONGINT; CDECL; EXTERNAL CLib NAME 'CloseLocalSemaphore';
FUNCTION _EnterCritSec : LONGINT; CDecl; EXTERNAL CLib NAME 'EnterCritSec';
FUNCTION _ExitCritSec : LONGINT; CDecl; EXTERNAL CLib NAME 'ExitCritSec';


FUNCTION _SetThreadGroupID (id : longint) : longint; CDecl; EXTERNAL CLib NAME 'SetThreadGroupID';
FUNCTION _GetThreadGroupID : longint; CDecl; EXTERNAL CLib NAME 'GetThreadGroupID';

CONST _SIGTERM = 6;

PROCEDURE _Signal (Sig : longint; SigFunc : pointer);  CDECL; EXTERNAL Clib NAME 'signal';

FUNCTION _SetCurrentNameSpace (newNameSpace : BYTE) : BYTE;  CDECL; EXTERNAL Clib NAME 'SetCurrentNameSpace';
FUNCTION _SetTargetNameSpace  (newNameSpace : BYTE) : BYTE;  CDECL; EXTERNAL Clib NAME 'SetTargetNameSpace';

CONST
  NW_NS_DOS     = 0;
  NW_NS_MAC     = 1;
  NW_NS_NFS     = 2;
  NW_NS_FTAM    = 3;
  NW_NS_LONG    = 4;

function _NWAddSearchPathAtEnd (searchPath : pchar; var number : longint) : longint; cdecl; external Clib name 'NWAddSearchPathAtEnd';
function _NWDeleteSearchPath (searchPathNumber : longint) : longint; cdecl; external Clib name 'NWDeleteSearchPath';
function _NWInsertSearchPath (searchPathNumber : longint; path : pchar) : longint; cdecl; external Clib name 'NWInsertSearchPath';
function _NWGetSearchPathElement (searchPathNumber : longint; var isDOSSearchPath : longint; searchPath : pchar) : longint; cdecl; external Clib name 'NWGetSearchPathElement';


// values for __mode used with spawnxx()
CONST
   P_WAIT                    = 0;
   P_NOWAIT                  = 1;
   P_OVERLAY                 = 2;
   P_NOWAITO                 = 4;
   P_SPAWN_IN_CURRENT_DOMAIN = 8;


//function spawnlp(mode:longint; path:Pchar; arg0:Pchar; args:array of const):longint;cdecl;external CLib name 'spawnlp';
function spawnlp(mode:longint; path:Pchar; arg0:Pchar):longint;cdecl;external Clib name 'spawnlp';
function spawnvp(mode:longint; path:Pchar; argv:PPchar):longint;cdecl;external Clib name 'spawnvp';



