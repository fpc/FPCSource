{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Interface to netware clib

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ 2000/08/27 armin: first version
  2001/03/08 armin: additional functions
}

CONST Clib   = 'clib.nlm';
      NlmLib = 'nlmlib.nlm';
      Threads= 'threads.nlm';
      CalNlm = 'calnlm32.nlm';
      ClxNlm = 'clxnlm32.nlm';
      NitNlm = 'nit.nlm';

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

FUNCTION  _stat  (path : PCHAR; VAR buf : NWStatBufT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'stat_411';
FUNCTION  _fstat (Fileno : LONGINT; VAR buf : NWStatBufT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'fstat_411';

PROCEDURE NWFree   (P : POINTER); CDECL; EXTERNAL Clib NAME 'free';

PROCEDURE PressAnyKeyToContinue; CDecl; EXTERNAL 'CLib.NLM';
PROCEDURE ExitThread (action_code, termination_code : LONGINT); CDecl; EXTERNAL 'CLib.NLM';
PROCEDURE _exit (ExitCode : LONGINT); CDecl; EXTERNAL 'CLib.NLM';
PROCEDURE ConsolePrintf (FormatStr : PCHAR; Param : LONGINT); CDecl; EXTERNAL ('CLib.NLM');
PROCEDURE printf (FormatStr : PCHAR; Param : LONGINT); CDecl; EXTERNAL ('CLib.NLM');
//PROCEDURE printf (FormatStr : PCHAR; Param : PCHAR); CDecl; EXTERNAL ('CLib.NLM');
PROCEDURE ConsolePrintf3 (FormatStr : PCHAR; P1,P2,P3 : LONGINT); CDecl; EXTERNAL ('CLib.NLM') NAME 'ConsolePrintf';
//FUNCTION  strlen(lpString: PChar): LONGINT; CDECL; EXTERNAL Clib;

// values for __action_code used with ExitThread()
CONST
  TSR_THREAD  = -1;
  EXIT_THREAD = 0;
  EXIT_NLM    = 1;

FUNCTION _GetStdIn  : POINTER; CDECL; EXTERNAL Clib NAME '__get_stdin';  // result: **FILE
FUNCTION _GetStdOut : POINTER; CDECL; EXTERNAL Clib NAME '__get_stdout';
FUNCTION _GetStdErr : POINTER; CDECL; EXTERNAL Clib NAME '__get_stderr';

// Stream FileIO
//FUNCTION _fopen (filename, mode : PCHAR) : LONGINT; CDECL; EXTERNAL Clib NAME 'fopen';
//FUNCTION _fclose (hFile : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'fclose';
//FUNCTION _fwrite (Buffer : POINTER; S1,S2,hFile : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'fwrite';
//FUNCTION _fread  (Buffer : POINTER; S1,S2,hFile : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'fread';
//FUNCTION _fseek  (hFile, Offset, Where : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'fseek';
//FUNCTION _ftell  (hFile : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'ftell';


// FileIO by Fileno
FUNCTION _open   (FileName : PCHAR; access, mode : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'open';
FUNCTION _close  (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'close';
FUNCTION _lseek  (FileNo,Pos,whence :LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'lseek';
FUNCTION _chsize (FileNo,Pos : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'chsize';
FUNCTION _tell   (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'tell';
FUNCTION _write  (FileNo : LONGINT; BufP : POINTER; Len : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'write';
FUNCTION _read   (FileNo : LONGINT; BufP : POINTER; Len : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'read';
FUNCTION _filelength (filedes : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'filelength';

// Directory
FUNCTION _chdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'chdir';
FUNCTION _getcwd (path : PCHAR; pathlen : LONGINT) : PCHAR; CDECL; EXTERNAL NlmLib NAME 'getcwd';
FUNCTION _mkdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'mkdir';
FUNCTION _rmdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'rmdir';

// get fileno from stream
FUNCTION _fileno (Handle : LONGINT) : LONGINT; CDECL; EXTERNAL Clib NAME 'fileno';
FUNCTION _isatty (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'isatty';

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
FUNCTION _unlink (FileName : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'unlink';
FUNCTION _rename (oldpath, newpath : PCHAR) : LONGINT; CDECL; EXTERNAL Clib NAME 'rename';

// Error
TYPE _PLONGINT = ^LONGINT;
FUNCTION __get_errno_ptr : _PLONGINT; CDECL; EXTERNAL Clib;

// Memory
FUNCTION _malloc (size : LONGINT) : POINTER; CDECL; EXTERNAL Threads NAME 'malloc';
PROCEDURE _free (what : POINTER); CDECL; EXTERNAL Threads NAME 'free';
FUNCTION _stackavail : LONGINT; CDECL; EXTERNAL Threads NAME 'stackavail';

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

PROCEDURE GetFileServerDateAndTime (VAR TimeBuf : NWDateAndTime); CDECL; EXTERNAL NitNlm NAME 'GetFileServerDateAndTime';
FUNCTION  SetFileServerDateAndTime(year:WORD; month:WORD; day:WORD; hour:WORD; minute:WORD;
               second:WORD):longint;cdecl; EXTERNAL NitNlm Name 'SetFileServerDateAndTime';

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

FUNCTION GetServerInformation(returnSize:longint; serverInfo:pFILE_SERV_INFO):longint;cdecl; EXTERNAL NitNlm NAME 'GetServerInformation';

// Directory
TYPE NWDirEnt =
  PACKED RECORD
    d_attr      : LONGINT;
    d_time      : WORD;
    d_date      : WORD;
    d_size      : LONGINT;
    d_ino       : LONGINT;
    d_dev       : LONGINT;
    d_cdatetime : LONGINT;
    d_adatetime : LONGINT;
    d_bdatetime : LONGINT;
    d_uid       : LONGINT;
    d_archivedID: LONGINT;
    d_updatedID : LONGINT;
    d_nameDOS   : ARRAY [0..12] OF CHAR;
    d_inheritedRightsMask : WORD;
    d_originatingNameSpace: BYTE;
    d_ddatetime           : LONGINT;
    d_deletedID           : LONGINT;
    {---- new fields starting in v4.11 ----}
    d_name                : ARRAY [0..255] OF CHAR;  { enty's namespace name }
  END;
  PNWDirEnt = ^NWDirEnt;

  FUNCTION _opendir (pathname : PCHAR) : PNWDirEnt; CDECL; EXTERNAL NlmLib NAME 'opendir_411';
  FUNCTION _closedir (dirH : PNWDirEnt) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'closedir';
  FUNCTION _readdir  (dirH : PNWDirEnt) : PNWDirEnt; CDECL; EXTERNAL NlmLib NAME 'readdir';
  FUNCTION _SetReaddirAttribute (dirH : PNWDirEnt; Attribute : LONGINT) : LONGINT; EXTERNAL NlmLib NAME 'SetReaddirAttribute';

// Environment
  FUNCTION _getenv (name : PCHAR) : PCHAR; CDECL; EXTERNAL NlmLib NAME 'getenv';

// Volumes
  FUNCTION _GetVolumeName (volumeNumber : LONGINT; volumeName : PCHAR) : LONGINT; CDECL; EXTERNAL NitNlm NAME 'GetVolumeName';
  FUNCTION _GetVolumeNumber (volumeName : PCHAR; VAR volumeNumber : LONGINT) : LONGINT; CDECL; EXTERNAL NitNlm NAME 'GetVolumeNumber';
  FUNCTION _GetVolumeInfoWithNumber (VolumeNumber : BYTE;
                                     VolumeName   : PCHAR;
                                 VAR TotalBlocks  : WORD;
                                 VAR SectorsPerBlock : WORD;
                                 VAR availableBlocks : WORD;
                                 VAR totalDirectorySlots : WORD;
                                 VAR availableDirSlots   : WORD;
                                 VAR volumeisRemovable   : WORD) : LONGINT; CDECL; EXTERNAL NitNlm NAME 'GetVolumeInfoWithNumber';
  FUNCTION _GetNumberOfVolumes : LONGINT; CDECL; EXTERNAL NitNlm NAME 'GetNumberOfVolumes';
