{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team
    Copyright (c) 2001 Armin Diehl

    Interface to netware clib

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ 2000/08/27 armin: first version
  2001/03/08 armin: additional functions
  2001/04/14 armin: additional functions for crt-unit
}

CONST Clib       = 'clib.nlm';
      NlmLib     = 'nlmlib.nlm';
      Threads    = 'threads.nlm';
      CalNlm     = 'calnlm32.nlm';
      ClxNlm     = 'clxnlm32.nlm';
      NitNlm     = 'nit.nlm';
      ThreadsNlm = 'threads.nlm';

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

PROCEDURE PressAnyKeyToContinue; CDecl; EXTERNAL; // Clib;


PROCEDURE ExitThread (action_code, termination_code : LONGINT); CDecl; EXTERNAL CLib;
FUNCTION _BeginThread (func, stack : pointer; Stacksize : LONGINT; arg : pointer) : LONGINT; Cdecl; EXTERNAL Threads NAME 'BeginThread';
FUNCTION _GetThreadDataAreaPtr : POINTER; CDecl; EXTERNAL NlmLib NAME 'GetThreadDataAreaPtr';
PROCEDURE _SaveThreadDataAreaPtr (P : POINTER); CDecl; EXTERNAL NlmLib NAME 'SaveThreadDataAreaPtr';
PROCEDURE _exit (ExitCode : LONGINT); CDecl; EXTERNAL CLib;
PROCEDURE ConsolePrintf (FormatStr : PCHAR; Param : LONGINT); CDecl; EXTERNAL CLib;
PROCEDURE printf (FormatStr : PCHAR; Param : LONGINT); CDecl; EXTERNAL CLib;
PROCEDURE ConsolePrintf3 (FormatStr : PCHAR; P1,P2,P3 : LONGINT); CDecl; EXTERNAL CLib NAME 'ConsolePrintf';

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
FUNCTION _open   (FileName : PCHAR; access, mode : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'open';
FUNCTION _close  (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'close';
FUNCTION _lseek  (FileNo,Pos,whence :LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'lseek';
FUNCTION _chsize (FileNo,Pos : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'chsize';
FUNCTION _tell   (FileNo : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'tell';
FUNCTION _write  (FileNo : LONGINT; BufP : POINTER; Len : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'write';
FUNCTION _read   (FileNo : LONGINT; BufP : POINTER; Len : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'read';
FUNCTION _filelength (filedes : LONGINT) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'filelength';

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
FUNCTION _chdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'chdir';
FUNCTION _getcwd (path : PCHAR; pathlen : LONGINT) : PCHAR; CDECL; EXTERNAL NlmLib NAME 'getcwd';
FUNCTION _mkdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'mkdir';
FUNCTION _rmdir  (path : PCHAR) : LONGINT; CDECL; EXTERNAL NlmLib NAME 'rmdir';
FUNCTION _ChangeDirectoryEntry (PathName : PCHAR; VAR ModyStruct : NWModifyStructure; ModifyBits, AllowWildcard : LONGINT) : LONGINT; CDECL; EXTERNAL NitNlm NAME 'ChangeDirectoryEntry';

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
FUNCTION _realloc (p : POINTER; size : LONGINT) : POINTER; CDECL; EXTERNAL Threads NAME 'realloc';
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

// Screen/Keyboad
PROCEDURE _CopyToScreenMemory (Height, Width : WORD; Data : POINTER; x, y : WORD); CDECL; EXTERNAL ThreadsNlm NAME 'CopyToScreenMemory';
PROCEDURE _CopyFromScreenMemory (Height, Width : WORD; Data : POINTER; x, y : WORD); CDECL; EXTERNAL ThreadsNlm NAME 'CopyFromScreenMemory';
FUNCTION  _DisplayInputCursor : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'DisplayInputCursor';
FUNCTION  _HideInputCursor : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'HideInputCursor';
FUNCTION  _SetPositionOfInputCursor (row,col : WORD): LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'SetPositionOfInputCursor';
PROCEDURE _GotoXY (col, row : WORD); CDECL; EXTERNAL ThreadsNlm NAME 'gotoxy';
FUNCTION  _GetSizeOfScreen (VAR height,width : WORD): LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'GetSizeOfScreen';
FUNCTION  _IsColorMonitor : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'IsColorMonitor';
PROCEDURE _RingTheBell; CDECL; EXTERNAL ThreadsNlm NAME 'RingTheBell';
FUNCTION  _SetCursorShape (startline,endline : BYTE) : WORD; CDECL; EXTERNAL ThreadsNlm NAME 'SetCursorShape';
FUNCTION  _GetCursorShape (VAR startline,endline : BYTE) : WORD; CDECL; EXTERNAL ThreadsNlm NAME 'GetCursorShape';
FUNCTION  _wherex : WORD; CDECL; EXTERNAL ThreadsNlm NAME 'wherex';
FUNCTION  _wherey : WORD; CDECL; EXTERNAL ThreadsNlm NAME 'wherey';
PROCEDURE _clrscr; CDECL; EXTERNAL ThreadsNlm NAME 'clrscr';
FUNCTION  _kbhit : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'kbhit';
FUNCTION  _getch : CHAR; CDECL; EXTERNAL ThreadsNlm NAME 'getch';
PROCEDURE _delay (miliseconds : longint); CDECL; EXTERNAL ThreadsNlm NAME 'delay';
FUNCTION  _SetCtrlCharCheckMode (Enabled : BOOLEAN) : BOOLEAN; CDECL; EXTERNAL ThreadsNlm NAME 'SetCtrlCharCheckMode';
FUNCTION  _SetAutoScreenDestructionMode (Enabled : BOOLEAN) : BOOLEAN; CDECL; EXTERNAL ThreadsNlm NAME 'SetAutoScreenDestructionMode';

// Misc
FUNCTION _memcpy (Dest, Src : POINTER; Len : LONGINT) : POINTER; CDECL; EXTERNAL ThreadsNlm NAME 'memcpy';

FUNCTION _OpenLocalSemaphore (InitialValue : LONGINT) : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'OpenLocalSemaphore';
FUNCTION _WaitOnLocalSemaphore (semaphoreHandle : LONGINT) : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'WaitOnLocalSemaphore';
FUNCTION _SignalLocalSemaphore (semaphoreHandle : LONGINT) : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'SignalLocalSemaphore';
FUNCTION _CloseLocalSemaphore (semaphoreHandle : LONGINT) : LONGINT; CDECL; EXTERNAL ThreadsNlm NAME 'CloseLocalSemaphore';
FUNCTION _EnterCritSec : LONGINT; CDecl; EXTERNAL ThreadsNlm NAME 'EnterCritSec';
FUNCTION _ExitCritSec : LONGINT; CDecl; EXTERNAL ThreadsNlm NAME 'ExitCritSec';



{
  $Log$
  Revision 1.4  2002-03-08 19:06:47  armin
  	* Additional routines needed for MT

  Revision 1.3  2001/04/16 18:39:50  florian
    * updates from Armin commited

  Revision 1.2  2001/04/11 14:17:00  florian
    * added logs, fixed email address of Armin, it is
      diehl@nordrhein.de

}
