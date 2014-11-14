{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by the Free Pascal development team.

    Additional OS/2 API functions implemented in DOSCALL1.DLL:
    - File handling (64-bit functions available in WSeB/MCP/eCS and
      protected access to file handles as available in OS/2 2.1+)
    - Certain SMP related functions for querying and setting status
      of processors and thread and system affinity (available
      in SMP-ready versions of OS/2 kernels)
    - Support for working with extended LIBPATH (available in
      OS/2 Warp 4.0 and higher).
    Availability of individual functions is checked dynamically during
    initialization and fake (simulated) functions are used if running
    under an OS/2 version not providing the respective functionality.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit DosCall2;

{***************************************************************************}
interface
{***************************************************************************}

uses
  DosCalls, Strings;

const
(* Status in DosGet/SetProcessorStatus *)
  PROC_OFFLINE = 0; (* Processor is offline *)
  PROC_ONLINE = 1;  (* Processor is online *)
(* Scope in DosQueryThreadAffinity *)
  AFNTY_THREAD = 0; (* Return the current threads processor affinity mask. *)
  AFNTY_SYSTEM = 1; (* Return the system's current capable processor affinity
                       mask. *)
(* Flags in DosQuery/SetExtLibPath *)
  BEGIN_LIBPATH = 1; (* The new path is searched before the LIBPATH. *)
  END_LIBPATH = 2;   (* The new path is searched after the LIBPATH. *)

(* Constants for DosSuppressPopups *)
  SPU_DisableSuppression = 0;
  SPU_EnableSuppression = 1;

(* Constants for DosDumpProcess *)
  DDP_DisableProcDump = 0;
  DDP_EnableProcDump = 1;
  DDP_PerformProcDump = 2;

(* Constants for DosPerfSysCall *)
  Cmd_KI_Enable = $60;
  Cmd_KI_RdCnt = $63;
  Cmd_SoftTrace_Log = $14;

(* Constants for DosQueryABIOSSupport *)
  HW_Cfg_MCA = 1;
  HW_Cfg_EISA = 2;
  HW_Cfg_ABIOS_Supported = 4;
  HW_Cfg_ABIOS_Present = 8;
  HW_Cfg_PCI = 16;
  HW_Cfg_OEM_ABIOS = 32;
  HW_Cfg_IBM_ABIOS = 0;
  HW_Cfg_Pentium_CPU = 64;


type
  TFileLockL = record
   case boolean of
    false:
     (Offset: int64;  (* Offset to beginning of the lock (or unlock) range.  *)
       Range: int64); (* Length of the lock (or unlock) range in bytes.      *)
                      (* Length of 0 => locking (or unlocking) not required. *)
    true:
     (lOffset: int64;
      lRange: int64);
  end;
  PFileLockL = ^TFileLockL;

  TMPAffinity = record
   Mask: array [0..1] of cardinal;
  end;
  PMPAffinity = ^TMPAffinity;

  TCPUUtil = record
   TotalLow,
   TotalHigh,
   IdleLow,
   IdleHigh,
   BusyLow,
   BusyHigh,
   IntrLow,
   IntrHigh: cardinal;
  end;
  PCPUUtil = ^TCPUUtil;


function DosOpenL (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                                 EA: pointer): cardinal; cdecl;

function DosSetFilePtrL (Handle: THandle; Pos: int64; Method: cardinal;
                                        var PosActual: int64): cardinal; cdecl;

function DosSetFileSizeL (Handle: THandle; Size: int64): cardinal; cdecl;

function DosProtectOpen (FileName: PChar; var Handle: longint;
                         var Action: longint; InitSize, Attrib,
                         OpenFlags, OpenMode: longint; ea: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectOpen (const FileName: string; var Handle: longint;
                         var Action: longint; InitSize, Attrib,
                         OpenFlags, OpenMode: longint; ea: PEAOp2;
                                     var FileHandleLockID: cardinal): cardinal;

function DosProtectOpen (const FileName: string; var Handle: THandle;
                         var Action: cardinal; InitSize, Attrib,
                         OpenFlags, OpenMode: cardinal; ea: PEAOp2;
                                     var FileHandleLockID: cardinal): cardinal;

function DosProtectRead (Handle: longint; var Buffer; Count: longint;
           var ActCount: longint; FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectWrite (Handle: longint; const Buffer; Count: longint;
                          var ActCount: longint;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFilePtr (Handle: longint; Pos, Method: longint;
                               var PosActual: longint;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFilePtr (Handle: THandle; Pos: longint;
                                         FileHandleLockID: cardinal): cardinal;

function DosProtectGetFilePtr (Handle: longint;
                 var PosActual: longint; FileHandleLockID: cardinal): cardinal;

function DosProtectGetFilePtr (Handle: THandle;
                var PosActual: cardinal; FileHandleLockID: cardinal): cardinal;

function DosProtectEnumAttribute (Handle: THandle; Entry: cardinal; var Buf;
                                  BufSize: cardinal; var Count: cardinal;
                                  InfoLevel: cardinal;
                                         FileHandleLockID: cardinal): cardinal;

function DosProtectEnumAttribute (const FileName: string; Entry: cardinal;
                                  var Buf; BufSize: cardinal;
                                  var Count: cardinal; InfoLevel: cardinal;
                                         FileHandleLockID: cardinal): cardinal;

function DosProtectOpen (FileName: PChar; var Handle: THandle;
                              var Action: cardinal; InitSize, Attrib,
                              OpenFlags, OpenMode: cardinal; ea: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectClose (Handle: THandle;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectRead (Handle: THandle; var Buffer; Count: cardinal;
          var ActCount: cardinal; FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectWrite (Handle: THandle; const Buffer; Count: cardinal;
                          var ActCount: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFilePtr (Handle: THandle; Pos: longint;
                               Method: cardinal; var PosActual: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFileSize (Handle: THandle; Size: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectQueryFHState (Handle: THandle; var FileMode: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFHState (Handle: THandle; FileMode: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectQueryFileInfo (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFileInfo (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectEnumAttribute (RefType: cardinal; AFile: pointer;
                                  Entry: cardinal; var Buf; BufSize: cardinal;
                                  var Count: cardinal; InfoLevel: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DosProtectSetFileLocks (Handle: THandle;
                                      var Unlock, Lock: TFileLock;
                                      Timeout, Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;


(*
 DosCancelLockRequestL cancels an outstanding DosSetFileLocksL request.
 If two threads in a process are waiting on a lock file range, and another
 thread issues DosCancelLockRequestL for that lock file range, then both
 waiting threads are released.
 Not all file-system drivers (FSDs) can cancel an outstanding lock request.
 Local Area Network (LAN) servers cannot cancel an outstanding lock request
 if they use a version of the operating system prior to OS/2 Version 2.00.

Possible results:
     0 No_Error
     6 Error_Invalid_Handle
    87 Error_Invalid_Parameter
   173 Error_Cancel_Violation

 Handle = File handle used in the DosSetFileLocksL function
          that is to be cancelled.
 Lock   = Specification of the lock request to be cancelled.
*)
function DosCancelLockRequestL (Handle: THandle;
                                        var Lock: TFileLockL): cardinal; cdecl;

(*
DosProtectSetFileLocksL locks and unlocks a range of an open file. 

Parameters:
 Handle = file handle
 Unlock = record containing the offset and length of a range to be unlocked
 Lock = record containing the offset and length of a range to be locked 
 Timeout = the maximum time that the process is to wait for the requested locks
           (in milliseconds)
 Flags = bit mask specifying action to be taken.
     Bits 31..2 are reserved.
     Bit 1 (Atomic) means request for atomic locking - if the bit is set
        and the lock range is equal to the unlock range, an atomic lock occurs.
        If this bit is set to 1 and the lock range is not equal to the unlock
        range, an error is returned. If this bit is set to 0, then the lock
        may or may not occur atomically with the unlock.
     Bit 0 (Share) defines the type of access that other processes may have
        to the file range that is being locked. If this bit is set to 0
        (default), other processes have no access to the locked file range.
        The current process has exclusive access to the locked file range,
        which must not overlap any other locked file range. If this bit is set
        to 1, the current process and other processes have shared read only
        access to the locked file range. A file range with shared access may
        overlap any other file range with shared access, but must not overlap
        any other file range with exclusive access.

 FileHandleLockID = filehandle lockid returned by a previous DosProtectOpenL.

Possible return codes:
0 NO_ERROR 
6 ERROR_INVALID_HANDLE 
33 ERROR_LOCK_VIOLATION 
36 ERROR_SHARING_BUFFER_EXCEEDED 
87 ERROR_INVALID_PARAMETER 
95 ERROR_INTERRUPT 
174 ERROR_ATOMIC_LOCK_NOT_SUPPORTED 
175 ERROR_READ_LOCKS_NOT_SUPPORTED 

Remarks:
DosProtectSetFileLocksL allows a process to lock and unlock a range in a file.
The time during which a file range is locked should be short.

If the lock and unlock ranges are both zero, ERROR_LOCK_VIOLATION is returned
to the caller.

If you only want to lock a file range, set the unlock file offset and the
unlock range length to zero.

If you only want to unlock a file range, set the lock file offset and the lock
range length to zero.

When the Atomic bit of flags is set to 0, and DosProtectSetFileLocksL specifies
a lock operation and an unlock operation, the unlock operation occurs first,
and then the lock operation is performed. If an error occurs during the unlock
operation, an error code is returned and the lock operation is not performed.
If an error occurs during the lock operation, an error code is returned and the
unlock remains in effect if it was successful.

The lock operation is atomic when all of these conditions are met:
- The Atomic bit is set to 1 in flags
- The unlock range is the same as the lock range
- The process has shared access to the file range, and has requested exclusive
  access to it; or the process has exclusive access to the file range, and has
  requested shared access to it.

Some file system drivers (FSDs) may not support atomic lock operations.
Versions of the operating system prior to OS/2 Version 2.00 do not support
atomic lock operations. If the application receives the error code
ERROR_ATOMIC_LOCK_NOT_SUPPORTED, the application should unlock the file range
and then lock it using a non-atomic operation (with the atomic bit set to 0
in Flags). The application should also refresh its internal buffers before
making any changes to the file.

If you issue DosProtectClose to close a file with locks still in effect,
the locks are released in no defined sequence.

If you end a process with a file open, and you have locks in effect in that
file, the file is closed and the locks are released in no defined sequence.

The locked range can be anywhere in the logical file. Locking beyond the end
of the file is not an error. A file range to be locked exclusively must first
be cleared of any locked file sub-ranges or overlapping locked file ranges.

If you repeat DosProtectSetFileLocksL for the same file handle and file range,
then you duplicate access to the file range. Access to locked file ranges
is not duplicated across DosExecPgm. The proper method of using locks is
to attempt to lock the file range, and to examine the return value.

The following table shows the level of access granted when the accessed file
range is locked with an exclusive lock or a shared lock.  Owner  refers to
a process that owns the lock.  Non-owner  refers to a process that does not own
the lock.

 Action       Exclusive Lock             Shared Lock
===================================================================
 Owner read   Success                    Success
-------------------------------------------------------------------
 Non-owner    Wait for unlock. Return    Success
 read         error code after time-out.
-------------------------------------------------------------------
 Owner write  Success                    Wait for unlock. Return
                                         error code after time-out.
-------------------------------------------------------------------
 Non-owner    Wait for unlock. Return    Wait for unlock. Return
 write        error code after time-out. error code after time-out.
-------------------------------------------------------------------


If only locking is specified, DosProtectSetFileLocksL locks the specified file
range using Lock. If the lock operation cannot be accomplished, an error is
returned, and the file range is not locked.

After the lock request is processed, a file range can be unlocked using the
Unlock parameter of another DosProtectSetFileLocksL request. If unlocking
cannot be accomplished, an error is returned.

Instead of denying read/write access to an entire file by specifying access
and sharing modes with DosProtectOpenL requests, a process attempts to lock
only the range needed for read/write access and examines the error code
returned.

Once a specified file range is locked exclusively, read and write access by
another process is denied until the file range is unlocked. If both unlocking
and locking are specified by DosProtectSetFileLocksL, the unlocking operation
is performed first, then locking is done.
*)
function DosProtectSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
                   var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

(*
DosSetFileLocksL locks and unlocks a range of an open file. 

Parameters:
 Handle = file handle
 Unlock = record containing the offset and length of a range to be unlocked
 Lock = record containing the offset and length of a range to be locked
 Timeout = the maximum time that the process is to wait for the requested locks
           (in milliseconds)
 Flags = bit mask specifying action to be taken.
     Bits 31..2 are reserved.
     Bit 1 (Atomic) means request for atomic locking - if the bit is set
        and the lock range is equal to the unlock range, an atomic lock occurs.
        If this bit is set to 1 and the lock range is not equal to the unlock
        range, an error is returned. If this bit is set to 0, then the lock
        may or may not occur atomically with the unlock.
     Bit 0 (Share) defines the type of access that other processes may have
        to the file range that is being locked. If this bit is set to 0
        (default), other processes have no access to the locked file range.
        The current process has exclusive access to the locked file range,
        which must not overlap any other locked file range. If this bit is set
        to 1, the current process and other processes have shared read only
        access to the locked file range. A file range with shared access may
        overlap any other file range with shared access, but must not overlap
        any other file range with exclusive access.

Possible return codes:
  0 NO_ERROR 
  1 ERROR_INVALID_FUNCTION 
  6 ERROR_INVALID_HANDLE 
 33 ERROR_LOCK_VIOLATION 
 36 ERROR_SHARING_BUFFER_EXCEEDED 
 87 ERROR_INVALID_PARAMETER 
 95 ERROR_INTERRUPT 
174 ERROR_ATOMIC_LOCK_NOT_SUPPORTED 
175 ERROR_READ_LOCKS_NOT_SUPPORTED 

Remarks:
DosSetFileLocksL allows a process to lock and unlock a range in a file. The
time during which a file range is locked should be short.

If the lock and unlock ranges are both zero, ERROR_LOCK_VIOLATION is returned
to the caller.

If you only want to lock a file range, set the unlock file offset and the
unlock range length to zero.

If you only want to unlock a file range, set the lock file offset and the lock
range length to zero.

When the Atomic bit of flags is set to 0, and DosSetFileLocksL specifies a lock
operation and an unlock operation, the unlock operation occurs first, and then
the lock operation is performed. If an error occurs during the unlock
operation, an error code is returned and the lock operation is not performed.
If an error occurs during the lock operation, an error code is returned and the
unlock remains in effect if it was successful.

The lock operation is atomic when all of these conditions are met:
- The Atomic bit is set to 1 in flags
- The unlock range is the same as the lock range
- The process has shared access to the file range, and has requested exclusive
  access to it; or the process has exclusive access to the file range, and has
  requested shared access to it.

Some file system drivers (FSDs) may not support atomic lock operations.
Versions of the operating system prior to OS/2 Version 2.00 do not support
atomic lock operations. If the application receives the error code
ERROR_ATOMIC_LOCK_NOT_SUPPORTED, the application should unlock the file range
and then lock it using a non-atomic operation (with the atomic bit set to 0 in
flags). The application should also refresh its internal buffers before making
any changes to the file.

If you issue DosClose to close a file with locks still in effect, the locks are
released in no defined sequence.

If you end a process with a file open, and you have locks in effect in that
file, the file is closed and the locks are released in no defined sequence.

The locked range can be anywhere in the logical file. Locking beyond the end of
the file is not an error. A file range to be locked exclusively must first be
cleared of any locked file subranges or overlapping locked file ranges.

If you repeat DosSetFileLocksL for the same file handle and file range, then
you duplicate access to the file range. Access to locked file ranges is not
duplicated across DosExecPgm. The proper method of using locks is to attempt to
lock the file range, and to examine the return value.

The following table shows the level of access granted when the accessed file
range is locked with an exclusive lock or a shared lock.  Owner  refers to
a process that owns the lock.  Non-owner  refers to a process that does not own
the lock.

 Action       Exclusive Lock             Shared Lock
===================================================================
 Owner read   Success                    Success
-------------------------------------------------------------------
 Non-owner    Wait for unlock. Return    Success
 read         error code after time-out.
-------------------------------------------------------------------
 Owner write  Success                    Wait for unlock. Return
                                         error code after time-out.
-------------------------------------------------------------------
 Non-owner    Wait for unlock. Return    Wait for unlock. Return
 write        error code after time-out. error code after time-out.
-------------------------------------------------------------------

If only locking is specified, DosSetFileLocksL locks the specified file range
using Lock. If the lock operation cannot be accomplished, an error is
returned, and the file range is not locked.

After the lock request is processed, a file range can be unlocked using the
Unlock parameter of another DosSetFileLocksL request. If unlocking cannot be
accomplished, an error is returned.

Instead of denying read/write access to an entire file by specifying access and
sharing modes with DosOpenL requests, a process attempts to lock only the range
needed for read/write access and examines the error code returned.

Once a specified file range is locked exclusively, read and write access by
another process is denied until the file range is unlocked. If both unlocking
and locking are specified by DosSetFileLocksL, the unlocking operation is
performed first, then locking is done.
*)
function DosSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
    var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal): cardinal; cdecl;

(*
DosProtectOpenL opens a new file, an existing file, or a replacement for an
existing file and returns a protected file handle. An open file can have
extended attributes.

Parameters:
 FileName = ASCIIZ path name of the file or device to be opened.
 Handle = handle for the file is returned here.
 Action = value that specifies the action taken by DosProtectOpenL is returned
          here; if DosProtectOpenL fails, this value has no meaning, otherwise,
          it is one of the following values:
   1 FILE_EXISTED - file already existed.
   2 FILE_CREATED - file was created.
   3 FILE_TRUNCATED - file existed and was changed to a given size (file was
                      replaced).
 InitSize = new logical size of the file (end of data, EOD), in bytes; this
            parameter is significant only when creating a new file or replacing
            an existing one. Otherwise, it is ignored. It is an error to create
            or replace a file with a nonzero length if the OpenMode
            Access-Mode flag is set to read-only.
 Attrib = file attributes; this parameter contains the following bit fields:
      Bits Description 

     31..6 - reserved, must be 0.
         5 FILE_ARCHIVED (0x00000020) - file has been archived.
         4 FILE_DIRECTORY (0x00000010) - file is a subdirectory.
         3 - reserved, must be 0.
         2 FILE_SYSTEM (0x00000004) - file is a system file.
         1 FILE_HIDDEN (0x00000002) - file is hidden and does not appear in
                                      a directory listing.
         0 FILE_READONLY (0x00000001) - file can be read from, but not written
                                        to.
         0 FILE_NORMAL (0x00000000) - file can be read from or written to.

File attributes apply only if the file is created. These bits may be set
individually or in combination. For example, an attribute value of 0x00000021
(bits 5 and 0 set to 1) indicates a read-only file that has been archived.

 OpenFlags = the action to be taken depending on whether the file exists or
             does not exist. This parameter contains the following bit fields:
     Bits Description 
    31..8 - reserved, must be 0.
     7..4 - the following flags apply if the file does not exist:
        0000 OPEN_ACTION_FAIL_IF_NEW 
             Open an existing file; fail if the file does not exist. 
        0001 OPEN_ACTION_CREATE_IF_NEW 
             Create the file if the file does not exist. 
     3..0 The following flags apply if the file does not exist:
        0000 OPEN_ACTION_FAIL_IF_EXISTS 
             Open the file; fail if the file already exists. 
        0001 OPEN_ACTION_OPEN_IF_EXISTS 
             Open the file if it already exists. 
        0010 OPEN_ACTION_REPLACE_IF_EXISTS 
             Replace the file if it already exists. 

 OpenMode = the mode of the open function. This parameter contains the
            following bit fields:
      Bits Description 
        31 - reserved, must be zero. 
        30 OPEN_FLAGS_PROTECTED_HANDLE (0x40000000) - protected file handle flag.
             0 - unprotected Handle
             1 - protected Handle
  Protected handle requires the FileHandleLockID to be specified on subsequent
  DosProtectxxxx calls.

  Unprotected handle requires the FileHandleLockID value to be specified as
  zero on subsequent DosProtectxxxx calls. An unprotected handle may be used
  with the unprotected calls such as DosRead and DosWrite.

        29 OPEN_SHARE_DENYLEGACY (0x10000000)
           Deny read/write access by the DosOpen command:
             0 - allow read/write access by the DosOpen command.
             1 - deny read/write access by the DosOpen command.
  A file opened by DosOpenL will not be allowed to grow larger than 2GB while
  that same file is open via a legacy DosOpen call. Setting this bit to 1 will
  prevent access by the obsolete DosOpen API and ensure that no error will
  occur when growing the file.
    28..16 - reserved, must be zero.
        15 OPEN_FLAGS_DASD (0x00008000)
           Direct Open flag:
             0 - FileName represents a file to be opened normally. 
             1 - FileName is drive (such as C or A), and represents a mounted
                 disk or diskette volume to be opened for direct access.
        14 OPEN_FLAGS_WRITE_THROUGH (0x00004000) 
           Write-Through flag:
             0 - writes to the file may go through the file-system driver's
                 cache; the file-system driver writes the sectors when the
                 cache is full or the file is closed.
             1 - writes to the file may go through the file-system driver's
                 cache, but the sectors are written (the actual file I/O
                 operation is completed) before a synchronous write call
                 returns. This state of the file defines it as a synchronous
                 file. For synchronous files, this bit must be set, because the
                 data must be written to the medium for synchronous write
                 operations.
  This bit flag is not inherited by child processes. 
       13 OPEN_FLAGS_FAIL_ON_ERROR (0x00002000) 
          Fail-Errors flag. Media I/O errors are handled as follows:
            0 - reported through the system critical-error handler.
            1 - reported directly to the caller by way of a return code.
  Media I/O errors generated through Category 08h Logical Disk Control IOCtl
  Commands always get reported directly to the caller by way of return code.
  The Fail-Errors function applies only to non-IOCtl handle-based file I/O
  calls.

  This flag bit is not inherited by child processes. 

       12 OPEN_FLAGS_NO_CACHE (0x00001000) 
          No-Cache/Cache flag:
            0 - the file-system driver should place data from I/O operations
                into its cache.
            1 - I/O operations to the file need not be done through the
                file-system driver's cache.
  The setting of this bit determines whether file-system drivers should place
  data into the cache. Like the write-through bit, this is a per-handle bit,
  and is not inherited by child processes. 
       11 - reserved; must be 0.
    10..8 - the locality of reference flags contain information about how the
            application is to get access to the file. The values are as
            follows:
          000 OPEN_FLAGS_NO_LOCALITY (0x00000000)
              No locality known.
          001 OPEN_FLAGS_SEQUENTIAL (0x00000100)
              Mainly sequential access.
          010 OPEN_FLAGS_RANDOM (0x00000200)
              Mainly random access.
          011 OPEN_FLAGS_RANDOMSEQUENTIAL (0x00000300)
              Random with some locality.
        7 OPEN_FLAGS_NOINHERIT (0x00000080)
          Inheritance flag:
            0 - file handle is inherited by a process created from a call to
                DosExecPgm.
            1 - file handle is private to the current process.
  This bit is not inherited by child processes. 
    6..4 Sharing Mode flags; this field defines any restrictions to file
         access placed by the caller on other processes. The values are as
         follows:
         001 OPEN_SHARE_DENYREADWRITE (0x00000010)
             Deny read write access.
         010 OPEN_SHARE_DENYWRITE (0x00000020)
             Deny write access.
         011 OPEN_SHARE_DENYREAD (0x00000030)
             Deny read access.
         100 OPEN_SHARE_DENYNONE (0x00000040)
             Deny neither read nor write access (deny none).
         Any other value is invalid. 
       3 Reserved; must be 0. 
    2..0 Access-Mode flags. This field defines the file access required by the
         caller. The values are as follows:
         000 OPEN_ACCESS_READONLY (0x00000000)
         Read-only access
         001 OPEN_ACCESS_WRITEONLY (0x00000001)
         Write-only access
         010 OPEN_ACCESS_READWRITE (0x00000002)
         Read/write access.
         Any other value is invalid, as are any other combinations.   

File sharing requires the cooperation of sharing processes. This cooperation is
communicated through sharing and access modes. Any sharing restrictions placed
on a file opened by a process are removed when the process closes the file with
a DosClose request.

Sharing Mode:
Specifies the type of file access that other processes may have. For example,
if other processes can continue to read the file while your process is
operating on it, specify Deny Write. The sharing mode prevents other processes
from writing to the file but still allows them to read it.

Access Mode:
Specifies the type of file access (access mode) needed by your process. For
example, if your process requires read/write access, and another process has
already opened the file with a sharing mode of Deny None, your DosProtectOpenL
request succeeds. However, if the file is open with a sharing mode of Deny
Write, the process is denied access.

If the file is inherited by a child process, all sharing and access
restrictions also are inherited.

If an open file handle is duplicated by a call to DosDupHandle, all sharing and
access restrictions also are duplicated.

 EA = pointer to an extended attribute buffer. The address of the
      extended-attribute buffer, which contains an EAOP2 structure. The
      fpFEA2List field in the EAOP2 structure points to a data area where the
      relevant FEA2 list is to be found. The fpGEA2List and oError fields are
      ignored.
Output fpGEA2List and fpFEA2List are unchanged. The area that fpFEA2List points
to is unchanged. If an error occurred during the set, oError is the offset of
the FEA2 entry where the error occurred. The return code from DosProtectOpenL
is the error code for that error condition. If no error occurred, oError is
undefined.

 EA is nil, then no extended attributes are defined for the file. If extended
 attributes are not to be defined or modified, the pointer EA must be set to
 nil.

 FileHandleLockID = 32-bit LockID for the file handle is returned here.

Possible return codes:
  0 NO_ERROR
  2 ERROR_FILE_NOT_FOUND
  3 ERROR_PATH_NOT_FOUND
  4 ERROR_TOO_MANY_OPEN_FILES
  5 ERROR_ACCESS_DENIED
 12 ERROR_INVALID_ACCESS
 26 ERROR_NOT_DOS_DISK
 32 ERROR_SHARING_VIOLATION
 36 ERROR_SHARING_BUFFER_EXCEEDED
 82 ERROR_CANNOT_MAKE
 87 ERROR_INVALID_PARAMETER
 99 ERROR_DEVICE_IN_USE
108 ERROR_DRIVE_LOCKED
110 ERROR_OPEN_FAILED
112 ERROR_DISK_FULL
206 ERROR_FILENAME_EXCED_RANGE
231 ERROR_PIPE_BUSY

Remarks:
A successful DosProtectOpenL request returns a handle and a 32-bit LockID for
accessing the file. The read/write pointer is set at the first byte of the
file. The position of the pointer can be changed with DosProtectSetFilePtrL or
by read and write operations on the file.

The file s date and time can be queried with DosProtectQueryFileInfo. They are
set with DosProtectSetFileInfo.

The read-only attribute of a file can be set with the ATTRIB command.

ulAttribute cannot be set to Volume Label. To set volume-label information,
issue DosProtectSetFileInfo with a logical drive number. Volume labels cannot
be opened.

InitSize affects the size of the file only when the file is new or is
a replacement. If an existing file is opened, cbFile is ignored. To change the
size of the existing file, issue DosProtectSetFileSizeL.

The value in InitSize is a recommended size. If the full size cannot be
allocated, the open request may still succeed. The file system makes
a reasonable attempt to allocate the new size in an area that is as nearly
contiguous as possible on the medium. When the file size is extended, the
values of the new bytes are undefined.

The Direct Open bit provides direct access to an entire disk or diskette
volume, independent of the file system. This mode of opening the volume that is
currently on the drive returns a handle to the calling function; the handle
represents the logical volume as a single file. The calling function specifies
this handle with a DosDevIOCtl Category 8, DSK_LOCKDRIVE request to prevent
other processes from accessing the logical volume. When you are finished using
the logical volume, issue a DosDevIOCtl Category 8, DSK_UNLOCKDRIVE request to
allow other processes to access the logical volume.

The file-handle state bits can be set by DosProtectOpenL and
DosProtectSetFHState. An application can query the file-handle state bits, as
well as the rest of the Open Mode field, by issuing DosProtectQueryFHState.

You can use an TEAOP2 structure to set extended attributes in EA when creating
a file, replacing an existing file, or truncating an existing file. No extended
attributes are set when an existing file is just opened.

A replacement operation is logically equivalent to atomically deleting and
re-creating the file. This means that any extended attributes associated with
the file also are deleted before the file is re-created.

The FileHandleLockID returned is required on each of the DosProtectxxx
functions. An incorrect pfhFileHandleLockID on subsequent DosProtectxxx calls
results in an ERROR_ACCESS_DENIED return code.

The DosProtectxxx functions can be used with a NULL filehandle LockID, if the
subject filehandle was obtained from DosOpen.
*)
function DosProtectOpenL (FileName: PChar; var Handle: THandle;
                         var Action: cardinal; InitSize: int64; Attrib,
                         OpenFlags, OpenMode: cardinal; EA: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;


(*
DosProtectSetFilePtrL moves the read or write pointer according to the type
of move specified.

Parameters:
 Handle = the handle returned by a previous DosOpenL function.
 Pos = The signed distance (offset) to move, in bytes.
 Method = The method of moving - location in the file at which the read/write
          pointer starts before adding the Pos offset. The values and their
          meanings are as shown in the following list:
    0 FILE_BEGIN - move the pointer from the beginning of the file. 
    1 FILE_CURRENT - move the pointer from the current location of the
                     read/write pointer.
    2 FILE_END - move the pointer from the end of the file; use this method
                 to determine a file's size.
 PosActual = address of the new pointer location.
 FileHandleLockID = The filehandle lockid returned by a previous
                    DosProtectOpenL. 

Possible return codes:
  0 NO_ERROR
  1 ERROR_INVALID_FUNCTION
  6 ERROR_INVALID_HANDLE
132 ERROR_SEEK_ON_DEVICE
131 ERROR_NEGATIVE_SEEK
130 ERROR_DIRECT_ACCESS_HANDLE

Remarks:

The read/write pointer in a file is a signed 64-bit number. A negative value
for Pos moves the pointer backward in the file; a positive value moves it
forward. DosProtectSetFilePtrL cannot be used to move to a negative position in
the file.

DosProtectSetFilePtrL cannot be used for a character device or pipe.
*)
function DosProtectSetFilePtrL (Handle: THandle; Pos: int64;
                              Method: cardinal; var PosActual: int64; 
                                  FileHandleLockID: cardinal): cardinal; cdecl;
(*
DosProtectSetFileSizeL changes the size of a file.

Parameters:
 Handle = handle of the file whose size to be changed.
 Size = new size, in bytes, of the file.
 FileHandleLockID = the filehandle lockid obtained from DosProtectOpenL.

Possible return codes:
  0 NO_ERROR
  5 ERROR_ACCESS_DENIED
  6 ERROR_INVALID_HANDLE
 26 ERROR_NOT_DOS_DISK
 33 ERROR_LOCK_VIOLATION
 87 ERROR_INVALID_PARAMETER
112 ERROR_DISK_FULL

Remarks:

When DosProtectSetFileSizeL is issued, the file must be open in a mode that
allows write access. 

The size of the open file can be truncated or extended. If the file size is
being extended, the file system tries to allocate additional bytes in
a contiguous (or nearly contiguous) space on the medium. The values of the new
bytes are undefined.
*)
function DosProtectSetFileSizeL (Handle: THandle; Size: int64;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

(*
DosGetProcessorStatus allows checking status of individual processors
in a SMP machine.

Parameters:
ProcID = Procesor ID numbered 1 through n, where there are n processors in
         total.
Status = Returned processor status defined as follows:
  PROC_OFFLINE 0x00000000 Processor is offline 
  PROC_ONLINE 0x00000001 Processor is online 

Possible return codes:
 0 NO_ERROR
87 ERROR_INVALID_PARAMETER
*)
function DosGetProcessorStatus (ProcID: cardinal;
                                        var Status: cardinal): cardinal; cdecl;


(*
DosSetProcessorStatus sets the ONLINE or OFFLINE status of a processor on
an SMP system. The processor status may be queried using DosGetProcessorStatus.
ONLINE status implies the processor is available for running work. OFFLINE
status implies the processor is not available for running work. The processor
that executes DosSetProcessorStatus must be ONLINE.

Parameters:
ProcID = Processor ID numbered from 1 through n, where there are n processors
         in total.
Status = Requested processor status defined as follows:
  PROC_OFFLINE 0x00000000 Processor is offline. 
  PROC_ONLINE 0x00000001 Processor is online. 

Possible return codes:
 0 NO_ERROR 
87 ERROR_INVALID_PARAMETER
*)
function DosSetProcessorStatus (ProcID: cardinal;
                                            Status: cardinal): cardinal; cdecl;

(*
DosQueryThreadAffinity allows a thread to inquire for the current thread's
processor affinity mask and the system's capable processor affinity mask.

Parameters:
Scope = Scope of the query defined by one of the following values:
  AFNTY_THREAD Return the current threads processor affinity mask. 
  AFNTY_SYSTEM Return the system's current capable processor affinity mask. 
AffinityMask = Affinity mask is returned here; processors 0..31 are in Mask [0]
               and processors 32..63 are in Mask [1].

Possible return codes:
13 ERROR_INVALID_DATA 
87 ERROR_INVALID_PARAMETER
*)
function DosQueryThreadAffinity (Scope: cardinal;
                               var AffinityMask: TMPAffinity): cardinal; cdecl;

(*
DosQueryExtLibPath returns the current path to be searched before or after the
system LIBPATH when locating DLLs.

Parameters:
ExtLIBPATH = Buffer for receiving the extended LIBPATH string.

???
If the buffer pointed to by this parameter is not large enough to hold
the extended LIBPATH or an extended LIBPATH is not currently defined, this
parameter returns a pointer to a NULL string.
??? How is it detected if the size is not passed???

Flags - flag indicating when the new path is searched - possible values:
  BEGIN_LIBPATH - The new path is searched before the LIBPATH.
  END_LIBPATH - The new path is searched after the LIBPATH.

Possible return codes:
  0 NO_ERROR 
 87 ERROR_INVALID_PARAMETER 
122 ERROR_INSUFFICIENT_BUFER
*)
function DosQueryExtLibPath (ExtLibPath: PChar; Flags: cardinal): cardinal;
                                                                         cdecl;

(*
DosSetExtLibPath defines the current path to be searched before or after the
system LIBPATH when locating DLLs.

Parameters:
ExtLIBPATH = New extended LIBPATH string. Maximum size is 1024 bytes.
             A pointer to a NULL string removes the extended LIBPATH.
Flags = When the new path is searched - possible values:
  BEGIN_LIBPATH (1) - the new path is searched before the LIBPATH.
  END_LIBPATH (2) - the new path is searched after the LIBPATH.

The LIBPATH string is an environment variable found in the CONFIG.SYS file
consisting of a set of paths. If a fully-qualified path is not specified when
a module is loaded, the system searches these paths to find the DLL.

There are two extended LIBPATH strings, BeginLIBPATH and EndLIBPATH.
BeginLIBPATH is searched before the system LIBPATH, and EndLIBPATH is searched
after both BeginLIBPATH and the system LIBPATH. These extended LIBPATHs can be
set either from the command line using the "SET" command, or by calling
DosSetExtLIBPATH. When DosSetExtLIBPATH is called, all modifications become
specific to that process. Initial settings can be set for all processes in the
system by setting the values in CONFIG.SYS using the "set" command.

Note: The extended LIBPATHs are not true environment variables, and do not
appear when querying the environment.

Every process inherits the settings of BeginLIBPATH and EndLIBPATH from the
process that starts it. If the extended library paths are initialized in
CONFIG.SYS, those extended library paths become the initial settings for new
processes. If a process changes BeginLIBPATH or EndLIBPATH and starts a new
process, the new child process inherits the changed contents. Child processes
that inherit their parent's extended LIBPATHs maintain their own copy.
Modifications made by the parent after the child has been created have no
effect on the children.

This function permits the use of two symbols within the path string:
%BeginLIBPATH% and %EndLIBPATH%. These symbols are replaced with the current
string settings for the extended library paths.

The LIBPATHs strings are only searched when the specified DLL is not currently
loaded. The only way to guarantee that the DLL being used is the correct
version is to set the fully-qualified path in DosLoadModule. When a request is
made to load a module and a path is not specified, the system searches the
paths in the LIBPATH string and uses the first instance of the specified DLL it
finds. If the new paths are added to the search strings, the system does not
check those directories to see if a different version exists. Consequently, if
two processes started from different directories use the same DLL, but
different versions of that DLL exist in both directories, the version of the
DLL loaded by the first process is the one used by both processes. The only way
to prevent this from occurring is to specify the path to the DLL when
DosLoadModule is called.

Consequently, if one process sets its BeginLIBPATH to C:\PROCESS1\DLL and loads
the DLL MYAPP.DLL from that directory, and then a second process sets its
BeginLIBPATH to C:\PROCESS2\DLL and there is for a different version of
MYAPP.DLL in C:\PROCESS2\DLL, the second process will link to the DLL from
C:\PROCESS1\DLL since it was already loaded.

Both BeginLIBPATH and EndLIBPATH can be set from the command line using the SET
command.

Possible return codes:
0 NO_ERROR 
8 ERROR_NOT_ENOUGH_MEMORY 
87 ERROR_INVALID_PARAMETER 
161 ERROR_BAD_PATHNAME 
*)
function DosSetExtLibPath (ExtLibPath: PChar; Flags: cardinal): cardinal;
                                                                         cdecl;

(*
DosQueryModFromEIP queries a module handle and name from a given flat address.
It takes a flat 32 bit address as a parameter and returns information about the
module (a protected mode application currently executing) owning the storage.

Parameters:
HMod = Address of a location in which the module handle is returned.
ObjNum = Address of a cardinal where the module object number corresponding to
         the Address is returned. The object number is zero based.
BuffLen = Length of the user supplied buffer pointed to by Buff.
Buff = Address of a user supplied buffer in which the module name is returned.
Offset = Address where the offset to the object corresponding to the Address is
         returned. The offset is zero based.
Address = Input address to be queried.

Possible return codes:
  0 NO_ERROR 
 87 ERROR_INVALID_PARAMETER 
487 ERROR_INVALID_ADDRESS
*)
function DosQueryModFromEIP (var HMod: THandle; var ObjNum: cardinal;
                         BuffLen: cardinal; Buff: PChar; var Offset: cardinal;
                                            Address: PtrUInt): cardinal; cdecl;


function DosDumpProcess (Flags: cardinal; Drive: cardinal;
                                               PID: cardinal): cardinal; cdecl;


function DosSuppressPopups (Flags: cardinal;
                                             Drive: cardinal): cardinal; cdecl;


(*
DosPerfSysCall retrieves system performance information and performs software
tracing.

Parameters:
Command = Command to be performed; the following commands are accepted:
  CMD_KI_RDCNT ($63) - reads CPU utilization information in both uniprocessor
                       and symmetric multi-processor (SMP) environments by
                       taking a snapshot of the time stamp counters. To
                       determine CPU utilization, the application must compute
                       the difference between two time stamp snapshots using 64
                       bit aritimetic.
  CMD_SOFTTRACE_LOG ($14) - records software trace information.
Parm1 (CPUUtil) = Command-specific. In case of CMD_KI_RDCNT, pointer to
                  TCPUUtil record. In case of CMD_SOFTTRACE_LOG, major code for
                  the trace entry in the range of 0 to 255. Major codes 184
                  ($B8) and 185 ($B9) have been reserved for IBM customer use.
                  Major code 1 is reserved for exclusive use by IBM.
Parm2 = Command-specific. In case of CMD_KI_RdCnt, it must be 0. In case of
        CMD_SOFTTRACE_LOG, minor code for the trace entry in the range of
        0 to 255.
Parm3 (HookData) = Command-specific. In case of CMD_KI_RdCnt, it must be 0. In
                   case of CMD_SOFTTRACE_LOG, pointer to a HOOKDATA data
                   structure (see example code).

Possible return codes:
0 NO_ERROR 
1 ERROR_INVALID_FUNCTION

Remarks:
DosPerfSysCall is a general purpose performance function. This function accepts
four parameters. The first parameter is the command requested. The other three
parameters are command specific.

Some functions of DosPerfSysCall may have a dependency on Intel Pentium or
Pentium-Pro (or higher) support. If a function cannot be provided because OS/2
is not running on a processor with the required features, a return code will
indicate an attempt to use an unsupported function.

Example code (C):
  int main (int argc, char *argv[])
  {
     APIRET     rc;
     BYTE       HookBuffer [256];
     HOOKDATA   Hookdata = {0,HookBuffer};
     ULONG      ulMajor, ulMinor;
     *((PULONG)  HookBuffer[0]) = 1;
     *((PULONG)  HookBuffer[4]) = 2;
     *((PULONG)  HookBuffer[8]) = 3;
     strcpy((PSZ  HookBuffer[12], "Test of 3 ULONG values and a string.")
     HookData.ulLength = 12 + strlen((PSZ HookBuffer[12]) + 1;

     ulMajor = 0x00b8 
     ulMinor = 0x0001

     rc = DosPerfSystCall(CMD_SOFTTRACE_LOG, ulMajor, ulMinor, (ULONG)  HookData);
     if (rc != NO_ERROR) {
       fprintf (stderr, "CMD_SOFTTRACE_LOG failed   rc = %u\n", rc);
       return 1;
       }

     return NO_ERROR;
  }
*)
function DosPerfSysCall (Command, Parm1, Parm2,
                                             Parm3: cardinal): cardinal; cdecl;
function DosPerfSysCall (Command, Parm1, Parm2: cardinal;
                                                var HookData): cardinal; cdecl;
function DosPerfSysCall (Command: cardinal; var CpuUtil: TCPUUtil; Parm2,
                                             Parm3: cardinal): cardinal; cdecl;


function DosQueryThreadContext (TID: cardinal; Level: cardinal;
                           var ContextRecord: TContextRecord): cardinal; cdecl;


(*
DosQueryABIOSSupport returns flags that indicate various basic hardware
configurations.

Parameters:
Reserved = Must be set to 0, no other value is defined.

The result of this function contains combination of flags (HW_Cfg_* constants)
signalizing the underlying hardware configuration.
*)
function DosQueryABIOSSupport (Reserved: cardinal): cardinal; cdecl;


{***************************************************************************}
implementation
{***************************************************************************}

uses
  OS2Def;


function DummyDosCancelLockRequestL (Handle: THandle; var Lock: TFileLockL): cardinal; cdecl;
var
  Lock0: TFileLock;
begin
  if (Lock.Offset > high (longint)) or (Lock.Range > high (longint)) or
                                     (Lock.Offset < 0) or (Lock.Range < 0) then
   DummyDosCancelLockRequestL := Error_Invalid_Parameter
  else
   begin
    Lock0.Offset := longint (Lock.Offset);
    Lock0.Range := longint (Lock.Range);
    DummyDosCancelLockRequestL := DosCancelLockRequest (Handle, Lock0);
   end;
end;


function DummyDosProtectSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
                   var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
var
  Lock0: TFileLock;
  UnLock0: TFileLock;
begin
  if (Lock.Offset > high (longint)) or (Lock.Range > high (longint)) or
       (Unlock.Offset > high (longint)) or (Unlock.Range > high (longint)) then
   DummyDosProtectSetFileLocksL := Error_Invalid_Parameter
  else
   begin
    Lock0.Offset := longint (Lock.Offset);
    Lock0.Range := longint (Lock.Range);
    Unlock0.Offset := longint (Unlock.Offset);
    Unlock0.Range := longint (Lock.Range);
    DummyDosProtectSetFileLocksL := DosProtectSetFileLocks (Handle, Unlock0,
                                      Lock0, Timeout, Flags, FileHandleLockID);
   end;
end;


function DummyDosSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
    var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal): cardinal; cdecl;
var
  Lock0: TFileLock;
  UnLock0: TFileLock;
begin
  if (Lock.Offset > high (longint)) or (Lock.Range > high (longint)) or
      (Lock.Offset < 0) or (Lock.Range < 0) or
      (Unlock.Offset < 0) or (Unlock.Range < 0) or
       (Unlock.Offset > high (longint)) or (Unlock.Range > high (longint)) then
   DummyDosSetFileLocksL := Error_Invalid_Parameter
  else
   begin
    Lock0.Offset := longint (Lock.Offset);
    Lock0.Range := longint (Lock.Range);
    Unlock0.Offset := longint (Unlock.Offset);
    Unlock0.Range := longint (Lock.Range);
    DummyDosSetFileLocksL := DosSetFileLocks (Handle, Unlock0, Lock0, Timeout,
                                                                        Flags);
   end;
end;


function DummyDosProtectOpenL (FileName: PChar; var Handle: THandle;
                         var Action: cardinal; InitSize: int64; Attrib,
                         OpenFlags, OpenMode: cardinal; EA: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;
begin
  if InitSize > high (longint) then
   DummyDosProtectOpenL := Error_Invalid_Parameter
  else
   DummyDosProtectOpenL := DosProtectOpen (FileName, Handle, Action,
        longint (InitSize), Attrib, OpenFlags, OpenMode, EA, FileHandleLockID);
end;


function DummyDosProtectSetFilePtrL (Handle: THandle; Pos: int64;
                              Method: cardinal; var PosActual: int64; 
                                  FileHandleLockID: cardinal): cardinal; cdecl;
var
  PosActual0: cardinal;
begin
  if (Pos < low (longint)) or (Pos > high (longint)) then
   DummyDosProtectSetFilePtrL := Error_Invalid_Parameter
  else
   begin
    DummyDosProtectSetFilePtrL := DosProtectSetFilePtr (Handle, longint (Pos),
                                         Method, PosActual0, FileHandleLockID);
    PosActual := PosActual0;
   end;
end;


function DummyDosProtectSetFileSizeL (Handle: THandle; Size: int64;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  if (Size > high (cardinal)) then
   DummyDosProtectSetFileSizeL := Error_Invalid_Parameter
  else
   DummyDosProtectSetFileSizeL := DosProtectSetFileSize (Handle,
                                            cardinal (Size), FileHandleLockID);
end;


function DummyDosProtectSetFileLocks (Handle: THandle;
                                      var Unlock, Lock: TFileLock;
                                      Timeout, Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  if FileHandleLockID <> 0 then
   DummyDosProtectSetFileLocks := Error_Invalid_Parameter
  else
   DummyDosProtectSetFileLocks := DosSetFileLocks (Handle, Unlock, Lock,
                                                               Timeout, Flags);
end;


function DummyDosProtectOpen (FileName: PChar; var Handle: THandle;
                              var Action: cardinal; InitSize, Attrib,
                              OpenFlags, OpenMode: cardinal; ea: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectOpen := DosOpen (FileName, Handle, Action, InitSize, Attrib,
                                                      OpenFlags, OpenMode, EA);
  FileHandleLockID := 0;
end;


function DummyDosProtectClose (Handle: THandle;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectClose := DosClose (Handle);
end;


function DummyDosProtectRead (Handle: THandle; var Buffer; Count: cardinal;
          var ActCount: cardinal; FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectRead := DosRead (Handle, Buffer, Count, ActCount);
end;


function DummyDosProtectWrite (Handle: THandle; const Buffer; Count: cardinal;
                                var ActCount: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectWrite := DosWrite (Handle, Buffer, Count, ActCount);
end;


function DummyDosProtectSetFilePtr (Handle: THandle; Pos: longint;
                                    Method: cardinal; var PosActual: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectSetFilePtr := DosSetFilePtr (Handle, Pos, Method, PosActual);
end;


function DummyDosProtectSetFileSize (Handle: THandle; Size: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectSetFileSize := DosSetFileSize (Handle, Size);
end;


function DummyDosProtectQueryFHState (Handle: THandle; var FileMode: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectQueryFHState := DosQueryFHState (Handle, FileMode);
end;


function DummyDosProtectSetFHState (Handle: THandle; FileMode: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectSetFHState := DosSetFHState (Handle, FileMode);
end;


function DummyDosProtectQueryFileInfo (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectQueryFileInfo := DosQueryFileInfo (Handle, InfoLevel,
                                                   AFileStatus, FileStatusLen);
end;


function DummyDosProtectSetFileInfo (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectSetFileInfo := DosSetFileInfo (Handle, InfoLevel,
                                                   AFileStatus, FileStatusLen);
end;


function DummyDosProtectEnumAttribute (RefType: cardinal; AFile: pointer;
                                  Entry: cardinal; var Buf; BufSize: cardinal;
                                     var Count: cardinal; InfoLevel: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
begin
  DummyDosProtectEnumAttribute := DosEnumAttribute (RefType, AFile, Entry,
                                               Buf, BufSize, Count, InfoLevel);
end;


function DummyDosGetProcessorStatus (ProcID: cardinal;
                                        var Status: cardinal): cardinal; cdecl;
begin
  if ProcID = 1 then
   begin
    Status := 1;
    DummyDosGetProcessorStatus := No_Error;
   end
  else
   DummyDosGetProcessorStatus := Error_Invalid_Parameter;
end;


function DummyDosSetProcessorStatus (ProcID: cardinal;
                                            Status: cardinal): cardinal; cdecl;
begin
  DummyDosSetProcessorStatus := Error_Invalid_Parameter;
end;


function DummyDosQueryThreadAffinity (Scope: cardinal;
                               var AffinityMask: TMPAffinity): cardinal; cdecl;
begin
  DummyDosQueryThreadAffinity := Error_Invalid_Function;
end;


function DummyDosSetThreadAffinity (var AffinityMask: TMPAffinity): cardinal;
                                                                         cdecl;
begin
  DummyDosSetThreadAffinity := Error_Invalid_Function;
end;


function DummyDosQueryExtLibPath (ExtLibPath: PChar;
                                             Flags: cardinal): cardinal; cdecl;
begin
  if ExtLibPath <> nil then
   begin
    ExtLibPath := #0;
    DummyDosQueryExtLibPath := No_Error;
   end
  else
   DummyDosQueryExtLibPath := Error_Invalid_Parameter;
end;


function DummyDosSetExtLibPath (ExtLibPath: PChar; Flags: cardinal): cardinal;
                                                                         cdecl;
begin
  DummyDosSetExtLibPath := Error_Not_Enough_Memory;
end;


function DummyDosQueryModFromEIP (var HMod: THandle; var ObjNum: cardinal;
                         BuffLen: cardinal; Buff: PChar; var Offset: cardinal;
                                            Address: PtrUInt): cardinal; cdecl;
begin
  DummyDosQueryModFromEIP := Error_Invalid_Parameter;
  HMod := THandle (-1);
  ObjNum := 0;
  if Buff <> nil then
   Buff^ := #0;
  Offset := 0;
end;


function DummyDosDumpProcess (Flags: cardinal; Drive: cardinal;
                                               PID: cardinal): cardinal; cdecl;
begin
  DummyDosDumpProcess := Error_Invalid_Function;
end;


function DummyDosSuppressPopups (Flags: cardinal;
                                             Drive: cardinal): cardinal; cdecl;
begin
  DummyDosSuppressPopups := Error_Invalid_Function;
end;


function DummyDosPerfSysCall (Command, Parm1, Parm2,
                                             Parm3: cardinal): cardinal; cdecl;
begin
  DummyDosPerfSysCall := Error_Invalid_Function;
end;


function DummyDosQueryThreadContext (TID: cardinal; Level: cardinal;
                           var ContextRecord: TContextRecord): cardinal; cdecl;
begin
  DummyDosQueryThreadContext := Error_Invalid_Function;
end;


function DummyDosQueryABIOSSupport (Reserved: cardinal): cardinal; cdecl;
begin
  DummyDosQueryABIOSSupport := 0;
end;



type
  TDosProtectOpen = function (FileName: PChar; var Handle: THandle;
                              var Action: cardinal; InitSize, Attrib,
                              OpenFlags, OpenMode: cardinal; ea: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectClose = function (Handle: THandle;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectRead = function (Handle: THandle; var Buffer; Count: cardinal;
          var ActCount: cardinal; FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectWrite = function (Handle: THandle; const Buffer; Count: cardinal;
                               var ActCount: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFilePtr = function (Handle: THandle; Pos: longint;
                                    Method: cardinal; var PosActual: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFileSize = function (Handle: THandle; Size: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectQueryFHState = function (Handle: THandle; var FileMode: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFHState = function (Handle: THandle; FileMode: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectQueryFileInfo = function (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFileInfo = function (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectEnumAttribute = function (RefType: cardinal; AFile: pointer;
                                  Entry: cardinal; var Buf; BufSize: cardinal;
                                  var Count: cardinal; InfoLevel: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFileLocks = function (Handle: THandle;
                                      var Unlock, Lock: TFileLock;
                                      Timeout, Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosCancelLockRequestL = function (Handle: THandle; var Lock: TFileLockL):
                                                               cardinal; cdecl;

  TDosProtectSetFileLocksL = function (Handle: THandle; var Unlock: TFileLockL;
                   var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosSetFileLocksL = function (Handle: THandle; var Unlock: TFileLockL;
    var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal): cardinal; cdecl;

  TDosProtectOpenL = function (FileName: PChar; var Handle: THandle;
                         var Action: cardinal; InitSize: int64; Attrib,
                         OpenFlags, OpenMode: cardinal; EA: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFilePtrL = function (Handle: THandle; Pos: int64;
          Method: cardinal;
            var PosActual: int64; FileHandleLockID: cardinal): cardinal; cdecl;

  TDosProtectSetFileSizeL = function (Handle: THandle; Size: int64;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

  TDosGetProcessorStatus = function (ProcID: cardinal;
                                        var Status: cardinal): cardinal; cdecl;

  TDosSetProcessorStatus = function (ProcID: cardinal;
                                            Status: cardinal): cardinal; cdecl;

  TDosQueryThreadAffinity = function (Scope: cardinal;
                               var AffinityMask: TMPAffinity): cardinal; cdecl;

  TDosSetThreadAffinity = function (var AffinityMask: TMPAffinity): cardinal;
                                                                         cdecl;

  TDosQueryExtLibPath = function (ExtLibPath: PChar;
                                             Flags: cardinal): cardinal; cdecl;

  TDosSetExtLibPath = function (ExtLibPath: PChar; Flags: cardinal): cardinal;
                                                                         cdecl;

  TDosQueryModFromEIP = function (var HMod: THandle; var ObjNum: cardinal;
                         BuffLen: cardinal; Buff: PChar; var Offset: cardinal;
                                            Address: PtrUInt): cardinal; cdecl;

  TDosDumpProcess = function (Flags: cardinal; Drive: cardinal;
                                               PID: cardinal): cardinal; cdecl;

  TDosSuppressPopups = function (Flags: cardinal;
                                             Drive: cardinal): cardinal; cdecl;

  TDosPerfSysCall = function (Command, Parm1, Parm2,
                                             Parm3: cardinal): cardinal; cdecl;

  TDosQueryThreadContext = function (TID: cardinal; Level: cardinal;
                           var ContextRecord: TContextRecord): cardinal; cdecl;

  TDosQueryABIOSSupport = function (Reserved: cardinal): cardinal; cdecl;



const
  Sys_DosCancelLockRequestL: TDosCancelLockRequestL =
                                                   @DummyDosCancelLockRequestL;
  Sys_DosSetFileLocksL: TDosSetFileLocksL = @DummyDosSetFileLocksL;
  Sys_DosProtectSetFileLocksL: TDosProtectSetFileLocksL =
                                                 @DummyDosProtectSetFileLocksL;
  Sys_DosProtectOpenL: TDosProtectOpenL = @DummyDosProtectOpenL;
  Sys_DosProtectSetFilePtrL: TDosProtectSetFilePtrL =
                                                   @DummyDosProtectSetFilePtrL;
  Sys_DosProtectSetFileSizeL: TDosProtectSetFileSizeL =
                                                  @DummyDosProtectSetFileSizeL;
  Sys_DosProtectOpen: TDosProtectOpen = @DummyDosProtectOpen;
  Sys_DosProtectClose: TDosProtectClose = @DummyDosProtectClose;
  Sys_DosProtectRead: TDosProtectRead = @DummyDosProtectRead;
  Sys_DosProtectWrite: TDosProtectWrite = @DummyDosProtectWrite;
  Sys_DosProtectSetFilePtr: TDosProtectSetFilePtr = @DummyDosProtectSetFilePtr;
  Sys_DosProtectSetFileSize: TDosProtectSetFileSize =
                                                   @DummyDosProtectSetFileSize;
  Sys_DosProtectQueryFHState: TDosProtectQueryFHState =
                                                  @DummyDosProtectQueryFHState;
  Sys_DosProtectSetFHState: TDosProtectSetFHState = @DummyDosProtectSetFHState;
  Sys_DosProtectQueryFileInfo: TDosProtectQueryFileInfo =
                                                 @DummyDosProtectQueryFileInfo;
  Sys_DosProtectSetFileInfo: TDosProtectSetFileInfo =
                                                   @DummyDosProtectSetFileInfo;
  Sys_DosProtectEnumAttribute: TDosProtectEnumAttribute =
                                                 @DummyDosProtectEnumAttribute;
  Sys_DosProtectSetFileLocks: TDosProtectSetFileLocks =
                                                  @DummyDosProtectSetFileLocks;
  Sys_DosGetProcessorStatus: TDosGetProcessorStatus =
                                                   @DummyDosGetProcessorStatus;
  Sys_DosSetProcessorStatus: TDosSetProcessorStatus =
                                                   @DummyDosSetProcessorStatus;
  Sys_DosQueryThreadAffinity: TDosQueryThreadAffinity =
                                                  @DummyDosQueryThreadAffinity;
  Sys_DosSetThreadAffinity: TDosSetThreadAffinity = @DummyDosSetThreadAffinity;
  Sys_DosQueryExtLibPath: TDosQueryExtLibPath = @DummyDosQueryExtLibPath;
  Sys_DosSetExtLibPath: TDosSetExtLibPath = @DummyDosSetExtLibPath;
  Sys_DosQueryModFromEIP: TDosQueryModFromEIP = @DummyDosQueryModFromEIP;
  Sys_DosDumpProcess: TDosDumpProcess = @DummyDosDumpProcess;
  Sys_DosSuppressPopups: TDosSuppressPopups = @DummyDosSuppressPopups;
  Sys_DosPerfSysCall: TDosPerfSysCall = @DummyDosPerfSysCall;
  Sys_DosQueryThreadContext: TDosQueryThreadContext =
                                                   @DummyDosQueryThreadContext;
  Sys_DosQueryABIOSSupport: TDosQueryABIOSSupport = @DummyDosQueryABIOSSupport;



function DosOpenL (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                         EA: pointer): cardinal; cdecl; inline;
begin
  DosOpenL := Sys_DosOpenL (FileName, Handle, Action, InitSize, Attrib,
                                                      OpenFlags, FileMode, EA);
end;


function DosSetFilePtrL (Handle: THandle; Pos: int64; Method: cardinal;
                                var PosActual: int64): cardinal; cdecl; inline;
begin
  DosSetFilePtrL := Sys_DosSetFilePtrL (Handle, Pos, Method, PosActual);
end;


function DosSetFileSizeL (Handle: THandle; Size: int64): cardinal; cdecl;
                                                                        inline;
begin
  DosSetFileSizeL := Sys_DosSetFileSizeL (Handle, Size);
end;


function DosProtectOpen (FileName: PChar; var Handle: THandle;
                              var Action: cardinal; InitSize, Attrib,
                              OpenFlags, OpenMode: cardinal; EA: PEAOp2;
                      var FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectOpen := Sys_DosProtectOpen (FileName, Handle, Action, InitSize,
                            Attrib, OpenFlags, OpenMode, EA, FileHandleLockID);
end;


function DosProtectClose (Handle: THandle;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectClose := Sys_DosProtectClose (Handle, FileHandleLockID);
end;


function DosProtectRead (Handle: THandle; var Buffer; Count: cardinal;
  var ActCount: cardinal; FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectRead := Sys_DosProtectRead (Handle, Buffer, Count, ActCount,
                                                             FileHandleLockID);
end;


function DosProtectWrite (Handle: THandle; const Buffer; Count: cardinal;
                          var ActCount: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectWrite := Sys_DosProtectWrite (Handle, Buffer, Count, ActCount,
                                                             FileHandleLockID);
end;


function DosProtectSetFilePtr (Handle: THandle; Pos: longint;
                               Method: cardinal; var PosActual: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFilePtr := Sys_DosProtectSetFilePtr (Handle, Pos, Method,
                                                  PosActual, FileHandleLockID);
end;


function DosProtectSetFileSize (Handle: THandle; Size: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFileSize := Sys_DosProtectSetFileSize (Handle, Size,
                                                             FileHandleLockID);
end;


function DosProtectQueryFHState (Handle: THandle; var FileMode: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectQueryFHState := Sys_DosProtectQueryFHState (Handle, FileMode,
                                                             FileHandleLockID);
end;


function DosProtectSetFHState (Handle: THandle; FileMode: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFHState := Sys_DosProtectSetFHState (Handle, FileMode,
                                                             FileHandleLockID);
end;


function DosProtectQueryFileInfo (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectQueryFileInfo := Sys_DosProtectQueryFileInfo (Handle, InfoLevel,
                                 AFileStatus, FileStatusLen, FileHandleLockID);
end;


function DosProtectSetFileInfo (Handle: THandle; InfoLevel: cardinal;
                            AFileStatus: PFileStatus; FileStatusLen: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFileInfo := Sys_DosProtectSetFileInfo (Handle, InfoLevel,
                                 AFileStatus, FileStatusLen, FileHandleLockID);
end;


function DosProtectEnumAttribute (RefType: cardinal; AFile: pointer;
                                  Entry: cardinal; var Buf; BufSize: cardinal;
                                  var Count: cardinal; InfoLevel: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectEnumAttribute := Sys_DosProtectEnumAttribute (RefType, AFile,
                      Entry, Buf, BufSize, Count, InfoLevel, FileHandleLockID);
end;


function DosProtectSetFileLocks (Handle: THandle;
                                 var Unlock, Lock: TFileLock;
                                 Timeout, Flags: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFileLocks := Sys_DosProtectSetFileLocks (Handle, Unlock, Lock,
                                             Timeout, Flags, FileHandleLockID);
end;


function DosProtectOpen (FileName: PChar; var Handle: longint;
                         var Action: longint; InitSize, Attrib,
                         OpenFlags, OpenMode: longint; ea: PEAOp2;
                      var FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectOpen := Sys_DosProtectOpen (FileName, THandle (Handle),
            cardinal (Action), cardinal (InitSize), cardinal (Attrib),
              cardinal (OpenFlags), cardinal (OpenMode), EA, FileHandleLockID);
end;


function DosProtectOpen (const FileName: string; var Handle: longint;
                         var Action: longint; InitSize, Attrib,
                         OpenFlags, OpenMode: longint; ea: PEAOp2;
                                     var FileHandleLockID: cardinal): cardinal;
var
  T: array [0..255] of char;
begin
  StrPCopy (@T, FileName);
  DosProtectOpen := Sys_DosProtectOpen (@T, THandle (Handle),
         cardinal (Action), cardinal (InitSize), cardinal (Attrib),
              cardinal (OpenFlags), cardinal (OpenMode), EA, FileHandleLockID);
end;


function DosProtectOpen (const FileName: string; var Handle: THandle;
                         var Action: cardinal; InitSize, Attrib,
                         OpenFlags, OpenMode: cardinal; ea: PEAOp2;
                               var FileHandleLockID: cardinal): cardinal;
var
  T: array [0..255] of char;
begin
  StrPCopy (@T, FileName);
  DosProtectOpen := Sys_DosProtectOpen (@T, Handle, Action, InitSize, Attrib,
                                    OpenFlags, OpenMode, EA, FileHandleLockID);
end;


function DosProtectRead (Handle: longint; var Buffer; Count: longint;
   var ActCount: longint; FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectRead := Sys_DosProtectRead (THandle (Handle), Buffer,
                      cardinal (Count), cardinal (ActCount), FileHandleLockID);
end;


function DosProtectWrite (Handle: longint; const Buffer; Count: longint;
                          var ActCount: longint;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectWrite := Sys_DosProtectWrite (THandle (Handle), Buffer,
                      cardinal (Count), cardinal (ActCount), FileHandleLockID);
end;


function DosProtectSetFilePtr (Handle: longint; Pos, Method: longint;
                               var PosActual: longint;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFilePtr := Sys_DosProtectSetFilePtr (THandle (Handle),
    cardinal (Pos), cardinal (Method), cardinal (PosActual), FileHandleLockID);
end;


function DosProtectSetFilePtr (Handle: THandle; Pos: longint;
                                         FileHandleLockID: cardinal): cardinal;
var
  PosActual: cardinal;
begin
  DosProtectSetFilePtr := DosProtectSetFilePtr (Handle, Pos, 0, PosActual,
                                                             FileHandleLockID);
end;


function DosProtectGetFilePtr (Handle: longint;
                 var PosActual: longint; FileHandleLockID: cardinal): cardinal;
begin
  DosProtectGetFilePtr := DosProtectSetFilePtr (THandle (Handle), 0, 1,
                                       cardinal (PosActual), FileHandleLockID);
end;


function DosProtectGetFilePtr (Handle: THandle;
                var PosActual: cardinal; FileHandleLockID: cardinal): cardinal;
begin
  DosProtectGetFilePtr := DosProtectSetFilePtr (Handle, 0, 1, PosActual,
                                                             FileHandleLockID);
end;


function DosProtectEnumAttribute (Handle: THandle; Entry: cardinal; var Buf;
                                  BufSize: cardinal; var Count: cardinal;
                                  InfoLevel: cardinal;
                                         FileHandleLockID: cardinal): cardinal;
begin
    DosProtectEnumAttribute := DosProtectEnumAttribute (0, @Handle, Entry, Buf,
                                  BufSize, Count, InfoLevel, FileHandleLockID);
end;


function DosProtectEnumAttribute (const FileName: string; Entry: cardinal;
                                  var Buf; BufSize: cardinal;
                                  var Count: cardinal; InfoLevel: cardinal;
                                         FileHandleLockID: cardinal): cardinal;
var
  T: array [0..255] of char;
begin
  StrPCopy (@T, FileName);
  DosProtectEnumAttribute := DosProtectEnumAttribute (1, @T, Entry, Buf,
                                  BufSize, Count, InfoLevel, FileHandleLockID);
end;


function DosCancelLockRequestL (Handle: THandle;
                                var Lock: TFileLockL): cardinal; cdecl; inline;
begin
  DosCancelLockRequestL := Sys_DosCancelLockRequestL (Handle, Lock);
end;


function DosProtectSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
                   var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFileLocksL := Sys_DosProtectSetFileLocksL (Handle, Unlock, Lock,
                                             Timeout, Flags, FileHandleLockID);
end;


function DosSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
    var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal): cardinal; cdecl;
                                                                        inline;
begin
  DosSetFileLocksL := Sys_DosSetFileLocksL (Handle, Unlock, Lock, Timeout,
                                                                        Flags);
end;


function DosProtectOpenL (FileName: PChar; var Handle: THandle;
                         var Action: cardinal; InitSize: int64; Attrib,
                         OpenFlags, OpenMode: cardinal; EA: PEAOp2;
                      var FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectOpenL := Sys_DosProtectOpenL (FileName, Handle, Action, InitSize,
                            Attrib, OpenFlags, OpenMode, EA, FileHandleLockID);
end;


function DosProtectSetFilePtrL (Handle: THandle; Pos: int64;
                              Method: cardinal; var PosActual: int64; 
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFilePtrL := Sys_DosProtectSetFilePtrL (Handle, Pos, Method,
                                                  PosActual, FileHandleLockID);
end;


function DosProtectSetFileSizeL (Handle: THandle; Size: int64;
                          FileHandleLockID: cardinal): cardinal; cdecl; inline;
begin
  DosProtectSetFileSizeL := Sys_DosProtectSetFileSizeL (Handle, Size,
                                                             FileHandleLockID);
end;


function DosGetProcessorStatus (ProcID: cardinal;
                                var Status: cardinal): cardinal; cdecl; inline;
begin
  DosGetProcessorStatus := Sys_DosGetProcessorStatus (ProcID, Status);
end;


function DosSetProcessorStatus (ProcID: cardinal;
                                    Status: cardinal): cardinal; cdecl; inline;
begin
  DosSetProcessorStatus := Sys_DosSetProcessorStatus (ProcID, Status);
end;


function DosQueryThreadAffinity (Scope: cardinal;
                       var AffinityMask: TMPAffinity): cardinal; cdecl; inline;
begin
  DosQueryThreadAffinity := Sys_DosQueryThreadAffinity (Scope, AffinityMask);
end;


function DosSetThreadAffinity (var AffinityMask: TMPAffinity): cardinal; cdecl;
                                                                        inline;
begin
  DosSetThreadAffinity := Sys_DosSetThreadAffinity (AffinityMask);
end;


function DosQueryExtLibPath (ExtLibPath: PChar; Flags: cardinal): cardinal;
                                                                 cdecl; inline;
begin
  DosQueryExtLibPath := Sys_DosQueryExtLibPath (ExtLibPath, Flags);
end;


function DosSetExtLibPath (ExtLibPath: PChar; Flags: cardinal): cardinal;
                                                                 cdecl; inline;
begin
  DosSetExtLibPath := Sys_DosSetExtLibPath (ExtLibPath, Flags);
end;


function DosQueryModFromEIP (var HMod: THandle; var ObjNum: cardinal;
                         BuffLen: cardinal; Buff: PChar; var Offset: cardinal;
                                    Address: PtrUInt): cardinal; cdecl; inline;
begin
  DosQueryModFromEIP := Sys_DosQueryModFromEIP (HMod, ObjNum, BuffLen, Buff,
                                                              Offset, Address);
end;


function DosDumpProcess (Flags: cardinal; Drive: cardinal;
                                       PID: cardinal): cardinal; cdecl; inline;
begin
  DosDumpProcess := Sys_DosDumpProcess (Flags, Drive, PID);
end;


function DosSuppressPopups (Flags: cardinal;
                                     Drive: cardinal): cardinal; cdecl; inline;
begin
  DosSuppressPopups := Sys_DosSuppressPopups (Flags, Drive);
end;


function DosPerfSysCall (Command, Parm1, Parm2,
                                     Parm3: cardinal): cardinal; cdecl; inline;
begin
  DosPerfSysCall := Sys_DosPerfSysCall (Command, Parm1, Parm2, Parm3);
end;


function DosPerfSysCall (Command, Parm1, Parm2: cardinal;
                                                var HookData): cardinal; cdecl;
begin
  DosPerfSysCall := Sys_DosPerfSysCall (Command, Parm1, Parm2,
                                                           PtrUInt (HookData));
end;


function DosPerfSysCall (Command: cardinal; var CpuUtil: TCPUUtil; Parm2,
                                             Parm3: cardinal): cardinal; cdecl;
begin
  DosPerfSysCall := Sys_DosPerfSysCall (Command, PtrUInt (@CPUUtil), Parm2,
                                                                        Parm3);
end;


function DosQueryThreadContext (TID: cardinal; Level: cardinal;
                   var ContextRecord: TContextRecord): cardinal; cdecl; inline;
begin
  DosQueryThreadContext := Sys_DosQueryThreadContext (TID, Level,
                                                                ContextRecord);
end;


function DosQueryABIOSSupport (Reserved: cardinal): cardinal; cdecl; inline;
begin
  DosQueryABIOSSupport := Sys_DosQueryABIOSSupport (Reserved);
end;


var
  P: pointer;

begin
  if FSApi64 then
  (* DosCallsHandle successfully initialized during initialization of unit *)
  (* System and basic 64-bit functions were loaded successfully.           *)
   begin
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32CancelLockRequestL, nil, P)
                                                                       = 0 then
     Sys_DosCancelLockRequestL := TDosCancelLockRequestL (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFileLocksL, nil, P)
                                                                       = 0 then
     Sys_DosProtectSetFileLocksL := TDosProtectSetFileLocksL (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32SetFileLocksL, nil, P)
                                                                       = 0 then
     Sys_DosSetFileLocksL := TDosSetFileLocksL (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectOpenL, nil, P) = 0
                                                                           then
     Sys_DosProtectOpenL := TDosProtectOpenL (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFilePtrL, nil, P)
                                                                       = 0 then
     Sys_DosProtectSetFilePtrL := TDosProtectSetFilePtrL (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFileSizeL, nil, P)
                                                                       = 0 then
     Sys_DosProtectSetFileSizeL := TDosProtectSetFileSizeL (P);
   end;

  if DosCallsHandle = THandle (-1) then
   Exit;

  if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectOpen, nil, P) = 0 then
   begin
    Sys_DosProtectOpen := TDosProtectOpen (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectClose, nil, P) = 0
                                                                           then
     Sys_DosProtectClose := TDosProtectClose (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectRead, nil, P) = 0 then
     Sys_DosProtectRead := TDosProtectRead (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectWrite, nil, P) = 0
                                                                           then
     Sys_DosProtectWrite := TDosProtectWrite (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFilePtr, nil,
                                                                    P) = 0 then
     Sys_DosProtectSetFilePtr := TDosProtectSetFilePtr (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFileSize, nil,
                                                                    P) = 0 then
     Sys_DosProtectSetFileSize := TDosProtectSetFileSize (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectQueryFHState, nil,
                                                                    P) = 0 then
     Sys_DosProtectQueryFHState := TDosProtectQueryFHState (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFHState, nil,
                                                                    P) = 0 then
     Sys_DosProtectSetFHState := TDosProtectSetFHState (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectQueryFileInfo, nil,
                                                                    P) = 0 then
     Sys_DosProtectQueryFileInfo := TDosProtectQueryFileInfo (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFileInfo, nil,
                                                                    P) = 0 then
     Sys_DosProtectSetFileInfo := TDosProtectSetFileInfo (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectEnumAttribute, nil,
                                                                    P) = 0 then
     Sys_DosProtectEnumAttribute := TDosProtectEnumAttribute (P);
    if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFileLocks, nil,
                                                                    P) = 0 then
     Sys_DosProtectSetFileLocks := TDosProtectSetFileLocks (P);
   end;

   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32GetProcessorStatus, nil,
                                                                    P) = 0 then
    Sys_DosGetProcessorStatus := TDosGetProcessorStatus (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32SetProcessorStatus, nil,
                                                                    P) = 0 then
    Sys_DosSetProcessorStatus := TDosSetProcessorStatus (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32QueryThreadAffinity, nil,
                                                                    P) = 0 then
    Sys_DosQueryThreadAffinity := TDosQueryThreadAffinity (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32SetThreadAffinity, nil,
                                                                    P) = 0 then
    Sys_DosSetThreadAffinity := TDosSetThreadAffinity (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32QueryExtLibPath, nil,
                                                                    P) = 0 then
    Sys_DosQueryExtLibPath := TDosQueryExtLibPath (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32SetExtLibPath, nil,
                                                                    P) = 0 then
    Sys_DosSetExtLibPath := TDosSetExtLibPath (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32QueryModFromEIP, nil,
                                                                    P) = 0 then
    Sys_DosQueryModFromEIP := TDosQueryModFromEIP (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32DumpProcess, nil, P) = 0 then
    Sys_DosDumpProcess := TDosDumpProcess (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32SuppressPopups, nil,
                                                                    P) = 0 then
    Sys_DosSuppressPopups := TDosSuppressPopups (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32PerfSysCall, nil, P) = 0 then
    Sys_DosPerfSysCall := TDosPerfSysCall (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32QueryThreadContext, nil,
                                                                    P) = 0 then
    Sys_DosQueryThreadContext := TDosQueryThreadContext (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32QueryABIOSSupport, nil,
                                                                    P) = 0 then
    Sys_DosQueryABIOSSupport := TDosQueryABIOSSupport (P);
end.

(*
Todo:
DosCreateSpinLock     = DOSCALLS.449 - might be simulated using semaphores on non-SMP
DosAcquireSpinLock    = DOSCALLS.450 - might be simulated using semaphores on non-SMP
DosReleaseSpinLock    = DOSCALLS.451 - might be simulated using semaphores on non-SMP
DosFreeSpinLock       = DOSCALLS.452 - might be simulated using semaphores on non-SMP

 DosQueryModFromEIP - may be simulated by returning empty value if not available or possibly by using data returned by DosQuerySysState (if they are equal across different OS/2 versions?)

___ function Dos16QueryModFromCS (...): ...
external 'DOSCALLS' index 359;


 DosVerifyPidTid - may be implemented by analyzing information returned by DosQuerySysState

x DosQueryExtLibPath - may be simulated by providing empty result if not available
x DosSetExtLibPath - may be simulated by returning ERROR_NOT_ENOUGH_MEMORY if not available

  Dos32AcquireSpinLock              | DOSCALLS | SMP   | SMP
  Dos32FreeSpinLock                 | DOSCALLS | SMP   | SMP
x  Dos32GetProcessorStatus           | DOSCALLS | SMP   | SMP
  Dos32ReleaseSpinLock              | DOSCALLS | SMP   | SMP
x  Dos32SetProcessorStatus           | DOSCALLS | SMP   | SMP
  Dos32TestPSD                      | DOSCALLS | SMP   | SMP

x  Dos32QueryThreadAffinity          | DOSCALLS | PROC  | 2.45
(x)  Dos32QueryThreadContext           | DOSCALLS | XCPT  | 2.40
x  Dos32SetThreadAffinity            | DOSCALLS | PROC  | 2.45

  Dos32AllocThreadLocalMemory       | DOSCALLS | PROC  | 2.30
  Dos32FreeThreadLocalMemory        | DOSCALLS | PROC  | 2.30
  Dos32ListIO                       | DOSCALLS | FILE  | 2.45
x  Dos32ProtectClose                 | DOSCALLS | FILE  | 2.10
x  Dos32ProtectEnumAttribute         | DOSCALLS | FILE  | 2.10
x  Dos32ProtectOpen                  | DOSCALLS | FILE  | 2.10
x  Dos32ProtectQueryFHState          | DOSCALLS | FILE  | 2.10
x  Dos32ProtectQueryFileInfo         | DOSCALLS | FILE  | 2.10
x  Dos32ProtectRead                  | DOSCALLS | FILE  | 2.10
x  Dos32ProtectSetFHState            | DOSCALLS | FILE  | 2.10
x  Dos32ProtectSetFileInfo           | DOSCALLS | FILE  | 2.10
x  Dos32ProtectSetFileLocks          | DOSCALLS | FILE  | 2.10
x  Dos32ProtectSetFilePtr            | DOSCALLS | FILE  | 2.10
x  Dos32ProtectSetFileSize           | DOSCALLS | FILE  | 2.10
x  Dos32ProtectWrite                 | DOSCALLS | FILE  | 2.10
  Dos32QueryABIOSSupport            | DOSCALLS | MOD   | 2.10
(x)  Dos32QueryModFromEIP              | DOSCALLS | MOD   | 2.10
(x)  Dos32SuppressPopUps               | DOSCALLS | MISC  | 2.10
  Dos32VerifyPidTid                 | DOSCALLS | MISC  | 2.30
*)
