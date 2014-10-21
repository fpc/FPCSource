{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by the Free Pascal development team.

    Additional 64-bit OS/2 API functions for file handling
    implemented in DOSCALL1.DLL - functions supported in WSeB/MCP/eCS
    or their fake (simulated) implementation for lower OS/2
    versions (real availability checked during initialization).

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
  DosCalls;

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
type
  TDosCancelLockRequestL = function (Handle: THandle; var Lock: TFileLockL):
                                                               cardinal; cdecl;
function DummyDosCancelLockRequestL (Handle: THandle;
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
type
  TDosProtectSetFileLocksL = function (Handle: THandle; var Unlock: TFileLockL;
                   var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

function DummyDosProtectSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
                   var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal;
                                  FileHandleLockID: cardinal): cardinal; cdecl;

(*
DosSetFileLocksL locks and unlocks a range of an open file. 

Parameters 

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

DosSetFileLocksL allows a process to lock and unlock a range in a file. The time during which a file range is locked should be short. 

If the lock and unlock ranges are both zero, ERROR_LOCK_VIOLATION is returned to the caller. 

If you only want to lock a file range, set the unlock file offset and the unlock range length to zero. 

If you only want to unlock a file range, set the lock file offset and the lock range length to zero. 

When the Atomic bit of flags is set to 0, and DosSetFileLocksL specifies a lock operation and an unlock operation, the unlock operation occurs first, and then the lock operation is performed. If an error occurs during the unlock operation, an error code is returned and the lock operation is not performed. If an error occurs during the lock operation, an error code is returned and the unlock remains in effect if it was successful. 

The lock operation is atomic when all of these conditions are met   

 The Atomic bit is set to 1 in flags 

 The unlock range is the same as the lock range 

 The process has shared access to the file range, and has requested exclusive access to it; or the process has exclusive access to the file range, and has requested shared access to it. 

Some file system drivers (FSDs) may not support atomic lock operations. Versions of the operating system prior to OS/2 Version 2.00 do not support atomic lock operations. If the application receives the error code ERROR_ATOMIC_LOCK_NOT_SUPPORTED, the application should unlock the file range and then lock it using a non-atomic operation (with the atomic bit set to 0 in flags). The application should also refresh its internal buffers before making any changes to the file. 

If you issue DosClose to close a file with locks still in effect, the locks are released in no defined sequence. 

If you end a process with a file open, and you have locks in effect in that file, the file is closed and the locks are released in no defined sequence. 

The locked range can be anywhere in the logical file. Locking beyond the end of the file is not an error. A file range to be locked exclusively must first be cleared of any locked file subranges or overlapping locked file ranges. 

If you repeat DosSetFileLocksL for the same file handle and file range, then you duplicate access to the file range. Access to locked file ranges is not duplicated across DosExecPgm. The proper method of using locks is to attempt to lock the file range, and to examine the return value. 

The following table shows the level of access granted when the accessed file range is locked with an exclusive lock or a shared lock.  Owner  refers to a process that owns the lock.  Non-owner  refers to a process that does not own the lock. 


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

If only locking is specified, DosSetFileLocksL locks the specified file range using pflLock. If the lock operation cannot be accomplished, an error is returned, and the file range is not locked. 

After the lock request is processed, a file range can be unlocked using the Unlock parameter of another DosSetFileLocksL request. If unlocking cannot be accomplished, an error is returned. 

Instead of denying read/write access to an entire file by specifying access and sharing modes with DosOpenL requests, a process attempts to lock only the range needed for read/write access and examines the error code returned. 

Once a specified file range is locked exclusively, read and write access by another process is denied until the file range is unlocked. If both unlocking and locking are specified by DosSetFileLocksL, the unlocking operation is performed first, then locking is done.
*)
type
  TDosSetFileLocksL = function (Handle: THandle; var Unlock: TFileLockL;
    var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal): cardinal; cdecl;
function DummyDosSetFileLocksL (Handle: THandle; var Unlock: TFileLockL;
 var Lock: TFileLockL; Timeout: cardinal; Flags: cardinal): cardinal; cdecl;

(*
DosProtectOpenL opens a new file, an existing file, or a replacement for an existing file and returns a protected file handle. An open file can have extended attributes. 

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
1 FILE_HIDDEN (0x00000002) - file is hidden and does not appear in a directory
                             listing.
0 FILE_READONLY (0x00000001) - file can be read from, but not written to.
0 FILE_NORMAL (0x00000000) - file can be read from or written to.

File attributes apply only if the file is created. These bits may be set
individually or in combination. For example, an attribute value of 0x00000021
(bits 5 and 0 set to 1) indicates a read-only file that has been archived.


 OpenFlags = the action to be taken depending on whether the file exists or does not exist. 

This parameter contains the following bit fields   

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


 OpenMode = the mode of the open function.

This parameter contains the following bit fields   

Bits Description 

31 - reserved, must be zero. 

30 OPEN_FLAGS_PROTECTED_HANDLE (0x40000000) - protected file handle flag.
  0 - unprotected Handle
  1 - protected Handle

Protected handle requires the FileHandleLockID to be specified on subsequent
DosProtectxxxx calls.

Unprotected handle requires the FileHandleLockID value to be specified as zero
on subsequent DosProtectxxxx calls. An unprotected handle may be used with the
unprotected calls such as DosRead and DosWrite.

29 OPEN_SHARE_DENYLEGACY (0x10000000)
Deny read/write access by the DosOpen command
  0 - allow read/write access by the DosOpen command.
  1 - deny read/write access by the DosOpen command.

  A file opened by DosOpenL will not be allowed to grow larger than 2GB while that same file is open via a legacy DosOpen call. Setting this bit to 1 will prevent access by the obsolete DosOpen API and ensure that no error will occur when growing the file.

28..16 - reserved, must be zero.

15 OPEN_FLAGS_DASD (0x00008000)
Direct Open flag   
  0 - FileName represents a file to be opened normally. 
  1 - FileName is drive (such as C or A), and represents a mounted disk or diskette volume to be opened for direct access.

14 OPEN_FLAGS_WRITE_THROUGH (0x00004000) 
Write-Through flag   
  0 - writes to the file may go through the file-system driver's cache; the file-system driver writes the sectors when the cache is full or the file is closed.
  1 - writes to the file may go through the file-system driver's cache, but the sectors are written (the actual file I/O operation is completed) before a synchronous write call returns. This state of the file defines it as a synchronous file. For synchronous files, this bit must be set, because the data must be written to the medium for synchronous write operations.

  This bit flag is not inherited by child processes. 

13 OPEN_FLAGS_FAIL_ON_ERROR (0x00002000) 
Fail-Errors flag. Media I O errors are handled as follows   
  0 - reported through the system critical-error handler.
  1 - reported directly to the caller by way of a return code.

  Media I/O errors generated through Category 08h Logical Disk Control IOCtl Commands always get reported directly to the caller by way of return code. The Fail-Errors function applies only to non-IOCtl handle-based file I/O calls.

  This flag bit is not inherited by child processes. 

12 OPEN_FLAGS_NO_CACHE (0x00001000) 
No-Cache/Cache flag   
 0 - the file-system driver should place data from I/O operations into its cache.
 1 - I/O operations to the file need not be done through the file-system driver's cache.

  The setting of this bit determines whether file-system drivers should place data into the cache. Like the write-through bit, this is a per-handle bit, and is not inherited by child processes. 

11 - reserved; must be 0.

10..8 - the locality of reference flags contain information about how the application is to get access to the file. The values are as follows:

000 OPEN_FLAGS_NO_LOCALITY (0x00000000)
No locality known.

001 OPEN_FLAGS_SEQUENTIAL (0x00000100)
Mainly sequential access.

010 OPEN_FLAGS_RANDOM (0x00000200)
Mainly random access.

011 OPEN_FLAGS_RANDOMSEQUENTIAL (0x00000300)
Random with some locality.

7 OPEN_FLAGS_NOINHERIT (0x00000080)
Inheritance flag

  0 - file handle is inherited by a process created from a call to DosExecPgm.
  1 - file handle is private to the current process.

  This bit is not inherited by child processes. 

6..4 Sharing Mode flags; this field defines any restrictions to file access placed by the caller on other processes. The values are as follows:

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

2 0 Access-Mode flags. This field defines the file access required by the caller. The values are as follows   

000 OPEN_ACCESS_READONLY (0x00000000) 

Read-only access 

001 OPEN_ACCESS_WRITEONLY (0x00000001) 

Write-only access 

010 OPEN_ACCESS_READWRITE (0x00000002) 

Read/write access. 


Any other value is invalid, as are any other combinations.   

File sharing requires the cooperation of sharing processes. This cooperation is communicated through sharing and access modes. Any sharing restrictions placed on a file opened by a process are removed when the process closes the file with a DosClose request. 

Sharing Mode 
Specifies the type of file access that other processes may have. For example, if other processes can continue to read the file while your process is operating on it, specify Deny Write. The sharing mode prevents other processes from writing to the file but still allows them to read it. 

Access Mode 
Specifies the type of file access (access mode) needed by your process. For example, if your process requires read/write access, and another process has already opened the file with a sharing mode of Deny None, your DosProtectOpenL request succeeds. However, if the file is open with a sharing mode of Deny Write, the process is denied access. 

If the file is inherited by a child process, all sharing and access restrictions also are inherited. 

If an open file handle is duplicated by a call to DosDupHandle, all sharing and access restrictions also are duplicated. 

 EA = pointer to an extended attribute buffer.

Input The address of the extended-attribute buffer, which contains an EAOP2 structure. The fpFEA2List field in the EAOP2 structure points to a data area where the relevant FEA2 list is to be found. The fpGEA2List and oError fields are ignored.

Output fpGEA2List and fpFEA2List are unchanged. The area that fpFEA2List points to is unchanged. If an error occurred during the set, oError is the offset of the FEA2 entry where the error occurred. The return code from DosProtectOpenL is the error code for that error condition. If no error occurred, oError is undefined.


 EA is nil, then no extended attributes are defined for the file. If extended attributes are not to be defined or modified, the pointer peaop2 must be set to zero. 

 FileHandleLockID = 32-bit lockid for the file handle is returned here

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

A successful DosProtectOpenL request returns a handle and a 32-bit lockid for accessing the file. The read/write pointer is set at the first byte of the file. The position of the pointer can be changed with DosProtectSetFilePtrL or by read and write operations on the file. 

The file s date and time can be queried with DosProtectQueryFileInfo. They are set with DosProtectSetFileInfo. 

The read-only attribute of a file can be set with the ATTRIB command. 

ulAttribute cannot be set to Volume Label. To set volume-label information, issue DosProtectSetFileInfo with a logical drive number.  Volume labels cannot be opened. 

cbFile affects the size of the file only when the file is new or is a replacement. If an existing file is opened, cbFile is ignored. To change the size of the existing file, issue DosProtectSetFileSizeL. 

The value in cbFile is a recommended size. If the full size cannot be allocated, the open request may still succeed.  The file system makes a reasonable attempt to allocate the new size in an area that is as nearly contiguous as possible on the medium. When the file size is extended, the values of the new bytes are undefined. 

The Direct Open bit provides direct access to an entire disk or diskette volume, independent of the file system. This mode of opening the volume that is currently on the drive returns a handle to the calling function; the handle represents the logical volume as a single file. The calling function specifies this handle with a DosDevIOCtl Category 8, DSK_LOCKDRIVE request to prevent other processes from accessing the logical volume. When you are finished using the logical volume, issue a DosDevIOCtl Category 8, DSK_UNLOCKDRIVE request to allow other processes to access the logical volume. 

The file-handle state bits can be set by DosProtectOpenL and DosProtectSetFHState. An application can query the file-handle state bits, as well as the rest of the Open Mode field, by issuing DosProtectQueryFHState. 

You can use an EAOP2 structure to set extended attributes in peaop2 when creating a file, replacing an existing file, or truncating an existing file. No extended attributes are set when an existing file is just opened. 

A replacement operation is logically equivalent to atomically deleting and re-creating the file. This means that any extended attributes associated with the file also are deleted before the file is re-created. 

The pfhFileHandleLockID returned is required on each of the DosProtectxxx functions. An incorrect pfhFileHandleLockID on subsequent DosProtectxxx calls results in an ERROR_ACCESS_DENIED return code. 

The DosProtectxxx functions can be used with a NULL filehandle lockid, if the subject filehandle was obtained from DosOpen.
*)
type
  TDosProtectOpenL = function (FileName: PChar; var Handle: THandle;
                         var Action: cardinal; InitSize: int64; Attrib,
                         OpenFlags, OpenMode: cardinal; EA: PEAOp2;
                              var FileHandleLockID: cardinal): cardinal; cdecl;
function DummyDosProtectOpenL (FileName: PChar; var Handle: THandle;
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
type
  TDosProtectSetFilePtrL = function (Handle: THandle; Pos: int64;
          Method: cardinal;
            var PosActual: int64; FileHandleLockID: cardinal): cardinal; cdecl;
function DummyDosProtectSetFilePtrL (Handle: THandle; Pos: int64;
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
type
  TDosProtectSetFileSizeL = function (Handle: THandle; Size: int64;
                                  FileHandleLockID: cardinal): cardinal; cdecl;
function DummyDosProtectSetFileSizeL (Handle: THandle; Size: int64;
                                  FileHandleLockID: cardinal): cardinal; cdecl;


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


{***************************************************************************}
implementation
{***************************************************************************}

uses
  OS2Def;

(*
 DosCreateThread2
*)

(*
DosDumpProcess initiates a process dump from a specified process. This may be used as part of an error handling routine to gather information about an error that may be analyzed later using the OS/2 System Dump Formatter. Configuration of Process Dump may be done using the PDUMPSYS, PDUMPUSR, and PROCDUMP commands. 


APIRET APIENTRY DosDumpProcess (ULONG Flag, ULONG Drive, PID Pid) 

Parameters 

flag (ULONG)   input 
Flags specify the function to be performed   

DDP_DISABLEPROCDUMP 0x00000000L Disable process dumps. 

DDP_ENABLEPROCDUMP 0x00000001L Enable process dumps. 

DDP_PERFORMPROCDUMP 0x00000002L Perform process dump. 

drive (ULONG)   input 
The ASCII character for the drive on which process dump files are to be created. This is required only with the DDP_ENABLEPROCDUMP. 

Note:  Use the PROCDUMP command to customize fully the drive and path. 

pid (PID)   input 
The process to be dumped. 0L specified the current process; otherwise a valid process ID must be specified. 

Note:  Use the PDUMPUSR command to specify what information will be dumped. Use the PROCDUMP command to customize options per process and in particular to specify whether child or parent process will be dumped. This parameter is actioned only with DDP_PERFORMPROCDUMP. 

Returns 

ulrc (APIRET)   returns 
Return Code. 

DosDumpProcess returns the following value   

87 ERROR_INVALID_PARAMETER

Remarks 

For maximum flexibility the use of DosDumpProcess should be limited to the DDP_PERFORMPROCDUMP function. This allows you to specify whether Process Dump should be enabled through the use of the PROCDUMP command. You may customize Process Dump completely through use of the PDUMPUSR, PDUMPSYS, AND PROCDUMP commands. For further information, see PROCDUMP.DOC in the OS2\SYSTEM\RAS directory. DDP_ENABLEPROCDUMP and DDP_DISABLEPROCDUMP are provided for backwards compatibility only. 

 DosDumpProcess
*)

(*
 DosForceSystemDump

function DosGetProcessorStatus (...): cardinal; cdecl;
external 'DOSCALLS' index 447;

function DosQueryPageUsage (...): cardinal; cdecl;
external 'DOSCALLS' index 358;
*)
(*
DosSetProcessorStatus = DOSCALLS.448
DosCreateSpinLock     = DOSCALLS.449
DosAcquireSpinLock    = DOSCALLS.450
DosReleaseSpinLock    = DOSCALLS.451
DosFreeSpinLock       = DOSCALLS.452
 DosListIO
 DosListIOL
 DosPerfSystemCall
 DosQueryABIOSSuport
function DosQueryMemState (...): cardinal; cdecl;
external 'DOSCALLS' index 307;

___ function Dos16QueryModFromCS (...): ...
external 'DOSCALLS' index 359;

 DosQueryModFromEIP
*)
(*
 The following API calls are made available through unit System:

 DosOpenL = DOSCALLS.981
 DosSetFilePtrL = DOSCALLS.988
 DosSetFileSizeL = DOSCALLS.989
*)
(* Todo:
WSeB/eCS APIs:
 Creates a private Read/Write alias or an LDT code segment alias to part
 of an existing memory object. The alias object is accessible only to the
 process that created it. The original object must be accessible to the caller
 of DosAliasMem.

 An alias is removed by calling DosFreeMem with the alias address.

 Although it is possible to create a Read/Write alias to a code segment
 to allow code modification, this is not recommended. On Pentium processors,
 and later, pipe-lining techniques used by the processor might allow
 the processor not to be aware of the modified code, if appropriate
 pipe-line serialization is not performed by the programmer. For further
 information see the processor documentation.

 Possible return values:
     0 No_Error
     8 Error_Not_Enough_Memory
    87 Error_Invalid_Parameter
    95 Error_Interrupt
 32798 Error_Crosses_Object_Boundary

pMem   = Pointer to the memory to be aliased. It must be on a page boundary
         (i.e. aligned to 4 kB), but may specify an address within a memory
         object.
Size   = Specifies size in bytes for the memory to alias. The entire range
         must lie within a single memory object and must be committed
         if OBJ_SELMAPALL is specified.
Alias  = Pointer where the address of the aliased memory is returned.
         The corresponding LDT selector is not explicitly returned but may be
         calculated by using the Compatibility Mapping Algorithm
         ((Alias shr 13) or 7).
Flags  = Combination of the following values:
            obj_SelMapAll = $800 (Create a Read/Write 32 bit alias
                                  to the address specified. The entire range
                                  must be committed, start on page boundary
                                  and be within the extent of a single memory
                                  object. An LDT selector is created to map
                                  the entire range specified. If obj_SelMapAll
                                  is not specified, then size is rounded up
                                  to a 4K multiple and the alias created
                                  inherits the permissions from the pages
                                  of the original object.)
            obj_Tile      =  $40  (Obj_Tile may be specified, but currently
                                   this is enforced whether or not specified.
                                   This forces LDT selectors to be based
                                   on 64K boundaries.)
            sel_Code      =    1  (Marks the LDT alias selector(s)
                                   Read-Executable code selectors.)
            sel_Use32     =    2  (Used with obj_SelMapAll, otherwise ignored.
                                   Marks the first alias LDT selector
                                   as a 32 bit selector by setting the BIG/C32
                                   bit.)
function DosAliasMem (pMem: pointer; Size: cardinal; var Alias: pointer; Flags: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 298;
*)

(*
 DosQueryThreadAffinity
 DosSetThreadAffinity
 Dos16SysTrace
 DosVerifyPidTid
*)


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
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectOpenL, nil, P) = 0 then
    Sys_DosProtectOpenL := TDosProtectOpenL (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFilePtrL, nil, P)
                                                                       = 0 then
    Sys_DosProtectSetFilePtrL := TDosProtectSetFilePtrL (P);
   if DosQueryProcAddr (DosCallsHandle, Ord_Dos32ProtectSetFileSizeL, nil, P)
                                                                       = 0 then
    Sys_DosProtectSetFileSizeL := TDosProtectSetFileSizeL (P);
  end;
end.
