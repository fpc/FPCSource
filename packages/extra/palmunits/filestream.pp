{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: FileStream.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Pilot File Stream equates -- File Streams were initially implemented
 *    in PalmOS v3.0 (not available in earlier versions)
 *
 * History:
 *    11/24/97 vmk      - Created by Vitaly Kruglikov
 *
 *****************************************************************************)

unit filestream;

interface

uses palmos, coretraps, errorbase;

(************************************************************
 * File Stream error codes
 * the constant dmErrorClass is defined in ErrorBase.h
 *************************************************************)

const
  fileErrMemError            = fileErrorClass or 1;  // out of memory error
  fileErrInvalidParam        = fileErrorClass or 2;  // invalid parameter value passed
  fileErrCorruptFile         = fileErrorClass or 3;  // the file is corrupted/invalid/not a stream file
  fileErrNotFound            = fileErrorClass or 4;  // couldn't find the file
  fileErrTypeCreatorMismatch = fileErrorClass or 5;  // file's type and creator didn't match those expected
  fileErrReplaceError        = fileErrorClass or 6;  // couldn't replace an existing file
  fileErrCreateError         = fileErrorClass or 7;  // couldn't create a new file
  fileErrOpenError           = fileErrorClass or 8;  // generic open error
  fileErrInUse               = fileErrorClass or 9;  // file couldn't be opened or deleted because it is in use
  fileErrReadOnly            = fileErrorClass or 10; // couldn't open in write mode because db is read-only
  fileErrInvalidDescriptor   = fileErrorClass or 11; // invalid file descriptor (FileHandle)
  fileErrCloseError          = fileErrorClass or 12; // error closing the database
  fileErrOutOfBounds         = fileErrorClass or 13; // attempted operation went out of bounds of the file
  fileErrPermissionDenied    = fileErrorClass or 14; // couldn't write to a file open for read-only access
  fileErrIOError             = fileErrorClass or 15; // general I/O error
  fileErrEOF                 = fileErrorClass or 16; // end-of-file error
  fileErrNotStream           = fileErrorClass or 17; // attempted to open a file that is not a stream

(************************************************************
 * File Stream handle type
 *************************************************************)

type
  FileHand = MemHandle;

const
  fileNullHandle = FileHand(0);

(************************************************************
 * Mode flags passed to FileOpen
 *************************************************************)

// fileModeReadOnly, fileModeReadWrite, fileModeUpdate, and fileModeAppend are mutually exclusive - only
// pass one of them to FileOpen!
const
  fileModeReadOnly       = $80000000; // open for read access
  fileModeReadWrite      = $40000000; // create for read/write access, discarding previous if any */
  fileModeUpdate         = $20000000; // open/create for read/write, preserving previous if any
  fileModeAppend         = $10000000; // open/create for read/write, always writing at the end

  fileModeLeaveOpen      = $08000000; // leave open when app quits
  fileModeExclusive      = $04000000; // don't let anyone else open it
  fileModeAnyTypeCreator = $02000000; // if set, skip type/creator validation when
                                      // opening or replacing an existing file

  fileModeTemporary      = $01000000; // will automatically delete the file when it is closed;
                                      // if this bit is set and the file type passed to FileOpen is zero,
                                      // FileOpen will use sysFileTTemp (defined in SystemResources.h for the file
                                      // type (recommended) - this will enable automatic cleanup of undeleted
                                      // temp files following a system crash in future PalmOS versions
                                      // (post-crash cleanup will likely come after 3.0)

  fileModeDontOverwrite  = $00800000; // if set, will prevent fileModeReadWrite from discarding an existing file
                                      // with the same name; may only be specified together with fileModeReadWrite

// For debugging/validation
const
  fileModeAllFlags = fileModeReadOnly or fileModeReadWrite or fileModeUpdate or
                     fileModeAppend or fileModeLeaveOpen or fileModeExclusive or
                     fileModeAnyTypeCreator or fileModeTemporary or fileModeDontOverwrite;

(************************************************************
 * Origin passed to FileSetPos
 *************************************************************)

type
  FileOriginEnum = Enum;

const
  fileOriginBeginning  = 1;                      // from the beginning (first data byte of file)
  fileOriginCurrent = Succ(fileOriginBeginning); // from the current position
  fileOriginEnd = Succ(fileOriginCurrent);       // from the end of file (one position beyond last data byte)

(************************************************************
 * Operation passed to FileControl
 *************************************************************)

type
  FileOpEnum = Enum;

const
  fileOpNone = 0;                                        // no-op

  fileOpDestructiveReadMode = Succ(fileOpNone);          // switch to destructive read mode (there is no turning back);
                                                         // implicitly rewinds the file to the beginning;
                                                         // destructive read mode deletes file stream data blocks as
                                                         // data is being read, thus freeing up storage automatically;
                                                         // once in destructive read mode, FileWrite, FileSeek and FileTruncate
                                                         // are not allowed; stream's contents after closing (or crash)
                                                         // are undefined.
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = NULL
                                                         //    valueLenP = NULL
                                                         // RETURNS:
                                                         //    zero on success; fileErr... on error

  fileOpGetEOFStatus = Succ(fileOpDestructiveReadMode);  // get end-of-file status (err = fileErrEOF indicates end of file condition);
                                                         // use FileClearerr to clear this error status
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = NULL
                                                         //    valueLenP = NULL
                                                         // RETURNS:
                                                         //    zero if _not_ end of file; non-zero if end of file

  fileOpGetLastError = Succ(fileOpGetEOFStatus);         // get error code from last operation on file stream, and
                                                         // clear the last error code value (will not change end of file
                                                         // or I/O error status -- use FileClearerr to reset all error codes)
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = NULL
                                                         //    valueLenP = NULL
                                                         // RETURNS:
                                                         //    Error code from last file stream operation

  fileOpClearError = Succ(fileOpGetLastError);           // clear I/O and end of file error status, and last error
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = NULL
                                                         //    valueLenP = NULL
                                                         // RETURNS:
                                                         //    zero on success; fileErr... on error

  fileOpGetIOErrorStatus = Succ(fileOpClearError);       // get I/O error status (like C runtime's ferror); use FileClearerr
                                                         // to clear this error status
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = NULL
                                                         //    valueLenP = NULL
                                                         // RETURNS:
                                                         //    zero if _not_ I/O error; non-zero if I/O error is pending

  fileOpGetCreatedStatus = Succ(fileOpGetIOErrorStatus); // find out whether the FileOpen call caused the file to
                                                         // be created
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = ptr to Boolean type variable
                                                         //    valueLenP = ptr to Int32 variable set to sizeof(Boolean)
                                                         // RETURNS:
                                                         //    zero on success; fileErr... on error;
                                                         //    the Boolean variable will be set to non zero if the file was created.

  fileOpGetOpenDbRef = Succ(fileOpGetCreatedStatus);     // get the open database reference (handle) of the underlying
                                                         // database that implements the stream (NULL if none);
                                                         // this is needed for performing PalmOS-specific operations on
                                                         // the underlying database, such as changing or getting creator/type,
                                                         // version, backup/reset bits, etc.
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = ptr to DmOpenRef type variable
                                                         //    valueLenP = ptr to Int32 variable set to sizeof(DmOpenRef)
                                                         // RETURNS:
                                                         //    zero on success; fileErr... on error;
                                                         //    the DmOpenRef variable will be set to the file's open db reference
                                                         //    that may be passed to Data Manager calls;
                                                         // WARNING:
                                                         //    Do not make any changes to the data of the underlying database --
                                                         //    this will cause the file stream to become corrupted.

  fileOpFlush = Succ(fileOpGetOpenDbRef);                // flush any cached data to storage
                                                         // ARGUMENTS:
                                                         //    stream = open stream handle
                                                         //    valueP = NULL
                                                         //    valueLenP = NULL
                                                         // RETURNS:
                                                         //    zero on success; fileErr... on error;

  fileOpLAST = Succ(fileOpFlush);                        // ***ADD NEW OPERATIONS BEFORE THIS ENTRY***
                                                         // ***  AND ALWAYS AFTER EXISTING ENTRIES ***
                                                         // ***     FOR BACKWARD COMPATIBILITY     ***

(************************************************************
 * File Stream procedures
 *************************************************************)

// Open/create a file stream (name must all be valid -- non-null, non-empty)
// (errP is optional - set to NULL to ignore)
function FileOpen(cardNo: UInt16; const nameP: PChar; type_, creator, openMode: UInt32; var errP: Err): FileHand; syscall sysTrapFileOpen;

// Close the file stream
function FileClose(stream: FileHand): Err; syscall sysTrapFileClose;

// Delete a file
function FileDelete(cardNo: UInt16; const nameP: PChar): Err; syscall sysTrapFileDelete;

(***********************************************************************
 *
 * MACRO:      FileRead
 *
 * DESCRIPTION:   Read data from a file into a buffer.  If you need to read into a data storage
 *                heap-based chunk, record or resource, you _must_ use FileDmRead instead.
 *
 * PROTOTYPE:  Int32 FileRead(FileHand stream, void *bufP, Int32 objSize, Int32 numObj, Err *errP)
 *
 * PARAMETERS: stream      -- handle of open file
 *             bufP        -- buffer for reading data
 *             objSize     -- size of each object to read
 *             numObj      -- number of objects to read
 *             errP        -- ptr to variable for returning the error code (fileErr...)
 *                            (OPTIONAL -- pass NULL to ignore)
 *
 * RETURNED:   the number of objects that were read - this may be less than
 *             the number of objects requested
 *
 ***********************************************************************)

function FileRead(stream: FileHand; bufP: Pointer; objSize, numObj: Int32; var errP: Err): Int32;

(***********************************************************************
 *
 * MACRO:      FileDmRead
 *
 * DESCRIPTION:   Read data from a file into a data storage heap-based chunk, record
 *                or resource.
 *
 * PROTOTYPE:  Int32 FileDmRead(FileHand stream, void *startOfDmChunkP, Int32 destOffset,
 *                   Int32 objSize, Int32 numObj, Err *errP)
 *
 * PARAMETERS: stream      -- handle of open file
 *             startOfDmChunkP
 *                         -- ptr to beginning of data storage heap-based chunk, record or resource
 *             destOffset  -- offset from base ptr to the destination area (must be >= 0)
 *             objSize     -- size of each object to read
 *             numObj      -- number of objects to read
 *             errP        -- ptr to variable for returning the error code (fileErr...)
 *                            (OPTIONAL -- pass NULL to ignore)
 *
 * RETURNED:   the number of objects that were read - this may be less than
 *             the number of objects requested
 *
 ***********************************************************************)

function FileDmRead(stream: FileHand; startOfDmChunkP: Pointer; destOffset: Int32; objSize, numObj: Int32; var errP: Err): Int32;

// Low-level routine for reading data from a file stream -- use helper macros FileRead and FileDmRead
// instead of calling this function directly;
// (errP is optional - set to NULL to ignore)
function FileReadLow(stream: FileHand; baseP: Pointer; offset: Int32; dataStoreBased: Boolean;
                     objSize, numObj: Int32; var errP: Err): Int32; syscall sysTrapFileReadLow;

// Write data to a file stream
// (errP is optional - set to NULL to ignore)
function FileWrite(stream: FileHand; const dataP: Pointer; objSize, numObj: Int32; var errP: Err): Int32; syscall sysTrapFileWrite;

// Set position within a file stream
function FileSeek(stream: FileHand; offset: Int32; origin: FileOriginEnum): Err; syscall sysTrapFileSeek;

function FileRewind(stream: FileHand): Err;

// Get current position and filesize
// (fileSizeP and errP are optional - set to NULL to ignore)
function FileTell(stream: FileHand; var fileSizeP: Int32; var errP: Err): Int32; syscall sysTrapFileTell;

// Truncate a file
function FileTruncate(stream: FileHand; newSize: Int32): Err; syscall sysTrapFileTruncate;

// Returns the error code from the last operation on this file stream;
// if resetLastError is non-zero, resets the error status
// function FileControl(op: FileOpEnum; stream: FileHand; valueP: Pointer; var valueLenP: Int32): Err; syscall sysTrapFileControl;
function FileControl(op: FileOpEnum; stream: FileHand; valueP: Pointer; valueLenP: Pointer): Err; syscall sysTrapFileControl;

function FileEOF(stream: FileHand): Boolean;

function FileError(stream: FileHand): Err;

function FileClearerr(stream: FileHand): Err;

function FileGetLastError(stream: FileHand): Err;

function FileFlush(stream: FileHand): Err;

implementation

function FileRead(stream: FileHand; bufP: Pointer; objSize, numObj: Int32; var errP: Err): Int32;
begin
  FileRead := FileReadLow(stream, bufP, 0{offset}, False{dataStoreBased}, objSize, numObj, errP);
end;

function FileDmRead(stream: FileHand; startOfDmChunkP: Pointer; destOffset: Int32; objSize, numObj: Int32; var errP: Err): Int32;
begin
  FileDmRead := FileReadLow(stream, startOfDmChunkP, destOffset, True{dataStoreBased}, objSize, numObj, errP);
end;

function FileRewind(stream: FileHand): Err;
begin
  FileClearerr(stream);
  FileRewind := FileSeek(stream, 0, fileOriginBeginning);
end;

function FileEOF(stream: FileHand): Boolean;
begin
  FileEOF := FileControl(fileOpGetEOFStatus, stream, nil, Longint(nil)) = fileErrEOF;
end;

function FileError(stream: FileHand): Err;
begin
  FileError := FileControl(fileOpGetIOErrorStatus, stream, nil, Longint(nil));
end;

function FileClearerr(stream: FileHand): Err;
begin
  FileClearerr := FileControl(fileOpClearError, stream, nil, Longint(nil));
end;

function FileGetLastError(stream: FileHand): Err;
begin
  FileGetLastError := FileControl(fileOpGetLastError, stream, nil, Longint(nil));
end;

function FileFlush(stream: FileHand): Err;
begin
  FileFlush := FileControl(fileOpFlush, stream, nil, Longint(nil));
end;

end.
