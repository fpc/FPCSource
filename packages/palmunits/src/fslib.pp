{$MACRO ON}

{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: FSLib.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *              Sample file system library implementation.
 *
 * History:
 *    02/29/00 Created by Steve Minns
 *    10/27/00 CS    Include VFSMgr.h, since we depend on it.
 *
 *****************************************************************************)

(********************************************************************
 * Filename and Label conventions:
 *
 * All path names are absolute
 *
 * All filesystems must support filenames and labels that are up to 255 characters long,
 * using any normal character including spaces and lower case characters in any
 * character set and the following special characters:
 * $ % ' - _ @ ~ ` ! ( ) ^ # & + , ; = [ ]
 ********************************************************************
 * When creating the 8.3 name or label from a long filename or label:
 *  a) Create the name from the first 1-6 valid, non-space characters, before the last period.
 *    The only valid characters are:
 *       A-Z 0-9 $ % ' - _ @ ~ ` ! ( ) ^ # &
 *  b) the extension is the first three valid characters after the last period '.'
 *  c) the end of the 6 byte name is appended with ~1, or the next unique number.
 *
 * A label is created from the first 11 valid non-space characters.
 ********************************************************************)

unit fslib;

interface

uses palmos, libtraps, vfsmgr;

// When building the PalmOS 3.5 version of ExpansionMgr,
// since this constant was not in the 3.5 SystemResources.h...
const
  sysFileTFileSystem = RSC('libf'); // File type for file system libraries

  fsLibAPIVersion_ = $00000002;

(********************************************************************
 * Type of FS Library database
 ********************************************************************)

(********************************************************************
 * FS library function trap ID's. Each library call gets a trap number:
 *   FSTrapXXXX which serves as an index into the library's dispatch table.
 *   The constant sysLibTrapCustom is the first available trap number after
 *   the system predefined library traps Open,Close,Sleep & Wake.
 *
 * WARNING!!! The order of these traps MUST match the order of the dispatch
 *  table in FSLibDispatch.c!!!
 ********************************************************************)

const
  FSTrapLibAPIVersion      = sysLibTrapCustom;
  FSTrapCustomControl      = sysLibTrapCustom + 1;
  FSTrapFilesystemType     = sysLibTrapCustom + 2;

  FSTrapFileCreate         = sysLibTrapCustom + 3;
  FSTrapFileOpen           = sysLibTrapCustom + 4;
  FSTrapFileClose          = sysLibTrapCustom + 5;
  FSTrapFileRead           = sysLibTrapCustom + 6;
  FSTrapFileWrite          = sysLibTrapCustom + 7;
  FSTrapFileDelete         = sysLibTrapCustom + 8;
  FSTrapFileRename         = sysLibTrapCustom + 9;
  FSTrapFileSeek           = sysLibTrapCustom + 10;
  FSTrapFileEOF            = sysLibTrapCustom + 11;
  FSTrapFileTell           = sysLibTrapCustom + 12;
  FSTrapFileResize         = sysLibTrapCustom + 13;
  FSTrapFileGetAttributes  = sysLibTrapCustom + 14;
  FSTrapFileSetAttributes  = sysLibTrapCustom + 15;
  FSTrapFileGetDate        = sysLibTrapCustom + 16;
  FSTrapFileSetDate        = sysLibTrapCustom + 17;
  FSTrapFileSize           = sysLibTrapCustom + 18;

  FSTrapDirCreate          = sysLibTrapCustom + 19;
  FSTrapDirEntryEnumerate  = sysLibTrapCustom + 20;

  FSTrapVolumeFormat       = sysLibTrapCustom + 21;
  FSTrapVolumeMount        = sysLibTrapCustom + 22;
  FSTrapVolumeUnmount      = sysLibTrapCustom + 23;
  FSTrapVolumeInfo         = sysLibTrapCustom + 24;
  FSTrapVolumeGetLabel     = sysLibTrapCustom + 25;
  FSTrapVolumeSetLabel     = sysLibTrapCustom + 26;
  FSTrapVolumeSize         = sysLibTrapCustom + 27;

  FSMaxSelector            = FSTrapVolumeSize;

(********************************************************************
 * API Prototypes
 ********************************************************************)

(********************************************************************
 * Standard library open, close, sleep and wake APIs:
 ********************************************************************)

function FSLibOpen(fsLibRefNum: UInt16): Err; syscall sysLibTrapOpen;

function FSLibClose(fsLibRefNum: UInt16): Err; syscall sysLibTrapClose;

function FSLibSleep(fsLibRefNum: UInt16): Err; syscall sysLibTrapSleep;

function FSLibWake(fsLibRefNum: UInt16): Err; syscall sysLibTrapWake;

(********************************************************************
 * Custom library APIs:
 ********************************************************************)

function FSLibAPIVersion(fsLibRefNum: UInt16): UInt32; syscall FSTrapLibAPIVersion;

function FSCustomControl(fsLibRefNum: UInt16; apiCreator: UInt32; apiSelector: UInt16;
                         valueP: Pointer; var valueLenP: UInt16): Err; syscall FSTrapCustomControl;

function FSFilesystemType(fsLibRefNum: UInt16; var filesystemTypeP: UInt32): Err; syscall FSTrapFilesystemType;

(********************************************************************
 * File Stream APIs:
 ********************************************************************)

function FSFileCreate(fsLibRefNum: UInt16; volRefNum: UInt16; const pathNameP: PChar): Err; syscall FSTrapFileCreate;

function FSFileOpen(fsLibRefNum: UInt16; volRefNum: UInt16; const pathNameP: PChar;
                    openMode: UInt16; var fileRefP: FileRef): Err; syscall FSTrapFileOpen;

function FSFileClose(fsLibRefNum: UInt16; fileRef: FileRef): Err; syscall FSTrapFileClose;

function FSFileRead(fsLibRefNumUInt16: UInt16; fileRef: FileRef; numBytes: UInt32;
                    bufBaseP: Pointer; offset: UInt32; dataStoreBased: Boolean;
                    var numBytesReadP: UInt32): Err; syscall FSTrapFileRead;

function FSFileWrite(fsLibRefNum: UInt16; fileRef: FileRef; numBytes: UInt32;
                     const dataP: Pointer; var numBytesWrittenP: UInt32): Err; syscall FSTrapFileWrite;

function FSFileDelete(fsLibRefNum: UInt16; volRefNum: UInt16; const pathNameP: PChar): Err; syscall FSTrapFileDelete;

function FSFileRename(fsLibRefNum: UInt16; volRefNum: UInt16; const pathNameP: PChar; const newNameP: PChar): Err; syscall FSTrapFileRename;

function FSFileSeek(fsLibRefNum: UInt16; fileRef: FileRef; origin: FileOrigin; offset: Int32): Err; syscall FSTrapFileSeek;

function FSFileEOF(fsLibRefNum: UInt16; fileRef: FileRef): Err; syscall FSTrapFileEOF;

function FSFileTell(fsLibRefNum: UInt16; fileRef: FileRef; var filePosP: UInt32): Err; syscall FSTrapFileTell;

function FSFileResize(fsLibRefNum: UInt16; fileRef: FileRef; newSize: UInt32): Err; syscall FSTrapFileResize;

function FSFileGetAttributes(fsLibRefNum: UInt16; fileRef: FileRef; var attributesP: UInt32): Err; syscall FSTrapFileGetAttributes;

function FSFileSetAttributes(fsLibRefNum: UInt16; fileRef: FileRef; attributes: UInt32): Err; syscall FSTrapFileSetAttributes;

function FSFileGetDate(fsLibRefNum: UInt16; fileRef: FileRef; whichDate: UInt16; var dateP: UInt32): Err; syscall FSTrapFileGetDate;

function FSFileSetDate(fsLibRefNum: UInt16; fileRef: FileRef; whichDate: UInt16; date: UInt32): Err; syscall FSTrapFileSetDate;

function FSFileSize(fsLibRefNum: UInt16; fileRef: FileRef; var fileSizeP: UInt32): Err; syscall FSTrapFileSize;

(********************************************************************
 * Directory APIs:
 ********************************************************************)

function FSDirCreate(fsLibRefNum: UInt16; volRefNum: UInt16; const dirNameP: PChar): Err; syscall FSTrapDirCreate;

(************************************************************
 *
 *  MACRO:        FSDirDelete
 *
 *  DESCRIPTION:  Delete a closed directory.
 *
 *  PARAMETERS:   fsLibRefNum          -- FS library reference number
 *          volRefNum            -- Volume reference number returned by FSVolumeMount
 *          pathNameP            -- Full path of the directory to be deleted
 *
 *  RETURNS:   errNone              -- no error
 *          expErrNotOpen        -- FS driver library has not been opened
 *          vfsErrFileStillOpen     -- Directory is still open
 *          vfsErrFileNotFound      -- the file could not be found
 *          vfsErrVolumeBadRef      -- the volume has not been mounted with FSVolumeMount
 *
 *************************************************************)

function FSDirDelete(fsLibRefNum: UInt16; volRefNum: UInt16; const dirNameP: PChar): Err;

function FSDirEntryEnumerate(fsLibRefNum: UInt16; dirRef: FileRef; var dirEntryIteratorP: UInt32; var infoP: FileInfoType): Err; syscall FSTrapDirEntryEnumerate;

(********************************************************************
 * Volume APIs:
 ********************************************************************)

function FSVolumeFormat(fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err; syscall FSTrapVolumeFormat;

function FSVolumeMount(fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err; syscall FSTrapVolumeMount;

function FSVolumeUnmount(fsLibRefNum: UInt16; volRefNum: UInt16): Err; syscall FSTrapVolumeUnmount;

function FSVolumeInfo(fsLibRefNum: UInt16; volRefNum: UInt16; var volInfoP: VolumeInfoType): Err; syscall FSTrapVolumeInfo;

function FSVolumeGetLabel(fsLibRefNum: UInt16; volRefNum: UInt16; labelP: PChar; bufLen: UInt16): Err; syscall FSTrapVolumeGetLabel;

function FSVolumeSetLabel(fsLibRefNum: UInt16; volRefNum: UInt16; const labelP: PChar): Err; syscall FSTrapVolumeSetLabel;

function FSVolumeSize(fsLibRefNum: UInt16; volRefNum: UInt16; var volumeUsedP: UInt32; var volumeTotalP: UInt32): Err; syscall FSTrapVolumeSize;

implementation

function FSDirDelete(fsLibRefNum: UInt16; volRefNum: UInt16; const dirNameP: PChar): Err;
begin
  FSDirDelete := FSFileDelete(fsLibRefNum, volRefNum, dirNameP);
end;

end.
