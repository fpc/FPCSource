{$MACRO ON}
{$define Rsc := }
(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: VFSMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header file for VFS Manager.
 *
 * History:
 *    02/25/00 jed   Created by Jesse Donaldson.
 *
 *****************************************************************************)

unit vfsmgr;

interface

uses palmos, coretraps, errorbase, datamgr, systemresources;

const
  sysTrapVFSMgr = sysTrapFileSystemDispatch;

  vfsFtrIDVersion = 0; // ID of feature containing version of VFSMgr.
                       // Check existence of this feature to see if VFSMgr is installed.

  vfsFtrIDDefaultFS = 1; // ID of feature containing the creator ID of the default filesystem library
                         // this is the default choice when choosing a library for formatting/mounting

  vfsMgrVersionNum = UInt16(200); // version of the VFSMgr, obtained from the feature

// MountClass constants:
  vfsMountClass_SlotDriver = sysFileTSlotDriver;
  vfsMountClass_Simulator  = sysFileTSimulator;
  vfsMountClass_POSE       = Rsc('pose');

// Base MountParamType; others such as SlotMountParamType are extensions of this base type,
// switched on value of "mountClass" parameter.  It will make more sense someday when there
// are other kinds of FileSystems...  (Trust us.  :-)
type
  VFSAnyMountParamTag = record
    volRefNum: UInt16;  // The volRefNum of the volume.
    reserved: UInt16;
    mountClass: UInt32; // 'libs' for slotDriver-based filesystems

    // Other fields here, depending on value of 'mountClass'
  end;

  VFSAnyMountParamType = VFSAnyMountParamTag;
  VFSAnyMountParamPtr = ^VFSAnyMountParamType;

  VFSSlotMountParamTag = record
   vfsMountParam: VFSAnyMountParamType; // mountClass = VFSMountClass_SlotDriver = 'libs'
   slotLibRefNum: UInt16;
   slotRefNum: UInt16;
  end;
  VFSSlotMountParamType = VFSSlotMountParamTag;

  VFSPOSEMountParamTag = record
    vfsMountParam: VFSAnyMountParamType; // mountClass = VFSMountClass_POSE = 'pose'
    poseSlotNum: UInt8;
  end;
  VFSPOSEMountParamType = VFSPOSEMountParamTag;

(* For Example...
  VFSOtherMountParamTag = record
    vfsMountParam: VFSAnyMountParamType; // mountClass = 'othr' (for example)
    otherValue: UInt16;
  end;
  VFSOtherMountParamType = VFSOtherMountParamTag;
*)

  FileInfoTag = record
    attributes: UInt32;
    nameP: PChar;       // buffer to receive full name; pass NULL to avoid getting name
    nameBufLen: UInt16; // size of nameP buffer, in bytes
  end;
  FileInfoType = FileInfoTag;
  FileInfoPtr = ^FileInfoType;

  VolumeInfoTag = record
    attributes: UInt32;    // read-only etc.
    fsType: UInt32;        // Filesystem type for this volume (defined below)
    fsCreator: UInt32;     // Creator code of filesystem driver for this volume.  For use with VFSCustomControl().
    mountClass: UInt32;    // mount class that mounted this volume

    // For slot based filesystems: (mountClass = vfsMountClass_SlotDriver)
    slotLibRefNum: UInt16; // Library on which the volume is mounted
    slotRefNum: UInt16;    // ExpMgr slot number of card containing volume
    mediaType: UInt32;     // Type of card media (mediaMemoryStick, mediaCompactFlash, etc...)
    reserved: UInt32;      // reserved for future use (other mountclasses may need more space)
  end;
  VolumeInfoType = VolumeInfoTag;
  VolumeInfoPtr = ^VolumeInfoType;

type
  FileRef = UInt32;

const
  vfsInvalidVolRef  = 0; // constant for an invalid volume reference, guaranteed not to represent a valid one.  Use it like you would use NULL for a FILE*.
  vfsInvalidFileRef = 0; // constant for an invalid file reference, guaranteed not to represent a valid one.  Use it like you would use NULL for a FILE*.

(************************************************************
 * File Origin constants: (for the origins of relative offsets passed to 'seek' type routines).
 *************************************************************)

const
  vfsOriginBeginning = 0; // from the beginning (first data byte of file)
  vfsOriginCurrent   = 1; // from the current position
  vfsOriginEnd       = 2; // from the end of file (one position beyond last data byte, only negative offsets are legal)

type
  FileOrigin = UInt16;

(************************************************************
 * openMode flags passed to VFSFileOpen
 *************************************************************)

const
  vfsModeExclusive = $0001;      // don't let anyone else open it
  vfsModeRead      = $0002;      // open for read access
  vfsModeWrite     = $0004 or vfsModeExclusive;   // open for write access, implies exclusive
  vfsModeCreate    = $0008;      // create the file if it doesn't already exist.  Implemented in VFS layer, no FS lib call will ever have to handle this.
  vfsModeTruncate  = $0010;      // Truncate file to 0 bytes after opening, removing all existing data.  Implemented in VFS layer, no FS lib call will ever have to handle this.
  vfsModeReadWrite = vfsModeWrite or vfsModeRead; // open for read/write access
  vfsModeLeaveOpen = $0020;      // Leave the file open even if when the foreground task closes

      // Combination flag constants, for error checking purposes:
  vfsModeAll          = vfsModeExclusive or vfsModeRead or vfsModeWrite or vfsModeCreate or vfsModeTruncate or vfsModeReadWrite or vfsModeLeaveOpen;
  vfsModeVFSLayerOnly = vfsModeCreate or vfsModeTruncate; // flags only used apps & the VFS layer, FS libraries will never see these.

(************************************************************
 * File Attributes
 *************************************************************)

  vfsFileAttrReadOnly    = $00000001;
  vfsFileAttrHidden      = $00000002;
  vfsFileAttrSystem      = $00000004;
  vfsFileAttrVolumeLabel = $00000008;
  vfsFileAttrDirectory   = $00000010;
  vfsFileAttrArchive     = $00000020;
  vfsFileAttrLink        = $00000040;

  vfsFileAttrAll         = $0000007f;

(************************************************************
 * Volume Attributes
 *************************************************************)

  vfsVolumeAttrSlotBased = $00000001; // reserved
  vfsVolumeAttrReadOnly  = $00000002; // volume is read only
  vfsVolumeAttrHidden    = $00000004; // volume should not be user-visible.

(************************************************************
 * Date constants (for use with VFSFileGet/SetDate)
 *************************************************************)

  vfsFileDateCreated  = 1;
  vfsFileDateModified = 2;
  vfsFileDateAccessed = 3;

(************************************************************
 * Iterator start and stop constants.
 * Used by VFSVolumeEnumerate, VFSDirEntryEnumerate, VFSDirEntryEnumerate
 *************************************************************)

  vfsIteratorStart = 0;
  vfsIteratorStop  = $ffffffff;

(************************************************************
 * 'handled' field bit constants
 * (for use with Volume Mounted/Unmounted notifications)
 *************************************************************)

  vfsHandledUIAppSwitch = $01; // Any UI app switching has already been handled.
                               // The VFSMgr will not UIAppSwitch to the start.prc app
                               // (but it will loaded & sent the AutoStart launchcode),
                               // and the Launcher will not switch to itself.
  vfsHandledStartPrc    = $02; // And automatic running of start.prc has already been handled.
                               // VFSMgr will not load it, send it the AutoStart launchcode,
                               // or UIAppSwitch to it.

(************************************************************
 * Format/Mount flags (for use with VFSVolumeFormat/Mount)
 *************************************************************)

  vfsMountFlagsUseThisFileSystem = $01; // Mount/Format the volume with the filesystem specified
// vfsMountFlagsPrivate1         = $02  // for system use only
// vfsMountFlagsPrivate2         = $04  // for system use only
  vfsMountFlagsReserved1         = $08; // reserved
  vfsMountFlagsReserved2         = $10; // reserved
  vfsMountFlagsReserved3         = $20; // reserved
  vfsMountFlagsReserved4         = $40; // reserved
  vfsMountFlagsReserved5         = $80; // reserved

(************************************************************
 * Common filesystem types.  Used by FSFilesystemType and SlotCardIsFilesystemSupported.
 *************************************************************)

  vfsFilesystemType_VFAT    = Rsc('vfat'); // FAT12 and FAT16 extended to handle long file names
  vfsFilesystemType_FAT     = Rsc('fats'); // FAT12 and FAT16 which only handles 8.3 file names
  vfsFilesystemType_NTFS    = Rsc('ntfs'); // Windows NT filesystem
  vfsFilesystemType_HFSPlus = Rsc('hfse'); // The Macintosh extended hierarchical filesystem
  vfsFilesystemType_HFS     = Rsc('hfss'); // The Macintosh standard hierarchical filesystem
  vfsFilesystemType_MFS     = Rsc('mfso'); // The Macintosh original filesystem
  vfsFilesystemType_EXT2    = Rsc('ext2'); // Linux filesystem
  vfsFilesystemType_FFS     = Rsc('ffsb'); // Unix Berkeley block based filesystem
  vfsFilesystemType_NFS     = Rsc('nfsu'); // Unix Networked filesystem
  vfsFilesystemType_AFS     = Rsc('afsu'); // Unix Andrew filesystem
  vfsFilesystemType_Novell  = Rsc('novl'); // Novell filesystem
  vfsFilesystemType_HPFS    = Rsc('hpfs'); // OS2 High Performance filesystem

(************************************************************
 * Error codes
 *************************************************************)

  vfsErrBufferOverflow       = vfsErrorClass or 1;  // passed in buffer is too small
  vfsErrFileGeneric          = vfsErrorClass or 2;  // Generic file error.
  vfsErrFileBadRef           = vfsErrorClass or 3;  // the fileref is invalid (has been closed, or was not obtained from VFSFileOpen())
  vfsErrFileStillOpen        = vfsErrorClass or 4;  // returned from FSFileDelete if the file is still open
  vfsErrFilePermissionDenied = vfsErrorClass or 5;  // The file is read only
  vfsErrFileAlreadyExists    = vfsErrorClass or 6;  // a file of this name exists already in this location
  vfsErrFileEOF              = vfsErrorClass or 7;  // file pointer is at end of file
  vfsErrFileNotFound         = vfsErrorClass or 8;  // file was not found at the path specified
  vfsErrVolumeBadRef         = vfsErrorClass or 9;  // the volume refnum is invalid.
  vfsErrVolumeStillMounted   = vfsErrorClass or 10; // returned from FSVolumeFormat if the volume is still mounted
  vfsErrNoFileSystem         = vfsErrorClass or 11; // no installed filesystem supports this operation
  vfsErrBadData              = vfsErrorClass or 12; // operation could not be completed because of invalid data (i.e., import DB from .PRC file)
  vfsErrDirNotEmpty          = vfsErrorClass or 13; // can't delete a non-empty directory
  vfsErrBadName              = vfsErrorClass or 14; // invalid filename, or path, or volume label or something...
  vfsErrVolumeFull           = vfsErrorClass or 15; // not enough space left on volume
  vfsErrUnimplemented        = vfsErrorClass or 16; // this call is not implemented
  vfsErrNotADirectory        = vfsErrorClass or 17; // This operation requires a directory
  vfsErrIsADirectory         = vfsErrorClass or 18; // This operation requires a regular file, not a directory
  vfsErrDirectoryNotFound    = vfsErrorClass or 19; // Returned from VFSFileCreate when the path leading up to the new file does not exist
  vfsErrNameShortened        = vfsErrorClass or 20; // A volume name or filename was automatically shortened to conform to filesystem spec

(************************************************************
 * Selectors for routines found in the VFS manager. The order
 * of these selectors MUST match the jump table in VFSMgr.c.
 *************************************************************)

  vfsTrapInit                         = 0;
  vfsTrapCustomControl                = 1;

  vfsTrapFileCreate                   = 2;
  vfsTrapFileOpen                     = 3;
  vfsTrapFileClose                    = 4;
  vfsTrapFileReadData                 = 5;
  vfsTrapFileRead                     = 6;
  vfsTrapFileWrite                    = 7;
  vfsTrapFileDelete                   = 8;
  vfsTrapFileRename                   = 9;
  vfsTrapFileSeek                     = 10;
  vfsTrapFileEOF                      = 11;
  vfsTrapFileTell                     = 12;
  vfsTrapFileResize                   = 13;
  vfsTrapFileGetAttributes            = 14;
  vfsTrapFileSetAttributes            = 15;
  vfsTrapFileGetDate                  = 16;
  vfsTrapFileSetDate                  = 17;
  vfsTrapFileSize                     = 18;

  vfsTrapDirCreate                    = 19;
  vfsTrapDirEntryEnumerate            = 20;
  vfsTrapGetDefaultDirectory          = 21;
  vfsTrapRegisterDefaultDirectory     = 22;
  vfsTrapUnregisterDefaultDirectory   = 23;

  vfsTrapVolumeFormat                 = 24;
  vfsTrapVolumeMount                  = 25;
  vfsTrapVolumeUnmount                = 26;
  vfsTrapVolumeEnumerate              = 27;
  vfsTrapVolumeInfo                   = 28;
  vfsTrapVolumeGetLabel               = 29;
  vfsTrapVolumeSetLabel               = 30;
  vfsTrapVolumeSize                   = 31;

  vfsTrapInstallFSLib                 = 32;
  vfsTrapRemoveFSLib                  = 33;
  vfsTrapImportDatabaseFromFile       = 34;
  vfsTrapExportDatabaseToFile         = 35;
  vfsTrapFileDBGetResource            = 36;
  vfsTrapFileDBInfo                   = 37;
  vfsTrapFileDBGetRecord              = 38;

  vfsTrapImportDatabaseFromFileCustom = 39;
  vfsTrapExportDatabaseToFileCustom   = 40;

// System use only
  vfsTrapPrivate1                     = 41;

  vfsMaxSelector                      = vfsTrapPrivate1;


type
  VFSImportProcPtr = function(totalBytes, offset: UInt32; userDataP: Pointer): Err;

  VFSExportProcPtr = function(totalBytes, offset: UInt32; userDataP: Pointer): Err;

function VFSInit: Err;

// if you pass NULL for fsCreator, VFS will iterate through
// all installed filesystems until it finds one that does not return an error.
function VFSCustomControl(fsCreator, apiCreator: UInt32; apiSelector: UInt16;
                          valueP: Pointer; var valueLenP: UInt16): Err;

function VFSFileCreate(volRefNum: UInt16; const pathNameP: PChar): Err;

function VFSFileOpen(volRefNum: UInt16; const pathNameP: PChar; openMode: UInt16; var fileRefP: FileRef): Err;

function VFSFileClose(fileRef: FileRef): Err;

function VFSFileReadData(fileRef: FileRef; numBytes: UInt32; bufBaseP: Pointer;
                         offset: UInt32; var numBytesReadP: UInt32): Err;

function VFSFileRead(fileRef: FileRef; numBytes: UInt32; bufP: Pointer; var numBytesReadP: UInt32): Err;

function VFSFileWrite(fileRef: FileRef; numBytes: UInt32; const dataP: Pointer; var numBytesWrittenP: UInt32): Err;

// some file routines work on directories
function VFSFileDelete(volRefNum: UInt16; const pathNameP: PChar): Err;

function VFSFileRename(volRefNum: UInt16; const pathNameP, newNameP: PChar): Err;

function VFSFileSeek(fileRef: FileRef; origin: FileOrigin; offset: Int32): Err;

function VFSFileEOF(fileRef: FileRef): Err;

function VFSFileTell(fileRef: FileRef; var filePosP: UInt32): Err;

function VFSFileSize(fileRef: FileRef; var fileSizeP: UInt32): Err;

function VFSFileResize(fileRef: FileRef; newSize: UInt32): Err;

function VFSFileGetAttributes(fileRef: FileRef; var attributesP: UInt32): Err;

function VFSFileSetAttributes(fileRef: FileRef; attributes: UInt32): Err;

function VFSFileGetDate(fileRef: FileRef; whichDate: UInt16; var dateP: UInt32): Err;

function VFSFileSetDate(fileRef: FileRef; whichDate: UInt16; date: UInt32): Err;


function VFSDirCreate(volRefNum: UInt16; const dirNameP: PChar): Err;

function VFSDirEntryEnumerate(dirRef: FileRef; var dirEntryIteratorP: UInt32; var infoP: FileInfoType): Err;


function VFSGetDefaultDirectory(volRefNum: UInt16; const fileTypeStr: PChar;
                                pathStr: PChar; var bufLenP: UInt16): Err;

function VFSRegisterDefaultDirectory(const fileTypeStr: PChar; mediaType: UInt32;
                                     const pathStr: PChar): Err;

function VFSUnregisterDefaultDirectory(const fileTypeStr: PChar; mediaType: UInt32): Err;


function VFSVolumeFormat(flags: UInt8; fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err;

function VFSVolumeMount(flags: UInt8; fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err;

function VFSVolumeUnmount(volRefNum: UInt16): Err;

function VFSVolumeEnumerate(var volRefNumP: UInt16; var volIteratorP: UInt32): Err;

function VFSVolumeInfo(volRefNum: UInt16; var volInfoP: VolumeInfoType): Err;

function VFSVolumeGetLabel(volRefNum: UInt16; labelP: PChar; bufLen: UInt16): Err;

function VFSVolumeSetLabel(volRefNum: UInt16; const labelP: PChar): Err;

function VFSVolumeSize(volRefNum: UInt16; var volumeUsedP, volumeTotalP: UInt32): Err;

function VFSInstallFSLib(creator: UInt32; var fsLibRefNumP: UInt16): Err;

function VFSRemoveFSLib(fsLibRefNum: UInt16): Err;

function VFSImportDatabaseFromFile(volRefNum: UInt16; const pathNameP: PChar;
                                   var cardNoP: UInt16; var dbIDP: LocalID): Err;

function VFSImportDatabaseFromFileCustom(volRefNum: UInt16; const pathNameP: PChar;
                                         var cardNoP: UInt16; var dbIDP: LocalID;
                                         importProcP: VFSImportProcPtr; userDataP: Pointer): Err;

function VFSExportDatabaseToFile(volRefNum: UInt16; const pathNameP: PChar;
                                 cardNo: UInt16; dbID: LocalID): Err;

function VFSExportDatabaseToFileCustom(volRefNum: UInt16; const pathNameP: PChar;
                                       cardNo: UInt16; dbID: LocalID;
                                       exportProcP: VFSExportProcPtr; userDataP: Pointer): Err;

function VFSFileDBGetResource(ref: FileRef; type_: DmResType; resID: DmResID; var resHP: MemHandle): Err;

function VFSFileDBInfo(ref: FileRef; nameP: PChar; var attributesP, versionP: UInt16;
                       var crDateP, modDateP, bckUpDateP, modNumP: UInt32;
                       var appInfoHP, sortInfoHP: MemHandle; var typeP, creatorP: UInt32;
                       var numRecordsP: UInt16): Err;

function VFSFileDBGetRecord(ref: FileRef; recIndex: UInt16; var recHP: MemHandle;
                            var recAttrP: UInt8; var uniqueIDP: UInt32): Err;

implementation

(**)
function __VFSInit: Err; syscall sysTrapVFSMgr;
function __VFSCustomControl(fsCreator, apiCreator: UInt32; apiSelector: UInt16;
                          valueP: Pointer; var valueLenP: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSFileCreate(volRefNum: UInt16; const pathNameP: PChar): Err; syscall sysTrapVFSMgr;
function __VFSFileOpen(volRefNum: UInt16; const pathNameP: PChar; openMode: UInt16; var fileRefP: FileRef): Err; syscall sysTrapVFSMgr;
function __VFSFileClose(fileRef: FileRef): Err; syscall sysTrapVFSMgr;
function __VFSFileReadData(fileRef: FileRef; numBytes: UInt32; bufBaseP: Pointer;
                         offset: UInt32; var numBytesReadP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileRead(fileRef: FileRef; numBytes: UInt32; bufP: Pointer; var numBytesReadP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileWrite(fileRef: FileRef; numBytes: UInt32; const dataP: Pointer; var numBytesWrittenP: UInt32): Err; syscall sysTrapVFSMgr;
// some file routines work on directories
function __VFSFileDelete(volRefNum: UInt16; const pathNameP: PChar): Err; syscall sysTrapVFSMgr;
function __VFSFileRename(volRefNum: UInt16; const pathNameP, newNameP: PChar): Err; syscall sysTrapVFSMgr;
function __VFSFileSeek(fileRef: FileRef; origin: FileOrigin; offset: Int32): Err; syscall sysTrapVFSMgr;
function __VFSFileEOF(fileRef: FileRef): Err; syscall sysTrapVFSMgr;
function __VFSFileTell(fileRef: FileRef; var filePosP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileSize(fileRef: FileRef; var fileSizeP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileResize(fileRef: FileRef; newSize: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileGetAttributes(fileRef: FileRef; var attributesP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileSetAttributes(fileRef: FileRef; attributes: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileGetDate(fileRef: FileRef; whichDate: UInt16; var dateP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSFileSetDate(fileRef: FileRef; whichDate: UInt16; date: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSDirCreate(volRefNum: UInt16; const dirNameP: PChar): Err; syscall sysTrapVFSMgr;
function __VFSDirEntryEnumerate(dirRef: FileRef; var dirEntryIteratorP: UInt32; var infoP: FileInfoType): Err; syscall sysTrapVFSMgr;
function __VFSGetDefaultDirectory(volRefNum: UInt16; const fileTypeStr: PChar;
                                pathStr: PChar; var bufLenP: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSRegisterDefaultDirectory(const fileTypeStr: PChar; mediaType: UInt32;
                                     const pathStr: PChar): Err; syscall sysTrapVFSMgr;
function __VFSUnregisterDefaultDirectory(const fileTypeStr: PChar; mediaType: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSVolumeFormat(flags: UInt8; fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err; syscall sysTrapVFSMgr;
function __VFSVolumeMount(flags: UInt8; fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err; syscall sysTrapVFSMgr;
function __VFSVolumeUnmount(volRefNum: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSVolumeEnumerate(var volRefNumP: UInt16; var volIteratorP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSVolumeInfo(volRefNum: UInt16; var volInfoP: VolumeInfoType): Err; syscall sysTrapVFSMgr;
function __VFSVolumeGetLabel(volRefNum: UInt16; labelP: PChar; bufLen: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSVolumeSetLabel(volRefNum: UInt16; const labelP: PChar): Err; syscall sysTrapVFSMgr;
function __VFSVolumeSize(volRefNum: UInt16; var volumeUsedP, volumeTotalP: UInt32): Err; syscall sysTrapVFSMgr;
function __VFSInstallFSLib(creator: UInt32; var fsLibRefNumP: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSRemoveFSLib(fsLibRefNum: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSImportDatabaseFromFile(volRefNum: UInt16; const pathNameP: PChar;
                                   var cardNoP: UInt16; var dbIDP: LocalID): Err; syscall sysTrapVFSMgr;
function __VFSImportDatabaseFromFileCustom(volRefNum: UInt16; const pathNameP: PChar;
                                         var cardNoP: UInt16; var dbIDP: LocalID;
                                         importProcP: VFSImportProcPtr; userDataP: Pointer): Err; syscall sysTrapVFSMgr;
function __VFSExportDatabaseToFile(volRefNum: UInt16; const pathNameP: PChar;
                                 cardNo: UInt16; dbID: LocalID): Err; syscall sysTrapVFSMgr;
function __VFSExportDatabaseToFileCustom(volRefNum: UInt16; const pathNameP: PChar;
                                       cardNo: UInt16; dbID: LocalID;
                                       exportProcP: VFSExportProcPtr; userDataP: Pointer): Err; syscall sysTrapVFSMgr;
function __VFSFileDBGetResource(ref: FileRef; type_: DmResType; resID: DmResID; var resHP: MemHandle): Err; syscall sysTrapVFSMgr;
function __VFSFileDBInfo(ref: FileRef; nameP: PChar; var attributesP, versionP: UInt16;
                       var crDateP, modDateP, bckUpDateP, modNumP: UInt32;
                       var appInfoHP, sortInfoHP: MemHandle; var typeP, creatorP: UInt32;
                       var numRecordsP: UInt16): Err; syscall sysTrapVFSMgr;
function __VFSFileDBGetRecord(ref: FileRef; recIndex: UInt16; var recHP: MemHandle;
                            var recAttrP: UInt8; var uniqueIDP: UInt32): Err; syscall sysTrapVFSMgr;
(**)

function VFSInit: Err;
begin
 asm
  move.l #$vfsTrapInit, D2;
 end;
 VFSInit := __VFSInit;
end;

function VFSCustomControl(fsCreator, apiCreator: UInt32; apiSelector: UInt16;
                          valueP: Pointer; var valueLenP: UInt16): Err;
begin
 asm
  move.l #$vfsTrapCustomControl, D2;
 end;
 VFSCustomControl := __VFSCustomControl(fsCreator, apiCreator, apiSelector, valueP, valueLenP);
end;

function VFSFileCreate(volRefNum: UInt16; const pathNameP: PChar): Err;
begin
 asm
  move.l #$vfsTrapFileCreate, D2;
 end;
 VFSFileCreate := __VFSFileCreate(volRefNum, pathNameP);
end;

function VFSFileOpen(volRefNum: UInt16; const pathNameP: PChar; openMode: UInt16; var fileRefP: FileRef): Err;
begin
 asm
  move.l #$vfsTrapFileOpen, D2;
 end;
 VFSFileOpen := __VFSFileOpen(volRefNum, pathNameP, openMode, fileRefP);
end;

function VFSFileClose(fileRef: FileRef): Err;
begin
 asm
  move.l #$vfsTrapFileClose, D2;
 end;
 VFSFileClose := __VFSFileClose(fileRef);
end;

function VFSFileReadData(fileRef: FileRef; numBytes: UInt32; bufBaseP: Pointer;
                         offset: UInt32; var numBytesReadP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileReadData, D2;
 end;
 VFSFileReadData := __VFSFileReadData(fileRef, numBytes, bufBaseP, offset, numBytesReadP);
end;

function VFSFileRead(fileRef: FileRef; numBytes: UInt32; bufP: Pointer; var numBytesReadP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileRead, D2;
 end;
 VFSFileRead := __VFSFileRead(fileRef, numBytes, bufP, numBytesReadP);
end;

function VFSFileWrite(fileRef: FileRef; numBytes: UInt32; const dataP: Pointer; var numBytesWrittenP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileWrite, D2;
 end;
 VFSFileWrite := __VFSFileWrite(fileRef, numBytes, dataP, numBytesWrittenP);
end;

function VFSFileDelete(volRefNum: UInt16; const pathNameP: PChar): Err;
begin
 asm
  move.l #$vfsTrapFileDelete, D2;
 end;
 VFSFileDelete := __VFSFileDelete(volRefNum, pathNameP);
end;

function VFSFileRename(volRefNum: UInt16; const pathNameP, newNameP: PChar): Err;
begin
 asm
  move.l #$vfsTrapFileRename, D2;
 end;
 VFSFileRename := __VFSFileRename(volRefNum, pathNameP, newNameP);
end;

function VFSFileSeek(fileRef: FileRef; origin: FileOrigin; offset: Int32): Err;
begin
 asm
  move.l #$vfsTrapFileSeek, D2;
 end;
 VFSFileSeek := __VFSFileSeek(fileRef, origin, offset);
end;

function VFSFileEOF(fileRef: FileRef): Err;
begin
 asm
  move.l #$vfsTrapFileEOF, D2;
 end;
 VFSFileEOF := __VFSFileEOF(fileRef);
end;

function VFSFileTell(fileRef: FileRef; var filePosP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileTell, D2;
 end;
 VFSFileTell := __VFSFileTell(fileRef, filePosP);
end;

function VFSFileSize(fileRef: FileRef; var fileSizeP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileSize, D2;
 end;
 VFSFileSize := __VFSFileSize(fileRef, fileSizeP);
end;

function VFSFileResize(fileRef: FileRef; newSize: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileResize, D2;
 end;
 VFSFileResize := __VFSFileResize(fileRef, newSize);
end;

function VFSFileGetAttributes(fileRef: FileRef; var attributesP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileGetAttributes, D2;
 end;
 VFSFileGetAttributes := __VFSFileGetAttributes(fileRef, attributesP);
end;

function VFSFileSetAttributes(fileRef: FileRef; attributes: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileSetAttributes, D2;
 end;
 VFSFileSetAttributes := __VFSFileSetAttributes(fileRef, attributes);
end;

function VFSFileGetDate(fileRef: FileRef; whichDate: UInt16; var dateP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileGetDate, D2;
 end;
 VFSFileGetDate := __VFSFileGetDate(fileRef, whichDate, dateP);
end;

function VFSFileSetDate(fileRef: FileRef; whichDate: UInt16; date: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileSetDate, D2;
 end;
 VFSFileSetDate := __VFSFileSetDate(fileRef, whichDate, date);
end;

function VFSDirCreate(volRefNum: UInt16; const dirNameP: PChar): Err;
begin
 asm
  move.l #$vfsTrapDirCreate, D2;
 end;
 VFSDirCreate := __VFSDirCreate(volRefNum, dirNameP);
end;

function VFSDirEntryEnumerate(dirRef: FileRef; var dirEntryIteratorP: UInt32; var infoP: FileInfoType): Err;
begin
 asm
  move.l #$vfsTrapDirEntryEnumerate, D2;
 end;
 VFSDirEntryEnumerate := __VFSDirEntryEnumerate(dirRef, dirEntryIteratorP, infoP);
end;

function VFSGetDefaultDirectory(volRefNum: UInt16; const fileTypeStr: PChar;
                                pathStr: PChar; var bufLenP: UInt16): Err;
begin
 asm
  move.l #$vfsTrapGetDefaultDirectory, D2;
 end;
 VFSGetDefaultDirectory := __VFSGetDefaultDirectory(volRefNum, fileTypeStr, pathStr, bufLenP);
end;

function VFSRegisterDefaultDirectory(const fileTypeStr: PChar; mediaType: UInt32;
                                     const pathStr: PChar): Err;
begin
 asm
  move.l #$vfsTrapRegisterDefaultDirectory, D2;
 end;
 VFSRegisterDefaultDirectory := __VFSRegisterDefaultDirectory(fileTypeStr, mediaType, pathStr);
end;

function VFSUnregisterDefaultDirectory(const fileTypeStr: PChar; mediaType: UInt32): Err;
begin
 asm
  move.l #$vfsTrapUnregisterDefaultDirectory, D2;
 end;
 VFSUnregisterDefaultDirectory := __VFSUnregisterDefaultDirectory(fileTypeStr, mediaType);
end;

function VFSVolumeFormat(flags: UInt8; fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err;
begin
 asm
  move.l #$vfsTrapVolumeFormat, D2;
 end;
 VFSVolumeFormat := __VFSVolumeFormat(flags, fsLibRefNum, vfsMountParamP);
end;

function VFSVolumeMount(flags: UInt8; fsLibRefNum: UInt16; vfsMountParamP: VFSAnyMountParamPtr): Err;
begin
 asm
  move.l #$vfsTrapVolumeMount, D2;
 end;
 VFSVolumeMount := __VFSVolumeMount(flags, fsLibRefNum, vfsMountParamP);
end;

function VFSVolumeUnmount(volRefNum: UInt16): Err;
begin
 asm
  move.l #$vfsTrapVolumeUnmount, D2;
 end;
 VFSVolumeUnmount := __VFSVolumeUnmount(volRefNum);
end;

function VFSVolumeEnumerate(var volRefNumP: UInt16; var volIteratorP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapVolumeEnumerate, D2;
 end;
 VFSVolumeEnumerate := __VFSVolumeEnumerate(volRefNumP, volIteratorP);
end;

function VFSVolumeInfo(volRefNum: UInt16; var volInfoP: VolumeInfoType): Err;
begin
 asm
  move.l #$vfsTrapVolumeInfo, D2;
 end;
 VFSVolumeInfo := __VFSVolumeInfo(volRefNum, volInfoP);
end;

function VFSVolumeGetLabel(volRefNum: UInt16; labelP: PChar; bufLen: UInt16): Err;
begin
 asm
  move.l #$vfsTrapVolumeGetLabel, D2;
 end;
 VFSVolumeGetLabel := __VFSVolumeGetLabel(volRefNum, labelP, bufLen);
end;

function VFSVolumeSetLabel(volRefNum: UInt16; const labelP: PChar): Err;
begin
 asm
  move.l #$vfsTrapVolumeSetLabel, D2;
 end;
 VFSVolumeSetLabel := __VFSVolumeSetLabel(volRefNum, labelP);
end;

function VFSVolumeSize(volRefNum: UInt16; var volumeUsedP, volumeTotalP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapVolumeSize, D2;
 end;
 VFSVolumeSize := __VFSVolumeSize(volRefNum, volumeUsedP, volumeTotalP);
end;

function VFSInstallFSLib(creator: UInt32; var fsLibRefNumP: UInt16): Err;
begin
 asm
  move.l #$vfsTrapInstallFSLib, D2;
 end;
 VFSInstallFSLib := __VFSInstallFSLib(creator, fsLibRefNumP);
end;

function VFSRemoveFSLib(fsLibRefNum: UInt16): Err;
begin
 asm
  move.l #$vfsTrapRemoveFSLib, D2;
 end;
 VFSRemoveFSLib := __VFSRemoveFSLib(fsLibRefNum);
end;

function VFSImportDatabaseFromFile(volRefNum: UInt16; const pathNameP: PChar;
                                   var cardNoP: UInt16; var dbIDP: LocalID): Err;
begin
 asm
  move.l #$vfsTrapImportDatabaseFromFile, D2;
 end;
 VFSImportDatabaseFromFile := __VFSImportDatabaseFromFile(volRefNum, pathNameP, cardNoP, dbIDP);
end;

function VFSImportDatabaseFromFileCustom(volRefNum: UInt16; const pathNameP: PChar;
                                         var cardNoP: UInt16; var dbIDP: LocalID;
                                         importProcP: VFSImportProcPtr; userDataP: Pointer): Err;
begin
 asm
  move.l #$vfsTrapImportDatabaseFromFileCustom, D2;
 end;
 VFSImportDatabaseFromFileCustom := __VFSImportDatabaseFromFileCustom(volRefNum, pathNameP,
                                    cardNoP, dbIDP, importProcP, userDataP);
end;

function VFSExportDatabaseToFile(volRefNum: UInt16; const pathNameP: PChar;
                                 cardNo: UInt16; dbID: LocalID): Err;
begin
 asm
  move.l #$vfsTrapExportDatabaseToFile, D2;
 end;
 VFSExportDatabaseToFile := __VFSExportDatabaseToFile(volRefNum, pathNameP, cardNo, dbID);
end;

function VFSExportDatabaseToFileCustom(volRefNum: UInt16; const pathNameP: PChar;
                                       cardNo: UInt16; dbID: LocalID;
                                       exportProcP: VFSExportProcPtr; userDataP: Pointer): Err;
begin
 asm
  move.l #$vfsTrapExportDatabaseToFileCustom, D2;
 end;
 VFSExportDatabaseToFileCustom := __VFSExportDatabaseToFileCustom(volRefNum, pathNameP,
                                  cardNo, dbID, exportProcP, userDataP);
end;

function VFSFileDBGetResource(ref: FileRef; type_: DmResType; resID: DmResID; var resHP: MemHandle): Err;
begin
 asm
  move.l #$vfsTrapFileDBGetResource, D2;
 end;
 VFSFileDBGetResource := __VFSFileDBGetResource(ref, type_, resID, resHP);
end;

function VFSFileDBInfo(ref: FileRef; nameP: PChar; var attributesP, versionP: UInt16;
                       var crDateP, modDateP, bckUpDateP, modNumP: UInt32;
                       var appInfoHP, sortInfoHP: MemHandle; var typeP, creatorP: UInt32;
                       var numRecordsP: UInt16): Err;
begin
 asm
  move.l #$vfsTrapFileDBInfo, D2;
 end;
 VFSFileDBInfo := __VFSFileDBInfo(ref, nameP, attributesP, versionP, crDateP, modDateP,
                  bckUpDateP, modNumP, appInfoHP, sortInfoHP, typeP, creatorP, numRecordsP);
end;

function VFSFileDBGetRecord(ref: FileRef; recIndex: UInt16; var recHP: MemHandle;
                            var recAttrP: UInt8; var uniqueIDP: UInt32): Err;
begin
 asm
  move.l #$vfsTrapFileDBGetRecord, D2;
 end;
 VFSFileDBGetRecord := __VFSFileDBGetRecord(ref, recIndex, recHP, recAttrP, uniqueIDP);
end;

end.
