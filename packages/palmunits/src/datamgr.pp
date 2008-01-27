(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: DataMgr.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    Header for the Data Manager
 *
 * History:
 *    11/14/94  RM - Created by Ron Marianetti
 *
 *****************************************************************************)

unit datamgr;

interface

uses palmos, coretraps, errorbase;

type
  DmResType = UInt32;
  DmResID = UInt16;

(************************************************************
 * Category equates
 *************************************************************)

const
  dmRecAttrCategoryMask = $0F;    // mask for category #
  dmRecNumCategories    = 16;     // number of categories
  dmCategoryLength      = 16;     // 15 chars + 1 null terminator

  dmAllCategories       = $ff;
  dmUnfiledCategory     = 0;

  dmMaxRecordIndex      = $ffff;

// Record Attributes
//
// *** IMPORTANT:
// ***
// *** Any changes to record attributes must be reflected in dmAllRecAttrs and dmSysOnlyRecAttrs ***
// ***
// *** Only one nibble is available for record attributes
//
// *** ANY CHANGES MADE TO THESE ATTRIBUTES MUST BE REFLECTED IN DESKTOP LINK
// *** SERVER CODE (DLCommon.h, DLServer.c)

  dmRecAttrDelete       = $80; // delete this record next sync
  dmRecAttrDirty        = $40; // archive this record next sync
  dmRecAttrBusy         = $20; // record currently in use
  dmRecAttrSecret       = $10; // "secret" record - password protected

// All record atributes (for error-checking)
  dmAllRecAttrs         = dmRecAttrDelete or dmRecAttrDirty or dmRecAttrBusy or dmRecAttrSecret;

// Record attributes which only the system is allowed to change (for error-checking)
  dmSysOnlyRecAttrs     = dmRecAttrBusy;

(************************************************************
 * Database Header equates
 *************************************************************)

  dmDBNameLength        = 32; // 31 chars + 1 null terminator

// Attributes of a Database
//
// *** IMPORTANT:
// ***
// *** Any changes to database attributes must be reflected in dmAllHdrAttrs and dmSysOnlyHdrAttrs ***
// ***

  dmHdrAttrResDB             = $0001; // Resource database
  dmHdrAttrReadOnly          = $0002; // Read Only database
  dmHdrAttrAppInfoDirty      = $0004; // Set if Application Info block is dirty
                                      // Optionally supported by an App's conduit
  dmHdrAttrBackup            = $0008; //  Set if database should be backed up to PC if
                                      //  no app-specific synchronization conduit has
                                      //  been supplied.
  dmHdrAttrOKToInstallNewer  = $0010; // This tells the backup conduit that it's OK
                                      //  for it to install a newer version of this database
                                      //  with a different name if the current database is
                                      //  open. This mechanism is used to update the
                                      //  Graffiti Shortcuts database, for example.
  dmHdrAttrResetAfterInstall = $0020; // Device requires a reset after this database is
                                      // installed.
  dmHdrAttrCopyPrevention    = $0040; // This database should not be copied to

  dmHdrAttrStream            = $0080; // This database is used for file stream implementation.
  dmHdrAttrHidden            = $0100; // This database should generally be hidden from view
                                      //  used to hide some apps from the main view of the
                                      //  launcher for example.
                                      // For data (non-resource) databases, this hides the record
                                      //   count within the launcher info screen.
  dmHdrAttrLaunchableData    = $0200; // This data database (not applicable for executables)
                                      //  can be "launched" by passing it's name to it's owner
                                      //  app ('appl' database with same creator) using
                                      //  the sysAppLaunchCmdOpenNamedDB action code.
  dmHdrAttrRecyclable        = $0400; // This database (resource or record) is recyclable:
                                      //  it will be deleted Real Soon Now, generally the next
                                      //  time the database is closed.

  dmHdrAttrBundle            = $0800; // This database (resource or record) is associated with
                                      // the application with the same creator. It will be beamed
                                      // and copied along with the application.

  dmHdrAttrOpen              = $8000; // Database not closed properly


// All database atributes (for error-checking)
  dmAllHdrAttrs              = dmHdrAttrResDB or
                               dmHdrAttrReadOnly or
                               dmHdrAttrAppInfoDirty or
                               dmHdrAttrBackup or
                               dmHdrAttrOKToInstallNewer or
                               dmHdrAttrResetAfterInstall or
                               dmHdrAttrCopyPrevention or
                               dmHdrAttrStream or
                               dmHdrAttrLaunchableData or
                               dmHdrAttrRecyclable or
                               dmHdrAttrBundle or
                               dmHdrAttrOpen;

// Database attributes which only the system is allowed to change (for error-checking)
  dmSysOnlyHdrAttrs          = dmHdrAttrResDB or dmHdrAttrOpen;

(************************************************************
 * Unique ID equates
 *************************************************************)

  dmRecordIDReservedRange    = 1; // The range of upper bits in the database's
                                  // uniqueIDSeed from 0 to this number are
                                  // reserved and not randomly picked when a
                                  // database is created.
  dmDefaultRecordsID         = 0; // Records in a default database are copied
                                  // with their uniqueIDSeeds set in this range.
  dmUnusedRecordID           = 0; // Record ID not allowed on the device

(************************************************************
 * Mode flags passed to DmOpenDatabase
 *************************************************************)

  dmModeReadOnly             = $0001; // read  access
  dmModeWrite                = $0002; // write access
  dmModeReadWrite            = $0003; // read & write access
  dmModeLeaveOpen            = $0004; // leave open when app quits
  dmModeExclusive            = $0008; // don't let anyone else open it
  dmModeShowSecret           = $0010; // force show of secret records

// Generic type used to represent an open Database
type
  DmOpenRef = Pointer;

(************************************************************
 * Structure passed to DmGetNextDatabaseByTypeCreator and used
 *  to cache search information between multiple searches.
 *************************************************************)

type
  DmSearchStateType = record
    info: array [0..8-1] of UInt32;
  end;

  DmSearchStatePtr = ^DmSearchStateType;

(************************************************************
 * Structures used by the sorting routines
 *************************************************************)

  SortRecordInfoType = record
    attributes: UInt8;                         // record attributes;
    uniqueID: array [0..3-1] of UInt8;         // unique ID of record
  end;

  SortRecordInfoPtr = ^SortRecordInfoType;

  DmComparF = function(p1, p2: Pointer; other: Int16; s1, s2: SortRecordInfoPtr; appInfoH: MemHandle): Int16;

(************************************************************
 * Database manager error codes
 * the constant dmErrorClass is defined in ErrorBase.h
 *************************************************************)

const
  dmErrMemError             = dmErrorClass or 1;
  dmErrIndexOutOfRange      = dmErrorClass or 2;
  dmErrInvalidParam         = dmErrorClass or 3;
  dmErrReadOnly             = dmErrorClass or 4;
  dmErrDatabaseOpen         = dmErrorClass or 5;
  dmErrCantOpen             = dmErrorClass or 6;
  dmErrCantFind             = dmErrorClass or 7;
  dmErrRecordInWrongCard    = dmErrorClass or 8;
  dmErrCorruptDatabase      = dmErrorClass or 9;
  dmErrRecordDeleted        = dmErrorClass or 10;
  dmErrRecordArchived       = dmErrorClass or 11;
  dmErrNotRecordDB          = dmErrorClass or 12;
  dmErrNotResourceDB        = dmErrorClass or 13;
  dmErrROMBased             = dmErrorClass or 14;
  dmErrRecordBusy           = dmErrorClass or 15;
  dmErrResourceNotFound     = dmErrorClass or 16;
  dmErrNoOpenDatabase       = dmErrorClass or 17;
  dmErrInvalidCategory      = dmErrorClass or 18;
  dmErrNotValidRecord       = dmErrorClass or 19;
  dmErrWriteOutOfBounds     = dmErrorClass or 20;
  dmErrSeekFailed           = dmErrorClass or 21;
  dmErrAlreadyOpenForWrites = dmErrorClass or 22;
  dmErrOpenedByAnotherTask  = dmErrorClass or 23;
  dmErrUniqueIDNotFound     = dmErrorClass or 24;
  dmErrAlreadyExists        = dmErrorClass or 25;
  dmErrInvalidDatabaseName  = dmErrorClass or 26;
  dmErrDatabaseProtected    = dmErrorClass or 27;
  dmErrDatabaseNotProtected = dmErrorClass or 28;

(************************************************************
 * Values for the direction parameter of DmSeekRecordInCategory
 *************************************************************)

  dmSeekForward             = 1;
  dmSeekBackward            = -1;

(************************************************************
 * Data Manager procedures
 *************************************************************)

// Initialization
function DmInit: Err; syscall sysTrapDmInit;

// Directory Lists
function DmCreateDatabase(cardNo: UInt16; const nameP: PChar;
                          creator, type_: UInt32; resDB: Boolean): Err; syscall sysTrapDmCreateDatabase;

function DmCreateDatabaseFromImage(bufferP: MemPtr): Err; syscall sysTrapDmCreateDatabaseFromImage;


function DmDeleteDatabase(cardNo: UInt16; dbID: LocalID): Err; syscall sysTrapDmDeleteDatabase;

function DmNumDatabases(cardNo: UInt16): UInt16; syscall sysTrapDmNumDatabases;

function DmGetDatabase(cardNo, index: UInt16): LocalID; syscall sysTrapDmGetDatabase;

function DmFindDatabase(cardNo: UInt16; const nameP: PChar): LocalID; syscall sysTrapDmFindDatabase;

function DmGetNextDatabaseByTypeCreator(newSearch: Boolean; stateInfoP: DmSearchStatePtr;
                                        type_, creator: UInt32; onlyLatestVers: Boolean;
                                        var cardNoP: UInt16; var dbIDP: LocalID): Err; syscall sysTrapDmGetNextDatabaseByTypeCreator;

// Database info
function DmDatabaseInfo(cardNo: UInt16; dbID: LocalID; nameP: PChar;
                        var attributesP, versionP: UInt16; var crDateP, modDateP, bckUpDateP, modNumP: UInt32;
                        var appInfoIDP, sortInfoIDP: LocalID; var typeP, creatorP: UInt32): Err; syscall sysTrapDmDatabaseInfo;

function DmSetDatabaseInfo(cardNo: UInt16; dbID: LocalID; const nameP: PChar;
                           var attributesP, versionP: UInt16; var crDateP, modDateP, bckUpDateP, modNumP: UInt32;
                           var appInfoIDP, sortInfoIDP: LocalID; var typeP, creatorP: UInt32): Err; syscall sysTrapDmSetDatabaseInfo;

function DmDatabaseSize(cardNo: UInt16; dbID: LocalID; var numRecordsP, totalBytesP, dataBytesP: UInt32): Err; syscall sysTrapDmDatabaseSize;

// This routine can be used to prevent a database from being deleted (by passing
//  true for 'protect'). It will increment the protect count if 'protect' is true
//  and decrement it if 'protect' is false. This is used by code that wants to
//  keep a particular record or resource in a database locked down but doesn't
//  want to keep the database open. This information is keep in the dynamic heap so
//  all databases are "unprotected" at system reset.
function DmDatabaseProtect(cardNo: UInt16; dbID: LocalID; protect: Boolean): Err; syscall sysTrapDmDatabaseProtect;

// Open/close Databases
function DmOpenDatabase(cardNo: UInt16; dbID: LocalID; mode: UInt16): DmOpenRef; syscall sysTrapDmOpenDatabase;

function DmOpenDatabaseByTypeCreator(type_, creator: UInt32; mode: UInt16): DmOpenRef; syscall sysTrapDmOpenDatabaseByTypeCreator;

function DmOpenDBNoOverlay(cardNo: UInt16; dbID: LocalID; mode: UInt16): DmOpenRef; syscall sysTrapDmOpenDBNoOverlay;

function DmCloseDatabase(dbP: DmOpenRef): Err; syscall sysTrapDmCloseDatabase;

// Info on open databases
function DmNextOpenDatabase(currentP: DmOpenRef): DmOpenRef; syscall sysTrapDmNextOpenDatabase;

function DmOpenDatabaseInfo(dbP: DmOpenRef; var dbIDP: LocalID;
                            var openCountP, modeP, cardNoP: UInt16; var resDBP: Boolean): Err; syscall sysTrapDmOpenDatabaseInfo;

function DmGetAppInfoID(dbP: DmOpenRef): LocalID; syscall sysTrapDmGetAppInfoID;

procedure DmGetDatabaseLockState(dbR: DmOpenRef; var highest: UInt8; count, busy: UInt32); syscall sysTrapDmGetDatabaseLockState;

// Utility to unlock all records and clear busy bits
function DmResetRecordStates(dbP: DmOpenRef): Err; syscall sysTrapDmResetRecordStates;

// Error Query
function DmGetLastErr: Err; syscall sysTrapDmGetLastErr;

//------------------------------------------------------------
// Record based access routines
//------------------------------------------------------------

// Record Info
function DmNumRecords(dbP: DmOpenRef): UInt16; syscall sysTrapDmNumRecords;

function DmNumRecordsInCategory(dbP: DmOpenRef; category: UInt16): UInt16; syscall sysTrapDmNumRecordsInCategory;

function DmRecordInfo(dbP: DmOpenRef; index: UInt16;
                      var attrP: UInt16; var uniqueIDP: UInt32; var chunkIDP: LocalID): Err; syscall sysTrapDmRecordInfo;

function DmSetRecordInfo(dbP: DmOpenRef; index: UInt16;
                         var attrP: UInt16; var uniqueIDP: UInt32): Err; syscall sysTrapDmSetRecordInfo;

// Record attaching and detaching
function DmAttachRecord(dbP: DmOpenRef; var atP: UInt16; newH: MemHandle; var oldHP: MemHandle): Err; syscall sysTrapDmAttachRecord;

function DmDetachRecord(dbP: DmOpenRef; index: UInt16; var oldHP: MemHandle): Err; syscall sysTrapDmDetachRecord;

function DmMoveRecord(dbP: DmOpenRef; from, to_: UInt16): Err; syscall sysTrapDmMoveRecord;

// Record creation and deletion
function DmNewRecord(dbP: DmOpenRef; var atP: UInt16; size: UInt32): MemHandle; syscall sysTrapDmNewRecord;

function DmRemoveRecord(dbP: DmOpenRef; index: UInt16): Err; syscall sysTrapDmRemoveRecord;

function DmDeleteRecord(dbP: DmOpenRef; index: UInt16): Err; syscall sysTrapDmDeleteRecord;

function DmArchiveRecord(dbP: DmOpenRef; index: UInt16): Err; syscall sysTrapDmArchiveRecord;

function DmNewHandle(dbP: DmOpenRef; size: UInt32): MemHandle; syscall sysTrapDmNewHandle;

function DmRemoveSecretRecords(dbP: DmOpenRef): Err; syscall sysTrapDmRemoveSecretRecords;

// Record viewing manipulation
function DmFindRecordByID(dbP: DmOpenRef; uniqueID: UInt32; var indexP: UInt16): Err; syscall sysTrapDmFindRecordByID;

function DmQueryRecord(dbP: DmOpenRef; index: UInt16): MemHandle; syscall sysTrapDmQueryRecord;

function DmGetRecord(dbP: DmOpenRef; index: UInt16): MemHandle; syscall sysTrapDmGetRecord;

function DmQueryNextInCategory(dbP: DmOpenRef; var indexP: UInt16; category: UInt16): MemHandle; syscall sysTrapDmQueryNextInCategory;

function DmPositionInCategory(dbP: DmOpenRef; index, category: UInt16): UInt16; syscall sysTrapDmPositionInCategory;

function DmSeekRecordInCategory(dbP: DmOpenRef; var indexP: UInt16; offset: UInt16;
                                direction: Int16; category: UInt16): Err; syscall sysTrapDmSeekRecordInCategory;


function DmResizeRecord(dbP: DmOpenRef; index: UInt16; newSize: UInt32): MemHandle; syscall sysTrapDmResizeRecord;

function DmReleaseRecord(dbP: DmOpenRef; index: UInt16; dirty: Boolean): Err; syscall sysTrapDmReleaseRecord;

function DmSearchRecord(recH: MemHandle; var dbPP: DmOpenRef): UInt16; syscall sysTrapDmSearchRecord;

// Category manipulation
function DmMoveCategory(dbP: DmOpenRef; toCategory, fromCategory: UInt16; dirty: Boolean): Err; syscall sysTrapDmMoveCategory;

function DmDeleteCategory(dbR: DmOpenRef; categoryNum: UInt16): Err; syscall sysTrapDmDeleteCategory;

// Validation for writing
function DmWriteCheck(recordP: Pointer; offset, bytes: UInt32): Err; syscall sysTrapDmWriteCheck;

// Writing
function DmWrite(recordP: Pointer; offset: UInt32; const srcP: Pointer; bytes: UInt32): Err; syscall sysTrapDmWrite;

function DmStrCopy(recordP: Pointer; offset: UInt32; const srcP: PChar): Err; syscall sysTrapDmStrCopy;

function DmSet(recordP: Pointer; offset, bytes: UInt32; value: UInt8): Err; syscall sysTrapDmSet;

//------------------------------------------------------------
// Resource based access routines
//------------------------------------------------------------

// High level access routines
function DmGetResource(type_: DmResType; resID: DmResID): MemHandle; syscall sysTrapDmGetResource;

function DmGet1Resource(type_: DmResType; resID: DmResID): MemHandle; syscall sysTrapDmGet1Resource;

function DmReleaseResource(resourceH: MemHandle): Err; syscall sysTrapDmReleaseResource;

function DmResizeResource(resourceH: MemHandle; newSize: UInt32): MemHandle; syscall sysTrapDmResizeResource;

// Searching resource databases
function DmNextOpenResDatabase(dbP: DmOpenRef): DmOpenRef; syscall sysTrapDmNextOpenResDatabase;

function DmFindResourceType(dbP: DmOpenRef; resType: DmResType; typeIndex: UInt16): UInt16; syscall sysTrapDmFindResourceType;

function DmFindResource(dbP: DmOpenRef; resType: DmResType; resID: DmResID; resH: MemHandle): UInt16; syscall sysTrapDmFindResource;

function DmSearchResource(resType: DmResType; resID: DmResID; resH: MemHandle; var dbPP: DmOpenRef): UInt16; syscall sysTrapDmSearchResource;

// Resource Info
function DmNumResources(dbP: DmOpenRef): UInt16; syscall sysTrapDmNumResources;

function DmResourceInfo(dbP: DmOpenRef; index: UInt16;
                        var resTypeP: DmResType; var resIDP: DmResID;
                        var chunkLocalIDP: LocalID): Err; syscall sysTrapDmResourceInfo;

function DmSetResourceInfo(dbP: DmOpenRef; index: UInt16;
                           var resTypeP: DmResType; var resIDP: DmResID): Err; syscall sysTrapDmSetResourceInfo;

// Resource attaching and detaching
function DmAttachResource(dbP: DmOpenRef; newH: MemHandle;
                          resType: DmResType; resID: DmResID): Err; syscall sysTrapDmAttachResource;

function DmDetachResource(dbP: DmOpenRef; index: UInt16; var oldHP: MemHandle): Err; syscall sysTrapDmDetachResource;

// Resource creation and deletion
function DmNewResource(dbP: DmOpenRef; resType: DmResType; resID: DmResID; size: UInt32): MemHandle; syscall sysTrapDmNewResource;

function DmRemoveResource(dbP: DmOpenRef; index: UInt16): Err; syscall sysTrapDmRemoveResource;

// Resource manipulation
function DmGetResourceIndex(dbP: DmOpenRef; index: UInt16): MemHandle; syscall sysTrapDmGetResourceIndex;

// Record sorting
function DmQuickSort(dbP: DmOpenRef; compar: DmComparF; other: Int16): Err; syscall sysTrapDmQuickSort;

function DmInsertionSort(dbR: DmOpenRef; compar: DmComparF; other: Int16): Err; syscall sysTrapDmInsertionSort;

function DmFindSortPosition(dbP: DmOpenRef; newRecord: Pointer;
                            newRecordInfo: SortRecordInfoPtr; compar: DmComparF; other: Int16): UInt16; syscall sysTrapDmFindSortPosition;

function DmFindSortPositionV10(dbP: DmOpenRef; newRecord: Pointer; compar: DmComparF; other: Int16): UInt16; syscall sysTrapDmFindSortPositionV10;

implementation

end.
