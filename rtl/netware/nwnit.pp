{
  Netware Server Imports for FreePascal, contains definition from the
  following ndk header files:

  nit/nwaccntg.h nit/nwafp.h nit/nwbindry.h nit/nwdatamg.h nit/nwdir.h
  nit/nwenvrn.h nit/nwenvrn1.h nit/nwextatt.h nit/nwmsg.h nit/nwnit.h
  nit/nwqueue.h nit/nwserial.h nit/nwservst.h nit/nwsync.h nit/nwtts.h

  Initial Version 2002/02/22 Armin (diehl@nordrhein.de or armin@freepascal.org)

  The C-NDK and Documentation can be found here:
    http://developer.novell.com

  This program is distributed in the hope that it will be useful,but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.

  Do not blame Novell if there are errors in this file, instead
  contact me and i will se what i can do.

}

unit nwnit;

interface

{$mode objfpc}
{$packrecords C}

const
  Clib='clib';

function AccountingInstalled (fileServerID:word):longint;cdecl;external Clib name 'AccountingInstalled';
function GetAccountStatus    (binderyObjectType:word;
                              binderyObjectName:Pchar;
                              balance,limits,holds:Plongint):longint;cdecl;external Clib name 'GetAccountStatus';
function GetAccountStatus    (binderyObjectType:word;
                              binderyObjectName:Pchar;
                          var balance,limits,holds:longint):longint;cdecl;external Clib name 'GetAccountStatus';

function SubmitAccountCharge(binderyObjectType:word;
                             binderyObjectName:Pchar;
                             serviceType:word;
                             chargeAmount:longint;
                             cancelHoldAmount:longint;
                             commentType:word;
                             comment:Pchar):longint;cdecl;external Clib name 'SubmitAccountCharge';
function SubmitAccountChargeWithLength(
                             binderyObjectType:word;
                             binderyObjectName:Pchar;
                             serviceType:word;
                             chargeAmount:longint;
                             cancelHoldAmount:longint;
                             commentType:word;
                             commentData:pointer;
                             commentLength:word):longint;cdecl;external Clib name 'SubmitAccountChargeWithLength';
function SubmitAccountHold  (binderyObjectType:word;
                             binderyObjectName:Pchar;
                             reserveAmount:longint):longint;cdecl;external Clib name 'SubmitAccountHold';
function SubmitAccountNote  (binderyObjectType:word;
                             binderyObjectName:Pchar;
                             serviceType:word;
                             commentType:word;
                             comment:Pchar):longint;cdecl;external Clib name 'SubmitAccountNote';

{------------------------------------------------------------------------------}

{$include npackon.inc}

type
   PAFPFILEINFO = ^TAFPFILEINFO;
   TAFPFILEINFO = record
        entryID            : longint;
        parentID           : longint;
        attributes         : word;
        dataForkLength     : longint;
        resourceForkLength : longint;
        numOffspring       : word;
        creationDate       : word;
        accessDate         : word;
        modifyDate         : word;
        modifyTime         : word;
        backupDate         : word;
        backupTime         : word;
        finderInfo         : array[0..31] of byte;
        longName           : array[0..32] of char;
        pad1               : char;
        ownerID            : longint;
        shortName          : array[0..12] of char;
        pad2               : char;
        accessPrivileges   : word;
        proDosInfo         : array[0..5] of byte;
     end;

   PAFPSETINFO = ^TAFPSETINFO;
   TAFPSETINFO = record
        attributes   : word;
        creationDate : word;
        accessDate   : word;
        modifyDate   : word;
        modifyTime   : word;
        backupDate   : word;
        backupTime   : word;
        finderInfo   : array[0..31] of byte;
        proDosInfo   : array[0..5] of byte;
     end;
{$include npackoff.inc}

function AFPAllocTemporaryDirHandle (connectionID:word;
                                     volumeNum:byte;
                                     AFPEntryID:longint;
                                     AFPPathString:Pchar;
                                     NetWareDirectoryHandle:PBYTE;
                                     AccessRights:PBYTE):longint;  cdecl;external Clib name 'AFPAllocTemporaryDirHandle';

function AFPCreateDirectory (connectionID:word;
                             volumeNum:byte;
                             AFPEntryID:longint;
                             finderInfo:PBYTE;
                             AFPPathString:Pchar;
                             newAFPEntryID:Plongint):longint;cdecl;external Clib name 'AFPCreateDirectory';

function AFPCreateFile (connectionID:word;
                        volumeNum:byte;
                        AFPEntryID:longint;
                        deleteExistingFile:byte;
                        finderInfo:PBYTE;
                        AFPPathString:Pchar;
                        newAFPEntryID:Plongint):longint;  cdecl;external Clib name 'AFPCreateFile';

function AFPDelete (connectionID:word;
                    volumeNum:byte;
                    AFPEntryID:longint;
                    AFPPathString:Pchar):longint;  cdecl;external Clib name 'AFPDelete';

function AFPDirectoryEntry (connectionID:word;
                            directoryHandle:byte;
                            pathName:Pchar):longint;  cdecl;external Clib name 'AFPDirectoryEntry';

function AFPGetEntryIDFromName (connectionID:word;
                                volumeNum:byte;
                                AFPEntryID:longint;
                                AFPPathString:Pchar;
                                newAFPEntryID:Plongint):longint;          cdecl;external Clib name 'AFPGetEntryIDFromName';

function AFPGetEntryIDFromNetWareHandle (connectionID:word;
                                         NetWareHandle:PBYTE;
                                         volumeID:PBYTE;
                                         AFPEntryID:Plongint;
                                         forkIndicator:PBYTE):longint;    cdecl;external Clib name 'AFPGetEntryIDFromNetWareHandle';

function AFPGetEntryIDFromNetWareHandle (connectionID:word;
                                         var NetWareHandle:byte;
                                         var volumeID:byte;
                                         var AFPEntryID:longint;
                                         var forkIndicator:byte):longint; cdecl;external Clib name 'AFPGetEntryIDFromNetWareHandle';

function AFPGetEntryIDFromPathName (connectionID:word;
                                    directoryHandle:byte;
                                    pathName:Pchar;
                                    AFPEntryID:Plongint):longint;         cdecl;external Clib name 'AFPGetEntryIDFromPathName';

function AFPGetFileInformation     (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    requestBitMap:word;
                                    AFPPathString:Pchar;
                                    strucSize:word;
                                    AFPFileInfo:PAFPFILEINFO):longint;    cdecl;external Clib name 'AFPGetFileInformation';

function AFPGetFileInformation     (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    requestBitMap:word;
                                    AFPPathString:Pchar;
                                    strucSize:word;
                                var AFPFileInfo:TAFPFILEINFO):longint;    cdecl;external Clib name 'AFPGetFileInformation';

function AFPOpenFileFork           (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    forkIndicator:byte;
                                    accessMode:byte;
                                    AFPPathString:Pchar;
                                    fileID:Plongint;
                                    forkLength:Plongint;
                                    NetWareHandle:PBYTE;
                                    fileHandle:Plongint):longint;         cdecl;external Clib name 'AFPOpenFileFork';

function AFPOpenFileFork           (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    forkIndicator:byte;
                                    accessMode:byte;
                                    AFPPathString:Pchar;
                                    var fileID:longint;
                                    var forkLength:longint;
                                    var NetWareHandle:byte;
                                    var fileHandle:longint):longint;         cdecl;external Clib name 'AFPOpenFileFork';


function AFPRename                 (connectionID:word;
                                    volumeNum:byte;
                                    AFPSourceEntryID:longint;
                                    AFPDestEntryID:longint;
                                    AFPSourcePath:Pchar;
                                    AFPDestPath:Pchar):longint;              cdecl;external Clib name 'AFPRename';

function AFPScanFileInformation    (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    AFPLastSeenID:Plongint;
                                    searchBitMap:word;
                                    requestBitMap:word;
                                    AFPPathString:Pchar;
                                    strucSize:word;
                                    AFPScanFileInfo:PAFPFILEINFO):longint;    cdecl;external Clib name 'AFPScanFileInformation';
function AFPScanFileInformation    (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    AFPLastSeenID:Plongint;
                                    searchBitMap:word;
                                    requestBitMap:word;
                                    AFPPathString:Pchar;
                                    strucSize:word;
                                var AFPScanFileInfo:TAFPFILEINFO):longint;    cdecl;external Clib name 'AFPScanFileInformation';

function AFPSetFileInformation     (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    requestBitMap:word;
                                    AFPPathString:Pchar;
                                    strucSize:word;
                                    AFPSetInfo:PAFPSETINFO):longint;          cdecl;external Clib name 'AFPSetFileInformation';

function AFPSetFileInformation     (connectionID:word;
                                    volumeNum:byte;
                                    AFPEntryID:longint;
                                    requestBitMap:word;
                                    AFPPathString:Pchar;
                                    strucSize:word;
                                var AFPSetInfo:TAFPSETINFO):longint;          cdecl;external Clib name 'AFPSetFileInformation';

function AFPSupported              (connectionID:word):longint;               cdecl;external Clib name 'AFPSupported';


{------------------------------------------------------------------------------}
const
   BS_ANY_READ        = $0000;
   BS_ANY_WRITE       = $0000;    // Writeable by anyone
   BS_LOGGED_READ     = $0001;    // Must be logged in to read
   BS_OBJECT_READ     = $0002;    // Readable by same object or super
   BS_SUPER_READ      = $0003;    // Readable by supervisor only
   BS_BINDERY_READ    = $0004;    // Readable only by the bindery
   BS_LOGGED_WRITE    = $0010;    // Must be logged in to write
   BS_OBJECT_WRITE    = $0020;    // Writeable by same object or super
   BS_SUPER_WRITE     = $0030;    // Writeable only by the supervisor
   BS_BINDERY_WRITE   = $0040;    // Writeable by the bindery only

   // Bindery object type definitions
   OT_WILD = -(1);

   OT_UNKNOWN                     = $0000;
   OT_USER                        = $0001;
   OT_USER_GROUP                  = $0002;
   OT_GROUP                       = $0002;
   OT_PRINT_QUEUE                 = $0003;

   OT_FILE_SERVER                 = $0004;
   OT_JOB_SERVER                  = $0005;
   OT_GATEWAY                     = $0006;
   OT_PRINT_SERVER                = $0007;
   OT_ARCHIVE_QUEUE               = $0008;

   OT_ARCHIVE_SERVER              = $0009;
   OT_JOB_QUEUE                   = $000A;
   OT_ADMINISTRATION              = $000B;
   OT_NAS_SNA_GATEWAY             = $0021;
   OT_REMOTE_BRIDGE_SERVER        = $0024;
   OT_TCPIP_GATEWAY               = $0027;
   OT_TIME_SYNCHRONIZATION_SERVER = $002D;
   OT_ARCHIVE_SERVER_DYNAMIC_SAP  = $002E;
   OT_ADVERTISING_PRINT_SERVER    = $0047;
   OT_BTRIEVE_VAP                 = $004B;
   OT_NWSQL_VAP                   = $004C;
   OT_PRINT_QUEUE_USER            = $0053;

   // Attributes of objects and properties in the bindery
   BF_STATIC                      = $0000;
   BF_DYNAMIC                     = $0001;
   BF_ITEM                        = $0000;
   BF_SET                         = $0002;

   BL_OBJECT                      = 48;     // Maximum lengths of object, properties, (includes terminating null)
   BL_PROPERTY                    = 16;
   BL_PASSWORD                    = 128;

function AddBinderyObjectToSet (objectName   : Pchar;
                                objectType   : word;
                                propertyName : Pchar;
                                memberName   : Pchar;
                                memberType   : word):longint;  cdecl;external Clib name 'AddBinderyObjectToSet';

function ChangeBinderyObjectPassword (objectName  : Pchar;
                                      objectType  : word;
                                      oldPassword : Pchar;
                                      newPassword : Pchar):longint;  cdecl;external Clib name 'ChangeBinderyObjectPassword';

function ChangeBinderyObjectSecurity (objectName  : Pchar;
                                      objectType  : word;
                                      newObjectSecurity : byte):longint;  cdecl;external Clib name 'ChangeBinderyObjectSecurity';


function ChangePropertySecurity (objectName         : Pchar;
                                 objectType         : word;
                                 propertyName       : Pchar;
                                 newPropertySecurity: byte):longint;  cdecl;external Clib name 'ChangePropertySecurity';

function CloseBindery:longint;  cdecl;external Clib name 'CloseBindery';
function CreateBinderyObject    (objectName         : Pchar;
                                 objectType         : word;
                                 objectFlag         : byte;
                                 objectSecurity     : byte):longint;       cdecl;external Clib name 'CreateBinderyObject';
function CreateProperty         (objectName         : Pchar;
                                 objectType         : word;
                                 propertyName       : Pchar;
                                 propertyFlags      : byte;
                                 propertySecurity   : byte):longint;     cdecl;external Clib name 'CreateProperty';

function DeleteBinderyObject    (objectName         : Pchar;
                                 objectType         : word):longint;           cdecl;external Clib name 'DeleteBinderyObject';

function DeleteBinderyObjectFromSet (objectName:Pchar;
                                     objectType:word;
                                     propertyName:Pchar;
                                     memberName:Pchar;
                                     memberType:word):longint;       cdecl;external Clib name 'DeleteBinderyObjectFromSet';

function DeleteProperty             (objectName:Pchar;
                                     objectType:word;
                                     propertyName:Pchar):longint;    cdecl;external Clib name 'DeleteProperty';
function GetBinderyAccessLevel      (accessLevel:PBYTE;
                                     objectID:Plongint):longint;     cdecl;external Clib name 'GetBinderyAccessLevel';
function GetBinderyAccessLevel      (var accessLevel:byte;
                                     var objectID:longint):longint;  cdecl;external Clib name 'GetBinderyAccessLevel';

function GetBinderyObjectID         (objectName:Pchar;
                                     objectType:word;
                                     objectID:Plongint):longint;     cdecl;external Clib name 'GetBinderyObjectID';
function GetBinderyObjectID         (objectName:Pchar;
                                     objectType:word;
                                 var objectID:longint):longint;      cdecl;external Clib name 'GetBinderyObjectID';
function GetBinderyObjectName       (objectID:longint;
                                     objectName:Pchar;
                                     objectType:PWORD):longint;      cdecl;external Clib name 'GetBinderyObjectName';
function GetBinderyObjectName       (objectID:longint;
                                     objectName:Pchar;
                                 var objectType:word):longint;       cdecl;external Clib name 'GetBinderyObjectName';

function IsBinderyObjectInSet       (objectName:Pchar;
                                     objectType:word;
                                     propertyName:Pchar;
                                     memberName:Pchar;
                                     memberType:word):longint;       cdecl;external Clib name 'IsBinderyObjectInSet';
function OpenBindery:longint;                                        cdecl;external Clib name 'OpenBindery';
function ReadPropertyValue          (objectName   : Pchar;
                                     objectType   : word;
                                     propertyName : Pchar;
                                     segmentNumber: longint;
                                     propertyValue: PBYTE;
                                     moreSegments : PBYTE;
                                     propertyFlags: PBYTE):longint;   cdecl;external Clib name 'ReadPropertyValue';
function ReadPropertyValue          (objectName   : Pchar;
                                     objectType   : word;
                                     propertyName : Pchar;
                                     segmentNumber: longint;
                                 var propertyValue: byte;
                                 var moreSegments : byte;
                                 var propertyFlags: byte):longint;   cdecl;external Clib name 'ReadPropertyValue';

function RenameBinderyObject        (objectName   : Pchar;
                                     newObjectName: Pchar;
                                     objectType   : word):longint;   cdecl;external Clib name 'RenameBinderyObject';

function ScanBinderyObject          (searchObjectName:Pchar;
                                     searchObjectType:word;
                                     objectID:Plongint;
                                     objectName:Pchar;
                                     objectType:PWORD;
                                     objectHasProperties:Pchar;
                                     objectFlag:Pchar;
                                     objectSecurity:Pchar):longint;   cdecl;external Clib name 'ScanBinderyObject';
function ScanBinderyObject          (searchObjectName:Pchar;
                                     searchObjectType:word;
                                 var objectID:longint;
                                     objectName:Pchar;
                                 var objectType:word;
                                     objectHasProperties:Pchar;
                                     objectFlag:Pchar;
                                     objectSecurity:Pchar):longint;   cdecl;external Clib name 'ScanBinderyObject';

function ScanProperty               (objectName:Pchar;
                                     objectType:word;
                                     searchPropertyName:Pchar;
                                     sequenceNumber:Plongint;
                                     propertyName:Pchar;
                                     propertyFlags:Pchar;
                                     propertySecurity:Pchar;
                                     propertyHasValue:Pchar;
                                     moreProperties:Pbyte):longint;   cdecl;external Clib name 'ScanProperty';
function ScanProperty               (objectName:Pchar;
                                     objectType:word;
                                     searchPropertyName:Pchar;
                                 var sequenceNumber:longint;
                                     propertyName:Pchar;
                                     propertyFlags:Pchar;
                                     propertySecurity:Pchar;
                                     propertyHasValue:Pchar;
                                 var moreProperties:byte):longint;   cdecl;external Clib name 'ScanProperty';


function VerifyBinderyObjectPassword (objectName : Pchar;
                                      objectType : word;
                                      password   : Pchar):longint;       cdecl;external Clib name 'VerifyBinderyObjectPassword';

function WritePropertyValue          (objectName    : Pchar;
                                      objectType    : word;
                                      propertyName  : Pchar;
                                      segmentNumber : longint;
                                      propertyValue : PBYTE;
                                      moreSegments  : byte):longint;        cdecl;external Clib name 'WritePropertyValue';
function WritePropertyValue          (objectName    : Pchar;
                                      objectType    : word;
                                      propertyName  : Pchar;
                                      segmentNumber : longint;
                                  var propertyValue : byte;
                                      moreSegments  : byte):longint;        cdecl;external Clib name 'WritePropertyValue';


{------------------------------------------------------------------------------}
const
   ERR_INVALID_SUPPORT_MODULE_ID         = 240;
   ERR_SUPPORT_MODULE_ALREADY_REGISTERED = 241;
   ERR_SUPPORT_MODULE_CREATE_FAILED      = 242;
   ERR_SUPPORT_MODULE_CLOSE_FAILED       = 243;
   ERR_SM_WRITE_NO_SPACE                 = 244;
   ERR_SM_WRITE_IO_ERROR                 = 245;
   ERR_SM_READ_IO_ERROR                  = 246;
   ERR_SUPPORT_MODULE_OPEN_FAILED        = 247;
   ERR_SUPPORT_MODULE_DELETE_FAILED      = 248;
   MaximumNumberOfDataStreams            = 3;
{$include npackon.inc}
(*
TInfo0Rep = record
   rIOStatus : longint;
   rInfoBlockSize: longint;
   rAvailSpace: longint;
   rUsedSpace: longint;
   rSMString: byte;
end;

TInfo1Rep = record
   rSMRegs : longint;
end;

TInfo2Rep = record
   NameLength : byte;
end;
*)

type
   PSUPPORT_MODULE_INFO = ^TSUPPORT_MODULE_INFO;
   TSUPPORT_MODULE_INFO = record
        IOStatus      : longint;
        InfoBlockSize : longint;
        AvailSpace    : longint;
        UsedSpace     : longint;
        SMString      : char; // 128 length limit, Info block follows string
     end;

{$include npackoff.inc}


function NWDeRegisterDMSupportModule    (SupportModuleID:longint;
                                         SupportModuleName:PChar;
                                         SlotNumber:longint):longint;  cdecl;external Clib name 'NWDeRegisterDMSupportModule';
function NWDeRegisterRTDataMigrationNLM (Station:longint;
                                         DMTAG:PBYTE;
                                         ForceFlag:longint):longint;   cdecl;external Clib name 'NWDeRegisterRTDataMigrationNLM';
function NWDeRegisterRTDataMigrationNLM (Station:longint;
                                     var DMTAG:byte;
                                         ForceFlag:longint):longint;   cdecl;external Clib name 'NWDeRegisterRTDataMigrationNLM';
{ Local and Remote Call  }
procedure NWGetDataMigratorInfo         (DMPresentFlag:PLongint;
                                         majorVersion:PLongint;
                                         minorVersion:PLongint;
                                         numberOfSupportModules:PLongint); cdecl;external Clib name 'NWGetDataMigratorInfo';
procedure NWGetDataMigratorInfo         (var DMPresentFlag:longint;
                                         var majorVersion:longint;
                                         var minorVersion:longint;
                                         var numberOfSupportModules:longint); cdecl;external Clib name 'NWGetDataMigratorInfo';
{ Local and Remote call  }
function NWGetDefaultSupportModule      (defaultSupportModuleID:PLongint):longint; cdecl;external Clib name 'NWGetDefaultSupportModule';
function NWGetDefaultSupportModule      (var defaultSupportModuleID:longint):longint; cdecl;external Clib name 'NWGetDefaultSupportModule';

{ Local and Remote call  }
function NWGetDMFileInfo (path:Pchar;
                          nameSpace:longint;
                          supportModuleID:PLongint;
                          validDataStreams:PLongint;
                          estRetrievalTime:PLongint;
                          info:PLongint):longint;cdecl;external Clib name 'NWGetDMFileInfo';
function NWGetDMFileInfo (path:Pchar;
                          nameSpace:longint;
                          var supportModuleID:longint;
                          var validDataStreams:longint;
                          var estRetrievalTime:longint;
                          var info:longint):longint;cdecl;external Clib name 'NWGetDMFileInfo';

{ Local and Remote call  }
function NWGetDMVolumeInfo (volume:longint;
                            supportModuleID:longint;
                            numberOfFilesMigrated:PLongint;
                            totalMigratedSize:PLongint;
                            spaceUsed:PLongint;
                            limboUsed:PLongint;
                            spaceMigrated:PLongint;
                            filesLimbo:PLongint):longint; cdecl;external Clib name 'NWGetDMVolumeInfo';
function NWGetDMVolumeInfo (volume:longint;
                            supportModuleID:longint;
                            var numberOfFilesMigrated:longint;
                            var totalMigratedSize:longint;
                            var spaceUsed:longint;
                            var limboUsed:longint;
                            var spaceMigrated:longint;
                            var filesLimbo:longint):longint; cdecl;external Clib name 'NWGetDMVolumeInfo';
{ Local and Remote call  }
function NWGetSupportModuleInfo (informationLevel:longint;
                                 supportModuleID:longint;
                                 returnInfo:pointer;
                                 returnInfoLen:PLongint):longint;cdecl;external Clib name 'NWGetSupportModuleInfo';
function NWGetSupportModuleInfo (informationLevel:longint;
                                 supportModuleID:longint;
                                 returnInfo:pointer;
                                 var returnInfoLen:longint):longint;cdecl;external Clib name 'NWGetSupportModuleInfo';

function NWIsDataMigrationAllowed (Volume:longint):longint; cdecl;external Clib name 'NWIsDataMigrationAllowed';
{ Local and Remote call  }
function NWMoveFileFromDM         (path:Pchar;
                                   nameSpace:longint):longint; cdecl;external Clib name 'NWMoveFileFromDM';
{ Local and Remote call  }
function NWMoveFileToDM           (path:Pchar;
                                   nameSpace:longint;
                                   SupportModuleID:longint;
                                   flags:longint):longint; cdecl;external Clib name 'NWMoveFileToDM';

function NWPeekFileData           (path:Pchar;
                                   nameSpace:longint;
                                   noWaitFlag:longint;
                                   startingSector:longint;
                                   sectorsToRead:longint;
                                   buffer:PBYTE;
                                   sectorsRead:PLongint;
                                   bytesRead:PLongint;
                                   NoWaitReason:PLongint):longint; cdecl;external Clib name 'NWPeekFileData';
function NWPeekFileData           (path:Pchar;
                                   nameSpace:longint;
                                   noWaitFlag:longint;
                                   startingSector:longint;
                                   sectorsToRead:longint;
                                   var buffer;
                                   var sectorsRead:longint;
                                   var bytesRead:longint;
                                   var NoWaitReason:longint):longint; cdecl;external Clib name 'NWPeekFileData';
type TFunction = function : longint; cdecl;

function NWRegisterDMSupportModule (ioFlag:longint;
                                    addr:array of TFunction;
                                    SupportModuleName:PBYTE;
                                    SupportModuleID:longint;
                                    MaxSectorsXF:longint;
                                    SlotNumber:PLongint):longint; cdecl;external Clib name 'NWRegisterDMSupportModule';

function NWRegisterRTDataMigrationNLM (Station:longint;
                                       addr:array of TFunction;
                                       DMTAG:PBYTE; majorVersion:longint;
                                       minorVersion:longint):longint; cdecl;external Clib name 'NWRegisterRTDataMigrationNLM';
{ Local and Remote call  }
function NWSetDefaultSupportModule    (newSupportModuleID:longint;
                                       currentSupportModuleID:PLongint):longint; cdecl;external Clib name 'NWSetDefaultSupportModule';
function NWSetDefaultSupportModule    (newSupportModuleID:longint;
                                   var currentSupportModuleID:longint):longint; cdecl;external Clib name 'NWSetDefaultSupportModule';

{------------------------------------------------------------------------------}
{ Trustee Access Rights in a network directory  }
const
   TA_NONE          = $0000;
   TA_READ          = $0001;
   TA_WRITE         = $0002;
   TA_CREATE        = $0008;
   TA_DELETE        = $0010;
   TA_ACCESSCONTROL = $0020;
   TA_SEEFILES      = $0040;
   TA_MODIFY        = $0080;
   TA_SUPERVISOR    = $0100;
   TA_ALL           = $01FB;

{ Old names for rights  }
   TA_OPEN          = $0004;
   TA_OWNERSHIP     = $0020;
   TA_SEARCH        = $0040;
   BYTES_PER_SECTOR = 512;

{ define volume types  }
   VINetWare386     = 0;
   VINetWare286     = 1;
   VINetWare386v30  = 2;
   VINetWare386v31  = 3;
{ define the extended volume information status flag bits  }
   NWSubAllocEnabledBit    = $01;
   NWCompressionEnabledBit = $02;
   NWMigrationEnabledBit   = $04;
   NWAuditingEnabledBit    = $08;
   NWReadOnlyEnabledBit    = $10;

{$include npackon.inc}
type
   PAnswerStructure = ^TAnswerStructure;
   TAnswerStructure = record
     ALevelNumber   : byte;
     AMaximumAmount : longint;  // ?? TMisalignedLONG;
     ACurrentAmount : longint;  // ?? TMisalignedLONG;
   end;


   PVOLUME_STATS = ^TVOLUME_STATS;
   TVOLUME_STATS = record
        systemElapsedTime : longint;
        volumeNumber : byte;
        logicalDriveNumber : byte;
        sectorsPerBlock : word;
        startingBlock : longint;
        totalBlocks : word;
        availableBlocks : word;
        totalDirectorySlots : word;
        availableDirectorySlots : word;
        maxDirectorySlotsUsed : word;
        isHashing : byte;
        isRemovable : byte;
        isMounted : byte;
        volumeName : array[0..16] of char;
        purgableBlocks : longint;
        notYetPurgableBlocks : longint;
     end;

   PVOLUME_INFO = ^TVOLUME_INFO;
   TVOLUME_INFO = record
        systemElapsedTime : longint;
        volumeNumber : byte;
        logicalDriveNumber : byte;
        sectorsPerBlock : word;
        startingBlock : smallint;
        totalBlocks : longint;
        availableBlocks : longint;
        totalDirectorySlots : longint;
        availableDirectorySlots : longint;
        isHashing : byte;
        isRemovable : byte;
        isMounted : byte;
        volumeName : array[0..16] of char;
        purgableBlocks : longint;
        notYetPurgableBlocks : longint;
     end;

   PNWVolExtendedInfo = ^TNWVolExtendedInfo;
   TNWVolExtendedInfo = record
        volType : longint;
        statusFlag : longint;
        sectorSize : longint;
        sectorsPerCluster : longint;
        volSizeInClusters : longint;
        freeClusters : longint;
        subAllocFreeableClusters : longint;
        freeableLimboSectors : longint;
        nonfreeableLimboSectors : longint;
        availSubAllocSectors : longint;
        nonuseableSubAllocSectors : longint;
        subAllocClusters : longint;
        numDataStreams : longint;
        numLimboDataStreams : longint;
        oldestDelFileAgeInTicks : longint;
        numCompressedDataStreams : longint;
        numCompressedLimboDataStreams : longint;
        numNoncompressibleDataStreams : longint;
        precompressedSectors : longint;
        compressedSectors : longint;
        numMigratedDataStreams : longint;
        migratedSectors : longint;
        clustersUsedByFAT : longint;
        clustersUsedByDirs : longint;
        clustersUsedByExtDirs : longint;
        totalDirEntries : longint;
        unusedDirEntries : longint;
        totalExtDirExtants : longint;
        unusedExtDirExtants : longint;
        extAttrsDefined : longint;
        extAttrExtantsUsed : longint;
        directoryServicesObjectID : longint;
        lastModifiedDateAndTime : longint;
     end;

{$include npackoff.inc}

function AddSpaceRestrictionForDirectory (pathName:Pchar;
                                          value:longint;
                                          allowWildCardsFlag:longint):longint;cdecl;external Clib name 'AddSpaceRestrictionForDirectory';
function AddTrustee                      (pathName:Pchar;
                                          trusteeID:longint;
                                          newRights:word):longint; cdecl;external Clib name 'AddTrustee';
function AddUserSpaceRestriction         (volume:longint;
                                          trusteeID:longint;
                                          value:longint):longint;cdecl;external Clib name 'AddUserSpaceRestriction';
function ChangeDirectoryEntry            (pathName:Pchar;
                                          modifyVector:pointer;  //PModifyStructure;
                                          modifyBits:longint;
                                          allowWildCardsFlag:longint):longint;cdecl;external Clib name 'ChangeDirectoryEntry';
function ChangeDirectoryEntry            (pathName:Pchar;
                                      var modifyVector;
                                          modifyBits:longint;
                                          allowWildCardsFlag:longint):longint;cdecl;external Clib name 'ChangeDirectoryEntry';
function ConvertNameToFullPath           (partialPath:Pchar;
                                          fullPath:Pchar):longint;cdecl;external Clib name 'ConvertNameToFullPath';
function ConvertNameToVolumePath         (fileName:Pchar;
                                          volumePath:Pchar):longint;cdecl;external Clib name 'ConvertNameToVolumePath';
function DeleteTrustee                   (pathName:Pchar;
                                          trusteeID:longint):longint;cdecl;external Clib name 'DeleteTrustee';
function DeleteUserSpaceRestriction      (volume:longint;
                                          trusteeID:longint):longint;cdecl;external Clib name 'DeleteUserSpaceRestriction';
function GetAvailableUserDiskSpace       (pathName:Pchar;
                                          availableSpace:PLongint):longint;cdecl;external Clib name 'GetAvailableUserDiskSpace';
function GetAvailableUserDiskSpace       (pathName:Pchar;
                                      var availableSpace:longint):longint;cdecl;external Clib name 'GetAvailableUserDiskSpace';
function GetDiskSpaceUsedByObject        (trusteeID:longint;
                                          volume:longint;
                                          usedSpace:PLongint):longint;cdecl;external Clib name 'GetDiskSpaceUsedByObject';
function GetDiskSpaceUsedByObject        (trusteeID:longint;
                                          volume:longint;
                                      var usedSpace:longint):longint;cdecl;external Clib name 'GetDiskSpaceUsedByObject';
function GetEffectiveRights              (pathName:Pchar;
                                          accessRights:PWORD):longint;cdecl;external Clib name 'GetEffectiveRights';
function GetEffectiveRights              (pathName:Pchar;
                                      var accessRights:word):longint;cdecl;external Clib name 'GetEffectiveRights';
function GetMaximumUserSpaceRestriction  (trusteeID, volume:longint; maxRestriction:PLongint):longint;cdecl;external Clib name 'GetMaximumUserSpaceRestriction';
function GetMaximumUserSpaceRestriction  (trusteeID, volume:longint; var maxRestriction:longint):longint;cdecl;external Clib name 'GetMaximumUserSpaceRestriction';
function GetNumberOfVolumes : longint; cdecl;external Clib name 'GetNumberOfVolumes';
function GetVolumeInformation            (connectionID:word;
                                          volumeNumber:byte;
                                          structSize:longint;
                                          volumeStatistics:PVOLUME_STATS):longint;cdecl;external Clib name 'GetVolumeInformation';
function GetVolumeInformation            (connectionID:word;
                                          volumeNumber:byte;
                                          structSize:longint;
                                      var volumeStatistics:TVOLUME_STATS):longint;cdecl;external Clib name 'GetVolumeInformation';
function GetVolumeInfoWithNumber         (volumeNumber:byte;
                                          volumeName:Pchar;
                                          totalBlocks:PWORD;
                                          sectorsPerBlock:PWORD;
                                          availableBlocks:PWORD;
                                          totalDirectorySlots:PWORD;
                                          availableDirectorySlots:PWORD;
                                          volumeIsRemovable:PWORD):longint;cdecl;external Clib name 'GetVolumeInfoWithNumber';
function GetVolumeInfoWithNumber         (volumeNumber:byte;
                                          volumeName:Pchar;
                                      var totalBlocks:word;
                                      var sectorsPerBlock:word;
                                      var availableBlocks:word;
                                      var totalDirectorySlots:word;
                                      var availableDirectorySlots:word;
                                      var volumeIsRemovable:word):longint;cdecl;external Clib name 'GetVolumeInfoWithNumber';
function GetVolumeName(volumeNumber:longint; volumeName:Pchar):longint;cdecl;external Clib name 'GetVolumeName';

function GetVolumeNumber(volumeName:Pchar; volumeNumber:Plongint):longint;cdecl;external Clib name 'GetVolumeNumber';
function GetVolumeNumber(volumeName:Pchar; var volumeNumber:longint):longint;cdecl;external Clib name 'GetVolumeNumber';
function GetVolumeStatistics             (connectionID:word;
                                          volumeNumber:byte;
                                          structSize:longint;
                                          volumeStatistics:PVOLUME_INFO):longint;cdecl;external Clib name 'GetVolumeStatistics';
function GetVolumeStatistics             (connectionID:word;
                                          volumeNumber:byte;
                                          structSize:longint;
                                      var volumeStatistics:TVOLUME_INFO):longint;cdecl;external Clib name 'GetVolumeStatistics';

procedure _makepath                      (path:Pchar;
                                          drive:Pchar;
                                          dir:Pchar;
                                          fname:Pchar;
                                          ext:Pchar);cdecl;external Clib name '_makepath';

function ModifyInheritedRightsMask       (path:Pchar;
                                          revokeRightsMask:word;
                                          grantRightsMask:word):longint;cdecl;external Clib name 'ModifyInheritedRightsMask';
function NWGetExtendedVolumeInfo         (volNumber:longint;
                                          volName:Pchar;
                                          volInfo:PNWVolExtendedInfo):longint;cdecl;external Clib name 'NWGetExtendedVolumeInfo';
function NWGetExtendedVolumeInfo         (volNumber:longint;
                                          volName:Pchar;
                                      var volInfo:TNWVolExtendedInfo):longint;cdecl;external Clib name 'NWGetExtendedVolumeInfo';
function NWVolumeIsCDROM                 (volNumber:longint;
                                          isCDROM:PLongint):longint;cdecl;external Clib name 'NWVolumeIsCDROM';
function NWVolumeIsCDROM                 (volNumber:longint;
                                      var isCDROM:longint):longint;cdecl;external Clib name 'NWVolumeIsCDROM';

function ParsePath                       (path:Pchar;
                                          server:Pchar;
                                          volume:Pchar;
                                          directories:Pchar):longint;cdecl;external Clib name 'ParsePath';
function PurgeTrusteeFromVolume          (volume:longint;
                                          trusteeID:longint):longint;cdecl;external Clib name 'PurgeTrusteeFromVolume';

function ReturnSpaceRestrictionForDirectory(pathName:Pchar;
                                            numberOfStructuresToReturn:longint;
                                            answerBuffer:pointer;
                                            numberOfStructuresReturned:PLongint):longint;cdecl;external Clib name 'ReturnSpaceRestrictionForDirectory';
function ScanBinderyObjectTrusteePaths     (objectID:longint;
                                            volumeNumber:byte;
                                            sequenceNumber:Plongint;
                                            trusteeAccessMask:PWORD;
                                            trusteePathName:Pchar):longint;cdecl;external Clib name 'ScanBinderyObjectTrusteePaths';
function ScanBinderyObjectTrusteePaths     (objectID:longint;
                                            volumeNumber:byte;
                                        var sequenceNumber:longint;
                                        var trusteeAccessMask:word;
                                            trusteePathName:Pchar):longint;cdecl;external Clib name 'ScanBinderyObjectTrusteePaths';
(* Const before type ignored *)
function ScanTrustees                      (pathName:Pchar;
                                            startingOffset:longint;
                                            vectorSize:longint;
                                            trusteeVector:PLongint;
                                            maskVector:PWORD;
                                            actualVectorSize:PLongint):longint;cdecl;external Clib name 'ScanTrustees';
function ScanTrustees                      (pathName:Pchar;
                                            startingOffset:longint;
                                            vectorSize:longint;
                                        var trusteeVector:longint;
                                        var maskVector:longint;
                                        var actualVectorSize:longint):longint;cdecl;external Clib name 'ScanTrustees';
function ScanUserSpaceRestrictions         (volume:longint;
                                            sequenceNumber:PLongint;
                                            numberOfTrusteesToReturn:longint;
                                            answerArea:PLongint;
                                            numberOfTrusteesReturned:PLongint):longint;cdecl;external Clib name 'ScanUserSpaceRestrictions';
function ScanUserSpaceRestrictions         (volume:longint;
                                        var sequenceNumber:longint;
                                            numberOfTrusteesToReturn:longint;
                                        var answerArea;
                                        var numberOfTrusteesReturned:longint):longint;cdecl;external Clib name 'ScanUserSpaceRestrictions';

function SetDirectoryInfo                  (pathName:Pchar;
                                            newCreationDateAndTime:PBYTE;
                                            newOwnerObjectID:longint;
                                            inheritedRightsMask:word):longint;cdecl;external Clib name 'SetDirectoryInfo';

function SetWildcardTranslationMode         (newMode:byte):byte;cdecl;external Clib name 'SetWildcardTranslationMode';

procedure _splitpath                        (path:Pchar;
                                             drive:Pchar;
                                             dir:Pchar;
                                             fname:Pchar;
                                             ext:Pchar);cdecl;external Clib name '_splitpath';

function StripFileServerFromPath            (path:Pchar;
                                             server:Pchar):Pchar;cdecl;external Clib name 'StripFileServerFromPath';
function UpdateDirectoryEntry               (handle:longint):longint;cdecl;external Clib name 'UpdateDirectoryEntry';

{------------------------------------------------------------------------------}
const
   TYPE_NORMAL_SERVER  = 0;
   TYPE_IO_ENGINE      = 1;
   TYPE_MS_ENGINE      = 2;
   LOADER_TYPE_DOS     = 1;
   LOADER_TYPE_OS2     = 2;
   LOADER_TYPE_MSWIN31 = 3;
   RSUPER              = 3;
{$include npackon.inc}
type

   PFILE_SERV_INFO = ^TFILE_SERV_INFO;
   TFILE_SERV_INFO = record
        serverName : array[0..47] of char;
        netwareVersion : byte;
        netwareSubVersion : byte;
        maxConnectionsSupported : word;
        connectionsInUse : word;
        maxVolumesSupported : word;
        revisionLevel : byte;
        SFTLevel : byte;
        TTSLevel : byte;
        peakConnectionsUsed : word;
        accountingVersion : byte;
        VAPversion : byte;
        queingVersion : byte;
        printServerVersion : byte;
        virtualConsoleVersion : byte;
        securityRestrictionLevel : byte;
        internetBridgeSupport : byte;
        reserved : array[0..59] of byte;
        CLibMajorVersion : byte;
        CLibMinorVersion : byte;
        CLibRevision : byte;
     end;

{$include npackoff.inc}

function CheckConsolePrivileges:longint;cdecl;external Clib name 'CheckConsolePrivileges';
function CheckNetWareVersion (majorVersion,
                              minorVersion,
                              revisionNumber,
                              minimumSFTLevel,
                              minimumTTSLevel:word):longint;cdecl;external Clib name 'CheckNetWareVersion';
function ClearConnectionNumber(connectionNumber:word):longint;cdecl;external Clib name 'ClearConnectionNumber';
function DisableFileServerLogin:longint;cdecl;external Clib name 'DisableFileServerLogin';
function DisableTransactionTracking:longint;cdecl;external Clib name 'DisableTransactionTracking';
function DownFileServer(forceFlag:longint):longint;cdecl;external Clib name 'DownFileServer';
function EnableFileServerLogin:longint;cdecl;external Clib name 'EnableFileServerLogin';
function EnableTransactionTracking:longint;cdecl;external Clib name 'EnableTransactionTracking';
function GetBinderyObjectDiskSpaceLeft (connectionID:word;
                                        binderyObjectID:longint;
                                        systemElapsedTime:PLongint;
                                        unusedDiskBlocks:PLongint;
                                        restrictionEnforced:PBYTE):longint;cdecl;external Clib name 'GetBinderyObjectDiskSpaceLeft';
function GetBinderyObjectDiskSpaceLeft (connectionID:word;
                                        binderyObjectID:longint;
                                    var systemElapsedTime:Longint;
                                    var unusedDiskBlocks:Longint;
                                    var restrictionEnforced:byte):longint;cdecl;external Clib name 'GetBinderyObjectDiskSpaceLeft';
function GetDiskUtilization            (objectID:longint;
                                        volumeNumber:char;
                                        usedDirectories:PLongint;
                                        usedFiles:PLongint;
                                        usedBlocks:PLongint):longint;cdecl;external Clib name 'GetDiskUtilization';
function GetDiskUtilization            (objectID:longint;
                                        volumeNumber:char;
                                    var usedDirectories:Longint;
                                    var usedFiles:Longint;
                                    var usedBlocks:Longint):longint; cdecl;external Clib name 'GetDiskUtilization';

//procedure GetFileServerConnectionID(fileServerName:Pchar; connectionID:PWORD);cdecl;external Clib name 'GetFileServerConnectionID';
procedure GetFileServerDateAndTime(dateAndTime:PBYTE);cdecl;external Clib name 'GetFileServerDateAndTime';
procedure GetFileServerDateAndTime(var dateAndTime);cdecl;external Clib name 'GetFileServerDateAndTime';
function GetFileServerDescriptionStrings (company_Name:Pchar;
                                          revision:Pchar;
                                          revisionDate:Pchar;
                                          copyrightNotice:Pchar):longint;cdecl;external Clib name 'GetFileServerDescriptionStrings';
function GetFileServerLoginStatus        (loginEnabledFlag:PLongint):longint;cdecl;external Clib name 'GetFileServerLoginStatus';
function GetFileServerLoginStatus        (var loginEnabledFlag:Longint):longint;cdecl;external Clib name 'GetFileServerLoginStatus';
procedure GetFileServerName(connectionID:word; fileServerName:Pchar);cdecl;external Clib name 'GetFileServerName';
function GetServerConfigurationInfo      (serverType:PLongint;
                                          loaderType:PLongint):longint;cdecl;external Clib name 'GetServerConfigurationInfo';
function GetServerConfigurationInfo      (var serverType:Longint;
                                          var loaderType:Longint):longint;cdecl;external Clib name 'GetServerConfigurationInfo';
function GetServerInformation            (returnSize:longint;
                                          serverInfo:PFILE_SERV_INFO):longint;cdecl;external Clib name 'GetServerInformation';
function GetServerInformation            (returnSize:longint;
                                      var serverInfo:TFILE_SERV_INFO):longint;cdecl;external Clib name 'GetServerInformation';
function GetServerMemorySize:longint;cdecl;external Clib name 'GetServerMemorySize';
function GetServerUtilization:longint;cdecl;external Clib name 'GetServerUtilization';
function SendConsoleBroadcast(msg:Pchar;
                              connectionCount:word;
                              connectionList:PWORD):longint;cdecl;external Clib name 'SendConsoleBroadcast';
function SendConsoleBroadcast(msg:Pchar;
                              connectionCount:word;
                              const connectionList:array of word):longint;cdecl;external Clib name 'SendConsoleBroadcast';
function SetFileServerDateAndTime (year,month,day,hour,minute,second:word):longint;cdecl;external Clib name 'SetFileServerDateAndTime';


{------------------------------------------------------------------------------}
const
   ENVSERV_OVERHEAD_SIZE = 2 * cardinal(sizeof(WORD));
   ENVSERV_BUFFER1_SIZE = 512 + ENVSERV_OVERHEAD_SIZE;
   ENVSERV_CONN_TYPE_286 = 286;
   ENVSERV_CONN_TYPE_386 = 386;

{$include npackon.inc}
type

   PCONN_USAGE = ^TCONN_USAGE;
   TCONN_USAGE = record
        systemElapsedTime : longint;
        bytesRead : array[0..5] of byte;
        bytesWritten : array[0..5] of byte;
        totalRequestPackets : longint;
     end;

   PDISK_CACHE_STATS = ^TDISK_CACHE_STATS;
   TDISK_CACHE_STATS = record
        systemElapsedTime : longint;
        cacheBufferCount : word;
        cacheBufferSize : word;
        dirtyCacheBuffers : word;
        cacheReadRequests : longint;
        cacheWriteRequests : longint;
        cacheHits : longint;
        cacheMisses : longint;
        physicalReadRequests : longint;
        physicalWriteRequests : longint;
        physicalReadErrors : word;
        physicalWriteErrors : word;
        cacheGetRequests : longint;
        cacheFullWriteRequests : longint;
        cachePartialWriteRequests : longint;
        backgroundDirtyWrites : longint;
        backgroundAgedWrites : longint;
        totalCacheWrites : longint;
        cacheAllocations : longint;
        thrashingCount : word;
        LRUBlockWasDirtyCount : word;
        readBeyondWriteCount : word;
        fragmentedWriteCount : word;
        cacheHitOnUnavailCount : word;
        cacheBlockScrappedCount : word;
     end;


   PDISK_CHANNEL_STATS = ^TDISK_CHANNEL_STATS;
   TDISK_CHANNEL_STATS = record
        systemElapsedTime : longint;
        channelState : word;
        channelSyncState : word;
        driverType : byte;
        driverMajorVersion : byte;
        driverMinorVersion : byte;
        driverDescription : array[0..64] of char;
        IOAddr1 : word;
        IOAddr1Size : word;
        IOAddr2 : word;
        IOAddr2Size : word;
        sharedMem1Seg : array[0..2] of byte;
        sharedMem1Ofs : word;
        sharedMem2Seg : array[0..2] of byte;
        sharedMem2Ofs : word;
        interrupt1Used : byte;
        interrupt1 : byte;
        interrupt2Used : byte;
        interrupt2 : byte;
        DMAChannel1Used : byte;
        DMAChannel1 : byte;
        DMAChannel2Used : byte;
        DMAChannel2 : byte;
        reserved2 : word;
        configDescription : array[0..79] of char;
     end;


   PDRIVE_MAP_TABLE = ^TDRIVE_MAP_TABLE;
   TDRIVE_MAP_TABLE = record
        systemElapsedTime : longint;
        SFTLevel : byte;
        logicalDriveCount : byte;
        physicalDriveCount : byte;
        diskChannelTable : array[0..4] of byte;
        pendingIOCommands : word;
        mappingTable : array[0..31] of byte;
        driveMirrorTable : array[0..31] of byte;
        deadMirrorTable : array[0..31] of byte;
        remirroredDrive : byte;
        reserved : byte;
        remirroredBlock : longint;
        SFTErrorTable : array[0..59] of word;
     end;


   PSERVER_LAN_IO = ^TSERVER_LAN_IO;
   TSERVER_LAN_IO = record
        systemElapsedTime : longint;
        maxRoutingBuffersAvail : word;
        maxRoutingBuffersUsed : word;
        routingBuffersInUse : word;
        totalFileServicePackets : longint;
        fileServicePacketsBuffered : word;
        invalidConnPacketCount : word;
        badLogicalConnCount : word;
        packetsRcvdDuringProcCount : word;
        reprocessedRequestCount : word;
        badSequenceNumberPacketCount : word;
        duplicateReplyCount : word;
        acknowledgementsSent : word;
        badRequestTypeCount : word;
        attachDuringProcCount : word;
        attachWhileAttachingCount : word;
        forgedDetachRequestCount : word;
        badConnNumberOnDetachCount : word;
        detachDuringProcCount : word;
        repliesCanceledCount : word;
        hopCountDiscardCount : word;
        unknownNetDiscardCount : word;
        noDGroupBufferDiscardCount : word;
        outPacketNoBufferDiscardCount : word;
        IPXNotMyNetworkCount : word;
        NetBIOSPropagationCount : longint;
        totalOtherPackets : longint;
        totalRoutedPackets : longint;
     end;


   PSERVER_MISC_INFO = ^TSERVER_MISC_INFO;
   TSERVER_MISC_INFO = record
        systemElapsedTime : longint;
        processorType : byte;
        reserved : byte;
        serviceProcessCount : byte;
        serverUtilizationPercent : byte;
        maxBinderyObjectsAvail : word;
        maxBinderyObjectsUsed : word;
        binderyObjectsInUse : word;
        serverMemoryInK : word;
        serverWastedMemoryInK : word;
        dynamicAreaCount : word;
        dynamicSpace1 : longint;
        maxUsedDynamicSpace1 : longint;
        dynamicSpaceInUse1 : longint;
        dynamicSpace2 : longint;
        maxUsedDynamicSpace2 : longint;
        dynamicSpaceInUse2 : longint;
        dynamicSpace3 : longint;
        maxUsedDynamicSpace3 : longint;
        dynamicSpaceInUse3 : longint;
     end;


   PFILE_SYS_STATS = ^TFILE_SYS_STATS;
   TFILE_SYS_STATS = record
        systemElapsedTime : longint;
        maxOpenFiles : word;
        maxFilesOpened : word;
        currOpenFiles : word;
        totalFilesOpened : longint;
        totalReadRequests : longint;
        totalWriteRequests : longint;
        currChangedFATSectors : word;
        totalChangedFATSectors : longint;
        FATWriteErrors : word;
        fatalFATWriteErrors : word;
        FATScanErrors : word;
        maxIndexFilesOpened : word;
        currOpenIndexedFiles : word;
        attachedIndexFiles : word;
        availableIndexFiles : word;
     end;


   PLAN_CONFIG = ^TLAN_CONFIG;
   TLAN_CONFIG = record
        networkAddress : array[0..3] of byte;
        hostAddress : array[0..5] of byte;
        LANDriverInstalled : byte;
        optionNumber : byte;
        configurationText : array[0..159] of char;
     end;

   PPHYS_DISK_STATS = ^TPHYS_DISK_STATS;
   TPHYS_DISK_STATS = record
        systemElapsedTime : longint;
        diskChannel : byte;
        diskRemovable : byte;
        driveType : byte;
        controllerDriveNumber : byte;
        controllerNumber : byte;
        controllerType : byte;
        driveSize : longint;
        driveCylinders : word;
        driveHeads : byte;
        sectorsPerTrack : byte;
        driveDefinition : array[0..63] of char;
        IOErrorCount : word;
        hotFixStart : longint;
        hotFixSize : word;
        hotFixBlockAvailable : word;
        hotFixDisabled : byte;
     end;


   PTTS_STATS = ^TTTS_STATS;
   TTTS_STATS = record
        systemElapsedTime : longint;
        TTS_Supported : byte;
        TTS_Enabled : byte;
        TTS_VolumeNumber : word;
        TTS_MaxOpenTransactions : word;
        TTS_MaxTransactionsOpened : word;
        TTS_CurrTransactionsOpen : word;
        TTS_TotalTransactions : longint;
        TTS_TotalWrites : longint;
        TTS_TotalBackouts : longint;
        TTS_UnfilledBackouts : word;
        TTS_DiskBlocksInUse : word;
        TTS_FATAllocations : longint;
        TTS_FileSizeChanges : longint;
        TTS_FilesTruncated : longint;
        numberOfTransactions : byte;
     end;

   PTTS_CONNECTIONS = ^TTTS_CONNECTIONS;
   TTTS_CONNECTIONS = record
        connectionNumber : byte;
        taskNumber : byte;
     end;


   PCONN_OPEN_FILES_286 = ^TCONN_OPEN_FILES_286;
   TCONN_OPEN_FILES_286 = record
        taskNumber : byte;
        lockType : byte;
        accessControl : byte;
        lockFlag : byte;
        volumeNumber : byte;
        dirEntry : word;
        fileName : array[0..13] of char;
     end;

   PCONN_OPEN_FILES_386 = ^TCONN_OPEN_FILES_386;
   TCONN_OPEN_FILES_386 = record
        taskNumber : word;
        lockType : byte;
        accessControl : byte;
        lockFlag : byte;
        volumeNumber : byte;
        parentDirEntry : longint;
        dirEntry : longint;
        forkCount : byte;
        nameSpace : byte;
        nameLength : byte;
        fileName : array[0..255] of byte;
     end;

   PCONN_OPEN_FILES = ^TCONN_OPEN_FILES;
   TCONN_OPEN_FILES = record
        unionType : word;
        u : record
            case longint of
               0 : ( con286 : TCONN_OPEN_FILES_286 );
               1 : ( con386 : TCONN_OPEN_FILES_386 );
            end;
     end;

   PCONN_SEMAPHORE_286 = ^TCONN_SEMAPHORE_286;
   TCONN_SEMAPHORE_286 = record
        openCount : word;
        semaphoreValue : byte;
        taskNumber : byte;
        nameLength : byte;
        semaphoreName : array[0..254] of byte;
     end;

   PCONN_SEMAPHORE_386 = ^TCONN_SEMAPHORE_386;
   TCONN_SEMAPHORE_386 = record
        openCount : word;
        semaphoreValue : word;
        taskNumber : word;
        nameLength : byte;
        semaphoreName : array[0..254] of byte;
     end;

   PCONN_SEMAPHORE = ^TCONN_SEMAPHORE;
   TCONN_SEMAPHORE = record
        unionType : word;
        u : record
            case longint of
               0 : ( con286 : TCONN_SEMAPHORE_286 );
               1 : ( con386 : TCONN_SEMAPHORE_386 );
            end;
     end;

   PCONN_TASK_INFO_286 = ^TCONN_TASK_INFO_286;
   TCONN_TASK_INFO_286 = record
        unionType : word;
        lockStatus : byte;
        waitRecord : record
            case longint of
               0 : ( LockStatus1 : record
                    taskNumber : byte;
                    beginAddress : word;
                    endAddress : word;
                    volumeNumber : byte;
                    directoryEntry : word;
                    nameLength : byte;
                    name : byte;
                 end );
               1 : ( LockStatus2 : record
                    taskNumber : byte;
                    volumeNumber : byte;
                    directoryEntry : word;
                    nameLength : byte;
                    name : byte;
                 end );
               2 : ( LockStatus3Or4 : record
                    taskNumber : byte;
                    nameLength : byte;
                    name : byte;
                 end );
            end;
     end;

   PCONN_TASK_INFO_386 = ^TCONN_TASK_INFO_386;
   TCONN_TASK_INFO_386 = record
        unionType : word;
        lockStatus : byte;
        waitRecord : record
            case longint of
               0 : ( LockStatus1 : record
                    taskNumber : word;
                    beginAddress : longint;
                    endAddress : longint;
                    volumeNumber : word;
                    parentID : longint;
                    directoryEntry : longint;
                    forkCount : byte;
                    nameSpace : byte;
                    nameLength : byte;
                    name : byte;
                 end );
               1 : ( LockStatus2 : record
                    taskNumber : word;
                    volumeNumber : word;
                    parentID : longint;
                    directoryEntry : longint;
                    forkCount : byte;
                    nameSpace : byte;
                    nameLength : byte;
                    name : byte;
                 end );
               2 : ( LockStatus3Or4 : record
                    taskNumber : word;
                    nameLength : byte;
                    name : byte;
                 end );
            end;
     end;

   PCONN_TASK_PAIRS_286 = ^TCONN_TASK_PAIRS_286;
   TCONN_TASK_PAIRS_286 = record
        task : byte;
        taskStatus : byte;
     end;

   PCONN_TASK_PAIRS_386 = ^TCONN_TASK_PAIRS_386;
   TCONN_TASK_PAIRS_386 = record
        task : word;
        taskStatus : byte;
     end;

   PCONN_USING_FILE_REQ_286 = ^TCONN_USING_FILE_REQ_286;
   TCONN_USING_FILE_REQ_286 = record
        lastRecordSeen : word;
        directoryHandle : byte;
        pathLength : byte;
        path : array[0..254] of byte;
     end;

   PCONN_USING_FILE_REQ_386 = ^TCONN_USING_FILE_REQ_386;
   TCONN_USING_FILE_REQ_386 = record
        forkType : byte;
        volume : byte;
        directoryID : longint;
        nextRecord : word;
     end;

   PCONN_USING_FILE_REQUEST = ^TCONN_USING_FILE_REQUEST;
   TCONN_USING_FILE_REQUEST = record
        unionType : word;
        reserved1 : word;
        reserved2 : byte;
        request : record
            case longint of
               0 : ( req286 : TCONN_USING_FILE_REQ_286 );
               1 : ( req386 : TCONN_USING_FILE_REQ_386 );
            end;
     end;

   PCONN_USING_FILE_REPLY_286 = ^TCONN_USING_FILE_REPLY_286;
   TCONN_USING_FILE_REPLY_286 = record
        useCount : word;
        openCount : word;
        openForReadCount : word;
        openForWriteCount : word;
        denyReadCount : word;
        denyWriteCount : word;
        nextRequestRecord : word;
        locked : byte;
        numberOfRecords : word;
     end;

   PCONN_USING_FILE_REPLY_386 = ^TCONN_USING_FILE_REPLY_386;
   TCONN_USING_FILE_REPLY_386 = record
        nextRequestRecord : word;
        useCount : word;
        openCount : word;
        openForReadCount : word;
        openForWriteCount : word;
        denyReadCount : word;
        denyWriteCount : word;
        locked : byte;
        forkCount : byte;
        numberOfRecords : word;
     end;

   PCONN_USING_FILE_RECORD_286 = ^TCONN_USING_FILE_RECORD_286;
   TCONN_USING_FILE_RECORD_286 = record
        connectionNumber : word;
        taskNumber : byte;
        lockType : byte;
        accessFlags : byte;
        lockStatus : byte;
     end;

   PCONN_USING_FILE_RECORD_386 = ^TCONN_USING_FILE_RECORD_386;
   TCONN_USING_FILE_RECORD_386 = record
        connectionNumber : word;
        taskNumber : word;
        lockType : byte;
        accessFlags : byte;
        lockStatus : byte;
     end;

   PCONN_USING_FILE_REPLY = ^TCONN_USING_FILE_REPLY;
   TCONN_USING_FILE_REPLY = record
        unionType : word;
        reply : record
            case longint of
               0 : ( rep286 : TCONN_USING_FILE_REPLY_286 );
               1 : ( rep386 : TCONN_USING_FILE_REPLY_386 );
            end;
     end;

   PLOGICAL_RECORD_INFO_286 = ^TLOGICAL_RECORD_INFO_286;
   TLOGICAL_RECORD_INFO_286 = record
        useCount : word;
        shareableLockCount : word;
        nextRequestRecord : word;
        locked : byte;
        numberOfRecords : byte;
     end;

   PLOGICAL_RECORD_INFO_386 = ^TLOGICAL_RECORD_INFO_386;
   TLOGICAL_RECORD_INFO_386 = record
        useCount : word;
        shareableLockCount : word;
        locked : byte;
        nextRequestRecord : word;
        numberOfRecords : word;
     end;

   PLOGICAL_RECORD_286 = ^TLOGICAL_RECORD_286;
   TLOGICAL_RECORD_286 = record
        connectionNumber : word;
        taskNumber : byte;
        lockStatus : byte;
     end;

   PLOGICAL_RECORD_386 = ^TLOGICAL_RECORD_386;
   TLOGICAL_RECORD_386 = record
        connectionNumber : word;
        taskNumber : word;
        lockStatus : byte;
     end;

   PLOGICAL_RECORD_INFO = ^TLOGICAL_RECORD_INFO;
   TLOGICAL_RECORD_INFO = record
        unionType : word;
        u : record
            case longint of
               0 : ( lr286 : TLOGICAL_RECORD_INFO_286 );
               1 : ( lr386 : TLOGICAL_RECORD_INFO_386 );
            end;
     end;

   PLOGICAL_RECORD_REQUEST = ^TLOGICAL_RECORD_REQUEST;
   TLOGICAL_RECORD_REQUEST = record
        reserved1 : word;
        reserved2 : byte;
        nextRecord : word;
        nameLength : byte;
        name : array[0..254] of byte;
     end;

   PCONN_LOGICAL_RECORD_286 = ^TCONN_LOGICAL_RECORD_286;
   TCONN_LOGICAL_RECORD_286 = record
        nextRequest : word;
        numberOfRecords : byte;
     end;

   PCONN_LOGICAL_RECORD_386 = ^TCONN_LOGICAL_RECORD_386;
   TCONN_LOGICAL_RECORD_386 = record
        nextRequest : word;
        numberOfRecords : word;
     end;

   PCONN_LOGICAL_RECORD = ^TCONN_LOGICAL_RECORD;
   TCONN_LOGICAL_RECORD = record
        unionType : word;
        u : record
            case longint of
               0 : ( lr286 : TCONN_LOGICAL_RECORD_286 );
               1 : ( lr386 : TCONN_LOGICAL_RECORD_386 );
            end;
     end;

   PCONN_LOGICAL_RECORD_BLOCK_286 = ^TCONN_LOGICAL_RECORD_BLOCK_286;
   TCONN_LOGICAL_RECORD_BLOCK_286 = record
        taskNumber : byte;
        lockStatus : byte;
        lockNameLength : byte;
        lockName : byte;
     end;

   PCONN_LOGICAL_RECORD_BLOCK_386 = ^TCONN_LOGICAL_RECORD_BLOCK_386;
   TCONN_LOGICAL_RECORD_BLOCK_386 = record
        taskNumber : word;
        lockStatus : byte;
        lockNameLength : byte;
        lockName : byte;
     end;

   PFILE_PHYSICAL_RECORD_LOCK_286 = ^TFILE_PHYSICAL_RECORD_LOCK_286;
   TFILE_PHYSICAL_RECORD_LOCK_286 = record
        nextRequest : word;
        numberOfLocks : byte;
        reserved : byte;
     end;

   PFILE_PHYSICAL_RECORD_LOCK_386 = ^TFILE_PHYSICAL_RECORD_LOCK_386;
   TFILE_PHYSICAL_RECORD_LOCK_386 = record
        nextRequest : word;
        numberOfLocks : word;
     end;

   PFILE_PHYSICAL_RECORD_LOCK = ^TFILE_PHYSICAL_RECORD_LOCK;
   TFILE_PHYSICAL_RECORD_LOCK = record
        unionType : word;
        u : record
            case longint of
               0 : ( pr286 : TFILE_PHYSICAL_RECORD_LOCK_286 );
               1 : ( pr386 : TFILE_PHYSICAL_RECORD_LOCK_286 );
            end;
     end;

   PFILE_PHYSICAL_RECORD_286 = ^TFILE_PHYSICAL_RECORD_286;
   TFILE_PHYSICAL_RECORD_286 = record
        loggedCount : word;
        shareLockCount : word;
        recordStart : longint;
        recordEnd : longint;
        connectionNumber : word;
        taskNumber : byte;
        lockType : byte;
     end;

   PFILE_PHYSICAL_RECORD_386 = ^TFILE_PHYSICAL_RECORD_386;
   TFILE_PHYSICAL_RECORD_386 = record
        loggedCount : word;
        shareLockCount : word;
        recordStart : longint;
        recordEnd : longint;
        connectionNumber : word;
        taskNumber : word;
        lockType : byte;
     end;

   PFILE_PHYSICAL_REQUEST_286 = ^TFILE_PHYSICAL_REQUEST_286;
   TFILE_PHYSICAL_REQUEST_286 = record
        lastRecord : word;
        directoryHandle : byte;
        pathLength : byte;
        name : array[0..254] of byte;
     end;

   PFILE_PHYSICAL_REQUEST_386 = ^TFILE_PHYSICAL_REQUEST_386;
   TFILE_PHYSICAL_REQUEST_386 = record
        forkType : byte;
        volume : byte;
        directoryID : longint;
        next : word;
     end;

   PFILE_PHYSICAL_RECORD_REQUEST = ^TFILE_PHYSICAL_RECORD_REQUEST;
   TFILE_PHYSICAL_RECORD_REQUEST = record
        unionType : word;
        reserved1 : word;
        reserved2 : byte;
        u : record
            case longint of
               0 : ( pr286 : TFILE_PHYSICAL_REQUEST_286 );
               1 : ( pr386 : TFILE_PHYSICAL_REQUEST_386 );
            end;
     end;

   PCONN_RECORD_LOCKS_286 = ^TCONN_RECORD_LOCKS_286;
   TCONN_RECORD_LOCKS_286 = record
        nextRecord : word;
        numberOfLocks : byte;
        reserved : byte;
     end;

   PCONN_RECORD_LOCKS_386 = ^TCONN_RECORD_LOCKS_386;
   TCONN_RECORD_LOCKS_386 = record
        nextRecord : word;
        numberOfLocks : word;
     end;

   PCONN_RECORD_LOCKS = ^TCONN_RECORD_LOCKS;
   TCONN_RECORD_LOCKS = record
        unionType : word;
        u : record
            case longint of
               0 : ( rl286 : TCONN_RECORD_LOCKS_286 );
               1 : ( rl386 : TCONN_RECORD_LOCKS_386 );
            end;
     end;

   PCONN_LOCK_RECORD_286 = ^TCONN_LOCK_RECORD_286;
   TCONN_LOCK_RECORD_286 = record
        taskNumber : byte;
        lockFlag : byte;
        recordStart : longint;
        recordEnd : longint;
     end;

   PCONN_LOCK_RECORD_386 = ^TCONN_LOCK_RECORD_386;
   TCONN_LOCK_RECORD_386 = record
        taskNumber : word;
        lockFlag : byte;
        recordStart : longint;
        recordEnd : longint;
     end;

   PCONN_LOCK_REQUEST_286 = ^TCONN_LOCK_REQUEST_286;
   TCONN_LOCK_REQUEST_286 = record
        connectionNumber : word;
        lastRecord : word;
        volume : byte;
        directoryID : word;
        pathLength : byte;
        fileName : array[0..13] of byte;
     end;

   PCONN_LOCK_REQUEST_386 = ^TCONN_LOCK_REQUEST_386;
   TCONN_LOCK_REQUEST_386 = record
        connectionNumber : word;
        forkType : byte;
        volume : byte;
        directoryID : longint;
        next : word;
     end;

   PCONN_LOCK_REQUEST = ^TCONN_LOCK_REQUEST;
   TCONN_LOCK_REQUEST = record
        unionType : word;
        reserved1 : word;
        reserved2 : byte;
        u : record
            case longint of
               0 : ( lr286 : TCONN_LOCK_REQUEST_286 );
               1 : ( lr386 : TCONN_LOCK_REQUEST_386 );
            end;
     end;

   PSEMAPHORE_INFO_286 = ^TSEMAPHORE_INFO_286;
   TSEMAPHORE_INFO_286 = record
        nextRequest : word;
        openCount : word;
        semaphoreValue : byte;
        numberOfRecords : byte;
     end;

   PSEMAPHORE_INFO_386 = ^TSEMAPHORE_INFO_386;
   TSEMAPHORE_INFO_386 = record
        nextRequest : word;
        openCount : word;
        semaphoreValue : word;
        numberOfRecords : word;
     end;

   PSEMAPHORE_INFO = ^TSEMAPHORE_INFO;
   TSEMAPHORE_INFO = record
        unionType : word;
        u : record
            case longint of
               0 : ( si286 : TSEMAPHORE_INFO_286 );
               1 : ( si386 : TSEMAPHORE_INFO_386 );
            end;
     end;

   PSEMAPHORE_INFO_RECORD_286 = ^TSEMAPHORE_INFO_RECORD_286;
   TSEMAPHORE_INFO_RECORD_286 = record
        connectionNumber : word;
        taskNumber : byte;
     end;

   PSEMAPHORE_INFO_RECORD_386 = ^TSEMAPHORE_INFO_RECORD_386;
   TSEMAPHORE_INFO_RECORD_386 = record
        connectionNumber : word;
        taskNumber : word;
     end;

   PSEMAPHORE_INFO_REQUEST = ^TSEMAPHORE_INFO_REQUEST;
   TSEMAPHORE_INFO_REQUEST = record
        reserved1 : word;
        reserved2 : byte;
        nextRecord : word;
        nameLength : byte;
        name : array[0..254] of byte;
     end;

{$include npackoff.inc}

function GetConnectionsOpenFiles(connectionNumber:word; lastRecord:Plongint; lastTask:Plongint; structSize:longint; openFiles:PCONN_OPEN_FILES;
               buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetConnectionsOpenFiles';
function GetConnectionsTaskInformation(connectionNumber:word; connectionTaskInfo:Ppointer; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetConnectionsTaskInformation';
function GetConnectionsUsageStats(connectionNumber:longint; connectionUsage:PCONN_USAGE):longint;cdecl;external Clib name 'GetConnectionsUsageStats';
function GetConnectionsUsingFile(requestSize:longint; request:pointer; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetConnectionsUsingFile';
function GetDiskCacheStats(cacheStats:PDISK_CACHE_STATS):longint;cdecl;external Clib name 'GetDiskCacheStats';
function GetDiskCacheStats(var cacheStats:TDISK_CACHE_STATS):longint;cdecl;external Clib name 'GetDiskCacheStats';
function GetDiskChannelStats(channelNumber:longint; diskChannelStats:PDISK_CHANNEL_STATS):longint;cdecl;external Clib name 'GetDiskChannelStats';
function GetDiskChannelStats(channelNumber:longint; var diskChannelStats:TDISK_CHANNEL_STATS):longint;cdecl;external Clib name 'GetDiskChannelStats';
function GetDriveMappingTable(driveMappingTable:PDRIVE_MAP_TABLE):longint;cdecl;external Clib name 'GetDriveMappingTable';
function GetDriveMappingTable(var driveMappingTable:TDRIVE_MAP_TABLE):longint;cdecl;external Clib name 'GetDriveMappingTable';
function GetFileServerLANIOStats(serverLANIOStats:PSERVER_LAN_IO):longint;cdecl;external Clib name 'GetFileServerLANIOStats';
function GetFileServerLANIOStats(var serverLANIOStats:TSERVER_LAN_IO):longint;cdecl;external Clib name 'GetFileServerLANIOStats';
function GetFileServerMiscInformation(miscInformation:PSERVER_MISC_INFO):longint;cdecl;external Clib name 'GetFileServerMiscInformation';
function GetFileServerMiscInformation(var miscInformation:TSERVER_MISC_INFO):longint;cdecl;external Clib name 'GetFileServerMiscInformation';
function GetFileSystemStats(fileSysStats:PFILE_SYS_STATS):longint;cdecl;external Clib name 'GetFileSystemStats';
function GetFileSystemStats(var fileSysStats:TFILE_SYS_STATS):longint;cdecl;external Clib name 'GetFileSystemStats';
function GetLANDriverConfigInfo(LANBoardNumber:byte; LANConfiguration:PLAN_CONFIG):longint;cdecl;external Clib name 'GetLANDriverConfigInfo';
function GetLANDriverConfigInfo(LANBoardNumber:byte; var LANConfiguration:TLAN_CONFIG):longint;cdecl;external Clib name 'GetLANDriverConfigInfo';
function GetLogicalRecordInformation(requestSize:longint; request:pointer; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetLogicalRecordInformation';
function GetLogicalRecordsByConnection(connectionNumber:word; nextRecord:word; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetLogicalRecordsByConnection';
function GetPathFromDirectoryEntry(volumeNumber:byte; directoryEntry:word; pathLength:PBYTE; path:Pchar):longint;cdecl;external Clib name 'GetPathFromDirectoryEntry';
function GetPhysicalDiskStats(physicalDiskNumber:byte; physicalDiskStats:PPHYS_DISK_STATS):longint;cdecl;external Clib name 'GetPhysicalDiskStats';
function GetPhysicalDiskStats(physicalDiskNumber:byte; var physicalDiskStats:TPHYS_DISK_STATS):longint;cdecl;external Clib name 'GetPhysicalDiskStats';
function GetPhysicalRecordLocksByFile(requestSize:longint; request:pointer; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetPhysicalRecordLocksByFile';
function GetPhysRecLockByConnectAndFile(requestSize:longint; request:pointer; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetPhysRecLockByConnectAndFile';
function GetSemaphoreInformation(requestSize:longint; request:pointer; buffer:pointer; bufferSize:longint):longint;cdecl;external Clib name 'GetSemaphoreInformation';
function TTSGetStats(TTSStats:PTTS_STATS; bufferLen:longint; buffer:PBYTE):longint;cdecl;external Clib name 'TTSGetStats';
function TTSGetStats(var TTSStats:TTTS_STATS; bufferLen:longint; var buffer):longint;cdecl;external Clib name 'TTSGetStats';

{------------------------------------------------------------------------------}
{$include npackon.inc}

const
   CRITICAL_ATTRIBUTE_FLAG = $00000080;
   RESERVED_FLAGS_MASK     = $0000FFFF;
   USER_FLAGS_MASK         = $FFFF0000;
{------------------------------------------------------------------
     T_enumerateEAnoKey is the structure returned in the dataBuffer
     by EnumerateEA if a empty key (or NULL) is specified.  The
     EAsInReply output parameter tells how many T_enumerateEAnoKey
     structures are in the dataBuffer.
    ------------------------------------------------------------------ }
{ length of entire EA  }
{ length of this field is given by keyLength  }
type

   PT_enumerateEAnoKey = ^TT_enumerateEAnoKey;
   TT_enumerateEAnoKey = record
        valueLength : longint;
        keyLength   : word;
        accessFlags : longint;
        keyValue    : array[0..0] of char;
     end;
{----------------------------------------------------------------------
     T_enumerateEAwithKey is the structure returned in the dataBuffer
     by EnumerateEA if a non-empty key is specified.  In this case the
     EAsInReply output parameter will return one and there will only be
     one T_enumerateEAwithKey structure in the dataBuffer.
    ---------------------------------------------------------------------- }

   PT_enumerateEAwithKey = ^TT_enumerateEAwithKey;
   TT_enumerateEAwithKey = record
        valueLength : longint;
        keyLength : word;
        accessFlags : longint;
        keyExtants : longint;
        valueExtants : longint;
        keyValue : array[0..0] of char;
     end;

{$include npackoff.inc}

function CloseEA(handle:longint):longint;cdecl;external Clib name 'CloseEA';
function CopyEA (srcPath,destPath:Pchar; destVolumeNumber,destDirectoryNumber:longint;
                 EAcount,EAdataSize,EAkeySize:PLongint):longint;cdecl;external Clib name 'CopyEA';
function CopyEA (srcPath,destPath:Pchar; destVolumeNumber,destDirectoryNumber:longint;
             var EAcount,EAdataSize,EAkeySize:longint):longint;cdecl;external Clib name 'CopyEA';

function EnumerateEA(handle:longint; keyBuffer:Pchar; dataBuffer:pointer; dataBufferSize:longint;
                     startPosition:longint;
                     dataSize,EAsInReply:PLongint):longint;cdecl;external Clib name 'EnumerateEA';
function EnumerateEA(handle:longint; keyBuffer:Pchar; var dataBuffer; dataBufferSize:longint;
                     startPosition:longint;
                 var dataSize,EAsInReply:longint):longint;cdecl;external Clib name 'EnumerateEA';
function EnumerateEA(handle:longint; keyBuffer:Pchar; dataBuffer:pointer; dataBufferSize:longint;
                     startPosition:longint;
                 var dataSize,EAsInReply:longint):longint;cdecl;external Clib name 'EnumerateEA';
function GetEAInfo (handle:longint; totalEAs,totalDataSizeOfEAs,totalKeySizeOfEAs:PLongint):longint;cdecl;external Clib name 'GetEAInfo';
function GetEAInfo (handle:longint; var totalEAs,totalDataSizeOfEAs,totalKeySizeOfEAs:longint):longint;cdecl;external Clib name 'GetEAInfo';

function OpenEA(path:Pchar; reserved:longint):longint;cdecl;external Clib name 'OpenEA';
function ReadEA(handle:longint; keyBuffer:Pchar; dataBuffer:Pchar; dataBufferSize:longint; accessFlags:PLongint):longint;cdecl;external Clib name 'ReadEA';
function ReadEA(handle:longint; keyBuffer:Pchar; dataBuffer:Pchar; dataBufferSize:longint; var accessFlags:longint):longint;cdecl;external Clib name 'ReadEA';
function WriteEA(handle:longint; keyBuffer:Pchar; dataBuffer:Pchar; dataBufferSize:longint; accessFlags:longint):longint;cdecl;external Clib name 'WriteEA';

{------------------------------------------------------------------------------}

const MAX_CONSOLE_MESSAGE_LENGTH =  80;
      MAX_MESSAGE_LENGTH         =  58;
      NEW_MAX_MESSAGE_LENGTH     = 250;

function BroadcastToConsole       (msg:Pchar):longint;       cdecl;external Clib name 'BroadcastToConsole';
function DisableStationBroadcasts:longint;                   cdecl;external Clib name 'DisableStationBroadcasts';
function EnableStationBroadcasts:longint;                    cdecl;external Clib name 'EnableStationBroadcasts';
function GetBroadcastMessage      (msgBuffer:Pchar):longint; cdecl;external Clib name 'GetBroadcastMessage';
function SendBroadcastMessage     (msg:Pchar;
                                   connectionList:PWORD;
                                   resultList:PBYTE;
                                   connectionCount:word):longint; cdecl;external Clib name 'SendBroadcastMessage';
function SendBroadcastMessage     (msg:Pchar;
                                   var connectionList;
                                   var resultList;
                                   var connectionCount:word):longint; cdecl;external Clib name 'SendBroadcastMessage';

{------------------------------------------------------------------------------}
{$include npackon.inc}

const
   NWMAX_QENTRIES = 250;
{ the following manifest constant applies to server versions BELOW 3.X  }
   NWMAX_JOB_SERVERS = 25;
{ the following manifest constant applies to server versions ABOVE 2.X  }
   NWQ_MAX_JOB_SERVERS = 50;
   QF_AUTO_START = $08;
   QF_SERVICE_RESTART = $10;
   QF_ENTRY_OPEN = $20;
   QF_USER_HOLD = $40;
   QF_OPERATOR_HOLD = $80;
{ Queue Status Flags  }
   QS_CANT_ADD_JOBS = $01;
   QS_SERVERS_CANT_ATTACH = $02;
   QS_CANT_SERVICE_JOBS = $04;
type

   PJobStruct = ^TJobStruct;
   TJobStruct = record
        clientStation : byte;
        clientTaskNumber : byte;
        clientIDNumber : longint;
        targetServerIDNumber : longint;
        targetExecutionTime : array[0..5] of byte;
        jobEntryTime : array[0..5] of byte;
        jobNumber : word;
        jobType : word;
        jobPosition : byte;
        jobControlFlags : byte;
        jobFileName : array[0..13] of byte;
        jobFileHandle : array[0..5] of byte;
        serverStation : byte;
        serverTaskNumber : byte;
        serverIDNumber : longint;
        textJobDescription : array[0..49] of byte;
        clientRecordArea : array[0..151] of byte;
     end;
{ (19)80 - (20)79    }
{ 1 = January,  ...  }
{ 1 - 31             }
{ 0 - 23             }
{ 0 - 59             }
{ 0 - 59             }

   PNWStandardChronRec_t = ^TNWStandardChronRec_t;
   TNWStandardChronRec_t = record
        year : byte;
        month : byte;
        day : byte;
        hour : byte;
        minute : byte;
        second : byte;
     end;

   PNWQChronRec_t = ^TNWQChronRec_t;
   TNWQChronRec_t = TNWStandardChronRec_t;

   PNWFileHandle_t = ^TNWFileHandle_t;
   TNWFileHandle_t = byte;

   PNWQEntityInfo_t = ^TNWQEntityInfo_t;
   TNWQEntityInfo_t = record
        clientConnNum : longint;
        taskNum : longint;
        id : longint;
     end;

   PNWQJobServerInfo_t = ^TNWQJobServerInfo_t;
   TNWQJobServerInfo_t = record
        id : longint;
        executionTime : TNWQChronRec_t;
     end;

   PNWQJobInfo_t = ^TNWQJobInfo_t;
   TNWQJobInfo_t = record
        entryTime : TNWQChronRec_t;
        num : longint;
        _type : word;
        position : word;
        controlFlags : word;
        assocFileName : array[0..13] of char;
        fileHandle : TNWFileHandle_t;
     end;

   PNWQPrintJobInfo_t = ^TNWQPrintJobInfo_t;
   TNWQPrintJobInfo_t = record
        entryTime : TNWQChronRec_t;
        num : longint;
        formType : word;
        position : word;
        controlFlags : word;
        assocFileName : array[0..13] of char;
        fileHandle : TNWFileHandle_t;
     end;

   PNWQJobRec_t = ^TNWQJobRec_t;
   TNWQJobRec_t = record
        reserved : array[0..9] of byte;
        client : TNWQEntityInfo_t;
        target : TNWQJobServerInfo_t;
        job : TNWQJobInfo_t;
        jobServer : TNWQEntityInfo_t;
        textJobDescription : array[0..49] of byte;
        jobServerRecord : array[0..151] of byte;
     end;

   PNWQPrintServerRec_t = ^TNWQPrintServerRec_t;
   TNWQPrintServerRec_t = record
        versionNumber : byte;
        tabSize : byte;
        numberOfCopies : word;
        printControlFlags : word;
        maxLinesPerPage : word;
        maxCharsPerLine : word;
        formName : array[0..12] of char;
        reserve : array[0..8] of byte;
        bannerNameField : array[0..12] of char;
        bannerFileField : array[0..12] of char;
        bannerFileName : array[0..13] of char;
        directoryPath : array[0..79] of char;
     end;
{$include npackoff.inc}

function AbortServicingQueueJobAndFile(queueID:longint; jobNumber:word; fileHandle:longint):longint;cdecl;external Clib name 'AbortServicingQueueJobAndFile';
function AttachQueueServerToQueue(queueID:longint):longint;cdecl;external Clib name 'AttachQueueServerToQueue';
function ChangeQueueJobEntry(queueID:longint; job:PJobStruct):longint;cdecl;external Clib name 'ChangeQueueJobEntry';
function ChangeQueueJobEntry(queueID:longint; var job:TJobStruct):longint;cdecl;external Clib name 'ChangeQueueJobEntry';
function ChangeQueueJobPosition(queueID:longint; jobNumber:word; newPosition:byte):longint;cdecl;external Clib name 'ChangeQueueJobPosition';
function ChangeToClientRights(queueID:longint; jobNumber:word):longint;cdecl;external Clib name 'ChangeToClientRights';
function CloseFileAndAbortQueueJob(queueID:longint; jobNumber:word; fileHandle:longint):longint;cdecl;external Clib name 'CloseFileAndAbortQueueJob';
function CloseFileAndStartQueueJob(queueID:longint; jobNumber:word; fileHandle:longint):longint;cdecl;external Clib name 'CloseFileAndStartQueueJob';
function CreateAQueue(queueName:Pchar; queueType:longint; pathName:Pchar; queueID:Plongint):longint;cdecl;external Clib name 'CreateAQueue';
function CreateAQueue(queueName:Pchar; queueType:longint; pathName:Pchar; var queueID:longint):longint;cdecl;external Clib name 'CreateAQueue';
function CreateQueueJobAndFile(queueID:longint; job:PJobStruct; fileHandle:Plongint):longint;cdecl;external Clib name 'CreateQueueJobAndFile';
function CreateQueueJobAndFile(queueID:longint; var job:TJobStruct; var fileHandle:longint):longint;cdecl;external Clib name 'CreateQueueJobAndFile';
function DestroyQueue(queueID:longint):longint;cdecl;external Clib name 'DestroyQueue';
function DetachQueueServerFromQueue(queueID:longint):longint;cdecl;external Clib name 'DetachQueueServerFromQueue';
function FinishServicingQueueJobAndFile(queueID:longint; jobNumber:word; charge:longint; fileHandle:longint):longint;cdecl;external Clib name 'FinishServicingQueueJobAndFile';
function GetQueueJobList(queueID:longint; jobCount:PWORD; jobNumberList:PWORD; maxJobNumbers:word):longint;cdecl;external Clib name 'GetQueueJobList';
function GetQueueJobsFileSize(queueID:longint; jobNumber:longint; fileSize:Plongint):longint;cdecl;external Clib name 'GetQueueJobsFileSize';
function NWQAbortJob(queueID:longint; jobNum:longint; fileHandle:longint):longint;cdecl;external Clib name 'NWQAbortJob';
function NWQAbortJobService(queueID:longint; jobNum:longint; fileHandle:longint):longint;cdecl;external Clib name 'NWQAbortJobService';
function NWQAttachServer(queueID:longint):longint;cdecl;external Clib name 'NWQAttachServer';
function NWQBeginJobService(queueID:longint; targetJobType:word; jobInfo:PNWQJobRec_t; fileHandle:Plongint):longint;cdecl;external Clib name 'NWQBeginJobService';
function NWQChangeJobEntry(queueID:longint; jobInfo:PNWQJobRec_t):longint;cdecl;external Clib name 'NWQChangeJobEntry';
function NWQChangeJobPosition(queueID:longint; jobNum:longint; newPosition:longint):longint;cdecl;external Clib name 'NWQChangeJobPosition';
function NWQChangeJobQueue(srcQueueID:longint; srcJobNum:longint; dstQueueID:longint; dstJobNum:Plongint):longint;cdecl;external Clib name 'NWQChangeJobQueue';
function NWQChangeToClientRights(queueID:longint; jobNum:longint):longint;cdecl;external Clib name 'NWQChangeToClientRights';
function NWQCreate(queueName:Pchar; queueType:word; pathName:Pchar; queueID:Plongint):longint;cdecl;external Clib name 'NWQCreate';
function NWQCreateJob(queueID:longint; jobInfo:PNWQJobRec_t; fileHandle:Plongint):longint;cdecl;external Clib name 'NWQCreateJob';
function NWQCreateJob(queueID:longint; var jobInfo:TNWQJobRec_t; fileHandle:Plongint):longint;cdecl;external Clib name 'NWQCreateJob';
function NWQDestroy(queueID:longint):longint;cdecl;external Clib name 'NWQDestroy';
function NWQDetachServer(queueID:longint):longint;cdecl;external Clib name 'NWQDetachServer';
function NWQEndJobService(queueID:longint; jobNum:longint; chargeInfo:longint; fileHandle:longint):longint;cdecl;external Clib name 'NWQEndJobService';
function NWQGetJobEntry(queueID:longint; jobNum:longint; jobInfo:PNWQJobRec_t):longint;cdecl;external Clib name 'NWQGetJobEntry';
function NWQGetJobEntry(queueID:longint; jobNum:longint; var jobInfo:TNWQJobRec_t):longint;cdecl;external Clib name 'NWQGetJobEntry';
function NWQGetJobFileSize(queueID:longint; jobNum:longint; fileSize:Plongint):longint;cdecl;external Clib name 'NWQGetJobFileSize';
function NWQGetJobFileSize(queueID:longint; jobNum:longint; var fileSize:longint):longint;cdecl;external Clib name 'NWQGetJobFileSize';
function NWQGetServers(queueID:longint; currentServers,qServerIDs,qServerConnNums:Plongint):longint;cdecl;external Clib name 'NWQGetServers';
function NWQGetServerStatus(queueID:longint; jobServerID:longint; jobServerConnNum:longint; jobServerRecord:pointer):longint;cdecl;external Clib name 'NWQGetServerStatus';
function NWQGetStatus(queueID:longint; queueStatus:Plongint; currentEntries:Plongint; currentServers:Plongint):longint;cdecl;external Clib name 'NWQGetStatus';
function NWQMarkJobForService(queueID:longint; jobNum:longint; fileHandle:longint):longint;cdecl;external Clib name 'NWQMarkJobForService';
function NWQRemoveJob(queueID:longint; jobNum:longint):longint;cdecl;external Clib name 'NWQRemoveJob';
function NWQRestoreServerRights:longint;cdecl;external Clib name 'NWQRestoreServerRights';
function NWQScanJobNums(queueID:longint; queueSequence:Plongint; totalJobs:Plongint; jobCount:Plongint; jobNumList:Plongint):longint;cdecl;external Clib name 'NWQScanJobNums';
function NWQServiceJob(queueID:longint; targetJobTypesCount:longint; targetJobTypes:PWORD; jobInfo:PNWQJobRec_t; fileHandle:Plongint):longint;cdecl;external Clib name 'NWQServiceJob';
function NWQSetServerStatus(queueID:longint; serverStatusRecord:pointer):longint;cdecl;external Clib name 'NWQSetServerStatus';
function NWQSetStatus(queueID:longint; queueStatus:longint):longint;cdecl;external Clib name 'NWQSetStatus';
function ReadQueueCurrentStatus(queueID:longint; queueStatus:PBYTE; numberOfJobs:PBYTE; numberOfServers:PBYTE; serverIDList:Plongint;
           serverStationList:PWORD; maxNumberOfServers:word):longint;cdecl;external Clib name 'ReadQueueCurrentStatus';
function ReadQueueJobEntry(queueID:longint; jobNumber:word; job:PJobStruct):longint;cdecl;external Clib name 'ReadQueueJobEntry';
function ReadQueueServerCurrentStatus(queueID:longint; serverID:longint; serverStation:char; serverStatusRecord:Pchar):longint;cdecl;external Clib name 'ReadQueueServerCurrentStatus';
function RemoveJobFromQueue(queueID:longint; jobNumber:word):longint;cdecl;external Clib name 'RemoveJobFromQueue';
function RestoreQueueServerRights:longint;cdecl;external Clib name 'RestoreQueueServerRights';
function ServiceQueueJobAndOpenFile(queueID:longint; targetJobType:word; job:PJobStruct; fileHandle:Plongint):longint;cdecl;external Clib name 'ServiceQueueJobAndOpenFile';
function ServiceQueueJobAndOpenFile(queueID:longint; targetJobType:word; var job:TJobStruct; var fileHandle:longint):longint;cdecl;external Clib name 'ServiceQueueJobAndOpenFile';
function SetQueueCurrentStatus(queueID:longint; queueStatus:byte):longint;cdecl;external Clib name 'SetQueueCurrentStatus';
function SetQueueServerCurrentStatus(queueID:longint; serverStatusRecord:PBYTE):longint;cdecl;external Clib name 'SetQueueServerCurrentStatus';
function SetQueueServerCurrentStatus(queueID:longint; var serverStatusRecord):longint;cdecl;external Clib name 'SetQueueServerCurrentStatus';

{------------------------------------------------------------------------------}
function GetNetworkSerialNumber    (networkSerialNumber : Plongint;
                                    applicationNumber   : Pword)     : longint; cdecl; external Clib name 'GetNetworkSerialNumber';
function GetNetworkSerialNumber    (var networkSerialNumber : longint;
                                    var applicationNumber   : word)  : longint; cdecl; external Clib name 'GetNetworkSerialNumber';
function VerifyNetworkSerialNumber (networkSerialNumber : longint;
                                    applicationNumber   : Pword)     : longint; cdecl; external Clib name 'VerifyNetworkSerialNumber';
function VerifyNetworkSerialNumber (networkSerialNumber : longint;
                                var applicationNumber   : word)      : longint; cdecl; external Clib name 'VerifyNetworkSerialNumber';

{------------------------------------------------------------------------------}
const
   OLD_SS_DEFAULT_BUFFER_SIZE = 538;
   SS_DEFAULT_BUFFER_SIZE = 600;

{ These connection types are used by SSGetActiveConnListByType          }
{ They are all conditionally defined because some of them may appear in }
{ other clib header files.                                              }
{                                                                       }
{ Connection service type  }
{ NOTE: type 1 is reserved by CLIB for backward compatability  }

   NCP_CONNECTION_TYPE = 2;
   NLM_CONNECTION_TYPE = 3;
   AFP_CONNECTION_TYPE = 4;
   FTAM_CONNECTION_TYPE = 5;
   ANCP_CONNECTION_TYPE = 6;

{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% }
{      User Interface Structures         }
{ %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% }
{$include npackon.inc}
type

   PSSDefaultBuffer = ^TSSDefaultBuffer;
   TSSDefaultBuffer = record
        data : array[0..(SS_DEFAULT_BUFFER_SIZE)-1] of byte;
     end;

   PCacheMemoryCounters = ^TCacheMemoryCounters;
   TCacheMemoryCounters = record
        OriginalNumberOfCacheBuffers : longint;
        CurrentNumberOfCacheBuffers : longint;
        CacheDirtyBlockThreshold : longint;
        debugCounters : array[0..6] of longint;
     end;

   PCacheTrendCounters = ^TCacheTrendCounters;
   TCacheTrendCounters = record
        NumOfCacheChecks : longint;
        NumOfCacheHits : longint;
        debugCounters : array[0..6] of longint;
        LRUSittingTime : longint;
     end;

   PCacheInformation = ^TCacheInformation;
   TCacheInformation = record
        MaximumByteCount : longint;
        MinimumNumberOfCacheBuffers : longint;
        MinimumCacheReportThreshold : longint;
        AllocateWaitingCount : longint;
        NDirtyBlocks : longint;
        CacheDirtyWaitTime : longint;
        CacheMaximumConcurrentWrites : longint;
        MaximumDirtyTime : longint;
        NumberOfDirectoryCacheBuffers : longint;
        CacheByteToBlockShiftFactor : longint;
     end;

   PGetCacheInfoStructure = ^TGetCacheInfoStructure;
   TGetCacheInfoStructure = record
        currentServerTime : longint;
        VConsoleVersion : byte;
        VConsoleRevision : byte;
        reserved : word;
        CacheCntrs : array[0..25] of longint;
        MemoryCntrs : TCacheMemoryCounters;
        TrendCntrs : TCacheTrendCounters;
        CacheInfo : TCacheInformation;
     end;

   PServerInformation = ^TServerInformation;
   TServerInformation = record
        ReplyCanceledCount : longint;
        WriteHeldOffCount : longint;
        reserved1 : longint;
        InvalidRequestTypeCount : longint;
        BeingAbortedCount : longint;
        AlreadyDoingReAllocateCount : longint;
        reserved2 : array[0..2] of longint;
        DeAllocateStillTransmittingCount : longint;
        StartStationErrorCount : longint;
        InvalidSlotCount : longint;
        BeingProcessedCount : longint;
        ForgedPacketCount : longint;
        StillTransmittingCount : longint;
        ReExecuteRequestCount : longint;
        InvalidSequenceNumberCount : longint;
        DuplicateIsBeingSentAlreadyCount : longint;
        SentPositiveAcknowledgeCount : longint;
        SentADuplicateReplyCount : longint;
        NoMemoryForStationControlCount : longint;
        NoAvailableConnectionsCount : longint;
        ReAllocateSlotCount : longint;
        ReAllocateSlotCameTooSoonCount : longint;
     end;

   PFSCounters = ^TFSCounters;
   TFSCounters = record
        TooManyHops : word;
        UnknownNetwork : word;
        NoSpaceForService : word;
        NoRecieveBuffers : word;
        NotMyNetwork : word;
        NetBIOSPropagatedCount : longint;
        TotalPacketsServiced : longint;
        TotalPacketsRouted : longint;
     end;

   PGetFileServerInfoStructure = ^TGetFileServerInfoStructure;
   TGetFileServerInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        NCPStaInUseCnt : longint;
        NCPPeakStaInUse : longint;
        numOfNCPReqs : longint;
        serverUtilization : longint;
        serverInfo : TServerInformation;
        fileServerCounters : TFSCounters;
     end;

   PGetFileSystemInfoStructure = ^TGetFileSystemInfoStructure;
   TGetFileSystemInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        debugCounters : array[0..12] of longint;
     end;

   PUserInformation = ^TUserInformation;
   TUserInformation = record
        connectionNumber : longint;
        useCount : longint;
        connectionServiceType : byte;
        loginTime : array[0..6] of byte;
        status : longint;
        expirationTime : longint;
        objectType : longint;
        transactionFlag : byte;
        logicalLockThreshold : byte;
        recordLockThreshold : byte;
        fileWriteFlags : byte;
        fileWriteState : byte;
        filler : byte;
        fileLockCount : word;
        recordLockCount : word;
        totalBytesRead : array[0..5] of byte;
        totalBytesWritten : array[0..5] of byte;
        totalRequests : longint;
        heldRequests : longint;
        heldBytesRead : array[0..5] of byte;
        heldBytesWritten : array[0..5] of byte;
     end;

   PGetUserInfoStructure = ^TGetUserInfoStructure;
   TGetUserInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        userInfo : TUserInformation;
        userNameLen : byte;
        username : byte;
     end;

   PPacketBurstInformation = ^TPacketBurstInformation;
   TPacketBurstInformation = record
        BigInvalidSlotCount : longint;
        BigForgedPacketCount : longint;
        BigInvalidPacketCount : longint;
        BigStillTransmittingCount : longint;
        StillDoingTheLastRequestCount : longint;
        InvalidControlRequestCount : longint;
        ControlInvalidMessageNumberCount : longint;
        ControlBeingTornDownCount : longint;
        BigRepeatTheFileReadCount : longint;
        BigSendExtraCCCount : longint;
        BigReturnAbortMessageCount : longint;
        BigReadInvalidMessageNumberCount : longint;
        BigReadDoItOverCount : longint;
        BigReadBeingTornDownCount : longint;
        PreviousControlPacketCount : longint;
        SendHoldOffMessageCount : longint;
        BigReadNoDataAvailableCount : longint;
        BigReadTryingToReadTooMuchCount : longint;
        ASyncReadErrorCount : longint;
        BigReadPhysicalReadErrorCount : longint;
        ControlBadACKFragmentListCount : longint;
        ControlNoDataReadCount : longint;
        WriteDuplicateRequestCount : longint;
        ShouldntBeACKingHereCount : longint;
        WriteInconsistentPacketLengthsCount : longint;
        FirstPacketIsntAWriteCount : longint;
        WriteTrashedDuplicateRequestCount : longint;
        BigWriteInvalidMessageNumberCount : longint;
        BigWriteBeingTornDownCount : longint;
        BigWriteBeingAbortedCount : longint;
        ZeroACKFragmentCountCount : longint;
        WriteCurrentlyTransmittingCount : longint;
        TryingToWriteTooMuchCount : longint;
        WriteOutOfMemoryForControlNodesCount : longint;
        WriteDidntNeedThisFragmentCount : longint;
        WriteTooManyBuffersCheckedOutCount : longint;
        WriteTimeOutCount : longint;
        WriteGotAnACKCount : longint;
        WriteGotAnACKCount1 : longint;
        PollerAbortedTheConnectionCount : longint;
        MaybeHadOutOfOrderWritesCount : longint;
        HadAnOutOfOrderWriteCount : longint;
        MovedTheACKBitDownCount : longint;
        BumpedOutOfOrderWriteCount : longint;
        PollerRemovedOldOutOfOrderCount : longint;
        WriteDidntNeedButRequestedACKCount : longint;
        WriteTrashedPacketCount : longint;
        TooManyACKFragmentsCount : longint;
        SavedAnOutOfOrderPacketCount : longint;
        ConnectionBeingAbortedCount : longint;
     end;

   PGetPacketBurstInfoStructure = ^TGetPacketBurstInfoStructure;
   TGetPacketBurstInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        packetBurstInfo : TPacketBurstInformation;
     end;

   PIPXInformation = ^TIPXInformation;
   TIPXInformation = record
        IpxSendPacketCount : longint;
        IpxMalformPacketCount : word;
        IpxGetECBRequestCount : longint;
        IpxGetECBFailCount : longint;
        IpxAESEventCount : longint;
        IpxPostponedAESCount : word;
        IpxMaxConfiguredSocketCount : word;
        IpxMaxOpenSocketCount : word;
        IpxOpensocketFailCount : word;
        IpxListenECBCount : longint;
        IpxECBCancelFailCount : word;
        IpxGetLocalTargetFailCount : word;
     end;

   PSPXInformation = ^TSPXInformation;
   TSPXInformation = record
        SpxMaxConnectionsCount : word;
        SpxMaxUsedConnections : word;
        SpxEstConnectionReq : word;
        SpxEstConnectionFail : word;
        SpxListenConnectReq : word;
        SpxListenConnectFail : word;
        SpxSendCount : longint;
        SpxWindowChokeCount : longint;
        SpxBadSendCount : word;
        SpxSendFailCount : word;
        SpxAbortedConnection : word;
        SpxListenPacketCount : longint;
        SpxBadListenCount : word;
        SpxIncomingPacketCount : longint;
        SpxBadInPacketCnt : word;
        SpxSuppressedPackCnt : word;
        SpxNoSesListenECBCnt : word;
        SpxWatchDogDestSesCnt : word;
     end;

   PGetIPXSPXInfoStructure = ^TGetIPXSPXInfoStructure;
   TGetIPXSPXInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        IPXInfo : TIPXInformation;
        SPXInfo : TSPXInformation;
     end;

   PGetGarbageCollInfoStructure = ^TGetGarbageCollInfoStructure;
   TGetGarbageCollInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        failedAllocReqCount : longint;
        numberOfAllocs : longint;
        noMoreMemAvlCnt : longint;
        numOfGarbageColl : longint;
        foundSomeMem : longint;
        numOfChecks : longint;
     end;
   TGetGarbageCollInfoStruc = TGetGarbageCollInfoStructure;
   PGetGarbageCollInfoStruc = ^TGetGarbageCollInfoStruc;

   PCPUInformation = ^TCPUInformation;
   TCPUInformation = record
        numberOfCPUs : longint;
        PageTableOwnerFlag : longint;
        CPUType : longint;
        CoProcessorFlag : longint;
        BusType : longint;
        IOEngineFlag : longint;
        FSEngineFlag : longint;
        NonDedFlag : longint;
     end;


   PGetCPUInfoStructure = ^TGetCPUInfoStructure;
   TGetCPUInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        CPUInfo : TCPUInformation;
        variableStringsStart : byte;
     end;
{ The LFSCountersStructure is provided  }
{ so that you can see field names.  It  }
{ isn't referenced and developers will  }
{ probably want to define their own     }
{ structure.  This structure may grow   }
{ beyond what one call can return at    }
{ some future date.                     }
{                                       }

   PLFSCountersStructure = ^TLFSCountersStructure;
   TLFSCountersStructure = record
        ReadFile : longint;
        WriteFile : longint;
        DeleteFile : longint;
        RenMove : longint;
        OpenFile : longint;
        CreateFile : longint;
        CreateAndOpenFile : longint;
        CloseFile : longint;
        ScanDeleteFile : longint;
        SalvageFile : longint;
        PurgeFile : longint;
        MigrateFile : longint;
        DeMigrateFile : longint;
        CreateDir : longint;
        DeleteDir : longint;
        DirectoryScans : longint;
        MapPathToDirNum : longint;
        ModifyDirEntry : longint;
        GetAccessRights : longint;
        GetAccessRightsFromIDs : longint;
        MapDirNumToPath : longint;
        GetEntryFromPathStrBase : longint;
        GetOtherNSEntry : longint;
        GetExtDirInfo : longint;
        GetParentDirNum : longint;
        AddTrusteeR : longint;
        ScanTrusteeR : longint;
        DelTrusteeR : longint;
        PurgeTrust : longint;
        FindNextTrustRef : longint;
        ScanUserRestNodes : longint;
        AddUserRest : longint;
        DeleteUserRest : longint;
        RtnDirSpaceRest : longint;
        GetActualAvailDskSp : longint;
        CntOwnedFilesAndDirs : longint;
        MigFileInfo : longint;
        VolMigInfo : longint;
        ReadMigFileData : longint;
        GetVolusageStats : longint;
        GetActualVolUsageStats : longint;
        GetDirUsageStats : longint;
        NMFileReadsCount : longint;
        NMFileWritesCount : longint;
        MapPathToDirectoryNumberOrPhantom : longint;
        StationHasAccessRightsGrantedBelow : longint;
        GetDataStreamLengthsFromPathStringBase : longint;
        CheckAndGetDirectoryEntry : longint;
        GetDeletedEntry : longint;
        GetOriginalNameSpace : longint;
        GetActualFileSize : longint;
        VerifyNameSpaceNumber : longint;
        VerifyDataStreamNumber : longint;
        CheckVolumeNumber : longint;
        CommitFile : longint;
        VMGetDirectoryEntry : longint;
        CreateDMFileEntry : longint;
        RenameNameSpaceEntry : longint;
        LogFile : longint;
        ReleaseFile : longint;
        ClearFile : longint;
        SetVolumeFlag : longint;
        ClearVolumeFlag : longint;
        GetOriginalInfo : longint;
        CreateMigratedDir : longint;
        F3OpenCreate : longint;
        F3InitFileSearch : longint;
        F3ContinueFileSearch : longint;
        F3RenameFile : longint;
        F3ScanForTrustees : longint;
        F3ObtainFileInfo : longint;
        F3ModifyInfo : longint;
        F3EraseFile : longint;
        F3SetDirHandle : longint;
        F3AddTrustees : longint;
        F3DeleteTrustees : longint;
        F3AllocDirHandle : longint;
        F3ScanSalvagedFiles : longint;
        F3RecoverSalvagedFiles : longint;
        F3PurgeSalvageableFile : longint;
        F3GetNSSpecificInfo : longint;
        F3ModifyNSSpecificInfo : longint;
        F3SearchSet : longint;
        F3GetDirBase : longint;
        F3QueryNameSpaceInfo : longint;
        F3GetNameSpaceList : longint;
        F3GetHugeInfo : longint;
        F3SetHugeInfo : longint;
        F3GetFullPathString : longint;
        F3GetEffectiveDirectoryRights : longint;
        ParseTree : longint;
     end;

   PGetVolumeSwitchInfoStructure = ^TGetVolumeSwitchInfoStructure;
   TGetVolumeSwitchInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        totalLFSCounters : longint;
        currentLFSCounters : longint;
        counters : longint;
     end;

   PGetNLMLoadedListStructure = ^TGetNLMLoadedListStructure;
   TGetNLMLoadedListStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        moreFlag : word;
        NLMLoadedCount : longint;
        NLMCount : longint;
        NLMNumbers : longint;
     end;


   PNLMInformation = ^TNLMInformation;
   TNLMInformation = record
        nlmIdentificationNumber : longint;
        nlmFlags : longint;
        nlmType : longint;
        nlmParentID : longint;
        nlmMajorVersion : longint;
        nlmMinorVersion : longint;
        nlmRevision : longint;
        nlmYear : longint;
        nlmMonth : longint;
        nlmDay : longint;
        nlmAllocAvailBytes : longint;
        nlmAllocFreeCount : longint;
        nlmLastGarbCollect : longint;
        nlmMessageLanguage : longint;
        nlmNumberOfReferencedPublics : longint;
     end;

{ In GetNLMInfoStructure:                 }
{ At startOFLStrings there will be three  }
{ length preceeded strings -- they may be }
{ zero bytes long!  The strings are:      }
{ the file name, the NLM name, and the    }
{ copyright.                              }
{ Each string consists of one byte which  }
{ contains the length of the string       }
{ followed by zero to 255 bytes of data,  }
{ depending upon the value of the length  }
{ byte.  When the length byte is zero, no }
{ data is present for that string.        }
{                                         }
{ 3 Len preceeded strings: filename, name, copyright  }

   PGetNLMInfoStructure = ^TGetNLMInfoStructure;
   TGetNLMInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        NLMInfo : TNLMInformation;
        startOfLStrings : byte;
     end;

   PDirectoryCacheInformation = ^TDirectoryCacheInformation;
   TDirectoryCacheInformation = record
        MinimumTimeSinceFileDelete : longint;
        AbsMinimumTimeSinceFileDelete : longint;
        MinimumNumberOfDirCacheBuffers : longint;
        MaximumNumberOfDirCacheBuffers : longint;
        NumberOfDirectoryCacheBuffers : longint;
        DCMinimumNonReferencedTime : longint;
        DCWaitTimeBeforeNewBuffer : longint;
        DCMaximumConcurrentWrites : longint;
        DCDirtyWaitTime : longint;
        debugCounters : array[0..3] of longint;
        PercentOfVolumeUsedByDirs : longint;
     end;

   PGetDirCacheInfoStructure = ^TGetDirCacheInfoStructure;
   TGetDirCacheInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        dirCacheInfo : TDirectoryCacheInformation;
     end;

   PGetOSVersionInfoStructure = ^TGetOSVersionInfoStructure;
   TGetOSVersionInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        OSMajorVersion : byte;
        OSMinorVersion : byte;
        OSRevision : byte;
        accountVersion : byte;
        VAPVersion : byte;
        queueingVersion : byte;
        securityRestLvl : byte;
        bridgingSupport : byte;
        maxNumOfVol : longint;
        maxNumOfConn : longint;
        maxNumOfUsers : longint;
        maxNumOfnameSpaces : longint;
        maxNumOfLANS : longint;
        maxNumOfMedias : longint;
        maxNumOfStacks : longint;
        maxDirDepth : longint;
        maxDataStreams : longint;
        maxNumOfSpoolPr : longint;
        serverSerialNumber : longint;
        serverApplicationNumber : word;
     end;

   PGetActiveConnListByTypeStructure = ^TGetActiveConnListByTypeStructure;
   TGetActiveConnListByTypeStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        ActiveConnBitList : array[0..511] of byte;
     end;

   PRTagStructure = ^TRTagStructure;
   TRTagStructure = record
        rTagNumber : longint;
        signature : longint;
        count : longint;
        name : byte;
     end;

   PGetNLMResourceTagList = ^TGetNLMResourceTagList;
   TGetNLMResourceTagList = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        totalNumOfRTags : longint;
        currentNumOfRTags : longint;
        RTagStart : TRTagStructure;
     end;

   PGetActiveLANBoardListStructure = ^TGetActiveLANBoardListStructure;
   TGetActiveLANBoardListStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        maxNumOfLANs : longint;
        itemsCount : longint;
        boardNumbers : longint;
     end;

   PDriverConfigStructure = ^TDriverConfigStructure;
   TDriverConfigStructure = record
        DriverCFG_MajorVersion : byte;
        DriverCFG_MinorVersion : byte;
        DriverNodeAddress : array[0..5] of byte;
        DriverModeFlags : word;
        DriverBoardNumber : word;
        DriverBoardInstance : word;
        DriverMaximumSize : longint;
        DriverMaxRecvSize : longint;
        DriverRecvSize : longint;
        DriverCardName : longint;
        DriverShortName : longint;
        DriverMediaType : longint;
        DriverCardID : word;
        DriverMediaID : word;
        DriverTransportTime : word;
        DriverReserved : array[0..15] of byte;
        DriverMajorVersion : byte;
        DriverMinorVersion : byte;
        DriverFlags : word;
        DriverSendRetries : word;
        DriverLink : longint;
        DriverSharingFlags : word;
        DriverSlot : word;
        DriverIOPortsAndLengths : array[0..3] of word;
        DriverMemoryDecode0 : longint;
        DriverLength0 : word;
        DriverMemoryDecode1 : longint;
        DriverLength1 : word;
        DriverInterrupt : array[0..1] of byte;
        DriverDMAUsage : array[0..1] of byte;
        DriverResourceTag : longint;
        DriverConfig : longint;
        DriverCommandString : longint;
        DriverLogicalName : array[0..17] of byte;
        DriverLinearMemory : array[0..1] of longint;
        DriverChannelNumber : word;
        DriverIOReserved : array[0..5] of byte;
     end;

   PGetLANConfigInfoStructure = ^TGetLANConfigInfoStructure;
   TGetLANConfigInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        LANConfig : TDriverConfigStructure;
     end;

   PCommonLANStructure = ^TCommonLANStructure;
   TCommonLANStructure = record
        notSupportedMask : longint;
        TotalTxPacketCount : longint;
        TotalRxPacketCount : longint;
        NoECBAvailableCount : longint;
        PacketTxTooBigCount : longint;
        PacketTxTooSmallCount : longint;
        PacketRxOverflowCount : longint;
        PacketRxTooBigCount : longint;
        PacketRxTooSmallCount : longint;
        PacketTxMiscErrorCount : longint;
        PacketRxMiscErrorCount : longint;
        RetryTxCount : longint;
        ChecksumErrorCount : longint;
        HardwareRxMismatchCount : longint;
        TotalTxOKByteCountLow : longint;
        TotalTxOKByteCountHigh : longint;
        TotalRxOKByteCountLow : longint;
        TotalRxOKByteCountHigh : longint;
        TotalGroupAddrTxCount : longint;
        TotalGroupAddrRxCount : longint;
        AdapterResetCount : longint;
        AdapterOprTimeStamp : longint;
        AdapterQueDepth : longint;
        MediaSpecificCounter1 : longint;
        MediaSpecificCounter2 : longint;
        MediaSpecificCounter3 : longint;
        MediaSpecificCounter4 : longint;
        MediaSpecificCounter5 : longint;
        MediaSpecificCounter6 : longint;
        MediaSpecificCounter7 : longint;
        MediaSpecificCounter8 : longint;
        MediaSpecificCounter9 : longint;
        MediaSpecificCounter10 : longint;
        ValidMask1 : longint;
        MediaSpecificCounter11 : longint;
        MediaSpecificCounter12 : longint;
        MediaSpecificCounter13 : longint;
        MediaSpecificCounter14 : longint;
     end;

   PGetLANCommonCountersStructure = ^TGetLANCommonCountersStructure;
   TGetLANCommonCountersStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        statMajorVersion : byte;
        statMinorVersion : byte;
        totalCommonCnts : longint;
        totalCntBlocks : longint;
        customCounters : longint;
        nextCntBlock : longint;
        info : TCommonLANStructure;
     end;

   PCustomCountersInfo = ^TCustomCountersInfo;
   TCustomCountersInfo = record
        value : longint;
        stringLength : byte;
        stringStart : byte;
     end;

   PGetCustomCountersInfoStructure = ^TGetCustomCountersInfoStructure;
   TGetCustomCountersInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        moreflag : word;
        numberOfCustomCounters : longint;
        startOfCustomCounters : byte;
     end;

   PLSLInformation = ^TLSLInformation;
   TLSLInformation = record
        RxBufs : longint;
        RxBufs75PerCent : longint;
        RxBufsCheckedOut : longint;
        RxBufMaxSize : longint;
        MaxPhysicalSize : longint;
        LastTimeRxBufAllocated : longint;
        MaxNumbersOfProtocols : longint;
        MaxNumbersOfMediaTypes : longint;
        TotalTXPackets : longint;
        GetECBBfrs : longint;
        GetECBFails : longint;
        AESEventCounts : longint;
        PostpondedEvents : longint;
        ECBCxlFails : longint;
        ValidBfrsReused : longint;
        EnqueuedSendCnt : longint;
        TotalRXPackets : longint;
        UnclaimedPackets : longint;
        StatisticsTableMajorVersion : byte;
        StatisticsTableMinorVersion : byte;
     end;

   PGetLSLInfoStructure = ^TGetLSLInfoStructure;
   TGetLSLInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        LSLInfo : TLSLInformation;
     end;

   PLogicalBoard = ^TLogicalBoard;
   TLogicalBoard = record
        LogTtlTxPackets : longint;
        LogTtlRxPackets : longint;
        LogUnclaimedPackets : longint;
        reserved : longint;
     end;

   PGetLSLBoardStatsStructure = ^TGetLSLBoardStatsStructure;
   TGetLSLBoardStatsStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        boardStats : TLogicalBoard;
     end;

   PCopyOfPMStructure = ^TCopyOfPMStructure;
   TCopyOfPMStructure = record
        f1 : array[0..63] of byte;
        f2 : longint;
        f3 : longint;
     end;

   PCopyOfGenericInfoDef = ^TCopyOfGenericInfoDef;
   TCopyOfGenericInfoDef = record
        mediaInfo : TCopyOfPMStructure;
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

   PGetMManagerObjInfoStructure = ^TGetMManagerObjInfoStructure;
   TGetMManagerObjInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        info : TCopyOfGenericInfoDef;
     end;

   PGetMMObjectListsStructure = ^TGetMMObjectListsStructure;
   TGetMMObjectListsStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        nextStartNum : longint;
        objectCount : longint;
        objects : longint;
     end;

   PGetMMObjectChildListStructure = ^TGetMMObjectChildListStructure;
   TGetMMObjectChildListStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        nextStartNum : longint;
        objectCount : longint;
        objects : longint;
     end;

   PVolumeSegmentStructure = ^TVolumeSegmentStructure;
   TVolumeSegmentStructure = record
        segmentDevice : longint;
        segmentOffset : longint;
        segmentSize : longint;
     end;

   PGetVolumeSegmentListStructure = ^TGetVolumeSegmentListStructure;
   TGetVolumeSegmentListStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        numberOfSegments : longint;
        segment : TVolumeSegmentStructure;
     end;

   PProtocolStackInfo = ^TProtocolStackInfo;
   TProtocolStackInfo = record
        stackNumber : longint;
        stackName : array[0..15] of byte;
     end;

   PGetActiveProtocolStackStructure = ^TGetActiveProtocolStackStructure;
   TGetActiveProtocolStackStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        maxNumberOfStacks : longint;
        stackCount : longint;
        nextStartNumber : longint;
        stackInfo : TProtocolStackInfo;
     end;

   PGetProtocolConfigStructure = ^TGetProtocolConfigStructure;
   TGetProtocolConfigStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        configMajorVersion : byte;
        configMinorVerstion : byte;
        stackMajorVersion : byte;
        stackMinorVersion : byte;
        shortName : array[0..15] of byte;
        fullNameLength : byte;
        fullName : byte;
     end;

   PGetProtocolStatsStructure = ^TGetProtocolStatsStructure;
   TGetProtocolStatsStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        StatMajorVersion : byte;
        StatMinorVersion : byte;
        GenericCounters : word;
        ValidCntsMask : longint;
        TotalTxPackets : longint;
        TotalRxPackets : longint;
        IgnoredRxPackets : longint;
        NumberOfCustomCounters : word;
     end;

   PProtocolCustomInfo = ^TProtocolCustomInfo;
   TProtocolCustomInfo = record
        value : longint;
        length : byte;
        customData : byte;
     end;

   PGetProtocolCustomInfoStructure = ^TGetProtocolCustomInfoStructure;
   TGetProtocolCustomInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        customCount : longint;
        info : TProtocolCustomInfo;
     end;

   PGetProtocolByMediaStructure = ^TGetProtocolByMediaStructure;
   TGetProtocolByMediaStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        stackIDCount : longint;
        stackID : longint;
     end;

   PGetProtocolByBoardStructure = ^TGetProtocolByBoardStructure;
   TGetProtocolByBoardStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        stackIDCount : longint;
        stackID : longint;
     end;

   PGetMediaNameByNumberStructure = ^TGetMediaNameByNumberStructure;
   TGetMediaNameByNumberStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        mediaNameLength : byte;
        mediaName : byte;
     end;

   PGetMediaNumberListStructure = ^TGetMediaNumberListStructure;
   TGetMediaNumberListStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        maxNumberOfMedia : longint;
        mediaListCount : longint;
        mediaList : longint;
     end;

   PGetRouterAndSAPInfoStructure = ^TGetRouterAndSAPInfoStructure;
   TGetRouterAndSAPInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        RIPSocketNumber : longint;
        routerDownFlag : longint;
        trackOnFlag : longint;
        extRouterActiveFlag : longint;
        SAPSocketNumber : longint;
        rpyNearestServerFlag : longint;
     end;

   PGetNetRouterInfoStructure = ^TGetNetRouterInfoStructure;
   TGetNetRouterInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        netIDNumber : longint;
        hopsToNet : word;
        netStatus : word;
        timeToNet : word;
     end;

   PRoutersInfoStructure = ^TRoutersInfoStructure;
   TRoutersInfoStructure = record
        node : array[0..5] of byte;
        connectedLAN : longint;
        hopsToNetCount : word;
        timeToNet : word;
     end;

   PGetNetworkRoutersInfoStructure = ^TGetNetworkRoutersInfoStructure;
   TGetNetworkRoutersInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        numberOfEntries : longint;
        info : TRoutersInfoStructure;
     end;

   PKnownNetworksStructure = ^TKnownNetworksStructure;
   TKnownNetworksStructure = record
        netIDNumber : longint;
        hopsToNet : word;
        netStatus : word;
        timeToNet : word;
     end;

   PGetKnownNetworksStructure = ^TGetKnownNetworksStructure;
   TGetKnownNetworksStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        numberOfEntries : longint;
        info : TKnownNetworksStructure;
     end;

   PGetServerInfoStructure = ^TGetServerInfoStructure;
   TGetServerInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        serverAddress : array[0..11] of byte;
        hopsToServer : word;
     end;

   PServerSourceInfoStructure = ^TServerSourceInfoStructure;
   TServerSourceInfoStructure = record
        serverNode : array[0..5] of byte;
        connectLAN : longint;
        hopCount : word;
     end;

   PGetServerSourcesStructure = ^TGetServerSourcesStructure;
   TGetServerSourcesStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        numberOfEntries : longint;
        info : TServerSourceInfoStructure;
     end;

   PKnownServerStructure = ^TKnownServerStructure;
   TKnownServerStructure = record
        serverAddress : array[0..11] of byte;
        hopCount : word;
        serverNameLength : byte;
        name : byte;
     end;

   PGetKnownServersInfoStructure = ^TGetKnownServersInfoStructure;
   TGetKnownServersInfoStructure = record
        currentServerTime : longint;
        vConsoleVersion : byte;
        vConsoleRevision : byte;
        reserved : word;
        numberOfEntries : longint;
        info : TKnownServerStructure;
     end;

{$include npackoff.inc}

function SSGetActiveConnListByType(startConnNumber:longint; connType:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetActiveConnListByType';
function SSGetActiveLANBoardList(startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetActiveLANBoardList';
function SSGetActiveProtocolStacks(startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetActiveProtocolStacks';
function SSGetCacheInfo(pointer:PBYTE; bufferLen:word):longint;cdecl;external Clib name 'SSGetCacheInfo';
function SSGetCPUInfo(CPUNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetCPUInfo';
function SSGetDirCacheInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetDirCacheInfo';
function SSGetFileServerInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetFileServerInfo';
function SSGetFileSystemInfo(fileSystemID:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetFileSystemInfo';
function SSGetGarbageCollectionInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetGarbageCollectionInfo';
function SSGetIPXSPXInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetIPXSPXInfo';
function SSGetKnownNetworksInfo(startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetKnownNetworksInfo';
function SSGetKnownServersInfo(startNumber:longint; serverType:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetKnownServersInfo';
function SSGetLANCommonCounters(boardNumber:longint; blockNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetLANCommonCounters';
function SSGetLANConfiguration(boardNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetLANConfiguration';
function SSGetLANCustomCounters(boardNumber:longint; startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetLANCustomCounters';
function SSGetLSLInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetLSLInfo';
function SSGetLSLLogicalBoardStats(boardNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetLSLLogicalBoardStats';
function SSGetLoadedMediaNumberList(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetLoadedMediaNumberList';
function SSGetMediaManagerObjChildList(startNumber:longint; objType:longint; parentObjNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetMediaManagerObjChildList';
function SSGetMediaManagerObjInfo(objNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetMediaManagerObjInfo';
function SSGetMediaManagerObjList(startNumber:longint; objType:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetMediaManagerObjList';
function SSGetMediaNameByNumber(mediaNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetMediaNameByNumber';
function SSGetNetRouterInfo(networkNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetNetRouterInfo';
function SSGetNetworkRoutersInfo(networkNumber:longint; startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetNetworkRoutersInfo';
function SSGetNLMInfo(NLMNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetNLMInfo';
function SSGetNLMLoadedList(startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetNLMLoadedList';
function SSGetNLMResourceTagList(NLMNumber:longint; startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetNLMResourceTagList';
function SSGetOSVersionInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetOSVersionInfo';
function SSGetPacketBurstInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetPacketBurstInfo';
function SSGetProtocolConfiguration(startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetProtocolConfiguration';
function SSGetProtocolCustomInfo(stackNumber:longint; customStartNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetProtocolCustomInfo';
function SSGetProtocolNumbersByLANBoard(LANBoardNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetProtocolNumbersByLANBoard';
function SSGetProtocolNumbersByMedia(mediaNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetProtocolNumbersByMedia';
function SSGetProtocolStatistics(stackNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetProtocolStatistics';
function SSGetRouterAndSAPInfo(buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetRouterAndSAPInfo';
function SSGetServerInfo(serverType:longint; nameLength:byte; name:PBYTE; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetServerInfo';
function SSGetServerSourcesInfo(startNumber:longint; serverType:longint; nameLength:byte; name:PChar; buffer:pointer;
           bufferLen:word):longint;cdecl;external Clib name 'SSGetServerSourcesInfo';
function SSGetUserInfo(connectionNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetUserInfo';
function SSGetVolumeSegmentList(volumeNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetVolumeSegmentList';
function SSGetVolumeSwitchInfo(startNumber:longint; buffer:pointer; bufferLen:word):longint;cdecl;external Clib name 'SSGetVolumeSwitchInfo';

{------------------------------------------------------------------------------}
const _MAX_LOGREC_NAME = 128;
     _MAX_SEMAPHORE_NAME = 128;

function  ClearFile(fileName:Pchar):longint; cdecl;external Clib name 'ClearFile';
procedure ClearFileSet;                      cdecl;external Clib name 'ClearFileSet';
function  ClearLogicalRecord(logicalRecordName:Pchar):longint;cdecl;external Clib name 'ClearLogicalRecord';
procedure ClearLogicalRecordSet;             cdecl;external Clib name 'ClearLogicalRecordSet';
function  ClearPhysicalRecord(fileHandle,recordStartOffset,recordLength:longint):longint;cdecl;external Clib name 'ClearPhysicalRecord';
procedure ClearPhysicalRecordSet;cdecl;external Clib name 'ClearPhysicalRecordSet';
function  CloseSemaphore(semaphoreHandle:longint):longint;cdecl;external Clib name 'CloseSemaphore';
function  ExamineSemaphore(semaphoreHandle:longint; semaphoreValue:Plongint; openCount:PWORD):longint;cdecl;external Clib name 'ExamineSemaphore';
function  ExamineSemaphore(semaphoreHandle:longint; var semaphoreValue:longint; var openCount:word):longint;cdecl;external Clib name 'ExamineSemaphore';
function  LockFileSet(timeoutLimit:word):longint;cdecl;external Clib name 'LockFileSet';
function  LockLogicalRecordSet(timeoutLimit:word):longint;cdecl;external Clib name 'LockLogicalRecordSet';
function  LockPhysicalRecordSet(lockDirective:byte; timeoutLimit:word):longint;cdecl;external Clib name 'LockPhysicalRecordSet';
function  LogFile(fileName:Pchar; lockDirective:byte; timeoutLimit:word):longint;cdecl;external Clib name 'LogFile';
function  LogLogicalRecord(logicalRecordName:Pchar; lockDirective:byte; timeoutLimit:word):longint;cdecl;external Clib name 'LogLogicalRecord';
function  LogPhysicalRecord(fileHandle,recordStartOffset,recordLength:longint; lockDirective:byte; timeoutLimit:word):longint;cdecl;external Clib name 'LogPhysicalRecord';
function  OpenSemaphore(semaphoreName:Pchar; initialValue:longint; semaphoreHandle:Plongint; openCount:PWORD):longint;cdecl;external Clib name 'OpenSemaphore';
function  OpenSemaphore(semaphoreName:Pchar; initialValue:longint; var semaphoreHandle:longint; var openCount:word):longint;cdecl;external Clib name 'OpenSemaphore';
function  ReleaseFile(fileName:Pchar):longint;cdecl;external Clib name 'ReleaseFile';
procedure ReleaseFileSet;cdecl;external Clib name 'ReleaseFileSet';
function  ReleaseLogicalRecord(logicalRecordName:Pchar):longint;cdecl;external Clib name 'ReleaseLogicalRecord';
procedure ReleaseLogicalRecordSet;cdecl;external Clib name 'ReleaseLogicalRecordSet';
function  ReleasePhysicalRecord(fileHandle,recordStartOffset,recordLength:longint):longint;cdecl;external Clib name 'ReleasePhysicalRecord';
procedure ReleasePhysicalRecordSet;cdecl;external Clib name 'ReleasePhysicalRecordSet';
function  SignalSemaphore(semaphoreHandle:longint):longint;cdecl;external Clib name 'SignalSemaphore';
function  WaitOnSemaphore(semaphoreHandle:longint; timeoutLimit:word):longint;cdecl;external Clib name 'WaitOnSemaphore';

{------------------------------------------------------------------------------}

function TTSAbortTransaction:longint;cdecl;external 'clib' name 'TTSAbortTransaction';
function TTSBeginTransaction:longint;cdecl;external 'clib' name 'TTSBeginTransaction';
function TTSEndTransaction(transactionNumber:Plongint):longint;cdecl;external 'clib' name 'TTSEndTransaction';
function TTSEndTransaction(var transactionNumber:longint):longint;cdecl;external 'clib' name 'TTSEndTransaction';
function TTSGetApplicationThresholds(logicalRecordLockThreshold:pbyte; physicalRecordLockThreshold:pbyte):longint;cdecl;external 'clib' name 'TTSGetApplicationThresholds';
function TTSGetWorkstationThresholds(logicalRecordLockThreshold:pbyte; physicalRecordLockThreshold:pbyte):longint;cdecl;external 'clib' name 'TTSGetWorkstationThresholds';
function TTSIsAvailable:longint;cdecl;external 'clib' name 'TTSIsAvailable';
function TTSSetApplicationThresholds(logicalRecordLockThreshold:pbyte; physicalRecordLockThreshold:pbyte):longint;cdecl;external 'clib' name 'TTSSetApplicationThresholds';
function TTSSetWorkstationThresholds(logicalRecordLockThreshold:pbyte; physicalRecordLockThreshold:pbyte):longint;cdecl;external 'clib' name 'TTSSetWorkstationThresholds';
function TTSTransactionStatus(transactionNumber:longint):longint;cdecl;external 'clib' name 'TTSTransactionStatus';

{------------------------------------------------------------------------------}

implementation

end.
