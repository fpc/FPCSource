{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Conversion of windbase_edb.h by Carolos Foscolos
 **********************************************************************
}

const
   EDB_MOUNT_FLAG = $80000000;   

//-------------------------------------------------------------------------
// Property Flags
//

const
   DB_PROP_NOTNULL = $00000001;   
   DB_PROP_COMPRESSED = $00000002;   
//--------------------------------------------------------------------------
// Option Ids
//
   DBOPT_SESSION_MAX_CHANGES = $01;   
   DBOPT_SESSION_LOCK_TIMEOUT = $02;   

//--------------------------------------------------------------------------
// New Tracking Types
//
   DB_TRACK_INSERT = $00000001;   
   DB_TRACK_UPDATE = $00000002;   
   DB_TRACK_DELETE = $00000003;   

// Tracking Modes
//
   DB_TRACK_LEVEL = $0000007C; // 1111100
   DB_TRACK_LEVEL_RECORD = $00000004;   
   DB_TRACK_LEVEL_PROPINCLUSIVE = $00000008;   
   DB_TRACK_LEVEL_PROPEXCLUSIVE = $00000010;   
   DB_TRACK_LEVEL_PROPVALCHANGE = $00000020;   

// Row Identifier for Tracking
//
   DB_TRACK_ROWIDENTITY = $00000003; // 000011
   DB_TRACK_ROWIDENTITY_PK = $00000001;   

// Sync Session Flags
//
   DB_SYNCSESSION_COMPLETE = $00000000;   
   DB_SYNCSESSION_PARTIAL = $00000001;   
   DB_SYNCSESSION_FAILED = $00000002;   
   DB_SYNCSESSION_TRACKSERVER = $00000000;   
   DB_SYNCSESSION_NOTRACK = $00000001;   
   DB_SYNCSESSION_OVERRRIDELOCALCHANGES = $00000002;   

//--------------------------------------------------------------------------
// Tracking generations: CEDBGEN
//
type
   CEDBGEN = ULONGLONG;

//--------------------------------------------------------------------------
// Default generation values
//
const
   CEDBGEN_DEFAULT = $FFFFFFFFFFFFFFFF;   

//--------------------------------------------------------------------------
// CECHANGEINFO
//
type
   _CECHANGEINFO = record
        dwChangeType : DWORD;
        stLastModified : SYSTEMTIME;
        genLastModified : CEDBGEN;
     end;
   CECHANGEINFO = _CECHANGEINFO;
   PCECHANGEINFO = ^_CECHANGEINFO;

//--------------------------------------------------------------------------
// CEDBISOLATIONLEVEL
//
   CEDBISOLATIONLEVEL = longint;

const
   CEDB_ISOLEVEL_DEFAULT = 1;   
   CEDB_ISOLEVEL_READCOMMITTED = 1;   
   CEDB_ISOLEVEL_REPEATABLEREAD = 2;   
   CEDB_ISOLEVEL_SERIALIZABLE = 3;   
   CEDB_ISOLEVEL_LAST = CEDB_ISOLEVEL_SERIALIZABLE;   

//--------------------------------------------------------------------------
// Limits - apply to various volume options
//
   CEDB_CB_MIN_BUFFER_POOL = $00040000;   
   CEDB_CB_MAX_BUFFER_POOL = $10000000;   
   CEDB_AUTO_SHRINK_OFF = $0;   
   CEDB_AUTO_SHRINK_THRESHOLD_MAX = 100;   
   CEDB_FLUSH_INTERVAL_MIN = 1;   
   CEDB_FLUSH_INTERVAL_MAX = 1000;   
   CEDB_C_MIN_MAX_NOTIFY_CHANGES = 0;   
   CEDB_C_MAX_MAX_NOTIFY_CHANGES = $FFFFFFFF;   

//--------------------------------------------------------------------------
// CCH_MAX_PASSWORD
//
  ///{$IFNDEF CCH_MAX_PASSWORD}
const
   CCH_MAX_PASSWORD = 40;   
  ///{$DEFINE CCH_MAX_PASSWORD}
  ///{$ENDIF}

//--------------------------------------------------------------------------
// CEVOLUMEOPTIONS
//
// wVersion         -   Structure version
// cbBufferPool     -   Count of bytes in the buffer pool
// dwAutoShrinkPercent- Percent free used to kick off autoshrink
// dwFlushInterval  -   Interval between two flushes
// cMaxNotifyChanges-   Max number of notifications on a database to track
// dwDefaultTimeout -   Interval in milliseconds to
//                      wait on an unavailable lock
// wszPassword      -   Volume password
// dwFlags          -   Contains a bitflag that specifies which of the
//                      fields contain valid data (see above for values)
//

const
   CEVOLUMEOPTIONS_VERSION = 1;   
type

   _CEVOLUMEOPTIONS = record
        wVersion : WORD;
        cbBufferPool : DWORD;
        dwAutoShrinkPercent : DWORD;
        dwFlushInterval : DWORD;
        cMaxNotifyChanges : DWORD;
        dwDefaultTimeout : DWORD;
        wszPassword : array[0..CCH_MAX_PASSWORD] of WCHAR;
        dwFlags : DWORD;
     end;
   CEVOLUMEOPTIONS = _CEVOLUMEOPTIONS;
   PCEVOLUMEOPTIONS = ^_CEVOLUMEOPTIONS;

//-------------------------------------------------------------------------
// Flags for CeMountDBVolEx
//-------------------------------------------------------------------------
// Used to overwrite the default buffer pool size
//
const
   CEDB_BUFFERPOOL = $00000001;   

// Used to overwrite the default auto shrink percent.
//
   CEDB_AUTOSHRINKPERCENT = $00000002;   

// Used to overwrite the default flush interval.
//
   CEDB_FLUSHINTERVAL = $00000004;   

// Used to overwrite the default maximum number of
// individual notifications changes to track.
//
   CEDB_MAXNOTIFYCHANGES = $00000008;   

// Used to overwrite the default lock time out.
//
   CEDB_DEFAULTTIMEOUT = $00000010;   

// Used to indicate that the password field contains a password
//
   CEDB_PASSWORD = $00000020;   

//-------------------------------------------------------------------------
// CEPROPSPEC
//
// wVersion - Structure version
// propid   - Property id
// dwFlags  - Combination of the following:
//              DB_PROP_NOTNULL     - the column does not accept nulls
//              DB_PROP_COMPRESSED  - the column should be compressed
//                                    (valid for long values only)
// pwszPropName - The readable name of the property
// cchPropName  - The user will fill this field with the number of
//              wide characters available in pwszPropName.
//
   CEPROPSPEC_VERSION = 1;   
type

   _CEPROPSPEC = record
        wVersion : WORD;
        propid : CEPROPID;
        dwFlags : DWORD;
        pwszPropName : LPWSTR;
        cchPropName : DWORD;
     end;
   CEPROPSPEC = _CEPROPSPEC;
   PCEPROPSPEC = ^_CEPROPSPEC;


//--------------------------------------------------------------------------
// New functions
//
function CeCreateDatabaseWithProps(pGuid:PCEGUID; var pInfo:CEDBASEINFOEX; cProps:DWORD; var prgProps:CEPROPSPEC):CEOID;cdecl;external KernelDLL name 'CeCreateDatabaseWithProps';
function CeAddDatabaseProps(pGuid:PCEGUID; oidDb:CEOID; cProps:DWORD; var prgProps:CEPROPSPEC):BOOL;cdecl;external KernelDLL name 'CeAddDatabaseProps';
function CeGetDatabaseProps(hHandle:HANDLE; var pcPropId:WORD; var prgPropId:CEPROPID; var prgProps:CEPROPSPEC):BOOL;cdecl;external KernelDLL name 'CeGetDatabaseProps';
function CeRemoveDatabaseProps(pGuid:PCEGUID; oidDb:CEOID; cPropID:DWORD; var prgPropID:CEPROPID):BOOL;cdecl;external KernelDLL name 'CeRemoveDatabaseProps';
function CeMountDBVolEx(pGuid:PCEGUID; pwszDBVol:LPWSTR; pOptions:PCEVOLUMEOPTIONS; dwFlags:DWORD):BOOL;cdecl;external KernelDLL name 'CeMountDBVolEx';

// Session API's
function CeCreateSession(var pGuid:CEGUID):HANDLE;cdecl;external KernelDLL name 'CeCreateSession';
function CeSetSessionOption(hSession:HANDLE; ulOptionId:ULONG; dwValue:DWORD):BOOL;cdecl;external KernelDLL name 'CeSetSessionOption';
function CeGetDatabaseSession(hDatabase:HANDLE):HANDLE;cdecl;external KernelDLL name 'CeGetDatabaseSession';
function CeBeginTransaction(hSession:HANDLE; isoLevel:CEDBISOLATIONLEVEL):BOOL;cdecl;external KernelDLL name 'CeBeginTransaction';
function CeEndTransaction(hSession:HANDLE; fCommit:BOOL):BOOL;cdecl;external KernelDLL name 'CeEndTransaction';
function CeOpenDatabaseInSession(hSession:HANDLE; pGuid:PCEGUID; poid:PCEOID; pwszName:LPWSTR; pSort:PSORTORDERSPECEX;
           dwFlags:DWORD; pRequest: PCENOTIFYREQUEST):HANDLE;cdecl;external KernelDLL name 'CeOpenDatabaseInSession';

// Stream API's
function CeOpenStream(hDatabase:HANDLE; propid:CEPROPID; dwMode:DWORD):HANDLE;cdecl;external KernelDLL name 'CeOpenStream';
function CeStreamRead(hStream:HANDLE; var prgbBuffer:BYTE; cbRead:DWORD; var pcbRead:DWORD):BOOL;cdecl;external KernelDLL name 'CeStreamRead';
function CeStreamWrite(hStream:HANDLE; var prgbBuffer:BYTE; cbWrite:DWORD; var pcbWritten:DWORD):BOOL;cdecl;external KernelDLL name 'CeStreamWrite';
function CeStreamSaveChanges(hStream:HANDLE):BOOL;cdecl;external KernelDLL name 'CeStreamSaveChanges';
function CeStreamSeek(hStream:HANDLE; cbMove:DWORD; dwOrigin:DWORD; var pcbNewOffset:DWORD):BOOL;cdecl;external KernelDLL name 'CeStreamSeek';
function CeStreamSetSize(hStream:HANDLE; cbSize:DWORD):BOOL;cdecl;external KernelDLL name 'CeStreamSetSize';

//-----------------------------------------------------------------------
// Tracking functions
//
function CeAddSyncPartner(pVolGuid:PCEGUID; pSyncPartnerGuid:PCEGUID; pwszFriendlyName:LPCWSTR; pwszFullName:LPCWSTR):BOOL;cdecl;external KernelDLL name 'CeAddSyncPartner';
function CeRemoveSyncPartner(pVolGuid:PCEGUID; pSyncPartnerGuid:PCEGUID):BOOL;cdecl;external KernelDLL name 'CeRemoveSyncPartner';
function CeTrackDatabase(pVolGuid:PCEGUID; pSyncPartnerGuid:PCEGUID; oidDB:CEOID; dwTrackingFlags:DWORD):BOOL;cdecl;external KernelDLL name 'CeTrackDatabase';
function CeRemoveDatabaseTracking(pVolGuid:PCEGUID; pSyncPartnerGuid:PCEGUID; oidDB:CEOID):BOOL;cdecl;external KernelDLL name 'CeRemoveDatabaseTracking';
function CeTrackProperty(pVolGuid:PCEGUID; oidDB:CEOID; propid:CEPROPID; fAddToScheme:BOOL):BOOL;cdecl;external KernelDLL name 'CeTrackProperty';
function CeBeginSyncSession(hSession:HANDLE; pSyncPartnerGuid:PCEGUID; genFrom:CEDBGEN; genTo:CEDBGEN; dwFlags:DWORD;
           var pGenCur:CEDBGEN):BOOL;cdecl;external KernelDLL name 'CeBeginSyncSession';
function CeEndSyncSession(hSession:HANDLE; dwOutcome:DWORD):BOOL;cdecl;external KernelDLL name 'CeEndSyncSession';
function CeGetChangedRecordCnt(hSession:HANDLE; oidDB:CEOID; var pdwCnt:DWORD):BOOL;cdecl;external KernelDLL name 'CeGetChangedRecordCnt';
function CeGetChangedRecords(hSession:HANDLE; oidDB:CEOID; dwChangeType:DWORD):HANDLE;cdecl;external KernelDLL name 'CeGetChangedRecords';
function CeFindNextChangedRecord(hChangeEnum:HANDLE):CEOID;cdecl;external KernelDLL name 'CeFindNextChangedRecord';
function CeGetPropChangeInfo(hChangeEnum:HANDLE; oidChangedRecord:CEOID; propid:CEPROPID; var pfPropChanged:BOOL):BOOL;cdecl;external KernelDLL name 'CeGetPropChangeInfo';
function CeGetRecordChangeInfo(hChangeEnum:HANDLE; oidChangedRecord:CEOID; var pInfo:CECHANGEINFO):BOOL;cdecl;external KernelDLL name 'CeGetRecordChangeInfo';
function CeMarkRecord(hChangeEnum:HANDLE; oidChangedRecord:CEOID; fChanged:BOOL):BOOL;cdecl;external KernelDLL name 'CeMarkRecord';
function CeAttachCustomTrackingData(hDB:HANDLE; oidChangedRecord:CEOID; var rgbData:BYTE; ccb:DWORD):BOOL;cdecl;external KernelDLL name 'CeAttachCustomTrackingData';
function CeGetCustomTrackingData(hChangeEnum:HANDLE; oidChangedRecord:CEOID; var rgbData:BYTE; var pccb:DWORD):BOOL;cdecl;external KernelDLL name 'CeGetCustomTrackingData';
function CePurgeTrackingData(pVolGuid:PCEGUID; pSyncPartnerGuid:PCEGUID; var pstThreshold:SYSTEMTIME):BOOL;cdecl;external KernelDLL name 'CePurgeTrackingData';
function CePurgeTrackingGenerations(pVolGuid:PCEGUID; pSyncPartnerGuid:PCEGUID; genThreshold:CEDBGEN):BOOL;cdecl;external KernelDLL name 'CePurgeTrackingGenerations';

