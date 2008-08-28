{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    conversion of windbase.h by Carolos Foscolos
 **********************************************************************

 Module:  windbase.pp
 Purpose: Master include file for WINCE Database APIs.
 Changes: 2008/08/20 - Carolos: * Fixed PCEOID, CREATE_INVALIDGUID and cleaned up file.
                                * Included windbase_edb.inc.
}

{$mode objfpc}
unit windbase;
interface
uses windows;

//{$DEFINE EDB}
{$ifndef EDB}
  {$WARNING If target platform is Windows Mobile 5.0 or greater, Please use the -dEDB compiler option to use EDB instead of CEDB.}
  // CEDB is deprecated and will no longer work in future versions of Windows Mobile.
  // It is strongly recommended that you #define EDB which will enable you to use EDB instead of CEDB.
{$endif}


{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

{$calling cdecl} //convention is cdecl for WinCE API

type
  //CEOID: Unique identifier for all WINCE objects.
  //Comment: Every WINCE object can be efficiently referred to by its OID.
  //         OID's are unique in the system and are not reused.
  CEOID = DWORD;
  TCEOID = DWORD;
  PCEOID = ^CEOID;

  CEGUID = record
    Data1 : DWORD;
    Data2 : DWORD;
    Data3 : DWORD;
    Data4 : DWORD;
  end;
  _CEGUID = CEGUID;
  PCEGUID = ^CEGUID;
  TCEGUID = CEGUID;

const
  WM_DBNOTIFICATION = $03FD;
  CEDB_EXNOTIFICATION = $00000001;

type
  CENOTIFYREQUEST = record
    dwSize : DWORD; //Must be set to the structure size
    hwnd : HWND;    //Window handle for notifications to be posted
    dwFlags : DWORD;
    hHeap : HANDLE; // heap from which to allocate EX-NOTIFICATIONS
    dwParam : DWORD;
  end;
  _CENOTIFYREQUEST = CENOTIFYREQUEST;
  PCENOTIFYREQUEST = ^CENOTIFYREQUEST;
  TCENOTIFYREQUEST = CENOTIFYREQUEST;

  CENOTIFICATION = record
    dwSize : DWORD;
    dwParam : DWORD;
    uType : UINT;
    guid : CEGUID;
    oid : CEOID;
    oidParent : CEOID;
  end;
  _CENOTIFICATION = CENOTIFICATION;
  PCENOTIFICATION = ^CENOTIFICATION;
  TCENOTIFICATION = CENOTIFICATION;

  //CEFILEINFO: Contains information about a file object.
  CEFILEINFO = record
    dwAttributes : DWORD; //File attributes
    oidParent : CEOID;    //CEOID of parent directory
    szFileName : array[0..(MAX_PATH)-1] of WCHAR; //Full path name of the file
    ftLastChanged : FILETIME; //Time stamp of last change
    dwLength : DWORD;         //Length of file
  end;
  _CEFILEINFO = CEFILEINFO;
  PCEFILEINFO = ^CEFILEINFO;
  TCEFILEINFO = CEFILEINFO;

  //CEDIRINFO: Contains information about a directory object.
  CEDIRINFO = record
    dwAttributes : DWORD; //Directory attributes
    oidParent : CEOID;    //CEOID of parent directory
    szDirName : array[0..(MAX_PATH)-1] of WCHAR; //Full path name of the directory
  end;
  _CEDIRINFO = CEDIRINFO;
  PCEDIRINFO = ^CEDIRINFO;
  TCEDIRINFO = CEDIRINFO;

const
  DB_CEOID_CREATED           = WM_USER+$1; //DB_CEOID_CREATED: Msg sent on creation of new oid.
                                           //WParam = CEOID modified, LParam = CEOID's parent CEOID

  DB_CEOID_DATABASE_DELETED  = WM_USER+$2; //DB_CEOID_DATABASE_DELETED: Msg sent on deletion of database.
                                           //WParam = CEOID modified, LParam = CEOID's parent CEOID

  DB_CEOID_RECORD_DELETED    = WM_USER+$3; //DB_CEOID_RECORD_DELETED: Msg sent on deletion of record.
                                           //WParam = CEOID modified, LParam = CEOID's parent CEOID

  DB_CEOID_FILE_DELETED      = WM_USER+$4; //DB_CEOID_FILE_DELETED: Msg sent on deletion of file.
                                           //WParam = CEOID modified, LParam = CEOID's parent CEOID

  DB_CEOID_DIRECTORY_DELETED = WM_USER+$5; //DB_CEOID_DIRECTORY_DELETED: Msg sent on deletion of directory.
                                           //WParam = CEOID modified, LParam = CEOID's parent CEOID

  DB_CEOID_CHANGED           = WM_USER+$6; //DB_CEOID_CHANGED: Msg sent on item modification.
                                           //WParam = CEOID modified, LParam = CEOID's parent CEOID

  REPL_CHANGE_WILLCLEAR = $00000001; //Flags for CeGetReplChangeMask.

type
  STORE_INFORMATION = record
    dwStoreSize : DWORD;
    dwFreeSize : DWORD;
  end;
  LPSTORE_INFORMATION = ^STORE_INFORMATION;
  PSTORE_INFORMATION = ^STORE_INFORMATION;
  TSTORE_INFORMATION = STORE_INFORMATION;

function GetStoreInformation(lpsi:LPSTORE_INFORMATION):BOOL;cdecl;external KernelDLL name 'GetStoreInformation';

type
  //CEPROPID: PropID's for WINCE properties.
  //Comment: PropID's on the WINCE match PropID's used by Mapi1. The top 2 bytes are an ID
  //         and the low 2 bytes are the type. For a list of supported types look at the tags
  //         supported in <t CEVALUNION>. We reserve one bit (0x4000) in the type as the
  //         flag <b CEPROPVAL_NULL> as a special flag. It denotes that a property was not
  //         found in a Read call, or that the property should be deleted in a write call.

  CEPROPID = DWORD;
  PCEPROPID = ^CEPROPID;
  TCEPROPID = CEPROPID;

function TypeFromPropID(propid : longint) : WORD;

type
  //CERECORDINFO: Contains information about a record object.
  CERECORDINFO = record
    oidParent : CEOID; //CEOID of parent database
  end;
  _CERECORDINFO = CERECORDINFO;
  PCERECORDINFO = ^CERECORDINFO;
  TCERECORDINFO = CERECORDINFO;

const
  CEDB_SORT_DESCENDING      = $00000001;
  CEDB_SORT_CASEINSENSITIVE = $00000002;
  CEDB_SORT_UNKNOWNFIRST    = $00000004;
  CEDB_SORT_GENERICORDER    = $00000008; //Internally used for generic ordering.
  CEDB_SORT_IGNORENONSPACE  = $00000010;
  CEDB_SORT_IGNORESYMBOLS   = $00000020;
  CEDB_SORT_IGNOREKANATYPE  = $00000040;
  CEDB_SORT_IGNOREWIDTH     = $00000080;
  CEDB_SORT_STRINGSORT      = $00000100;
  CEDB_SORT_UNIQUE          = $00000200;
  CEDB_SORT_NONNULL         = $00000400;

// High nibble of flags reserved
type
  SORTORDERSPEC = record //SORTORDERSPEC: Specifies details about a sort order in a database.
  //Comment: Note that we only support simple sorts on a primary key.
  //         Records with the same key value will be sorted in arbitrary order.
    propid : CEPROPID; //PropID to be sorted on.
    dwFlags : DWORD;   //Any combination of the following
    //dwFlags:
    //CEDB_SORT_DESCENDING: Sort in descending order. Default is ascending.
    //CEDB_SORT_CASEINSENSITIVE: Only valid for strings.
    //CEDB_SORT_UNKNOWNFIRST: Puts records which do not contain this property
    //                        before all the other records.
    //                        Default is to put them last.
    //CEDB_SORT_IGNORENONSPACE: Only valid for strings.
    //                          This flag only has an effect for the locales in
    //                          which accented characters are sorted in a second
    //                          pass from main characters.
    //CEDB_SORT_IGNORESYMBOLS: Only valid for strings.
    //CEDB_SORT_IGNOREKANATYPE: Only valid for strings.
    //                          Do not differentiate between Hiragana and
    //                          Katakana characters.
    //CEDB_SORT_IGNOREWIDTH: Only valid for strings.
    //                       Do not differentiate between a single-byte
    //                       character and the same character as a
    //                       double-byte character.
    //CEDB_SORT_UNIQUE: Require the property to be unique across all records
    //                  in the database.
    //CEDB_SORT_NONNULL: Require the property to be present in all records.
  end;
  _SORTORDERSPEC = SORTORDERSPEC;
  PSORTORDERSPEC = ^SORTORDERSPEC;
  TSORTORDERSPEC = SORTORDERSPEC;

const
  CEDB_MAXSORTPROP = 3;
  SORTORDERSPECEX_VERSION = 1;

type
  //SORTORDERSPECEX: Specifies details about a sort order in a database.
  //comment: Supports a hierarchy of sorts.
  SORTORDERSPECEX = record
    wVersion  : WORD; //Version of this structure.
    wNumProps : WORD; //Number of properties in this sort order.
                      //Must not be more than CEDB_MAXSORTPROP.
    wKeyFlags : WORD; //Flags that correspond to the sort key.
                      //Any combination of the following:
                      // CEDB_SORT_UNIQUE: Require the key to be
                      //   unique across all records in the database.
    wReserved : WORD; //Padding for DWORD alignment
    rgPropID  : array[0..(CEDB_MAXSORTPROP)-1] of CEPROPID; //Array of PropIDs to be sorted
                                                            // on, in order of importance.
    rgdwFlags : array[0..(CEDB_MAXSORTPROP)-1] of DWORD;  //Flags that correspond to the sort PropIDs
                                                          //Any combination of the following:
                                                          //CEDB_SORT_DESCENDING: Sort in descending order. Default is ascending
                                                          //CEDB_SORT_CASEINSENSITIVE: Only valid for strings.
                                                          //CEDB_SORT_UNKNOWNFIRST: Puts records which do
                                                          // not contain this property before all the other records.
                                                          // Default is to put them last.
                                                          //CEDB_SORT_IGNORENONSPACE: Only valid for strings.
                                                          // This flag only has an effect for the locales in which
                                                          // accented characters are sorted in a second pass from
                                                          // main characters.
                                                          //CEDB_SORT_IGNORESYMBOLS: Only valid for strings.
                                                          //CEDB_SORT_IGNOREKANATYPE: Only valid for strings.
                                                          // Do not differentiate between Hiragana and Katakana characters.
                                                          //CEDB_SORT_IGNOREWIDTH: Only valid for strings.
                                                          // Do not differentiate between a single-byte character
                                                          // and the same character as a double-byte character.
                                                          //CEDB_SORT_NONNULL: Require the property to be
                                                          // present in all records.

  end;
  _SORTORDERSPECEX = SORTORDERSPECEX;
  PSORTORDERSPECEX = ^_SORTORDERSPECEX;
  TSORTORDERSPECEX = SORTORDERSPECEX;

const
  ERROR_DBPROP_NOT_FOUND = ERROR_ACCESS_DENIED;
  ERROR_REPEATED_KEY     = ERROR_ALREADY_EXISTS;
  CEDB_MAXDBASENAMELEN   = 32;
  CEDB_MAXSORTORDER      = 4;
  // Values for validity mask flags
  CEDB_VALIDNAME     = $0001;
  CEDB_VALIDTYPE     = $0002;
  CEDB_VALIDSORTSPEC = $0004;
  CEDB_VALIDMODTIME  = $0008;
  CEDB_VALIDDBFLAGS  = $0010;
  CEDB_VALIDCREATE   = ( (CEDB_VALIDNAME or CEDB_VALIDTYPE)
                        or CEDB_VALIDSORTSPEC )
                        or CEDB_VALIDDBFLAGS;
  // Values for dbflags
  CEDB_NOCOMPRESS = $00010000;
  CEDB_SYSTEMDB   = $00020000;

type
  // CEDBASEINFO: Contains information about a database object
  CEDBASEINFO = record
    dwFlags : DWORD; //Indicates which fields are valid. Possible values are:
    //CEDB_VALIDNAME: The name field is valid and should be used.
    //CEDB_VALIDTYPE: The type field is valid and should be used.
    //CEDB_VALIDSORTSPEC: The sortspecs are valid and should be used.
    szDbaseName : array[0..(CEDB_MAXDBASENAMELEN)-1] of WCHAR; //Name of Database. Max CEDB_MAXDBASENAMELEN characters.
    dwDbaseType : DWORD; //A type ID for this database.
    wNumRecords : WORD;  //Number of records in the database
    wNumSortOrder : WORD;  //Number of sort orders active in the database.
                           //Maximum is CEDB_MAXSORTORDER.
    dwSize : DWORD;  //Size in bytes that this database is using
    ftLastModified : FILETIME; //Last time this database was modified
    rgSortSpecs : array[0..(CEDB_MAXSORTORDER)-1] of SORTORDERSPEC; //Actual sort order descriptions.
                                                                    //Only first wNumSortOrder of this array are valid.
  end;
  _CEDBASEINFO = CEDBASEINFO;
  PCEDBASEINFO = ^CEDBASEINFO;
  TCEDBASEINFO = CEDBASEINFO;

const
  CEDBASEINFOEX_VERSION = 1;

type
  //CEDBASEINFOEX: Contains extended information about a database object.
  CEDBASEINFOEX = record
    wVersion : WORD; //Version of this structure.
    wNumSortOrder : WORD; //Number of sort orders active in the database
                          //Maximum is CEDB_MAXSORTORDER.
    dwFlags : DWORD; //Indicates which fields are valid. Possible values are:
                     // CEDB_VALIDNAME: The name field is valid and should be used.
                     // CEDB_VALIDTYPE: The type field is valid and should be used.
                     // CEDB_VALIDSORTSPEC: The sortspecs are valid and should be used.
    szDbaseName : array[0..(CEDB_MAXDBASENAMELEN)-1] of WCHAR;//Name of Database. Max CEDB_MAXDBASENAMELEN characters.
    dwDbaseType : DWORD;  //A type ID for this database
    dwNumRecords : DWORD; //Number of records in the database
    dwSize : DWORD; //Size in bytes that this database is using
    ftLastModified : FILETIME; //Last time this database was modified
    rgSortSpecs : array[0..(CEDB_MAXSORTORDER)-1] of SORTORDERSPECEX; //Actual sort order descriptions.
                                                                      //Only first wNumSortOrder of this array are valid.
  end;
  LPCEDBASEINFOEX = ^CEDBASEINFOEX;
  _CEDBASEINFOEX = CEDBASEINFOEX;
  TCEDBASEINFOEX = CEDBASEINFOEX;
  PCEDBASEINFOEX = ^CEDBASEINFOEX;

const
  BY_HANDLE_DB_INFORMATION_VERSION = 1;
type
  //BY_HANDLE_DB_INFORMATION: Contains extended information about an open database
  BY_HANDLE_DB_INFORMATION = record
    wVersion : WORD; //Version of this structure.
    wReserved : WORD; //Padding for DWORD alignment.
    guidVol : CEGUID; //GUID of parent volume.
    oidDbase : CEOID; //OID of database.
    infDatabase : CEDBASEINFOEX; //Extended database information.
  end;
  _BY_HANDLE_DB_INFORMATION  = BY_HANDLE_DB_INFORMATION;
  LPBY_HANDLE_DB_INFORMATION = ^BY_HANDLE_DB_INFORMATION;
  TBY_HANDLE_DB_INFORMATION  = BY_HANDLE_DB_INFORMATION;
  PBY_HANDLE_DB_INFORMATION  = ^BY_HANDLE_DB_INFORMATION;

  { flags for open database - use low word }
const
  CEDB_AUTOINCREMENT        = $00000001;
  CEDB_SEEK_CEOID           = $00000001;
  CEDB_SEEK_BEGINNING       = $00000002;
  CEDB_SEEK_END             = $00000004;
  CEDB_SEEK_CURRENT         = $00000008;
  CEDB_SEEK_VALUESMALLER    = $00000010;
  CEDB_SEEK_VALUEFIRSTEQUAL = $00000020;
  CEDB_SEEK_VALUEGREATER    = $00000040;
  CEDB_SEEK_VALUENEXTEQUAL  = $00000080;

type
  CEBLOB = record
    dwCount : DWORD;
    lpb : LPBYTE;
  end;
  _CEBLOB = CEBLOB;
  PCEBLOB = ^CEBLOB;
  TCEBLOB = CEBLOB;

const
  CEVT_I2          = 2;
  CEVT_UI2         = 18;
  CEVT_I4          = 3;
  CEVT_UI4         = 19;
  CEVT_FILETIME    = 64;
  CEVT_LPWSTR      = 31;
  CEVT_BLOB        = 65;
  CEVT_BOOL        = 11;
  CEVT_R8          = 5;


type
  //CEVALUNION: value types for a property.
  CEVALUNION = record
    case longint of //UNION
      0 : ( iVal      : smallint ); //CEVT_I2
      1 : ( uiVal     : USHORT   ); //CEVT_UI2
      2 : ( lVal      : longint  ); //CEVT_I4
      3 : ( ulVal     : ULONG    ); //CEVT_UI4
      4 : ( filetime  : TFILETIME ); //CEVT_FILETIME
      5 : ( lpwstr    : LPWSTR   ); //CEVT_LPWSTR - Ptr to null terminated string
      6 : ( blob      : CEBLOB   ); //CEVT_BLOB - DWORD count, and Ptr to bytes
      7 : ( boolVal   : BOOL     ); //CEVT_BOOL
      8 : ( dblVal    : double   ); //CEVT_R8
  end;
  _CEVALUNION = CEVALUNION;
  PCEVALUNION = ^CEVALUNION;
  TCEVALUNION = CEVALUNION;
  { @struct  }
  { Don't define flags in low byte or high nibble }

const
  CEDB_PROPNOTFOUND = $0100;
  CEDB_PROPDELETE = $0200;
type
  //CEPROPVAL: Contains a property value
  CEPROPVAL = record
    propid : CEPROPID; //PropID of the value.
    wLenData : WORD;   //Private field - can be garbage on entry
    wFlags : WORD;     //Special flags for this property. Possible flags
                       //CEDB_PROPNOTFOUND: Set by CeReadRecordProps field, if property not found.
                       //CEDB_PROPDELETE: If passed to CeWriteRecordProps field, it causes
                       //                 this property to be deleted.
    val : CEVALUNION;  //Actual value for simple types, ptr for strings/blobs
  end;
  _CEPROPVAL = CEPROPVAL;
  PCEPROPVAL = ^CEPROPVAL;
  TCEPROPVAL = CEPROPVAL;

const
  //Max record length defines
  CEDB_MAXDATABLOCKSIZE = 4092; //zero is a valid length so we cant have full 4196
  CEDB_MAXPROPDATASIZE  = ( CEDB_MAXDATABLOCKSIZE * 16 ) - 1;

  // max record size is bound only by the max logging space we want to consume
  // this is not explicitly checked for - if you read too much data and cause the log
  // page to overflow the call will fail.
  CEDB_MAXRECORDSIZE = 128*1024;

  // Max number of records allowed in a single database.
  CEDB_MAXNUMRECORDS = $FFFF;

  // flags for ReadRecord
  CEDB_ALLOWREALLOC = $00000001;

  procedure CREATE_SYSTEMGUID  (out pguid : CEGUID);
  procedure CREATE_INVALIDGUID (out pguid : CEGUID);

  function CHECK_SYSTEMGUID  (pguid : PCEGUID) : longint;
  function CHECK_INVALIDGUID (pguid : PCEGUID) : longint;

  // Obsolete (CEDB) function versions for backward compatibility
  function CeFindFirstDatabase(dwClassID:DWORD):HANDLE; external KernelDLL name 'CeFindFirstDatabase';
  function CeFindNextDatabase(hEnum:HANDLE):CEOID; external KernelDLL name 'CeFindNextDatabase';
  function CeCreateDatabase(lpszname:LPWSTR; dwClassID:DWORD; wNumSortOrder:WORD; var rgSortSpecs:SORTORDERSPEC):CEOID; external KernelDLL name 'CeCreateDatabase';
  function CeCreateDatabaseEx(pguid:PCEGUID; var pInfo:CEDBASEINFO):CEOID; external KernelDLL name 'CeCreateDatabaseEx';
  function CeSetDatabaseInfo(oidDbase:CEOID; var pNewInfo:CEDBASEINFO):Boolean; external KernelDLL name 'CeSetDatabaseInfo';
  function CeSetDatabaseInfoEx(pguid:PCEGUID; oidDbase:CEOID; var pNewInfo:CEDBASEINFO):Boolean; external KernelDLL name 'CeSetDatabaseInfoEx';
  function CeOpenDatabase(poid:PCEOID; lpszName:LPWSTR; propid:CEPROPID; dwFlags:DWORD; hwndNotify:HWND):HANDLE; external KernelDLL name 'CeOpenDatabase';
  function CeOpenDatabaseEx(pguid:PCEGUID; poid:PCEOID; lpszName:LPWSTR; propid:CEPROPID; dwFlags:DWORD;
                            var pReq:CENOTIFYREQUEST):HANDLE; external KernelDLL name 'CeOpenDatabaseEx';
  function CeDeleteDatabase(oid:CEOID):BOOL; external KernelDLL name 'CeDeleteDatabase';
  function CeReadRecordProps(hDbase:HANDLE; dwFlags:DWORD; lpcPropID:LPWORD; var rgPropID:CEPROPID; var lplpBuffer:LPBYTE;
                             lpcbBuffer:LPDWORD):CEOID; external KernelDLL name 'CeReadRecordProps';
  function CeSeekDatabase(hDatabase:HANDLE; dwSeekType:DWORD; dwValue:DWORD; lpdwIndex:LPDWORD):CEOID; external KernelDLL name 'CeSeekDatabase';
  function CeGetDBInformationByHandle(hDbase:HANDLE; lpDBInfo:LPBY_HANDLE_DB_INFORMATION):Boolean; external KernelDLL name 'CeGetDBInformationByHandle';
  function CeFindFirstDatabaseEx(pguid:PCEGUID; dwClassID:DWORD):HANDLE; external KernelDLL name 'CeFindFirstDatabaseEx';
  function CeFindNextDatabaseEx(hEnum:HANDLE; pguid:PCEGUID):CEOID; external KernelDLL name 'CeFindNextDatabaseEx';
  function CeCreateDatabaseEx2(pguid:PCEGUID; pInfo:PCEDBASEINFOEX):CEOID; external KernelDLL name 'CeCreateDatabaseEx2';
  function CeSetDatabaseInfoEx2(pguid:PCEGUID; oidDbase:CEOID; var pNewInfo:CEDBASEINFOEX):Boolean; external KernelDLL name 'CeSetDatabaseInfoEx2';
  function CeOpenDatabaseEx2(pguid:PCEGUID; poid:PCEOID; lpszName:LPWSTR; var pSort:SORTORDERSPECEX; dwFlags:DWORD;
                             var pReq:CENOTIFYREQUEST):HANDLE; external KernelDLL name 'CeOpenDatabaseEx2';
  function CeDeleteDatabaseEx(pguid:PCEGUID; oid:CEOID):Boolean; external KernelDLL name 'CeDeleteDatabaseEx';
  function CeSeekDatabaseEx(hDatabase:HANDLE; dwSeekType:DWORD; dwValue:DWORD; wNumVals:WORD; lpdwIndex:LPDWORD):CEOID; external KernelDLL name 'CeSeekDatabaseEx';
  function CeDeleteRecord(hDatabase:HANDLE; oidRecord:CEOID):Boolean; external KernelDLL name 'CeDeleteRecord';
  function CeReadRecordPropsEx(hDbase:HANDLE; dwFlags:DWORD; lpcPropID:LPWORD; rgPropID: PCEPROPID; lplpBuffer:LPBYTE;
                               lpcbBuffer:LPDWORD; hHeap:HANDLE):CEOID; external KernelDLL name 'CeReadRecordPropsEx';
  function CeWriteRecordProps(hDbase:HANDLE; oidRecord:CEOID; cPropID:WORD; var rgPropVal:CEPROPVAL):CEOID; external KernelDLL name 'CeWriteRecordProps';
  function CeMountDBVol(pguid:PCEGUID; lpszVol:LPWSTR; dwFlags:DWORD):Boolean; external KernelDLL name 'CeMountDBVol';
  function CeUnmountDBVol(pguid:PCEGUID):Boolean; external KernelDLL name 'CeUnmountDBVol';
  function CeFlushDBVol(pguid:PCEGUID):Boolean; external KernelDLL name 'CeFlushDBVol';
  function CeEnumDBVolumes(pguid:PCEGUID; lpBuf:LPWSTR; dwSize:DWORD):Boolean; external KernelDLL name 'CeEnumDBVolumes';
  function CeFreeNotification(pRequest:PCENOTIFYREQUEST; pNotify:PCENOTIFICATION):Boolean; external KernelDLL name 'CeFreeNotification';

const
  OBJTYPE_INVALID   = 0;
  OBJTYPE_FILE      = 1;
  OBJTYPE_DIRECTORY = 2;
  OBJTYPE_DATABASE  = 3;
  OBJTYPE_RECORD    = 4;

type
  CEOIDINFO = record
    wObjType : WORD; //Type of object
                     //  OBJTYPE_INVALID   : There was no valid object with this CEOID
                     //  OBJTYPE_FILE      : The object is a file
                     //  OBJTYPE_DIRECTORY : The object is a directory
                     //  OBJTYPE_DATABASE  : The object is a database
                     //  OBJTYPE_RECORD    : The object is a record inside a database
    wPad     : WORD; // dword alignment
    case longint of  //This is a union
      0 : ( infFile      : CEFILEINFO   ); //Valid for file objects
      1 : ( infDirectory : CEDIRINFO;   ); //Valid for directory objects
      //IF FILESYS_FSDBASE
      2 : ( infDatabase  : CEDBASEINFO; ); //Valid for database objects
      3 : ( infRecord    : CERECORDINFO;); //Valid for record objects
  end;
  _CEOIDINFO = CEOIDINFO;
  PCEOIDINFO = ^CEOIDINFO;
  TCEOIDINFO = CEOIDINFO;

  const CEOIDINFOEX_VERSION =  1;

  type  CEOIDINFOEX = record
    wVersion : Word; //Version of this structure.
                     //  OBJTYPE_INVALID   : There was no valid object with this CEOID
                     //  OBJTYPE_FILE      : The object is a file
                     //  OBJTYPE_DIRECTORY : The object is a directory
                     //  OBJTYPE_DATABASE  : The object is a database
                     //  OBJTYPE_RECORD    : The object is a record inside a database
    wObjType : Word; //Type of object
    case longint of
      0 : ( infFile : CEFILEINFO );      //Valid for file objects
      1 : ( infDirectory : CEDIRINFO; ); //Valid for directory objects
      //IF FILESYS_FSDBASE
      2 : ( infDatabase : CEDBASEINFOEX; ); //Valid for database objects
      3 : ( infRecord : CERECORDINFO; );  //Valid for record objects
  end;
  _CEOIDINFOEX = CEOIDINFOEX;
  PCEOIDINFOEX = ^CEOIDINFOEX;
  TCEOIDINFOEX = CEOIDINFOEX;

// Functions
function CeOidGetInfoEx2(pguid:PCEGUID; oid:CEOID; oidInfo:PCEOIDINFOEX):Boolean; external KernelDLL name 'CeOidGetInfoEx2';
function CeOidGetInfoEx(pguid:PCEGUID; oid:CEOID; var oidInfo:CEOIDINFO):Boolean; external KernelDLL name 'CeOidGetInfoEx';
function CeOidGetInfo(oid:CEOID; var oidInfo:CEOIDINFO):Boolean; external KernelDLL name 'CeOidGetInfo';
{$ifdef EDB}
  {$I windbase_edb.inc}
{$endif} //EDB

//{$ifdef WINCEOEM}
//{$include <pwindbas.h>   // internal defines }
//{$ifdef WINCEMACRO}
//{$include <mwindbas.h>}
//{$endif}
//{$endif}
//    { @CESYSGEN ENDIF }
//{$endif}

implementation

function TypeFromPropID(propid : longint) : WORD;
begin
  TypeFromPropID := LOWORD(propid);
end;

procedure CREATE_SYSTEMGUID(out pguid : CEGUID);
begin
  fillchar(pguid,0,sizeof(CEGUID));
end;

procedure CREATE_INVALIDGUID(out pguid : CEGUID);
begin
  fillchar(pguid,sizeof(CEGUID),-(1));
end;

function CHECK_SYSTEMGUID(pguid : PCEGUID) : longint;
begin
  CHECK_SYSTEMGUID:= not( (((pguid^.Data1) or (pguid^.Data2)) or (pguid^.Data3)) or (pguid^.Data4) );
end;

function CHECK_INVALIDGUID(pguid : PCEGUID) : longint;
begin
  CHECK_INVALIDGUID:= not( not ((((pguid^.Data1) and (pguid^.Data2)) and (pguid^.Data3)) and (pguid^.Data4) ) );
end;

end.
