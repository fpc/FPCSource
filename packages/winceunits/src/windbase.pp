{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    conversion of windbase.h by Carolos Foscolos
 **********************************************************************}

{$mode objfpc}
unit windbase;
interface
 uses Windows;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{ convention is cdecl for WinCE API}
{$calling cdecl}

  {*


   Module: windbase.h

   Purpose: Master include file for WINCE Database APIs

  * }
  { @doc OBJSTORE }
  {
  @topic Windows CE Object Store |
      The Windows CE object store has 3 parts - a registry API, a file system API and a database API.
      <nl>The standard Win32 API's supported by the registry are:
      <nl>RegCloseKey
      <nl>RegCreateKeyEx
      <nl>RegDeleteKey
      <nl>RegDeleteValue
      <nl>RegEnumValue
      <nl>RegEnumKeyEx
      <nl>RegOpenKeyEx
      <nl>RegQueryInfoKey
      <nl>RegQueryValueEx
      <nl>RegSetValueEx

      The standard Win32 API's supported by the filesytem are:
      <nl>CreateDirectory
      <nl>RemoveDirectory
      <nl>MoveFile
      <nl>CopyFile
      <nl>DeleteFile
      <nl>GetFileAttributes
      <nl>FindFirstFile
      <nl>CreateFileW
      <nl>ReadFile
      <nl>WriteFile
      <nl>GetFileSize
      <nl>SetFilePointer
      <nl>GetFileInformationByHandle
      <nl>FlushFileBuffers
      <nl>GetFileTime
      <nl>SetFileTime
      <nl>SetEndOfFile
      <nl>FindClose
      <nl>FindNextFile

      In addition, the following additional filesystem call is available:
      <nl>CreateContainer

      The various functions and data structures are described in this
      document.
   }
{$ifndef __WINDBASE__}
{$define __WINDBASE__}
  { @CESYSGEN IF CE_MODULES_FILESYS }
  {
  @type CEOID | Unique identifier for all WINCE objects
  @comm Every WINCE object can be efficiently referred to by its OID. OID's are unique
        in the system and are not reused
   }

  type
     CEOID = DWORD;
     PCEOID = CEOID;

     CEGUID = record
          Data1 : DWORD;
          Data2 : DWORD;
          Data3 : DWORD;
          Data4 : DWORD;
       end;
     _CEGUID = CEGUID;
     PCEGUID = ^CEGUID;
     TCEGUID = CEGUID;
{$ifndef WM_DBNOTIFICATION}

  const
     WM_DBNOTIFICATION = $03FD;
{$else}
(* error
ERRFALSE(WM_DBNOTIFICATION == 0x03FD);
 in declarator_list *)
{$endif}

  const
     CEDB_EXNOTIFICATION = $00000001;
  { must be set to the structure size }
  { window handle for notifications to be posted }
  { heap from which to allocate EX-NOTIFICATIONS }

  type

     CENOTIFYREQUEST = record
          dwSize : DWORD;
          hwnd : HWND;
          dwFlags : DWORD;
          hHeap : HANDLE;
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
  { @CESYSGEN IF FILESYS_FSMAIN }
  { @struct CEFILEINFO | Contains information about a file object     }
  {@field File attributes }
  {@field CEOID of parent directory }
  {@field Full path name of the file }
  {@field Time stamp of last change }
  {@field Length of file }

     CEFILEINFO = record
          dwAttributes : DWORD;
          oidParent : CEOID;
          szFileName : array[0..(MAX_PATH)-1] of WCHAR;
          ftLastChanged : FILETIME;
          dwLength : DWORD;
       end;
     _CEFILEINFO = CEFILEINFO;
     PCEFILEINFO = ^CEFILEINFO;
     TCEFILEINFO = CEFILEINFO;
  {@struct CEDIRINFO | Contains information about a directory object     }
  {@field Directory attributes }
  {@field CEOID of parent directory }
  {@field Full path name of the directory }

     CEDIRINFO = record
          dwAttributes : DWORD;
          oidParent : CEOID;
          szDirName : array[0..(MAX_PATH)-1] of WCHAR;
       end;
     _CEDIRINFO = CEDIRINFO;
     PCEDIRINFO = ^CEDIRINFO;
     TCEDIRINFO = CEDIRINFO;
  {
  @msg DB_CEOID_CREATED | Msg sent on creation of new oid
  @comm WParam == CEOID modified
        LParam == CEOID's parent CEOID
  @xref <f CeRegisterReplNotification>
   }

  const
     DB_CEOID_CREATED = WM_USER+$1;
  {
  @msg DB_CEOID_DATABASE_DELETED | Msg sent on deletion of database
  @comm WParam == CEOID modified
        LParam == CEOID's parent CEOID
  @xref <f CeRegisterReplNotification>
   }
     DB_CEOID_DATABASE_DELETED = WM_USER+$2;
  {
  @msg DB_CEOID_RECORD_DELETED | Msg sent on deletion of record
  @comm WParam == CEOID modified
        LParam == CEOID's parent CEOID
  @xref <f CeRegisterReplNotification>
   }
     DB_CEOID_RECORD_DELETED = WM_USER+$3;
  {
  @msg DB_CEOID_FILE_DELETED | Msg sent on deletion of file
  @comm WParam == CEOID modified
        LParam == CEOID's parent CEOID
  @xref <f CeRegisterReplNotification>
   }
     DB_CEOID_FILE_DELETED = WM_USER+$4;
  {
  @msg DB_CEOID_DIRECTORY_DELETED | Msg sent on deletion of directory
  @comm WParam == CEOID modified
        LParam == CEOID's parent CEOID
  @xref <f CeRegisterReplNotification>
   }
     DB_CEOID_DIRECTORY_DELETED = WM_USER+$5;
  {
  @msg DB_CEOID_CHANGED | Msg sent on item modification
  @comm WParam == CEOID modified
        LParam == CEOID's parent CEOID
  @xref <f CeRegisterReplNotification>
   }
     DB_CEOID_CHANGED = WM_USER+$6;
  { flags for CeGetReplChangeMask }
     REPL_CHANGE_WILLCLEAR = $00000001;

  type

     STORE_INFORMATION = record
          dwStoreSize : DWORD;
          dwFreeSize : DWORD;
       end;
     LPSTORE_INFORMATION = ^STORE_INFORMATION;
     PSTORE_INFORMATION = ^STORE_INFORMATION;
     TSTORE_INFORMATION = STORE_INFORMATION;

  function GetStoreInformation(lpsi:LPSTORE_INFORMATION):BOOL;cdecl;external KernelDLL name 'GetStoreInformation';

  { @CESYSGEN ENDIF }
  { @CESYSGEN IF FILESYS_FSDBASE }
  {
  @type CEPROPID | PropID's for WINCE properties
  @comm PropID's on the WINCE match PropID's used by Mapi1. The top 2 bytes are an ID
       and the low 2 bytes are the type. For a list of supported types look at the tags
       supported in <t CEVALUNION>. We reserve one bit (0x4000) in the type as the
       flag <b CEPROPVAL_NULL> as a special flag. It denotes that a property was not
       found in a Read call, or that the property should be deleted in a write call.
   }

  type

     CEPROPID = DWORD;
     PCEPROPID = CEPROPID;
     TCEPROPID = CEPROPID;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }
  function TypeFromPropID(propid : longint) : longint;

  {@struct CERECORDINFO | Contains information about a record object     }
  {@field CEOID of parent database }

  type

     CERECORDINFO = record
          oidParent : CEOID;
       end;
     _CERECORDINFO = CERECORDINFO;
     PCERECORDINFO = ^CERECORDINFO;
     TCERECORDINFO = CERECORDINFO;

  const
     CEDB_SORT_DESCENDING = $00000001;
     CEDB_SORT_CASEINSENSITIVE = $00000002;
     CEDB_SORT_UNKNOWNFIRST = $00000004;
     CEDB_SORT_GENERICORDER = $00000008;     { internally used for generic ordering }
     CEDB_SORT_IGNORENONSPACE = $00000010;
     CEDB_SORT_IGNORESYMBOLS = $00000020;
     CEDB_SORT_IGNOREKANATYPE = $00000040;
     CEDB_SORT_IGNOREWIDTH = $00000080;
     CEDB_SORT_STRINGSORT = $00000100;
     CEDB_SORT_UNIQUE = $00000200;
     CEDB_SORT_NONNULL = $00000400;
  { High nibble of flags reserved }
  {@struct SORTORDERSPEC | Specifies details about a sort order in a database }
  {@comm Note that we only support simple sorts on a primary key. Records with the same key value }
  {      will be sorted in arbitrary order. }
  {@field PropID to be sorted on. }
  {@field Any combination of the following }
  {@flag CEDB_SORT_DESCENDING | Sort in descending order. Default is ascending. }
  {@flag CEDB_SORT_CASEINSENSITIVE | Only valid for strings. }
  {@flag CEDB_SORT_UNKNOWNFIRST | Puts records which do  }
  { not contain this property before all the other records. }
  { Default is to put them last. }
  {@flag CEDB_SORT_IGNORENONSPACE | Only valid for strings. }
  { This flag only has an effect for the locales in which  }
  { accented characters are sorted in a second pass from }
  { main characters. }
  {@flag CEDB_SORT_IGNORESYMBOLS | Only valid for strings. }
  {@flag CEDB_SORT_IGNOREKANATYPE | Only valid for strings. }
  { Do not differentiate between Hiragana and Katakana characters. }
  {@flag CEDB_SORT_IGNOREWIDTH | Only valid for strings. }
  { Do not differentiate between a single-byte character  }
  { and the same character as a double-byte character. }
  {@flag CEDB_SORT_UNIQUE | Require the property to be }
  { unique across all records in the database. }
  {@flag CEDB_SORT_NONNULL | Require the property to be }
  { present in all records. }

  type

     SORTORDERSPEC = record
          propid : CEPROPID;
          dwFlags : DWORD;
       end;
     _SORTORDERSPEC = SORTORDERSPEC;
     PSORTORDERSPEC = ^SORTORDERSPEC;
     TSORTORDERSPEC = SORTORDERSPEC;

  const
     CEDB_MAXSORTPROP = 3;
     SORTORDERSPECEX_VERSION = 1;
  {@struct SORTORDERSPECEX | Specifies details about a sort order in a database }
  {@comm Supports a hierarchy of sorts. }
  {@field Version of this structure. }
  {@field Number of properties in this sort order. }
  { Must not be more than CEDB_MAXSORTPROP. }
  {@field Flags that correspond to the sort key. }
  { Any combination of the following: }
  {@flag CEDB_SORT_UNIQUE | Require the key to be }
  { unique across all records in the database. }
  {Padding for DWORD alignment }
  {@field Array of PropIDs to be sorted }
  { on, in order of importance. }
  {@field Flags that correspond to the sort PropIDs }
  { Any combination of the following: }
  {@flag CEDB_SORT_DESCENDING | Sort in descending order. Default is ascending }
  {@flag CEDB_SORT_CASEINSENSITIVE | Only valid for strings. }
  {@flag CEDB_SORT_UNKNOWNFIRST | Puts records which do  }
  { not contain this property before all the other records. }
  { Default is to put them last. }
  {@flag CEDB_SORT_IGNORENONSPACE | Only valid for strings. }
  { This flag only has an effect for the locales in which  }
  { accented characters are sorted in a second pass from }
  { main characters. }
  {@flag CEDB_SORT_IGNORESYMBOLS | Only valid for strings. }
  {@flag CEDB_SORT_IGNOREKANATYPE | Only valid for strings. }
  { Do not differentiate between Hiragana and Katakana characters. }
  {@flag CEDB_SORT_IGNOREWIDTH | Only valid for strings. }
  { Do not differentiate between a single-byte character  }
  { and the same character as a double-byte character. }
  {@flag CEDB_SORT_NONNULL | Require the property to be }
  { present in all records. }

  type

     SORTORDERSPECEX = record
          wVersion : WORD;
          wNumProps : WORD;
          wKeyFlags : WORD;
          wReserved : WORD;
          rgPropID : array[0..(CEDB_MAXSORTPROP)-1] of CEPROPID;
          rgdwFlags : array[0..(CEDB_MAXSORTPROP)-1] of DWORD;
       end;
     _SORTORDERSPECEX = SORTORDERSPECEX;
     PSORTORDERSPECEX = ^_SORTORDERSPECEX;
     TSORTORDERSPECEX = SORTORDERSPECEX;
  { NOTENOTE someday this should become a separate CE-only error code }

  const
     ERROR_DBPROP_NOT_FOUND = ERROR_ACCESS_DENIED;
     ERROR_REPEATED_KEY = ERROR_ALREADY_EXISTS;
     CEDB_MAXDBASENAMELEN = 32;
     CEDB_MAXSORTORDER = 4;
  { values for validity mask flags }
     CEDB_VALIDNAME = $0001;
     CEDB_VALIDTYPE = $0002;
     CEDB_VALIDSORTSPEC = $0004;
     CEDB_VALIDMODTIME = $0008;
     CEDB_VALIDDBFLAGS = $0010;
     CEDB_VALIDCREATE = ((CEDB_VALIDNAME or CEDB_VALIDTYPE) or CEDB_VALIDSORTSPEC) or CEDB_VALIDDBFLAGS;
  { values for dbflags }
     CEDB_NOCOMPRESS = $00010000;
     CEDB_SYSTEMDB = $00020000;
  { @struct CEDBASEINFO | Contains information about a database object     }
  {@field Indicates which fields are valid. Possible values are: }
  {  @flag CEDB_VALIDNAME | The name field is valid and should be used }
  {  @flag CEDB_VALIDTYPE | The type field is valid and should be used }
  {  @flag CEDB_VALIDSORTSPEC | The sortspecs are valid and should be used }
  {@field Name of Database. Max CEDB_MAXDBASENAMELEN characters. }
  {@field A type ID for this database }
  {@field Number of records in the database }
  {@field Number of sort orders active in the database }
  { Maximum is CEDB_MAXSORTORDER. }
  {@field Size in bytes that this database is using }
  {@field Last time this database was modified }
  {@field Actual sort order descriptions.  }
  { Only first wNumSortOrder of this array are valid. }

  type

     CEDBASEINFO = record
          dwFlags : DWORD;
          szDbaseName : array[0..(CEDB_MAXDBASENAMELEN)-1] of WCHAR;
          dwDbaseType : DWORD;
          wNumRecords : WORD;
          wNumSortOrder : WORD;
          dwSize : DWORD;
          ftLastModified : FILETIME;
          rgSortSpecs : array[0..(CEDB_MAXSORTORDER)-1] of SORTORDERSPEC;
       end;
     _CEDBASEINFO = CEDBASEINFO;
     PCEDBASEINFO = ^CEDBASEINFO;
     TCEDBASEINFO = CEDBASEINFO;

  const
     CEDBASEINFOEX_VERSION = 1;
  { @struct CEDBASEINFOEX | Contains extended information about a database object     }
  {@field Version of this structure }
  {@field Number of sort orders active in the database }
  { Maximum is CEDB_MAXSORTORDER. }
  {@field Indicates which fields are valid. Possible values are: }
  {  @flag CEDB_VALIDNAME | The name field is valid and should be used }
  {  @flag CEDB_VALIDTYPE | The type field is valid and should be used }
  {  @flag CEDB_VALIDSORTSPEC | The sortspecs are valid and should be used }
  {@field Name of Database. Max CEDB_MAXDBASENAMELEN characters. }
  {@field A type ID for this database }
  {@field Number of records in the database }
  {@field Size in bytes that this database is using }
  {@field Last time this database was modified }
  {@field Actual sort order descriptions.  }
  { Only first wNumSortOrder of this array are valid. }

  type
     CEDBASEINFOEX = record
          wVersion : WORD;
          wNumSortOrder : WORD;
          dwFlags : DWORD;
          szDbaseName : array[0..(CEDB_MAXDBASENAMELEN)-1] of WCHAR;
          dwDbaseType : DWORD;
          dwNumRecords : DWORD;
          dwSize : DWORD;
          ftLastModified : FILETIME;
          rgSortSpecs : array[0..(CEDB_MAXSORTORDER)-1] of SORTORDERSPECEX;
       end;
     LPCEDBASEINFOEX = ^CEDBASEINFOEX;
     _CEDBASEINFOEX = CEDBASEINFOEX;
     TCEDBASEINFOEX = CEDBASEINFOEX;
     PCEDBASEINFOEX = ^CEDBASEINFOEX;

  const
     BY_HANDLE_DB_INFORMATION_VERSION = 1;
  { @struct BY_HANDLE_DB_INFORMATION | Contains extended information about an open database }
  {@field Version of this structure }
  {Padding for DWORD alignment }
  {@field GUID of parent volume }
  {@field OID of database }
  {@field Extended database information }

  type

     BY_HANDLE_DB_INFORMATION = record
          wVersion : WORD;
          wReserved : WORD;
          guidVol : CEGUID;
          oidDbase : CEOID;
          infDatabase : CEDBASEINFOEX;
       end;
     _BY_HANDLE_DB_INFORMATION = BY_HANDLE_DB_INFORMATION;
     LPBY_HANDLE_DB_INFORMATION = ^BY_HANDLE_DB_INFORMATION;
     TBY_HANDLE_DB_INFORMATION = BY_HANDLE_DB_INFORMATION;
     PBY_HANDLE_DB_INFORMATION = ^BY_HANDLE_DB_INFORMATION;
  { flags for open database - use low word }

  const
     CEDB_AUTOINCREMENT = $00000001;
     CEDB_SEEK_CEOID = $00000001;
     CEDB_SEEK_BEGINNING = $00000002;
     CEDB_SEEK_END = $00000004;
     CEDB_SEEK_CURRENT = $00000008;
     CEDB_SEEK_VALUESMALLER = $00000010;
     CEDB_SEEK_VALUEFIRSTEQUAL = $00000020;
     CEDB_SEEK_VALUEGREATER = $00000040;
     CEDB_SEEK_VALUENEXTEQUAL = $00000080;

  type

     CEBLOB = record
          dwCount : DWORD;
          lpb : LPBYTE;
       end;
     _CEBLOB = CEBLOB;
     PCEBLOB = ^CEBLOB;
     TCEBLOB = CEBLOB;

  const
     CEVT_I2 = 2;
     CEVT_UI2 = 18;
     CEVT_I4 = 3;
     CEVT_UI4 = 19;
     CEVT_FILETIME = 64;
     CEVT_LPWSTR = 31;
     CEVT_BLOB = 65;
     CEVT_BOOL = 11;
     CEVT_R8 = 5;
  { @union CEVALUNION | value types for a property }
  {@field CEVT_I2 }
  {@field CEVT_UI2 }
  {@field CEVT_I4 }
  {@field CEVT_UI4 }
  {@field CEVT_FILETIME  }
  {@field CEVT_LPWSTR - Ptr to null terminated string }
  {@field CEVT_BLOB - DWORD count, and Ptr to bytes }
  {@field CEVT_BOOL }
  {@field CEVT_R8 }

  type

     CEVALUNION = record
         case longint of
            0 : ( iVal : smallint );
            1 : ( uiVal : USHORT );
            2 : ( lVal : longint );
            3 : ( ulVal : ULONG );
            4 : ( filetime : FILETIME );
            5 : ( lpwstr : LPWSTR );
            6 : ( blob : CEBLOB );
            7 : ( boolVal : BOOL );
            8 : ( dblVal : double );
         end;
     _CEVALUNION = CEVALUNION;
     PCEVALUNION = ^CEVALUNION;
     TCEVALUNION = CEVALUNION;
  { @struct CEPROPVAL | Contains a property value }
  { Don't define flags in low byte or high nibble }

  const
     CEDB_PROPNOTFOUND = $0100;
     CEDB_PROPDELETE = $0200;
  {@field PropID of the value. }
  {@field Private field - can be garbage on entry }
  {@field Special flags for this property. Possible flags }
  {@flag CEDB_PROPNOTFOUND | Set by <f CeReadRecordProps> if property not found }
  {@flag CEDB_PROPDELETE | If passed to <f CeWriteRecordProps> it causes  }
  { this property to be deleted }
  {@field Actual value for simple types, ptr for strings/blobs                         }

  type

     CEPROPVAL = record
          propid : CEPROPID;
          wLenData : WORD;
          wFlags : WORD;
          val : CEVALUNION;
       end;
     _CEPROPVAL = CEPROPVAL;
     PCEPROPVAL = ^CEPROPVAL;
     TCEPROPVAL = CEPROPVAL;
  { Max record length defines }
  { zero is a valid length so we cant have full 4196 }

  const
     CEDB_MAXDATABLOCKSIZE = 4092;
     CEDB_MAXPROPDATASIZE  = ((CEDB_MAXDATABLOCKSIZE*16)-1);
(* error
#define CEDB_MAXPROPDATASIZE  ((CEDB_MAXDATABLOCKSIZE*16)-1)
in define line 405 *)
    { max record size is bound only by the max logging space we want to consume }
    { this is not explicitly checked for - if you read too much data and cause the log }
    { page to overflow the call will fail. }
       CEDB_MAXRECORDSIZE = 128*1024;
    { Max number of records allowed in a single database. }
       CEDB_MAXNUMRECORDS = $FFFF;
    { flags for ReadRecord }
       CEDB_ALLOWREALLOC = $00000001;
    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    //function CREATE_SYSTEMGUID(pguid : longint) : longint;
    procedure CREATE_SYSTEMGUID(out pguid : CEGUID);

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    //function CREATE_INVALIDGUID(pguid : longint) : longint;
    procedure CREATE_INVALIDGUID(out pguid : CEGUID);


    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    //function CHECK_SYSTEMGUID(pguid : longint) : longint;
    function CHECK_SYSTEMGUID(pguid : PCEGUID) : longint;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    //function CHECK_INVALIDGUID(pguid : longint) : longint;
    function CHECK_INVALIDGUID(pguid : PCEGUID) : longint;

    { Obsolete versions for backward compatibility }
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

    function CeReadRecordPropsEx(hDbase:HANDLE; dwFlags:DWORD; lpcPropID:LPWORD; var rgPropID:CEPROPID; var lplpBuffer:LPBYTE;
               lpcbBuffer:LPDWORD; hHeap:HANDLE):CEOID; external KernelDLL name 'CeReadRecordPropsEx';

    function CeWriteRecordProps(hDbase:HANDLE; oidRecord:CEOID; cPropID:WORD; var rgPropVal:CEPROPVAL):CEOID; external KernelDLL name 'CeWriteRecordProps';

    function CeMountDBVol(pguid:PCEGUID; lpszVol:LPWSTR; dwFlags:DWORD):Boolean; external KernelDLL name 'CeMountDBVol';

    function CeUnmountDBVol(pguid:PCEGUID):Boolean; external KernelDLL name 'CeUnmountDBVol';

    function CeFlushDBVol(pguid:PCEGUID):Boolean; external KernelDLL name 'CeFlushDBVol';

    function CeEnumDBVolumes(pguid:PCEGUID; lpBuf:LPWSTR; dwSize:DWORD):Boolean; external KernelDLL name 'CeEnumDBVolumes';

    function CeFreeNotification(pRequest:PCENOTIFYREQUEST; pNotify:PCENOTIFICATION):Boolean; external KernelDLL name 'CeFreeNotification';

    { @CESYSGEN ENDIF }
    { @CESYSGEN IF FILESYS_FSMAIN }
    {
    @struct CEOIDINFO | Contains information about a WINCE object
    @field WORD | wObjType | Type of object
       @flag   OBJTYPE_INVALID   | There was no valid object with this CEOID
       @flag   OBJTYPE_FILE      | The object is a file
       @flag   OBJTYPE_DIRECTORY | The object is a directory
       @flag   OBJTYPE_DATABASE  | The object is a database
       @flag   OBJTYPE_RECORD    | The object is a record inside a database
    @field <lt>SeeBelow<gt> | <lt>CEOIDINFOUNIONref<gt> | Note: The remaining members form a union
    @field CEFILEINFO   | infFile      | Valid for file objects
    @field CEDIRINFO    | infDirectory | Valid for directory objects
    @field CEDBASEINFO  | infDatabase  | Valid for database objects
    @field CERECORDINFO | infRecord    | Valid for record objects
    @xref   <t CEFILEINFO>  <t CEDIRINFO> <t CEDBASEINFO>  <t CERECORDINFO>
     }

    const
       OBJTYPE_INVALID = 0;
       OBJTYPE_FILE = 1;
       OBJTYPE_DIRECTORY = 2;
       OBJTYPE_DATABASE = 3;
       OBJTYPE_RECORD = 4;
    {Type of object }
    {        OBJTYPE_INVALID   | There was no valid object with this CEOID }
    {        OBJTYPE_FILE      | The object is a file }
    {        OBJTYPE_DIRECTORY | The object is a directory }
    {        OBJTYPE_DATABASE  | The object is a database }
    {        OBJTYPE_RECORD    | The object is a record inside a database }
    { dword alignment             }
    {This is a union  }
    {Valid for file objects }
    {Valid for directory objects }
    { @CESYSGEN IF FILESYS_FSDBASE }
    {Valid for database objects }
    {Valid for record objects }
    { @CESYSGEN ENDIF }

type CEOIDINFO = record
  wObjType : WORD; //Type of object
            //        OBJTYPE_INVALID   | There was no valid object with this CEOID
            //        OBJTYPE_FILE      | The object is a file
            //        OBJTYPE_DIRECTORY | The object is a directory
            //        OBJTYPE_DATABASE  | The object is a database
            //        OBJTYPE_RECORD    | The object is a record inside a database
  wPad : WORD;        // dword alignment
  case longint of
    0 : ( infFile : CEFILEINFO ); //Valid for file objects
    1 : ( infDirectory : CEDIRINFO; ); //Valid for directory objects
    2 : ( infDatabase : CEDBASEINFO; ); //Valid for database objects
    3 : ( infRecord : CERECORDINFO; );  //Valid for record objects
  end;
  _CEOIDINFO = CEOIDINFO;
  PCEOIDINFO = ^CEOIDINFO;
  TCEOIDINFO = CEOIDINFO;

  const CEOIDINFOEX_VERSION =  1;

  type  CEOIDINFOEX = record
    wVersion : Word;  //@field Version of this structure
                //@flag OBJTYPE_INVALID   | There was no valid object with this CEOID
                //@flag OBJTYPE_FILE      | The object is a file
                //@flag OBJTYPE_DIRECTORY | The object is a directory
                //@flag OBJTYPE_DATABASE  | The object is a database
                //@flag OBJTYPE_RECORD    | The object is a record inside a database
    wObjType : Word;  //@field Type of object
    u : record  //This is a union
      case longint of
        0 : ( infFile : CEFILEINFO ); //Valid for file objects
        1 : ( infDirectory : CEDIRINFO; ); //Valid for directory objects
        // @CESYSGEN IF FILESYS_FSDBASE
        2 : ( infDatabase : CEDBASEINFO; ); //Valid for database objects
        3 : ( infRecord : CERECORDINFO; );  //Valid for record objects
      end;
  end;
  _CEOIDINFOEX = CEOIDINFOEX;
  PCEOIDINFOEX = ^CEOIDINFOEX;
  TCEOIDINFOEX = CEOIDINFOEX;

(* error
    };
 in member_list *)
(* error
#define CEOIDINFOEX_VERSION 1
    {
    @struct CEOIDINFOEX | Contains extended information about a WINCE object
    @field WORD | wObjType | Type of object
       @flag   OBJTYPE_INVALID   | There was no valid object with this CEOID
       @flag   OBJTYPE_FILE      | The object is a file
       @flag   OBJTYPE_DIRECTORY | The object is a directory
       @flag   OBJTYPE_DATABASE  | The object is a database
       @flag   OBJTYPE_RECORD    | The object is a record inside a database
    @field <lt>SeeBelow<gt> | <lt>CEOIDINFOUNIONref<gt> | Note: The remaining members form a union
    @field CEFILEINFO   | infFile      | Valid for file objects
    @field CEDIRINFO    | infDirectory | Valid for directory objects
    @field CEDBASEINFO  | infDatabase  | Valid for database objects
    @field CERECORDINFO | infRecord    | Valid for record objects
    @xref   <t CEFILEINFO>  <t CEDIRINFO> <t CEDBASEINFO>  <t CERECORDINFO>
     }
    {@field Version of this structure }
    {@field Type of object }
    {@flag OBJTYPE_INVALID   | There was no valid object with this CEOID }
    {@flag OBJTYPE_FILE      | The object is a file }
    {@flag OBJTYPE_DIRECTORY | The object is a directory }
    {@flag OBJTYPE_DATABASE  | The object is a database }
    {@flag OBJTYPE_RECORD    | The object is a record inside a database }
    { Valid for file objects }
    { Valid for directory objects }
    { @CESYSGEN IF FILESYS_FSDBASE }
    { Valid for database objects }
    { Valid for record objects }
    { @CESYSGEN ENDIF }
 in member_list *)
(* error
    };
in declaration at line 541 *)
(* error
} CEOIDINFOEX, PCEOIDINFOEX;
in declaration at line 542 *)
    { Functions }

    function CeOidGetInfoEx2(pguid:PCEGUID; oid:CEOID; var oidInfo:CEOIDINFOEX):Boolean; external KernelDLL name 'CeOidGetInfoEx2';
    function CeOidGetInfoEx(pguid:PCEGUID; oid:CEOID; var oidInfo:CEOIDINFO):Boolean; external KernelDLL name 'CeOidGetInfoEx';
    function CeOidGetInfo(oid:CEOID; var oidInfo:CEOIDINFO):Boolean; external KernelDLL name 'CeOidGetInfo';

    { @CESYSGEN ENDIF }
{$ifdef WINCEOEM}
{$include <pwindbas.h>   // internal defines }
{$ifdef WINCEMACRO}
{$include <mwindbas.h>}
{$endif}
{$endif}
    { @CESYSGEN ENDIF }
{$endif}

implementation

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }
  function TypeFromPropID(propid : longint) : longint;
    begin
       TypeFromPropID:=LOWORD(propid);
    end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    procedure CREATE_SYSTEMGUID(out pguid : CEGUID);
      begin
         //CREATE_SYSTEMGUID:=memset(pguid,0,sizeof(CEGUID));
         fillchar(pguid,0,sizeof(CEGUID));
      end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    procedure CREATE_INVALIDGUID(out pguid : CEGUID);
      begin
         //CREATE_INVALIDGUID:=memset(pguid,-(1),sizeof(CEGUID));
         fillchar(pguid,-(1),sizeof(CEGUID));
      end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function CHECK_SYSTEMGUID(pguid : PCEGUID) : longint;
      begin
         CHECK_SYSTEMGUID:= not ((((pguid^.Data1) or (pguid^.Data2)) or (pguid^.Data3)) or (pguid^.Data4));
         //CHECK_SYSTEMGUID:= _CEGUID(not ((((pguid.Data1) or (pguid.Data2)) or (pguid.Data3)) or (pguid.Data4)));
      end;

    { was #define dname(params) para_def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function CHECK_INVALIDGUID(pguid : PCEGUID) : longint;
      begin
         CHECK_INVALIDGUID:= not ( not ((((pguid^.Data1) and (pguid^.Data2)) and (pguid^.Data3)) and (pguid^.Data4)));
      end;


end.
