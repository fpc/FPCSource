{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Include file for synchronization modules for Windows CE
 Converted from cesync.h by Yury Sidorov                               }

{$mode objfpc}
unit cesync;

interface

uses Windows {$ifdef UNDER_CE}, windbase {$endif};

{$IFDEF FPC}
{$PACKRECORDS C}
{$ELSE}
type
  TCHAR = char;
{$ENDIF}

{ max size of the object type name }
const
   MAX_OBJTYPE_NAME = 100;   
{ max. size of a packet in IReplObjHandler::GetPacket & IReplObjHandler::SetPacket (about 254K) }
   MAX_PACKET_SIZE = 260000;   
   MAX_ACTIVE_VOL = 16;   { up to 16 active volumes (including the default system volume) can be synchronized during each connection }
   
type
  OBJTYPENAME = array[0..MAX_OBJTYPE_NAME-1] of TCHAR;
  OBJTYPENAMEA = array[0..MAX_OBJTYPE_NAME-1] of CHAR;
  OBJTYPENAMEW = array[0..MAX_OBJTYPE_NAME-1] of WCHAR;
  
{ Error/Return code used }
const
   RERR_SHUT_DOWN = $80140001;   { serious error, asking implementation to shut down immediately }
   RERR_STORE_REPLACED = $80140002;   { the store was replaced. }
   RERR_CANCEL = $80140003;   { user cancel the operation }
   RERR_RESTART = $80140004;   { restart the operation, applicable in RSC_END_SYNC & RSC_END_CHECK }
   RERR_IGNORE = $80140005;   { used by IReplStore::GetConflictInfo. }
   RERR_UNLOAD = $80140006;   { used by IReplStore::ActivateDialog or IReplStore::IsFolderChanged to request unloading of replication modules }
   RERR_OBJECT_DELETED = $80140007;   { used by IReplStore::IsValidObject, indicates the object identified by the hObject is deleted }
   RERR_CORRUPT = $80140008;   { used by IReplStore::IsValidObject, indicates the object identified by the hObject is corrupted }
   RERR_NO_DEVICE = $80140009;   { returned by IReplNotify::QueryDevice. indicates no selected or connected device exists }
   RERR_NO_ERR_PROMPT = $80140010;   { returned by IReplStore::Initialize. indicates error initializing. No UI is needed to show this error. }
   RERR_DISCARD = $80140011;   { returned by IReplObjHandler::SetPacket. indicates this object should be discarded from the device immediately. }
   RERR_DISCARD_LOCAL = $80140012;   { returned by IReplObjHandler::SetPaket. indicates this object should be discarded from the desktop only. }
   RERR_VOL_INACTIVE = $80140013;   { returned by IReplObjHandler::GetPacket && IReplObjHandler::SetPacket, the volume has become inactive. }
   RERR_BIG_OBJ_TYPE = $80140014;   { returned by IReplNotify::QueryDevice on QDC_SYNC_DATA }
   RERR_BIG_CODE = $80140015;   { returned by IReplNotify::QueryDevice on QDC_SYNC_DATA }
   RERR_UNMATCHED = $80140016;   { returned by IReplNotify::QueryDevice on QDC_SYNC_DATA }
   RERR_DEVICE_WIN = $80140017;   { returned by IReplStore::GetConflictInfo, resolve the conflict so device object wins }
   RERR_DESKTOP_WIN = $80140018;   { returned by IReplStore::GetConflictInfo, resolve the conflict so desktop object wins }
   RERR_SKIP_ALL_OBJ = $80140019;   { returned by IReplStore::ReportStatus on RSC_WRITE_OBJ_FAILED, skip sync of all remaining objects }
{ use by IReplObjHandler }
   RERR_SKIP_ALL = $80140100;   { skip all incoming packets because of write errors }
   RERR_BAD_OBJECT = $80140101;   { this is a bad object because of read error, server should not try to replicate it again }
   RERR_TRY_AGAIN = $80140102;   { this is a bad object because of read error, server should can try to replicate it again later }
   RERR_USER_SKIP = $80140103;   { object skipped by the user }
{ these are warning codes }
   RWRN_LAST_PACKET = $00140001;   
{ flags used in RSC_BEGIN_SYNC }
   BSF_AUTO_SYNC = $00000001;   { This flag is being obsoleted in ActiveSync 3.0 }
   BSF_REMOTE_SYNC = $00000002;   { consistent with RSC_REMOTE_SYNC, set if we are sync'ing remotely }
   BSF_SHOW_FATAL_ERRORS = $00000004;   { If an error occurs which prevents the SSP from synchronizing at all, it can show the error to the user when this flag is set }
   BSF_SHOW_RESOLVE_ERRORS = $00000008;   { The SSP can show any error messages that it comes across as during the resolve items phase }
   BSF_RESERVED = $80000000;   { Reserved by ActiveSync server. }
{ Code for ReportStatus }
   RSC_BEGIN_SYNC = 1;   { Synchronization is about to start, uReserved is combination of bit flags, see BSF_* above }
   RSC_END_SYNC = 2;   { Synchronization is about to end }
   RSC_BEGIN_CHECK = 3;   { FindFirstItem is about to be called, followed by FindNextItem }
   RSC_END_CHECK = 4;   { FindItemClose has been called }
   RSC_DATE_CHANGED = 5;   { System Date has changed, this is called for each known desktop object, unless when both hFolder & hItem are NULL }
   RSC_RELEASE = 6;   { Replication is about to release the store }
   RSC_REMOTE_SYNC = 7;   { Indicates if remote sync is enabled. uParam will TRUE if all sync }
{ will be remote until this status is reported again with uParam set to FALSE }
   RSC_INTERRUPT = 8;   { interrupt current operation }
   RSC_BEGIN_SYNC_OBJ = 9;   { Synchronization is about to start on an object type. uReserved points to }
   RSC_END_SYNC_OBJ = 10;   { Synchronization is about to end on an object type. }
   RSC_OBJ_TYPE_ENABLED = 11;   { Synchronization of the given object is enabled, hFolder is indeed a pointer to a string (object type name) }
   RSC_OBJ_TYPE_DISABLED = 12;   { Synchronization of the given object is disabled, hFolder is indeed a pointer to a string (object type name) }
   RSC_BEGIN_BATCH_WRITE = 13;   { A series of SetPacket will be called on a number of objects, this is the right time for some service providers to start a transaction }
   RSC_END_BATCH_WRITE = 14;   { above write ends, this is the right time for some service providers to commit the transaction }
   RSC_CONNECTION_CHG = 15;   { connection status has changed. uParam is TRUE if connection established. FALSE otherwise. }
   RSC_WRITE_OBJ_FAILED = 16;   { failed writing an object on the device. uParam is the HRESULT code. }
   RSC_DELETE_OBJ_FAILED = 17;   { failed deleting an object on the device. uParam is the HRESULT code. }
   RSC_WRITE_OBJ_SUCCESS = 18;   { writing of an object succeeded on the device. uParam is a pointer to SDREQUEST (with (lpbData, cbData) representing the volume ID) }
   RSC_DELETE_OBJ_SUCCESS = 19;   { deletion of an object succeeded on the device. uParam is a pointer to SDREQUEST (with (lpbData, cbData) representing the volume ID) }
   RSC_READ_OBJ_FAILED = 20;   { failed to read an object from the device. uParam is the HRESULT code }
   RSC_TIME_CHANGED = 21;   { System time has changed, this is called only once. }
   RSC_BEGIN_BACKUP = 22;   { Backup is about to start. }
   RSC_END_BACKUP = 23;   { Backup has ended. }
   RSC_BEGIN_RESTORE = 24;   { Restore is about to start. }
   RSC_PREPARE_SYNC_FLD = 26;   { Prepare to sync one specific folder whether or not any objects of the type are dirty. hFolder is a pointer to the object name that will be synced.  }

{ code for QueryDevice }
   QDC_SEL_DEVICE = 1;   { Selected device info, *ppvData points to DEVINFO }
   QDC_CON_DEVICE = 2;   { Connected device info, *ppvData points to DEVINFO }
   QDC_SEL_DEVICE_KEY = 3;   { get a registry key that can be used to store selected device specific settings. }
{ *ppvData points to HKEY, caller must close reg key when its usage is over }
   QDC_CON_DEVICE_KEY = 4;   { get a registry key that can be used to store connnected device specific settings. }
{ *ppvData points to HKEY, caller must close reg key when its usage is over }
   QDC_SYNC_DATA = 5;   { get or set custom sync data from the device, *ppvData points to SDREQUEST }
   INF_OVERRIDE = $0001000;   { used for OnItemNotify, override the default action of "delete wins over change" }

   RNC_CREATED = 1;
   RNC_MODIFIED = 2;
   RNC_DELETED = 3;
   RNC_SHUTDOWN = 4;
   RNC_IDLE = 5;
{$ifndef UNDER_CE}
   SCF_SINGLE_THREAD = $00000001;   { set if the implementation only supports single thread operation. }
   SCF_SIMULATE_RTS = $00000002;   { set if the implementation wants to simulate detection of real-time change/deletes }

{ flags for uParam of IReplStore::ReportStatus }
   PSA_RESET_INTERRUPT = $00000001;   { this flag is set if we're clearing the interrupt state (ie. we go back to normal operation) }
   PSA_SYS_SHUTDOWN = $00000002;   { Windows is shutting down }
{ Actions for Setup }
   RSTP_SETUP = $0001;   { New setup }
   RSTP_CREATE = $0002;   { New profile }
   RSTP_RENAME = $0003;   { Rename profile }
   RSTP_DELETE = $0004;   { Delete profile }
   
   ISF_SELECTED_DEVICE = $00000001;
   ISF_REMOTE_CONNECTED = $00000002;
{$endif UNDER_CE}

{========================= IReplNotify ============================== }
type
   tagDevInfo = record
        pid : DWORD;                             { device ID }
        szName : array[0..(MAX_PATH)-1] of char; { device name }
        szType : array[0..79] of char;           { device type }
        szPath : array[0..(MAX_PATH)-1] of char; { device path }
     end;
   DEVINFO = tagDevInfo;
   PDEVINFO = ^DEVINFO;
   PPDEVINFO = ^PDEVINFO;

{ a structure used to get/set custom sync. data from/to the device }
{ the object type where this data is coming from }
{ TRUE if sending data down and FALSE if getting data up }
{ for getting data from the device, this code must be less than 8 }
   PSDREQUEST = ^SDREQUEST;
   SDREQUEST = record
        szObjType : OBJTYPENAME;
        fSet : BOOL;
        uCode : UINT;
        lpbData : pointer;
        cbData : UINT;
     end;
   PPSDREQUEST = ^PSDREQUEST;
   
{$ifndef UNDER_CE}
   HREPLOBJ = pointer;
   HREPLITEM = pointer;
   HREPLFLD = pointer;
   PHREPLOBJ = ^HREPLOBJ;
   PHREPLITEM = ^HREPLITEM;
   PHREPLFLD = ^HREPLFLD;
   
   IReplStore = interface;
{$endif}
   IReplNotify = interface;

   _tagReplSetup = record
        cbStruct : UINT;
        fRead : BOOL;
        dwFlags : DWORD;
        hr : HRESULT;
        szObjType : OBJTYPENAME;
        pNotify : IReplNotify;
        oid : DWORD;
        oidNew : DWORD;
{$ifndef UNDER_CE}
        pStore : IReplStore;
        hFolder : HREPLFLD;
        hItem : HREPLITEM;
{$endif}
        lpbVolumeID : PBYTE;
        cbVolumeID : UINT;
     end;
   REPLSETUP = _tagReplSetup;
   PREPLSETUP = ^REPLSETUP;
   PPREPLSETUP = ^PREPLSETUP;
   
  IReplNotify = interface(IUnknown)
{$ifndef UNDER_CE}
    function SetStatusText(lpszText: LPSTR): HRESULT; stdcall; // lpszText can have special syntax, see programmer's guide
    function GetWindow(uFlags: UINT): HWND; stdcall;
    function OnItemNotify(uCode: UINT; lpszProgId, lpszName: LPSTR; hItem: HREPLITEM; ulFlags: ULONG): HRESULT; stdcall;
    function QueryDevice(uCode: UINT; ppvData: pointer): HRESULT; stdcall;
{$endif}
    // Internal use only
    function OnItemCompleted(pSetup: PREPLSETUP): HRESULT; stdcall;
  end;

{$ifndef UNDER_CE}
   tagStoreInfo = record
        cbStruct : UINT;
        uFlags : UINT;
        szProgId : array[0..255] of TCHAR;
        szStoreDesc : array[0..199] of TCHAR;
        uTimerRes : UINT;
        cbMaxStoreId : UINT;
        cbStoreId : UINT;
        lpbStoreId : PBYTE;
     end;
   STOREINFO = tagStoreInfo;
   PSTOREINFO = ^STOREINFO;
   PPSTOREINFO = ^PSTOREINFO;

   tagObjUIData = record
        cbStruct : UINT;
        hIconLarge : HICON;
        hIconSmall : HICON;
        szName : array[0..(MAX_PATH)-1] of char;
        szSyncText : array[0..(MAX_PATH)-1] of char;
        szTypeText : array[0..79] of char;
        szPlTypeText : array[0..79] of char;
     end;
   OBJUIDATA = tagObjUIData;
   POBJUIDATA = ^OBJUIDATA;
   PPOBJUIDATA = ^POBJUIDATA;
   TReplDialogs = (OPTIONS_DIALOG);

{========================= IEnumReplItem ============================== }
  IEnumReplItem = interface(IUnknown)
    ['{a417bc0e-7be1-11ce-ad82-00aa006ec559}']
    function Next(celt: ULONG; var hItem: HREPLITEM; var celtFetched: ULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(var enum: IEnumReplItem): HRESULT; stdcall;
    function GetFolderHandle: HREPLFLD; stdcall;
  end;

   tagConfInfo = record
        cbStruct : UINT;
        hFolder : HREPLFLD;
        hLocalItem : HREPLITEM;
        hRemoteItem : HREPLITEM;
        szLocalName : OBJTYPENAME;
        szLocalDesc : array[0..511] of TCHAR;
        szRemoteName : OBJTYPENAME;
        szRemoteDesc : array[0..511] of TCHAR;
     end;
   CONFINFO = tagConfInfo;
   PCONFINFO = ^CONFINFO;
   PPCONFINFO = ^PCONFINFO;
   
{========================= IReplSetup ============================== }
  IReplSetup = interface(IUnknown)
    ['{60178ec0-c670-11d0-837a-0000f80220b9}']
    function Setup(hwndParent: HWND; dwDeviceId: DWORD; wAction: WORD): HRESULT; stdcall;
  end;
  
//========================= IReplStore ==============================
  IReplStore = interface(IUnknown)
    ['{a417bc0f-7be1-11ce-ad82-00aa006ec559}']
    // *** IReplStore methods ***
    function Initialize(Notify: IReplNotify; uFlags: UINT): HRESULT; stdcall;
    function GetStoreInfo(StoreInfo: PSTOREINFO): HRESULT; stdcall;
    function ReportStatus(hFld: HREPLFLD; hItem: HREPLITEM; uStatus, uParam: UINT): HRESULT; stdcall;
    function CompareStoreIDs(lpbID1: pointer; cbID1: UINT; lpbID2: pointer; cbID2: UINT): LongInt; stdcall;
    // Item related routines
    function CompareItem(hItem1, hItem2: HREPLITEM): LongInt; stdcall;
    function IsItemChanged(hFld: HREPLFLD; hItem, hItemComp: HREPLITEM): BOOL; stdcall;
    function IsItemReplicated(hFld: HREPLFLD; hItem: HREPLITEM): BOOL; stdcall;
    procedure UpdateItem(hFld: HREPLFLD; hItemDst, hItemSrc: HREPLITEM); stdcall;
    // Folder related routines
    function GetFolderInfo(lpszObjType: LPSTR; var phFld: HREPLFLD; var ppObjHandler: IUnknown): HRESULT; stdcall;
    function IsFolderChanged(hFld: HREPLFLD; var pfChanged: BOOL): HRESULT; stdcall;
    // Enumeration of folders
    function FindFirstItem(hFld: HREPLFLD; var phItem: HREPLITEM; var pfExist: BOOL): HRESULT; stdcall;  // get first object the folder
    function FindNextItem(hFld: HREPLFLD; var phItem: HREPLITEM; var pfExist: BOOL): HRESULT; stdcall;   // get next object the folder
    function FindItemClose(hFld: HREPLFLD): HRESULT; stdcall;                                            // done enumerating
    // Object management routines
    function ObjectToBytes(hObject:HREPLOBJ; lpb:pointer):UINT; stdcall;
    function BytesToObject(lpb:pointer; cb:UINT):HREPLOBJ; stdcall;
    procedure FreeObject(hObject:HREPLOBJ); stdcall;
    function CopyObject(hObjSrc:HREPLOBJ; hObjDest:HREPLOBJ):BOOL; stdcall;
    function IsValidObject(hFld:HREPLFLD; hObject:HREPLITEM; uFlags:UINT): HRESULT; stdcall;
    // UI related routines
    function ActivateDialog(uidDialog:TReplDialogs; hwndParent:HWND; hFld:HREPLFLD; penumItem:IEnumReplItem): HRESULT; stdcall;
    function GetObjTypeUIData(hFld:HREPLFLD; pData:POBJUIDATA): HRESULT; stdcall;
    function GetConflictInfo(pConfInfo:PCONFINFO): HRESULT; stdcall;
    function RemoveDuplicates(lpszObjType:LPSTR; uFlags:UINT): HRESULT; stdcall;
  end;
{$endif UNDER_CE}

{=========== Section for object serializing & deserializing interfaces ========== }
const
   RSF_CONFLICT_OBJECT = $00000001;   { this is about getting/writting a conflicting object }
   RSF_NEW_OBJECT = $00000002;   { this is a new object to be written }
   RSF_DUPLICATED_OBJECT = $00000004;   { the object is an exact duplicate of an existing object }
   RSF_COMBINE = $00000008;   { the object is being writen to desktop during a combine operation }
   RSF_SYNC_DEVICE_ONLY = $00000010;   { the object should be sync'ed from device to desktop only }
   RSF_SYNC_DESKTOP_ONLY = $00000020;   { the object should be sync'ed from desktop to device only }
   RSF_UPDATED_HANDLE = $00000040;   { this is a new object, but the oid already exists (eg, file rename) }
   RSF_DISCARDED_OBJ = $00000080;   { used in DeleteObj. indicates the object is deleted as a result of RERR_DISCARD being returned by SetPacket }
   RSF_NEW_VOLUME = $00000100;   { used by ActiveSync manager only. }
   RSF_AUTO_COMBINE = $00000200;   { the object is being written to the desktop, similar to RSF_COMBINE except there were no items on the desktop to combine with }
   RSF_RESERVED1 = $00100000;   { reserved by ActiveSync manager: DO NOT USE THESE }
   RSF_RESERVED2 = $00200000;   
   RSF_RESERVED3 = $00400000;   
   RSF_RESERVED4 = $00800000;   
   
{========================= IReplObjHandler ============================== }
{ Specifies the interface for replication object handler }
{ (object serializer/deserializer) }

type
  IReplObjHandler = interface(IUnknown)
    //  Called everytime when an object is about to be serialized/deserialized
    function Setup(pSetup: PREPLSETUP): HRESULT; stdcall;
    //  Called everytime when it's the time to clean up the serializer/deserializer for the object
    function Reset(pSetup: PREPLSETUP): HRESULT; stdcall;
    // A request to get a data packet (serialize the object)
    // handler should pass back the buffer along with the size bytes
    function GetPacket(var lpbData: pointer; var cbData: DWORD; cbRecommend: DWORD): HRESULT; stdcall;
    // A request to set a data packet (deserialize the byte stream)
    function SetPacket(lpbData: pointer; cbData: DWORD): HRESULT; stdcall;
    // A request to delete the given object
    function DeleteObj(pSetup: PREPLSETUP): HRESULT; stdcall;
  end;

   tagObjTypeInfo = record
        cbStruct : UINT;
        szObjType : OBJTYPENAMEW;
        uFlags : UINT;
        szName : array[0..79] of WCHAR;
        cObjects : UINT;
        cbAllObj : UINT;
        ftLastModified : FILETIME;
     end;
   OBJTYPEINFO = tagObjTypeInfo;
   POBJTYPEINFO = ^OBJTYPEINFO;
   PPOBJTYPEINFO = ^POBJTYPEINFO;

{$ifdef UNDER_CE}
const
   ONF_FILE = $00000001;   
   ONF_DIRECTORY = $00000002;   
   ONF_DATABASE = $00000004;   
   ONF_RECORD = $00000008;   
   ONF_CHANGED = $00000010;   { set if the file system object is changed }
   ONF_DELETED = $00000020;   { set if the file system object is deleted }
   ONF_CLEAR_CHANGE = $00000040;   { client should clear the change bit for the object whose object id is pointed at by poid }
   ONF_CALL_BACK = $00000080;   { Output, client asks server to call ObjectNotify 2 sec. later. (ObjectNotify is callback  }
{ function, see definition of POBJNOTIFYPROC) }
   ONF_CALLING_BACK = $00000100;   { set if this call is a result of ONF_CALL_BACK being set earlier }
{  Definitions of cOidChg, cOidDel and poid
    in all cases, poid points to a list of object id's

1) when ONF_CHANGED is set, cOidChg is the number of object id's in the list that should be synchronized. cOidDel is not used
2) when ONF_DELETED is set, cOidChg is not used, cOidDel is the number of deleted object id's in the list that should be synchronized
3) when both ONF_CHANGED & ONF_DELETED is not set,
    cOidChg is count of object id's in the first part of the list for objects that are changed
    cOidDel is count of object id's in the later part of the list for objects that are not changed

 }
type
   tagObjNotify = record
        cbStruct : UINT;
        szObjType : OBJTYPENAME;
        uFlags : UINT;
        uPartnerBit : UINT;
        oidObject : CEOID;
        oidInfo : CEOIDINFO;
        cOidChg : UINT;
        cOidDel : UINT;
        poid : PUINT;
        lpbVolumeID : LPBYTE;
        cbVolumeID : UINT;
     end;
   OBJNOTIFY = tagObjNotify;
   POBJNOTIFY = ^OBJNOTIFY;
   PPOBJNOTIFY = ^POBJNOTIFY;

const
   FO_MORE_VOLUME = $00000001;   { set by ActiveSync module. there are more volumes of objects }
   FO_DONE_ONE_VOL = $00000002;   { set by ActiveSync manager, let ActiveSync module to free up the memory allocated in FINDOBJINFO }

type
   tagFindObjInfo = record
        uFlags : UINT;
        szObjType : OBJTYPENAME;
        poid : PUINT;
        cUnChg : UINT;
        cChg : UINT;
        lpbVolumeID : LPBYTE;
        cbVolumeID : UINT;
        lpvUser : LPVOID;
     end;
   FINDOBJINFO = tagFindObjInfo;
   PFINDOBJINFO = ^FINDOBJINFO;
   PPFINDOBJINFO = ^PFINDOBJINFO;

{ Functions exported by client's device module }
{ for Function: InitObjType }
   PINITOBJPROC = function (lpszObjType:LPWSTR; var ppObjHandler:IReplObjHandler; uPartnerBit:UINT):BOOL;cdecl;
{ for Function: ObjectNotify }
   POBJNOTIFYPROC = function (_para1:POBJNOTIFY):BOOL;cdecl;
{ for Function: GetObjTypeInfo }
   PGETOBJTYPEINFO = function (_para1:POBJTYPEINFO):BOOL;cdecl;
{ for Function: ReportStatus }
   PREPORTSTATUS = function (lpszObjType:LPWSTR; uCode:UINT; uParam:UINT):BOOL;cdecl;
{ for Function: FindObjects }
   PFINDOBJECTS = function (_para1:PFINDOBJINFO):HRESULT;cdecl;
{ for Function: SyncData }
   PSYNCDATA = function (psd:PSDREQUEST):HRESULT;cdecl;

{$endif UNDER_CE}

const
   SZ_OUTSTORE_PROG_ID = 'MS.WinCE.OutLook';   
   SZ_SCDSTORE_PROG_ID = 'MS.WinCE.SchedulePlus';   
   SZ_APPT = 'Appointment';   
   SZ_CONTACT = 'Contact';   
   SZ_TASK = 'Task';   
   SZ_FILE = 'File';   
   SZ_INBOX = 'Inbox';   
   SZ_CHANNELS = 'Channel';
   
implementation

end.
