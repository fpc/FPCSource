{
    $Id: objbase.inc,v 1.1 2005/03/28 15:09:35 peter Exp $
    This file is part of the Free Pascal run time library.

    Contains the corresponding declarations to objbase.h
    Used objbase.h, file date 2001-09-14 from MingW-C
    This file is heavily edited after conversion!

 **********************************************************************}
{$ifdef read_interface}

  function CoBuildVersion:DWORD;stdcall; external  'ole32.dll' name 'CoBuildVersion';

  function CoInitialize(_para1:PVOID):HRESULT;stdcall; external  'ole32.dll' name 'CoInitialize';

  function CoInitializeEx(_para1:LPVOID; _para2:DWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoInitializeEx';

  procedure CoUninitialize;stdcall; external  'ole32.dll' name 'CoUninitialize';

  function CoGetMalloc(_para1:DWORD; out _para2:IMalloc):HRESULT;stdcall; external  'ole32.dll' name 'CoGetMalloc';

  function CoGetCurrentProcess:DWORD;stdcall; external  'ole32.dll' name 'CoGetCurrentProcess';

  function CoRegisterMallocSpy(_para1:IMallocSpy):HRESULT;stdcall; external  'ole32.dll' name 'CoRegisterMallocSpy';

  function CoRevokeMallocSpy:HRESULT;stdcall; external  'ole32.dll' name 'CoRevokeMallocSpy';

  function CoCreateStandardMalloc(_para1:DWORD; out _para2:IMalloc):HRESULT;stdcall; external  'ole32.dll' name 'CoGetMalloc';

  function CoGetClassObject(const _para1:TCLSID; _para2:DWORD; _para3:PVOID; const _para4:TIID; out _para5):HRESULT;stdcall; external  'ole32.dll' name 'CoGetClassObject';

  function CoRegisterClassObject(const _para1:TCLSID; _para2:IUnknown; _para3:DWORD; _para4:DWORD; _para5:PDWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoRegisterClassObject';

  function CoRevokeClassObject(_para1:DWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoRevokeClassObject';

  function CoGetMarshalSizeMax(_para1:PULONG;const _para2:TIID; _para3:IUnknown; _para4:DWORD; _para5:PVOID;
             _para6:DWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoGetMarshalSizeMax';

  function CoMarshalInterface(_para1:IStream;const _para2:TIID; _para3:IUnknown; _para4:DWORD; _para5:PVOID;
             _para6:DWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoMarshalInterface';

  function CoUnmarshalInterface(_para1:IStream;const _para2:TIID; out _para3):HRESULT;stdcall; external  'ole32.dll' name 'CoUnmarshalInterface';

  function CoMarshalHresult(_para1:IStream; _para2:HRESULT):HRESULT;stdcall; external  'ole32.dll' name 'CoMarshalHresult';

  function CoUnmarshalHresult(_para1:IStream; _para2:HRESULT):HRESULT;stdcall; external  'ole32.dll' name 'CoUnmarshalHresult';

  function CoReleaseMarshalData(_para1:IStream):HRESULT;stdcall; external  'ole32.dll' name 'CoReleaseMarshalData';

  function CoDisconnectObject(_para1:IUnknown; _para2:DWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoDisconnectObject';

  function CoLockObjectExternal(_para1:IUnknown; _para2:BOOL; _para3:BOOL):HRESULT;stdcall; external  'ole32.dll' name 'CoLockObjectExternal';

  function CoGetStandardMarshal(const _para1:TIID; _para2:IUnknown; _para3:DWORD; _para4:PVOID; _para5:DWORD;
             out _para6:IMarshal):HRESULT;stdcall; external  'ole32.dll' name 'CoGetStandardMarshal';

  function CoGetStdMarshalEx(_para1:IUnknown; _para2:DWORD; out _para3:IUnknown):HRESULT;stdcall; external  'ole32.dll' name 'CoGetStdMarshalEx';

  function CoIsHandlerConnected(_para1:IUnknown):BOOL;stdcall; external  'ole32.dll' name 'CoIsHandlerConnected';

  function CoHasStrongExternalConnections(_para1:IUnknown):BOOL;stdcall; external  'ole32.dll' name 'CoHasStrongExternalConnections';

  function CoMarshalInterThreadInterfaceInStream(const _para1:TIID; _para2:IUnknown; out _para3:IStream):HRESULT;stdcall; external  'ole32.dll' name 'CoMarshalInterThreadInterfaceInStream';

  function CoGetInterfaceAndReleaseStream(_para1:IStream;const _para2:TIID; out _para3):HRESULT;stdcall; external  'ole32.dll' name 'CoGetInterfaceAndReleaseStream';

  function CoCreateFreeThreadedMarshaler(_para1:IUnknown; out _para2:IUnknown):HRESULT;stdcall; external  'ole32.dll' name 'CoCreateFreeThreadedMarshaler';

  function CoLoadLibrary(_para1:LPOLESTR; _para2:BOOL):THandle;stdcall; external  'ole32.dll' name 'CoLoadLibrary';

  procedure CoFreeLibrary(_para1:THandle);stdcall; external  'ole32.dll' name 'CoFreeLibrary';

  procedure CoFreeAllLibraries;stdcall; external  'ole32.dll' name 'CoFreeAllLibraries';

  procedure CoFreeUnusedLibraries;stdcall; external  'ole32.dll' name 'CoFreeUnusedLibraries';

  function CoCreateInstance(const _para1:TCLSID; _para2:IUnknown; _para3:DWORD;const _para4:TIID;out _para5):HRESULT;stdcall; external  'ole32.dll' name 'CoCreateInstance';

  function CoCreateInstanceEx(const _para1:TCLSID; _para2:IUnknown; _para3:DWORD; _para4:PCOSERVERINFO; _para5:DWORD;
             _para6:PMULTI_QI):HRESULT;stdcall; external  'ole32.dll' name 'CoCreateInstanceEx';

  function StringFromCLSID(const _para1:TCLSID; out _para2:POLESTR):HRESULT;stdcall; external  'ole32.dll' name 'StringFromCLSID';

  function CLSIDFromString(_para1:LPOLESTR; _para2:LPCLSID):HRESULT;stdcall; external  'ole32.dll' name 'CLSIDFromString';

  function StringFromIID(const _para1:TIID; out _para2:POLESTR):HRESULT;stdcall; external  'ole32.dll' name 'StringFromIID';

  function IIDFromString(_para1:LPOLESTR; out _para2:TIID):HRESULT;stdcall; external  'ole32.dll' name 'IIDFromString';

  function CoIsOle1Class(const _para1:TCLSID):BOOL;stdcall; external  'ole32.dll' name 'CoIsOle1Class';

  function ProgIDFromCLSID(const _para1:TCLSID; out _para2:POLESTR):HRESULT;stdcall; external  'ole32.dll' name 'ProgIDFromCLSID';

  function CLSIDFromProgID(_para1:POLESTR; _para2:LPCLSID):HRESULT;stdcall; external  'ole32.dll' name 'CLSIDFromProgID';

  function StringFromGUID2(const _para1:TGUID; _para2:LPOLESTR; _para3:longint):longint;stdcall; external  'ole32.dll' name 'StringFromGUID2';

  function CoCreateGuid(_para1:PGUID):HRESULT;stdcall; external  'ole32.dll' name 'CoCreateGuid';

  function CoFileTimeToDosDateTime(_para1:PFILETIME; _para2:LPWORD; _para3:LPWORD):BOOL;stdcall; external  'ole32.dll' name 'CoFileTimeToDosDateTime';

  function CoDosDateTimeToFileTime(_para1:WORD; _para2:WORD; _para3:PFILETIME):BOOL;stdcall; external  'ole32.dll' name 'CoDosDateTimeToFileTime';

  function CoFileTimeNow(_para1:PFILETIME):HRESULT;stdcall; external  'ole32.dll' name 'CoFileTimeNow';

  function CoRegisterMessageFilter(_para1:IMessageFilter;out _para2:IMessageFilter):HRESULT;stdcall; external  'ole32.dll' name 'CoRegisterMessageFilter';

  function CoGetTreatAsClass(const _para1:TCLSID; _para2:LPCLSID):HRESULT;stdcall; external  'ole32.dll' name 'CoGetTreatAsClass';

  function CoTreatAsClass(const _para1:TCLSID; const _para2:TCLSID):HRESULT;stdcall; external  'ole32.dll' name 'CoTreatAsClass';


  type

     LPFNGETCLASSOBJECT = function (const _para1:TCLSID; const _para2:TIID;out _para3):HRESULT;stdcall;

     LPFNCANUNLOADNOW = function:HRESULT;stdcall;

  function DllGetClassObject(const _para1:TCLSID; const _para2:TIID; out _para3):HRESULT;stdcall; external  'ole32.dll' name 'DllGetClassObject';

  function DllCanUnloadNow:HRESULT;stdcall; external  'ole32.dll' name 'DllCanUnloadNow';

  function CoTaskMemAlloc(_para1:ULONG):PVOID;stdcall; external  'ole32.dll' name 'CoTaskMemAlloc';

  function CoTaskMemRealloc(_para1:PVOID; _para2:ULONG):PVOID;stdcall; external  'ole32.dll' name 'CoTaskMemRealloc';

  procedure CoTaskMemFree(_para1:PVOID);stdcall; external  'ole32.dll' name 'CoTaskMemFree';

  function CreateDataAdviseHolder(_para1:IDataAdviseHolder):HRESULT;stdcall; external  'ole32.dll' name 'CreateDataAdviseHolder';

  function CreateDataCache(_para1:IUnknown; const _para2:TCLSID; const _para3:TIID; out _para4):HRESULT;stdcall; external  'ole32.dll' name 'CreateDataCache';

(* Const before type ignored *)
  function StgCreateDocfile(_para1:POLESTR; _para2:DWORD; _para3:DWORD; out _para4:IStorage):HRESULT;stdcall; external  'ole32.dll' name 'StgCreateDocfile';

  function StgCreateDocfileOnILockBytes(_para1:ILockBytes; _para2:DWORD; _para3:DWORD; out _para4:IStorage):HRESULT;stdcall; external  'ole32.dll' name 'StgCreateDocfileOnILockBytes';

(* Const before type ignored *)
  function StgOpenStorage(_para1:POLESTR; _para2:IStorage; _para3:DWORD; _para4:SNB; _para5:DWORD;
             out _para6:IStorage):HRESULT;stdcall; external  'ole32.dll' name 'StgOpenStorage';

  function StgOpenStorageOnILockBytes(_para1:ILockBytes; _para2:IStorage; _para3:DWORD; _para4:SNB; _para5:DWORD;
             out _para6:IStorage):HRESULT;stdcall; external  'ole32.dll' name 'StgOpenStorageOnILockBytes';

  function StgIsStorageFile(_para1:POLESTR):HRESULT;stdcall; external  'ole32.dll' name 'StgIsStorageFile';

  function StgIsStorageILockBytes(_para1:ILockBytes):HRESULT;stdcall; external  'ole32.dll' name 'StgIsStorageILockBytes';

  function StgSetTimes(_para1:POLESTR; _para2:PFILETIME; _para3:PFILETIME; _para4:PFILETIME):HRESULT;stdcall; external  'ole32.dll' name 'StgSetTimes';

  function BindMoniker(_para1:IMoniker; _para2:DWORD; _para3:TIID; out _para4):HRESULT;stdcall; external  'ole32.dll' name 'BindMoniker';

  function MkParseDisplayName(_para1:IBindCtx; _para2:POLESTR; out _para3:PULONG; out _para4:IMoniker):HRESULT;stdcall; external  'ole32.dll' name 'MkParseDisplayName';

  function MonikerRelativePathTo(_para1:IMoniker; _para2:IMoniker; out _para3:IMoniker; _para4:BOOL):HRESULT;stdcall; external  'ole32.dll' name 'MonikerRelativePathTo';

  function MonikerCommonPrefixWith(_para1:IMoniker; _para2:IMoniker; _para3:PIMoniker):HRESULT;stdcall; external  'ole32.dll' name 'MonikerCommonPrefixWith';

  function CreateBindCtx(_para1:DWORD;out _para2:IBindCtx):HRESULT;stdcall; external  'ole32.dll' name 'CreateBindCtx';

  function CreateGenericComposite(_para1:IMoniker; _para2:IMoniker; out _para3:IMoniker):HRESULT;stdcall; external  'ole32.dll' name 'CreateGenericComposite';

  function GetClassFile(_para1:POLESTR; out _para2:TCLSID):HRESULT;stdcall; external  'ole32.dll' name 'GetClassFile';

  function CreateFileMoniker(_para1:POLESTR; out _para2:IMoniker):HRESULT;stdcall; external  'ole32.dll' name 'CreateFileMoniker';

  function CreateItemMoniker(_para1:POLESTR; _para2:POLESTR;out _para3:IMoniker):HRESULT;stdcall; external  'ole32.dll' name 'CreateItemMoniker';

  function CreateAntiMoniker(_para1:PIMoniker):HRESULT;stdcall; external  'ole32.dll' name 'CreateAntiMoniker';

  function CreatePointerMoniker(_para1:IUnknown; out _para2:IMoniker):HRESULT;stdcall; external  'ole32.dll' name 'CreatePointerMoniker';

  function GetRunningObjectTable(_para1:DWORD; _para2:IRunningObjectTable):HRESULT;stdcall; external  'ole32.dll' name 'GetRunningObjectTable';

  function CoInitializeSecurity(_para1:PSECURITY_DESCRIPTOR; _para2:LONG; _para3:PSOLE_AUTHENTICATION_SERVICE; _para4:pointer; _para5:DWORD;
             _para6:DWORD; _para7:pointer; _para8:DWORD; _para9:pointer):HRESULT;stdcall; external  'ole32.dll' name 'CoInitializeSecurity';

  function CoGetCallContext(const _para1:TIID; _para2:Ppointer):HRESULT;stdcall; external  'ole32.dll' name 'CoGetCallContext';

  function CoQueryProxyBlanket(_para1:IUnknown; _para2:PDWORD; _para3:PDWORD; _para4:POLESTR; _para5:PDWORD;
             _para6:PDWORD; _para7:Pointer; _para8:PDWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoQueryProxyBlanket';

  function CoSetProxyBlanket(_para1:IUnknown; _para2:DWORD; _para3:DWORD; _para4:POLESTR; _para5:DWORD;
             _para6:DWORD; _para7:pointer; _para8:DWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoSetProxyBlanket';

  function CoCopyProxy(_para1:IUnknown; var _para2:IUnknown):HRESULT;stdcall; external  'ole32.dll' name 'CoCopyProxy';

  function CoQueryClientBlanket(_para1:PDWORD; _para2:PDWORD; _para3:POLESTR; _para4:PDWORD; _para5:PDWORD;
             _para6:pointer; _para7:PDWORD):HRESULT;stdcall; external  'ole32.dll' name 'CoQueryClientBlanket';

  function CoImpersonateClient:HRESULT;stdcall; external  'ole32.dll' name 'CoImpersonateClient';

  function CoRevertToSelf:HRESULT;stdcall; external  'ole32.dll' name 'CoRevertToSelf';

  function CoQueryAuthenticationServices(_para1:PDWORD; _para2:PSOLE_AUTHENTICATION_SERVICE):HRESULT;stdcall; external  'ole32.dll' name 'CoQueryAuthenticationServices';

  function CoSwitchCallContext(_para1:IUnknown; var _para2:IUnknown):HRESULT;stdcall; external  'ole32.dll' name 'CoSwitchCallContext';

  function CoGetInstanceFromFile(_para1:PCOSERVERINFO; _para2:PCLSID; _para3:IUnknown; _para4:DWORD; _para5:DWORD;
             _para6:POLESTR; _para7:DWORD; _para8:PMULTI_QI):HRESULT;stdcall; external  'ole32.dll' name 'CoGetInstanceFromFile';

  function CoGetInstanceFromIStorage(_para1:PCOSERVERINFO; _para2:PCLSID; _para3:IUnknown; _para4:DWORD; _para5:IStorage;
             _para6:DWORD; _para7:PMULTI_QI):HRESULT;stdcall; external  'ole32.dll' name 'CoGetInstanceFromIStorage';

{$endif read_interface}

{
  $Log: objbase.inc,v $
  Revision 1.1  2005/03/28 15:09:35  peter
  new winunits packages

  Revision 1.4  2005/02/14 17:13:32  peter
    * truncate log

}
