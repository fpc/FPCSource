{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file defines type names as they are used by Delphi

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type
   TRTLCriticalSection = CRITICAL_SECTION;
   PRTLCriticalSection = PCRITICAL_SECTION;

   PIID = PGUID;
   TIID = TGUID;
   THANDLE = HANDLE;

   PSmallRect = ^TSmallRect;
   TSmallRect = SMALL_RECT;

   PCharInfo = ^TCharInfo;
   TCharInfo = _CHAR_INFO;

   TFarProc = FARPROC;

  procedure InitializeCriticalSection(var CriticalSection : TRTLCriticalSection);
  procedure EnterCriticalSection(var CriticalSection : TRTLCriticalSection);
  procedure LeaveCriticalSection(var CriticalSection : TRTLCriticalSection);
  procedure DeleteCriticalSection(var CriticalSection : TRTLCriticalSection);


  const
     { HRESULT codes, delphilike }
     S_OK    = $00000000;
     S_FALSE = $00000001;
     NOERROR = 0;
     E_UNEXPECTED = DWORD($8000FFFF);
     E_NOTIMPL = DWORD($80004001);
     E_OUTOFMEMORY = DWORD($8007000E);
     E_INVALIDARG = DWORD($80070057);
     E_NOINTERFACE = DWORD($80004002);
     E_POINTER = DWORD($80004003);
     E_HANDLE = DWORD($80070006);
     E_ABORT = DWORD($80004004);
     E_FAIL = DWORD($80004005);
     E_ACCESSDENIED = DWORD($80070005);
     E_PENDING = DWORD($8000000A);
     CO_E_INIT_TLS = DWORD($80004006);

     CO_E_INIT_MEMORY_ALLOCATOR = DWORD($80004008);
     CO_E_INIT_CLASS_CACHE = DWORD($80004009);
     CO_E_INIT_RPC_CHANNEL = DWORD($8000400A);
     CO_E_INIT_TLS_SET_CHANNEL_CONTROL = DWORD($8000400B);
     CO_E_INIT_TLS_CHANNEL_CONTROL = DWORD($8000400C);
     CO_E_INIT_UNACCEPTED_USER_ALLOCATOR = DWORD($8000400D);
     CO_E_INIT_SCM_MUTEX_EXISTS = DWORD($8000400E);
     CO_E_INIT_SCM_FILE_MAPPING_EXISTS = DWORD($8000400F);
     CO_E_INIT_SCM_MAP_VIEW_OF_FILE = DWORD($80004010);
     CO_E_INIT_SCM_EXEC_FAILURE = DWORD($80004011);
     CO_E_INIT_ONLY_SINGLE_THREADED = DWORD($80004012);
     CO_E_CANT_REMOTE = DWORD($80004013);
     CO_E_BAD_SERVER_NAME = DWORD($80004014);
     CO_E_WRONG_SERVER_IDENTITY = DWORD($80004015);
     CO_E_OLE1DDE_DISABLED = DWORD($80004016);
     CO_E_RUNAS_SYNTAX = DWORD($80004017);
     CO_E_CREATEPROCESS_FAILURE = DWORD($80004018);
     CO_E_RUNAS_CREATEPROCESS_FAILURE = DWORD($80004019);
     CO_E_RUNAS_LOGON_FAILURE = DWORD($8000401A);
     CO_E_LAUNCH_PERMSSION_DENIED = DWORD($8000401B);
     CO_E_START_SERVICE_FAILURE = DWORD($8000401C);
     CO_E_REMOTE_COMMUNICATION_FAILURE = DWORD($8000401D);
     CO_E_SERVER_START_TIMEOUT = DWORD($8000401E);
     CO_E_CLSREG_INCONSISTENT = DWORD($8000401F);
     CO_E_IIDREG_INCONSISTENT = DWORD($80004020);
     CO_E_NOT_SUPPORTED = DWORD($80004021);
     CO_E_FIRST = DWORD($800401F0);
     CO_E_LAST  = DWORD($800401FF);
     CO_S_FIRST = $401F0;
     CO_E_NOTINITIALIZED = DWORD($800401F0);
     CO_E_ALREADYINITIALIZED = DWORD($800401F1);
     CO_E_CANTDETERMINECLASS = DWORD($800401F2);
     CO_E_CLASSSTRING = DWORD($800401F3);
     CO_E_IIDSTRING = DWORD($800401F4);
     CO_E_APPNOTFOUND = DWORD($800401F5);
     CO_E_APPSINGLEUSE = DWORD($800401F6);
     CO_E_ERRORINAPP = DWORD($800401F7);
     CO_E_DLLNOTFOUND = DWORD($800401F8);
     CO_E_ERRORINDLL = DWORD($800401F9);
     CO_E_WRONGOSFORAPP = DWORD($800401FA);
     CO_E_OBJNOTREG = DWORD($800401FB);
     CO_E_OBJISREG = DWORD($800401FC);
     CO_E_OBJNOTCONNECTED = DWORD($800401FD);
     CO_E_APPDIDNTREG = DWORD($800401FE);
     CO_E_RELEASED = DWORD($800401FF);

{MvdV: JCL templates}
function GetTimeZoneInformation(var TimeZoneInformation:TTimeZoneInformation):DWORD;
function FileTimeToLocalFileTime(const lpFileTime:TFILETIME; var LocalFileTime :TFILETIME):WINBOOL;
function FileTimeToSystemTime(const lpFileTime:TFILETIME; var lpSystemTime:TSYSTEMTIME):WINBOOL;
function DosDateTimeToFileTime(wFatDate:WORD; wFatTime:WORD; var lpFileTime:tFILETIME):WINBOOL;
function SystemTimeToFileTime(const lSystemTime:tSYSTEMTIME; var FileTime:TFILETIME):WINBOOL;


{
  $Log$
  Revision 1.4  2000-03-19 20:29:06  marco
   * some stuff for JCL

  Revision 1.3  2000/02/09 16:59:36  peter
    * truncated log

  Revision 1.2  2000/01/07 16:41:56  daniel
    * copyright 2000

  Revision 1.1  1999/09/16 13:38:24  peter
    * windows unit include moved to wininc/

}