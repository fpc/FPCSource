{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains base definition for the Win32 API
    Copyright (c) 1993,97 by Florian Klaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
    This file is generated using h2pas written by Florian Klaempfl,
    but some modifications are done.

    The C header file was base.h of the
    GNU Windows32 API Library Version 0.1.2

 ****************************************************************************

  FK     Florian Klaempfl
  +      added
  -      removed
  *      modified

  History (for more informations look to the rtl log file):
      18th november 1997 version:
         + started (FK)

  ToDo:
      o unicode strings

  Not recommended to use:
      #define HIBYTE(w)   ((BYTE) (((WORD) (w) >> 8) & 0xFF))
      #define HIWORD(l)   ((WORD) (((DWORD) (l) >> 16) & 0xFFFF))
      #define LOBYTE(w)   ((BYTE) (w))
      #define LOWORD(l)   ((WORD) (l))

  Unimplemented:
      #define FORWARD_WM_NOTIFY(hwnd, idFrom, pnmhdr, fn) \
          (void)(fn)((hwnd), WM_NOTIFY, (WPARAM)(int)(id), \
          (LPARAM)(NMHDR FAR*)(pnmhdr))

      #define HANDLE_WM_NOTIFY(hwnd, wParam, lParam, fn) \
          (fn)((hwnd), (int)(wParam), (NMHDR FAR*)(lParam))

      #define MAKELANGID(p, s) ((((WORD) (s)) << 10) | (WORD) (p))
      #define PRIMARYLANGID(lgid)    ((WORD  )(lgid) & 0x3ff)
      #define SUBLANGID(lgid)        ((WORD  )(lgid) >> 10)

      #define LANGIDFROMLCID(lcid)   ((WORD) (lcid))
      #define SORTIDFROMLCID(lcid) \
                ((WORD  )((((DWORD)(lcid)) & 0x000FFFFF) >> 16))
      #define MAKELCID(lgid, srtid)  \
          ((DWORD)((((DWORD)((WORD)(srtid))) << 16) |  \
                  ((DWORD)((WORD)(lgid)))))
      #define MAKELPARAM(l, h)   ((LPARAM) MAKELONG(l, h))
      #define MAKELRESULT(l, h)   ((LRESULT) MAKELONG(l, h))
      #define MAKEPOINTS(l)   (*((POINTS FAR *) & (l)))
      #define MAKEROP4(fore,back) \
                (DWORD)((((back) << 8) & 0xFF000000) | (fore))
      #define MAKEWPARAM(l, h)   ((WPARAM) MAKELONG(l, h))

      #ifndef max
      #define max(a, b)  (((a) > (b)) ? (a) : (b))
      #endif

      #ifndef min
      #define min(a, b)  (((a) < (b)) ? (a) : (b))
      #endif

      #define PALETTEINDEX(i) \
          ((COLORREF) (0x01000000 | (DWORD) (WORD) (i)))
      #define PALETTERGB(r, g, b)  (0x02000000 | RGB(r, g, b))
      #define POINTSTOPOINT(pt, pts)
            (pt).x = (SHORT) LOWORD(pts);
            (pt).y = (SHORT) HIWORD(pts);
      #define POINTTOPOINTS(pt)
          (MAKELONG((short) ((pt).x), (short) ((pt).y)))


      #define SECURITY_NULL_SID_AUTHORITY     0,0,0,0,0,0
      #define SECURITY_WORLD_SID_AUTHORITY    0,0,0,0,0,1
      #define SECURITY_LOCAL_SID_AUTHORITY    0,0,0,0,0,2
      #define SECURITY_CREATOR_SID_AUTHORITY  0,0,0,0,0,3
      #define SECURITY_NON_UNIQUE_AUTHORITY   0,0,0,0,0,4
      #define SECURITY_NT_AUTHORITY           0,0,0,0,0,5

 ****************************************************************************}

unit base;

  interface

    type
       ATOM = word;
       { Changed from BOOL to WINBOOL to avoid Objective-C conflict}
       WINBOOL = longint;
       BOOL = longint;
       CALTYPE = cardinal;
       CALID = cardinal;
       CCHAR = char;
       WCHAR = word;
       COLORREF = cardinal;
       DWORD = cardinal;
       THandle = cardinal;

       DWORDLONG = double;
       PDWORDLONG = ^DWORDLONG;
       FLOAT = single;

       HANDLE = THandle;
       HACCEL = HANDLE;
       HBITMAP = HANDLE;
       HBRUSH = HANDLE;
       HCOLORSPACE = HANDLE;
       HCONV = HANDLE;
       HCONVLIST = HANDLE;
       HCURSOR = HANDLE;
       HDC = HANDLE;
       HDDEDATA = HANDLE;
       HDESK = HANDLE;
       HDWP = HANDLE;
       HENHMETAFILE = HANDLE;
       HFILE = longint;
       HFONT = HANDLE;
       HGDIOBJ = HANDLE;
       HGLOBAL = HANDLE;
       HGLRC = HANDLE;
       HHOOK = HANDLE;
       HICON = HANDLE;
       HIMAGELIST = HANDLE;
       HINSTANCE = HANDLE;
       HKEY = HANDLE;
       PHKEY = ^HANDLE;
       HKL = HANDLE;
       HLOCAL = HANDLE;
       HMENU = HANDLE;
       HMETAFILE = HANDLE;
       HMODULE = HANDLE;
       HPALETTE = HANDLE;
       HPEN = HANDLE;
       HRASCONN = HANDLE;
       HRESULT = longint;
       HRGN = HANDLE;
       HRSRC = HANDLE;
       HSZ = HANDLE;
       HWINSTA = HANDLE;
       HWND = HANDLE;
       INT = longint;
       LANGID = word;
       LCID = DWORD;
       LCTYPE = DWORD;
       LONG = longint;
       LONGLONG = double;
       PLONGLONG = ^double;
       LP = ^word;
       LPARAM = longint;
       LPBOOL = ^WINBOOL;
       LPBYTE = ^BYTE;
       LPCCH = ^CHAR;
       LPCH = ^CHAR;
       LPCOLORREF = ^COLORREF;
       LPCSTR = pchar;

{$ifdef UNICODE}
       LPCTSTR = ^word;
{$else UNICODE}
       LPCTSTR = ^char;
{$endif UNICODE}

       LPCWCH = ^word;
       LPCWSTR = ^word;
       LPDWORD = ^DWORD;
       LPHANDLE = ^HANDLE;
       LPINT = ^longint;
       LPLONG = ^longint;
       LPSTR = ^char;
{$ifdef UNICODE}
       LPTCH = ^word;
       LPTSTR = ^word;
{$else UNICODE}
       LPTCH = ^char;
       LPTSTR = ^char;
{$endif UNICODE}
       LRESULT = longint;
       LPVOID = pointer;
       LPCVOID = pointer;
       LPWCH = ^word;
       LPWORD = ^word;
       LPWSTR = ^word;
       NWPSTR = ^word;
       PWINBOOL = ^WINBOOL;
       PBOOLEAN = ^BYTE;
       PBYTE = ^BYTE;
       PCCH = ^CHAR;
       PCH = ^CHAR;
       PCHAR = ^CHAR;
       PCSTR = ^char;
       PCWCH = ^word;
       PCWSTR = ^word;
       PDWORD = ^DWORD;
       PFLOAT = ^float;
       PHANDLE = ^HANDLE;
       PINT = ^longint;
       PLONG = ^longint;
       PSHORT = ^integer;
       PSTR = ^char;
       PSZ = ^char;
{$ifdef UNICODE}
       PTBYTE = ^word;
       PTCH = ^word;
       PTCHAR = ^word;
       PTSTR = ^word;
{$else UNICODE}
       PTBYTE = ^char;
       PTCH = ^char;
       PTCHAR = ^char;
       PTSTR = ^char;
{$endif UNICODE}
       PUCHAR = ^char;
       PUINT = ^cardinal;
       PULONG = ^cardinal;
       PUSHORT = ^word;
       PVOID = pointer;
       PWCH = ^word;
       PWCHAR = ^word;
       PWORD = ^word;
       SC_HANDLE = HANDLE;
       SC_LOCK = LPVOID;
       LPSC_HANDLE = ^SC_HANDLE;
       SERVICE_STATUS_HANDLE = DWORD;
       SHORT = integer;
{$ifdef UNICODE}
       TBYTE = word;
       TCHAR = word;
       BCHAR = word;
{$else UNICODE}
       TBYTE = char;
       TCHAR = char;
       BCHAR = BYTE;
{$endif UNICODE}
       UCHAR = char;
       UINT = cardinal;
       ULONG = cardinal;
       USHORT = word;
       WPARAM = longint;

       ACL_INFORMATION_CLASS = (
          AclRevisionInformation:=1,
          AclSizeInformation);

       MEDIA_TYPE = (
         Unknown,
         F5_1Pt2_512,
         F3_1Pt44_512,
         F3_2Pt88_512,
         F3_20Pt8_512,
         F3_720_512,
         F5_360_512,
         F5_320_512,
         F5_320_1024,
         F5_180_512,
         F5_160_512,
         RemovableMedia,
         FixedMedia);

    const
       RASCS_DONE = $2000;
       RASCS_PAUSED = $1000;

    type
       RASCONNSTATE = (
         RASCS_OpenPort,
         RASCS_PortOpened,
         RASCS_ConnectDevice,
         RASCS_DeviceConnected,
         RASCS_AllDevicesConnected,
         RASCS_Authenticate,
         RASCS_AuthNotify,
         RASCS_AuthRetry,
         RASCS_AuthCallback,
         RASCS_AuthChangePassword,
         RASCS_AuthProject,
         RASCS_AuthLinkSpeed,
         RASCS_AuthAck,
         RASCS_ReAuthenticate,
         RASCS_Authenticated,
         RASCS_PrepareForCallback,
         RASCS_WaitForModemReset,
         RASCS_WaitForCallback,
         RASCS_Projected,
         RASCS_StartAuthentication,
         RASCS_CallbackComplete,
         RASCS_LogonNetwork,
         RASCS_Interactive:=RASCS_PAUSED,
         RASCS_RetryAuthentication,
         RASCS_CallbackSetByCaller,
         RASCS_PasswordExpired,

         RASCS_Connected:=RASCS_DONE,
         RASCS_Disconnected);

       RASPROJECTION = (
         RASP_Amb:=$10000,
         RASP_PppNbf:=$803F,
         RASP_PppIpx:=$802B,
         RASP_PppIp:=$8021);

       SECURITY_IMPERSONATION_LEVEL = (
         SecurityAnonymous,
         SecurityIdentification,
         SecurityImpersonation,
         SecurityDelegation);

       SID_NAME_USE = (
         SidTypeUser:=1,
         SidTypeGroup,
         SidTypeDomain,
         SidTypeAlias,
         SidTypeWellKnownGroup,
         SidTypeDeletedAccount,
         SidTypeInvalid,
         SidTypeUnknown);

       PSID_NAME_USE = ^SID_NAME_USE;

       TOKEN_INFORMATION_CLASS = (
         TokenUser:=1,
         TokenGroups,
         TokenPrivileges,
         TokenOwner,
         TokenPrimaryGroup,
         TokenDefaultDacl,
         TokenSource,
         TokenType,
         TokenImpersonationLevel,
         TokenStatistics);

       TOKEN_TYPE = (
         TokenPrimary:=1,
         TokenImpersonation);

       { Callbacks }
       BFFCALLBACK = function(_para1:HWND; _para2:UINT; _para3:LPARAM; _para:LPARAM):longint;
       LPCCHOOKPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       LPCFHOOKPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       PTHREAD_START_ROUTINE = function(_para1:LPVOID):DWORD;
       LPTHREAD_START_ROUTINE = PTHREAD_START_ROUTINE;
       EDITSTREAMCALLBACK = function(_para1:DWORD; _para2:LPBYTE; _para3:LONG; _para4:LONG):DWORD;
       LPFRHOOKPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       LPOFNHOOKPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       LPPRINTHOOKPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       LPSETUPHOOKPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       DLGPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):WINBOOL;
       PFNPROPSHEETCALLBACK = function(_para1:HWND; _para2:UINT; _para3:LPARAM):longint;
       LPSERVICE_MAIN_FUNCTION = procedure(_para1:DWORD; _para2:LPTSTR);
       PFNTVCOMPARE = function(_para1:LPARAM; _para2:LPARAM; _para3:LPARAM):longint;
       WNDPROC = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):LRESULT;
       FARPROC = function:longint;
       PROC = FARPROC;
       ENUMRESTYPEPROC = function(_para1:HANDLE; _para2:LPTSTR; _para3:LONG):WINBOOL;
       ENUMRESNAMEPROC = function(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; _para4:LONG):WINBOOL;
       ENUMRESLANGPROC = function(_para1:HANDLE; _para2:LPCTSTR; _para3:LPCTSTR; _para4:WORD; _para5:LONG):WINBOOL;
       DESKTOPENUMPROC = FARPROC;
       ENUMWINDOWSPROC = function(_para1:HWND; _para2:LPARAM):WINBOOL;
       ENUMWINDOWSTATIONPROC = function(_para1:LPTSTR; _para2:LPARAM):WINBOOL;
       SENDASYNCPROC = procedure(_para1:HWND; _para2:UINT; _para3:DWORD; _para4:LRESULT);
       TIMERPROC = procedure(_para1:HWND; _para2:UINT; _para3:UINT; _para4:DWORD);
       GRAYSTRINGPROC = FARPROC;
       DRAWSTATEPROC = function(_para1:HDC; _para2:LPARAM; _para3:WPARAM; _para4:longint; _para5:longint):WINBOOL;
       PROPENUMPROCEX = function(_para1:HWND; _para2:LPCTSTR; _para3:HANDLE; _para4:DWORD):WINBOOL;
       PROPENUMPROC = function(_para1:HWND; _para2:LPCTSTR; _para3:HANDLE):WINBOOL;
       HOOKPROC = function(_para1:longint; _para2:WPARAM; _para3:LPARAM):LRESULT;
       ENUMOBJECTSPROC = procedure(_para1:LPVOID; _para2:LPARAM);
       LINEDDAPROC = procedure(_para1:longint; _para2:longint; _para3:LPARAM);
       ABORTPROC = function(_para1:HDC; _para2:longint):WINBOOL;
       LPPAGEPAINTHOOK = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       LPPAGESETUPHOOK = function(_para1:HWND; _para2:UINT; _para3:WPARAM; _para4:LPARAM):UINT;
       ICMENUMPROC = function(_para1:LPTSTR; _para2:LPARAM):longint;
       EDITWORDBREAKPROCEX = function(_para1:pchar; _para2:LONG; _para3:BYTE; _para4:INT):LONG;
       PFNLVCOMPARE = function(_para1:LPARAM; _para2:LPARAM; _para3:LPARAM):longint;
       LOCALE_ENUMPROC = function(_para1:LPTSTR):WINBOOL;
       CODEPAGE_ENUMPROC = function(_para1:LPTSTR):WINBOOL;
       DATEFMT_ENUMPROC = function(_para1:LPTSTR):WINBOOL;
       TIMEFMT_ENUMPROC = function(_para1:LPTSTR):WINBOOL;
       CALINFO_ENUMPROC = function(_para1:LPTSTR):WINBOOL;
       PHANDLER_ROUTINE = function(_para1:DWORD):WINBOOL;
       LPHANDLER_FUNCTION = function(_para1:DWORD):WINBOOL;
       PFNGETPROFILEPATH = function(_para1:LPCTSTR; _para2:LPSTR; _para3:UINT):UINT;
       PFNRECONCILEPROFILE = function(_para1:LPCTSTR; _para2:LPCTSTR; _para3:DWORD):UINT;
       PFNPROCESSPOLICIES = function(_para1:HWND; _para2:LPCTSTR; _para3:LPCTSTR; _para4:LPCTSTR; _para5:DWORD):WINBOOL;

    const
{$ifdef UNICODE}
       { !!!!! how do unicode support? }
       SERVICES_ACTIVE_DATABASE       = SERVICES_ACTIVE_DATABASEW;
       SERVICES_FAILED_DATABASE       = SERVICES_FAILED_DATABASEW;
       SC_GROUP_IDENTIFIER            = SC_GROUP_IDENTIFIERW;
{$else UNICODE}
       SE_CREATE_TOKEN_NAME              = 'SeCreateTokenPrivilege';
       SE_ASSIGNPRIMARYTOKEN_NAME        = 'SeAssignPrimaryTokenPrivilege';
       SE_LOCK_MEMORY_NAME               = 'SeLockMemoryPrivilege';
       SE_INCREASE_QUOTA_NAME            = 'SeIncreaseQuotaPrivilege';
       SE_UNSOLICITED_INPUT_NAME         = 'SeUnsolicitedInputPrivilege';
       SE_MACHINE_ACCOUNT_NAME           = 'SeMachineAccountPrivilege';
       SE_TCB_NAME                       = 'SeTcbPrivilege';
       SE_SECURITY_NAME                  = 'SeSecurityPrivilege';
       SE_TAKE_OWNERSHIP_NAME            = 'SeTakeOwnershipPrivilege';
       SE_LOAD_DRIVER_NAME               = 'SeLoadDriverPrivilege';
       SE_SYSTEM_PROFILE_NAME            = 'SeSystemProfilePrivilege';
       SE_SYSTEMTIME_NAME                = 'SeSystemtimePrivilege';
       SE_PROF_SINGLE_PROCESS_NAME       = 'SeProfileSingleProcessPrivilege';
       SE_INC_BASE_PRIORITY_NAME         = 'SeIncreaseBasePriorityPrivilege';
       SE_CREATE_PAGEFILE_NAME           = 'SeCreatePagefilePrivilege';
       SE_CREATE_PERMANENT_NAME          = 'SeCreatePermanentPrivilege';
       SE_BACKUP_NAME                    = 'SeBackupPrivilege';
       SE_RESTORE_NAME                   = 'SeRestorePrivilege';
       SE_SHUTDOWN_NAME                  = 'SeShutdownPrivilege';
       SE_DEBUG_NAME                     = 'SeDebugPrivilege';
       SE_AUDIT_NAME                     = 'SeAuditPrivilege';
       SE_SYSTEM_ENVIRONMENT_NAME        = 'SeSystemEnvironmentPrivilege';
       SE_CHANGE_NOTIFY_NAME             = 'SeChangeNotifyPrivilege';
       SE_REMOTE_SHUTDOWN_NAME           = 'SeRemoteShutdownPrivilege';

       SERVICES_ACTIVE_DATABASEW      = 'ServicesActive';
       SERVICES_FAILED_DATABASEW      = 'ServicesFailed';
       SERVICES_ACTIVE_DATABASEA      = 'ServicesActive';
       SERVICES_FAILED_DATABASEA      = 'ServicesFailed';
       SC_GROUP_IDENTIFIERW           = '+';
       SC_GROUP_IDENTIFIERA           = '+';
       SERVICES_ACTIVE_DATABASE       = SERVICES_ACTIVE_DATABASEA;
       SERVICES_FAILED_DATABASE       = SERVICES_FAILED_DATABASEA;
       SC_GROUP_IDENTIFIER            = SC_GROUP_IDENTIFIERA;
{$endif UNICODE}

    function GetRValue(rgb : longint) : byte;
    function GetGValue(rgb : longint) : byte;
    function GetBValue(rgb : longint) : byte;
    function RGB(r,g,b : byte) : longint;
    function MAKELONG(a,b : word) : LONG;
    function MAKEWORD(a,b : BYTE) : WORD;
    function INDEXTOOVERLAYMASK(i : longint) : longint;
    function INDEXTOSTATEIMAGEMASK(i : longint) : longint;
    function MAKEINTATOM(i : word) : LPTSTR;
    function MAKEINTRESOURCE(i : word) : LPTSTR;

  implementation

    function GetBValue(rgb : longint) : byte;

      begin
         GetBValue:=(rgb shr 16) and $ff;
      end;

    function GetGValue(rgb : longint) : byte;

      begin
         GetGValue:=(rgb shr 8) and $ff;
      end;

    function GetRValue(rgb : longint) : byte;

      begin
         GetRValue:=rgb and $ff;
      end;

    function RGB(r,g,b : byte) : longint;

      begin
         RGB:=r or (g shl 8) or (b shl 16);
      end;

    function MAKELONG(a,b : WORD) : LONG;

      begin
         MAKELONG:=(a shl 16) or b;
      end;

    function MAKEWORD(a,b : BYTE) : WORD;

      begin
         MAKEWORD:=(a shl 8) or b;
      end;

    function INDEXTOOVERLAYMASK(i : longint) : longint;

      begin
         INDEXTOOVERLAYMASK:=i shl 8;
      end;

    function INDEXTOSTATEIMAGEMASK(i : longint) : longint;

      begin
         INDEXTOSTATEIMAGEMASK:=i shl 12;
      end;

    function MAKEINTATOM(i : word) : LPTSTR;

      begin
         MAKEINTATOM:=LPTSTR(DWORD(i));
      end;

    function MAKEINTRESOURCE(i : word) : LPTSTR;

      begin
         MAKEINTRESOURCE:=LPTSTR(DWORD(i));
      end;

end.

{
  $Log$
  Revision 1.4  1998-06-10 10:39:11  peter
    * working w32 rtl

}
