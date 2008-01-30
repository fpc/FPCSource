{
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1999-2002 by Marco van de Voort,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ************************************************************************}

{ leave out unused functions so the unit can be used on win2000 as well }
{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}

{$PACKRECORDS C}
{$calling stdcall}
{$mode objfpc}

Unit ShellApi;

//+-------------------------------------------------------------------------
//
//  Microsoft Windows
//  Copyright (c) Microsoft Corporation. All rights reserved.
//
//  File: shellapi.h
//
//  Header translation by Marco van de Voort for Free Pascal Platform
//  SDK dl'ed January 2002
//
//--------------------------------------------------------------------------


Interface

Uses Windows;
  {
    shellapi.h -  SHELL.DLL functions, types, and definitions
    Copyright (c) Microsoft Corporation. All rights reserved.             }

  Type   
     HDROP    = THandle;
     PHIcon   = ^HIcon;

     STARTUPINFOW = record  // a guess. Omission should get fixed in Windows.
          cb : DWORD;
          lpReserved : LPTSTR;
          lpDesktop : LPTSTR;
          lpTitle : LPTSTR;
          dwX : DWORD;
          dwY : DWORD;
          dwXSize : DWORD;
          dwYSize : DWORD;
          dwXCountChars : DWORD;
          dwYCountChars : DWORD;
          dwFillAttribute : DWORD;
          dwFlags : DWORD;
          wShowWindow : WORD;
          cbReserved2 : WORD;
          lpReserved2 : LPBYTE;
          hStdInput : HANDLE;
          hStdOutput : HANDLE;
          hStdError : HANDLE;
       end;
     LPSTARTUPINFOW = ^STARTUPINFOW;
     _STARTUPINFOW = STARTUPINFOW;
     TSTARTUPINFOW = STARTUPINFOW;
     PSTARTUPINFOW = ^STARTUPINFOW;


{unicode}
Function DragQueryFileA(arg1 : HDROP; arg2 : UINT;arg3 : LPSTR ; arg4 : UINT):UINT;external 'shell32.dll' name 'DragQueryFileA';
Function DragQueryFileW(arg1 : HDROP; arg2 : UINT;arg3 : LPWSTR; arg4 : UINT):UINT;external 'shell32.dll' name 'DragQueryFileW';
Function DragQueryFile(arg1 : HDROP; arg2 : UINT;arg3 : LPSTR ; arg4 : UINT):UINT;external 'shell32.dll' name 'DragQueryFileA';
Function DragQueryFile(arg1 : HDROP; arg2 : UINT;arg3 : LPWSTR; arg4 : UINT):UINT;external 'shell32.dll' name 'DragQueryFileW';

Function DragQueryPoint(arg1 : HDROP; arg2 :LPPOINT):BOOL; external 'shell32.dll' name 'DragQueryPoint';
Procedure DragFinish(arg1 : HDROP);                     external 'shell32.dll' name 'DragFinish';
Procedure DragAcceptFiles(hwnd : HWND;arg2: BOOL);      external 'shell32.dll' name 'DragAcceptFiles';

Function ShellExecuteA(HWND: hwnd;lpOperation : LPCSTR ; lpFile : LPCSTR ; lpParameters : LPCSTR; lpDirectory:  LPCSTR; nShowCmd:LONGINT):HInst; external 'shell32.dll' name 'ShellExecuteA';
Function ShellExecuteW(hwnd: HWND;lpOperation : LPCWSTR ; lpFile : LPCWSTR ; lpParameters : LPCWSTR; lpDirectory:  LPCWSTR; nShowCmd:LONGINT):HInst; external 'shell32.dll' name 'ShellExecuteW';
Function ShellExecute(HWND: hwnd;lpOperation : LPCSTR ; lpFile : LPCSTR ; lpParameters : LPCSTR; lpDirectory:  LPCSTR; nShowCmd:LONGINT):HInst; external 'shell32.dll' name 'ShellExecuteA';
Function ShellExecute(hwnd: HWND;lpOperation : LPCWSTR ; lpFile : LPCWSTR ; lpParameters : LPCWSTR; lpDirectory:  LPCWSTR; nShowCmd:LONGINT):HInst; external 'shell32.dll' name 'ShellExecuteW';

Function FindExecutableA(lpFile : LPCSTR ;lpDirectory : LPCSTR ; lpResult : LPSTR):HInst;external 'shell32.dll' name 'FindExecutableA';
Function FindExecutableW(lpFile : LPCWSTR;lpDirectory : LPCWSTR; lpResult : LPWSTR):HInst;external 'shell32.dll' name 'FindExecutableW';
Function FindExecutable(lpFile : LPCSTR ;lpDirectory : LPCSTR ; lpResult : LPSTR):HInst;external 'shell32.dll' name 'FindExecutableA';
Function FindExecutable(lpFile : LPCWSTR;lpDirectory : LPCWSTR; lpResult : LPWSTR):HInst;external 'shell32.dll' name 'FindExecutableW';

Function CommandLineToArgvW(lpCmdLine : LPCWSTR;pNumArgs : plongint):pLPWSTR;external 'shell32.dll' name 'CommandLineToArgvW';

Function ShellAboutA(HWND: hWnd; szApp : LPCSTR; szOtherStuff : LPCSTR; HICON : hIcon):Longint; external 'shell32.dll' name 'ShellAboutA';
Function ShellAboutW(HWND: hWnd; szApp : LPCWSTR; szOtherStuff : LPCWSTR; HICON : hIcon):Longint; external 'shell32.dll' name 'ShellAboutW';
Function ShellAbout(HWND: hWnd; szApp : LPCSTR; szOtherStuff : LPCSTR; HICON : hIcon):Longint; external 'shell32.dll' name 'ShellAboutA';
Function ShellAbout(HWND: hWnd; szApp : LPCWSTR; szOtherStuff : LPCWSTR; HICON : hIcon):Longint; external 'shell32.dll' name 'ShellAboutW';

Function DuplicateIcon(hinst : HINST; HICON: hIcon):HIcon; external 'shell32.dll' name 'DuplicateIcon';

Function  ExtractAssociatedIconA(hInst : HINST; lpIconPath : LPSTR; lpiIcon : LPWORD):HICON;external 'shell32.dll' name 'ExtractAssociatedIconA';
Function  ExtractAssociatedIconW(hInst : HINST; lpIconPath : LPWSTR; lpiIcon : LPWORD):HICON;external 'shell32.dll' name 'ExtractAssociatedIconW';
Function  ExtractAssociatedIcon(hInst : HINST; lpIconPath : LPSTR; lpiIcon : LPWORD):HICON;external 'shell32.dll' name 'ExtractAssociatedIconA';
Function  ExtractAssociatedIcon(hInst : HINST; lpIconPath : LPWSTR; lpiIcon : LPWORD):HICON;external 'shell32.dll' name 'ExtractAssociatedIconW';

Function ExtractIconA(hInst: HINST; lpszExeFileName :LPCSTR ; nIconIndex : UINT):HICON;external 'shell32.dll' name 'ExtractIconA';
Function ExtractIconW(hInst: HINST; lpszExeFileName :LPCWSTR ; nIconIndex : UINT):HICON;external 'shell32.dll' name 'ExtractIconW';

Function ExtractIcon(hInst: HINST; lpszExeFileName :LPCSTR ; nIconIndex : UINT):HICON;external 'shell32.dll' name 'ExtractIconA';
Function ExtractIcon(hInst: HINST; lpszExeFileName :LPCWSTR ; nIconIndex : UINT):HICON;external 'shell32.dll' name 'ExtractIconW';

// if(WINVER >= 0x0400)

Type
    { init with sizeof(DRAGINFO)  }

       _DRAGINFOA = Record
                     uSize       : UINT;
                     pt          : POINT;
                     fNC         : BOOL;
                     lpFileList  : LPSTR;
                     grfKeyState : DWORD;
                   end;
       DRAGINFOA   = _DRAGINFOA;
       TDRAGINFOA  = _DRAGINFOA;
       LPDRAGINFOA = ^_DRAGINFOA;

    { init with sizeof(DRAGINFO)  }

       _DRAGINFOW = Record
                     uSize       : UINT;
                     pt          : POINT;
                     fNC         : BOOL;
                     lpFileList  : LPWSTR;
                     grfKeyState : DWORD;
                    end;
       DRAGINFOW   = _DRAGINFOW;
       TDRAGINFOW  = _DRAGINFOW;
       LPDRAGINFOW = ^_DRAGINFOW;

{$ifdef UNICODE}
       DRAGINFO         = DRAGINFOW;
       TDRAGINFO        = DRAGINFOW;
       LPDRAGINFO       = LPDRAGINFOW;
{$else}
       DRAGINFO         = DRAGINFOA;
       TDRAGINFO        = DRAGINFOW;
       LPDRAGINFO       = LPDRAGINFOA;
{$endif}

Const
       ABM_NEW                  = $00000000;
       ABM_REMOVE               = $00000001;
       ABM_QUERYPOS             = $00000002;
       ABM_SETPOS               = $00000003;
       ABM_GETSTATE             = $00000004;
       ABM_GETTASKBARPOS        = $00000005;
       ABM_ACTIVATE             = $00000006;     { lParam == TRUE/FALSE means activate/deactivate }
       ABM_GETAUTOHIDEBAR       = $00000007;
       ABM_SETAUTOHIDEBAR       = $00000008;     { this can fail at any time.  MUST check the result }
                                                 { lParam = TRUE/FALSE  Set/Unset }
                                                 { uEdge = what edge }
       ABM_WINDOWPOSCHANGED     = $0000009;
       ABM_SETSTATE             = $0000000a;
       ABN_STATECHANGE          = $0000000;      { these are put in the wparam of callback messages }
       ABN_POSCHANGED           = $0000001;
       ABN_FULLSCREENAPP        = $0000002;
       ABN_WINDOWARRANGE        = $0000003;      { lParam == TRUE means hide }

       { flags for get state }
       ABS_AUTOHIDE             = $0000001;
       ABS_ALWAYSONTOP          = $0000002;
       ABE_LEFT                 = 0;
       ABE_TOP                  = 1;
       ABE_RIGHT                = 2;
       ABE_BOTTOM               = 3;


Type

       _AppBarData        = Record
                             cbSize             : DWORD;
                             hWnd               : HWND;
                             uCallbackMessage   : UINT;
                             uEdge              : UINT;
                             rc                 : RECT;
                             lParam             : LPARAM; { message specific }
                            end;
       APPBARDATA         = _AppBarData;
       TAPPBARDATA        = _AppBarData;
       PAPPBARDATA        = ^_AppBarData;


Function SHAppBarMessage(dwMessage : DWORD; pData : APPBARDATA):UINT_PTR;external 'shell32.dll' name 'SHAppBarMessage';

    //
    //  EndAppBar
    //

Function   DoEnvironmentSubstA(szString: LPSTR; cchString:UINT):DWORD;external 'shell32.dll' name 'DoEnvironmentSubstA';
Function   DoEnvironmentSubstW(szString: LPWSTR; cchString:UINT):DWORD;external 'shell32.dll' name 'DoEnvironmentSubstW';
Function   DoEnvironmentSubst(szString: LPSTR; cchString:UINT):DWORD;external 'shell32.dll' name 'DoEnvironmentSubstA';
Function   DoEnvironmentSubst(szString: LPWSTR; cchString:UINT):DWORD;external 'shell32.dll' name 'DoEnvironmentSubstW';

{Macro}
function EIRESID(x : longint) : longint;

Function ExtractIconExA(lpszFile : LPCSTR; nIconIndex:Longint; phiconLarge:pHICON; phiconSmall:pHIcon; nIcons:UINT):UINT;   external 'shell32.dll' name 'ExtractIconExA';
Function ExtractIconExW(lpszFile : LPCWSTR; nIconIndex:Longint; phiconLarge:pHICON; phiconSmall:pHIcon; nIcons:UINT):UINT;  external 'shell32.dll' name 'ExtractIconExW';
Function ExtractIconExA(lpszFile : LPCSTR; nIconIndex:Longint; var phiconLarge:HICON;var phiconSmall:HIcon; nIcons:UINT):UINT;   external 'shell32.dll' name 'ExtractIconExA';
Function ExtractIconExW(lpszFile : LPCWSTR; nIconIndex:Longint; var phiconLarge:HICON;var phiconSmall:HIcon; nIcons:UINT):UINT;  external 'shell32.dll' name 'ExtractIconExW';

Function ExtractIconEx (lpszFile : LPCSTR; nIconIndex:Longint; phiconLarge:pHICON; phiconSmall:pHIcon; nIcons:UINT):UINT; external 'shell32.dll' name 'ExtractIconExA';
Function ExtractIconEx (lpszFile : LPCWSTR; nIconIndex:Longint; phiconLarge:pHICON; phiconSmall:pHIcon; nIcons:UINT):UINT; external 'shell32.dll' name 'ExtractIconExW';
Function ExtractIconEx (lpszFile : LPCSTR; nIconIndex:Longint; var phiconLarge:HICON;var phiconSmall:HIcon; nIcons:UINT):UINT; external 'shell32.dll' name 'ExtractIconExA';
Function ExtractIconEx (lpszFile : LPCWSTR; nIconIndex:Longint; var phiconLarge:HICON;var phiconSmall:HIcon; nIcons:UINT):UINT; external 'shell32.dll' name 'ExtractIconExW';

//
// Shell File Operations
//

//ifndef FO_MOVE  //these need to be kept in sync with the ones in shlobj.h}
Const
       FO_MOVE                  = $0001;
       FO_COPY                  = $0002;
       FO_DELETE                = $0003;
       FO_RENAME                = $0004;
       FOF_MULTIDESTFILES       = $0001;
       FOF_CONFIRMMOUSE         = $0002;
       FOF_SILENT               = $0004;    { don't create progress/report }
       FOF_RENAMEONCOLLISION    = $0008;
       FOF_NOCONFIRMATION       = $0010;    { Don't prompt the user. }
       FOF_WANTMAPPINGHANDLE    = $0020;    { Fill in SHFILEOPSTRUCT.hNameMappings }
       FOF_ALLOWUNDO            = $0040;    { Must be freed using SHFreeNameMappings }
       FOF_FILESONLY            = $0080;    { on *.*, do only files }
       FOF_SIMPLEPROGRESS       = $0100;    { means don't show names of files }
       FOF_NOCONFIRMMKDIR       = $0200;    { don't confirm making any needed dirs }
       FOF_NOERRORUI            = $0400;    { don't put up error UI }
       FOF_NOCOPYSECURITYATTRIBS= $0800;    { dont copy NT file Security Attributes }
       FOF_NORECURSION          = $1000;    { don't recurse into directories. }

//if (_WIN32_IE >= 0x0500)
       FOF_NO_CONNECTED_ELEMENTS= $2000;    { don't operate on connected elements. }
       FOF_WANTNUKEWARNING      = $4000;    { during delete operation, warn if nuking instead of recycling (partially overrides FOF_NOCONFIRMATION) }
//endif

//if (_WIN32_WINNT >= 0x0501)
       FOF_NORECURSEREPARSE     = $8000;    { treat reparse points as objects, not containers }
//endif

Type
       FILEOP_FLAGS             = WORD;

Const
       PO_DELETE                = $0013;    { printer is being deleted }
       PO_RENAME                = $0014;    { printer is being renamed }

       PO_PORTCHANGE            = $0020;    { port this printer connected to is being changed }
                                            { if this id is set, the strings received by }
                                            { the copyhook are a doubly-null terminated }
                                            { list of strings.  The first is the printer }
                                            { name and the second is the printer port. }

       PO_REN_PORT              = $0034;    { PO_RENAME and PO_PORTCHANGE at same time. }

{ no POF_ flags currently defined }

Type

       PRINTEROP_FLAGS = WORD;
//endif}

    { FO_MOVE }
    { implicit parameters are: }
    {      if pFrom or pTo are unqualified names the current directories are }
    {      taken from the global current drive/directory settings managed }
    {      by Get/SetCurrentDrive/Directory }
    { }
    {      the global confirmation settings }
    { only used if FOF_SIMPLEPROGRESS }

Type

       _SHFILEOPSTRUCTA         = Record
                                   wnd               : HWND;
                                   wFunc             : UINT;
                                   pFrom             : LPCSTR;
                                   pTo               : LPCSTR;
                                   fFlags            : FILEOP_FLAGS;
                                   fAnyOperationsAborted : BOOL;
                                   hNameMappings     : LPVOID;
                                   lpszProgressTitle : LPCSTR;     { only used if FOF_SIMPLEPROGRESS }
                                  end;
       SHFILEOPSTRUCTA          = _SHFILEOPSTRUCTA;
       TSHFILEOPSTRUCTA         = _SHFILEOPSTRUCTA;
       LPSHFILEOPSTRUCTA        = ^_SHFILEOPSTRUCTA;


       _SHFILEOPSTRUCTW         = record
                                   wnd               : HWND;
                                   wFunc             : UINT;
                                   pFrom             : LPCWSTR;
                                   pTo               : LPCWSTR;
                                   fFlags            : FILEOP_FLAGS;
                                   fAnyOperationsAborted : BOOL;
                                   hNameMappings     : LPVOID;
                                   lpszProgressTitle : LPCWSTR;
                                  end;
       SHFILEOPSTRUCTW          = _SHFILEOPSTRUCTW;
       TSHFILEOPSTRUCTW         = _SHFILEOPSTRUCTW;
       LPSHFILEOPSTRUCTW        = ^_SHFILEOPSTRUCTW;
{$ifdef UNICODE}
       SHFILEOPSTRUCT           = SHFILEOPSTRUCTW;
       TSHFILEOPSTRUCT          = SHFILEOPSTRUCTW;
       LPSHFILEOPSTRUCT         = LPSHFILEOPSTRUCTW;
{$else}
       SHFILEOPSTRUCT           = SHFILEOPSTRUCTA;
       TSHFILEOPSTRUCT          = SHFILEOPSTRUCTA;
       LPSHFILEOPSTRUCT         = LPSHFILEOPSTRUCTA;
{$endif}

Function SHFileOperationA(lpFileOp:LPSHFILEOPSTRUCTA ):Longint;external 'shell32.dll' name 'SHFileOperationA';

Function SHFileOperationW(lpFileOp:LPSHFILEOPSTRUCTW ):Longint;external 'shell32.dll' name 'SHFileOperationW';

Function SHFileOperation(lpFileOp:LPSHFILEOPSTRUCTA ):Longint;external 'shell32.dll' name 'SHFileOperationA';
Function SHFileOperation(var lpFileOp:SHFILEOPSTRUCTA ):Longint;external 'shell32.dll' name 'SHFileOperationA';
Function SHFileOperation(lpFileOp:LPSHFILEOPSTRUCTW ):Longint;external 'shell32.dll' name 'SHFileOperationW';

Procedure SHFreeNameMappings(hNameMappings : THandle);external 'shell32.dll' name 'SHFreeNameMappings';

Type

       _SHNAMEMAPPINGA          = Record
                                   pszOldPath : LPSTR;
                                   pszNewPath : LPSTR;
                                   cchOldPath : longint;
                                   cchNewPath : longint;
                                  end;
       SHNAMEMAPPINGA           = _SHNAMEMAPPINGA;
       TSHNAMEMAPPINGA          = _SHNAMEMAPPINGA;
       LPSHNAMEMAPPINGA         = ^_SHNAMEMAPPINGA;

       _SHNAMEMAPPINGW          = Record
                                   pszOldPath : LPWSTR;
                                   pszNewPath : LPWSTR;
                                   cchOldPath : longint;
                                   cchNewPath : longint;
                                  end;
       SHNAMEMAPPINGW           = _SHNAMEMAPPINGW;
       TSHNAMEMAPPINGW          = _SHNAMEMAPPINGW;
       LPSHNAMEMAPPINGW         = ^_SHNAMEMAPPINGW;
{$ifndef UNICODE}
       SHNAMEMAPPING            = SHNAMEMAPPINGW;
       TSHNAMEMAPPING           = SHNAMEMAPPINGW;
       LPSHNAMEMAPPING          = LPSHNAMEMAPPINGW;
{$else}
       SHNAMEMAPPING            = SHNAMEMAPPINGA;
       TSHNAMEMAPPING           = SHNAMEMAPPINGA;
       LPSHNAMEMAPPING          = LPSHNAMEMAPPINGA;
{$endif}

    //
    // End Shell File Operations
    //
    //
    //  Begin ShellExecuteEx and family
    //


    { ShellExecute() and ShellExecuteEx() error codes  }
    { regular WinExec() codes  }


    const
       SE_ERR_FNF               = 2;    { file not found }
       SE_ERR_PNF               = 3;    { path not found }
       SE_ERR_ACCESSDENIED      = 5;    { access denied }
       SE_ERR_OOM               = 8;    { out of memory }
       SE_ERR_DLLNOTFOUND       = 32;
// endif   WINVER >= 0x0400

    { error values for ShellExecute() beyond the regular WinExec() codes  }
       SE_ERR_SHARE             = 26;
       SE_ERR_ASSOCINCOMPLETE   = 27;
       SE_ERR_DDETIMEOUT        = 28;
       SE_ERR_DDEFAIL           = 29;
       SE_ERR_DDEBUSY           = 30;
       SE_ERR_NOASSOC           = 31;

//if(WINVER >= 0x0400)}

    { Note CLASSKEY overrides CLASSNAME }

       SEE_MASK_CLASSNAME       = $00000001;
       SEE_MASK_CLASSKEY        = $00000003;
    { Note INVOKEIDLIST overrides IDLIST }
       SEE_MASK_IDLIST          = $00000004;
       SEE_MASK_INVOKEIDLIST    = $0000000c;
       SEE_MASK_ICON            = $00000010;
       SEE_MASK_HOTKEY          = $00000020;
       SEE_MASK_NOCLOSEPROCESS  = $00000040;
       SEE_MASK_CONNECTNETDRV   = $00000080;
       SEE_MASK_FLAG_DDEWAIT    = $00000100;
       SEE_MASK_DOENVSUBST      = $00000200;
       SEE_MASK_FLAG_NO_UI      = $00000400;
       SEE_MASK_UNICODE         = $00004000;
       SEE_MASK_NO_CONSOLE      = $00008000;
       SEE_MASK_ASYNCOK         = $00100000;
       SEE_MASK_HMONITOR        = $00200000;
//if (_WIN32_IE >= 0x0500)
       SEE_MASK_NOQUERYCLASSSTORE= $01000000;
       SEE_MASK_WAITFORINPUTIDLE= $02000000;
//endif  (_WIN32_IE >= 0x500)
//if (_WIN32_IE >= 0x0560)
       SEE_MASK_FLAG_LOG_USAGE  = $04000000;
//endif
    { (_WIN32_IE >= 0x560) }

    type

       _SHELLEXECUTEINFOA       = record
                                   cbSize : DWORD;
                                   fMask : ULONG;
                                   hwnd : HWND;
                                   lpVerb : LPCSTR;
                                   lpFile : LPCSTR;
                                   lpParameters : LPCSTR;
                                   lpDirectory : LPCSTR;
                                   nShow : longint;
                                   hInstApp : HINST;
                                   lpIDList : LPVOID;
                                   lpClass : LPCSTR;
                                   hkeyClass : HKEY;
                                   dwHotKey : DWORD;
                                   DUMMYUNIONNAME : record
                                                      case longint of
                                                       0 : ( hIcon : HANDLE );
                                                       1 : ( hMonitor : HANDLE );
                                                      end;
                                   hProcess : HANDLE;
                                  end;

       SHELLEXECUTEINFOA        = _SHELLEXECUTEINFOA;
       TSHELLEXECUTEINFOA       = _SHELLEXECUTEINFOA;
       LPSHELLEXECUTEINFOA      = ^_SHELLEXECUTEINFOA;


       _SHELLEXECUTEINFOW       = record
                                   cbSize : DWORD;
                                   fMask : ULONG;
                                   hwnd : HWND;
                                   lpVerb : lpcwstr;
                                   lpFile : lpcwstr;
                                   lpParameters : lpcwstr;
                                   lpDirectory : lpcwstr;
                                   nShow : longint;
                                   hInstApp : HINST;
                                   lpIDList : LPVOID;
                                   lpClass : LPCWSTR;
                                   hkeyClass : HKEY;
                                   dwHotKey : DWORD;
                                   DUMMYUNIONNAME : record
                                                      case longint of
                                                       0 : ( hIcon : HANDLE );
                                                       1 : ( hMonitor : HANDLE );
                                                      end;
                                   hProcess : HANDLE;
                                  end;

       SHELLEXECUTEINFOW        = _SHELLEXECUTEINFOW;
       TSHELLEXECUTEINFOW       = _SHELLEXECUTEINFOW;
       LPSHELLEXECUTEINFOW      = ^_SHELLEXECUTEINFOW;

{$ifdef UNICODE}
       SHELLEXECUTEINFO         = SHELLEXECUTEINFOW;
       TSHELLEXECUTEINFO        = SHELLEXECUTEINFOW;
       LPSHELLEXECUTEINFO       = LPSHELLEXECUTEINFOW;
{$else}
       SHELLEXECUTEINFO         = SHELLEXECUTEINFOA;
       TSHELLEXECUTEINFO        = SHELLEXECUTEINFOA;
       LPSHELLEXECUTEINFO       = LPSHELLEXECUTEINFOA;
{$endif}

Function ShellExecuteExA(lpExecInfo: LPSHELLEXECUTEINFOA):Bool;external 'shell32.dll' name 'ShellExecuteExA';
Function ShellExecuteExW(lpExecInfo: LPSHELLEXECUTEINFOW):Bool;external 'shell32.dll' name 'ShellExecuteExW';
Function ShellExecuteEx(lpExecInfo: LPSHELLEXECUTEINFOA):Bool;external 'shell32.dll' name 'ShellExecuteExA';
Function ShellExecuteEx(lpExecInfo: LPSHELLEXECUTEINFOW):Bool;external 'shell32.dll' name 'ShellExecuteExW';

Procedure WinExecErrorA(HWND : hwnd; error : Longint;lpstrFileName:LPCSTR; lpstrTitle:LPCSTR);   external 'shell32.dll' name 'WinExecErrorA';
Procedure WinExecErrorW(HWND : hwnd; error : Longint;lpstrFileName:LPCWSTR; lpstrTitle:LPCWSTR); external 'shell32.dll' name 'WinExecErrorW';
Procedure WinExecError(HWND : hwnd; error : Longint;lpstrFileName:LPCSTR; lpstrTitle:LPCSTR); external 'shell32.dll' name 'WinExecErrorA';
Procedure WinExecError(HWND : hwnd; error : Longint;lpstrFileName:LPCWSTR; lpstrTitle:LPCWSTR); external 'shell32.dll' name 'WinExecErrorW';

type

     _SHCREATEPROCESSINFOW      = record
                                   cbSize               : DWORD;
                                   fMask                : ULONG;
                                   hwnd                 : HWND;
                                   pszFile              : LPCWSTR;
                                   pszParameters        : LPCWSTR;
                                   pszCurrentDirectory  : LPCWSTR;
                             {in}  hUserToken           : HANDLE;
                             {in}  lpProcessAttributes  : LPSECURITY_ATTRIBUTES;
                             {in}  lpThreadAttributes   : LPSECURITY_ATTRIBUTES;
                             {in}  bInheritHandles      : BOOL;
                             {in}  dwCreationFlags      : DWORD;
                             {in}  lpStartupInfo        : LPSTARTUPINFOW;
                             {out} lpProcessInformation : LPPROCESS_INFORMATION;
                                  end;
     SHCREATEPROCESSINFOW       = _SHCREATEPROCESSINFOW;
     TSHCREATEPROCESSINFOW      = _SHCREATEPROCESSINFOW;
     PSHCREATEPROCESSINFOW      = ^_SHCREATEPROCESSINFOW;

Function SHCreateProcessAsUserW(pscpi : PSHCREATEPROCESSINFOW):Bool;external 'shell32.dll' name 'SHCreateProcessAsUserW';

    //
    //  End ShellExecuteEx and family }
    //

    //
    // RecycleBin
    //

    { struct for query recycle bin info }

Type
       _SHQUERYRBINFO           = record
                                   cbSize       : DWORD;
                                   i64Size      : int64;
                                   i64NumItems  : int64;
                                  end;
       SHQUERYRBINFO            = _SHQUERYRBINFO;
       TSHQUERYRBINFO           = _SHQUERYRBINFO;
       LPSHQUERYRBINFO          = ^_SHQUERYRBINFO;

       { flags for SHEmptyRecycleBin }

const
       SHERB_NOCONFIRMATION     = $00000001;
       SHERB_NOPROGRESSUI       = $00000002;
       SHERB_NOSOUND            = $00000004;

function SHQueryRecycleBinA(pszRootPath:LPCSTR; pSHQueryRBInfo:LPSHQUERYRBINFO):HRESULT;external 'shell32.dll' name 'SHQueryRecycleBinA';
function SHQueryRecycleBinW(pszRootPath:LPCWSTR; pSHQueryRBInfo:LPSHQUERYRBINFO):HRESULT;external 'shell32.dll' name 'SHQueryRecycleBinW';
function SHQueryRecycleBin(pszRootPath:LPCSTR; pSHQueryRBInfo:LPSHQUERYRBINFO):HRESULT;external 'shell32.dll' name 'SHQueryRecycleBinA';
function SHQueryRecycleBin(pszRootPath:LPCWSTR; pSHQueryRBInfo:LPSHQUERYRBINFO):HRESULT;external 'shell32.dll' name 'SHQueryRecycleBinW';

function SHEmptyRecycleBinA(hwnd:HWND; pszRootPath:LPCSTR; dwFlags:DWORD):HRESULT;external 'shell32.dll' name 'SHEmptyRecycleBinA';
function SHEmptyRecycleBinW(hwnd:HWND; pszRootPath:LPCWSTR; dwFlags:DWORD):HRESULT;external 'shell32.dll' name 'SHEmptyRecycleBinW';
function SHEmptyRecycleBin(hwnd:HWND; pszRootPath:LPCSTR; dwFlags:DWORD):HRESULT;external 'shell32.dll' name 'SHEmptyRecycleBinA';
function SHEmptyRecycleBin(hwnd:HWND; pszRootPath:LPCWSTR; dwFlags:DWORD):HRESULT;external 'shell32.dll' name 'SHEmptyRecycleBinW';

//
// end of RecycleBin
//

//
// Tray notification definitions
//

Type

       _NOTIFYICONDATAA         = record
                                   cbSize               : DWORD;
                                   hWnd                 : HWND;
                                   uID                  : UINT;
                                   uFlags               : UINT;
                                   uCallbackMessage     : UINT;
                                   hIcon                : HICON;
                                   {$ifdef IELower5}
                                    szTip               : array[0..63] of CHAR;
                                   {$else}
                                    szTip               : array[0..127] of CHAR;
                                   {$endif}
                                   {$ifdef IEhigherEqual5}
                                    dwState             : DWORD;
                                    dwStateMask         : DWORD;
                                    szInfo              : array[0..255] of CHAR;
                                    DUMMYUNIONNAME      : record
                                                           case longint of
                                                               0 : ( uTimeout : UINT );
                                                               1 : ( uVersion : UINT );
                                                              end;
                                    szInfoTitle : array[0..63] of CHAR;
                                    dwInfoFlags : DWORD;
                                   {$endif}
                                   {$ifdef IEHighEq6}
                                    guidItem : GUID;
                                   {$endif}
                                   end;
       NOTIFYICONDATAA          = _NOTIFYICONDATAA;
       TNOTIFYICONDATAA         = _NOTIFYICONDATAA;
       PNOTIFYICONDATAA         = ^_NOTIFYICONDATAA;


       _NOTIFYICONDATAW         = record
                                   cbSize               : DWORD;
                                   hWnd                 : HWND;
                                   uID                  : UINT;
                                   uFlags               : UINT;
                                   uCallbackMessage     : UINT;
                                   hIcon                : HICON;
                                   {$ifdef IELower5}
                                    szTip               : array[0..63] of WCHAR;
                                   {$else}
                                    szTip               : array[0..127] of WCHAR;
                                   {$endif}
                                   {$ifdef IEhigherEqual5}
                                    dwState             : DWORD;
                                    dwStateMask         : DWORD;
                                    szInfo              : array[0..255] of WCHAR;
                                    DUMMYUNIONNAME      : record
                                                           case longint of
                                                               0 : ( uTimeout : UINT );
                                                               1 : ( uVersion : UINT );
                                                              end;
                                    szInfoTitle : array[0..63] of CHAR;
                                    dwInfoFlags : DWORD;
                                   {$endif}
                                   {$ifdef IEHighEq6}
                                    guidItem : GUID;
                                   {$endif}
                                   end;
       NOTIFYICONDATAW          = _NOTIFYICONDATAW;
       TNOTIFYICONDATAW         = _NOTIFYICONDATAW;
       PNOTIFYICONDATAW         = ^_NOTIFYICONDATAW;
{$ifdef UNICODE}
       NOTIFYICONDATA           = NOTIFYICONDATAW;
       TNOTIFYICONDATA          = NOTIFYICONDATAW;
       PNOTIFYICONDATA          = PNOTIFYICONDATAW;
{$else}
       NOTIFYICONDATA           = NOTIFYICONDATAA;
       TNOTIFYICONDATA          = NOTIFYICONDATAA;
       PNOTIFYICONDATA          = PNOTIFYICONDATAA;
{$endif}
    { UNICODE }

    {

#define NOTIFYICONDATAA_V1_SIZE     FIELD_OFFSET(NOTIFYICONDATAA, szTip[64])
#define NOTIFYICONDATAW_V1_SIZE     FIELD_OFFSET(NOTIFYICONDATAW, szTip[64])
#ifdef UNICODE
#define NOTIFYICONDATA_V1_SIZE      NOTIFYICONDATAW_V1_SIZE
#else
#define NOTIFYICONDATA_V1_SIZE      NOTIFYICONDATAA_V1_SIZE
#endif

#define NOTIFYICONDATAA_V2_SIZE     FIELD_OFFSET(NOTIFYICONDATAA, guidItem)
#define NOTIFYICONDATAW_V2_SIZE     FIELD_OFFSET(NOTIFYICONDATAW, guidItem)
#ifdef UNICODE
#define NOTIFYICONDATA_V2_SIZE      NOTIFYICONDATAW_V2_SIZE
#else
#define NOTIFYICONDATA_V2_SIZE      NOTIFYICONDATAA_V2_SIZE
#endif
}


    const
       NIN_SELECT               = WM_USER + 0;
       NINF_KEY                 = $1;
       NIN_KEYSELECT            = NIN_SELECT or NINF_KEY;
// if (_WIN32_IE >= 0x0501)}

       NIN_BALLOONSHOW          = WM_USER + 2;
       NIN_BALLOONHIDE          = WM_USER + 3;
       NIN_BALLOONTIMEOUT       = WM_USER + 4;
       NIN_BALLOONUSERCLICK     = WM_USER + 5;
       NIM_ADD                  = $00000000;
       NIM_MODIFY               = $00000001;
       NIM_DELETE               = $00000002;
//if (_WIN32_IE >= 0x0500)}

       NIM_SETFOCUS             = $00000003;
       NIM_SETVERSION           = $00000004;
       NOTIFYICON_VERSION       = 3;

       NIF_MESSAGE              = $00000001;
       NIF_ICON                 = $00000002;
       NIF_TIP                  = $00000004;
// if (_WIN32_IE >= 0x0500)}
       NIF_STATE                = $00000008;
       NIF_INFO                 = $00000010;
//if (_WIN32_IE >= 0x600)}

       NIF_GUID                 = $00000020;
//if (_WIN32_IE >= 0x0500)}

       NIS_HIDDEN               = $00000001;
       NIS_SHAREDICON           = $00000002;
    { says this is the source of a shared icon }
    { Notify Icon Infotip flags }
       NIIF_NONE                = $00000000;
    { icon flags are mutually exclusive }
    { and take only the lowest 2 bits }
       NIIF_INFO                = $00000001;
       NIIF_WARNING             = $00000002;
       NIIF_ERROR               = $00000003;
       NIIF_ICON_MASK           = $0000000F;
//if (_WIN32_IE >= 0x0501)}

       NIIF_NOSOUND             = $00000010;

Function Shell_NotifyIconA( dwMessage: Dword;lpData: PNOTIFYICONDATAA):Bool;external 'shell32.dll' name 'Shell_NotifyIconA';
Function Shell_NotifyIconW( dwMessage: Dword;lpData: PNOTIFYICONDATAW):Bool;external 'shell32.dll' name 'Shell_NotifyIconW';

Function Shell_NotifyIcon( dwMessage: Dword;lpData: PNOTIFYICONDATAA):Bool;external 'shell32.dll' name 'Shell_NotifyIconA';
Function Shell_NotifyIcon( dwMessage: Dword;lpData: PNOTIFYICONDATAW):Bool;external 'shell32.dll' name 'Shell_NotifyIconW';
//
// End Tray Notification Icons
//

//
// Begin SHGetFileInfo
//
    {
       The SHGetFileInfo API provides an easy way to get attributes
       for a file given a pathname.

         PARAMETERS

           pszPath              file name to get info about
           dwFileAttributes     file attribs, only used with SHGFI_USEFILEATTRIBUTES
           psfi                 place to return file info
           cbFileInfo           size of structure
           uFlags               flags

         RETURN
           TRUE if things worked
      }
    { out: icon }
    { out: icon index }
    { out: SFGAO_ flags }
    { out: display name (or path) }
    { out: type name }

    type

       _SHFILEINFOA                     = record
                                            hIcon         : HICON;                          { out: icon }
                                            iIcon         : longint;                        { out: icon index }
                                            dwAttributes  : DWORD;                          { out: SFGAO_ flags }
                                            szDisplayName : array[0..(MAX_PATH)-1] of CHAR; { out: display name (or path) }
                                            szTypeName    : array[0..79] of CHAR;           { out: type name }
                                           end;
       SHFILEINFOA                      = _SHFILEINFOA;
       TSHFILEINFOA                     = _SHFILEINFOA;
       pSHFILEINFOA                     = ^_SHFILEINFOA;

       _SHFILEINFOW                     = record
                                            hIcon         : HICON;                          { out: icon }
                                            iIcon         : longint;                        { out: icon index }
                                            dwAttributes  : DWORD;                          { out: SFGAO_ flags }
                                            szDisplayName : array[0..(MAX_PATH)-1] of WCHAR;{ out: display name (or path) }
                                            szTypeName    : array[0..79] of WCHAR;          { out: type name }
                                           end;
       SHFILEINFOW                      = _SHFILEINFOW;
       TSHFILEINFOW                     = _SHFILEINFOW;
       pSHFILEINFOW                     = ^_SHFILEINFOW;

{$ifdef UNICODE}
       SHFILEINFO                       = SHFILEINFOW;
       TSHFILEINFO                      = SHFILEINFOW;
       pFILEINFO                        = SHFILEINFOW;
{$else}
       SHFILEINFO                       = SHFILEINFOA;
       TSHFILEINFO                      = SHFILEINFOA;
       pFILEINFO                        = SHFILEINFOA;
{$endif}
    { NOTE: This is also in shlwapi.h.  Please keep in synch. }

    const
       SHGFI_ICON               = $000000100;    { get Icon}
       SHGFI_DISPLAYNAME        = $000000200;    { get display name }
       SHGFI_TYPENAME           = $000000400;    { get type name }
       SHGFI_ATTRIBUTES         = $000000800;    { get attributes }
       SHGFI_ICONLOCATION       = $000001000;    { get icon location}
       SHGFI_EXETYPE            = $000002000;    { return exe type }
       SHGFI_SYSICONINDEX       = $000004000;    { get system icon index }
       SHGFI_LINKOVERLAY        = $000008000;    { put a link overlay on icon }
       SHGFI_SELECTED           = $000010000;    { show icon in selected state }
       SHGFI_ATTR_SPECIFIED     = $000020000;    { get only specified attributes }
       SHGFI_LARGEICON          = $000000000;    { get large icon }
       SHGFI_SMALLICON          = $000000001;    { get small icon }
       SHGFI_OPENICON           = $000000002;    { get open icon }
       SHGFI_SHELLICONSIZE      = $000000004;    { get shell size icon }
       SHGFI_PIDL               = $000000008;    { pszPath is a pidl }
       SHGFI_USEFILEATTRIBUTES  = $000000010;    { use passed dwFileAttribute }
//if (_WIN32_IE >= 0x0500)}
       SHGFI_ADDOVERLAYS        = $000000020;    { apply the appropriate overlays }
       SHGFI_OVERLAYINDEX       = $000000040;    { Get the index of the overlay }
                                                 { in the upper 8 bits of the iIcon  }
Function SHGetFileInfoA(pszPath: LPCSTR; dwFileAttributes : DWORD; psfi: pSHFILEINFOA; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoA';
Function SHGetFileInfoW(pszPath: LPCWSTR; dwFileAttributes : DWORD; psfi: pSHFILEINFOW; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoW';
Function SHGetFileInfo(pszPath: LPCSTR; dwFileAttributes : DWORD; psfi: pSHFILEINFOA; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoA';

Function SHGetFileInfoA(pszPath: LPCSTR; dwFileAttributes : DWORD; var psfi: TSHFILEINFOA; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoA';
Function SHGetFileInfoW(pszPath: LPCWSTR; dwFileAttributes : DWORD; var psfi: TSHFILEINFOW; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoW';
Function SHGetFileInfo(pszPath: LPCSTR; dwFileAttributes : DWORD; var psfi: TSHFILEINFOA; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoA';
Function SHGetFileInfo(pszPath: LPCWSTR; dwFileAttributes : DWORD; var psfi: TSHFILEINFOW; cbFileInfo,UFlags: UINT):DWORD;external 'shell32.dll' name 'SHGetFileInfoW';

Function SHGetDiskFreeSpaceExA( pszDirectoryName : LPCSTR; pulFreeBytesAvailableToCaller : pULARGE_INTEGER; pulTotalNumberOfBytes : pULARGE_INTEGER;pulTotalNumberOfFreeBytes: pULARGE_INTEGER):Bool;external 'shell32.dll' name 'SHGetDiskFreeSpaceExA';
Function SHGetDiskFreeSpaceExW( pszDirectoryName : LPCWSTR; pulFreeBytesAvailableToCaller : pULARGE_INTEGER; pulTotalNumberOfBytes : pULARGE_INTEGER;pulTotalNumberOfFreeBytes: pULARGE_INTEGER):Bool;external 'shell32.dll' name 'SHGetDiskFreeSpaceExW';
Function SHGetDiskFreeSpaceEx( pszDirectoryName : LPCSTR; pulFreeBytesAvailableToCaller : pULARGE_INTEGER; pulTotalNumberOfBytes : pULARGE_INTEGER;pulTotalNumberOfFreeBytes: pULARGE_INTEGER):Bool;external 'shell32.dll' name 'SHGetDiskFreeSpaceExA';
Function SHGetDiskFreeSpace( pszDirectoryName : LPCSTR; pulFreeBytesAvailableToCaller : pULARGE_INTEGER; pulTotalNumberOfBytes : pULARGE_INTEGER;pulTotalNumberOfFreeBytes: pULARGE_INTEGER):Bool;external 'shell32.dll' name 'SHGetDiskFreeSpaceExA';
Function SHGetDiskFreeSpaceEx( pszDirectoryName : LPCWSTR; pulFreeBytesAvailableToCaller : pULARGE_INTEGER; pulTotalNumberOfBytes : pULARGE_INTEGER;pulTotalNumberOfFreeBytes: pULARGE_INTEGER):Bool;external 'shell32.dll' name 'SHGetDiskFreeSpaceExW';
Function SHGetDiskFreeSpace( pszDirectoryName : LPCWSTR; pulFreeBytesAvailableToCaller : pULARGE_INTEGER; pulTotalNumberOfBytes : pULARGE_INTEGER;pulTotalNumberOfFreeBytes: pULARGE_INTEGER):Bool;external 'shell32.dll' name 'SHGetDiskFreeSpaceExW';

Function SHGetNewLinkInfoA(pszLinkTo:LPCSTR;pszDir:LPCSTR; pszName:LPSTR; pfMustCopy: pBool; uFlags:UINT):Bool;external 'shell32.dll' name 'SHGetNewLinkInfoA';
Function SHGetNewLinkInfoW(pszLinkTo:LPCWSTR;pszDir:LPCWSTR; pszName:LPWSTR; pfMustCopy: pBool; uFlags:UINT):Bool;external 'shell32.dll' name 'SHGetNewLinkInfoW';

Function SHGetNewLinkInfo (pszLinkTo:LPCSTR;pszDir:LPCSTR; pszName:LPSTR; pfMustCopy: pBool; uFlags:UINT):Bool;external 'shell32.dll' name 'SHGetNewLinkInfoA';
Function SHGetNewLinkInfo (pszLinkTo:LPCWSTR;pszDir:LPCWSTR; pszName:LPWSTR; pfMustCopy: pBool; uFlags:UINT):Bool;external 'shell32.dll' name 'SHGetNewLinkInfoW';

    const
       SHGNLI_PIDL              = $000000001;    { pszLinkTo is a pidl }
       SHGNLI_PREFIXNAME        = $000000002;    { Make name "Shortcut to xxx" }
       SHGNLI_NOUNIQUE          = $000000004;    { don't do the unique name generation }
// {if (_WIN32_IE >= 0x0501)}
       SHGNLI_NOLNK             = $000000008;    { don't add ".lnk" extension }
// {$endif}
    { _WIN2_IE >= 0x0501 }
//
// End SHGetFileInfo
//

    { Printer stuff }
       PRINTACTION_OPEN             = 0;
       PRINTACTION_PROPERTIES       = 1;
       PRINTACTION_NETINSTALL       = 2;
       PRINTACTION_NETINSTALLLINK   = 3;
       PRINTACTION_TESTPAGE         = 4;
       PRINTACTION_OPENNETPRN       = 5;
{$ifdef WINNT}
       PRINTACTION_DOCUMENTDEFAULTS = 6;
       PRINTACTION_SERVERPROPERTIES = 7;
{$endif}

Function SHInvokePrinterCommandA(HWND: hwnd; uAction:UINT; lpBuf1: LPCSTR; lpBuf2: LPCSTR; fModal:Bool):Bool;external 'shell32.dll' name 'SHInvokePrinterCommandA';
Function SHInvokePrinterCommandW(HWND: hwnd; uAction:UINT; lpBuf1: LPCWSTR; lpBuf2: LPCWSTR; fModal:Bool):Bool;external 'shell32.dll' name 'SHInvokePrinterCommandW';
Function SHInvokePrinterCommand(HWND: hwnd; uAction:UINT; lpBuf1: LPCSTR; lpBuf2: LPCSTR; fModal:Bool):Bool;external 'shell32.dll' name 'SHInvokePrinterCommandA';
Function SHInvokePrinterCommand(HWND: hwnd; uAction:UINT; lpBuf1: LPCWSTR; lpBuf2: LPCWSTR; fModal:Bool):Bool;external 'shell32.dll' name 'SHInvokePrinterCommandW';

// WINVER >= 0x0400
//if (_WIN32_WINNT >= 0x0500) || (_WIN32_WINDOWS >= 0x0500)
    //
    // The SHLoadNonloadedIconOverlayIdentifiers API causes the shell's
    // icon overlay manager to load any registered icon overlay
    // identifers that are not currently loaded.  This is useful if an
    // overlay identifier did not load at shell startup but is needed
    // and can be loaded at a later time.  Identifiers already loaded
    // are not affected.  Overlay identifiers implement the
    // IShellIconOverlayIdentifier interface.
    //
    // Returns:
    //      S_OK
    //

function SHLoadNonloadedIconOverlayIdentifiers:HResult; external 'shell32.dll' name 'SHInvokePrinterCommandW';

    //
    // The SHIsFileAvailableOffline API determines whether a file
    // or folder is available for offline use.
    //
    // Parameters:
    //     pwszPath             file name to get info about
    //     pdwStatus            (optional) OFFLINE_STATUS_* flags returned here
    //
    // Returns:
    //     S_OK                 File/directory is available offline, unless
    //                            OFFLINE_STATUS_INCOMPLETE is returned.
    //     E_INVALIDARG         Path is invalid, or not a net path
    //     E_FAIL               File/directory is not available offline
    //
    // Notes:
    //     OFFLINE_STATUS_INCOMPLETE is never returned for directories.
    //     Both OFFLINE_STATUS_LOCAL and OFFLINE_STATUS_REMOTE may be returned,
    //     indicating "open in both places." This is common when the server is online.
    //
function SHIsFileAvailableOffline(pwszPath:LPCWSTR; pdwStatus:LPDWORD):HRESULT; external 'shell32.dll' name 'SHIsFileAvailableOffline';

const
       OFFLINE_STATUS_LOCAL         = $0001;    { If open, it's open locally }
       OFFLINE_STATUS_REMOTE        = $0002;    { If open, it's open remotely }
       OFFLINE_STATUS_INCOMPLETE    = $0004;    { The local copy is currently incomplete. }
                                                { The file will not be available offline }
                                                { until it has been synchronized. }
    {  sets the specified path to use the string resource }
    {  as the UI instead of the file system name }

function SHSetLocalizedName(pszPath:LPWSTR; pszResModule:LPCWSTR; idsRes:longint):HRESULT;external 'shell32.dll' name 'SHSetLocalizedName';

//if         _WIN32_IE >= 0x0600}

function SHEnumerateUnreadMailAccountsA(hKeyUser:HKEY; dwIndex:DWORD; pszMailAddress:LPSTR; cchMailAddress:longint):HRESULT;external 'shell32.dll' name 'SHEnumerateUnreadMailAccountsA';
function SHEnumerateUnreadMailAccountsW(hKeyUser:HKEY; dwIndex:DWORD; pszMailAddress:LPWSTR; cchMailAddress:longint):HRESULT;external 'shell32.dll' name 'SHEnumerateUnreadMailAccountsW';

function SHEnumerateUnreadMailAccounts(hKeyUser:HKEY; dwIndex:DWORD; pszMailAddress:LPWSTR; cchMailAddress:longint):HRESULT;external 'shell32.dll' name 'SHEnumerateUnreadMailAccountsW';

function SHGetUnreadMailCountA(hKeyUser:HKEY; pszMailAddress:LPCSTR; pdwCount:PDWORD; pFileTime:PFILETIME; pszShellExecuteCommand:LPSTR;cchShellExecuteCommand:longint):HRESULT;external 'shell32.dll' name 'SHGetUnreadMailCountA';
function SHGetUnreadMailCountW(hKeyUser:HKEY; pszMailAddress:LPCWSTR; pdwCount:PDWORD; pFileTime:PFILETIME; pszShellExecuteCommand:LPWSTR;cchShellExecuteCommand:longint):HRESULT;external 'shell32.dll' name 'SHGetUnreadMailCountW';
function SHGetUnreadMailCount(hKeyUser:HKEY; pszMailAddress:LPCSTR; pdwCount:PDWORD; pFileTime:PFILETIME; pszShellExecuteCommand:LPSTR;cchShellExecuteCommand:longint):HRESULT;external 'shell32.dll' name 'SHGetUnreadMailCountA';
function SHGetUnreadMailCount(hKeyUser:HKEY; pszMailAddress:LPCWSTR; pdwCount:PDWORD; pFileTime:PFILETIME; pszShellExecuteCommand:LPWSTR;cchShellExecuteCommand:longint):HRESULT;external 'shell32.dll' name 'SHGetUnreadMailCountW';

function SHSetUnreadMailCountA(pszMailAddress:LPCSTR; dwCount:DWORD; pszShellExecuteCommand:LPCSTR):HRESULT;external 'shell32.dll' name 'SHSetUnreadMailCountA';
function SHSetUnreadMailCountW(pszMailAddress:LPCWSTR; dwCount:DWORD; pszShellExecuteCommand:LPCWSTR):HRESULT;external 'shell32.dll' name 'SHSetUnreadMailCountW';
function SHSetUnreadMailCount(pszMailAddress:LPCSTR; dwCount:DWORD; pszShellExecuteCommand:LPCSTR):HRESULT;external 'shell32.dll' name 'SHSetUnreadMailCountA';
function SHSetUnreadMailCount(pszMailAddress:LPCWSTR; dwCount:DWORD; pszShellExecuteCommand:LPCWSTR):HRESULT;external 'shell32.dll' name 'SHSetUnreadMailCountW';

//  _WIN32_IE >= 0x0600      }
//  if         _WIN32_IE >= 0x0600}

function SHGetImageList(iImageList:longint;CONST riid:TIID; ppvObj:Ppointer):HRESULT;external 'shell32.dll' name 'SHGetImageList';

Const
       SHIL_LARGE                   = 0;    { normally 32x32 }
       SHIL_SMALL                   = 1;    { normally 16x16 }
       SHIL_EXTRALARGE              = 2;
       SHIL_SYSSMALL                = 3;    { like SHIL_SMALL, but tracks system small icon metric correctly }
       SHIL_LAST                    = SHIL_SYSSMALL;

    { Function call types for ntshrui folder sharing helpers }

//typedef HRESULT (STDMETHODCALLTYPE *PFNSHOWSHAREFOLDERUIW)(IN HWND hwndParent, IN LPCSTR pszPath);
//typedef HRESULT (STDMETHODCALLTYPE *PFNSHOWSHAREFOLDERUIW)(IN HWND hwndParent, IN LPCWSTR pszPath);

implementation

function EIRESID(x : longint) : longint;
Begin
  EIRESID:=-x;
End;

end.
