{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1993,97 by Florian KLaempfl,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef windows_include_files}
{$define read_interface}
{$define read_implementation}
{$endif not windows_include_files}


{$ifndef windows_include_files}

unit unifun;

{  Automatically converted by H2PAS.EXE from unicfun.h
   Utility made by Florian Klaempfl 25th-28th september 96
   Improvements made by Mark A. Malakanov 22nd-25th may 97 
   Further improvements by Michael Van Canneyt, April 1998 
   define handling and error recovery by Pierre Muller, June 1998 }


  interface

   uses
      base,defines,struct;

{$endif not windows_include_files}

{$ifdef read_interface}

  { C default packing is dword }

{$PACKRECORDS 4}
  { 
     UnicodeFunctions.h
  
     Declarations for all the Windows32 API Unicode Functions
  
     Copyright (C) 1996 Free Software Foundation, Inc.
  
     Author:  Scott Christley <scottc@net-community.com>
     Date: 1996
     
     This file is part of the Windows32 API Library.
  
     This library is free software; you can redistribute it and/or
     modify it under the terms of the GNU Library General Public
     License as published by the Free Software Foundation; either
     version 2 of the License, or (at your option) any later version.
     
     This library is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
     Library General Public License for more details.
  
     If you are interested in a warranty or support for this source code,
     contact Scott Christley <scottc@net-community.com> for more information.
     
     You should have received a copy of the GNU Library General Public
     License along with this library; see the file COPYING.LIB.
     If not, write to the Free Software Foundation, 
     59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
   }
{$ifndef _GNU_H_WINDOWS32_UNICODEFUNCTIONS}
{$define _GNU_H_WINDOWS32_UNICODEFUNCTIONS}
{ C++ extern C conditionnal removed }
  { __cplusplus  }

  function GetBinaryTypeW(lpApplicationName:LPCWSTR; lpBinaryType:LPDWORD):WINBOOL;

  function GetShortPathNameW(lpszLongPath:LPCWSTR; lpszShortPath:LPWSTR; cchBuffer:DWORD):DWORD;

  function GetEnvironmentStringsW:LPWSTR;

  function FreeEnvironmentStringsW(_para1:LPWSTR):WINBOOL;

  function FormatMessageW(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPWSTR; 
             nSize:DWORD; var Arguments:va_list):DWORD;

  function CreateMailslotW(lpName:LPCWSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function lstrcmpW(lpString1:LPCWSTR; lpString2:LPCWSTR):longint;

  function lstrcmpiW(lpString1:LPCWSTR; lpString2:LPCWSTR):longint;

  function lstrcpynW(lpString1:LPWSTR; lpString2:LPCWSTR; iMaxLength:longint):LPWSTR;

  function lstrcpyW(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR;

  function lstrcatW(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR;

  function lstrlenW(lpString:LPCWSTR):longint;

  function CreateMutexW(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCWSTR):HANDLE;

  function OpenMutexW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function CreateEventW(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCWSTR):HANDLE;

  function OpenEventW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function CreateSemaphoreW(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCWSTR):HANDLE;

  function OpenSemaphoreW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function CreateFileMappingW(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD; 
             lpName:LPCWSTR):HANDLE;

  function OpenFileMappingW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function GetLogicalDriveStringsW(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD;

  function LoadLibraryW(lpLibFileName:LPCWSTR):HINSTANCE;

  function LoadLibraryExW(lpLibFileName:LPCWSTR; hFile:HANDLE; dwFlags:DWORD):HINSTANCE;

  function GetModuleFileNameW(hModule:HINSTANCE; lpFilename:LPWSTR; nSize:DWORD):DWORD;

  function GetModuleHandleW(lpModuleName:LPCWSTR):HMODULE;

  procedure FatalAppExitW(uAction:UINT; lpMessageText:LPCWSTR);

  function GetCommandLineW:LPWSTR;

  function GetEnvironmentVariableW(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD;

  function SetEnvironmentVariableW(lpName:LPCWSTR; lpValue:LPCWSTR):WINBOOL;

  function ExpandEnvironmentStringsW(lpSrc:LPCWSTR; lpDst:LPWSTR; nSize:DWORD):DWORD;

  procedure OutputDebugStringW(lpOutputString:LPCWSTR);

  function FindResourceW(hModule:HINSTANCE; lpName:LPCWSTR; lpType:LPCWSTR):HRSRC;

  function FindResourceExW(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD):HRSRC;

  function EnumResourceTypesW(hModule:HINSTANCE; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL;

  function EnumResourceNamesW(hModule:HINSTANCE; lpType:LPCWSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL;

  function EnumResourceLanguagesW(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL;

  function BeginUpdateResourceW(pFileName:LPCWSTR; bDeleteExistingResources:WINBOOL):HANDLE;

  function UpdateResourceW(hUpdate:HANDLE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD; lpData:LPVOID; 
             cbData:DWORD):WINBOOL;

  function EndUpdateResourceW(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL;

  function GlobalAddAtomW(lpString:LPCWSTR):ATOM;

  function GlobalFindAtomW(lpString:LPCWSTR):ATOM;

  function GlobalGetAtomNameW(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT;

  function AddAtomW(lpString:LPCWSTR):ATOM;

  function FindAtomW(lpString:LPCWSTR):ATOM;

  function GetAtomNameW(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT;

  function GetProfileIntW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT):UINT;

  function GetProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD;

  function WriteProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR):WINBOOL;

  function GetProfileSectionW(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD;

  function WriteProfileSectionW(lpAppName:LPCWSTR; lpString:LPCWSTR):WINBOOL;

  function GetPrivateProfileIntW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT; lpFileName:LPCWSTR):UINT;

  function GetPrivateProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; 
             lpFileName:LPCWSTR):DWORD;

  function WritePrivateProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL;

  function GetPrivateProfileSectionW(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; lpFileName:LPCWSTR):DWORD;

  function WritePrivateProfileSectionW(lpAppName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL;

  function GetDriveTypeW(lpRootPathName:LPCWSTR):UINT;

  function GetSystemDirectoryW(lpBuffer:LPWSTR; uSize:UINT):UINT;

  function GetTempPathW(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD;

  function GetTempFileNameW(lpPathName:LPCWSTR; lpPrefixString:LPCWSTR; uUnique:UINT; lpTempFileName:LPWSTR):UINT;

  function GetWindowsDirectoryW(lpBuffer:LPWSTR; uSize:UINT):UINT;

  function SetCurrentDirectoryW(lpPathName:LPCWSTR):WINBOOL;

  function GetCurrentDirectoryW(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD;

  function GetDiskFreeSpaceW(lpRootPathName:LPCWSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL;

  function CreateDirectoryW(lpPathName:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function CreateDirectoryExW(lpTemplateDirectory:LPCWSTR; lpNewDirectory:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function RemoveDirectoryW(lpPathName:LPCWSTR):WINBOOL;

  function GetFullPathNameW(lpFileName:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; var lpFilePart:LPWSTR):DWORD;

  function DefineDosDeviceW(dwFlags:DWORD; lpDeviceName:LPCWSTR; lpTargetPath:LPCWSTR):WINBOOL;

  function QueryDosDeviceW(lpDeviceName:LPCWSTR; lpTargetPath:LPWSTR; ucchMax:DWORD):DWORD;

  function CreateFileW(lpFileName:LPCWSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD; 
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE;

  function SetFileAttributesW(lpFileName:LPCWSTR; dwFileAttributes:DWORD):WINBOOL;

  function GetFileAttributesW(lpFileName:LPCWSTR):DWORD;

  function GetCompressedFileSizeW(lpFileName:LPCWSTR; lpFileSizeHigh:LPDWORD):DWORD;

  function DeleteFileW(lpFileName:LPCWSTR):WINBOOL;

  function SearchPathW(lpPath:LPCWSTR; lpFileName:LPCWSTR; lpExtension:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; 
             var lpFilePart:LPWSTR):DWORD;

  function CopyFileW(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; bFailIfExists:WINBOOL):WINBOOL;

  function MoveFileW(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR):WINBOOL;

  function MoveFileExW(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; dwFlags:DWORD):WINBOOL;

  function CreateNamedPipeW(lpName:LPCWSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD; 
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function GetNamedPipeHandleStateW(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD; 
             lpUserName:LPWSTR; nMaxUserNameSize:DWORD):WINBOOL;

  function CallNamedPipeW(lpNamedPipeName:LPCWSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD; 
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL;

  function WaitNamedPipeW(lpNamedPipeName:LPCWSTR; nTimeOut:DWORD):WINBOOL;

  function SetVolumeLabelW(lpRootPathName:LPCWSTR; lpVolumeName:LPCWSTR):WINBOOL;

  function GetVolumeInformationW(lpRootPathName:LPCWSTR; lpVolumeNameBuffer:LPWSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD; 
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPWSTR; nFileSystemNameSize:DWORD):WINBOOL;

  function ClearEventLogW(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL;

  function BackupEventLogW(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL;

  function OpenEventLogW(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE;

  function RegisterEventSourceW(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE;

  function OpenBackupEventLogW(lpUNCServerName:LPCWSTR; lpFileName:LPCWSTR):HANDLE;

  function ReadEventLogW(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; 
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL;

  function ReportEventW(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID; 
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCWSTR; lpRawData:LPVOID):WINBOOL;

  function AccessCheckAndAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR; 
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL; 
             pfGenerateOnClose:LPBOOL):WINBOOL;

  function ObjectOpenAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR; 
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL; 
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL;

  function ObjectPrivilegeAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET; 
             AccessGranted:WINBOOL):WINBOOL;

  function ObjectCloseAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL;

  function PrivilegedServiceAuditAlarmW(SubsystemName:LPCWSTR; ServiceName:LPCWSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL;

  function SetFileSecurityW(lpFileName:LPCWSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function GetFileSecurityW(lpFileName:LPCWSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function FindFirstChangeNotificationW(lpPathName:LPCWSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE;

  function IsBadStringPtrW(lpsz:LPCWSTR; ucchMax:UINT):WINBOOL;

  function LookupAccountSidW(lpSystemName:LPCWSTR; Sid:PSID; Name:LPWSTR; cbName:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupAccountNameW(lpSystemName:LPCWSTR; lpAccountName:LPCWSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupPrivilegeValueW(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpLuid:PLUID):WINBOOL;

  function LookupPrivilegeNameW(lpSystemName:LPCWSTR; lpLuid:PLUID; lpName:LPWSTR; cbName:LPDWORD):WINBOOL;

  function LookupPrivilegeDisplayNameW(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpDisplayName:LPWSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL;

  function BuildCommDCBW(lpDef:LPCWSTR; lpDCB:LPDCB):WINBOOL;

  function BuildCommDCBAndTimeoutsW(lpDef:LPCWSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL;

  function CommConfigDialogW(lpszName:LPCWSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL;

  function GetDefaultCommConfigW(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL;

  function SetDefaultCommConfigW(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL;

  function GetComputerNameW(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL;

  function SetComputerNameW(lpComputerName:LPCWSTR):WINBOOL;

  function GetUserNameW(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL;

  function wvsprintfW(_para1:LPWSTR; _para2:LPCWSTR; arglist:va_list):longint;

  { variable number of args not yet implemented in FPC
  function wsprintfW(_para1:LPWSTR; _para2:LPCWSTR; ...):longint;}

  function LoadKeyboardLayoutW(pwszKLID:LPCWSTR; Flags:UINT):HKL;

  function GetKeyboardLayoutNameW(pwszKLID:LPWSTR):WINBOOL;

  function CreateDesktopW(lpszDesktop:LPWSTR; lpszDevice:LPWSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD; 
             lpsa:LPSECURITY_ATTRIBUTES):HDESK;

  function OpenDesktopW(lpszDesktop:LPWSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK;

  function EnumDesktopsW(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL;

  function CreateWindowStationW(lpwinsta:LPWSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA;

  function OpenWindowStationW(lpszWinSta:LPWSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA;

  function EnumWindowStationsW(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL;

  function GetUserObjectInformationW(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function SetUserObjectInformationW(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL;

  function RegisterWindowMessageW(lpString:LPCWSTR):UINT;

  function GetMessageW(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL;

(* Const before type ignored *)
  function DispatchMessageW(var lpMsg:MSG):LONG;

  function PeekMessageW(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL;

  function SendMessageW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function SendMessageTimeoutW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT; 
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT;

  function SendNotifyMessageW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function SendMessageCallbackW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC; 
             dwData:DWORD):WINBOOL;

  function PostMessageW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function PostThreadMessageW(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function DefWindowProcW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallWindowProcW(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

(* Const before type ignored *)
  function RegisterClassW(var lpWndClass:WNDCLASS):ATOM;

  function UnregisterClassW(lpClassName:LPCWSTR; hInstance:HINSTANCE):WINBOOL;

  function GetClassInfoW(hInstance:HINSTANCE; lpClassName:LPCWSTR; lpWndClass:LPWNDCLASS):WINBOOL;

(* Const before type ignored *)
  function RegisterClassExW(var _para1:WNDCLASSEX):ATOM;

  function GetClassInfoExW(_para1:HINSTANCE; _para2:LPCWSTR; _para3:LPWNDCLASSEX):WINBOOL;

  function CreateWindowExW(dwExStyle:DWORD; lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint; 
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;

  function CreateDialogParamW(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function CreateDialogIndirectParamW(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function DialogBoxParamW(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function DialogBoxIndirectParamW(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function SetDlgItemTextW(hDlg:HWND; nIDDlgItem:longint; lpString:LPCWSTR):WINBOOL;

  function GetDlgItemTextW(hDlg:HWND; nIDDlgItem:longint; lpString:LPWSTR; nMaxCount:longint):UINT;

  function SendDlgItemMessageW(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG;

  function DefDlgProcW(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallMsgFilterW(lpMsg:LPMSG; nCode:longint):WINBOOL;

  function RegisterClipboardFormatW(lpszFormat:LPCWSTR):UINT;

  function GetClipboardFormatNameW(format:UINT; lpszFormatName:LPWSTR; cchMaxCount:longint):longint;

  function CharToOemW(lpszSrc:LPCWSTR; lpszDst:LPSTR):WINBOOL;

  function OemToCharW(lpszSrc:LPCSTR; lpszDst:LPWSTR):WINBOOL;

  function CharToOemBuffW(lpszSrc:LPCWSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL;

  function OemToCharBuffW(lpszSrc:LPCSTR; lpszDst:LPWSTR; cchDstLength:DWORD):WINBOOL;

  function CharUpperW(lpsz:LPWSTR):LPWSTR;

  function CharUpperBuffW(lpsz:LPWSTR; cchLength:DWORD):DWORD;

  function CharLowerW(lpsz:LPWSTR):LPWSTR;

  function CharLowerBuffW(lpsz:LPWSTR; cchLength:DWORD):DWORD;

  function CharNextW(lpsz:LPCWSTR):LPWSTR;

  function CharPrevW(lpszStart:LPCWSTR; lpszCurrent:LPCWSTR):LPWSTR;

  function IsCharAlphaW(ch:WCHAR):WINBOOL;

  function IsCharAlphaNumericW(ch:WCHAR):WINBOOL;

  function IsCharUpperW(ch:WCHAR):WINBOOL;

  function IsCharLowerW(ch:WCHAR):WINBOOL;

  function GetKeyNameTextW(lParam:LONG; lpString:LPWSTR; nSize:longint):longint;

  function VkKeyScanW(ch:WCHAR):SHORT;

  function VkKeyScanExW(ch:WCHAR; dwhkl:HKL):SHORT;

  function MapVirtualKeyW(uCode:UINT; uMapType:UINT):UINT;

  function MapVirtualKeyExW(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT;

  function LoadAcceleratorsW(hInstance:HINSTANCE; lpTableName:LPCWSTR):HACCEL;

  function CreateAcceleratorTableW(_para1:LPACCEL; _para2:longint):HACCEL;

  function CopyAcceleratorTableW(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint;

  function TranslateAcceleratorW(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint;

  function LoadMenuW(hInstance:HINSTANCE; lpMenuName:LPCWSTR):HMENU;

(* Const before type ignored *)
  function LoadMenuIndirectW(var lpMenuTemplate:MENUTEMPLATE):HMENU;

  function ChangeMenuW(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCWSTR; cmdInsert:UINT; flags:UINT):WINBOOL;

  function GetMenuStringW(hMenu:HMENU; uIDItem:UINT; lpString:LPWSTR; nMaxCount:longint; uFlag:UINT):longint;

  function InsertMenuW(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL;

  function AppendMenuW(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL;

  function ModifyMenuW(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL;

  function InsertMenuItemW(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function GetMenuItemInfoW(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL;

  function SetMenuItemInfoW(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function DrawTextW(hDC:HDC; lpString:LPCWSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint;

  function DrawTextExW(_para1:HDC; _para2:LPWSTR; _para3:longint; _para4:LPRECT; _para5:UINT; 
             _para6:LPDRAWTEXTPARAMS):longint;

  function GrayStringW(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint; 
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL;

  function DrawStateW(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM; 
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL;

  function TabbedTextOutW(hDC:HDC; X:longint; Y:longint; lpString:LPCWSTR; nCount:longint; 
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG;

  function GetTabbedTextExtentW(hDC:HDC; lpString:LPCWSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD;

  function SetPropW(hWnd:HWND; lpString:LPCWSTR; hData:HANDLE):WINBOOL;

  function GetPropW(hWnd:HWND; lpString:LPCWSTR):HANDLE;

  function RemovePropW(hWnd:HWND; lpString:LPCWSTR):HANDLE;

  function EnumPropsExW(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint;

  function EnumPropsW(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint;

  function SetWindowTextW(hWnd:HWND; lpString:LPCWSTR):WINBOOL;

  function GetWindowTextW(hWnd:HWND; lpString:LPWSTR; nMaxCount:longint):longint;

  function GetWindowTextLengthW(hWnd:HWND):longint;

  function MessageBoxW(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT):longint;

  function MessageBoxExW(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT; wLanguageId:WORD):longint;

  function MessageBoxIndirectW(_para1:LPMSGBOXPARAMS):longint;

  function GetWindowLongW(hWnd:HWND; nIndex:longint):LONG;

  function SetWindowLongW(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG;

  function GetClassLongW(hWnd:HWND; nIndex:longint):DWORD;

  function SetClassLongW(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD;

  function FindWindowW(lpClassName:LPCWSTR; lpWindowName:LPCWSTR):HWND;

  function FindWindowExW(_para1:HWND; _para2:HWND; _para3:LPCWSTR; _para4:LPCWSTR):HWND;

  function GetClassNameW(hWnd:HWND; lpClassName:LPWSTR; nMaxCount:longint):longint;

  function SetWindowsHookExW(idHook:longint; lpfn:HOOKPROC; hmod:HINSTANCE; dwThreadId:DWORD):HHOOK;

  function LoadBitmapW(hInstance:HINSTANCE; lpBitmapName:LPCWSTR):HBITMAP;

  function LoadCursorW(hInstance:HINSTANCE; lpCursorName:LPCWSTR):HCURSOR;

  function LoadCursorFromFileW(lpFileName:LPCWSTR):HCURSOR;

  function LoadIconW(hInstance:HINSTANCE; lpIconName:LPCWSTR):HICON;

  function LoadImageW(_para1:HINSTANCE; _para2:LPCWSTR; _para3:UINT; _para4:longint; _para5:longint; 
             _para6:UINT):HANDLE;

  function LoadStringW(hInstance:HINSTANCE; uID:UINT; lpBuffer:LPWSTR; nBufferMax:longint):longint;

  function IsDialogMessageW(hDlg:HWND; lpMsg:LPMSG):WINBOOL;

  function DlgDirListW(hDlg:HWND; lpPathSpec:LPWSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint;

  function DlgDirSelectExW(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDListBox:longint):WINBOOL;

  function DlgDirListComboBoxW(hDlg:HWND; lpPathSpec:LPWSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint;

  function DlgDirSelectComboBoxExW(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDComboBox:longint):WINBOOL;

  function DefFrameProcW(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function DefMDIChildProcW(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CreateMDIWindowW(lpClassName:LPWSTR; lpWindowName:LPWSTR; dwStyle:DWORD; X:longint; Y:longint; 
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINSTANCE; lParam:LPARAM):HWND;

  function WinHelpW(hWndMain:HWND; lpszHelp:LPCWSTR; uCommand:UINT; dwData:DWORD):WINBOOL;

  function ChangeDisplaySettingsW(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG;

  function EnumDisplaySettingsW(lpszDeviceName:LPCWSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL;

  function SystemParametersInfoW(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL;

  function AddFontResourceW(_para1:LPCWSTR):longint;

  function CopyMetaFileW(_para1:HMETAFILE; _para2:LPCWSTR):HMETAFILE;

(* Const before type ignored *)
  function CreateFontIndirectW(var _para1:LOGFONT):HFONT;

  function CreateFontW(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCWSTR):HFONT;

(* Const before type ignored *)
  function CreateICW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC;

  function CreateMetaFileW(_para1:LPCWSTR):HDC;

  function CreateScalableFontResourceW(_para1:DWORD; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR):WINBOOL;

(* Const before type ignored *)
  function DeviceCapabilitiesW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:WORD; _para4:LPWSTR; var _para5:DEVMODE):longint;

  function EnumFontFamiliesExW(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint;

  function EnumFontFamiliesW(_para1:HDC; _para2:LPCWSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint;

  function EnumFontsW(_para1:HDC; _para2:LPCWSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint;

  function GetCharWidthW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidth32W(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidthFloatW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL;

  function GetCharABCWidthsW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL;

  function GetCharABCWidthsFloatW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL;

(* Const before type ignored *)
  function GetGlyphOutlineW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD; 
             _para6:LPVOID; var _para7:MAT2):DWORD;

  function GetMetaFileW(_para1:LPCWSTR):HMETAFILE;

  function GetOutlineTextMetricsW(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT;

  function GetTextExtentPointW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentPoint32W(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentExPointW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPINT; 
             _para6:LPINT; _para7:LPSIZE):WINBOOL;

  function GetCharacterPlacementW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS; 
             _para6:DWORD):DWORD;

(* Const before type ignored *)
  function ResetDCW(_para1:HDC; var _para2:DEVMODE):HDC;

  function RemoveFontResourceW(_para1:LPCWSTR):WINBOOL;

  function CopyEnhMetaFileW(_para1:HENHMETAFILE; _para2:LPCWSTR):HENHMETAFILE;

(* Const before type ignored *)
  function CreateEnhMetaFileW(_para1:HDC; _para2:LPCWSTR; var _para3:RECT; _para4:LPCWSTR):HDC;

  function GetEnhMetaFileW(_para1:LPCWSTR):HENHMETAFILE;

  function GetEnhMetaFileDescriptionW(_para1:HENHMETAFILE; _para2:UINT; _para3:LPWSTR):UINT;

  function GetTextMetricsW(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL;

(* Const before type ignored *)
  function StartDocW(_para1:HDC; var _para2:DOCINFO):longint;

  function GetObjectW(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint;

  function TextOutW(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCWSTR; _para5:longint):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ExtTextOutW(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT; 
             _para6:LPCWSTR; _para7:UINT; var _para8:INT):WINBOOL;

(* Const before type ignored *)
  function PolyTextOutW(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL;

  function GetTextFaceW(_para1:HDC; _para2:longint; _para3:LPWSTR):longint;

  function GetKerningPairsW(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD;

  function GetLogColorSpaceW(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL;

  function CreateColorSpaceW(_para1:LPLOGCOLORSPACE):HCOLORSPACE;

  function GetICMProfileW(_para1:HDC; _para2:DWORD; _para3:LPWSTR):WINBOOL;

  function SetICMProfileW(_para1:HDC; _para2:LPWSTR):WINBOOL;

  function UpdateICMRegKeyW(_para1:DWORD; _para2:DWORD; _para3:LPWSTR; _para4:UINT):WINBOOL;

  function EnumICMProfilesW(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint;

  function CreatePropertySheetPageW(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE;

  function PropertySheetW(lppsph:LPCPROPSHEETHEADER):longint;

  function ImageList_LoadImageW(hi:HINSTANCE; lpbmp:LPCWSTR; cx:longint; cGrow:longint; crMask:COLORREF; 
             uType:UINT; uFlags:UINT):HIMAGELIST;

  function CreateStatusWindowW(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND;

  procedure DrawStatusTextW(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT);

  function GetOpenFileNameW(_para1:LPOPENFILENAME):WINBOOL;

  function GetSaveFileNameW(_para1:LPOPENFILENAME):WINBOOL;

  function GetFileTitleW(_para1:LPCWSTR; _para2:LPWSTR; _para3:WORD):integer;

  function ChooseColorW(_para1:LPCHOOSECOLOR):WINBOOL;

  function ReplaceTextW(_para1:LPFINDREPLACE):HWND;

  function ChooseFontW(_para1:LPCHOOSEFONT):WINBOOL;

  function FindTextW(_para1:LPFINDREPLACE):HWND;

  function PrintDlgW(_para1:LPPRINTDLG):WINBOOL;

  function PageSetupDlgW(_para1:LPPAGESETUPDLG):WINBOOL;

  function CreateProcessW(lpApplicationName:LPCWSTR; lpCommandLine:LPWSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL; 
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCWSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL;

  procedure GetStartupInfoW(lpStartupInfo:LPSTARTUPINFO);

  function FindFirstFileW(lpFileName:LPCWSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE;

  function FindNextFileW(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL;

  function GetVersionExW(lpVersionInformation:LPOSVERSIONINFO):WINBOOL;

  { was #define dname(params) def_expr }
  function CreateWindowW(lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogW(hInstance:HINSTANCE; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogIndirectW(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function DialogBoxW(hInstance:HINSTANCE; lpTemplate:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  { was #define dname(params) def_expr }
  function DialogBoxIndirectW(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

(* Const before type ignored *)
  function CreateDCW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC;

  function CreateFontA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCSTR):HFONT;

  function VerInstallFileW(uFlags:DWORD; szSrcFileName:LPWSTR; szDestFileName:LPWSTR; szSrcDir:LPWSTR; szDestDir:LPWSTR; 
             szCurDir:LPWSTR; szTmpFile:LPWSTR; lpuTmpFileLen:PUINT):DWORD;

  function GetFileVersionInfoSizeW(lptstrFilename:LPWSTR; lpdwHandle:LPDWORD):DWORD;

  function GetFileVersionInfoW(lptstrFilename:LPWSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL;

  function VerLanguageNameW(wLang:DWORD; szLang:LPWSTR; nSize:DWORD):DWORD;

(* Const before type ignored *)
  function VerQueryValueW(pBlock:LPVOID; lpSubBlock:LPWSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL;

  function VerFindFileW(uFlags:DWORD; szFileName:LPWSTR; szWinDir:LPWSTR; szAppDir:LPWSTR; szCurDir:LPWSTR; 
             lpuCurDirLen:PUINT; szDestDir:LPWSTR; lpuDestDirLen:PUINT):DWORD;

(* Const before type ignored *)
  function RegSetValueExW(hKey:HKEY; lpValueName:LPCWSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE; 
             cbData:DWORD):LONG;

  function RegUnLoadKeyW(hKey:HKEY; lpSubKey:LPCWSTR):LONG;

  function InitiateSystemShutdownW(lpMachineName:LPWSTR; lpMessage:LPWSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL;

  function AbortSystemShutdownW(lpMachineName:LPWSTR):WINBOOL;

  function RegRestoreKeyW(hKey:HKEY; lpFile:LPCWSTR; dwFlags:DWORD):LONG;

  function RegSaveKeyW(hKey:HKEY; lpFile:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG;

  function RegSetValueW(hKey:HKEY; lpSubKey:LPCWSTR; dwType:DWORD; lpData:LPCWSTR; cbData:DWORD):LONG;

  function RegQueryValueW(hKey:HKEY; lpSubKey:LPCWSTR; lpValue:LPWSTR; lpcbValue:PLONG):LONG;

  function RegQueryMultipleValuesW(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPWSTR; ldwTotsize:LPDWORD):LONG;

  function RegQueryValueExW(hKey:HKEY; lpValueName:LPCWSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE; 
             lpcbData:LPDWORD):LONG;

  function RegReplaceKeyW(hKey:HKEY; lpSubKey:LPCWSTR; lpNewFile:LPCWSTR; lpOldFile:LPCWSTR):LONG;

  function RegConnectRegistryW(lpMachineName:LPWSTR; hKey:HKEY; phkResult:PHKEY):LONG;

  function RegCreateKeyW(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG;

  function RegCreateKeyExW(hKey:HKEY; lpSubKey:LPCWSTR; Reserved:DWORD; lpClass:LPWSTR; dwOptions:DWORD; 
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG;

  function RegDeleteKeyW(hKey:HKEY; lpSubKey:LPCWSTR):LONG;

  function RegDeleteValueW(hKey:HKEY; lpValueName:LPCWSTR):LONG;

  function RegEnumKeyW(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; cbName:DWORD):LONG;

  function RegEnumKeyExW(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; lpcbName:LPDWORD; lpReserved:LPDWORD; 
             lpClass:LPWSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function RegEnumValueW(hKey:HKEY; dwIndex:DWORD; lpValueName:LPWSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD; 
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG;

  function RegLoadKeyW(hKey:HKEY; lpSubKey:LPCWSTR; lpFile:LPCWSTR):LONG;

  function RegOpenKeyW(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG;

  function RegOpenKeyExW(hKey:HKEY; lpSubKey:LPCWSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG;

  function RegQueryInfoKeyW(hKey:HKEY; lpClass:LPWSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD; 
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD; 
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function CompareStringW(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCWSTR; cchCount1:longint; lpString2:LPCWSTR; 
             cchCount2:longint):longint;

  function LCMapStringW(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; 
             cchDest:longint):longint;

  function GetLocaleInfoW(Locale:LCID; LCType:LCTYPE; lpLCData:LPWSTR; cchData:longint):longint;

  function SetLocaleInfoW(Locale:LCID; LCType:LCTYPE; lpLCData:LPCWSTR):WINBOOL;

(* Const before type ignored *)
  function GetTimeFormatW(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCWSTR; lpTimeStr:LPWSTR; 
             cchTime:longint):longint;

(* Const before type ignored *)
  function GetDateFormatW(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCWSTR; lpDateStr:LPWSTR; 
             cchDate:longint):longint;

(* Const before type ignored *)
  function GetNumberFormatW(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPWSTR; 
             cchNumber:longint):longint;

(* Const before type ignored *)
  function GetCurrencyFormatW(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPWSTR; 
             cchCurrency:longint):longint;

  function EnumCalendarInfoW(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL;

  function EnumTimeFormatsW(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function EnumDateFormatsW(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function GetStringTypeExW(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function GetStringTypeW(dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function FoldStringW(dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; cchDest:longint):longint;

  function EnumSystemLocalesW(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function EnumSystemCodePagesW(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function PeekConsoleInputW(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL;

  function ReadConsoleInputW(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleInputW(hConsoleInput:HANDLE; var lpBuffer:INPUT_RECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL;

  function ReadConsoleOutputW(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleOutputW(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL;

  function ReadConsoleOutputCharacterW(hConsoleOutput:HANDLE; lpCharacter:LPWSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL;

  function WriteConsoleOutputCharacterW(hConsoleOutput:HANDLE; lpCharacter:LPCWSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function FillConsoleOutputCharacterW(hConsoleOutput:HANDLE; cCharacter:WCHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function ScrollConsoleScreenBufferW(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL;

  function GetConsoleTitleW(lpConsoleTitle:LPWSTR; nSize:DWORD):DWORD;

  function SetConsoleTitleW(lpConsoleTitle:LPCWSTR):WINBOOL;

  function ReadConsoleW(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleW(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL;

  function WNetAddConnectionW(lpRemoteName:LPCWSTR; lpPassword:LPCWSTR; lpLocalName:LPCWSTR):DWORD;

  function WNetAddConnection2W(lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD;

  function WNetAddConnection3W(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD;

  function WNetCancelConnectionW(lpName:LPCWSTR; fForce:WINBOOL):DWORD;

  function WNetCancelConnection2W(lpName:LPCWSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD;

  function WNetGetConnectionW(lpLocalName:LPCWSTR; lpRemoteName:LPWSTR; lpnLength:LPDWORD):DWORD;

  function WNetUseConnectionW(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCWSTR; lpPassword:LPCWSTR; dwFlags:DWORD; 
             lpAccessName:LPWSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD;

  function WNetSetConnectionW(lpName:LPCWSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD;

  function WNetConnectionDialog1W(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD;

  function WNetDisconnectDialog1W(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD;

  function WNetOpenEnumW(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD;

  function WNetEnumResourceW(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUniversalNameW(lpLocalPath:LPCWSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUserW(lpName:LPCWSTR; lpUserName:LPWSTR; lpnLength:LPDWORD):DWORD;

  function WNetGetProviderNameW(dwNetType:DWORD; lpProviderName:LPWSTR; lpBufferSize:LPDWORD):DWORD;

  function WNetGetNetworkInformationW(lpProvider:LPCWSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD;

  function WNetGetLastErrorW(lpError:LPDWORD; lpErrorBuf:LPWSTR; nErrorBufSize:DWORD; lpNameBuf:LPWSTR; nNameBufSize:DWORD):DWORD;

  function MultinetGetConnectionPerformanceW(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD;

  function ChangeServiceConfigW(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; 
             lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR; 
             lpDisplayName:LPCWSTR):WINBOOL;

  function CreateServiceW(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPCWSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD; 
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; 
             lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR):SC_HANDLE;

  function EnumDependentServicesW(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD; 
             lpServicesReturned:LPDWORD):WINBOOL;

  function EnumServicesStatusW(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; 
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL;

  function GetServiceKeyNameW(hSCManager:SC_HANDLE; lpDisplayName:LPCWSTR; lpServiceName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function GetServiceDisplayNameW(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function OpenSCManagerW(lpMachineName:LPCWSTR; lpDatabaseName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function OpenServiceW(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function QueryServiceConfigW(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function QueryServiceLockStatusW(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function RegisterServiceCtrlHandlerW(lpServiceName:LPCWSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE;

  function StartServiceCtrlDispatcherW(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL;

  function StartServiceW(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCWSTR):WINBOOL;

  { Extensions to OpenGL  }
  function wglUseFontBitmapsW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL;

  function wglUseFontOutlinesW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:FLOAT; 
             _para6:FLOAT; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL;

  { -------------------------------------  }
  { From shellapi.h in old Cygnus headers  }
  function DragQueryFileW(_para1:HDROP; _para2:cardinal; _para3:LPCWSTR; _para4:cardinal):cardinal;

  function ExtractAssociatedIconW(_para1:HINSTANCE; _para2:LPCWSTR; var _para3:WORD):HICON;

(* Const before type ignored *)
  function ExtractIconW(_para1:HINSTANCE; _para2:LPCWSTR; _para3:cardinal):HICON;

(* Const before type ignored *)
(* Const before type ignored *)
  function FindExecutableW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR):HINSTANCE;

(* Const before type ignored *)
(* Const before type ignored *)
  function ShellAboutW(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:HICON):longint;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function ShellExecuteW(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR; _para5:LPCWSTR; 
             _para6:longint):HINSTANCE;

  { end of stuff from shellapi.h in old Cygnus headers  }
  { --------------------------------------------------  }
  { From ddeml.h in old Cygnus headers  }
  function DdeCreateStringHandleW(_para1:DWORD; _para2:LPCWSTR; _para3:longint):HSZ;

  function DdeInitializeW(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT;

  function DdeQueryStringW(_para1:DWORD; _para2:HSZ; _para3:LPCWSTR; _para4:DWORD; _para5:longint):DWORD;

  { end of stuff from ddeml.h in old Cygnus headers  }
  { -----------------------------------------------  }
  function LogonUserW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR; _para4:DWORD; _para5:DWORD; 
             var _para6:HANDLE):WINBOOL;

  function CreateProcessAsUserW(_para1:HANDLE; _para2:LPCWSTR; _para3:LPWSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES; 
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCWSTR; var _para10:STARTUPINFO; 
             var _para11:PROCESS_INFORMATION):WINBOOL;

{ C++ end of extern C conditionnal removed }
  { __cplusplus  }
{$endif}
  { _GNU_H_WINDOWS32_UNICODEFUNCTIONS  }

{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

  function GetBinaryTypeW(lpApplicationName:LPCWSTR; lpBinaryType:LPDWORD):WINBOOL; external 'kernel32.dll' name 'GetBinaryTypeW';

  function GetShortPathNameW(lpszLongPath:LPCWSTR; lpszShortPath:LPWSTR; cchBuffer:DWORD):DWORD; external 'kernel32.dll' name 'GetShortPathNameW';

  function GetEnvironmentStringsW:LPWSTR; external 'kernel32.dll' name 'GetEnvironmentStringsW';

  function FreeEnvironmentStringsW(_para1:LPWSTR):WINBOOL; external 'kernel32.dll' name 'FreeEnvironmentStringsW';

  function FormatMessageW(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPWSTR; 
             nSize:DWORD; var Arguments:va_list):DWORD; external 'kernel32.dll' name 'FormatMessageW';

  function CreateMailslotW(lpName:LPCWSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32.dll' name 'CreateMailslotW';

  function lstrcmpW(lpString1:LPCWSTR; lpString2:LPCWSTR):longint; external 'kernel32.dll' name 'lstrcmpW';

  function lstrcmpiW(lpString1:LPCWSTR; lpString2:LPCWSTR):longint; external 'kernel32.dll' name 'lstrcmpiW';

  function lstrcpynW(lpString1:LPWSTR; lpString2:LPCWSTR; iMaxLength:longint):LPWSTR; external 'kernel32.dll' name 'lstrcpynW';

  function lstrcpyW(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR; external 'kernel32.dll' name 'lstrcpyW';

  function lstrcatW(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR; external 'kernel32.dll' name 'lstrcatW';

  function lstrlenW(lpString:LPCWSTR):longint; external 'kernel32.dll' name 'lstrlenW';

  function CreateMutexW(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'CreateMutexW';

  function OpenMutexW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'OpenMutexW';

  function CreateEventW(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'CreateEventW';

  function OpenEventW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'OpenEventW';

  function CreateSemaphoreW(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'CreateSemaphoreW';

  function OpenSemaphoreW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'OpenSemaphoreW';

  function CreateFileMappingW(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD; 
             lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'CreateFileMappingW';

  function OpenFileMappingW(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external 'kernel32.dll' name 'OpenFileMappingW';

  function GetLogicalDriveStringsW(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external 'kernel32.dll' name 'GetLogicalDriveStringsW';

  function LoadLibraryW(lpLibFileName:LPCWSTR):HINSTANCE; external 'kernel32.dll' name 'LoadLibraryW';

  function LoadLibraryExW(lpLibFileName:LPCWSTR; hFile:HANDLE; dwFlags:DWORD):HINSTANCE; external 'kernel32.dll' name 'LoadLibraryExW';

  function GetModuleFileNameW(hModule:HINSTANCE; lpFilename:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'GetModuleFileNameW';

  function GetModuleHandleW(lpModuleName:LPCWSTR):HMODULE; external 'kernel32.dll' name 'GetModuleHandleW';

  procedure FatalAppExitW(uAction:UINT; lpMessageText:LPCWSTR); external 'kernel32.dll' name 'FatalAppExitW';

  function GetCommandLineW:LPWSTR; external 'kernel32.dll' name 'GetCommandLineW';

  function GetEnvironmentVariableW(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'GetEnvironmentVariableW';

  function SetEnvironmentVariableW(lpName:LPCWSTR; lpValue:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'SetEnvironmentVariableW';

  function ExpandEnvironmentStringsW(lpSrc:LPCWSTR; lpDst:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'ExpandEnvironmentStringsW';

  procedure OutputDebugStringW(lpOutputString:LPCWSTR); external 'kernel32.dll' name 'OutputDebugStringW';

  function FindResourceW(hModule:HINSTANCE; lpName:LPCWSTR; lpType:LPCWSTR):HRSRC; external 'kernel32.dll' name 'FindResourceW';

  function FindResourceExW(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD):HRSRC; external 'kernel32.dll' name 'FindResourceExW';

  function EnumResourceTypesW(hModule:HINSTANCE; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL; external 'kernel32.dll' name 'EnumResourceTypesW';

  function EnumResourceNamesW(hModule:HINSTANCE; lpType:LPCWSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL; external 'kernel32.dll' name 'EnumResourceNamesW';

  function EnumResourceLanguagesW(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL; external 'kernel32.dll' name 'EnumResourceLanguagesW';

  function BeginUpdateResourceW(pFileName:LPCWSTR; bDeleteExistingResources:WINBOOL):HANDLE; external 'kernel32.dll' name 'BeginUpdateResourceW';

  function UpdateResourceW(hUpdate:HANDLE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD; lpData:LPVOID; 
             cbData:DWORD):WINBOOL; external 'kernel32.dll' name 'UpdateResourceW';

  function EndUpdateResourceW(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL; external 'kernel32.dll' name 'EndUpdateResourceW';

  function GlobalAddAtomW(lpString:LPCWSTR):ATOM; external 'kernel32.dll' name 'GlobalAddAtomW';

  function GlobalFindAtomW(lpString:LPCWSTR):ATOM; external 'kernel32.dll' name 'GlobalFindAtomW';

  function GlobalGetAtomNameW(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT; external 'kernel32.dll' name 'GlobalGetAtomNameW';

  function AddAtomW(lpString:LPCWSTR):ATOM; external 'kernel32.dll' name 'AddAtomW';

  function FindAtomW(lpString:LPCWSTR):ATOM; external 'kernel32.dll' name 'FindAtomW';

  function GetAtomNameW(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT; external 'kernel32.dll' name 'GetAtomNameW';

  function GetProfileIntW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT):UINT; external 'kernel32.dll' name 'GetProfileIntW';

  function GetProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'GetProfileStringW';

  function WriteProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'WriteProfileStringW';

  function GetProfileSectionW(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'GetProfileSectionW';

  function WriteProfileSectionW(lpAppName:LPCWSTR; lpString:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'WriteProfileSectionW';

  function GetPrivateProfileIntW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT; lpFileName:LPCWSTR):UINT; external 'kernel32.dll' name 'GetPrivateProfileIntW';

  function GetPrivateProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; 
             lpFileName:LPCWSTR):DWORD; external 'kernel32.dll' name 'GetPrivateProfileStringW';

  function WritePrivateProfileStringW(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'WritePrivateProfileStringW';

  function GetPrivateProfileSectionW(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; lpFileName:LPCWSTR):DWORD; external 'kernel32.dll' name 'GetPrivateProfileSectionW';

  function WritePrivateProfileSectionW(lpAppName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'WritePrivateProfileSectionW';

  function GetDriveTypeW(lpRootPathName:LPCWSTR):UINT; external 'kernel32.dll' name 'GetDriveTypeW';

  function GetSystemDirectoryW(lpBuffer:LPWSTR; uSize:UINT):UINT; external 'kernel32.dll' name 'GetSystemDirectoryW';

  function GetTempPathW(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external 'kernel32.dll' name 'GetTempPathW';

  function GetTempFileNameW(lpPathName:LPCWSTR; lpPrefixString:LPCWSTR; uUnique:UINT; lpTempFileName:LPWSTR):UINT; external 'kernel32.dll' name 'GetTempFileNameW';

  function GetWindowsDirectoryW(lpBuffer:LPWSTR; uSize:UINT):UINT; external 'kernel32.dll' name 'GetWindowsDirectoryW';

  function SetCurrentDirectoryW(lpPathName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'SetCurrentDirectoryW';

  function GetCurrentDirectoryW(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external 'kernel32.dll' name 'GetCurrentDirectoryW';

  function GetDiskFreeSpaceW(lpRootPathName:LPCWSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL; external 'kernel32.dll' name 'GetDiskFreeSpaceW';

  function CreateDirectoryW(lpPathName:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32.dll' name 'CreateDirectoryW';

  function CreateDirectoryExW(lpTemplateDirectory:LPCWSTR; lpNewDirectory:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32.dll' name 'CreateDirectoryExW';

  function RemoveDirectoryW(lpPathName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'RemoveDirectoryW';

  function GetFullPathNameW(lpFileName:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; var lpFilePart:LPWSTR):DWORD; external 'kernel32.dll' name 'GetFullPathNameW';

  function DefineDosDeviceW(dwFlags:DWORD; lpDeviceName:LPCWSTR; lpTargetPath:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'DefineDosDeviceW';

  function QueryDosDeviceW(lpDeviceName:LPCWSTR; lpTargetPath:LPWSTR; ucchMax:DWORD):DWORD; external 'kernel32.dll' name 'QueryDosDeviceW';

  function CreateFileW(lpFileName:LPCWSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD; 
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external 'kernel32.dll' name 'CreateFileW';

  function SetFileAttributesW(lpFileName:LPCWSTR; dwFileAttributes:DWORD):WINBOOL; external 'kernel32.dll' name 'SetFileAttributesW';

  function GetFileAttributesW(lpFileName:LPCWSTR):DWORD; external 'kernel32.dll' name 'GetFileAttributesW';

  function GetCompressedFileSizeW(lpFileName:LPCWSTR; lpFileSizeHigh:LPDWORD):DWORD; external 'kernel32.dll' name 'GetCompressedFileSizeW';

  function DeleteFileW(lpFileName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'DeleteFileW';

  function SearchPathW(lpPath:LPCWSTR; lpFileName:LPCWSTR; lpExtension:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; 
             var lpFilePart:LPWSTR):DWORD; external 'kernel32.dll' name 'SearchPathW';

  function CopyFileW(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; bFailIfExists:WINBOOL):WINBOOL; external 'kernel32.dll' name 'CopyFileW';

  function MoveFileW(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'MoveFileW';

  function MoveFileExW(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; dwFlags:DWORD):WINBOOL; external 'kernel32.dll' name 'MoveFileExW';

  function CreateNamedPipeW(lpName:LPCWSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD; 
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32.dll' name 'CreateNamedPipeW';

  function GetNamedPipeHandleStateW(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD; 
             lpUserName:LPWSTR; nMaxUserNameSize:DWORD):WINBOOL; external 'kernel32.dll' name 'GetNamedPipeHandleStateW';

  function CallNamedPipeW(lpNamedPipeName:LPCWSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD; 
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL; external 'kernel32.dll' name 'CallNamedPipeW';

  function WaitNamedPipeW(lpNamedPipeName:LPCWSTR; nTimeOut:DWORD):WINBOOL; external 'kernel32.dll' name 'WaitNamedPipeW';

  function SetVolumeLabelW(lpRootPathName:LPCWSTR; lpVolumeName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'SetVolumeLabelW';

  function GetVolumeInformationW(lpRootPathName:LPCWSTR; lpVolumeNameBuffer:LPWSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD; 
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPWSTR; nFileSystemNameSize:DWORD):WINBOOL; external 'kernel32.dll' name 'GetVolumeInformationW';

  function ClearEventLogW(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL; external 'advapi32.dll' name 'ClearEventLogW';

  function BackupEventLogW(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL; external 'advapi32.dll' name 'BackupEventLogW';

  function OpenEventLogW(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE; external 'advapi32.dll' name 'OpenEventLogW';

  function RegisterEventSourceW(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE; external 'advapi32.dll' name 'RegisterEventSourceW';

  function OpenBackupEventLogW(lpUNCServerName:LPCWSTR; lpFileName:LPCWSTR):HANDLE; external 'advapi32.dll' name 'OpenBackupEventLogW';

  function ReadEventLogW(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; 
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL; external 'advapi32.dll' name 'ReadEventLogW';

  function ReportEventW(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID; 
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCWSTR; lpRawData:LPVOID):WINBOOL; external 'advapi32.dll' name 'ReportEventW';

  function AccessCheckAndAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR; 
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL; 
             pfGenerateOnClose:LPBOOL):WINBOOL; external 'advapi32.dll' name 'AccessCheckAndAuditAlarmW';

  function ObjectOpenAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR; 
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL; 
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL; external 'advapi32.dll' name 'ObjectOpenAuditAlarmW';

  function ObjectPrivilegeAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET; 
             AccessGranted:WINBOOL):WINBOOL; external 'advapi32.dll' name 'ObjectPrivilegeAuditAlarmW';

  function ObjectCloseAuditAlarmW(SubsystemName:LPCWSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL; external 'advapi32.dll' name 'ObjectCloseAuditAlarmW';

  function PrivilegedServiceAuditAlarmW(SubsystemName:LPCWSTR; ServiceName:LPCWSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL; external 'advapi32.dll' name 'PrivilegedServiceAuditAlarmW';

  function SetFileSecurityW(lpFileName:LPCWSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32.dll' name 'SetFileSecurityW';

  function GetFileSecurityW(lpFileName:LPCWSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'advapi32.dll' name 'GetFileSecurityW';

  function FindFirstChangeNotificationW(lpPathName:LPCWSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE; external 'kernel32.dll' name 'FindFirstChangeNotificationW';

  function IsBadStringPtrW(lpsz:LPCWSTR; ucchMax:UINT):WINBOOL; external 'kernel32.dll' name 'IsBadStringPtrW';

  function LookupAccountSidW(lpSystemName:LPCWSTR; Sid:PSID; Name:LPWSTR; cbName:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32.dll' name 'LookupAccountSidW';

  function LookupAccountNameW(lpSystemName:LPCWSTR; lpAccountName:LPCWSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32.dll' name 'LookupAccountNameW';

  function LookupPrivilegeValueW(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpLuid:PLUID):WINBOOL; external 'advapi32.dll' name 'LookupPrivilegeValueW';

  function LookupPrivilegeNameW(lpSystemName:LPCWSTR; lpLuid:PLUID; lpName:LPWSTR; cbName:LPDWORD):WINBOOL; external 'advapi32.dll' name 'LookupPrivilegeNameW';

  function LookupPrivilegeDisplayNameW(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpDisplayName:LPWSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL; external 'advapi32.dll' name 'LookupPrivilegeDisplayNameW';

  function BuildCommDCBW(lpDef:LPCWSTR; lpDCB:LPDCB):WINBOOL; external 'kernel32.dll' name 'BuildCommDCBW';

  function BuildCommDCBAndTimeoutsW(lpDef:LPCWSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external 'kernel32.dll' name 'BuildCommDCBAndTimeoutsW';

  function CommConfigDialogW(lpszName:LPCWSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL; external 'kernel32.dll' name 'CommConfigDialogW';

  function GetDefaultCommConfigW(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external 'kernel32.dll' name 'GetDefaultCommConfigW';

  function SetDefaultCommConfigW(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external 'kernel32.dll' name 'SetDefaultCommConfigW';

  function GetComputerNameW(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL; external 'kernel32.dll' name 'GetComputerNameW';

  function SetComputerNameW(lpComputerName:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'SetComputerNameW';

  function GetUserNameW(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL; external 'advapi32.dll' name 'GetUserNameW';

  function wvsprintfW(_para1:LPWSTR; _para2:LPCWSTR; arglist:va_list):longint; external 'user32.dll' name 'wvsprintfW';

  {function wsprintfW(_para1:LPWSTR; _para2:LPCWSTR; ...):longint;CDECL; external 'user32.dll' name 'wsprintfW';}

  function LoadKeyboardLayoutW(pwszKLID:LPCWSTR; Flags:UINT):HKL; external 'user32.dll' name 'LoadKeyboardLayoutW';

  function GetKeyboardLayoutNameW(pwszKLID:LPWSTR):WINBOOL; external 'user32.dll' name 'GetKeyboardLayoutNameW';

  function CreateDesktopW(lpszDesktop:LPWSTR; lpszDevice:LPWSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD; 
             lpsa:LPSECURITY_ATTRIBUTES):HDESK; external 'user32.dll' name 'CreateDesktopW';

  function OpenDesktopW(lpszDesktop:LPWSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external 'user32.dll' name 'OpenDesktopW';

  function EnumDesktopsW(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL; external 'user32.dll' name 'EnumDesktopsW';

  function CreateWindowStationW(lpwinsta:LPWSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA; external 'user32.dll' name 'CreateWindowStationW';

  function OpenWindowStationW(lpszWinSta:LPWSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA; external 'user32.dll' name 'OpenWindowStationW';

  function EnumWindowStationsW(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL; external 'user32.dll' name 'EnumWindowStationsW';

  function GetUserObjectInformationW(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'user32.dll' name 'GetUserObjectInformationW';

  function SetUserObjectInformationW(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL; external 'user32.dll' name 'SetUserObjectInformationW';

  function RegisterWindowMessageW(lpString:LPCWSTR):UINT; external 'user32.dll' name 'RegisterWindowMessageW';

  function GetMessageW(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL; external 'user32.dll' name 'GetMessageW';

  function DispatchMessageW(var lpMsg:MSG):LONG; external 'user32.dll' name 'DispatchMessageW';

  function PeekMessageW(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external 'user32.dll' name 'PeekMessageW';

  function SendMessageW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32.dll' name 'SendMessageW';

  function SendMessageTimeoutW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT; 
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external 'user32.dll' name 'SendMessageTimeoutW';

  function SendNotifyMessageW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32.dll' name 'SendNotifyMessageW';

  function SendMessageCallbackW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC; 
             dwData:DWORD):WINBOOL; external 'user32.dll' name 'SendMessageCallbackW';

  function PostMessageW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32.dll' name 'PostMessageW';

  function PostThreadMessageW(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32.dll' name 'PostThreadMessageW';

  function DefWindowProcW(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32.dll' name 'DefWindowProcW';

  function CallWindowProcW(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32.dll' name 'CallWindowProcW';

  function RegisterClassW(var lpWndClass:WNDCLASS):ATOM; external 'user32.dll' name 'RegisterClassW';

  function UnregisterClassW(lpClassName:LPCWSTR; hInstance:HINSTANCE):WINBOOL; external 'user32.dll' name 'UnregisterClassW';

  function GetClassInfoW(hInstance:HINSTANCE; lpClassName:LPCWSTR; lpWndClass:LPWNDCLASS):WINBOOL; external 'user32.dll' name 'GetClassInfoW';

  function RegisterClassExW(var _para1:WNDCLASSEX):ATOM; external 'user32.dll' name 'RegisterClassExW';

  function GetClassInfoExW(_para1:HINSTANCE; _para2:LPCWSTR; _para3:LPWNDCLASSEX):WINBOOL; external 'user32.dll' name 'GetClassInfoExW';

  function CreateWindowExW(dwExStyle:DWORD; lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint; 
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND; external 'user32.dll' name 'CreateWindowExW';

  function CreateDialogParamW(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32.dll' name 'CreateDialogParamW';

  function CreateDialogIndirectParamW(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32.dll' name 'CreateDialogIndirectParamW';

  function DialogBoxParamW(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32.dll' name 'DialogBoxParamW';

  function DialogBoxIndirectParamW(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32.dll' name 'DialogBoxIndirectParamW';

  function SetDlgItemTextW(hDlg:HWND; nIDDlgItem:longint; lpString:LPCWSTR):WINBOOL; external 'user32.dll' name 'SetDlgItemTextW';

  function GetDlgItemTextW(hDlg:HWND; nIDDlgItem:longint; lpString:LPWSTR; nMaxCount:longint):UINT; external 'user32.dll' name 'GetDlgItemTextW';

  function SendDlgItemMessageW(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG; external 'user32.dll' name 'SendDlgItemMessageW';

  function DefDlgProcW(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32.dll' name 'DefDlgProcW';

  function CallMsgFilterW(lpMsg:LPMSG; nCode:longint):WINBOOL; external 'user32.dll' name 'CallMsgFilterW';

  function RegisterClipboardFormatW(lpszFormat:LPCWSTR):UINT; external 'user32.dll' name 'RegisterClipboardFormatW';

  function GetClipboardFormatNameW(format:UINT; lpszFormatName:LPWSTR; cchMaxCount:longint):longint; external 'user32.dll' name 'GetClipboardFormatNameW';

  function CharToOemW(lpszSrc:LPCWSTR; lpszDst:LPSTR):WINBOOL; external 'user32.dll' name 'CharToOemW';

  function OemToCharW(lpszSrc:LPCSTR; lpszDst:LPWSTR):WINBOOL; external 'user32.dll' name 'OemToCharW';

  function CharToOemBuffW(lpszSrc:LPCWSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external 'user32.dll' name 'CharToOemBuffW';

  function OemToCharBuffW(lpszSrc:LPCSTR; lpszDst:LPWSTR; cchDstLength:DWORD):WINBOOL; external 'user32.dll' name 'OemToCharBuffW';

  function CharUpperW(lpsz:LPWSTR):LPWSTR; external 'user32.dll' name 'CharUpperW';

  function CharUpperBuffW(lpsz:LPWSTR; cchLength:DWORD):DWORD; external 'user32.dll' name 'CharUpperBuffW';

  function CharLowerW(lpsz:LPWSTR):LPWSTR; external 'user32.dll' name 'CharLowerW';

  function CharLowerBuffW(lpsz:LPWSTR; cchLength:DWORD):DWORD; external 'user32.dll' name 'CharLowerBuffW';

  function CharNextW(lpsz:LPCWSTR):LPWSTR; external 'user32.dll' name 'CharNextW';

  function CharPrevW(lpszStart:LPCWSTR; lpszCurrent:LPCWSTR):LPWSTR; external 'user32.dll' name 'CharPrevW';

  function IsCharAlphaW(ch:WCHAR):WINBOOL; external 'user32.dll' name 'IsCharAlphaW';

  function IsCharAlphaNumericW(ch:WCHAR):WINBOOL; external 'user32.dll' name 'IsCharAlphaNumericW';

  function IsCharUpperW(ch:WCHAR):WINBOOL; external 'user32.dll' name 'IsCharUpperW';

  function IsCharLowerW(ch:WCHAR):WINBOOL; external 'user32.dll' name 'IsCharLowerW';

  function GetKeyNameTextW(lParam:LONG; lpString:LPWSTR; nSize:longint):longint; external 'user32.dll' name 'GetKeyNameTextW';

  function VkKeyScanW(ch:WCHAR):SHORT; external 'user32.dll' name 'VkKeyScanW';

  function VkKeyScanExW(ch:WCHAR; dwhkl:HKL):SHORT; external 'user32.dll' name 'VkKeyScanExW';

  function MapVirtualKeyW(uCode:UINT; uMapType:UINT):UINT; external 'user32.dll' name 'MapVirtualKeyW';

  function MapVirtualKeyExW(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT; external 'user32.dll' name 'MapVirtualKeyExW';

  function LoadAcceleratorsW(hInstance:HINSTANCE; lpTableName:LPCWSTR):HACCEL; external 'user32.dll' name 'LoadAcceleratorsW';

  function CreateAcceleratorTableW(_para1:LPACCEL; _para2:longint):HACCEL; external 'user32.dll' name 'CreateAcceleratorTableW';

  function CopyAcceleratorTableW(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint; external 'user32.dll' name 'CopyAcceleratorTableW';

  function TranslateAcceleratorW(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint; external 'user32.dll' name 'TranslateAcceleratorW';

  function LoadMenuW(hInstance:HINSTANCE; lpMenuName:LPCWSTR):HMENU; external 'user32.dll' name 'LoadMenuW';

  function LoadMenuIndirectW(var lpMenuTemplate:MENUTEMPLATE):HMENU; external 'user32.dll' name 'LoadMenuIndirectW';

  function ChangeMenuW(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCWSTR; cmdInsert:UINT; flags:UINT):WINBOOL; external 'user32.dll' name 'ChangeMenuW';

  function GetMenuStringW(hMenu:HMENU; uIDItem:UINT; lpString:LPWSTR; nMaxCount:longint; uFlag:UINT):longint; external 'user32.dll' name 'GetMenuStringW';

  function InsertMenuW(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external 'user32.dll' name 'InsertMenuW';

  function AppendMenuW(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external 'user32.dll' name 'AppendMenuW';

  function ModifyMenuW(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external 'user32.dll' name 'ModifyMenuW';

  function InsertMenuItemW(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32.dll' name 'InsertMenuItemW';

  function GetMenuItemInfoW(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL; external 'user32.dll' name 'GetMenuItemInfoW';

  function SetMenuItemInfoW(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32.dll' name 'SetMenuItemInfoW';

  function DrawTextW(hDC:HDC; lpString:LPCWSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint; external 'user32.dll' name 'DrawTextW';

  function DrawTextExW(_para1:HDC; _para2:LPWSTR; _para3:longint; _para4:LPRECT; _para5:UINT; 
             _para6:LPDRAWTEXTPARAMS):longint; external 'user32.dll' name 'DrawTextExW';

  function GrayStringW(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint; 
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL; external 'user32.dll' name 'GrayStringW';

  function DrawStateW(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM; 
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL; external 'user32.dll' name 'DrawStateW';

  function TabbedTextOutW(hDC:HDC; X:longint; Y:longint; lpString:LPCWSTR; nCount:longint; 
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG; external 'user32.dll' name 'TabbedTextOutW';

  function GetTabbedTextExtentW(hDC:HDC; lpString:LPCWSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD; external 'user32.dll' name 'GetTabbedTextExtentW';

  function SetPropW(hWnd:HWND; lpString:LPCWSTR; hData:HANDLE):WINBOOL; external 'user32.dll' name 'SetPropW';

  function GetPropW(hWnd:HWND; lpString:LPCWSTR):HANDLE; external 'user32.dll' name 'GetPropW';

  function RemovePropW(hWnd:HWND; lpString:LPCWSTR):HANDLE; external 'user32.dll' name 'RemovePropW';

  function EnumPropsExW(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint; external 'user32.dll' name 'EnumPropsExW';

  function EnumPropsW(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint; external 'user32.dll' name 'EnumPropsW';

  function SetWindowTextW(hWnd:HWND; lpString:LPCWSTR):WINBOOL; external 'user32.dll' name 'SetWindowTextW';

  function GetWindowTextW(hWnd:HWND; lpString:LPWSTR; nMaxCount:longint):longint; external 'user32.dll' name 'GetWindowTextW';

  function GetWindowTextLengthW(hWnd:HWND):longint; external 'user32.dll' name 'GetWindowTextLengthW';

  function MessageBoxW(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT):longint; external 'user32.dll' name 'MessageBoxW';

  function MessageBoxExW(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT; wLanguageId:WORD):longint; external 'user32.dll' name 'MessageBoxExW';

  function MessageBoxIndirectW(_para1:LPMSGBOXPARAMS):longint; external 'user32.dll' name 'MessageBoxIndirectW';

  function GetWindowLongW(hWnd:HWND; nIndex:longint):LONG; external 'user32.dll' name 'GetWindowLongW';

  function SetWindowLongW(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG; external 'user32.dll' name 'SetWindowLongW';

  function GetClassLongW(hWnd:HWND; nIndex:longint):DWORD; external 'user32.dll' name 'GetClassLongW';

  function SetClassLongW(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD; external 'user32.dll' name 'SetClassLongW';

  function FindWindowW(lpClassName:LPCWSTR; lpWindowName:LPCWSTR):HWND; external 'user32.dll' name 'FindWindowW';

  function FindWindowExW(_para1:HWND; _para2:HWND; _para3:LPCWSTR; _para4:LPCWSTR):HWND; external 'user32.dll' name 'FindWindowExW';

  function GetClassNameW(hWnd:HWND; lpClassName:LPWSTR; nMaxCount:longint):longint; external 'user32.dll' name 'GetClassNameW';

  function SetWindowsHookExW(idHook:longint; lpfn:HOOKPROC; hmod:HINSTANCE; dwThreadId:DWORD):HHOOK; external 'user32.dll' name 'SetWindowsHookExW';

  function LoadBitmapW(hInstance:HINSTANCE; lpBitmapName:LPCWSTR):HBITMAP; external 'user32.dll' name 'LoadBitmapW';

  function LoadCursorW(hInstance:HINSTANCE; lpCursorName:LPCWSTR):HCURSOR; external 'user32.dll' name 'LoadCursorW';

  function LoadCursorFromFileW(lpFileName:LPCWSTR):HCURSOR; external 'user32.dll' name 'LoadCursorFromFileW';

  function LoadIconW(hInstance:HINSTANCE; lpIconName:LPCWSTR):HICON; external 'user32.dll' name 'LoadIconW';

  function LoadImageW(_para1:HINSTANCE; _para2:LPCWSTR; _para3:UINT; _para4:longint; _para5:longint; 
             _para6:UINT):HANDLE; external 'user32.dll' name 'LoadImageW';

  function LoadStringW(hInstance:HINSTANCE; uID:UINT; lpBuffer:LPWSTR; nBufferMax:longint):longint; external 'user32.dll' name 'LoadStringW';

  function IsDialogMessageW(hDlg:HWND; lpMsg:LPMSG):WINBOOL; external 'user32.dll' name 'IsDialogMessageW';

  function DlgDirListW(hDlg:HWND; lpPathSpec:LPWSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint; external 'user32.dll' name 'DlgDirListW';

  function DlgDirSelectExW(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDListBox:longint):WINBOOL; external 'user32.dll' name 'DlgDirSelectExW';

  function DlgDirListComboBoxW(hDlg:HWND; lpPathSpec:LPWSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint; external 'user32.dll' name 'DlgDirListComboBoxW';

  function DlgDirSelectComboBoxExW(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDComboBox:longint):WINBOOL; external 'user32.dll' name 'DlgDirSelectComboBoxExW';

  function DefFrameProcW(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32.dll' name 'DefFrameProcW';

  function DefMDIChildProcW(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32.dll' name 'DefMDIChildProcW';

  function CreateMDIWindowW(lpClassName:LPWSTR; lpWindowName:LPWSTR; dwStyle:DWORD; X:longint; Y:longint; 
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINSTANCE; lParam:LPARAM):HWND; external 'user32.dll' name 'CreateMDIWindowW';

  function WinHelpW(hWndMain:HWND; lpszHelp:LPCWSTR; uCommand:UINT; dwData:DWORD):WINBOOL; external 'user32.dll' name 'WinHelpW';

  function ChangeDisplaySettingsW(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG; external 'user32.dll' name 'ChangeDisplaySettingsW';

  function EnumDisplaySettingsW(lpszDeviceName:LPCWSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL; external 'user32.dll' name 'EnumDisplaySettingsW';

  function SystemParametersInfoW(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL; external 'user32.dll' name 'SystemParametersInfoW';

  function AddFontResourceW(_para1:LPCWSTR):longint; external 'gdi32.dll' name 'AddFontResourceW';

  function CopyMetaFileW(_para1:HMETAFILE; _para2:LPCWSTR):HMETAFILE; external 'gdi32.dll' name 'CopyMetaFileW';

  function CreateFontIndirectW(var _para1:LOGFONT):HFONT; external 'gdi32.dll' name 'CreateFontIndirectW';

  function CreateFontW(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCWSTR):HFONT; external 'gdi32.dll' name 'CreateFontW';

  function CreateICW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC; external 'gdi32.dll' name 'CreateICW';

  function CreateMetaFileW(_para1:LPCWSTR):HDC; external 'gdi32.dll' name 'CreateMetaFileW';

  function CreateScalableFontResourceW(_para1:DWORD; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR):WINBOOL; external 'gdi32.dll' name 'CreateScalableFontResourceW';

  function DeviceCapabilitiesW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:WORD; _para4:LPWSTR; var _para5:DEVMODE):longint; external 'winspool.drv' name 'DeviceCapabilitiesW';

  function EnumFontFamiliesExW(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint; external 'gdi32.dll' name 'EnumFontFamiliesExW';

  function EnumFontFamiliesW(_para1:HDC; _para2:LPCWSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint; external 'gdi32.dll' name 'EnumFontFamiliesW';

  function EnumFontsW(_para1:HDC; _para2:LPCWSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint; external 'gdi32.dll' name 'EnumFontsW';

  function GetCharWidthW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32.dll' name 'GetCharWidthW';

  function GetCharWidth32W(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32.dll' name 'GetCharWidth32W';

  function GetCharWidthFloatW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL; external 'gdi32.dll' name 'GetCharWidthFloatW';

  function GetCharABCWidthsW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external 'gdi32.dll' name 'GetCharABCWidthsW';

  function GetCharABCWidthsFloatW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL; external 'gdi32.dll' name 'GetCharABCWidthsFloatW';

  function GetGlyphOutlineW(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD; 
             _para6:LPVOID; var _para7:MAT2):DWORD; external 'gdi32.dll' name 'GetGlyphOutlineW';

  function GetMetaFileW(_para1:LPCWSTR):HMETAFILE; external 'gdi32.dll' name 'GetMetaFileW';

  function GetOutlineTextMetricsW(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT; external 'gdi32.dll' name 'GetOutlineTextMetricsW';

  function GetTextExtentPointW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32.dll' name 'GetTextExtentPointW';

  function GetTextExtentPoint32W(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32.dll' name 'GetTextExtentPoint32W';

  function GetTextExtentExPointW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPINT; 
             _para6:LPINT; _para7:LPSIZE):WINBOOL; external 'gdi32.dll' name 'GetTextExtentExPointW';

  function GetCharacterPlacementW(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS; 
             _para6:DWORD):DWORD; external 'gdi32.dll' name 'GetCharacterPlacementW';

  function ResetDCW(_para1:HDC; var _para2:DEVMODE):HDC; external 'gdi32.dll' name 'ResetDCW';

  function RemoveFontResourceW(_para1:LPCWSTR):WINBOOL; external 'gdi32.dll' name 'RemoveFontResourceW';

  function CopyEnhMetaFileW(_para1:HENHMETAFILE; _para2:LPCWSTR):HENHMETAFILE; external 'gdi32.dll' name 'CopyEnhMetaFileW';

  function CreateEnhMetaFileW(_para1:HDC; _para2:LPCWSTR; var _para3:RECT; _para4:LPCWSTR):HDC; external 'gdi32.dll' name 'CreateEnhMetaFileW';

  function GetEnhMetaFileW(_para1:LPCWSTR):HENHMETAFILE; external 'gdi32.dll' name 'GetEnhMetaFileW';

  function GetEnhMetaFileDescriptionW(_para1:HENHMETAFILE; _para2:UINT; _para3:LPWSTR):UINT; external 'gdi32.dll' name 'GetEnhMetaFileDescriptionW';

  function GetTextMetricsW(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL; external 'gdi32.dll' name 'GetTextMetricsW';

  function StartDocW(_para1:HDC; var _para2:DOCINFO):longint; external 'gdi32.dll' name 'StartDocW';

  function GetObjectW(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint; external 'gdi32.dll' name 'GetObjectW';

  function TextOutW(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCWSTR; _para5:longint):WINBOOL; external 'gdi32.dll' name 'TextOutW';

  function ExtTextOutW(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT; 
             _para6:LPCWSTR; _para7:UINT; var _para8:INT):WINBOOL; external 'gdi32.dll' name 'ExtTextOutW';

  function PolyTextOutW(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL; external 'gdi32.dll' name 'PolyTextOutW';

  function GetTextFaceW(_para1:HDC; _para2:longint; _para3:LPWSTR):longint; external 'gdi32.dll' name 'GetTextFaceW';

  function GetKerningPairsW(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD; external 'gdi32.dll' name 'GetKerningPairsW';

  function GetLogColorSpaceW(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL; external 'gdi32.dll' name 'GetLogColorSpaceW';

  function CreateColorSpaceW(_para1:LPLOGCOLORSPACE):HCOLORSPACE; external 'gdi32.dll' name 'CreateColorSpaceW';

  function GetICMProfileW(_para1:HDC; _para2:DWORD; _para3:LPWSTR):WINBOOL; external 'gdi32.dll' name 'GetICMProfileW';

  function SetICMProfileW(_para1:HDC; _para2:LPWSTR):WINBOOL; external 'gdi32.dll' name 'SetICMProfileW';

  function UpdateICMRegKeyW(_para1:DWORD; _para2:DWORD; _para3:LPWSTR; _para4:UINT):WINBOOL; external 'gdi32.dll' name 'UpdateICMRegKeyW';

  function EnumICMProfilesW(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint; external 'gdi32.dll' name 'EnumICMProfilesW';

  function CreatePropertySheetPageW(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external 'comctl32.dll' name 'CreatePropertySheetPageW';

  function PropertySheetW(lppsph:LPCPROPSHEETHEADER):longint; external 'comctl32.dll' name 'PropertySheetW';

  function ImageList_LoadImageW(hi:HINSTANCE; lpbmp:LPCWSTR; cx:longint; cGrow:longint; crMask:COLORREF; 
             uType:UINT; uFlags:UINT):HIMAGELIST; external 'comctl32.dll' name 'ImageList_LoadImageW';

  function CreateStatusWindowW(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND; external 'comctl32.dll' name 'CreateStatusWindowW';

  procedure DrawStatusTextW(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT); external 'comctl32.dll' name 'DrawStatusTextW';

  function GetOpenFileNameW(_para1:LPOPENFILENAME):WINBOOL; external 'comdlg32.dll' name 'GetOpenFileNameW';

  function GetSaveFileNameW(_para1:LPOPENFILENAME):WINBOOL; external 'comdlg32.dll' name 'GetSaveFileNameW';

  function GetFileTitleW(_para1:LPCWSTR; _para2:LPWSTR; _para3:WORD):integer; external 'comdlg32.dll' name 'GetFileTitleW';

  function ChooseColorW(_para1:LPCHOOSECOLOR):WINBOOL; external 'comdlg32.dll' name 'ChooseColorW';

  function ReplaceTextW(_para1:LPFINDREPLACE):HWND; external 'comdlg32.dll' name 'ReplaceTextW';

  function ChooseFontW(_para1:LPCHOOSEFONT):WINBOOL; external 'comdlg32.dll' name 'ChooseFontW';

  function FindTextW(_para1:LPFINDREPLACE):HWND; external 'comdlg32.dll' name 'FindTextW';

  function PrintDlgW(_para1:LPPRINTDLG):WINBOOL; external 'comdlg32.dll' name 'PrintDlgW';

  function PageSetupDlgW(_para1:LPPAGESETUPDLG):WINBOOL; external 'comdlg32.dll' name 'PageSetupDlgW';

  function CreateProcessW(lpApplicationName:LPCWSTR; lpCommandLine:LPWSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL; 
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCWSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL; external 'kernel32.dll' name 'CreateProcessW';

  procedure GetStartupInfoW(lpStartupInfo:LPSTARTUPINFO); external 'kernel32.dll' name 'GetStartupInfoW';

  function FindFirstFileW(lpFileName:LPCWSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE; external 'kernel32.dll' name 'FindFirstFileW';

  function FindNextFileW(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL; external 'kernel32.dll' name 'FindNextFileW';

  function GetVersionExW(lpVersionInformation:LPOSVERSIONINFO):WINBOOL; external 'kernel32.dll' name 'GetVersionExW';

  { was #define dname(params) def_expr }
  function CreateWindowW(lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;
    begin
       CreateWindowW:=CreateWindowExW(0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogW(hInstance:HINSTANCE; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogW:=CreateDialogParamW(hInstance,lpName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogIndirectW(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogIndirectW:=CreateDialogIndirectParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxW(hInstance:HINSTANCE; lpTemplate:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxW:=DialogBoxParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxIndirectW(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxIndirectW:=DialogBoxIndirectParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  function CreateDCW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC; external 'gdi32.dll' name 'CreateDCW';

  function CreateFontA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCSTR):HFONT; external 'gdi32.dll' name 'CreateFontA';

  function VerInstallFileW(uFlags:DWORD; szSrcFileName:LPWSTR; szDestFileName:LPWSTR; szSrcDir:LPWSTR; szDestDir:LPWSTR; 
             szCurDir:LPWSTR; szTmpFile:LPWSTR; lpuTmpFileLen:PUINT):DWORD; external 'version.dll' name 'VerInstallFileW';

  function GetFileVersionInfoSizeW(lptstrFilename:LPWSTR; lpdwHandle:LPDWORD):DWORD; external 'version.dll' name 'GetFileVersionInfoSizeW';

  function GetFileVersionInfoW(lptstrFilename:LPWSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external 'version.dll' name 'GetFileVersionInfoW';

  function VerLanguageNameW(wLang:DWORD; szLang:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'VerLanguageNameW';

  function VerQueryValueW(pBlock:LPVOID; lpSubBlock:LPWSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external 'version.dll' name 'VerQueryValueW';

  function VerFindFileW(uFlags:DWORD; szFileName:LPWSTR; szWinDir:LPWSTR; szAppDir:LPWSTR; szCurDir:LPWSTR; 
             lpuCurDirLen:PUINT; szDestDir:LPWSTR; lpuDestDirLen:PUINT):DWORD; external 'version.dll' name 'VerFindFileW';

  function RegSetValueExW(hKey:HKEY; lpValueName:LPCWSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE; 
             cbData:DWORD):LONG; external 'advapi32.dll' name 'RegSetValueExW';

  function RegUnLoadKeyW(hKey:HKEY; lpSubKey:LPCWSTR):LONG; external 'advapi32.dll' name 'RegUnLoadKeyW';

  function InitiateSystemShutdownW(lpMachineName:LPWSTR; lpMessage:LPWSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL; external 'advapi32.dll' name 'InitiateSystemShutdownW';

  function AbortSystemShutdownW(lpMachineName:LPWSTR):WINBOOL; external 'advapi32.dll' name 'AbortSystemShutdownW';

  function RegRestoreKeyW(hKey:HKEY; lpFile:LPCWSTR; dwFlags:DWORD):LONG; external 'advapi32.dll' name 'RegRestoreKeyW';

  function RegSaveKeyW(hKey:HKEY; lpFile:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external 'advapi32.dll' name 'RegSaveKeyW';

  function RegSetValueW(hKey:HKEY; lpSubKey:LPCWSTR; dwType:DWORD; lpData:LPCWSTR; cbData:DWORD):LONG; external 'advapi32.dll' name 'RegSetValueW';

  function RegQueryValueW(hKey:HKEY; lpSubKey:LPCWSTR; lpValue:LPWSTR; lpcbValue:PLONG):LONG; external 'advapi32.dll' name 'RegQueryValueW';

  function RegQueryMultipleValuesW(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPWSTR; ldwTotsize:LPDWORD):LONG; external 'advapi32.dll' name 'RegQueryMultipleValuesW';

  function RegQueryValueExW(hKey:HKEY; lpValueName:LPCWSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE; 
             lpcbData:LPDWORD):LONG; external 'advapi32.dll' name 'RegQueryValueExW';

  function RegReplaceKeyW(hKey:HKEY; lpSubKey:LPCWSTR; lpNewFile:LPCWSTR; lpOldFile:LPCWSTR):LONG; external 'advapi32.dll' name 'RegReplaceKeyW';

  function RegConnectRegistryW(lpMachineName:LPWSTR; hKey:HKEY; phkResult:PHKEY):LONG; external 'advapi32.dll' name 'RegConnectRegistryW';

  function RegCreateKeyW(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG; external 'advapi32.dll' name 'RegCreateKeyW';

  function RegCreateKeyExW(hKey:HKEY; lpSubKey:LPCWSTR; Reserved:DWORD; lpClass:LPWSTR; dwOptions:DWORD; 
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG; external 'advapi32.dll' name 'RegCreateKeyExW';

  function RegDeleteKeyW(hKey:HKEY; lpSubKey:LPCWSTR):LONG; external 'advapi32.dll' name 'RegDeleteKeyW';

  function RegDeleteValueW(hKey:HKEY; lpValueName:LPCWSTR):LONG; external 'advapi32.dll' name 'RegDeleteValueW';

  function RegEnumKeyW(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; cbName:DWORD):LONG; external 'advapi32.dll' name 'RegEnumKeyW';

  function RegEnumKeyExW(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; lpcbName:LPDWORD; lpReserved:LPDWORD; 
             lpClass:LPWSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external 'advapi32.dll' name 'RegEnumKeyExW';

  function RegEnumValueW(hKey:HKEY; dwIndex:DWORD; lpValueName:LPWSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD; 
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG; external 'advapi32.dll' name 'RegEnumValueW';

  function RegLoadKeyW(hKey:HKEY; lpSubKey:LPCWSTR; lpFile:LPCWSTR):LONG; external 'advapi32.dll' name 'RegLoadKeyW';

  function RegOpenKeyW(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG; external 'advapi32.dll' name 'RegOpenKeyW';

  function RegOpenKeyExW(hKey:HKEY; lpSubKey:LPCWSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG; external 'advapi32.dll' name 'RegOpenKeyExW';

  function RegQueryInfoKeyW(hKey:HKEY; lpClass:LPWSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD; 
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD; 
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external 'advapi32.dll' name 'RegQueryInfoKeyW';

  function CompareStringW(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCWSTR; cchCount1:longint; lpString2:LPCWSTR; 
             cchCount2:longint):longint; external 'kernel32.dll' name 'CompareStringW';

  function LCMapStringW(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; 
             cchDest:longint):longint; external 'kernel32.dll' name 'LCMapStringW';

  function GetLocaleInfoW(Locale:LCID; LCType:LCTYPE; lpLCData:LPWSTR; cchData:longint):longint; external 'kernel32.dll' name 'GetLocaleInfoW';

  function SetLocaleInfoW(Locale:LCID; LCType:LCTYPE; lpLCData:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'SetLocaleInfoW';

  function GetTimeFormatW(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCWSTR; lpTimeStr:LPWSTR; 
             cchTime:longint):longint; external 'kernel32.dll' name 'GetTimeFormatW';

  function GetDateFormatW(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCWSTR; lpDateStr:LPWSTR; 
             cchDate:longint):longint; external 'kernel32.dll' name 'GetDateFormatW';

  function GetNumberFormatW(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPWSTR; 
             cchNumber:longint):longint; external 'kernel32.dll' name 'GetNumberFormatW';

  function GetCurrencyFormatW(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPWSTR; 
             cchCurrency:longint):longint; external 'kernel32.dll' name 'GetCurrencyFormatW';

  function EnumCalendarInfoW(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL; external 'kernel32.dll' name 'EnumCalendarInfoW';

  function EnumTimeFormatsW(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32.dll' name 'EnumTimeFormatsW';

  function EnumDateFormatsW(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32.dll' name 'EnumDateFormatsW';

  function GetStringTypeExW(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32.dll' name 'GetStringTypeExW';

  function GetStringTypeW(dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32.dll' name 'GetStringTypeW';

  function FoldStringW(dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; cchDest:longint):longint; external 'kernel32.dll' name 'FoldStringW';

  function EnumSystemLocalesW(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL; external 'kernel32.dll' name 'EnumSystemLocalesW';

  function EnumSystemCodePagesW(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL; external 'kernel32.dll' name 'EnumSystemCodePagesW';

  function PeekConsoleInputW(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external 'kernel32.dll' name 'PeekConsoleInputW';

  function ReadConsoleInputW(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external 'kernel32.dll' name 'ReadConsoleInputW';

  function WriteConsoleInputW(hConsoleInput:HANDLE; var lpBuffer:INPUT_RECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL; external 'kernel32.dll' name 'WriteConsoleInputW';

  function ReadConsoleOutputW(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL; external 'kernel32.dll' name 'ReadConsoleOutputW';

  function WriteConsoleOutputW(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL; external 'kernel32.dll' name 'WriteConsoleOutputW';

  function ReadConsoleOutputCharacterW(hConsoleOutput:HANDLE; lpCharacter:LPWSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL; external 'kernel32.dll' name 'ReadConsoleOutputCharacterW';

  function WriteConsoleOutputCharacterW(hConsoleOutput:HANDLE; lpCharacter:LPCWSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32.dll' name 'WriteConsoleOutputCharacterW';

  function FillConsoleOutputCharacterW(hConsoleOutput:HANDLE; cCharacter:WCHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32.dll' name 'FillConsoleOutputCharacterW';

  function ScrollConsoleScreenBufferW(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL; external 'kernel32.dll' name 'ScrollConsoleScreenBufferW';

  function GetConsoleTitleW(lpConsoleTitle:LPWSTR; nSize:DWORD):DWORD; external 'kernel32.dll' name 'GetConsoleTitleW';

  function SetConsoleTitleW(lpConsoleTitle:LPCWSTR):WINBOOL; external 'kernel32.dll' name 'SetConsoleTitleW';

  function ReadConsoleW(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32.dll' name 'ReadConsoleW';

  function WriteConsoleW(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32.dll' name 'WriteConsoleW';

  function WNetAddConnectionW(lpRemoteName:LPCWSTR; lpPassword:LPCWSTR; lpLocalName:LPCWSTR):DWORD; external 'mpr.dll' name 'WNetAddConnectionW';

  function WNetAddConnection2W(lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD; external 'mpr.dll' name 'WNetAddConnection2W';

  function WNetAddConnection3W(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD; external 'mpr.dll' name 'WNetAddConnection3W';

  function WNetCancelConnectionW(lpName:LPCWSTR; fForce:WINBOOL):DWORD; external 'mpr.dll' name 'WNetCancelConnectionW';

  function WNetCancelConnection2W(lpName:LPCWSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD; external 'mpr.dll' name 'WNetCancelConnection2W';

  function WNetGetConnectionW(lpLocalName:LPCWSTR; lpRemoteName:LPWSTR; lpnLength:LPDWORD):DWORD; external 'mpr.dll' name 'WNetGetConnectionW';

  function WNetUseConnectionW(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCWSTR; lpPassword:LPCWSTR; dwFlags:DWORD; 
             lpAccessName:LPWSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD; external 'mpr.dll' name 'WNetUseConnectionW';

  function WNetSetConnectionW(lpName:LPCWSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD; external 'mpr.dll' name 'WNetSetConnectionW';

  function WNetConnectionDialog1W(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD; external 'mpr.dll' name 'WNetConnectionDialog1W';

  function WNetDisconnectDialog1W(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD; external 'mpr.dll' name 'WNetDisconnectDialog1W';

  function WNetOpenEnumW(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD; external 'mpr.dll' name 'WNetOpenEnumW';

  function WNetEnumResourceW(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external 'mpr.dll' name 'WNetEnumResourceW';

  function WNetGetUniversalNameW(lpLocalPath:LPCWSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external 'mpr.dll' name 'WNetGetUniversalNameW';

  function WNetGetUserW(lpName:LPCWSTR; lpUserName:LPWSTR; lpnLength:LPDWORD):DWORD; external 'mpr.dll' name 'WNetGetUserW';

  function WNetGetProviderNameW(dwNetType:DWORD; lpProviderName:LPWSTR; lpBufferSize:LPDWORD):DWORD; external 'mpr.dll' name 'WNetGetProviderNameW';

  function WNetGetNetworkInformationW(lpProvider:LPCWSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD; external 'mpr.dll' name 'WNetGetNetworkInformationW';

  function WNetGetLastErrorW(lpError:LPDWORD; lpErrorBuf:LPWSTR; nErrorBufSize:DWORD; lpNameBuf:LPWSTR; nNameBufSize:DWORD):DWORD; external 'mpr.dll' name 'WNetGetLastErrorW';

  function MultinetGetConnectionPerformanceW(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD; external 'mpr.dll' name 'MultinetGetConnectionPerformanceW';

  function ChangeServiceConfigW(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; 
             lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR; 
             lpDisplayName:LPCWSTR):WINBOOL; external 'advapi32.dll' name 'ChangeServiceConfigW';

  function CreateServiceW(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPCWSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD; 
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; 
             lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR):SC_HANDLE; external 'advapi32.dll' name 'CreateServiceW';

  function EnumDependentServicesW(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD; 
             lpServicesReturned:LPDWORD):WINBOOL; external 'advapi32.dll' name 'EnumDependentServicesW';

  function EnumServicesStatusW(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; 
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL; external 'advapi32.dll' name 'EnumServicesStatusW';

  function GetServiceKeyNameW(hSCManager:SC_HANDLE; lpDisplayName:LPCWSTR; lpServiceName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32.dll' name 'GetServiceKeyNameW';

  function GetServiceDisplayNameW(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32.dll' name 'GetServiceDisplayNameW';

  function OpenSCManagerW(lpMachineName:LPCWSTR; lpDatabaseName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32.dll' name 'OpenSCManagerW';

  function OpenServiceW(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32.dll' name 'OpenServiceW';

  function QueryServiceConfigW(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32.dll' name 'QueryServiceConfigW';

  function QueryServiceLockStatusW(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32.dll' name 'QueryServiceLockStatusW';

  function RegisterServiceCtrlHandlerW(lpServiceName:LPCWSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE; external 'advapi32.dll' name 'RegisterServiceCtrlHandlerW';

  function StartServiceCtrlDispatcherW(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL; external 'advapi32.dll' name 'StartServiceCtrlDispatcherW';

  function StartServiceW(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCWSTR):WINBOOL; external 'advapi32.dll' name 'StartServiceW';

  function wglUseFontBitmapsW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external 'opengl32.dll' name 'wglUseFontBitmapsW';

  function wglUseFontOutlinesW(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:FLOAT; 
             _para6:FLOAT; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external 'opengl32.dll' name 'wglUseFontOutlinesW';

  function DragQueryFileW(_para1:HDROP; _para2:cardinal; _para3:LPCWSTR; _para4:cardinal):cardinal; external 'shell32.dll' name 'DragQueryFileW';

  function ExtractAssociatedIconW(_para1:HINSTANCE; _para2:LPCWSTR; var _para3:WORD):HICON; external 'shell32.dll' name 'ExtractAssociatedIconW';

  function ExtractIconW(_para1:HINSTANCE; _para2:LPCWSTR; _para3:cardinal):HICON; external 'shell32.dll' name 'ExtractIconW';

  function FindExecutableW(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR):HINSTANCE; external 'shell32.dll' name 'FindExecutableW';

  function ShellAboutW(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:HICON):longint; external 'shell32.dll' name 'ShellAboutW';

  function ShellExecuteW(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR; _para5:LPCWSTR; 
             _para6:longint):HINSTANCE; external 'shell32.dll' name 'ShellExecuteW';

  function DdeCreateStringHandleW(_para1:DWORD; _para2:LPCWSTR; _para3:longint):HSZ; external 'user32.dll' name 'DdeCreateStringHandleW';

  function DdeInitializeW(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT; external 'user32.dll' name 'DdeInitializeW';

  function DdeQueryStringW(_para1:DWORD; _para2:HSZ; _para3:LPCWSTR; _para4:DWORD; _para5:longint):DWORD; external 'user32.dll' name 'DdeQueryStringW';

  function LogonUserW(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR; _para4:DWORD; _para5:DWORD; 
             var _para6:HANDLE):WINBOOL; external 'advapi32.dll' name 'LogonUserW';

  function CreateProcessAsUserW(_para1:HANDLE; _para2:LPCWSTR; _para3:LPWSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES; 
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCWSTR; var _para10:STARTUPINFO; 
             var _para11:PROCESS_INFORMATION):WINBOOL; external 'advapi32.dll' name 'CreateProcessAsUserW';


{$endif read_implementation}

{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.3  1998-09-03 18:17:38  pierre
    * small improvements in number of found functions
      all remaining are in func.pp

  Revision 1.2  1998/09/03 17:14:57  pierre
    * most functions found in main DLL's
      still some missing
      use 'make dllnames' to get missing names

  Revision 1.1  1998/08/31 11:54:02  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

}
