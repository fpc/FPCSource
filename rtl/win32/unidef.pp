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

unit unidef;

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
{$ifndef _GNU_H_WINDOWS32_UNICODEFUNCFIONSDEFAULT}
{$define _GNU_H_WINDOWS32_UNICODEFUNCFIONSDEFAULT}
{ C++ extern C conditionnal removed }
  { __cplusplus  }

  function GetBinaryType(lpApplicationName:LPCWSTR; lpBinaryType:LPDWORD):WINBOOL;

  function GetShortPathName(lpszLongPath:LPCWSTR; lpszShortPath:LPWSTR; cchBuffer:DWORD):DWORD;

  function GetEnvironmentStrings : LPWSTR;

  function FreeEnvironmentStrings(_para1:LPWSTR):WINBOOL;

  function FormatMessage(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPWSTR; 
             nSize:DWORD; var Arguments:va_list):DWORD;

  function CreateMailslot(lpName:LPCWSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function lstrcmp(lpString1:LPCWSTR; lpString2:LPCWSTR):longint;

  function lstrcmpi(lpString1:LPCWSTR; lpString2:LPCWSTR):longint;

  function lstrcpyn(lpString1:LPWSTR; lpString2:LPCWSTR; iMaxLength:longint):LPWSTR;

  function lstrcpy(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR;

  function lstrcat(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR;

  function lstrlen(lpString:LPCWSTR):longint;

  function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCWSTR):HANDLE;

  function OpenMutex(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCWSTR):HANDLE;

  function OpenEvent(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCWSTR):HANDLE;

  function OpenSemaphore(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function CreateFileMapping(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD; 
             lpName:LPCWSTR):HANDLE;

  function OpenFileMapping(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE;

  function GetLogicalDriveStrings(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD;

  function LoadLibrary(lpLibFileName:LPCWSTR):HINSTANCE;

  function LoadLibraryEx(lpLibFileName:LPCWSTR; hFile:HANDLE; dwFlags:DWORD):HINSTANCE;

  function GetModuleFileName(hModule:HINSTANCE; lpFilename:LPWSTR; nSize:DWORD):DWORD;

  function GetModuleHandle(lpModuleName:LPCWSTR):HMODULE;

  procedure FatalAppExit(uAction:UINT; lpMessageText:LPCWSTR);

  function GetCommandLine : LPWSTR;

  function GetEnvironmentVariable(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD;

  function SetEnvironmentVariable(lpName:LPCWSTR; lpValue:LPCWSTR):WINBOOL;

  function ExpandEnvironmentStrings(lpSrc:LPCWSTR; lpDst:LPWSTR; nSize:DWORD):DWORD;

  procedure OutputDebugString(lpOutputString:LPCWSTR);

  function FindResource(hModule:HINSTANCE; lpName:LPCWSTR; lpType:LPCWSTR):HRSRC;

  function FindResourceEx(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD):HRSRC;

  function EnumResourceTypes(hModule:HINSTANCE; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL;

  function EnumResourceNames(hModule:HINSTANCE; lpType:LPCWSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL;

  function EnumResourceLanguages(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL;

  function BeginUpdateResource(pFileName:LPCWSTR; bDeleteExistingResources:WINBOOL):HANDLE;

  function UpdateResource(hUpdate:HANDLE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD; lpData:LPVOID; 
             cbData:DWORD):WINBOOL;

  function EndUpdateResource(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL;

  function GlobalAddAtom(lpString:LPCWSTR):ATOM;

  function GlobalFindAtom(lpString:LPCWSTR):ATOM;

  function GlobalGetAtomName(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT;

  function AddAtom(lpString:LPCWSTR):ATOM;

  function FindAtom(lpString:LPCWSTR):ATOM;

  function GetAtomName(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT;

  function GetProfileInt(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT):UINT;

  function GetProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD;

  function WriteProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR):WINBOOL;

  function GetProfileSection(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD;

  function WriteProfileSection(lpAppName:LPCWSTR; lpString:LPCWSTR):WINBOOL;

  function GetPrivateProfileInt(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT; lpFileName:LPCWSTR):UINT;

  function GetPrivateProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; 
             lpFileName:LPCWSTR):DWORD;

  function WritePrivateProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL;

  function GetPrivateProfileSection(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; lpFileName:LPCWSTR):DWORD;

  function WritePrivateProfileSection(lpAppName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL;

  function GetDriveType(lpRootPathName:LPCWSTR):UINT;

  function GetSystemDirectory(lpBuffer:LPWSTR; uSize:UINT):UINT;

  function GetTempPath(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD;

  function GetTempFileName(lpPathName:LPCWSTR; lpPrefixString:LPCWSTR; uUnique:UINT; lpTempFileName:LPWSTR):UINT;

  function GetWindowsDirectory(lpBuffer:LPWSTR; uSize:UINT):UINT;

  function SetCurrentDirectory(lpPathName:LPCWSTR):WINBOOL;

  function GetCurrentDirectory(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD;

  function GetDiskFreeSpace(lpRootPathName:LPCWSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL;

  function CreateDirectory(lpPathName:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function CreateDirectoryEx(lpTemplateDirectory:LPCWSTR; lpNewDirectory:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function RemoveDirectory(lpPathName:LPCWSTR):WINBOOL;

  function GetFullPathName(lpFileName:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; var lpFilePart:LPWSTR):DWORD;

  function DefineDosDevice(dwFlags:DWORD; lpDeviceName:LPCWSTR; lpTargetPath:LPCWSTR):WINBOOL;

  function QueryDosDevice(lpDeviceName:LPCWSTR; lpTargetPath:LPWSTR; ucchMax:DWORD):DWORD;

  function CreateFile(lpFileName:LPCWSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD; 
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE;

  function SetFileAttributes(lpFileName:LPCWSTR; dwFileAttributes:DWORD):WINBOOL;

  function GetFileAttributes(lpFileName:LPCWSTR):DWORD;

  function GetCompressedFileSize(lpFileName:LPCWSTR; lpFileSizeHigh:LPDWORD):DWORD;

  function DeleteFile(lpFileName:LPCWSTR):WINBOOL;

  function SearchPath(lpPath:LPCWSTR; lpFileName:LPCWSTR; lpExtension:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; 
             var lpFilePart:LPWSTR):DWORD;

  function CopyFile(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; bFailIfExists:WINBOOL):WINBOOL;

  function MoveFile(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR):WINBOOL;

  function MoveFileEx(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; dwFlags:DWORD):WINBOOL;

  function CreateNamedPipe(lpName:LPCWSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD; 
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function GetNamedPipeHandleState(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD; 
             lpUserName:LPWSTR; nMaxUserNameSize:DWORD):WINBOOL;

  function CallNamedPipe(lpNamedPipeName:LPCWSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD; 
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL;

  function WaitNamedPipe(lpNamedPipeName:LPCWSTR; nTimeOut:DWORD):WINBOOL;

  function SetVolumeLabel(lpRootPathName:LPCWSTR; lpVolumeName:LPCWSTR):WINBOOL;

  function GetVolumeInformation(lpRootPathName:LPCWSTR; lpVolumeNameBuffer:LPWSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD; 
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPWSTR; nFileSystemNameSize:DWORD):WINBOOL;

  function ClearEventLog(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL;

  function BackupEventLog(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL;

  function OpenEventLog(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE;

  function RegisterEventSource(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE;

  function OpenBackupEventLog(lpUNCServerName:LPCWSTR; lpFileName:LPCWSTR):HANDLE;

  function ReadEventLog(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; 
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL;

  function ReportEvent(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID; 
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCWSTR; lpRawData:LPVOID):WINBOOL;

  function AccessCheckAndAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR; 
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL; 
             pfGenerateOnClose:LPBOOL):WINBOOL;

  function ObjectOpenAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR; 
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL; 
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL;

  function ObjectPrivilegeAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET; 
             AccessGranted:WINBOOL):WINBOOL;

  function ObjectCloseAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL;

  function PrivilegedServiceAuditAlarm(SubsystemName:LPCWSTR; ServiceName:LPCWSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL;

  function SetFileSecurity(lpFileName:LPCWSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function GetFileSecurity(lpFileName:LPCWSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function FindFirstChangeNotification(lpPathName:LPCWSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE;

  function IsBadStringPtr(lpsz:LPCWSTR; ucchMax:UINT):WINBOOL;

  function LookupAccountSid(lpSystemName:LPCWSTR; Sid:PSID; Name:LPWSTR; cbName:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupAccountName(lpSystemName:LPCWSTR; lpAccountName:LPCWSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupPrivilegeValue(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpLuid:PLUID):WINBOOL;

  function LookupPrivilegeName(lpSystemName:LPCWSTR; lpLuid:PLUID; lpName:LPWSTR; cbName:LPDWORD):WINBOOL;

  function LookupPrivilegeDisplayName(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpDisplayName:LPWSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL;

  function BuildCommDCB(lpDef:LPCWSTR; lpDCB:LPDCB):WINBOOL;

  function BuildCommDCBAndTimeouts(lpDef:LPCWSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL;

  function CommConfigDialog(lpszName:LPCWSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL;

  function GetDefaultCommConfig(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL;

  function SetDefaultCommConfig(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL;

  function GetComputerName(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL;

  function SetComputerName(lpComputerName:LPCWSTR):WINBOOL;

  function GetUserName(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL;

  function wvsprintf(_para1:LPWSTR; _para2:LPCWSTR; arglist:va_list):longint;

  { variable number of args not yet implemented in FPC
  function wsprintf(_para1:LPWSTR; _para2:LPCWSTR; ...):longint;}

  function LoadKeyboardLayout(pwszKLID:LPCWSTR; Flags:UINT):HKL;

  function GetKeyboardLayoutName(pwszKLID:LPWSTR):WINBOOL;

  function CreateDesktop(lpszDesktop:LPWSTR; lpszDevice:LPWSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD; 
             lpsa:LPSECURITY_ATTRIBUTES):HDESK;

  function OpenDesktop(lpszDesktop:LPWSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK;

  function EnumDesktops(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL;

  function CreateWindowStation(lpwinsta:LPWSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA;

  function OpenWindowStation(lpszWinSta:LPWSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA;

  function EnumWindowStations(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL;

  function GetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function SetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL;

  function RegisterWindowMessage(lpString:LPCWSTR):UINT;

  function GetMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL;

(* Const before type ignored *)
  function DispatchMessage(var lpMsg:MSG):LONG;

  function PeekMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL;

  function SendMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function SendMessageTimeout(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT; 
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT;

  function SendNotifyMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function SendMessageCallback(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC; 
             dwData:DWORD):WINBOOL;

  function PostMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function PostThreadMessage(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function DefWindowProc(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallWindowProc(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

(* Const before type ignored *)
  function RegisterClass(var lpWndClass:WNDCLASS):ATOM;

  function UnregisterClass(lpClassName:LPCWSTR; hInstance:HINSTANCE):WINBOOL;

  function GetClassInfo(hInstance:HINSTANCE; lpClassName:LPCWSTR; lpWndClass:LPWNDCLASS):WINBOOL;

(* Const before type ignored *)
  function RegisterClassEx(var _para1:WNDCLASSEX):ATOM;

  function GetClassInfoEx(_para1:HINSTANCE; _para2:LPCWSTR; _para3:LPWNDCLASSEX):WINBOOL;

  function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint; 
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;

  function CreateDialogParam(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function CreateDialogIndirectParam(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function DialogBoxParam(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function DialogBoxIndirectParam(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function SetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPCWSTR):WINBOOL;

  function GetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPWSTR; nMaxCount:longint):UINT;

  function SendDlgItemMessage(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG;

  function DefDlgProc(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallMsgFilter(lpMsg:LPMSG; nCode:longint):WINBOOL;

  function RegisterClipboardFormat(lpszFormat:LPCWSTR):UINT;

  function GetClipboardFormatName(format:UINT; lpszFormatName:LPWSTR; cchMaxCount:longint):longint;

  function CharToOem(lpszSrc:LPCWSTR; lpszDst:LPSTR):WINBOOL;

  function OemToChar(lpszSrc:LPCSTR; lpszDst:LPWSTR):WINBOOL;

  function CharToOemBuff(lpszSrc:LPCWSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL;

  function OemToCharBuff(lpszSrc:LPCSTR; lpszDst:LPWSTR; cchDstLength:DWORD):WINBOOL;

  function CharUpper(lpsz:LPWSTR):LPWSTR;

  function CharUpperBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD;

  function CharLower(lpsz:LPWSTR):LPWSTR;

  function CharLowerBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD;

  function CharNext(lpsz:LPCWSTR):LPWSTR;

  function CharPrev(lpszStart:LPCWSTR; lpszCurrent:LPCWSTR):LPWSTR;

  function IsCharAlpha(ch:WCHAR):WINBOOL;

  function IsCharAlphaNumeric(ch:WCHAR):WINBOOL;

  function IsCharUpper(ch:WCHAR):WINBOOL;

  function IsCharLower(ch:WCHAR):WINBOOL;

  function GetKeyNameText(lParam:LONG; lpString:LPWSTR; nSize:longint):longint;

  function VkKeyScan(ch:WCHAR):SHORT;

  function VkKeyScanEx(ch:WCHAR; dwhkl:HKL):SHORT;

  function MapVirtualKey(uCode:UINT; uMapType:UINT):UINT;

  function MapVirtualKeyEx(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT;

  function LoadAccelerators(hInstance:HINSTANCE; lpTableName:LPCWSTR):HACCEL;

  function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL;

  function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint;

  function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint;

  function LoadMenu(hInstance:HINSTANCE; lpMenuName:LPCWSTR):HMENU;

(* Const before type ignored *)
  function LoadMenuIndirect(var lpMenuTemplate:MENUTEMPLATE):HMENU;

  function ChangeMenu(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCWSTR; cmdInsert:UINT; flags:UINT):WINBOOL;

  function GetMenuString(hMenu:HMENU; uIDItem:UINT; lpString:LPWSTR; nMaxCount:longint; uFlag:UINT):longint;

  function InsertMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL;

  function AppendMenu(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL;

  function ModifyMenu(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL;

  function InsertMenuItem(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function GetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL;

  function SetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function DrawText(hDC:HDC; lpString:LPCWSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint;

  function DrawTextEx(_para1:HDC; _para2:LPWSTR; _para3:longint; _para4:LPRECT; _para5:UINT; 
             _para6:LPDRAWTEXTPARAMS):longint;

  function GrayString(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint; 
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL;

  function DrawState(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM; 
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL;

  function TabbedTextOut(hDC:HDC; X:longint; Y:longint; lpString:LPCWSTR; nCount:longint; 
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG;

  function GetTabbedTextExtent(hDC:HDC; lpString:LPCWSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD;

  function SetProp(hWnd:HWND; lpString:LPCWSTR; hData:HANDLE):WINBOOL;

  function GetProp(hWnd:HWND; lpString:LPCWSTR):HANDLE;

  function RemoveProp(hWnd:HWND; lpString:LPCWSTR):HANDLE;

  function EnumPropsEx(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint;

  function EnumProps(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint;

  function SetWindowText(hWnd:HWND; lpString:LPCWSTR):WINBOOL;

  function GetWindowText(hWnd:HWND; lpString:LPWSTR; nMaxCount:longint):longint;

  function GetWindowTextLength(hWnd:HWND):longint;

  function MessageBox(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT):longint;

  function MessageBoxEx(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT; wLanguageId:WORD):longint;

  function MessageBoxIndirect(_para1:LPMSGBOXPARAMS):longint;

  function GetWindowLong(hWnd:HWND; nIndex:longint):LONG;

  function SetWindowLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG;

  function GetClassLong(hWnd:HWND; nIndex:longint):DWORD;

  function SetClassLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD;

  function FindWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR):HWND;

  function FindWindowEx(_para1:HWND; _para2:HWND; _para3:LPCWSTR; _para4:LPCWSTR):HWND;

  function GetClassName(hWnd:HWND; lpClassName:LPWSTR; nMaxCount:longint):longint;

  function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINSTANCE; dwThreadId:DWORD):HHOOK;

  function LoadBitmap(hInstance:HINSTANCE; lpBitmapName:LPCWSTR):HBITMAP;

  function LoadCursor(hInstance:HINSTANCE; lpCursorName:LPCWSTR):HCURSOR;

  function LoadCursorFromFile(lpFileName:LPCWSTR):HCURSOR;

  function LoadIcon(hInstance:HINSTANCE; lpIconName:LPCWSTR):HICON;

  function LoadImage(_para1:HINSTANCE; _para2:LPCWSTR; _para3:UINT; _para4:longint; _para5:longint; 
             _para6:UINT):HANDLE;

  function LoadString(hInstance:HINSTANCE; uID:UINT; lpBuffer:LPWSTR; nBufferMax:longint):longint;

  function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL;

  function DlgDirList(hDlg:HWND; lpPathSpec:LPWSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint;

  function DlgDirSelectEx(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDListBox:longint):WINBOOL;

  function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPWSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint;

  function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDComboBox:longint):WINBOOL;

  function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CreateMDIWindow(lpClassName:LPWSTR; lpWindowName:LPWSTR; dwStyle:DWORD; X:longint; Y:longint; 
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINSTANCE; lParam:LPARAM):HWND;

  function WinHelp(hWndMain:HWND; lpszHelp:LPCWSTR; uCommand:UINT; dwData:DWORD):WINBOOL;

  function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG;

  function EnumDisplaySettings(lpszDeviceName:LPCWSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL;

  function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL;

  function AddFontResource(_para1:LPCWSTR):longint;

  function CopyMetaFile(_para1:HMETAFILE; _para2:LPCWSTR):HMETAFILE;

(* Const before type ignored *)
  function CreateFontIndirect(var _para1:LOGFONT):HFONT;

  function CreateFont(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCWSTR):HFONT;

(* Const before type ignored *)
  function CreateIC(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC;

  function CreateMetaFile(_para1:LPCWSTR):HDC;

  function CreateScalableFontResource(_para1:DWORD; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR):WINBOOL;

(* Const before type ignored *)
  function DeviceCapabilities(_para1:LPCWSTR; _para2:LPCWSTR; _para3:WORD; _para4:LPWSTR; var _para5:DEVMODE):longint;

  function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint;

  function EnumFontFamilies(_para1:HDC; _para2:LPCWSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint;

  function EnumFonts(_para1:HDC; _para2:LPCWSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint;

  function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL;

  function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL;

  function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL;

(* Const before type ignored *)
  function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD; 
             _para6:LPVOID; var _para7:MAT2):DWORD;

  function GetMetaFile(_para1:LPCWSTR):HMETAFILE;

  function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT;

  function GetTextExtentPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentPoint32(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentExPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPINT; 
             _para6:LPINT; _para7:LPSIZE):WINBOOL;

  function GetCharacterPlacement(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS; 
             _para6:DWORD):DWORD;

(* Const before type ignored *)
  function ResetDC(_para1:HDC; var _para2:DEVMODE):HDC;

  function RemoveFontResource(_para1:LPCWSTR):WINBOOL;

  function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCWSTR):HENHMETAFILE;

(* Const before type ignored *)
  function CreateEnhMetaFile(_para1:HDC; _para2:LPCWSTR; var _para3:RECT; _para4:LPCWSTR):HDC;

  function GetEnhMetaFile(_para1:LPCWSTR):HENHMETAFILE;

  function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPWSTR):UINT;

  function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL;

(* Const before type ignored *)
  function StartDoc(_para1:HDC; var _para2:DOCINFO):longint;

  function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint;

  function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCWSTR; _para5:longint):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT; 
             _para6:LPCWSTR; _para7:UINT; var _para8:INT):WINBOOL;

(* Const before type ignored *)
  function PolyTextOut(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL;

  function GetTextFace(_para1:HDC; _para2:longint; _para3:LPWSTR):longint;

  function GetKerningPairs(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD;

  function GetLogColorSpace(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL;

  function CreateColorSpace(_para1:LPLOGCOLORSPACE):HCOLORSPACE;

  function GetICMProfile(_para1:HDC; _para2:DWORD; _para3:LPWSTR):WINBOOL;

  function SetICMProfile(_para1:HDC; _para2:LPWSTR):WINBOOL;

  function UpdateICMRegKey(_para1:DWORD; _para2:DWORD; _para3:LPWSTR; _para4:UINT):WINBOOL;

  function EnumICMProfiles(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint;

  function CreatePropertySheetPage(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE;

  function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint;

  function ImageList_LoadImage(hi:HINSTANCE; lpbmp:LPCWSTR; cx:longint; cGrow:longint; crMask:COLORREF; 
             uType:UINT; uFlags:UINT):HIMAGELIST;

  function CreateStatusWindow(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND;

  procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT);

  function GetOpenFileName(_para1:LPOPENFILENAME):WINBOOL;

  function GetSaveFileName(_para1:LPOPENFILENAME):WINBOOL;

  function GetFileTitle(_para1:LPCWSTR; _para2:LPWSTR; _para3:WORD):integer;

  function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL;

  function ReplaceText(_para1:LPFINDREPLACE):HWND;

  function ChooseFont(_para1:LPCHOOSEFONT):WINBOOL;

  function FindText(_para1:LPFINDREPLACE):HWND;

  function PrintDlg(_para1:LPPRINTDLG):WINBOOL;

  function PageSetupDlg(_para1:LPPAGESETUPDLG):WINBOOL;

  function CreateProcess(lpApplicationName:LPCWSTR; lpCommandLine:LPWSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL; 
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCWSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL;

  procedure GetStartupInfo(lpStartupInfo:LPSTARTUPINFO);

  function FindFirstFile(lpFileName:LPCWSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE;

  function FindNextFile(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL;

  function GetVersionEx(lpVersionInformation:LPOSVERSIONINFO):WINBOOL;

  { was #define dname(params) def_expr }
  function CreateWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;

  { was #define dname(params) def_expr }
  function CreateDialog(hInstance:HINSTANCE; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogIndirect(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function DialogBox(hInstance:HINSTANCE; lpTemplate:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  { was #define dname(params) def_expr }
  function DialogBoxIndirect(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

(* Const before type ignored *)
  function CreateDC(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC;

  function CreateFontA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCSTR):HFONT;

  function VerInstallFile(uFlags:DWORD; szSrcFileName:LPWSTR; szDestFileName:LPWSTR; szSrcDir:LPWSTR; szDestDir:LPWSTR; 
             szCurDir:LPWSTR; szTmpFile:LPWSTR; lpuTmpFileLen:PUINT):DWORD;

  function GetFileVersionInfoSize(lptstrFilename:LPWSTR; lpdwHandle:LPDWORD):DWORD;

  function GetFileVersionInfo(lptstrFilename:LPWSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL;

  function VerLanguageName(wLang:DWORD; szLang:LPWSTR; nSize:DWORD):DWORD;

(* Const before type ignored *)
  function VerQueryValue(pBlock:LPVOID; lpSubBlock:LPWSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL;

  function VerFindFile(uFlags:DWORD; szFileName:LPWSTR; szWinDir:LPWSTR; szAppDir:LPWSTR; szCurDir:LPWSTR; 
             lpuCurDirLen:PUINT; szDestDir:LPWSTR; lpuDestDirLen:PUINT):DWORD;

(* Const before type ignored *)
  function RegSetValueEx(hKey:HKEY; lpValueName:LPCWSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE; 
             cbData:DWORD):LONG;

  function RegUnLoadKey(hKey:HKEY; lpSubKey:LPCWSTR):LONG;

  function InitiateSystemShutdown(lpMachineName:LPWSTR; lpMessage:LPWSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL;

  function AbortSystemShutdown(lpMachineName:LPWSTR):WINBOOL;

  function RegRestoreKey(hKey:HKEY; lpFile:LPCWSTR; dwFlags:DWORD):LONG;

  function RegSaveKey(hKey:HKEY; lpFile:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG;

  function RegSetValue(hKey:HKEY; lpSubKey:LPCWSTR; dwType:DWORD; lpData:LPCWSTR; cbData:DWORD):LONG;

  function RegQueryValue(hKey:HKEY; lpSubKey:LPCWSTR; lpValue:LPWSTR; lpcbValue:PLONG):LONG;

  function RegQueryMultipleValues(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPWSTR; ldwTotsize:LPDWORD):LONG;

  function RegQueryValueEx(hKey:HKEY; lpValueName:LPCWSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE; 
             lpcbData:LPDWORD):LONG;

  function RegReplaceKey(hKey:HKEY; lpSubKey:LPCWSTR; lpNewFile:LPCWSTR; lpOldFile:LPCWSTR):LONG;

  function RegConnectRegistry(lpMachineName:LPWSTR; hKey:HKEY; phkResult:PHKEY):LONG;

  function RegCreateKey(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG;

  function RegCreateKeyEx(hKey:HKEY; lpSubKey:LPCWSTR; Reserved:DWORD; lpClass:LPWSTR; dwOptions:DWORD; 
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG;

  function RegDeleteKey(hKey:HKEY; lpSubKey:LPCWSTR):LONG;

  function RegDeleteValue(hKey:HKEY; lpValueName:LPCWSTR):LONG;

  function RegEnumKey(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; cbName:DWORD):LONG;

  function RegEnumKeyEx(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; lpcbName:LPDWORD; lpReserved:LPDWORD; 
             lpClass:LPWSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function RegEnumValue(hKey:HKEY; dwIndex:DWORD; lpValueName:LPWSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD; 
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG;

  function RegLoadKey(hKey:HKEY; lpSubKey:LPCWSTR; lpFile:LPCWSTR):LONG;

  function RegOpenKey(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG;

  function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCWSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG;

  function RegQueryInfoKey(hKey:HKEY; lpClass:LPWSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD; 
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD; 
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function CompareString(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCWSTR; cchCount1:longint; lpString2:LPCWSTR; 
             cchCount2:longint):longint;

  function LCMapString(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; 
             cchDest:longint):longint;

  function GetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPWSTR; cchData:longint):longint;

  function SetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPCWSTR):WINBOOL;

(* Const before type ignored *)
  function GetTimeFormat(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCWSTR; lpTimeStr:LPWSTR; 
             cchTime:longint):longint;

(* Const before type ignored *)
  function GetDateFormat(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCWSTR; lpDateStr:LPWSTR; 
             cchDate:longint):longint;

(* Const before type ignored *)
  function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPWSTR; 
             cchNumber:longint):longint;

(* Const before type ignored *)
  function GetCurrencyFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPWSTR; 
             cchCurrency:longint):longint;

  function EnumCalendarInfo(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL;

  function EnumTimeFormats(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function EnumDateFormats(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function GetStringTypeEx(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function GetStringType(dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function FoldString(dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; cchDest:longint):longint;

  function EnumSystemLocales(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function EnumSystemCodePages(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function PeekConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL;

  function ReadConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleInput(hConsoleInput:HANDLE; var lpBuffer:INPUT_RECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL;

  function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleOutput(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL;

  function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPWSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL;

  function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCWSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:WCHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL;

  function GetConsoleTitle(lpConsoleTitle:LPWSTR; nSize:DWORD):DWORD;

  function SetConsoleTitle(lpConsoleTitle:LPCWSTR):WINBOOL;

  function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL;

(* Const before type ignored *)
  function WriteConsole(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL;

  function WNetAddConnection(lpRemoteName:LPCWSTR; lpPassword:LPCWSTR; lpLocalName:LPCWSTR):DWORD;

  function WNetAddConnection2(lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD;

  function WNetAddConnection3(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD;

  function WNetCancelConnection(lpName:LPCWSTR; fForce:WINBOOL):DWORD;

  function WNetCancelConnection2(lpName:LPCWSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD;

  function WNetGetConnection(lpLocalName:LPCWSTR; lpRemoteName:LPWSTR; lpnLength:LPDWORD):DWORD;

  function WNetUseConnection(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCWSTR; lpPassword:LPCWSTR; dwFlags:DWORD; 
             lpAccessName:LPWSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD;

  function WNetSetConnection(lpName:LPCWSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD;

  function WNetConnectionDialog1(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD;

  function WNetDisconnectDialog1(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD;

  function WNetOpenEnum(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD;

  function WNetEnumResource(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUniversalName(lpLocalPath:LPCWSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUser(lpName:LPCWSTR; lpUserName:LPWSTR; lpnLength:LPDWORD):DWORD;

  function WNetGetProviderName(dwNetType:DWORD; lpProviderName:LPWSTR; lpBufferSize:LPDWORD):DWORD;

  function WNetGetNetworkInformation(lpProvider:LPCWSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD;

  function WNetGetLastError(lpError:LPDWORD; lpErrorBuf:LPWSTR; nErrorBufSize:DWORD; lpNameBuf:LPWSTR; nNameBufSize:DWORD):DWORD;

  function MultinetGetConnectionPerformance(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD;

  function ChangeServiceConfig(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; 
             lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR; 
             lpDisplayName:LPCWSTR):WINBOOL;

  function CreateService(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPCWSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD; 
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; 
             lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR):SC_HANDLE;

  function EnumDependentServices(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD; 
             lpServicesReturned:LPDWORD):WINBOOL;

  function EnumServicesStatus(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; 
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL;

  function GetServiceKeyName(hSCManager:SC_HANDLE; lpDisplayName:LPCWSTR; lpServiceName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function GetServiceDisplayName(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function OpenSCManager(lpMachineName:LPCWSTR; lpDatabaseName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function OpenService(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function QueryServiceConfig(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function QueryServiceLockStatus(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function RegisterServiceCtrlHandler(lpServiceName:LPCWSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE;

  function StartServiceCtrlDispatcher(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL;

  function StartService(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCWSTR):WINBOOL;

  { Extensions to OpenGL  }
  function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL;

  function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:FLOAT; 
             _para6:FLOAT; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL;

  { -------------------------------------  }
  { From shellapi.h in old Cygnus headers  }
  function DragQueryFile(_para1:HDROP; _para2:cardinal; _para3:LPCWSTR; _para4:cardinal):cardinal;

  function ExtractAssociatedIcon(_para1:HINSTANCE; _para2:LPCWSTR; var _para3:WORD):HICON;

(* Const before type ignored *)
  function ExtractIcon(_para1:HINSTANCE; _para2:LPCWSTR; _para3:cardinal):HICON;

(* Const before type ignored *)
(* Const before type ignored *)
  function FindExecutable(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR):HINSTANCE;

(* Const before type ignored *)
(* Const before type ignored *)
  function ShellAbout(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:HICON):longint;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function ShellExecute(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR; _para5:LPCWSTR; 
             _para6:longint):HINSTANCE;

  { end of stuff from shellapi.h in old Cygnus headers  }
  { --------------------------------------------------  }
  { From ddeml.h in old Cygnus headers  }
  function DdeCreateStringHandle(_para1:DWORD; _para2:LPCWSTR; _para3:longint):HSZ;

  function DdeInitialize(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT;

  function DdeQueryString(_para1:DWORD; _para2:HSZ; _para3:LPCWSTR; _para4:DWORD; _para5:longint):DWORD;

  { end of stuff from ddeml.h in old Cygnus headers  }
  { -----------------------------------------------  }
  function LogonUser(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR; _para4:DWORD; _para5:DWORD; 
             var _para6:HANDLE):WINBOOL;

  function CreateProcessAsUser(_para1:HANDLE; _para2:LPCWSTR; _para3:LPWSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES; 
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCWSTR; var _para10:STARTUPINFO; 
             var _para11:PROCESS_INFORMATION):WINBOOL;

{ C++ end of extern C conditionnal removed }
  { __cplusplus  }
{$endif}
  { _GNU_H_WINDOWS32_UNICODEFUNCFIONSDEFAULT  }

{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

  function GetBinaryType(lpApplicationName:LPCWSTR; lpBinaryType:LPDWORD):WINBOOL; external External_library name 'GetBinaryTypeW';

  function GetShortPathName(lpszLongPath:LPCWSTR; lpszShortPath:LPWSTR; cchBuffer:DWORD):DWORD; external External_library name 'GetShortPathNameW';

  function GetEnvironmentStrings : LPWSTR; external External_library name 'GetEnvironmentStringsW';

  function FreeEnvironmentStrings(_para1:LPWSTR):WINBOOL; external External_library name 'FreeEnvironmentStringsW';

  function FormatMessage(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPWSTR; 
             nSize:DWORD; var Arguments:va_list):DWORD; external External_library name 'FormatMessageW';

  function CreateMailslot(lpName:LPCWSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external External_library name 'CreateMailslotW';

  function lstrcmp(lpString1:LPCWSTR; lpString2:LPCWSTR):longint; external External_library name 'lstrcmpW';

  function lstrcmpi(lpString1:LPCWSTR; lpString2:LPCWSTR):longint; external External_library name 'lstrcmpiW';

  function lstrcpyn(lpString1:LPWSTR; lpString2:LPCWSTR; iMaxLength:longint):LPWSTR; external External_library name 'lstrcpynW';

  function lstrcpy(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR; external External_library name 'lstrcpyW';

  function lstrcat(lpString1:LPWSTR; lpString2:LPCWSTR):LPWSTR; external External_library name 'lstrcatW';

  function lstrlen(lpString:LPCWSTR):longint; external External_library name 'lstrlenW';

  function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCWSTR):HANDLE; external External_library name 'CreateMutexW';

  function OpenMutex(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external External_library name 'OpenMutexW';

  function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCWSTR):HANDLE; external External_library name 'CreateEventW';

  function OpenEvent(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external External_library name 'OpenEventW';

  function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCWSTR):HANDLE; external External_library name 'CreateSemaphoreW';

  function OpenSemaphore(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external External_library name 'OpenSemaphoreW';

  function CreateFileMapping(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD; 
             lpName:LPCWSTR):HANDLE; external External_library name 'CreateFileMappingW';

  function OpenFileMapping(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCWSTR):HANDLE; external External_library name 'OpenFileMappingW';

  function GetLogicalDriveStrings(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external External_library name 'GetLogicalDriveStringsW';

  function LoadLibrary(lpLibFileName:LPCWSTR):HINSTANCE; external External_library name 'LoadLibraryW';

  function LoadLibraryEx(lpLibFileName:LPCWSTR; hFile:HANDLE; dwFlags:DWORD):HINSTANCE; external External_library name 'LoadLibraryExW';

  function GetModuleFileName(hModule:HINSTANCE; lpFilename:LPWSTR; nSize:DWORD):DWORD; external External_library name 'GetModuleFileNameW';

  function GetModuleHandle(lpModuleName:LPCWSTR):HMODULE; external External_library name 'GetModuleHandleW';

  procedure FatalAppExit(uAction:UINT; lpMessageText:LPCWSTR); external External_library name 'FatalAppExitW';

  function GetCommandLine : LPWSTR; external External_library name 'GetCommandLineW';

  function GetEnvironmentVariable(lpName:LPCWSTR; lpBuffer:LPWSTR; nSize:DWORD):DWORD; external External_library name 'GetEnvironmentVariableW';

  function SetEnvironmentVariable(lpName:LPCWSTR; lpValue:LPCWSTR):WINBOOL; external External_library name 'SetEnvironmentVariableW';

  function ExpandEnvironmentStrings(lpSrc:LPCWSTR; lpDst:LPWSTR; nSize:DWORD):DWORD; external External_library name 'ExpandEnvironmentStringsW';

  procedure OutputDebugString(lpOutputString:LPCWSTR); external External_library name 'OutputDebugStringW';

  function FindResource(hModule:HINSTANCE; lpName:LPCWSTR; lpType:LPCWSTR):HRSRC; external External_library name 'FindResourceW';

  function FindResourceEx(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD):HRSRC; external External_library name 'FindResourceExW';

  function EnumResourceTypes(hModule:HINSTANCE; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL; external External_library name 'EnumResourceTypesW';

  function EnumResourceNames(hModule:HINSTANCE; lpType:LPCWSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL; external External_library name 'EnumResourceNamesW';

  function EnumResourceLanguages(hModule:HINSTANCE; lpType:LPCWSTR; lpName:LPCWSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL; external External_library name 'EnumResourceLanguagesW';

  function BeginUpdateResource(pFileName:LPCWSTR; bDeleteExistingResources:WINBOOL):HANDLE; external External_library name 'BeginUpdateResourceW';

  function UpdateResource(hUpdate:HANDLE; lpType:LPCWSTR; lpName:LPCWSTR; wLanguage:WORD; lpData:LPVOID; 
             cbData:DWORD):WINBOOL; external External_library name 'UpdateResourceW';

  function EndUpdateResource(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL; external External_library name 'EndUpdateResourceW';

  function GlobalAddAtom(lpString:LPCWSTR):ATOM; external External_library name 'GlobalAddAtomW';

  function GlobalFindAtom(lpString:LPCWSTR):ATOM; external External_library name 'GlobalFindAtomW';

  function GlobalGetAtomName(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT; external External_library name 'GlobalGetAtomNameW';

  function AddAtom(lpString:LPCWSTR):ATOM; external External_library name 'AddAtomW';

  function FindAtom(lpString:LPCWSTR):ATOM; external External_library name 'FindAtomW';

  function GetAtomName(nAtom:ATOM; lpBuffer:LPWSTR; nSize:longint):UINT; external External_library name 'GetAtomNameW';

  function GetProfileInt(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT):UINT; external External_library name 'GetProfileIntW';

  function GetProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD; external External_library name 'GetProfileStringW';

  function WriteProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR):WINBOOL; external External_library name 'WriteProfileStringW';

  function GetProfileSection(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD):DWORD; external External_library name 'GetProfileSectionW';

  function WriteProfileSection(lpAppName:LPCWSTR; lpString:LPCWSTR):WINBOOL; external External_library name 'WriteProfileSectionW';

  function GetPrivateProfileInt(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; nDefault:INT; lpFileName:LPCWSTR):UINT; external External_library name 'GetPrivateProfileIntW';

  function GetPrivateProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpDefault:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; 
             lpFileName:LPCWSTR):DWORD; external External_library name 'GetPrivateProfileStringW';

  function WritePrivateProfileString(lpAppName:LPCWSTR; lpKeyName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL; external External_library name 'WritePrivateProfileStringW';

  function GetPrivateProfileSection(lpAppName:LPCWSTR; lpReturnedString:LPWSTR; nSize:DWORD; lpFileName:LPCWSTR):DWORD; external External_library name 'GetPrivateProfileSectionW';

  function WritePrivateProfileSection(lpAppName:LPCWSTR; lpString:LPCWSTR; lpFileName:LPCWSTR):WINBOOL; external External_library name 'WritePrivateProfileSectionW';

  function GetDriveType(lpRootPathName:LPCWSTR):UINT; external External_library name 'GetDriveTypeW';

  function GetSystemDirectory(lpBuffer:LPWSTR; uSize:UINT):UINT; external External_library name 'GetSystemDirectoryW';

  function GetTempPath(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external External_library name 'GetTempPathW';

  function GetTempFileName(lpPathName:LPCWSTR; lpPrefixString:LPCWSTR; uUnique:UINT; lpTempFileName:LPWSTR):UINT; external External_library name 'GetTempFileNameW';

  function GetWindowsDirectory(lpBuffer:LPWSTR; uSize:UINT):UINT; external External_library name 'GetWindowsDirectoryW';

  function SetCurrentDirectory(lpPathName:LPCWSTR):WINBOOL; external External_library name 'SetCurrentDirectoryW';

  function GetCurrentDirectory(nBufferLength:DWORD; lpBuffer:LPWSTR):DWORD; external External_library name 'GetCurrentDirectoryW';

  function GetDiskFreeSpace(lpRootPathName:LPCWSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL; external External_library name 'GetDiskFreeSpaceW';

  function CreateDirectory(lpPathName:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external External_library name 'CreateDirectoryW';

  function CreateDirectoryEx(lpTemplateDirectory:LPCWSTR; lpNewDirectory:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external External_library name 'CreateDirectoryExW';

  function RemoveDirectory(lpPathName:LPCWSTR):WINBOOL; external External_library name 'RemoveDirectoryW';

  function GetFullPathName(lpFileName:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; var lpFilePart:LPWSTR):DWORD; external External_library name 'GetFullPathNameW';

  function DefineDosDevice(dwFlags:DWORD; lpDeviceName:LPCWSTR; lpTargetPath:LPCWSTR):WINBOOL; external External_library name 'DefineDosDeviceW';

  function QueryDosDevice(lpDeviceName:LPCWSTR; lpTargetPath:LPWSTR; ucchMax:DWORD):DWORD; external External_library name 'QueryDosDeviceW';

  function CreateFile(lpFileName:LPCWSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD; 
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external External_library name 'CreateFileW';

  function SetFileAttributes(lpFileName:LPCWSTR; dwFileAttributes:DWORD):WINBOOL; external External_library name 'SetFileAttributesW';

  function GetFileAttributes(lpFileName:LPCWSTR):DWORD; external External_library name 'GetFileAttributesW';

  function GetCompressedFileSize(lpFileName:LPCWSTR; lpFileSizeHigh:LPDWORD):DWORD; external External_library name 'GetCompressedFileSizeW';

  function DeleteFile(lpFileName:LPCWSTR):WINBOOL; external External_library name 'DeleteFileW';

  function SearchPath(lpPath:LPCWSTR; lpFileName:LPCWSTR; lpExtension:LPCWSTR; nBufferLength:DWORD; lpBuffer:LPWSTR; 
             var lpFilePart:LPWSTR):DWORD; external External_library name 'SearchPathW';

  function CopyFile(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; bFailIfExists:WINBOOL):WINBOOL; external External_library name 'CopyFileW';

  function MoveFile(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR):WINBOOL; external External_library name 'MoveFileW';

  function MoveFileEx(lpExistingFileName:LPCWSTR; lpNewFileName:LPCWSTR; dwFlags:DWORD):WINBOOL; external External_library name 'MoveFileExW';

  function CreateNamedPipe(lpName:LPCWSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD; 
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external External_library name 'CreateNamedPipeW';

  function GetNamedPipeHandleState(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD; 
             lpUserName:LPWSTR; nMaxUserNameSize:DWORD):WINBOOL; external External_library name 'GetNamedPipeHandleStateW';

  function CallNamedPipe(lpNamedPipeName:LPCWSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD; 
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL; external External_library name 'CallNamedPipeW';

  function WaitNamedPipe(lpNamedPipeName:LPCWSTR; nTimeOut:DWORD):WINBOOL; external External_library name 'WaitNamedPipeW';

  function SetVolumeLabel(lpRootPathName:LPCWSTR; lpVolumeName:LPCWSTR):WINBOOL; external External_library name 'SetVolumeLabelW';

  function GetVolumeInformation(lpRootPathName:LPCWSTR; lpVolumeNameBuffer:LPWSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD; 
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPWSTR; nFileSystemNameSize:DWORD):WINBOOL; external External_library name 'GetVolumeInformationW';

  function ClearEventLog(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL; external External_library name 'ClearEventLogW';

  function BackupEventLog(hEventLog:HANDLE; lpBackupFileName:LPCWSTR):WINBOOL; external External_library name 'BackupEventLogW';

  function OpenEventLog(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE; external External_library name 'OpenEventLogW';

  function RegisterEventSource(lpUNCServerName:LPCWSTR; lpSourceName:LPCWSTR):HANDLE; external External_library name 'RegisterEventSourceW';

  function OpenBackupEventLog(lpUNCServerName:LPCWSTR; lpFileName:LPCWSTR):HANDLE; external External_library name 'OpenBackupEventLogW';

  function ReadEventLog(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; 
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL; external External_library name 'ReadEventLogW';

  function ReportEvent(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID; 
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCWSTR; lpRawData:LPVOID):WINBOOL; external External_library name 'ReportEventW';

  function AccessCheckAndAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR; 
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL; 
             pfGenerateOnClose:LPBOOL):WINBOOL; external External_library name 'AccessCheckAndAuditAlarmW';

  function ObjectOpenAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ObjectTypeName:LPWSTR; ObjectName:LPWSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR; 
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL; 
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL; external External_library name 'ObjectOpenAuditAlarmW';

  function ObjectPrivilegeAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET; 
             AccessGranted:WINBOOL):WINBOOL; external External_library name 'ObjectPrivilegeAuditAlarmW';

  function ObjectCloseAuditAlarm(SubsystemName:LPCWSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL; external External_library name 'ObjectCloseAuditAlarmW';

  function PrivilegedServiceAuditAlarm(SubsystemName:LPCWSTR; ServiceName:LPCWSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL; external External_library name 'PrivilegedServiceAuditAlarmW';

  function SetFileSecurity(lpFileName:LPCWSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external External_library name 'SetFileSecurityW';

  function GetFileSecurity(lpFileName:LPCWSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external External_library name 'GetFileSecurityW';

  function FindFirstChangeNotification(lpPathName:LPCWSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE; external External_library name 'FindFirstChangeNotificationW';

  function IsBadStringPtr(lpsz:LPCWSTR; ucchMax:UINT):WINBOOL; external External_library name 'IsBadStringPtrW';

  function LookupAccountSid(lpSystemName:LPCWSTR; Sid:PSID; Name:LPWSTR; cbName:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external External_library name 'LookupAccountSidW';

  function LookupAccountName(lpSystemName:LPCWSTR; lpAccountName:LPCWSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPWSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external External_library name 'LookupAccountNameW';

  function LookupPrivilegeValue(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpLuid:PLUID):WINBOOL; external External_library name 'LookupPrivilegeValueW';

  function LookupPrivilegeName(lpSystemName:LPCWSTR; lpLuid:PLUID; lpName:LPWSTR; cbName:LPDWORD):WINBOOL; external External_library name 'LookupPrivilegeNameW';

  function LookupPrivilegeDisplayName(lpSystemName:LPCWSTR; lpName:LPCWSTR; lpDisplayName:LPWSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL; external External_library name 'LookupPrivilegeDisplayNameW';

  function BuildCommDCB(lpDef:LPCWSTR; lpDCB:LPDCB):WINBOOL; external External_library name 'BuildCommDCBW';

  function BuildCommDCBAndTimeouts(lpDef:LPCWSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external External_library name 'BuildCommDCBAndTimeoutsW';

  function CommConfigDialog(lpszName:LPCWSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL; external External_library name 'CommConfigDialogW';

  function GetDefaultCommConfig(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external External_library name 'GetDefaultCommConfigW';

  function SetDefaultCommConfig(lpszName:LPCWSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external External_library name 'SetDefaultCommConfigW';

  function GetComputerName(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL; external External_library name 'GetComputerNameW';

  function SetComputerName(lpComputerName:LPCWSTR):WINBOOL; external External_library name 'SetComputerNameW';

  function GetUserName(lpBuffer:LPWSTR; nSize:LPDWORD):WINBOOL; external External_library name 'GetUserNameW';

  function wvsprintf(_para1:LPWSTR; _para2:LPCWSTR; arglist:va_list):longint; external External_library name 'wvsprintfW';

  {function wsprintf(_para1:LPWSTR; _para2:LPCWSTR; ...):longint;CDECL; external External_library name 'wsprintfW';}

  function LoadKeyboardLayout(pwszKLID:LPCWSTR; Flags:UINT):HKL; external External_library name 'LoadKeyboardLayoutW';

  function GetKeyboardLayoutName(pwszKLID:LPWSTR):WINBOOL; external External_library name 'GetKeyboardLayoutNameW';

  function CreateDesktop(lpszDesktop:LPWSTR; lpszDevice:LPWSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD; 
             lpsa:LPSECURITY_ATTRIBUTES):HDESK; external External_library name 'CreateDesktopW';

  function OpenDesktop(lpszDesktop:LPWSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external External_library name 'OpenDesktopW';

  function EnumDesktops(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL; external External_library name 'EnumDesktopsW';

  function CreateWindowStation(lpwinsta:LPWSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA; external External_library name 'CreateWindowStationW';

  function OpenWindowStation(lpszWinSta:LPWSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA; external External_library name 'OpenWindowStationW';

  function EnumWindowStations(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL; external External_library name 'EnumWindowStationsW';

  function GetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external External_library name 'GetUserObjectInformationW';

  function SetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL; external External_library name 'SetUserObjectInformationW';

  function RegisterWindowMessage(lpString:LPCWSTR):UINT; external External_library name 'RegisterWindowMessageW';

  function GetMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL; external External_library name 'GetMessageW';

  function DispatchMessage(var lpMsg:MSG):LONG; external External_library name 'DispatchMessageW';

  function PeekMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external External_library name 'PeekMessageW';

  function SendMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'SendMessageW';

  function SendMessageTimeout(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT; 
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external External_library name 'SendMessageTimeoutW';

  function SendNotifyMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external External_library name 'SendNotifyMessageW';

  function SendMessageCallback(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC; 
             dwData:DWORD):WINBOOL; external External_library name 'SendMessageCallbackW';

  function PostMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external External_library name 'PostMessageW';

  function PostThreadMessage(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external External_library name 'PostThreadMessageW';

  function DefWindowProc(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefWindowProcW';

  function CallWindowProc(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'CallWindowProcW';

  function RegisterClass(var lpWndClass:WNDCLASS):ATOM; external External_library name 'RegisterClassW';

  function UnregisterClass(lpClassName:LPCWSTR; hInstance:HINSTANCE):WINBOOL; external External_library name 'UnregisterClassW';

  function GetClassInfo(hInstance:HINSTANCE; lpClassName:LPCWSTR; lpWndClass:LPWNDCLASS):WINBOOL; external External_library name 'GetClassInfoW';

  function RegisterClassEx(var _para1:WNDCLASSEX):ATOM; external External_library name 'RegisterClassExW';

  function GetClassInfoEx(_para1:HINSTANCE; _para2:LPCWSTR; _para3:LPWNDCLASSEX):WINBOOL; external External_library name 'GetClassInfoExW';

  function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint; 
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND; external External_library name 'CreateWindowExW';

  function CreateDialogParam(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external External_library name 'CreateDialogParamW';

  function CreateDialogIndirectParam(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external External_library name 'CreateDialogIndirectParamW';

  function DialogBoxParam(hInstance:HINSTANCE; lpTemplateName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external External_library name 'DialogBoxParamW';

  function DialogBoxIndirectParam(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external External_library name 'DialogBoxIndirectParamW';

  function SetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPCWSTR):WINBOOL; external External_library name 'SetDlgItemTextW';

  function GetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPWSTR; nMaxCount:longint):UINT; external External_library name 'GetDlgItemTextW';

  function SendDlgItemMessage(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG; external External_library name 'SendDlgItemMessageW';

  function DefDlgProc(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefDlgProcW';

  function CallMsgFilter(lpMsg:LPMSG; nCode:longint):WINBOOL; external External_library name 'CallMsgFilterW';

  function RegisterClipboardFormat(lpszFormat:LPCWSTR):UINT; external External_library name 'RegisterClipboardFormatW';

  function GetClipboardFormatName(format:UINT; lpszFormatName:LPWSTR; cchMaxCount:longint):longint; external External_library name 'GetClipboardFormatNameW';

  function CharToOem(lpszSrc:LPCWSTR; lpszDst:LPSTR):WINBOOL; external External_library name 'CharToOemW';

  function OemToChar(lpszSrc:LPCSTR; lpszDst:LPWSTR):WINBOOL; external External_library name 'OemToCharW';

  function CharToOemBuff(lpszSrc:LPCWSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external External_library name 'CharToOemBuffW';

  function OemToCharBuff(lpszSrc:LPCSTR; lpszDst:LPWSTR; cchDstLength:DWORD):WINBOOL; external External_library name 'OemToCharBuffW';

  function CharUpper(lpsz:LPWSTR):LPWSTR; external External_library name 'CharUpperW';

  function CharUpperBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD; external External_library name 'CharUpperBuffW';

  function CharLower(lpsz:LPWSTR):LPWSTR; external External_library name 'CharLowerW';

  function CharLowerBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD; external External_library name 'CharLowerBuffW';

  function CharNext(lpsz:LPCWSTR):LPWSTR; external External_library name 'CharNextW';

  function CharPrev(lpszStart:LPCWSTR; lpszCurrent:LPCWSTR):LPWSTR; external External_library name 'CharPrevW';

  function IsCharAlpha(ch:WCHAR):WINBOOL; external External_library name 'IsCharAlphaW';

  function IsCharAlphaNumeric(ch:WCHAR):WINBOOL; external External_library name 'IsCharAlphaNumericW';

  function IsCharUpper(ch:WCHAR):WINBOOL; external External_library name 'IsCharUpperW';

  function IsCharLower(ch:WCHAR):WINBOOL; external External_library name 'IsCharLowerW';

  function GetKeyNameText(lParam:LONG; lpString:LPWSTR; nSize:longint):longint; external External_library name 'GetKeyNameTextW';

  function VkKeyScan(ch:WCHAR):SHORT; external External_library name 'VkKeyScanW';

  function VkKeyScanEx(ch:WCHAR; dwhkl:HKL):SHORT; external External_library name 'VkKeyScanExW';

  function MapVirtualKey(uCode:UINT; uMapType:UINT):UINT; external External_library name 'MapVirtualKeyW';

  function MapVirtualKeyEx(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT; external External_library name 'MapVirtualKeyExW';

  function LoadAccelerators(hInstance:HINSTANCE; lpTableName:LPCWSTR):HACCEL; external External_library name 'LoadAcceleratorsW';

  function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL; external External_library name 'CreateAcceleratorTableW';

  function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint; external External_library name 'CopyAcceleratorTableW';

  function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint; external External_library name 'TranslateAcceleratorW';

  function LoadMenu(hInstance:HINSTANCE; lpMenuName:LPCWSTR):HMENU; external External_library name 'LoadMenuW';

  function LoadMenuIndirect(var lpMenuTemplate:MENUTEMPLATE):HMENU; external External_library name 'LoadMenuIndirectW';

  function ChangeMenu(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCWSTR; cmdInsert:UINT; flags:UINT):WINBOOL; external External_library name 'ChangeMenuW';

  function GetMenuString(hMenu:HMENU; uIDItem:UINT; lpString:LPWSTR; nMaxCount:longint; uFlag:UINT):longint; external External_library name 'GetMenuStringW';

  function InsertMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external External_library name 'InsertMenuW';

  function AppendMenu(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external External_library name 'AppendMenuW';

  function ModifyMenu(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCWSTR):WINBOOL; external External_library name 'ModifyMenuW';

  function InsertMenuItem(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external External_library name 'InsertMenuItemW';

  function GetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL; external External_library name 'GetMenuItemInfoW';

  function SetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external External_library name 'SetMenuItemInfoW';

  function DrawText(hDC:HDC; lpString:LPCWSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint; external External_library name 'DrawTextW';

  function DrawTextEx(_para1:HDC; _para2:LPWSTR; _para3:longint; _para4:LPRECT; _para5:UINT; 
             _para6:LPDRAWTEXTPARAMS):longint; external External_library name 'DrawTextExW';

  function GrayString(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint; 
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL; external External_library name 'GrayStringW';

  function DrawState(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM; 
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL; external External_library name 'DrawStateW';

  function TabbedTextOut(hDC:HDC; X:longint; Y:longint; lpString:LPCWSTR; nCount:longint; 
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG; external External_library name 'TabbedTextOutW';

  function GetTabbedTextExtent(hDC:HDC; lpString:LPCWSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD; external External_library name 'GetTabbedTextExtentW';

  function SetProp(hWnd:HWND; lpString:LPCWSTR; hData:HANDLE):WINBOOL; external External_library name 'SetPropW';

  function GetProp(hWnd:HWND; lpString:LPCWSTR):HANDLE; external External_library name 'GetPropW';

  function RemoveProp(hWnd:HWND; lpString:LPCWSTR):HANDLE; external External_library name 'RemovePropW';

  function EnumPropsEx(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint; external External_library name 'EnumPropsExW';

  function EnumProps(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint; external External_library name 'EnumPropsW';

  function SetWindowText(hWnd:HWND; lpString:LPCWSTR):WINBOOL; external External_library name 'SetWindowTextW';

  function GetWindowText(hWnd:HWND; lpString:LPWSTR; nMaxCount:longint):longint; external External_library name 'GetWindowTextW';

  function GetWindowTextLength(hWnd:HWND):longint; external External_library name 'GetWindowTextLengthW';

  function MessageBox(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT):longint; external External_library name 'MessageBoxW';

  function MessageBoxEx(hWnd:HWND; lpText:LPCWSTR; lpCaption:LPCWSTR; uType:UINT; wLanguageId:WORD):longint; external External_library name 'MessageBoxExW';

  function MessageBoxIndirect(_para1:LPMSGBOXPARAMS):longint; external External_library name 'MessageBoxIndirectW';

  function GetWindowLong(hWnd:HWND; nIndex:longint):LONG; external External_library name 'GetWindowLongW';

  function SetWindowLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG; external External_library name 'SetWindowLongW';

  function GetClassLong(hWnd:HWND; nIndex:longint):DWORD; external External_library name 'GetClassLongW';

  function SetClassLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD; external External_library name 'SetClassLongW';

  function FindWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR):HWND; external External_library name 'FindWindowW';

  function FindWindowEx(_para1:HWND; _para2:HWND; _para3:LPCWSTR; _para4:LPCWSTR):HWND; external External_library name 'FindWindowExW';

  function GetClassName(hWnd:HWND; lpClassName:LPWSTR; nMaxCount:longint):longint; external External_library name 'GetClassNameW';

  function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINSTANCE; dwThreadId:DWORD):HHOOK; external External_library name 'SetWindowsHookExW';

  function LoadBitmap(hInstance:HINSTANCE; lpBitmapName:LPCWSTR):HBITMAP; external External_library name 'LoadBitmapW';

  function LoadCursor(hInstance:HINSTANCE; lpCursorName:LPCWSTR):HCURSOR; external External_library name 'LoadCursorW';

  function LoadCursorFromFile(lpFileName:LPCWSTR):HCURSOR; external External_library name 'LoadCursorFromFileW';

  function LoadIcon(hInstance:HINSTANCE; lpIconName:LPCWSTR):HICON; external External_library name 'LoadIconW';

  function LoadImage(_para1:HINSTANCE; _para2:LPCWSTR; _para3:UINT; _para4:longint; _para5:longint; 
             _para6:UINT):HANDLE; external External_library name 'LoadImageW';

  function LoadString(hInstance:HINSTANCE; uID:UINT; lpBuffer:LPWSTR; nBufferMax:longint):longint; external External_library name 'LoadStringW';

  function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL; external External_library name 'IsDialogMessageW';

  function DlgDirList(hDlg:HWND; lpPathSpec:LPWSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint; external External_library name 'DlgDirListW';

  function DlgDirSelectEx(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDListBox:longint):WINBOOL; external External_library name 'DlgDirSelectExW';

  function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPWSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint; external External_library name 'DlgDirListComboBoxW';

  function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPWSTR; nCount:longint; nIDComboBox:longint):WINBOOL; external External_library name 'DlgDirSelectComboBoxExW';

  function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefFrameProcW';

  function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefMDIChildProcW';

  function CreateMDIWindow(lpClassName:LPWSTR; lpWindowName:LPWSTR; dwStyle:DWORD; X:longint; Y:longint; 
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINSTANCE; lParam:LPARAM):HWND; external External_library name 'CreateMDIWindowW';

  function WinHelp(hWndMain:HWND; lpszHelp:LPCWSTR; uCommand:UINT; dwData:DWORD):WINBOOL; external External_library name 'WinHelpW';

  function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG; external External_library name 'ChangeDisplaySettingsW';

  function EnumDisplaySettings(lpszDeviceName:LPCWSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL; external External_library name 'EnumDisplaySettingsW';

  function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL; external External_library name 'SystemParametersInfoW';

  function AddFontResource(_para1:LPCWSTR):longint; external External_library name 'AddFontResourceW';

  function CopyMetaFile(_para1:HMETAFILE; _para2:LPCWSTR):HMETAFILE; external External_library name 'CopyMetaFileW';

  function CreateFontIndirect(var _para1:LOGFONT):HFONT; external External_library name 'CreateFontIndirectW';

  function CreateFont(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCWSTR):HFONT; external External_library name 'CreateFontW';

  function CreateIC(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC; external External_library name 'CreateICW';

  function CreateMetaFile(_para1:LPCWSTR):HDC; external External_library name 'CreateMetaFileW';

  function CreateScalableFontResource(_para1:DWORD; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR):WINBOOL; external External_library name 'CreateScalableFontResourceW';

  function DeviceCapabilities(_para1:LPCWSTR; _para2:LPCWSTR; _para3:WORD; _para4:LPWSTR; var _para5:DEVMODE):longint; external External_library name 'DeviceCapabilitiesW';

  function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint; external External_library name 'EnumFontFamiliesExW';

  function EnumFontFamilies(_para1:HDC; _para2:LPCWSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint; external External_library name 'EnumFontFamiliesW';

  function EnumFonts(_para1:HDC; _para2:LPCWSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint; external External_library name 'EnumFontsW';

  function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external External_library name 'GetCharWidthW';

  function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external External_library name 'GetCharWidth32W';

  function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL; external External_library name 'GetCharWidthFloatW';

  function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external External_library name 'GetCharABCWidthsW';

  function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL; external External_library name 'GetCharABCWidthsFloatW';

  function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD; 
             _para6:LPVOID; var _para7:MAT2):DWORD; external External_library name 'GetGlyphOutlineW';

  function GetMetaFile(_para1:LPCWSTR):HMETAFILE; external External_library name 'GetMetaFileW';

  function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT; external External_library name 'GetOutlineTextMetricsW';

  function GetTextExtentPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external External_library name 'GetTextExtentPointW';

  function GetTextExtentPoint32(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external External_library name 'GetTextExtentPoint32W';

  function GetTextExtentExPoint(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPINT; 
             _para6:LPINT; _para7:LPSIZE):WINBOOL; external External_library name 'GetTextExtentExPointW';

  function GetCharacterPlacement(_para1:HDC; _para2:LPCWSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS; 
             _para6:DWORD):DWORD; external External_library name 'GetCharacterPlacementW';

  function ResetDC(_para1:HDC; var _para2:DEVMODE):HDC; external External_library name 'ResetDCW';

  function RemoveFontResource(_para1:LPCWSTR):WINBOOL; external External_library name 'RemoveFontResourceW';

  function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCWSTR):HENHMETAFILE; external External_library name 'CopyEnhMetaFileW';

  function CreateEnhMetaFile(_para1:HDC; _para2:LPCWSTR; var _para3:RECT; _para4:LPCWSTR):HDC; external External_library name 'CreateEnhMetaFileW';

  function GetEnhMetaFile(_para1:LPCWSTR):HENHMETAFILE; external External_library name 'GetEnhMetaFileW';

  function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPWSTR):UINT; external External_library name 'GetEnhMetaFileDescriptionW';

  function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL; external External_library name 'GetTextMetricsW';

  function StartDoc(_para1:HDC; var _para2:DOCINFO):longint; external External_library name 'StartDocW';

  function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint; external External_library name 'GetObjectW';

  function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCWSTR; _para5:longint):WINBOOL; external External_library name 'TextOutW';

  function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT; 
             _para6:LPCWSTR; _para7:UINT; var _para8:INT):WINBOOL; external External_library name 'ExtTextOutW';

  function PolyTextOut(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL; external External_library name 'PolyTextOutW';

  function GetTextFace(_para1:HDC; _para2:longint; _para3:LPWSTR):longint; external External_library name 'GetTextFaceW';

  function GetKerningPairs(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD; external External_library name 'GetKerningPairsW';

  function GetLogColorSpace(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL; external External_library name 'GetLogColorSpaceW';

  function CreateColorSpace(_para1:LPLOGCOLORSPACE):HCOLORSPACE; external External_library name 'CreateColorSpaceW';

  function GetICMProfile(_para1:HDC; _para2:DWORD; _para3:LPWSTR):WINBOOL; external External_library name 'GetICMProfileW';

  function SetICMProfile(_para1:HDC; _para2:LPWSTR):WINBOOL; external External_library name 'SetICMProfileW';

  function UpdateICMRegKey(_para1:DWORD; _para2:DWORD; _para3:LPWSTR; _para4:UINT):WINBOOL; external External_library name 'UpdateICMRegKeyW';

  function EnumICMProfiles(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint; external External_library name 'EnumICMProfilesW';

  function CreatePropertySheetPage(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external External_library name 'CreatePropertySheetPageW';

  function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint; external External_library name 'PropertySheetW';

  function ImageList_LoadImage(hi:HINSTANCE; lpbmp:LPCWSTR; cx:longint; cGrow:longint; crMask:COLORREF; 
             uType:UINT; uFlags:UINT):HIMAGELIST; external External_library name 'ImageList_LoadImageW';

  function CreateStatusWindow(style:LONG; lpszText:LPCWSTR; hwndParent:HWND; wID:UINT):HWND; external External_library name 'CreateStatusWindowW';

  procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCWSTR; uFlags:UINT); external External_library name 'DrawStatusTextW';

  function GetOpenFileName(_para1:LPOPENFILENAME):WINBOOL; external External_library name 'GetOpenFileNameW';

  function GetSaveFileName(_para1:LPOPENFILENAME):WINBOOL; external External_library name 'GetSaveFileNameW';

  function GetFileTitle(_para1:LPCWSTR; _para2:LPWSTR; _para3:WORD):integer; external External_library name 'GetFileTitleW';

  function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL; external External_library name 'ChooseColorW';

  function ReplaceText(_para1:LPFINDREPLACE):HWND; external External_library name 'ReplaceTextW';

  function ChooseFont(_para1:LPCHOOSEFONT):WINBOOL; external External_library name 'ChooseFontW';

  function FindText(_para1:LPFINDREPLACE):HWND; external External_library name 'FindTextW';

  function PrintDlg(_para1:LPPRINTDLG):WINBOOL; external External_library name 'PrintDlgW';

  function PageSetupDlg(_para1:LPPAGESETUPDLG):WINBOOL; external External_library name 'PageSetupDlgW';

  function CreateProcess(lpApplicationName:LPCWSTR; lpCommandLine:LPWSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL; 
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCWSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL; external External_library name 'CreateProcessW';

  procedure GetStartupInfo(lpStartupInfo:LPSTARTUPINFO); external External_library name 'GetStartupInfoW';

  function FindFirstFile(lpFileName:LPCWSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE; external External_library name 'FindFirstFileW';

  function FindNextFile(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL; external External_library name 'FindNextFileW';

  function GetVersionEx(lpVersionInformation:LPOSVERSIONINFO):WINBOOL; external External_library name 'GetVersionExW';

  { was #define dname(params) def_expr }
  function CreateWindow(lpClassName:LPCWSTR; lpWindowName:LPCWSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;
    begin
       CreateWindow:=CreateWindowExW(0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam);
    end;

  { was #define dname(params) def_expr }
  function CreateDialog(hInstance:HINSTANCE; lpName:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialog:=CreateDialogParamW(hInstance,lpName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogIndirect(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogIndirect:=CreateDialogIndirectParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBox(hInstance:HINSTANCE; lpTemplate:LPCWSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBox:=DialogBoxParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxIndirect(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxIndirect:=DialogBoxIndirectParamW(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  function CreateDC(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR; var _para4:DEVMODE):HDC; external External_library name 'CreateDCW';

  function CreateFontA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint; 
             _para6:DWORD; _para7:DWORD; _para8:DWORD; _para9:DWORD; _para10:DWORD; 
             _para11:DWORD; _para12:DWORD; _para13:DWORD; _para14:LPCSTR):HFONT; external External_library name 'CreateFontA';

  function VerInstallFile(uFlags:DWORD; szSrcFileName:LPWSTR; szDestFileName:LPWSTR; szSrcDir:LPWSTR; szDestDir:LPWSTR; 
             szCurDir:LPWSTR; szTmpFile:LPWSTR; lpuTmpFileLen:PUINT):DWORD; external External_library name 'VerInstallFileW';

  function GetFileVersionInfoSize(lptstrFilename:LPWSTR; lpdwHandle:LPDWORD):DWORD; external External_library name 'GetFileVersionInfoSizeW';

  function GetFileVersionInfo(lptstrFilename:LPWSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external External_library name 'GetFileVersionInfoW';

  function VerLanguageName(wLang:DWORD; szLang:LPWSTR; nSize:DWORD):DWORD; external External_library name 'VerLanguageNameW';

  function VerQueryValue(pBlock:LPVOID; lpSubBlock:LPWSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external External_library name 'VerQueryValueW';

  function VerFindFile(uFlags:DWORD; szFileName:LPWSTR; szWinDir:LPWSTR; szAppDir:LPWSTR; szCurDir:LPWSTR; 
             lpuCurDirLen:PUINT; szDestDir:LPWSTR; lpuDestDirLen:PUINT):DWORD; external External_library name 'VerFindFileW';

  function RegSetValueEx(hKey:HKEY; lpValueName:LPCWSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE; 
             cbData:DWORD):LONG; external External_library name 'RegSetValueExW';

  function RegUnLoadKey(hKey:HKEY; lpSubKey:LPCWSTR):LONG; external External_library name 'RegUnLoadKeyW';

  function InitiateSystemShutdown(lpMachineName:LPWSTR; lpMessage:LPWSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL; external External_library name 'InitiateSystemShutdownW';

  function AbortSystemShutdown(lpMachineName:LPWSTR):WINBOOL; external External_library name 'AbortSystemShutdownW';

  function RegRestoreKey(hKey:HKEY; lpFile:LPCWSTR; dwFlags:DWORD):LONG; external External_library name 'RegRestoreKeyW';

  function RegSaveKey(hKey:HKEY; lpFile:LPCWSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external External_library name 'RegSaveKeyW';

  function RegSetValue(hKey:HKEY; lpSubKey:LPCWSTR; dwType:DWORD; lpData:LPCWSTR; cbData:DWORD):LONG; external External_library name 'RegSetValueW';

  function RegQueryValue(hKey:HKEY; lpSubKey:LPCWSTR; lpValue:LPWSTR; lpcbValue:PLONG):LONG; external External_library name 'RegQueryValueW';

  function RegQueryMultipleValues(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPWSTR; ldwTotsize:LPDWORD):LONG; external External_library name 'RegQueryMultipleValuesW';

  function RegQueryValueEx(hKey:HKEY; lpValueName:LPCWSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE; 
             lpcbData:LPDWORD):LONG; external External_library name 'RegQueryValueExW';

  function RegReplaceKey(hKey:HKEY; lpSubKey:LPCWSTR; lpNewFile:LPCWSTR; lpOldFile:LPCWSTR):LONG; external External_library name 'RegReplaceKeyW';

  function RegConnectRegistry(lpMachineName:LPWSTR; hKey:HKEY; phkResult:PHKEY):LONG; external External_library name 'RegConnectRegistryW';

  function RegCreateKey(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG; external External_library name 'RegCreateKeyW';

  function RegCreateKeyEx(hKey:HKEY; lpSubKey:LPCWSTR; Reserved:DWORD; lpClass:LPWSTR; dwOptions:DWORD; 
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG; external External_library name 'RegCreateKeyExW';

  function RegDeleteKey(hKey:HKEY; lpSubKey:LPCWSTR):LONG; external External_library name 'RegDeleteKeyW';

  function RegDeleteValue(hKey:HKEY; lpValueName:LPCWSTR):LONG; external External_library name 'RegDeleteValueW';

  function RegEnumKey(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; cbName:DWORD):LONG; external External_library name 'RegEnumKeyW';

  function RegEnumKeyEx(hKey:HKEY; dwIndex:DWORD; lpName:LPWSTR; lpcbName:LPDWORD; lpReserved:LPDWORD; 
             lpClass:LPWSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external External_library name 'RegEnumKeyExW';

  function RegEnumValue(hKey:HKEY; dwIndex:DWORD; lpValueName:LPWSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD; 
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG; external External_library name 'RegEnumValueW';

  function RegLoadKey(hKey:HKEY; lpSubKey:LPCWSTR; lpFile:LPCWSTR):LONG; external External_library name 'RegLoadKeyW';

  function RegOpenKey(hKey:HKEY; lpSubKey:LPCWSTR; phkResult:PHKEY):LONG; external External_library name 'RegOpenKeyW';

  function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCWSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG; external External_library name 'RegOpenKeyExW';

  function RegQueryInfoKey(hKey:HKEY; lpClass:LPWSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD; 
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD; 
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external External_library name 'RegQueryInfoKeyW';

  function CompareString(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCWSTR; cchCount1:longint; lpString2:LPCWSTR; 
             cchCount2:longint):longint; external External_library name 'CompareStringW';

  function LCMapString(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; 
             cchDest:longint):longint; external External_library name 'LCMapStringW';

  function GetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPWSTR; cchData:longint):longint; external External_library name 'GetLocaleInfoW';

  function SetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPCWSTR):WINBOOL; external External_library name 'SetLocaleInfoW';

  function GetTimeFormat(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCWSTR; lpTimeStr:LPWSTR; 
             cchTime:longint):longint; external External_library name 'GetTimeFormatW';

  function GetDateFormat(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCWSTR; lpDateStr:LPWSTR; 
             cchDate:longint):longint; external External_library name 'GetDateFormatW';

  function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPWSTR; 
             cchNumber:longint):longint; external External_library name 'GetNumberFormatW';

  function GetCurrencyFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCWSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPWSTR; 
             cchCurrency:longint):longint; external External_library name 'GetCurrencyFormatW';

  function EnumCalendarInfo(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL; external External_library name 'EnumCalendarInfoW';

  function EnumTimeFormats(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external External_library name 'EnumTimeFormatsW';

  function EnumDateFormats(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external External_library name 'EnumDateFormatsW';

  function GetStringTypeEx(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external External_library name 'GetStringTypeExW';

  function GetStringType(dwInfoType:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external External_library name 'GetStringTypeW';

  function FoldString(dwMapFlags:DWORD; lpSrcStr:LPCWSTR; cchSrc:longint; lpDestStr:LPWSTR; cchDest:longint):longint; external External_library name 'FoldStringW';

  function EnumSystemLocales(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL; external External_library name 'EnumSystemLocalesW';

  function EnumSystemCodePages(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL; external External_library name 'EnumSystemCodePagesW';

  function PeekConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external External_library name 'PeekConsoleInputW';

  function ReadConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external External_library name 'ReadConsoleInputW';

  function WriteConsoleInput(hConsoleInput:HANDLE; var lpBuffer:INPUT_RECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL; external External_library name 'WriteConsoleInputW';

  function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL; external External_library name 'ReadConsoleOutputW';

  function WriteConsoleOutput(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL; external External_library name 'WriteConsoleOutputW';

  function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPWSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL; external External_library name 'ReadConsoleOutputCharacterW';

  function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCWSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external External_library name 'WriteConsoleOutputCharacterW';

  function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:WCHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external External_library name 'FillConsoleOutputCharacterW';

  function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL; external External_library name 'ScrollConsoleScreenBufferW';

  function GetConsoleTitle(lpConsoleTitle:LPWSTR; nSize:DWORD):DWORD; external External_library name 'GetConsoleTitleW';

  function SetConsoleTitle(lpConsoleTitle:LPCWSTR):WINBOOL; external External_library name 'SetConsoleTitleW';

  function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL; external External_library name 'ReadConsoleW';

  function WriteConsole(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL; external External_library name 'WriteConsoleW';

  function WNetAddConnection(lpRemoteName:LPCWSTR; lpPassword:LPCWSTR; lpLocalName:LPCWSTR):DWORD; external External_library name 'WNetAddConnectionW';

  function WNetAddConnection2(lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD; external External_library name 'WNetAddConnection2W';

  function WNetAddConnection3(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCWSTR; lpUserName:LPCWSTR; dwFlags:DWORD):DWORD; external External_library name 'WNetAddConnection3W';

  function WNetCancelConnection(lpName:LPCWSTR; fForce:WINBOOL):DWORD; external External_library name 'WNetCancelConnectionW';

  function WNetCancelConnection2(lpName:LPCWSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD; external External_library name 'WNetCancelConnection2W';

  function WNetGetConnection(lpLocalName:LPCWSTR; lpRemoteName:LPWSTR; lpnLength:LPDWORD):DWORD; external External_library name 'WNetGetConnectionW';

  function WNetUseConnection(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCWSTR; lpPassword:LPCWSTR; dwFlags:DWORD; 
             lpAccessName:LPWSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD; external External_library name 'WNetUseConnectionW';

  function WNetSetConnection(lpName:LPCWSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD; external External_library name 'WNetSetConnectionW';

  function WNetConnectionDialog1(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD; external External_library name 'WNetConnectionDialog1W';

  function WNetDisconnectDialog1(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD; external External_library name 'WNetDisconnectDialog1W';

  function WNetOpenEnum(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD; external External_library name 'WNetOpenEnumW';

  function WNetEnumResource(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external External_library name 'WNetEnumResourceW';

  function WNetGetUniversalName(lpLocalPath:LPCWSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external External_library name 'WNetGetUniversalNameW';

  function WNetGetUser(lpName:LPCWSTR; lpUserName:LPWSTR; lpnLength:LPDWORD):DWORD; external External_library name 'WNetGetUserW';

  function WNetGetProviderName(dwNetType:DWORD; lpProviderName:LPWSTR; lpBufferSize:LPDWORD):DWORD; external External_library name 'WNetGetProviderNameW';

  function WNetGetNetworkInformation(lpProvider:LPCWSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD; external External_library name 'WNetGetNetworkInformationW';

  function WNetGetLastError(lpError:LPDWORD; lpErrorBuf:LPWSTR; nErrorBufSize:DWORD; lpNameBuf:LPWSTR; nNameBufSize:DWORD):DWORD; external External_library name 'WNetGetLastErrorW';

  function MultinetGetConnectionPerformance(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD; external External_library name 'MultinetGetConnectionPerformanceW';

  function ChangeServiceConfig(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; 
             lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR; 
             lpDisplayName:LPCWSTR):WINBOOL; external External_library name 'ChangeServiceConfigW';

  function CreateService(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPCWSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD; 
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCWSTR; lpLoadOrderGroup:LPCWSTR; lpdwTagId:LPDWORD; 
             lpDependencies:LPCWSTR; lpServiceStartName:LPCWSTR; lpPassword:LPCWSTR):SC_HANDLE; external External_library name 'CreateServiceW';

  function EnumDependentServices(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD; 
             lpServicesReturned:LPDWORD):WINBOOL; external External_library name 'EnumDependentServicesW';

  function EnumServicesStatus(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; 
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL; external External_library name 'EnumServicesStatusW';

  function GetServiceKeyName(hSCManager:SC_HANDLE; lpDisplayName:LPCWSTR; lpServiceName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL; external External_library name 'GetServiceKeyNameW';

  function GetServiceDisplayName(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; lpDisplayName:LPWSTR; lpcchBuffer:LPDWORD):WINBOOL; external External_library name 'GetServiceDisplayNameW';

  function OpenSCManager(lpMachineName:LPCWSTR; lpDatabaseName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE; external External_library name 'OpenSCManagerW';

  function OpenService(hSCManager:SC_HANDLE; lpServiceName:LPCWSTR; dwDesiredAccess:DWORD):SC_HANDLE; external External_library name 'OpenServiceW';

  function QueryServiceConfig(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external External_library name 'QueryServiceConfigW';

  function QueryServiceLockStatus(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external External_library name 'QueryServiceLockStatusW';

  function RegisterServiceCtrlHandler(lpServiceName:LPCWSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE; external External_library name 'RegisterServiceCtrlHandlerW';

  function StartServiceCtrlDispatcher(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL; external External_library name 'StartServiceCtrlDispatcherW';

  function StartService(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCWSTR):WINBOOL; external External_library name 'StartServiceW';

  function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external External_library name 'wglUseFontBitmapsW';

  function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:FLOAT; 
             _para6:FLOAT; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external External_library name 'wglUseFontOutlinesW';

  function DragQueryFile(_para1:HDROP; _para2:cardinal; _para3:LPCWSTR; _para4:cardinal):cardinal; external External_library name 'DragQueryFileW';

  function ExtractAssociatedIcon(_para1:HINSTANCE; _para2:LPCWSTR; var _para3:WORD):HICON; external External_library name 'ExtractAssociatedIconW';

  function ExtractIcon(_para1:HINSTANCE; _para2:LPCWSTR; _para3:cardinal):HICON; external External_library name 'ExtractIconW';

  function FindExecutable(_para1:LPCWSTR; _para2:LPCWSTR; _para3:LPCWSTR):HINSTANCE; external External_library name 'FindExecutableW';

  function ShellAbout(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:HICON):longint; external External_library name 'ShellAboutW';

  function ShellExecute(_para1:HWND; _para2:LPCWSTR; _para3:LPCWSTR; _para4:LPCWSTR; _para5:LPCWSTR; 
             _para6:longint):HINSTANCE; external External_library name 'ShellExecuteW';

  function DdeCreateStringHandle(_para1:DWORD; _para2:LPCWSTR; _para3:longint):HSZ; external External_library name 'DdeCreateStringHandleW';

  function DdeInitialize(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT; external External_library name 'DdeInitializeW';

  function DdeQueryString(_para1:DWORD; _para2:HSZ; _para3:LPCWSTR; _para4:DWORD; _para5:longint):DWORD; external External_library name 'DdeQueryStringW';

  function LogonUser(_para1:LPWSTR; _para2:LPWSTR; _para3:LPWSTR; _para4:DWORD; _para5:DWORD; 
             var _para6:HANDLE):WINBOOL; external External_library name 'LogonUserW';

  function CreateProcessAsUser(_para1:HANDLE; _para2:LPCWSTR; _para3:LPWSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES; 
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCWSTR; var _para10:STARTUPINFO; 
             var _para11:PROCESS_INFORMATION):WINBOOL; external External_library name 'CreateProcessAsUserW';


{$endif read_implementation}

{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.1  1998-08-31 11:54:01  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

}
