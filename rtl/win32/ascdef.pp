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

unit ascdef;

{  Automatically converted by H2PAS.EXE from asciifun.h
   Utility made by Florian Klaempfl 25th-28th september 96
   Improvements made by Mark A. Malakanov 22nd-25th may 97 
   Further improvements by Michael Van Canneyt, April 1998 
   define handling and error recovery by Pierre Muller, June 1998 }


  interface

   uses
      base,defines,struct;

{$endif windows_include_files}

{$ifdef read_interface}

  { C default packing is dword }

{$PACKRECORDS 4}
  { 
     ASCIIFunctions.h
  
     Declarations for all the Win32 ASCII Functions
  
     Copyright (C) 1996 Free Software Foundation, Inc.
  
     Author:  Scott Christley <scottc@net-community.com>
  
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
{$ifndef _GNU_H_WINDOWS32_ASCIIFUNCFIONSDEFAULT}
{$define _GNU_H_WINDOWS32_ASCIIFUNCFIONSDEFAULT}
{ C++ extern C conditionnal removed }
  { __cplusplus  }

  function GetBinaryType(lpApplicationName:LPCSTR; lpBinaryType:LPDWORD):WINBOOL;

  function GetShortPathName(lpszLongPath:LPCSTR; lpszShortPath:LPSTR; cchBuffer:DWORD):DWORD;

  function GetEnvironmentStrings : LPSTR;

  function FreeEnvironmentStrings(_para1:LPSTR):WINBOOL;

  function FormatMessage(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPSTR; 
             nSize:DWORD; var Arguments:va_list):DWORD;

  function CreateMailslot(lpName:LPCSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function lstrcmp(lpString1:LPCSTR; lpString2:LPCSTR):longint;

  function lstrcmpi(lpString1:LPCSTR; lpString2:LPCSTR):longint;

  function lstrcpyn(lpString1:LPSTR; lpString2:LPCSTR; iMaxLength:longint):LPSTR;

  function lstrcpy(lpString1:LPSTR; lpString2:LPCSTR):LPSTR;

  function lstrcat(lpString1:LPSTR; lpString2:LPCSTR):LPSTR;

  function lstrlen(lpString:LPCSTR):longint;

  function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCSTR):HANDLE;

  function OpenMutex(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCSTR):HANDLE;

  function OpenEvent(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCSTR):HANDLE;

  function OpenSemaphore(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function CreateFileMapping(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD; 
             lpName:LPCSTR):HANDLE;

  function OpenFileMapping(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function GetLogicalDriveStrings(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD;

  function LoadLibrary(lpLibFileName:LPCSTR):HINSTANCE;

  function LoadLibraryEx(lpLibFileName:LPCSTR; hFile:HANDLE; dwFlags:DWORD):HINSTANCE;

  function GetModuleFileName(hModule:HINSTANCE; lpFilename:LPSTR; nSize:DWORD):DWORD;

  function GetModuleHandle(lpModuleName:LPCSTR):HMODULE;

  procedure FatalAppExit(uAction:UINT; lpMessageText:LPCSTR);

  function GetCommandLine : LPSTR;

  function GetEnvironmentVariable(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD;

  function SetEnvironmentVariable(lpName:LPCSTR; lpValue:LPCSTR):WINBOOL;

  function ExpandEnvironmentStrings(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):DWORD;

  procedure OutputDebugString(lpOutputString:LPCSTR);

  function FindResource(hModule:HINSTANCE; lpName:LPCSTR; lpType:LPCSTR):HRSRC;

  function FindResourceEx(hModule:HINSTANCE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD):HRSRC;

  function EnumResourceTypes(hModule:HINSTANCE; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL;

  function EnumResourceNames(hModule:HINSTANCE; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL;

  function EnumResourceLanguages(hModule:HINSTANCE; lpType:LPCSTR; lpName:LPCSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL;

  function BeginUpdateResource(pFileName:LPCSTR; bDeleteExistingResources:WINBOOL):HANDLE;

  function UpdateResource(hUpdate:HANDLE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD; lpData:LPVOID; 
             cbData:DWORD):WINBOOL;

  function EndUpdateResource(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL;

  function GlobalAddAtom(lpString:LPCSTR):ATOM;

  function GlobalFindAtom(lpString:LPCSTR):ATOM;

  function GlobalGetAtomName(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT;

  function AddAtom(lpString:LPCSTR):ATOM;

  function FindAtom(lpString:LPCSTR):ATOM;

  function GetAtomName(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT;

  function GetProfileInt(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT):UINT;

  function GetProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD;

  function WriteProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR):WINBOOL;

  function GetProfileSection(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD;

  function WriteProfileSection(lpAppName:LPCSTR; lpString:LPCSTR):WINBOOL;

  function GetPrivateProfileInt(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT; lpFileName:LPCSTR):UINT;

  function GetPrivateProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; 
             lpFileName:LPCSTR):DWORD;

  function WritePrivateProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL;

  function GetPrivateProfileSection(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; lpFileName:LPCSTR):DWORD;

  function WritePrivateProfileSection(lpAppName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL;

  function GetDriveType(lpRootPathName:LPCSTR):UINT;

  function GetSystemDirectory(lpBuffer:LPSTR; uSize:UINT):UINT;

  function GetTempPath(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD;

  function GetTempFileName(lpPathName:LPCSTR; lpPrefixString:LPCSTR; uUnique:UINT; lpTempFileName:LPSTR):UINT;

  function GetWindowsDirectory(lpBuffer:LPSTR; uSize:UINT):UINT;

  function SetCurrentDirectory(lpPathName:LPCSTR):WINBOOL;

  function GetCurrentDirectory(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD;

  function GetDiskFreeSpace(lpRootPathName:LPCSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL;

  function CreateDirectory(lpPathName:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function CreateDirectoryEx(lpTemplateDirectory:LPCSTR; lpNewDirectory:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function RemoveDirectory(lpPathName:LPCSTR):WINBOOL;

  function GetFullPathName(lpFileName:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; var lpFilePart:LPSTR):DWORD;

  function DefineDosDevice(dwFlags:DWORD; lpDeviceName:LPCSTR; lpTargetPath:LPCSTR):WINBOOL;

  function QueryDosDevice(lpDeviceName:LPCSTR; lpTargetPath:LPSTR; ucchMax:DWORD):DWORD;

  function CreateFile(lpFileName:LPCSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD; 
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE;

  function SetFileAttributes(lpFileName:LPCSTR; dwFileAttributes:DWORD):WINBOOL;

  function GetFileAttributes(lpFileName:LPCSTR):DWORD;

  function GetCompressedFileSize(lpFileName:LPCSTR; lpFileSizeHigh:LPDWORD):DWORD;

  function DeleteFile(lpFileName:LPCSTR):WINBOOL;

  function SearchPath(lpPath:LPCSTR; lpFileName:LPCSTR; lpExtension:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; 
             var lpFilePart:LPSTR):DWORD;

  function CopyFile(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; bFailIfExists:WINBOOL):WINBOOL;

  function MoveFile(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR):WINBOOL;

  function MoveFileEx(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; dwFlags:DWORD):WINBOOL;

  function CreateNamedPipe(lpName:LPCSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD; 
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function GetNamedPipeHandleState(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD; 
             lpUserName:LPSTR; nMaxUserNameSize:DWORD):WINBOOL;

  function CallNamedPipe(lpNamedPipeName:LPCSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD; 
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL;

  function WaitNamedPipe(lpNamedPipeName:LPCSTR; nTimeOut:DWORD):WINBOOL;

  function SetVolumeLabel(lpRootPathName:LPCSTR; lpVolumeName:LPCSTR):WINBOOL;

  function GetVolumeInformation(lpRootPathName:LPCSTR; lpVolumeNameBuffer:LPSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD; 
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPSTR; nFileSystemNameSize:DWORD):WINBOOL;

  function ClearEventLog(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL;

  function BackupEventLog(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL;

  function OpenEventLog(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE;

  function RegisterEventSource(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE;

  function OpenBackupEventLog(lpUNCServerName:LPCSTR; lpFileName:LPCSTR):HANDLE;

  function ReadEventLog(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; 
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL;

  function ReportEvent(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID; 
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCSTR; lpRawData:LPVOID):WINBOOL;

  function AccessCheckAndAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR; 
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL; 
             pfGenerateOnClose:LPBOOL):WINBOOL;

  function ObjectOpenAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR; 
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL; 
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL;

  function ObjectPrivilegeAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET; 
             AccessGranted:WINBOOL):WINBOOL;

  function ObjectCloseAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL;

  function PrivilegedServiceAuditAlarm(SubsystemName:LPCSTR; ServiceName:LPCSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL;

  function SetFileSecurity(lpFileName:LPCSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function GetFileSecurity(lpFileName:LPCSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function FindFirstChangeNotification(lpPathName:LPCSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE;

  function IsBadStringPtr(lpsz:LPCSTR; ucchMax:UINT):WINBOOL;

  function LookupAccountSid(lpSystemName:LPCSTR; Sid:PSID; Name:LPSTR; cbName:LPDWORD; ReferencedDomainName:LPSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupAccountName(lpSystemName:LPCSTR; lpAccountName:LPCSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupPrivilegeValue(lpSystemName:LPCSTR; lpName:LPCSTR; lpLuid:PLUID):WINBOOL;

  function LookupPrivilegeName(lpSystemName:LPCSTR; lpLuid:PLUID; lpName:LPSTR; cbName:LPDWORD):WINBOOL;

  function LookupPrivilegeDisplayName(lpSystemName:LPCSTR; lpName:LPCSTR; lpDisplayName:LPSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL;

  function BuildCommDCB(lpDef:LPCSTR; lpDCB:LPDCB):WINBOOL;

  function BuildCommDCBAndTimeouts(lpDef:LPCSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL;

  function CommConfigDialog(lpszName:LPCSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL;

  function GetDefaultCommConfig(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL;

  function SetDefaultCommConfig(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL;

  function GetComputerName(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL;

  function SetComputerName(lpComputerName:LPCSTR):WINBOOL;

  function GetUserName(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL;

  function wvsprintf(_para1:LPSTR; _para2:LPCSTR; arglist:va_list):longint;

  (* function wsprintf(_para1:LPSTR; _para2:LPCSTR; ...):longint;
      not allowed in FPC *)
      
  function LoadKeyboardLayout(pwszKLID:LPCSTR; Flags:UINT):HKL;

  function GetKeyboardLayoutName(pwszKLID:LPSTR):WINBOOL;

  function CreateDesktop(lpszDesktop:LPSTR; lpszDevice:LPSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD; 
             lpsa:LPSECURITY_ATTRIBUTES):HDESK;

  function OpenDesktop(lpszDesktop:LPSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK;

  function EnumDesktops(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL;

  function CreateWindowStation(lpwinsta:LPSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA;

  function OpenWindowStation(lpszWinSta:LPSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA;

  function EnumWindowStations(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL;

  function GetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function SetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL;

  function RegisterWindowMessage(lpString:LPCSTR):UINT;

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

  function UnregisterClass(lpClassName:LPCSTR; hInstance:HINSTANCE):WINBOOL;

  function GetClassInfo(hInstance:HINSTANCE; lpClassName:LPCSTR; lpWndClass:LPWNDCLASS):WINBOOL;

(* Const before type ignored *)
  function RegisterClassEx(var _para1:WNDCLASSEX):ATOM;

  function GetClassInfoEx(_para1:HINSTANCE; _para2:LPCSTR; _para3:LPWNDCLASSEX):WINBOOL;

  function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint; 
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;

  function CreateDialogParam(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function CreateDialogIndirectParam(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function DialogBoxParam(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function DialogBoxIndirectParam(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function SetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPCSTR):WINBOOL;

  function GetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPSTR; nMaxCount:longint):UINT;

  function SendDlgItemMessage(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG;

  function DefDlgProc(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallMsgFilter(lpMsg:LPMSG; nCode:longint):WINBOOL;

  function RegisterClipboardFormat(lpszFormat:LPCSTR):UINT;

  function GetClipboardFormatName(format:UINT; lpszFormatName:LPSTR; cchMaxCount:longint):longint;

  function CharToOem(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL;

  function OemToChar(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL;

  function CharToOemBuff(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL;

  function OemToCharBuff(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL;

  function CharUpper(lpsz:LPSTR):LPSTR;

  function CharUpperBuff(lpsz:LPSTR; cchLength:DWORD):DWORD;

  function CharLower(lpsz:LPSTR):LPSTR;

  function CharLowerBuff(lpsz:LPSTR; cchLength:DWORD):DWORD;

  function CharNext(lpsz:LPCSTR):LPSTR;

  function CharPrev(lpszStart:LPCSTR; lpszCurrent:LPCSTR):LPSTR;

  function IsCharAlpha(ch:CHAR):WINBOOL;

  function IsCharAlphaNumeric(ch:CHAR):WINBOOL;

  function IsCharUpper(ch:CHAR):WINBOOL;

  function IsCharLower(ch:CHAR):WINBOOL;

  function GetKeyNameText(lParam:LONG; lpString:LPSTR; nSize:longint):longint;

  function VkKeyScan(ch:CHAR):SHORT;

  function VkKeyScanEx(ch:CHAR; dwhkl:HKL):SHORT;

  function MapVirtualKey(uCode:UINT; uMapType:UINT):UINT;

  function MapVirtualKeyEx(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT;

  function LoadAccelerators(hInstance:HINSTANCE; lpTableName:LPCSTR):HACCEL;

  function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL;

  function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint;

  function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint;

  function LoadMenu(hInstance:HINSTANCE; lpMenuName:LPCSTR):HMENU;

(* Const before type ignored *)
  function LoadMenuIndirect(var lpMenuTemplate:MENUTEMPLATE):HMENU;

  function ChangeMenu(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCSTR; cmdInsert:UINT; flags:UINT):WINBOOL;

  function GetMenuString(hMenu:HMENU; uIDItem:UINT; lpString:LPSTR; nMaxCount:longint; uFlag:UINT):longint;

  function InsertMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL;

  function AppendMenu(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL;

  function ModifyMenu(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL;

  function InsertMenuItem(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function GetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL;

  function SetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function DrawText(hDC:HDC; lpString:LPCSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint;

  function DrawTextEx(_para1:HDC; _para2:LPSTR; _para3:longint; _para4:LPRECT; _para5:UINT; 
             _para6:LPDRAWTEXTPARAMS):longint;

  function GrayString(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint; 
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL;

  function DrawState(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM; 
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL;

  function TabbedTextOut(hDC:HDC; X:longint; Y:longint; lpString:LPCSTR; nCount:longint; 
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG;

  function GetTabbedTextExtent(hDC:HDC; lpString:LPCSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD;

  function SetProp(hWnd:HWND; lpString:LPCSTR; hData:HANDLE):WINBOOL;

  function GetProp(hWnd:HWND; lpString:LPCSTR):HANDLE;

  function RemoveProp(hWnd:HWND; lpString:LPCSTR):HANDLE;

  function EnumPropsEx(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint;

  function EnumProps(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint;

  function SetWindowText(hWnd:HWND; lpString:LPCSTR):WINBOOL;

  function GetWindowText(hWnd:HWND; lpString:LPSTR; nMaxCount:longint):longint;

  function GetWindowTextLength(hWnd:HWND):longint;

  function MessageBox(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT):longint;

  function MessageBoxEx(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT; wLanguageId:WORD):longint;

  function MessageBoxIndirect(_para1:LPMSGBOXPARAMS):longint;

  function GetWindowLong(hWnd:HWND; nIndex:longint):LONG;

  function SetWindowLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG;

  function GetClassLong(hWnd:HWND; nIndex:longint):DWORD;

  function SetClassLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD;

  function FindWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR):HWND;

  function FindWindowEx(_para1:HWND; _para2:HWND; _para3:LPCSTR; _para4:LPCSTR):HWND;

  function GetClassName(hWnd:HWND; lpClassName:LPSTR; nMaxCount:longint):longint;

  function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINSTANCE; dwThreadId:DWORD):HHOOK;

  function LoadBitmap(hInstance:HINSTANCE; lpBitmapName:LPCSTR):HBITMAP;

  function LoadCursor(hInstance:HINSTANCE; lpCursorName:LPCSTR):HCURSOR;

  function LoadCursorFromFile(lpFileName:LPCSTR):HCURSOR;

  function LoadIcon(hInstance:HINSTANCE; lpIconName:LPCSTR):HICON;

  function LoadImage(_para1:HINSTANCE; _para2:LPCSTR; _para3:UINT; _para4:longint; _para5:longint; 
             _para6:UINT):HANDLE;

  function LoadString(hInstance:HINSTANCE; uID:UINT; lpBuffer:LPSTR; nBufferMax:longint):longint;

  function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL;

  function DlgDirList(hDlg:HWND; lpPathSpec:LPSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint;

  function DlgDirSelectEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDListBox:longint):WINBOOL;

  function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint;

  function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDComboBox:longint):WINBOOL;

  function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CreateMDIWindow(lpClassName:LPSTR; lpWindowName:LPSTR; dwStyle:DWORD; X:longint; Y:longint; 
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINSTANCE; lParam:LPARAM):HWND;

  function WinHelp(hWndMain:HWND; lpszHelp:LPCSTR; uCommand:UINT; dwData:DWORD):WINBOOL;

  function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG;

  function EnumDisplaySettings(lpszDeviceName:LPCSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL;

  function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL;

  function AddFontResource(_para1:LPCSTR):longint;

  function CopyMetaFile(_para1:HMETAFILE; _para2:LPCSTR):HMETAFILE;

(* Const before type ignored *)
  function CreateFontIndirect(var _para1:LOGFONT):HFONT;

(* Const before type ignored *)
  function CreateIC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC;

  function CreateMetaFile(_para1:LPCSTR):HDC;

  function CreateScalableFontResource(_para1:DWORD; _para2:LPCSTR; _para3:LPCSTR; _para4:LPCSTR):WINBOOL;

(* Const before type ignored *)
  function DeviceCapabilities(_para1:LPCSTR; _para2:LPCSTR; _para3:WORD; _para4:LPSTR; var _para5:DEVMODE):longint;

  function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint;

  function EnumFontFamilies(_para1:HDC; _para2:LPCSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint;

  function EnumFonts(_para1:HDC; _para2:LPCSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint;

  function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL;

  function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL;

  function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL;

(* Const before type ignored *)
  function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD; 
             _para6:LPVOID; var _para7:MAT2):DWORD;

  function GetMetaFile(_para1:LPCSTR):HMETAFILE;

  function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT;

  function GetTextExtentPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentPoint32(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentExPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPINT; 
             _para6:LPINT; _para7:LPSIZE):WINBOOL;

  function GetCharacterPlacement(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS; 
             _para6:DWORD):DWORD;

(* Const before type ignored *)
  function ResetDC(_para1:HDC; var _para2:DEVMODE):HDC;

  function RemoveFontResource(_para1:LPCSTR):WINBOOL;

  function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCSTR):HENHMETAFILE;

(* Const before type ignored *)
  function CreateEnhMetaFile(_para1:HDC; _para2:LPCSTR; var _para3:RECT; _para4:LPCSTR):HDC;

  function GetEnhMetaFile(_para1:LPCSTR):HENHMETAFILE;

  function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPSTR):UINT;

  function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL;

(* Const before type ignored *)
  function StartDoc(_para1:HDC; var _para2:DOCINFO):longint;

  function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint;

  function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT; 
             _para6:LPCSTR; _para7:UINT; var _para8:INT):WINBOOL;

(* Const before type ignored *)
  function PolyTextOut(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL;

  function GetTextFace(_para1:HDC; _para2:longint; _para3:LPSTR):longint;

  function GetKerningPairs(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD;

  function CreateColorSpace(_para1:LPLOGCOLORSPACE):HCOLORSPACE;

  function GetLogColorSpace(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL;

  function GetICMProfile(_para1:HDC; _para2:DWORD; _para3:LPSTR):WINBOOL;

  function SetICMProfile(_para1:HDC; _para2:LPSTR):WINBOOL;

  function UpdateICMRegKey(_para1:DWORD; _para2:DWORD; _para3:LPSTR; _para4:UINT):WINBOOL;

  function EnumICMProfiles(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint;

  function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint;

  function ImageList_LoadImage(hi:HINSTANCE; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF; 
             uType:UINT; uFlags:UINT):HIMAGELIST;

  function CreateStatusWindow(style:LONG; lpszText:LPCSTR; hwndParent:HWND; wID:UINT):HWND;

  procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCSTR; uFlags:UINT);

  function GetOpenFileName(_para1:LPOPENFILENAME):WINBOOL;

  function GetSaveFileName(_para1:LPOPENFILENAME):WINBOOL;

  function GetFileTitle(_para1:LPCSTR; _para2:LPSTR; _para3:WORD):integer;

  function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL;

  function FindText(_para1:LPFINDREPLACE):HWND;

  function ReplaceText(_para1:LPFINDREPLACE):HWND;

  function ChooseFont(_para1:LPCHOOSEFONT):WINBOOL;

  function PrintDlg(_para1:LPPRINTDLG):WINBOOL;

  function PageSetupDlg(_para1:LPPAGESETUPDLG):WINBOOL;

  function CreateProcess(lpApplicationName:LPCSTR; lpCommandLine:LPSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL; 
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL;

  procedure GetStartupInfo(lpStartupInfo:LPSTARTUPINFO);

  function FindFirstFile(lpFileName:LPCSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE;

  function FindNextFile(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL;

  function GetVersionEx(lpVersionInformation:LPOSVERSIONINFO):WINBOOL;

  { was #define dname(params) def_expr }
  function CreateWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;

  { was #define dname(params) def_expr }
  function CreateDialog(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogIndirect(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function DialogBox(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  { was #define dname(params) def_expr }
  function DialogBoxIndirect(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

(* Const before type ignored *)
  function CreateDC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC;

  function VerInstallFile(uFlags:DWORD; szSrcFileName:LPSTR; szDestFileName:LPSTR; szSrcDir:LPSTR; szDestDir:LPSTR; 
             szCurDir:LPSTR; szTmpFile:LPSTR; lpuTmpFileLen:PUINT):DWORD;

  function GetFileVersionInfoSize(lptstrFilename:LPSTR; lpdwHandle:LPDWORD):DWORD;

  function GetFileVersionInfo(lptstrFilename:LPSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL;

  function VerLanguageName(wLang:DWORD; szLang:LPSTR; nSize:DWORD):DWORD;

(* Const before type ignored *)
  function VerQueryValue(pBlock:LPVOID; lpSubBlock:LPSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL;

  function VerFindFile(uFlags:DWORD; szFileName:LPSTR; szWinDir:LPSTR; szAppDir:LPSTR; szCurDir:LPSTR; 
             lpuCurDirLen:PUINT; szDestDir:LPSTR; lpuDestDirLen:PUINT):DWORD;

  function RegConnectRegistry(lpMachineName:LPSTR; hKey:HKEY; phkResult:PHKEY):LONG;

  function RegCreateKey(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG;

  function RegCreateKeyEx(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD; 
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG;

  function RegDeleteKey(hKey:HKEY; lpSubKey:LPCSTR):LONG;

  function RegDeleteValue(hKey:HKEY; lpValueName:LPCSTR):LONG;

  function RegEnumKey(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; cbName:DWORD):LONG;

  function RegEnumKeyEx(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; lpcbName:LPDWORD; lpReserved:LPDWORD; 
             lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function RegEnumValue(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD; 
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG;

  function RegLoadKey(hKey:HKEY; lpSubKey:LPCSTR; lpFile:LPCSTR):LONG;

  function RegOpenKey(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG;

  function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG;

  function RegQueryInfoKey(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD; 
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD; 
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function RegQueryValue(hKey:HKEY; lpSubKey:LPCSTR; lpValue:LPSTR; lpcbValue:PLONG):LONG;

  function RegQueryMultipleValues(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPSTR; ldwTotsize:LPDWORD):LONG;

  function RegQueryValueEx(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE; 
             lpcbData:LPDWORD):LONG;

  function RegReplaceKey(hKey:HKEY; lpSubKey:LPCSTR; lpNewFile:LPCSTR; lpOldFile:LPCSTR):LONG;

  function RegRestoreKey(hKey:HKEY; lpFile:LPCSTR; dwFlags:DWORD):LONG;

  function RegSaveKey(hKey:HKEY; lpFile:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG;

  function RegSetValue(hKey:HKEY; lpSubKey:LPCSTR; dwType:DWORD; lpData:LPCSTR; cbData:DWORD):LONG;

(* Const before type ignored *)
  function RegSetValueEx(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE; 
             cbData:DWORD):LONG;

  function RegUnLoadKey(hKey:HKEY; lpSubKey:LPCSTR):LONG;

  function InitiateSystemShutdown(lpMachineName:LPSTR; lpMessage:LPSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL;

  function AbortSystemShutdown(lpMachineName:LPSTR):WINBOOL;

  function CompareString(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCSTR; cchCount1:longint; lpString2:LPCSTR; 
             cchCount2:longint):longint;

  function LCMapString(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; 
             cchDest:longint):longint;

  function GetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPSTR; cchData:longint):longint;

  function SetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPCSTR):WINBOOL;

(* Const before type ignored *)
  function GetTimeFormat(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCSTR; lpTimeStr:LPSTR; 
             cchTime:longint):longint;

(* Const before type ignored *)
  function GetDateFormat(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCSTR; lpDateStr:LPSTR; 
             cchDate:longint):longint;

(* Const before type ignored *)
  function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPSTR; 
             cchNumber:longint):longint;

(* Const before type ignored *)
  function GetCurrencyFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPSTR; 
             cchCurrency:longint):longint;

  function EnumCalendarInfo(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL;

  function EnumTimeFormats(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function EnumDateFormats(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function GetStringTypeEx(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function GetStringType(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function FoldString(dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; cchDest:longint):longint;

  function EnumSystemLocales(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function EnumSystemCodePages(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function PeekConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL;

  function ReadConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleInput(hConsoleInput:HANDLE; var lpBuffer:INPUT_RECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL;

  function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL;

(* Const before type ignored *)
  function WriteConsoleOutput(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL;

  function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL;

  function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:CHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL;

  function GetConsoleTitle(lpConsoleTitle:LPSTR; nSize:DWORD):DWORD;

  function SetConsoleTitle(lpConsoleTitle:LPCSTR):WINBOOL;

  function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL;

(* Const before type ignored *)
  function WriteConsole(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL;

  function WNetAddConnection(lpRemoteName:LPCSTR; lpPassword:LPCSTR; lpLocalName:LPCSTR):DWORD;

  function WNetAddConnection2(lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD;

  function WNetAddConnection3(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD;

  function WNetCancelConnection(lpName:LPCSTR; fForce:WINBOOL):DWORD;

  function WNetCancelConnection2(lpName:LPCSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD;

  function WNetGetConnection(lpLocalName:LPCSTR; lpRemoteName:LPSTR; lpnLength:LPDWORD):DWORD;

  function WNetUseConnection(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCSTR; lpPassword:LPCSTR; dwFlags:DWORD; 
             lpAccessName:LPSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD;

  function WNetSetConnection(lpName:LPCSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD;

  function WNetConnectionDialog1(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD;

  function WNetDisconnectDialog1(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD;

  function WNetOpenEnum(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD;

  function WNetEnumResource(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUniversalName(lpLocalPath:LPCSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUser(lpName:LPCSTR; lpUserName:LPSTR; lpnLength:LPDWORD):DWORD;

  function WNetGetProviderName(dwNetType:DWORD; lpProviderName:LPSTR; lpBufferSize:LPDWORD):DWORD;

  function WNetGetNetworkInformation(lpProvider:LPCSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD;

  function WNetGetLastError(lpError:LPDWORD; lpErrorBuf:LPSTR; nErrorBufSize:DWORD; lpNameBuf:LPSTR; nNameBufSize:DWORD):DWORD;

  function MultinetGetConnectionPerformance(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD;

  function ChangeServiceConfig(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; 
             lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR; 
             lpDisplayName:LPCSTR):WINBOOL;

  function CreateService(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPCSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD; 
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; 
             lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR):SC_HANDLE;

  function EnumDependentServices(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD; 
             lpServicesReturned:LPDWORD):WINBOOL;

  function EnumServicesStatus(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; 
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL;

  function GetServiceKeyName(hSCManager:SC_HANDLE; lpDisplayName:LPCSTR; lpServiceName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function GetServiceDisplayName(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function OpenSCManager(lpMachineName:LPCSTR; lpDatabaseName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function OpenService(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function QueryServiceConfig(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function QueryServiceLockStatus(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function RegisterServiceCtrlHandler(lpServiceName:LPCSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE;

  function StartServiceCtrlDispatcher(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL;

  function StartService(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCSTR):WINBOOL;

  { Extensions to OpenGL  }
  function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL;

  function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:FLOAT; 
             _para6:FLOAT; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL;

  { -------------------------------------  }
  { From shellapi.h in old Cygnus headers  }
  function DragQueryFile(_para1:HDROP; _para2:cardinal; var _para3:char; _para4:cardinal):cardinal;

  function ExtractAssociatedIcon(_para1:HINSTANCE; var _para2:char; var _para3:WORD):HICON;

(* Const before type ignored *)
  function ExtractIcon(_para1:HINSTANCE; var _para2:char; _para3:cardinal):HICON;

(* Const before type ignored *)
(* Const before type ignored *)
  function FindExecutable(var _para1:char; var _para2:char; var _para3:char):HINSTANCE;

(* Const before type ignored *)
(* Const before type ignored *)
  function ShellAbout(_para1:HWND; var _para2:char; var _para3:char; _para4:HICON):longint;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function ShellExecute(_para1:HWND; var _para2:char; var _para3:char; var _para4:char; var _para5:char; 
             _para6:longint):HINSTANCE;

  { end of stuff from shellapi.h in old Cygnus headers  }
  { --------------------------------------------------  }
  { From ddeml.h in old Cygnus headers  }
  function DdeCreateStringHandle(_para1:DWORD; var _para2:char; _para3:longint):HSZ;

  function DdeInitialize(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT;

  function DdeQueryString(_para1:DWORD; _para2:HSZ; var _para3:char; _para4:DWORD; _para5:longint):DWORD;

  { end of stuff from ddeml.h in old Cygnus headers  }
  { -----------------------------------------------  }
  function LogonUser(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:DWORD; _para5:DWORD; 
             var _para6:HANDLE):WINBOOL;

  function CreateProcessAsUser(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES; 
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCTSTR; var _para10:STARTUPINFO; 
             var _para11:PROCESS_INFORMATION):WINBOOL;

{ C++ end of extern C conditionnal removed }
  { __cplusplus  }
{$endif}
  { _GNU_H_WINDOWS32_ASCIIFUNCFIONSDEFAULT  }

{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

  function GetBinaryType(lpApplicationName:LPCSTR; lpBinaryType:LPDWORD):WINBOOL; external External_library name 'GetBinaryTypeA';

  function GetShortPathName(lpszLongPath:LPCSTR; lpszShortPath:LPSTR; cchBuffer:DWORD):DWORD; external External_library name 'GetShortPathNameA';

  function GetEnvironmentStrings : LPSTR; external External_library name 'GetEnvironmentStringsA';

  function FreeEnvironmentStrings(_para1:LPSTR):WINBOOL; external External_library name 'FreeEnvironmentStringsA';

  function FormatMessage(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPSTR; 
             nSize:DWORD; var Arguments:va_list):DWORD; external External_library name 'FormatMessageA';

  function CreateMailslot(lpName:LPCSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external External_library name 'CreateMailslotA';

  function lstrcmp(lpString1:LPCSTR; lpString2:LPCSTR):longint; external External_library name 'lstrcmpA';

  function lstrcmpi(lpString1:LPCSTR; lpString2:LPCSTR):longint; external External_library name 'lstrcmpiA';

  function lstrcpyn(lpString1:LPSTR; lpString2:LPCSTR; iMaxLength:longint):LPSTR; external External_library name 'lstrcpynA';

  function lstrcpy(lpString1:LPSTR; lpString2:LPCSTR):LPSTR; external External_library name 'lstrcpyA';

  function lstrcat(lpString1:LPSTR; lpString2:LPCSTR):LPSTR; external External_library name 'lstrcatA';

  function lstrlen(lpString:LPCSTR):longint; external External_library name 'lstrlenA';

  function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCSTR):HANDLE; external External_library name 'CreateMutexA';

  function OpenMutex(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external External_library name 'OpenMutexA';

  function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCSTR):HANDLE; external External_library name 'CreateEventA';

  function OpenEvent(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external External_library name 'OpenEventA';

  function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCSTR):HANDLE; external External_library name 'CreateSemaphoreA';

  function OpenSemaphore(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external External_library name 'OpenSemaphoreA';

  function CreateFileMapping(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD; 
             lpName:LPCSTR):HANDLE; external External_library name 'CreateFileMappingA';

  function OpenFileMapping(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external External_library name 'OpenFileMappingA';

  function GetLogicalDriveStrings(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external External_library name 'GetLogicalDriveStringsA';

  function LoadLibrary(lpLibFileName:LPCSTR):HINSTANCE; external External_library name 'LoadLibraryA';

  function LoadLibraryEx(lpLibFileName:LPCSTR; hFile:HANDLE; dwFlags:DWORD):HINSTANCE; external External_library name 'LoadLibraryExA';

  function GetModuleFileName(hModule:HINSTANCE; lpFilename:LPSTR; nSize:DWORD):DWORD; external External_library name 'GetModuleFileNameA';

  function GetModuleHandle(lpModuleName:LPCSTR):HMODULE; external External_library name 'GetModuleHandleA';

  procedure FatalAppExit(uAction:UINT; lpMessageText:LPCSTR); external External_library name 'FatalAppExitA';

  function GetCommandLine : LPSTR; external External_library name 'GetCommandLineA';

  function GetEnvironmentVariable(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD; external External_library name 'GetEnvironmentVariableA';

  function SetEnvironmentVariable(lpName:LPCSTR; lpValue:LPCSTR):WINBOOL; external External_library name 'SetEnvironmentVariableA';

  function ExpandEnvironmentStrings(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):DWORD; external External_library name 'ExpandEnvironmentStringsA';

  procedure OutputDebugString(lpOutputString:LPCSTR); external External_library name 'OutputDebugStringA';

  function FindResource(hModule:HINSTANCE; lpName:LPCSTR; lpType:LPCSTR):HRSRC; external External_library name 'FindResourceA';

  function FindResourceEx(hModule:HINSTANCE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD):HRSRC; external External_library name 'FindResourceExA';

  function EnumResourceTypes(hModule:HINSTANCE; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL; external External_library name 'EnumResourceTypesA';

  function EnumResourceNames(hModule:HINSTANCE; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL; external External_library name 'EnumResourceNamesA';

  function EnumResourceLanguages(hModule:HINSTANCE; lpType:LPCSTR; lpName:LPCSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL; external External_library name 'EnumResourceLanguagesA';

  function BeginUpdateResource(pFileName:LPCSTR; bDeleteExistingResources:WINBOOL):HANDLE; external External_library name 'BeginUpdateResourceA';

  function UpdateResource(hUpdate:HANDLE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD; lpData:LPVOID; 
             cbData:DWORD):WINBOOL; external External_library name 'UpdateResourceA';

  function EndUpdateResource(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL; external External_library name 'EndUpdateResourceA';

  function GlobalAddAtom(lpString:LPCSTR):ATOM; external External_library name 'GlobalAddAtomA';

  function GlobalFindAtom(lpString:LPCSTR):ATOM; external External_library name 'GlobalFindAtomA';

  function GlobalGetAtomName(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT; external External_library name 'GlobalGetAtomNameA';

  function AddAtom(lpString:LPCSTR):ATOM; external External_library name 'AddAtomA';

  function FindAtom(lpString:LPCSTR):ATOM; external External_library name 'FindAtomA';

  function GetAtomName(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT; external External_library name 'GetAtomNameA';

  function GetProfileInt(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT):UINT; external External_library name 'GetProfileIntA';

  function GetProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD; external External_library name 'GetProfileStringA';

  function WriteProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR):WINBOOL; external External_library name 'WriteProfileStringA';

  function GetProfileSection(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD; external External_library name 'GetProfileSectionA';

  function WriteProfileSection(lpAppName:LPCSTR; lpString:LPCSTR):WINBOOL; external External_library name 'WriteProfileSectionA';

  function GetPrivateProfileInt(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT; lpFileName:LPCSTR):UINT; external External_library name 'GetPrivateProfileIntA';

  function GetPrivateProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; 
             lpFileName:LPCSTR):DWORD; external External_library name 'GetPrivateProfileStringA';

  function WritePrivateProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL; external External_library name 'WritePrivateProfileStringA';

  function GetPrivateProfileSection(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; lpFileName:LPCSTR):DWORD; external External_library name 'GetPrivateProfileSectionA';

  function WritePrivateProfileSection(lpAppName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL; external External_library name 'WritePrivateProfileSectionA';

  function GetDriveType(lpRootPathName:LPCSTR):UINT; external External_library name 'GetDriveTypeA';

  function GetSystemDirectory(lpBuffer:LPSTR; uSize:UINT):UINT; external External_library name 'GetSystemDirectoryA';

  function GetTempPath(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external External_library name 'GetTempPathA';

  function GetTempFileName(lpPathName:LPCSTR; lpPrefixString:LPCSTR; uUnique:UINT; lpTempFileName:LPSTR):UINT; external External_library name 'GetTempFileNameA';

  function GetWindowsDirectory(lpBuffer:LPSTR; uSize:UINT):UINT; external External_library name 'GetWindowsDirectoryA';

  function SetCurrentDirectory(lpPathName:LPCSTR):WINBOOL; external External_library name 'SetCurrentDirectoryA';

  function GetCurrentDirectory(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external External_library name 'GetCurrentDirectoryA';

  function GetDiskFreeSpace(lpRootPathName:LPCSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL; external External_library name 'GetDiskFreeSpaceA';

  function CreateDirectory(lpPathName:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external External_library name 'CreateDirectoryA';

  function CreateDirectoryEx(lpTemplateDirectory:LPCSTR; lpNewDirectory:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external External_library name 'CreateDirectoryExA';

  function RemoveDirectory(lpPathName:LPCSTR):WINBOOL; external External_library name 'RemoveDirectoryA';

  function GetFullPathName(lpFileName:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; var lpFilePart:LPSTR):DWORD; external External_library name 'GetFullPathNameA';

  function DefineDosDevice(dwFlags:DWORD; lpDeviceName:LPCSTR; lpTargetPath:LPCSTR):WINBOOL; external External_library name 'DefineDosDeviceA';

  function QueryDosDevice(lpDeviceName:LPCSTR; lpTargetPath:LPSTR; ucchMax:DWORD):DWORD; external External_library name 'QueryDosDeviceA';

  function CreateFile(lpFileName:LPCSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD; 
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external External_library name 'CreateFileA';

  function SetFileAttributes(lpFileName:LPCSTR; dwFileAttributes:DWORD):WINBOOL; external External_library name 'SetFileAttributesA';

  function GetFileAttributes(lpFileName:LPCSTR):DWORD; external External_library name 'GetFileAttributesA';

  function GetCompressedFileSize(lpFileName:LPCSTR; lpFileSizeHigh:LPDWORD):DWORD; external External_library name 'GetCompressedFileSizeA';

  function DeleteFile(lpFileName:LPCSTR):WINBOOL; external External_library name 'DeleteFileA';

  function SearchPath(lpPath:LPCSTR; lpFileName:LPCSTR; lpExtension:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; 
             var lpFilePart:LPSTR):DWORD; external External_library name 'SearchPathA';

  function CopyFile(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; bFailIfExists:WINBOOL):WINBOOL; external External_library name 'CopyFileA';

  function MoveFile(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR):WINBOOL; external External_library name 'MoveFileA';

  function MoveFileEx(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; dwFlags:DWORD):WINBOOL; external External_library name 'MoveFileExA';

  function CreateNamedPipe(lpName:LPCSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD; 
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external External_library name 'CreateNamedPipeA';

  function GetNamedPipeHandleState(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD; 
             lpUserName:LPSTR; nMaxUserNameSize:DWORD):WINBOOL; external External_library name 'GetNamedPipeHandleStateA';

  function CallNamedPipe(lpNamedPipeName:LPCSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD; 
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL; external External_library name 'CallNamedPipeA';

  function WaitNamedPipe(lpNamedPipeName:LPCSTR; nTimeOut:DWORD):WINBOOL; external External_library name 'WaitNamedPipeA';

  function SetVolumeLabel(lpRootPathName:LPCSTR; lpVolumeName:LPCSTR):WINBOOL; external External_library name 'SetVolumeLabelA';

  function GetVolumeInformation(lpRootPathName:LPCSTR; lpVolumeNameBuffer:LPSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD; 
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPSTR; nFileSystemNameSize:DWORD):WINBOOL; external External_library name 'GetVolumeInformationA';

  function ClearEventLog(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL; external External_library name 'ClearEventLogA';

  function BackupEventLog(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL; external External_library name 'BackupEventLogA';

  function OpenEventLog(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE; external External_library name 'OpenEventLogA';

  function RegisterEventSource(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE; external External_library name 'RegisterEventSourceA';

  function OpenBackupEventLog(lpUNCServerName:LPCSTR; lpFileName:LPCSTR):HANDLE; external External_library name 'OpenBackupEventLogA';

  function ReadEventLog(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; 
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL; external External_library name 'ReadEventLogA';

  function ReportEvent(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID; 
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCSTR; lpRawData:LPVOID):WINBOOL; external External_library name 'ReportEventA';

  function AccessCheckAndAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR; 
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL; 
             pfGenerateOnClose:LPBOOL):WINBOOL; external External_library name 'AccessCheckAndAuditAlarmA';

  function ObjectOpenAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR; 
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL; 
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL; external External_library name 'ObjectOpenAuditAlarmA';

  function ObjectPrivilegeAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET; 
             AccessGranted:WINBOOL):WINBOOL; external External_library name 'ObjectPrivilegeAuditAlarmA';

  function ObjectCloseAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL; external External_library name 'ObjectCloseAuditAlarmA';

  function PrivilegedServiceAuditAlarm(SubsystemName:LPCSTR; ServiceName:LPCSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL; external External_library name 'PrivilegedServiceAuditAlarmA';

  function SetFileSecurity(lpFileName:LPCSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external External_library name 'SetFileSecurityA';

  function GetFileSecurity(lpFileName:LPCSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external External_library name 'GetFileSecurityA';

  function FindFirstChangeNotification(lpPathName:LPCSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE; external External_library name 'FindFirstChangeNotificationA';

  function IsBadStringPtr(lpsz:LPCSTR; ucchMax:UINT):WINBOOL; external External_library name 'IsBadStringPtrA';

  function LookupAccountSid(lpSystemName:LPCSTR; Sid:PSID; Name:LPSTR; cbName:LPDWORD; ReferencedDomainName:LPSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external External_library name 'LookupAccountSidA';

  function LookupAccountName(lpSystemName:LPCSTR; lpAccountName:LPCSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPSTR; 
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external External_library name 'LookupAccountNameA';

  function LookupPrivilegeValue(lpSystemName:LPCSTR; lpName:LPCSTR; lpLuid:PLUID):WINBOOL; external External_library name 'LookupPrivilegeValueA';

  function LookupPrivilegeName(lpSystemName:LPCSTR; lpLuid:PLUID; lpName:LPSTR; cbName:LPDWORD):WINBOOL; external External_library name 'LookupPrivilegeNameA';

  function LookupPrivilegeDisplayName(lpSystemName:LPCSTR; lpName:LPCSTR; lpDisplayName:LPSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL; external External_library name 'LookupPrivilegeDisplayNameA';

  function BuildCommDCB(lpDef:LPCSTR; lpDCB:LPDCB):WINBOOL; external External_library name 'BuildCommDCBA';

  function BuildCommDCBAndTimeouts(lpDef:LPCSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external External_library name 'BuildCommDCBAndTimeoutsA';

  function CommConfigDialog(lpszName:LPCSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL; external External_library name 'CommConfigDialogA';

  function GetDefaultCommConfig(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external External_library name 'GetDefaultCommConfigA';

  function SetDefaultCommConfig(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external External_library name 'SetDefaultCommConfigA';

  function GetComputerName(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL; external External_library name 'GetComputerNameA';

  function SetComputerName(lpComputerName:LPCSTR):WINBOOL; external External_library name 'SetComputerNameA';

  function GetUserName(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL; external External_library name 'GetUserNameA';

  function wvsprintf(_para1:LPSTR; _para2:LPCSTR; arglist:va_list):longint; external External_library name 'wvsprintfA';

(*  function wsprintf(_para1:LPSTR; _para2:LPCSTR; ...):longint;CDECL; external External_library name 'wsprintfA';
  not implemented *)
  
  function LoadKeyboardLayout(pwszKLID:LPCSTR; Flags:UINT):HKL; external External_library name 'LoadKeyboardLayoutA';

  function GetKeyboardLayoutName(pwszKLID:LPSTR):WINBOOL; external External_library name 'GetKeyboardLayoutNameA';

  function CreateDesktop(lpszDesktop:LPSTR; lpszDevice:LPSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD; 
             lpsa:LPSECURITY_ATTRIBUTES):HDESK; external External_library name 'CreateDesktopA';

  function OpenDesktop(lpszDesktop:LPSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external External_library name 'OpenDesktopA';

  function EnumDesktops(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL; external External_library name 'EnumDesktopsA';

  function CreateWindowStation(lpwinsta:LPSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA; external External_library name 'CreateWindowStationA';

  function OpenWindowStation(lpszWinSta:LPSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA; external External_library name 'OpenWindowStationA';

  function EnumWindowStations(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL; external External_library name 'EnumWindowStationsA';

  function GetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external External_library name 'GetUserObjectInformationA';

  function SetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL; external External_library name 'SetUserObjectInformationA';

  function RegisterWindowMessage(lpString:LPCSTR):UINT; external External_library name 'RegisterWindowMessageA';

  function GetMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL; external External_library name 'GetMessageA';

  function DispatchMessage(var lpMsg:MSG):LONG; external External_library name 'DispatchMessageA';

  function PeekMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external External_library name 'PeekMessageA';

  function SendMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'SendMessageA';

  function SendMessageTimeout(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT; 
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external External_library name 'SendMessageTimeoutA';

  function SendNotifyMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external External_library name 'SendNotifyMessageA';

  function SendMessageCallback(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC; 
             dwData:DWORD):WINBOOL; external External_library name 'SendMessageCallbackA';

  function PostMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external External_library name 'PostMessageA';

  function PostThreadMessage(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external External_library name 'PostThreadMessageA';

  function DefWindowProc(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefWindowProcA';

  function CallWindowProc(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'CallWindowProcA';

  function RegisterClass(var lpWndClass:WNDCLASS):ATOM; external External_library name 'RegisterClassA';

  function UnregisterClass(lpClassName:LPCSTR; hInstance:HINSTANCE):WINBOOL; external External_library name 'UnregisterClassA';

  function GetClassInfo(hInstance:HINSTANCE; lpClassName:LPCSTR; lpWndClass:LPWNDCLASS):WINBOOL; external External_library name 'GetClassInfoA';

  function RegisterClassEx(var _para1:WNDCLASSEX):ATOM; external External_library name 'RegisterClassExA';

  function GetClassInfoEx(_para1:HINSTANCE; _para2:LPCSTR; _para3:LPWNDCLASSEX):WINBOOL; external External_library name 'GetClassInfoExA';

  function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint; 
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND; external External_library name 'CreateWindowExA';

  function CreateDialogParam(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external External_library name 'CreateDialogParamA';

  function CreateDialogIndirectParam(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external External_library name 'CreateDialogIndirectParamA';

  function DialogBoxParam(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external External_library name 'DialogBoxParamA';

  function DialogBoxIndirectParam(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external External_library name 'DialogBoxIndirectParamA';

  function SetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPCSTR):WINBOOL; external External_library name 'SetDlgItemTextA';

  function GetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPSTR; nMaxCount:longint):UINT; external External_library name 'GetDlgItemTextA';

  function SendDlgItemMessage(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG; external External_library name 'SendDlgItemMessageA';

  function DefDlgProc(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefDlgProcA';

  function CallMsgFilter(lpMsg:LPMSG; nCode:longint):WINBOOL; external External_library name 'CallMsgFilterA';

  function RegisterClipboardFormat(lpszFormat:LPCSTR):UINT; external External_library name 'RegisterClipboardFormatA';

  function GetClipboardFormatName(format:UINT; lpszFormatName:LPSTR; cchMaxCount:longint):longint; external External_library name 'GetClipboardFormatNameA';

  function CharToOem(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL; external External_library name 'CharToOemA';

  function OemToChar(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL; external External_library name 'OemToCharA';

  function CharToOemBuff(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external External_library name 'CharToOemBuffA';

  function OemToCharBuff(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external External_library name 'OemToCharBuffA';

  function CharUpper(lpsz:LPSTR):LPSTR; external External_library name 'CharUpperA';

  function CharUpperBuff(lpsz:LPSTR; cchLength:DWORD):DWORD; external External_library name 'CharUpperBuffA';

  function CharLower(lpsz:LPSTR):LPSTR; external External_library name 'CharLowerA';

  function CharLowerBuff(lpsz:LPSTR; cchLength:DWORD):DWORD; external External_library name 'CharLowerBuffA';

  function CharNext(lpsz:LPCSTR):LPSTR; external External_library name 'CharNextA';

  function CharPrev(lpszStart:LPCSTR; lpszCurrent:LPCSTR):LPSTR; external External_library name 'CharPrevA';

  function IsCharAlpha(ch:CHAR):WINBOOL; external External_library name 'IsCharAlphaA';

  function IsCharAlphaNumeric(ch:CHAR):WINBOOL; external External_library name 'IsCharAlphaNumericA';

  function IsCharUpper(ch:CHAR):WINBOOL; external External_library name 'IsCharUpperA';

  function IsCharLower(ch:CHAR):WINBOOL; external External_library name 'IsCharLowerA';

  function GetKeyNameText(lParam:LONG; lpString:LPSTR; nSize:longint):longint; external External_library name 'GetKeyNameTextA';

  function VkKeyScan(ch:CHAR):SHORT; external External_library name 'VkKeyScanA';

  function VkKeyScanEx(ch:CHAR; dwhkl:HKL):SHORT; external External_library name 'VkKeyScanExA';

  function MapVirtualKey(uCode:UINT; uMapType:UINT):UINT; external External_library name 'MapVirtualKeyA';

  function MapVirtualKeyEx(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT; external External_library name 'MapVirtualKeyExA';

  function LoadAccelerators(hInstance:HINSTANCE; lpTableName:LPCSTR):HACCEL; external External_library name 'LoadAcceleratorsA';

  function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL; external External_library name 'CreateAcceleratorTableA';

  function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint; external External_library name 'CopyAcceleratorTableA';

  function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint; external External_library name 'TranslateAcceleratorA';

  function LoadMenu(hInstance:HINSTANCE; lpMenuName:LPCSTR):HMENU; external External_library name 'LoadMenuA';

  function LoadMenuIndirect(var lpMenuTemplate:MENUTEMPLATE):HMENU; external External_library name 'LoadMenuIndirectA';

  function ChangeMenu(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCSTR; cmdInsert:UINT; flags:UINT):WINBOOL; external External_library name 'ChangeMenuA';

  function GetMenuString(hMenu:HMENU; uIDItem:UINT; lpString:LPSTR; nMaxCount:longint; uFlag:UINT):longint; external External_library name 'GetMenuStringA';

  function InsertMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external External_library name 'InsertMenuA';

  function AppendMenu(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external External_library name 'AppendMenuA';

  function ModifyMenu(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external External_library name 'ModifyMenuA';

  function InsertMenuItem(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external External_library name 'InsertMenuItemA';

  function GetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL; external External_library name 'GetMenuItemInfoA';

  function SetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external External_library name 'SetMenuItemInfoA';

  function DrawText(hDC:HDC; lpString:LPCSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint; external External_library name 'DrawTextA';

  function DrawTextEx(_para1:HDC; _para2:LPSTR; _para3:longint; _para4:LPRECT; _para5:UINT; 
             _para6:LPDRAWTEXTPARAMS):longint; external External_library name 'DrawTextExA';

  function GrayString(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint; 
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL; external External_library name 'GrayStringA';

  function DrawState(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM; 
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL; external External_library name 'DrawStateA';

  function TabbedTextOut(hDC:HDC; X:longint; Y:longint; lpString:LPCSTR; nCount:longint; 
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG; external External_library name 'TabbedTextOutA';

  function GetTabbedTextExtent(hDC:HDC; lpString:LPCSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD; external External_library name 'GetTabbedTextExtentA';

  function SetProp(hWnd:HWND; lpString:LPCSTR; hData:HANDLE):WINBOOL; external External_library name 'SetPropA';

  function GetProp(hWnd:HWND; lpString:LPCSTR):HANDLE; external External_library name 'GetPropA';

  function RemoveProp(hWnd:HWND; lpString:LPCSTR):HANDLE; external External_library name 'RemovePropA';

  function EnumPropsEx(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint; external External_library name 'EnumPropsExA';

  function EnumProps(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint; external External_library name 'EnumPropsA';

  function SetWindowText(hWnd:HWND; lpString:LPCSTR):WINBOOL; external External_library name 'SetWindowTextA';

  function GetWindowText(hWnd:HWND; lpString:LPSTR; nMaxCount:longint):longint; external External_library name 'GetWindowTextA';

  function GetWindowTextLength(hWnd:HWND):longint; external External_library name 'GetWindowTextLengthA';

  function MessageBox(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT):longint; external External_library name 'MessageBoxA';

  function MessageBoxEx(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT; wLanguageId:WORD):longint; external External_library name 'MessageBoxExA';

  function MessageBoxIndirect(_para1:LPMSGBOXPARAMS):longint; external External_library name 'MessageBoxIndirectA';

  function GetWindowLong(hWnd:HWND; nIndex:longint):LONG; external External_library name 'GetWindowLongA';

  function SetWindowLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG; external External_library name 'SetWindowLongA';

  function GetClassLong(hWnd:HWND; nIndex:longint):DWORD; external External_library name 'GetClassLongA';

  function SetClassLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD; external External_library name 'SetClassLongA';

  function FindWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR):HWND; external External_library name 'FindWindowA';

  function FindWindowEx(_para1:HWND; _para2:HWND; _para3:LPCSTR; _para4:LPCSTR):HWND; external External_library name 'FindWindowExA';

  function GetClassName(hWnd:HWND; lpClassName:LPSTR; nMaxCount:longint):longint; external External_library name 'GetClassNameA';

  function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINSTANCE; dwThreadId:DWORD):HHOOK; external External_library name 'SetWindowsHookExA';

  function LoadBitmap(hInstance:HINSTANCE; lpBitmapName:LPCSTR):HBITMAP; external External_library name 'LoadBitmapA';

  function LoadCursor(hInstance:HINSTANCE; lpCursorName:LPCSTR):HCURSOR; external External_library name 'LoadCursorA';

  function LoadCursorFromFile(lpFileName:LPCSTR):HCURSOR; external External_library name 'LoadCursorFromFileA';

  function LoadIcon(hInstance:HINSTANCE; lpIconName:LPCSTR):HICON; external External_library name 'LoadIconA';

  function LoadImage(_para1:HINSTANCE; _para2:LPCSTR; _para3:UINT; _para4:longint; _para5:longint; 
             _para6:UINT):HANDLE; external External_library name 'LoadImageA';

  function LoadString(hInstance:HINSTANCE; uID:UINT; lpBuffer:LPSTR; nBufferMax:longint):longint; external External_library name 'LoadStringA';

  function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL; external External_library name 'IsDialogMessageA';

  function DlgDirList(hDlg:HWND; lpPathSpec:LPSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint; external External_library name 'DlgDirListA';

  function DlgDirSelectEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDListBox:longint):WINBOOL; external External_library name 'DlgDirSelectExA';

  function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint; external External_library name 'DlgDirListComboBoxA';

  function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDComboBox:longint):WINBOOL; external External_library name 'DlgDirSelectComboBoxExA';

  function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefFrameProcA';

  function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external External_library name 'DefMDIChildProcA';

  function CreateMDIWindow(lpClassName:LPSTR; lpWindowName:LPSTR; dwStyle:DWORD; X:longint; Y:longint; 
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINSTANCE; lParam:LPARAM):HWND; external External_library name 'CreateMDIWindowA';

  function WinHelp(hWndMain:HWND; lpszHelp:LPCSTR; uCommand:UINT; dwData:DWORD):WINBOOL; external External_library name 'WinHelpA';

  function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG; external External_library name 'ChangeDisplaySettingsA';

  function EnumDisplaySettings(lpszDeviceName:LPCSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL; external External_library name 'EnumDisplaySettingsA';

  function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL; external External_library name 'SystemParametersInfoA';

  function AddFontResource(_para1:LPCSTR):longint; external External_library name 'AddFontResourceA';

  function CopyMetaFile(_para1:HMETAFILE; _para2:LPCSTR):HMETAFILE; external External_library name 'CopyMetaFileA';

  function CreateFontIndirect(var _para1:LOGFONT):HFONT; external External_library name 'CreateFontIndirectA';

  function CreateIC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC; external External_library name 'CreateICA';

  function CreateMetaFile(_para1:LPCSTR):HDC; external External_library name 'CreateMetaFileA';

  function CreateScalableFontResource(_para1:DWORD; _para2:LPCSTR; _para3:LPCSTR; _para4:LPCSTR):WINBOOL; external External_library name 'CreateScalableFontResourceA';

  function DeviceCapabilities(_para1:LPCSTR; _para2:LPCSTR; _para3:WORD; _para4:LPSTR; var _para5:DEVMODE):longint; external External_library name 'DeviceCapabilitiesA';

  function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint; external External_library name 'EnumFontFamiliesExA';

  function EnumFontFamilies(_para1:HDC; _para2:LPCSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint; external External_library name 'EnumFontFamiliesA';

  function EnumFonts(_para1:HDC; _para2:LPCSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint; external External_library name 'EnumFontsA';

  function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external External_library name 'GetCharWidthA';

  function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external External_library name 'GetCharWidth32A';

  function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL; external External_library name 'GetCharWidthFloatA';

  function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external External_library name 'GetCharABCWidthsA';

  function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL; external External_library name 'GetCharABCWidthsFloatA';

  function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD; 
             _para6:LPVOID; var _para7:MAT2):DWORD; external External_library name 'GetGlyphOutlineA';

  function GetMetaFile(_para1:LPCSTR):HMETAFILE; external External_library name 'GetMetaFileA';

  function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT; external External_library name 'GetOutlineTextMetricsA';

  function GetTextExtentPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external External_library name 'GetTextExtentPointA';

  function GetTextExtentPoint32(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external External_library name 'GetTextExtentPoint32A';

  function GetTextExtentExPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPINT; 
             _para6:LPINT; _para7:LPSIZE):WINBOOL; external External_library name 'GetTextExtentExPointA';

  function GetCharacterPlacement(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS; 
             _para6:DWORD):DWORD; external External_library name 'GetCharacterPlacementA';

  function ResetDC(_para1:HDC; var _para2:DEVMODE):HDC; external External_library name 'ResetDCA';

  function RemoveFontResource(_para1:LPCSTR):WINBOOL; external External_library name 'RemoveFontResourceA';

  function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCSTR):HENHMETAFILE; external External_library name 'CopyEnhMetaFileA';

  function CreateEnhMetaFile(_para1:HDC; _para2:LPCSTR; var _para3:RECT; _para4:LPCSTR):HDC; external External_library name 'CreateEnhMetaFileA';

  function GetEnhMetaFile(_para1:LPCSTR):HENHMETAFILE; external External_library name 'GetEnhMetaFileA';

  function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPSTR):UINT; external External_library name 'GetEnhMetaFileDescriptionA';

  function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL; external External_library name 'GetTextMetricsA';

  function StartDoc(_para1:HDC; var _para2:DOCINFO):longint; external External_library name 'StartDocA';

  function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint; external External_library name 'GetObjectA';

  function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint):WINBOOL; external External_library name 'TextOutA';

  function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT; 
             _para6:LPCSTR; _para7:UINT; var _para8:INT):WINBOOL; external External_library name 'ExtTextOutA';

  function PolyTextOut(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL; external External_library name 'PolyTextOutA';

  function GetTextFace(_para1:HDC; _para2:longint; _para3:LPSTR):longint; external External_library name 'GetTextFaceA';

  function GetKerningPairs(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD; external External_library name 'GetKerningPairsA';

  function CreateColorSpace(_para1:LPLOGCOLORSPACE):HCOLORSPACE; external External_library name 'CreateColorSpaceA';

  function GetLogColorSpace(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL; external External_library name 'GetLogColorSpaceA';

  function GetICMProfile(_para1:HDC; _para2:DWORD; _para3:LPSTR):WINBOOL; external External_library name 'GetICMProfileA';

  function SetICMProfile(_para1:HDC; _para2:LPSTR):WINBOOL; external External_library name 'SetICMProfileA';

  function UpdateICMRegKey(_para1:DWORD; _para2:DWORD; _para3:LPSTR; _para4:UINT):WINBOOL; external External_library name 'UpdateICMRegKeyA';

  function EnumICMProfiles(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint; external External_library name 'EnumICMProfilesA';

  function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint; external External_library name 'PropertySheetA';

  function ImageList_LoadImage(hi:HINSTANCE; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF; 
             uType:UINT; uFlags:UINT):HIMAGELIST; external External_library name 'ImageList_LoadImageA';

  function CreateStatusWindow(style:LONG; lpszText:LPCSTR; hwndParent:HWND; wID:UINT):HWND; external External_library name 'CreateStatusWindowA';

  procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCSTR; uFlags:UINT); external External_library name 'DrawStatusTextA';

  function GetOpenFileName(_para1:LPOPENFILENAME):WINBOOL; external External_library name 'GetOpenFileNameA';

  function GetSaveFileName(_para1:LPOPENFILENAME):WINBOOL; external External_library name 'GetSaveFileNameA';

  function GetFileTitle(_para1:LPCSTR; _para2:LPSTR; _para3:WORD):integer; external External_library name 'GetFileTitleA';

  function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL; external External_library name 'ChooseColorA';

  function FindText(_para1:LPFINDREPLACE):HWND; external External_library name 'FindTextA';

  function ReplaceText(_para1:LPFINDREPLACE):HWND; external External_library name 'ReplaceTextA';

  function ChooseFont(_para1:LPCHOOSEFONT):WINBOOL; external External_library name 'ChooseFontA';

  function PrintDlg(_para1:LPPRINTDLG):WINBOOL; external External_library name 'PrintDlgA';

  function PageSetupDlg(_para1:LPPAGESETUPDLG):WINBOOL; external External_library name 'PageSetupDlgA';

  function CreateProcess(lpApplicationName:LPCSTR; lpCommandLine:LPSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL; 
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL; external External_library name 'CreateProcessA';

  procedure GetStartupInfo(lpStartupInfo:LPSTARTUPINFO); external External_library name 'GetStartupInfoA';

  function FindFirstFile(lpFileName:LPCSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE; external External_library name 'FindFirstFileA';

  function FindNextFile(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL; external External_library name 'FindNextFileA';

  function GetVersionEx(lpVersionInformation:LPOSVERSIONINFO):WINBOOL; external External_library name 'GetVersionExA';

  { was #define dname(params) def_expr }
  function CreateWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU; 
             hInstance:HINSTANCE; lpParam:LPVOID):HWND;
    begin
       CreateWindow:=CreateWindowExA(0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam);
    end;

  { was #define dname(params) def_expr }
  function CreateDialog(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialog:=CreateDialogParamA(hInstance,lpTemplateName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogIndirect(hInstance:HINSTANCE; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogIndirect:=CreateDialogIndirectParamA(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBox(hInstance:HINSTANCE; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBox:=DialogBoxParamA(hInstance,lpTemplateName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxIndirect(hInstance:HINSTANCE; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxIndirect:=DialogBoxIndirectParamA(hInstance,hDialogTemplate,hWndParent,lpDialogFunc,0);
    end;

  function CreateDC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC; external External_library name 'CreateDCA';

  function VerInstallFile(uFlags:DWORD; szSrcFileName:LPSTR; szDestFileName:LPSTR; szSrcDir:LPSTR; szDestDir:LPSTR; 
             szCurDir:LPSTR; szTmpFile:LPSTR; lpuTmpFileLen:PUINT):DWORD; external External_library name 'VerInstallFileA';

  function GetFileVersionInfoSize(lptstrFilename:LPSTR; lpdwHandle:LPDWORD):DWORD; external External_library name 'GetFileVersionInfoSizeA';

  function GetFileVersionInfo(lptstrFilename:LPSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external External_library name 'GetFileVersionInfoA';

  function VerLanguageName(wLang:DWORD; szLang:LPSTR; nSize:DWORD):DWORD; external External_library name 'VerLanguageNameA';

  function VerQueryValue(pBlock:LPVOID; lpSubBlock:LPSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external External_library name 'VerQueryValueA';

  function VerFindFile(uFlags:DWORD; szFileName:LPSTR; szWinDir:LPSTR; szAppDir:LPSTR; szCurDir:LPSTR; 
             lpuCurDirLen:PUINT; szDestDir:LPSTR; lpuDestDirLen:PUINT):DWORD; external External_library name 'VerFindFileA';

  function RegConnectRegistry(lpMachineName:LPSTR; hKey:HKEY; phkResult:PHKEY):LONG; external External_library name 'RegConnectRegistryA';

  function RegCreateKey(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG; external External_library name 'RegCreateKeyA';

  function RegCreateKeyEx(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD; 
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG; external External_library name 'RegCreateKeyExA';

  function RegDeleteKey(hKey:HKEY; lpSubKey:LPCSTR):LONG; external External_library name 'RegDeleteKeyA';

  function RegDeleteValue(hKey:HKEY; lpValueName:LPCSTR):LONG; external External_library name 'RegDeleteValueA';

  function RegEnumKey(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; cbName:DWORD):LONG; external External_library name 'RegEnumKeyA';

  function RegEnumKeyEx(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; lpcbName:LPDWORD; lpReserved:LPDWORD; 
             lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external External_library name 'RegEnumKeyExA';

  function RegEnumValue(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD; 
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG; external External_library name 'RegEnumValueA';

  function RegLoadKey(hKey:HKEY; lpSubKey:LPCSTR; lpFile:LPCSTR):LONG; external External_library name 'RegLoadKeyA';

  function RegOpenKey(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG; external External_library name 'RegOpenKeyA';

  function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; phkResult:PHKEY):LONG; external External_library name 'RegOpenKeyExA';

  function RegQueryInfoKey(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD; 
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD; 
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external External_library name 'RegQueryInfoKeyA';

  function RegQueryValue(hKey:HKEY; lpSubKey:LPCSTR; lpValue:LPSTR; lpcbValue:PLONG):LONG; external External_library name 'RegQueryValueA';

  function RegQueryMultipleValues(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPSTR; ldwTotsize:LPDWORD):LONG; external External_library name 'RegQueryMultipleValuesA';

  function RegQueryValueEx(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE; 
             lpcbData:LPDWORD):LONG; external External_library name 'RegQueryValueExA';

  function RegReplaceKey(hKey:HKEY; lpSubKey:LPCSTR; lpNewFile:LPCSTR; lpOldFile:LPCSTR):LONG; external External_library name 'RegReplaceKeyA';

  function RegRestoreKey(hKey:HKEY; lpFile:LPCSTR; dwFlags:DWORD):LONG; external External_library name 'RegRestoreKeyA';

  function RegSaveKey(hKey:HKEY; lpFile:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external External_library name 'RegSaveKeyA';

  function RegSetValue(hKey:HKEY; lpSubKey:LPCSTR; dwType:DWORD; lpData:LPCSTR; cbData:DWORD):LONG; external External_library name 'RegSetValueA';

  function RegSetValueEx(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE; 
             cbData:DWORD):LONG; external External_library name 'RegSetValueExA';

  function RegUnLoadKey(hKey:HKEY; lpSubKey:LPCSTR):LONG; external External_library name 'RegUnLoadKeyA';

  function InitiateSystemShutdown(lpMachineName:LPSTR; lpMessage:LPSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL; external External_library name 'InitiateSystemShutdownA';

  function AbortSystemShutdown(lpMachineName:LPSTR):WINBOOL; external External_library name 'AbortSystemShutdownA';

  function CompareString(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCSTR; cchCount1:longint; lpString2:LPCSTR; 
             cchCount2:longint):longint; external External_library name 'CompareStringA';

  function LCMapString(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; 
             cchDest:longint):longint; external External_library name 'LCMapStringA';

  function GetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPSTR; cchData:longint):longint; external External_library name 'GetLocaleInfoA';

  function SetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPCSTR):WINBOOL; external External_library name 'SetLocaleInfoA';

  function GetTimeFormat(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCSTR; lpTimeStr:LPSTR; 
             cchTime:longint):longint; external External_library name 'GetTimeFormatA';

  function GetDateFormat(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCSTR; lpDateStr:LPSTR; 
             cchDate:longint):longint; external External_library name 'GetDateFormatA';

  function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPSTR; 
             cchNumber:longint):longint; external External_library name 'GetNumberFormatA';

  function GetCurrencyFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPSTR; 
             cchCurrency:longint):longint; external External_library name 'GetCurrencyFormatA';

  function EnumCalendarInfo(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL; external External_library name 'EnumCalendarInfoA';

  function EnumTimeFormats(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external External_library name 'EnumTimeFormatsA';

  function EnumDateFormats(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external External_library name 'EnumDateFormatsA';

  function GetStringTypeEx(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external External_library name 'GetStringTypeExA';

  function GetStringType(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external External_library name 'GetStringTypeA';

  function FoldString(dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; cchDest:longint):longint; external External_library name 'FoldStringA';

  function EnumSystemLocales(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL; external External_library name 'EnumSystemLocalesA';

  function EnumSystemCodePages(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL; external External_library name 'EnumSystemCodePagesA';

  function PeekConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external External_library name 'PeekConsoleInputA';

  function ReadConsoleInput(hConsoleInput:HANDLE; lpBuffer:PINPUT_RECORD; nLength:DWORD; lpNumberOfEventsRead:LPDWORD):WINBOOL; external External_library name 'ReadConsoleInputA';

  function WriteConsoleInput(hConsoleInput:HANDLE; var lpBuffer:INPUT_RECORD; nLength:DWORD; lpNumberOfEventsWritten:LPDWORD):WINBOOL; external External_library name 'WriteConsoleInputA';

  function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL; external External_library name 'ReadConsoleOutputA';

  function WriteConsoleOutput(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL; external External_library name 'WriteConsoleOutputA';

  function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL; external External_library name 'ReadConsoleOutputCharacterA';

  function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external External_library name 'WriteConsoleOutputCharacterA';

  function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:CHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external External_library name 'FillConsoleOutputCharacterA';

  function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL; external External_library name 'ScrollConsoleScreenBufferA';

  function GetConsoleTitle(lpConsoleTitle:LPSTR; nSize:DWORD):DWORD; external External_library name 'GetConsoleTitleA';

  function SetConsoleTitle(lpConsoleTitle:LPCSTR):WINBOOL; external External_library name 'SetConsoleTitleA';

  function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL; external External_library name 'ReadConsoleA';

  function WriteConsole(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL; external External_library name 'WriteConsoleA';

  function WNetAddConnection(lpRemoteName:LPCSTR; lpPassword:LPCSTR; lpLocalName:LPCSTR):DWORD; external External_library name 'WNetAddConnectionA';

  function WNetAddConnection2(lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD; external External_library name 'WNetAddConnection2A';

  function WNetAddConnection3(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD; external External_library name 'WNetAddConnection3A';

  function WNetCancelConnection(lpName:LPCSTR; fForce:WINBOOL):DWORD; external External_library name 'WNetCancelConnectionA';

  function WNetCancelConnection2(lpName:LPCSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD; external External_library name 'WNetCancelConnection2A';

  function WNetGetConnection(lpLocalName:LPCSTR; lpRemoteName:LPSTR; lpnLength:LPDWORD):DWORD; external External_library name 'WNetGetConnectionA';

  function WNetUseConnection(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCSTR; lpPassword:LPCSTR; dwFlags:DWORD; 
             lpAccessName:LPSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD; external External_library name 'WNetUseConnectionA';

  function WNetSetConnection(lpName:LPCSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD; external External_library name 'WNetSetConnectionA';

  function WNetConnectionDialog1(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD; external External_library name 'WNetConnectionDialog1A';

  function WNetDisconnectDialog1(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD; external External_library name 'WNetDisconnectDialog1A';

  function WNetOpenEnum(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD; external External_library name 'WNetOpenEnumA';

  function WNetEnumResource(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external External_library name 'WNetEnumResourceA';

  function WNetGetUniversalName(lpLocalPath:LPCSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external External_library name 'WNetGetUniversalNameA';

  function WNetGetUser(lpName:LPCSTR; lpUserName:LPSTR; lpnLength:LPDWORD):DWORD; external External_library name 'WNetGetUserA';

  function WNetGetProviderName(dwNetType:DWORD; lpProviderName:LPSTR; lpBufferSize:LPDWORD):DWORD; external External_library name 'WNetGetProviderNameA';

  function WNetGetNetworkInformation(lpProvider:LPCSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD; external External_library name 'WNetGetNetworkInformationA';

  function WNetGetLastError(lpError:LPDWORD; lpErrorBuf:LPSTR; nErrorBufSize:DWORD; lpNameBuf:LPSTR; nNameBufSize:DWORD):DWORD; external External_library name 'WNetGetLastErrorA';

  function MultinetGetConnectionPerformance(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD; external External_library name 'MultinetGetConnectionPerformanceA';

  function ChangeServiceConfig(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; 
             lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR; 
             lpDisplayName:LPCSTR):WINBOOL; external External_library name 'ChangeServiceConfigA';

  function CreateService(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPCSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD; 
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; 
             lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR):SC_HANDLE; external External_library name 'CreateServiceA';

  function EnumDependentServices(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD; 
             lpServicesReturned:LPDWORD):WINBOOL; external External_library name 'EnumDependentServicesA';

  function EnumServicesStatus(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; 
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL; external External_library name 'EnumServicesStatusA';

  function GetServiceKeyName(hSCManager:SC_HANDLE; lpDisplayName:LPCSTR; lpServiceName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL; external External_library name 'GetServiceKeyNameA';

  function GetServiceDisplayName(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL; external External_library name 'GetServiceDisplayNameA';

  function OpenSCManager(lpMachineName:LPCSTR; lpDatabaseName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE; external External_library name 'OpenSCManagerA';

  function OpenService(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE; external External_library name 'OpenServiceA';

  function QueryServiceConfig(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external External_library name 'QueryServiceConfigA';

  function QueryServiceLockStatus(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external External_library name 'QueryServiceLockStatusA';

  function RegisterServiceCtrlHandler(lpServiceName:LPCSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE; external External_library name 'RegisterServiceCtrlHandlerA';

  function StartServiceCtrlDispatcher(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL; external External_library name 'StartServiceCtrlDispatcherA';

  function StartService(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCSTR):WINBOOL; external External_library name 'StartServiceA';

  function wglUseFontBitmaps(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD):WINBOOL; external External_library name 'wglUseFontBitmapsA';

  function wglUseFontOutlines(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:DWORD; _para5:FLOAT; 
             _para6:FLOAT; _para7:longint; _para8:LPGLYPHMETRICSFLOAT):WINBOOL; external External_library name 'wglUseFontOutlinesA';

  function DragQueryFile(_para1:HDROP; _para2:cardinal; var _para3:char; _para4:cardinal):cardinal; external External_library name 'DragQueryFileA';

  function ExtractAssociatedIcon(_para1:HINSTANCE; var _para2:char; var _para3:WORD):HICON; external External_library name 'ExtractAssociatedIconA';

  function ExtractIcon(_para1:HINSTANCE; var _para2:char; _para3:cardinal):HICON; external External_library name 'ExtractIconA';

  function FindExecutable(var _para1:char; var _para2:char; var _para3:char):HINSTANCE; external External_library name 'FindExecutableA';

  function ShellAbout(_para1:HWND; var _para2:char; var _para3:char; _para4:HICON):longint; external External_library name 'ShellAboutA';

  function ShellExecute(_para1:HWND; var _para2:char; var _para3:char; var _para4:char; var _para5:char; 
             _para6:longint):HINSTANCE; external External_library name 'ShellExecuteA';

  function DdeCreateStringHandle(_para1:DWORD; var _para2:char; _para3:longint):HSZ; external External_library name 'DdeCreateStringHandleA';

  function DdeInitialize(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT; external External_library name 'DdeInitializeA';

  function DdeQueryString(_para1:DWORD; _para2:HSZ; var _para3:char; _para4:DWORD; _para5:longint):DWORD; external External_library name 'DdeQueryStringA';

  function LogonUser(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:DWORD; _para5:DWORD; 
             var _para6:HANDLE):WINBOOL; external External_library name 'LogonUserA';

  function CreateProcessAsUser(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES; 
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCTSTR; var _para10:STARTUPINFO; 
             var _para11:PROCESS_INFORMATION):WINBOOL; external External_library name 'CreateProcessAsUserA';

{$endif read_implementation}

{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.1  1998-08-31 11:53:53  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

}
