{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Contains the Ascii functions for windows unit

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
interface
uses
  base,defines,struct;

{$endif windows_include_files}

{$ifdef read_interface}

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

  function LoadLibrary(lpLibFileName:LPCSTR):HINST;

  function LoadLibraryEx(lpLibFileName:LPCSTR; hFile:HANDLE; dwFlags:DWORD):HINST;

  function GetModuleFileName(hModule:HINST; lpFilename:LPSTR; nSize:DWORD):DWORD;

  function GetModuleHandle(lpModuleName:LPCSTR):HMODULE;

  procedure FatalAppExit(uAction:UINT; lpMessageText:LPCSTR);

  function GetCommandLine : LPSTR;

  function GetEnvironmentVariable(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD;

  function SetEnvironmentVariable(lpName:LPCSTR; lpValue:LPCSTR):WINBOOL;

  function ExpandEnvironmentStrings(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):DWORD;

  procedure OutputDebugString(lpOutputString:LPCSTR);

  function FindResource(hModule:HINST; lpName:LPCSTR; lpType:LPCSTR):HRSRC;

  function FindResourceEx(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD):HRSRC;

  function EnumResourceTypes(hModule:HINST; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL;

  function EnumResourceNames(hModule:HINST; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL;

  function EnumResourceLanguages(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL;

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

  function RegisterClass(var lpWndClass:WNDCLASS):ATOM;

  function UnregisterClass(lpClassName:LPCSTR; hInstance:HINST):WINBOOL;

  function GetClassInfo(hInstance:HINST; lpClassName:LPCSTR; lpWndClass:LPWNDCLASS):WINBOOL;

  function GetClassInfo(hInstance:HINST; lpClassName:LPCSTR; var WndClass:TWNDCLASS):WINBOOL;

  function RegisterClassEx(var _para1:WNDCLASSEX):ATOM;

  function GetClassInfoEx(_para1:HINST; _para2:LPCSTR; _para3:LPWNDCLASSEX):WINBOOL;

  function GetClassInfoEx(_para1:HINST; _para2:LPCSTR; var _para3:TWNDCLASSEX):WINBOOL;

  function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND;

  function CreateDialogParam(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function CreateDialogIndirectParam(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function DialogBoxParam(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function DialogBoxIndirectParam(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

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

  function LoadAccelerators(hInstance:HINST; lpTableName:LPCSTR):HACCEL;

  function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL;

  function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint;

  function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint;

  function LoadMenu(hInstance:HINST; lpMenuName:LPCSTR):HMENU;

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

  function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINST; dwThreadId:DWORD):HHOOK;

  function LoadBitmap(hInstance:HINST; lpBitmapName:LPCSTR):HBITMAP;

  function LoadCursor(hInstance:HINST; lpCursorName:LPCSTR):HCURSOR;

  function LoadCursorFromFile(lpFileName:LPCSTR):HCURSOR;

  function LoadIcon(hInstance:HINST; lpIconName:LPCSTR):HICON;

  function LoadImage(_para1:HINST; _para2:LPCSTR; _para3:UINT; _para4:longint; _para5:longint;
             _para6:UINT):HANDLE;

  function LoadString(hInstance:HINST; uID:UINT; lpBuffer:LPSTR; nBufferMax:longint):longint;

  function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL;

  function DlgDirList(hDlg:HWND; lpPathSpec:LPSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint;

  function DlgDirSelectEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDListBox:longint):WINBOOL;

  function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint;

  function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDComboBox:longint):WINBOOL;

  function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CreateMDIWindow(lpClassName:LPSTR; lpWindowName:LPSTR; dwStyle:DWORD; X:longint; Y:longint;
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINST; lParam:LPARAM):HWND;

  function WinHelp(hWndMain:HWND; lpszHelp:LPCSTR; uCommand:UINT; dwData:DWORD):WINBOOL;

  function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG;

  function EnumDisplaySettings(lpszDeviceName:LPCSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL;

  function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL;

  function AddFontResource(_para1:LPCSTR):longint;

  function CopyMetaFile(_para1:HMETAFILE; _para2:LPCSTR):HMETAFILE;

  function CreateFontIndirect(var _para1:LOGFONT):HFONT;

  function CreateIC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC;

  function CreateMetaFile(_para1:LPCSTR):HDC;

  function CreateScalableFontResource(_para1:DWORD; _para2:LPCSTR; _para3:LPCSTR; _para4:LPCSTR):WINBOOL;

  function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint;

  function EnumFontFamilies(_para1:HDC; _para2:LPCSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint;

  function EnumFonts(_para1:HDC; _para2:LPCSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint;

  function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL;

  function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL;

  function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL;

  function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD;
             _para6:LPVOID; var _para7:MAT2):DWORD;

  function GetMetaFile(_para1:LPCSTR):HMETAFILE;

  function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT;

  function GetTextExtentPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; var _para4:TSIZE):WINBOOL;

  function GetTextExtentPoint32(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentExPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPINT;
             _para6:LPINT; _para7:LPSIZE):WINBOOL;

  function GetCharacterPlacement(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS;
             _para6:DWORD):DWORD;

  function ResetDC(_para1:HDC; var _para2:DEVMODE):HDC;

  function RemoveFontResource(_para1:LPCSTR):WINBOOL;

  function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCSTR):HENHMETAFILE;

  function CreateEnhMetaFile(_para1:HDC; _para2:LPCSTR; var _para3:RECT; _para4:LPCSTR):HDC;

  function GetEnhMetaFile(_para1:LPCSTR):HENHMETAFILE;

  function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPSTR):UINT;

  function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL;

  function GetTextMetrics(_para1:HDC; var _para2:TTEXTMETRIC):WINBOOL;

  function StartDoc(_para1:HDC; var _para2:DOCINFO):longint;

  function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint;

  function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint):WINBOOL;

  function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT;
             _para6:LPCSTR; _para7:UINT; var _para8:INT):WINBOOL;

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

  function ImageList_LoadImage(hi:HINST; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF;
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

  function GetVersionEx(var VersionInformation:OSVERSIONINFO):WINBOOL;

  { was #define dname(params) def_expr }
  function CreateWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND;

  { was #define dname(params) def_expr }
  function CreateDialog(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogIndirect(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function DialogBox(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  { was #define dname(params) def_expr }
  function DialogBoxIndirect(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  function CreateDC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; _para4:pDEVMODE):HDC;

  function VerInstallFile(uFlags:DWORD; szSrcFileName:LPSTR; szDestFileName:LPSTR; szSrcDir:LPSTR; szDestDir:LPSTR;
             szCurDir:LPSTR; szTmpFile:LPSTR; lpuTmpFileLen:PUINT):DWORD;

  function GetFileVersionInfoSize(lptstrFilename:LPSTR; lpdwHandle:LPDWORD):DWORD;

  function GetFileVersionInfo(lptstrFilename:LPSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL;

  function VerLanguageName(wLang:DWORD; szLang:LPSTR; nSize:DWORD):DWORD;

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

  function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM;var phkResult:HKEY):LONG;

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

  function GetTimeFormat(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCSTR; lpTimeStr:LPSTR;
             cchTime:longint):longint;

  function GetDateFormat(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCSTR; lpDateStr:LPSTR;
             cchDate:longint):longint;

  function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPSTR;
             cchNumber:longint):longint;

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

  function PeekConsoleInput(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL;

  function ReadConsoleInput(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL;

  function WriteConsoleInput(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsWritten:DWORD):WINBOOL;

  function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL;

  function WriteConsoleOutput(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL;

  function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL;

  function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:CHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL;

  function GetConsoleTitle(lpConsoleTitle:LPSTR; nSize:DWORD):DWORD;

  function SetConsoleTitle(lpConsoleTitle:LPCSTR):WINBOOL;

  function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL;

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

  function DragQueryFile(_para1:HDROP; _para2:cardinal; _para3:pchar;
    _para4:cardinal):cardinal;

  function ExtractAssociatedIcon(_para1:HINST;_para2:pchar; var _para3:WORD):HICON;

  function ExtractIcon(_para1:HINST;_para2:pchar; _para3:cardinal):HICON;

  function FindExecutable(_para1:pchar;_para2:pchar; _para3:pchar):HINST;

  function ShellAbout(_para1:HWND; _para2:pchar;
    _para3:pchar; _para4:HICON):longint;

  function ShellExecute(_para1:HWND; _para2:pchar; _para3:pchar;
    _para4:pchar; _para5:pchar;_para6:longint):HINST;

  function DdeCreateStringHandle(_para1:DWORD; _para2:pchar;
    _para3:longint):HSZ;

  function DdeInitialize(var _para1:DWORD; _para2:CALLB;
    _para3:DWORD; _para4:DWORD):UINT;

  function DdeQueryString(_para1:DWORD; _para2:HSZ; _para3:pchar;
    _para4:DWORD; _para5:longint):DWORD;

  function LogonUser(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:DWORD; _para5:DWORD;
             var _para6:HANDLE):WINBOOL;

  function CreateProcessAsUser(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES;
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCTSTR; var _para10:STARTUPINFO;
             var _para11:PROCESS_INFORMATION):WINBOOL;

{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

  function GetBinaryType(lpApplicationName:LPCSTR; lpBinaryType:LPDWORD):WINBOOL; external 'kernel32' name 'GetBinaryTypeA';

  function GetShortPathName(lpszLongPath:LPCSTR; lpszShortPath:LPSTR; cchBuffer:DWORD):DWORD; external 'kernel32' name 'GetShortPathNameA';

  function GetEnvironmentStrings : LPSTR; external 'kernel32' name 'GetEnvironmentStringsA';

  function FreeEnvironmentStrings(_para1:LPSTR):WINBOOL; external 'kernel32' name 'FreeEnvironmentStringsA';

  function FormatMessage(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPSTR;
             nSize:DWORD; var Arguments:va_list):DWORD; external 'kernel32' name 'FormatMessageA';

  function CreateMailslot(lpName:LPCSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32' name 'CreateMailslotA';

  function lstrcmp(lpString1:LPCSTR; lpString2:LPCSTR):longint; external 'kernel32' name 'lstrcmpA';

  function lstrcmpi(lpString1:LPCSTR; lpString2:LPCSTR):longint; external 'kernel32' name 'lstrcmpiA';

  function lstrcpyn(lpString1:LPSTR; lpString2:LPCSTR; iMaxLength:longint):LPSTR; external 'kernel32' name 'lstrcpynA';

  function lstrcpy(lpString1:LPSTR; lpString2:LPCSTR):LPSTR; external 'kernel32' name 'lstrcpyA';

  function lstrcat(lpString1:LPSTR; lpString2:LPCSTR):LPSTR; external 'kernel32' name 'lstrcatA';

  function lstrlen(lpString:LPCSTR):longint; external 'kernel32' name 'lstrlenA';

  function CreateMutex(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateMutexA';

  function OpenMutex(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenMutexA';

  function CreateEvent(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateEventA';

  function OpenEvent(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenEventA';

  function CreateSemaphore(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateSemaphoreA';

  function OpenSemaphore(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenSemaphoreA';

  function CreateFileMapping(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD;
             lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateFileMappingA';

  function OpenFileMapping(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenFileMappingA';

  function GetLogicalDriveStrings(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external 'kernel32' name 'GetLogicalDriveStringsA';

  function LoadLibrary(lpLibFileName:LPCSTR):HINST; external 'kernel32' name 'LoadLibraryA';

  function LoadLibraryEx(lpLibFileName:LPCSTR; hFile:HANDLE; dwFlags:DWORD):HINST; external 'kernel32' name 'LoadLibraryExA';

  function GetModuleFileName(hModule:HINST; lpFilename:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetModuleFileNameA';

  function GetModuleHandle(lpModuleName:LPCSTR):HMODULE; external 'kernel32' name 'GetModuleHandleA';

  procedure FatalAppExit(uAction:UINT; lpMessageText:LPCSTR); external 'kernel32' name 'FatalAppExitA';

  function GetCommandLine : LPSTR; external 'kernel32' name 'GetCommandLineA';

  function GetEnvironmentVariable(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetEnvironmentVariableA';

  function SetEnvironmentVariable(lpName:LPCSTR; lpValue:LPCSTR):WINBOOL; external 'kernel32' name 'SetEnvironmentVariableA';

  function ExpandEnvironmentStrings(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'ExpandEnvironmentStringsA';

  procedure OutputDebugString(lpOutputString:LPCSTR); external 'kernel32' name 'OutputDebugStringA';

  function FindResource(hModule:HINST; lpName:LPCSTR; lpType:LPCSTR):HRSRC; external 'kernel32' name 'FindResourceA';

  function FindResourceEx(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD):HRSRC; external 'kernel32' name 'FindResourceExA';

  function EnumResourceTypes(hModule:HINST; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceTypesA';

  function EnumResourceNames(hModule:HINST; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceNamesA';

  function EnumResourceLanguages(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceLanguagesA';

  function BeginUpdateResource(pFileName:LPCSTR; bDeleteExistingResources:WINBOOL):HANDLE; external 'kernel32' name 'BeginUpdateResourceA';

  function UpdateResource(hUpdate:HANDLE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD; lpData:LPVOID;
             cbData:DWORD):WINBOOL; external 'kernel32' name 'UpdateResourceA';

  function EndUpdateResource(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL; external 'kernel32' name 'EndUpdateResourceA';

  function GlobalAddAtom(lpString:LPCSTR):ATOM; external 'kernel32' name 'GlobalAddAtomA';

  function GlobalFindAtom(lpString:LPCSTR):ATOM; external 'kernel32' name 'GlobalFindAtomA';

  function GlobalGetAtomName(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT; external 'kernel32' name 'GlobalGetAtomNameA';

  function AddAtom(lpString:LPCSTR):ATOM; external 'kernel32' name 'AddAtomA';

  function FindAtom(lpString:LPCSTR):ATOM; external 'kernel32' name 'FindAtomA';

  function GetAtomName(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT; external 'kernel32' name 'GetAtomNameA';

  function GetProfileInt(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT):UINT; external 'kernel32' name 'GetProfileIntA';

  function GetProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetProfileStringA';

  function WriteProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR):WINBOOL; external 'kernel32' name 'WriteProfileStringA';

  function GetProfileSection(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetProfileSectionA';

  function WriteProfileSection(lpAppName:LPCSTR; lpString:LPCSTR):WINBOOL; external 'kernel32' name 'WriteProfileSectionA';

  function GetPrivateProfileInt(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT; lpFileName:LPCSTR):UINT; external 'kernel32' name 'GetPrivateProfileIntA';

  function GetPrivateProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD;
             lpFileName:LPCSTR):DWORD; external 'kernel32' name 'GetPrivateProfileStringA';

  function WritePrivateProfileString(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL; external 'kernel32' name 'WritePrivateProfileStringA';

  function GetPrivateProfileSection(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; lpFileName:LPCSTR):DWORD; external 'kernel32' name 'GetPrivateProfileSectionA';

  function WritePrivateProfileSection(lpAppName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL; external 'kernel32' name 'WritePrivateProfileSectionA';

  function GetDriveType(lpRootPathName:LPCSTR):UINT; external 'kernel32' name 'GetDriveTypeA';

  function GetSystemDirectory(lpBuffer:LPSTR; uSize:UINT):UINT; external 'kernel32' name 'GetSystemDirectoryA';

  function GetTempPath(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external 'kernel32' name 'GetTempPathA';

  function GetTempFileName(lpPathName:LPCSTR; lpPrefixString:LPCSTR; uUnique:UINT; lpTempFileName:LPSTR):UINT; external 'kernel32' name 'GetTempFileNameA';

  function GetWindowsDirectory(lpBuffer:LPSTR; uSize:UINT):UINT; external 'kernel32' name 'GetWindowsDirectoryA';

  function SetCurrentDirectory(lpPathName:LPCSTR):WINBOOL; external 'kernel32' name 'SetCurrentDirectoryA';

  function GetCurrentDirectory(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external 'kernel32' name 'GetCurrentDirectoryA';

  function GetDiskFreeSpace(lpRootPathName:LPCSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL; external 'kernel32' name 'GetDiskFreeSpaceA';

  function CreateDirectory(lpPathName:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32' name 'CreateDirectoryA';

  function CreateDirectoryEx(lpTemplateDirectory:LPCSTR; lpNewDirectory:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32' name 'CreateDirectoryExA';

  function RemoveDirectory(lpPathName:LPCSTR):WINBOOL; external 'kernel32' name 'RemoveDirectoryA';

  function GetFullPathName(lpFileName:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; var lpFilePart:LPSTR):DWORD; external 'kernel32' name 'GetFullPathNameA';

  function DefineDosDevice(dwFlags:DWORD; lpDeviceName:LPCSTR; lpTargetPath:LPCSTR):WINBOOL; external 'kernel32' name 'DefineDosDeviceA';

  function QueryDosDevice(lpDeviceName:LPCSTR; lpTargetPath:LPSTR; ucchMax:DWORD):DWORD; external 'kernel32' name 'QueryDosDeviceA';

  function CreateFile(lpFileName:LPCSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD;
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external 'kernel32' name 'CreateFileA';

  function SetFileAttributes(lpFileName:LPCSTR; dwFileAttributes:DWORD):WINBOOL; external 'kernel32' name 'SetFileAttributesA';

  function GetFileAttributes(lpFileName:LPCSTR):DWORD; external 'kernel32' name 'GetFileAttributesA';

  function GetCompressedFileSize(lpFileName:LPCSTR; lpFileSizeHigh:LPDWORD):DWORD; external 'kernel32' name 'GetCompressedFileSizeA';

  function DeleteFile(lpFileName:LPCSTR):WINBOOL; external 'kernel32' name 'DeleteFileA';

  function SearchPath(lpPath:LPCSTR; lpFileName:LPCSTR; lpExtension:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR;
             var lpFilePart:LPSTR):DWORD; external 'kernel32' name 'SearchPathA';

  function CopyFile(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; bFailIfExists:WINBOOL):WINBOOL; external 'kernel32' name 'CopyFileA';

  function MoveFile(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR):WINBOOL; external 'kernel32' name 'MoveFileA';

  function MoveFileEx(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'MoveFileExA';

  function CreateNamedPipe(lpName:LPCSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD;
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32' name 'CreateNamedPipeA';

  function GetNamedPipeHandleState(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD;
             lpUserName:LPSTR; nMaxUserNameSize:DWORD):WINBOOL; external 'kernel32' name 'GetNamedPipeHandleStateA';

  function CallNamedPipe(lpNamedPipeName:LPCSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL; external 'kernel32' name 'CallNamedPipeA';

  function WaitNamedPipe(lpNamedPipeName:LPCSTR; nTimeOut:DWORD):WINBOOL; external 'kernel32' name 'WaitNamedPipeA';

  function SetVolumeLabel(lpRootPathName:LPCSTR; lpVolumeName:LPCSTR):WINBOOL; external 'kernel32' name 'SetVolumeLabelA';

  function GetVolumeInformation(lpRootPathName:LPCSTR; lpVolumeNameBuffer:LPSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD;
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPSTR; nFileSystemNameSize:DWORD):WINBOOL; external 'kernel32' name 'GetVolumeInformationA';

  function ClearEventLog(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL; external 'advapi32' name 'ClearEventLogA';

  function BackupEventLog(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL; external 'advapi32' name 'BackupEventLogA';

  function OpenEventLog(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE; external 'advapi32' name 'OpenEventLogA';

  function RegisterEventSource(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE; external 'advapi32' name 'RegisterEventSourceA';

  function OpenBackupEventLog(lpUNCServerName:LPCSTR; lpFileName:LPCSTR):HANDLE; external 'advapi32' name 'OpenBackupEventLogA';

  function ReadEventLog(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD;
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL; external 'advapi32' name 'ReadEventLogA';

  function ReportEvent(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID;
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCSTR; lpRawData:LPVOID):WINBOOL; external 'advapi32' name 'ReportEventA';

  function AccessCheckAndAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR;
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL;
             pfGenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheckAndAuditAlarmA';

  function ObjectOpenAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR;
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL;
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'ObjectOpenAuditAlarmA';

  function ObjectPrivilegeAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET;
             AccessGranted:WINBOOL):WINBOOL; external 'advapi32' name 'ObjectPrivilegeAuditAlarmA';

  function ObjectCloseAuditAlarm(SubsystemName:LPCSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL; external 'advapi32' name 'ObjectCloseAuditAlarmA';

  function PrivilegedServiceAuditAlarm(SubsystemName:LPCSTR; ServiceName:LPCSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL; external 'advapi32' name 'PrivilegedServiceAuditAlarmA';

  function SetFileSecurity(lpFileName:LPCSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'SetFileSecurityA';

  function GetFileSecurity(lpFileName:LPCSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'GetFileSecurityA';

  function FindFirstChangeNotification(lpPathName:LPCSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE; external 'kernel32' name 'FindFirstChangeNotificationA';

  function IsBadStringPtr(lpsz:LPCSTR; ucchMax:UINT):WINBOOL; external 'kernel32' name 'IsBadStringPtrA';

  function LookupAccountSid(lpSystemName:LPCSTR; Sid:PSID; Name:LPSTR; cbName:LPDWORD; ReferencedDomainName:LPSTR;
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32' name 'LookupAccountSidA';

  function LookupAccountName(lpSystemName:LPCSTR; lpAccountName:LPCSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPSTR;
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32' name 'LookupAccountNameA';

  function LookupPrivilegeValue(lpSystemName:LPCSTR; lpName:LPCSTR; lpLuid:PLUID):WINBOOL; external 'advapi32' name 'LookupPrivilegeValueA';

  function LookupPrivilegeName(lpSystemName:LPCSTR; lpLuid:PLUID; lpName:LPSTR; cbName:LPDWORD):WINBOOL; external 'advapi32' name 'LookupPrivilegeNameA';

  function LookupPrivilegeDisplayName(lpSystemName:LPCSTR; lpName:LPCSTR; lpDisplayName:LPSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL; external 'advapi32' name 'LookupPrivilegeDisplayNameA';

  function BuildCommDCB(lpDef:LPCSTR; lpDCB:LPDCB):WINBOOL; external 'kernel32' name 'BuildCommDCBA';

  function BuildCommDCBAndTimeouts(lpDef:LPCSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external 'kernel32' name 'BuildCommDCBAndTimeoutsA';

  function CommConfigDialog(lpszName:LPCSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL; external 'kernel32' name 'CommConfigDialogA';

  function GetDefaultCommConfig(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetDefaultCommConfigA';

  function SetDefaultCommConfig(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external 'kernel32' name 'SetDefaultCommConfigA';

  function GetComputerName(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetComputerNameA';

  function SetComputerName(lpComputerName:LPCSTR):WINBOOL; external 'kernel32' name 'SetComputerNameA';

  function GetUserName(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL; external 'advapi32' name 'GetUserNameA';

  function wvsprintf(_para1:LPSTR; _para2:LPCSTR; arglist:va_list):longint; external 'user32' name 'wvsprintfA';

  function LoadKeyboardLayout(pwszKLID:LPCSTR; Flags:UINT):HKL; external 'user32' name 'LoadKeyboardLayoutA';

  function GetKeyboardLayoutName(pwszKLID:LPSTR):WINBOOL; external 'user32' name 'GetKeyboardLayoutNameA';

  function CreateDesktop(lpszDesktop:LPSTR; lpszDevice:LPSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD;
             lpsa:LPSECURITY_ATTRIBUTES):HDESK; external 'user32' name 'CreateDesktopA';

  function OpenDesktop(lpszDesktop:LPSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external 'user32' name 'OpenDesktopA';

  function EnumDesktops(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumDesktopsA';

  function CreateWindowStation(lpwinsta:LPSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA; external 'user32' name 'CreateWindowStationA';

  function OpenWindowStation(lpszWinSta:LPSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA; external 'user32' name 'OpenWindowStationA';

  function EnumWindowStations(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumWindowStationsA';

  function GetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'user32' name 'GetUserObjectInformationA';

  function SetUserObjectInformation(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL; external 'user32' name 'SetUserObjectInformationA';

  function RegisterWindowMessage(lpString:LPCSTR):UINT; external 'user32' name 'RegisterWindowMessageA';

  function GetMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL; external 'user32' name 'GetMessageA';

  function DispatchMessage(var lpMsg:MSG):LONG; external 'user32' name 'DispatchMessageA';

  function PeekMessage(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external 'user32' name 'PeekMessageA';

  function SendMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'SendMessageA';

  function SendMessageTimeout(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT;
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external 'user32' name 'SendMessageTimeoutA';

  function SendNotifyMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32' name 'SendNotifyMessageA';

  function SendMessageCallback(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC;
             dwData:DWORD):WINBOOL; external 'user32' name 'SendMessageCallbackA';

  function PostMessage(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32' name 'PostMessageA';

  function PostThreadMessage(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32' name 'PostThreadMessageA';

  function DefWindowProc(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefWindowProcA';

  function CallWindowProc(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'CallWindowProcA';

  function RegisterClass(var lpWndClass:WNDCLASS):ATOM; external 'user32' name 'RegisterClassA';

  function UnregisterClass(lpClassName:LPCSTR; hInstance:HINST):WINBOOL; external 'user32' name 'UnregisterClassA';

  function GetClassInfo(hInstance:HINST; lpClassName:LPCSTR; lpWndClass:LPWNDCLASS):WINBOOL; external 'user32' name 'GetClassInfoA';

  function GetClassInfo(hInstance:HINST; lpClassName:LPCSTR; var WndClass:TWNDCLASS):WINBOOL; external 'user32' name 'GetClassInfoA';

  function RegisterClassEx(var _para1:WNDCLASSEX):ATOM; external 'user32' name 'RegisterClassExA';

  function GetClassInfoEx(_para1:HINST; _para2:LPCSTR; _para3:LPWNDCLASSEX):WINBOOL; external 'user32' name 'GetClassInfoExA';

  function GetClassInfoEx(_para1:HINST; _para2:LPCSTR; var _para3:TWNDCLASSEX):WINBOOL; external 'user32' name 'GetClassInfoExA';

  function CreateWindowEx(dwExStyle:DWORD; lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND; external 'user32' name 'CreateWindowExA';

  function CreateDialogParam(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32' name 'CreateDialogParamA';

  function CreateDialogIndirectParam(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32' name 'CreateDialogIndirectParamA';

  function DialogBoxParam(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32' name 'DialogBoxParamA';

  function DialogBoxIndirectParam(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32' name 'DialogBoxIndirectParamA';

  function SetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPCSTR):WINBOOL; external 'user32' name 'SetDlgItemTextA';

  function GetDlgItemText(hDlg:HWND; nIDDlgItem:longint; lpString:LPSTR; nMaxCount:longint):UINT; external 'user32' name 'GetDlgItemTextA';

  function SendDlgItemMessage(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG; external 'user32' name 'SendDlgItemMessageA';

  function DefDlgProc(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefDlgProcA';

  function CallMsgFilter(lpMsg:LPMSG; nCode:longint):WINBOOL; external 'user32' name 'CallMsgFilterA';

  function RegisterClipboardFormat(lpszFormat:LPCSTR):UINT; external 'user32' name 'RegisterClipboardFormatA';

  function GetClipboardFormatName(format:UINT; lpszFormatName:LPSTR; cchMaxCount:longint):longint; external 'user32' name 'GetClipboardFormatNameA';

  function CharToOem(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL; external 'user32' name 'CharToOemA';

  function OemToChar(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL; external 'user32' name 'OemToCharA';

  function CharToOemBuff(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external 'user32' name 'CharToOemBuffA';

  function OemToCharBuff(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external 'user32' name 'OemToCharBuffA';

  function CharUpper(lpsz:LPSTR):LPSTR; external 'user32' name 'CharUpperA';

  function CharUpperBuff(lpsz:LPSTR; cchLength:DWORD):DWORD; external 'user32' name 'CharUpperBuffA';

  function CharLower(lpsz:LPSTR):LPSTR; external 'user32' name 'CharLowerA';

  function CharLowerBuff(lpsz:LPSTR; cchLength:DWORD):DWORD; external 'user32' name 'CharLowerBuffA';

  function CharNext(lpsz:LPCSTR):LPSTR; external 'user32' name 'CharNextA';

  function CharPrev(lpszStart:LPCSTR; lpszCurrent:LPCSTR):LPSTR; external 'user32' name 'CharPrevA';

  function IsCharAlpha(ch:CHAR):WINBOOL; external 'user32' name 'IsCharAlphaA';

  function IsCharAlphaNumeric(ch:CHAR):WINBOOL; external 'user32' name 'IsCharAlphaNumericA';

  function IsCharUpper(ch:CHAR):WINBOOL; external 'user32' name 'IsCharUpperA';

  function IsCharLower(ch:CHAR):WINBOOL; external 'user32' name 'IsCharLowerA';

  function GetKeyNameText(lParam:LONG; lpString:LPSTR; nSize:longint):longint; external 'user32' name 'GetKeyNameTextA';

  function VkKeyScan(ch:CHAR):SHORT; external 'user32' name 'VkKeyScanA';

  function VkKeyScanEx(ch:CHAR; dwhkl:HKL):SHORT; external 'user32' name 'VkKeyScanExA';

  function MapVirtualKey(uCode:UINT; uMapType:UINT):UINT; external 'user32' name 'MapVirtualKeyA';

  function MapVirtualKeyEx(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT; external 'user32' name 'MapVirtualKeyExA';

  function LoadAccelerators(hInstance:HINST; lpTableName:LPCSTR):HACCEL; external 'user32' name 'LoadAcceleratorsA';

  function CreateAcceleratorTable(_para1:LPACCEL; _para2:longint):HACCEL; external 'user32' name 'CreateAcceleratorTableA';

  function CopyAcceleratorTable(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint; external 'user32' name 'CopyAcceleratorTableA';

  function TranslateAccelerator(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint; external 'user32' name 'TranslateAcceleratorA';

  function LoadMenu(hInstance:HINST; lpMenuName:LPCSTR):HMENU; external 'user32' name 'LoadMenuA';

  function LoadMenuIndirect(var lpMenuTemplate:MENUTEMPLATE):HMENU; external 'user32' name 'LoadMenuIndirectA';

  function ChangeMenu(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCSTR; cmdInsert:UINT; flags:UINT):WINBOOL; external 'user32' name 'ChangeMenuA';

  function GetMenuString(hMenu:HMENU; uIDItem:UINT; lpString:LPSTR; nMaxCount:longint; uFlag:UINT):longint; external 'user32' name 'GetMenuStringA';

  function InsertMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external 'user32' name 'InsertMenuA';

  function AppendMenu(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external 'user32' name 'AppendMenuA';

  function ModifyMenu(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external 'user32' name 'ModifyMenuA';

  function InsertMenuItem(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32' name 'InsertMenuItemA';

  function GetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL; external 'user32' name 'GetMenuItemInfoA';

  function SetMenuItemInfo(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32' name 'SetMenuItemInfoA';

  function DrawText(hDC:HDC; lpString:LPCSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint; external 'user32' name 'DrawTextA';

  function DrawTextEx(_para1:HDC; _para2:LPSTR; _para3:longint; _para4:LPRECT; _para5:UINT;
             _para6:LPDRAWTEXTPARAMS):longint; external 'user32' name 'DrawTextExA';

  function GrayString(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint;
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL; external 'user32' name 'GrayStringA';

  function DrawState(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL; external 'user32' name 'DrawStateA';

  function TabbedTextOut(hDC:HDC; X:longint; Y:longint; lpString:LPCSTR; nCount:longint;
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG; external 'user32' name 'TabbedTextOutA';

  function GetTabbedTextExtent(hDC:HDC; lpString:LPCSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD; external 'user32' name 'GetTabbedTextExtentA';

  function SetProp(hWnd:HWND; lpString:LPCSTR; hData:HANDLE):WINBOOL; external 'user32' name 'SetPropA';

  function GetProp(hWnd:HWND; lpString:LPCSTR):HANDLE; external 'user32' name 'GetPropA';

  function RemoveProp(hWnd:HWND; lpString:LPCSTR):HANDLE; external 'user32' name 'RemovePropA';

  function EnumPropsEx(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint; external 'user32' name 'EnumPropsExA';

  function EnumProps(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint; external 'user32' name 'EnumPropsA';

  function SetWindowText(hWnd:HWND; lpString:LPCSTR):WINBOOL; external 'user32' name 'SetWindowTextA';

  function GetWindowText(hWnd:HWND; lpString:LPSTR; nMaxCount:longint):longint; external 'user32' name 'GetWindowTextA';

  function GetWindowTextLength(hWnd:HWND):longint; external 'user32' name 'GetWindowTextLengthA';

  function MessageBox(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT):longint; external 'user32' name 'MessageBoxA';

  function MessageBoxEx(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT; wLanguageId:WORD):longint; external 'user32' name 'MessageBoxExA';

  function MessageBoxIndirect(_para1:LPMSGBOXPARAMS):longint; external 'user32' name 'MessageBoxIndirectA';

  function GetWindowLong(hWnd:HWND; nIndex:longint):LONG; external 'user32' name 'GetWindowLongA';

  function SetWindowLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG; external 'user32' name 'SetWindowLongA';

  function GetClassLong(hWnd:HWND; nIndex:longint):DWORD; external 'user32' name 'GetClassLongA';

  function SetClassLong(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD; external 'user32' name 'SetClassLongA';

  function FindWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR):HWND; external 'user32' name 'FindWindowA';

  function FindWindowEx(_para1:HWND; _para2:HWND; _para3:LPCSTR; _para4:LPCSTR):HWND; external 'user32' name 'FindWindowExA';

  function GetClassName(hWnd:HWND; lpClassName:LPSTR; nMaxCount:longint):longint; external 'user32' name 'GetClassNameA';

  function SetWindowsHookEx(idHook:longint; lpfn:HOOKPROC; hmod:HINST; dwThreadId:DWORD):HHOOK; external 'user32' name 'SetWindowsHookExA';

  function LoadBitmap(hInstance:HINST; lpBitmapName:LPCSTR):HBITMAP; external 'user32' name 'LoadBitmapA';

  function LoadCursor(hInstance:HINST; lpCursorName:LPCSTR):HCURSOR; external 'user32' name 'LoadCursorA';

  function LoadCursorFromFile(lpFileName:LPCSTR):HCURSOR; external 'user32' name 'LoadCursorFromFileA';

  function LoadIcon(hInstance:HINST; lpIconName:LPCSTR):HICON; external 'user32' name 'LoadIconA';

  function LoadImage(_para1:HINST; _para2:LPCSTR; _para3:UINT; _para4:longint; _para5:longint;
             _para6:UINT):HANDLE; external 'user32' name 'LoadImageA';

  function LoadString(hInstance:HINST; uID:UINT; lpBuffer:LPSTR; nBufferMax:longint):longint; external 'user32' name 'LoadStringA';

  function IsDialogMessage(hDlg:HWND; lpMsg:LPMSG):WINBOOL; external 'user32' name 'IsDialogMessageA';

  function DlgDirList(hDlg:HWND; lpPathSpec:LPSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint; external 'user32' name 'DlgDirListA';

  function DlgDirSelectEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDListBox:longint):WINBOOL; external 'user32' name 'DlgDirSelectExA';

  function DlgDirListComboBox(hDlg:HWND; lpPathSpec:LPSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint; external 'user32' name 'DlgDirListComboBoxA';

  function DlgDirSelectComboBoxEx(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDComboBox:longint):WINBOOL; external 'user32' name 'DlgDirSelectComboBoxExA';

  function DefFrameProc(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefFrameProcA';

  function DefMDIChildProc(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefMDIChildProcA';

  function CreateMDIWindow(lpClassName:LPSTR; lpWindowName:LPSTR; dwStyle:DWORD; X:longint; Y:longint;
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINST; lParam:LPARAM):HWND; external 'user32' name 'CreateMDIWindowA';

  function WinHelp(hWndMain:HWND; lpszHelp:LPCSTR; uCommand:UINT; dwData:DWORD):WINBOOL; external 'user32' name 'WinHelpA';

  function ChangeDisplaySettings(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG; external 'user32' name 'ChangeDisplaySettingsA';

  function EnumDisplaySettings(lpszDeviceName:LPCSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL; external 'user32' name 'EnumDisplaySettingsA';

  function SystemParametersInfo(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL; external 'user32' name 'SystemParametersInfoA';

  function AddFontResource(_para1:LPCSTR):longint; external 'gdi32' name 'AddFontResourceA';

  function CopyMetaFile(_para1:HMETAFILE; _para2:LPCSTR):HMETAFILE; external 'gdi32' name 'CopyMetaFileA';

  function CreateFontIndirect(var _para1:LOGFONT):HFONT; external 'gdi32' name 'CreateFontIndirectA';

  function CreateIC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC; external 'gdi32' name 'CreateICA';

  function CreateMetaFile(_para1:LPCSTR):HDC; external 'gdi32' name 'CreateMetaFileA';

  function CreateScalableFontResource(_para1:DWORD; _para2:LPCSTR; _para3:LPCSTR; _para4:LPCSTR):WINBOOL; external 'gdi32' name 'CreateScalableFontResourceA';

  function EnumFontFamiliesEx(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint; external 'gdi32' name 'EnumFontFamiliesExA';

  function EnumFontFamilies(_para1:HDC; _para2:LPCSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint; external 'gdi32' name 'EnumFontFamiliesA';

  function EnumFonts(_para1:HDC; _para2:LPCSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint; external 'gdi32' name 'EnumFontsA';

  function GetCharWidth(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32' name 'GetCharWidthA';

  function GetCharWidth32(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32' name 'GetCharWidth32A';

  function GetCharWidthFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL; external 'gdi32' name 'GetCharWidthFloatA';

  function GetCharABCWidths(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external 'gdi32' name 'GetCharABCWidthsA';

  function GetCharABCWidthsFloat(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL; external 'gdi32' name 'GetCharABCWidthsFloatA';

  function GetGlyphOutline(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD;
             _para6:LPVOID; var _para7:MAT2):DWORD; external 'gdi32' name 'GetGlyphOutlineA';

  function GetMetaFile(_para1:LPCSTR):HMETAFILE; external 'gdi32' name 'GetMetaFileA';

  function GetOutlineTextMetrics(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT; external 'gdi32' name 'GetOutlineTextMetricsA';

  function GetTextExtentPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPointA';

  function GetTextExtentPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; var _para4:TSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPointA';

  function GetTextExtentPoint32(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPoint32A';

  function GetTextExtentExPoint(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPINT;
             _para6:LPINT; _para7:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentExPointA';

  function GetCharacterPlacement(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS;
             _para6:DWORD):DWORD; external 'gdi32' name 'GetCharacterPlacementA';

  function ResetDC(_para1:HDC; var _para2:DEVMODE):HDC; external 'gdi32' name 'ResetDCA';

  function RemoveFontResource(_para1:LPCSTR):WINBOOL; external 'gdi32' name 'RemoveFontResourceA';

  function CopyEnhMetaFile(_para1:HENHMETAFILE; _para2:LPCSTR):HENHMETAFILE; external 'gdi32' name 'CopyEnhMetaFileA';

  function CreateEnhMetaFile(_para1:HDC; _para2:LPCSTR; var _para3:RECT; _para4:LPCSTR):HDC; external 'gdi32' name 'CreateEnhMetaFileA';

  function GetEnhMetaFile(_para1:LPCSTR):HENHMETAFILE; external 'gdi32' name 'GetEnhMetaFileA';

  function GetEnhMetaFileDescription(_para1:HENHMETAFILE; _para2:UINT; _para3:LPSTR):UINT; external 'gdi32' name 'GetEnhMetaFileDescriptionA';

  function GetTextMetrics(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL; external 'gdi32' name 'GetTextMetricsA';

  function GetTextMetrics(_para1:HDC; var _para2:TTEXTMETRIC):WINBOOL; external 'gdi32' name 'GetTextMetricsA';

  function StartDoc(_para1:HDC; var _para2:DOCINFO):longint; external 'gdi32' name 'StartDocA';

  function GetObject(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint; external 'gdi32' name 'GetObjectA';

  function TextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint):WINBOOL; external 'gdi32' name 'TextOutA';

  function ExtTextOut(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT;
             _para6:LPCSTR; _para7:UINT; var _para8:INT):WINBOOL; external 'gdi32' name 'ExtTextOutA';

  function PolyTextOut(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL; external 'gdi32' name 'PolyTextOutA';

  function GetTextFace(_para1:HDC; _para2:longint; _para3:LPSTR):longint; external 'gdi32' name 'GetTextFaceA';

  function GetKerningPairs(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD; external 'gdi32' name 'GetKerningPairsA';

  function CreateColorSpace(_para1:LPLOGCOLORSPACE):HCOLORSPACE; external 'gdi32' name 'CreateColorSpaceA';

  function GetLogColorSpace(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL; external 'gdi32' name 'GetLogColorSpaceA';

  function GetICMProfile(_para1:HDC; _para2:DWORD; _para3:LPSTR):WINBOOL; external 'gdi32' name 'GetICMProfileA';

  function SetICMProfile(_para1:HDC; _para2:LPSTR):WINBOOL; external 'gdi32' name 'SetICMProfileA';

  function UpdateICMRegKey(_para1:DWORD; _para2:DWORD; _para3:LPSTR; _para4:UINT):WINBOOL; external 'gdi32' name 'UpdateICMRegKeyA';

  function EnumICMProfiles(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint; external 'gdi32' name 'EnumICMProfilesA';

  function PropertySheet(lppsph:LPCPROPSHEETHEADER):longint; external 'comctl32' name 'PropertySheetA';

  function ImageList_LoadImage(hi:HINST; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF;
             uType:UINT; uFlags:UINT):HIMAGELIST; external 'comctl32' name 'ImageList_LoadImageA';

  function CreateStatusWindow(style:LONG; lpszText:LPCSTR; hwndParent:HWND; wID:UINT):HWND; external 'comctl32' name 'CreateStatusWindowA';

  procedure DrawStatusText(hDC:HDC; lprc:LPRECT; pszText:LPCSTR; uFlags:UINT); external 'comctl32' name 'DrawStatusTextA';

  function GetOpenFileName(_para1:LPOPENFILENAME):WINBOOL; external 'comdlg32' name 'GetOpenFileNameA';

  function GetSaveFileName(_para1:LPOPENFILENAME):WINBOOL; external 'comdlg32' name 'GetSaveFileNameA';

  function GetFileTitle(_para1:LPCSTR; _para2:LPSTR; _para3:WORD):integer; external 'comdlg32' name 'GetFileTitleA';

  function ChooseColor(_para1:LPCHOOSECOLOR):WINBOOL; external 'comdlg32' name 'ChooseColorA';

  function FindText(_para1:LPFINDREPLACE):HWND; external 'comdlg32' name 'FindTextA';

  function ReplaceText(_para1:LPFINDREPLACE):HWND; external 'comdlg32' name 'ReplaceTextA';

  function ChooseFont(_para1:LPCHOOSEFONT):WINBOOL; external 'comdlg32' name 'ChooseFontA';

  function PrintDlg(_para1:LPPRINTDLG):WINBOOL; external 'comdlg32' name 'PrintDlgA';

  function PageSetupDlg(_para1:LPPAGESETUPDLG):WINBOOL; external 'comdlg32' name 'PageSetupDlgA';

  function CreateProcess(lpApplicationName:LPCSTR; lpCommandLine:LPSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL;
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL; external 'kernel32' name 'CreateProcessA';

  procedure GetStartupInfo(lpStartupInfo:LPSTARTUPINFO); external 'kernel32' name 'GetStartupInfoA';

  function FindFirstFile(lpFileName:LPCSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE; external 'kernel32' name 'FindFirstFileA';

  function FindNextFile(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL; external 'kernel32' name 'FindNextFileA';

  function GetVersionEx(var VersionInformation:OSVERSIONINFO):WINBOOL; external 'kernel32' name 'GetVersionExA';

  { was #define dname(params) def_expr }
  function CreateWindow(lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND;
    begin
       CreateWindow:=CreateWindowEx(0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam);
    end;

  { was #define dname(params) def_expr }
  function CreateDialog(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialog:=CreateDialogParam(hInstance,lpTemplateName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogIndirect(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogIndirect:=CreateDialogIndirectParam(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBox(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBox:=DialogBoxParam(hInstance,lpTemplateName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxIndirect(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxIndirect:=DialogBoxIndirectParam(hInstance,hDialogTemplate,hWndParent,lpDialogFunc,0);
    end;

  function CreateDC(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; _para4:pDEVMODE):HDC; external 'gdi32' name 'CreateDCA';

  function VerInstallFile(uFlags:DWORD; szSrcFileName:LPSTR; szDestFileName:LPSTR; szSrcDir:LPSTR; szDestDir:LPSTR;
             szCurDir:LPSTR; szTmpFile:LPSTR; lpuTmpFileLen:PUINT):DWORD; external 'version' name 'VerInstallFileA';

  function GetFileVersionInfoSize(lptstrFilename:LPSTR; lpdwHandle:LPDWORD):DWORD; external 'version' name 'GetFileVersionInfoSizeA';

  function GetFileVersionInfo(lptstrFilename:LPSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external 'version' name 'GetFileVersionInfoA';

  function VerLanguageName(wLang:DWORD; szLang:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'VerLanguageNameA';

  function VerQueryValue(pBlock:LPVOID; lpSubBlock:LPSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external 'version' name 'VerQueryValueA';

  function VerFindFile(uFlags:DWORD; szFileName:LPSTR; szWinDir:LPSTR; szAppDir:LPSTR; szCurDir:LPSTR;
             lpuCurDirLen:PUINT; szDestDir:LPSTR; lpuDestDirLen:PUINT):DWORD; external 'version' name 'VerFindFileA';

  function RegConnectRegistry(lpMachineName:LPSTR; hKey:HKEY; phkResult:PHKEY):LONG; external 'advapi32' name 'RegConnectRegistryA';

  function RegCreateKey(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG; external 'advapi32' name 'RegCreateKeyA';

  function RegCreateKeyEx(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD;
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG; external 'advapi32' name 'RegCreateKeyExA';

  function RegDeleteKey(hKey:HKEY; lpSubKey:LPCSTR):LONG; external 'advapi32' name 'RegDeleteKeyA';

  function RegDeleteValue(hKey:HKEY; lpValueName:LPCSTR):LONG; external 'advapi32' name 'RegDeleteValueA';

  function RegEnumKey(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; cbName:DWORD):LONG; external 'advapi32' name 'RegEnumKeyA';

  function RegEnumKeyEx(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; lpcbName:LPDWORD; lpReserved:LPDWORD;
             lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external 'advapi32' name 'RegEnumKeyExA';

  function RegEnumValue(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD;
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG; external 'advapi32' name 'RegEnumValueA';

  function RegLoadKey(hKey:HKEY; lpSubKey:LPCSTR; lpFile:LPCSTR):LONG; external 'advapi32' name 'RegLoadKeyA';

  function RegOpenKey(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG; external 'advapi32' name 'RegOpenKeyA';

  function RegOpenKeyEx(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; var phkResult:HKEY):LONG; external 'advapi32' name 'RegOpenKeyExA';

  function RegQueryInfoKey(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD;
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external 'advapi32' name 'RegQueryInfoKeyA';

  function RegQueryValue(hKey:HKEY; lpSubKey:LPCSTR; lpValue:LPSTR; lpcbValue:PLONG):LONG; external 'advapi32' name 'RegQueryValueA';

  function RegQueryMultipleValues(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPSTR; ldwTotsize:LPDWORD):LONG; external 'advapi32' name 'RegQueryMultipleValuesA';

  function RegQueryValueEx(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE;
             lpcbData:LPDWORD):LONG; external 'advapi32' name 'RegQueryValueExA';

  function RegReplaceKey(hKey:HKEY; lpSubKey:LPCSTR; lpNewFile:LPCSTR; lpOldFile:LPCSTR):LONG; external 'advapi32' name 'RegReplaceKeyA';

  function RegRestoreKey(hKey:HKEY; lpFile:LPCSTR; dwFlags:DWORD):LONG; external 'advapi32' name 'RegRestoreKeyA';

  function RegSaveKey(hKey:HKEY; lpFile:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external 'advapi32' name 'RegSaveKeyA';

  function RegSetValue(hKey:HKEY; lpSubKey:LPCSTR; dwType:DWORD; lpData:LPCSTR; cbData:DWORD):LONG; external 'advapi32' name 'RegSetValueA';

  function RegSetValueEx(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE;
             cbData:DWORD):LONG; external 'advapi32' name 'RegSetValueExA';

  function RegUnLoadKey(hKey:HKEY; lpSubKey:LPCSTR):LONG; external 'advapi32' name 'RegUnLoadKeyA';

  function InitiateSystemShutdown(lpMachineName:LPSTR; lpMessage:LPSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL; external 'advapi32' name 'InitiateSystemShutdownA';

  function AbortSystemShutdown(lpMachineName:LPSTR):WINBOOL; external 'advapi32' name 'AbortSystemShutdownA';

  function CompareString(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCSTR; cchCount1:longint; lpString2:LPCSTR;
             cchCount2:longint):longint; external 'kernel32' name 'CompareStringA';

  function LCMapString(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR;
             cchDest:longint):longint; external 'kernel32' name 'LCMapStringA';

  function GetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPSTR; cchData:longint):longint; external 'kernel32' name 'GetLocaleInfoA';

  function SetLocaleInfo(Locale:LCID; LCType:LCTYPE; lpLCData:LPCSTR):WINBOOL; external 'kernel32' name 'SetLocaleInfoA';

  function GetTimeFormat(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCSTR; lpTimeStr:LPSTR;
             cchTime:longint):longint; external 'kernel32' name 'GetTimeFormatA';

  function GetDateFormat(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCSTR; lpDateStr:LPSTR;
             cchDate:longint):longint; external 'kernel32' name 'GetDateFormatA';

  function GetNumberFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPSTR;
             cchNumber:longint):longint; external 'kernel32' name 'GetNumberFormatA';

  function GetCurrencyFormat(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPSTR;
             cchCurrency:longint):longint; external 'kernel32' name 'GetCurrencyFormatA';

  function EnumCalendarInfo(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL; external 'kernel32' name 'EnumCalendarInfoA';

  function EnumTimeFormats(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumTimeFormatsA';

  function EnumDateFormats(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumDateFormatsA';

  function GetStringTypeEx(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32' name 'GetStringTypeExA';

  function GetStringType(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32' name 'GetStringTypeA';

  function FoldString(dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; cchDest:longint):longint; external 'kernel32' name 'FoldStringA';

  function EnumSystemLocales(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumSystemLocalesA';

  function EnumSystemCodePages(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumSystemCodePagesA';

  function PeekConsoleInput(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL; external 'kernel32' name 'PeekConsoleInputA';

  function ReadConsoleInput(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL; external 'kernel32' name 'ReadConsoleInputA';

  function WriteConsoleInput(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsWritten:DWORD):WINBOOL; external 'kernel32' name 'WriteConsoleInputA';

  function ReadConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL; external 'kernel32' name 'ReadConsoleOutputA';

  function WriteConsoleOutput(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL; external 'kernel32' name 'WriteConsoleOutputA';

  function ReadConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadConsoleOutputCharacterA';

  function WriteConsoleOutputCharacter(hConsoleOutput:HANDLE; lpCharacter:LPCSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteConsoleOutputCharacterA';

  function FillConsoleOutputCharacter(hConsoleOutput:HANDLE; cCharacter:CHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'FillConsoleOutputCharacterA';

  function ScrollConsoleScreenBuffer(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL; external 'kernel32' name 'ScrollConsoleScreenBufferA';

  function GetConsoleTitle(lpConsoleTitle:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetConsoleTitleA';

  function SetConsoleTitle(lpConsoleTitle:LPCSTR):WINBOOL; external 'kernel32' name 'SetConsoleTitleA';

  function ReadConsole(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32' name 'ReadConsoleA';

  function WriteConsole(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32' name 'WriteConsoleA';

  function WNetAddConnection(lpRemoteName:LPCSTR; lpPassword:LPCSTR; lpLocalName:LPCSTR):DWORD; external 'mpr' name 'WNetAddConnectionA';

  function WNetAddConnection2(lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD; external 'mpr' name 'WNetAddConnection2A';

  function WNetAddConnection3(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD; external 'mpr' name 'WNetAddConnection3A';

  function WNetCancelConnection(lpName:LPCSTR; fForce:WINBOOL):DWORD; external 'mpr' name 'WNetCancelConnectionA';

  function WNetCancelConnection2(lpName:LPCSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD; external 'mpr' name 'WNetCancelConnection2A';

  function WNetGetConnection(lpLocalName:LPCSTR; lpRemoteName:LPSTR; lpnLength:LPDWORD):DWORD; external 'mpr' name 'WNetGetConnectionA';

  function WNetUseConnection(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCSTR; lpPassword:LPCSTR; dwFlags:DWORD;
             lpAccessName:LPSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD; external 'mpr' name 'WNetUseConnectionA';

  function WNetSetConnection(lpName:LPCSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD; external 'mpr' name 'WNetSetConnectionA';

  function WNetConnectionDialog1(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD; external 'mpr' name 'WNetConnectionDialog1A';

  function WNetDisconnectDialog1(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD; external 'mpr' name 'WNetDisconnectDialog1A';

  function WNetOpenEnum(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD; external 'mpr' name 'WNetOpenEnumA';

  function WNetEnumResource(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetEnumResourceA';

  function WNetGetUniversalName(lpLocalPath:LPCSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetGetUniversalNameA';

  function WNetGetUser(lpName:LPCSTR; lpUserName:LPSTR; lpnLength:LPDWORD):DWORD; external 'mpr' name 'WNetGetUserA';

  function WNetGetProviderName(dwNetType:DWORD; lpProviderName:LPSTR; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetGetProviderNameA';

  function WNetGetNetworkInformation(lpProvider:LPCSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD; external 'mpr' name 'WNetGetNetworkInformationA';

  function WNetGetLastError(lpError:LPDWORD; lpErrorBuf:LPSTR; nErrorBufSize:DWORD; lpNameBuf:LPSTR; nNameBufSize:DWORD):DWORD; external 'mpr' name 'WNetGetLastErrorA';

  function MultinetGetConnectionPerformance(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD; external 'mpr' name 'MultinetGetConnectionPerformanceA';

  function ChangeServiceConfig(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR;
             lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR;
             lpDisplayName:LPCSTR):WINBOOL; external 'advapi32' name 'ChangeServiceConfigA';

  function CreateService(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPCSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD;
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD;
             lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR):SC_HANDLE; external 'advapi32' name 'CreateServiceA';

  function EnumDependentServices(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD;
             lpServicesReturned:LPDWORD):WINBOOL; external 'advapi32' name 'EnumDependentServicesA';

  function EnumServicesStatus(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD;
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL; external 'advapi32' name 'EnumServicesStatusA';

  function GetServiceKeyName(hSCManager:SC_HANDLE; lpDisplayName:LPCSTR; lpServiceName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32' name 'GetServiceKeyNameA';

  function GetServiceDisplayName(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32' name 'GetServiceDisplayNameA';

  function OpenSCManager(lpMachineName:LPCSTR; lpDatabaseName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32' name 'OpenSCManagerA';

  function OpenService(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32' name 'OpenServiceA';

  function QueryServiceConfig(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'QueryServiceConfigA';

  function QueryServiceLockStatus(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'QueryServiceLockStatusA';

  function RegisterServiceCtrlHandler(lpServiceName:LPCSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE; external 'advapi32' name 'RegisterServiceCtrlHandlerA';

  function StartServiceCtrlDispatcher(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL; external 'advapi32' name 'StartServiceCtrlDispatcherA';

  function StartService(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCSTR):WINBOOL; external 'advapi32' name 'StartServiceA';

  function DragQueryFile(_para1:HDROP; _para2:cardinal; _para3:Pchar;
    _para4:cardinal):cardinal; external 'shell32' name 'DragQueryFileA';

  function ExtractAssociatedIcon(_para1:HINST; _para2:Pchar;
    var _para3:WORD):HICON; external 'shell32' name 'ExtractAssociatedIconA';

  function ExtractIcon(_para1:HINST; _para2:Pchar; _para3:cardinal):HICON;
    external 'shell32' name 'ExtractIconA';

  function FindExecutable(_para1:pchar; _para2:pchar;
    _para3:pchar):HINST; external 'shell32' name 'FindExecutableA';

  function ShellAbout(_para1:HWND; _para2:pchar; _para3:pchar;
    _para4:HICON):longint; external 'shell32' name 'ShellAboutA';

  function ShellExecute(_para1:HWND; _para2:pchar; _para3:pchar;
    _para4:pchar; _para5:pchar;
    _para6:longint):HINST; external 'shell32' name 'ShellExecuteA';

  function DdeCreateStringHandle(_para1:DWORD; _para2:pchar;
    _para3:longint):HSZ; external 'user32' name 'DdeCreateStringHandleA';

  function DdeInitialize(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT;
    external 'user32' name 'DdeInitializeA';

  function DdeQueryString(_para1:DWORD; _para2:HSZ; _para3:pchar;
    _para4:DWORD; _para5:longint):DWORD; external 'user32' name 'DdeQueryStringA';

  function LogonUser(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:DWORD; _para5:DWORD;
             var _para6:HANDLE):WINBOOL; external 'advapi32' name 'LogonUserA';

  function CreateProcessAsUser(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES;
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCTSTR; var _para10:STARTUPINFO;
             var _para11:PROCESS_INFORMATION):WINBOOL; external 'advapi32' name 'CreateProcessAsUserA';

{$endif read_implementation}

{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.5  2000-01-07 16:41:53  daniel
    * copyright 2000

  Revision 1.4  1999/11/21 01:39:26  pierre
   * used TWNDCLASS in interface and WNDCLASS in implementation

  Revision 1.3  1999/11/05 09:53:02  pierre
   + some overloaded functions for Delphi compatibility

  Revision 1.2  1999/09/28 22:29:59  peter
    * fixed createdc to be D4 compatible

  Revision 1.1  1999/09/16 13:38:22  peter
    * windows unit include moved to wininc/

  Revision 1.11  1999/07/06 22:44:10  florian
    * some fixes to compile ddraw units from the jedi project

  Revision 1.10  1999/07/05 14:47:43  florian
    * some more functions fixed to get them work

  Revision 1.9  1999/05/10 19:34:09  florian
    * moved all opengl32.dll stuff to a newly created opengl32 unit, so
      win32 programs should also run on Windows without opengl32.dll

  Revision 1.8  1999/05/01 12:27:50  peter
    * fixed conflicting declarations

  Revision 1.7  1999/04/20 11:36:08  peter
    * compatibility fixes

  Revision 1.6  1999/01/07 15:52:23  peter
    * removed winspool requirement

  Revision 1.5  1998/10/27 11:17:09  peter
    * type HINSTANCE -> HINST

  Revision 1.4  1998/09/04 17:17:31  pierre
    + all unknown function ifdef with
      conditionnal unknown_functions
      testwin works now, but windowcreate still fails !!

  Revision 1.3  1998/09/03 18:17:31  pierre
    * small improvements in number of found functions
      all remaining are in func.pp

  Revision 1.2  1998/09/03 17:14:51  pierre
    * most functions found in main DLL's
      still some missing
      use 'make dllnames' to get missing names

  Revision 1.1  1998/08/31 11:53:53  pierre
    * compilable windows.pp file
      still to do :
       - findout problems
       - findout the correct DLL for each call !!

}
