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

unit ascfun;
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

  function GetBinaryTypeA(lpApplicationName:LPCSTR; lpBinaryType:LPDWORD):WINBOOL;

  function GetShortPathNameA(lpszLongPath:LPCSTR; lpszShortPath:LPSTR; cchBuffer:DWORD):DWORD;

  function GetEnvironmentStringsA:LPSTR;

  function FreeEnvironmentStringsA(_para1:LPSTR):WINBOOL;

  function FormatMessageA(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPSTR;
             nSize:DWORD; var Arguments:va_list):DWORD;

  function CreateMailslotA(lpName:LPCSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function lstrcmpA(lpString1:LPCSTR; lpString2:LPCSTR):longint;

  function lstrcmpiA(lpString1:LPCSTR; lpString2:LPCSTR):longint;

  function lstrcpynA(lpString1:LPSTR; lpString2:LPCSTR; iMaxLength:longint):LPSTR;

  function lstrcpyA(lpString1:LPSTR; lpString2:LPCSTR):LPSTR;

  function lstrcatA(lpString1:LPSTR; lpString2:LPCSTR):LPSTR;

  function lstrlenA(lpString:LPCSTR):longint;

  function CreateMutexA(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCSTR):HANDLE;

  function OpenMutexA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function CreateEventA(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCSTR):HANDLE;

  function OpenEventA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function CreateSemaphoreA(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCSTR):HANDLE;

  function OpenSemaphoreA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function CreateFileMappingA(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD;
             lpName:LPCSTR):HANDLE;

  function OpenFileMappingA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE;

  function GetLogicalDriveStringsA(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD;

  function LoadLibraryA(lpLibFileName:LPCSTR):HINST;

  function LoadLibraryExA(lpLibFileName:LPCSTR; hFile:HANDLE; dwFlags:DWORD):HINST;

  function GetModuleFileNameA(hModule:HINST; lpFilename:LPSTR; nSize:DWORD):DWORD;

  function GetModuleHandleA(lpModuleName:LPCSTR):HMODULE;

  procedure FatalAppExitA(uAction:UINT; lpMessageText:LPCSTR);

  function GetCommandLineA:LPSTR;

  function GetEnvironmentVariableA(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD;

  function SetEnvironmentVariableA(lpName:LPCSTR; lpValue:LPCSTR):WINBOOL;

  function ExpandEnvironmentStringsA(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):DWORD;

  procedure OutputDebugStringA(lpOutputString:LPCSTR);

  function FindResourceA(hModule:HINST; lpName:LPCSTR; lpType:LPCSTR):HRSRC;

  function FindResourceExA(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD):HRSRC;

  function EnumResourceTypesA(hModule:HINST; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL;

  function EnumResourceNamesA(hModule:HINST; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL;

  function EnumResourceLanguagesA(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL;

  function BeginUpdateResourceA(pFileName:LPCSTR; bDeleteExistingResources:WINBOOL):HANDLE;

  function UpdateResourceA(hUpdate:HANDLE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD; lpData:LPVOID;
             cbData:DWORD):WINBOOL;

  function EndUpdateResourceA(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL;

  function GlobalAddAtomA(lpString:LPCSTR):ATOM;

  function GlobalFindAtomA(lpString:LPCSTR):ATOM;

  function GlobalGetAtomNameA(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT;

  function AddAtomA(lpString:LPCSTR):ATOM;

  function FindAtomA(lpString:LPCSTR):ATOM;

  function GetAtomNameA(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT;

  function GetProfileIntA(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT):UINT;

  function GetProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD;

  function WriteProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR):WINBOOL;

  function GetProfileSectionA(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD;

  function WriteProfileSectionA(lpAppName:LPCSTR; lpString:LPCSTR):WINBOOL;

  function GetPrivateProfileIntA(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT; lpFileName:LPCSTR):UINT;

  function GetPrivateProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD;
             lpFileName:LPCSTR):DWORD;

  function WritePrivateProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL;

  function GetPrivateProfileSectionA(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; lpFileName:LPCSTR):DWORD;

  function WritePrivateProfileSectionA(lpAppName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL;

  function GetDriveTypeA(lpRootPathName:LPCSTR):UINT;

  function GetSystemDirectoryA(lpBuffer:LPSTR; uSize:UINT):UINT;

  function GetTempPathA(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD;

  function GetTempFileNameA(lpPathName:LPCSTR; lpPrefixString:LPCSTR; uUnique:UINT; lpTempFileName:LPSTR):UINT;

  function GetWindowsDirectoryA(lpBuffer:LPSTR; uSize:UINT):UINT;

  function SetCurrentDirectoryA(lpPathName:LPCSTR):WINBOOL;

  function GetCurrentDirectoryA(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD;

  function GetDiskFreeSpaceA(lpRootPathName:LPCSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL;

  function CreateDirectoryA(lpPathName:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function CreateDirectoryExA(lpTemplateDirectory:LPCSTR; lpNewDirectory:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL;

  function RemoveDirectoryA(lpPathName:LPCSTR):WINBOOL;

  function GetFullPathNameA(lpFileName:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; var lpFilePart:LPSTR):DWORD;

  function DefineDosDeviceA(dwFlags:DWORD; lpDeviceName:LPCSTR; lpTargetPath:LPCSTR):WINBOOL;

  function QueryDosDeviceA(lpDeviceName:LPCSTR; lpTargetPath:LPSTR; ucchMax:DWORD):DWORD;

  function CreateFileA(lpFileName:LPCSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD;
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE;

  function SetFileAttributesA(lpFileName:LPCSTR; dwFileAttributes:DWORD):WINBOOL;

  function GetFileAttributesA(lpFileName:LPCSTR):DWORD;

  function GetCompressedFileSizeA(lpFileName:LPCSTR; lpFileSizeHigh:LPDWORD):DWORD;

  function DeleteFileA(lpFileName:LPCSTR):WINBOOL;

  function SearchPathA(lpPath:LPCSTR; lpFileName:LPCSTR; lpExtension:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR;
             var lpFilePart:LPSTR):DWORD;

  function CopyFileA(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; bFailIfExists:WINBOOL):WINBOOL;

  function MoveFileA(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR):WINBOOL;

  function MoveFileExA(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; dwFlags:DWORD):WINBOOL;

  function CreateNamedPipeA(lpName:LPCSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD;
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE;

  function GetNamedPipeHandleStateA(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD;
             lpUserName:LPSTR; nMaxUserNameSize:DWORD):WINBOOL;

  function CallNamedPipeA(lpNamedPipeName:LPCSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL;

  function WaitNamedPipeA(lpNamedPipeName:LPCSTR; nTimeOut:DWORD):WINBOOL;

  function SetVolumeLabelA(lpRootPathName:LPCSTR; lpVolumeName:LPCSTR):WINBOOL;

  function GetVolumeInformationA(lpRootPathName:LPCSTR; lpVolumeNameBuffer:LPSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD;
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPSTR; nFileSystemNameSize:DWORD):WINBOOL;

  function ClearEventLogA(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL;

  function BackupEventLogA(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL;

  function OpenEventLogA(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE;

  function RegisterEventSourceA(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE;

  function OpenBackupEventLogA(lpUNCServerName:LPCSTR; lpFileName:LPCSTR):HANDLE;

  function ReadEventLogA(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD;
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL;

  function ReportEventA(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID;
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCSTR; lpRawData:LPVOID):WINBOOL;

  function AccessCheckAndAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR;
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL;
             pfGenerateOnClose:LPBOOL):WINBOOL;

  function ObjectOpenAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR;
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL;
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL;

  function ObjectPrivilegeAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET;
             AccessGranted:WINBOOL):WINBOOL;

  function ObjectCloseAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL;

  function PrivilegedServiceAuditAlarmA(SubsystemName:LPCSTR; ServiceName:LPCSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL;

  function SetFileSecurityA(lpFileName:LPCSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function GetFileSecurityA(lpFileName:LPCSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function FindFirstChangeNotificationA(lpPathName:LPCSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE;

  function IsBadStringPtrA(lpsz:LPCSTR; ucchMax:UINT):WINBOOL;

  function LookupAccountSidA(lpSystemName:LPCSTR; Sid:PSID; Name:LPSTR; cbName:LPDWORD; ReferencedDomainName:LPSTR;
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupAccountNameA(lpSystemName:LPCSTR; lpAccountName:LPCSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPSTR;
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL;

  function LookupPrivilegeValueA(lpSystemName:LPCSTR; lpName:LPCSTR; lpLuid:PLUID):WINBOOL;

  function LookupPrivilegeNameA(lpSystemName:LPCSTR; lpLuid:PLUID; lpName:LPSTR; cbName:LPDWORD):WINBOOL;

  function LookupPrivilegeDisplayNameA(lpSystemName:LPCSTR; lpName:LPCSTR; lpDisplayName:LPSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL;

  function BuildCommDCBA(lpDef:LPCSTR; lpDCB:LPDCB):WINBOOL;

  function BuildCommDCBAndTimeoutsA(lpDef:LPCSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL;

  function CommConfigDialogA(lpszName:LPCSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL;

  function GetDefaultCommConfigA(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL;

  function SetDefaultCommConfigA(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL;

  function GetComputerNameA(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL;

  function SetComputerNameA(lpComputerName:LPCSTR):WINBOOL;

  function GetUserNameA(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL;

  function wvsprintfA(_para1:LPSTR; _para2:LPCSTR; arglist:va_list):longint;

  function LoadKeyboardLayoutA(pwszKLID:LPCSTR; Flags:UINT):HKL;

  function GetKeyboardLayoutNameA(pwszKLID:LPSTR):WINBOOL;

  function CreateDesktopA(lpszDesktop:LPSTR; lpszDevice:LPSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD;
             lpsa:LPSECURITY_ATTRIBUTES):HDESK;

  function OpenDesktopA(lpszDesktop:LPSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK;

  function EnumDesktopsA(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL;

  function CreateWindowStationA(lpwinsta:LPSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA;

  function OpenWindowStationA(lpszWinSta:LPSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA;

  function EnumWindowStationsA(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL;

  function GetUserObjectInformationA(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function SetUserObjectInformationA(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL;

  function RegisterWindowMessageA(lpString:LPCSTR):UINT;

  function GetMessageA(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL;

  function DispatchMessageA(var lpMsg:MSG):LONG;

  function PeekMessageA(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL;
  { delphi compatibility }
  function PeekMessageA(var Msg:TMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL;

  function SendMessageA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function SendMessageTimeoutA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT;
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT;

  function SendNotifyMessageA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function SendMessageCallbackA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC;
             dwData:DWORD):WINBOOL;

  function PostMessageA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function PostThreadMessageA(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL;

  function DefWindowProcA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallWindowProcA(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function RegisterClassA(var lpWndClass:WNDCLASS):ATOM;

  function UnregisterClassA(lpClassName:LPCSTR; hInstance:HINST):WINBOOL;

  function GetClassInfoA(hInstance:HINST; lpClassName:LPCSTR; lpWndClass:LPWNDCLASS):WINBOOL;

  function RegisterClassExA(var _para1:WNDCLASSEX):ATOM;

  function GetClassInfoExA(_para1:HINST; _para2:LPCSTR; _para3:LPWNDCLASSEX):WINBOOL;

  function CreateWindowExA(dwExStyle:DWORD; lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND;

  function CreateDialogParamA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function CreateDialogIndirectParamA(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND;

  function DialogBoxParamA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function DialogBoxIndirectParamA(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint;

  function SetDlgItemTextA(hDlg:HWND; nIDDlgItem:longint; lpString:LPCSTR):WINBOOL;

  function GetDlgItemTextA(hDlg:HWND; nIDDlgItem:longint; lpString:LPSTR; nMaxCount:longint):UINT;

  function SendDlgItemMessageA(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG;

  function DefDlgProcA(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CallMsgFilterA(lpMsg:LPMSG; nCode:longint):WINBOOL;

  function RegisterClipboardFormatA(lpszFormat:LPCSTR):UINT;

  function GetClipboardFormatNameA(format:UINT; lpszFormatName:LPSTR; cchMaxCount:longint):longint;

  function CharToOemA(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL;

  function OemToCharA(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL;

  function CharToOemBuffA(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL;

  function OemToCharBuffA(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL;

  function CharUpperA(lpsz:LPSTR):LPSTR;

  function CharUpperBuffA(lpsz:LPSTR; cchLength:DWORD):DWORD;

  function CharLowerA(lpsz:LPSTR):LPSTR;

  function CharLowerBuffA(lpsz:LPSTR; cchLength:DWORD):DWORD;

  function CharNextA(lpsz:LPCSTR):LPSTR;

  function CharPrevA(lpszStart:LPCSTR; lpszCurrent:LPCSTR):LPSTR;

  function IsCharAlphaA(ch:CHAR):WINBOOL;

  function IsCharAlphaNumericA(ch:CHAR):WINBOOL;

  function IsCharUpperA(ch:CHAR):WINBOOL;

  function IsCharLowerA(ch:CHAR):WINBOOL;

  function GetKeyNameTextA(lParam:LONG; lpString:LPSTR; nSize:longint):longint;

  function VkKeyScanA(ch:CHAR):SHORT;

  function VkKeyScanExA(ch:CHAR; dwhkl:HKL):SHORT;

  function MapVirtualKeyA(uCode:UINT; uMapType:UINT):UINT;

  function MapVirtualKeyExA(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT;

  function LoadAcceleratorsA(hInstance:HINST; lpTableName:LPCSTR):HACCEL;

  function CreateAcceleratorTableA(_para1:LPACCEL; _para2:longint):HACCEL;

  function CopyAcceleratorTableA(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint;

  function TranslateAcceleratorA(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint;

  function LoadMenuA(hInstance:HINST; lpMenuName:LPCSTR):HMENU;

  function LoadMenuIndirectA(var lpMenuTemplate:MENUTEMPLATE):HMENU;

  function ChangeMenuA(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCSTR; cmdInsert:UINT; flags:UINT):WINBOOL;

  function GetMenuStringA(hMenu:HMENU; uIDItem:UINT; lpString:LPSTR; nMaxCount:longint; uFlag:UINT):longint;

  function InsertMenuA(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL;

  function AppendMenuA(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL;

  function ModifyMenuA(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL;

  function InsertMenuItemA(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function GetMenuItemInfoA(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL;

  function SetMenuItemInfoA(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL;

  function DrawTextA(hDC:HDC; lpString:LPCSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint;

  function DrawTextExA(_para1:HDC; _para2:LPSTR; _para3:longint; _para4:LPRECT; _para5:UINT;
             _para6:LPDRAWTEXTPARAMS):longint;

  function GrayStringA(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint;
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL;

  function DrawStateA(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL;

  function TabbedTextOutA(hDC:HDC; X:longint; Y:longint; lpString:LPCSTR; nCount:longint;
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG;

  function GetTabbedTextExtentA(hDC:HDC; lpString:LPCSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD;

  function SetPropA(hWnd:HWND; lpString:LPCSTR; hData:HANDLE):WINBOOL;

  function GetPropA(hWnd:HWND; lpString:LPCSTR):HANDLE;

  function RemovePropA(hWnd:HWND; lpString:LPCSTR):HANDLE;

  function EnumPropsExA(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint;

  function EnumPropsA(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint;

  function SetWindowTextA(hWnd:HWND; lpString:LPCSTR):WINBOOL;

  function GetWindowTextA(hWnd:HWND; lpString:LPSTR; nMaxCount:longint):longint;

  function GetWindowTextLengthA(hWnd:HWND):longint;

  function MessageBoxA(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT):longint;

  function MessageBoxExA(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT; wLanguageId:WORD):longint;

  function MessageBoxIndirectA(_para1:LPMSGBOXPARAMS):longint;

  function GetWindowLongA(hWnd:HWND; nIndex:longint):LONG;

  function SetWindowLongA(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG;

  function GetClassLongA(hWnd:HWND; nIndex:longint):DWORD;

  function SetClassLongA(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD;

  function FindWindowA(lpClassName:LPCSTR; lpWindowName:LPCSTR):HWND;

  function FindWindowExA(_para1:HWND; _para2:HWND; _para3:LPCSTR; _para4:LPCSTR):HWND;

  function GetClassNameA(hWnd:HWND; lpClassName:LPSTR; nMaxCount:longint):longint;

  function SetWindowsHookExA(idHook:longint; lpfn:HOOKPROC; hmod:HINST; dwThreadId:DWORD):HHOOK;

  function LoadBitmapA(hInstance:HINST; lpBitmapName:LPCSTR):HBITMAP;

  function LoadCursorA(hInstance:HINST; lpCursorName:LPCSTR):HCURSOR;

  function LoadCursorFromFileA(lpFileName:LPCSTR):HCURSOR;

  function LoadIconA(hInstance:HINST; lpIconName:LPCSTR):HICON;

  function LoadImageA(_para1:HINST; _para2:LPCSTR; _para3:UINT; _para4:longint; _para5:longint;
             _para6:UINT):HANDLE;

  function LoadStringA(hInstance:HINST; uID:UINT; lpBuffer:LPSTR; nBufferMax:longint):longint;

  function IsDialogMessageA(hDlg:HWND; lpMsg:LPMSG):WINBOOL;

  function DlgDirListA(hDlg:HWND; lpPathSpec:LPSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint;

  function DlgDirSelectExA(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDListBox:longint):WINBOOL;

  function DlgDirListComboBoxA(hDlg:HWND; lpPathSpec:LPSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint;

  function DlgDirSelectComboBoxExA(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDComboBox:longint):WINBOOL;

  function DefFrameProcA(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function DefMDIChildProcA(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CreateMDIWindowA(lpClassName:LPSTR; lpWindowName:LPSTR; dwStyle:DWORD; X:longint; Y:longint;
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINST; lParam:LPARAM):HWND;

  function WinHelpA(hWndMain:HWND; lpszHelp:LPCSTR; uCommand:UINT; dwData:DWORD):WINBOOL;

  function ChangeDisplaySettingsA(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG;

  function EnumDisplaySettingsA(lpszDeviceName:LPCSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL;

  function SystemParametersInfoA(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL;

  function AddFontResourceA(_para1:LPCSTR):longint;

  function CopyMetaFileA(_para1:HMETAFILE; _para2:LPCSTR):HMETAFILE;

  function CreateFontIndirectA(var _para1:LOGFONT):HFONT;

  function CreateICA(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC;

  function CreateMetaFileA(_para1:LPCSTR):HDC;

  function CreateScalableFontResourceA(_para1:DWORD; _para2:LPCSTR; _para3:LPCSTR; _para4:LPCSTR):WINBOOL;

  function EnumFontFamiliesExA(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint;

  function EnumFontFamiliesA(_para1:HDC; _para2:LPCSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint;

  function EnumFontsA(_para1:HDC; _para2:LPCSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint;

  function GetCharWidthA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidth32A(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL;

  function GetCharWidthFloatA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL;

  function GetCharABCWidthsA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL;

  function GetCharABCWidthsFloatA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL;

  function GetGlyphOutlineA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD;
             _para6:LPVOID; var _para7:MAT2):DWORD;

  function GetMetaFileA(_para1:LPCSTR):HMETAFILE;

  function GetOutlineTextMetricsA(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT;

  function GetTextExtentPointA(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentPoint32A(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL;

  function GetTextExtentExPointA(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPINT;
             _para6:LPINT; _para7:LPSIZE):WINBOOL;

  function GetCharacterPlacementA(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS;
             _para6:DWORD):DWORD;

  function ResetDCA(_para1:HDC; var _para2:DEVMODE):HDC;

  function RemoveFontResourceA(_para1:LPCSTR):WINBOOL;

  function CopyEnhMetaFileA(_para1:HENHMETAFILE; _para2:LPCSTR):HENHMETAFILE;

  function CreateEnhMetaFileA(_para1:HDC; _para2:LPCSTR; var _para3:RECT; _para4:LPCSTR):HDC;

  function GetEnhMetaFileA(_para1:LPCSTR):HENHMETAFILE;

  function GetEnhMetaFileDescriptionA(_para1:HENHMETAFILE; _para2:UINT; _para3:LPSTR):UINT;

  function GetTextMetricsA(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL;

  function StartDocA(_para1:HDC; var _para2:DOCINFO):longint;

  function GetObjectA(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint;

  function TextOutA(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint):WINBOOL;

  function ExtTextOutA(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT;
             _para6:LPCSTR; _para7:UINT; var _para8:INT):WINBOOL;

  function PolyTextOutA(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL;

  function GetTextFaceA(_para1:HDC; _para2:longint; _para3:LPSTR):longint;

  function GetKerningPairsA(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD;

  function CreateColorSpaceA(_para1:LPLOGCOLORSPACE):HCOLORSPACE;

  function GetLogColorSpaceA(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL;

  function GetICMProfileA(_para1:HDC; _para2:DWORD; _para3:LPSTR):WINBOOL;

  function SetICMProfileA(_para1:HDC; _para2:LPSTR):WINBOOL;

  function UpdateICMRegKeyA(_para1:DWORD; _para2:DWORD; _para3:LPSTR; _para4:UINT):WINBOOL;

  function EnumICMProfilesA(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint;

  function PropertySheetA(lppsph:LPCPROPSHEETHEADER):longint;

  function ImageList_LoadImageA(hi:HINST; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF;
             uType:UINT; uFlags:UINT):HIMAGELIST;

  function CreateStatusWindowA(style:LONG; lpszText:LPCSTR; hwndParent:HWND; wID:UINT):HWND;

  procedure DrawStatusTextA(hDC:HDC; lprc:LPRECT; pszText:LPCSTR; uFlags:UINT);

  function GetOpenFileNameA(_para1:LPOPENFILENAME):WINBOOL;

  function GetSaveFileNameA(_para1:LPOPENFILENAME):WINBOOL;

  function GetFileTitleA(_para1:LPCSTR; _para2:LPSTR; _para3:WORD):integer;

  function ChooseColorA(_para1:LPCHOOSECOLOR):WINBOOL;

  function FindTextA(_para1:LPFINDREPLACE):HWND;

  function ReplaceTextA(_para1:LPFINDREPLACE):HWND;

  function ChooseFontA(_para1:LPCHOOSEFONT):WINBOOL;

  function PrintDlgA(_para1:LPPRINTDLG):WINBOOL;

  function PageSetupDlgA(_para1:LPPAGESETUPDLG):WINBOOL;

  function CreateProcessA(lpApplicationName:LPCSTR; lpCommandLine:LPSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL;
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL;

  procedure GetStartupInfoA(lpStartupInfo:LPSTARTUPINFO);

  function FindFirstFileA(lpFileName:LPCSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE;

  function FindNextFileA(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL;

  function GetVersionExA(var VersionInformation:OSVERSIONINFO):WINBOOL;

  { was #define dname(params) def_expr }
  function CreateWindowA(lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function CreateDialogIndirectA(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;

  { was #define dname(params) def_expr }
  function DialogBoxA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  { was #define dname(params) def_expr }
  function DialogBoxIndirectA(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;

  function CreateDCA(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; _para4:pDEVMODE):HDC;

  function VerInstallFileA(uFlags:DWORD; szSrcFileName:LPSTR; szDestFileName:LPSTR; szSrcDir:LPSTR; szDestDir:LPSTR;
             szCurDir:LPSTR; szTmpFile:LPSTR; lpuTmpFileLen:PUINT):DWORD;

  function GetFileVersionInfoSizeA(lptstrFilename:LPSTR; lpdwHandle:LPDWORD):DWORD;

  function GetFileVersionInfoA(lptstrFilename:LPSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL;

  function VerLanguageNameA(wLang:DWORD; szLang:LPSTR; nSize:DWORD):DWORD;

  function VerQueryValueA(pBlock:LPVOID; lpSubBlock:LPSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL;

  function VerFindFileA(uFlags:DWORD; szFileName:LPSTR; szWinDir:LPSTR; szAppDir:LPSTR; szCurDir:LPSTR;
             lpuCurDirLen:PUINT; szDestDir:LPSTR; lpuDestDirLen:PUINT):DWORD;

  function RegConnectRegistryA(lpMachineName:LPSTR; hKey:HKEY; phkResult:PHKEY):LONG;

  function RegCreateKeyA(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG;

  function RegCreateKeyExA(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD;
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG;

  function RegDeleteKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG;

  function RegDeleteValueA(hKey:HKEY; lpValueName:LPCSTR):LONG;

  function RegEnumKeyA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; cbName:DWORD):LONG;

  function RegEnumKeyExA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; lpcbName:LPDWORD; lpReserved:LPDWORD;
             lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function RegEnumValueA(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD;
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG;

  function RegLoadKeyA(hKey:HKEY; lpSubKey:LPCSTR; lpFile:LPCSTR):LONG;

  function RegOpenKeyA(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG;

  function RegOpenKeyExA(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM;var phkResult:HKEY):LONG;

  function RegQueryInfoKeyA(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD;
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG;

  function RegQueryValueA(hKey:HKEY; lpSubKey:LPCSTR; lpValue:LPSTR; lpcbValue:PLONG):LONG;

  function RegQueryMultipleValuesA(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPSTR; ldwTotsize:LPDWORD):LONG;

  function RegQueryValueExA(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE;
             lpcbData:LPDWORD):LONG;

  function RegReplaceKeyA(hKey:HKEY; lpSubKey:LPCSTR; lpNewFile:LPCSTR; lpOldFile:LPCSTR):LONG;

  function RegRestoreKeyA(hKey:HKEY; lpFile:LPCSTR; dwFlags:DWORD):LONG;

  function RegSaveKeyA(hKey:HKEY; lpFile:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG;

  function RegSetValueA(hKey:HKEY; lpSubKey:LPCSTR; dwType:DWORD; lpData:LPCSTR; cbData:DWORD):LONG;

  function RegSetValueExA(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE;
             cbData:DWORD):LONG;

  function RegUnLoadKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG;

  function InitiateSystemShutdownA(lpMachineName:LPSTR; lpMessage:LPSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL;

  function AbortSystemShutdownA(lpMachineName:LPSTR):WINBOOL;

  function CompareStringA(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCSTR; cchCount1:longint; lpString2:LPCSTR;
             cchCount2:longint):longint;

  function LCMapStringA(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR;
             cchDest:longint):longint;

  function GetLocaleInfoA(Locale:LCID; LCType:LCTYPE; lpLCData:LPSTR; cchData:longint):longint;

  function SetLocaleInfoA(Locale:LCID; LCType:LCTYPE; lpLCData:LPCSTR):WINBOOL;

  function GetTimeFormatA(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCSTR; lpTimeStr:LPSTR;
             cchTime:longint):longint;

  function GetDateFormatA(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCSTR; lpDateStr:LPSTR;
             cchDate:longint):longint;

  function GetNumberFormatA(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPSTR;
             cchNumber:longint):longint;

  function GetCurrencyFormatA(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPSTR;
             cchCurrency:longint):longint;

  function EnumCalendarInfoA(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL;

  function EnumTimeFormatsA(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function EnumDateFormatsA(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL;

  function GetStringTypeExA(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function GetStringTypeA(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL;

  function FoldStringA(dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; cchDest:longint):longint;

  function EnumSystemLocalesA(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function EnumSystemCodePagesA(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL;

  function PeekConsoleInputA(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL;

  function ReadConsoleInputA(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL;

  function WriteConsoleInputA(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsWritten:DWORD):WINBOOL;

  function ReadConsoleOutputA(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL;

  function WriteConsoleOutputA(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL;

  function ReadConsoleOutputCharacterA(hConsoleOutput:HANDLE; lpCharacter:LPSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL;

  function WriteConsoleOutputCharacterA(hConsoleOutput:HANDLE; lpCharacter:LPCSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function FillConsoleOutputCharacterA(hConsoleOutput:HANDLE; cCharacter:CHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL;

  function ScrollConsoleScreenBufferA(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL;

  function GetConsoleTitleA(lpConsoleTitle:LPSTR; nSize:DWORD):DWORD;

  function SetConsoleTitleA(lpConsoleTitle:LPCSTR):WINBOOL;

  function ReadConsoleA(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL;

  function WriteConsoleA(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL;

  function WNetAddConnectionA(lpRemoteName:LPCSTR; lpPassword:LPCSTR; lpLocalName:LPCSTR):DWORD;

  function WNetAddConnection2A(lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD;

  function WNetAddConnection3A(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD;

  function WNetCancelConnectionA(lpName:LPCSTR; fForce:WINBOOL):DWORD;

  function WNetCancelConnection2A(lpName:LPCSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD;

  function WNetGetConnectionA(lpLocalName:LPCSTR; lpRemoteName:LPSTR; lpnLength:LPDWORD):DWORD;

  function WNetUseConnectionA(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCSTR; lpPassword:LPCSTR; dwFlags:DWORD;
             lpAccessName:LPSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD;

  function WNetSetConnectionA(lpName:LPCSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD;

  function WNetConnectionDialog1A(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD;

  function WNetDisconnectDialog1A(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD;

  function WNetOpenEnumA(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD;

  function WNetEnumResourceA(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUniversalNameA(lpLocalPath:LPCSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD;

  function WNetGetUserA(lpName:LPCSTR; lpUserName:LPSTR; lpnLength:LPDWORD):DWORD;

  function WNetGetProviderNameA(dwNetType:DWORD; lpProviderName:LPSTR; lpBufferSize:LPDWORD):DWORD;

  function WNetGetNetworkInformationA(lpProvider:LPCSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD;

  function WNetGetLastErrorA(lpError:LPDWORD; lpErrorBuf:LPSTR; nErrorBufSize:DWORD; lpNameBuf:LPSTR; nNameBufSize:DWORD):DWORD;

  function MultinetGetConnectionPerformanceA(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD;

  function ChangeServiceConfigA(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR;
             lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR;
             lpDisplayName:LPCSTR):WINBOOL;

  function CreateServiceA(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPCSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD;
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD;
             lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR):SC_HANDLE;

  function EnumDependentServicesA(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD;
             lpServicesReturned:LPDWORD):WINBOOL;

  function EnumServicesStatusA(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD;
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL;

  function GetServiceKeyNameA(hSCManager:SC_HANDLE; lpDisplayName:LPCSTR; lpServiceName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function GetServiceDisplayNameA(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL;

  function OpenSCManagerA(lpMachineName:LPCSTR; lpDatabaseName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function OpenServiceA(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE;

  function QueryServiceConfigA(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function QueryServiceLockStatusA(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

  function RegisterServiceCtrlHandlerA(lpServiceName:LPCSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE;

  function StartServiceCtrlDispatcherA(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL;

  function StartServiceA(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCSTR):WINBOOL;

  function DragQueryFileA(_para1:HDROP; _para2:cardinal; _para3:pchar;
    _para4:cardinal):cardinal;

  function ExtractAssociatedIconA(_para1:HINST;_para2:pchar; var _para3:WORD):HICON;

  function ExtractIconA(_para1:HINST;_para2:pchar; _para3:cardinal):HICON;

  function FindExecutableA(_para1:pchar;_para2:pchar; _para3:pchar):HINST;

  function ShellAboutA(_para1:HWND; _para2:pchar;
    _para3:pchar; _para4:HICON):longint;

  function ShellExecuteA(_para1:HWND; _para2:pchar; _para3:pchar;
    _para4:pchar; _para5:pchar;_para6:longint):HINST;

  function DdeCreateStringHandleA(_para1:DWORD; _para2:pchar;
    _para3:longint):HSZ;

  function DdeInitializeA(var _para1:DWORD; _para2:CALLB;
    _para3:DWORD; _para4:DWORD):UINT;

  function DdeQueryStringA(_para1:DWORD; _para2:HSZ; _para3:pchar;
    _para4:DWORD; _para5:longint):DWORD;

  function LogonUserA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:DWORD; _para5:DWORD;
             var _para6:HANDLE):WINBOOL;

  function CreateProcessAsUserA(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES;
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCTSTR; var _para10:STARTUPINFO;
             var _para11:PROCESS_INFORMATION):WINBOOL;

{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

  function GetBinaryTypeA(lpApplicationName:LPCSTR; lpBinaryType:LPDWORD):WINBOOL; external 'kernel32' name 'GetBinaryTypeA';

  function GetShortPathNameA(lpszLongPath:LPCSTR; lpszShortPath:LPSTR; cchBuffer:DWORD):DWORD; external 'kernel32' name 'GetShortPathNameA';

  function GetEnvironmentStringsA:LPSTR; external 'kernel32' name 'GetEnvironmentStringsA';

  function FreeEnvironmentStringsA(_para1:LPSTR):WINBOOL; external 'kernel32' name 'FreeEnvironmentStringsA';

  function FormatMessageA(dwFlags:DWORD; lpSource:LPCVOID; dwMessageId:DWORD; dwLanguageId:DWORD; lpBuffer:LPSTR;
             nSize:DWORD; var Arguments:va_list):DWORD; external 'kernel32' name 'FormatMessageA';

  function CreateMailslotA(lpName:LPCSTR; nMaxMessageSize:DWORD; lReadTimeout:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32' name 'CreateMailslotA';

  function lstrcmpA(lpString1:LPCSTR; lpString2:LPCSTR):longint; external 'kernel32' name 'lstrcmpA';

  function lstrcmpiA(lpString1:LPCSTR; lpString2:LPCSTR):longint; external 'kernel32' name 'lstrcmpiA';

  function lstrcpynA(lpString1:LPSTR; lpString2:LPCSTR; iMaxLength:longint):LPSTR; external 'kernel32' name 'lstrcpynA';

  function lstrcpyA(lpString1:LPSTR; lpString2:LPCSTR):LPSTR; external 'kernel32' name 'lstrcpyA';

  function lstrcatA(lpString1:LPSTR; lpString2:LPCSTR):LPSTR; external 'kernel32' name 'lstrcatA';

  function lstrlenA(lpString:LPCSTR):longint; external 'kernel32' name 'lstrlenA';

  function CreateMutexA(lpMutexAttributes:LPSECURITY_ATTRIBUTES; bInitialOwner:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateMutexA';

  function OpenMutexA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenMutexA';

  function CreateEventA(lpEventAttributes:LPSECURITY_ATTRIBUTES; bManualReset:WINBOOL; bInitialState:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateEventA';

  function OpenEventA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenEventA';

  function CreateSemaphoreA(lpSemaphoreAttributes:LPSECURITY_ATTRIBUTES; lInitialCount:LONG; lMaximumCount:LONG; lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateSemaphoreA';

  function OpenSemaphoreA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenSemaphoreA';

  function CreateFileMappingA(hFile:HANDLE; lpFileMappingAttributes:LPSECURITY_ATTRIBUTES; flProtect:DWORD; dwMaximumSizeHigh:DWORD; dwMaximumSizeLow:DWORD;
             lpName:LPCSTR):HANDLE; external 'kernel32' name 'CreateFileMappingA';

  function OpenFileMappingA(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; lpName:LPCSTR):HANDLE; external 'kernel32' name 'OpenFileMappingA';

  function GetLogicalDriveStringsA(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external 'kernel32' name 'GetLogicalDriveStringsA';

  function LoadLibraryA(lpLibFileName:LPCSTR):HINST; external 'kernel32' name 'LoadLibraryA';

  function LoadLibraryExA(lpLibFileName:LPCSTR; hFile:HANDLE; dwFlags:DWORD):HINST; external 'kernel32' name 'LoadLibraryExA';

  function GetModuleFileNameA(hModule:HINST; lpFilename:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetModuleFileNameA';

  function GetModuleHandleA(lpModuleName:LPCSTR):HMODULE; external 'kernel32' name 'GetModuleHandleA';

  procedure FatalAppExitA(uAction:UINT; lpMessageText:LPCSTR); external 'kernel32' name 'FatalAppExitA';

  function GetCommandLineA:LPSTR; external 'kernel32' name 'GetCommandLineA';

  function GetEnvironmentVariableA(lpName:LPCSTR; lpBuffer:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetEnvironmentVariableA';

  function SetEnvironmentVariableA(lpName:LPCSTR; lpValue:LPCSTR):WINBOOL; external 'kernel32' name 'SetEnvironmentVariableA';

  function ExpandEnvironmentStringsA(lpSrc:LPCSTR; lpDst:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'ExpandEnvironmentStringsA';

  procedure OutputDebugStringA(lpOutputString:LPCSTR); external 'kernel32' name 'OutputDebugStringA';

  function FindResourceA(hModule:HINST; lpName:LPCSTR; lpType:LPCSTR):HRSRC; external 'kernel32' name 'FindResourceA';

  function FindResourceExA(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD):HRSRC; external 'kernel32' name 'FindResourceExA';

  function EnumResourceTypesA(hModule:HINST; lpEnumFunc:ENUMRESTYPEPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceTypesA';

  function EnumResourceNamesA(hModule:HINST; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceNamesA';

  function EnumResourceLanguagesA(hModule:HINST; lpType:LPCSTR; lpName:LPCSTR; lpEnumFunc:ENUMRESLANGPROC; lParam:LONG):WINBOOL; external 'kernel32' name 'EnumResourceLanguagesA';

  function BeginUpdateResourceA(pFileName:LPCSTR; bDeleteExistingResources:WINBOOL):HANDLE; external 'kernel32' name 'BeginUpdateResourceA';

  function UpdateResourceA(hUpdate:HANDLE; lpType:LPCSTR; lpName:LPCSTR; wLanguage:WORD; lpData:LPVOID;
             cbData:DWORD):WINBOOL; external 'kernel32' name 'UpdateResourceA';

  function EndUpdateResourceA(hUpdate:HANDLE; fDiscard:WINBOOL):WINBOOL; external 'kernel32' name 'EndUpdateResourceA';

  function GlobalAddAtomA(lpString:LPCSTR):ATOM; external 'kernel32' name 'GlobalAddAtomA';

  function GlobalFindAtomA(lpString:LPCSTR):ATOM; external 'kernel32' name 'GlobalFindAtomA';

  function GlobalGetAtomNameA(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT; external 'kernel32' name 'GlobalGetAtomNameA';

  function AddAtomA(lpString:LPCSTR):ATOM; external 'kernel32' name 'AddAtomA';

  function FindAtomA(lpString:LPCSTR):ATOM; external 'kernel32' name 'FindAtomA';

  function GetAtomNameA(nAtom:ATOM; lpBuffer:LPSTR; nSize:longint):UINT; external 'kernel32' name 'GetAtomNameA';

  function GetProfileIntA(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT):UINT; external 'kernel32' name 'GetProfileIntA';

  function GetProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetProfileStringA';

  function WriteProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR):WINBOOL; external 'kernel32' name 'WriteProfileStringA';

  function GetProfileSectionA(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetProfileSectionA';

  function WriteProfileSectionA(lpAppName:LPCSTR; lpString:LPCSTR):WINBOOL; external 'kernel32' name 'WriteProfileSectionA';

  function GetPrivateProfileIntA(lpAppName:LPCSTR; lpKeyName:LPCSTR; nDefault:INT; lpFileName:LPCSTR):UINT; external 'kernel32' name 'GetPrivateProfileIntA';

  function GetPrivateProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpDefault:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD;
             lpFileName:LPCSTR):DWORD; external 'kernel32' name 'GetPrivateProfileStringA';

  function WritePrivateProfileStringA(lpAppName:LPCSTR; lpKeyName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL; external 'kernel32' name 'WritePrivateProfileStringA';

  function GetPrivateProfileSectionA(lpAppName:LPCSTR; lpReturnedString:LPSTR; nSize:DWORD; lpFileName:LPCSTR):DWORD; external 'kernel32' name 'GetPrivateProfileSectionA';

  function WritePrivateProfileSectionA(lpAppName:LPCSTR; lpString:LPCSTR; lpFileName:LPCSTR):WINBOOL; external 'kernel32' name 'WritePrivateProfileSectionA';

  function GetDriveTypeA(lpRootPathName:LPCSTR):UINT; external 'kernel32' name 'GetDriveTypeA';

  function GetSystemDirectoryA(lpBuffer:LPSTR; uSize:UINT):UINT; external 'kernel32' name 'GetSystemDirectoryA';

  function GetTempPathA(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external 'kernel32' name 'GetTempPathA';

  function GetTempFileNameA(lpPathName:LPCSTR; lpPrefixString:LPCSTR; uUnique:UINT; lpTempFileName:LPSTR):UINT; external 'kernel32' name 'GetTempFileNameA';

  function GetWindowsDirectoryA(lpBuffer:LPSTR; uSize:UINT):UINT; external 'kernel32' name 'GetWindowsDirectoryA';

  function SetCurrentDirectoryA(lpPathName:LPCSTR):WINBOOL; external 'kernel32' name 'SetCurrentDirectoryA';

  function GetCurrentDirectoryA(nBufferLength:DWORD; lpBuffer:LPSTR):DWORD; external 'kernel32' name 'GetCurrentDirectoryA';

  function GetDiskFreeSpaceA(lpRootPathName:LPCSTR; lpSectorsPerCluster:LPDWORD; lpBytesPerSector:LPDWORD; lpNumberOfFreeClusters:LPDWORD; lpTotalNumberOfClusters:LPDWORD):WINBOOL; external 'kernel32' name 'GetDiskFreeSpaceA';

  function CreateDirectoryA(lpPathName:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32' name 'CreateDirectoryA';

  function CreateDirectoryExA(lpTemplateDirectory:LPCSTR; lpNewDirectory:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):WINBOOL; external 'kernel32' name 'CreateDirectoryExA';

  function RemoveDirectoryA(lpPathName:LPCSTR):WINBOOL; external 'kernel32' name 'RemoveDirectoryA';

  function GetFullPathNameA(lpFileName:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR; var lpFilePart:LPSTR):DWORD; external 'kernel32' name 'GetFullPathNameA';

  function DefineDosDeviceA(dwFlags:DWORD; lpDeviceName:LPCSTR; lpTargetPath:LPCSTR):WINBOOL; external 'kernel32' name 'DefineDosDeviceA';

  function QueryDosDeviceA(lpDeviceName:LPCSTR; lpTargetPath:LPSTR; ucchMax:DWORD):DWORD; external 'kernel32' name 'QueryDosDeviceA';

  function CreateFileA(lpFileName:LPCSTR; dwDesiredAccess:DWORD; dwShareMode:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; dwCreationDisposition:DWORD;
             dwFlagsAndAttributes:DWORD; hTemplateFile:HANDLE):HANDLE; external 'kernel32' name 'CreateFileA';

  function SetFileAttributesA(lpFileName:LPCSTR; dwFileAttributes:DWORD):WINBOOL; external 'kernel32' name 'SetFileAttributesA';

  function GetFileAttributesA(lpFileName:LPCSTR):DWORD; external 'kernel32' name 'GetFileAttributesA';

  function GetCompressedFileSizeA(lpFileName:LPCSTR; lpFileSizeHigh:LPDWORD):DWORD; external 'kernel32' name 'GetCompressedFileSizeA';

  function DeleteFileA(lpFileName:LPCSTR):WINBOOL; external 'kernel32' name 'DeleteFileA';

  function SearchPathA(lpPath:LPCSTR; lpFileName:LPCSTR; lpExtension:LPCSTR; nBufferLength:DWORD; lpBuffer:LPSTR;
             var lpFilePart:LPSTR):DWORD; external 'kernel32' name 'SearchPathA';

  function CopyFileA(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; bFailIfExists:WINBOOL):WINBOOL; external 'kernel32' name 'CopyFileA';

  function MoveFileA(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR):WINBOOL; external 'kernel32' name 'MoveFileA';

  function MoveFileExA(lpExistingFileName:LPCSTR; lpNewFileName:LPCSTR; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'MoveFileExA';

  function CreateNamedPipeA(lpName:LPCSTR; dwOpenMode:DWORD; dwPipeMode:DWORD; nMaxInstances:DWORD; nOutBufferSize:DWORD;
             nInBufferSize:DWORD; nDefaultTimeOut:DWORD; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):HANDLE; external 'kernel32' name 'CreateNamedPipeA';

  function GetNamedPipeHandleStateA(hNamedPipe:HANDLE; lpState:LPDWORD; lpCurInstances:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD;
             lpUserName:LPSTR; nMaxUserNameSize:DWORD):WINBOOL; external 'kernel32' name 'GetNamedPipeHandleStateA';

  function CallNamedPipeA(lpNamedPipeName:LPCSTR; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;
             lpBytesRead:LPDWORD; nTimeOut:DWORD):WINBOOL; external 'kernel32' name 'CallNamedPipeA';

  function WaitNamedPipeA(lpNamedPipeName:LPCSTR; nTimeOut:DWORD):WINBOOL; external 'kernel32' name 'WaitNamedPipeA';

  function SetVolumeLabelA(lpRootPathName:LPCSTR; lpVolumeName:LPCSTR):WINBOOL; external 'kernel32' name 'SetVolumeLabelA';

  function GetVolumeInformationA(lpRootPathName:LPCSTR; lpVolumeNameBuffer:LPSTR; nVolumeNameSize:DWORD; lpVolumeSerialNumber:LPDWORD; lpMaximumComponentLength:LPDWORD;
             lpFileSystemFlags:LPDWORD; lpFileSystemNameBuffer:LPSTR; nFileSystemNameSize:DWORD):WINBOOL; external 'kernel32' name 'GetVolumeInformationA';

  function ClearEventLogA(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL; external 'advapi32' name 'ClearEventLogA';

  function BackupEventLogA(hEventLog:HANDLE; lpBackupFileName:LPCSTR):WINBOOL; external 'advapi32' name 'BackupEventLogA';

  function OpenEventLogA(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE; external 'advapi32' name 'OpenEventLogA';

  function RegisterEventSourceA(lpUNCServerName:LPCSTR; lpSourceName:LPCSTR):HANDLE; external 'advapi32' name 'RegisterEventSourceA';

  function OpenBackupEventLogA(lpUNCServerName:LPCSTR; lpFileName:LPCSTR):HANDLE; external 'advapi32' name 'OpenBackupEventLogA';

  function ReadEventLogA(hEventLog:HANDLE; dwReadFlags:DWORD; dwRecordOffset:DWORD; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD;
             var pnBytesRead:DWORD; var pnMinNumberOfBytesNeeded:DWORD):WINBOOL; external 'advapi32' name 'ReadEventLogA';

  function ReportEventA(hEventLog:HANDLE; wType:WORD; wCategory:WORD; dwEventID:DWORD; lpUserSid:PSID;
             wNumStrings:WORD; dwDataSize:DWORD; var lpStrings:LPCSTR; lpRawData:LPVOID):WINBOOL; external 'advapi32' name 'ReportEventA';

  function AccessCheckAndAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR;
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL;
             pfGenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheckAndAuditAlarmA';

  function ObjectOpenAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; ObjectTypeName:LPSTR; ObjectName:LPSTR; pSecurityDescriptor:PSECURITY_DESCRIPTOR;
             ClientToken:HANDLE; DesiredAccess:DWORD; GrantedAccess:DWORD; Privileges:PPRIVILEGE_SET; ObjectCreation:WINBOOL;
             AccessGranted:WINBOOL; GenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'ObjectOpenAuditAlarmA';

  function ObjectPrivilegeAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; ClientToken:HANDLE; DesiredAccess:DWORD; Privileges:PPRIVILEGE_SET;
             AccessGranted:WINBOOL):WINBOOL; external 'advapi32' name 'ObjectPrivilegeAuditAlarmA';

  function ObjectCloseAuditAlarmA(SubsystemName:LPCSTR; HandleId:LPVOID; GenerateOnClose:WINBOOL):WINBOOL; external 'advapi32' name 'ObjectCloseAuditAlarmA';

  function PrivilegedServiceAuditAlarmA(SubsystemName:LPCSTR; ServiceName:LPCSTR; ClientToken:HANDLE; Privileges:PPRIVILEGE_SET; AccessGranted:WINBOOL):WINBOOL; external 'advapi32' name 'PrivilegedServiceAuditAlarmA';

  function SetFileSecurityA(lpFileName:LPCSTR; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'SetFileSecurityA';

  function GetFileSecurityA(lpFileName:LPCSTR; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'GetFileSecurityA';

  function FindFirstChangeNotificationA(lpPathName:LPCSTR; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD):HANDLE; external 'kernel32' name 'FindFirstChangeNotificationA';

  function IsBadStringPtrA(lpsz:LPCSTR; ucchMax:UINT):WINBOOL; external 'kernel32' name 'IsBadStringPtrA';

  function LookupAccountSidA(lpSystemName:LPCSTR; Sid:PSID; Name:LPSTR; cbName:LPDWORD; ReferencedDomainName:LPSTR;
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32' name 'LookupAccountSidA';

  function LookupAccountNameA(lpSystemName:LPCSTR; lpAccountName:LPCSTR; Sid:PSID; cbSid:LPDWORD; ReferencedDomainName:LPSTR;
             cbReferencedDomainName:LPDWORD; peUse:PSID_NAME_USE):WINBOOL; external 'advapi32' name 'LookupAccountNameA';

  function LookupPrivilegeValueA(lpSystemName:LPCSTR; lpName:LPCSTR; lpLuid:PLUID):WINBOOL; external 'advapi32' name 'LookupPrivilegeValueA';

  function LookupPrivilegeNameA(lpSystemName:LPCSTR; lpLuid:PLUID; lpName:LPSTR; cbName:LPDWORD):WINBOOL; external 'advapi32' name 'LookupPrivilegeNameA';

  function LookupPrivilegeDisplayNameA(lpSystemName:LPCSTR; lpName:LPCSTR; lpDisplayName:LPSTR; cbDisplayName:LPDWORD; lpLanguageId:LPDWORD):WINBOOL; external 'advapi32' name 'LookupPrivilegeDisplayNameA';

  function BuildCommDCBA(lpDef:LPCSTR; lpDCB:LPDCB):WINBOOL; external 'kernel32' name 'BuildCommDCBA';

  function BuildCommDCBAndTimeoutsA(lpDef:LPCSTR; lpDCB:LPDCB; lpCommTimeouts:LPCOMMTIMEOUTS):WINBOOL; external 'kernel32' name 'BuildCommDCBAndTimeoutsA';

  function CommConfigDialogA(lpszName:LPCSTR; hWnd:HWND; lpCC:LPCOMMCONFIG):WINBOOL; external 'kernel32' name 'CommConfigDialogA';

  function GetDefaultCommConfigA(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetDefaultCommConfigA';

  function SetDefaultCommConfigA(lpszName:LPCSTR; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external 'kernel32' name 'SetDefaultCommConfigA';

  function GetComputerNameA(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetComputerNameA';

  function SetComputerNameA(lpComputerName:LPCSTR):WINBOOL; external 'kernel32' name 'SetComputerNameA';

  function GetUserNameA(lpBuffer:LPSTR; nSize:LPDWORD):WINBOOL; external 'advapi32' name 'GetUserNameA';

  function wvsprintfA(_para1:LPSTR; _para2:LPCSTR; arglist:va_list):longint; external 'user32' name 'wvsprintfA';

  function LoadKeyboardLayoutA(pwszKLID:LPCSTR; Flags:UINT):HKL; external 'user32' name 'LoadKeyboardLayoutA';

  function GetKeyboardLayoutNameA(pwszKLID:LPSTR):WINBOOL; external 'user32' name 'GetKeyboardLayoutNameA';

  function CreateDesktopA(lpszDesktop:LPSTR; lpszDevice:LPSTR; pDevmode:LPDEVMODE; dwFlags:DWORD; dwDesiredAccess:DWORD;
             lpsa:LPSECURITY_ATTRIBUTES):HDESK; external 'user32' name 'CreateDesktopA';

  function OpenDesktopA(lpszDesktop:LPSTR; dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external 'user32' name 'OpenDesktopA';

  function EnumDesktopsA(hwinsta:HWINSTA; lpEnumFunc:DESKTOPENUMPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumDesktopsA';

  function CreateWindowStationA(lpwinsta:LPSTR; dwReserved:DWORD; dwDesiredAccess:DWORD; lpsa:LPSECURITY_ATTRIBUTES):HWINSTA; external 'user32' name 'CreateWindowStationA';

  function OpenWindowStationA(lpszWinSta:LPSTR; fInherit:WINBOOL; dwDesiredAccess:DWORD):HWINSTA; external 'user32' name 'OpenWindowStationA';

  function EnumWindowStationsA(lpEnumFunc:ENUMWINDOWSTATIONPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumWindowStationsA';

  function GetUserObjectInformationA(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'user32' name 'GetUserObjectInformationA';

  function SetUserObjectInformationA(hObj:HANDLE; nIndex:longint; pvInfo:PVOID; nLength:DWORD):WINBOOL; external 'user32' name 'SetUserObjectInformationA';

  function RegisterWindowMessageA(lpString:LPCSTR):UINT; external 'user32' name 'RegisterWindowMessageA';

  function GetMessageA(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT):WINBOOL; external 'user32' name 'GetMessageA';

  function DispatchMessageA(var lpMsg:MSG):LONG; external 'user32' name 'DispatchMessageA';

  function PeekMessageA(lpMsg:LPMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external 'user32' name 'PeekMessageA';
  { delphi compatibility }
  function PeekMessageA(var Msg:TMSG; hWnd:HWND; wMsgFilterMin:UINT; wMsgFilterMax:UINT; wRemoveMsg:UINT):WINBOOL; external 'user32' name 'PeekMessageA';

  function SendMessageA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'SendMessageA';

  function SendMessageTimeoutA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; fuFlags:UINT;
             uTimeout:UINT; lpdwResult:LPDWORD):LRESULT; external 'user32' name 'SendMessageTimeoutA';

  function SendNotifyMessageA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32' name 'SendNotifyMessageA';

  function SendMessageCallbackA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM; lpResultCallBack:SENDASYNCPROC;
             dwData:DWORD):WINBOOL; external 'user32' name 'SendMessageCallbackA';

  function PostMessageA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32' name 'PostMessageA';

  function PostThreadMessageA(idThread:DWORD; Msg:UINT; wParam:WPARAM; lParam:LPARAM):WINBOOL; external 'user32' name 'PostThreadMessageA';

  function DefWindowProcA(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefWindowProcA';

  function CallWindowProcA(lpPrevWndFunc:WNDPROC; hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'CallWindowProcA';

  function RegisterClassA(var lpWndClass:WNDCLASS):ATOM; external 'user32' name 'RegisterClassA';

  function UnregisterClassA(lpClassName:LPCSTR; hInstance:HINST):WINBOOL; external 'user32' name 'UnregisterClassA';

  function GetClassInfoA(hInstance:HINST; lpClassName:LPCSTR; lpWndClass:LPWNDCLASS):WINBOOL; external 'user32' name 'GetClassInfoA';

  function RegisterClassExA(var _para1:WNDCLASSEX):ATOM; external 'user32' name 'RegisterClassExA';

  function GetClassInfoExA(_para1:HINST; _para2:LPCSTR; _para3:LPWNDCLASSEX):WINBOOL; external 'user32' name 'GetClassInfoExA';

  function CreateWindowExA(dwExStyle:DWORD; lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND; external 'user32' name 'CreateWindowExA';

  function CreateDialogParamA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32' name 'CreateDialogParamA';

  function CreateDialogIndirectParamA(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):HWND; external 'user32' name 'CreateDialogIndirectParamA';

  function DialogBoxParamA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32' name 'DialogBoxParamA';

  function DialogBoxIndirectParamA(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC; dwInitParam:LPARAM):longint; external 'user32' name 'DialogBoxIndirectParamA';

  function SetDlgItemTextA(hDlg:HWND; nIDDlgItem:longint; lpString:LPCSTR):WINBOOL; external 'user32' name 'SetDlgItemTextA';

  function GetDlgItemTextA(hDlg:HWND; nIDDlgItem:longint; lpString:LPSTR; nMaxCount:longint):UINT; external 'user32' name 'GetDlgItemTextA';

  function SendDlgItemMessageA(hDlg:HWND; nIDDlgItem:longint; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LONG; external 'user32' name 'SendDlgItemMessageA';

  function DefDlgProcA(hDlg:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefDlgProcA';

  function CallMsgFilterA(lpMsg:LPMSG; nCode:longint):WINBOOL; external 'user32' name 'CallMsgFilterA';

  function RegisterClipboardFormatA(lpszFormat:LPCSTR):UINT; external 'user32' name 'RegisterClipboardFormatA';

  function GetClipboardFormatNameA(format:UINT; lpszFormatName:LPSTR; cchMaxCount:longint):longint; external 'user32' name 'GetClipboardFormatNameA';

  function CharToOemA(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL; external 'user32' name 'CharToOemA';

  function OemToCharA(lpszSrc:LPCSTR; lpszDst:LPSTR):WINBOOL; external 'user32' name 'OemToCharA';

  function CharToOemBuffA(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external 'user32' name 'CharToOemBuffA';

  function OemToCharBuffA(lpszSrc:LPCSTR; lpszDst:LPSTR; cchDstLength:DWORD):WINBOOL; external 'user32' name 'OemToCharBuffA';

  function CharUpperA(lpsz:LPSTR):LPSTR; external 'user32' name 'CharUpperA';

  function CharUpperBuffA(lpsz:LPSTR; cchLength:DWORD):DWORD; external 'user32' name 'CharUpperBuffA';

  function CharLowerA(lpsz:LPSTR):LPSTR; external 'user32' name 'CharLowerA';

  function CharLowerBuffA(lpsz:LPSTR; cchLength:DWORD):DWORD; external 'user32' name 'CharLowerBuffA';

  function CharNextA(lpsz:LPCSTR):LPSTR; external 'user32' name 'CharNextA';

  function CharPrevA(lpszStart:LPCSTR; lpszCurrent:LPCSTR):LPSTR; external 'user32' name 'CharPrevA';

  function IsCharAlphaA(ch:CHAR):WINBOOL; external 'user32' name 'IsCharAlphaA';

  function IsCharAlphaNumericA(ch:CHAR):WINBOOL; external 'user32' name 'IsCharAlphaNumericA';

  function IsCharUpperA(ch:CHAR):WINBOOL; external 'user32' name 'IsCharUpperA';

  function IsCharLowerA(ch:CHAR):WINBOOL; external 'user32' name 'IsCharLowerA';

  function GetKeyNameTextA(lParam:LONG; lpString:LPSTR; nSize:longint):longint; external 'user32' name 'GetKeyNameTextA';

  function VkKeyScanA(ch:CHAR):SHORT; external 'user32' name 'VkKeyScanA';

  function VkKeyScanExA(ch:CHAR; dwhkl:HKL):SHORT; external 'user32' name 'VkKeyScanExA';

  function MapVirtualKeyA(uCode:UINT; uMapType:UINT):UINT; external 'user32' name 'MapVirtualKeyA';

  function MapVirtualKeyExA(uCode:UINT; uMapType:UINT; dwhkl:HKL):UINT; external 'user32' name 'MapVirtualKeyExA';

  function LoadAcceleratorsA(hInstance:HINST; lpTableName:LPCSTR):HACCEL; external 'user32' name 'LoadAcceleratorsA';

  function CreateAcceleratorTableA(_para1:LPACCEL; _para2:longint):HACCEL; external 'user32' name 'CreateAcceleratorTableA';

  function CopyAcceleratorTableA(hAccelSrc:HACCEL; lpAccelDst:LPACCEL; cAccelEntries:longint):longint; external 'user32' name 'CopyAcceleratorTableA';

  function TranslateAcceleratorA(hWnd:HWND; hAccTable:HACCEL; lpMsg:LPMSG):longint; external 'user32' name 'TranslateAcceleratorA';

  function LoadMenuA(hInstance:HINST; lpMenuName:LPCSTR):HMENU; external 'user32' name 'LoadMenuA';

  function LoadMenuIndirectA(var lpMenuTemplate:MENUTEMPLATE):HMENU; external 'user32' name 'LoadMenuIndirectA';

  function ChangeMenuA(hMenu:HMENU; cmd:UINT; lpszNewItem:LPCSTR; cmdInsert:UINT; flags:UINT):WINBOOL; external 'user32' name 'ChangeMenuA';

  function GetMenuStringA(hMenu:HMENU; uIDItem:UINT; lpString:LPSTR; nMaxCount:longint; uFlag:UINT):longint; external 'user32' name 'GetMenuStringA';

  function InsertMenuA(hMenu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external 'user32' name 'InsertMenuA';

  function AppendMenuA(hMenu:HMENU; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external 'user32' name 'AppendMenuA';

  function ModifyMenuA(hMnu:HMENU; uPosition:UINT; uFlags:UINT; uIDNewItem:UINT; lpNewItem:LPCSTR):WINBOOL; external 'user32' name 'ModifyMenuA';

  function InsertMenuItemA(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32' name 'InsertMenuItemA';

  function GetMenuItemInfoA(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPMENUITEMINFO):WINBOOL; external 'user32' name 'GetMenuItemInfoA';

  function SetMenuItemInfoA(_para1:HMENU; _para2:UINT; _para3:WINBOOL; _para4:LPCMENUITEMINFO):WINBOOL; external 'user32' name 'SetMenuItemInfoA';

  function DrawTextA(hDC:HDC; lpString:LPCSTR; nCount:longint; lpRect:LPRECT; uFormat:UINT):longint; external 'user32' name 'DrawTextA';

  function DrawTextExA(_para1:HDC; _para2:LPSTR; _para3:longint; _para4:LPRECT; _para5:UINT;
             _para6:LPDRAWTEXTPARAMS):longint; external 'user32' name 'DrawTextExA';

  function GrayStringA(hDC:HDC; hBrush:HBRUSH; lpOutputFunc:GRAYSTRINGPROC; lpData:LPARAM; nCount:longint;
             X:longint; Y:longint; nWidth:longint; nHeight:longint):WINBOOL; external 'user32' name 'GrayStringA';

  function DrawStateA(_para1:HDC; _para2:HBRUSH; _para3:DRAWSTATEPROC; _para4:LPARAM; _para5:WPARAM;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:UINT):WINBOOL; external 'user32' name 'DrawStateA';

  function TabbedTextOutA(hDC:HDC; X:longint; Y:longint; lpString:LPCSTR; nCount:longint;
             nTabPositions:longint; lpnTabStopPositions:LPINT; nTabOrigin:longint):LONG; external 'user32' name 'TabbedTextOutA';

  function GetTabbedTextExtentA(hDC:HDC; lpString:LPCSTR; nCount:longint; nTabPositions:longint; lpnTabStopPositions:LPINT):DWORD; external 'user32' name 'GetTabbedTextExtentA';

  function SetPropA(hWnd:HWND; lpString:LPCSTR; hData:HANDLE):WINBOOL; external 'user32' name 'SetPropA';

  function GetPropA(hWnd:HWND; lpString:LPCSTR):HANDLE; external 'user32' name 'GetPropA';

  function RemovePropA(hWnd:HWND; lpString:LPCSTR):HANDLE; external 'user32' name 'RemovePropA';

  function EnumPropsExA(hWnd:HWND; lpEnumFunc:PROPENUMPROCEX; lParam:LPARAM):longint; external 'user32' name 'EnumPropsExA';

  function EnumPropsA(hWnd:HWND; lpEnumFunc:PROPENUMPROC):longint; external 'user32' name 'EnumPropsA';

  function SetWindowTextA(hWnd:HWND; lpString:LPCSTR):WINBOOL; external 'user32' name 'SetWindowTextA';

  function GetWindowTextA(hWnd:HWND; lpString:LPSTR; nMaxCount:longint):longint; external 'user32' name 'GetWindowTextA';

  function GetWindowTextLengthA(hWnd:HWND):longint; external 'user32' name 'GetWindowTextLengthA';

  function MessageBoxA(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT):longint; external 'user32' name 'MessageBoxA';

  function MessageBoxExA(hWnd:HWND; lpText:LPCSTR; lpCaption:LPCSTR; uType:UINT; wLanguageId:WORD):longint; external 'user32' name 'MessageBoxExA';

  function MessageBoxIndirectA(_para1:LPMSGBOXPARAMS):longint; external 'user32' name 'MessageBoxIndirectA';

  function GetWindowLongA(hWnd:HWND; nIndex:longint):LONG; external 'user32' name 'GetWindowLongA';

  function SetWindowLongA(hWnd:HWND; nIndex:longint; dwNewLong:LONG):LONG; external 'user32' name 'SetWindowLongA';

  function GetClassLongA(hWnd:HWND; nIndex:longint):DWORD; external 'user32' name 'GetClassLongA';

  function SetClassLongA(hWnd:HWND; nIndex:longint; dwNewLong:LONG):DWORD; external 'user32' name 'SetClassLongA';

  function FindWindowA(lpClassName:LPCSTR; lpWindowName:LPCSTR):HWND; external 'user32' name 'FindWindowA';

  function FindWindowExA(_para1:HWND; _para2:HWND; _para3:LPCSTR; _para4:LPCSTR):HWND; external 'user32' name 'FindWindowExA';

  function GetClassNameA(hWnd:HWND; lpClassName:LPSTR; nMaxCount:longint):longint; external 'user32' name 'GetClassNameA';

  function SetWindowsHookExA(idHook:longint; lpfn:HOOKPROC; hmod:HINST; dwThreadId:DWORD):HHOOK; external 'user32' name 'SetWindowsHookExA';

  function LoadBitmapA(hInstance:HINST; lpBitmapName:LPCSTR):HBITMAP; external 'user32' name 'LoadBitmapA';

  function LoadCursorA(hInstance:HINST; lpCursorName:LPCSTR):HCURSOR; external 'user32' name 'LoadCursorA';

  function LoadCursorFromFileA(lpFileName:LPCSTR):HCURSOR; external 'user32' name 'LoadCursorFromFileA';

  function LoadIconA(hInstance:HINST; lpIconName:LPCSTR):HICON; external 'user32' name 'LoadIconA';

  function LoadImageA(_para1:HINST; _para2:LPCSTR; _para3:UINT; _para4:longint; _para5:longint;
             _para6:UINT):HANDLE; external 'user32' name 'LoadImageA';

  function LoadStringA(hInstance:HINST; uID:UINT; lpBuffer:LPSTR; nBufferMax:longint):longint; external 'user32' name 'LoadStringA';

  function IsDialogMessageA(hDlg:HWND; lpMsg:LPMSG):WINBOOL; external 'user32' name 'IsDialogMessageA';

  function DlgDirListA(hDlg:HWND; lpPathSpec:LPSTR; nIDListBox:longint; nIDStaticPath:longint; uFileType:UINT):longint; external 'user32' name 'DlgDirListA';

  function DlgDirSelectExA(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDListBox:longint):WINBOOL; external 'user32' name 'DlgDirSelectExA';

  function DlgDirListComboBoxA(hDlg:HWND; lpPathSpec:LPSTR; nIDComboBox:longint; nIDStaticPath:longint; uFiletype:UINT):longint; external 'user32' name 'DlgDirListComboBoxA';

  function DlgDirSelectComboBoxExA(hDlg:HWND; lpString:LPSTR; nCount:longint; nIDComboBox:longint):WINBOOL; external 'user32' name 'DlgDirSelectComboBoxExA';

  function DefFrameProcA(hWnd:HWND; hWndMDIClient:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefFrameProcA';

  function DefMDIChildProcA(hWnd:HWND; uMsg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefMDIChildProcA';

  function CreateMDIWindowA(lpClassName:LPSTR; lpWindowName:LPSTR; dwStyle:DWORD; X:longint; Y:longint;
             nWidth:longint; nHeight:longint; hWndParent:HWND; hInstance:HINST; lParam:LPARAM):HWND; external 'user32' name 'CreateMDIWindowA';

  function WinHelpA(hWndMain:HWND; lpszHelp:LPCSTR; uCommand:UINT; dwData:DWORD):WINBOOL; external 'user32' name 'WinHelpA';

  function ChangeDisplaySettingsA(lpDevMode:LPDEVMODE; dwFlags:DWORD):LONG; external 'user32' name 'ChangeDisplaySettingsA';

  function EnumDisplaySettingsA(lpszDeviceName:LPCSTR; iModeNum:DWORD; lpDevMode:LPDEVMODE):WINBOOL; external 'user32' name 'EnumDisplaySettingsA';

  function SystemParametersInfoA(uiAction:UINT; uiParam:UINT; pvParam:PVOID; fWinIni:UINT):WINBOOL; external 'user32' name 'SystemParametersInfoA';

  function AddFontResourceA(_para1:LPCSTR):longint; external 'gdi32' name 'AddFontResourceA';

  function CopyMetaFileA(_para1:HMETAFILE; _para2:LPCSTR):HMETAFILE; external 'gdi32' name 'CopyMetaFileA';

  function CreateFontIndirectA(var _para1:LOGFONT):HFONT; external 'gdi32' name 'CreateFontIndirectA';

  function CreateICA(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; var _para4:DEVMODE):HDC; external 'gdi32' name 'CreateICA';

  function CreateMetaFileA(_para1:LPCSTR):HDC; external 'gdi32' name 'CreateMetaFileA';

  function CreateScalableFontResourceA(_para1:DWORD; _para2:LPCSTR; _para3:LPCSTR; _para4:LPCSTR):WINBOOL; external 'gdi32' name 'CreateScalableFontResourceA';

  function EnumFontFamiliesExA(_para1:HDC; _para2:LPLOGFONT; _para3:FONTENUMEXPROC; _para4:LPARAM; _para5:DWORD):longint; external 'gdi32' name 'EnumFontFamiliesExA';

  function EnumFontFamiliesA(_para1:HDC; _para2:LPCSTR; _para3:FONTENUMPROC; _para4:LPARAM):longint; external 'gdi32' name 'EnumFontFamiliesA';

  function EnumFontsA(_para1:HDC; _para2:LPCSTR; _para3:ENUMFONTSPROC; _para4:LPARAM):longint; external 'gdi32' name 'EnumFontsA';

  function GetCharWidthA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32' name 'GetCharWidthA';

  function GetCharWidth32A(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPINT):WINBOOL; external 'gdi32' name 'GetCharWidth32A';

  function GetCharWidthFloatA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:PFLOAT):WINBOOL; external 'gdi32' name 'GetCharWidthFloatA';

  function GetCharABCWidthsA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABC):WINBOOL; external 'gdi32' name 'GetCharABCWidthsA';

  function GetCharABCWidthsFloatA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPABCFLOAT):WINBOOL; external 'gdi32' name 'GetCharABCWidthsFloatA';

  function GetGlyphOutlineA(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPGLYPHMETRICS; _para5:DWORD;
             _para6:LPVOID; var _para7:MAT2):DWORD; external 'gdi32' name 'GetGlyphOutlineA';

  function GetMetaFileA(_para1:LPCSTR):HMETAFILE; external 'gdi32' name 'GetMetaFileA';

  function GetOutlineTextMetricsA(_para1:HDC; _para2:UINT; _para3:LPOUTLINETEXTMETRIC):UINT; external 'gdi32' name 'GetOutlineTextMetricsA';

  function GetTextExtentPointA(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPointA';

  function GetTextExtentPoint32A(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentPoint32A';

  function GetTextExtentExPointA(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPINT;
             _para6:LPINT; _para7:LPSIZE):WINBOOL; external 'gdi32' name 'GetTextExtentExPointA';

  function GetCharacterPlacementA(_para1:HDC; _para2:LPCSTR; _para3:longint; _para4:longint; _para5:LPGCP_RESULTS;
             _para6:DWORD):DWORD; external 'gdi32' name 'GetCharacterPlacementA';

  function ResetDCA(_para1:HDC; var _para2:DEVMODE):HDC; external 'gdi32' name 'ResetDCA';

  function RemoveFontResourceA(_para1:LPCSTR):WINBOOL; external 'gdi32' name 'RemoveFontResourceA';

  function CopyEnhMetaFileA(_para1:HENHMETAFILE; _para2:LPCSTR):HENHMETAFILE; external 'gdi32' name 'CopyEnhMetaFileA';

  function CreateEnhMetaFileA(_para1:HDC; _para2:LPCSTR; var _para3:RECT; _para4:LPCSTR):HDC; external 'gdi32' name 'CreateEnhMetaFileA';

  function GetEnhMetaFileA(_para1:LPCSTR):HENHMETAFILE; external 'gdi32' name 'GetEnhMetaFileA';

  function GetEnhMetaFileDescriptionA(_para1:HENHMETAFILE; _para2:UINT; _para3:LPSTR):UINT; external 'gdi32' name 'GetEnhMetaFileDescriptionA';

  function GetTextMetricsA(_para1:HDC; _para2:LPTEXTMETRIC):WINBOOL; external 'gdi32' name 'GetTextMetricsA';

  function StartDocA(_para1:HDC; var _para2:DOCINFO):longint; external 'gdi32' name 'StartDocA';

  function GetObjectA(_para1:HGDIOBJ; _para2:longint; _para3:LPVOID):longint; external 'gdi32' name 'GetObjectA';

  function TextOutA(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint):WINBOOL; external 'gdi32' name 'TextOutA';

  function ExtTextOutA(_para1:HDC; _para2:longint; _para3:longint; _para4:UINT; var _para5:RECT;
             _para6:LPCSTR; _para7:UINT; var _para8:INT):WINBOOL; external 'gdi32' name 'ExtTextOutA';

  function PolyTextOutA(_para1:HDC; var _para2:POLYTEXT; _para3:longint):WINBOOL; external 'gdi32' name 'PolyTextOutA';

  function GetTextFaceA(_para1:HDC; _para2:longint; _para3:LPSTR):longint; external 'gdi32' name 'GetTextFaceA';

  function GetKerningPairsA(_para1:HDC; _para2:DWORD; _para3:LPKERNINGPAIR):DWORD; external 'gdi32' name 'GetKerningPairsA';

  function CreateColorSpaceA(_para1:LPLOGCOLORSPACE):HCOLORSPACE; external 'gdi32' name 'CreateColorSpaceA';

  function GetLogColorSpaceA(_para1:HCOLORSPACE; _para2:LPLOGCOLORSPACE; _para3:DWORD):WINBOOL; external 'gdi32' name 'GetLogColorSpaceA';

  function GetICMProfileA(_para1:HDC; _para2:DWORD; _para3:LPSTR):WINBOOL; external 'gdi32' name 'GetICMProfileA';

  function SetICMProfileA(_para1:HDC; _para2:LPSTR):WINBOOL; external 'gdi32' name 'SetICMProfileA';

  function UpdateICMRegKeyA(_para1:DWORD; _para2:DWORD; _para3:LPSTR; _para4:UINT):WINBOOL; external 'gdi32' name 'UpdateICMRegKeyA';

  function EnumICMProfilesA(_para1:HDC; _para2:ICMENUMPROC; _para3:LPARAM):longint; external 'gdi32' name 'EnumICMProfilesA';

  function PropertySheetA(lppsph:LPCPROPSHEETHEADER):longint; external 'comctl32' name 'PropertySheetA';

  function ImageList_LoadImageA(hi:HINST; lpbmp:LPCSTR; cx:longint; cGrow:longint; crMask:COLORREF;
             uType:UINT; uFlags:UINT):HIMAGELIST; external 'comctl32' name 'ImageList_LoadImageA';

  function CreateStatusWindowA(style:LONG; lpszText:LPCSTR; hwndParent:HWND; wID:UINT):HWND; external 'comctl32' name 'CreateStatusWindowA';

  procedure DrawStatusTextA(hDC:HDC; lprc:LPRECT; pszText:LPCSTR; uFlags:UINT); external 'comctl32' name 'DrawStatusTextA';

  function GetOpenFileNameA(_para1:LPOPENFILENAME):WINBOOL; external 'comdlg32' name 'GetOpenFileNameA';

  function GetSaveFileNameA(_para1:LPOPENFILENAME):WINBOOL; external 'comdlg32' name 'GetSaveFileNameA';

  function GetFileTitleA(_para1:LPCSTR; _para2:LPSTR; _para3:WORD):integer; external 'comdlg32' name 'GetFileTitleA';

  function ChooseColorA(_para1:LPCHOOSECOLOR):WINBOOL; external 'comdlg32' name 'ChooseColorA';

  function FindTextA(_para1:LPFINDREPLACE):HWND; external 'comdlg32' name 'FindTextA';

  function ReplaceTextA(_para1:LPFINDREPLACE):HWND; external 'comdlg32' name 'ReplaceTextA';

  function ChooseFontA(_para1:LPCHOOSEFONT):WINBOOL; external 'comdlg32' name 'ChooseFontA';

  function PrintDlgA(_para1:LPPRINTDLG):WINBOOL; external 'comdlg32' name 'PrintDlgA';

  function PageSetupDlgA(_para1:LPPAGESETUPDLG):WINBOOL; external 'comdlg32' name 'PageSetupDlgA';

  function CreateProcessA(lpApplicationName:LPCSTR; lpCommandLine:LPSTR; lpProcessAttributes:LPSECURITY_ATTRIBUTES; lpThreadAttributes:LPSECURITY_ATTRIBUTES; bInheritHandles:WINBOOL;
             dwCreationFlags:DWORD; lpEnvironment:LPVOID; lpCurrentDirectory:LPCSTR; lpStartupInfo:LPSTARTUPINFO; lpProcessInformation:LPPROCESS_INFORMATION):WINBOOL; external 'kernel32' name 'CreateProcessA';

  procedure GetStartupInfoA(lpStartupInfo:LPSTARTUPINFO); external 'kernel32' name 'GetStartupInfoA';

  function FindFirstFileA(lpFileName:LPCSTR; lpFindFileData:LPWIN32_FIND_DATA):HANDLE; external 'kernel32' name 'FindFirstFileA';

  function FindNextFileA(hFindFile:HANDLE; lpFindFileData:LPWIN32_FIND_DATA):WINBOOL; external 'kernel32' name 'FindNextFileA';

  function GetVersionExA(var VersionInformation:OSVERSIONINFO):WINBOOL; external 'kernel32' name 'GetVersionExA';

  { was #define dname(params) def_expr }
  function CreateWindowA(lpClassName:LPCSTR; lpWindowName:LPCSTR; dwStyle:DWORD; X:longint;
             Y:longint; nWidth:longint; nHeight:longint; hWndParent:HWND; hMenu:HMENU;
             hInstance:HINST; lpParam:LPVOID):HWND;
    begin
       CreateWindowA:=CreateWindowExA(0,lpClassName,lpWindowName,dwStyle,x,y,nWidth,nHeight,hWndParent,hMenu,hInstance,lpParam);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogA:=CreateDialogParamA(hInstance,lpTemplateName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function CreateDialogIndirectA(hInstance:HINST; lpTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):HWND;
    begin
       CreateDialogIndirectA:=CreateDialogIndirectParamA(hInstance,lpTemplate,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxA(hInstance:HINST; lpTemplateName:LPCSTR; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxA:=DialogBoxParamA(hInstance,lpTemplateName,hWndParent,lpDialogFunc,0);
    end;

  { was #define dname(params) def_expr }
  function DialogBoxIndirectA(hInstance:HINST; hDialogTemplate:LPCDLGTEMPLATE; hWndParent:HWND; lpDialogFunc:DLGPROC):longint;
    begin
       DialogBoxIndirectA:=DialogBoxIndirectParamA(hInstance,hDialogTemplate,hWndParent,lpDialogFunc,0);
    end;

  function CreateDCA(_para1:LPCSTR; _para2:LPCSTR; _para3:LPCSTR; _para4:pDEVMODE):HDC; external 'gdi32' name 'CreateDCA';

  function VerInstallFileA(uFlags:DWORD; szSrcFileName:LPSTR; szDestFileName:LPSTR; szSrcDir:LPSTR; szDestDir:LPSTR;
             szCurDir:LPSTR; szTmpFile:LPSTR; lpuTmpFileLen:PUINT):DWORD; external 'version' name 'VerInstallFileA';

  function GetFileVersionInfoSizeA(lptstrFilename:LPSTR; lpdwHandle:LPDWORD):DWORD; external 'version' name 'GetFileVersionInfoSizeA';

  function GetFileVersionInfoA(lptstrFilename:LPSTR; dwHandle:DWORD; dwLen:DWORD; lpData:LPVOID):WINBOOL; external 'version' name 'GetFileVersionInfoA';

  function VerLanguageNameA(wLang:DWORD; szLang:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'VerLanguageNameA';

  function VerQueryValueA(pBlock:LPVOID; lpSubBlock:LPSTR; var lplpBuffer:LPVOID; puLen:PUINT):WINBOOL; external 'version' name 'VerQueryValueA';

  function VerFindFileA(uFlags:DWORD; szFileName:LPSTR; szWinDir:LPSTR; szAppDir:LPSTR; szCurDir:LPSTR;
             lpuCurDirLen:PUINT; szDestDir:LPSTR; lpuDestDirLen:PUINT):DWORD; external 'version' name 'VerFindFileA';

  function RegConnectRegistryA(lpMachineName:LPSTR; hKey:HKEY; phkResult:PHKEY):LONG; external 'advapi32' name 'RegConnectRegistryA';

  function RegCreateKeyA(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG; external 'advapi32' name 'RegCreateKeyA';

  function RegCreateKeyExA(hKey:HKEY; lpSubKey:LPCSTR; Reserved:DWORD; lpClass:LPSTR; dwOptions:DWORD;
             samDesired:REGSAM; lpSecurityAttributes:LPSECURITY_ATTRIBUTES; phkResult:PHKEY; lpdwDisposition:LPDWORD):LONG; external 'advapi32' name 'RegCreateKeyExA';

  function RegDeleteKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG; external 'advapi32' name 'RegDeleteKeyA';

  function RegDeleteValueA(hKey:HKEY; lpValueName:LPCSTR):LONG; external 'advapi32' name 'RegDeleteValueA';

  function RegEnumKeyA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; cbName:DWORD):LONG; external 'advapi32' name 'RegEnumKeyA';

  function RegEnumKeyExA(hKey:HKEY; dwIndex:DWORD; lpName:LPSTR; lpcbName:LPDWORD; lpReserved:LPDWORD;
             lpClass:LPSTR; lpcbClass:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external 'advapi32' name 'RegEnumKeyExA';

  function RegEnumValueA(hKey:HKEY; dwIndex:DWORD; lpValueName:LPSTR; lpcbValueName:LPDWORD; lpReserved:LPDWORD;
             lpType:LPDWORD; lpData:LPBYTE; lpcbData:LPDWORD):LONG; external 'advapi32' name 'RegEnumValueA';

  function RegLoadKeyA(hKey:HKEY; lpSubKey:LPCSTR; lpFile:LPCSTR):LONG; external 'advapi32' name 'RegLoadKeyA';

  function RegOpenKeyA(hKey:HKEY; lpSubKey:LPCSTR; phkResult:PHKEY):LONG; external 'advapi32' name 'RegOpenKeyA';

  function RegOpenKeyExA(hKey:HKEY; lpSubKey:LPCSTR; ulOptions:DWORD; samDesired:REGSAM; var phkResult:HKEY):LONG; external 'advapi32' name 'RegOpenKeyExA';

  function RegQueryInfoKeyA(hKey:HKEY; lpClass:LPSTR; lpcbClass:LPDWORD; lpReserved:LPDWORD; lpcSubKeys:LPDWORD;
             lpcbMaxSubKeyLen:LPDWORD; lpcbMaxClassLen:LPDWORD; lpcValues:LPDWORD; lpcbMaxValueNameLen:LPDWORD; lpcbMaxValueLen:LPDWORD;
             lpcbSecurityDescriptor:LPDWORD; lpftLastWriteTime:PFILETIME):LONG; external 'advapi32' name 'RegQueryInfoKeyA';

  function RegQueryValueA(hKey:HKEY; lpSubKey:LPCSTR; lpValue:LPSTR; lpcbValue:PLONG):LONG; external 'advapi32' name 'RegQueryValueA';

  function RegQueryMultipleValuesA(hKey:HKEY; val_list:PVALENT; num_vals:DWORD; lpValueBuf:LPSTR; ldwTotsize:LPDWORD):LONG; external 'advapi32' name 'RegQueryMultipleValuesA';

  function RegQueryValueExA(hKey:HKEY; lpValueName:LPCSTR; lpReserved:LPDWORD; lpType:LPDWORD; lpData:LPBYTE;
             lpcbData:LPDWORD):LONG; external 'advapi32' name 'RegQueryValueExA';

  function RegReplaceKeyA(hKey:HKEY; lpSubKey:LPCSTR; lpNewFile:LPCSTR; lpOldFile:LPCSTR):LONG; external 'advapi32' name 'RegReplaceKeyA';

  function RegRestoreKeyA(hKey:HKEY; lpFile:LPCSTR; dwFlags:DWORD):LONG; external 'advapi32' name 'RegRestoreKeyA';

  function RegSaveKeyA(hKey:HKEY; lpFile:LPCSTR; lpSecurityAttributes:LPSECURITY_ATTRIBUTES):LONG; external 'advapi32' name 'RegSaveKeyA';

  function RegSetValueA(hKey:HKEY; lpSubKey:LPCSTR; dwType:DWORD; lpData:LPCSTR; cbData:DWORD):LONG; external 'advapi32' name 'RegSetValueA';

  function RegSetValueExA(hKey:HKEY; lpValueName:LPCSTR; Reserved:DWORD; dwType:DWORD; var lpData:BYTE;
             cbData:DWORD):LONG; external 'advapi32' name 'RegSetValueExA';

  function RegUnLoadKeyA(hKey:HKEY; lpSubKey:LPCSTR):LONG; external 'advapi32' name 'RegUnLoadKeyA';

  function InitiateSystemShutdownA(lpMachineName:LPSTR; lpMessage:LPSTR; dwTimeout:DWORD; bForceAppsClosed:WINBOOL; bRebootAfterShutdown:WINBOOL):WINBOOL; external 'advapi32' name 'InitiateSystemShutdownA';

  function AbortSystemShutdownA(lpMachineName:LPSTR):WINBOOL; external 'advapi32' name 'AbortSystemShutdownA';

  function CompareStringA(Locale:LCID; dwCmpFlags:DWORD; lpString1:LPCSTR; cchCount1:longint; lpString2:LPCSTR;
             cchCount2:longint):longint; external 'kernel32' name 'CompareStringA';

  function LCMapStringA(Locale:LCID; dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR;
             cchDest:longint):longint; external 'kernel32' name 'LCMapStringA';

  function GetLocaleInfoA(Locale:LCID; LCType:LCTYPE; lpLCData:LPSTR; cchData:longint):longint; external 'kernel32' name 'GetLocaleInfoA';

  function SetLocaleInfoA(Locale:LCID; LCType:LCTYPE; lpLCData:LPCSTR):WINBOOL; external 'kernel32' name 'SetLocaleInfoA';

  function GetTimeFormatA(Locale:LCID; dwFlags:DWORD; var lpTime:SYSTEMTIME; lpFormat:LPCSTR; lpTimeStr:LPSTR;
             cchTime:longint):longint; external 'kernel32' name 'GetTimeFormatA';

  function GetDateFormatA(Locale:LCID; dwFlags:DWORD; var lpDate:SYSTEMTIME; lpFormat:LPCSTR; lpDateStr:LPSTR;
             cchDate:longint):longint; external 'kernel32' name 'GetDateFormatA';

  function GetNumberFormatA(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:NUMBERFMT; lpNumberStr:LPSTR;
             cchNumber:longint):longint; external 'kernel32' name 'GetNumberFormatA';

  function GetCurrencyFormatA(Locale:LCID; dwFlags:DWORD; lpValue:LPCSTR; var lpFormat:CURRENCYFMT; lpCurrencyStr:LPSTR;
             cchCurrency:longint):longint; external 'kernel32' name 'GetCurrencyFormatA';

  function EnumCalendarInfoA(lpCalInfoEnumProc:CALINFO_ENUMPROC; Locale:LCID; Calendar:CALID; CalType:CALTYPE):WINBOOL; external 'kernel32' name 'EnumCalendarInfoA';

  function EnumTimeFormatsA(lpTimeFmtEnumProc:TIMEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumTimeFormatsA';

  function EnumDateFormatsA(lpDateFmtEnumProc:DATEFMT_ENUMPROC; Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumDateFormatsA';

  function GetStringTypeExA(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32' name 'GetStringTypeExA';

  function GetStringTypeA(Locale:LCID; dwInfoType:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpCharType:LPWORD):WINBOOL; external 'kernel32' name 'GetStringTypeA';

  function FoldStringA(dwMapFlags:DWORD; lpSrcStr:LPCSTR; cchSrc:longint; lpDestStr:LPSTR; cchDest:longint):longint; external 'kernel32' name 'FoldStringA';

  function EnumSystemLocalesA(lpLocaleEnumProc:LOCALE_ENUMPROC; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumSystemLocalesA';

  function EnumSystemCodePagesA(lpCodePageEnumProc:CODEPAGE_ENUMPROC; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'EnumSystemCodePagesA';

  function PeekConsoleInputA(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL; external 'kernel32' name 'PeekConsoleInputA';

  function ReadConsoleInputA(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsRead:DWORD):WINBOOL; external 'kernel32' name 'ReadConsoleInputA';

  function WriteConsoleInputA(hConsoleInput:HANDLE; var lpBuffer:TINPUTRECORD; nLength:DWORD; var lpNumberOfEventsWritten:DWORD):WINBOOL; external 'kernel32' name 'WriteConsoleInputA';

  function ReadConsoleOutputA(hConsoleOutput:HANDLE; lpBuffer:PCHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpReadRegion:PSMALL_RECT):WINBOOL; external 'kernel32' name 'ReadConsoleOutputA';

  function WriteConsoleOutputA(hConsoleOutput:HANDLE; var lpBuffer:CHAR_INFO; dwBufferSize:COORD; dwBufferCoord:COORD; lpWriteRegion:PSMALL_RECT):WINBOOL; external 'kernel32' name 'WriteConsoleOutputA';

  function ReadConsoleOutputCharacterA(hConsoleOutput:HANDLE; lpCharacter:LPSTR; nLength:DWORD; dwReadCoord:COORD; lpNumberOfCharsRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadConsoleOutputCharacterA';

  function WriteConsoleOutputCharacterA(hConsoleOutput:HANDLE; lpCharacter:LPCSTR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteConsoleOutputCharacterA';

  function FillConsoleOutputCharacterA(hConsoleOutput:HANDLE; cCharacter:CHAR; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfCharsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'FillConsoleOutputCharacterA';

  function ScrollConsoleScreenBufferA(hConsoleOutput:HANDLE; var lpScrollRectangle:SMALL_RECT; var lpClipRectangle:SMALL_RECT; dwDestinationOrigin:COORD; var lpFill:CHAR_INFO):WINBOOL; external 'kernel32' name 'ScrollConsoleScreenBufferA';

  function GetConsoleTitleA(lpConsoleTitle:LPSTR; nSize:DWORD):DWORD; external 'kernel32' name 'GetConsoleTitleA';

  function SetConsoleTitleA(lpConsoleTitle:LPCSTR):WINBOOL; external 'kernel32' name 'SetConsoleTitleA';

  function ReadConsoleA(hConsoleInput:HANDLE; lpBuffer:LPVOID; nNumberOfCharsToRead:DWORD; lpNumberOfCharsRead:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32' name 'ReadConsoleA';

  function WriteConsoleA(hConsoleOutput:HANDLE;lpBuffer:pointer; nNumberOfCharsToWrite:DWORD; lpNumberOfCharsWritten:LPDWORD; lpReserved:LPVOID):WINBOOL; external 'kernel32' name 'WriteConsoleA';

  function WNetAddConnectionA(lpRemoteName:LPCSTR; lpPassword:LPCSTR; lpLocalName:LPCSTR):DWORD; external 'mpr' name 'WNetAddConnectionA';

  function WNetAddConnection2A(lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD; external 'mpr' name 'WNetAddConnection2A';

  function WNetAddConnection3A(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpPassword:LPCSTR; lpUserName:LPCSTR; dwFlags:DWORD):DWORD; external 'mpr' name 'WNetAddConnection3A';

  function WNetCancelConnectionA(lpName:LPCSTR; fForce:WINBOOL):DWORD; external 'mpr' name 'WNetCancelConnectionA';

  function WNetCancelConnection2A(lpName:LPCSTR; dwFlags:DWORD; fForce:WINBOOL):DWORD; external 'mpr' name 'WNetCancelConnection2A';

  function WNetGetConnectionA(lpLocalName:LPCSTR; lpRemoteName:LPSTR; lpnLength:LPDWORD):DWORD; external 'mpr' name 'WNetGetConnectionA';

  function WNetUseConnectionA(hwndOwner:HWND; lpNetResource:LPNETRESOURCE; lpUserID:LPCSTR; lpPassword:LPCSTR; dwFlags:DWORD;
             lpAccessName:LPSTR; lpBufferSize:LPDWORD; lpResult:LPDWORD):DWORD; external 'mpr' name 'WNetUseConnectionA';

  function WNetSetConnectionA(lpName:LPCSTR; dwProperties:DWORD; pvValues:LPVOID):DWORD; external 'mpr' name 'WNetSetConnectionA';

  function WNetConnectionDialog1A(lpConnDlgStruct:LPCONNECTDLGSTRUCT):DWORD; external 'mpr' name 'WNetConnectionDialog1A';

  function WNetDisconnectDialog1A(lpConnDlgStruct:LPDISCDLGSTRUCT):DWORD; external 'mpr' name 'WNetDisconnectDialog1A';

  function WNetOpenEnumA(dwScope:DWORD; dwType:DWORD; dwUsage:DWORD; lpNetResource:LPNETRESOURCE; lphEnum:LPHANDLE):DWORD; external 'mpr' name 'WNetOpenEnumA';

  function WNetEnumResourceA(hEnum:HANDLE; lpcCount:LPDWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetEnumResourceA';

  function WNetGetUniversalNameA(lpLocalPath:LPCSTR; dwInfoLevel:DWORD; lpBuffer:LPVOID; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetGetUniversalNameA';

  function WNetGetUserA(lpName:LPCSTR; lpUserName:LPSTR; lpnLength:LPDWORD):DWORD; external 'mpr' name 'WNetGetUserA';

  function WNetGetProviderNameA(dwNetType:DWORD; lpProviderName:LPSTR; lpBufferSize:LPDWORD):DWORD; external 'mpr' name 'WNetGetProviderNameA';

  function WNetGetNetworkInformationA(lpProvider:LPCSTR; lpNetInfoStruct:LPNETINFOSTRUCT):DWORD; external 'mpr' name 'WNetGetNetworkInformationA';

  function WNetGetLastErrorA(lpError:LPDWORD; lpErrorBuf:LPSTR; nErrorBufSize:DWORD; lpNameBuf:LPSTR; nNameBufSize:DWORD):DWORD; external 'mpr' name 'WNetGetLastErrorA';

  function MultinetGetConnectionPerformanceA(lpNetResource:LPNETRESOURCE; lpNetConnectInfoStruct:LPNETCONNECTINFOSTRUCT):DWORD; external 'mpr' name 'MultinetGetConnectionPerformanceA';

  function ChangeServiceConfigA(hService:SC_HANDLE; dwServiceType:DWORD; dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR;
             lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD; lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR;
             lpDisplayName:LPCSTR):WINBOOL; external 'advapi32' name 'ChangeServiceConfigA';

  function CreateServiceA(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPCSTR; dwDesiredAccess:DWORD; dwServiceType:DWORD;
             dwStartType:DWORD; dwErrorControl:DWORD; lpBinaryPathName:LPCSTR; lpLoadOrderGroup:LPCSTR; lpdwTagId:LPDWORD;
             lpDependencies:LPCSTR; lpServiceStartName:LPCSTR; lpPassword:LPCSTR):SC_HANDLE; external 'advapi32' name 'CreateServiceA';

  function EnumDependentServicesA(hService:SC_HANDLE; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD;
             lpServicesReturned:LPDWORD):WINBOOL; external 'advapi32' name 'EnumDependentServicesA';

  function EnumServicesStatusA(hSCManager:SC_HANDLE; dwServiceType:DWORD; dwServiceState:DWORD; lpServices:LPENUM_SERVICE_STATUS; cbBufSize:DWORD;
             pcbBytesNeeded:LPDWORD; lpServicesReturned:LPDWORD; lpResumeHandle:LPDWORD):WINBOOL; external 'advapi32' name 'EnumServicesStatusA';

  function GetServiceKeyNameA(hSCManager:SC_HANDLE; lpDisplayName:LPCSTR; lpServiceName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32' name 'GetServiceKeyNameA';

  function GetServiceDisplayNameA(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; lpDisplayName:LPSTR; lpcchBuffer:LPDWORD):WINBOOL; external 'advapi32' name 'GetServiceDisplayNameA';

  function OpenSCManagerA(lpMachineName:LPCSTR; lpDatabaseName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32' name 'OpenSCManagerA';

  function OpenServiceA(hSCManager:SC_HANDLE; lpServiceName:LPCSTR; dwDesiredAccess:DWORD):SC_HANDLE; external 'advapi32' name 'OpenServiceA';

  function QueryServiceConfigA(hService:SC_HANDLE; lpServiceConfig:LPQUERY_SERVICE_CONFIG; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'QueryServiceConfigA';

  function QueryServiceLockStatusA(hSCManager:SC_HANDLE; lpLockStatus:LPQUERY_SERVICE_LOCK_STATUS; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'QueryServiceLockStatusA';

  function RegisterServiceCtrlHandlerA(lpServiceName:LPCSTR; lpHandlerProc:LPHANDLER_FUNCTION):SERVICE_STATUS_HANDLE; external 'advapi32' name 'RegisterServiceCtrlHandlerA';

  function StartServiceCtrlDispatcherA(lpServiceStartTable:LPSERVICE_TABLE_ENTRY):WINBOOL; external 'advapi32' name 'StartServiceCtrlDispatcherA';

  function StartServiceA(hService:SC_HANDLE; dwNumServiceArgs:DWORD; var lpServiceArgVectors:LPCSTR):WINBOOL; external 'advapi32' name 'StartServiceA';

  function DragQueryFileA(_para1:HDROP; _para2:cardinal; _para3:Pchar;
    _para4:cardinal):cardinal; external 'shell32' name 'DragQueryFileA';

  function ExtractAssociatedIconA(_para1:HINST; _para2:Pchar;
    var _para3:WORD):HICON; external 'shell32' name 'ExtractAssociatedIconA';

  function ExtractIconA(_para1:HINST; _para2:Pchar; _para3:cardinal):HICON;
    external 'shell32' name 'ExtractIconA';

  function FindExecutableA(_para1:pchar; _para2:pchar;
    _para3:pchar):HINST; external 'shell32' name 'FindExecutableA';

  function ShellAboutA(_para1:HWND; _para2:pchar; _para3:pchar;
    _para4:HICON):longint; external 'shell32' name 'ShellAboutA';

  function ShellExecuteA(_para1:HWND; _para2:pchar; _para3:pchar;
    _para4:pchar; _para5:pchar;
    _para6:longint):HINST; external 'shell32' name 'ShellExecuteA';

  function DdeCreateStringHandleA(_para1:DWORD; _para2:pchar;
    _para3:longint):HSZ; external 'user32' name 'DdeCreateStringHandleA';

  function DdeInitializeA(var _para1:DWORD; _para2:CALLB; _para3:DWORD; _para4:DWORD):UINT;
    external 'user32' name 'DdeInitializeA';

  function DdeQueryStringA(_para1:DWORD; _para2:HSZ; _para3:pchar;
    _para4:DWORD; _para5:longint):DWORD; external 'user32' name 'DdeQueryStringA';

  function LogonUserA(_para1:LPSTR; _para2:LPSTR; _para3:LPSTR; _para4:DWORD; _para5:DWORD;
             var _para6:HANDLE):WINBOOL; external 'advapi32' name 'LogonUserA';

  function CreateProcessAsUserA(_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; var _para4:SECURITY_ATTRIBUTES; var _para5:SECURITY_ATTRIBUTES;
             _para6:WINBOOL; _para7:DWORD; _para8:LPVOID; _para9:LPCTSTR; var _para10:STARTUPINFO;
             var _para11:PROCESS_INFORMATION):WINBOOL; external 'advapi32' name 'CreateProcessAsUserA';

{$endif read_implementation}

{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.5  2000-03-20 23:44:43  alex
   + added overlaoded PeekMessage fundtion for delphi compatibility

  Revision 1.4  2000/02/09 16:59:35  peter
    * truncated log

  Revision 1.3  2000/01/07 16:41:54  daniel
    * copyright 2000

  Revision 1.2  1999/09/28 22:30:00  peter
    * fixed createdc to be D4 compatible

  Revision 1.1  1999/09/16 13:38:22  peter
    * windows unit include moved to wininc/

}
