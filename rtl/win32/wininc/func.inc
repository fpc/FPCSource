{
    $Id$
    This file is part of the Free Pascal run time library.
    This unit contains the record definition for the Win32 API
    Copyright (c) 1999-2000 by Florian KLaempfl,
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

unit func;

{  Automatically converted by H2PAS.EXE from function.h
   Utility made by Florian Klaempfl 25th-28th september 96
   Improvements made by Mark A. Malakanov 22nd-25th may 97
   Further improvements by Michael Van Canneyt, April 1998
   define handling and error recovery by Pierre Muller, June 1998 }


  interface

   uses
      base,defines,struct,
{$ifdef UNICODE}
      unidef,
{$else not UNICODE}
      ascdef,
{$endif UNICODE}
      messages;

{$endif windows_include_files}

{$define Win95  used below }

{$ifdef read_interface}

  { C default packing is dword }

{$PACKRECORDS 4}
  {
     Functions.h

     Declarations for all the Windows32 API Functions

     Copyright (C) 1996, 1997 Free Software Foundation, Inc.

     Author: Scott Christley <scottc@net-community.com>

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
{$ifndef _GNU_H_WINDOWS32_FUNCTIONS}
{$define _GNU_H_WINDOWS32_FUNCTIONS}
{ C++ extern C conditionnal removed }
  { __cplusplus  }
  { These functions were a real pain, having to figure out which
     had Unicode/Ascii versions and which did not  }
(* removed
{$ifndef UNICODE_ONLY}
{$include <Windows32/UnicodeFunctions.h>}
{$endif}
  { !UNICODE_ONLY  }
{$ifndef ANSI_ONLY}
{$include <Windows32/ASCIIFunctions.h>}
{$endif}  *)
  { !ANSI_ONLY  }
  { Define the approprate declaration based upon UNICODE or ASCII  }
(* all this is not usable for FPC
  { UNICODE  }
{$ifdef UNICODE}

  const
     RegConnectRegistry = RegConnectRegistryW;
     RegCreateKey = RegCreateKeyW;
     RegCreateKeyEx = RegCreateKeyExW;
     RegDeleteKey = RegDeleteKeyW;
     RegDeleteValue = RegDeleteValueW;
     RegEnumKey = RegEnumKeyW;
     RegEnumKeyEx = RegEnumKeyExW;
     RegEnumValue = RegEnumValueW;
     RegLoadKey = RegLoadKeyW;
     RegOpenKey = RegOpenKeyW;
     RegOpenKeyEx = RegOpenKeyExW;
     RegQueryInfoKey = RegQueryInfoKeyW;
     RegQueryValue = RegQueryValueW;
     RegQueryMultipleValues = RegQueryMultipleValuesW;
     RegQueryValueEx = RegQueryValueExW;
     RegReplaceKey = RegReplaceKeyW;
     RegRestoreKey = RegRestoreKeyW;
     RegSaveKey = RegSaveKeyW;
     RegSetValue = RegSetValueW;
     RegSetValueEx = RegSetValueExW;
     AbortSystemShutdown = AbortSystemShutdownW;
     InitiateSystemShutdown = InitiateSystemShutdownW;
     RegUnLoadKey = RegUnLoadKeyW;
     SetProp = SetPropW;
     GetProp = GetPropW;
     RemoveProp = RemovePropW;
     EnumPropsEx = EnumPropsExW;
     EnumProps = EnumPropsW;
     SetWindowText = SetWindowTextW;
     GetWindowText = GetWindowTextW;
     GetWindowTextLength = GetWindowTextLengthW;
     MessageBox = MessageBoxW;
     MessageBoxEx = MessageBoxExW;
     MessageBoxIndirect = MessageBoxIndirectW;
     GetWindowLong = GetWindowLongW;
     SetWindowLong = SetWindowLongW;
     GetClassLong = GetClassLongW;
     SetClassLong = SetClassLongW;
     FindWindow = FindWindowW;
     FindWindowEx = FindWindowExW;
     GetClassName = GetClassNameW;
     SetWindowsHookEx = SetWindowsHookExW;
     LoadBitmap = LoadBitmapW;
     LoadCursor = LoadCursorW;
     LoadCursorFromFile = LoadCursorFromFileW;
     LoadIcon = LoadIconW;
     LoadImage = LoadImageW;
     LoadString = LoadStringW;
     IsDialogMessage = IsDialogMessageW;
     DlgDirList = DlgDirListW;
     DlgDirSelectEx = DlgDirSelectExW;
     DlgDirListComboBox = DlgDirListComboBoxW;
     DlgDirSelectComboBoxEx = DlgDirSelectComboBoxExW;
     DefFrameProc = DefFrameProcW;
     DefMDIChildProc = DefMDIChildProcW;
     CreateMDIWindow = CreateMDIWindowW;
     WinHelp = WinHelpW;
     ChangeDisplaySettings = ChangeDisplaySettingsW;
     EnumDisplaySettings = EnumDisplaySettingsW;
     SystemParametersInfo = SystemParametersInfoW;
     AddFontResource = AddFontResourceW;
     CopyMetaFile = CopyMetaFileW;
     CreateDC = CreateDCW;
     CreateFontIndirect = CreateFontIndirectW;
     CreateFont = CreateFontW;
     CreateIC = CreateICW;
     CreateMetaFile = CreateMetaFileW;
     CreateScalableFontResource = CreateScalableFontResourceW;
     DeviceCapabilities = DeviceCapabilitiesW;
     EnumFontFamiliesEx = EnumFontFamiliesExW;
     EnumFontFamilies = EnumFontFamiliesW;
     EnumFonts = EnumFontsW;
     GetCharWidth = GetCharWidthW;
     GetCharWidth32 = GetCharWidth32W;
     GetCharWidthFloat = GetCharWidthFloatW;
     GetCharABCWidths = GetCharABCWidthsW;
     GetCharABCWidthsFloat = GetCharABCWidthsFloatW;
     GetGlyphOutline = GetGlyphOutlineW;
     GetMetaFile = GetMetaFileW;
     GetOutlineTextMetrics = GetOutlineTextMetricsW;
     GetTextExtentPoint = GetTextExtentPointW;
     GetTextExtentPoint32 = GetTextExtentPoint32W;
     GetTextExtentExPoint = GetTextExtentExPointW;
     GetCharacterPlacement = GetCharacterPlacementW;
     ResetDC = ResetDCW;
     RemoveFontResource = RemoveFontResourceW;
     CopyEnhMetaFile = CopyEnhMetaFileW;
     CreateEnhMetaFile = CreateEnhMetaFileW;
     GetEnhMetaFile = GetEnhMetaFileW;
     GetEnhMetaFileDescription = GetEnhMetaFileDescriptionW;
     GetTextMetrics = GetTextMetricsW;
     StartDoc = StartDocW;
     GetObject = GetObjectW;
     TextOut = TextOutW;
     ExtTextOut = ExtTextOutW;
     PolyTextOut = PolyTextOutW;
     GetTextFace = GetTextFaceW;
     GetKerningPairs = GetKerningPairsW;
     GetLogColorSpace = GetLogColorSpaceW;
     CreateColorSpace = CreateColorSpaceW;
     GetICMProfile = GetICMProfileW;
     SetICMProfile = SetICMProfileW;
     UpdateICMRegKey = UpdateICMRegKeyW;
     EnumICMProfiles = EnumICMProfilesW;
     CreatePropertySheetPage = CreatePropertySheetPageW;
     PropertySheet = PropertySheetW;
     ImageList_LoadImage = ImageList_LoadImageW;
     CreateStatusWindow = CreateStatusWindowW;
     DrawStatusText = DrawStatusTextW;
     GetOpenFileName = GetOpenFileNameW;
     GetSaveFileName = GetSaveFileNameW;
     GetFileTitle = GetFileTitleW;
     ChooseColor = ChooseColorW;
     FindText = FindTextW;
     ReplaceText = ReplaceTextW;
     ChooseFont = ChooseFontW;
     PrintDlg = PrintDlgW;
     PageSetupDlg = PageSetupDlgW;
     DefWindowProc = DefWindowProcW;
     CallWindowProc = CallWindowProcW;
     RegisterClass = RegisterClassW;
     UnregisterClass = UnregisterClassW;
     GetClassInfo = GetClassInfoW;
     RegisterClassEx = RegisterClassExW;
     GetClassInfoEx = GetClassInfoExW;
     CreateWindowEx = CreateWindowExW;
     CreateWindow = CreateWindowW;
     CreateDialogParam = CreateDialogParamW;
     CreateDialogIndirectParam = CreateDialogIndirectParamW;
     CreateDialog = CreateDialogW;
     CreateDialogIndirect = CreateDialogIndirectW;
     DialogBoxParam = DialogBoxParamW;
     DialogBoxIndirectParam = DialogBoxIndirectParamW;
     DialogBox = DialogBoxW;
     DialogBoxIndirect = DialogBoxIndirectW;
     RegisterClipboardFormat = RegisterClipboardFormatW;
     SetDlgItemText = SetDlgItemTextW;
     GetDlgItemText = GetDlgItemTextW;
     SendDlgItemMessage = SendDlgItemMessageW;
     DefDlgProc = DefDlgProcW;
     CallMsgFilter = CallMsgFilterW;
     GetClipboardFormatName = GetClipboardFormatNameW;
     CharToOem = CharToOemW;
     OemToChar = OemToCharW;
     CharToOemBuff = CharToOemBuffW;
     OemToCharBuff = OemToCharBuffW;
     CharUpper = CharUpperW;
     CharUpperBuff = CharUpperBuffW;
     CharLower = CharLowerW;
     CharLowerBuff = CharLowerBuffW;
     CharNext = CharNextW;
     CharPrev = CharPrevW;
     IsCharAlpha = IsCharAlphaW;
     IsCharAlphaNumeric = IsCharAlphaNumericW;
     IsCharUpper = IsCharUpperW;
     IsCharLower = IsCharLowerW;
     GetKeyNameText = GetKeyNameTextW;
     VkKeyScan = VkKeyScanW;
     VkKeyScanEx = VkKeyScanExW;
     MapVirtualKey = MapVirtualKeyW;
     MapVirtualKeyEx = MapVirtualKeyExW;
     LoadAccelerators = LoadAcceleratorsW;
     CreateAcceleratorTable = CreateAcceleratorTableW;
     CopyAcceleratorTable = CopyAcceleratorTableW;
     TranslateAccelerator = TranslateAcceleratorW;
     LoadMenu = LoadMenuW;
     LoadMenuIndirect = LoadMenuIndirectW;
     ChangeMenu = ChangeMenuW;
     GetMenuString = GetMenuStringW;
     InsertMenu = InsertMenuW;
     AppendMenu = AppendMenuW;
     ModifyMenu = ModifyMenuW;
     InsertMenuItem = InsertMenuItemW;
     GetMenuItemInfo = GetMenuItemInfoW;
     SetMenuItemInfo = SetMenuItemInfoW;
     DrawText = DrawTextW;
     DrawTextEx = DrawTextExW;
     GrayString = GrayStringW;
     DrawState = DrawStateW;
     TabbedTextOut = TabbedTextOutW;
     GetTabbedTextExtent = GetTabbedTextExtentW;
     GetVersionEx = GetVersionExW;
     wvsprintf = wvsprintfW;
     wsprintf = wsprintfW;
     LoadKeyboardLayout = LoadKeyboardLayoutW;
     GetKeyboardLayoutName = GetKeyboardLayoutNameW;
     CreateDesktop = CreateDesktopW;
     OpenDesktop = OpenDesktopW;
     EnumDesktops = EnumDesktopsW;
     CreateWindowStation = CreateWindowStationW;
     OpenWindowStation = OpenWindowStationW;
     EnumWindowStations = EnumWindowStationsW;
     IsBadStringPtr = IsBadStringPtrW;
     LookupAccountSid = LookupAccountSidW;
     LookupAccountName = LookupAccountNameW;
     LookupPrivilegeValue = LookupPrivilegeValueW;
     LookupPrivilegeName = LookupPrivilegeNameW;
     LookupPrivilegeDisplayName = LookupPrivilegeDisplayNameW;
     BuildCommDCB = BuildCommDCBW;
     BuildCommDCBAndTimeouts = BuildCommDCBAndTimeoutsW;
     CommConfigDialog = CommConfigDialogW;
     GetDefaultCommConfig = GetDefaultCommConfigW;
     SetDefaultCommConfig = SetDefaultCommConfigW;
     GetComputerName = GetComputerNameW;
     SetComputerName = SetComputerNameW;
     GetUserName = GetUserNameW;
     CreateMailslot = CreateMailslotW;
     FormatMessage = FormatMessageW;
     GetEnvironmentStrings = GetEnvironmentStringsW;
     FreeEnvironmentStrings = FreeEnvironmentStringsW;
     lstrcmp = lstrcmpW;
     lstrcmpi = lstrcmpiW;
     lstrcpyn = lstrcpynW;
     lstrcpy = lstrcpyW;
     lstrcat = lstrcatW;
     lstrlen = lstrlenW;
     GetBinaryType = GetBinaryTypeW;
     GetShortPathName = GetShortPathNameW;
     SetFileSecurity = SetFileSecurityW;
     GetFileSecurity = GetFileSecurityW;
     FindFirstChangeNotification = FindFirstChangeNotificationW;
     AccessCheckAndAuditAlarm = AccessCheckAndAuditAlarmW;
     ObjectOpenAuditAlarm = ObjectOpenAuditAlarmW;
     ObjectPrivilegeAuditAlarm = ObjectPrivilegeAuditAlarmW;
     ObjectCloseAuditAlarm = ObjectCloseAuditAlarmW;
     PrivilegedServiceAuditAlarm = PrivilegedServiceAuditAlarmW;
     OpenEventLog = OpenEventLogW;
     RegisterEventSource = RegisterEventSourceW;
     OpenBackupEventLog = OpenBackupEventLogW;
     ReadEventLog = ReadEventLogW;
     ReportEvent = ReportEventW;
     CreateProcess = CreateProcessW;
     FatalAppExit = FatalAppExitW;
     GetStartupInfo = GetStartupInfoW;
     GetEnvironmentVariable = GetEnvironmentVariableW;
     GetCommandLine = GetCommandLineW;
     SetEnvironmentVariable = SetEnvironmentVariableW;
     ExpandEnvironmentStrings = ExpandEnvironmentStringsW;
     OutputDebugString = OutputDebugStringW;
     FindResource = FindResourceW;
     FindResourceEx = FindResourceExW;
     EnumResourceTypes = EnumResourceTypesW;
     EnumResourceNames = EnumResourceNamesW;
     EnumResourceLanguages = EnumResourceLanguagesW;
     BeginUpdateResource = BeginUpdateResourceW;
     UpdateResource = UpdateResourceW;
     EndUpdateResource = EndUpdateResourceW;
     GlobalAddAtom = GlobalAddAtomW;
     GlobalFindAtom = GlobalFindAtomW;
     GlobalGetAtomName = GlobalGetAtomNameW;
     AddAtom = AddAtomW;
     FindAtom = FindAtomW;
     GetAtomName = GetAtomNameW;
     GetProfileInt = GetProfileIntW;
     GetProfileString = GetProfileStringW;
     WriteProfileString = WriteProfileStringW;
     GetProfileSection = GetProfileSectionW;
     WriteProfileSection = WriteProfileSectionW;
     GetPrivateProfileInt = GetPrivateProfileIntW;
     GetPrivateProfileString = GetPrivateProfileStringW;
     WritePrivateProfileString = WritePrivateProfileStringW;
     GetPrivateProfileSection = GetPrivateProfileSectionW;
     WritePrivateProfileSection = WritePrivateProfileSectionW;
     GetDriveType = GetDriveTypeW;
     GetSystemDirectory = GetSystemDirectoryW;
     GetTempPath = GetTempPathW;
     GetTempFileName = GetTempFileNameW;
     GetWindowsDirectory = GetWindowsDirectoryW;
     SetCurrentDirectory = SetCurrentDirectoryW;
     GetCurrentDirectory = GetCurrentDirectoryW;
     GetDiskFreeSpace = GetDiskFreeSpaceW;
     CreateDirectory = CreateDirectoryW;
     CreateDirectoryEx = CreateDirectoryExW;
     RemoveDirectory = RemoveDirectoryW;
     GetFullPathName = GetFullPathNameW;
     DefineDosDevice = DefineDosDeviceW;
     QueryDosDevice = QueryDosDeviceW;
     CreateFile = CreateFileW;
     SetFileAttributes = SetFileAttributesW;
     GetFileAttributes = GetFileAttributesW;
     GetCompressedFileSize = GetCompressedFileSizeW;
     DeleteFile = DeleteFileW;
     FindFirstFile = FindFirstFileW;
     FindNextFile = FindNextFileW;
     SearchPath = SearchPathW;
     CopyFile = CopyFileW;
     MoveFile = MoveFileW;
     MoveFileEx = MoveFileExW;
     CreateNamedPipe = CreateNamedPipeW;
     GetNamedPipeHandleState = GetNamedPipeHandleStateW;
     CallNamedPipe = CallNamedPipeW;
     WaitNamedPipe = WaitNamedPipeW;
     SetVolumeLabel = SetVolumeLabelW;
     GetVolumeInformation = GetVolumeInformationW;
     ClearEventLog = ClearEventLogW;
     BackupEventLog = BackupEventLogW;
     CreateMutex = CreateMutexW;
     OpenMutex = OpenMutexW;
     CreateEvent = CreateEventW;
     OpenEvent = OpenEventW;
     CreateSemaphore = CreateSemaphoreW;
     OpenSemaphore = OpenSemaphoreW;
     CreateFileMapping = CreateFileMappingW;
     OpenFileMapping = OpenFileMappingW;
     GetLogicalDriveStrings = GetLogicalDriveStringsW;
     LoadLibrary = LoadLibraryW;
     LoadLibraryEx = LoadLibraryExW;
     GetModuleFileName = GetModuleFileNameW;
     GetModuleHandle = GetModuleHandleW;
     GetUserObjectInformation = GetUserObjectInformationW;
     SetUserObjectInformation = SetUserObjectInformationW;
     RegisterWindowMessage = RegisterWindowMessageW;
     GetMessage = GetMessageW;
     DispatchMessage = DispatchMessageW;
     PeekMessage = PeekMessageW;
     SendMessage = SendMessageW;
     SendMessageTimeout = SendMessageTimeoutW;
     SendNotifyMessage = SendNotifyMessageW;
     SendMessageCallback = SendMessageCallbackW;
     PostMessage = PostMessageW;
     PostThreadMessage = PostThreadMessageW;
     VerFindFile = VerFindFileW;
     VerInstallFile = VerInstallFileW;
     GetFileVersionInfoSize = GetFileVersionInfoSizeW;
     GetFileVersionInfo = GetFileVersionInfoW;
     VerLanguageName = VerLanguageNameW;
     VerQueryValue = VerQueryValueW;
     CompareString = CompareStringW;
     LCMapString = LCMapStringW;
     GetLocaleInfo = GetLocaleInfoW;
     SetLocaleInfo = SetLocaleInfoW;
     GetTimeFormat = GetTimeFormatW;
     GetDateFormat = GetDateFormatW;
     GetNumberFormat = GetNumberFormatW;
     GetCurrencyFormat = GetCurrencyFormatW;
     EnumCalendarInfo = EnumCalendarInfoW;
     EnumTimeFormats = EnumTimeFormatsW;
     FoldString = FoldStringW;
     EnumSystemCodePages = EnumSystemCodePagesW;
     EnumSystemLocales = EnumSystemLocalesW;
     GetStringTypeEx = GetStringTypeExW;
     EnumDateFormats = EnumDateFormatsW;
     GetConsoleTitle = GetConsoleTitleW;
     ScrollConsoleScreenBuffer = ScrollConsoleScreenBufferW;
     SetConsoleTitle = SetConsoleTitleW;
     ReadConsole = ReadConsoleW;
     WriteConsole = WriteConsoleW;
     PeekConsoleInput = PeekConsoleInputW;
     ReadConsoleInput = ReadConsoleInputW;
     WriteConsoleInput = WriteConsoleInputW;
     ReadConsoleOutput = ReadConsoleOutputW;
     WriteConsoleOutput = WriteConsoleOutputW;
     ReadConsoleOutputCharacter = ReadConsoleOutputCharacterW;
     WriteConsoleOutputCharacter = WriteConsoleOutputCharacterW;
     FillConsoleOutputCharacter = FillConsoleOutputCharacterW;
     WNetGetProviderName = WNetGetProviderNameW;
     WNetGetNetworkInformation = WNetGetNetworkInformationW;
     WNetGetLastError = WNetGetLastErrorW;
     MultinetGetConnectionPerformance = MultinetGetConnectionPerformanceW;
     WNetConnectionDialog1 = WNetConnectionDialog1W;
     WNetDisconnectDialog1 = WNetDisconnectDialog1W;
     WNetOpenEnum = WNetOpenEnumW;
     WNetEnumResource = WNetEnumResourceW;
     WNetGetUniversalName = WNetGetUniversalNameW;
     WNetGetUser = WNetGetUserW;
     WNetAddConnection = WNetAddConnectionW;
     WNetAddConnection2 = WNetAddConnection2W;
     WNetAddConnection3 = WNetAddConnection3W;
     WNetCancelConnection = WNetCancelConnectionW;
     WNetCancelConnection2 = WNetCancelConnection2W;
     WNetGetConnection = WNetGetConnectionW;
     WNetUseConnection = WNetUseConnectionW;
     WNetSetConnection = WNetSetConnectionW;
     CreateService = CreateServiceW;
     ChangeServiceConfig = ChangeServiceConfigW;
     EnumDependentServices = EnumDependentServicesW;
     EnumServicesStatus = EnumServicesStatusW;
     GetServiceKeyName = GetServiceKeyNameW;
     GetServiceDisplayName = GetServiceDisplayNameW;
     OpenSCManager = OpenSCManagerW;
     OpenService = OpenServiceW;
     QueryServiceConfig = QueryServiceConfigW;
     QueryServiceLockStatus = QueryServiceLockStatusW;
     RegisterServiceCtrlHandler = RegisterServiceCtrlHandlerW;
     StartServiceCtrlDispatcher = StartServiceCtrlDispatcherW;
     StartService = StartServiceW;
     DragQueryFile = DragQueryFileW;
     ExtractAssociatedIcon = ExtractAssociatedIconW;
     ExtractIcon = ExtractIconW;
     FindExecutable = FindExecutableW;
     ShellAbout = ShellAboutW;
     ShellExecute = ShellExecuteW;
     DdeCreateStringHandle = DdeCreateStringHandleW;
     DdeInitialize = DdeInitializeW;
     DdeQueryString = DdeQueryStringW;
     LogonUser = LogonUserW;
     CreateProcessAsUser = CreateProcessAsUserW;
  { ASCII  }
{$else}

  const
     RegConnectRegistry = RegConnectRegistryA;
     RegCreateKey = RegCreateKeyA;
     RegCreateKeyEx = RegCreateKeyExA;
     RegDeleteKey = RegDeleteKeyA;
     RegDeleteValue = RegDeleteValueA;
     RegEnumKey = RegEnumKeyA;
     RegEnumKeyEx = RegEnumKeyExA;
     RegEnumValue = RegEnumValueA;
     RegLoadKey = RegLoadKeyA;
     RegOpenKey = RegOpenKeyA;
     RegOpenKeyEx = RegOpenKeyExA;
     RegQueryInfoKey = RegQueryInfoKeyA;
     RegQueryValue = RegQueryValueA;
     RegQueryMultipleValues = RegQueryMultipleValuesA;
     RegQueryValueEx = RegQueryValueExA;
     RegReplaceKey = RegReplaceKeyA;
     RegRestoreKey = RegRestoreKeyA;
     RegSaveKey = RegSaveKeyA;
     RegSetValue = RegSetValueA;
     RegSetValueEx = RegSetValueExA;
     AbortSystemShutdown = AbortSystemShutdownA;
     InitiateSystemShutdown = InitiateSystemShutdownA;
     RegUnLoadKey = RegUnLoadKeyA;
     LoadIcon = LoadIconA;
     LoadImage = LoadImageA;
     LoadString = LoadStringA;
     IsDialogMessage = IsDialogMessageA;
     DlgDirList = DlgDirListA;
     DlgDirSelectEx = DlgDirSelectExA;
     DlgDirListComboBox = DlgDirListComboBoxA;
     DlgDirSelectComboBoxEx = DlgDirSelectComboBoxExA;
     DefFrameProc = DefFrameProcA;
     DefMDIChildProc = DefMDIChildProcA;
     CreateMDIWindow = CreateMDIWindowA;
     WinHelp = WinHelpA;
     ChangeDisplaySettings = ChangeDisplaySettingsA;
     EnumDisplaySettings = EnumDisplaySettingsA;
     SystemParametersInfo = SystemParametersInfoA;
     GetWindowLong = GetWindowLongA;
     SetWindowLong = SetWindowLongA;
     GetClassLong = GetClassLongA;
     SetClassLong = SetClassLongA;
     FindWindow = FindWindowA;
     FindWindowEx = FindWindowExA;
     GetClassName = GetClassNameA;
     SetWindowsHookEx = SetWindowsHookExA;
     LoadBitmap = LoadBitmapA;
     LoadCursor = LoadCursorA;
     LoadCursorFromFile = LoadCursorFromFileA;
     SetProp = SetPropA;
     GetProp = GetPropA;
     RemoveProp = RemovePropA;
     EnumPropsEx = EnumPropsExA;
     EnumProps = EnumPropsA;
     SetWindowText = SetWindowTextA;
     GetWindowText = GetWindowTextA;
     GetWindowTextLength = GetWindowTextLengthA;
     MessageBox = MessageBoxA;
     MessageBoxEx = MessageBoxExA;
     MessageBoxIndirect = MessageBoxIndirectA;
     AddFontResource = AddFontResourceA;
     CopyMetaFile = CopyMetaFileA;
     CreateDC = CreateDCA;
     CreateFontIndirect = CreateFontIndirectA;
     CreateFont = CreateFontA;
     CreateIC = CreateICA;
     CreateMetaFile = CreateMetaFileA;
     CreateScalableFontResource = CreateScalableFontResourceA;
     DeviceCapabilities = DeviceCapabilitiesA;
     EnumFontFamiliesEx = EnumFontFamiliesExA;
     EnumFontFamilies = EnumFontFamiliesA;
     EnumFonts = EnumFontsA;
     GetCharWidth = GetCharWidthA;
     GetCharWidth32 = GetCharWidth32A;
     GetCharWidthFloat = GetCharWidthFloatA;
     GetCharABCWidths = GetCharABCWidthsA;
     GetCharABCWidthsFloat = GetCharABCWidthsFloatA;
     GetGlyphOutline = GetGlyphOutlineA;
     GetMetaFile = GetMetaFileA;
     GetOutlineTextMetrics = GetOutlineTextMetricsA;
     GetTextExtentPoint = GetTextExtentPointA;
     GetTextExtentPoint32 = GetTextExtentPoint32A;
     GetTextExtentExPoint = GetTextExtentExPointA;
     GetCharacterPlacement = GetCharacterPlacementA;
     ResetDC = ResetDCA;
     RemoveFontResource = RemoveFontResourceA;
     CopyEnhMetaFile = CopyEnhMetaFileA;
     CreateEnhMetaFile = CreateEnhMetaFileA;
     GetEnhMetaFile = GetEnhMetaFileA;
     GetEnhMetaFileDescription = GetEnhMetaFileDescriptionA;
     GetTextMetrics = GetTextMetricsA;
     StartDoc = StartDocA;
     GetObject = GetObjectA;
     TextOut = TextOutA;
     ExtTextOut = ExtTextOutA;
     PolyTextOut = PolyTextOutA;
     GetTextFace = GetTextFaceA;
     GetKerningPairs = GetKerningPairsA;
     GetLogColorSpace = GetLogColorSpaceA;
     CreateColorSpace = CreateColorSpaceA;
     GetICMProfile = GetICMProfileA;
     SetICMProfile = SetICMProfileA;
     UpdateICMRegKey = UpdateICMRegKeyA;
     EnumICMProfiles = EnumICMProfilesA;
     CreatePropertySheetPage = CreatePropertySheetPageA;
     PropertySheet = PropertySheetA;
     ImageList_LoadImage = ImageList_LoadImageA;
     CreateStatusWindow = CreateStatusWindowA;
     DrawStatusText = DrawStatusTextA;
     GetOpenFileName = GetOpenFileNameA;
     GetSaveFileName = GetSaveFileNameA;
     GetFileTitle = GetFileTitleA;
     ChooseColor = ChooseColorA;
     FindText = FindTextA;
     ReplaceText = ReplaceTextA;
     ChooseFont = ChooseFontA;
     PrintDlg = PrintDlgA;
     PageSetupDlg = PageSetupDlgA;
     DefWindowProc = DefWindowProcA;
     CallWindowProc = CallWindowProcA;
     RegisterClass = RegisterClassA;
     UnregisterClass = UnregisterClassA;
     GetClassInfo = GetClassInfoA;
     RegisterClassEx = RegisterClassExA;
     GetClassInfoEx = GetClassInfoExA;
     CreateWindowEx = CreateWindowExA;
     CreateWindow = CreateWindowA;
     CreateDialogParam = CreateDialogParamA;
     CreateDialogIndirectParam = CreateDialogIndirectParamA;
     CreateDialog = CreateDialogA;
     CreateDialogIndirect = CreateDialogIndirectA;
     DialogBoxParam = DialogBoxParamA;
     DialogBoxIndirectParam = DialogBoxIndirectParamA;
     DialogBox = DialogBoxA;
     DialogBoxIndirect = DialogBoxIndirectA;
     RegisterClipboardFormat = RegisterClipboardFormatA;
     SetDlgItemText = SetDlgItemTextA;
     GetDlgItemText = GetDlgItemTextA;
     SendDlgItemMessage = SendDlgItemMessageA;
     DefDlgProc = DefDlgProcA;
     CallMsgFilter = CallMsgFilterA;
     GetClipboardFormatName = GetClipboardFormatNameA;
     CharToOem = CharToOemA;
     OemToChar = OemToCharA;
     CharToOemBuff = CharToOemBuffA;
     OemToCharBuff = OemToCharBuffA;
     CharUpper = CharUpperA;
     CharUpperBuff = CharUpperBuffA;
     CharLower = CharLowerA;
     CharLowerBuff = CharLowerBuffA;
     CharNext = CharNextA;
     CharPrev = CharPrevA;
     IsCharAlpha = IsCharAlphaA;
     IsCharAlphaNumeric = IsCharAlphaNumericA;
     IsCharUpper = IsCharUpperA;
     IsCharLower = IsCharLowerA;
     GetKeyNameText = GetKeyNameTextA;
     VkKeyScan = VkKeyScanA;
     VkKeyScanEx = VkKeyScanExA;
     MapVirtualKey = MapVirtualKeyA;
     MapVirtualKeyEx = MapVirtualKeyExA;
     LoadAccelerators = LoadAcceleratorsA;
     CreateAcceleratorTable = CreateAcceleratorTableA;
     CopyAcceleratorTable = CopyAcceleratorTableA;
     TranslateAccelerator = TranslateAcceleratorA;
     LoadMenu = LoadMenuA;
     LoadMenuIndirect = LoadMenuIndirectA;
     ChangeMenu = ChangeMenuA;
     GetMenuString = GetMenuStringA;
     InsertMenu = InsertMenuA;
     AppendMenu = AppendMenuA;
     ModifyMenu = ModifyMenuA;
     InsertMenuItem = InsertMenuItemA;
     GetMenuItemInfo = GetMenuItemInfoA;
     SetMenuItemInfo = SetMenuItemInfoA;
     DrawText = DrawTextA;
     DrawTextEx = DrawTextExA;
     GrayString = GrayStringA;
     DrawState = DrawStateA;
     TabbedTextOut = TabbedTextOutA;
     GetTabbedTextExtent = GetTabbedTextExtentA;
     GetVersionEx = GetVersionExA;
     wvsprintf = wvsprintfA;
     wsprintf = wsprintfA;
     LoadKeyboardLayout = LoadKeyboardLayoutA;
     GetKeyboardLayoutName = GetKeyboardLayoutNameA;
     CreateDesktop = CreateDesktopA;
     OpenDesktop = OpenDesktopA;
     EnumDesktops = EnumDesktopsA;
     CreateWindowStation = CreateWindowStationA;
     OpenWindowStation = OpenWindowStationA;
     EnumWindowStations = EnumWindowStationsA;
     IsBadStringPtr = IsBadStringPtrA;
     LookupAccountSid = LookupAccountSidA;
     LookupAccountName = LookupAccountNameA;
     LookupPrivilegeValue = LookupPrivilegeValueA;
     LookupPrivilegeName = LookupPrivilegeNameA;
     LookupPrivilegeDisplayName = LookupPrivilegeDisplayNameA;
     BuildCommDCB = BuildCommDCBA;
     BuildCommDCBAndTimeouts = BuildCommDCBAndTimeoutsA;
     CommConfigDialog = CommConfigDialogA;
     GetDefaultCommConfig = GetDefaultCommConfigA;
     SetDefaultCommConfig = SetDefaultCommConfigA;
     GetComputerName = GetComputerNameA;
     SetComputerName = SetComputerNameA;
     GetUserName = GetUserNameA;
     CreateMailslot = CreateMailslotA;
     FormatMessage = FormatMessageA;
     GetEnvironmentStrings = GetEnvironmentStringsA;
     FreeEnvironmentStrings = FreeEnvironmentStringsA;
     lstrcmp = lstrcmpA;
     lstrcmpi = lstrcmpiA;
     lstrcpyn = lstrcpynA;
     lstrcpy = lstrcpyA;
     lstrcat = lstrcatA;
     lstrlen = lstrlenA;
     GetBinaryType = GetBinaryTypeA;
     GetShortPathName = GetShortPathNameA;
     SetFileSecurity = SetFileSecurityA;
     GetFileSecurity = GetFileSecurityA;
     FindFirstChangeNotification = FindFirstChangeNotificationA;
     AccessCheckAndAuditAlarm = AccessCheckAndAuditAlarmA;
     ObjectOpenAuditAlarm = ObjectOpenAuditAlarmA;
     ObjectPrivilegeAuditAlarm = ObjectPrivilegeAuditAlarmA;
     ObjectCloseAuditAlarm = ObjectCloseAuditAlarmA;
     PrivilegedServiceAuditAlarm = PrivilegedServiceAuditAlarmA;
     OpenEventLog = OpenEventLogA;
     RegisterEventSource = RegisterEventSourceA;
     OpenBackupEventLog = OpenBackupEventLogA;
     ReadEventLog = ReadEventLogA;
     ReportEvent = ReportEventA;
     CreateProcess = CreateProcessA;
     FatalAppExit = FatalAppExitA;
     GetStartupInfo = GetStartupInfoA;
     GetCommandLine = GetCommandLineA;
     GetEnvironmentVariable = GetEnvironmentVariableA;
     SetEnvironmentVariable = SetEnvironmentVariableA;
     ExpandEnvironmentStrings = ExpandEnvironmentStringsA;
     OutputDebugString = OutputDebugStringA;
     FindResource = FindResourceA;
     FindResourceEx = FindResourceExA;
     EnumResourceTypes = EnumResourceTypesA;
     EnumResourceNames = EnumResourceNamesA;
     EnumResourceLanguages = EnumResourceLanguagesA;
     BeginUpdateResource = BeginUpdateResourceA;
     UpdateResource = UpdateResourceA;
     EndUpdateResource = EndUpdateResourceA;
     GlobalAddAtom = GlobalAddAtomA;
     GlobalFindAtom = GlobalFindAtomA;
     GlobalGetAtomName = GlobalGetAtomNameA;
     AddAtom = AddAtomA;
     FindAtom = FindAtomA;
     GetProfileInt = GetProfileIntA;
     GetAtomName = GetAtomNameA;
     GetProfileString = GetProfileStringA;
     WriteProfileString = WriteProfileStringA;
     GetProfileSection = GetProfileSectionA;
     WriteProfileSection = WriteProfileSectionA;
     GetPrivateProfileInt = GetPrivateProfileIntA;
     GetPrivateProfileString = GetPrivateProfileStringA;
     WritePrivateProfileString = WritePrivateProfileStringA;
     GetPrivateProfileSection = GetPrivateProfileSectionA;
     WritePrivateProfileSection = WritePrivateProfileSectionA;
     GetDriveType = GetDriveTypeA;
     GetSystemDirectory = GetSystemDirectoryA;
     GetTempPath = GetTempPathA;
     GetTempFileName = GetTempFileNameA;
     GetWindowsDirectory = GetWindowsDirectoryA;
     SetCurrentDirectory = SetCurrentDirectoryA;
     GetCurrentDirectory = GetCurrentDirectoryA;
     GetDiskFreeSpace = GetDiskFreeSpaceA;
     CreateDirectory = CreateDirectoryA;
     CreateDirectoryEx = CreateDirectoryExA;
     RemoveDirectory = RemoveDirectoryA;
     GetFullPathName = GetFullPathNameA;
     DefineDosDevice = DefineDosDeviceA;
     QueryDosDevice = QueryDosDeviceA;
     CreateFile = CreateFileA;
     SetFileAttributes = SetFileAttributesA;
     GetFileAttributes = GetFileAttributesA;
     GetCompressedFileSize = GetCompressedFileSizeA;
     DeleteFile = DeleteFileA;
     FindFirstFile = FindFirstFileA;
     FindNextFile = FindNextFileA;
     SearchPath = SearchPathA;
     CopyFile = CopyFileA;
     MoveFile = MoveFileA;
     MoveFileEx = MoveFileExA;
     CreateNamedPipe = CreateNamedPipeA;
     GetNamedPipeHandleState = GetNamedPipeHandleStateA;
     CallNamedPipe = CallNamedPipeA;
     WaitNamedPipe = WaitNamedPipeA;
     SetVolumeLabel = SetVolumeLabelA;
     GetVolumeInformation = GetVolumeInformationA;
     ClearEventLog = ClearEventLogA;
     BackupEventLog = BackupEventLogA;
     CreateMutex = CreateMutexA;
     OpenMutex = OpenMutexA;
     CreateEvent = CreateEventA;
     OpenEvent = OpenEventA;
     CreateSemaphore = CreateSemaphoreA;
     OpenSemaphore = OpenSemaphoreA;
     CreateFileMapping = CreateFileMappingA;
     OpenFileMapping = OpenFileMappingA;
     GetLogicalDriveStrings = GetLogicalDriveStringsA;
     LoadLibrary = LoadLibraryA;
     LoadLibraryEx = LoadLibraryExA;
     GetModuleFileName = GetModuleFileNameA;
     GetModuleHandle = GetModuleHandleA;
     GetUserObjectInformation = GetUserObjectInformationA;
     SetUserObjectInformation = SetUserObjectInformationA;
     RegisterWindowMessage = RegisterWindowMessageA;
     GetMessage = GetMessageA;
     DispatchMessage = DispatchMessageA;
     PeekMessage = PeekMessageA;
     SendMessage = SendMessageA;
     SendMessageTimeout = SendMessageTimeoutA;
     SendNotifyMessage = SendNotifyMessageA;
     SendMessageCallback = SendMessageCallbackA;
     PostMessage = PostMessageA;
     PostThreadMessage = PostThreadMessageA;
     VerFindFile = VerFindFileA;
     VerInstallFile = VerInstallFileA;
     GetFileVersionInfoSize = GetFileVersionInfoSizeA;
     GetFileVersionInfo = GetFileVersionInfoA;
     VerLanguageName = VerLanguageNameA;
     VerQueryValue = VerQueryValueA;
     CompareString = CompareStringA;
     LCMapString = LCMapStringA;
     GetLocaleInfo = GetLocaleInfoA;
     SetLocaleInfo = SetLocaleInfoA;
     GetTimeFormat = GetTimeFormatA;
     GetDateFormat = GetDateFormatA;
     GetNumberFormat = GetNumberFormatA;
     GetCurrencyFormat = GetCurrencyFormatA;
     EnumCalendarInfo = EnumCalendarInfoA;
     EnumTimeFormats = EnumTimeFormatsA;
     FoldString = FoldStringA;
     EnumSystemCodePages = EnumSystemCodePagesA;
     EnumSystemLocales = EnumSystemLocalesA;
     GetStringTypeEx = GetStringTypeExA;
     EnumDateFormats = EnumDateFormatsA;
     GetConsoleTitle = GetConsoleTitleA;
     ScrollConsoleScreenBuffer = ScrollConsoleScreenBufferA;
     SetConsoleTitle = SetConsoleTitleA;
     ReadConsole = ReadConsoleA;
     WriteConsole = WriteConsoleA;
     PeekConsoleInput = PeekConsoleInputA;
     ReadConsoleInput = ReadConsoleInputA;
     WriteConsoleInput = WriteConsoleInputA;
     ReadConsoleOutput = ReadConsoleOutputA;
     WriteConsoleOutput = WriteConsoleOutputA;
     ReadConsoleOutputCharacter = ReadConsoleOutputCharacterA;
     WriteConsoleOutputCharacter = WriteConsoleOutputCharacterA;
     FillConsoleOutputCharacter = FillConsoleOutputCharacterA;
     MultinetGetConnectionPerformance = MultinetGetConnectionPerformanceA;
     WNetGetLastError = WNetGetLastErrorA;
     WNetGetProviderName = WNetGetProviderNameA;
     WNetGetNetworkInformation = WNetGetNetworkInformationA;
     WNetConnectionDialog1 = WNetConnectionDialog1A;
     WNetDisconnectDialog1 = WNetDisconnectDialog1A;
     WNetOpenEnum = WNetOpenEnumA;
     WNetEnumResource = WNetEnumResourceA;
     WNetGetUniversalName = WNetGetUniversalNameA;
     WNetGetUser = WNetGetUserA;
     WNetAddConnection = WNetAddConnectionA;
     WNetAddConnection2 = WNetAddConnection2A;
     WNetAddConnection3 = WNetAddConnection3A;
     WNetCancelConnection = WNetCancelConnectionA;
     WNetCancelConnection2 = WNetCancelConnection2A;
     WNetGetConnection = WNetGetConnectionA;
     WNetUseConnection = WNetUseConnectionA;
     WNetSetConnection = WNetSetConnectionA;
     OpenService = OpenServiceA;
     QueryServiceConfig = QueryServiceConfigA;
     QueryServiceLockStatus = QueryServiceLockStatusA;
     RegisterServiceCtrlHandler = RegisterServiceCtrlHandlerA;
     StartServiceCtrlDispatcher = StartServiceCtrlDispatcherA;
     StartService = StartServiceA;
     ChangeServiceConfig = ChangeServiceConfigA;
     CreateService = CreateServiceA;
     EnumDependentServices = EnumDependentServicesA;
     EnumServicesStatus = EnumServicesStatusA;
     GetServiceKeyName = GetServiceKeyNameA;
     GetServiceDisplayName = GetServiceDisplayNameA;
     OpenSCManager = OpenSCManagerA;
     DragQueryFile = DragQueryFileA;
     ExtractAssociatedIcon = ExtractAssociatedIconA;
     ExtractIcon = ExtractIconA;
     FindExecutable = FindExecutableA;
     ShellAbout = ShellAboutA;
     ShellExecute = ShellExecuteA;
     DdeCreateStringHandle = DdeCreateStringHandleA;
     DdeInitialize = DdeInitializeA;
     DdeQueryString = DdeQueryStringA;
     LogonUser = LogonUserA;
     CreateProcessAsUser = CreateProcessAsUserA;
{$endif}
  { UNICODE and ASCII defines  }  *)

{$ifdef Unknown_functions}
{ WARNING: function not found !!}
  function AbnormalTermination:WINBOOL;
{$endif Unknown_functions}

  function AbortDoc(_para1:HDC):longint;

  function AbortPath(_para1:HDC):WINBOOL;

  function AbortPrinter(_para1:HANDLE):WINBOOL;

{$ifdef Unknown_functions}
{ WARNING: function not found !!}
  function AbortProc(_para1:HDC; _para2:longint):WINBOOL;
{$endif Unknown_functions}


{$ifndef windows_include_files}
  function AbortSystemShutdown(_para1:LPTSTR):WINBOOL;
{$endif windows_include_files}

  function AccessCheck(pSecurityDescriptor:PSECURITY_DESCRIPTOR; ClientToken:HANDLE; DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; PrivilegeSet:PPRIVILEGE_SET;
             PrivilegeSetLength:LPDWORD; GrantedAccess:LPDWORD; AccessStatus:LPBOOL):WINBOOL;

{$ifndef windows_include_files}
  function AccessCheckAndAuditAlarm(SubsystemName:LPCTSTR; HandleId:LPVOID; ObjectTypeName:LPTSTR; ObjectName:LPTSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR;
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL;
             pfGenerateOnClose:LPBOOL):WINBOOL;
{$endif windows_include_files}

  function InterlockedIncrement(lpAddend:LPLONG):LONG;

  function InterlockedDecrement(lpAddend:LPLONG):LONG;

  function InterlockedExchange(Target:LPLONG; Value:LONG):LONG;

  function FreeResource(hResData:HGLOBAL):WINBOOL;

  function LockResource(hResData:HGLOBAL):LPVOID;

{$ifdef Unknown_functions}
{ WARNING: function not found !!}
  function WinMain(hInstance:HINST; hPrevInstance:HINST; lpCmdLine:LPSTR; nShowCmd:longint):longint;
{$endif Unknown_functions}


  function FreeLibrary(hLibModule:HINST):WINBOOL;

  procedure FreeLibraryAndExitThread(hLibModule:HMODULE; dwExitCode:DWORD);

  function DisableThreadLibraryCalls(hLibModule:HMODULE):WINBOOL;

  function GetProcAddress(hModule:HINST; lpProcName:LPCSTR):FARPROC;

  function GetVersion:DWORD;

  function GlobalAlloc(uFlags:UINT; dwBytes:DWORD):HGLOBAL;

  function GlobalDiscard(hglbMem:HGLOBAL):HGLOBAL;

  function GlobalReAlloc(hMem:HGLOBAL; dwBytes:DWORD; uFlags:UINT):HGLOBAL;

  function GlobalSize(hMem:HGLOBAL):DWORD;

  function GlobalFlags(hMem:HGLOBAL):UINT;

  function GlobalLock(hMem:HGLOBAL):LPVOID;

  function GlobalHandle(pMem:LPCVOID):HGLOBAL;

  function GlobalUnlock(hMem:HGLOBAL):WINBOOL;

  function GlobalFree(hMem:HGLOBAL):HGLOBAL;

  function GlobalCompact(dwMinFree:DWORD):UINT;

  procedure GlobalFix(hMem:HGLOBAL);

  procedure GlobalUnfix(hMem:HGLOBAL);

  function GlobalWire(hMem:HGLOBAL):LPVOID;

  function GlobalUnWire(hMem:HGLOBAL):WINBOOL;

  procedure GlobalMemoryStatus(lpBuffer:LPMEMORYSTATUS);

  function LocalAlloc(uFlags:UINT; uBytes:UINT):HLOCAL;

  function LocalDiscard(hlocMem:HLOCAL):HLOCAL;

  function LocalReAlloc(hMem:HLOCAL; uBytes:UINT; uFlags:UINT):HLOCAL;

  function LocalLock(hMem:HLOCAL):LPVOID;

  function LocalHandle(pMem:LPCVOID):HLOCAL;

  function LocalUnlock(hMem:HLOCAL):WINBOOL;

  function LocalSize(hMem:HLOCAL):UINT;

  function LocalFlags(hMem:HLOCAL):UINT;

  function LocalFree(hMem:HLOCAL):HLOCAL;

  function LocalShrink(hMem:HLOCAL; cbNewSize:UINT):UINT;

  function LocalCompact(uMinFree:UINT):UINT;

  function FlushInstructionCache(hProcess:HANDLE; lpBaseAddress:LPCVOID; dwSize:DWORD):WINBOOL;

  function VirtualAlloc(lpAddress:LPVOID; dwSize:DWORD; flAllocationType:DWORD; flProtect:DWORD):LPVOID;

  function VirtualFree(lpAddress:LPVOID; dwSize:DWORD; dwFreeType:DWORD):WINBOOL;

  function VirtualProtect(lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect:PDWORD):WINBOOL;

  function VirtualQuery(lpAddress:LPCVOID; lpBuffer:PMEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD;

  function VirtualQuery(lpAddress:LPCVOID;var Buffer:MEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD;

  function VirtualProtectEx(hProcess:HANDLE; lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect:PDWORD):WINBOOL;

  function VirtualQueryEx(hProcess:HANDLE; lpAddress:LPCVOID; lpBuffer:PMEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD;

  function VirtualQueryEx(hProcess:HANDLE; lpAddress:LPCVOID; var Buffer:MEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD;

  function HeapCreate(flOptions:DWORD; dwInitialSize:DWORD; dwMaximumSize:DWORD):HANDLE;

  function HeapDestroy(hHeap:HANDLE):WINBOOL;

  function HeapAlloc(hHeap:HANDLE; dwFlags:DWORD; dwBytes:DWORD):LPVOID;

  function HeapReAlloc(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPVOID; dwBytes:DWORD):LPVOID;

  function HeapFree(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPVOID):WINBOOL;

  function HeapSize(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPCVOID):DWORD;

  function HeapValidate(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPCVOID):WINBOOL;

  function HeapCompact(hHeap:HANDLE; dwFlags:DWORD):UINT;

  function GetProcessHeap:HANDLE;

  function GetProcessHeaps(NumberOfHeaps:DWORD; ProcessHeaps:PHANDLE):DWORD;

  function HeapLock(hHeap:HANDLE):WINBOOL;

  function HeapUnlock(hHeap:HANDLE):WINBOOL;

  function HeapWalk(hHeap:HANDLE; lpEntry:LPPROCESS_HEAP_ENTRY):WINBOOL;

  function GetProcessAffinityMask(hProcess:HANDLE; lpProcessAffinityMask:LPDWORD; lpSystemAffinityMask:LPDWORD):WINBOOL;

  function GetProcessTimes(hProcess:HANDLE; lpCreationTime:LPFILETIME; lpExitTime:LPFILETIME; lpKernelTime:LPFILETIME; lpUserTime:LPFILETIME):WINBOOL;

  function GetProcessWorkingSetSize(hProcess:HANDLE; lpMinimumWorkingSetSize:LPDWORD; lpMaximumWorkingSetSize:LPDWORD):WINBOOL;

  function SetProcessWorkingSetSize(hProcess:HANDLE; dwMinimumWorkingSetSize:DWORD; dwMaximumWorkingSetSize:DWORD):WINBOOL;

  function OpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE;

  function GetCurrentProcess:HANDLE;

  function GetCurrentProcessId:DWORD;

(* error
STDCALL
ExitProcess(
 in declarator_list

    var
 : void'; *)
  procedure ExitProcess(uExitCode:UINT);

  function TerminateProcess(hProcess:HANDLE; uExitCode:UINT):WINBOOL;

  function GetExitCodeProcess(hProcess:HANDLE; lpExitCode:LPDWORD):WINBOOL;

  procedure FatalExit(ExitCode:longint);

(* Const before type ignored *)
  procedure RaiseException(dwExceptionCode:DWORD; dwExceptionFlags:DWORD; nNumberOfArguments:DWORD; var lpArguments:DWORD);

  function UnhandledExceptionFilter(var ExceptionInfo:emptyrecord):LONG;

  {
   TODO: what is TOP_LEVEL_EXCEPTION_FILTER?
  LPTOP_LEVEL_EXCEPTION_FILTER
  STDCALL
  SetUnhandledExceptionFilter(
      LPTOP_LEVEL_EXCEPTION_FILTER lpTopLevelExceptionFilter
      );
   }
  function CreateThread(lpThreadAttributes:LPSECURITY_ATTRIBUTES; dwStackSize:DWORD; lpStartAddress:LPTHREAD_START_ROUTINE; lpParameter:LPVOID; dwCreationFlags:DWORD;
                var lpThreadId:DWORD):HANDLE;

  function CreateRemoteThread(hProcess:HANDLE; lpThreadAttributes:LPSECURITY_ATTRIBUTES; dwStackSize:DWORD; lpStartAddress:LPTHREAD_START_ROUTINE; lpParameter:LPVOID;
             dwCreationFlags:DWORD; lpThreadId:LPDWORD):HANDLE;

  function GetCurrentThread:HANDLE;

  function GetCurrentThreadId:DWORD;

  function SetThreadAffinityMask(hThread:HANDLE; dwThreadAffinityMask:DWORD):DWORD;

  function SetThreadPriority(hThread:HANDLE; nPriority:longint):WINBOOL;

  function GetThreadPriority(hThread:HANDLE):longint;

  function GetThreadTimes(hThread:HANDLE; lpCreationTime:LPFILETIME; lpExitTime:LPFILETIME; lpKernelTime:LPFILETIME; lpUserTime:LPFILETIME):WINBOOL;

  procedure ExitThread(dwExitCode:DWORD);

  function TerminateThread(hThread:HANDLE; dwExitCode:DWORD):WINBOOL;

  function GetExitCodeThread(hThread:HANDLE; lpExitCode:LPDWORD):WINBOOL;

  function GetThreadSelectorEntry(hThread:HANDLE; dwSelector:DWORD; lpSelectorEntry:LPLDT_ENTRY):WINBOOL;

  function GetLastError:DWORD;

  procedure SetLastError(dwErrCode:DWORD);

  function GetOverlappedResult(hFile:HANDLE; const lpOverlapped:TOVERLAPPED; var lpNumberOfBytesTransferred:DWORD; bWait:WINBOOL):WINBOOL;

  function CreateIoCompletionPort(FileHandle:HANDLE; ExistingCompletionPort:HANDLE; CompletionKey:DWORD; NumberOfConcurrentThreads:DWORD):HANDLE;

  function GetQueuedCompletionStatus(CompletionPort:HANDLE; lpNumberOfBytesTransferred:LPDWORD; lpCompletionKey:LPDWORD; var lpOverlapped:LPOVERLAPPED; dwMilliseconds:DWORD):WINBOOL;

  function SetErrorMode(uMode:UINT):UINT;

  function ReadProcessMemory(hProcess:HANDLE; lpBaseAddress:LPCVOID; lpBuffer:LPVOID; nSize:DWORD; lpNumberOfBytesRead:LPDWORD):WINBOOL;

  function WriteProcessMemory(hProcess:HANDLE; lpBaseAddress:LPVOID; lpBuffer:LPVOID; nSize:DWORD; lpNumberOfBytesWritten:LPDWORD):WINBOOL;

  function GetThreadContext(hThread:HANDLE; lpContext:LPCONTEXT):WINBOOL;

(* Const before type ignored *)
  function SetThreadContext(hThread:HANDLE; var lpContext:CONTEXT):WINBOOL;

  function SuspendThread(hThread:HANDLE):DWORD;

  function ResumeThread(hThread:HANDLE):DWORD;

  procedure DebugBreak;

  function WaitForDebugEvent(lpDebugEvent:LPDEBUG_EVENT; dwMilliseconds:DWORD):WINBOOL;

  function ContinueDebugEvent(dwProcessId:DWORD; dwThreadId:DWORD; dwContinueStatus:DWORD):WINBOOL;

  function DebugActiveProcess(dwProcessId:DWORD):WINBOOL;

  procedure InitializeCriticalSection(lpCriticalSection:LPCRITICAL_SECTION);

  procedure EnterCriticalSection(lpCriticalSection:LPCRITICAL_SECTION);

  procedure LeaveCriticalSection(lpCriticalSection:LPCRITICAL_SECTION);

  procedure DeleteCriticalSection(lpCriticalSection:LPCRITICAL_SECTION);

  function SetEvent(hEvent:HANDLE):WINBOOL;

  function ResetEvent(hEvent:HANDLE):WINBOOL;

  function PulseEvent(hEvent:HANDLE):WINBOOL;

  function ReleaseSemaphore(hSemaphore:HANDLE; lReleaseCount:LONG; lpPreviousCount:LPLONG):WINBOOL;

  function ReleaseMutex(hMutex:HANDLE):WINBOOL;

  function WaitForSingleObject(hHandle:HANDLE; dwMilliseconds:DWORD):DWORD;

(* Const before type ignored *)
  function WaitForMultipleObjects(nCount:DWORD; var lpHandles:HANDLE; bWaitAll:WINBOOL; dwMilliseconds:DWORD):DWORD;

  procedure Sleep(dwMilliseconds:DWORD);

  function LoadResource(hModule:HINST; hResInfo:HRSRC):HGLOBAL;

  function SizeofResource(hModule:HINST; hResInfo:HRSRC):DWORD;

  function GlobalDeleteAtom(nAtom:ATOM):ATOM;

  function InitAtomTable(nSize:DWORD):WINBOOL;

  function DeleteAtom(nAtom:ATOM):ATOM;

  function SetHandleCount(uNumber:UINT):UINT;

  function GetLogicalDrives:DWORD;

  function LockFile(hFile:HANDLE; dwFileOffsetLow:DWORD; dwFileOffsetHigh:DWORD; nNumberOfBytesToLockLow:DWORD; nNumberOfBytesToLockHigh:DWORD):WINBOOL;

  function UnlockFile(hFile:HANDLE; dwFileOffsetLow:DWORD; dwFileOffsetHigh:DWORD; nNumberOfBytesToUnlockLow:DWORD; nNumberOfBytesToUnlockHigh:DWORD):WINBOOL;

  function LockFileEx(hFile:HANDLE; dwFlags:DWORD; dwReserved:DWORD; nNumberOfBytesToLockLow:DWORD; nNumberOfBytesToLockHigh:DWORD;
             lpOverlapped:LPOVERLAPPED):WINBOOL;

  function UnlockFileEx(hFile:HANDLE; dwReserved:DWORD; nNumberOfBytesToUnlockLow:DWORD; nNumberOfBytesToUnlockHigh:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function GetFileInformationByHandle(hFile:HANDLE; lpFileInformation:LPBY_HANDLE_FILE_INFORMATION):WINBOOL;

  function GetFileType(hFile:HANDLE):DWORD;

  function GetFileSize(hFile:HANDLE; lpFileSizeHigh:LPDWORD):DWORD;

  function GetStdHandle(nStdHandle:DWORD):HANDLE;

  function SetStdHandle(nStdHandle:DWORD; hHandle:HANDLE):WINBOOL;

  function WriteFile(hFile:HANDLE; var lpBuffer; nNumberOfBytesToWrite:DWORD; var lpNumberOfBytesWritten:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function ReadFile(hFile:HANDLE; var lpBuffer; nNumberOfBytesToRead:DWORD; var lpNumberOfBytesRead:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function FlushFileBuffers(hFile:HANDLE):WINBOOL;

  function DeviceIoControl(hDevice:HANDLE; dwIoControlCode:DWORD; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID;
             nOutBufferSize:DWORD; lpBytesReturned:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function SetEndOfFile(hFile:HANDLE):WINBOOL;

  function SetFilePointer(hFile:HANDLE; lDistanceToMove:LONG; lpDistanceToMoveHigh:PLONG; dwMoveMethod:DWORD):DWORD;

  function FindClose(hFindFile:HANDLE):WINBOOL;

  function GetFileTime(hFile:HANDLE; lpCreationTime:LPFILETIME; lpLastAccessTime:LPFILETIME; lpLastWriteTime:LPFILETIME):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function SetFileTime(hFile:HANDLE; var lpCreationTime:FILETIME; var lpLastAccessTime:FILETIME; var lpLastWriteTime:FILETIME):WINBOOL;

  function CloseHandle(hObject:HANDLE):WINBOOL;

  function DuplicateHandle(hSourceProcessHandle:HANDLE; hSourceHandle:HANDLE; hTargetProcessHandle:HANDLE; lpTargetHandle:LPHANDLE; dwDesiredAccess:DWORD;
             bInheritHandle:WINBOOL; dwOptions:DWORD):WINBOOL;

  function GetHandleInformation(hObject:HANDLE; lpdwFlags:LPDWORD):WINBOOL;

  function SetHandleInformation(hObject:HANDLE; dwMask:DWORD; dwFlags:DWORD):WINBOOL;

  function LoadModule(lpModuleName:LPCSTR; lpParameterBlock:LPVOID):DWORD;

  function WinExec(lpCmdLine:LPCSTR; uCmdShow:UINT):UINT;

  function ClearCommBreak(hFile:HANDLE):WINBOOL;

  function ClearCommError(hFile:HANDLE; lpErrors:LPDWORD; lpStat:LPCOMSTAT):WINBOOL;

  function SetupComm(hFile:HANDLE; dwInQueue:DWORD; dwOutQueue:DWORD):WINBOOL;

  function EscapeCommFunction(hFile:HANDLE; dwFunc:DWORD):WINBOOL;

  function GetCommConfig(hCommDev:HANDLE; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL;

  function GetCommMask(hFile:HANDLE; var lpEvtMask: DWORD):WINBOOL;

  function GetCommProperties(hFile:HANDLE; var lpCommProp:TCOMMPROP):WINBOOL;

  function GetCommModemStatus(hFile:HANDLE; var lpModemStat:DWORD):WINBOOL;

  function GetCommState(hFile:HANDLE; var lpDCB:TDCB):WINBOOL;

  function GetCommTimeouts(hFile:HANDLE; var lpCommTimeouts:TCOMMTIMEOUTS):WINBOOL;

  function PurgeComm(hFile:HANDLE; dwFlags:DWORD):WINBOOL;

  function SetCommBreak(hFile:HANDLE):WINBOOL;

  function SetCommConfig(hCommDev:HANDLE; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL;

  function SetCommMask(hFile:HANDLE; dwEvtMask:DWORD):WINBOOL;

  function SetCommState(hFile:HANDLE; var lpDCB: TDCB):WINBOOL;

  function SetCommTimeouts(hFile:HANDLE; var lpCommTimeouts:TCOMMTIMEOUTS):WINBOOL;

  function TransmitCommChar(hFile:HANDLE; cChar:char):WINBOOL;

  function WaitCommEvent(hFile:HANDLE; var lpEvtMask:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function SetTapePosition(hDevice:HANDLE; dwPositionMethod:DWORD; dwPartition:DWORD; dwOffsetLow:DWORD; dwOffsetHigh:DWORD;
             bImmediate:WINBOOL):DWORD;

  function GetTapePosition(hDevice:HANDLE; dwPositionType:DWORD; lpdwPartition:LPDWORD; lpdwOffsetLow:LPDWORD; lpdwOffsetHigh:LPDWORD):DWORD;

  function PrepareTape(hDevice:HANDLE; dwOperation:DWORD; bImmediate:WINBOOL):DWORD;

  function EraseTape(hDevice:HANDLE; dwEraseType:DWORD; bImmediate:WINBOOL):DWORD;

  function CreateTapePartition(hDevice:HANDLE; dwPartitionMethod:DWORD; dwCount:DWORD; dwSize:DWORD):DWORD;

  function WriteTapemark(hDevice:HANDLE; dwTapemarkType:DWORD; dwTapemarkCount:DWORD; bImmediate:WINBOOL):DWORD;

  function GetTapeStatus(hDevice:HANDLE):DWORD;

  function GetTapeParameters(hDevice:HANDLE; dwOperation:DWORD; lpdwSize:LPDWORD; lpTapeInformation:LPVOID):DWORD;

  function SetTapeParameters(hDevice:HANDLE; dwOperation:DWORD; lpTapeInformation:LPVOID):DWORD;

  function Beep(dwFreq:DWORD; dwDuration:DWORD):WINBOOL;

{$ifdef Unknown_functions}
{ WARNING: functions not found !!}
  procedure OpenSound;

  procedure CloseSound;

  procedure StartSound;

  procedure StopSound;

  function WaitSoundState(nState:DWORD):DWORD;

  function SyncAllVoices:DWORD;

  function CountVoiceNotes(nVoice:DWORD):DWORD;

  function GetThresholdEvent:LPDWORD;

  function GetThresholdStatus:DWORD;

  function SetSoundNoise(nSource:DWORD; nDuration:DWORD):DWORD;

  function SetVoiceAccent(nVoice:DWORD; nTempo:DWORD; nVolume:DWORD; nMode:DWORD; nPitch:DWORD):DWORD;

  function SetVoiceEnvelope(nVoice:DWORD; nShape:DWORD; nRepeat:DWORD):DWORD;

  function SetVoiceNote(nVoice:DWORD; nValue:DWORD; nLength:DWORD; nCdots:DWORD):DWORD;

  function SetVoiceQueueSize(nVoice:DWORD; nBytes:DWORD):DWORD;

  function SetVoiceSound(nVoice:DWORD; Frequency:DWORD; nDuration:DWORD):DWORD;

  function SetVoiceThreshold(nVoice:DWORD; nNotes:DWORD):DWORD;
{$endif Unknown_functions}

  function MulDiv(nNumber:longint; nNumerator:longint; nDenominator:longint):longint;

  procedure GetSystemTime(lpSystemTime:LPSYSTEMTIME);

(* Const before type ignored *)
  function SetSystemTime(var lpSystemTime:SYSTEMTIME):WINBOOL;

  procedure GetLocalTime(lpSystemTime:LPSYSTEMTIME);

(* Const before type ignored *)
  function SetLocalTime(var lpSystemTime:SYSTEMTIME):WINBOOL;

  procedure GetSystemInfo(lpSystemInfo:LPSYSTEM_INFO);

  procedure GetSystemInfo(var SystemInfo:SYSTEM_INFO);

  function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION; lpUniversalTime:LPSYSTEMTIME; lpLocalTime:LPSYSTEMTIME):WINBOOL;

  function GetTimeZoneInformation(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION):DWORD;

(* Const before type ignored *)
  function SetTimeZoneInformation(var lpTimeZoneInformation:TIME_ZONE_INFORMATION):WINBOOL;

(* Const before type ignored *)
  function SystemTimeToFileTime(var lpSystemTime:SYSTEMTIME; lpFileTime:LPFILETIME):WINBOOL;

(* Const before type ignored *)
  function FileTimeToLocalFileTime(var lpFileTime:FILETIME; lpLocalFileTime:LPFILETIME):WINBOOL;

(* Const before type ignored *)
  function LocalFileTimeToFileTime(var lpLocalFileTime:FILETIME; lpFileTime:LPFILETIME):WINBOOL;

(* Const before type ignored *)
  function FileTimeToSystemTime(var lpFileTime:FILETIME; lpSystemTime:LPSYSTEMTIME):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function CompareFileTime(var lpFileTime1:FILETIME; var lpFileTime2:FILETIME):LONG;

(* Const before type ignored *)
  function FileTimeToDosDateTime(var lpFileTime:FILETIME; lpFatDate:LPWORD; lpFatTime:LPWORD):WINBOOL;

  function DosDateTimeToFileTime(wFatDate:WORD; wFatTime:WORD; lpFileTime:LPFILETIME):WINBOOL;

  function GetTickCount:DWORD;

  function SetSystemTimeAdjustment(dwTimeAdjustment:DWORD; bTimeAdjustmentDisabled:WINBOOL):WINBOOL;

  function GetSystemTimeAdjustment(lpTimeAdjustment:PDWORD; lpTimeIncrement:PDWORD; lpTimeAdjustmentDisabled:PWINBOOL):WINBOOL;

  function CreatePipe(hReadPipe:PHANDLE; hWritePipe:PHANDLE; lpPipeAttributes:LPSECURITY_ATTRIBUTES; nSize:DWORD):WINBOOL;

  function ConnectNamedPipe(hNamedPipe:HANDLE; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function DisconnectNamedPipe(hNamedPipe:HANDLE):WINBOOL;

  function SetNamedPipeHandleState(hNamedPipe:HANDLE; lpMode:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD):WINBOOL;

  function GetNamedPipeInfo(hNamedPipe:HANDLE; lpFlags:LPDWORD; lpOutBufferSize:LPDWORD; lpInBufferSize:LPDWORD; lpMaxInstances:LPDWORD):WINBOOL;

  function PeekNamedPipe(hNamedPipe:HANDLE; lpBuffer:LPVOID; nBufferSize:DWORD; lpBytesRead:LPDWORD; lpTotalBytesAvail:LPDWORD;
             lpBytesLeftThisMessage:LPDWORD):WINBOOL;

  function TransactNamedPipe(hNamedPipe:HANDLE; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;
             lpBytesRead:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL;

  function GetMailslotInfo(hMailslot:HANDLE; lpMaxMessageSize:LPDWORD; lpNextSize:LPDWORD; lpMessageCount:LPDWORD; lpReadTimeout:LPDWORD):WINBOOL;

  function SetMailslotInfo(hMailslot:HANDLE; lReadTimeout:DWORD):WINBOOL;

  function MapViewOfFile(hFileMappingObject:HANDLE; dwDesiredAccess:DWORD; dwFileOffsetHigh:DWORD; dwFileOffsetLow:DWORD; dwNumberOfBytesToMap:DWORD):LPVOID;

  function FlushViewOfFile(lpBaseAddress:LPCVOID; dwNumberOfBytesToFlush:DWORD):WINBOOL;

  function UnmapViewOfFile(lpBaseAddress:LPVOID):WINBOOL;

  function OpenFile(lpFileName:LPCSTR; lpReOpenBuff:LPOFSTRUCT; uStyle:UINT):HFILE;

  function _lopen(lpPathName:LPCSTR; iReadWrite:longint):HFILE;

  function _lcreat(lpPathName:LPCSTR; iAttribute:longint):HFILE;

  function _lread(hFile:HFILE; lpBuffer:LPVOID; uBytes:UINT):UINT;

  function _lwrite(hFile:HFILE; lpBuffer:LPCSTR; uBytes:UINT):UINT;

  function _hread(hFile:HFILE; lpBuffer:LPVOID; lBytes:longint):longint;

  function _hwrite(hFile:HFILE; lpBuffer:LPCSTR; lBytes:longint):longint;

  function _lclose(hFile:HFILE):HFILE;

  function _llseek(hFile:HFILE; lOffset:LONG; iOrigin:longint):LONG;

(* Const before type ignored *)
  function IsTextUnicode(lpBuffer:LPVOID; cb:longint; lpi:LPINT):WINBOOL;

  function TlsAlloc:DWORD;

  function TlsGetValue(dwTlsIndex:DWORD):LPVOID;

  function TlsSetValue(dwTlsIndex:DWORD; lpTlsValue:LPVOID):WINBOOL;

  function TlsFree(dwTlsIndex:DWORD):WINBOOL;

  function SleepEx(dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD;

  function WaitForSingleObjectEx(hHandle:HANDLE; dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD;

(* Const before type ignored *)
  function WaitForMultipleObjectsEx(nCount:DWORD; var lpHandles:HANDLE; bWaitAll:WINBOOL; dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD;

  function ReadFileEx(hFile:HANDLE; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; lpOverlapped:LPOVERLAPPED; lpCompletionRoutine:LPOVERLAPPED_COMPLETION_ROUTINE):WINBOOL;

  function WriteFileEx(hFile:HANDLE; lpBuffer:LPCVOID; nNumberOfBytesToWrite:DWORD; lpOverlapped:LPOVERLAPPED; lpCompletionRoutine:LPOVERLAPPED_COMPLETION_ROUTINE):WINBOOL;

  function BackupRead(hFile:HANDLE; lpBuffer:LPBYTE; nNumberOfBytesToRead:DWORD; lpNumberOfBytesRead:LPDWORD; bAbort:WINBOOL;
             bProcessSecurity:WINBOOL; var lpContext:LPVOID):WINBOOL;

  function BackupSeek(hFile:HANDLE; dwLowBytesToSeek:DWORD; dwHighBytesToSeek:DWORD; lpdwLowByteSeeked:LPDWORD; lpdwHighByteSeeked:LPDWORD;
             var lpContext:LPVOID):WINBOOL;

  function BackupWrite(hFile:HANDLE; lpBuffer:LPBYTE; nNumberOfBytesToWrite:DWORD; lpNumberOfBytesWritten:LPDWORD; bAbort:WINBOOL;
             bProcessSecurity:WINBOOL; var lpContext:LPVOID):WINBOOL;

  function SetProcessShutdownParameters(dwLevel:DWORD; dwFlags:DWORD):WINBOOL;

  function GetProcessShutdownParameters(lpdwLevel:LPDWORD; lpdwFlags:LPDWORD):WINBOOL;

  procedure SetFileApisToOEM;

  procedure SetFileApisToANSI;

  function AreFileApisANSI:WINBOOL;

  function CloseEventLog(hEventLog:HANDLE):WINBOOL;

  function DeregisterEventSource(hEventLog:HANDLE):WINBOOL;

  function NotifyChangeEventLog(hEventLog:HANDLE; hEvent:HANDLE):WINBOOL;

  function GetNumberOfEventLogRecords(hEventLog:HANDLE; NumberOfRecords:PDWORD):WINBOOL;

  function GetOldestEventLogRecord(hEventLog:HANDLE; OldestRecord:PDWORD):WINBOOL;

  function DuplicateToken(ExistingTokenHandle:HANDLE; ImpersonationLevel:SECURITY_IMPERSONATION_LEVEL; DuplicateTokenHandle:PHANDLE):WINBOOL;

  function GetKernelObjectSecurity(Handle:HANDLE; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

  function ImpersonateNamedPipeClient(hNamedPipe:HANDLE):WINBOOL;

  function ImpersonateLoggedOnUser(hToken:HANDLE):WINBOOL;

  function ImpersonateSelf(ImpersonationLevel:SECURITY_IMPERSONATION_LEVEL):WINBOOL;

  function RevertToSelf:WINBOOL;

  function SetThreadToken(Thread:PHANDLE; Token:HANDLE):WINBOOL;

(*  function AccessCheck(pSecurityDescriptor:PSECURITY_DESCRIPTOR; ClientToken:HANDLE; DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; PrivilegeSet:PPRIVILEGE_SET;
             PrivilegeSetLength:LPDWORD; GrantedAccess:LPDWORD; AccessStatus:LPBOOL):WINBOOL; *)

  function OpenProcessToken(ProcessHandle:HANDLE; DesiredAccess:DWORD; TokenHandle:PHANDLE):WINBOOL;

  function OpenThreadToken(ThreadHandle:HANDLE; DesiredAccess:DWORD; OpenAsSelf:WINBOOL; TokenHandle:PHANDLE):WINBOOL;

  function GetTokenInformation(TokenHandle:HANDLE; TokenInformationClass:TOKEN_INFORMATION_CLASS; TokenInformation:LPVOID; TokenInformationLength:DWORD; ReturnLength:PDWORD):WINBOOL;

  function SetTokenInformation(TokenHandle:HANDLE; TokenInformationClass:TOKEN_INFORMATION_CLASS; TokenInformation:LPVOID; TokenInformationLength:DWORD):WINBOOL;

  function AdjustTokenPrivileges(TokenHandle:HANDLE; DisableAllPrivileges:WINBOOL; NewState:PTOKEN_PRIVILEGES; BufferLength:DWORD; PreviousState:PTOKEN_PRIVILEGES;
             ReturnLength:PDWORD):WINBOOL;

  function AdjustTokenGroups(TokenHandle:HANDLE; ResetToDefault:WINBOOL; NewState:PTOKEN_GROUPS; BufferLength:DWORD; PreviousState:PTOKEN_GROUPS;
             ReturnLength:PDWORD):WINBOOL;

  function PrivilegeCheck(ClientToken:HANDLE; RequiredPrivileges:PPRIVILEGE_SET; pfResult:LPBOOL):WINBOOL;

  function IsValidSid(pSid:PSID):WINBOOL;

  function EqualSid(pSid1:PSID; pSid2:PSID):WINBOOL;

  function EqualPrefixSid(pSid1:PSID; pSid2:PSID):WINBOOL;

  function GetSidLengthRequired(nSubAuthorityCount:UCHAR):DWORD;

  function AllocateAndInitializeSid(pIdentifierAuthority:PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount:BYTE; nSubAuthority0:DWORD; nSubAuthority1:DWORD; nSubAuthority2:DWORD;
             nSubAuthority3:DWORD; nSubAuthority4:DWORD; nSubAuthority5:DWORD; nSubAuthority6:DWORD; nSubAuthority7:DWORD;
             var pSid:PSID):WINBOOL;

  function FreeSid(pSid:PSID):PVOID;

  function InitializeSid(Sid:PSID; pIdentifierAuthority:PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount:BYTE):WINBOOL;

  function GetSidIdentifierAuthority(pSid:PSID):PSID_IDENTIFIER_AUTHORITY;

  function GetSidSubAuthority(pSid:PSID; nSubAuthority:DWORD):PDWORD;

  function GetSidSubAuthorityCount(pSid:PSID):PUCHAR;

  function GetLengthSid(pSid:PSID):DWORD;

  function CopySid(nDestinationSidLength:DWORD; pDestinationSid:PSID; pSourceSid:PSID):WINBOOL;

  function AreAllAccessesGranted(GrantedAccess:DWORD; DesiredAccess:DWORD):WINBOOL;

  function AreAnyAccessesGranted(GrantedAccess:DWORD; DesiredAccess:DWORD):WINBOOL;

  procedure MapGenericMask(AccessMask:PDWORD; GenericMapping:PGENERIC_MAPPING);

  function IsValidAcl(pAcl:PACL):WINBOOL;

  function InitializeAcl(pAcl:PACL; nAclLength:DWORD; dwAclRevision:DWORD):WINBOOL;

  function GetAclInformation(pAcl:PACL; pAclInformation:LPVOID; nAclInformationLength:DWORD; dwAclInformationClass:ACL_INFORMATION_CLASS):WINBOOL;

  function SetAclInformation(pAcl:PACL; pAclInformation:LPVOID; nAclInformationLength:DWORD; dwAclInformationClass:ACL_INFORMATION_CLASS):WINBOOL;

  function AddAce(pAcl:PACL; dwAceRevision:DWORD; dwStartingAceIndex:DWORD; pAceList:LPVOID; nAceListLength:DWORD):WINBOOL;

  function DeleteAce(pAcl:PACL; dwAceIndex:DWORD):WINBOOL;

  function GetAce(pAcl:PACL; dwAceIndex:DWORD; var pAce:LPVOID):WINBOOL;

  function AddAccessAllowedAce(pAcl:PACL; dwAceRevision:DWORD; AccessMask:DWORD; pSid:PSID):WINBOOL;

  function AddAccessDeniedAce(pAcl:PACL; dwAceRevision:DWORD; AccessMask:DWORD; pSid:PSID):WINBOOL;

  function AddAuditAccessAce(pAcl:PACL; dwAceRevision:DWORD; dwAccessMask:DWORD; pSid:PSID; bAuditSuccess:WINBOOL;
             bAuditFailure:WINBOOL):WINBOOL;

  function FindFirstFreeAce(pAcl:PACL; var pAce:LPVOID):WINBOOL;

  function InitializeSecurityDescriptor(pSecurityDescriptor:PSECURITY_DESCRIPTOR; dwRevision:DWORD):WINBOOL;

  function IsValidSecurityDescriptor(pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function GetSecurityDescriptorLength(pSecurityDescriptor:PSECURITY_DESCRIPTOR):DWORD;

  function GetSecurityDescriptorControl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pControl:PSECURITY_DESCRIPTOR_CONTROL; lpdwRevision:LPDWORD):WINBOOL;

  function SetSecurityDescriptorDacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; bDaclPresent:WINBOOL; pDacl:PACL; bDaclDefaulted:WINBOOL):WINBOOL;

  function GetSecurityDescriptorDacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpbDaclPresent:LPBOOL; var pDacl:PACL; lpbDaclDefaulted:LPBOOL):WINBOOL;

  function SetSecurityDescriptorSacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; bSaclPresent:WINBOOL; pSacl:PACL; bSaclDefaulted:WINBOOL):WINBOOL;

  function GetSecurityDescriptorSacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpbSaclPresent:LPBOOL; var pSacl:PACL; lpbSaclDefaulted:LPBOOL):WINBOOL;

  function SetSecurityDescriptorOwner(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pOwner:PSID; bOwnerDefaulted:WINBOOL):WINBOOL;

  function GetSecurityDescriptorOwner(pSecurityDescriptor:PSECURITY_DESCRIPTOR; var pOwner:PSID; lpbOwnerDefaulted:LPBOOL):WINBOOL;

  function SetSecurityDescriptorGroup(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pGroup:PSID; bGroupDefaulted:WINBOOL):WINBOOL;

  function GetSecurityDescriptorGroup(pSecurityDescriptor:PSECURITY_DESCRIPTOR; var pGroup:PSID; lpbGroupDefaulted:LPBOOL):WINBOOL;

  function CreatePrivateObjectSecurity(ParentDescriptor:PSECURITY_DESCRIPTOR; CreatorDescriptor:PSECURITY_DESCRIPTOR; var NewDescriptor:PSECURITY_DESCRIPTOR; IsDirectoryObject:WINBOOL; Token:HANDLE;
             GenericMapping:PGENERIC_MAPPING):WINBOOL;

  function SetPrivateObjectSecurity(SecurityInformation:SECURITY_INFORMATION; ModificationDescriptor:PSECURITY_DESCRIPTOR; var ObjectsSecurityDescriptor:PSECURITY_DESCRIPTOR; GenericMapping:PGENERIC_MAPPING; Token:HANDLE):WINBOOL;

  function GetPrivateObjectSecurity(ObjectDescriptor:PSECURITY_DESCRIPTOR; SecurityInformation:SECURITY_INFORMATION; ResultantDescriptor:PSECURITY_DESCRIPTOR; DescriptorLength:DWORD; ReturnLength:PDWORD):WINBOOL;

  function DestroyPrivateObjectSecurity(var ObjectDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function MakeSelfRelativeSD(pAbsoluteSecurityDescriptor:PSECURITY_DESCRIPTOR; pSelfRelativeSecurityDescriptor:PSECURITY_DESCRIPTOR; lpdwBufferLength:LPDWORD):WINBOOL;

  function MakeAbsoluteSD(pSelfRelativeSecurityDescriptor:PSECURITY_DESCRIPTOR; pAbsoluteSecurityDescriptor:PSECURITY_DESCRIPTOR; lpdwAbsoluteSecurityDescriptorSize:LPDWORD; pDacl:PACL; lpdwDaclSize:LPDWORD;
             pSacl:PACL; lpdwSaclSize:LPDWORD; pOwner:PSID; lpdwOwnerSize:LPDWORD; pPrimaryGroup:PSID;
             lpdwPrimaryGroupSize:LPDWORD):WINBOOL;

  function SetKernelObjectSecurity(Handle:HANDLE; SecurityInformation:SECURITY_INFORMATION; SecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

  function FindNextChangeNotification(hChangeHandle:HANDLE):WINBOOL;

  function FindCloseChangeNotification(hChangeHandle:HANDLE):WINBOOL;

  function VirtualLock(lpAddress:LPVOID; dwSize:DWORD):WINBOOL;

  function VirtualUnlock(lpAddress:LPVOID; dwSize:DWORD):WINBOOL;

  function MapViewOfFileEx(hFileMappingObject:HANDLE; dwDesiredAccess:DWORD; dwFileOffsetHigh:DWORD; dwFileOffsetLow:DWORD; dwNumberOfBytesToMap:DWORD;
             lpBaseAddress:LPVOID):LPVOID;

  function SetPriorityClass(hProcess:HANDLE; dwPriorityClass:DWORD):WINBOOL;

  function GetPriorityClass(hProcess:HANDLE):DWORD;

(* Const before type ignored *)
  function IsBadReadPtr(lp:pointer; ucb:UINT):WINBOOL;

  function IsBadWritePtr(lp:LPVOID; ucb:UINT):WINBOOL;

(* Const before type ignored *)
  function IsBadHugeReadPtr(lp:pointer; ucb:UINT):WINBOOL;

  function IsBadHugeWritePtr(lp:LPVOID; ucb:UINT):WINBOOL;

  function IsBadCodePtr(lpfn:FARPROC):WINBOOL;

  function AllocateLocallyUniqueId(Luid:PLUID):WINBOOL;

  function QueryPerformanceCounter(var lpPerformanceCount:LARGE_INTEGER):WINBOOL;

  function QueryPerformanceFrequency(var lpFrequency:LARGE_INTEGER):WINBOOL;

(* Const before type ignored *)
  procedure MoveMemory(Destination:PVOID; Source:pointer; Length:DWORD);

  { from Delphi interface }
  procedure CopyMemory(Destination:PVOID; Source:pointer; Length:DWORD);

  procedure FillMemory(Destination:PVOID; Length:DWORD; Fill:BYTE);

  procedure ZeroMemory(Destination:PVOID; Length:DWORD);

(*  { The memory functions don't seem to be defined in the libraries, so
     define macro versions as well.   }
  { was #define dname(params) def_expr }
  procedure MoveMemory(var t,s; c : longint);

  { was #define dname(params) def_expr }
  procedure FillMemory(var p;c,v : longint);

  { was #define dname(params) def_expr }
  procedure ZeroMemory(var p;c : longint); *)

{$ifdef WIN95}

  function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):HKL;

{$else}

  function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):WINBOOL;

{$endif}
  { WIN95  }

{ Not in my user32 !!! PM
  function ToUnicodeEx(wVirtKey:UINT; wScanCode:UINT; lpKeyState:PBYTE; pwszBuff:LPWSTR; cchBuff:longint;
             wFlags:UINT; dwhkl:HKL):longint;
}
  function UnloadKeyboardLayout(hkl:HKL):WINBOOL;

  function GetKeyboardLayoutList(nBuff:longint; var lpList:HKL):longint;

  function GetKeyboardLayout(dwLayout:DWORD):HKL;

  function OpenInputDesktop(dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK;

  function EnumDesktopWindows(hDesktop:HDESK; lpfn:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL;

  function SwitchDesktop(hDesktop:HDESK):WINBOOL;

  function SetThreadDesktop(hDesktop:HDESK):WINBOOL;

  function CloseDesktop(hDesktop:HDESK):WINBOOL;

  function GetThreadDesktop(dwThreadId:DWORD):HDESK;

  function CloseWindowStation(hWinSta:HWINSTA):WINBOOL;

  function SetProcessWindowStation(hWinSta:HWINSTA):WINBOOL;

  function GetProcessWindowStation:HWINSTA;

  function SetUserObjectSecurity(hObj:HANDLE; pSIRequested:PSECURITY_INFORMATION; pSID:PSECURITY_DESCRIPTOR):WINBOOL;

  function GetUserObjectSecurity(hObj:HANDLE; pSIRequested:PSECURITY_INFORMATION; pSID:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL;

(* Const before type ignored *)
  function TranslateMessage(var lpMsg:MSG):WINBOOL;

  function SetMessageQueue(cMessagesMax:longint):WINBOOL;

  function RegisterHotKey(hWnd:HWND; anID:longint; fsModifiers:UINT; vk:UINT):WINBOOL;

  function UnregisterHotKey(hWnd:HWND; anID:longint):WINBOOL;

  function ExitWindowsEx(uFlags:UINT; dwReserved:DWORD):WINBOOL;

  function SwapMouseButton(fSwap:WINBOOL):WINBOOL;

  function GetMessagePos:DWORD;

  function GetMessageTime:LONG;

  function GetMessageExtraInfo:LONG;

  function SetMessageExtraInfo(lParam:LPARAM):LPARAM;

  function BroadcastSystemMessage(_para1:DWORD; _para2:LPDWORD; _para3:UINT; _para4:WPARAM; _para5:LPARAM):longint;

  function AttachThreadInput(idAttach:DWORD; idAttachTo:DWORD; fAttach:WINBOOL):WINBOOL;

  function ReplyMessage(lResult:LRESULT):WINBOOL;

  function WaitMessage:WINBOOL;

  function WaitForInputIdle(hProcess:HANDLE; dwMilliseconds:DWORD):DWORD;

  procedure PostQuitMessage(nExitCode:longint);

  function InSendMessage:WINBOOL;

  function GetDoubleClickTime:UINT;

  function SetDoubleClickTime(_para1:UINT):WINBOOL;

  function IsWindow(hWnd:HWND):WINBOOL;

  function IsMenu(hMenu:HMENU):WINBOOL;

  function IsChild(hWndParent:HWND; hWnd:HWND):WINBOOL;

  function DestroyWindow(hWnd:HWND):WINBOOL;

  function ShowWindow(hWnd:HWND; nCmdShow:longint):WINBOOL;

  function ShowWindowAsync(hWnd:HWND; nCmdShow:longint):WINBOOL;

  function FlashWindow(hWnd:HWND; bInvert:WINBOOL):WINBOOL;

  function ShowOwnedPopups(hWnd:HWND; fShow:WINBOOL):WINBOOL;

  function OpenIcon(hWnd:HWND):WINBOOL;

  function CloseWindow(hWnd:HWND):WINBOOL;

  function MoveWindow(hWnd:HWND; X:longint; Y:longint; nWidth:longint; nHeight:longint;
             bRepaint:WINBOOL):WINBOOL;

  function SetWindowPos(hWnd:HWND; hWndInsertAfter:HWND; X:longint; Y:longint; cx:longint;
             cy:longint; uFlags:UINT):WINBOOL;

  function GetWindowPlacement(hWnd:HWND; var lpwndpl:WINDOWPLACEMENT):WINBOOL;

(* Const before type ignored *)
  function SetWindowPlacement(hWnd:HWND; var lpwndpl:WINDOWPLACEMENT):WINBOOL;

  function BeginDeferWindowPos(nNumWindows:longint):HDWP;

  function DeferWindowPos(hWinPosInfo:HDWP; hWnd:HWND; hWndInsertAfter:HWND; x:longint; y:longint;
             cx:longint; cy:longint; uFlags:UINT):HDWP;

  function EndDeferWindowPos(hWinPosInfo:HDWP):WINBOOL;

  function IsWindowVisible(hWnd:HWND):WINBOOL;

  function IsIconic(hWnd:HWND):WINBOOL;

  function AnyPopup:WINBOOL;

  function BringWindowToTop(hWnd:HWND):WINBOOL;

  function IsZoomed(hWnd:HWND):WINBOOL;

  function EndDialog(hDlg:HWND; nResult:longint):WINBOOL;

  function GetDlgItem(hDlg:HWND; nIDDlgItem:longint):HWND;

  function SetDlgItemInt(hDlg:HWND; nIDDlgItem:longint; uValue:UINT; bSigned:WINBOOL):WINBOOL;

  function GetDlgItemInt(hDlg:HWND; nIDDlgItem:longint; var lpTranslated:WINBOOL; bSigned:WINBOOL):UINT;

  function CheckDlgButton(hDlg:HWND; nIDButton:longint; uCheck:UINT):WINBOOL;

  function CheckRadioButton(hDlg:HWND; nIDFirstButton:longint; nIDLastButton:longint; nIDCheckButton:longint):WINBOOL;

  function IsDlgButtonChecked(hDlg:HWND; nIDButton:longint):UINT;

  function GetNextDlgGroupItem(hDlg:HWND; hCtl:HWND; bPrevious:WINBOOL):HWND;

  function GetNextDlgTabItem(hDlg:HWND; hCtl:HWND; bPrevious:WINBOOL):HWND;

  function GetDlgCtrlID(hWnd:HWND):longint;

  function GetDialogBaseUnits:longint;

  function OpenClipboard(hWndNewOwner:HWND):WINBOOL;

  function CloseClipboard:WINBOOL;

  function GetClipboardOwner:HWND;

  function SetClipboardViewer(hWndNewViewer:HWND):HWND;

  function GetClipboardViewer:HWND;

  function ChangeClipboardChain(hWndRemove:HWND; hWndNewNext:HWND):WINBOOL;

  function SetClipboardData(uFormat:UINT; hMem:HANDLE):HANDLE;

  function GetClipboardData(uFormat:UINT):HANDLE;

  function CountClipboardFormats:longint;

  function EnumClipboardFormats(format:UINT):UINT;

  function EmptyClipboard:WINBOOL;

  function IsClipboardFormatAvailable(format:UINT):WINBOOL;

  function GetPriorityClipboardFormat(var paFormatPriorityList:UINT; cFormats:longint):longint;

  function GetOpenClipboardWindow:HWND;

  { Despite the A these are ASCII functions!  }
  function CharNextExA(CodePage:WORD; lpCurrentChar:LPCSTR; dwFlags:DWORD):LPSTR;

  function CharPrevExA(CodePage:WORD; lpStart:LPCSTR; lpCurrentChar:LPCSTR; dwFlags:DWORD):LPSTR;

  function SetFocus(hWnd:HWND):HWND;

  function GetActiveWindow:HWND;

  function GetFocus:HWND;

  function GetKBCodePage:UINT;

  function GetKeyState(nVirtKey:longint):SHORT;

  function GetAsyncKeyState(vKey:longint):SHORT;

  function GetKeyboardState(lpKeyState:PBYTE):WINBOOL;

  function SetKeyboardState(lpKeyState:LPBYTE):WINBOOL;

  function GetKeyboardType(nTypeFlag:longint):longint;

  function ToAscii(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE; lpChar:LPWORD; uFlags:UINT):longint;

  function ToAsciiEx(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE; lpChar:LPWORD; uFlags:UINT;
             dwhkl:HKL):longint;

  function ToUnicode(wVirtKey:UINT; wScanCode:UINT; lpKeyState:PBYTE; pwszBuff:LPWSTR; cchBuff:longint;
             wFlags:UINT):longint;

  function OemKeyScan(wOemChar:WORD):DWORD;

  procedure keybd_event(bVk:BYTE; bScan:BYTE; dwFlags:DWORD; dwExtraInfo:DWORD);

  procedure mouse_event(dwFlags:DWORD; dx:DWORD; dy:DWORD; cButtons:DWORD; dwExtraInfo:DWORD);

  function GetInputState:WINBOOL;

  function GetQueueStatus(flags:UINT):DWORD;

  function GetCapture:HWND;

  function SetCapture(hWnd:HWND):HWND;

  function ReleaseCapture:WINBOOL;

  function MsgWaitForMultipleObjects(nCount:DWORD; pHandles:LPHANDLE; fWaitAll:WINBOOL; dwMilliseconds:DWORD; dwWakeMask:DWORD):DWORD;

  function SetTimer(hWnd:HWND; nIDEvent:UINT; uElapse:UINT; lpTimerFunc:TIMERPROC):UINT;

  function KillTimer(hWnd:HWND; uIDEvent:UINT):WINBOOL;

  function IsWindowUnicode(hWnd:HWND):WINBOOL;

  function EnableWindow(hWnd:HWND; bEnable:WINBOOL):WINBOOL;

  function IsWindowEnabled(hWnd:HWND):WINBOOL;

  function DestroyAcceleratorTable(hAccel:HACCEL):WINBOOL;

  function GetSystemMetrics(nIndex:longint):longint;

  function GetMenu(hWnd:HWND):HMENU;

  function SetMenu(hWnd:HWND; hMenu:HMENU):WINBOOL;

  function HiliteMenuItem(hWnd:HWND; hMenu:HMENU; uIDHiliteItem:UINT; uHilite:UINT):WINBOOL;

  function GetMenuState(hMenu:HMENU; uId:UINT; uFlags:UINT):UINT;

  function DrawMenuBar(hWnd:HWND):WINBOOL;

  function GetSystemMenu(hWnd:HWND; bRevert:WINBOOL):HMENU;

  function CreateMenu:HMENU;

  function CreatePopupMenu:HMENU;

  function DestroyMenu(hMenu:HMENU):WINBOOL;

  function CheckMenuItem(hMenu:HMENU; uIDCheckItem:UINT; uCheck:UINT):DWORD;

  function EnableMenuItem(hMenu:HMENU; uIDEnableItem:UINT; uEnable:UINT):WINBOOL;

  function GetSubMenu(hMenu:HMENU; nPos:longint):HMENU;

  function GetMenuItemID(hMenu:HMENU; nPos:longint):UINT;

  function GetMenuItemCount(hMenu:HMENU):longint;

  function RemoveMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT):WINBOOL;

  function DeleteMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT):WINBOOL;

  function SetMenuItemBitmaps(hMenu:HMENU; uPosition:UINT; uFlags:UINT; hBitmapUnchecked:HBITMAP; hBitmapChecked:HBITMAP):WINBOOL;

  function GetMenuCheckMarkDimensions:LONG;

(* Const before type ignored *)
  function TrackPopupMenu(hMenu:HMENU; uFlags:UINT; x:longint; y:longint; nReserved:longint;
             hWnd:HWND; var prcRect:RECT):WINBOOL;

  function GetMenuDefaultItem(hMenu:HMENU; fByPos:UINT; gmdiFlags:UINT):UINT;

  function SetMenuDefaultItem(hMenu:HMENU; uItem:UINT; fByPos:UINT):WINBOOL;

  function GetMenuItemRect(hWnd:HWND; hMenu:HMENU; uItem:UINT; lprcItem:LPRECT):WINBOOL;

  function MenuItemFromPoint(hWnd:HWND; hMenu:HMENU; ptScreen:POINT):longint;

  function DragObject(_para1:HWND; _para2:HWND; _para3:UINT; _para4:DWORD; _para5:HCURSOR):DWORD;

  function DragDetect(hwnd:HWND; pt:POINT):WINBOOL;

  function DrawIcon(hDC:HDC; X:longint; Y:longint; hIcon:HICON):WINBOOL;

  function UpdateWindow(hWnd:HWND):WINBOOL;

  function SetActiveWindow(hWnd:HWND):HWND;

  function GetForegroundWindow:HWND;

  function PaintDesktop(hdc:HDC):WINBOOL;

  function SetForegroundWindow(hWnd:HWND):WINBOOL;

  function WindowFromDC(hDC:HDC):HWND;

  function GetDC(hWnd:HWND):HDC;

  function GetDCEx(hWnd:HWND; hrgnClip:HRGN; flags:DWORD):HDC;

  function GetWindowDC(hWnd:HWND):HDC;

  function ReleaseDC(hWnd:HWND; hDC:HDC):longint;

  function BeginPaint(hWnd:HWND; lpPaint:LPPAINTSTRUCT):HDC;

  function BeginPaint(hWnd:HWND;var lPaint:PAINTSTRUCT):HDC;

  function EndPaint(hWnd:HWND; var lpPaint:PAINTSTRUCT):WINBOOL;

  function GetUpdateRect(hWnd:HWND; lpRect:LPRECT; bErase:WINBOOL):WINBOOL;

  function GetUpdateRgn(hWnd:HWND; hRgn:HRGN; bErase:WINBOOL):longint;

  function SetWindowRgn(hWnd:HWND; hRgn:HRGN; bRedraw:WINBOOL):longint;

  function GetWindowRgn(hWnd:HWND; hRgn:HRGN):longint;

  function ExcludeUpdateRgn(hDC:HDC; hWnd:HWND):longint;

(* Const before type ignored *)
  function InvalidateRect(hWnd:HWND; var lpRect:RECT; bErase:WINBOOL):WINBOOL;

(* Const before type ignored *)
  function ValidateRect(hWnd:HWND; var lpRect:RECT):WINBOOL;

  function InvalidateRgn(hWnd:HWND; hRgn:HRGN; bErase:WINBOOL):WINBOOL;

  function ValidateRgn(hWnd:HWND; hRgn:HRGN):WINBOOL;

(* Const before type ignored *)
  function RedrawWindow(hWnd:HWND; var lprcUpdate:RECT; hrgnUpdate:HRGN; flags:UINT):WINBOOL;

  function LockWindowUpdate(hWndLock:HWND):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ScrollWindow(hWnd:HWND; XAmount:longint; YAmount:longint; var lpRect:RECT; var lpClipRect:RECT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:RECT; var lprcClip:RECT;
             hrgnUpdate:HRGN; lprcUpdate:LPRECT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ScrollWindowEx(hWnd:HWND; dx:longint; dy:longint; var prcScroll:RECT; var prcClip:RECT;
             hrgnUpdate:HRGN; prcUpdate:LPRECT; flags:UINT):longint;

  function SetScrollPos(hWnd:HWND; nBar:longint; nPos:longint; bRedraw:WINBOOL):longint;

  function GetScrollPos(hWnd:HWND; nBar:longint):longint;

  function SetScrollRange(hWnd:HWND; nBar:longint; nMinPos:longint; nMaxPos:longint; bRedraw:WINBOOL):WINBOOL;

  function GetScrollRange(hWnd:HWND; nBar:longint; lpMinPos:LPINT; lpMaxPos:LPINT):WINBOOL;

  function ShowScrollBar(hWnd:HWND; wBar:longint; bShow:WINBOOL):WINBOOL;

  function EnableScrollBar(hWnd:HWND; wSBflags:UINT; wArrows:UINT):WINBOOL;

  function GetClientRect(hWnd:HWND; lpRect:LPRECT):WINBOOL;

  function GetWindowRect(hWnd:HWND; lpRect:LPRECT):WINBOOL;

  function AdjustWindowRect(lpRect:LPRECT; dwStyle:DWORD; bMenu:WINBOOL):WINBOOL;

  function AdjustWindowRectEx(lpRect:LPRECT; dwStyle:DWORD; bMenu:WINBOOL; dwExStyle:DWORD):WINBOOL;

  function SetWindowContextHelpId(_para1:HWND; _para2:DWORD):WINBOOL;

  function GetWindowContextHelpId(_para1:HWND):DWORD;

  function SetMenuContextHelpId(_para1:HMENU; _para2:DWORD):WINBOOL;

  function GetMenuContextHelpId(_para1:HMENU):DWORD;

  function MessageBeep(uType:UINT):WINBOOL;

  function ShowCursor(bShow:WINBOOL):longint;

  function SetCursorPos(X:longint; Y:longint):WINBOOL;

  function SetCursor(hCursor:HCURSOR):HCURSOR;

  function GetCursorPos(lpPoint:LPPOINT):WINBOOL;

(* Const before type ignored *)
  function ClipCursor(var lpRect:RECT):WINBOOL;

  function GetClipCursor(lpRect:LPRECT):WINBOOL;

  function GetCursor:HCURSOR;

  function CreateCaret(hWnd:HWND; hBitmap:HBITMAP; nWidth:longint; nHeight:longint):WINBOOL;

  function GetCaretBlinkTime:UINT;

  function SetCaretBlinkTime(uMSeconds:UINT):WINBOOL;

  function DestroyCaret:WINBOOL;

  function HideCaret(hWnd:HWND):WINBOOL;

  function ShowCaret(hWnd:HWND):WINBOOL;

  function SetCaretPos(X:longint; Y:longint):WINBOOL;

  function GetCaretPos(lpPoint:LPPOINT):WINBOOL;

  function ClientToScreen(hWnd:HWND; lpPoint:LPPOINT):WINBOOL;

  function ScreenToClient(hWnd:HWND; lpPoint:LPPOINT):WINBOOL;

  function MapWindowPoints(hWndFrom:HWND; hWndTo:HWND; lpPoints:LPPOINT; cPoints:UINT):longint;

  function WindowFromPoint(Point:POINT):HWND;

  function ChildWindowFromPoint(hWndParent:HWND; Point:POINT):HWND;

  function GetSysColor(nIndex:longint):DWORD;

  function GetSysColorBrush(nIndex:longint):HBRUSH;

(* Const before type ignored *)
(* Const before type ignored *)
  function SetSysColors(cElements:longint; var lpaElements:INT; var lpaRgbValues:COLORREF):WINBOOL;

(* Const before type ignored *)
  function DrawFocusRect(hDC:HDC; var lprc:RECT):WINBOOL;

(* Const before type ignored *)
  function FillRect(hDC:HDC; var lprc:RECT; hbr:HBRUSH):longint;

(* Const before type ignored *)
  function FrameRect(hDC:HDC; var lprc:RECT; hbr:HBRUSH):longint;

(* Const before type ignored *)
  function InvertRect(hDC:HDC; var lprc:RECT):WINBOOL;

  function SetRect(lprc:LPRECT; xLeft:longint; yTop:longint; xRight:longint; yBottom:longint):WINBOOL;

  function SetRectEmpty(lprc:LPRECT):WINBOOL;

(* Const before type ignored *)
  function CopyRect(lprcDst:LPRECT; var lprcSrc:RECT):WINBOOL;

  function InflateRect(lprc:LPRECT; dx:longint; dy:longint):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function IntersectRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function UnionRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function SubtractRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL;

  function OffsetRect(lprc:LPRECT; dx:longint; dy:longint):WINBOOL;

(* Const before type ignored *)
  function IsRectEmpty(var lprc:RECT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function EqualRect(var lprc1:RECT; var lprc2:RECT):WINBOOL;

(* Const before type ignored *)
  function PtInRect(var lprc:RECT; pt:POINT):WINBOOL;

  function GetWindowWord(hWnd:HWND; nIndex:longint):WORD;

  function SetWindowWord(hWnd:HWND; nIndex:longint; wNewWord:WORD):WORD;

  function GetClassWord(hWnd:HWND; nIndex:longint):WORD;

  function SetClassWord(hWnd:HWND; nIndex:longint; wNewWord:WORD):WORD;

  function GetDesktopWindow:HWND;

  function GetParent(hWnd:HWND):HWND;

  function SetParent(hWndChild:HWND; hWndNewParent:HWND):HWND;

  function EnumChildWindows(hWndParent:HWND; lpEnumFunc:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL;

  function EnumWindows(lpEnumFunc:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL;

  function EnumThreadWindows(dwThreadId:DWORD; lpfn:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL;

  function GetTopWindow(hWnd:HWND):HWND;

  function GetWindowThreadProcessId(hWnd:HWND; lpdwProcessId:LPDWORD):DWORD;

  function GetLastActivePopup(hWnd:HWND):HWND;

  function GetWindow(hWnd:HWND; uCmd:UINT):HWND;

  function UnhookWindowsHook(nCode:longint; pfnFilterProc:HOOKPROC):WINBOOL;

  function UnhookWindowsHookEx(hhk:HHOOK):WINBOOL;

  function CallNextHookEx(hhk:HHOOK; nCode:longint; wParam:WPARAM; lParam:LPARAM):LRESULT;

  function CheckMenuRadioItem(_para1:HMENU; _para2:UINT; _para3:UINT; _para4:UINT; _para5:UINT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function CreateCursor(hInst:HINST; xHotSpot:longint; yHotSpot:longint; nWidth:longint; nHeight:longint;
             pvANDPlane:pointer; pvXORPlane:pointer):HCURSOR;

  function DestroyCursor(hCursor:HCURSOR):WINBOOL;

  function SetSystemCursor(hcur:HCURSOR; anID:DWORD):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function CreateIcon(hInstance:HINST; nWidth:longint; nHeight:longint; cPlanes:BYTE; cBitsPixel:BYTE;
             var lpbANDbits:BYTE; var lpbXORbits:BYTE):HICON;

  function DestroyIcon(hIcon:HICON):WINBOOL;

  function LookupIconIdFromDirectory(presbits:PBYTE; fIcon:WINBOOL):longint;

  function LookupIconIdFromDirectoryEx(presbits:PBYTE; fIcon:WINBOOL; cxDesired:longint; cyDesired:longint; Flags:UINT):longint;

  function CreateIconFromResource(presbits:PBYTE; dwResSize:DWORD; fIcon:WINBOOL; dwVer:DWORD):HICON;

  function CreateIconFromResourceEx(presbits:PBYTE; dwResSize:DWORD; fIcon:WINBOOL; dwVer:DWORD; cxDesired:longint;
             cyDesired:longint; Flags:UINT):HICON;

  function CopyImage(_para1:HANDLE; _para2:UINT; _para3:longint; _para4:longint; _para5:UINT):HICON;

  function CreateIconIndirect(piconinfo:PICONINFO):HICON;

  function CopyIcon(hIcon:HICON):HICON;

  function GetIconInfo(hIcon:HICON; piconinfo:PICONINFO):WINBOOL;

  function MapDialogRect(hDlg:HWND; lpRect:LPRECT):WINBOOL;

  function SetScrollInfo(_para1:HWND; _para2:longint; _para3:LPCSCROLLINFO; _para4:WINBOOL):longint;

  function GetScrollInfo(_para1:HWND; _para2:longint; _para3:LPSCROLLINFO):WINBOOL;

  function TranslateMDISysAccel(hWndClient:HWND; lpMsg:LPMSG):WINBOOL;

  function ArrangeIconicWindows(hWnd:HWND):UINT;

(* Const before type ignored *)
(* Const before type ignored *)
  function TileWindows(hwndParent:HWND; wHow:UINT; var lpRect:RECT; cKids:UINT; var lpKids:HWND):WORD;

(* Const before type ignored *)
(* Const before type ignored *)
  function CascadeWindows(hwndParent:HWND; wHow:UINT; var lpRect:RECT; cKids:UINT; var lpKids:HWND):WORD;

  procedure SetLastErrorEx(dwErrCode:DWORD; dwType:DWORD);

  procedure SetDebugErrorLevel(dwLevel:DWORD);

  function DrawEdge(hdc:HDC; qrc:LPRECT; edge:UINT; grfFlags:UINT):WINBOOL;

  function DrawFrameControl(_para1:HDC; _para2:LPRECT; _para3:UINT; _para4:UINT):WINBOOL;

(* Const before type ignored *)
  function DrawCaption(_para1:HWND; _para2:HDC; var _para3:RECT; _para4:UINT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function DrawAnimatedRects(hwnd:HWND; idAni:longint; var lprcFrom:RECT; var lprcTo:RECT):WINBOOL;

  function TrackPopupMenuEx(_para1:HMENU; _para2:UINT; _para3:longint; _para4:longint; _para5:HWND;
             _para6:LPTPMPARAMS):WINBOOL;

  function ChildWindowFromPointEx(_para1:HWND; _para2:POINT; _para3:UINT):HWND;

  function DrawIconEx(hdc:HDC; xLeft:longint; yTop:longint; hIcon:HICON; cxWidth:longint;
             cyWidth:longint; istepIfAniCur:UINT; hbrFlickerFreeDraw:HBRUSH; diFlags:UINT):WINBOOL;

(* Const before type ignored *)
  function AnimatePalette(_para1:HPALETTE; _para2:UINT; _para3:UINT; var _para4:PALETTEENTRY):WINBOOL;

  function Arc(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL;

  function BitBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:HDC; _para7:longint; _para8:longint; _para9:DWORD):WINBOOL;

  function CancelDC(_para1:HDC):WINBOOL;

  function Chord(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL;

  function CloseMetaFile(_para1:HDC):HMETAFILE;

  function CombineRgn(_para1:HRGN; _para2:HRGN; _para3:HRGN; _para4:longint):longint;

(* Const before type ignored *)
  function CreateBitmap(_para1:longint; _para2:longint; _para3:UINT; _para4:UINT; _para5:pointer):HBITMAP;

(* Const before type ignored *)
  function CreateBitmapIndirect(var _para1:BITMAP):HBITMAP;

(* Const before type ignored *)
  function CreateBrushIndirect(var _para1:LOGBRUSH):HBRUSH;

  function CreateCompatibleBitmap(_para1:HDC; _para2:longint; _para3:longint):HBITMAP;

  function CreateDiscardableBitmap(_para1:HDC; _para2:longint; _para3:longint):HBITMAP;

  function CreateCompatibleDC(_para1:HDC):HDC;

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function CreateDIBitmap(_para1:HDC; var _para2:BITMAPINFOHEADER; _para3:DWORD; _para4:pointer; var _para5:BITMAPINFO;
             _para6:UINT):HBITMAP;

  function CreateDIBPatternBrush(_para1:HGLOBAL; _para2:UINT):HBRUSH;

(* Const before type ignored *)
  function CreateDIBPatternBrushPt(_para1:pointer; _para2:UINT):HBRUSH;

  function CreateEllipticRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint):HRGN;

(* Const before type ignored *)
  function CreateEllipticRgnIndirect(var _para1:RECT):HRGN;

  function CreateHatchBrush(_para1:longint; _para2:COLORREF):HBRUSH;

(* Const before type ignored *)
  function CreatePalette(var _para1:LOGPALETTE):HPALETTE;

  function CreatePen(_para1:longint; _para2:longint; _para3:COLORREF):HPEN;

(* Const before type ignored *)
  function CreatePenIndirect(var _para1:LOGPEN):HPEN;

(* Const before type ignored *)
(* Const before type ignored *)
  function CreatePolyPolygonRgn(var _para1:POINT; var _para2:INT; _para3:longint; _para4:longint):HRGN;

  function CreatePatternBrush(_para1:HBITMAP):HBRUSH;

  function CreateRectRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint):HRGN;

(* Const before type ignored *)
  function CreateRectRgnIndirect(var _para1:RECT):HRGN;

  function CreateRoundRectRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint):HRGN;

  function CreateSolidBrush(_para1:COLORREF):HBRUSH;

  function DeleteDC(_para1:HDC):WINBOOL;

  function DeleteMetaFile(_para1:HMETAFILE):WINBOOL;

  function DeleteObject(_para1:HGDIOBJ):WINBOOL;

  function DrawEscape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR):longint;

  function Ellipse(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL;

  function EnumObjects(_para1:HDC; _para2:longint; _para3:ENUMOBJECTSPROC; _para4:LPARAM):longint;

  function EqualRgn(_para1:HRGN; _para2:HRGN):WINBOOL;

  function Escape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:LPVOID):longint;

  function ExtEscape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint;
             _para6:LPSTR):longint;

  function ExcludeClipRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):longint;

(* Const before type ignored *)
(* Const before type ignored *)
  function ExtCreateRegion(var _para1:XFORM; _para2:DWORD; var _para3:RGNDATA):HRGN;

  function ExtFloodFill(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF; _para5:UINT):WINBOOL;

  function FillRgn(_para1:HDC; _para2:HRGN; _para3:HBRUSH):WINBOOL;

  function FloodFill(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):WINBOOL;

  function FrameRgn(_para1:HDC; _para2:HRGN; _para3:HBRUSH; _para4:longint; _para5:longint):WINBOOL;

  function GetROP2(_para1:HDC):longint;

  function GetAspectRatioFilterEx(_para1:HDC; _para2:LPSIZE):WINBOOL;

  function GetBkColor(_para1:HDC):COLORREF;

  function GetBkMode(_para1:HDC):longint;

  function GetBitmapBits(_para1:HBITMAP; _para2:LONG; _para3:LPVOID):LONG;

  function GetBitmapDimensionEx(_para1:HBITMAP; _para2:LPSIZE):WINBOOL;

  function GetBoundsRect(_para1:HDC; _para2:LPRECT; _para3:UINT):UINT;

  function GetBrushOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL;

  function GetClipBox(_para1:HDC; _para2:LPRECT):longint;

  function GetClipRgn(_para1:HDC; _para2:HRGN):longint;

  function GetMetaRgn(_para1:HDC; _para2:HRGN):longint;

  function GetCurrentObject(_para1:HDC; _para2:UINT):HGDIOBJ;

  function GetCurrentPositionEx(_para1:HDC; _para2:LPPOINT):WINBOOL;

  function GetDeviceCaps(_para1:HDC; _para2:longint):longint;

  function GetDIBits(_para1:HDC; _para2:HBITMAP; _para3:UINT; _para4:UINT; _para5:LPVOID;
             _para6:LPBITMAPINFO; _para7:UINT):longint;

  function GetFontData(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:LPVOID; _para5:DWORD):DWORD;

  function GetGraphicsMode(_para1:HDC):longint;

  function GetMapMode(_para1:HDC):longint;

  function GetMetaFileBitsEx(_para1:HMETAFILE; _para2:UINT; _para3:LPVOID):UINT;

  function GetNearestColor(_para1:HDC; _para2:COLORREF):COLORREF;

  function GetNearestPaletteIndex(_para1:HPALETTE; _para2:COLORREF):UINT;

  function GetObjectType(h:HGDIOBJ):DWORD;

  function GetPaletteEntries(_para1:HPALETTE; _para2:UINT; _para3:UINT; _para4:LPPALETTEENTRY):UINT;

  function GetPixel(_para1:HDC; _para2:longint; _para3:longint):COLORREF;

  function GetPixelFormat(_para1:HDC):longint;

  function GetPolyFillMode(_para1:HDC):longint;

  function GetRasterizerCaps(_para1:LPRASTERIZER_STATUS; _para2:UINT):WINBOOL;

  function GetRegionData(_para1:HRGN; _para2:DWORD; _para3:LPRGNDATA):DWORD;

  function GetRgnBox(_para1:HRGN; _para2:LPRECT):longint;

  function GetStockObject(_para1:longint):HGDIOBJ;

  function GetStretchBltMode(_para1:HDC):longint;

  function GetSystemPaletteEntries(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPPALETTEENTRY):UINT;

  function GetSystemPaletteUse(_para1:HDC):UINT;

  function GetTextCharacterExtra(_para1:HDC):longint;

  function GetTextAlign(_para1:HDC):UINT;

  function GetTextColor(_para1:HDC):COLORREF;

  function GetTextCharset(hdc:HDC):longint;

  function GetTextCharsetInfo(hdc:HDC; lpSig:LPFONTSIGNATURE; dwFlags:DWORD):longint;

  function TranslateCharsetInfo(var lpSrc:DWORD; lpCs:LPCHARSETINFO; dwFlags:DWORD):WINBOOL;

  function GetFontLanguageInfo(_para1:HDC):DWORD;

  function GetViewportExtEx(_para1:HDC; _para2:LPSIZE):WINBOOL;

  function GetViewportOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL;

  function GetWindowExtEx(_para1:HDC; _para2:LPSIZE):WINBOOL;

  function GetWindowOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL;

  function IntersectClipRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):longint;

  function InvertRgn(_para1:HDC; _para2:HRGN):WINBOOL;

  function LineDDA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:LINEDDAPROC;
             _para6:LPARAM):WINBOOL;

  function LineTo(_para1:HDC; _para2:longint; _para3:longint):WINBOOL;

  function MaskBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:HDC; _para7:longint; _para8:longint; _para9:HBITMAP; _para10:longint;
             _para11:longint; _para12:DWORD):WINBOOL;

(* Const before type ignored *)
  function PlgBlt(_para1:HDC; var _para2:POINT; _para3:HDC; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:HBITMAP; _para9:longint; _para10:longint):WINBOOL;

  function OffsetClipRgn(_para1:HDC; _para2:longint; _para3:longint):longint;

  function OffsetRgn(_para1:HRGN; _para2:longint; _para3:longint):longint;

  function PatBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:DWORD):WINBOOL;

  function Pie(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL;

  function PlayMetaFile(_para1:HDC; _para2:HMETAFILE):WINBOOL;

  function PaintRgn(_para1:HDC; _para2:HRGN):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function PolyPolygon(_para1:HDC; var _para2:POINT; var _para3:INT; _para4:longint):WINBOOL;

  function PtInRegion(_para1:HRGN; _para2:longint; _para3:longint):WINBOOL;

  function PtVisible(_para1:HDC; _para2:longint; _para3:longint):WINBOOL;

(* Const before type ignored *)
  function RectInRegion(_para1:HRGN; var _para2:RECT):WINBOOL;

(* Const before type ignored *)
  function RectVisible(_para1:HDC; var _para2:RECT):WINBOOL;

  function Rectangle(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL;

  function RestoreDC(_para1:HDC; _para2:longint):WINBOOL;

  function RealizePalette(_para1:HDC):UINT;

  function RoundRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint):WINBOOL;

  function ResizePalette(_para1:HPALETTE; _para2:UINT):WINBOOL;

  function SaveDC(_para1:HDC):longint;

  function SelectClipRgn(_para1:HDC; _para2:HRGN):longint;

  function ExtSelectClipRgn(_para1:HDC; _para2:HRGN; _para3:longint):longint;

  function SetMetaRgn(_para1:HDC):longint;

  function SelectObject(_para1:HDC; _para2:HGDIOBJ):HGDIOBJ;

  function SelectPalette(_para1:HDC; _para2:HPALETTE; _para3:WINBOOL):HPALETTE;

  function SetBkColor(_para1:HDC; _para2:COLORREF):COLORREF;

  function SetBkMode(_para1:HDC; _para2:longint):longint;

(* Const before type ignored *)
  function SetBitmapBits(_para1:HBITMAP; _para2:DWORD; _para3:pointer):LONG;

(* Const before type ignored *)
  function SetBoundsRect(_para1:HDC; var _para2:RECT; _para3:UINT):UINT;

(* Const before type ignored *)
(* Const before type ignored *)
  function SetDIBits(_para1:HDC; _para2:HBITMAP; _para3:UINT; _para4:UINT; _para5:pointer;
             var _para6:BITMAPINFO; _para7:UINT):longint;

(* Const before type ignored *)
(* Const before type ignored *)
  function SetDIBitsToDevice(_para1:HDC; _para2:longint; _para3:longint; _para4:DWORD; _para5:DWORD;
             _para6:longint; _para7:longint; _para8:UINT; _para9:UINT; _para10:pointer;
             var _para11:BITMAPINFO; _para12:UINT):longint;

  function SetMapperFlags(_para1:HDC; _para2:DWORD):DWORD;

  function SetGraphicsMode(hdc:HDC; iMode:longint):longint;

  function SetMapMode(_para1:HDC; _para2:longint):longint;

(* Const before type ignored *)
  function SetMetaFileBitsEx(_para1:UINT; var _para2:BYTE):HMETAFILE;

(* Const before type ignored *)
  function SetPaletteEntries(_para1:HPALETTE; _para2:UINT; _para3:UINT; var _para4:PALETTEENTRY):UINT;

  function SetPixel(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):COLORREF;

  function SetPixelV(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):WINBOOL;

  function SetPolyFillMode(_para1:HDC; _para2:longint):longint;

  function StretchBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:HDC; _para7:longint; _para8:longint; _para9:longint; _para10:longint;
             _para11:DWORD):WINBOOL;

  function SetRectRgn(_para1:HRGN; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function StretchDIBits(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:pointer;
             var _para11:BITMAPINFO; _para12:UINT; _para13:DWORD):longint;

  function SetROP2(_para1:HDC; _para2:longint):longint;

  function SetStretchBltMode(_para1:HDC; _para2:longint):longint;

  function SetSystemPaletteUse(_para1:HDC; _para2:UINT):UINT;

  function SetTextCharacterExtra(_para1:HDC; _para2:longint):longint;

  function SetTextColor(_para1:HDC; _para2:COLORREF):COLORREF;

  function SetTextAlign(_para1:HDC; _para2:UINT):UINT;

  function SetTextJustification(_para1:HDC; _para2:longint; _para3:longint):WINBOOL;

  function UpdateColors(_para1:HDC):WINBOOL;

  function PlayMetaFileRecord(_para1:HDC; _para2:LPHANDLETABLE; _para3:LPMETARECORD; _para4:UINT):WINBOOL;

  function EnumMetaFile(_para1:HDC; _para2:HMETAFILE; _para3:ENUMMETAFILEPROC; _para4:LPARAM):WINBOOL;

  function CloseEnhMetaFile(_para1:HDC):HENHMETAFILE;

  function DeleteEnhMetaFile(_para1:HENHMETAFILE):WINBOOL;

(* Const before type ignored *)
  function EnumEnhMetaFile(_para1:HDC; _para2:HENHMETAFILE; _para3:ENHMETAFILEPROC; _para4:LPVOID; var _para5:RECT):WINBOOL;

  function GetEnhMetaFileHeader(_para1:HENHMETAFILE; _para2:UINT; _para3:LPENHMETAHEADER):UINT;

  function GetEnhMetaFilePaletteEntries(_para1:HENHMETAFILE; _para2:UINT; _para3:LPPALETTEENTRY):UINT;

  function GetWinMetaFileBits(_para1:HENHMETAFILE; _para2:UINT; _para3:LPBYTE; _para4:INT; _para5:HDC):UINT;

(* Const before type ignored *)
  function PlayEnhMetaFile(_para1:HDC; _para2:HENHMETAFILE; var _para3:RECT):WINBOOL;

(* Const before type ignored *)
  function PlayEnhMetaFileRecord(_para1:HDC; _para2:LPHANDLETABLE; var _para3:ENHMETARECORD; _para4:UINT):WINBOOL;

(* Const before type ignored *)
  function SetEnhMetaFileBits(_para1:UINT; var _para2:BYTE):HENHMETAFILE;

(* Const before type ignored *)
(* Const before type ignored *)
  function SetWinMetaFileBits(_para1:UINT; var _para2:BYTE; _para3:HDC; var _para4:METAFILEPICT):HENHMETAFILE;

(* Const before type ignored *)
  function GdiComment(_para1:HDC; _para2:UINT; var _para3:BYTE):WINBOOL;

  function AngleArc(_para1:HDC; _para2:longint; _para3:longint; _para4:DWORD; _para5:FLOAT;
             _para6:FLOAT):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function PolyPolyline(_para1:HDC; var _para2:POINT; var _para3:DWORD; _para4:DWORD):WINBOOL;

  function GetWorldTransform(_para1:HDC; _para2:LPXFORM):WINBOOL;

(* Const before type ignored *)
  function SetWorldTransform(_para1:HDC; var _para2:XFORM):WINBOOL;

(* Const before type ignored *)
  function ModifyWorldTransform(_para1:HDC; var _para2:XFORM; _para3:DWORD):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function CombineTransform(_para1:LPXFORM; var _para2:XFORM; var _para3:XFORM):WINBOOL;

(* Const before type ignored *)
  function CreateDIBSection(_para1:HDC; var _para2:BITMAPINFO; _para3:UINT; var _para4:pointer; _para5:HANDLE;
             _para6:DWORD):HBITMAP;

  function GetDIBColorTable(_para1:HDC; _para2:UINT; _para3:UINT; var _para4:RGBQUAD):UINT;

(* Const before type ignored *)
  function SetDIBColorTable(_para1:HDC; _para2:UINT; _para3:UINT; var _para4:RGBQUAD):UINT;

(* Const before type ignored *)
  function SetColorAdjustment(_para1:HDC; var _para2:COLORADJUSTMENT):WINBOOL;

  function GetColorAdjustment(_para1:HDC; _para2:LPCOLORADJUSTMENT):WINBOOL;

  function CreateHalftonePalette(_para1:HDC):HPALETTE;

  function EndDoc(_para1:HDC):longint;

  function StartPage(_para1:HDC):longint;

  function EndPage(_para1:HDC):longint;

(*  function AbortDoc(_para1:HDC):longint; already above *)

  function SetAbortProc(_para1:HDC; _para2:TABORTPROC):longint;

(*  function AbortPath(_para1:HDC):WINBOOL; already above *)

  function ArcTo(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL;

  function BeginPath(_para1:HDC):WINBOOL;

  function CloseFigure(_para1:HDC):WINBOOL;

  function EndPath(_para1:HDC):WINBOOL;

  function FillPath(_para1:HDC):WINBOOL;

  function FlattenPath(_para1:HDC):WINBOOL;

  function GetPath(_para1:HDC; _para2:LPPOINT; _para3:LPBYTE; _para4:longint):longint;

  function PathToRegion(_para1:HDC):HRGN;

(* Const before type ignored *)
(* Const before type ignored *)
  function PolyDraw(_para1:HDC; var _para2:POINT; var _para3:BYTE; _para4:longint):WINBOOL;

  function SelectClipPath(_para1:HDC; _para2:longint):WINBOOL;

  function SetArcDirection(_para1:HDC; _para2:longint):longint;

  function SetMiterLimit(_para1:HDC; _para2:FLOAT; _para3:PFLOAT):WINBOOL;

  function StrokeAndFillPath(_para1:HDC):WINBOOL;

  function StrokePath(_para1:HDC):WINBOOL;

  function WidenPath(_para1:HDC):WINBOOL;

(* Const before type ignored *)
(* Const before type ignored *)
  function ExtCreatePen(_para1:DWORD; _para2:DWORD; var _para3:LOGBRUSH; _para4:DWORD; var _para5:DWORD):HPEN;

  function GetMiterLimit(_para1:HDC; _para2:PFLOAT):WINBOOL;

  function GetArcDirection(_para1:HDC):longint;

  function MoveToEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

(* Const before type ignored *)
  function CreatePolygonRgn(var _para1:POINT; _para2:longint; _para3:longint):HRGN;

  function DPtoLP(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL;

  function LPtoDP(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL;

(* Const before type ignored *)
  function Polygon(_para1:HDC; var _para2:POINT; _para3:longint):WINBOOL;

(* Const before type ignored *)
  function Polyline(_para1:HDC; var _para2:POINT; _para3:longint):WINBOOL;

(* Const before type ignored *)
  function PolyBezier(_para1:HDC; var _para2:POINT; _para3:DWORD):WINBOOL;

(* Const before type ignored *)
  function PolyBezierTo(_para1:HDC; var _para2:POINT; _para3:DWORD):WINBOOL;

(* Const before type ignored *)
  function PolylineTo(_para1:HDC; var _para2:POINT; _para3:DWORD):WINBOOL;

  function SetViewportExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL;

  function SetViewportOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

  function SetWindowExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL;

  function SetWindowOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

  function OffsetViewportOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

  function OffsetWindowOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

  function ScaleViewportExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:LPSIZE):WINBOOL;

  function ScaleWindowExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:LPSIZE):WINBOOL;

  function SetBitmapDimensionEx(_para1:HBITMAP; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL;

  function SetBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

  function GetDCOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL;

  function FixBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL;

  function UnrealizeObject(_para1:HGDIOBJ):WINBOOL;

  function GdiFlush:WINBOOL;

  function GdiSetBatchLimit(_para1:DWORD):DWORD;

  function GdiGetBatchLimit:DWORD;

  function SetICMMode(_para1:HDC; _para2:longint):longint;

  function CheckColorsInGamut(_para1:HDC; _para2:LPVOID; _para3:LPVOID; _para4:DWORD):WINBOOL;

  function GetColorSpace(_para1:HDC):HANDLE;

  function SetColorSpace(_para1:HDC; _para2:HCOLORSPACE):WINBOOL;

  function DeleteColorSpace(_para1:HCOLORSPACE):WINBOOL;

  function GetDeviceGammaRamp(_para1:HDC; _para2:LPVOID):WINBOOL;

  function SetDeviceGammaRamp(_para1:HDC; _para2:LPVOID):WINBOOL;

  function ColorMatchToTarget(_para1:HDC; _para2:HDC; _para3:DWORD):WINBOOL;

  function CreatePropertySheetPageA(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE;

  function DestroyPropertySheetPage(hPSPage:HPROPSHEETPAGE):WINBOOL;

  procedure InitCommonControls;

  { was #define dname(params) def_expr }
  function ImageList_AddIcon(himl:HIMAGELIST; hicon:HICON):longint;

  function ImageList_Create(cx:longint; cy:longint; flags:UINT; cInitial:longint; cGrow:longint):HIMAGELIST;

  function ImageList_Destroy(himl:HIMAGELIST):WINBOOL;

  function ImageList_GetImageCount(himl:HIMAGELIST):longint;

  function ImageList_Add(himl:HIMAGELIST; hbmImage:HBITMAP; hbmMask:HBITMAP):longint;

  function ImageList_ReplaceIcon(himl:HIMAGELIST; i:longint; hicon:HICON):longint;

  function ImageList_SetBkColor(himl:HIMAGELIST; clrBk:COLORREF):COLORREF;

  function ImageList_GetBkColor(himl:HIMAGELIST):COLORREF;

  function ImageList_SetOverlayImage(himl:HIMAGELIST; iImage:longint; iOverlay:longint):WINBOOL;

  function ImageList_Draw(himl:HIMAGELIST; i:longint; hdcDst:HDC; x:longint; y:longint;
             fStyle:UINT):WINBOOL;

  function ImageList_Replace(himl:HIMAGELIST; i:longint; hbmImage:HBITMAP; hbmMask:HBITMAP):WINBOOL;

  function ImageList_AddMasked(himl:HIMAGELIST; hbmImage:HBITMAP; crMask:COLORREF):longint;

  function ImageList_DrawEx(himl:HIMAGELIST; i:longint; hdcDst:HDC; x:longint; y:longint;
             dx:longint; dy:longint; rgbBk:COLORREF; rgbFg:COLORREF; fStyle:UINT):WINBOOL;

  function ImageList_Remove(himl:HIMAGELIST; i:longint):WINBOOL;

  function ImageList_GetIcon(himl:HIMAGELIST; i:longint; flags:UINT):HICON;

  function ImageList_BeginDrag(himlTrack:HIMAGELIST; iTrack:longint; dxHotspot:longint; dyHotspot:longint):WINBOOL;

  procedure ImageList_EndDrag;

  function ImageList_DragEnter(hwndLock:HWND; x:longint; y:longint):WINBOOL;

  function ImageList_DragLeave(hwndLock:HWND):WINBOOL;

  function ImageList_DragMove(x:longint; y:longint):WINBOOL;

  function ImageList_SetDragCursorImage(himlDrag:HIMAGELIST; iDrag:longint; dxHotspot:longint; dyHotspot:longint):WINBOOL;

  function ImageList_DragShowNolock(fShow:WINBOOL):WINBOOL;

  function ImageList_GetDragImage(var ppt:POINT; var pptHotspot:POINT):HIMAGELIST;

  function ImageList_GetIconSize(himl:HIMAGELIST; var cx:longint; var cy:longint):WINBOOL;

  function ImageList_SetIconSize(himl:HIMAGELIST; cx:longint; cy:longint):WINBOOL;

  function ImageList_GetImageInfo(himl:HIMAGELIST; i:longint; var pImageInfo:IMAGEINFO):WINBOOL;

  function ImageList_Merge(himl1:HIMAGELIST; i1:longint; himl2:HIMAGELIST; i2:longint; dx:longint;
             dy:longint):HIMAGELIST;

  function CreateToolbarEx(hwnd:HWND; ws:DWORD; wID:UINT; nBitmaps:longint; hBMInst:HINST;
             wBMID:UINT; lpButtons:LPCTBBUTTON; iNumButtons:longint; dxButton:longint; dyButton:longint;
             dxBitmap:longint; dyBitmap:longint; uStructSize:UINT):HWND;

  function CreateMappedBitmap(hInstance:HINST; idBitmap:longint; wFlags:UINT; lpColorMap:LPCOLORMAP; iNumMaps:longint):HBITMAP;

  procedure MenuHelp(uMsg:UINT; wParam:WPARAM; lParam:LPARAM; hMainMenu:HMENU; hInst:HINST;
              hwndStatus:HWND; var lpwIDs:UINT);

  function ShowHideMenuCtl(hWnd:HWND; uFlags:UINT; lpInfo:LPINT):WINBOOL;

  procedure GetEffectiveClientRect(hWnd:HWND; lprc:LPRECT; lpInfo:LPINT);

  function MakeDragList(hLB:HWND):WINBOOL;

  procedure DrawInsert(handParent:HWND; hLB:HWND; nItem:longint);

  function LBItemFromPt(hLB:HWND; pt:POINT; bAutoScroll:WINBOOL):longint;

  function CreateUpDownControl(dwStyle:DWORD; x:longint; y:longint; cx:longint; cy:longint;
             hParent:HWND; nID:longint; hInst:HINST; hBuddy:HWND; nUpper:longint;
             nLower:longint; nPos:longint):HWND;

  function CommDlgExtendedError:DWORD;

  { Animation controls  }
  { was #define dname(params) def_expr }
  function Animate_Create(hWndP:HWND; id:HMENU;dwStyle:DWORD;hInstance:HINST):HWND;

  { was #define dname(params) def_expr }
  function Animate_Open(hwnd : HWND;szName : LPTSTR) : LRESULT;

  { was #define dname(params) def_expr }
  function Animate_Play(hwnd : HWND;from,_to : longint;rep : UINT) : LRESULT;

  { was #define dname(params) def_expr }
  function Animate_Stop(hwnd : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function Animate_Close(hwnd : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function Animate_Seek(hwnd : HWND;frame : longint) : LRESULT;

  { Property sheet macros  }
  { was #define dname(params) def_expr }
  function PropSheet_AddPage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_Apply(hPropSheetDlg : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_CancelToClose(hPropSheetDlg : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_Changed(hPropSheetDlg,hwndPage : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_GetCurrentPageHwnd(hDlg : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_GetTabControl(hPropSheetDlg : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_IsDialogMessage(hDlg : HWND;pMsg : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_PressButton(hPropSheetDlg : HWND;iButton : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_QuerySiblings(hPropSheetDlg : HWND;param1,param2 : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_RebootSystem(hPropSheetDlg : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_RemovePage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_RestartWindows(hPropSheetDlg : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_SetCurSel(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_SetCurSelByID(hPropSheetDlg : HWND; id : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_SetFinishText(hPropSheetDlg:HWND;lpszText : LPTSTR) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_SetTitle(hPropSheetDlg:HWND;dwStyle:DWORD;lpszText : LPCTSTR) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_SetWizButtons(hPropSheetDlg:HWND;dwFlags : DWORD) : LRESULT;

  { was #define dname(params) def_expr }
  function PropSheet_UnChanged(hPropSheetDlg:HWND;hwndPage : HWND) : LRESULT;

  { Header control  }
  { was #define dname(params) def_expr }
  function Header_DeleteItem(hwndHD:HWND;index : longint) : WINBOOL;

  (* far ignored *)
  { was #define dname(params) def_expr }
  function Header_GetItem(hwndHD:HWND;index:longint;var hdi : HD_ITEM) : WINBOOL;

  { was #define dname(params) def_expr }
  function Header_GetItemCount(hwndHD : HWND) : longint;

(* Const before type ignored *)
  (* far ignored *)
  { was #define dname(params) def_expr }
  function Header_InsertItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : longint;

  (* far ignored *)
  { was #define dname(params) def_expr }
  function Header_Layout(hwndHD:HWND;var layout : HD_LAYOUT) : WINBOOL;

(* Const before type ignored *)
  (* far ignored *)
  { was #define dname(params) def_expr }
  function Header_SetItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : WINBOOL;

  { List View  }
  { was #define dname(params) def_expr }
  function ListView_Arrange(hwndLV:HWND;code : UINT) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_CreateDragImage(hwnd:HWND;i : longint;lpptUpLeft : LPPOINT) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_DeleteAllItems(hwnd : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_DeleteColumn(hwnd:HWND;iCol : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_DeleteItem(hwnd:HWND;iItem : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_EditLabel(hwndLV:HWND;i : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_EnsureVisible(hwndLV:HWND;i,fPartialOK : longint) : LRESULT;

(* Const before type ignored *)
  { was #define dname(params) def_expr }
  function ListView_FindItem(hwnd:HWND;iStart : longint;var lvfi : LV_FINDINFO) : longint;

  { was #define dname(params) def_expr }
  function ListView_GetBkColor(hwnd : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetCallbackMask(hwnd : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetColumnWidth(hwnd:HWND;iCol : longint) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetCountPerPage(hwndLV : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetEditControl(hwndLV : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetImageList(hwnd:HWND;iImageList : INT) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetISearchString(hwndLV:HWND;lpsz : LPTSTR) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetItemCount(hwnd : HWND) : LRESULT;

  { was #define dname(params) def_expr }
  function ListView_GetItemPosition(hwndLV:HWND;i : longint;var pt : POINT) : longint;

{ error
#define ListView_GetItemRect(hwnd, i, prc, code) \
SendMessage(hwnd, LVM_GETITEMRECT, (WPARAM)(int)i, \
           ((prc) ? (((RECT *)(prc))->left = (code), \
                     (LPARAM)(RECT *)(prc)) : (LPARAM)(RECT *)nil))
in define line 6717 }
    { was #define dname(params) def_expr }
    function ListView_GetItemSpacing(hwndLV:HWND;fSmall : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetItemState(hwndLV:HWND;i,mask : longint) : LRESULT;

{ error
#define ListView_GetItemText(hwndLV, i, iSubItem_, pszText_, cchTextMax_) \
 LV_ITEM _gnu_lvi;\
  _gnu_lvi.iSubItem = iSubItem_;\
  _gnu_lvi.cchTextMax = cchTextMax_;\
  _gnu_lvi.pszText = pszText_;\
  SendMessage((hwndLV), LVM_GETITEMTEXT, (WPARAM)i, \
              (LPARAM)(LV_ITEM *)&_gnu_lvi);\

in declaration at line 6725 }

{  this one was scratched by the error above
#define ListView_GetNextItem(hwnd, iStart, flags) \
SendMessage(hwnd, LVM_GETNEXTITEM, (WPARAM)(int)iStart, (LPARAM)flags)
inserted manually PM }
  function ListView_GetNextItem(hwnd:HWND; iStart, flags : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetOrigin(hwndLV:HWND;var pt : POINT) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetSelectedCount(hwndLV : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetStringWidth(hwndLV:HWND;psz : LPCTSTR) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetTextBkColor(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetTextColor(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetTopIndex(hwndLV : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_GetViewRect(hwnd:HWND;var rc : RECT) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_HitTest(hwndLV:HWND;var info : LV_HITTESTINFO) : LRESULT;

(* Const before type ignored *)
    { was #define dname(params) def_expr }
    function ListView_InsertColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;

(* Const before type ignored *)
    { was #define dname(params) def_expr }
    function ListView_InsertItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_RedrawItems(hwndLV:HWND;iFirst,iLast : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_Scroll(hwndLV:HWND;dx,dy : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetBkColor(hwnd:HWND;clrBk : COLORREF) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetCallbackMask(hwnd:HWND;mask : UINT) : LRESULT;

(* Const before type ignored *)
    { was #define dname(params) def_expr }
    function ListView_SetColumn(hwnd:HWND;iCol : longint; var col : LV_COLUMN) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetColumnWidth(hwnd:HWND;iCol,cx : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetImageList(hwnd:HWND;himl : longint;iImageList : HIMAGELIST) : LRESULT;

(* Const before type ignored *)
    { was #define dname(params) def_expr }
    function ListView_SetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetItemCount(hwndLV:HWND;cItems : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetItemPosition(hwndLV:HWND;i,x,y : longint) : LRESULT;

(* error
            MAKELPARAM((x), (y)))

in declaration at line 6803 *)
(* error
#define ListView_SetItemPosition32(hwndLV, i, x, y) \
{ POINT ptNewPos = x,y; \
    SendMessage((hwndLV), LVM_SETITEMPOSITION32, (WPARAM)(int)(i), \
                (LPARAM)&ptNewPos); \
}
 inserted by hand PM *)
    { was #define dname(params) def_expr }
    function ListView_SetItemPosition32(hwndLV:HWND;i,x,y : longint) : LRESULT;

(* error
#define ListView_SetItemState(hwndLV, i, data, mask) \
{ LV_ITEM _gnu_lvi;\
  _gnu_lvi.stateMask = mask;\
  _gnu_lvi.state = data;\
  SendMessage((hwndLV), LVM_SETITEMSTATE, (WPARAM)i, \
              (LPARAM)(LV_ITEM * )&_gnu_lvi);\
}
in declaration at line 6817
 error *)
    function ListView_SetItemState(hwndLV:HWND; i, data, mask:longint) : LRESULT;

(* error
#define ListView_SetItemText(hwndLV, i, iSubItem_, pszText_) \
{ LV_ITEM _gnu_lvi;\
  _gnu_lvi.iSubItem = iSubItem_;\
  _gnu_lvi.pszText = pszText_;\
  SendMessage((hwndLV), LVM_SETITEMTEXT, (WPARAM)i, \
              (LPARAM)(LV_ITEM * )&_gnu_lvi);\
}
in define line 6826 *)
    function ListView_SetItemText(hwndLV:HWND; i, iSubItem_:longint;pszText_ : LPTSTR) : LRESULT;

    { also eaten by errors !! }
    function ListView_SetTextBkColor(hwnd:HWND;clrTextBk : COLORREF) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SetTextColor(hwnd:HWND;clrText : COLORREF) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_SortItems(hwndLV:HWND;_pfnCompare:PFNLVCOMPARE;_lPrm : LPARAM) : LRESULT;

    { was #define dname(params) def_expr }
    function ListView_Update(hwndLV:HWND;i : longint) : LRESULT;

    { Tree View  }
    { was #define dname(params) def_expr }
    function TreeView_InsertItem(hwnd:HWND;lpis : LPTV_INSERTSTRUCT) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_DeleteItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_DeleteAllItems(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_Expand(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;

(* error
SendMessage((hwnd), TVM_EXPAND, (WPARAM)code, (LPARAM)(HTREEITEM)(hitem))

in define line 6852 *)
    { was #define dname(params) def_expr }
    function TreeView_GetCount(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetIndent(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SetIndent(hwnd:HWND;indent : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetImageList(hwnd:HWND;iImage : WPARAM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SetImageList(hwnd:HWND;himl:HIMAGELIST;iImage : WPARAM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetNextItem(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetChild(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetNextSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetPrevSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetParent(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetFirstVisible(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetNextVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetPrevVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetSelection(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetDropHilight(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetRoot(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_Select(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SelectItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SelectDropTarget(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SelectSetFirstVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;

(* Const before type ignored *)
    { was #define dname(params) def_expr }
    function TreeView_SetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_EditLabel(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetEditControl(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetVisibleCount(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_HitTest(hwnd:HWND;lpht : LPTV_HITTESTINFO) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_CreateDragImage(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SortChildren(hwnd:HWND;hitem:HTREEITEM;recurse : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_EnsureVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_SortChildrenCB(hwnd:HWND;psort:LPTV_SORTCB;recurse : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_EndEditLabelNow(hwnd:HWND;fCancel : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TreeView_GetISearchString(hwndTV:HWND;lpsz : LPTSTR) : LRESULT;

    { Tab control  }
    { was #define dname(params) def_expr }
    function TabCtrl_GetImageList(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetImageList(hwnd:HWND;himl : HIMAGELIST) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetItemCount(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;

(* Const before type ignored *)
    { was #define dname(params) def_expr }
    function TabCtrl_InsertItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_DeleteItem(hwnd:HWND;i : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_DeleteAllItems(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetItemRect(hwnd:HWND;i : longint;var rc : RECT) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetCurSel(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetCurSel(hwnd:HWND;i : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_HitTest(hwndTC:HWND;var info : TC_HITTESTINFO) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetItemExtra(hwndTC:HWND;cb : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_AdjustRect(hwnd:HWND;bLarger:WINBOOL;var rc : RECT) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetItemSize(hwnd:HWND;x,y : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_RemoveImage(hwnd:HWND;i : WPARAM) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetPadding(hwnd:HWND;cx,cy : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetRowCount(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetToolTips(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetToolTips(hwnd:HWND;hwndTT : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_GetCurFocus(hwnd : HWND) : LRESULT;

    { was #define dname(params) def_expr }
    function TabCtrl_SetCurFocus(hwnd:HWND;i : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetSpecA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetSpecW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;

{$ifndef Unicode}
    function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
{$else Unicode}
    function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
{$endif Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFilePathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFilePathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;

{$ifndef Unicode}
    function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
{$else Unicode}
    function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
{$endif Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFolderPathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFolderPathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;

{$ifndef Unicode}
    function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
{$else Unicode}
    function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
{$endif Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFolderIDList(_hdlg:HWND;_pidl:LPVOID;_cbmax : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_SetControlText(_hdlg:HWND;_id : longint;_text : LPSTR) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_HideControl(_hdlg:HWND;_id : longint) : LRESULT;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_SetDefExt(_hdlg:HWND;_pszext : LPSTR) : LRESULT;

    function RegCloseKey(hKey:HKEY):LONG;

    function RegSetKeySecurity(hKey:HKEY; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):LONG;

    function RegFlushKey(hKey:HKEY):LONG;

    function RegGetKeySecurity(hKey:HKEY; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpcbSecurityDescriptor:LPDWORD):LONG;

    function RegNotifyChangeKeyValue(hKey:HKEY; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD; hEvent:HANDLE; fAsynchronus:WINBOOL):LONG;

    function IsValidCodePage(CodePage:UINT):WINBOOL;

    function GetACP:UINT;

    function GetOEMCP:UINT;

    function GetCPInfo(_para1:UINT; _para2:LPCPINFO):WINBOOL;

    function IsDBCSLeadByte(TestChar:BYTE):WINBOOL;

    function IsDBCSLeadByteEx(CodePage:UINT; TestChar:BYTE):WINBOOL;

    function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:LPCSTR; cchMultiByte:longint; lpWideCharStr:LPWSTR;
               cchWideChar:longint):longint;

    function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:LPCWSTR; cchWideChar:longint; lpMultiByteStr:LPSTR;
               cchMultiByte:longint; lpDefaultChar:LPCSTR; lpUsedDefaultChar:LPBOOL):longint;

    function IsValidLocale(Locale:LCID; dwFlags:DWORD):WINBOOL;

    function ConvertDefaultLocale(Locale:LCID):LCID;

    function GetThreadLocale:LCID;

    function SetThreadLocale(Locale:LCID):WINBOOL;

    function GetSystemDefaultLangID:LANGID;

    function GetUserDefaultLangID:LANGID;

    function GetSystemDefaultLCID:LCID;

    function GetUserDefaultLCID:LCID;

    function ReadConsoleOutputAttribute(hConsoleOutput:HANDLE; lpAttribute:LPWORD; nLength:DWORD; dwReadCoord:COORD; lpNumberOfAttrsRead:LPDWORD):WINBOOL;

(* Const before type ignored *)
    function WriteConsoleOutputAttribute(hConsoleOutput:HANDLE; var lpAttribute:WORD; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfAttrsWritten:LPDWORD):WINBOOL;

    function FillConsoleOutputAttribute(hConsoleOutput:HANDLE; wAttribute:WORD; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfAttrsWritten:LPDWORD):WINBOOL;

    function GetConsoleMode(hConsoleHandle:HANDLE; lpMode:LPDWORD):WINBOOL;

    function GetNumberOfConsoleInputEvents(hConsoleInput:HANDLE; var lpNumberOfEvents:DWORD):WINBOOL;

    function GetConsoleScreenBufferInfo(hConsoleOutput:HANDLE; lpConsoleScreenBufferInfo:PCONSOLE_SCREEN_BUFFER_INFO):WINBOOL;
    function GetConsoleScreenBufferInfo(hConsoleOutput:HANDLE; var lpConsoleScreenBufferInfo:CONSOLE_SCREEN_BUFFER_INFO):WINBOOL;

    function GetLargestConsoleWindowSize(hConsoleOutput:HANDLE):COORD;

    function GetConsoleCursorInfo(hConsoleOutput:HANDLE; lpConsoleCursorInfo:PCONSOLE_CURSOR_INFO):WINBOOL;
    function GetConsoleCursorInfo(hConsoleOutput:HANDLE; var lpConsoleCursorInfo:CONSOLE_CURSOR_INFO):WINBOOL;

    function GetNumberOfConsoleMouseButtons(lpNumberOfMouseButtons:LPDWORD):WINBOOL;

    function SetConsoleMode(hConsoleHandle:HANDLE; dwMode:DWORD):WINBOOL;

    function SetConsoleActiveScreenBuffer(hConsoleOutput:HANDLE):WINBOOL;

    function FlushConsoleInputBuffer(hConsoleInput:HANDLE):WINBOOL;

    function SetConsoleScreenBufferSize(hConsoleOutput:HANDLE; dwSize:COORD):WINBOOL;

    function SetConsoleCursorPosition(hConsoleOutput:HANDLE; dwCursorPosition:COORD):WINBOOL;

(* Const before type ignored *)
    function SetConsoleCursorInfo(hConsoleOutput:HANDLE; var lpConsoleCursorInfo:CONSOLE_CURSOR_INFO):WINBOOL;

(* Const before type ignored *)
    function SetConsoleWindowInfo(hConsoleOutput:HANDLE; bAbsolute:WINBOOL; var lpConsoleWindow:SMALL_RECT):WINBOOL;

    function SetConsoleTextAttribute(hConsoleOutput:HANDLE; wAttributes:WORD):WINBOOL;

    function SetConsoleCtrlHandler(HandlerRoutine:PHANDLER_ROUTINE; Add:WINBOOL):WINBOOL;

    function GenerateConsoleCtrlEvent(dwCtrlEvent:DWORD; dwProcessGroupId:DWORD):WINBOOL;

    function AllocConsole:WINBOOL;

    function FreeConsole:WINBOOL;

(* Const before type ignored *)
    function CreateConsoleScreenBuffer(dwDesiredAccess:DWORD; dwShareMode:DWORD; var lpSecurityAttributes:SECURITY_ATTRIBUTES; dwFlags:DWORD; lpScreenBufferData:LPVOID):HANDLE;

    function GetConsoleCP:UINT;

    function SetConsoleCP(wCodePageID:UINT):WINBOOL;

    function GetConsoleOutputCP:UINT;

    function SetConsoleOutputCP(wCodePageID:UINT):WINBOOL;

    function WNetConnectionDialog(hwnd:HWND; dwType:DWORD):DWORD;

    function WNetDisconnectDialog(hwnd:HWND; dwType:DWORD):DWORD;

    function WNetCloseEnum(hEnum:HANDLE):DWORD;

    function CloseServiceHandle(hSCObject:SC_HANDLE):WINBOOL;

    function ControlService(hService:SC_HANDLE; dwControl:DWORD; lpServiceStatus:LPSERVICE_STATUS):WINBOOL;

    function DeleteService(hService:SC_HANDLE):WINBOOL;

    function LockServiceDatabase(hSCManager:SC_HANDLE):SC_LOCK;

    function NotifyBootConfigStatus(BootAcceptable:WINBOOL):WINBOOL;

    function QueryServiceObjectSecurity(hService:SC_HANDLE; dwSecurityInformation:SECURITY_INFORMATION; lpSecurityDescriptor:PSECURITY_DESCRIPTOR; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;

    function QueryServiceStatus(hService:SC_HANDLE; lpServiceStatus:LPSERVICE_STATUS):WINBOOL;

    function SetServiceObjectSecurity(hService:SC_HANDLE; dwSecurityInformation:SECURITY_INFORMATION; lpSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;

    function SetServiceStatus(hServiceStatus:SERVICE_STATUS_HANDLE; lpServiceStatus:LPSERVICE_STATUS):WINBOOL;

    function UnlockServiceDatabase(ScLock:SC_LOCK):WINBOOL;

    { Extensions to OpenGL  }
(* Const before type ignored *)
    function ChoosePixelFormat(_para1:HDC; var _para2:PIXELFORMATDESCRIPTOR):longint;

    function DescribePixelFormat(_para1:HDC; _para2:longint; _para3:UINT; _para4:LPPIXELFORMATDESCRIPTOR):longint;

(* Const before type ignored *)
    function SetPixelFormat(_para1:HDC; _para2:longint;

       _para3:PPIXELFORMATDESCRIPTOR):WINBOOL;

    function SwapBuffers(_para1:HDC):WINBOOL;

    function DragQueryPoint(_para1:HDROP; _para2:LPPOINT):WINBOOL;

    procedure DragFinish(_para1:HDROP);

    procedure DragAcceptFiles(_para1:HWND; _para2:WINBOOL);

    function DuplicateIcon(_para1:HINST; _para2:HICON):HICON;

    { end of stuff from shellapi.h in old Cygnus headers  }
    { --------------------------------------------------  }
    { From ddeml.h in old Cygnus headers  }
    function DdeConnect(_para1:DWORD; _para2:HSZ; _para3:HSZ; var _para4:CONVCONTEXT):HCONV;

    function DdeDisconnect(_para1:HCONV):WINBOOL;

    function DdeFreeDataHandle(_para1:HDDEDATA):WINBOOL;

    function DdeGetData(_para1:HDDEDATA; var _para2:BYTE; _para3:DWORD; _para4:DWORD):DWORD;

    function DdeGetLastError(_para1:DWORD):UINT;

    function DdeNameService(_para1:DWORD; _para2:HSZ; _para3:HSZ; _para4:UINT):HDDEDATA;

    function DdePostAdvise(_para1:DWORD; _para2:HSZ; _para3:HSZ):WINBOOL;

    function DdeReconnect(_para1:HCONV):HCONV;

    function DdeUninitialize(_para1:DWORD):WINBOOL;

    function DdeCmpStringHandles(_para1:HSZ; _para2:HSZ):longint;

    function DdeCreateDataHandle(_para1:DWORD; _para2:LPBYTE; _para3:DWORD; _para4:DWORD; _para5:HSZ;
               _para6:UINT; _para7:UINT):HDDEDATA;

    { end of stuff from ddeml.h in old Cygnus headers  }
    { -----------------------------------------------  }
{$ifdef Unknown_functions}
    function NetUserEnum(_para1:LPWSTR; _para2:DWORD; _para3:DWORD; var _para4:LPBYTE; _para5:DWORD;
               _para6:LPDWORD; _para7:LPDWORD; _para8:LPDWORD):DWORD;

    function NetApiBufferFree(_para1:LPVOID):DWORD;

    function NetUserGetInfo(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:LPBYTE):DWORD;

    function NetGetDCName(_para1:LPWSTR; _para2:LPWSTR; var _para3:LPBYTE):DWORD;

    function NetGroupEnum(_para1:LPWSTR; _para2:DWORD; var _para3:LPBYTE; _para4:DWORD; _para5:LPDWORD;
               _para6:LPDWORD; _para7:LPDWORD):DWORD;

    function NetLocalGroupEnum(_para1:LPWSTR; _para2:DWORD; var _para3:LPBYTE; _para4:DWORD; _para5:LPDWORD;
               _para6:LPDWORD; _para7:LPDWORD):DWORD;
{$endif Unknown_functions}

    procedure SHAddToRecentDocs(_para1:UINT; _para2:LPCVOID);

    function SHBrowseForFolder(_para1:LPBROWSEINFO):LPITEMIDLIST;

    procedure SHChangeNotify(_para1:LONG; _para2:UINT; _para3:LPCVOID; _para4:LPCVOID);

    function SHFileOperation(_para1:LPSHFILEOPSTRUCT):longint;

    procedure SHFreeNameMappings(_para1:HANDLE);

    { Define when SHELLFOLDER is defined.
    HRESULT WINAPI
    SHGetDataFromIDList (LPSHELLFOLDER, LPCITEMIDLIST, int, PVOID, int);

    HRESULT WINAPI
    SHGetDesktopFolder (LPSHELLFOLDER);
     }
    (* far ignored *)
    function SHGetFileInfo(_para1:LPCTSTR; _para2:DWORD; var _para3:SHFILEINFO; _para4:UINT; _para5:UINT):DWORD;

    { Define when IUnknown is defined.
    HRESULT WINAPI
    SHGetInstanceExplorer (IUnknown   );
     }
    { Define when MALLOC is defined.
    HRESULT WINAPI
    SHGetMalloc (LPMALLOC  );
     }
    function SHGetPathFromIDList(_para1:LPCITEMIDLIST; _para2:LPTSTR):WINBOOL;

    function SHGetSpecialFolderLocation(_para1:HWND; _para2:longint; var _para3:LPITEMIDLIST):HRESULT;

    { Define when REFCLSID is defined.
    HRESULT WINAPI
    SHLoadInProc (REFCLSID);
     }
{ C++ end of extern C conditionnal removed }
    { __cplusplus  }
{$endif}
    { _GNU_H_WINDOWS32_FUNCTIONS  }

{$endif read_interface}

{$ifndef windows_include_files}
  implementation

    const External_library='kernel32'; {Setup as you need!}

{$endif not windows_include_files}

{$ifdef read_implementation}

{$ifdef Unknown_functions}
{ WARNING: function not found !!}
  function AbnormalTermination:WINBOOL; external External_library name 'AbnormalTermination';
{$endif Unknown_functions}


(*  function AbortDoc(_para1:HDC):longint; external 'gdi32' name 'AbortDoc';
*)

  function AbortPath(_para1:HDC):WINBOOL; external 'gdi32' name 'AbortPath';

  function AbortPrinter(_para1:HANDLE):WINBOOL; external 'spoolss' name 'AbortPrinter';

{$ifdef Unknown_functions}
{ WARNING: function not found !!}
  function AbortProc(_para1:HDC; _para2:longint):WINBOOL; external External_library name 'AbortProc';
{$endif Unknown_functions}


{$ifndef windows_include_files}
  function AbortSystemShutdown(_para1:LPTSTR):WINBOOL; external 'advapi32' name 'AbortSystemShutdownA';
{$endif windows_include_files}

  function AccessCheck(pSecurityDescriptor:PSECURITY_DESCRIPTOR; ClientToken:HANDLE; DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; PrivilegeSet:PPRIVILEGE_SET;
             PrivilegeSetLength:LPDWORD; GrantedAccess:LPDWORD; AccessStatus:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheck';

{$ifndef windows_include_files}
  function AccessCheckAndAuditAlarm(SubsystemName:LPCTSTR; HandleId:LPVOID; ObjectTypeName:LPTSTR; ObjectName:LPTSTR; SecurityDescriptor:PSECURITY_DESCRIPTOR;
             DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; ObjectCreation:WINBOOL; GrantedAccess:LPDWORD; AccessStatus:LPBOOL;
             pfGenerateOnClose:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheckAndAuditAlarmA';
{$endif windows_include_files}

  function InterlockedIncrement(lpAddend:LPLONG):LONG; external 'kernel32' name 'InterlockedIncrement';

  function InterlockedDecrement(lpAddend:LPLONG):LONG; external 'kernel32' name 'InterlockedDecrement';

  function InterlockedExchange(Target:LPLONG; Value:LONG):LONG; external 'kernel32' name 'InterlockedExchange';

  function FreeResource(hResData:HGLOBAL):WINBOOL; external 'kernel32' name 'FreeResource';

  function LockResource(hResData:HGLOBAL):LPVOID; external 'kernel32' name 'LockResource';

{$ifdef Unknown_functions}
{ WARNING: function not found !!}
  function WinMain(hInstance:HINST; hPrevInstance:HINST; lpCmdLine:LPSTR; nShowCmd:longint):longint; external External_library name 'WinMain';
{$endif Unknown_functions}


  function FreeLibrary(hLibModule:HINST):WINBOOL; external 'kernel32' name 'FreeLibrary';

  procedure FreeLibraryAndExitThread(hLibModule:HMODULE; dwExitCode:DWORD); external 'kernel32' name 'FreeLibraryAndExitThread';

  function DisableThreadLibraryCalls(hLibModule:HMODULE):WINBOOL; external 'kernel32' name 'DisableThreadLibraryCalls';

  function GetProcAddress(hModule:HINST; lpProcName:LPCSTR):FARPROC; external 'kernel32' name 'GetProcAddress';

  function GetVersion:DWORD; external 'kernel32' name 'GetVersion';

  function GlobalAlloc(uFlags:UINT; dwBytes:DWORD):HGLOBAL; external 'kernel32' name 'GlobalAlloc';

  function GlobalDiscard(hglbMem:HGLOBAL):HGLOBAL;
    {CDECL; so it is internal !!}
    begin
       GlobalDiscard:=GlobalReAlloc(hglbMem,0,GMEM_MOVEABLE);
    end;

  function GlobalReAlloc(hMem:HGLOBAL; dwBytes:DWORD; uFlags:UINT):HGLOBAL; external 'kernel32' name 'GlobalReAlloc';

  function GlobalSize(hMem:HGLOBAL):DWORD; external 'kernel32' name 'GlobalSize';

  function GlobalFlags(hMem:HGLOBAL):UINT; external 'kernel32' name 'GlobalFlags';

  function GlobalLock(hMem:HGLOBAL):LPVOID; external 'kernel32' name 'GlobalLock';

  function GlobalHandle(pMem:LPCVOID):HGLOBAL; external 'kernel32' name 'GlobalHandle';

  function GlobalUnlock(hMem:HGLOBAL):WINBOOL; external 'kernel32' name 'GlobalUnlock';

  function GlobalFree(hMem:HGLOBAL):HGLOBAL; external 'kernel32' name 'GlobalFree';

  function GlobalCompact(dwMinFree:DWORD):UINT; external 'kernel32' name 'GlobalCompact';

  procedure GlobalFix(hMem:HGLOBAL); external 'kernel32' name 'GlobalFix';

  procedure GlobalUnfix(hMem:HGLOBAL); external 'kernel32' name 'GlobalUnfix';

  function GlobalWire(hMem:HGLOBAL):LPVOID; external 'kernel32' name 'GlobalWire';

  function GlobalUnWire(hMem:HGLOBAL):WINBOOL; external 'kernel32' name 'GlobalUnWire';

  procedure GlobalMemoryStatus(lpBuffer:LPMEMORYSTATUS); external 'kernel32' name 'GlobalMemoryStatus';

  function LocalAlloc(uFlags:UINT; uBytes:UINT):HLOCAL; external 'kernel32' name 'LocalAlloc';

  function LocalDiscard(hlocMem:HLOCAL):HLOCAL;
    {CDECL; so it is internal }
    begin
       LocalDiscard := LocalReAlloc(hlocMem,0,LMEM_MOVEABLE);
    end;

  function LocalReAlloc(hMem:HLOCAL; uBytes:UINT; uFlags:UINT):HLOCAL; external 'kernel32' name 'LocalReAlloc';

  function LocalLock(hMem:HLOCAL):LPVOID; external 'kernel32' name 'LocalLock';

  function LocalHandle(pMem:LPCVOID):HLOCAL; external 'kernel32' name 'LocalHandle';

  function LocalUnlock(hMem:HLOCAL):WINBOOL; external 'kernel32' name 'LocalUnlock';

  function LocalSize(hMem:HLOCAL):UINT; external 'kernel32' name 'LocalSize';

  function LocalFlags(hMem:HLOCAL):UINT; external 'kernel32' name 'LocalFlags';

  function LocalFree(hMem:HLOCAL):HLOCAL; external 'kernel32' name 'LocalFree';

  function LocalShrink(hMem:HLOCAL; cbNewSize:UINT):UINT; external 'kernel32' name 'LocalShrink';

  function LocalCompact(uMinFree:UINT):UINT; external 'kernel32' name 'LocalCompact';

  function FlushInstructionCache(hProcess:HANDLE; lpBaseAddress:LPCVOID; dwSize:DWORD):WINBOOL; external 'kernel32' name 'FlushInstructionCache';

  function VirtualAlloc(lpAddress:LPVOID; dwSize:DWORD; flAllocationType:DWORD; flProtect:DWORD):LPVOID; external 'kernel32' name 'VirtualAlloc';

  function VirtualFree(lpAddress:LPVOID; dwSize:DWORD; dwFreeType:DWORD):WINBOOL; external 'kernel32' name 'VirtualFree';

  function VirtualProtect(lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect:PDWORD):WINBOOL; external 'kernel32' name 'VirtualProtect';

  function VirtualQuery(lpAddress:LPCVOID; lpBuffer:PMEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD; external 'kernel32' name 'VirtualQuery';

  function VirtualQuery(lpAddress:LPCVOID; var Buffer:MEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD; external 'kernel32' name 'VirtualQuery';

  function VirtualProtectEx(hProcess:HANDLE; lpAddress:LPVOID; dwSize:DWORD; flNewProtect:DWORD; lpflOldProtect:PDWORD):WINBOOL; external 'kernel32' name 'VirtualProtectEx';

  function VirtualQueryEx(hProcess:HANDLE; lpAddress:LPCVOID; lpBuffer:PMEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD; external 'kernel32' name 'VirtualQueryEx';

  function VirtualQueryEx(hProcess:HANDLE; lpAddress:LPCVOID; var Buffer:MEMORY_BASIC_INFORMATION; dwLength:DWORD):DWORD; external 'kernel32' name 'VirtualQueryEx';

  function HeapCreate(flOptions:DWORD; dwInitialSize:DWORD; dwMaximumSize:DWORD):HANDLE; external 'kernel32' name 'HeapCreate';

  function HeapDestroy(hHeap:HANDLE):WINBOOL; external 'kernel32' name 'HeapDestroy';

  function HeapAlloc(hHeap:HANDLE; dwFlags:DWORD; dwBytes:DWORD):LPVOID; external 'kernel32' name 'HeapAlloc';

  function HeapReAlloc(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPVOID; dwBytes:DWORD):LPVOID; external 'kernel32' name 'HeapReAlloc';

  function HeapFree(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPVOID):WINBOOL; external 'kernel32' name 'HeapFree';

  function HeapSize(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPCVOID):DWORD; external 'kernel32' name 'HeapSize';

  function HeapValidate(hHeap:HANDLE; dwFlags:DWORD; lpMem:LPCVOID):WINBOOL; external 'kernel32' name 'HeapValidate';

  function HeapCompact(hHeap:HANDLE; dwFlags:DWORD):UINT; external 'kernel32' name 'HeapCompact';

  function GetProcessHeap:HANDLE; external 'kernel32' name 'GetProcessHeap';

  function GetProcessHeaps(NumberOfHeaps:DWORD; ProcessHeaps:PHANDLE):DWORD; external 'kernel32' name 'GetProcessHeaps';

  function HeapLock(hHeap:HANDLE):WINBOOL; external 'kernel32' name 'HeapLock';

  function HeapUnlock(hHeap:HANDLE):WINBOOL; external 'kernel32' name 'HeapUnlock';

  function HeapWalk(hHeap:HANDLE; lpEntry:LPPROCESS_HEAP_ENTRY):WINBOOL; external 'kernel32' name 'HeapWalk';

  function GetProcessAffinityMask(hProcess:HANDLE; lpProcessAffinityMask:LPDWORD; lpSystemAffinityMask:LPDWORD):WINBOOL; external 'kernel32' name 'GetProcessAffinityMask';

  function GetProcessTimes(hProcess:HANDLE; lpCreationTime:LPFILETIME; lpExitTime:LPFILETIME; lpKernelTime:LPFILETIME; lpUserTime:LPFILETIME):WINBOOL; external 'kernel32' name 'GetProcessTimes';

  function GetProcessWorkingSetSize(hProcess:HANDLE; lpMinimumWorkingSetSize:LPDWORD; lpMaximumWorkingSetSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetProcessWorkingSetSize';

  function SetProcessWorkingSetSize(hProcess:HANDLE; dwMinimumWorkingSetSize:DWORD; dwMaximumWorkingSetSize:DWORD):WINBOOL; external 'kernel32' name 'SetProcessWorkingSetSize';

  function OpenProcess(dwDesiredAccess:DWORD; bInheritHandle:WINBOOL; dwProcessId:DWORD):HANDLE; external 'kernel32' name 'OpenProcess';

  function GetCurrentProcess:HANDLE; external 'kernel32' name 'GetCurrentProcess';

  function GetCurrentProcessId:DWORD; external 'kernel32' name 'GetCurrentProcessId';

  procedure ExitProcess(uExitCode:UINT);external 'kernel32' name 'ExitProcess';

  function TerminateProcess(hProcess:HANDLE; uExitCode:UINT):WINBOOL; external 'kernel32' name 'TerminateProcess';

  function GetExitCodeProcess(hProcess:HANDLE; lpExitCode:LPDWORD):WINBOOL; external 'kernel32' name 'GetExitCodeProcess';

  procedure FatalExit(ExitCode:longint); external 'kernel32' name 'FatalExit';

  procedure RaiseException(dwExceptionCode:DWORD; dwExceptionFlags:DWORD; nNumberOfArguments:DWORD; var lpArguments:DWORD); external 'kernel32' name 'RaiseException';

  function UnhandledExceptionFilter(var ExceptionInfo:emptyrecord):LONG; external 'kernel32' name 'UnhandledExceptionFilter';

  function CreateThread(lpThreadAttributes:LPSECURITY_ATTRIBUTES; dwStackSize:DWORD; lpStartAddress:LPTHREAD_START_ROUTINE; lpParameter:LPVOID; dwCreationFlags:DWORD;
             var lpThreadId:DWORD):HANDLE; external 'kernel32' name 'CreateThread';

  function CreateRemoteThread(hProcess:HANDLE; lpThreadAttributes:LPSECURITY_ATTRIBUTES; dwStackSize:DWORD; lpStartAddress:LPTHREAD_START_ROUTINE; lpParameter:LPVOID;
             dwCreationFlags:DWORD; lpThreadId:LPDWORD):HANDLE; external 'kernel32' name 'CreateRemoteThread';

  function GetCurrentThread:HANDLE; external 'kernel32' name 'GetCurrentThread';

  function GetCurrentThreadId:DWORD; external 'kernel32' name 'GetCurrentThreadId';

  function SetThreadAffinityMask(hThread:HANDLE; dwThreadAffinityMask:DWORD):DWORD; external 'kernel32' name 'SetThreadAffinityMask';

  function SetThreadPriority(hThread:HANDLE; nPriority:longint):WINBOOL; external 'kernel32' name 'SetThreadPriority';

  function GetThreadPriority(hThread:HANDLE):longint; external 'kernel32' name 'GetThreadPriority';

  function GetThreadTimes(hThread:HANDLE; lpCreationTime:LPFILETIME; lpExitTime:LPFILETIME; lpKernelTime:LPFILETIME; lpUserTime:LPFILETIME):WINBOOL; external 'kernel32' name 'GetThreadTimes';

  procedure ExitThread(dwExitCode:DWORD); external 'kernel32' name 'ExitThread';

  function TerminateThread(hThread:HANDLE; dwExitCode:DWORD):WINBOOL; external 'kernel32' name 'TerminateThread';

  function GetExitCodeThread(hThread:HANDLE; lpExitCode:LPDWORD):WINBOOL; external 'kernel32' name 'GetExitCodeThread';

  function GetThreadSelectorEntry(hThread:HANDLE; dwSelector:DWORD; lpSelectorEntry:LPLDT_ENTRY):WINBOOL; external 'kernel32' name 'GetThreadSelectorEntry';

  function GetLastError:DWORD; external 'kernel32' name 'GetLastError';

  procedure SetLastError(dwErrCode:DWORD); external 'kernel32' name 'SetLastError';

  function GetOverlappedResult(hFile:HANDLE; const lpOverlapped:TOVERLAPPED; var lpNumberOfBytesTransferred:DWORD; bWait:WINBOOL):WINBOOL; external 'kernel32' name 'GetOverlappedResult';

  function CreateIoCompletionPort(FileHandle:HANDLE; ExistingCompletionPort:HANDLE; CompletionKey:DWORD; NumberOfConcurrentThreads:DWORD):HANDLE; external 'kernel32' name 'CreateIoCompletionPort';

  function GetQueuedCompletionStatus(CompletionPort:HANDLE; lpNumberOfBytesTransferred:LPDWORD; lpCompletionKey:LPDWORD; var lpOverlapped:LPOVERLAPPED; dwMilliseconds:DWORD):WINBOOL; external 'kernel32' name 'GetQueuedCompletionStatus';

  function SetErrorMode(uMode:UINT):UINT; external 'kernel32' name 'SetErrorMode';

  function ReadProcessMemory(hProcess:HANDLE; lpBaseAddress:LPCVOID; lpBuffer:LPVOID; nSize:DWORD; lpNumberOfBytesRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadProcessMemory';

  function WriteProcessMemory(hProcess:HANDLE; lpBaseAddress:LPVOID; lpBuffer:LPVOID; nSize:DWORD; lpNumberOfBytesWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteProcessMemory';

  function GetThreadContext(hThread:HANDLE; lpContext:LPCONTEXT):WINBOOL; external 'kernel32' name 'GetThreadContext';

  function SetThreadContext(hThread:HANDLE; var lpContext:CONTEXT):WINBOOL; external 'kernel32' name 'SetThreadContext';

  function SuspendThread(hThread:HANDLE):DWORD; external 'kernel32' name 'SuspendThread';

  function ResumeThread(hThread:HANDLE):DWORD; external 'kernel32' name 'ResumeThread';

  procedure DebugBreak; external 'kernel32' name 'DebugBreak';

  function WaitForDebugEvent(lpDebugEvent:LPDEBUG_EVENT; dwMilliseconds:DWORD):WINBOOL; external 'kernel32' name 'WaitForDebugEvent';

  function ContinueDebugEvent(dwProcessId:DWORD; dwThreadId:DWORD; dwContinueStatus:DWORD):WINBOOL; external 'kernel32' name 'ContinueDebugEvent';

  function DebugActiveProcess(dwProcessId:DWORD):WINBOOL; external 'kernel32' name 'DebugActiveProcess';

  procedure InitializeCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external 'kernel32' name 'InitializeCriticalSection';

  procedure EnterCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external 'kernel32' name 'EnterCriticalSection';

  procedure LeaveCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external 'kernel32' name 'LeaveCriticalSection';

  procedure DeleteCriticalSection(lpCriticalSection:LPCRITICAL_SECTION); external 'kernel32' name 'DeleteCriticalSection';

  function SetEvent(hEvent:HANDLE):WINBOOL; external 'kernel32' name 'SetEvent';

  function ResetEvent(hEvent:HANDLE):WINBOOL; external 'kernel32' name 'ResetEvent';

  function PulseEvent(hEvent:HANDLE):WINBOOL; external 'kernel32' name 'PulseEvent';

  function ReleaseSemaphore(hSemaphore:HANDLE; lReleaseCount:LONG; lpPreviousCount:LPLONG):WINBOOL; external 'kernel32' name 'ReleaseSemaphore';

  function ReleaseMutex(hMutex:HANDLE):WINBOOL; external 'kernel32' name 'ReleaseMutex';

  function WaitForSingleObject(hHandle:HANDLE; dwMilliseconds:DWORD):DWORD; external 'kernel32' name 'WaitForSingleObject';

  function WaitForMultipleObjects(nCount:DWORD; var lpHandles:HANDLE; bWaitAll:WINBOOL; dwMilliseconds:DWORD):DWORD; external 'kernel32' name 'WaitForMultipleObjects';

  procedure Sleep(dwMilliseconds:DWORD); external 'kernel32' name 'Sleep';

  function LoadResource(hModule:HINST; hResInfo:HRSRC):HGLOBAL; external 'kernel32' name 'LoadResource';

  function SizeofResource(hModule:HINST; hResInfo:HRSRC):DWORD; external 'kernel32' name 'SizeofResource';

  function GlobalDeleteAtom(nAtom:ATOM):ATOM; external 'kernel32' name 'GlobalDeleteAtom';

  function InitAtomTable(nSize:DWORD):WINBOOL; external 'kernel32' name 'InitAtomTable';

  function DeleteAtom(nAtom:ATOM):ATOM; external 'kernel32' name 'DeleteAtom';

  function SetHandleCount(uNumber:UINT):UINT; external 'kernel32' name 'SetHandleCount';

  function GetLogicalDrives:DWORD; external 'kernel32' name 'GetLogicalDrives';

  function LockFile(hFile:HANDLE; dwFileOffsetLow:DWORD; dwFileOffsetHigh:DWORD; nNumberOfBytesToLockLow:DWORD; nNumberOfBytesToLockHigh:DWORD):WINBOOL; external 'kernel32' name 'LockFile';

  function UnlockFile(hFile:HANDLE; dwFileOffsetLow:DWORD; dwFileOffsetHigh:DWORD; nNumberOfBytesToUnlockLow:DWORD; nNumberOfBytesToUnlockHigh:DWORD):WINBOOL; external 'kernel32' name 'UnlockFile';

  function LockFileEx(hFile:HANDLE; dwFlags:DWORD; dwReserved:DWORD; nNumberOfBytesToLockLow:DWORD; nNumberOfBytesToLockHigh:DWORD;
             lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'LockFileEx';

  function UnlockFileEx(hFile:HANDLE; dwReserved:DWORD; nNumberOfBytesToUnlockLow:DWORD; nNumberOfBytesToUnlockHigh:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'UnlockFileEx';

  function GetFileInformationByHandle(hFile:HANDLE; lpFileInformation:LPBY_HANDLE_FILE_INFORMATION):WINBOOL; external 'kernel32' name 'GetFileInformationByHandle';

  function GetFileType(hFile:HANDLE):DWORD; external 'kernel32' name 'GetFileType';

  function GetFileSize(hFile:HANDLE; lpFileSizeHigh:LPDWORD):DWORD; external 'kernel32' name 'GetFileSize';

  function GetStdHandle(nStdHandle:DWORD):HANDLE; external 'kernel32' name 'GetStdHandle';

  function SetStdHandle(nStdHandle:DWORD; hHandle:HANDLE):WINBOOL; external 'kernel32' name 'SetStdHandle';

  function WriteFile(hFile:HANDLE; var lpBuffer; nNumberOfBytesToWrite:DWORD; var lpNumberOfBytesWritten:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'WriteFile';

  function ReadFile(hFile:HANDLE; var lpBuffer; nNumberOfBytesToRead:DWORD; var lpNumberOfBytesRead:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'ReadFile';

  function FlushFileBuffers(hFile:HANDLE):WINBOOL; external 'kernel32' name 'FlushFileBuffers';

  function DeviceIoControl(hDevice:HANDLE; dwIoControlCode:DWORD; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID;
             nOutBufferSize:DWORD; lpBytesReturned:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'DeviceIoControl';

  function SetEndOfFile(hFile:HANDLE):WINBOOL; external 'kernel32' name 'SetEndOfFile';

  function SetFilePointer(hFile:HANDLE; lDistanceToMove:LONG; lpDistanceToMoveHigh:PLONG; dwMoveMethod:DWORD):DWORD; external 'kernel32' name 'SetFilePointer';

  function FindClose(hFindFile:HANDLE):WINBOOL; external 'kernel32' name 'FindClose';

  function GetFileTime(hFile:HANDLE; lpCreationTime:LPFILETIME; lpLastAccessTime:LPFILETIME; lpLastWriteTime:LPFILETIME):WINBOOL; external 'kernel32' name 'GetFileTime';

  function SetFileTime(hFile:HANDLE; var lpCreationTime:FILETIME; var lpLastAccessTime:FILETIME; var lpLastWriteTime:FILETIME):WINBOOL; external 'kernel32' name 'SetFileTime';

  function CloseHandle(hObject:HANDLE):WINBOOL; external 'kernel32' name 'CloseHandle';

  function DuplicateHandle(hSourceProcessHandle:HANDLE; hSourceHandle:HANDLE; hTargetProcessHandle:HANDLE; lpTargetHandle:LPHANDLE; dwDesiredAccess:DWORD;
             bInheritHandle:WINBOOL; dwOptions:DWORD):WINBOOL; external 'kernel32' name 'DuplicateHandle';

  function GetHandleInformation(hObject:HANDLE; lpdwFlags:LPDWORD):WINBOOL; external 'kernel32' name 'GetHandleInformation';

  function SetHandleInformation(hObject:HANDLE; dwMask:DWORD; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'SetHandleInformation';

  function LoadModule(lpModuleName:LPCSTR; lpParameterBlock:LPVOID):DWORD; external 'kernel32' name 'LoadModule';

  function WinExec(lpCmdLine:LPCSTR; uCmdShow:UINT):UINT; external 'kernel32' name 'WinExec';

  function ClearCommBreak(hFile:HANDLE):WINBOOL; external 'kernel32' name 'ClearCommBreak';

  function ClearCommError(hFile:HANDLE; lpErrors:LPDWORD; lpStat:LPCOMSTAT):WINBOOL; external 'kernel32' name 'ClearCommError';

  function SetupComm(hFile:HANDLE; dwInQueue:DWORD; dwOutQueue:DWORD):WINBOOL; external 'kernel32' name 'SetupComm';

  function EscapeCommFunction(hFile:HANDLE; dwFunc:DWORD):WINBOOL; external 'kernel32' name 'EscapeCommFunction';

  function GetCommConfig(hCommDev:HANDLE; lpCC:LPCOMMCONFIG; lpdwSize:LPDWORD):WINBOOL; external 'kernel32' name 'GetCommConfig';

  function GetCommMask(hFile:HANDLE; var lpEvtMask:DWORD):WINBOOL; external 'kernel32' name 'GetCommMask';

  function GetCommProperties(hFile:HANDLE; var lpCommProp:TCOMMPROP):WINBOOL; external 'kernel32' name 'GetCommProperties';

  function GetCommModemStatus(hFile:HANDLE; var lpModemStat:DWORD):WINBOOL; external 'kernel32' name 'GetCommModemStatus';

  function GetCommState(hFile:HANDLE; var lpDCB:TDCB):WINBOOL; external 'kernel32' name 'GetCommState';

  function GetCommTimeouts(hFile:HANDLE; var lpCommTimeouts:TCOMMTIMEOUTS):WINBOOL; external 'kernel32' name 'GetCommTimeouts';

  function PurgeComm(hFile:HANDLE; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'PurgeComm';

  function SetCommBreak(hFile:HANDLE):WINBOOL; external 'kernel32' name 'SetCommBreak';

  function SetCommConfig(hCommDev:HANDLE; lpCC:LPCOMMCONFIG; dwSize:DWORD):WINBOOL; external 'kernel32' name 'SetCommConfig';

  function SetCommMask(hFile:HANDLE; dwEvtMask:DWORD):WINBOOL; external 'kernel32' name 'SetCommMask';

  function SetCommState(hFile:HANDLE; var lpDCB:TDCB):WINBOOL; external 'kernel32' name 'SetCommState';

  function SetCommTimeouts(hFile:HANDLE; var lpCommTimeouts:TCOMMTIMEOUTS):WINBOOL; external 'kernel32' name 'SetCommTimeouts';

  function TransmitCommChar(hFile:HANDLE; cChar:char):WINBOOL; external 'kernel32' name 'TransmitCommChar';

  function WaitCommEvent(hFile:HANDLE; var lpEvtMask:DWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'WaitCommEvent';

  function SetTapePosition(hDevice:HANDLE; dwPositionMethod:DWORD; dwPartition:DWORD; dwOffsetLow:DWORD; dwOffsetHigh:DWORD;
             bImmediate:WINBOOL):DWORD; external 'kernel32' name 'SetTapePosition';

  function GetTapePosition(hDevice:HANDLE; dwPositionType:DWORD; lpdwPartition:LPDWORD; lpdwOffsetLow:LPDWORD; lpdwOffsetHigh:LPDWORD):DWORD; external 'kernel32' name 'GetTapePosition';

  function PrepareTape(hDevice:HANDLE; dwOperation:DWORD; bImmediate:WINBOOL):DWORD; external 'kernel32' name 'PrepareTape';

  function EraseTape(hDevice:HANDLE; dwEraseType:DWORD; bImmediate:WINBOOL):DWORD; external 'kernel32' name 'EraseTape';

  function CreateTapePartition(hDevice:HANDLE; dwPartitionMethod:DWORD; dwCount:DWORD; dwSize:DWORD):DWORD; external 'kernel32' name 'CreateTapePartition';

  function WriteTapemark(hDevice:HANDLE; dwTapemarkType:DWORD; dwTapemarkCount:DWORD; bImmediate:WINBOOL):DWORD; external 'kernel32' name 'WriteTapemark';

  function GetTapeStatus(hDevice:HANDLE):DWORD; external 'kernel32' name 'GetTapeStatus';

  function GetTapeParameters(hDevice:HANDLE; dwOperation:DWORD; lpdwSize:LPDWORD; lpTapeInformation:LPVOID):DWORD; external 'kernel32' name 'GetTapeParameters';

  function SetTapeParameters(hDevice:HANDLE; dwOperation:DWORD; lpTapeInformation:LPVOID):DWORD; external 'kernel32' name 'SetTapeParameters';

  function Beep(dwFreq:DWORD; dwDuration:DWORD):WINBOOL; external 'kernel32' name 'Beep';

{$ifdef Unknown_functions}
{ WARNING: functions not found !!}
  procedure OpenSound; external External_library name 'OpenSound';

  procedure CloseSound; external External_library name 'CloseSound';

  procedure StartSound; external External_library name 'StartSound';

  procedure StopSound; external External_library name 'StopSound';

  function WaitSoundState(nState:DWORD):DWORD; external External_library name 'WaitSoundState';

  function SyncAllVoices:DWORD; external External_library name 'SyncAllVoices';

  function CountVoiceNotes(nVoice:DWORD):DWORD; external External_library name 'CountVoiceNotes';

  function GetThresholdEvent:LPDWORD; external External_library name 'GetThresholdEvent';

  function GetThresholdStatus:DWORD; external External_library name 'GetThresholdStatus';

  function SetSoundNoise(nSource:DWORD; nDuration:DWORD):DWORD; external External_library name 'SetSoundNoise';

  function SetVoiceAccent(nVoice:DWORD; nTempo:DWORD; nVolume:DWORD; nMode:DWORD; nPitch:DWORD):DWORD; external External_library name 'SetVoiceAccent';

  function SetVoiceEnvelope(nVoice:DWORD; nShape:DWORD; nRepeat:DWORD):DWORD; external External_library name 'SetVoiceEnvelope';

  function SetVoiceNote(nVoice:DWORD; nValue:DWORD; nLength:DWORD; nCdots:DWORD):DWORD; external External_library name 'SetVoiceNote';

  function SetVoiceQueueSize(nVoice:DWORD; nBytes:DWORD):DWORD; external External_library name 'SetVoiceQueueSize';

  function SetVoiceSound(nVoice:DWORD; Frequency:DWORD; nDuration:DWORD):DWORD; external External_library name 'SetVoiceSound';

  function SetVoiceThreshold(nVoice:DWORD; nNotes:DWORD):DWORD; external External_library name 'SetVoiceThreshold';
{$endif Unknown_functions}


  function MulDiv(nNumber:longint; nNumerator:longint; nDenominator:longint):longint; external 'kernel32' name 'MulDiv';

  procedure GetSystemTime(lpSystemTime:LPSYSTEMTIME); external 'kernel32' name 'GetSystemTime';

  function SetSystemTime(var lpSystemTime:SYSTEMTIME):WINBOOL; external 'kernel32' name 'SetSystemTime';

  procedure GetLocalTime(lpSystemTime:LPSYSTEMTIME); external 'kernel32' name 'GetLocalTime';

  function SetLocalTime(var lpSystemTime:SYSTEMTIME):WINBOOL; external 'kernel32' name 'SetLocalTime';

  procedure GetSystemInfo(lpSystemInfo:LPSYSTEM_INFO); external 'kernel32' name 'GetSystemInfo';

  procedure GetSystemInfo(var SystemInfo:SYSTEM_INFO); external 'kernel32' name 'GetSystemInfo';

  function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION; lpUniversalTime:LPSYSTEMTIME; lpLocalTime:LPSYSTEMTIME):WINBOOL; external 'kernel32' name 'SystemTimeToTzSpecificLocalTime';

  function GetTimeZoneInformation(lpTimeZoneInformation:LPTIME_ZONE_INFORMATION):DWORD; external 'kernel32' name 'GetTimeZoneInformation';

  function SetTimeZoneInformation(var lpTimeZoneInformation:TIME_ZONE_INFORMATION):WINBOOL; external 'kernel32' name 'SetTimeZoneInformation';

  function SystemTimeToFileTime(var lpSystemTime:SYSTEMTIME; lpFileTime:LPFILETIME):WINBOOL; external 'kernel32' name 'SystemTimeToFileTime';

  function FileTimeToLocalFileTime(var lpFileTime:FILETIME; lpLocalFileTime:LPFILETIME):WINBOOL; external 'kernel32' name 'FileTimeToLocalFileTime';

  function LocalFileTimeToFileTime(var lpLocalFileTime:FILETIME; lpFileTime:LPFILETIME):WINBOOL; external 'kernel32' name 'LocalFileTimeToFileTime';

  function FileTimeToSystemTime(var lpFileTime:FILETIME; lpSystemTime:LPSYSTEMTIME):WINBOOL; external 'kernel32' name 'FileTimeToSystemTime';

  function CompareFileTime(var lpFileTime1:FILETIME; var lpFileTime2:FILETIME):LONG; external 'kernel32' name 'CompareFileTime';

  function FileTimeToDosDateTime(var lpFileTime:FILETIME; lpFatDate:LPWORD; lpFatTime:LPWORD):WINBOOL; external 'kernel32' name 'FileTimeToDosDateTime';

  function DosDateTimeToFileTime(wFatDate:WORD; wFatTime:WORD; lpFileTime:LPFILETIME):WINBOOL; external 'kernel32' name 'DosDateTimeToFileTime';

  function GetTickCount:DWORD; external 'kernel32' name 'GetTickCount';

  function SetSystemTimeAdjustment(dwTimeAdjustment:DWORD; bTimeAdjustmentDisabled:WINBOOL):WINBOOL; external 'kernel32' name 'SetSystemTimeAdjustment';

  function GetSystemTimeAdjustment(lpTimeAdjustment:PDWORD; lpTimeIncrement:PDWORD; lpTimeAdjustmentDisabled:PWINBOOL):WINBOOL; external 'kernel32' name 'GetSystemTimeAdjustment';

  function CreatePipe(hReadPipe:PHANDLE; hWritePipe:PHANDLE; lpPipeAttributes:LPSECURITY_ATTRIBUTES; nSize:DWORD):WINBOOL; external 'kernel32' name 'CreatePipe';

  function ConnectNamedPipe(hNamedPipe:HANDLE; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'ConnectNamedPipe';

  function DisconnectNamedPipe(hNamedPipe:HANDLE):WINBOOL; external 'kernel32' name 'DisconnectNamedPipe';

  function SetNamedPipeHandleState(hNamedPipe:HANDLE; lpMode:LPDWORD; lpMaxCollectionCount:LPDWORD; lpCollectDataTimeout:LPDWORD):WINBOOL; external 'kernel32' name 'SetNamedPipeHandleState';

  function GetNamedPipeInfo(hNamedPipe:HANDLE; lpFlags:LPDWORD; lpOutBufferSize:LPDWORD; lpInBufferSize:LPDWORD; lpMaxInstances:LPDWORD):WINBOOL; external 'kernel32' name 'GetNamedPipeInfo';

  function PeekNamedPipe(hNamedPipe:HANDLE; lpBuffer:LPVOID; nBufferSize:DWORD; lpBytesRead:LPDWORD; lpTotalBytesAvail:LPDWORD;
             lpBytesLeftThisMessage:LPDWORD):WINBOOL; external 'kernel32' name 'PeekNamedPipe';

  function TransactNamedPipe(hNamedPipe:HANDLE; lpInBuffer:LPVOID; nInBufferSize:DWORD; lpOutBuffer:LPVOID; nOutBufferSize:DWORD;
             lpBytesRead:LPDWORD; lpOverlapped:LPOVERLAPPED):WINBOOL; external 'kernel32' name 'TransactNamedPipe';

  function GetMailslotInfo(hMailslot:HANDLE; lpMaxMessageSize:LPDWORD; lpNextSize:LPDWORD; lpMessageCount:LPDWORD; lpReadTimeout:LPDWORD):WINBOOL; external 'kernel32' name 'GetMailslotInfo';

  function SetMailslotInfo(hMailslot:HANDLE; lReadTimeout:DWORD):WINBOOL; external 'kernel32' name 'SetMailslotInfo';

  function MapViewOfFile(hFileMappingObject:HANDLE; dwDesiredAccess:DWORD; dwFileOffsetHigh:DWORD; dwFileOffsetLow:DWORD; dwNumberOfBytesToMap:DWORD):LPVOID; external 'kernel32' name 'MapViewOfFile';

  function FlushViewOfFile(lpBaseAddress:LPCVOID; dwNumberOfBytesToFlush:DWORD):WINBOOL; external 'kernel32' name 'FlushViewOfFile';

  function UnmapViewOfFile(lpBaseAddress:LPVOID):WINBOOL; external 'kernel32' name 'UnmapViewOfFile';

  function OpenFile(lpFileName:LPCSTR; lpReOpenBuff:LPOFSTRUCT; uStyle:UINT):HFILE; external 'kernel32' name 'OpenFile';

  function _lopen(lpPathName:LPCSTR; iReadWrite:longint):HFILE; external 'kernel32' name '_lopen';

  function _lcreat(lpPathName:LPCSTR; iAttribute:longint):HFILE; external 'kernel32' name '_lcreat';

  function _lread(hFile:HFILE; lpBuffer:LPVOID; uBytes:UINT):UINT; external 'kernel32' name '_lread';

  function _lwrite(hFile:HFILE; lpBuffer:LPCSTR; uBytes:UINT):UINT; external 'kernel32' name '_lwrite';

  function _hread(hFile:HFILE; lpBuffer:LPVOID; lBytes:longint):longint; external 'kernel32' name '_hread';

  function _hwrite(hFile:HFILE; lpBuffer:LPCSTR; lBytes:longint):longint; external 'kernel32' name '_hwrite';

  function _lclose(hFile:HFILE):HFILE; external 'kernel32' name '_lclose';

  function _llseek(hFile:HFILE; lOffset:LONG; iOrigin:longint):LONG; external 'kernel32' name '_llseek';

  function IsTextUnicode(lpBuffer:LPVOID; cb:longint; lpi:LPINT):WINBOOL; external 'advapi32' name 'IsTextUnicode';

  function TlsAlloc:DWORD; external 'kernel32' name 'TlsAlloc';

  function TlsGetValue(dwTlsIndex:DWORD):LPVOID; external 'kernel32' name 'TlsGetValue';

  function TlsSetValue(dwTlsIndex:DWORD; lpTlsValue:LPVOID):WINBOOL; external 'kernel32' name 'TlsSetValue';

  function TlsFree(dwTlsIndex:DWORD):WINBOOL; external 'kernel32' name 'TlsFree';

  function SleepEx(dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD; external 'kernel32' name 'SleepEx';

  function WaitForSingleObjectEx(hHandle:HANDLE; dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD; external 'kernel32' name 'WaitForSingleObjectEx';

  function WaitForMultipleObjectsEx(nCount:DWORD; var lpHandles:HANDLE; bWaitAll:WINBOOL; dwMilliseconds:DWORD; bAlertable:WINBOOL):DWORD; external 'kernel32' name 'WaitForMultipleObjectsEx';

  function ReadFileEx(hFile:HANDLE; lpBuffer:LPVOID; nNumberOfBytesToRead:DWORD; lpOverlapped:LPOVERLAPPED; lpCompletionRoutine:LPOVERLAPPED_COMPLETION_ROUTINE):WINBOOL; external 'kernel32' name 'ReadFileEx';

  function WriteFileEx(hFile:HANDLE; lpBuffer:LPCVOID; nNumberOfBytesToWrite:DWORD; lpOverlapped:LPOVERLAPPED; lpCompletionRoutine:LPOVERLAPPED_COMPLETION_ROUTINE):WINBOOL; external 'kernel32' name 'WriteFileEx';

  function BackupRead(hFile:HANDLE; lpBuffer:LPBYTE; nNumberOfBytesToRead:DWORD; lpNumberOfBytesRead:LPDWORD; bAbort:WINBOOL;
             bProcessSecurity:WINBOOL; var lpContext:LPVOID):WINBOOL; external 'kernel32' name 'BackupRead';

  function BackupSeek(hFile:HANDLE; dwLowBytesToSeek:DWORD; dwHighBytesToSeek:DWORD; lpdwLowByteSeeked:LPDWORD; lpdwHighByteSeeked:LPDWORD;
             var lpContext:LPVOID):WINBOOL; external 'kernel32' name 'BackupSeek';

  function BackupWrite(hFile:HANDLE; lpBuffer:LPBYTE; nNumberOfBytesToWrite:DWORD; lpNumberOfBytesWritten:LPDWORD; bAbort:WINBOOL;
             bProcessSecurity:WINBOOL; var lpContext:LPVOID):WINBOOL; external 'kernel32' name 'BackupWrite';

  function SetProcessShutdownParameters(dwLevel:DWORD; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'SetProcessShutdownParameters';

  function GetProcessShutdownParameters(lpdwLevel:LPDWORD; lpdwFlags:LPDWORD):WINBOOL; external 'kernel32' name 'GetProcessShutdownParameters';

  procedure SetFileApisToOEM; external 'kernel32' name 'SetFileApisToOEM';

  procedure SetFileApisToANSI; external 'kernel32' name 'SetFileApisToANSI';

  function AreFileApisANSI:WINBOOL; external 'kernel32' name 'AreFileApisANSI';

  function CloseEventLog(hEventLog:HANDLE):WINBOOL; external 'advapi32' name 'CloseEventLog';

  function DeregisterEventSource(hEventLog:HANDLE):WINBOOL; external 'advapi32' name 'DeregisterEventSource';

  function NotifyChangeEventLog(hEventLog:HANDLE; hEvent:HANDLE):WINBOOL; external 'advapi32' name 'NotifyChangeEventLog';

  function GetNumberOfEventLogRecords(hEventLog:HANDLE; NumberOfRecords:PDWORD):WINBOOL; external 'advapi32' name 'GetNumberOfEventLogRecords';

  function GetOldestEventLogRecord(hEventLog:HANDLE; OldestRecord:PDWORD):WINBOOL; external 'advapi32' name 'GetOldestEventLogRecord';

  function DuplicateToken(ExistingTokenHandle:HANDLE; ImpersonationLevel:SECURITY_IMPERSONATION_LEVEL; DuplicateTokenHandle:PHANDLE):WINBOOL; external 'advapi32' name 'DuplicateToken';

  function GetKernelObjectSecurity(Handle:HANDLE; RequestedInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'advapi32' name 'GetKernelObjectSecurity';

  function ImpersonateNamedPipeClient(hNamedPipe:HANDLE):WINBOOL; external 'advapi32' name 'ImpersonateNamedPipeClient';

  function ImpersonateLoggedOnUser(hToken:HANDLE):WINBOOL; external 'advapi32' name 'ImpersonateLoggedOnUser';

  function ImpersonateSelf(ImpersonationLevel:SECURITY_IMPERSONATION_LEVEL):WINBOOL; external 'advapi32' name 'ImpersonateSelf';

  function RevertToSelf:WINBOOL; external 'advapi32' name 'RevertToSelf';

  function SetThreadToken(Thread:PHANDLE; Token:HANDLE):WINBOOL; external 'advapi32' name 'SetThreadToken';

{  function AccessCheck(pSecurityDescriptor:PSECURITY_DESCRIPTOR; ClientToken:HANDLE; DesiredAccess:DWORD; GenericMapping:PGENERIC_MAPPING; PrivilegeSet:PPRIVILEGE_SET;
             PrivilegeSetLength:LPDWORD; GrantedAccess:LPDWORD; AccessStatus:LPBOOL):WINBOOL; external 'advapi32' name 'AccessCheck';
             }

  function OpenProcessToken(ProcessHandle:HANDLE; DesiredAccess:DWORD; TokenHandle:PHANDLE):WINBOOL; external 'advapi32' name 'OpenProcessToken';

  function OpenThreadToken(ThreadHandle:HANDLE; DesiredAccess:DWORD; OpenAsSelf:WINBOOL; TokenHandle:PHANDLE):WINBOOL; external 'advapi32' name 'OpenThreadToken';

  function GetTokenInformation(TokenHandle:HANDLE; TokenInformationClass:TOKEN_INFORMATION_CLASS; TokenInformation:LPVOID; TokenInformationLength:DWORD; ReturnLength:PDWORD):WINBOOL; external 'advapi32' name 'GetTokenInformation';

  function SetTokenInformation(TokenHandle:HANDLE; TokenInformationClass:TOKEN_INFORMATION_CLASS; TokenInformation:LPVOID; TokenInformationLength:DWORD):WINBOOL; external 'advapi32' name 'SetTokenInformation';

  function AdjustTokenPrivileges(TokenHandle:HANDLE; DisableAllPrivileges:WINBOOL; NewState:PTOKEN_PRIVILEGES; BufferLength:DWORD; PreviousState:PTOKEN_PRIVILEGES;
             ReturnLength:PDWORD):WINBOOL; external 'advapi32' name 'AdjustTokenPrivileges';

  function AdjustTokenGroups(TokenHandle:HANDLE; ResetToDefault:WINBOOL; NewState:PTOKEN_GROUPS; BufferLength:DWORD; PreviousState:PTOKEN_GROUPS;
             ReturnLength:PDWORD):WINBOOL; external 'advapi32' name 'AdjustTokenGroups';

  function PrivilegeCheck(ClientToken:HANDLE; RequiredPrivileges:PPRIVILEGE_SET; pfResult:LPBOOL):WINBOOL; external 'advapi32' name 'PrivilegeCheck';

  function IsValidSid(pSid:PSID):WINBOOL; external 'advapi32' name 'IsValidSid';

  function EqualSid(pSid1:PSID; pSid2:PSID):WINBOOL; external 'advapi32' name 'EqualSid';

  function EqualPrefixSid(pSid1:PSID; pSid2:PSID):WINBOOL; external 'advapi32' name 'EqualPrefixSid';

  function GetSidLengthRequired(nSubAuthorityCount:UCHAR):DWORD; external 'advapi32' name 'GetSidLengthRequired';

  function AllocateAndInitializeSid(pIdentifierAuthority:PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount:BYTE; nSubAuthority0:DWORD; nSubAuthority1:DWORD; nSubAuthority2:DWORD;
             nSubAuthority3:DWORD; nSubAuthority4:DWORD; nSubAuthority5:DWORD; nSubAuthority6:DWORD; nSubAuthority7:DWORD;
             var pSid:PSID):WINBOOL; external 'advapi32' name 'AllocateAndInitializeSid';

  function FreeSid(pSid:PSID):PVOID; external 'advapi32' name 'FreeSid';

  function InitializeSid(Sid:PSID; pIdentifierAuthority:PSID_IDENTIFIER_AUTHORITY; nSubAuthorityCount:BYTE):WINBOOL; external 'advapi32' name 'InitializeSid';

  function GetSidIdentifierAuthority(pSid:PSID):PSID_IDENTIFIER_AUTHORITY; external 'advapi32' name 'GetSidIdentifierAuthority';

  function GetSidSubAuthority(pSid:PSID; nSubAuthority:DWORD):PDWORD; external 'advapi32' name 'GetSidSubAuthority';

  function GetSidSubAuthorityCount(pSid:PSID):PUCHAR; external 'advapi32' name 'GetSidSubAuthorityCount';

  function GetLengthSid(pSid:PSID):DWORD; external 'advapi32' name 'GetLengthSid';

  function CopySid(nDestinationSidLength:DWORD; pDestinationSid:PSID; pSourceSid:PSID):WINBOOL; external 'advapi32' name 'CopySid';

  function AreAllAccessesGranted(GrantedAccess:DWORD; DesiredAccess:DWORD):WINBOOL; external 'advapi32' name 'AreAllAccessesGranted';

  function AreAnyAccessesGranted(GrantedAccess:DWORD; DesiredAccess:DWORD):WINBOOL; external 'advapi32' name 'AreAnyAccessesGranted';

  procedure MapGenericMask(AccessMask:PDWORD; GenericMapping:PGENERIC_MAPPING); external 'advapi32' name 'MapGenericMask';

  function IsValidAcl(pAcl:PACL):WINBOOL; external 'advapi32' name 'IsValidAcl';

  function InitializeAcl(pAcl:PACL; nAclLength:DWORD; dwAclRevision:DWORD):WINBOOL; external 'advapi32' name 'InitializeAcl';

  function GetAclInformation(pAcl:PACL; pAclInformation:LPVOID; nAclInformationLength:DWORD; dwAclInformationClass:ACL_INFORMATION_CLASS):WINBOOL; external 'advapi32' name 'GetAclInformation';

  function SetAclInformation(pAcl:PACL; pAclInformation:LPVOID; nAclInformationLength:DWORD; dwAclInformationClass:ACL_INFORMATION_CLASS):WINBOOL; external 'advapi32' name 'SetAclInformation';

  function AddAce(pAcl:PACL; dwAceRevision:DWORD; dwStartingAceIndex:DWORD; pAceList:LPVOID; nAceListLength:DWORD):WINBOOL; external 'advapi32' name 'AddAce';

  function DeleteAce(pAcl:PACL; dwAceIndex:DWORD):WINBOOL; external 'advapi32' name 'DeleteAce';

  function GetAce(pAcl:PACL; dwAceIndex:DWORD; var pAce:LPVOID):WINBOOL; external 'advapi32' name 'GetAce';

  function AddAccessAllowedAce(pAcl:PACL; dwAceRevision:DWORD; AccessMask:DWORD; pSid:PSID):WINBOOL; external 'advapi32' name 'AddAccessAllowedAce';

  function AddAccessDeniedAce(pAcl:PACL; dwAceRevision:DWORD; AccessMask:DWORD; pSid:PSID):WINBOOL; external 'advapi32' name 'AddAccessDeniedAce';

  function AddAuditAccessAce(pAcl:PACL; dwAceRevision:DWORD; dwAccessMask:DWORD; pSid:PSID; bAuditSuccess:WINBOOL;
             bAuditFailure:WINBOOL):WINBOOL; external 'advapi32' name 'AddAuditAccessAce';

  function FindFirstFreeAce(pAcl:PACL; var pAce:LPVOID):WINBOOL; external 'advapi32' name 'FindFirstFreeAce';

  function InitializeSecurityDescriptor(pSecurityDescriptor:PSECURITY_DESCRIPTOR; dwRevision:DWORD):WINBOOL; external 'advapi32' name 'InitializeSecurityDescriptor';

  function IsValidSecurityDescriptor(pSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'IsValidSecurityDescriptor';

  function GetSecurityDescriptorLength(pSecurityDescriptor:PSECURITY_DESCRIPTOR):DWORD; external 'advapi32' name 'GetSecurityDescriptorLength';

  function GetSecurityDescriptorControl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pControl:PSECURITY_DESCRIPTOR_CONTROL; lpdwRevision:LPDWORD):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorControl';

  function SetSecurityDescriptorDacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; bDaclPresent:WINBOOL; pDacl:PACL; bDaclDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorDacl';

  function GetSecurityDescriptorDacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpbDaclPresent:LPBOOL; var pDacl:PACL; lpbDaclDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorDacl';

  function SetSecurityDescriptorSacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; bSaclPresent:WINBOOL; pSacl:PACL; bSaclDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorSacl';

  function GetSecurityDescriptorSacl(pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpbSaclPresent:LPBOOL; var pSacl:PACL; lpbSaclDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorSacl';

  function SetSecurityDescriptorOwner(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pOwner:PSID; bOwnerDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorOwner';

  function GetSecurityDescriptorOwner(pSecurityDescriptor:PSECURITY_DESCRIPTOR; var pOwner:PSID; lpbOwnerDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorOwner';

  function SetSecurityDescriptorGroup(pSecurityDescriptor:PSECURITY_DESCRIPTOR; pGroup:PSID; bGroupDefaulted:WINBOOL):WINBOOL; external 'advapi32' name 'SetSecurityDescriptorGroup';

  function GetSecurityDescriptorGroup(pSecurityDescriptor:PSECURITY_DESCRIPTOR; var pGroup:PSID; lpbGroupDefaulted:LPBOOL):WINBOOL; external 'advapi32' name 'GetSecurityDescriptorGroup';

  function CreatePrivateObjectSecurity(ParentDescriptor:PSECURITY_DESCRIPTOR; CreatorDescriptor:PSECURITY_DESCRIPTOR; var NewDescriptor:PSECURITY_DESCRIPTOR; IsDirectoryObject:WINBOOL; Token:HANDLE;
             GenericMapping:PGENERIC_MAPPING):WINBOOL; external 'advapi32' name 'CreatePrivateObjectSecurity';

  function SetPrivateObjectSecurity(SecurityInformation:SECURITY_INFORMATION; ModificationDescriptor:PSECURITY_DESCRIPTOR; var ObjectsSecurityDescriptor:PSECURITY_DESCRIPTOR; GenericMapping:PGENERIC_MAPPING; Token:HANDLE):WINBOOL;
             external 'advapi32' name 'SetPrivateObjectSecurity';

  function GetPrivateObjectSecurity(ObjectDescriptor:PSECURITY_DESCRIPTOR; SecurityInformation:SECURITY_INFORMATION; ResultantDescriptor:PSECURITY_DESCRIPTOR; DescriptorLength:DWORD; ReturnLength:PDWORD):WINBOOL;
             external 'advapi32' name 'GetPrivateObjectSecurity';

  function DestroyPrivateObjectSecurity(var ObjectDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'DestroyPrivateObjectSecurity';

  function MakeSelfRelativeSD(pAbsoluteSecurityDescriptor:PSECURITY_DESCRIPTOR; pSelfRelativeSecurityDescriptor:PSECURITY_DESCRIPTOR; lpdwBufferLength:LPDWORD):WINBOOL; external 'advapi32' name 'MakeSelfRelativeSD';

  function MakeAbsoluteSD(pSelfRelativeSecurityDescriptor:PSECURITY_DESCRIPTOR; pAbsoluteSecurityDescriptor:PSECURITY_DESCRIPTOR; lpdwAbsoluteSecurityDescriptorSize:LPDWORD; pDacl:PACL; lpdwDaclSize:LPDWORD;
             pSacl:PACL; lpdwSaclSize:LPDWORD; pOwner:PSID; lpdwOwnerSize:LPDWORD; pPrimaryGroup:PSID;
             lpdwPrimaryGroupSize:LPDWORD):WINBOOL; external 'advapi32' name 'MakeAbsoluteSD';

  function SetKernelObjectSecurity(Handle:HANDLE; SecurityInformation:SECURITY_INFORMATION; SecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL; external 'advapi32' name 'SetKernelObjectSecurity';

  function FindNextChangeNotification(hChangeHandle:HANDLE):WINBOOL; external 'kernel32' name 'FindNextChangeNotification';

  function FindCloseChangeNotification(hChangeHandle:HANDLE):WINBOOL; external 'kernel32' name 'FindCloseChangeNotification';

  function VirtualLock(lpAddress:LPVOID; dwSize:DWORD):WINBOOL; external 'kernel32' name 'VirtualLock';

  function VirtualUnlock(lpAddress:LPVOID; dwSize:DWORD):WINBOOL; external 'kernel32' name 'VirtualUnlock';

  function MapViewOfFileEx(hFileMappingObject:HANDLE; dwDesiredAccess:DWORD; dwFileOffsetHigh:DWORD; dwFileOffsetLow:DWORD; dwNumberOfBytesToMap:DWORD;
             lpBaseAddress:LPVOID):LPVOID; external 'kernel32' name 'MapViewOfFileEx';

  function SetPriorityClass(hProcess:HANDLE; dwPriorityClass:DWORD):WINBOOL; external 'kernel32' name 'SetPriorityClass';

  function GetPriorityClass(hProcess:HANDLE):DWORD; external 'kernel32' name 'GetPriorityClass';

  function IsBadReadPtr(lp:pointer; ucb:UINT):WINBOOL; external 'kernel32' name 'IsBadReadPtr';

  function IsBadWritePtr(lp:LPVOID; ucb:UINT):WINBOOL; external 'kernel32' name 'IsBadWritePtr';

  function IsBadHugeReadPtr(lp:pointer; ucb:UINT):WINBOOL; external 'kernel32' name 'IsBadHugeReadPtr';

  function IsBadHugeWritePtr(lp:LPVOID; ucb:UINT):WINBOOL; external 'kernel32' name 'IsBadHugeWritePtr';

  function IsBadCodePtr(lpfn:FARPROC):WINBOOL; external 'kernel32' name 'IsBadCodePtr';

  function AllocateLocallyUniqueId(Luid:PLUID):WINBOOL; external 'advapi32' name 'AllocateLocallyUniqueId';

  function QueryPerformanceCounter(var lpPerformanceCount:LARGE_INTEGER):WINBOOL; external 'kernel32' name 'QueryPerformanceCounter';

  function QueryPerformanceFrequency(var lpFrequency:LARGE_INTEGER):WINBOOL; external 'kernel32' name 'QueryPerformanceFrequency';

  procedure MoveMemory(Destination:PVOID; Source:pointer; Length:DWORD);
    begin
       Move(Source^,Destination^,Length);
    end;

  procedure CopyMemory(Destination:PVOID; Source:pointer; Length:DWORD);
    begin
       Move(Source^, Destination^, Length);
    end;

  procedure FillMemory(Destination:PVOID; Length:DWORD; Fill:BYTE);
    begin
       FillChar(Destination^,Length,Char(Fill));
    end;

  procedure ZeroMemory(Destination:PVOID; Length:DWORD);
    begin
       FillChar(Destination^,Length,#0);
    end;



(*  { was #define dname(params) def_expr }
  procedure MoveMemory(var t,s; c : longint);
    begin
       move(s,t,c);
    end;

  { was #define dname(params) def_expr }
  procedure FillMemory(var p;c,v : longint);
    begin
       fillchar(p,c,char(byte(v)));
    end;

  { was #define dname(params) def_expr }
  { argument types are unknown }
  { return type might be wrong }
  procedure ZeroMemory(var p;c : longint);
    { return type might be wrong }
    begin
       fillchar(p,c,#0);
    end; *)

{$ifdef WIN95}

  function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):HKL; external 'user32' name 'ActivateKeyboardLayout';

{$else}

  function ActivateKeyboardLayout(hkl:HKL; Flags:UINT):WINBOOL; external 'user32' name 'ActivateKeyboardLayout';

{$endif}


{ Not in my user32 !!! PM
  function ToUnicodeEx(wVirtKey:UINT; wScanCode:UINT; lpKeyState:PBYTE; pwszBuff:LPWSTR; cchBuff:longint;
             wFlags:UINT; dwhkl:HKL):longint; external 'user32' name 'ToUnicodeEx';
}

  function UnloadKeyboardLayout(hkl:HKL):WINBOOL; external 'user32' name 'UnloadKeyboardLayout';

  function GetKeyboardLayoutList(nBuff:longint; var lpList:HKL):longint; external 'user32' name 'GetKeyboardLayoutList';

  function GetKeyboardLayout(dwLayout:DWORD):HKL; external 'user32' name 'GetKeyboardLayout';

  function OpenInputDesktop(dwFlags:DWORD; fInherit:WINBOOL; dwDesiredAccess:DWORD):HDESK; external 'user32' name 'OpenInputDesktop';

  function EnumDesktopWindows(hDesktop:HDESK; lpfn:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumDesktopWindows';

  function SwitchDesktop(hDesktop:HDESK):WINBOOL; external 'user32' name 'SwitchDesktop';

  function SetThreadDesktop(hDesktop:HDESK):WINBOOL; external 'user32' name 'SetThreadDesktop';

  function CloseDesktop(hDesktop:HDESK):WINBOOL; external 'user32' name 'CloseDesktop';

  function GetThreadDesktop(dwThreadId:DWORD):HDESK; external 'user32' name 'GetThreadDesktop';

  function CloseWindowStation(hWinSta:HWINSTA):WINBOOL; external 'user32' name 'CloseWindowStation';

  function SetProcessWindowStation(hWinSta:HWINSTA):WINBOOL; external 'user32' name 'SetProcessWindowStation';

  function GetProcessWindowStation:HWINSTA; external 'user32' name 'GetProcessWindowStation';

  function SetUserObjectSecurity(hObj:HANDLE; pSIRequested:PSECURITY_INFORMATION; pSID:PSECURITY_DESCRIPTOR):WINBOOL; external 'user32' name 'SetUserObjectSecurity';

  function GetUserObjectSecurity(hObj:HANDLE; pSIRequested:PSECURITY_INFORMATION; pSID:PSECURITY_DESCRIPTOR; nLength:DWORD; lpnLengthNeeded:LPDWORD):WINBOOL; external 'user32' name 'GetUserObjectSecurity';

  function TranslateMessage(var lpMsg:MSG):WINBOOL; external 'user32' name 'TranslateMessage';

  function SetMessageQueue(cMessagesMax:longint):WINBOOL; external 'user32' name 'SetMessageQueue';

  function RegisterHotKey(hWnd:HWND; anID:longint; fsModifiers:UINT; vk:UINT):WINBOOL; external 'user32' name 'RegisterHotKey';

  function UnregisterHotKey(hWnd:HWND; anID:longint):WINBOOL; external 'user32' name 'UnregisterHotKey';

  function ExitWindowsEx(uFlags:UINT; dwReserved:DWORD):WINBOOL; external 'user32' name 'ExitWindowsEx';

  function SwapMouseButton(fSwap:WINBOOL):WINBOOL; external 'user32' name 'SwapMouseButton';

  function GetMessagePos:DWORD; external 'user32' name 'GetMessagePos';

  function GetMessageTime:LONG; external 'user32' name 'GetMessageTime';

  function GetMessageExtraInfo:LONG; external 'user32' name 'GetMessageExtraInfo';

  function SetMessageExtraInfo(lParam:LPARAM):LPARAM; external 'user32' name 'SetMessageExtraInfo';

  function BroadcastSystemMessage(_para1:DWORD; _para2:LPDWORD; _para3:UINT; _para4:WPARAM; _para5:LPARAM):longint; external 'user32' name 'BroadcastSystemMessage';

  function AttachThreadInput(idAttach:DWORD; idAttachTo:DWORD; fAttach:WINBOOL):WINBOOL; external 'user32' name 'AttachThreadInput';

  function ReplyMessage(lResult:LRESULT):WINBOOL; external 'user32' name 'ReplyMessage';

  function WaitMessage:WINBOOL; external 'user32' name 'WaitMessage';

  function WaitForInputIdle(hProcess:HANDLE; dwMilliseconds:DWORD):DWORD; external 'user32' name 'WaitForInputIdle';

  procedure PostQuitMessage(nExitCode:longint); external 'user32' name 'PostQuitMessage';

  function InSendMessage:WINBOOL; external 'user32' name 'InSendMessage';

  function GetDoubleClickTime:UINT; external 'user32' name 'GetDoubleClickTime';

  function SetDoubleClickTime(_para1:UINT):WINBOOL; external 'user32' name 'SetDoubleClickTime';

  function IsWindow(hWnd:HWND):WINBOOL; external 'user32' name 'IsWindow';

  function IsMenu(hMenu:HMENU):WINBOOL; external 'user32' name 'IsMenu';

  function IsChild(hWndParent:HWND; hWnd:HWND):WINBOOL; external 'user32' name 'IsChild';

  function DestroyWindow(hWnd:HWND):WINBOOL; external 'user32' name 'DestroyWindow';

  function ShowWindow(hWnd:HWND; nCmdShow:longint):WINBOOL; external 'user32' name 'ShowWindow';

  function ShowWindowAsync(hWnd:HWND; nCmdShow:longint):WINBOOL; external 'user32' name 'ShowWindowAsync';

  function FlashWindow(hWnd:HWND; bInvert:WINBOOL):WINBOOL; external 'user32' name 'FlashWindow';

  function ShowOwnedPopups(hWnd:HWND; fShow:WINBOOL):WINBOOL; external 'user32' name 'ShowOwnedPopups';

  function OpenIcon(hWnd:HWND):WINBOOL; external 'user32' name 'OpenIcon';

  function CloseWindow(hWnd:HWND):WINBOOL; external 'user32' name 'CloseWindow';

  function MoveWindow(hWnd:HWND; X:longint; Y:longint; nWidth:longint; nHeight:longint;
             bRepaint:WINBOOL):WINBOOL; external 'user32' name 'MoveWindow';

  function SetWindowPos(hWnd:HWND; hWndInsertAfter:HWND; X:longint; Y:longint; cx:longint;
             cy:longint; uFlags:UINT):WINBOOL; external 'user32' name 'SetWindowPos';

  function GetWindowPlacement(hWnd:HWND; var lpwndpl:WINDOWPLACEMENT):WINBOOL; external 'user32' name 'GetWindowPlacement';

  function SetWindowPlacement(hWnd:HWND; var lpwndpl:WINDOWPLACEMENT):WINBOOL; external 'user32' name 'SetWindowPlacement';

  function BeginDeferWindowPos(nNumWindows:longint):HDWP; external 'user32' name 'BeginDeferWindowPos';

  function DeferWindowPos(hWinPosInfo:HDWP; hWnd:HWND; hWndInsertAfter:HWND; x:longint; y:longint;
             cx:longint; cy:longint; uFlags:UINT):HDWP; external 'user32' name 'DeferWindowPos';

  function EndDeferWindowPos(hWinPosInfo:HDWP):WINBOOL; external 'user32' name 'EndDeferWindowPos';

  function IsWindowVisible(hWnd:HWND):WINBOOL; external 'user32' name 'IsWindowVisible';

  function IsIconic(hWnd:HWND):WINBOOL; external 'user32' name 'IsIconic';

  function AnyPopup:WINBOOL; external 'user32' name 'AnyPopup';

  function BringWindowToTop(hWnd:HWND):WINBOOL; external 'user32' name 'BringWindowToTop';

  function IsZoomed(hWnd:HWND):WINBOOL; external 'user32' name 'IsZoomed';

  function EndDialog(hDlg:HWND; nResult:longint):WINBOOL; external 'user32' name 'EndDialog';

  function GetDlgItem(hDlg:HWND; nIDDlgItem:longint):HWND; external 'user32' name 'GetDlgItem';

  function SetDlgItemInt(hDlg:HWND; nIDDlgItem:longint; uValue:UINT; bSigned:WINBOOL):WINBOOL; external 'user32' name 'SetDlgItemInt';

  function GetDlgItemInt(hDlg:HWND; nIDDlgItem:longint; var lpTranslated:WINBOOL; bSigned:WINBOOL):UINT; external 'user32' name 'GetDlgItemInt';

  function CheckDlgButton(hDlg:HWND; nIDButton:longint; uCheck:UINT):WINBOOL; external 'user32' name 'CheckDlgButton';

  function CheckRadioButton(hDlg:HWND; nIDFirstButton:longint; nIDLastButton:longint; nIDCheckButton:longint):WINBOOL; external 'user32' name 'CheckRadioButton';

  function IsDlgButtonChecked(hDlg:HWND; nIDButton:longint):UINT; external 'user32' name 'IsDlgButtonChecked';

  function GetNextDlgGroupItem(hDlg:HWND; hCtl:HWND; bPrevious:WINBOOL):HWND; external 'user32' name 'GetNextDlgGroupItem';

  function GetNextDlgTabItem(hDlg:HWND; hCtl:HWND; bPrevious:WINBOOL):HWND; external 'user32' name 'GetNextDlgTabItem';

  function GetDlgCtrlID(hWnd:HWND):longint; external 'user32' name 'GetDlgCtrlID';

  function GetDialogBaseUnits:longint; external 'user32' name 'GetDialogBaseUnits';

  function OpenClipboard(hWndNewOwner:HWND):WINBOOL; external 'user32' name 'OpenClipboard';

  function CloseClipboard:WINBOOL; external 'user32' name 'CloseClipboard';

  function GetClipboardOwner:HWND; external 'user32' name 'GetClipboardOwner';

  function SetClipboardViewer(hWndNewViewer:HWND):HWND; external 'user32' name 'SetClipboardViewer';

  function GetClipboardViewer:HWND; external 'user32' name 'GetClipboardViewer';

  function ChangeClipboardChain(hWndRemove:HWND; hWndNewNext:HWND):WINBOOL; external 'user32' name 'ChangeClipboardChain';

  function SetClipboardData(uFormat:UINT; hMem:HANDLE):HANDLE; external 'user32' name 'SetClipboardData';

  function GetClipboardData(uFormat:UINT):HANDLE; external 'user32' name 'GetClipboardData';

  function CountClipboardFormats:longint; external 'user32' name 'CountClipboardFormats';

  function EnumClipboardFormats(format:UINT):UINT; external 'user32' name 'EnumClipboardFormats';

  function EmptyClipboard:WINBOOL; external 'user32' name 'EmptyClipboard';

  function IsClipboardFormatAvailable(format:UINT):WINBOOL; external 'user32' name 'IsClipboardFormatAvailable';

  function GetPriorityClipboardFormat(var paFormatPriorityList:UINT; cFormats:longint):longint; external 'user32' name 'GetPriorityClipboardFormat';

  function GetOpenClipboardWindow:HWND; external 'user32' name 'GetOpenClipboardWindow';

  function CharNextExA(CodePage:WORD; lpCurrentChar:LPCSTR; dwFlags:DWORD):LPSTR; external 'user32' name 'CharNextExA';

  function CharPrevExA(CodePage:WORD; lpStart:LPCSTR; lpCurrentChar:LPCSTR; dwFlags:DWORD):LPSTR; external 'user32' name 'CharPrevExA';

  function SetFocus(hWnd:HWND):HWND; external 'user32' name 'SetFocus';

  function GetActiveWindow:HWND; external 'user32' name 'GetActiveWindow';

  function GetFocus:HWND; external 'user32' name 'GetFocus';

  function GetKBCodePage:UINT; external 'user32' name 'GetKBCodePage';

  function GetKeyState(nVirtKey:longint):SHORT; external 'user32' name 'GetKeyState';

  function GetAsyncKeyState(vKey:longint):SHORT; external 'user32' name 'GetAsyncKeyState';

  function GetKeyboardState(lpKeyState:PBYTE):WINBOOL; external 'user32' name 'GetKeyboardState';

  function SetKeyboardState(lpKeyState:LPBYTE):WINBOOL; external 'user32' name 'SetKeyboardState';

  function GetKeyboardType(nTypeFlag:longint):longint; external 'user32' name 'GetKeyboardType';

  function ToAscii(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE; lpChar:LPWORD; uFlags:UINT):longint; external 'user32' name 'ToAscii';

  function ToAsciiEx(uVirtKey:UINT; uScanCode:UINT; lpKeyState:PBYTE; lpChar:LPWORD; uFlags:UINT;
             dwhkl:HKL):longint; external 'user32' name 'ToAsciiEx';

  function ToUnicode(wVirtKey:UINT; wScanCode:UINT; lpKeyState:PBYTE; pwszBuff:LPWSTR; cchBuff:longint;
             wFlags:UINT):longint; external 'user32' name 'ToUnicode';

  function OemKeyScan(wOemChar:WORD):DWORD; external 'user32' name 'OemKeyScan';

  procedure keybd_event(bVk:BYTE; bScan:BYTE; dwFlags:DWORD; dwExtraInfo:DWORD); external 'user32' name 'keybd_event';

  procedure mouse_event(dwFlags:DWORD; dx:DWORD; dy:DWORD; cButtons:DWORD; dwExtraInfo:DWORD); external 'user32' name 'mouse_event';

  function GetInputState:WINBOOL; external 'user32' name 'GetInputState';

  function GetQueueStatus(flags:UINT):DWORD; external 'user32' name 'GetQueueStatus';

  function GetCapture:HWND; external 'user32' name 'GetCapture';

  function SetCapture(hWnd:HWND):HWND; external 'user32' name 'SetCapture';

  function ReleaseCapture:WINBOOL; external 'user32' name 'ReleaseCapture';

  function MsgWaitForMultipleObjects(nCount:DWORD; pHandles:LPHANDLE; fWaitAll:WINBOOL; dwMilliseconds:DWORD; dwWakeMask:DWORD):DWORD; external 'user32' name 'MsgWaitForMultipleObjects';

  function SetTimer(hWnd:HWND; nIDEvent:UINT; uElapse:UINT; lpTimerFunc:TIMERPROC):UINT; external 'user32' name 'SetTimer';

  function KillTimer(hWnd:HWND; uIDEvent:UINT):WINBOOL; external 'user32' name 'KillTimer';

  function IsWindowUnicode(hWnd:HWND):WINBOOL; external 'user32' name 'IsWindowUnicode';

  function EnableWindow(hWnd:HWND; bEnable:WINBOOL):WINBOOL; external 'user32' name 'EnableWindow';

  function IsWindowEnabled(hWnd:HWND):WINBOOL; external 'user32' name 'IsWindowEnabled';

  function DestroyAcceleratorTable(hAccel:HACCEL):WINBOOL; external 'user32' name 'DestroyAcceleratorTable';

  function GetSystemMetrics(nIndex:longint):longint; external 'user32' name 'GetSystemMetrics';

  function GetMenu(hWnd:HWND):HMENU; external 'user32' name 'GetMenu';

  function SetMenu(hWnd:HWND; hMenu:HMENU):WINBOOL; external 'user32' name 'SetMenu';

  function HiliteMenuItem(hWnd:HWND; hMenu:HMENU; uIDHiliteItem:UINT; uHilite:UINT):WINBOOL; external 'user32' name 'HiliteMenuItem';

  function GetMenuState(hMenu:HMENU; uId:UINT; uFlags:UINT):UINT; external 'user32' name 'GetMenuState';

  function DrawMenuBar(hWnd:HWND):WINBOOL; external 'user32' name 'DrawMenuBar';

  function GetSystemMenu(hWnd:HWND; bRevert:WINBOOL):HMENU; external 'user32' name 'GetSystemMenu';

  function CreateMenu:HMENU; external 'user32' name 'CreateMenu';

  function CreatePopupMenu:HMENU; external 'user32' name 'CreatePopupMenu';

  function DestroyMenu(hMenu:HMENU):WINBOOL; external 'user32' name 'DestroyMenu';

  function CheckMenuItem(hMenu:HMENU; uIDCheckItem:UINT; uCheck:UINT):DWORD; external 'user32' name 'CheckMenuItem';

  function EnableMenuItem(hMenu:HMENU; uIDEnableItem:UINT; uEnable:UINT):WINBOOL; external 'user32' name 'EnableMenuItem';

  function GetSubMenu(hMenu:HMENU; nPos:longint):HMENU; external 'user32' name 'GetSubMenu';

  function GetMenuItemID(hMenu:HMENU; nPos:longint):UINT; external 'user32' name 'GetMenuItemID';

  function GetMenuItemCount(hMenu:HMENU):longint; external 'user32' name 'GetMenuItemCount';

  function RemoveMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT):WINBOOL; external 'user32' name 'RemoveMenu';

  function DeleteMenu(hMenu:HMENU; uPosition:UINT; uFlags:UINT):WINBOOL; external 'user32' name 'DeleteMenu';

  function SetMenuItemBitmaps(hMenu:HMENU; uPosition:UINT; uFlags:UINT; hBitmapUnchecked:HBITMAP; hBitmapChecked:HBITMAP):WINBOOL; external 'user32' name 'SetMenuItemBitmaps';

  function GetMenuCheckMarkDimensions:LONG; external 'user32' name 'GetMenuCheckMarkDimensions';

  function TrackPopupMenu(hMenu:HMENU; uFlags:UINT; x:longint; y:longint; nReserved:longint;
             hWnd:HWND; var prcRect:RECT):WINBOOL; external 'user32' name 'TrackPopupMenu';

  function GetMenuDefaultItem(hMenu:HMENU; fByPos:UINT; gmdiFlags:UINT):UINT; external 'user32' name 'GetMenuDefaultItem';

  function SetMenuDefaultItem(hMenu:HMENU; uItem:UINT; fByPos:UINT):WINBOOL; external 'user32' name 'SetMenuDefaultItem';

  function GetMenuItemRect(hWnd:HWND; hMenu:HMENU; uItem:UINT; lprcItem:LPRECT):WINBOOL; external 'user32' name 'GetMenuItemRect';

  function MenuItemFromPoint(hWnd:HWND; hMenu:HMENU; ptScreen:POINT):longint; external 'user32' name 'MenuItemFromPoint';

  function DragObject(_para1:HWND; _para2:HWND; _para3:UINT; _para4:DWORD; _para5:HCURSOR):DWORD; external 'user32' name 'DragObject';

  function DragDetect(hwnd:HWND; pt:POINT):WINBOOL; external 'user32' name 'DragDetect';

  function DrawIcon(hDC:HDC; X:longint; Y:longint; hIcon:HICON):WINBOOL; external 'user32' name 'DrawIcon';

  function UpdateWindow(hWnd:HWND):WINBOOL; external 'user32' name 'UpdateWindow';

  function SetActiveWindow(hWnd:HWND):HWND; external 'user32' name 'SetActiveWindow';

  function GetForegroundWindow:HWND; external 'user32' name 'GetForegroundWindow';

  function PaintDesktop(hdc:HDC):WINBOOL; external 'user32' name 'PaintDesktop';

  function SetForegroundWindow(hWnd:HWND):WINBOOL; external 'user32' name 'SetForegroundWindow';

  function WindowFromDC(hDC:HDC):HWND; external 'user32' name 'WindowFromDC';

  function GetDC(hWnd:HWND):HDC; external 'user32' name 'GetDC';

  function GetDCEx(hWnd:HWND; hrgnClip:HRGN; flags:DWORD):HDC; external 'user32' name 'GetDCEx';

  function GetWindowDC(hWnd:HWND):HDC; external 'user32' name 'GetWindowDC';

  function ReleaseDC(hWnd:HWND; hDC:HDC):longint; external 'user32' name 'ReleaseDC';

  function BeginPaint(hWnd:HWND; lpPaint:LPPAINTSTRUCT):HDC; external 'user32' name 'BeginPaint';
  function BeginPaint(hWnd:HWND;var lPaint:PAINTSTRUCT):HDC; external 'user32' name 'BeginPaint';

  function EndPaint(hWnd:HWND; var lpPaint:PAINTSTRUCT):WINBOOL; external 'user32' name 'EndPaint';

  function GetUpdateRect(hWnd:HWND; lpRect:LPRECT; bErase:WINBOOL):WINBOOL; external 'user32' name 'GetUpdateRect';

  function GetUpdateRgn(hWnd:HWND; hRgn:HRGN; bErase:WINBOOL):longint; external 'user32' name 'GetUpdateRgn';

  function SetWindowRgn(hWnd:HWND; hRgn:HRGN; bRedraw:WINBOOL):longint; external 'user32' name 'SetWindowRgn';

  function GetWindowRgn(hWnd:HWND; hRgn:HRGN):longint; external 'user32' name 'GetWindowRgn';

  function ExcludeUpdateRgn(hDC:HDC; hWnd:HWND):longint; external 'user32' name 'ExcludeUpdateRgn';

  function InvalidateRect(hWnd:HWND; var lpRect:RECT; bErase:WINBOOL):WINBOOL; external 'user32' name 'InvalidateRect';

  function ValidateRect(hWnd:HWND; var lpRect:RECT):WINBOOL; external 'user32' name 'ValidateRect';

  function InvalidateRgn(hWnd:HWND; hRgn:HRGN; bErase:WINBOOL):WINBOOL; external 'user32' name 'InvalidateRgn';

  function ValidateRgn(hWnd:HWND; hRgn:HRGN):WINBOOL; external 'user32' name 'ValidateRgn';

  function RedrawWindow(hWnd:HWND; var lprcUpdate:RECT; hrgnUpdate:HRGN; flags:UINT):WINBOOL; external 'user32' name 'RedrawWindow';

  function LockWindowUpdate(hWndLock:HWND):WINBOOL; external 'user32' name 'LockWindowUpdate';

  function ScrollWindow(hWnd:HWND; XAmount:longint; YAmount:longint; var lpRect:RECT; var lpClipRect:RECT):WINBOOL; external 'user32' name 'ScrollWindow';

  function ScrollDC(hDC:HDC; dx:longint; dy:longint; var lprcScroll:RECT; var lprcClip:RECT;
             hrgnUpdate:HRGN; lprcUpdate:LPRECT):WINBOOL; external 'user32' name 'ScrollDC';

  function ScrollWindowEx(hWnd:HWND; dx:longint; dy:longint; var prcScroll:RECT; var prcClip:RECT;
             hrgnUpdate:HRGN; prcUpdate:LPRECT; flags:UINT):longint; external 'user32' name 'ScrollWindowEx';

  function SetScrollPos(hWnd:HWND; nBar:longint; nPos:longint; bRedraw:WINBOOL):longint; external 'user32' name 'SetScrollPos';

  function GetScrollPos(hWnd:HWND; nBar:longint):longint; external 'user32' name 'GetScrollPos';

  function SetScrollRange(hWnd:HWND; nBar:longint; nMinPos:longint; nMaxPos:longint; bRedraw:WINBOOL):WINBOOL; external 'user32' name 'SetScrollRange';

  function GetScrollRange(hWnd:HWND; nBar:longint; lpMinPos:LPINT; lpMaxPos:LPINT):WINBOOL; external 'user32' name 'GetScrollRange';

  function ShowScrollBar(hWnd:HWND; wBar:longint; bShow:WINBOOL):WINBOOL; external 'user32' name 'ShowScrollBar';

  function EnableScrollBar(hWnd:HWND; wSBflags:UINT; wArrows:UINT):WINBOOL; external 'user32' name 'EnableScrollBar';

  function GetClientRect(hWnd:HWND; lpRect:LPRECT):WINBOOL; external 'user32' name 'GetClientRect';

  function GetWindowRect(hWnd:HWND; lpRect:LPRECT):WINBOOL; external 'user32' name 'GetWindowRect';

  function AdjustWindowRect(lpRect:LPRECT; dwStyle:DWORD; bMenu:WINBOOL):WINBOOL; external 'user32' name 'AdjustWindowRect';

  function AdjustWindowRectEx(lpRect:LPRECT; dwStyle:DWORD; bMenu:WINBOOL; dwExStyle:DWORD):WINBOOL; external 'user32' name 'AdjustWindowRectEx';

  function SetWindowContextHelpId(_para1:HWND; _para2:DWORD):WINBOOL; external 'user32' name 'SetWindowContextHelpId';

  function GetWindowContextHelpId(_para1:HWND):DWORD; external 'user32' name 'GetWindowContextHelpId';

  function SetMenuContextHelpId(_para1:HMENU; _para2:DWORD):WINBOOL; external 'user32' name 'SetMenuContextHelpId';

  function GetMenuContextHelpId(_para1:HMENU):DWORD; external 'user32' name 'GetMenuContextHelpId';

  function MessageBeep(uType:UINT):WINBOOL; external 'user32' name 'MessageBeep';

  function ShowCursor(bShow:WINBOOL):longint; external 'user32' name 'ShowCursor';

  function SetCursorPos(X:longint; Y:longint):WINBOOL; external 'user32' name 'SetCursorPos';

  function SetCursor(hCursor:HCURSOR):HCURSOR; external 'user32' name 'SetCursor';

  function GetCursorPos(lpPoint:LPPOINT):WINBOOL; external 'user32' name 'GetCursorPos';

  function ClipCursor(var lpRect:RECT):WINBOOL; external 'user32' name 'ClipCursor';

  function GetClipCursor(lpRect:LPRECT):WINBOOL; external 'user32' name 'GetClipCursor';

  function GetCursor:HCURSOR; external 'user32' name 'GetCursor';

  function CreateCaret(hWnd:HWND; hBitmap:HBITMAP; nWidth:longint; nHeight:longint):WINBOOL; external 'user32' name 'CreateCaret';

  function GetCaretBlinkTime:UINT; external 'user32' name 'GetCaretBlinkTime';

  function SetCaretBlinkTime(uMSeconds:UINT):WINBOOL; external 'user32' name 'SetCaretBlinkTime';

  function DestroyCaret:WINBOOL; external 'user32' name 'DestroyCaret';

  function HideCaret(hWnd:HWND):WINBOOL; external 'user32' name 'HideCaret';

  function ShowCaret(hWnd:HWND):WINBOOL; external 'user32' name 'ShowCaret';

  function SetCaretPos(X:longint; Y:longint):WINBOOL; external 'user32' name 'SetCaretPos';

  function GetCaretPos(lpPoint:LPPOINT):WINBOOL; external 'user32' name 'GetCaretPos';

  function ClientToScreen(hWnd:HWND; lpPoint:LPPOINT):WINBOOL; external 'user32' name 'ClientToScreen';

  function ScreenToClient(hWnd:HWND; lpPoint:LPPOINT):WINBOOL; external 'user32' name 'ScreenToClient';

  function MapWindowPoints(hWndFrom:HWND; hWndTo:HWND; lpPoints:LPPOINT; cPoints:UINT):longint; external 'user32' name 'MapWindowPoints';

  function WindowFromPoint(Point:POINT):HWND; external 'user32' name 'WindowFromPoint';

  function ChildWindowFromPoint(hWndParent:HWND; Point:POINT):HWND; external 'user32' name 'ChildWindowFromPoint';

  function GetSysColor(nIndex:longint):DWORD; external 'user32' name 'GetSysColor';

  function GetSysColorBrush(nIndex:longint):HBRUSH; external 'user32' name 'GetSysColorBrush';

  function SetSysColors(cElements:longint; var lpaElements:INT; var lpaRgbValues:COLORREF):WINBOOL; external 'user32' name 'SetSysColors';

  function DrawFocusRect(hDC:HDC; var lprc:RECT):WINBOOL; external 'user32' name 'DrawFocusRect';

  function FillRect(hDC:HDC; var lprc:RECT; hbr:HBRUSH):longint; external 'user32' name 'FillRect';

  function FrameRect(hDC:HDC; var lprc:RECT; hbr:HBRUSH):longint; external 'user32' name 'FrameRect';

  function InvertRect(hDC:HDC; var lprc:RECT):WINBOOL; external 'user32' name 'InvertRect';

  function SetRect(lprc:LPRECT; xLeft:longint; yTop:longint; xRight:longint; yBottom:longint):WINBOOL; external 'user32' name 'SetRect';

  function SetRectEmpty(lprc:LPRECT):WINBOOL; external 'user32' name 'SetRectEmpty';

  function CopyRect(lprcDst:LPRECT; var lprcSrc:RECT):WINBOOL; external 'user32' name 'CopyRect';

  function InflateRect(lprc:LPRECT; dx:longint; dy:longint):WINBOOL; external 'user32' name 'InflateRect';

  function IntersectRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL; external 'user32' name 'IntersectRect';

  function UnionRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL; external 'user32' name 'UnionRect';

  function SubtractRect(lprcDst:LPRECT; var lprcSrc1:RECT; var lprcSrc2:RECT):WINBOOL; external 'user32' name 'SubtractRect';

  function OffsetRect(lprc:LPRECT; dx:longint; dy:longint):WINBOOL; external 'user32' name 'OffsetRect';

  function IsRectEmpty(var lprc:RECT):WINBOOL; external 'user32' name 'IsRectEmpty';

  function EqualRect(var lprc1:RECT; var lprc2:RECT):WINBOOL; external 'user32' name 'EqualRect';

  function PtInRect(var lprc:RECT; pt:POINT):WINBOOL; external 'user32' name 'PtInRect';

  function GetWindowWord(hWnd:HWND; nIndex:longint):WORD; external 'user32' name 'GetWindowWord';

  function SetWindowWord(hWnd:HWND; nIndex:longint; wNewWord:WORD):WORD; external 'user32' name 'SetWindowWord';

  function GetClassWord(hWnd:HWND; nIndex:longint):WORD; external 'user32' name 'GetClassWord';

  function SetClassWord(hWnd:HWND; nIndex:longint; wNewWord:WORD):WORD; external 'user32' name 'SetClassWord';

  function GetDesktopWindow:HWND; external 'user32' name 'GetDesktopWindow';

  function GetParent(hWnd:HWND):HWND; external 'user32' name 'GetParent';

  function SetParent(hWndChild:HWND; hWndNewParent:HWND):HWND; external 'user32' name 'SetParent';

  function EnumChildWindows(hWndParent:HWND; lpEnumFunc:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumChildWindows';

  function EnumWindows(lpEnumFunc:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumWindows';

  function EnumThreadWindows(dwThreadId:DWORD; lpfn:ENUMWINDOWSPROC; lParam:LPARAM):WINBOOL; external 'user32' name 'EnumThreadWindows';

  function GetTopWindow(hWnd:HWND):HWND; external 'user32' name 'GetTopWindow';

  function GetWindowThreadProcessId(hWnd:HWND; lpdwProcessId:LPDWORD):DWORD; external 'user32' name 'GetWindowThreadProcessId';

  function GetLastActivePopup(hWnd:HWND):HWND; external 'user32' name 'GetLastActivePopup';

  function GetWindow(hWnd:HWND; uCmd:UINT):HWND; external 'user32' name 'GetWindow';

  function UnhookWindowsHook(nCode:longint; pfnFilterProc:HOOKPROC):WINBOOL; external 'user32' name 'UnhookWindowsHook';

  function UnhookWindowsHookEx(hhk:HHOOK):WINBOOL; external 'user32' name 'UnhookWindowsHookEx';

  function CallNextHookEx(hhk:HHOOK; nCode:longint; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'CallNextHookEx';

  function CheckMenuRadioItem(_para1:HMENU; _para2:UINT; _para3:UINT; _para4:UINT; _para5:UINT):WINBOOL; external 'user32' name 'CheckMenuRadioItem';

  function CreateCursor(hInst:HINST; xHotSpot:longint; yHotSpot:longint; nWidth:longint; nHeight:longint;
             pvANDPlane:pointer; pvXORPlane:pointer):HCURSOR; external 'user32' name 'CreateCursor';

  function DestroyCursor(hCursor:HCURSOR):WINBOOL; external 'user32' name 'DestroyCursor';

  function SetSystemCursor(hcur:HCURSOR; anID:DWORD):WINBOOL; external 'user32' name 'SetSystemCursor';

  function CreateIcon(hInstance:HINST; nWidth:longint; nHeight:longint; cPlanes:BYTE; cBitsPixel:BYTE;
             var lpbANDbits:BYTE; var lpbXORbits:BYTE):HICON; external 'user32' name 'CreateIcon';

  function DestroyIcon(hIcon:HICON):WINBOOL; external 'user32' name 'DestroyIcon';

  function LookupIconIdFromDirectory(presbits:PBYTE; fIcon:WINBOOL):longint; external 'user32' name 'LookupIconIdFromDirectory';

  function LookupIconIdFromDirectoryEx(presbits:PBYTE; fIcon:WINBOOL; cxDesired:longint; cyDesired:longint; Flags:UINT):longint; external 'user32' name 'LookupIconIdFromDirectoryEx';

  function CreateIconFromResource(presbits:PBYTE; dwResSize:DWORD; fIcon:WINBOOL; dwVer:DWORD):HICON; external 'user32' name 'CreateIconFromResource';

  function CreateIconFromResourceEx(presbits:PBYTE; dwResSize:DWORD; fIcon:WINBOOL; dwVer:DWORD; cxDesired:longint;
             cyDesired:longint; Flags:UINT):HICON; external 'user32' name 'CreateIconFromResourceEx';

  function CopyImage(_para1:HANDLE; _para2:UINT; _para3:longint; _para4:longint; _para5:UINT):HICON; external 'user32' name 'CopyImage';

  function CreateIconIndirect(piconinfo:PICONINFO):HICON; external 'user32' name 'CreateIconIndirect';

  function CopyIcon(hIcon:HICON):HICON; external 'user32' name 'CopyIcon';

  function GetIconInfo(hIcon:HICON; piconinfo:PICONINFO):WINBOOL; external 'user32' name 'GetIconInfo';

  function MapDialogRect(hDlg:HWND; lpRect:LPRECT):WINBOOL; external 'user32' name 'MapDialogRect';

  function SetScrollInfo(_para1:HWND; _para2:longint; _para3:LPCSCROLLINFO; _para4:WINBOOL):longint; external 'user32' name 'SetScrollInfo';

  function GetScrollInfo(_para1:HWND; _para2:longint; _para3:LPSCROLLINFO):WINBOOL; external 'user32' name 'GetScrollInfo';

  function TranslateMDISysAccel(hWndClient:HWND; lpMsg:LPMSG):WINBOOL; external 'user32' name 'TranslateMDISysAccel';

  function ArrangeIconicWindows(hWnd:HWND):UINT; external 'user32' name 'ArrangeIconicWindows';

  function TileWindows(hwndParent:HWND; wHow:UINT; var lpRect:RECT; cKids:UINT; var lpKids:HWND):WORD; external 'user32' name 'TileWindows';

  function CascadeWindows(hwndParent:HWND; wHow:UINT; var lpRect:RECT; cKids:UINT; var lpKids:HWND):WORD; external 'user32' name 'CascadeWindows';

  procedure SetLastErrorEx(dwErrCode:DWORD; dwType:DWORD); external 'user32' name 'SetLastErrorEx';

  procedure SetDebugErrorLevel(dwLevel:DWORD); external 'user32' name 'SetDebugErrorLevel';

  function DrawEdge(hdc:HDC; qrc:LPRECT; edge:UINT; grfFlags:UINT):WINBOOL; external 'user32' name 'DrawEdge';

  function DrawFrameControl(_para1:HDC; _para2:LPRECT; _para3:UINT; _para4:UINT):WINBOOL; external 'user32' name 'DrawFrameControl';

  function DrawCaption(_para1:HWND; _para2:HDC; var _para3:RECT; _para4:UINT):WINBOOL; external 'user32' name 'DrawCaption';

  function DrawAnimatedRects(hwnd:HWND; idAni:longint; var lprcFrom:RECT; var lprcTo:RECT):WINBOOL; external 'user32' name 'DrawAnimatedRects';

  function TrackPopupMenuEx(_para1:HMENU; _para2:UINT; _para3:longint; _para4:longint; _para5:HWND;
             _para6:LPTPMPARAMS):WINBOOL; external 'user32' name 'TrackPopupMenuEx';

  function ChildWindowFromPointEx(_para1:HWND; _para2:POINT; _para3:UINT):HWND; external 'user32' name 'ChildWindowFromPointEx';

  function DrawIconEx(hdc:HDC; xLeft:longint; yTop:longint; hIcon:HICON; cxWidth:longint;
             cyWidth:longint; istepIfAniCur:UINT; hbrFlickerFreeDraw:HBRUSH; diFlags:UINT):WINBOOL; external 'user32' name 'DrawIconEx';

  function AnimatePalette(_para1:HPALETTE; _para2:UINT; _para3:UINT; var _para4:PALETTEENTRY):WINBOOL; external 'gdi32' name 'AnimatePalette';

  function Arc(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'Arc';

  function BitBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:HDC; _para7:longint; _para8:longint; _para9:DWORD):WINBOOL; external 'gdi32' name 'BitBlt';

  function CancelDC(_para1:HDC):WINBOOL; external 'gdi32' name 'CancelDC';

  function Chord(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'Chord';

  function CloseMetaFile(_para1:HDC):HMETAFILE; external 'gdi32' name 'CloseMetaFile';

  function CombineRgn(_para1:HRGN; _para2:HRGN; _para3:HRGN; _para4:longint):longint; external 'gdi32' name 'CombineRgn';

  function CreateBitmap(_para1:longint; _para2:longint; _para3:UINT; _para4:UINT; _para5:pointer):HBITMAP; external 'gdi32' name 'CreateBitmap';

  function CreateBitmapIndirect(var _para1:BITMAP):HBITMAP; external 'gdi32' name 'CreateBitmapIndirect';

  function CreateBrushIndirect(var _para1:LOGBRUSH):HBRUSH; external 'gdi32' name 'CreateBrushIndirect';

  function CreateCompatibleBitmap(_para1:HDC; _para2:longint; _para3:longint):HBITMAP; external 'gdi32' name 'CreateCompatibleBitmap';

  function CreateDiscardableBitmap(_para1:HDC; _para2:longint; _para3:longint):HBITMAP; external 'gdi32' name 'CreateDiscardableBitmap';

  function CreateCompatibleDC(_para1:HDC):HDC; external 'gdi32' name 'CreateCompatibleDC';

  function CreateDIBitmap(_para1:HDC; var _para2:BITMAPINFOHEADER; _para3:DWORD; _para4:pointer; var _para5:BITMAPINFO;
             _para6:UINT):HBITMAP; external 'gdi32' name 'CreateDIBitmap';

  function CreateDIBPatternBrush(_para1:HGLOBAL; _para2:UINT):HBRUSH; external 'gdi32' name 'CreateDIBPatternBrush';

  function CreateDIBPatternBrushPt(_para1:pointer; _para2:UINT):HBRUSH; external 'gdi32' name 'CreateDIBPatternBrushPt';

  function CreateEllipticRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint):HRGN; external 'gdi32' name 'CreateEllipticRgn';

  function CreateEllipticRgnIndirect(var _para1:RECT):HRGN; external 'gdi32' name 'CreateEllipticRgnIndirect';

  function CreateHatchBrush(_para1:longint; _para2:COLORREF):HBRUSH; external 'gdi32' name 'CreateHatchBrush';

  function CreatePalette(var _para1:LOGPALETTE):HPALETTE; external 'gdi32' name 'CreatePalette';

  function CreatePen(_para1:longint; _para2:longint; _para3:COLORREF):HPEN; external 'gdi32' name 'CreatePen';

  function CreatePenIndirect(var _para1:LOGPEN):HPEN; external 'gdi32' name 'CreatePenIndirect';

  function CreatePolyPolygonRgn(var _para1:POINT; var _para2:INT; _para3:longint; _para4:longint):HRGN; external 'gdi32' name 'CreatePolyPolygonRgn';

  function CreatePatternBrush(_para1:HBITMAP):HBRUSH; external 'gdi32' name 'CreatePatternBrush';

  function CreateRectRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint):HRGN; external 'gdi32' name 'CreateRectRgn';

  function CreateRectRgnIndirect(var _para1:RECT):HRGN; external 'gdi32' name 'CreateRectRgnIndirect';

  function CreateRoundRectRgn(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint):HRGN; external 'gdi32' name 'CreateRoundRectRgn';

  function CreateSolidBrush(_para1:COLORREF):HBRUSH; external 'gdi32' name 'CreateSolidBrush';

  function DeleteDC(_para1:HDC):WINBOOL; external 'gdi32' name 'DeleteDC';

  function DeleteMetaFile(_para1:HMETAFILE):WINBOOL; external 'gdi32' name 'DeleteMetaFile';

  function DeleteObject(_para1:HGDIOBJ):WINBOOL; external 'gdi32' name 'DeleteObject';

  function DrawEscape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR):longint; external 'gdi32' name 'DrawEscape';

  function Ellipse(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL; external 'gdi32' name 'Ellipse';

  function EnumObjects(_para1:HDC; _para2:longint; _para3:ENUMOBJECTSPROC; _para4:LPARAM):longint; external 'gdi32' name 'EnumObjects';

  function EqualRgn(_para1:HRGN; _para2:HRGN):WINBOOL; external 'gdi32' name 'EqualRgn';

  function Escape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:LPVOID):longint; external 'gdi32' name 'Escape';

  function ExtEscape(_para1:HDC; _para2:longint; _para3:longint; _para4:LPCSTR; _para5:longint;
             _para6:LPSTR):longint; external 'gdi32' name 'ExtEscape';

  function ExcludeClipRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):longint; external 'gdi32' name 'ExcludeClipRect';

  function ExtCreateRegion(var _para1:XFORM; _para2:DWORD; var _para3:RGNDATA):HRGN; external 'gdi32' name 'ExtCreateRegion';

  function ExtFloodFill(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF; _para5:UINT):WINBOOL; external 'gdi32' name 'ExtFloodFill';

  function FillRgn(_para1:HDC; _para2:HRGN; _para3:HBRUSH):WINBOOL; external 'gdi32' name 'FillRgn';

  function FloodFill(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):WINBOOL; external 'gdi32' name 'FloodFill';

  function FrameRgn(_para1:HDC; _para2:HRGN; _para3:HBRUSH; _para4:longint; _para5:longint):WINBOOL; external 'gdi32' name 'FrameRgn';

  function GetROP2(_para1:HDC):longint; external 'gdi32' name 'GetROP2';

  function GetAspectRatioFilterEx(_para1:HDC; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetAspectRatioFilterEx';

  function GetBkColor(_para1:HDC):COLORREF; external 'gdi32' name 'GetBkColor';

  function GetBkMode(_para1:HDC):longint; external 'gdi32' name 'GetBkMode';

  function GetBitmapBits(_para1:HBITMAP; _para2:LONG; _para3:LPVOID):LONG; external 'gdi32' name 'GetBitmapBits';

  function GetBitmapDimensionEx(_para1:HBITMAP; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetBitmapDimensionEx';

  function GetBoundsRect(_para1:HDC; _para2:LPRECT; _para3:UINT):UINT; external 'gdi32' name 'GetBoundsRect';

  function GetBrushOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetBrushOrgEx';

  function GetClipBox(_para1:HDC; _para2:LPRECT):longint; external 'gdi32' name 'GetClipBox';

  function GetClipRgn(_para1:HDC; _para2:HRGN):longint; external 'gdi32' name 'GetClipRgn';

  function GetMetaRgn(_para1:HDC; _para2:HRGN):longint; external 'gdi32' name 'GetMetaRgn';

  function GetCurrentObject(_para1:HDC; _para2:UINT):HGDIOBJ; external 'gdi32' name 'GetCurrentObject';

  function GetCurrentPositionEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetCurrentPositionEx';

  function GetDeviceCaps(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'GetDeviceCaps';

  function GetDIBits(_para1:HDC; _para2:HBITMAP; _para3:UINT; _para4:UINT; _para5:LPVOID;
             _para6:LPBITMAPINFO; _para7:UINT):longint; external 'gdi32' name 'GetDIBits';

  function GetFontData(_para1:HDC; _para2:DWORD; _para3:DWORD; _para4:LPVOID; _para5:DWORD):DWORD; external 'gdi32' name 'GetFontData';

  function GetGraphicsMode(_para1:HDC):longint; external 'gdi32' name 'GetGraphicsMode';

  function GetMapMode(_para1:HDC):longint; external 'gdi32' name 'GetMapMode';

  function GetMetaFileBitsEx(_para1:HMETAFILE; _para2:UINT; _para3:LPVOID):UINT; external 'gdi32' name 'GetMetaFileBitsEx';

  function GetNearestColor(_para1:HDC; _para2:COLORREF):COLORREF; external 'gdi32' name 'GetNearestColor';

  function GetNearestPaletteIndex(_para1:HPALETTE; _para2:COLORREF):UINT; external 'gdi32' name 'GetNearestPaletteIndex';

  function GetObjectType(h:HGDIOBJ):DWORD; external 'gdi32' name 'GetObjectType';

  function GetPaletteEntries(_para1:HPALETTE; _para2:UINT; _para3:UINT; _para4:LPPALETTEENTRY):UINT; external 'gdi32' name 'GetPaletteEntries';

  function GetPixel(_para1:HDC; _para2:longint; _para3:longint):COLORREF; external 'gdi32' name 'GetPixel';

  function GetPixelFormat(_para1:HDC):longint; external 'gdi32' name 'GetPixelFormat';

  function GetPolyFillMode(_para1:HDC):longint; external 'gdi32' name 'GetPolyFillMode';

  function GetRasterizerCaps(_para1:LPRASTERIZER_STATUS; _para2:UINT):WINBOOL; external 'gdi32' name 'GetRasterizerCaps';

  function GetRegionData(_para1:HRGN; _para2:DWORD; _para3:LPRGNDATA):DWORD; external 'gdi32' name 'GetRegionData';

  function GetRgnBox(_para1:HRGN; _para2:LPRECT):longint; external 'gdi32' name 'GetRgnBox';

  function GetStockObject(_para1:longint):HGDIOBJ; external 'gdi32' name 'GetStockObject';

  function GetStretchBltMode(_para1:HDC):longint; external 'gdi32' name 'GetStretchBltMode';

  function GetSystemPaletteEntries(_para1:HDC; _para2:UINT; _para3:UINT; _para4:LPPALETTEENTRY):UINT; external 'gdi32' name 'GetSystemPaletteEntries';

  function GetSystemPaletteUse(_para1:HDC):UINT; external 'gdi32' name 'GetSystemPaletteUse';

  function GetTextCharacterExtra(_para1:HDC):longint; external 'gdi32' name 'GetTextCharacterExtra';

  function GetTextAlign(_para1:HDC):UINT; external 'gdi32' name 'GetTextAlign';

  function GetTextColor(_para1:HDC):COLORREF; external 'gdi32' name 'GetTextColor';

  function GetTextCharset(hdc:HDC):longint; external 'gdi32' name 'GetTextCharset';

  function GetTextCharsetInfo(hdc:HDC; lpSig:LPFONTSIGNATURE; dwFlags:DWORD):longint; external 'gdi32' name 'GetTextCharsetInfo';

  function TranslateCharsetInfo(var lpSrc:DWORD; lpCs:LPCHARSETINFO; dwFlags:DWORD):WINBOOL; external 'gdi32' name 'TranslateCharsetInfo';

  function GetFontLanguageInfo(_para1:HDC):DWORD; external 'gdi32' name 'GetFontLanguageInfo';

  function GetViewportExtEx(_para1:HDC; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetViewportExtEx';

  function GetViewportOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetViewportOrgEx';

  function GetWindowExtEx(_para1:HDC; _para2:LPSIZE):WINBOOL; external 'gdi32' name 'GetWindowExtEx';

  function GetWindowOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetWindowOrgEx';

  function IntersectClipRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):longint; external 'gdi32' name 'IntersectClipRect';

  function InvertRgn(_para1:HDC; _para2:HRGN):WINBOOL; external 'gdi32' name 'InvertRgn';

  function LineDDA(_para1:longint; _para2:longint; _para3:longint; _para4:longint; _para5:LINEDDAPROC;
             _para6:LPARAM):WINBOOL; external 'gdi32' name 'LineDDA';

  function LineTo(_para1:HDC; _para2:longint; _para3:longint):WINBOOL; external 'gdi32' name 'LineTo';

  function MaskBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:HDC; _para7:longint; _para8:longint; _para9:HBITMAP; _para10:longint;
             _para11:longint; _para12:DWORD):WINBOOL; external 'gdi32' name 'MaskBlt';

  function PlgBlt(_para1:HDC; var _para2:POINT; _para3:HDC; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:HBITMAP; _para9:longint; _para10:longint):WINBOOL; external 'gdi32' name 'PlgBlt';

  function OffsetClipRgn(_para1:HDC; _para2:longint; _para3:longint):longint; external 'gdi32' name 'OffsetClipRgn';

  function OffsetRgn(_para1:HRGN; _para2:longint; _para3:longint):longint; external 'gdi32' name 'OffsetRgn';

  function PatBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:DWORD):WINBOOL; external 'gdi32' name 'PatBlt';

  function Pie(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'Pie';

  function PlayMetaFile(_para1:HDC; _para2:HMETAFILE):WINBOOL; external 'gdi32' name 'PlayMetaFile';

  function PaintRgn(_para1:HDC; _para2:HRGN):WINBOOL; external 'gdi32' name 'PaintRgn';

  function PolyPolygon(_para1:HDC; var _para2:POINT; var _para3:INT; _para4:longint):WINBOOL; external 'gdi32' name 'PolyPolygon';

  function PtInRegion(_para1:HRGN; _para2:longint; _para3:longint):WINBOOL; external 'gdi32' name 'PtInRegion';

  function PtVisible(_para1:HDC; _para2:longint; _para3:longint):WINBOOL; external 'gdi32' name 'PtVisible';

  function RectInRegion(_para1:HRGN; var _para2:RECT):WINBOOL; external 'gdi32' name 'RectInRegion';

  function RectVisible(_para1:HDC; var _para2:RECT):WINBOOL; external 'gdi32' name 'RectVisible';

  function Rectangle(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL; external 'gdi32' name 'Rectangle';

  function RestoreDC(_para1:HDC; _para2:longint):WINBOOL; external 'gdi32' name 'RestoreDC';

  function RealizePalette(_para1:HDC):UINT; external 'gdi32' name 'RealizePalette';

  function RoundRect(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint):WINBOOL; external 'gdi32' name 'RoundRect';

  function ResizePalette(_para1:HPALETTE; _para2:UINT):WINBOOL; external 'gdi32' name 'ResizePalette';

  function SaveDC(_para1:HDC):longint; external 'gdi32' name 'SaveDC';

  function SelectClipRgn(_para1:HDC; _para2:HRGN):longint; external 'gdi32' name 'SelectClipRgn';

  function ExtSelectClipRgn(_para1:HDC; _para2:HRGN; _para3:longint):longint; external 'gdi32' name 'ExtSelectClipRgn';

  function SetMetaRgn(_para1:HDC):longint; external 'gdi32' name 'SetMetaRgn';

  function SelectObject(_para1:HDC; _para2:HGDIOBJ):HGDIOBJ; external 'gdi32' name 'SelectObject';

  function SelectPalette(_para1:HDC; _para2:HPALETTE; _para3:WINBOOL):HPALETTE; external 'gdi32' name 'SelectPalette';

  function SetBkColor(_para1:HDC; _para2:COLORREF):COLORREF; external 'gdi32' name 'SetBkColor';

  function SetBkMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetBkMode';

  function SetBitmapBits(_para1:HBITMAP; _para2:DWORD; _para3:pointer):LONG; external 'gdi32' name 'SetBitmapBits';

  function SetBoundsRect(_para1:HDC; var _para2:RECT; _para3:UINT):UINT; external 'gdi32' name 'SetBoundsRect';

  function SetDIBits(_para1:HDC; _para2:HBITMAP; _para3:UINT; _para4:UINT; _para5:pointer;
             var _para6:BITMAPINFO; _para7:UINT):longint; external 'gdi32' name 'SetDIBits';

  function SetDIBitsToDevice(_para1:HDC; _para2:longint; _para3:longint; _para4:DWORD; _para5:DWORD;
             _para6:longint; _para7:longint; _para8:UINT; _para9:UINT; _para10:pointer;
             var _para11:BITMAPINFO; _para12:UINT):longint; external 'gdi32' name 'SetDIBitsToDevice';

  function SetMapperFlags(_para1:HDC; _para2:DWORD):DWORD; external 'gdi32' name 'SetMapperFlags';

  function SetGraphicsMode(hdc:HDC; iMode:longint):longint; external 'gdi32' name 'SetGraphicsMode';

  function SetMapMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetMapMode';

  function SetMetaFileBitsEx(_para1:UINT; var _para2:BYTE):HMETAFILE; external 'gdi32' name 'SetMetaFileBitsEx';

  function SetPaletteEntries(_para1:HPALETTE; _para2:UINT; _para3:UINT; var _para4:PALETTEENTRY):UINT; external 'gdi32' name 'SetPaletteEntries';

  function SetPixel(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):COLORREF; external 'gdi32' name 'SetPixel';

  function SetPixelV(_para1:HDC; _para2:longint; _para3:longint; _para4:COLORREF):WINBOOL; external 'gdi32' name 'SetPixelV';

  function SetPolyFillMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetPolyFillMode';

  function StretchBlt(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:HDC; _para7:longint; _para8:longint; _para9:longint; _para10:longint;
             _para11:DWORD):WINBOOL; external 'gdi32' name 'StretchBlt';

  function SetRectRgn(_para1:HRGN; _para2:longint; _para3:longint; _para4:longint; _para5:longint):WINBOOL; external 'gdi32' name 'SetRectRgn';

  function StretchDIBits(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint; _para10:pointer;
             var _para11:BITMAPINFO; _para12:UINT; _para13:DWORD):longint; external 'gdi32' name 'StretchDIBits';

  function SetROP2(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetROP2';

  function SetStretchBltMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetStretchBltMode';

  function SetSystemPaletteUse(_para1:HDC; _para2:UINT):UINT; external 'gdi32' name 'SetSystemPaletteUse';

  function SetTextCharacterExtra(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetTextCharacterExtra';

  function SetTextColor(_para1:HDC; _para2:COLORREF):COLORREF; external 'gdi32' name 'SetTextColor';

  function SetTextAlign(_para1:HDC; _para2:UINT):UINT; external 'gdi32' name 'SetTextAlign';

  function SetTextJustification(_para1:HDC; _para2:longint; _para3:longint):WINBOOL; external 'gdi32' name 'SetTextJustification';

  function UpdateColors(_para1:HDC):WINBOOL; external 'gdi32' name 'UpdateColors';

  function PlayMetaFileRecord(_para1:HDC; _para2:LPHANDLETABLE; _para3:LPMETARECORD; _para4:UINT):WINBOOL; external 'gdi32' name 'PlayMetaFileRecord';

  function EnumMetaFile(_para1:HDC; _para2:HMETAFILE; _para3:ENUMMETAFILEPROC; _para4:LPARAM):WINBOOL; external 'gdi32' name 'EnumMetaFile';

  function CloseEnhMetaFile(_para1:HDC):HENHMETAFILE; external 'gdi32' name 'CloseEnhMetaFile';

  function DeleteEnhMetaFile(_para1:HENHMETAFILE):WINBOOL; external 'gdi32' name 'DeleteEnhMetaFile';

  function EnumEnhMetaFile(_para1:HDC; _para2:HENHMETAFILE; _para3:ENHMETAFILEPROC; _para4:LPVOID; var _para5:RECT):WINBOOL; external 'gdi32' name 'EnumEnhMetaFile';

  function GetEnhMetaFileHeader(_para1:HENHMETAFILE; _para2:UINT; _para3:LPENHMETAHEADER):UINT; external 'gdi32' name 'GetEnhMetaFileHeader';

  function GetEnhMetaFilePaletteEntries(_para1:HENHMETAFILE; _para2:UINT; _para3:LPPALETTEENTRY):UINT; external 'gdi32' name 'GetEnhMetaFilePaletteEntries';

  function GetWinMetaFileBits(_para1:HENHMETAFILE; _para2:UINT; _para3:LPBYTE; _para4:INT; _para5:HDC):UINT; external 'gdi32' name 'GetWinMetaFileBits';

  function PlayEnhMetaFile(_para1:HDC; _para2:HENHMETAFILE; var _para3:RECT):WINBOOL; external 'gdi32' name 'PlayEnhMetaFile';

  function PlayEnhMetaFileRecord(_para1:HDC; _para2:LPHANDLETABLE; var _para3:ENHMETARECORD; _para4:UINT):WINBOOL; external 'gdi32' name 'PlayEnhMetaFileRecord';

  function SetEnhMetaFileBits(_para1:UINT; var _para2:BYTE):HENHMETAFILE; external 'gdi32' name 'SetEnhMetaFileBits';

  function SetWinMetaFileBits(_para1:UINT; var _para2:BYTE; _para3:HDC; var _para4:METAFILEPICT):HENHMETAFILE; external 'gdi32' name 'SetWinMetaFileBits';

  function GdiComment(_para1:HDC; _para2:UINT; var _para3:BYTE):WINBOOL; external 'gdi32' name 'GdiComment';

  function AngleArc(_para1:HDC; _para2:longint; _para3:longint; _para4:DWORD; _para5:FLOAT;
             _para6:FLOAT):WINBOOL; external 'gdi32' name 'AngleArc';

  function PolyPolyline(_para1:HDC; var _para2:POINT; var _para3:DWORD; _para4:DWORD):WINBOOL; external 'gdi32' name 'PolyPolyline';

  function GetWorldTransform(_para1:HDC; _para2:LPXFORM):WINBOOL; external 'gdi32' name 'GetWorldTransform';

  function SetWorldTransform(_para1:HDC; var _para2:XFORM):WINBOOL; external 'gdi32' name 'SetWorldTransform';

  function ModifyWorldTransform(_para1:HDC; var _para2:XFORM; _para3:DWORD):WINBOOL; external 'gdi32' name 'ModifyWorldTransform';

  function CombineTransform(_para1:LPXFORM; var _para2:XFORM; var _para3:XFORM):WINBOOL; external 'gdi32' name 'CombineTransform';

  function CreateDIBSection(_para1:HDC; var _para2:BITMAPINFO; _para3:UINT; var _para4:pointer; _para5:HANDLE;
             _para6:DWORD):HBITMAP; external 'gdi32' name 'CreateDIBSection';

  function GetDIBColorTable(_para1:HDC; _para2:UINT; _para3:UINT; var _para4:RGBQUAD):UINT; external 'gdi32' name 'GetDIBColorTable';

  function SetDIBColorTable(_para1:HDC; _para2:UINT; _para3:UINT; var _para4:RGBQUAD):UINT; external 'gdi32' name 'SetDIBColorTable';

  function SetColorAdjustment(_para1:HDC; var _para2:COLORADJUSTMENT):WINBOOL; external 'gdi32' name 'SetColorAdjustment';

  function GetColorAdjustment(_para1:HDC; _para2:LPCOLORADJUSTMENT):WINBOOL; external 'gdi32' name 'GetColorAdjustment';

  function CreateHalftonePalette(_para1:HDC):HPALETTE; external 'gdi32' name 'CreateHalftonePalette';

  function EndDoc(_para1:HDC):longint; external 'gdi32' name 'EndDoc';

  function StartPage(_para1:HDC):longint; external 'gdi32' name 'StartPage';

  function EndPage(_para1:HDC):longint; external 'gdi32' name 'EndPage';

  function AbortDoc(_para1:HDC):longint; external 'gdi32' name 'AbortDoc';

  function SetAbortProc(_para1:HDC; _para2:TABORTPROC):longint; external 'gdi32' name 'SetAbortProc';

(*  function AbortPath(_para1:HDC):WINBOOL; external 'gdi32' name 'AbortPath';
*)

  function ArcTo(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:longint; _para7:longint; _para8:longint; _para9:longint):WINBOOL; external 'gdi32' name 'ArcTo';

  function BeginPath(_para1:HDC):WINBOOL; external 'gdi32' name 'BeginPath';

  function CloseFigure(_para1:HDC):WINBOOL; external 'gdi32' name 'CloseFigure';

  function EndPath(_para1:HDC):WINBOOL; external 'gdi32' name 'EndPath';

  function FillPath(_para1:HDC):WINBOOL; external 'gdi32' name 'FillPath';

  function FlattenPath(_para1:HDC):WINBOOL; external 'gdi32' name 'FlattenPath';

  function GetPath(_para1:HDC; _para2:LPPOINT; _para3:LPBYTE; _para4:longint):longint; external 'gdi32' name 'GetPath';

  function PathToRegion(_para1:HDC):HRGN; external 'gdi32' name 'PathToRegion';

  function PolyDraw(_para1:HDC; var _para2:POINT; var _para3:BYTE; _para4:longint):WINBOOL; external 'gdi32' name 'PolyDraw';

  function SelectClipPath(_para1:HDC; _para2:longint):WINBOOL; external 'gdi32' name 'SelectClipPath';

  function SetArcDirection(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetArcDirection';

  function SetMiterLimit(_para1:HDC; _para2:FLOAT; _para3:PFLOAT):WINBOOL; external 'gdi32' name 'SetMiterLimit';

  function StrokeAndFillPath(_para1:HDC):WINBOOL; external 'gdi32' name 'StrokeAndFillPath';

  function StrokePath(_para1:HDC):WINBOOL; external 'gdi32' name 'StrokePath';

  function WidenPath(_para1:HDC):WINBOOL; external 'gdi32' name 'WidenPath';

  function ExtCreatePen(_para1:DWORD; _para2:DWORD; var _para3:LOGBRUSH; _para4:DWORD; var _para5:DWORD):HPEN; external 'gdi32' name 'ExtCreatePen';

  function GetMiterLimit(_para1:HDC; _para2:PFLOAT):WINBOOL; external 'gdi32' name 'GetMiterLimit';

  function GetArcDirection(_para1:HDC):longint; external 'gdi32' name 'GetArcDirection';

  function MoveToEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'MoveToEx';

  function CreatePolygonRgn(var _para1:POINT; _para2:longint; _para3:longint):HRGN; external 'gdi32' name 'CreatePolygonRgn';

  function DPtoLP(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL; external 'gdi32' name 'DPtoLP';

  function LPtoDP(_para1:HDC; _para2:LPPOINT; _para3:longint):WINBOOL; external 'gdi32' name 'LPtoDP';

  function Polygon(_para1:HDC; var _para2:POINT; _para3:longint):WINBOOL; external 'gdi32' name 'Polygon';

  function Polyline(_para1:HDC; var _para2:POINT; _para3:longint):WINBOOL; external 'gdi32' name 'Polyline';

  function PolyBezier(_para1:HDC; var _para2:POINT; _para3:DWORD):WINBOOL; external 'gdi32' name 'PolyBezier';

  function PolyBezierTo(_para1:HDC; var _para2:POINT; _para3:DWORD):WINBOOL; external 'gdi32' name 'PolyBezierTo';

  function PolylineTo(_para1:HDC; var _para2:POINT; _para3:DWORD):WINBOOL; external 'gdi32' name 'PolylineTo';

  function SetViewportExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'SetViewportExtEx';

  function SetViewportOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'SetViewportOrgEx';

  function SetWindowExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'SetWindowExtEx';

  function SetWindowOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'SetWindowOrgEx';

  function OffsetViewportOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'OffsetViewportOrgEx';

  function OffsetWindowOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'OffsetWindowOrgEx';

  function ScaleViewportExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:LPSIZE):WINBOOL; external 'gdi32' name 'ScaleViewportExtEx';

  function ScaleWindowExtEx(_para1:HDC; _para2:longint; _para3:longint; _para4:longint; _para5:longint;
             _para6:LPSIZE):WINBOOL; external 'gdi32' name 'ScaleWindowExtEx';

  function SetBitmapDimensionEx(_para1:HBITMAP; _para2:longint; _para3:longint; _para4:LPSIZE):WINBOOL; external 'gdi32' name 'SetBitmapDimensionEx';

  function SetBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'SetBrushOrgEx';

  function GetDCOrgEx(_para1:HDC; _para2:LPPOINT):WINBOOL; external 'gdi32' name 'GetDCOrgEx';

  function FixBrushOrgEx(_para1:HDC; _para2:longint; _para3:longint; _para4:LPPOINT):WINBOOL; external 'gdi32' name 'FixBrushOrgEx';

  function UnrealizeObject(_para1:HGDIOBJ):WINBOOL; external 'gdi32' name 'UnrealizeObject';

  function GdiFlush:WINBOOL; external 'gdi32' name 'GdiFlush';

  function GdiSetBatchLimit(_para1:DWORD):DWORD; external 'gdi32' name 'GdiSetBatchLimit';

  function GdiGetBatchLimit:DWORD; external 'gdi32' name 'GdiGetBatchLimit';

  function SetICMMode(_para1:HDC; _para2:longint):longint; external 'gdi32' name 'SetICMMode';

  function CheckColorsInGamut(_para1:HDC; _para2:LPVOID; _para3:LPVOID; _para4:DWORD):WINBOOL; external 'gdi32' name 'CheckColorsInGamut';

  function GetColorSpace(_para1:HDC):HANDLE; external 'gdi32' name 'GetColorSpace';

  function SetColorSpace(_para1:HDC; _para2:HCOLORSPACE):WINBOOL; external 'gdi32' name 'SetColorSpace';

  function DeleteColorSpace(_para1:HCOLORSPACE):WINBOOL; external 'gdi32' name 'DeleteColorSpace';

  function GetDeviceGammaRamp(_para1:HDC; _para2:LPVOID):WINBOOL; external 'gdi32' name 'GetDeviceGammaRamp';

  function SetDeviceGammaRamp(_para1:HDC; _para2:LPVOID):WINBOOL; external 'gdi32' name 'SetDeviceGammaRamp';

  function ColorMatchToTarget(_para1:HDC; _para2:HDC; _para3:DWORD):WINBOOL; external 'gdi32' name 'ColorMatchToTarget';

  function CreatePropertySheetPageA(lppsp:LPCPROPSHEETPAGE):HPROPSHEETPAGE; external 'comctl32' name 'CreatePropertySheetPageA';

  function DestroyPropertySheetPage(hPSPage:HPROPSHEETPAGE):WINBOOL; external 'comctl32' name 'DestroyPropertySheetPage';

  procedure InitCommonControls; external 'comctl32' name 'InitCommonControls';

  { was #define dname(params) def_expr }
  function ImageList_AddIcon(himl:HIMAGELIST; hicon:HICON):longint;
    begin
       ImageList_AddIcon:=ImageList_ReplaceIcon(himl,-(1),hicon);
    end;

  function ImageList_Create(cx:longint; cy:longint; flags:UINT; cInitial:longint; cGrow:longint):HIMAGELIST; external 'comctl32' name 'ImageList_Create';

  function ImageList_Destroy(himl:HIMAGELIST):WINBOOL; external 'comctl32' name 'ImageList_Destroy';

  function ImageList_GetImageCount(himl:HIMAGELIST):longint; external 'comctl32' name 'ImageList_GetImageCount';

  function ImageList_Add(himl:HIMAGELIST; hbmImage:HBITMAP; hbmMask:HBITMAP):longint; external 'comctl32' name 'ImageList_Add';

  function ImageList_ReplaceIcon(himl:HIMAGELIST; i:longint; hicon:HICON):longint; external 'comctl32' name 'ImageList_ReplaceIcon';

  function ImageList_SetBkColor(himl:HIMAGELIST; clrBk:COLORREF):COLORREF; external 'comctl32' name 'ImageList_SetBkColor';

  function ImageList_GetBkColor(himl:HIMAGELIST):COLORREF; external 'comctl32' name 'ImageList_GetBkColor';

  function ImageList_SetOverlayImage(himl:HIMAGELIST; iImage:longint; iOverlay:longint):WINBOOL; external 'comctl32' name 'ImageList_SetOverlayImage';

  function ImageList_Draw(himl:HIMAGELIST; i:longint; hdcDst:HDC; x:longint; y:longint;
             fStyle:UINT):WINBOOL; external 'comctl32' name 'ImageList_Draw';

  function ImageList_Replace(himl:HIMAGELIST; i:longint; hbmImage:HBITMAP; hbmMask:HBITMAP):WINBOOL; external 'comctl32' name 'ImageList_Replace';

  function ImageList_AddMasked(himl:HIMAGELIST; hbmImage:HBITMAP; crMask:COLORREF):longint; external 'comctl32' name 'ImageList_AddMasked';

  function ImageList_DrawEx(himl:HIMAGELIST; i:longint; hdcDst:HDC; x:longint; y:longint;
             dx:longint; dy:longint; rgbBk:COLORREF; rgbFg:COLORREF; fStyle:UINT):WINBOOL; external 'comctl32' name 'ImageList_DrawEx';

  function ImageList_Remove(himl:HIMAGELIST; i:longint):WINBOOL; external 'comctl32' name 'ImageList_Remove';

  function ImageList_GetIcon(himl:HIMAGELIST; i:longint; flags:UINT):HICON; external 'comctl32' name 'ImageList_GetIcon';

  function ImageList_BeginDrag(himlTrack:HIMAGELIST; iTrack:longint; dxHotspot:longint; dyHotspot:longint):WINBOOL; external 'comctl32' name 'ImageList_BeginDrag';

  procedure ImageList_EndDrag; external 'comctl32' name 'ImageList_EndDrag';

  function ImageList_DragEnter(hwndLock:HWND; x:longint; y:longint):WINBOOL; external 'comctl32' name 'ImageList_DragEnter';

  function ImageList_DragLeave(hwndLock:HWND):WINBOOL; external 'comctl32' name 'ImageList_DragLeave';

  function ImageList_DragMove(x:longint; y:longint):WINBOOL; external 'comctl32' name 'ImageList_DragMove';

  function ImageList_SetDragCursorImage(himlDrag:HIMAGELIST; iDrag:longint; dxHotspot:longint; dyHotspot:longint):WINBOOL; external 'comctl32' name 'ImageList_SetDragCursorImage';

  function ImageList_DragShowNolock(fShow:WINBOOL):WINBOOL; external 'comctl32' name 'ImageList_DragShowNolock';

  function ImageList_GetDragImage(var ppt:POINT; var pptHotspot:POINT):HIMAGELIST; external 'comctl32' name 'ImageList_GetDragImage';

  function ImageList_GetIconSize(himl:HIMAGELIST; var cx:longint; var cy:longint):WINBOOL; external 'comctl32' name 'ImageList_GetIconSize';

  function ImageList_SetIconSize(himl:HIMAGELIST; cx:longint; cy:longint):WINBOOL; external 'comctl32' name 'ImageList_SetIconSize';

  function ImageList_GetImageInfo(himl:HIMAGELIST; i:longint; var pImageInfo:IMAGEINFO):WINBOOL; external 'comctl32' name 'ImageList_GetImageInfo';

  function ImageList_Merge(himl1:HIMAGELIST; i1:longint; himl2:HIMAGELIST; i2:longint; dx:longint;
             dy:longint):HIMAGELIST; external 'comctl32' name 'ImageList_Merge';

  function CreateToolbarEx(hwnd:HWND; ws:DWORD; wID:UINT; nBitmaps:longint; hBMInst:HINST;
             wBMID:UINT; lpButtons:LPCTBBUTTON; iNumButtons:longint; dxButton:longint; dyButton:longint;
             dxBitmap:longint; dyBitmap:longint; uStructSize:UINT):HWND; external 'comctl32' name 'CreateToolbarEx';

  function CreateMappedBitmap(hInstance:HINST; idBitmap:longint; wFlags:UINT; lpColorMap:LPCOLORMAP; iNumMaps:longint):HBITMAP; external 'comctl32' name 'CreateMappedBitmap';

  procedure MenuHelp(uMsg:UINT; wParam:WPARAM; lParam:LPARAM; hMainMenu:HMENU; hInst:HINST;
              hwndStatus:HWND; var lpwIDs:UINT); external 'comctl32' name 'MenuHelp';

  function ShowHideMenuCtl(hWnd:HWND; uFlags:UINT; lpInfo:LPINT):WINBOOL; external 'comctl32' name 'ShowHideMenuCtl';

  procedure GetEffectiveClientRect(hWnd:HWND; lprc:LPRECT; lpInfo:LPINT); external 'comctl32' name 'GetEffectiveClientRect';

  function MakeDragList(hLB:HWND):WINBOOL; external 'comctl32' name 'MakeDragList';

  procedure DrawInsert(handParent:HWND; hLB:HWND; nItem:longint); external 'comctl32' name 'DrawInsert';

  function LBItemFromPt(hLB:HWND; pt:POINT; bAutoScroll:WINBOOL):longint; external 'comctl32' name 'LBItemFromPt';

  function CreateUpDownControl(dwStyle:DWORD; x:longint; y:longint; cx:longint; cy:longint;
             hParent:HWND; nID:longint; hInst:HINST; hBuddy:HWND; nUpper:longint;
             nLower:longint; nPos:longint):HWND; external 'comctl32' name 'CreateUpDownControl';

  function CommDlgExtendedError:DWORD; external 'comdlg32' name 'CommDlgExtendedError';

  { was #define dname(params) def_expr }
  function Animate_Create(hWndP:HWND; id:HMENU;dwStyle:DWORD;hInstance:HINST):HWND;
    begin
       Animate_Create:=CreateWindow(LPCSTR(ANIMATE_CLASS),nil,dwStyle,0,0,0,0,hwndP,id,hInstance,nil);
    end;

  { was #define dname(params) def_expr }
  { argument types are unknown }
  { return type might be wrong }
  function Animate_Open(hwnd : HWND;szName : LPTSTR) : LRESULT;
    { return type might be wrong }
    begin
       Animate_Open:=SendMessage(hwnd,ACM_OPEN,0,LPARAM(szName));
    end;

  { was #define dname(params) def_expr }
  function Animate_Play(hwnd : HWND;from,_to : longint;rep : UINT) : LRESULT;
    begin
       Animate_Play:=SendMessage(hwnd,ACM_PLAY,WPARAM(rep),LPARAM(MAKELONG(from,_to)));
    end;

  { was #define dname(params) def_expr }
  function Animate_Stop(hwnd : HWND) : LRESULT;
    begin
       Animate_Stop:=SendMessage(hwnd,ACM_STOP,0,0);
    end;

  { was #define dname(params) def_expr }
  function Animate_Close(hwnd : HWND) : LRESULT;
    begin
       Animate_Close:=Animate_Open(hwnd,nil);
    end;

  { was #define dname(params) def_expr }
  function Animate_Seek(hwnd : HWND;frame : longint) : LRESULT;
    begin
       Animate_Seek:=Animate_Play(hwnd,frame,frame,1);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_AddPage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE) : LRESULT;
    begin
       PropSheet_AddPage:=SendMessage(hPropSheetDlg,PSM_ADDPAGE,0,LPARAM(hpage));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_Apply(hPropSheetDlg : HWND) : LRESULT;
    begin
       PropSheet_Apply:=SendMessage(hPropSheetDlg,PSM_APPLY,0,0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_CancelToClose(hPropSheetDlg : HWND) : LRESULT;
    begin
       PropSheet_CancelToClose:=SendMessage(hPropSheetDlg,PSM_CANCELTOCLOSE,0,0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_Changed(hPropSheetDlg,hwndPage : HWND) : LRESULT;
    begin
       PropSheet_Changed:=SendMessage(hPropSheetDlg,PSM_CHANGED,WPARAM(hwndPage),0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_GetCurrentPageHwnd(hDlg : HWND) : LRESULT;
    begin
       PropSheet_GetCurrentPageHwnd:=SendMessage(hDlg,PSM_GETCURRENTPAGEHWND,0,0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_GetTabControl(hPropSheetDlg : HWND) : LRESULT;
    begin
       PropSheet_GetTabControl:=SendMessage(hPropSheetDlg,PSM_GETTABCONTROL,0,0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_IsDialogMessage(hDlg : HWND;pMsg : longint) : LRESULT;
    begin
       PropSheet_IsDialogMessage:=SendMessage(hDlg,PSM_ISDIALOGMESSAGE,0,LPARAM(pMsg));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_PressButton(hPropSheetDlg : HWND;iButton : longint) : LRESULT;
    begin
       PropSheet_PressButton:=SendMessage(hPropSheetDlg,PSM_PRESSBUTTON,WPARAM(longint(iButton)),0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_QuerySiblings(hPropSheetDlg : HWND;param1,param2 : longint) : LRESULT;
    begin
       PropSheet_QuerySiblings:=SendMessage(hPropSheetDlg,PSM_QUERYSIBLINGS,WPARAM(param1),LPARAM(param2));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_RebootSystem(hPropSheetDlg : HWND) : LRESULT;
    begin
       PropSheet_RebootSystem:=SendMessage(hPropSheetDlg,PSM_REBOOTSYSTEM,0,0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_RemovePage(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;
    { return type might be wrong }
    begin
       PropSheet_RemovePage:=SendMessage(hPropSheetDlg,PSM_REMOVEPAGE,WPARAM(index),LPARAM(hpage));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_RestartWindows(hPropSheetDlg : HWND) : LRESULT;
    begin
       PropSheet_RestartWindows:=SendMessage(hPropSheetDlg,PSM_RESTARTWINDOWS,0,0);
    end;

  { was #define dname(params) def_expr }
  function PropSheet_SetCurSel(hPropSheetDlg : HWND;hpage : HPROPSHEETPAGE; index : longint) : LRESULT;
    begin
       PropSheet_SetCurSel:=SendMessage(hPropSheetDlg,PSM_SETCURSEL,WPARAM(index),LPARAM(hpage));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_SetCurSelByID(hPropSheetDlg : HWND; id : longint) : LRESULT;
    begin
       PropSheet_SetCurSelByID:=SendMessage(hPropSheetDlg,PSM_SETCURSELID,0,LPARAM(id));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_SetFinishText(hPropSheetDlg:HWND;lpszText : LPTSTR) : LRESULT;
    begin
       PropSheet_SetFinishText:=SendMessage(hPropSheetDlg,PSM_SETFINISHTEXT,0,LPARAM(lpszText));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_SetTitle(hPropSheetDlg:HWND;dwStyle:DWORD;lpszText : LPCTSTR) : LRESULT;
    begin
       PropSheet_SetTitle:=SendMessage(hPropSheetDlg,PSM_SETTITLE,WPARAM(dwStyle),LPARAM(lpszText));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_SetWizButtons(hPropSheetDlg:HWND;dwFlags : DWORD) : LRESULT;
    begin
       PropSheet_SetWizButtons:=SendMessage(hPropSheetDlg,PSM_SETWIZBUTTONS,0,LPARAM(dwFlags));
    end;

  { was #define dname(params) def_expr }
  function PropSheet_UnChanged(hPropSheetDlg:HWND;hwndPage : HWND) : LRESULT;
    begin
       PropSheet_UnChanged:=SendMessage(hPropSheetDlg,PSM_UNCHANGED,WPARAM(hwndPage),0);
    end;

  { was #define dname(params) def_expr }
  function Header_DeleteItem(hwndHD:HWND;index : longint) : WINBOOL;
    begin
       Header_DeleteItem:=WINBOOL(SendMessage(hwndHD,HDM_DELETEITEM,WPARAM(index),0));
    end;

  { was #define dname(params) def_expr }
  function Header_GetItem(hwndHD:HWND;index:longint;var hdi : HD_ITEM) : WINBOOL;
    begin
       Header_GetItem:=WINBOOL(SendMessage(hwndHD,HDM_GETITEM,WPARAM(index),LPARAM(@hdi)));
    end;

  { was #define dname(params) def_expr }
  function Header_GetItemCount(hwndHD : HWND) : longint;
    begin
       Header_GetItemCount:=longint(SendMessage(hwndHD,HDM_GETITEMCOUNT,0,0));
    end;

  { was #define dname(params) def_expr }
  function Header_InsertItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : longint;
    begin
       Header_InsertItem:=longint(SendMessage(hwndHD,HDM_INSERTITEM,WPARAM(index),LPARAM(@hdi)));
    end;

  { was #define dname(params) def_expr }
  function Header_Layout(hwndHD:HWND;var layout : HD_LAYOUT) : WINBOOL;
    begin
       Header_Layout:=WINBOOL(SendMessage(hwndHD,HDM_LAYOUT,0,LPARAM(@layout)));
    end;

  { was #define dname(params) def_expr }
  function Header_SetItem(hwndHD:HWND;index : longint;var hdi : HD_ITEM) : WINBOOL;
    begin
       Header_SetItem:=WINBOOL(SendMessage(hwndHD,HDM_SETITEM,WPARAM(index),LPARAM(@hdi)));
    end;

  { was #define dname(params) def_expr }
  function ListView_Arrange(hwndLV:HWND;code : UINT) : LRESULT;
    begin
       ListView_Arrange:=SendMessage(hwndLV,LVM_ARRANGE,WPARAM(UINT(code)),0);
    end;

  { was #define dname(params) def_expr }
  function ListView_CreateDragImage(hwnd:HWND;i : longint;lpptUpLeft : LPPOINT) : LRESULT;
    begin
       ListView_CreateDragImage:=SendMessage(hwnd,LVM_CREATEDRAGIMAGE,WPARAM(i),LPARAM(lpptUpLeft));
    end;

  { was #define dname(params) def_expr }
  function ListView_DeleteAllItems(hwnd : HWND) : LRESULT;
    begin
       ListView_DeleteAllItems:=SendMessage(hwnd,LVM_DELETEALLITEMS,0,0);
    end;

  { was #define dname(params) def_expr }
  function ListView_DeleteColumn(hwnd:HWND;iCol : longint) : LRESULT;
    begin
       ListView_DeleteColumn:=SendMessage(hwnd,LVM_DELETECOLUMN,WPARAM(iCol),0);
    end;

  { was #define dname(params) def_expr }
  function ListView_DeleteItem(hwnd:HWND;iItem : longint) : LRESULT;
    begin
       ListView_DeleteItem:=SendMessage(hwnd,LVM_DELETEITEM,WPARAM(iItem),0);
    end;

  { was #define dname(params) def_expr }
  function ListView_EditLabel(hwndLV:HWND;i : longint) : LRESULT;
    begin
       ListView_EditLabel:=SendMessage(hwndLV,LVM_EDITLABEL,WPARAM(longint(i)),0);
    end;

  { was #define dname(params) def_expr }
  { argument fPartialOK unclear PM }
  function ListView_EnsureVisible(hwndLV:HWND;i,fPartialOK : longint) : LRESULT;
    begin
       ListView_EnsureVisible:=SendMessage(hwndLV,LVM_ENSUREVISIBLE,WPARAM(i),MAKELPARAM(fPartialOK,0));
    end;

  { was #define dname(params) def_expr }
  function ListView_FindItem(hwnd:HWND;iStart : longint;var lvfi : LV_FINDINFO) : longint;
    begin
       ListView_FindItem:=SendMessage(hwnd,LVM_FINDITEM,WPARAM(iStart),LPARAM(@lvfi));
    end;

  { was #define dname(params) def_expr }
  function ListView_GetBkColor(hwnd : HWND) : LRESULT;
    begin
       ListView_GetBkColor:=SendMessage(hwnd,LVM_GETBKCOLOR,0,0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetCallbackMask(hwnd : HWND) : LRESULT;
    begin
       ListView_GetCallbackMask:=SendMessage(hwnd,LVM_GETCALLBACKMASK,0,0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;
    begin
       ListView_GetColumn:=SendMessage(hwnd,LVM_GETCOLUMN,WPARAM(iCol),LPARAM(@col));
    end;

  { was #define dname(params) def_expr }
  function ListView_GetColumnWidth(hwnd:HWND;iCol : longint) : LRESULT;
    begin
       ListView_GetColumnWidth:=SendMessage(hwnd,LVM_GETCOLUMNWIDTH,WPARAM(iCol),0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetCountPerPage(hwndLV : HWND) : LRESULT;
    begin
       ListView_GetCountPerPage:=SendMessage(hwndLV,LVM_GETCOUNTPERPAGE,0,0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetEditControl(hwndLV : HWND) : LRESULT;
    begin
       ListView_GetEditControl:=SendMessage(hwndLV,LVM_GETEDITCONTROL,0,0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetImageList(hwnd:HWND;iImageList : INT) : LRESULT;
    begin
       ListView_GetImageList:=SendMessage(hwnd,LVM_GETIMAGELIST,WPARAM(iImageList),0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetISearchString(hwndLV:HWND;lpsz : LPTSTR) : LRESULT;
    begin
       ListView_GetISearchString:=SendMessage(hwndLV,LVM_GETISEARCHSTRING,0,LPARAM(lpsz));
    end;

  { was #define dname(params) def_expr }
  function ListView_GetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
    begin
       ListView_GetItem:=SendMessage(hwnd,LVM_GETITEM,0,LPARAM(@item));
    end;

  { was #define dname(params) def_expr }
  function ListView_GetItemCount(hwnd : HWND) : LRESULT;
    begin
       ListView_GetItemCount:=SendMessage(hwnd,LVM_GETITEMCOUNT,0,0);
    end;

  { was #define dname(params) def_expr }
  function ListView_GetItemPosition(hwndLV:HWND;i : longint;var pt : POINT) : longint;
    begin
       ListView_GetItemPosition:=SendMessage(hwndLV,LVM_GETITEMPOSITION,WPARAM(longint(i)),LPARAM(@pt));
    end;

    { was #define dname(params) def_expr }
    { argument fSmall type unsure PM }
    function ListView_GetItemSpacing(hwndLV:HWND;fSmall : longint) : LRESULT;
      begin
         ListView_GetItemSpacing:=SendMessage(hwndLV,LVM_GETITEMSPACING,fSmall,0);
      end;

    { was #define dname(params) def_expr }
    function ListView_GetItemState(hwndLV:HWND;i,mask : longint) : LRESULT;
      begin
         ListView_GetItemState:=SendMessage(hwndLV,LVM_GETITEMSTATE,WPARAM(i),LPARAM(mask));
      end;

  {inserted manually PM }
  function ListView_GetNextItem(hwnd:HWND; iStart, flags : longint) : LRESULT;
    begin
       ListView_GetNextItem:=SendMessage(hwnd, LVM_GETNEXTITEM, WPARAM(iStart), LPARAM(flags));
    end;

    { was #define dname(params) def_expr }
    function ListView_GetOrigin(hwndLV:HWND;var pt : POINT) : LRESULT;
      begin
         ListView_GetOrigin:=SendMessage(hwndLV,LVM_GETORIGIN,WPARAM(0),LPARAM(@pt));
      end;

    { was #define dname(params) def_expr }
    function ListView_GetSelectedCount(hwndLV : HWND) : LRESULT;
      begin
         ListView_GetSelectedCount:=SendMessage(hwndLV,LVM_GETSELECTEDCOUNT,0,0);
      end;

    { was #define dname(params) def_expr }
    function ListView_GetStringWidth(hwndLV:HWND;psz : LPCTSTR) : LRESULT;
      begin
         ListView_GetStringWidth:=SendMessage(hwndLV,LVM_GETSTRINGWIDTH,0,LPARAM(psz));
      end;

    { was #define dname(params) def_expr }
    function ListView_GetTextBkColor(hwnd : HWND) : LRESULT;
      begin
         ListView_GetTextBkColor:=SendMessage(hwnd,LVM_GETTEXTBKCOLOR,0,0);
      end;

    { was #define dname(params) def_expr }
    function ListView_GetTextColor(hwnd : HWND) : LRESULT;
      begin
         ListView_GetTextColor:=SendMessage(hwnd,LVM_GETTEXTCOLOR,0,0);
      end;

    { was #define dname(params) def_expr }
    function ListView_GetTopIndex(hwndLV : HWND) : LRESULT;
      begin
         ListView_GetTopIndex:=SendMessage(hwndLV,LVM_GETTOPINDEX,0,0);
      end;

    { was #define dname(params) def_expr }
    function ListView_GetViewRect(hwnd:HWND;var rc : RECT) : LRESULT;
      begin
         ListView_GetViewRect:=SendMessage(hwnd,LVM_GETVIEWRECT,0,LPARAM(@rc));
      end;

    { was #define dname(params) def_expr }
    function ListView_HitTest(hwndLV:HWND;var info : LV_HITTESTINFO) : LRESULT;
      begin
         ListView_HitTest:=SendMessage(hwndLV,LVM_HITTEST,0,LPARAM(@info));
      end;

    { was #define dname(params) def_expr }
    function ListView_InsertColumn(hwnd:HWND;iCol : longint;var col : LV_COLUMN) : LRESULT;
      begin
         ListView_InsertColumn:=SendMessage(hwnd,LVM_INSERTCOLUMN,WPARAM(iCol),LPARAM(@col));
      end;

    { was #define dname(params) def_expr }
    function ListView_InsertItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
      begin
         ListView_InsertItem:=SendMessage(hwnd,LVM_INSERTITEM,0,LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function ListView_RedrawItems(hwndLV:HWND;iFirst,iLast : longint) : LRESULT;
      begin
         ListView_RedrawItems:=SendMessage(hwndLV,LVM_REDRAWITEMS,WPARAM(iFirst),LPARAM(iLast));
      end;

    { was #define dname(params) def_expr }
    function ListView_Scroll(hwndLV:HWND;dx,dy : longint) : LRESULT;
      begin
         ListView_Scroll:=SendMessage(hwndLV,LVM_SCROLL,WPARAM(dx),LPARAM(dy));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetBkColor(hwnd:HWND;clrBk : COLORREF) : LRESULT;
      begin
         ListView_SetBkColor:=SendMessage(hwnd,LVM_SETBKCOLOR,0,LPARAM(clrBk));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetCallbackMask(hwnd:HWND;mask : UINT) : LRESULT;
      begin
         ListView_SetCallbackMask:=SendMessage(hwnd,LVM_SETCALLBACKMASK,WPARAM(mask),0);
      end;

    { was #define dname(params) def_expr }
    function ListView_SetColumn(hwnd:HWND;iCol : longint; var col : LV_COLUMN) : LRESULT;
      begin
         ListView_SetColumn:=SendMessage(hwnd,LVM_SETCOLUMN,WPARAM(iCol),LPARAM(@col));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetColumnWidth(hwnd:HWND;iCol,cx : longint) : LRESULT;
      begin
         ListView_SetColumnWidth:=SendMessage(hwnd,LVM_SETCOLUMNWIDTH,WPARAM(iCol),MAKELPARAM(cx,0));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetImageList(hwnd:HWND;himl : longint;iImageList : HIMAGELIST) : LRESULT;
      begin
         ListView_SetImageList:=SendMessage(hwnd,LVM_SETIMAGELIST,WPARAM(iImageList),LPARAM(UINT(himl)));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetItem(hwnd:HWND;var item : LV_ITEM) : LRESULT;
      begin
         ListView_SetItem:=SendMessage(hwnd,LVM_SETITEM,0,LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetItemCount(hwndLV:HWND;cItems : longint) : LRESULT;
      begin
         ListView_SetItemCount:=SendMessage(hwndLV,LVM_SETITEMCOUNT,WPARAM(cItems),0);
      end;

    { was #define dname(params) def_expr }
    { argument types are unknown }
    { return type might be wrong }
    function ListView_SetItemPosition(hwndLV:HWND;i,x,y : longint) : LRESULT;
      { return type might be wrong }
      begin
         ListView_SetItemPosition:=SendMessage(hwndLV,LVM_SETITEMPOSITION,WPARAM(i),MAKELPARAM(x,y));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetItemPosition32(hwndLV:HWND;i,x,y : longint) : LRESULT;
      var ptNewPos : POINT;
      begin
         ptNewPos.x:=x;
         ptNewPos.y:=y;
         ListView_SetItemPosition32:=SendMessage(hwndLV, LVM_SETITEMPOSITION32, WPARAM(i),LPARAM(@ptNewPos));
      end;
    function ListView_SetItemState(hwndLV:HWND; i, data, mask:longint) : LRESULT;
      var _gnu_lvi : LV_ITEM;
      begin
         _gnu_lvi.stateMask:=mask;
         _gnu_lvi.state:=data;
         ListView_SetItemState:=SendMessage(hwndLV, LVM_SETITEMSTATE, WPARAM(i),
              LPARAM(@_gnu_lvi));
      end;

(* error
#define ListView_SetItemState(hwndLV, i, data, mask) \
{ LV_ITEM _gnu_lvi;\
  _gnu_lvi.stateMask = mask;\
  _gnu_lvi.state = data;\
  SendMessage((hwndLV), LVM_SETITEMSTATE, (WPARAM)i, \
              (LPARAM)(LV_ITEM * )&_gnu_lvi);\
}
in declaration at line 6817
 error *)

    function ListView_SetItemText(hwndLV:HWND; i, iSubItem_:longint;pszText_ : LPTSTR) : LRESULT;
      var _gnu_lvi : LV_ITEM;
      begin
        _gnu_lvi.iSubItem:=iSubItem_;
        _gnu_lvi.pszText:=pszText_;
         ListView_SetItemText:=SendMessage(hwndLV, LVM_SETITEMTEXT, WPARAM(i),
              LPARAM(@_gnu_lvi));
      end;
(* error
#define ListView_SetItemText(hwndLV, i, iSubItem_, pszText_) \
{ LV_ITEM _gnu_lvi;\
  _gnu_lvi.iSubItem = iSubItem_;\
  _gnu_lvi.pszText = pszText_;\
  SendMessage((hwndLV), LVM_SETITEMTEXT, (WPARAM)i, \
              (LPARAM)(LV_ITEM * )&_gnu_lvi);\
}
in define line 6826 *)

    { was #define dname(params) def_expr }
    function ListView_SetTextBkColor(hwnd:HWND;clrTextBk : COLORREF) : LRESULT;
      begin
         ListView_SetTextBkColor:=SendMessage(hwnd,LVM_SETTEXTBKCOLOR,0,LPARAM(clrTextBk));
      end;

    { was #define dname(params) def_expr }
    function ListView_SetTextColor(hwnd:HWND;clrText : COLORREF) : LRESULT;
      begin
         ListView_SetTextColor:=SendMessage(hwnd,LVM_SETTEXTCOLOR,0,LPARAM(clrText));
      end;

    { was #define dname(params) def_expr }
    function ListView_SortItems(hwndLV:HWND;_pfnCompare:PFNLVCOMPARE;_lPrm : LPARAM) : LRESULT;
      begin
         ListView_SortItems:=SendMessage(hwndLV,LVM_SORTITEMS,WPARAM(_lPrm),LPARAM(_pfnCompare));
      end;

    { was #define dname(params) def_expr }
    function ListView_Update(hwndLV:HWND;i : longint) : LRESULT;
      begin
         ListView_Update:=SendMessage(hwndLV,LVM_UPDATE,WPARAM(i),0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_InsertItem(hwnd:HWND;lpis : LPTV_INSERTSTRUCT) : LRESULT;
      begin
         TreeView_InsertItem:=SendMessage(hwnd,TVM_INSERTITEM,0,LPARAM(lpis));
      end;

    { was #define dname(params) def_expr }
    function TreeView_DeleteItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_DeleteItem:=SendMessage(hwnd,TVM_DELETEITEM,0,LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_DeleteAllItems(hwnd : HWND) : LRESULT;
      begin
         TreeView_DeleteAllItems:=SendMessage(hwnd,TVM_DELETEITEM,0,LPARAM(TVI_ROOT));
      end;

    { was #define dname(params) def_expr }
    function TreeView_Expand(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
      begin
         TreeView_Expand:=SendMessage(hwnd,TVM_EXPAND,WPARAM(code),LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetCount(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetCount:=SendMessage(hwnd,TVM_GETCOUNT,0,0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetIndent(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetIndent:=SendMessage(hwnd,TVM_GETINDENT,0,0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_SetIndent(hwnd:HWND;indent : longint) : LRESULT;
      begin
         TreeView_SetIndent:=SendMessage(hwnd,TVM_SETINDENT,WPARAM(indent),0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetImageList(hwnd:HWND;iImage : WPARAM) : LRESULT;
      begin
         TreeView_GetImageList:=SendMessage(hwnd,TVM_GETIMAGELIST,iImage,0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_SetImageList(hwnd:HWND;himl:HIMAGELIST;iImage : WPARAM) : LRESULT;
      begin
         TreeView_SetImageList:=SendMessage(hwnd,TVM_SETIMAGELIST,iImage,LPARAM(UINT(himl)));
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetNextItem(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
      begin
         TreeView_GetNextItem:=SendMessage(hwnd,TVM_GETNEXTITEM,WPARAM(code),LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetChild(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_GetChild:=TreeView_GetNextItem(hwnd,hitem,TVGN_CHILD);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetNextSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_GetNextSibling:=TreeView_GetNextItem(hwnd,hitem,TVGN_NEXT);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetPrevSibling(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_GetPrevSibling:=TreeView_GetNextItem(hwnd,hitem,TVGN_PREVIOUS);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetParent(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_GetParent:=TreeView_GetNextItem(hwnd,hitem,TVGN_PARENT);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetFirstVisible(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetFirstVisible:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_FIRSTVISIBLE);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetNextVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_GetNextVisible:=TreeView_GetNextItem(hwnd,hitem,TVGN_NEXTVISIBLE);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetPrevVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_GetPrevVisible:=TreeView_GetNextItem(hwnd,hitem,TVGN_PREVIOUSVISIBLE);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetSelection(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetSelection:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_CARET);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetDropHilight(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetDropHilight:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_DROPHILITE);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetRoot(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetRoot:=TreeView_GetNextItem(hwnd,HTREEITEM(nil),TVGN_ROOT);
      end;

    { was #define dname(params) def_expr }
    function TreeView_Select(hwnd:HWND;hitem:HTREEITEM;code : longint) : LRESULT;
      begin
         TreeView_Select:=SendMessage(hwnd,TVM_SELECTITEM,WPARAM(code),LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_SelectItem(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_SelectItem:=TreeView_Select(hwnd,hitem,TVGN_CARET);
      end;

    { was #define dname(params) def_expr }
    function TreeView_SelectDropTarget(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_SelectDropTarget:=TreeView_Select(hwnd,hitem,TVGN_DROPHILITE);
      end;

    { was #define dname(params) def_expr }
    function TreeView_SelectSetFirstVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_SelectSetFirstVisible:=TreeView_Select(hwnd,hitem,TVGN_FIRSTVISIBLE);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;
      begin
         TreeView_GetItem:=SendMessage(hwnd,TVM_GETITEM,0,LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function TreeView_SetItem(hwnd:HWND;var item : TV_ITEM) : LRESULT;
      begin
         TreeView_SetItem:=SendMessage(hwnd,TVM_SETITEM,0,LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function TreeView_EditLabel(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_EditLabel:=SendMessage(hwnd,TVM_EDITLABEL,0,LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetEditControl(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetEditControl:=SendMessage(hwnd,TVM_GETEDITCONTROL,0,0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetVisibleCount(hwnd : HWND) : LRESULT;
      begin
         TreeView_GetVisibleCount:=SendMessage(hwnd,TVM_GETVISIBLECOUNT,0,0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_HitTest(hwnd:HWND;lpht : LPTV_HITTESTINFO) : LRESULT;
      begin
         TreeView_HitTest:=SendMessage(hwnd,TVM_HITTEST,0,LPARAM(lpht));
      end;

    { was #define dname(params) def_expr }
    function TreeView_CreateDragImage(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_CreateDragImage:=SendMessage(hwnd,TVM_CREATEDRAGIMAGE,0,LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_SortChildren(hwnd:HWND;hitem:HTREEITEM;recurse : longint) : LRESULT;
      begin
         TreeView_SortChildren:=SendMessage(hwnd,TVM_SORTCHILDREN,WPARAM(recurse),LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_EnsureVisible(hwnd:HWND;hitem : HTREEITEM) : LRESULT;
      begin
         TreeView_EnsureVisible:=SendMessage(hwnd,TVM_ENSUREVISIBLE,0,LPARAM(hitem));
      end;

    { was #define dname(params) def_expr }
    function TreeView_SortChildrenCB(hwnd:HWND;psort:LPTV_SORTCB;recurse : longint) : LRESULT;
      begin
         TreeView_SortChildrenCB:=SendMessage(hwnd,TVM_SORTCHILDRENCB,WPARAM(recurse),LPARAM(psort));
      end;

    { was #define dname(params) def_expr }
    function TreeView_EndEditLabelNow(hwnd:HWND;fCancel : longint) : LRESULT;
      begin
         TreeView_EndEditLabelNow:=SendMessage(hwnd,TVM_ENDEDITLABELNOW,WPARAM(fCancel),0);
      end;

    { was #define dname(params) def_expr }
    function TreeView_GetISearchString(hwndTV:HWND;lpsz : LPTSTR) : LRESULT;
      begin
         TreeView_GetISearchString:=SendMessage(hwndTV,TVM_GETISEARCHSTRING,0,LPARAM(lpsz));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetImageList(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_GetImageList:=SendMessage(hwnd,TCM_GETIMAGELIST,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetImageList(hwnd:HWND;himl : HIMAGELIST) : LRESULT;
      begin
         TabCtrl_SetImageList:=SendMessage(hwnd,TCM_SETIMAGELIST,0,LPARAM(UINT(himl)));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetItemCount(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_GetItemCount:=SendMessage(hwnd,TCM_GETITEMCOUNT,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
      begin
         TabCtrl_GetItem:=SendMessage(hwnd,TCM_GETITEM,WPARAM(iItem),LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
      begin
         TabCtrl_SetItem:=SendMessage(hwnd,TCM_SETITEM,WPARAM(iItem),LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_InsertItem(hwnd:HWND;iItem : longint;var item : TC_ITEM) : LRESULT;
      begin
         TabCtrl_InsertItem:=SendMessage(hwnd,TCM_INSERTITEM,WPARAM(iItem),LPARAM(@item));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_DeleteItem(hwnd:HWND;i : longint) : LRESULT;
      begin
         TabCtrl_DeleteItem:=SendMessage(hwnd,TCM_DELETEITEM,WPARAM(i),0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_DeleteAllItems(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_DeleteAllItems:=SendMessage(hwnd,TCM_DELETEALLITEMS,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetItemRect(hwnd:HWND;i : longint;var rc : RECT) : LRESULT;
      begin
         TabCtrl_GetItemRect:=SendMessage(hwnd,TCM_GETITEMRECT,WPARAM(longint(i)),LPARAM(@rc));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetCurSel(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_GetCurSel:=SendMessage(hwnd,TCM_GETCURSEL,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetCurSel(hwnd:HWND;i : longint) : LRESULT;
      begin
         TabCtrl_SetCurSel:=SendMessage(hwnd,TCM_SETCURSEL,WPARAM(i),0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_HitTest(hwndTC:HWND;var info : TC_HITTESTINFO) : LRESULT;
      begin
         TabCtrl_HitTest:=SendMessage(hwndTC,TCM_HITTEST,0,LPARAM(@info));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetItemExtra(hwndTC:HWND;cb : longint) : LRESULT;
      begin
         TabCtrl_SetItemExtra:=SendMessage(hwndTC,TCM_SETITEMEXTRA,WPARAM(cb),0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_AdjustRect(hwnd:HWND;bLarger:WINBOOL;var rc : RECT) : LRESULT;
      begin
         TabCtrl_AdjustRect:=SendMessage(hwnd,TCM_ADJUSTRECT,WPARAM(bLarger),LPARAM(@rc));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetItemSize(hwnd:HWND;x,y : longint) : LRESULT;
      begin
         TabCtrl_SetItemSize:=SendMessage(hwnd,TCM_SETITEMSIZE,0,MAKELPARAM(x,y));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_RemoveImage(hwnd:HWND;i : WPARAM) : LRESULT;
      begin
         TabCtrl_RemoveImage:=SendMessage(hwnd,TCM_REMOVEIMAGE,i,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetPadding(hwnd:HWND;cx,cy : longint) : LRESULT;
      begin
         TabCtrl_SetPadding:=SendMessage(hwnd,TCM_SETPADDING,0,MAKELPARAM(cx,cy));
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetRowCount(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_GetRowCount:=SendMessage(hwnd,TCM_GETROWCOUNT,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetToolTips(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_GetToolTips:=SendMessage(hwnd,TCM_GETTOOLTIPS,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetToolTips(hwnd:HWND;hwndTT : longint) : LRESULT;
      begin
         TabCtrl_SetToolTips:=SendMessage(hwnd,TCM_SETTOOLTIPS,WPARAM(hwndTT),0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_GetCurFocus(hwnd : HWND) : LRESULT;
      begin
         TabCtrl_GetCurFocus:=SendMessage(hwnd,TCM_GETCURFOCUS,0,0);
      end;

    { was #define dname(params) def_expr }
    function TabCtrl_SetCurFocus(hwnd:HWND;i : longint) : LRESULT;
      begin
         TabCtrl_SetCurFocus:=SendMessage(hwnd,TCM_SETCURFOCUS,i,0);
      end;

      { added by hand not found in C headers PM }
  function SNDMSG(hWnd:HWND; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT;
    begin
       SNDMSG:=SendMessage(hWnd,Msg,wParam,lParam);
    end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetSpecA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetSpecA:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
      end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetSpecW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetSpecW:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
      end;

{$ifndef Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetSpec:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
      end;
{$else Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetSpec(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetSpec:=SNDMSG(_hdlg,CDM_GETSPEC,WPARAM(_cbmax),LPARAM(_psz));
      end;
{$endif Unicode}

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFilePathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFilePathA:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
      end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFilePathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFilePathW:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
      end;

{$ifndef Unicode}
    function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFilePath:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
      end;
{$else Unicode}
    function CommDlg_OpenSave_GetFilePath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFilePath:=SNDMSG(_hdlg,CDM_GETFILEPATH,WPARAM(_cbmax),LPARAM(_psz));
      end;
{$endif Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFolderPathA(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFolderPathA:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPSTR(_psz)));
      end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFolderPathW(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFolderPathW:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
      end;

{$ifndef Unicode}
    function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFolderPath:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPSTR(_psz)));
      end;
{$else Unicode}
    function CommDlg_OpenSave_GetFolderPath(_hdlg:HWND;_psz:LPWSTR;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFolderPath:=SNDMSG(_hdlg,CDM_GETFOLDERPATH,WPARAM(_cbmax),LPARAM(LPWSTR(_psz)));
      end;
{$endif Unicode}
    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_GetFolderIDList(_hdlg:HWND;_pidl:LPVOID;_cbmax : longint) : LRESULT;
      begin
         CommDlg_OpenSave_GetFolderIDList:=SNDMSG(_hdlg,CDM_GETFOLDERIDLIST,WPARAM(_cbmax),LPARAM(_pidl));
      end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_SetControlText(_hdlg:HWND;_id : longint;_text : LPSTR) : LRESULT;
      begin
         CommDlg_OpenSave_SetControlText:=SNDMSG(_hdlg,CDM_SETCONTROLTEXT,WPARAM(_id),LPARAM(_text));
      end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_HideControl(_hdlg:HWND;_id : longint) : LRESULT;
      begin
         CommDlg_OpenSave_HideControl:=SNDMSG(_hdlg,CDM_HIDECONTROL,WPARAM(_id),0);
      end;

    { was #define dname(params) def_expr }
    function CommDlg_OpenSave_SetDefExt(_hdlg:HWND;_pszext : LPSTR) : LRESULT;
      begin
         CommDlg_OpenSave_SetDefExt:=SNDMSG(_hdlg,CDM_SETDEFEXT,0,LPARAM(_pszext));
      end;

    function RegCloseKey(hKey:HKEY):LONG; external 'advapi32' name 'RegCloseKey';

    function RegSetKeySecurity(hKey:HKEY; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR):LONG; external 'advapi32' name 'RegSetKeySecurity';

    function RegFlushKey(hKey:HKEY):LONG; external 'advapi32' name 'RegFlushKey';

    function RegGetKeySecurity(hKey:HKEY; SecurityInformation:SECURITY_INFORMATION; pSecurityDescriptor:PSECURITY_DESCRIPTOR; lpcbSecurityDescriptor:LPDWORD):LONG; external 'advapi32' name 'RegGetKeySecurity';

    function RegNotifyChangeKeyValue(hKey:HKEY; bWatchSubtree:WINBOOL; dwNotifyFilter:DWORD; hEvent:HANDLE; fAsynchronus:WINBOOL):LONG; external 'advapi32' name 'RegNotifyChangeKeyValue';

    function IsValidCodePage(CodePage:UINT):WINBOOL; external 'kernel32' name 'IsValidCodePage';

    function GetACP:UINT; external 'kernel32' name 'GetACP';

    function GetOEMCP:UINT; external 'kernel32' name 'GetOEMCP';

    function GetCPInfo(_para1:UINT; _para2:LPCPINFO):WINBOOL; external 'kernel32' name 'GetCPInfo';

    function IsDBCSLeadByte(TestChar:BYTE):WINBOOL; external 'kernel32' name 'IsDBCSLeadByte';

    function IsDBCSLeadByteEx(CodePage:UINT; TestChar:BYTE):WINBOOL; external 'kernel32' name 'IsDBCSLeadByteEx';

    function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:LPCSTR; cchMultiByte:longint; lpWideCharStr:LPWSTR;
               cchWideChar:longint):longint; external 'kernel32' name 'MultiByteToWideChar';

    function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:LPCWSTR; cchWideChar:longint; lpMultiByteStr:LPSTR;
               cchMultiByte:longint; lpDefaultChar:LPCSTR; lpUsedDefaultChar:LPBOOL):longint; external 'kernel32' name 'WideCharToMultiByte';

    function IsValidLocale(Locale:LCID; dwFlags:DWORD):WINBOOL; external 'kernel32' name 'IsValidLocale';

    function ConvertDefaultLocale(Locale:LCID):LCID; external 'kernel32' name 'ConvertDefaultLocale';

    function GetThreadLocale:LCID; external 'kernel32' name 'GetThreadLocale';

    function SetThreadLocale(Locale:LCID):WINBOOL; external 'kernel32' name 'SetThreadLocale';

    function GetSystemDefaultLangID:LANGID; external 'kernel32' name 'GetSystemDefaultLangID';

    function GetUserDefaultLangID:LANGID; external 'kernel32' name 'GetUserDefaultLangID';

    function GetSystemDefaultLCID:LCID; external 'kernel32' name 'GetSystemDefaultLCID';

    function GetUserDefaultLCID:LCID; external 'kernel32' name 'GetUserDefaultLCID';

    function ReadConsoleOutputAttribute(hConsoleOutput:HANDLE; lpAttribute:LPWORD; nLength:DWORD; dwReadCoord:COORD; lpNumberOfAttrsRead:LPDWORD):WINBOOL; external 'kernel32' name 'ReadConsoleOutputAttribute';

    function WriteConsoleOutputAttribute(hConsoleOutput:HANDLE; var lpAttribute:WORD; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfAttrsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'WriteConsoleOutputAttribute';

    function FillConsoleOutputAttribute(hConsoleOutput:HANDLE; wAttribute:WORD; nLength:DWORD; dwWriteCoord:COORD; lpNumberOfAttrsWritten:LPDWORD):WINBOOL; external 'kernel32' name 'FillConsoleOutputAttribute';

    function GetConsoleMode(hConsoleHandle:HANDLE; lpMode:LPDWORD):WINBOOL; external 'kernel32' name 'GetConsoleMode';

    function GetNumberOfConsoleInputEvents(hConsoleInput:HANDLE; var lpNumberOfEvents:DWORD):WINBOOL; external 'kernel32' name 'GetNumberOfConsoleInputEvents';

    function GetConsoleScreenBufferInfo(hConsoleOutput:HANDLE; lpConsoleScreenBufferInfo:PCONSOLE_SCREEN_BUFFER_INFO):WINBOOL; external 'kernel32' name 'GetConsoleScreenBufferInfo';
    function GetConsoleScreenBufferInfo(hConsoleOutput:HANDLE; var lpConsoleScreenBufferInfo: CONSOLE_SCREEN_BUFFER_INFO):WINBOOL; external 'kernel32' name 'GetConsoleScreenBufferInfo';

    function GetLargestConsoleWindowSize(hConsoleOutput:HANDLE):COORD; external 'kernel32' name 'GetLargestConsoleWindowSize';

    function GetConsoleCursorInfo(hConsoleOutput:HANDLE; lpConsoleCursorInfo:PCONSOLE_CURSOR_INFO):WINBOOL; external 'kernel32' name 'GetConsoleCursorInfo';
    function GetConsoleCursorInfo(hConsoleOutput:HANDLE; var lpConsoleCursorInfo:CONSOLE_CURSOR_INFO):WINBOOL; external 'kernel32' name 'GetConsoleCursorInfo';

    function GetNumberOfConsoleMouseButtons(lpNumberOfMouseButtons:LPDWORD):WINBOOL; external 'kernel32' name 'GetNumberOfConsoleMouseButtons';

    function SetConsoleMode(hConsoleHandle:HANDLE; dwMode:DWORD):WINBOOL; external 'kernel32' name 'SetConsoleMode';

    function SetConsoleActiveScreenBuffer(hConsoleOutput:HANDLE):WINBOOL; external 'kernel32' name 'SetConsoleActiveScreenBuffer';

    function FlushConsoleInputBuffer(hConsoleInput:HANDLE):WINBOOL; external 'kernel32' name 'FlushConsoleInputBuffer';

    function SetConsoleScreenBufferSize(hConsoleOutput:HANDLE; dwSize:COORD):WINBOOL; external 'kernel32' name 'SetConsoleScreenBufferSize';

    function SetConsoleCursorPosition(hConsoleOutput:HANDLE; dwCursorPosition:COORD):WINBOOL; external 'kernel32' name 'SetConsoleCursorPosition';

    function SetConsoleCursorInfo(hConsoleOutput:HANDLE; var lpConsoleCursorInfo:CONSOLE_CURSOR_INFO):WINBOOL; external 'kernel32' name 'SetConsoleCursorInfo';

    function SetConsoleWindowInfo(hConsoleOutput:HANDLE; bAbsolute:WINBOOL; var lpConsoleWindow:SMALL_RECT):WINBOOL; external 'kernel32' name 'SetConsoleWindowInfo';

    function SetConsoleTextAttribute(hConsoleOutput:HANDLE; wAttributes:WORD):WINBOOL; external 'kernel32' name 'SetConsoleTextAttribute';

    function SetConsoleCtrlHandler(HandlerRoutine:PHANDLER_ROUTINE; Add:WINBOOL):WINBOOL; external 'kernel32' name 'SetConsoleCtrlHandler';

    function GenerateConsoleCtrlEvent(dwCtrlEvent:DWORD; dwProcessGroupId:DWORD):WINBOOL; external 'kernel32' name 'GenerateConsoleCtrlEvent';

    function AllocConsole:WINBOOL; external 'kernel32' name 'AllocConsole';

    function FreeConsole:WINBOOL; external 'kernel32' name 'FreeConsole';

    function CreateConsoleScreenBuffer(dwDesiredAccess:DWORD; dwShareMode:DWORD; var lpSecurityAttributes:SECURITY_ATTRIBUTES; dwFlags:DWORD; lpScreenBufferData:LPVOID):HANDLE; external 'kernel32' name 'CreateConsoleScreenBuffer';

    function GetConsoleCP:UINT; external 'kernel32' name 'GetConsoleCP';

    function SetConsoleCP(wCodePageID:UINT):WINBOOL; external 'kernel32' name 'SetConsoleCP';

    function GetConsoleOutputCP:UINT; external 'kernel32' name 'GetConsoleOutputCP';

    function SetConsoleOutputCP(wCodePageID:UINT):WINBOOL; external 'kernel32' name 'SetConsoleOutputCP';

    function WNetConnectionDialog(hwnd:HWND; dwType:DWORD):DWORD; external 'mpr' name 'WNetConnectionDialog';

    function WNetDisconnectDialog(hwnd:HWND; dwType:DWORD):DWORD; external 'mpr' name 'WNetDisconnectDialog';

    function WNetCloseEnum(hEnum:HANDLE):DWORD; external 'mpr' name 'WNetCloseEnum';

    function CloseServiceHandle(hSCObject:SC_HANDLE):WINBOOL; external 'advapi32' name 'CloseServiceHandle';

    function ControlService(hService:SC_HANDLE; dwControl:DWORD; lpServiceStatus:LPSERVICE_STATUS):WINBOOL; external 'advapi32' name 'ControlService';

    function DeleteService(hService:SC_HANDLE):WINBOOL; external 'advapi32' name 'DeleteService';

    function LockServiceDatabase(hSCManager:SC_HANDLE):SC_LOCK; external 'advapi32' name 'LockServiceDatabase';

    function NotifyBootConfigStatus(BootAcceptable:WINBOOL):WINBOOL; external 'advapi32' name 'NotifyBootConfigStatus';

    function QueryServiceObjectSecurity(hService:SC_HANDLE; dwSecurityInformation:SECURITY_INFORMATION; lpSecurityDescriptor:PSECURITY_DESCRIPTOR; cbBufSize:DWORD; pcbBytesNeeded:LPDWORD):WINBOOL;
               external 'advapi32' name 'QueryServiceObjectSecurity';

    function QueryServiceStatus(hService:SC_HANDLE; lpServiceStatus:LPSERVICE_STATUS):WINBOOL; external 'advapi32' name 'QueryServiceStatus';

    function SetServiceObjectSecurity(hService:SC_HANDLE; dwSecurityInformation:SECURITY_INFORMATION; lpSecurityDescriptor:PSECURITY_DESCRIPTOR):WINBOOL;
               external 'advapi32' name 'SetServiceObjectSecurity';

    function SetServiceStatus(hServiceStatus:SERVICE_STATUS_HANDLE; lpServiceStatus:LPSERVICE_STATUS):WINBOOL; external 'advapi32' name 'SetServiceStatus';

    function UnlockServiceDatabase(ScLock:SC_LOCK):WINBOOL; external 'advapi32' name 'UnlockServiceDatabase';

    function ChoosePixelFormat(_para1:HDC; var _para2:PIXELFORMATDESCRIPTOR):longint; external 'gdi32' name 'ChoosePixelFormat';

    function DescribePixelFormat(_para1:HDC; _para2:longint; _para3:UINT; _para4:LPPIXELFORMATDESCRIPTOR):longint; external 'gdi32' name 'DescribePixelFormat';

{$ifdef Unknown_functions}
{ WARNING: function is not in my gdi32.dll !! PM}
    function GetEnhMetaFilePixelFormat(_para1:HENHMETAFILE; _para2:DWORD; var _para3:PIXELFORMATDESCRIPTOR):UINT; external 'gdi32' name 'GetEnhMetaFilePixelFormat';
{$endif Unknown_functions}

{    function GetPixelFormat(_para1:HDC):longint; external 'gdi32' name 'GetPixelFormat'; }

    function SetPixelFormat(_para1:HDC; _para2:longint;

      _para3:PPIXELFORMATDESCRIPTOR):WINBOOL; external 'gdi32' name 'SetPixelFormat';

    function SwapBuffers(_para1:HDC):WINBOOL; external 'gdi32' name 'SwapBuffers';

    function DragQueryPoint(_para1:HDROP; _para2:LPPOINT):WINBOOL; external 'shell32' name 'DragQueryPoint';

    procedure DragFinish(_para1:HDROP); external 'shell32' name 'DragFinish';

    procedure DragAcceptFiles(_para1:HWND; _para2:WINBOOL); external 'shell32' name 'DragAcceptFiles';

    function DuplicateIcon(_para1:HINST; _para2:HICON):HICON; external 'shell32' name 'DuplicateIcon';

    function DdeConnect(_para1:DWORD; _para2:HSZ; _para3:HSZ; var _para4:CONVCONTEXT):HCONV; external 'user32' name 'DdeConnect';

    function DdeDisconnect(_para1:HCONV):WINBOOL; external 'user32' name 'DdeDisconnect';

    function DdeFreeDataHandle(_para1:HDDEDATA):WINBOOL; external 'user32' name 'DdeFreeDataHandle';

    function DdeGetData(_para1:HDDEDATA; var _para2:BYTE; _para3:DWORD; _para4:DWORD):DWORD; external 'user32' name 'DdeGetData';

    function DdeGetLastError(_para1:DWORD):UINT; external 'user32' name 'DdeGetLastError';

    function DdeNameService(_para1:DWORD; _para2:HSZ; _para3:HSZ; _para4:UINT):HDDEDATA; external 'user32' name 'DdeNameService';

    function DdePostAdvise(_para1:DWORD; _para2:HSZ; _para3:HSZ):WINBOOL; external 'user32' name 'DdePostAdvise';

    function DdeReconnect(_para1:HCONV):HCONV; external 'user32' name 'DdeReconnect';

    function DdeUninitialize(_para1:DWORD):WINBOOL; external 'user32' name 'DdeUninitialize';

    function DdeCmpStringHandles(_para1:HSZ; _para2:HSZ):longint; external 'user32' name 'DdeCmpStringHandles';

    function DdeCreateDataHandle(_para1:DWORD; _para2:LPBYTE; _para3:DWORD; _para4:DWORD; _para5:HSZ;
               _para6:UINT; _para7:UINT):HDDEDATA; external 'user32' name 'DdeCreateDataHandle';

{$ifdef Unknown_functions}
    function NetUserEnum(_para1:LPWSTR; _para2:DWORD; _para3:DWORD; var _para4:LPBYTE; _para5:DWORD;
               _para6:LPDWORD; _para7:LPDWORD; _para8:LPDWORD):DWORD; external 'netapi32' name 'NetUserEnum';

    function NetApiBufferFree(_para1:LPVOID):DWORD; external 'netapi32' name 'NetApiBufferFree';

    function NetUserGetInfo(_para1:LPWSTR; _para2:LPWSTR; _para3:DWORD; _para4:LPBYTE):DWORD; external 'netapi32' name 'NetUserGetInfo';

    function NetGetDCName(_para1:LPWSTR; _para2:LPWSTR; var _para3:LPBYTE):DWORD; external 'netapi32' name 'NetGetDCName';

    function NetGroupEnum(_para1:LPWSTR; _para2:DWORD; var _para3:LPBYTE; _para4:DWORD; _para5:LPDWORD;
               _para6:LPDWORD; _para7:LPDWORD):DWORD; external 'netapi32' name 'NetGroupEnum';

    function NetLocalGroupEnum(_para1:LPWSTR; _para2:DWORD; var _para3:LPBYTE; _para4:DWORD; _para5:LPDWORD;
               _para6:LPDWORD; _para7:LPDWORD):DWORD; external 'netapi32' name 'NetLocalGroupEnum';
{$endif Unknown_functions}

    procedure SHAddToRecentDocs(_para1:UINT; _para2:LPCVOID); external 'shell32' name 'SHAddToRecentDocs';

    function SHBrowseForFolder(_para1:LPBROWSEINFO):LPITEMIDLIST; external 'shell32' name 'SHBrowseForFolder';

    procedure SHChangeNotify(_para1:LONG; _para2:UINT; _para3:LPCVOID; _para4:LPCVOID); external 'shell32' name 'SHChangeNotify';

    function SHFileOperation(_para1:LPSHFILEOPSTRUCT):longint; external 'shell32' name 'SHFileOperation';

    procedure SHFreeNameMappings(_para1:HANDLE); external 'shell32' name 'SHFreeNameMappings';

    function SHGetFileInfo(_para1:LPCTSTR; _para2:DWORD; var _para3:SHFILEINFO; _para4:UINT; _para5:UINT):DWORD; external 'shell32' name 'SHGetFileInfo';

    function SHGetPathFromIDList(_para1:LPCITEMIDLIST; _para2:LPTSTR):WINBOOL; external 'shell32' name 'SHGetPathFromIDList';

    function SHGetSpecialFolderLocation(_para1:HWND; _para2:longint; var _para3:LPITEMIDLIST):HRESULT; external 'shell32' name 'SHGetSpecialFolderLocation';

{$endif read_implementation}

{$ifndef windows_include_files}
end.
{$endif not windows_include_files}
{
  $Log$
  Revision 1.5  2000-02-09 16:59:35  peter
    * truncated log

  Revision 1.4  2000/01/07 16:41:55  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:36  daniel
    * copyright 2000 added

  Revision 1.2  1999/10/26 09:38:51  pierre
   + some Delphi overload added

  Revision 1.1  1999/09/16 13:38:22  peter
    * windows unit include moved to wininc/

}
