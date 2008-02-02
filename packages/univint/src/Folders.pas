{
     File:       Folders.p
 
     Contains:   Folder Manager Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1995-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit Folders;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,MixedMode,Files;


{$ALIGN MAC68K}


const
	kOnSystemDisk				= -32768;						{  previously was 0x8000 but that is an unsigned value whereas vRefNum is signed }
	kOnAppropriateDisk			= -32767;						{  Generally, the same as kOnSystemDisk, but it's clearer that this isn't always the 'boot' disk. }
																{  Folder Domains - Carbon only.  The constants above can continue to be used, but the folder/volume returned will }
																{  be from one of the domains below. }
	kSystemDomain				= -32766;						{  Read-only system hierarchy. }
	kLocalDomain				= -32765;						{  All users of a single machine have access to these resources. }
	kNetworkDomain				= -32764;						{  All users configured to use a common network server has access to these resources. }
	kUserDomain					= -32763;						{  Read/write. Resources that are private to the user. }
	kClassicDomain				= -32762;						{  Domain referring to the currently configured Classic System Folder }

	kCreateFolder				= true;
	kDontCreateFolder			= false;

	kSystemFolderType			= $6D616373 (* 'macs' *);						{  the system folder  }
	kDesktopFolderType			= $6465736B (* 'desk' *);						{  the desktop folder; objects in this folder show on the desk top.  }
	kSystemDesktopFolderType	= $7364736B (* 'sdsk' *);						{  the desktop folder at the root of the hard drive, never the redirected user desktop folder  }
	kTrashFolderType			= $74727368 (* 'trsh' *);						{  the trash folder; objects in this folder show up in the trash  }
	kSystemTrashFolderType		= $73747273 (* 'strs' *);						{  the trash folder at the root of the drive, never the redirected user trash folder  }
	kWhereToEmptyTrashFolderType = $656D7074 (* 'empt' *);						{  the "empty trash" folder; Finder starts empty from here down  }
	kPrintMonitorDocsFolderType	= $70726E74 (* 'prnt' *);						{  Print Monitor documents  }
	kStartupFolderType			= $73747274 (* 'strt' *);						{  Finder objects (applications, documents, DAs, aliases, to...) to open at startup go here  }
	kShutdownFolderType			= $73686466 (* 'shdf' *);						{  Finder objects (applications, documents, DAs, aliases, to...) to open at shutdown go here  }
	kAppleMenuFolderType		= $616D6E75 (* 'amnu' *);						{  Finder objects to put into the Apple menu go here  }
	kControlPanelFolderType		= $6374726C (* 'ctrl' *);						{  Control Panels go here (may contain INITs)  }
	kSystemControlPanelFolderType = $7363746C (* 'sctl' *);						{  System control panels folder - never the redirected one, always "Control Panels" inside the System Folder  }
	kExtensionFolderType		= $6578746E (* 'extn' *);						{  System extensions go here  }
	kFontsFolderType			= $666F6E74 (* 'font' *);						{  Fonts go here  }
	kPreferencesFolderType		= $70726566 (* 'pref' *);						{  preferences for applications go here  }
	kSystemPreferencesFolderType = $73707266 (* 'sprf' *);						{  System-type Preferences go here - this is always the system's preferences folder, never a logged in user's  }
	kTemporaryFolderType		= $74656D70 (* 'temp' *);						{  temporary files go here (deleted periodically, but don't rely on it.)  }

	{
	 *  FindFolder()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FindFolder(vRefNum: SInt16; folderType: OSType; createFolder: boolean; var foundVRefNum: SInt16; var foundDirID: UInt32): OSErr; external name '_FindFolder';
{
 *  FindFolderExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FindFolderExtended(vol: SInt16; foldType: OSType; createFolder: boolean; flags: UInt32; data: UnivPtr; var vRefNum: SInt16; var dirID: UInt32): OSErr; external name '_FindFolderExtended';
{
 *  ReleaseFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReleaseFolder(vRefNum: SInt16; folderType: OSType): OSErr; external name '_ReleaseFolder';
{$ifc NOT TARGET_OS_MAC}
{ Since non-mac targets don't know about VRef's or DirID's, the Ex version returns
   the found folder path.
 }
{$ifc CALL_NOT_IN_CARBON}
{
 *  FindFolderEx()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FindFolderEx(vRefNum: SInt16; folderType: OSType; createFolder: boolean; var foundVRefNum: SInt16; var foundDirID: UInt32; foundFolder: CStringPtr): OSErr; external name '_FindFolderEx';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{
 *  FSFindFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSFindFolder(vRefNum: SInt16; folderType: OSType; createFolder: boolean; var foundRef: FSRef): OSErr; external name '_FSFindFolder';
{
 *  FSFindFolderExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSFindFolderExtended(vol: SInt16; foldType: OSType; createFolder: boolean; flags: UInt32; data: UnivPtr; var foundRef: FSRef): OSErr; external name '_FSFindFolderExtended';
{****************************************}
{ Extensible Folder Manager declarations }
{****************************************}

{**************************}
{ Folder Manager constants }
{**************************}


const
	kExtensionDisabledFolderType = $65787444 (* 'extD' *);
	kControlPanelDisabledFolderType = $63747244 (* 'ctrD' *);
	kSystemExtensionDisabledFolderType = $6D616344 (* 'macD' *);
	kStartupItemsDisabledFolderType = $73747244 (* 'strD' *);
	kShutdownItemsDisabledFolderType = $73686444 (* 'shdD' *);
	kApplicationsFolderType		= $61707073 (* 'apps' *);
	kDocumentsFolderType		= $646F6373 (* 'docs' *);

																{  new constants  }
	kVolumeRootFolderType		= $726F6F74 (* 'root' *);						{  root folder of a volume  }
	kChewableItemsFolderType	= $666C6E74 (* 'flnt' *);						{  items deleted at boot  }
	kApplicationSupportFolderType = $61737570 (* 'asup' *);						{  third-party items and folders  }
	kTextEncodingsFolderType	= $C6927465 (* 'Ätex' *);						{  encoding tables  }
	kStationeryFolderType		= $6F647374 (* 'odst' *);						{  stationery  }
	kOpenDocFolderType			= $6F646F64 (* 'odod' *);						{  OpenDoc root  }
	kOpenDocShellPlugInsFolderType = $6F647370 (* 'odsp' *);					{  OpenDoc Shell Plug-Ins in OpenDoc folder  }
	kEditorsFolderType			= $6F646564 (* 'oded' *);						{  OpenDoc editors in MacOS Folder  }
	kOpenDocEditorsFolderType	= $C6926F64 (* 'Äodf' *);						{  OpenDoc subfolder of Editors folder  }
	kOpenDocLibrariesFolderType	= $6F646C62 (* 'odlb' *);						{  OpenDoc libraries folder  }
	kGenEditorsFolderType		= $C6926564 (* 'Äedi' *);						{  CKH general editors folder at root level of Sys folder  }
	kHelpFolderType				= $C692686C (* 'Ählp' *);						{  CKH help folder currently at root of system folder  }
	kInternetPlugInFolderType	= $C6926E65 (* 'Änet' *);						{  CKH internet plug ins for browsers and stuff  }
	kModemScriptsFolderType		= $C6926D6F (* 'Ämod' *);						{  CKH modem scripts, get 'em OUT of the Extensions folder  }
	kPrinterDescriptionFolderType = $70706466 (* 'ppdf' *);						{  CKH new folder at root of System folder for printer descs.  }
	kPrinterDriverFolderType	= $C6927072 (* 'Äprd' *);						{  CKH new folder at root of System folder for printer drivers  }
	kScriptingAdditionsFolderType = $C6927363 (* 'Äscr' *);						{  CKH at root of system folder  }
	kSharedLibrariesFolderType	= $C6926C69 (* 'Älib' *);						{  CKH for general shared libs.  }
	kVoicesFolderType			= $66766F63 (* 'fvoc' *);						{  CKH macintalk can live here  }
	kControlStripModulesFolderType = $73646576 (* 'sdev' *);					{  CKH for control strip modules  }
	kAssistantsFolderType		= $617374C6 (* 'astÄ' *);						{  SJF for Assistants (MacOS Setup Assistant, etc)  }
	kUtilitiesFolderType		= $757469C6 (* 'utiÄ' *);						{  SJF for Utilities folder  }
	kAppleExtrasFolderType		= $616578C6 (* 'aexÄ' *);						{  SJF for Apple Extras folder  }
	kContextualMenuItemsFolderType = $636D6E75 (* 'cmnu' *);					{  SJF for Contextual Menu items  }
	kMacOSReadMesFolderType		= $6D6F72C6 (* 'morÄ' *);						{  SJF for MacOS ReadMes folder  }
	kALMModulesFolderType		= $77616C6B (* 'walk' *);						{  EAS for Location Manager Module files except type 'thng' (within kExtensionFolderType)  }
	kALMPreferencesFolderType	= $74726970 (* 'trip' *);						{  EAS for Location Manager Preferences (within kPreferencesFolderType; contains kALMLocationsFolderType)  }
	kALMLocationsFolderType		= $66616C6C (* 'fall' *);						{  EAS for Location Manager Locations (within kALMPreferencesFolderType)  }
	kColorSyncProfilesFolderType = $70726F66 (* 'prof' *);						{  for ColorSyncª Profiles  }
	kThemesFolderType			= $74686D65 (* 'thme' *);						{  for Theme data files  }
	kFavoritesFolderType		= $66617673 (* 'favs' *);						{  Favorties folder for Navigation Services  }
	kInternetFolderType			= $696E74C6 (* 'intÄ' *);						{  Internet folder (root level of startup volume)  }
	kAppearanceFolderType		= $61707072 (* 'appr' *);						{  Appearance folder (root of system folder)  }
	kSoundSetsFolderType		= $736E6473 (* 'snds' *);						{  Sound Sets folder (in Appearance folder)  }
	kDesktopPicturesFolderType	= $647470C6 (* 'dtpÄ' *);						{  Desktop Pictures folder (in Appearance folder)  }
	kInternetSearchSitesFolderType = $69737366 (* 'issf' *);					{  Internet Search Sites folder  }
	kFindSupportFolderType		= $666E6473 (* 'fnds' *);						{  Find support folder  }
	kFindByContentFolderType	= $66626366 (* 'fbcf' *);						{  Find by content folder  }
	kInstallerLogsFolderType	= $696C6766 (* 'ilgf' *);						{  Installer Logs folder  }
	kScriptsFolderType			= $736372C6 (* 'scrÄ' *);						{  Scripts folder  }
	kFolderActionsFolderType	= $66617366 (* 'fasf' *);						{  Folder Actions Scripts folder  }
	kLauncherItemsFolderType	= $6C61756E (* 'laun' *);						{  Launcher Items folder  }
	kRecentApplicationsFolderType = $72617070 (* 'rapp' *);						{  Recent Applications folder  }
	kRecentDocumentsFolderType	= $72646F63 (* 'rdoc' *);						{  Recent Documents folder  }
	kRecentServersFolderType	= $72737672 (* 'rsvr' *);						{  Recent Servers folder  }
	kSpeakableItemsFolderType	= $73706B69 (* 'spki' *);						{  Speakable Items folder  }
	kKeychainFolderType			= $6B63686E (* 'kchn' *);						{  Keychain folder  }
	kQuickTimeExtensionsFolderType = $71746578 (* 'qtex' *);					{  QuickTime Extensions Folder (in Extensions folder)  }
	kDisplayExtensionsFolderType = $6473706C (* 'dspl' *);						{  Display Extensions Folder (in Extensions folder)  }
	kMultiprocessingFolderType	= $6D707866 (* 'mpxf' *);						{  Multiprocessing Folder (in Extensions folder)  }
	kPrintingPlugInsFolderType	= $70706C67 (* 'pplg' *);						{  Printing Plug-Ins Folder (in Extensions folder)  }


	{	 New Folder Types to accommodate the Mac OS X Folder Manager 	}
	{	 These folder types are not applicable on Mac OS 9.          	}
	kDomainTopLevelFolderType	= $64746F70 (* 'dtop' *);						{  The top-level of a Folder domain, e.g. "/System" }
	kDomainLibraryFolderType	= $646C6962 (* 'dlib' *);						{  the Library subfolder of a particular domain }
	kColorSyncFolderType		= $73796E63 (* 'sync' *);						{  Contains ColorSync-related folders }
	kColorSyncCMMFolderType		= $63636D6D (* 'ccmm' *);						{  ColorSync CMMs }
	kColorSyncScriptingFolderType = $63736372 (* 'cscr' *);						{  ColorSync Scripting support }
	kPrintersFolderType			= $696D7072 (* 'impr' *);						{  Contains Printing-related folders }
	kSpeechFolderType			= $73706368 (* 'spch' *);						{  Contains Speech-related folders }
	kCarbonLibraryFolderType	= $63617262 (* 'carb' *);						{  Contains Carbon-specific file }
	kDocumentationFolderType	= $696E666F (* 'info' *);						{  Contains Documentation files (not user documents) }
	kDeveloperDocsFolderType	= $64646F63 (* 'ddoc' *);						{  Contains Developer Documentation files and folders }
	kDeveloperHelpFolderType	= $64657668 (* 'devh' *);						{  Contains Developer Help related files }
	kISSDownloadsFolderType		= $69737364 (* 'issd' *);						{  Contains Internet Search Sites downloaded from the Internet }
	kUserSpecificTmpFolderType	= $75746D70 (* 'utmp' *);						{  Contains temporary items created on behalf of the current user }
	kCachedDataFolderType		= $63616368 (* 'cach' *);						{  Contains various cache files for different clients }
	kFrameworksFolderType		= $6672616D (* 'fram' *);						{  Contains MacOS X Framework folders      }
	kPrivateFrameworksFolderType = $7066726D (* 'pfrm' *);						{  Contains MacOS X Private Framework folders      }
	kClassicDesktopFolderType	= $7364736B (* 'sdsk' *);						{  MacOS 9 compatible desktop folder - same as  }
																{  kSystemDesktopFolderType but with a more appropriate }
																{  name for Mac OS X code. }
	kDeveloperFolderType		= $64657666 (* 'devf' *);						{  Contains MacOS X Developer Resources }
	kSystemSoundsFolderType		= $73736E64 (* 'ssnd' *);						{  Contains Mac OS X System Sound Files }
	kComponentsFolderType		= $636D7064 (* 'cmpd' *);						{  Contains Mac OS X components }
	kQuickTimeComponentsFolderType = $77636D70 (* 'wcmp' *);					{  Contains QuickTime components for Mac OS X }
	kCoreServicesFolderType		= $63737276 (* 'csrv' *);						{  Refers to the "CoreServices" folder on Mac OS X }
	kPictureDocumentsFolderType	= $70646F63 (* 'pdoc' *);						{  Refers to the "Pictures" folder in a users home directory }
	kMovieDocumentsFolderType	= $6D646F63 (* 'mdoc' *);						{  Refers to the "Movies" folder in a users home directory }
	kMusicDocumentsFolderType	= $C2B5646F (* 'µdoc' *);						{  Refers to the "Music" folder in a users home directory }
	kInternetSitesFolderType	= $73697465 (* 'site' *);						{  Refers to the "Sites" folder in a users home directory }
	kPublicFolderType			= $70756262 (* 'pubb' *);						{  Refers to the "Public" folder in a users home directory }
	kAudioSupportFolderType		= $6164696F (* 'adio' *);						{  Refers to the Audio support folder for Mac OS X }
	kAudioSoundsFolderType		= $61736E64 (* 'asnd' *);						{  Refers to the Sounds subfolder of Audio Support }
	kAudioSoundBanksFolderType	= $62616E6B (* 'bank' *);						{  Refers to the Banks subfolder of the Sounds Folder }
	kAudioAlertSoundsFolderType	= $616C7274 (* 'alrt' *);						{  Refers to the Alert Sounds subfolder of the Sound Folder }
	kAudioPlugInsFolderType		= $61706C67 (* 'aplg' *);						{  Refers to the Plug-ins subfolder of the Audio Folder    }
	kAudioComponentsFolderType	= $61636D70 (* 'acmp' *);						{  Refers to the Components subfolder of the Audio Plug-ins Folder     }
	kKernelExtensionsFolderType	= $6B657874 (* 'kext' *);						{  Refers to the Kernel Extensions Folder on Mac OS X }
	kDirectoryServicesFolderType = $64737276 (* 'dsrv' *);						{  Refers to the Directory Services folder on Mac OS X }
	kDirectoryServicesPlugInsFolderType = $64706C67 (* 'dplg' *);				{  Refers to the Directory Services Plug-Ins folder on Mac OS X  }
	kInstallerReceiptsFolderType = $72637074 (* 'rcpt' *);						{  Refers to the "Receipts" folder in Mac OS X }
	kFileSystemSupportFolderType = $66737973 (* 'fsys' *);						{  Refers to the [domain]/Library/Filesystems folder in Mac OS X }
	kAppleShareSupportFolderType = $73686172 (* 'shar' *);						{  Refers to the [domain]/Library/Filesystems/AppleShare folder in Mac OS X }
	kAppleShareAuthenticationFolderType = $61757468 (* 'auth' *);				{  Refers to the [domain]/Library/Filesystems/AppleShare/Authentication folder in Mac OS X }
	kMIDIDriversFolderType		= $6D696469 (* 'midi' *);						{  Refers to the MIDI Drivers folder on Mac OS X }

	kLocalesFolderType			= $C6926C6F (* 'Äloc' *);						{  PKE for Locales folder  }
	kFindByContentPluginsFolderType = $66626370 (* 'fbcp' *);					{  Find By Content Plug-ins  }

	kUsersFolderType			= $75737273 (* 'usrs' *);						{  "Users" folder, contains one folder for each user.  }
	kCurrentUserFolderType		= $63757372 (* 'cusr' *);						{  The folder for the currently logged on user.  }
	kCurrentUserRemoteFolderLocation = $72757366 (* 'rusf' *);					{  The remote folder for the currently logged on user  }
	kCurrentUserRemoteFolderType = $72757372 (* 'rusr' *);						{  The remote folder location for the currently logged on user  }
	kSharedUserDataFolderType	= $73646174 (* 'sdat' *);						{  A Shared "Documents" folder, readable & writeable by all users  }
	kVolumeSettingsFolderType	= $76736664 (* 'vsfd' *);						{  Volume specific user information goes here  }

	kAppleshareAutomountServerAliasesFolderType = $737276C6 (* 'srvÄ' *);		{  Appleshare puts volumes to automount inside this folder.  }
	kPreMacOS91ApplicationsFolderType = $C3A57070 (* 'Œpps' *);					{  The "Applications" folder, pre Mac OS 9.1  }
	kPreMacOS91InstallerLogsFolderType = $C3AE6C67 (* '”lgf' *);				{  The "Installer Logs" folder, pre Mac OS 9.1  }
	kPreMacOS91AssistantsFolderType = $C3A57374 (* 'ŒstÄ' *);					{  The "Assistants" folder, pre Mac OS 9.1  }
	kPreMacOS91UtilitiesFolderType = $C3BC7469 (* 'ŸtiÄ' *);					{  The "Utilities" folder, pre Mac OS 9.1  }
	kPreMacOS91AppleExtrasFolderType = $C3A56578 (* 'ŒexÄ' *);					{  The "Apple Extras" folder, pre Mac OS 9.1  }
	kPreMacOS91MacOSReadMesFolderType = $C2B56F72 (* 'µorÄ' *);					{  The "Mac OS ReadMes" folder, pre Mac OS 9.1  }
	kPreMacOS91InternetFolderType = $C3AE6E74 (* '”ntÄ' *);						{  The "Internet" folder, pre Mac OS 9.1  }
	kPreMacOS91AutomountedServersFolderType = $C39F7276 (* '§rvÄ' *);			{  The "Servers" folder, pre Mac OS 9.1  }
	kPreMacOS91StationeryFolderType = $C3B86473 (* '¿dst' *);					{  The "Stationery" folder, pre Mac OS 9.1  }

	{	 FolderDescFlags values 	}
	kCreateFolderAtBoot			= $00000002;
	kCreateFolderAtBootBit		= 1;
	kFolderCreatedInvisible		= $00000004;
	kFolderCreatedInvisibleBit	= 2;
	kFolderCreatedNameLocked	= $00000008;
	kFolderCreatedNameLockedBit	= 3;
	kFolderCreatedAdminPrivs	= $00000010;
	kFolderCreatedAdminPrivsBit	= 4;

	kFolderInUserFolder			= $00000020;
	kFolderInUserFolderBit		= 5;
	kFolderTrackedByAlias		= $00000040;
	kFolderTrackedByAliasBit	= 6;
	kFolderInRemoteUserFolderIfAvailable = $00000080;
	kFolderInRemoteUserFolderIfAvailableBit = 7;
	kFolderNeverMatchedInIdentifyFolder = $00000100;
	kFolderNeverMatchedInIdentifyFolderBit = 8;
	kFolderMustStayOnSameVolume	= $00000200;
	kFolderMustStayOnSameVolumeBit = 9;
	kFolderManagerFolderInMacOS9FolderIfMacOSXIsInstalledMask = $00000400;
	kFolderManagerFolderInMacOS9FolderIfMacOSXIsInstalledBit = 10;
	kFolderInLocalOrRemoteUserFolder = $000000A0;


type
	FolderDescFlags						= UInt32;
	{	 FolderClass values 	}

const
	kRelativeFolder				= $72656C66 (* 'relf' *);
	kSpecialFolder				= $73706366 (* 'spcf' *);


type
	FolderClass							= OSType;
	{	 special folder locations 	}

const
	kBlessedFolder				= $626C7366 (* 'blsf' *);
	kRootFolder					= $726F7466 (* 'rotf' *);

	kCurrentUserFolderLocation	= $63757366 (* 'cusf' *);						{     the magic 'Current User' folder location }


type
	FolderType							= OSType;
	FolderLocation						= OSType;

	FolderDescPtr = ^FolderDesc;
	FolderDesc = record
		descSize:				Size;
		foldType:				FolderType;
		flags:					FolderDescFlags;
		foldClass:				FolderClass;
		foldLocation:			FolderType;
		badgeSignature:			OSType;
		badgeType:				OSType;
		reserved:				UInt32;
		name:					StrFileName;							{  Str63 on MacOS }
	end;


	RoutingFlags						= UInt32;
	FolderRoutingPtr = ^FolderRouting;
	FolderRouting = record
		descSize:				Size;
		fileType:				OSType;
		routeFromFolder:		FolderType;
		routeToFolder:			FolderType;
		flags:					RoutingFlags;
	end;

	{	 routing constants 	}
	{   These are bits in the .flags field of the FindFolderUserRedirectionGlobals struct }

const
																{     Set this bit to 1 in the .flags field of a FindFolderUserRedirectionGlobals }
																{     structure if the userName in the struct should be used as the current }
																{     "User" name }
	kFindFolderRedirectionFlagUseDistinctUserFoldersBit = 0;	{     Set this bit to 1 and the currentUserFolderVRefNum and currentUserFolderDirID }
																{     fields of the user record will get used instead of finding the user folder }
																{     with the userName field. }
	kFindFolderRedirectionFlagUseGivenVRefAndDirIDAsUserFolderBit = 1; {     Set this bit to 1 and the remoteUserFolderVRefNum and remoteUserFolderDirID }
																{     fields of the user record will get used instead of finding the user folder }
																{     with the userName field. }
	kFindFolderRedirectionFlagsUseGivenVRefNumAndDirIDAsRemoteUserFolderBit = 2;


type
	FindFolderUserRedirectionGlobalsPtr = ^FindFolderUserRedirectionGlobals;
	FindFolderUserRedirectionGlobals = record
		version:				UInt32;
		flags:					UInt32;
		userName:				Str31;
		userNameScript:			SInt16;
		currentUserFolderVRefNum: SInt16;
		currentUserFolderDirID:	UInt32;
		remoteUserFolderVRefNum: SInt16;
		remoteUserFolderDirID:	UInt32;
	end;


const
	kFolderManagerUserRedirectionGlobalsCurrentVersion = 1;

	{
	    These are passed into FindFolderExtended(), FindFolderInternalExtended(), and
	    FindFolderNewInstallerEntryExtended() in the flags field. 
	}
	kFindFolderExtendedFlagsDoNotFollowAliasesBit = 0;
	kFindFolderExtendedFlagsDoNotUseUserFolderBit = 1;
	kFindFolderExtendedFlagsUseOtherUserRecord = $01000000;


type
{$ifc TYPED_FUNCTION_POINTERS}
	FolderManagerNotificationProcPtr = function(message: OSType; arg: UnivPtr; userRefCon: UnivPtr): OSStatus;
{$elsec}
	FolderManagerNotificationProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	FolderManagerNotificationUPP = ^SInt32; { an opaque UPP }
{$elsec}
	FolderManagerNotificationUPP = UniversalProcPtr;
{$endc}	

const
	uppFolderManagerNotificationProcInfo = $00000FF0;
	{
	 *  NewFolderManagerNotificationUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0.2 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewFolderManagerNotificationUPP(userRoutine: FolderManagerNotificationProcPtr): FolderManagerNotificationUPP; external name '_NewFolderManagerNotificationUPP'; { old name was NewFolderManagerNotificationProc }
{
 *  DisposeFolderManagerNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeFolderManagerNotificationUPP(userUPP: FolderManagerNotificationUPP); external name '_DisposeFolderManagerNotificationUPP';
{
 *  InvokeFolderManagerNotificationUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0.2 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeFolderManagerNotificationUPP(message: OSType; arg: UnivPtr; userRefCon: UnivPtr; userRoutine: FolderManagerNotificationUPP): OSStatus; external name '_InvokeFolderManagerNotificationUPP'; { old name was CallFolderManagerNotificationProc }
const
	kFolderManagerNotificationMessageUserLogIn = $6C6F672B (* 'log+' *);		{     Sent by system & third party software after a user logs in.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationMessagePreUserLogIn = $6C6F676A (* 'logj' *);		{     Sent by system & third party software before a user logs in.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationMessageUserLogOut = $6C6F672D (* 'log-' *);		{     Sent by system & third party software before a user logs out.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationMessagePostUserLogOut = $6C6F6770 (* 'logp' *);	{     Sent by system & third party software after a user logs out.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationDiscardCachedData = $64636865 (* 'dche' *);		{     Sent by system & third party software when the entire Folder Manager cache should be flushed }
	kFolderManagerNotificationMessageLoginStartup = $73747570 (* 'stup' *);		{     Sent by 'Login' application the first time it starts up after each boot }


	{   These get used in the options parameter of FolderManagerRegisterNotificationProc() }
	kDoNotRemoveWhenCurrentApplicationQuitsBit = 0;
	kDoNotRemoveWheCurrentApplicationQuitsBit = 0;				{     Going away soon, use kDoNotRemoveWheCurrentApplicationQuitsBit }

	{   These get used in the options parameter of FolderManagerCallNotificationProcs() }
	kStopIfAnyNotificationProcReturnsErrorBit = 31;

	{	*************************	}
	{	 Folder Manager routines 	}
	{	*************************	}
	{	 Folder Manager administration routines 	}
	{
	 *  AddFolderDescriptor()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function AddFolderDescriptor(foldType: FolderType; flags: FolderDescFlags; foldClass: FolderClass; foldLocation: FolderLocation; badgeSignature: OSType; badgeType: OSType; const (*var*) name: StrFileName; replaceFlag: boolean): OSErr; external name '_AddFolderDescriptor';
{
 *  GetFolderDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetFolderDescriptor(foldType: FolderType; descSize: Size; var foldDesc: FolderDesc): OSErr; external name '_GetFolderDescriptor';
{
 *  GetFolderTypes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetFolderTypes(requestedTypeCount: UInt32; var totalTypeCount: UInt32; var theTypes: FolderType): OSErr; external name '_GetFolderTypes';
{
 *  RemoveFolderDescriptor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RemoveFolderDescriptor(foldType: FolderType): OSErr; external name '_RemoveFolderDescriptor';
{ legacy routines }
{
 *  GetFolderName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetFolderName(vRefNum: SInt16; foldType: OSType; var foundVRefNum: SInt16; var name: StrFileName): OSErr; external name '_GetFolderName';
{ routing routines }
{
 *  AddFolderRouting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AddFolderRouting(fileType: OSType; routeFromFolder: FolderType; routeToFolder: FolderType; flags: RoutingFlags; replaceFlag: boolean): OSErr; external name '_AddFolderRouting';
{
 *  RemoveFolderRouting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RemoveFolderRouting(fileType: OSType; routeFromFolder: FolderType): OSErr; external name '_RemoveFolderRouting';
{
 *  FindFolderRouting()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FindFolderRouting(fileType: OSType; routeFromFolder: FolderType; var routeToFolder: FolderType; var flags: RoutingFlags): OSErr; external name '_FindFolderRouting';
{
 *  GetFolderRoutings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetFolderRoutings(requestedRoutingCount: UInt32; var totalRoutingCount: UInt32; routingSize: Size; var theRoutings: FolderRouting): OSErr; external name '_GetFolderRoutings';
{
 *  InvalidateFolderDescriptorCache()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvalidateFolderDescriptorCache(vRefNum: SInt16; dirID: UInt32): OSErr; external name '_InvalidateFolderDescriptorCache';
{
 *  IdentifyFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IdentifyFolder(vRefNum: SInt16; dirID: UInt32; var foldType: FolderType): OSErr; external name '_IdentifyFolder';
{
 *  FolderManagerRegisterNotificationProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FolderManagerRegisterNotificationProc(notificationProc: FolderManagerNotificationUPP; refCon: UnivPtr; options: UInt32): OSErr; external name '_FolderManagerRegisterNotificationProc';
{
 *  FolderManagerUnregisterNotificationProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FolderManagerUnregisterNotificationProc(notificationProc: FolderManagerNotificationUPP; refCon: UnivPtr): OSErr; external name '_FolderManagerUnregisterNotificationProc';
{
 *  FolderManagerRegisterCallNotificationProcs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FolderManagerRegisterCallNotificationProcs(message: OSType; arg: UnivPtr; options: UInt32): OSStatus; external name '_FolderManagerRegisterCallNotificationProcs';
{*****************************}
{ MultiUser (At Ease) globals }
{*****************************}
{
   This structure has been through some evolution since the early days of At Ease 1.0.  The structure
   has been expanded (and developers should assume that it will continue this way into the future).  Older
   fields have been obsoleted as the features have changed in newer versions of the code.
}

{  Some fields in here are really only valid for the network version of Macintosh Manager }


type
	MultiUserGestaltPtr = ^MultiUserGestalt;
	MultiUserGestalt = record
																		{     Version 1 fields. }
		giVersion:				SInt16;								{  structure version: 0 = invalid, 6 = OS 9 }
		giReserved0:			SInt16;								{  [OBSOLETE with v3] giIsActive: if true then At Ease is currently running }
		giReserved1:			SInt16;								{  [OBSOLETE] if true then auto create alias }
		giReserved2:			SInt16;								{  [OBSOLETE with v6]  if true then request floppy on new saves }
		giReserved3:			SInt16;								{  [OBSOLETE] if true then hypercard stacks are shown on Applications panel }
		giReserved4:			FSSpec;									{  [OBSOLETE with v6] location of At Ease Items folder }
																		{     Version 2 fields. }
		giDocsVRefNum:			SInt16;								{  vrefnum of user's documents location (only valid if not on floppy) }
		giDocsDirID:			UInt32;								{  directory id of user's documents folder (only valid if not on floppy) }
		giForceSaves:			SInt16;								{  true if user is forced to save to their documents folder }
		giForceOpens:			SInt16;								{  true if user is forced to open from their documents folder }
		giSetupName:			Str31;									{  name of current setup }
		giUserName:				Str31;									{  name of current user }
		giFrontAppName:			Str31;									{  name of the frontmost application }
		giReserved5:			SInt16;								{  [OBSOLETE with v6] true if user has Go To Finder menu item }
		giIsOn:					SInt16;								{  true if Multiple Users/Macintosh Manager is on right now }
																		{     Version 3 fields. }
																		{   There were no additional fields for version 3.x }
																		{     Version 4 fields. }
		giUserLoggedInType:		SInt16;								{  0 = normal user, 1 = workgroup admin, 2 = global admin }
		giUserEncryptPwd:		packed array [0..15] of char;			{  encrypted user password (our digest form) }
		giUserEnvironment:		SInt16;								{  0 = panels, 1 = normal Finder, 2 = limited/restricted Finder }
		giReserved6:			SInt32;								{  [OBSOLETE] }
		giReserved7:			SInt32;								{  [OBSOLETE] }
		giDisableScrnShots:		boolean;								{  true if screen shots are not allowed }
																		{     Version 5 fields. }
		giSupportsAsyncFSCalls:	boolean;								{  Finder uses this to tell if our patches support async trap patches }
		giPrefsVRefNum:			SInt16;								{  vrefnum of preferences }
		giPrefsDirID:			UInt32;								{  dirID of the At Ease Items folder on preferences volume }
		giUserLogInTime:		UInt32;									{  time in seconds we've been logged in (0 or 1 mean not logged in) }
		giUsingPrintQuotas:		boolean;								{  true if logged in user is using printer quotas }
		giUsingDiskQuotas:		boolean;								{  true if logged in user has disk quotas active }
																		{  Version 6 fields - As of Mac OS 9's "Multiple Users 1.0" }
		giInSystemAccess:		boolean;								{  true if system is in System Access (i.e., owner logged in) }
		giUserFolderEnabled:	boolean;								{  true if FindFolder is redirecting folders (uses giUserName for user) }
		giReserved8:			SInt16;
		giReserved9:			SInt32;
		giInLoginScreen:		boolean;								{  true if no user has logged in (including owner) }
																		{  May have more fields added in future, so never check for sizeof(GestaltRec) }
	end;

	MultiUserGestaltHandle				= ^MultiUserGestaltPtr;


{$ALIGN MAC68K}


end.
