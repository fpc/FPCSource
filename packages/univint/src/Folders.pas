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
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit Folders;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

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

	kSystemFolderType			= FourCharCode('macs');						{  the system folder  }
	kDesktopFolderType			= FourCharCode('desk');						{  the desktop folder; objects in this folder show on the desk top.  }
	kSystemDesktopFolderType	= FourCharCode('sdsk');						{  the desktop folder at the root of the hard drive, never the redirected user desktop folder  }
	kTrashFolderType			= FourCharCode('trsh');						{  the trash folder; objects in this folder show up in the trash  }
	kSystemTrashFolderType		= FourCharCode('strs');						{  the trash folder at the root of the drive, never the redirected user trash folder  }
	kWhereToEmptyTrashFolderType = FourCharCode('empt');						{  the "empty trash" folder; Finder starts empty from here down  }
	kPrintMonitorDocsFolderType	= FourCharCode('prnt');						{  Print Monitor documents  }
	kStartupFolderType			= FourCharCode('strt');						{  Finder objects (applications, documents, DAs, aliases, to...) to open at startup go here  }
	kShutdownFolderType			= FourCharCode('shdf');						{  Finder objects (applications, documents, DAs, aliases, to...) to open at shutdown go here  }
	kAppleMenuFolderType		= FourCharCode('amnu');						{  Finder objects to put into the Apple menu go here  }
	kControlPanelFolderType		= FourCharCode('ctrl');						{  Control Panels go here (may contain INITs)  }
	kSystemControlPanelFolderType = FourCharCode('sctl');						{  System control panels folder - never the redirected one, always "Control Panels" inside the System Folder  }
	kExtensionFolderType		= FourCharCode('extn');						{  System extensions go here  }
	kFontsFolderType			= FourCharCode('font');						{  Fonts go here  }
	kPreferencesFolderType		= FourCharCode('pref');						{  preferences for applications go here  }
	kSystemPreferencesFolderType = FourCharCode('sprf');						{  System-type Preferences go here - this is always the system's preferences folder, never a logged in user's  }
	kTemporaryFolderType		= FourCharCode('temp');						{  temporary files go here (deleted periodically, but don't rely on it.)  }

	{
	 *  FindFolder()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FindFolder(vRefNum: SInt16; folderType: OSType; createFolder: boolean; var foundVRefNum: SInt16; var foundDirID: DirIDType): OSErr; external name '_FindFolder';
{
 *  FindFolderExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FindFolderExtended(vol: SInt16; foldType: OSType; createFolder: boolean; flags: UInt32; data: UnivPtr; var vRefNum: SInt16; var dirID: DirIDType): OSErr; external name '_FindFolderExtended';
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
function FindFolderEx(vRefNum: SInt16; folderType: OSType; createFolder: boolean; var foundVRefNum: SInt16; var foundDirID: DirIDType; foundFolder: CStringPtr): OSErr; external name '_FindFolderEx';

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
	kExtensionDisabledFolderType = FourCharCode('extD');
	kControlPanelDisabledFolderType = FourCharCode('ctrD');
	kSystemExtensionDisabledFolderType = FourCharCode('macD');
	kStartupItemsDisabledFolderType = FourCharCode('strD');
	kShutdownItemsDisabledFolderType = FourCharCode('shdD');
	kApplicationsFolderType		= FourCharCode('apps');
	kDocumentsFolderType		= FourCharCode('docs');

																{  new constants  }
	kVolumeRootFolderType		= FourCharCode('root');						{  root folder of a volume  }
	kChewableItemsFolderType	= FourCharCode('flnt');						{  items deleted at boot  }
	kApplicationSupportFolderType = FourCharCode('asup');						{  third-party items and folders  }
	kTextEncodingsFolderType	= FourCharCode('Ätex');						{  encoding tables  }
	kStationeryFolderType		= FourCharCode('odst');						{  stationery  }
	kOpenDocFolderType			= FourCharCode('odod');						{  OpenDoc root  }
	kOpenDocShellPlugInsFolderType = FourCharCode('odsp');					{  OpenDoc Shell Plug-Ins in OpenDoc folder  }
	kEditorsFolderType			= FourCharCode('oded');						{  OpenDoc editors in MacOS Folder  }
	kOpenDocEditorsFolderType	= FourCharCode('Äodf');						{  OpenDoc subfolder of Editors folder  }
	kOpenDocLibrariesFolderType	= FourCharCode('odlb');						{  OpenDoc libraries folder  }
	kGenEditorsFolderType		= FourCharCode('Äedi');						{  CKH general editors folder at root level of Sys folder  }
	kHelpFolderType				= FourCharCode('Ählp');						{  CKH help folder currently at root of system folder  }
	kInternetPlugInFolderType	= FourCharCode('Änet');						{  CKH internet plug ins for browsers and stuff  }
	kModemScriptsFolderType		= FourCharCode('Ämod');						{  CKH modem scripts, get 'em OUT of the Extensions folder  }
	kPrinterDescriptionFolderType = FourCharCode('ppdf');						{  CKH new folder at root of System folder for printer descs.  }
	kPrinterDriverFolderType	= FourCharCode('Äprd');						{  CKH new folder at root of System folder for printer drivers  }
	kScriptingAdditionsFolderType = FourCharCode('Äscr');						{  CKH at root of system folder  }
	kSharedLibrariesFolderType	= FourCharCode('Älib');						{  CKH for general shared libs.  }
	kVoicesFolderType			= FourCharCode('fvoc');						{  CKH macintalk can live here  }
	kControlStripModulesFolderType = FourCharCode('sdev');					{  CKH for control strip modules  }
	kAssistantsFolderType		= FourCharCode('astÄ');						{  SJF for Assistants (MacOS Setup Assistant, etc)  }
	kUtilitiesFolderType		= FourCharCode('utiÄ');						{  SJF for Utilities folder  }
	kAppleExtrasFolderType		= FourCharCode('aexÄ');						{  SJF for Apple Extras folder  }
	kContextualMenuItemsFolderType = FourCharCode('cmnu');					{  SJF for Contextual Menu items  }
	kMacOSReadMesFolderType		= FourCharCode('morÄ');						{  SJF for MacOS ReadMes folder  }
	kALMModulesFolderType		= FourCharCode('walk');						{  EAS for Location Manager Module files except type 'thng' (within kExtensionFolderType)  }
	kALMPreferencesFolderType	= FourCharCode('trip');						{  EAS for Location Manager Preferences (within kPreferencesFolderType; contains kALMLocationsFolderType)  }
	kALMLocationsFolderType		= FourCharCode('fall');						{  EAS for Location Manager Locations (within kALMPreferencesFolderType)  }
	kColorSyncProfilesFolderType = FourCharCode('prof');						{  for ColorSyncª Profiles  }
	kThemesFolderType			= FourCharCode('thme');						{  for Theme data files  }
	kFavoritesFolderType		= FourCharCode('favs');						{  Favorties folder for Navigation Services  }
	kInternetFolderType			= FourCharCode('intÄ');						{  Internet folder (root level of startup volume)  }
	kAppearanceFolderType		= FourCharCode('appr');						{  Appearance folder (root of system folder)  }
	kSoundSetsFolderType		= FourCharCode('snds');						{  Sound Sets folder (in Appearance folder)  }
	kDesktopPicturesFolderType	= FourCharCode('dtpÄ');						{  Desktop Pictures folder (in Appearance folder)  }
	kInternetSearchSitesFolderType = FourCharCode('issf');					{  Internet Search Sites folder  }
	kFindSupportFolderType		= FourCharCode('fnds');						{  Find support folder  }
	kFindByContentFolderType	= FourCharCode('fbcf');						{  Find by content folder  }
	kInstallerLogsFolderType	= FourCharCode('ilgf');						{  Installer Logs folder  }
	kScriptsFolderType			= FourCharCode('scrÄ');						{  Scripts folder  }
	kFolderActionsFolderType	= FourCharCode('fasf');						{  Folder Actions Scripts folder  }
	kLauncherItemsFolderType	= FourCharCode('laun');						{  Launcher Items folder  }
	kRecentApplicationsFolderType = FourCharCode('rapp');						{  Recent Applications folder  }
	kRecentDocumentsFolderType	= FourCharCode('rdoc');						{  Recent Documents folder  }
	kRecentServersFolderType	= FourCharCode('rsvr');						{  Recent Servers folder  }
	kSpeakableItemsFolderType	= FourCharCode('spki');						{  Speakable Items folder  }
	kKeychainFolderType			= FourCharCode('kchn');						{  Keychain folder  }
	kQuickTimeExtensionsFolderType = FourCharCode('qtex');					{  QuickTime Extensions Folder (in Extensions folder)  }
	kDisplayExtensionsFolderType = FourCharCode('dspl');						{  Display Extensions Folder (in Extensions folder)  }
	kMultiprocessingFolderType	= FourCharCode('mpxf');						{  Multiprocessing Folder (in Extensions folder)  }
	kPrintingPlugInsFolderType	= FourCharCode('pplg');						{  Printing Plug-Ins Folder (in Extensions folder)  }


	{	 New Folder Types to accommodate the Mac OS X Folder Manager 	}
	{	 These folder types are not applicable on Mac OS 9.          	}
	kDomainTopLevelFolderType	= FourCharCode('dtop');						{  The top-level of a Folder domain, e.g. "/System" }
	kDomainLibraryFolderType	= FourCharCode('dlib');						{  the Library subfolder of a particular domain }
	kColorSyncFolderType		= FourCharCode('sync');						{  Contains ColorSync-related folders }
	kColorSyncCMMFolderType		= FourCharCode('ccmm');						{  ColorSync CMMs }
	kColorSyncScriptingFolderType = FourCharCode('cscr');						{  ColorSync Scripting support }
	kPrintersFolderType			= FourCharCode('impr');						{  Contains Printing-related folders }
	kSpeechFolderType			= FourCharCode('spch');						{  Contains Speech-related folders }
	kCarbonLibraryFolderType	= FourCharCode('carb');						{  Contains Carbon-specific file }
	kDocumentationFolderType	= FourCharCode('info');						{  Contains Documentation files (not user documents) }
	kDeveloperDocsFolderType	= FourCharCode('ddoc');						{  Contains Developer Documentation files and folders }
	kDeveloperHelpFolderType	= FourCharCode('devh');						{  Contains Developer Help related files }
	kISSDownloadsFolderType		= FourCharCode('issd');						{  Contains Internet Search Sites downloaded from the Internet }
	kUserSpecificTmpFolderType	= FourCharCode('utmp');						{  Contains temporary items created on behalf of the current user }
	kCachedDataFolderType		= FourCharCode('cach');						{  Contains various cache files for different clients }
	kFrameworksFolderType		= FourCharCode('fram');						{  Contains MacOS X Framework folders      }
	kPrivateFrameworksFolderType = FourCharCode('pfrm');						{  Contains MacOS X Private Framework folders      }
	kClassicDesktopFolderType	= FourCharCode('sdsk');						{  MacOS 9 compatible desktop folder - same as  }
																{  kSystemDesktopFolderType but with a more appropriate }
																{  name for Mac OS X code. }
	kDeveloperFolderType		= FourCharCode('devf');						{  Contains MacOS X Developer Resources }
	kSystemSoundsFolderType		= FourCharCode('ssnd');						{  Contains Mac OS X System Sound Files }
	kComponentsFolderType		= FourCharCode('cmpd');						{  Contains Mac OS X components }
	kQuickTimeComponentsFolderType = FourCharCode('wcmp');					{  Contains QuickTime components for Mac OS X }
	kCoreServicesFolderType		= FourCharCode('csrv');						{  Refers to the "CoreServices" folder on Mac OS X }
	kPictureDocumentsFolderType	= FourCharCode('pdoc');						{  Refers to the "Pictures" folder in a users home directory }
	kMovieDocumentsFolderType	= FourCharCode('mdoc');						{  Refers to the "Movies" folder in a users home directory }
	kMusicDocumentsFolderType	= FourCharCode('µdoc');						{  Refers to the "Music" folder in a users home directory }
	kInternetSitesFolderType	= FourCharCode('site');						{  Refers to the "Sites" folder in a users home directory }
	kPublicFolderType			= FourCharCode('pubb');						{  Refers to the "Public" folder in a users home directory }
	kAudioSupportFolderType		= FourCharCode('adio');						{  Refers to the Audio support folder for Mac OS X }
	kAudioSoundsFolderType		= FourCharCode('asnd');						{  Refers to the Sounds subfolder of Audio Support }
	kAudioSoundBanksFolderType	= FourCharCode('bank');						{  Refers to the Banks subfolder of the Sounds Folder }
	kAudioAlertSoundsFolderType	= FourCharCode('alrt');						{  Refers to the Alert Sounds subfolder of the Sound Folder }
	kAudioPlugInsFolderType		= FourCharCode('aplg');						{  Refers to the Plug-ins subfolder of the Audio Folder    }
	kAudioComponentsFolderType	= FourCharCode('acmp');						{  Refers to the Components subfolder of the Audio Plug-ins Folder     }
	kKernelExtensionsFolderType	= FourCharCode('kext');						{  Refers to the Kernel Extensions Folder on Mac OS X }
	kDirectoryServicesFolderType = FourCharCode('dsrv');						{  Refers to the Directory Services folder on Mac OS X }
	kDirectoryServicesPlugInsFolderType = FourCharCode('dplg');				{  Refers to the Directory Services Plug-Ins folder on Mac OS X  }
	kInstallerReceiptsFolderType = FourCharCode('rcpt');						{  Refers to the "Receipts" folder in Mac OS X }
	kFileSystemSupportFolderType = FourCharCode('fsys');						{  Refers to the [domain]/Library/Filesystems folder in Mac OS X }
	kAppleShareSupportFolderType = FourCharCode('shar');						{  Refers to the [domain]/Library/Filesystems/AppleShare folder in Mac OS X }
	kAppleShareAuthenticationFolderType = FourCharCode('auth');				{  Refers to the [domain]/Library/Filesystems/AppleShare/Authentication folder in Mac OS X }
	kMIDIDriversFolderType		= FourCharCode('midi');						{  Refers to the MIDI Drivers folder on Mac OS X }

	kLocalesFolderType			= FourCharCode('Äloc');						{  PKE for Locales folder  }
	kFindByContentPluginsFolderType = FourCharCode('fbcp');					{  Find By Content Plug-ins  }

	kUsersFolderType			= FourCharCode('usrs');						{  "Users" folder, contains one folder for each user.  }
	kCurrentUserFolderType		= FourCharCode('cusr');						{  The folder for the currently logged on user.  }
	kCurrentUserRemoteFolderLocation = FourCharCode('rusf');					{  The remote folder for the currently logged on user  }
	kCurrentUserRemoteFolderType = FourCharCode('rusr');						{  The remote folder location for the currently logged on user  }
	kSharedUserDataFolderType	= FourCharCode('sdat');						{  A Shared "Documents" folder, readable & writeable by all users  }
	kVolumeSettingsFolderType	= FourCharCode('vsfd');						{  Volume specific user information goes here  }

	kAppleshareAutomountServerAliasesFolderType = FourCharCode('srvÄ');		{  Appleshare puts volumes to automount inside this folder.  }
	kPreMacOS91ApplicationsFolderType = FourCharCode('Œpps');					{  The "Applications" folder, pre Mac OS 9.1  }
	kPreMacOS91InstallerLogsFolderType = FourCharCode('”lgf');				{  The "Installer Logs" folder, pre Mac OS 9.1  }
	kPreMacOS91AssistantsFolderType = FourCharCode('ŒstÄ');					{  The "Assistants" folder, pre Mac OS 9.1  }
	kPreMacOS91UtilitiesFolderType = FourCharCode('ŸtiÄ');					{  The "Utilities" folder, pre Mac OS 9.1  }
	kPreMacOS91AppleExtrasFolderType = FourCharCode('ŒexÄ');					{  The "Apple Extras" folder, pre Mac OS 9.1  }
	kPreMacOS91MacOSReadMesFolderType = FourCharCode('µorÄ');					{  The "Mac OS ReadMes" folder, pre Mac OS 9.1  }
	kPreMacOS91InternetFolderType = FourCharCode('”ntÄ');						{  The "Internet" folder, pre Mac OS 9.1  }
	kPreMacOS91AutomountedServersFolderType = FourCharCode('§rvÄ');			{  The "Servers" folder, pre Mac OS 9.1  }
	kPreMacOS91StationeryFolderType = FourCharCode('¿dst');					{  The "Stationery" folder, pre Mac OS 9.1  }

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
	kRelativeFolder				= FourCharCode('relf');
	kSpecialFolder				= FourCharCode('spcf');


type
	FolderClass							= OSType;
	{	 special folder locations 	}

const
	kBlessedFolder				= FourCharCode('blsf');
	kRootFolder					= FourCharCode('rotf');

	kCurrentUserFolderLocation	= FourCharCode('cusf');						{     the magic 'Current User' folder location }


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
		currentUserFolderDirID:	DirIDType;
		remoteUserFolderVRefNum: SInt16;
		remoteUserFolderDirID:	DirIDType;
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
	kFolderManagerNotificationMessageUserLogIn = FourCharCode('log+');		{     Sent by system & third party software after a user logs in.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationMessagePreUserLogIn = FourCharCode('logj');		{     Sent by system & third party software before a user logs in.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationMessageUserLogOut = FourCharCode('log-');		{     Sent by system & third party software before a user logs out.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationMessagePostUserLogOut = FourCharCode('logp');	{     Sent by system & third party software after a user logs out.  arg should point to a valid FindFolderUserRedirectionGlobals structure or nil for the owner }
	kFolderManagerNotificationDiscardCachedData = FourCharCode('dche');		{     Sent by system & third party software when the entire Folder Manager cache should be flushed }
	kFolderManagerNotificationMessageLoginStartup = FourCharCode('stup');		{     Sent by 'Login' application the first time it starts up after each boot }


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
function InvalidateFolderDescriptorCache(vRefNum: SInt16; dirID: DirIDType): OSErr; external name '_InvalidateFolderDescriptorCache';
{
 *  IdentifyFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in FoldersLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IdentifyFolder(vRefNum: SInt16; dirID: DirIDType; var foldType: FolderType): OSErr; external name '_IdentifyFolder';
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
		giDocsDirID:			DirIDType;							{  directory id of user's documents folder (only valid if not on floppy) }
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
		giPrefsDirID:			DirIDType;							{  dirID of the At Ease Items folder on preferences volume }
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
