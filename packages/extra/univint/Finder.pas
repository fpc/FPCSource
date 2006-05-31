{
     File:       Finder.p
 
     Contains:   Finder flags and container types.
 
     Version:    Technology: Mac OS 8.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1990-2002 by Apple Computer, Inc., all rights reserved
 
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

unit Finder;
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
uses MacTypes;


{$ALIGN MAC68K}

{ Creator and type of clipping files }

const
	kClippingCreator			= $64726167 (* 'drag' *);
	kClippingPictureType		= $636C7070 (* 'clpp' *);
	kClippingTextType			= $636C7074 (* 'clpt' *);
	kClippingSoundType			= $636C7073 (* 'clps' *);
	kClippingUnknownType		= $636C7075 (* 'clpu' *);


	{	 Creator and type of Internet Location files 	}
	kInternetLocationCreator	= $64726167 (* 'drag' *);
	kInternetLocationHTTP		= $696C6874 (* 'ilht' *);
	kInternetLocationFTP		= $696C6674 (* 'ilft' *);
	kInternetLocationFile		= $696C6669 (* 'ilfi' *);
	kInternetLocationMail		= $696C6D61 (* 'ilma' *);
	kInternetLocationNNTP		= $696C6E77 (* 'ilnw' *);
	kInternetLocationAFP		= $696C6166 (* 'ilaf' *);
	kInternetLocationAppleTalk	= $696C6174 (* 'ilat' *);
	kInternetLocationNSL		= $696C6E73 (* 'ilns' *);
	kInternetLocationGeneric	= $696C6765 (* 'ilge' *);


	kCustomIconResource			= -16455;						{  Custom icon family resource ID  }

	{	 In order to specify any of the information described in the 	}
	{	 CustomBadgeResource data structure you must clear the kExtendedFlagsAreInvalid 	}
	{	 and set kExtendedFlagHasCustomBadge of the FXInfo.fdXFlags or DXInfo.frXFlags field, 	}
	{	 and add a resource of type kCustomBadgeResourceType and ID kCustomBadgeResourceID to 	}
	{	 the file or to the "Icon/n" file for a folder 	}
	kCustomBadgeResourceType	= $62616467 (* 'badg' *);
	kCustomBadgeResourceID		= -16455;
	kCustomBadgeResourceVersion	= 0;


type
	CustomBadgeResourcePtr = ^CustomBadgeResource;
	CustomBadgeResource = record
		version:				SInt16;									{  This is version kCustomBadgeResourceVersion }
		customBadgeResourceID:	SInt16;									{  If not 0, the ID of a resource to use on top }
																		{  of the icon for this file or folder }
		customBadgeType:		OSType;									{  If not 0, the type and creator of an icon }
		customBadgeCreator:		OSType;									{  to use on top of the icon }
		windowBadgeType:		OSType;									{  If not 0, the type and creator of an icon }
		windowBadgeCreator:		OSType;									{  to display in the header of the window for this  }
																		{  file or folder }
		overrideType:			OSType;									{  If not 0, the type and creator of an icon to }
		overrideCreator:		OSType;									{  use INSTEAD of the icon for this file or folder }
	end;

	CustomBadgeResourceHandle			= ^CustomBadgeResourcePtr;
	{	 You can specify routing information for a file by including a 'rout' 0 
	    resource in it and setting the kExtendedFlagHasRoutingInfo bit in the extended 
	    Finder flags. 
	    The 'rout' resource is an array of RoutingResourceEntry. Each entry is considered
	    in turn. The first matching entry is used.
	    If the creator and fileType match the file being dropped and targetFolder match
	    the folder ID of the folder being dropped onto, then the file is rerouted 
	    into the specified destination folder.
	    The only target folder currently supported is the system folder, 
	    kSystemFolderType = 'macs'.
		}

const
	kRoutingResourceType		= $726F7574 (* 'rout' *);
	kRoutingResourceID			= 0;


type
	RoutingResourceEntryPtr = ^RoutingResourceEntry;
	RoutingResourceEntry = record
		creator:				OSType;									{  Use '****' or 0 to match any creator  }
		fileType:				OSType;									{  Use '****' or 0 to match any file type  }
		targetFolder:			OSType;									{  Folder ID of the folder this file was dropped onto  }
		destinationFolder:		OSType;									{  Folder that the source will be routed to  }
		reservedField:			OSType;									{  Set to 0  }
	end;

	RoutingResourcePtr					= ^RoutingResourceEntry;
	RoutingResourceHandle				= ^RoutingResourcePtr;

	{	 Types for special container aliases 	}

const
	kContainerFolderAliasType	= $66647270 (* 'fdrp' *);						{  type for folder aliases  }
	kContainerTrashAliasType	= $74727368 (* 'trsh' *);						{  type for trash folder aliases  }
	kContainerHardDiskAliasType	= $6864736B (* 'hdsk' *);						{  type for hard disk aliases  }
	kContainerFloppyAliasType	= $666C7079 (* 'flpy' *);						{  type for floppy aliases  }
	kContainerServerAliasType	= $73727672 (* 'srvr' *);						{  type for server aliases  }
	kApplicationAliasType		= $61647270 (* 'adrp' *);						{  type for application aliases  }
	kContainerAliasType			= $64726F70 (* 'drop' *);						{  type for all other containers  }
	kDesktopPrinterAliasType	= $64747061 (* 'dtpa' *);						{  type for Desktop Printer alias  }
	kContainerCDROMAliasType	= $63646472 (* 'cddr' *);						{  type for CD-ROM alias  }
	kApplicationCPAliasType		= $61636470 (* 'acdp' *);						{  type for application control panel alias  }
	kApplicationDAAliasType		= $61646470 (* 'addp' *);						{  type for application DA alias  }
	kPackageAliasType			= $66706B61 (* 'fpka' *);						{  type for plain package alias  }
	kAppPackageAliasType		= $66617061 (* 'fapa' *);						{  type for application package alias  }

	{	 Types for Special folder aliases 	}
	kSystemFolderAliasType		= $66617379 (* 'fasy' *);
	kAppleMenuFolderAliasType	= $6661616D (* 'faam' *);
	kStartupFolderAliasType		= $66617374 (* 'fast' *);
	kPrintMonitorDocsFolderAliasType = $6661706E (* 'fapn' *);
	kPreferencesFolderAliasType	= $66617066 (* 'fapf' *);
	kControlPanelFolderAliasType = $66616374 (* 'fact' *);
	kExtensionFolderAliasType	= $66616578 (* 'faex' *);

	{	 Types for AppleShare folder aliases 	}
	kExportedFolderAliasType	= $66616574 (* 'faet' *);
	kDropFolderAliasType		= $66616472 (* 'fadr' *);
	kSharedFolderAliasType		= $66617368 (* 'fash' *);
	kMountedFolderAliasType		= $66616D6E (* 'famn' *);

	{	 Finder flags (finderFlags, fdFlags and frFlags) 	}
	{	 Any flag reserved or not specified should be set to 0. 	}
	{	 If a flag applies to a file, but not to a folder, make sure to check 	}
	{	 that the item is not a folder by checking ((ParamBlockRec.ioFlAttrib & ioDirMask) == 0) 	}
	kIsOnDesk					= $0001;						{  Files and folders (System 6)  }
	kColor						= $000E;						{  Files and folders  }
																{  bit 0x0020 was kRequireSwitchLaunch, but is now reserved for future use }
	kIsShared					= $0040;						{  Files only (Applications only)  }
																{  If clear, the application needs to write to  }
																{  its resource fork, and therefore cannot be  }
																{  shared on a server  }
	kHasNoINITs					= $0080;						{  Files only (Extensions/Control Panels only)  }
																{  This file contains no INIT resource  }
	kHasBeenInited				= $0100;						{  Files only  }
																{  Clear if the file contains desktop database  }
																{  resources ('BNDL', 'FREF', 'open', 'kind'...)  }
																{  that have not been added yet. Set only by the Finder  }
																{  Reserved for folders - make sure this bit is cleared for folders  }
																{  bit 0x0200 was the letter bit for AOCE, but is now reserved for future use  }
	kHasCustomIcon				= $0400;						{  Files and folders  }
	kIsStationery				= $0800;						{  Files only  }
	kNameLocked					= $1000;						{  Files and folders  }
	kHasBundle					= $2000;						{  Files only  }
	kIsInvisible				= $4000;						{  Files and folders  }
	kIsAlias					= $8000;						{  Files only  }

	{	 Obsolete. Use names defined above. 	}
	fOnDesk						= $0001;
	fHasBundle					= $2000;
	fInvisible					= $4000;

	{	 Obsolete 	}
	fTrash						= -3;
	fDesktop					= -2;
	fDisk						= 0;

{$ifc OLDROUTINENAMES}
	kIsStationary				= $0800;

{$endc}  {OLDROUTINENAMES}

	{	 Extended flags (extendedFinderFlags, fdXFlags and frXFlags) 	}
	{	 Any flag not specified should be set to 0. 	}
	kExtendedFlagsAreInvalid	= $8000;						{  If set the other extended flags are ignored  }
	kExtendedFlagHasCustomBadge	= $0100;						{  Set if the file or folder has a badge resource  }
	kExtendedFlagHasRoutingInfo	= $0004;						{  Set if the file contains routing info resource  }


	{	 Use a filetype in this range to indicate that a file is temporarily busy 	}
	{	 (while it is being downloaded or installed, for example).  This prevents 	}
	{	 Finder 8.5 and later from trying to change the item's attributes before it 	}
	{	 is fully created. -- If you provide a series of 'BNDL' icons for your creator 	}
	{	 and some of these filetypes, you can achieve limited icon animation while 	}
	{	 the file creation progresses. 	}
	kFirstMagicBusyFiletype		= $627A7920 (* 'bzy ' *);
	kLastMagicBusyFiletype		= $627A793F (* 'bzy?' *);

	{	 Use this date as a file's or folder's creation date to indicate that it is 	}
	{	 temporarily busy (while it is being downloaded or installed, for example). 	}
	{	 This prevents Finder from trying to change the item's attributes before it 	}
	{	 is fully created (Finder 8.5 and 8.6 check file creation dates; later Finders 	}
	{	 may check folder creation dates as well). 	}
	kMagicBusyCreationDate		= $4F3AFDB0;


	{	------------------------------------------------------------------------	}
	{
	   The following data structures are binary compatible with FInfo, DInfo,
	   FXInfo and DXInfo but represent the Mac OS 8 semantic of the fields.
	   Use these data structures preferably to FInfo, etc...
	}
	{	------------------------------------------------------------------------	}


type
	FileInfoPtr = ^FileInfo;
	FileInfo = record
		fileType:				OSType;									{  The type of the file  }
		fileCreator:			OSType;									{  The file's creator  }
		finderFlags:			UInt16;									{  ex: kHasBundle, kIsInvisible...  }
		location:				Point;									{  File's location in the folder  }
																		{  If set to (0, 0), the Finder will place the item automatically  }
		reservedField:			UInt16;									{  (set to 0)  }
	end;

	FolderInfoPtr = ^FolderInfo;
	FolderInfo = record
		windowBounds:			Rect;									{  The position and dimension of the folder's window  }
		finderFlags:			UInt16;									{  ex. kIsInvisible, kNameLocked, etc. }
		location:				Point;									{  Folder's location in the parent folder  }
																		{  If set to (0, 0), the Finder will place the item automatically  }
		reservedField:			UInt16;									{  (set to 0)  }
	end;

	ExtendedFileInfoPtr = ^ExtendedFileInfo;
	ExtendedFileInfo = record
		reserved1:				array [0..3] of SInt16;					{  Reserved (set to 0)  }
		extendedFinderFlags:	UInt16;									{  Extended flags (custom badge, routing info...)  }
		reserved2:				SInt16;									{  Reserved (set to 0). Comment ID if high-bit is clear  }
		putAwayFolderID:		SInt32;									{  Put away folder ID  }
	end;

	ExtendedFolderInfoPtr = ^ExtendedFolderInfo;
	ExtendedFolderInfo = record
		scrollPosition:			Point;									{  Scroll position (for icon views)  }
		reserved1:				SInt32;									{  Reserved (set to 0)  }
		extendedFinderFlags:	UInt16;									{  Extended flags (custom badge, routing info...)  }
		reserved2:				SInt16;									{  Reserved (set to 0). Comment ID if high-bit is clear  }
		putAwayFolderID:		SInt32;									{  Put away folder ID  }
	end;

	{	------------------------------------------------------------------------	}
	{
	   The following data structures are here for compatibility.
	   Use the new data structures replacing them if possible (i.e. FileInfo 
	   instead of FInfo, etc...)
	}
	{	------------------------------------------------------------------------	}
	{	 File info 	}
	{
	     IMPORTANT:
	     In MacOS 8, the fdFldr field has become reserved for the Finder.
	}
	FInfoPtr = ^FInfo;
	FInfo = record
		fdType:					OSType;									{  The type of the file  }
		fdCreator:				OSType;									{  The file's creator  }
		fdFlags:				UInt16;									{  Flags ex. kHasBundle, kIsInvisible, etc.  }
		fdLocation:				Point;									{  File's location in folder.  }
																		{  If set to (0, 0), the Finder will place the item automatically  }
		fdFldr:					SInt16;									{  Reserved (set to 0)  }
	end;

	{	 Extended file info 	}
	{
	     IMPORTANT:
	     In MacOS 8, the fdIconID and fdComment fields were changed
	     to become reserved fields for the Finder.
	     The fdScript has become an extended flag.
	}
	FXInfoPtr = ^FXInfo;
	FXInfo = record
		fdIconID:				SInt16;									{  Reserved (set to 0)  }
		fdReserved:				array [0..2] of SInt16;					{  Reserved (set to 0)  }
		fdScript:				SInt8;									{  Extended flags. Script code if high-bit is set  }
		fdXFlags:				SInt8;									{  Extended flags  }
		fdComment:				SInt16;									{  Reserved (set to 0). Comment ID if high-bit is clear  }
		fdPutAway:				SInt32;									{  Put away folder ID  }
	end;

	{	 Folder info 	}
	{
	     IMPORTANT:
	     In MacOS 8, the frView field was changed to become reserved 
	     field for the Finder.
	}
	DInfoPtr = ^DInfo;
	DInfo = record
		frRect:					Rect;									{  Folder's window bounds  }
		frFlags:				UInt16;									{  Flags ex. kIsInvisible, kNameLocked, etc. }
		frLocation:				Point;									{  Folder's location in parent folder  }
																		{  If set to (0, 0), the Finder will place the item automatically  }
		frView:					SInt16;									{  Reserved (set to 0)  }
	end;

	{	 Extended folder info 	}
	{
	     IMPORTANT:
	     In MacOS 8, the frOpenChain and frComment fields were changed
	     to become reserved fields for the Finder.
	     The frScript has become an extended flag.
	}
	DXInfoPtr = ^DXInfo;
	DXInfo = record
		frScroll:				Point;									{  Scroll position  }
		frOpenChain:			SInt32;									{  Reserved (set to 0)  }
		frScript:				SInt8;									{  Extended flags. Script code if high-bit is set  }
		frXFlags:				SInt8;									{  Extended flags  }
		frComment:				SInt16;									{  Reserved (set to 0). Comment ID if high-bit is clear  }
		frPutAway:				SInt32;									{  Put away folder ID  }
	end;

	{  ControlPanelDefProcPtr and cdev constants have all been moved to Processes.i }
{$ALIGN MAC68K}


end.
