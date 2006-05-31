{
     File:       Files.p
 
     Contains:   File Manager (MFS, HFS, and HFS+) Interfaces.
 
     Version:    Technology: Mac OS 9
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
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

unit Files;
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
uses MacTypes,MixedMode,OSUtils,TextCommon,UTCUtils,Finder;

{ Finder constants were moved to Finder.Å }


{$ALIGN MAC68K}

{ HFSUniStr255 is the Unicode equivalent of Str255 }

type
	HFSUniStr255Ptr = ^HFSUniStr255;
	HFSUniStr255 = record
		length:					UInt16;									{  number of unicode characters  }
		unicode:				array [0..254] of UniChar;				{  unicode characters  }
	end;

	ConstHFSUniStr255Param				= ^HFSUniStr255;

const
	fsCurPerm					= $00;							{  open access permissions in ioPermssn  }
	fsRdPerm					= $01;
	fsWrPerm					= $02;
	fsRdWrPerm					= $03;
	fsRdWrShPerm				= $04;
	fsRdDenyPerm				= $10;							{  for use with OpenDeny and OpenRFDeny  }
	fsWrDenyPerm				= $20;							{  for use with OpenDeny and OpenRFDeny  }

	fsRtParID					= 1;
	fsRtDirID					= 2;

	fsAtMark					= 0;							{  positioning modes in ioPosMode  }
	fsFromStart					= 1;
	fsFromLEOF					= 2;
	fsFromMark					= 3;

																{  ioPosMode flags  }
	pleaseCacheBit				= 4;							{  please cache this request  }
	pleaseCacheMask				= $0010;
	noCacheBit					= 5;							{  please don't cache this request  }
	noCacheMask					= $0020;
	rdVerifyBit					= 6;							{  read verify mode  }
	rdVerifyMask				= $0040;
	rdVerify					= 64;							{  old name of rdVerifyMask  }
	forceReadBit				= 6;
	forceReadMask				= $0040;
	newLineBit					= 7;							{  newline mode  }
	newLineMask					= $0080;
	newLineCharMask				= $FF00;						{  newline character  }


																{  CatSearch Search bitmask Constants  }
	fsSBPartialName				= 1;
	fsSBFullName				= 2;
	fsSBFlAttrib				= 4;
	fsSBFlFndrInfo				= 8;
	fsSBFlLgLen					= 32;
	fsSBFlPyLen					= 64;
	fsSBFlRLgLen				= 128;
	fsSBFlRPyLen				= 256;
	fsSBFlCrDat					= 512;
	fsSBFlMdDat					= 1024;
	fsSBFlBkDat					= 2048;
	fsSBFlXFndrInfo				= 4096;
	fsSBFlParID					= 8192;
	fsSBNegate					= 16384;
	fsSBDrUsrWds				= 8;
	fsSBDrNmFls					= 16;
	fsSBDrCrDat					= 512;
	fsSBDrMdDat					= 1024;
	fsSBDrBkDat					= 2048;
	fsSBDrFndrInfo				= 4096;
	fsSBDrParID					= 8192;

																{  CatSearch Search bit value Constants  }
	fsSBPartialNameBit			= 0;							{ ioFileName points to a substring }
	fsSBFullNameBit				= 1;							{ ioFileName points to a match string }
	fsSBFlAttribBit				= 2;							{ search includes file attributes }
	fsSBFlFndrInfoBit			= 3;							{ search includes finder info }
	fsSBFlLgLenBit				= 5;							{ search includes data logical length }
	fsSBFlPyLenBit				= 6;							{ search includes data physical length }
	fsSBFlRLgLenBit				= 7;							{ search includes resource logical length }
	fsSBFlRPyLenBit				= 8;							{ search includes resource physical length }
	fsSBFlCrDatBit				= 9;							{ search includes create date }
	fsSBFlMdDatBit				= 10;							{ search includes modification date }
	fsSBFlBkDatBit				= 11;							{ search includes backup date }
	fsSBFlXFndrInfoBit			= 12;							{ search includes extended finder info }
	fsSBFlParIDBit				= 13;							{ search includes file's parent ID }
	fsSBNegateBit				= 14;							{ return all non-matches }
	fsSBDrUsrWdsBit				= 3;							{ search includes directory finder info }
	fsSBDrNmFlsBit				= 4;							{ search includes directory valence }
	fsSBDrCrDatBit				= 9;							{ directory-named version of fsSBFlCrDatBit }
	fsSBDrMdDatBit				= 10;							{ directory-named version of fsSBFlMdDatBit }
	fsSBDrBkDatBit				= 11;							{ directory-named version of fsSBFlBkDatBit }
	fsSBDrFndrInfoBit			= 12;							{ directory-named version of fsSBFlXFndrInfoBit }
	fsSBDrParIDBit				= 13;							{ directory-named version of fsSBFlParIDBit }

																{  vMAttrib (GetVolParms) bit position constants  }
	bLimitFCBs					= 31;
	bLocalWList					= 30;
	bNoMiniFndr					= 29;
	bNoVNEdit					= 28;
	bNoLclSync					= 27;
	bTrshOffLine				= 26;
	bNoSwitchTo					= 25;
	bDontShareIt				= 21;							{  this volume should not be shared by Macintosh File Sharing (see Technical Note NW 29)  }
	bNoDeskItems				= 20;
	bNoBootBlks					= 19;
	bAccessCntl					= 18;
	bNoSysDir					= 17;
	bHasExtFSVol				= 16;
	bHasOpenDeny				= 15;
	bHasCopyFile				= 14;
	bHasMoveRename				= 13;
	bHasDesktopMgr				= 12;
	bHasShortName				= 11;
	bHasFolderLock				= 10;
	bHasPersonalAccessPrivileges = 9;
	bHasUserGroupList			= 8;
	bHasCatSearch				= 7;
	bHasFileIDs					= 6;
	bHasBTreeMgr				= 5;
	bHasBlankAccessPrivileges	= 4;
	bSupportsAsyncRequests		= 3;							{  asynchronous requests to this volume are handled correctly at any time }
	bSupportsTrashVolumeCache	= 2;

																{  vMExtendedAttributes (GetVolParms) bit position constants  }
	bIsEjectable				= 0;							{  volume is in an ejectable disk drive  }
	bSupportsHFSPlusAPIs		= 1;							{  volume supports HFS Plus APIs directly (not through compatibility layer)  }
	bSupportsFSCatalogSearch	= 2;							{  volume supports FSCatalogSearch  }
	bSupportsFSExchangeObjects	= 3;							{  volume supports FSExchangeObjects  }
	bSupports2TBFiles			= 4;							{  volume supports supports 2 terabyte files  }
	bSupportsLongNames			= 5;							{  volume supports file/directory/volume names longer than 31 characters  }
	bSupportsMultiScriptNames	= 6;							{  volume supports file/directory/volume names with characters from multiple script systems  }
	bSupportsNamedForks			= 7;							{  volume supports forks beyond the data and resource forks  }
	bSupportsSubtreeIterators	= 8;							{  volume supports recursive iterators not at the volume root  }
	bL2PCanMapFileBlocks		= 9;							{  volume supports Lg2Phys SPI correctly  }

																{  vMExtendedAttributes (GetVolParms) bit position constants  }
	bParentModDateChanges		= 10;							{  Changing a file or folder causes its parent's mod date to change  }
	bAncestorModDateChanges		= 11;							{  Changing a file or folder causes all ancestor mod dates to change  }

																{  vMExtendedAttributes (GetVolParms) bit position constants  }
	bSupportsSymbolicLinks		= 13;							{  volume supports the creation and use of symbolic links (Mac OS X only)  }
	bIsAutoMounted				= 14;							{  volume was mounted automatically (Mac OS X only)  }
	bAllowCDiDataHandler		= 17;							{  allow QuickTime's CDi data handler to examine this volume  }

																{  Desktop Database, ffsGetIconMessage and fsmGetFSIconMessage icon type and size Constants  }
	kLargeIcon					= 1;
	kLarge4BitIcon				= 2;
	kLarge8BitIcon				= 3;
	kSmallIcon					= 4;
	kSmall4BitIcon				= 5;
	kSmall8BitIcon				= 6;
	kicnsIconFamily				= 239;							{  Note: The 'icns' icon family record is variable sized.  }

	kLargeIconSize				= 256;
	kLarge4BitIconSize			= 512;
	kLarge8BitIconSize			= 1024;
	kSmallIconSize				= 64;
	kSmall4BitIconSize			= 128;
	kSmall8BitIconSize			= 256;

																{  Large Volume Constants  }
	kWidePosOffsetBit			= 8;
	kUseWidePositioning			= $0100;
	kMaximumBlocksIn4GB			= $007FFFFF;

																{  Foreign Privilege Model Identifiers  }
	fsUnixPriv					= 1;

																{  Authentication Constants  }
	kNoUserAuthentication		= 1;
	kPassword					= 2;
	kEncryptPassword			= 3;
	kTwoWayEncryptPassword		= 6;


	{	 mapping codes (ioObjType) for MapName & MapID 	}
	kOwnerID2Name				= 1;
	kGroupID2Name				= 2;
	kOwnerName2ID				= 3;
	kGroupName2ID				= 4;							{  types of oj object to be returned (ioObjType) for _GetUGEntry  }
	kReturnNextUser				= 1;
	kReturnNextGroup			= 2;
	kReturnNextUG				= 3;

	{	 vcbFlags bits 	}
	kVCBFlagsIdleFlushBit		= 3;							{  Set if volume should be flushed at idle time  }
	kVCBFlagsIdleFlushMask		= $0008;
	kVCBFlagsHFSPlusAPIsBit		= 4;							{  Set if volume implements HFS Plus APIs itself (not via emulation)  }
	kVCBFlagsHFSPlusAPIsMask	= $0010;
	kVCBFlagsHardwareGoneBit	= 5;							{  Set if disk driver returned a hardwareGoneErr to Read or Write  }
	kVCBFlagsHardwareGoneMask	= $0020;
	kVCBFlagsVolumeDirtyBit		= 15;							{  Set if volume information has changed since the last FlushVol  }
	kVCBFlagsVolumeDirtyMask	= $8000;

	{	 ioVAtrb bits returned by PBHGetVInfo and PBXGetVolInfo 	}
	kioVAtrbDefaultVolumeBit	= 5;							{  Set if the volume is the default volume  }
	kioVAtrbDefaultVolumeMask	= $0020;
	kioVAtrbFilesOpenBit		= 6;							{  Set if there are open files or iterators  }
	kioVAtrbFilesOpenMask		= $0040;
	kioVAtrbHardwareLockedBit	= 7;							{  Set if volume is locked by a hardware setting  }
	kioVAtrbHardwareLockedMask	= $0080;
	kioVAtrbSoftwareLockedBit	= 15;							{  Set if volume is locked by software  }
	kioVAtrbSoftwareLockedMask	= $8000;

	{	 ioFlAttrib bits returned by PBGetCatInfo 	}
																{  file and directory attributes in ioFlAttrib  }
	kioFlAttribLockedBit		= 0;							{  Set if file or directory is locked  }
	kioFlAttribLockedMask		= $01;
	kioFlAttribResOpenBit		= 2;							{  Set if resource fork is open  }
	kioFlAttribResOpenMask		= $04;
	kioFlAttribDataOpenBit		= 3;							{  Set if data fork is open  }
	kioFlAttribDataOpenMask		= $08;
	kioFlAttribDirBit			= 4;							{  Set if this is a directory  }
	kioFlAttribDirMask			= $10;
	ioDirFlg					= 4;							{  Set if this is a directory (old name)  }
	ioDirMask					= $10;
	kioFlAttribCopyProtBit		= 6;							{  Set if AppleShare server "copy-protects" the file  }
	kioFlAttribCopyProtMask		= $40;
	kioFlAttribFileOpenBit		= 7;							{  Set if file (either fork) is open  }
	kioFlAttribFileOpenMask		= $80;							{  ioFlAttrib for directories only  }
	kioFlAttribInSharedBit		= 2;							{  Set if the directory is within a shared area of the directory hierarchy  }
	kioFlAttribInSharedMask		= $04;
	kioFlAttribMountedBit		= 3;							{  Set if the directory is a share point that is mounted by some user  }
	kioFlAttribMountedMask		= $08;
	kioFlAttribSharePointBit	= 5;							{  Set if the directory is a share point  }
	kioFlAttribSharePointMask	= $20;

	{	 ioFCBFlags bits returned by PBGetFCBInfo 	}
	kioFCBWriteBit				= 8;							{  Data can be written to this file  }
	kioFCBWriteMask				= $0100;
	kioFCBResourceBit			= 9;							{  This file is a resource fork  }
	kioFCBResourceMask			= $0200;
	kioFCBWriteLockedBit		= 10;							{  File has a locked byte range  }
	kioFCBWriteLockedMask		= $0400;
	kioFCBLargeFileBit			= 11;							{  File may grow beyond 2GB; cache uses file blocks, not bytes  }
	kioFCBLargeFileMask			= $0800;
	kioFCBSharedWriteBit		= 12;							{  File is open for shared write access  }
	kioFCBSharedWriteMask		= $1000;
	kioFCBFileLockedBit			= 13;							{  File is locked (write-protected)  }
	kioFCBFileLockedMask		= $2000;
	kioFCBOwnClumpBit			= 14;							{  File has clump size specified in FCB  }
	kioFCBOwnClumpMask			= $4000;
	kioFCBModifiedBit			= 15;							{  File has changed since it was last flushed  }
	kioFCBModifiedMask			= $8000;

	{	 ioACUser bits returned by PBGetCatInfo 	}
	{	 Note: you must clear ioACUser before calling PBGetCatInfo because some file systems do not use this field 	}
	kioACUserNoSeeFolderBit		= 0;							{  Set if user does not have See Folder privileges  }
	kioACUserNoSeeFolderMask	= $01;
	kioACUserNoSeeFilesBit		= 1;							{  Set if user does not have See Files privileges  }
	kioACUserNoSeeFilesMask		= $02;
	kioACUserNoMakeChangesBit	= 2;							{  Set if user does not have Make Changes privileges  }
	kioACUserNoMakeChangesMask	= $04;
	kioACUserNotOwnerBit		= 7;							{  Set if user is not owner of the directory  }
	kioACUserNotOwnerMask		= $80;

	{	 Folder and File values of access privileges in ioACAccess 	}
	kioACAccessOwnerBit			= 31;							{  User is owner of directory  }
	kioACAccessOwnerMask		= $80000000;
	kioACAccessBlankAccessBit	= 28;							{  Directory has blank access privileges  }
	kioACAccessBlankAccessMask	= $10000000;
	kioACAccessUserWriteBit		= 26;							{  User has write privileges  }
	kioACAccessUserWriteMask	= $04000000;
	kioACAccessUserReadBit		= 25;							{  User has read privileges  }
	kioACAccessUserReadMask		= $02000000;
	kioACAccessUserSearchBit	= 24;							{  User has search privileges  }
	kioACAccessUserSearchMask	= $01000000;
	kioACAccessEveryoneWriteBit	= 18;							{  Everyone has write privileges  }
	kioACAccessEveryoneWriteMask = $00040000;
	kioACAccessEveryoneReadBit	= 17;							{  Everyone has read privileges  }
	kioACAccessEveryoneReadMask	= $00020000;
	kioACAccessEveryoneSearchBit = 16;							{  Everyone has search privileges  }
	kioACAccessEveryoneSearchMask = $00010000;
	kioACAccessGroupWriteBit	= 10;							{  Group has write privileges  }
	kioACAccessGroupWriteMask	= $00000400;
	kioACAccessGroupReadBit		= 9;							{  Group has read privileges  }
	kioACAccessGroupReadMask	= $00000200;
	kioACAccessGroupSearchBit	= 8;							{  Group has search privileges  }
	kioACAccessGroupSearchMask	= $00000100;
	kioACAccessOwnerWriteBit	= 2;							{  Owner has write privileges  }
	kioACAccessOwnerWriteMask	= $00000004;
	kioACAccessOwnerReadBit		= 1;							{  Owner has read privileges  }
	kioACAccessOwnerReadMask	= $00000002;
	kioACAccessOwnerSearchBit	= 0;							{  Owner has search privileges  }
	kioACAccessOwnerSearchMask	= $00000001;
	kfullPrivileges				= $00070007;					{  all privileges for everybody and owner }
	kownerPrivileges			= $00000007;					{  all privileges for owner only }

	{	 values of user IDs and group IDs 	}
	knoUser						= 0;
	kadministratorUser			= 1;

	knoGroup					= 0;


type
	GetVolParmsInfoBufferPtr = ^GetVolParmsInfoBuffer;
	GetVolParmsInfoBuffer = record
		vMVersion:				SInt16;								{ version number }
		vMAttrib:				SInt32;								{ bit vector of attributes (see vMAttrib constants) }
		vMLocalHand:			Handle;									{ handle to private data }
		vMServerAdr:			SInt32;								{ AppleTalk server address or zero }
																		{        vMVersion 1 GetVolParmsInfoBuffer ends here  }
		vMVolumeGrade:			SInt32;								{ approx. speed rating or zero if unrated }
		vMForeignPrivID:		SInt16;								{ foreign privilege model supported or zero if none }
																		{        vMVersion 2 GetVolParmsInfoBuffer ends here  }
		vMExtendedAttributes:	SInt32;								{ extended attribute bits (see vMExtendedAttributes constants) }
																		{        vMVersion 3 GetVolParmsInfoBuffer ends here  }
		vMDeviceID:				Ptr;									{  device id name for interoperability with IOKit  }
																		{        vMVersion 4 GetVolParmsInfoBuffer ends here  }
	end;

	ParmBlkPtr							= ^ParamBlockRec;
{$ifc TYPED_FUNCTION_POINTERS}
	IOCompletionProcPtr = procedure(paramBlock: ParmBlkPtr);
{$elsec}
	IOCompletionProcPtr = Register68kProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	IOCompletionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	IOCompletionUPP = UniversalProcPtr;
{$endc}	
	IOParamPtr = ^IOParam;
	IOParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioRefNum:				SInt16;								{ refNum for I/O operation }
		ioVersNum:				SInt8;									{ version number }
		ioPermssn:				SInt8;									{ Open: permissions (byte) }
		ioMisc:					Ptr;									{ Rename: new name (GetEOF,SetEOF: logical end of file) (Open: optional ptr to buffer) (SetFileType: new type) }
		ioBuffer:				Ptr;									{ data buffer Ptr }
		ioReqCount:				SInt32;								{ requested byte count; also = ioNewDirID }
		ioActCount:				SInt32;								{ actual byte count completed }
		ioPosMode:				SInt16;								{ initial file positioning }
		ioPosOffset:			SInt32;								{ file position offset }
	end;

	FileParamPtr = ^FileParam;
	FileParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioFRefNum:				SInt16;								{ reference number }
		ioFVersNum:				SInt8;									{ version number }
		filler1:				SInt8;
		ioFDirIndex:			SInt16;								{ GetFInfo directory index }
		ioFlAttrib:				SInt8;									{ GetFInfo: in-use bit=7, lock bit=0 }
		ioFlVersNum:			SInt8;									{ file version number }
		ioFlFndrInfo:			FInfo;									{ user info }
		ioFlNum:				UInt32;									{ GetFInfo: file number; TF- ioDirID }
		ioFlStBlk:				UInt16;									{ start file block (0 if none) }
		ioFlLgLen:				SInt32;								{ logical length (EOF) }
		ioFlPyLen:				SInt32;								{ physical length }
		ioFlRStBlk:				UInt16;									{ start block rsrc fork }
		ioFlRLgLen:				SInt32;								{ file logical length rsrc fork }
		ioFlRPyLen:				SInt32;								{ file physical length rsrc fork }
		ioFlCrDat:				UInt32;									{ file creation date& time (32 bits in secs) }
		ioFlMdDat:				UInt32;									{ last modified date and time }
	end;

	VolumeParamPtr = ^VolumeParam;
	VolumeParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		filler2:				SInt32;
		ioVolIndex:				SInt16;								{ volume index number }
		ioVCrDate:				UInt32;									{ creation date and time }
		ioVLsBkUp:				UInt32;									{ last backup date and time }
		ioVAtrb:				UInt16;									{ volume attrib }
		ioVNmFls:				UInt16;									{ number of files in directory }
		ioVDirSt:				UInt16;									{ start block of file directory }
		ioVBlLn:				SInt16;								{ GetVolInfo: length of dir in blocks }
		ioVNmAlBlks:			UInt16;									{ for compatibilty ioVNmAlBlks * ioVAlBlkSiz <= 2 GB }
		ioVAlBlkSiz:			UInt32;									{ for compatibilty ioVAlBlkSiz is <= $0000FE00 (65,024) }
		ioVClpSiz:				UInt32;									{ GetVolInfo: bytes to allocate at a time }
		ioAlBlSt:				UInt16;									{ starting disk(512-byte) block in block map }
		ioVNxtFNum:				UInt32;									{ GetVolInfo: next free file number }
		ioVFrBlk:				UInt16;									{ GetVolInfo: # free alloc blks for this vol }
	end;

	CntrlParamPtr = ^CntrlParam;
	CntrlParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioCRefNum:				SInt16;								{ refNum for I/O operation }
		csCode:					SInt16;								{ word for control status code }
		csParam:				array [0..10] of SInt16;				{ operation-defined parameters }
	end;

	SlotDevParamPtr = ^SlotDevParam;
	SlotDevParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioSRefNum:				SInt16;
		ioSVersNum:				SInt8;
		ioSPermssn:				SInt8;
		ioSMix:					Ptr;
		ioSFlags:				SInt16;
		ioSlot:					SInt8;
		ioID:					SInt8;
	end;

	MultiDevParamPtr = ^MultiDevParam;
	MultiDevParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioMRefNum:				SInt16;
		ioMVersNum:				SInt8;
		ioMPermssn:				SInt8;
		ioMMix:					Ptr;
		ioMFlags:				SInt16;
		ioSEBlkPtr:				Ptr;
	end;

	ParamBlockRecPtr = ^ParamBlockRec;
	ParamBlockRec = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		case SInt16 of
		0: (
			ioRefNum:			SInt16;								{ refNum for I/O operation }
			ioVersNum:			SInt8;									{ version number }
			ioPermssn:			SInt8;									{ Open: permissions (byte) }
			ioMisc:				Ptr;									{ Rename: new name (GetEOF,SetEOF: logical end of file) (Open: optional ptr to buffer) (SetFileType: new type) }
			ioBuffer:			Ptr;									{ data buffer Ptr }
			ioReqCount:			SInt32;								{ requested byte count; also = ioNewDirID }
			ioActCount:			SInt32;								{ actual byte count completed }
			ioPosMode:			SInt16;								{ initial file positioning }
			ioPosOffset:		SInt32;								{ file position offset }
		   );
		1: (
			ioFRefNum:			SInt16;								{ reference number }
			ioFVersNum:			SInt8;									{ version number }
			filler1:			SInt8;
			ioFDirIndex:		SInt16;								{ GetFInfo directory index }
			ioFlAttrib:			SInt8;									{ GetFInfo: in-use bit=7, lock bit=0 }
			ioFlVersNum:		SInt8;									{ file version number }
			ioFlFndrInfo:		FInfo;									{ user info }
			ioFlNum:			UInt32;									{ GetFInfo: file number; TF- ioDirID }
			ioFlStBlk:			UInt16;									{ start file block (0 if none) }
			ioFlLgLen:			SInt32;								{ logical length (EOF) }
			ioFlPyLen:			SInt32;								{ physical length }
			ioFlRStBlk:			UInt16;									{ start block rsrc fork }
			ioFlRLgLen:			SInt32;								{ file logical length rsrc fork }
			ioFlRPyLen:			SInt32;								{ file physical length rsrc fork }
			ioFlCrDat:			UInt32;									{ file creation date& time (32 bits in secs) }
			ioFlMdDat:			UInt32;									{ last modified date and time }
		   );
		2: (
			filler2:			SInt32;
			ioVolIndex:			SInt16;								{ volume index number }
			ioVCrDate:			UInt32;									{ creation date and time }
			ioVLsBkUp:			UInt32;									{ last backup date and time }
			ioVAtrb:			UInt16;									{ volume attrib }
			ioVNmFls:			UInt16;									{ number of files in directory }
			ioVDirSt:			UInt16;									{ start block of file directory }
			ioVBlLn:			SInt16;								{ GetVolInfo: length of dir in blocks }
			ioVNmAlBlks:		UInt16;									{ for compatibilty ioVNmAlBlks * ioVAlBlkSiz <= 2 GB }
			ioVAlBlkSiz:		UInt32;									{ for compatibilty ioVAlBlkSiz is <= $0000FE00 (65,024) }
			ioVClpSiz:			UInt32;									{ GetVolInfo: bytes to allocate at a time }
			ioAlBlSt:			UInt16;									{ starting disk(512-byte) block in block map }
			ioVNxtFNum:			UInt32;									{ GetVolInfo: next free file number }
			ioVFrBlk:			UInt16;									{ GetVolInfo: # free alloc blks for this vol }
		   );
		3: (
			ioCRefNum:			SInt16;								{ refNum for I/O operation }
			csCode:				SInt16;								{ word for control status code }
			csParam:			array [0..10] of SInt16;				{ operation-defined parameters }
		   );
		4: (
			ioSRefNum:			SInt16;
			ioSVersNum:			SInt8;
			ioSPermssn:			SInt8;
			ioSMix:				Ptr;
			ioSFlags:			SInt16;
			ioSlot:				SInt8;
			ioID:				SInt8;
		   );
		5: (
			ioMRefNum:			SInt16;
			ioMVersNum:			SInt8;
			ioMPermssn:			SInt8;
			ioMMix:				Ptr;
			ioMFlags:			SInt16;
			ioSEBlkPtr:			Ptr;
		   );
	end;

	CInfoPBRecPtr = ^CInfoPBRec;
	CInfoPBRec = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioFRefNum:				SInt16;
		ioFVersNum:				SInt8;
		filler1:				SInt8;
		ioFDirIndex:			SInt16;
		ioFlAttrib:				SInt8;
		ioACUser:				SInt8;
		case SInt16 of
		0: (
			ioFlFndrInfo:		FInfo;
			ioDirID:			UInt32;
			ioFlStBlk:			UInt16;
			ioFlLgLen:			SInt32;
			ioFlPyLen:			SInt32;
			ioFlRStBlk:			UInt16;
			ioFlRLgLen:			SInt32;
			ioFlRPyLen:			SInt32;
			ioFlCrDat:			UInt32;
			ioFlMdDat:			UInt32;
			ioFlBkDat:			UInt32;
			ioFlXFndrInfo:		FXInfo;
			ioFlParID:			SInt32;
			ioFlClpSiz:			SInt32;
		   );
		1: (
			ioDrUsrWds:			DInfo;
			ioDrDirID:			UInt32;
			ioDrNmFls:			UInt16;
			filler3:			array [1..9] of SInt16;
			ioDrCrDat:			UInt32;
			ioDrMdDat:			UInt32;
			ioDrBkDat:			UInt32;
			ioDrFndrInfo:		DXInfo;
			ioDrParID:			SInt32;
		   );
	end;

	CInfoPBPtr							= ^CInfoPBRec;
	XCInfoPBRecPtr = ^XCInfoPBRec;
	XCInfoPBRec = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			ProcPtr;								{  --> A pointer to a completion routine  }
		ioResult:				OSErr;									{  --> The result code of the function  }
		ioNamePtr:				StringPtr;								{  --> Pointer to pathname to object  }
		ioVRefNum:				SInt16;								{  --> A volume specification  }
		filler1:				SInt32;
		ioShortNamePtr:			StringPtr;								{  <-> A pointer to the short name string buffer - required!  }
		filler2:				SInt16;
		ioPDType:				SInt16;								{  <-- The ProDOS file type  }
		ioPDAuxType:			SInt32;								{  <-- The ProDOS aux type  }
		filler3:				array [0..1] of SInt32;
		ioDirID:				UInt32;								{  --> A directory ID  }
	end;

	XCInfoPBPtr							= ^XCInfoPBRec;
	{	 Catalog position record 	}
	CatPositionRecPtr = ^CatPositionRec;
	CatPositionRec = record
		initialize:				SInt32;
		priv:					array [1..6] of SInt16;
	end;

	FSSpecPtr = ^FSSpec;
	FSSpec = record
		vRefNum:				SInt16;
		parID:					UInt32;
		name:					StrFileName;							{  a Str63 on MacOS }
	end;

	FSSpecHandle						= ^FSSpecPtr;
	{	 pointer to array of FSSpecs 	}
	FSSpecArray							= array [0..0] of FSSpec;
	FSSpecArrayPtr						= ^FSSpecArray;
	{	 
	    The only difference between "const FSSpec*" and "ConstFSSpecPtr" is 
	    that as a parameter, ConstFSSpecPtr is allowed to be NULL 
		}
	ConstFSSpecPtr						= FSSpecPtr;
	{	 
	    The following are structures to be filled out with the _PBGetVolMountInfo call
	    and passed back into the _PBVolumeMount call for external file system mounts. 
		}
	{	 the "signature" of the file system 	}
	VolumeType							= OSType;

const
																{  the signature for AppleShare  }
	AppleShareMediaType			= $6166706D (* 'afpm' *);

	{	
	    VolMount stuff was once in FSM.Å
		}

type
	VolMountInfoHeaderPtr = ^VolMountInfoHeader;
	VolMountInfoHeader = record
		length:					SInt16;								{  length of location data (including self)  }
		media:					VolumeType;								{  type of media.  Variable length data follows  }
	end;

	VolMountInfoPtr						= ^VolMountInfoHeader;
	{	 The new volume mount info record.  The old one is included for compatibility. 
	    the new record allows access by foriegn filesystems writers to the flags 
	    portion of the record. This portion is now public.  
		}
	VolumeMountInfoHeaderPtr = ^VolumeMountInfoHeader;
	VolumeMountInfoHeader = record
		length:					SInt16;								{  length of location data (including self)  }
		media:					VolumeType;								{  type of media (must be registered with Apple)  }
		flags:					SInt16;								{  volume mount flags. Variable length data follows  }
	end;

	{	 volume mount flags 	}

const
	volMountNoLoginMsgFlagBit	= 0;							{  Input to VolumeMount: If set, the file system  }
	volMountNoLoginMsgFlagMask	= $0001;						{   should suppresss any log-in message/greeting dialog  }
	volMountExtendedFlagsBit	= 7;							{  Input to VolumeMount: If set, the mount info is a  }
	volMountExtendedFlagsMask	= $0080;						{   AFPXVolMountInfo record for 3.7 AppleShare Client  }
	volMountInteractBit			= 15;							{  Input to VolumeMount: If set, it's OK for the file system  }
	volMountInteractMask		= $8000;						{   to perform user interaction to mount the volume  }
	volMountChangedBit			= 14;							{  Output from VoumeMount: If set, the volume was mounted, but  }
	volMountChangedMask			= $4000;						{   the volume mounting information record needs to be updated.  }
	volMountFSReservedMask		= $00FF;						{  bits 0-7 are defined by each file system for its own use  }
	volMountSysReservedMask		= $FF00;						{  bits 8-15 are reserved for Apple system use  }


type
	AFPVolMountInfoPtr = ^AFPVolMountInfo;
	AFPVolMountInfo = record
		length:					SInt16;								{  length of location data (including self)  }
		media:					VolumeType;								{  type of media  }
		flags:					SInt16;								{  bits for no messages, no reconnect  }
		nbpInterval:			SInt8;									{  NBP Interval parameter (IM2, p.322)  }
		nbpCount:				SInt8;									{  NBP Interval parameter (IM2, p.322)  }
		uamType:				SInt16;								{  User Authentication Method  }
		zoneNameOffset:			SInt16;								{  short positive offset from start of struct to Zone Name  }
		serverNameOffset:		SInt16;								{  offset to pascal Server Name string  }
		volNameOffset:			SInt16;								{  offset to pascal Volume Name string  }
		userNameOffset:			SInt16;								{  offset to pascal User Name string  }
		userPasswordOffset:		SInt16;								{  offset to pascal User Password string  }
		volPasswordOffset:		SInt16;								{  offset to pascal Volume Password string  }
		AFPData:				packed array [1..144] of char;			{  variable length data may follow  }
	end;


	{	 AFPXVolMountInfo is the new AFP volume mount info record, requires the 3.7 AppleShare Client 	}
	AFPXVolMountInfoPtr = ^AFPXVolMountInfo;
	AFPXVolMountInfo = record
		length:					SInt16;								{  length of location data (including self)  }
		media:					VolumeType;								{  type of media  }
		flags:					SInt16;								{  bits for no messages, no reconnect  }
		nbpInterval:			SInt8;									{  NBP Interval parameter (IM2, p.322)  }
		nbpCount:				SInt8;									{  NBP Interval parameter (IM2, p.322)  }
		uamType:				SInt16;								{  User Authentication Method type  }
		zoneNameOffset:			SInt16;								{  short positive offset from start of struct to Zone Name  }
		serverNameOffset:		SInt16;								{  offset to pascal Server Name string  }
		volNameOffset:			SInt16;								{  offset to pascal Volume Name string  }
		userNameOffset:			SInt16;								{  offset to pascal User Name string  }
		userPasswordOffset:		SInt16;								{  offset to pascal User Password string  }
		volPasswordOffset:		SInt16;								{  offset to pascal Volume Password string  }
		extendedFlags:			SInt16;								{  extended flags word  }
		uamNameOffset:			SInt16;								{  offset to a pascal UAM name string  }
		alternateAddressOffset:	SInt16;								{  offset to Alternate Addresses in tagged format  }
		AFPData:				packed array [1..176] of char;			{  variable length data may follow  }
	end;


const
	kAFPExtendedFlagsAlternateAddressMask = 1;					{   bit in AFPXVolMountInfo.extendedFlags that means alternateAddressOffset is used }


																{  constants for use in AFPTagData.fType field }
	kAFPTagTypeIP				= $01;							{  4 byte IP address (MSB first)             }
	kAFPTagTypeIPPort			= $02;							{  4 byte IP address, 2 byte port (MSB first)      }
	kAFPTagTypeDDP				= $03;							{  Net,Node,Socket Sent by the server, currently unused by the client  }
	kAFPTagTypeDNS				= $04;							{  DNS name in  address:port format   (total length variable up to 254 chars of dns name)           }


																{  constants for use in AFPTagData.fLength field }
	kAFPTagLengthIP				= $06;
	kAFPTagLengthIPPort			= $08;
	kAFPTagLengthDDP			= $06;


type
	AFPTagDataPtr = ^AFPTagData;
	AFPTagData = packed record
		fLength:				UInt8;									{  length of this data tag including the fLength field  }
		fType:					UInt8;
		fData:					packed array [0..0] of UInt8;			{  variable length data  }
	end;

	AFPAlternateAddressPtr = ^AFPAlternateAddress;
	AFPAlternateAddress = packed record
																		{  ¥¥¥ÊNOTE: fVersion was missing in 3.2 Universal Interfaces }
		fVersion:				UInt8;									{  version of the structure (currently 0x00) }
		fAddressCount:			UInt8;
		fAddressList:			packed array [0..0] of UInt8;			{  actually variable length packed set of AFPTagData  }
	end;

	DTPBRecPtr = ^DTPBRec;
	DTPBRec = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioDTRefNum:				SInt16;								{  desktop refnum  }
		ioIndex:				SInt16;
		ioTagInfo:				SInt32;
		ioDTBuffer:				Ptr;
		ioDTReqCount:			SInt32;
		ioDTActCount:			SInt32;
		ioFiller1:				SInt8;
		ioIconType:				SInt8;
		ioFiller2:				SInt16;
		ioDirID:				UInt32;
		ioFileCreator:			OSType;
		ioFileType:				OSType;
		ioFiller3:				SInt32;
		ioDTLgLen:				SInt32;
		ioDTPyLen:				SInt32;
		ioFiller4:				array [1..14] of SInt16;
		ioAPPLParID:			SInt32;
	end;

	DTPBPtr								= ^DTPBRec;

	HIOParamPtr = ^HIOParam;
	HIOParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioRefNum:				SInt16;
		ioVersNum:				SInt8;
		ioPermssn:				SInt8;
		ioMisc:					Ptr;
		ioBuffer:				Ptr;
		ioReqCount:				SInt32;
		ioActCount:				SInt32;
		ioPosMode:				SInt16;
		ioPosOffset:			SInt32;
	end;

	HFileParamPtr = ^HFileParam;
	HFileParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioFRefNum:				SInt16;
		ioFVersNum:				SInt8;
		filler1:				SInt8;
		ioFDirIndex:			SInt16;
		ioFlAttrib:				SInt8;
		ioFlVersNum:			SInt8;
		ioFlFndrInfo:			FInfo;
		ioDirID:				UInt32;
		ioFlStBlk:				UInt16;
		ioFlLgLen:				SInt32;
		ioFlPyLen:				SInt32;
		ioFlRStBlk:				UInt16;
		ioFlRLgLen:				SInt32;
		ioFlRPyLen:				SInt32;
		ioFlCrDat:				UInt32;
		ioFlMdDat:				UInt32;
	end;

	HVolumeParamPtr = ^HVolumeParam;
	HVolumeParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		filler2:				SInt32;
		ioVolIndex:				SInt16;
		ioVCrDate:				UInt32;
		ioVLsMod:				UInt32;
		ioVAtrb:				SInt16;
		ioVNmFls:				UInt16;
		ioVBitMap:				UInt16;
		ioAllocPtr:				UInt16;
		ioVNmAlBlks:			UInt16;
		ioVAlBlkSiz:			UInt32;
		ioVClpSiz:				UInt32;
		ioAlBlSt:				UInt16;
		ioVNxtCNID:				UInt32;
		ioVFrBlk:				UInt16;
		ioVSigWord:				UInt16;
		ioVDrvInfo:				SInt16;
		ioVDRefNum:				SInt16;
		ioVFSID:				SInt16;
		ioVBkUp:				UInt32;
		ioVSeqNum:				SInt16;
		ioVWrCnt:				UInt32;
		ioVFilCnt:				UInt32;
		ioVDirCnt:				UInt32;
		ioVFndrInfo:			array [1..8] of SInt32;
	end;

	XIOParamPtr = ^XIOParam;
	XIOParam = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			IOCompletionUPP;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		ioRefNum:				SInt16;
		ioVersNum:				SInt8;
		ioPermssn:				SInt8;
		ioMisc:					Ptr;
		ioBuffer:				Ptr;
		ioReqCount:				SInt32;
		ioActCount:				SInt32;
		ioPosMode:				SInt16;								{  must have kUseWidePositioning bit set  }
		ioWPosOffset:			wide;									{  wide positioning offset  }
	end;

	XVolumeParamPtr = ^XVolumeParam;
	XVolumeParam = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			IOCompletionUPP;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		ioXVersion:				UInt32;									{  this XVolumeParam version (0)  }
		ioVolIndex:				SInt16;
		ioVCrDate:				UInt32;
		ioVLsMod:				UInt32;
		ioVAtrb:				SInt16;
		ioVNmFls:				UInt16;
		ioVBitMap:				UInt16;
		ioAllocPtr:				UInt16;
		ioVNmAlBlks:			UInt16;
		ioVAlBlkSiz:			UInt32;
		ioVClpSiz:				UInt32;
		ioAlBlSt:				UInt16;
		ioVNxtCNID:				UInt32;
		ioVFrBlk:				UInt16;
		ioVSigWord:				UInt16;
		ioVDrvInfo:				SInt16;
		ioVDRefNum:				SInt16;
		ioVFSID:				SInt16;
		ioVBkUp:				UInt32;
		ioVSeqNum:				SInt16;
		ioVWrCnt:				UInt32;
		ioVFilCnt:				UInt32;
		ioVDirCnt:				UInt32;
		ioVFndrInfo:			array [1..8] of SInt32;
		ioVTotalBytes:			UInt64;									{  total number of bytes on volume  }
		ioVFreeBytes:			UInt64;									{  number of free bytes on volume  }
	end;

	AccessParamPtr = ^AccessParam;
	AccessParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		filler3:				SInt16;
		ioDenyModes:			SInt16;								{ access rights data }
		filler4:				SInt16;
		filler5:				SInt8;
		ioACUser:				SInt8;									{ access rights for directory only }
		filler6:				SInt32;
		ioACOwnerID:			SInt32;								{ owner ID }
		ioACGroupID:			SInt32;								{ group ID }
		ioACAccess:				SInt32;								{ access rights }
		ioDirID:				UInt32;
	end;

	ObjParamPtr = ^ObjParam;
	ObjParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		filler7:				SInt16;
		ioObjType:				SInt16;								{ function code }
		ioObjNamePtr:			StringPtr;								{ ptr to returned creator/group name }
		ioObjID:				SInt32;								{ creator/group ID }
	end;

	CopyParamPtr = ^CopyParam;
	CopyParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioDstVRefNum:			SInt16;								{ destination vol identifier }
		filler8:				SInt16;
		ioNewName:				StringPtr;								{ ptr to destination pathname }
		ioCopyName:				StringPtr;								{ ptr to optional name }
		ioNewDirID:				UInt32;								{ destination directory ID }
		filler14:				SInt32;
		filler15:				SInt32;
		ioDirID:				UInt32;
	end;

	WDParamPtr = ^WDParam;
	WDParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioWDCreated:			SInt16;
		ioWDIndex:				SInt16;
		ioWDProcID:				SInt32;
		ioWDVRefNum:			SInt16;
		filler10:				SInt16;
		filler11:				SInt32;
		filler12:				SInt32;
		filler13:				SInt32;
		ioWDDirID:				UInt32;
	end;

	FIDParamPtr = ^FIDParam;
	FIDParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		filler14:				SInt32;
		ioDestNamePtr:			StringPtr;								{  dest file name  }
		filler15:				SInt32;
		ioDestDirID:			UInt32;								{  dest file's directory id  }
		filler16:				SInt32;
		filler17:				SInt32;
		ioSrcDirID:				UInt32;								{  source file's directory id  }
		filler18:				SInt16;
		ioFileID:				SInt32;								{  file ID  }
	end;

	ForeignPrivParamPtr = ^ForeignPrivParam;
	ForeignPrivParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioFiller21:				SInt32;
		ioFiller22:				SInt32;
		ioForeignPrivBuffer:	Ptr;
		ioForeignPrivActCount:	SInt32;
		ioForeignPrivReqCount:	SInt32;
		ioFiller23:				SInt32;
		ioForeignPrivDirID:		UInt32;
		ioForeignPrivInfo1:		SInt32;
		ioForeignPrivInfo2:		SInt32;
		ioForeignPrivInfo3:		SInt32;
		ioForeignPrivInfo4:		SInt32;
	end;

	CSParamPtr = ^CSParam;
	CSParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioMatchPtr:				FSSpecPtr;								{  match array  }
		ioReqMatchCount:		SInt32;								{  maximum allowable matches  }
		ioActMatchCount:		SInt32;								{  actual match count  }
		ioSearchBits:			SInt32;								{  search criteria selector  }
		ioSearchInfo1:			CInfoPBPtr;								{  search values and range lower bounds  }
		ioSearchInfo2:			CInfoPBPtr;								{  search values and range upper bounds  }
		ioSearchTime:			SInt32;								{  length of time to run search  }
		ioCatPosition:			CatPositionRec;							{  current position in the catalog  }
		ioOptBuffer:			Ptr;									{  optional performance enhancement buffer  }
		ioOptBufSize:			SInt32;								{  size of buffer pointed to by ioOptBuffer  }
	end;


	HParamBlockRecPtr = ^HParamBlockRec;
	HParamBlockRec = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		case SInt16 of
		0: (
			ioRefNum:			SInt16;
			ioVersNum:			SInt8;
			ioPermssn:			SInt8;
			ioMisc:				Ptr;
			ioBuffer:			Ptr;
			ioReqCount:			SInt32;
			ioActCount:			SInt32;
			ioPosMode:			SInt16;
			ioPosOffset:		SInt32;
		   );
		1: (
			ioFRefNum:			SInt16;
			ioFVersNum:			SInt8;
			filler1:			SInt8;
			ioFDirIndex:		SInt16;
			ioFlAttrib:			SInt8;
			ioFlVersNum:		SInt8;
			ioFlFndrInfo:		FInfo;
			ioDirID:			UInt32;
			ioFlStBlk:			UInt16;
			ioFlLgLen:			SInt32;
			ioFlPyLen:			SInt32;
			ioFlRStBlk:			UInt16;
			ioFlRLgLen:			SInt32;
			ioFlRPyLen:			SInt32;
			ioFlCrDat:			UInt32;
			ioFlMdDat:			UInt32;
		   );
		2: (
			filler2:			SInt32;
			ioVolIndex:			SInt16;
			ioVCrDate:			UInt32;
			ioVLsMod:			UInt32;
			ioVAtrb:			SInt16;
			ioVNmFls:			UInt16;
			ioVBitMap:			UInt16;
			ioAllocPtr:			UInt16;
			ioVNmAlBlks:		UInt16;
			ioVAlBlkSiz:		UInt32;
			ioVClpSiz:			UInt32;
			ioAlBlSt:			UInt16;
			ioVNxtCNID:			UInt32;
			ioVFrBlk:			UInt16;
			ioVSigWord:			UInt16;
			ioVDrvInfo:			SInt16;
			ioVDRefNum:			SInt16;
			ioVFSID:			SInt16;
			ioVBkUp:			UInt32;
			ioVSeqNum:			UInt16;
			ioVWrCnt:			UInt32;
			ioVFilCnt:			UInt32;
			ioVDirCnt:			UInt32;
			ioVFndrInfo:		array [1..8] of SInt32;
		   );
		3: (
			filler3:			SInt16;
			ioDenyModes:		SInt16;								{ access rights data }
			filler4:			SInt16;
			filler5:			SInt8;
			ioACUser:			SInt8;									{ access rights for directory only }
			filler6:			SInt32;
			ioACOwnerID:		SInt32;								{ owner ID }
			ioACGroupID:		SInt32;								{ group ID }
			ioACAccess:			SInt32;								{ access rights }
		   );
		4: (
			filler7:			SInt16;
			ioObjType:			SInt16;								{ function code }
			ioObjNamePtr:		StringPtr;								{ ptr to returned creator/group name }
			ioObjID:			SInt32;								{ creator/group ID }
		   );
		5: (
			ioDstVRefNum:		SInt16;								{ destination vol identifier }
			filler8:			SInt16;
			ioNewName:			StringPtr;								{ ptr to destination pathname }
			ioCopyName:			StringPtr;								{ ptr to optional name }
			ioNewDirID:			UInt32;								{ destination directory ID }
		   );
		6: (
			ioWDCreated:		SInt16;
			ioWDIndex:			SInt16;
			ioWDProcID:			SInt32;
			ioWDVRefNum:		SInt16;
			filler10:			SInt16;
			filler11:			SInt32;
			filler12:			SInt32;
			filler13:			SInt32;
			ioWDDirID:			UInt32;
		   );
		7: (
			filler14:			SInt32;
			ioDestNamePtr:		StringPtr;								{  dest file name  }
			filler15:			SInt32;
			ioDestDirID:		UInt32;								{  dest file's directory id  }
			filler16:			SInt32;
			filler17:			SInt32;
			ioSrcDirID:			UInt32;								{  source file's directory id  }
			filler18:			SInt16;
			ioFileID:			SInt32;								{  file ID  }
		   );
		8: (
			ioMatchPtr:			FSSpecPtr;								{  match array  }
			ioReqMatchCount:	SInt32;								{  maximum allowable matches  }
			ioActMatchCount:	SInt32;								{  actual match count  }
			ioSearchBits:		SInt32;								{  search criteria selector  }
			ioSearchInfo1:		CInfoPBPtr;								{  search values and range lower bounds  }
			ioSearchInfo2:		CInfoPBPtr;								{  search values and range upper bounds  }
			ioSearchTime:		SInt32;								{  length of time to run search  }
			ioCatPosition:		CatPositionRec;							{  current position in the catalog  }
			ioOptBuffer:		Ptr;									{  optional performance enhancement buffer  }
			ioOptBufSize:		SInt32;								{  size of buffer pointed to by ioOptBuffer  }
		   );
		9: (
			ioFiller21:			SInt32;
			ioFiller22:			SInt32;
			ioForeignPrivBuffer: Ptr;
			ioForeignPrivActCount: SInt32;
			ioForeignPrivReqCount: SInt32;
			ioFiller23:			SInt32;
			ioForeignPrivDirID:	UInt32;
			ioForeignPrivInfo1:	SInt32;
			ioForeignPrivInfo2:	SInt32;
			ioForeignPrivInfo3:	SInt32;
			ioForeignPrivInfo4:	SInt32;
		   );
	end;

	HParmBlkPtr							= ^HParamBlockRec;

	CMovePBRecPtr = ^CMovePBRec;
	CMovePBRec = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			IOCompletionUPP;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		filler1:				SInt32;
		ioNewName:				StringPtr;
		filler2:				SInt32;
		ioNewDirID:				UInt32;
		filler3:				array [1..2] of SInt32;
		ioDirID:				UInt32;
	end;

	CMovePBPtr							= ^CMovePBRec;
	WDPBRecPtr = ^WDPBRec;
	WDPBRec = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			IOCompletionUPP;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		filler1:				SInt16;
		ioWDIndex:				SInt16;
		ioWDProcID:				SInt32;
		ioWDVRefNum:			SInt16;
		filler2:				array [1..7] of SInt16;
		ioWDDirID:				UInt32;
	end;

	WDPBPtr								= ^WDPBRec;
	FCBPBRecPtr = ^FCBPBRec;
	FCBPBRec = record
		qLink:					QElemPtr;
		qType:					SInt16;
		ioTrap:					SInt16;
		ioCmdAddr:				Ptr;
		ioCompletion:			IOCompletionUPP;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				SInt16;
		ioRefNum:				SInt16;
		filler:					SInt16;
		ioFCBIndx:				SInt16;
		filler1:				SInt16;
		ioFCBFlNm:				SInt32;
		ioFCBFlags:				SInt16;
		ioFCBStBlk:				UInt16;
		ioFCBEOF:				SInt32;
		ioFCBPLen:				SInt32;
		ioFCBCrPs:				SInt32;
		ioFCBVRefNum:			SInt16;
		ioFCBClpSiz:			SInt32;
		ioFCBParID:				SInt32;
	end;

	FCBPBPtr							= ^FCBPBRec;
	VCBPtr = ^VCB;
	VCB = record
		qLink:					QElemPtr;
		qType:					SInt16;
		vcbFlags:				SInt16;
		vcbSigWord:				UInt16;
		vcbCrDate:				UInt32;
		vcbLsMod:				UInt32;
		vcbAtrb:				SInt16;
		vcbNmFls:				UInt16;
		vcbVBMSt:				SInt16;
		vcbAllocPtr:			SInt16;
		vcbNmAlBlks:			UInt16;
		vcbAlBlkSiz:			SInt32;
		vcbClpSiz:				SInt32;
		vcbAlBlSt:				SInt16;
		vcbNxtCNID:				SInt32;
		vcbFreeBks:				UInt16;
		vcbVN:					Str27;
		vcbDrvNum:				SInt16;
		vcbDRefNum:				SInt16;
		vcbFSID:				SInt16;
		vcbVRefNum:				SInt16;
		vcbMAdr:				Ptr;
		vcbBufAdr:				Ptr;
		vcbMLen:				SInt16;
		vcbDirIndex:			SInt16;
		vcbDirBlk:				SInt16;
		vcbVolBkUp:				UInt32;
		vcbVSeqNum:				UInt16;
		vcbWrCnt:				SInt32;
		vcbXTClpSiz:			SInt32;
		vcbCTClpSiz:			SInt32;
		vcbNmRtDirs:			UInt16;
		vcbFilCnt:				SInt32;
		vcbDirCnt:				SInt32;
		vcbFndrInfo:			array [1..8] of SInt32;
		vcbVCSize:				UInt16;
		vcbVBMCSiz:				UInt16;
		vcbCtlCSiz:				UInt16;
		vcbXTAlBlks:			UInt16;
		vcbCTAlBlks:			UInt16;
		vcbXTRef:				SInt16;
		vcbCTRef:				SInt16;
		vcbCtlBuf:				Ptr;
		vcbDirIDM:				SInt32;
		vcbOffsM:				SInt16;
	end;

	DrvQElPtr = ^DrvQEl;
	DrvQEl = record
		qLink:					QElemPtr;
		qType:					SInt16;
		dQDrive:				SInt16;
		dQRefNum:				SInt16;
		dQFSID:					SInt16;
		dQDrvSz:				UInt16;
		dQDrvSz2:				UInt16;
	end;


const
	uppIOCompletionProcInfo = $00009802;
	{
	 *  NewIOCompletionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewIOCompletionUPP(userRoutine: IOCompletionProcPtr): IOCompletionUPP; external name '_NewIOCompletionUPP'; { old name was NewIOCompletionProc }
{
 *  DisposeIOCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeIOCompletionUPP(userUPP: IOCompletionUPP); external name '_DisposeIOCompletionUPP';
{
 *  InvokeIOCompletionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeIOCompletionUPP(paramBlock: ParmBlkPtr; userRoutine: IOCompletionUPP); external name '_InvokeIOCompletionUPP'; { old name was CallIOCompletionProc }

{
   PBOpenSync(), PBOpenAsync(), PBOpenImmed() were moved to Devices.h
   PBCloseSync(), PBCloseAsync(), PBCloseImmed() were moved to Devices.h
   PBReadSync(), PBReadAsync(), PBReadImmed() were moved to Devices.h
   PBWriteSync(), PBWriteAsync(), PBWriteImmed() were moved to Devices.h
}


{$ifc CALL_NOT_IN_CARBON}
{
 *  PBGetVInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetVInfoSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetVInfoSync';
{
 *  PBGetVInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetVInfoAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetVInfoAsync';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBXGetVolInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBXGetVolInfoSync(paramBlock: XVolumeParamPtr): OSErr; external name '_PBXGetVolInfoSync';
{
 *  PBXGetVolInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBXGetVolInfoAsync(paramBlock: XVolumeParamPtr): OSErr; external name '_PBXGetVolInfoAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBGetVolSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetVolSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetVolSync';
{
 *  PBGetVolAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetVolAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetVolAsync';
{
 *  PBSetVolSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetVolSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetVolSync';
{
 *  PBSetVolAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetVolAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetVolAsync';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBFlushVolSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBFlushVolSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBFlushVolSync';
{
 *  PBFlushVolAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBFlushVolAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBFlushVolAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBHTrashVolumeCachesSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHTrashVolumeCachesSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBHTrashVolumeCachesSync';
{
 *  PBCreateSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCreateSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBCreateSync';
{
 *  PBCreateAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCreateAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBCreateAsync';
{
 *  PBDeleteSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDeleteSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBDeleteSync';
{
 *  PBDeleteAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDeleteAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBDeleteAsync';
{
 *  PBOpenDFSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenDFSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenDFSync';
{
 *  PBOpenDFAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenDFAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenDFAsync';
{
 *  PBOpenRFSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenRFSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenRFSync';
{
 *  PBOpenRFAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenRFAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBOpenRFAsync';
{
 *  PBRenameSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRenameSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBRenameSync';
{
 *  PBRenameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRenameAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBRenameAsync';
{
 *  PBGetFInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetFInfoSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetFInfoSync';
{
 *  PBGetFInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetFInfoAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetFInfoAsync';
{
 *  PBSetFInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFInfoSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFInfoSync';
{
 *  PBSetFInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFInfoAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFInfoAsync';
{
 *  PBSetFLockSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFLockSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFLockSync';
{
 *  PBSetFLockAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFLockAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFLockAsync';
{
 *  PBRstFLockSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRstFLockSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBRstFLockSync';
{
 *  PBRstFLockAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRstFLockAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBRstFLockAsync';
{
 *  PBSetFVersSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFVersSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFVersSync';
{
 *  PBSetFVersAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFVersAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFVersAsync';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBAllocateSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBAllocateSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBAllocateSync';
{
 *  PBAllocateAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBAllocateAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBAllocateAsync';
{
 *  PBGetEOFSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetEOFSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetEOFSync';
{
 *  PBGetEOFAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetEOFAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetEOFAsync';
{
 *  PBSetEOFSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetEOFSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetEOFSync';
{
 *  PBSetEOFAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetEOFAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetEOFAsync';
{
 *  PBGetFPosSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetFPosSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetFPosSync';
{
 *  PBGetFPosAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetFPosAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetFPosAsync';
{
 *  PBSetFPosSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetFPosSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFPosSync';
{
 *  PBSetFPosAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetFPosAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBSetFPosAsync';
{
 *  PBFlushFileSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBFlushFileSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBFlushFileSync';
{
 *  PBFlushFileAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBFlushFileAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBFlushFileAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBMountVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBMountVol(paramBlock: ParmBlkPtr): OSErr; external name '_PBMountVol';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBUnmountVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBUnmountVol(paramBlock: ParmBlkPtr): OSErr; external name '_PBUnmountVol';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBUnmountVolImmed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBUnmountVolImmed(paramBlock: ParmBlkPtr): OSErr; external name '_PBUnmountVolImmed';
{
 *  PBEject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBEject(paramBlock: ParmBlkPtr): OSErr; external name '_PBEject';
{
 *  PBOffLine()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOffLine(paramBlock: ParmBlkPtr): OSErr; external name '_PBOffLine';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBCatSearchSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCatSearchSync(paramBlock: CSParamPtr): OSErr; external name '_PBCatSearchSync';
{
 *  PBCatSearchAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCatSearchAsync(paramBlock: CSParamPtr): OSErr; external name '_PBCatSearchAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  SetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetVol(volName: ConstStringPtr; vRefNum: SInt16): OSErr; external name '_SetVol';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  UnmountVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UnmountVol(volName: ConstStringPtr; vRefNum: SInt16): OSErr; external name '_UnmountVol';

{$ifc CALL_NOT_IN_CARBON}
{
 *  Eject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Eject(volName: ConstStringPtr; vRefNum: SInt16): OSErr; external name '_Eject';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  FlushVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FlushVol(volName: ConstStringPtr; vRefNum: SInt16): OSErr; external name '_FlushVol';

{
 *  HSetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HSetVol(volName: ConstStringPtr; vRefNum: SInt16; dirID: UInt32): OSErr; external name '_HSetVol';

{  AddDrive() was moved to Devices.h }

{$ifc CALL_NOT_IN_CARBON}
{
 *  FSOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FSOpen(const (*var*) fileName: Str255; vRefNum: SInt16; var refNum: SInt16): OSErr; external name '_FSOpen';

{
 *  OpenDF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenDF(const (*var*) fileName: Str255; vRefNum: SInt16; var refNum: SInt16): OSErr; external name '_OpenDF';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  FSClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSClose(refNum: SInt16): OSErr; external name '_FSClose';

{
 *  FSRead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSRead(refNum: SInt16; var count: SInt32; buffPtr: UnivPtr): OSErr; external name '_FSRead';

{
 *  FSWrite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSWrite(refNum: SInt16; var count: SInt32; buffPtr: UnivPtr): OSErr; external name '_FSWrite';

{$ifc CALL_NOT_IN_CARBON}
{
 *  GetVInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetVInfo(drvNum: SInt16; volName: StringPtr; var vRefNum: SInt16; var freeBytes: SInt32): OSErr; external name '_GetVInfo';

{
 *  GetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetFInfo(const (*var*) fileName: Str255; vRefNum: SInt16; var fndrInfo: FInfo): OSErr; external name '_GetFInfo';

{
 *  GetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetVol(volName: StringPtr; var vRefNum: SInt16): OSErr; external name '_GetVol';

{
 *  Create()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Create(const (*var*) fileName: Str255; vRefNum: SInt16; creator: OSType; fileType: OSType): OSErr; external name '_Create';

{
 *  FSDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function FSDelete(const (*var*) fileName: Str255; vRefNum: SInt16): OSErr; external name '_FSDelete';

{
 *  OpenRF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenRF(const (*var*) fileName: Str255; vRefNum: SInt16; var refNum: SInt16): OSErr; external name '_OpenRF';

{
 *  Rename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Rename(const (*var*) oldName: Str255; vRefNum: SInt16; const (*var*) newName: Str255): OSErr; external name '_Rename';

{
 *  SetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetFInfo(const (*var*) fileName: Str255; vRefNum: SInt16; const (*var*) fndrInfo: FInfo): OSErr; external name '_SetFInfo';

{
 *  SetFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function SetFLock(const (*var*) fileName: Str255; vRefNum: SInt16): OSErr; external name '_SetFLock';

{
 *  RstFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RstFLock(const (*var*) fileName: Str255; vRefNum: SInt16): OSErr; external name '_RstFLock';

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  Allocate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function Allocate(refNum: SInt16; var count: SInt32): OSErr; external name '_Allocate';

{
 *  GetEOF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetEOF(refNum: SInt16; var logEOF: SInt32): OSErr; external name '_GetEOF';

{
 *  SetEOF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetEOF(refNum: SInt16; logEOF: SInt32): OSErr; external name '_SetEOF';

{
 *  GetFPos()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetFPos(refNum: SInt16; var filePos: SInt32): OSErr; external name '_GetFPos';

{
 *  SetFPos()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetFPos(refNum: SInt16; posMode: SInt16; posOff: SInt32): OSErr; external name '_SetFPos';

{
 *  GetVRefNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetVRefNum(fileRefNum: SInt16; var vRefNum: SInt16): OSErr; external name '_GetVRefNum';

{$ifc CALL_NOT_IN_CARBON}
{
 *  PBOpenWDSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenWDSync(paramBlock: WDPBPtr): OSErr; external name '_PBOpenWDSync';
{
 *  PBOpenWDAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenWDAsync(paramBlock: WDPBPtr): OSErr; external name '_PBOpenWDAsync';
{
 *  PBCloseWDSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCloseWDSync(paramBlock: WDPBPtr): OSErr; external name '_PBCloseWDSync';
{
 *  PBCloseWDAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCloseWDAsync(paramBlock: WDPBPtr): OSErr; external name '_PBCloseWDAsync';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBHSetVolSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetVolSync(paramBlock: WDPBPtr): OSErr; external name '_PBHSetVolSync';
{
 *  PBHSetVolAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetVolAsync(paramBlock: WDPBPtr): OSErr; external name '_PBHSetVolAsync';
{
 *  PBHGetVolSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetVolSync(paramBlock: WDPBPtr): OSErr; external name '_PBHGetVolSync';
{
 *  PBHGetVolAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetVolAsync(paramBlock: WDPBPtr): OSErr; external name '_PBHGetVolAsync';
{
 *  PBCatMoveSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCatMoveSync(paramBlock: CMovePBPtr): OSErr; external name '_PBCatMoveSync';
{
 *  PBCatMoveAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCatMoveAsync(paramBlock: CMovePBPtr): OSErr; external name '_PBCatMoveAsync';
{
 *  PBDirCreateSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDirCreateSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBDirCreateSync';
{
 *  PBDirCreateAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDirCreateAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBDirCreateAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBGetWDInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetWDInfoSync(paramBlock: WDPBPtr): OSErr; external name '_PBGetWDInfoSync';
{
 *  PBGetWDInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetWDInfoAsync(paramBlock: WDPBPtr): OSErr; external name '_PBGetWDInfoAsync';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBGetFCBInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetFCBInfoSync(paramBlock: FCBPBPtr): OSErr; external name '_PBGetFCBInfoSync';
{
 *  PBGetFCBInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetFCBInfoAsync(paramBlock: FCBPBPtr): OSErr; external name '_PBGetFCBInfoAsync';
{
 *  PBGetCatInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetCatInfoSync(paramBlock: CInfoPBPtr): OSErr; external name '_PBGetCatInfoSync';
{
 *  PBGetCatInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetCatInfoAsync(paramBlock: CInfoPBPtr): OSErr; external name '_PBGetCatInfoAsync';
{
 *  PBSetCatInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetCatInfoSync(paramBlock: CInfoPBPtr): OSErr; external name '_PBSetCatInfoSync';
{
 *  PBSetCatInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetCatInfoAsync(paramBlock: CInfoPBPtr): OSErr; external name '_PBSetCatInfoAsync';
{
 *  PBAllocContigSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBAllocContigSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBAllocContigSync';
{
 *  PBAllocContigAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBAllocContigAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBAllocContigAsync';
{
 *  PBLockRangeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBLockRangeSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBLockRangeSync';
{
 *  PBLockRangeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBLockRangeAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBLockRangeAsync';
{
 *  PBUnlockRangeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBUnlockRangeSync(paramBlock: ParmBlkPtr): OSErr; external name '_PBUnlockRangeSync';
{
 *  PBUnlockRangeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBUnlockRangeAsync(paramBlock: ParmBlkPtr): OSErr; external name '_PBUnlockRangeAsync';
{
 *  PBSetVInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetVInfoSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBSetVInfoSync';
{
 *  PBSetVInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetVInfoAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBSetVInfoAsync';
{
 *  PBHGetVInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetVInfoSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetVInfoSync';
{
 *  PBHGetVInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetVInfoAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetVInfoAsync';
{
 *  PBHOpenSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenSync';
{
 *  PBHOpenAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenAsync';
{
 *  PBHOpenRFSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenRFSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenRFSync';
{
 *  PBHOpenRFAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenRFAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenRFAsync';
{
 *  PBHOpenDFSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenDFSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenDFSync';
{
 *  PBHOpenDFAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenDFAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenDFAsync';
{
 *  PBHCreateSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHCreateSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHCreateSync';
{
 *  PBHCreateAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHCreateAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHCreateAsync';
{
 *  PBHDeleteSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHDeleteSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHDeleteSync';
{
 *  PBHDeleteAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHDeleteAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHDeleteAsync';
{
 *  PBHRenameSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHRenameSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHRenameSync';
{
 *  PBHRenameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHRenameAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHRenameAsync';
{
 *  PBHRstFLockSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHRstFLockSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHRstFLockSync';
{
 *  PBHRstFLockAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHRstFLockAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHRstFLockAsync';
{
 *  PBHSetFLockSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetFLockSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHSetFLockSync';
{
 *  PBHSetFLockAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetFLockAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHSetFLockAsync';
{
 *  PBHGetFInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetFInfoSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetFInfoSync';
{
 *  PBHGetFInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetFInfoAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetFInfoAsync';
{
 *  PBHSetFInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetFInfoSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHSetFInfoSync';
{
 *  PBHSetFInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetFInfoAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHSetFInfoAsync';
{
 *  PBMakeFSSpecSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBMakeFSSpecSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBMakeFSSpecSync';
{
 *  PBMakeFSSpecAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBMakeFSSpecAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBMakeFSSpecAsync';
{$ifc CALL_NOT_IN_CARBON}
{
 *  FInitQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure FInitQueue; external name '_FInitQueue';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  GetFSQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetFSQHdr: QHdrPtr; external name '_GetFSQHdr';
{
 *  GetVCBQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetVCBQHdr: QHdrPtr; external name '_GetVCBQHdr';
{  GetDrvQHdr was moved to Devices.h }

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  HGetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HGetVol(volName: StringPtr; var vRefNum: SInt16; var dirID: UInt32): OSErr; external name '_HGetVol';

{
 *  HOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HOpen(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; permission: SInt8; var refNum: SInt16): OSErr; external name '_HOpen';

{
 *  HOpenDF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HOpenDF(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; permission: SInt8; var refNum: SInt16): OSErr; external name '_HOpenDF';

{
 *  HOpenRF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HOpenRF(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; permission: SInt8; var refNum: SInt16): OSErr; external name '_HOpenRF';

{
 *  AllocContig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AllocContig(refNum: SInt16; var count: SInt32): OSErr; external name '_AllocContig';

{
 *  HCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HCreate(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; creator: OSType; fileType: OSType): OSErr; external name '_HCreate';

{
 *  DirCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DirCreate(vRefNum: SInt16; parentDirID: UInt32; const (*var*) directoryName: Str255; var createdDirID: UInt32): OSErr; external name '_DirCreate';

{
 *  HDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HDelete(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255): OSErr; external name '_HDelete';

{
 *  HGetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HGetFInfo(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; var fndrInfo: FInfo): OSErr; external name '_HGetFInfo';

{
 *  HSetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HSetFInfo(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; const (*var*) fndrInfo: FInfo): OSErr; external name '_HSetFInfo';

{
 *  HSetFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HSetFLock(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255): OSErr; external name '_HSetFLock';

{
 *  HRstFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRstFLock(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255): OSErr; external name '_HRstFLock';

{
 *  HRename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRename(vRefNum: SInt16; dirID: UInt32; const (*var*) oldName: Str255; const (*var*) newName: Str255): OSErr; external name '_HRename';

{
 *  CatMove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CatMove(vRefNum: SInt16; dirID: UInt32; const (*var*) oldName: Str255; newDirID: UInt32; const (*var*) newName: Str255): OSErr; external name '_CatMove';

{$ifc CALL_NOT_IN_CARBON}
{
 *  OpenWD()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OpenWD(vRefNum: SInt16; dirID: UInt32; procID: SInt32; var wdRefNum: SInt16): OSErr; external name '_OpenWD';

{
 *  CloseWD()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CloseWD(wdRefNum: SInt16): OSErr; external name '_CloseWD';

{
 *  GetWDInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function GetWDInfo(wdRefNum: SInt16; var vRefNum: SInt16; var dirID: UInt32; var procID: SInt32): OSErr; external name '_GetWDInfo';

{  shared environment  }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PBHGetVolParmsSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetVolParmsSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetVolParmsSync';
{
 *  PBHGetVolParmsAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetVolParmsAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetVolParmsAsync';
{
 *  PBHGetLogInInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetLogInInfoSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetLogInInfoSync';
{
 *  PBHGetLogInInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetLogInInfoAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetLogInInfoAsync';
{
 *  PBHGetDirAccessSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetDirAccessSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetDirAccessSync';
{
 *  PBHGetDirAccessAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHGetDirAccessAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHGetDirAccessAsync';
{
 *  PBHSetDirAccessSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetDirAccessSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHSetDirAccessSync';
{
 *  PBHSetDirAccessAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHSetDirAccessAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHSetDirAccessAsync';
{
 *  PBHMapIDSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHMapIDSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHMapIDSync';
{
 *  PBHMapIDAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHMapIDAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHMapIDAsync';
{
 *  PBHMapNameSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHMapNameSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHMapNameSync';
{
 *  PBHMapNameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHMapNameAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHMapNameAsync';
{
 *  PBHCopyFileSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHCopyFileSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHCopyFileSync';
{
 *  PBHCopyFileAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHCopyFileAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHCopyFileAsync';
{
 *  PBHMoveRenameSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHMoveRenameSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHMoveRenameSync';
{
 *  PBHMoveRenameAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHMoveRenameAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHMoveRenameAsync';
{
 *  PBHOpenDenySync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenDenySync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenDenySync';
{
 *  PBHOpenDenyAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenDenyAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenDenyAsync';
{
 *  PBHOpenRFDenySync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenRFDenySync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenRFDenySync';
{
 *  PBHOpenRFDenyAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBHOpenRFDenyAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBHOpenRFDenyAsync';
{
 *  PBGetXCatInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetXCatInfoSync(paramBlock: XCInfoPBPtr): OSErr; external name '_PBGetXCatInfoSync';
{
 *  PBGetXCatInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetXCatInfoAsync(paramBlock: XCInfoPBPtr): OSErr; external name '_PBGetXCatInfoAsync';
{
 *  PBExchangeFilesSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBExchangeFilesSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBExchangeFilesSync';
{
 *  PBExchangeFilesAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBExchangeFilesAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBExchangeFilesAsync';
{
 *  PBCreateFileIDRefSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCreateFileIDRefSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBCreateFileIDRefSync';
{
 *  PBCreateFileIDRefAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCreateFileIDRefAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBCreateFileIDRefAsync';
{
 *  PBResolveFileIDRefSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBResolveFileIDRefSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBResolveFileIDRefSync';
{
 *  PBResolveFileIDRefAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBResolveFileIDRefAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBResolveFileIDRefAsync';
{
 *  PBDeleteFileIDRefSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDeleteFileIDRefSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBDeleteFileIDRefSync';
{
 *  PBDeleteFileIDRefAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDeleteFileIDRefAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBDeleteFileIDRefAsync';
{
 *  PBGetForeignPrivsSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetForeignPrivsSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBGetForeignPrivsSync';
{
 *  PBGetForeignPrivsAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetForeignPrivsAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBGetForeignPrivsAsync';
{
 *  PBSetForeignPrivsSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetForeignPrivsSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBSetForeignPrivsSync';
{
 *  PBSetForeignPrivsAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetForeignPrivsAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBSetForeignPrivsAsync';
{  Desktop Manager  }
{
 *  PBDTGetPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetPath(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetPath';
{
 *  PBDTCloseDown()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTCloseDown(paramBlock: DTPBPtr): OSErr; external name '_PBDTCloseDown';
{
 *  PBDTAddIconSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTAddIconSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTAddIconSync';
{
 *  PBDTAddIconAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTAddIconAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTAddIconAsync';
{
 *  PBDTGetIconSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetIconSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetIconSync';
{
 *  PBDTGetIconAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetIconAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetIconAsync';
{
 *  PBDTGetIconInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetIconInfoSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetIconInfoSync';
{
 *  PBDTGetIconInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetIconInfoAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetIconInfoAsync';
{
 *  PBDTAddAPPLSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTAddAPPLSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTAddAPPLSync';
{
 *  PBDTAddAPPLAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTAddAPPLAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTAddAPPLAsync';
{
 *  PBDTRemoveAPPLSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTRemoveAPPLSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTRemoveAPPLSync';
{
 *  PBDTRemoveAPPLAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTRemoveAPPLAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTRemoveAPPLAsync';
{
 *  PBDTGetAPPLSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetAPPLSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetAPPLSync';
{
 *  PBDTGetAPPLAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetAPPLAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetAPPLAsync';
{
 *  PBDTSetCommentSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTSetCommentSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTSetCommentSync';
{
 *  PBDTSetCommentAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTSetCommentAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTSetCommentAsync';
{
 *  PBDTRemoveCommentSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTRemoveCommentSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTRemoveCommentSync';
{
 *  PBDTRemoveCommentAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTRemoveCommentAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTRemoveCommentAsync';
{
 *  PBDTGetCommentSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetCommentSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetCommentSync';
{
 *  PBDTGetCommentAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetCommentAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetCommentAsync';
{
 *  PBDTFlushSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTFlushSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTFlushSync';
{
 *  PBDTFlushAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTFlushAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTFlushAsync';
{
 *  PBDTResetSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTResetSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTResetSync';
{
 *  PBDTResetAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTResetAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTResetAsync';
{
 *  PBDTGetInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetInfoSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetInfoSync';
{
 *  PBDTGetInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTGetInfoAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTGetInfoAsync';
{
 *  PBDTOpenInform()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTOpenInform(paramBlock: DTPBPtr): OSErr; external name '_PBDTOpenInform';
{
 *  PBDTDeleteSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTDeleteSync(paramBlock: DTPBPtr): OSErr; external name '_PBDTDeleteSync';
{
 *  PBDTDeleteAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDTDeleteAsync(paramBlock: DTPBPtr): OSErr; external name '_PBDTDeleteAsync';
{  VolumeMount traps  }
{
 *  PBGetVolMountInfoSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetVolMountInfoSize(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetVolMountInfoSize';
{
 *  PBGetVolMountInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetVolMountInfo(paramBlock: ParmBlkPtr): OSErr; external name '_PBGetVolMountInfo';
{
 *  PBVolumeMount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBVolumeMount(paramBlock: ParmBlkPtr): OSErr; external name '_PBVolumeMount';
{  FSp traps  }
{
 *  FSMakeFSSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSMakeFSSpec(vRefNum: SInt16; dirID: UInt32; const (*var*) fileName: Str255; var spec: FSSpec): OSErr; external name '_FSMakeFSSpec';
{
 *  FSpOpenDF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpOpenDF(const (*var*) spec: FSSpec; permission: SInt8; var refNum: SInt16): OSErr; external name '_FSpOpenDF';
{
 *  FSpOpenRF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpOpenRF(const (*var*) spec: FSSpec; permission: SInt8; var refNum: SInt16): OSErr; external name '_FSpOpenRF';
{
 *  FSpCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpCreate(const (*var*) spec: FSSpec; creator: OSType; fileType: OSType; scriptTag: ScriptCode): OSErr; external name '_FSpCreate';
{
 *  FSpDirCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpDirCreate(const (*var*) spec: FSSpec; scriptTag: ScriptCode; var createdDirID: UInt32): OSErr; external name '_FSpDirCreate';
{
 *  FSpDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpDelete(const (*var*) spec: FSSpec): OSErr; external name '_FSpDelete';
{
 *  FSpGetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpGetFInfo(const (*var*) spec: FSSpec; var fndrInfo: FInfo): OSErr; external name '_FSpGetFInfo';
{
 *  FSpSetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpSetFInfo(const (*var*) spec: FSSpec; const (*var*) fndrInfo: FInfo): OSErr; external name '_FSpSetFInfo';
{
 *  FSpSetFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpSetFLock(const (*var*) spec: FSSpec): OSErr; external name '_FSpSetFLock';
{
 *  FSpRstFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpRstFLock(const (*var*) spec: FSSpec): OSErr; external name '_FSpRstFLock';
{
 *  FSpRename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpRename(const (*var*) spec: FSSpec; const (*var*) newName: Str255): OSErr; external name '_FSpRename';
{
 *  FSpCatMove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpCatMove(const (*var*) source: FSSpec; const (*var*) dest: FSSpec): OSErr; external name '_FSpCatMove';
{
 *  FSpExchangeFiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSpExchangeFiles(const (*var*) source: FSSpec; const (*var*) dest: FSSpec): OSErr; external name '_FSpExchangeFiles';
{
 *  PBShareSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBShareSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBShareSync';
{
 *  PBShareAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBShareAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBShareAsync';
{
 *  PBUnshareSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBUnshareSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBUnshareSync';
{
 *  PBUnshareAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBUnshareAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBUnshareAsync';
{
 *  PBGetUGEntrySync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetUGEntrySync(paramBlock: HParmBlkPtr): OSErr; external name '_PBGetUGEntrySync';
{
 *  PBGetUGEntryAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetUGEntryAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBGetUGEntryAsync';
{$ifc TARGET_CPU_68K}
{
    PBGetAltAccess and PBSetAltAccess are obsolete and will not be supported 
    on PowerPC. Equivalent functionality is provided by the routines 
    PBGetForeignPrivs and PBSetForeignPrivs.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBGetAltAccessSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetAltAccessSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBGetAltAccessSync';
{
 *  PBGetAltAccessAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetAltAccessAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBGetAltAccessAsync';
{
 *  PBSetAltAccessSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetAltAccessSync(paramBlock: HParmBlkPtr): OSErr; external name '_PBSetAltAccessSync';
{
 *  PBSetAltAccessAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetAltAccessAsync(paramBlock: HParmBlkPtr): OSErr; external name '_PBSetAltAccessAsync';
{$endc}  {CALL_NOT_IN_CARBON}
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBGetAltAccess()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetAltAccess(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBGetAltAccess';

{
 *  PBSetAltAccess()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetAltAccess(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBSetAltAccess';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_CPU_68K}


{
    The PBxxx() routines are obsolete.  
    
    Use the PBxxxSync() or PBxxxAsync() version instead.
}
{$ifc OLDROUTINENAMES}
{$ifc CALL_NOT_IN_CARBON}
{
 *  PBGetVInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetVInfo(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBGetVInfo';

{
 *  PBXGetVolInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBXGetVolInfo(paramBlock: XVolumeParamPtr; async: boolean): OSErr; external name '_PBXGetVolInfo';

{
 *  PBGetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetVol(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBGetVol';

{
 *  PBSetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetVol(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBSetVol';

{
 *  PBFlushVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBFlushVol(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBFlushVol';

{
 *  PBCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCreate(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBCreate';

{
 *  PBDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDelete(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBDelete';

{
 *  PBOpenDF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenDF(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBOpenDF';

{
 *  PBOpenRF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenRF(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBOpenRF';

{
 *  PBRename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRename(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBRename';

{
 *  PBGetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetFInfo(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBGetFInfo';

{
 *  PBSetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFInfo(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBSetFInfo';

{
 *  PBSetFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFLock(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBSetFLock';

{
 *  PBRstFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBRstFLock(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBRstFLock';

{
 *  PBSetFVers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFVers(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBSetFVers';

{
 *  PBAllocate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBAllocate(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBAllocate';

{
 *  PBGetEOF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetEOF(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBGetEOF';

{
 *  PBSetEOF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetEOF(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBSetEOF';

{
 *  PBGetFPos()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetFPos(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBGetFPos';

{
 *  PBSetFPos()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetFPos(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBSetFPos';

{
 *  PBFlushFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBFlushFile(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBFlushFile';

{
 *  PBCatSearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCatSearch(paramBlock: CSParamPtr; async: boolean): OSErr; external name '_PBCatSearch';

{
 *  PBOpenWD()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBOpenWD(paramBlock: WDPBPtr; async: boolean): OSErr; external name '_PBOpenWD';

{
 *  PBCloseWD()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCloseWD(paramBlock: WDPBPtr; async: boolean): OSErr; external name '_PBCloseWD';

{
 *  PBHSetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHSetVol(paramBlock: WDPBPtr; async: boolean): OSErr; external name '_PBHSetVol';

{
 *  PBHGetVol()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHGetVol(paramBlock: WDPBPtr; async: boolean): OSErr; external name '_PBHGetVol';

{
 *  PBCatMove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCatMove(paramBlock: CMovePBPtr; async: boolean): OSErr; external name '_PBCatMove';

{
 *  PBDirCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDirCreate(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBDirCreate';

{
 *  PBGetWDInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetWDInfo(paramBlock: WDPBPtr; async: boolean): OSErr; external name '_PBGetWDInfo';

{
 *  PBGetFCBInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetFCBInfo(paramBlock: FCBPBPtr; async: boolean): OSErr; external name '_PBGetFCBInfo';

{
 *  PBGetCatInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetCatInfo(paramBlock: CInfoPBPtr; async: boolean): OSErr; external name '_PBGetCatInfo';

{
 *  PBSetCatInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetCatInfo(paramBlock: CInfoPBPtr; async: boolean): OSErr; external name '_PBSetCatInfo';

{
 *  PBAllocContig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBAllocContig(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBAllocContig';

{
 *  PBLockRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBLockRange(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBLockRange';

{
 *  PBUnlockRange()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBUnlockRange(paramBlock: ParmBlkPtr; async: boolean): OSErr; external name '_PBUnlockRange';

{
 *  PBSetVInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetVInfo(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBSetVInfo';

{
 *  PBHGetVInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHGetVInfo(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHGetVInfo';

{
 *  PBHOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHOpen(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHOpen';

{
 *  PBHOpenRF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHOpenRF(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHOpenRF';

{
 *  PBHOpenDF()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHOpenDF(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHOpenDF';

{
 *  PBHCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHCreate(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHCreate';

{
 *  PBHDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHDelete(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHDelete';

{
 *  PBHRename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHRename(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHRename';

{
 *  PBHRstFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHRstFLock(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHRstFLock';

{
 *  PBHSetFLock()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHSetFLock(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHSetFLock';

{
 *  PBHGetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHGetFInfo(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHGetFInfo';

{
 *  PBHSetFInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHSetFInfo(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHSetFInfo';

{
 *  PBMakeFSSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBMakeFSSpec(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBMakeFSSpec';

{
 *  PBHGetVolParms()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHGetVolParms(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHGetVolParms';

{
 *  PBHGetLogInInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHGetLogInInfo(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHGetLogInInfo';

{
 *  PBHGetDirAccess()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHGetDirAccess(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHGetDirAccess';

{
 *  PBHSetDirAccess()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHSetDirAccess(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHSetDirAccess';

{
 *  PBHMapID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHMapID(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHMapID';

{
 *  PBHMapName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHMapName(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHMapName';

{
 *  PBHCopyFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHCopyFile(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHCopyFile';

{
 *  PBHMoveRename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHMoveRename(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHMoveRename';

{
 *  PBHOpenDeny()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHOpenDeny(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHOpenDeny';

{
 *  PBHOpenRFDeny()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBHOpenRFDeny(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBHOpenRFDeny';

{
 *  PBExchangeFiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBExchangeFiles(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBExchangeFiles';

{
 *  PBCreateFileIDRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBCreateFileIDRef(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBCreateFileIDRef';

{
 *  PBResolveFileIDRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBResolveFileIDRef(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBResolveFileIDRef';

{
 *  PBDeleteFileIDRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDeleteFileIDRef(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBDeleteFileIDRef';

{
 *  PBGetForeignPrivs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBGetForeignPrivs(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBGetForeignPrivs';

{
 *  PBSetForeignPrivs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBSetForeignPrivs(paramBlock: HParmBlkPtr; async: boolean): OSErr; external name '_PBSetForeignPrivs';

{
 *  PBDTAddIcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTAddIcon(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTAddIcon';

{
 *  PBDTGetIcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTGetIcon(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTGetIcon';

{
 *  PBDTGetIconInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTGetIconInfo(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTGetIconInfo';

{
 *  PBDTAddAPPL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTAddAPPL(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTAddAPPL';

{
 *  PBDTRemoveAPPL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTRemoveAPPL(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTRemoveAPPL';

{
 *  PBDTGetAPPL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTGetAPPL(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTGetAPPL';

{
 *  PBDTSetComment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTSetComment(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTSetComment';

{
 *  PBDTRemoveComment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTRemoveComment(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTRemoveComment';

{
 *  PBDTGetComment()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTGetComment(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTGetComment';

{
 *  PBDTFlush()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTFlush(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTFlush';

{
 *  PBDTReset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTReset(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTReset';

{
 *  PBDTGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTGetInfo(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTGetInfo';

{
 *  PBDTDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PBDTDelete(paramBlock: DTPBPtr; async: boolean): OSErr; external name '_PBDTDelete';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}


type
	FSVolumeRefNum						= SInt16;
	FSVolumeRefNumPtr					= ^FSVolumeRefNum; { when a VAR xx: FSVolumeRefNum parameter can be nil, it is changed to xx: FSVolumeRefNumPtr }

const
	kFSInvalidVolumeRefNum		= 0;


type
	FSRefPtr = ^FSRef;
	FSRef = record
		hidden:					packed array [0..79] of UInt8;			{  private to File Manager; ¥¥ need symbolic constant  }
	end;


	{
	 *  FSPermissionInfo
	 *  
	 *  Discussion:
	 *    This structure is used when kFSCatInfoPermissions is passed to
	 *    the HFSPlus API. On return from GetCatalogInfo and
	 *    GetCatalogInfoBulk, the userID, groupID, and mode fields are
	 *    returned.  When passed to SetCatalogInfo, only the mode field is
	 *    set.  See chmod(2) for details about the mode field. This is
	 *    supported on Mac OS X only.
	 	}
	FSPermissionInfoPtr = ^FSPermissionInfo;
	FSPermissionInfo = record
		userID:					UInt32;
		groupID:				UInt32;
		reserved1:				SInt8;
		userAccess:				SInt8;
		mode:					UInt16;
		reserved2:				UInt32;
	end;

	{   CatalogInfoBitmap describes which fields of the CatalogInfo you wish to get or set. }
	FSCatalogInfoBitmap					= UInt32;

const
	kFSCatInfoNone				= $00000000;
	kFSCatInfoTextEncoding		= $00000001;
	kFSCatInfoNodeFlags			= $00000002;					{  Locked (bit 0) and directory (bit 4) only  }
	kFSCatInfoVolume			= $00000004;
	kFSCatInfoParentDirID		= $00000008;
	kFSCatInfoNodeID			= $00000010;
	kFSCatInfoCreateDate		= $00000020;
	kFSCatInfoContentMod		= $00000040;
	kFSCatInfoAttrMod			= $00000080;
	kFSCatInfoAccessDate		= $00000100;
	kFSCatInfoBackupDate		= $00000200;
	kFSCatInfoPermissions		= $00000400;					{  Should this be finer granularity?  }
	kFSCatInfoFinderInfo		= $00000800;
	kFSCatInfoFinderXInfo		= $00001000;
	kFSCatInfoValence			= $00002000;					{  Folders only, zero for files  }
	kFSCatInfoDataSizes			= $00004000;					{  Data fork logical and physical size  }
	kFSCatInfoRsrcSizes			= $00008000;					{  Resource fork logical and physical size  }
	kFSCatInfoSharingFlags		= $00010000;					{  sharingFlags: kioFlAttribMountedBit, kioFlAttribSharePointBit  }
	kFSCatInfoUserPrivs			= $00020000;					{  userPrivileges  }
	kFSCatInfoUserAccess		= $00080000;					{  (OS X only)  }
	kFSCatInfoAllDates			= $000003E0;
	kFSCatInfoGettableInfo		= $0003FFFF;
	kFSCatInfoSettableInfo		= $00001FE3;					{  flags, dates, permissions, Finder info, text encoding  }
	kFSCatInfoReserved			= $FFFC0000;					{  bits that are currently reserved  }

	{	  Constants for nodeFlags field of FSCatalogInfo 	}
	kFSNodeLockedBit			= 0;							{  Set if file or directory is locked  }
	kFSNodeLockedMask			= $0001;
	kFSNodeResOpenBit			= 2;							{  Set if the resource fork is open  }
	kFSNodeResOpenMask			= $0004;
	kFSNodeDataOpenBit			= 3;							{  Set if the data fork is open  }
	kFSNodeDataOpenMask			= $0008;
	kFSNodeIsDirectoryBit		= 4;							{  Set if the object is a directory  }
	kFSNodeIsDirectoryMask		= $0010;
	kFSNodeCopyProtectBit		= 6;
	kFSNodeCopyProtectMask		= $0040;
	kFSNodeForkOpenBit			= 7;							{  Set if the file or directory has any open fork  }
	kFSNodeForkOpenMask			= $0080;

	{	  Constants for sharingFlags field of FSCatalogInfo 	}
	kFSNodeInSharedBit			= 2;							{  Set if a directory is within a share point  }
	kFSNodeInSharedMask			= $0004;
	kFSNodeIsMountedBit			= 3;							{  Set if a directory is a share point currently mounted by some user  }
	kFSNodeIsMountedMask		= $0008;
	kFSNodeIsSharePointBit		= 5;							{  Set if a directory is a share point (exported volume)  }
	kFSNodeIsSharePointMask		= $0020;


type
	FSCatalogInfoPtr = ^FSCatalogInfo;
	FSCatalogInfo = record
		nodeFlags:				UInt16;									{  node flags  }
		volume:					FSVolumeRefNum;							{  object's volume ref  }
		parentDirID:			UInt32;									{  parent directory's ID  }
		nodeID:					UInt32;									{  file/directory ID  }
		sharingFlags:			SInt8;									{  kioFlAttribMountedBit and kioFlAttribSharePointBit  }
		userPrivileges:			SInt8;									{  user's effective AFP privileges (same as ioACUser)  }
		reserved1:				SInt8;
		reserved2:				SInt8;
		createDate:				UTCDateTime;							{  date and time of creation  }
		contentModDate:			UTCDateTime;							{  date and time of last fork modification  }
		attributeModDate:		UTCDateTime;							{  date and time of last attribute modification  }
		accessDate:				UTCDateTime;							{  date and time of last access (for Mac OS X)  }
		backupDate:				UTCDateTime;							{  date and time of last backup  }
		permissions:			array [0..3] of UInt32;					{  permissions (for Mac OS X)  }
		finderInfo:				packed array [0..15] of UInt8;			{  Finder information part 1  }
		extFinderInfo:			packed array [0..15] of UInt8;			{  Finder information part 2  }
		dataLogicalSize:		UInt64;									{  files only  }
		dataPhysicalSize:		UInt64;									{  files only  }
		rsrcLogicalSize:		UInt64;									{  files only  }
		rsrcPhysicalSize:		UInt64;									{  files only  }
		valence:				UInt32;									{  folders only  }
		textEncodingHint:		TextEncoding;
	end;

	FSRefParamPtr = ^FSRefParam;
	FSRefParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				ConstStringPtr;							{ ptr to Vol:FileName string }
		ioVRefNum:				SInt16;								{ volume refnum (DrvNum for Eject and MountVol) }
		reserved1:				SInt16;									{  was ioRefNum  }
		reserved2:				SInt8;									{  was ioVersNum  }
		reserved3:				SInt8;									{  was ioPermssn  }
		ref:					FSRefPtr;								{  Input ref; the target of the call  }
		whichInfo:				FSCatalogInfoBitmap;
		catInfo:				FSCatalogInfoPtr;
		nameLength:				UniCharCount;							{  input name length for create/rename  }
		name:					UniCharPtr;								{  input name for create/rename  }
		ioDirID:				UInt32;
		spec:					FSSpecPtr;
		parentRef:				FSRefPtr;								{  ref of directory to move another ref to  }
		newRef:					FSRefPtr;								{  Output ref  }
		textEncodingHint:		TextEncoding;							{  for Rename, MakeFSRefUnicode  }
		outName:				HFSUniStr255Ptr;						{  Output name for GetCatalogInfo  }
	end;

	FSIterator    = ^SInt32; { an opaque 32-bit type }
	FSIteratorPtr = ^FSIterator;  { when a var xx:FSIterator parameter can be nil, it is changed to xx: FSIteratorPtr }

const
	kFSIterateFlat				= 0;							{  Immediate children of container only  }
	kFSIterateSubtree			= 1;							{  Entire subtree rooted at container  }
	kFSIterateDelete			= 2;
	kFSIterateReserved			= $FFFFFFFC;


type
	FSIteratorFlags						= OptionBits;

const
																{  CatalogSearch constants  }
	fsSBNodeID					= $00008000;					{  search by range of nodeID  }
	fsSBAttributeModDate		= $00010000;					{  search by range of attributeModDate  }
	fsSBAccessDate				= $00020000;					{  search by range of accessDate  }
	fsSBPermissions				= $00040000;					{  search by value/mask of permissions  }
	fsSBNodeIDBit				= 15;
	fsSBAttributeModDateBit		= 16;
	fsSBAccessDateBit			= 17;
	fsSBPermissionsBit			= 18;


type
	FSSearchParamsPtr = ^FSSearchParams;
	FSSearchParams = record
		searchTime:				Duration;								{  a Time Manager duration  }
		searchBits:				OptionBits;								{  which fields to search on  }
		searchNameLength:		UniCharCount;
		searchName:				UniCharPtr;
		searchInfo1:			FSCatalogInfoPtr;						{  values and lower bounds  }
		searchInfo2:			FSCatalogInfoPtr;						{  masks and upper bounds  }
	end;

	FSCatalogBulkParamPtr = ^FSCatalogBulkParam;
	FSCatalogBulkParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		containerChanged:		boolean;								{  true if container changed since last iteration  }
		reserved:				SInt8;									{  make following fields 4-byte aligned  }
		iteratorFlags:			FSIteratorFlags;
		iterator:				FSIterator;
		container:				FSRefPtr;								{  directory/volume to iterate  }
		maximumItems:			ItemCount;
		actualItems:			ItemCount;
		whichInfo:				FSCatalogInfoBitmap;
		catalogInfo:			FSCatalogInfoPtr;						{  returns an array  }
		refs:					FSRefPtr;								{  returns an array  }
		specs:					FSSpecPtr;								{  returns an array  }
		names:					HFSUniStr255Ptr;						{  returns an array  }
		searchParams:			FSSearchParamsPtr;
	end;

	FSAllocationFlags					= UInt16;

const
	kFSAllocDefaultFlags		= $0000;						{  as much as possible, not contiguous  }
	kFSAllocAllOrNothingMask	= $0001;						{  allocate all of the space, or nothing  }
	kFSAllocContiguousMask		= $0002;						{  new space must be one contiguous piece  }
	kFSAllocNoRoundUpMask		= $0004;						{  don't round up allocation to clump size  }
	kFSAllocReservedMask		= $FFF8;						{  these bits are reserved and must not be set  }


type
	FSForkIOParamPtr = ^FSForkIOParam;
	FSForkIOParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		reserved1:				Ptr;									{  was ioNamePtr  }
		reserved2:				SInt16;									{  was ioVRefNum  }
		forkRefNum:				SInt16;									{  same as ioRefNum  }
		reserved3:				SInt8;									{  was ioVersNum  }
		permissions:			SInt8;									{  desired access to the fork  }
		ref:					FSRefPtr;								{  which object to open  }
		buffer:					Ptr;									{ data buffer Ptr }
		requestCount:			UInt32;									{ requested byte count }
		actualCount:			UInt32;									{ actual byte count completed }
		positionMode:			UInt16;									{ initial file positioning }
		positionOffset:			SInt64;									{ file position offset }
		allocationFlags:		FSAllocationFlags;
		allocationAmount:		UInt64;
		forkNameLength:			UniCharCount;							{  input; length of fork name  }
		forkName:				UniCharPtr;								{  input; name of fork  }
		forkIterator:			CatPositionRec;
		outForkName:			HFSUniStr255Ptr;						{  output; name of fork  }
	end;

	FSForkInfoPtr = ^FSForkInfo;
	FSForkInfo = record
		flags:					SInt8;									{  copy of FCB flags  }
		permissions:			SInt8;
		volume:					FSVolumeRefNum;
		reserved2:				UInt32;
		nodeID:					UInt32;									{  file or directory ID  }
		forkID:					UInt32;									{  fork ID  }
		currentPosition:		UInt64;
		logicalEOF:				UInt64;
		physicalEOF:			UInt64;
		process:				UInt64;									{  should be ProcessSerialNumber  }
	end;

	FSForkCBInfoParamPtr = ^FSForkCBInfoParam;
	FSForkCBInfoParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		desiredRefNum:			SInt16;									{  0 to iterate, non-0 for specific refnum  }
		volumeRefNum:			SInt16;									{  volume to match, or 0 for all volumes  }
		iterator:				SInt16;									{  0 to start iteration  }
		actualRefNum:			SInt16;									{  actual refnum found  }
		ref:					FSRefPtr;
		forkInfo:				FSForkInfoPtr;
		forkName:				HFSUniStr255Ptr;
	end;

	FSVolumeInfoBitmap					= UInt32;

const
	kFSVolInfoNone				= $0000;
	kFSVolInfoCreateDate		= $0001;
	kFSVolInfoModDate			= $0002;
	kFSVolInfoBackupDate		= $0004;
	kFSVolInfoCheckedDate		= $0008;
	kFSVolInfoFileCount			= $0010;
	kFSVolInfoDirCount			= $0020;
	kFSVolInfoSizes				= $0040;						{  totalBytes and freeBytes  }
	kFSVolInfoBlocks			= $0080;						{  blockSize, totalBlocks, freeBlocks  }
	kFSVolInfoNextAlloc			= $0100;
	kFSVolInfoRsrcClump			= $0200;
	kFSVolInfoDataClump			= $0400;
	kFSVolInfoNextID			= $0800;
	kFSVolInfoFinderInfo		= $1000;
	kFSVolInfoFlags				= $2000;
	kFSVolInfoFSInfo			= $4000;						{  filesystemID, signature  }
	kFSVolInfoDriveInfo			= $8000;						{  driveNumber, driverRefNum  }
	kFSVolInfoGettableInfo		= $FFFF;						{  This seems like it is here just for completeness  }
	kFSVolInfoSettableInfo		= $3004;						{  backup date, Finder info, flags  }

	{	 FSVolumeInfo.flags bits.  These are the same as for ioVAtrb, but with nicer names. 	}
	kFSVolFlagDefaultVolumeBit	= 5;							{  Set if the volume is the default volume  }
	kFSVolFlagDefaultVolumeMask	= $0020;
	kFSVolFlagFilesOpenBit		= 6;							{  Set if there are open files or iterators  }
	kFSVolFlagFilesOpenMask		= $0040;
	kFSVolFlagHardwareLockedBit	= 7;							{  Set if volume is locked by a hardware setting  }
	kFSVolFlagHardwareLockedMask = $0080;
	kFSVolFlagSoftwareLockedBit	= 15;							{  Set if volume is locked by software  }
	kFSVolFlagSoftwareLockedMask = $8000;


type
	FSVolumeInfoPtr = ^FSVolumeInfo;
	FSVolumeInfo = record
																		{  Dates -- zero means "never" or "unknown"  }
		createDate:				UTCDateTime;
		modifyDate:				UTCDateTime;
		backupDate:				UTCDateTime;
		checkedDate:			UTCDateTime;
																		{  File/Folder counts -- return zero if unknown  }
		fileCount:				UInt32;									{  total files on volume  }
		folderCount:			UInt32;									{  total folders on volume  }
																		{  Note: no root directory counts  }
		totalBytes:				UInt64;									{  total number of bytes on volume  }
		freeBytes:				UInt64;									{  number of free bytes on volume  }
																		{  HFS and HFS Plus specific.  Set fields to zero if not appropriate  }
		blockSize:				UInt32;									{  size (in bytes) of allocation blocks  }
		totalBlocks:			UInt32;									{  number of allocation blocks in volume  }
		freeBlocks:				UInt32;									{  number of unused allocation blocks  }
		nextAllocation:			UInt32;									{  start of next allocation search  }
		rsrcClumpSize:			UInt32;									{  default resource fork clump size  }
		dataClumpSize:			UInt32;									{  default data fork clump size  }
		nextCatalogID:			UInt32;									{  next unused catalog node ID ¥¥¥ OYG ¥¥¥ need to make HFSVolumes.h work Should be HFSCatalogNodeID }
		finderInfo:				packed array [0..31] of UInt8;			{  information used by Finder  }
																		{  Identifying information  }
		flags:					UInt16;									{  ioVAtrb  }
		filesystemID:			UInt16;									{  ioVFSID  }
		signature:				UInt16;									{  ioVSigWord, unique within an FSID  }
		driveNumber:			UInt16;									{  ioVDrvInfo  }
		driverRefNum:			SInt16;								{  ioVDRefNum  }
	end;

	FSVolumeInfoParamPtr = ^FSVolumeInfoParam;
	FSVolumeInfoParam = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{  unused  }
		ioVRefNum:				FSVolumeRefNum;							{  volume refnum  }
		volumeIndex:			UInt32;									{  index, or 0 to use ioVRefNum  }
		whichInfo:				FSVolumeInfoBitmap;						{  which volumeInfo fields to get/set  }
		volumeInfo:				FSVolumeInfoPtr;						{  information about the volume  }
		volumeName:				HFSUniStr255Ptr;						{  output; pointer to volume name  }
		ref:					FSRefPtr;								{  volume's FSRef  }
	end;

	{
	    MakeFSRef
	    Create an FSRef for an existing object specified by a combination
	    of volume refnum, parent directory, and pathname.
	    ->  ioCompletion    A pointer to a completion routine
	    <-  ioResult        The result code of the function
	    ->  ioNamePtr       A pointer to a pathname
	    ->  ioVRefNum       A volume specification
	    ->  ioDirID         A directory ID
	    <-  newRef          A pointer to an FSRef
	}
	{
	 *  FSpMakeFSRef()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FSpMakeFSRef(const (*var*) source: FSSpec; var newRef: FSRef): OSErr; external name '_FSpMakeFSRef';
{
 *  PBMakeFSRefSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBMakeFSRefSync(var paramBlock: FSRefParam): OSErr; external name '_PBMakeFSRefSync';
{
 *  PBMakeFSRefAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBMakeFSRefAsync(var paramBlock: FSRefParam); external name '_PBMakeFSRefAsync';
{
    MakeFSRefUnicode
    Create an FSRef for an existing object specified by 
    Parent FSRef and Unicode name.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             A pointer to the parent directory FSRef
    ->  name            A pointer to Unicde name
    ->  nameLength      The length of the Unicode Name
    ->  textEncodingHint A suggested text encoding to use for the name
    <-  newRef          A pointer to an FSRef
}
{
 *  FSMakeFSRefUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSMakeFSRefUnicode(const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; textEncodingHint: TextEncoding; var newRef: FSRef): OSErr; external name '_FSMakeFSRefUnicode';
{
 *  PBMakeFSRefUnicodeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBMakeFSRefUnicodeSync(var paramBlock: FSRefParam): OSErr; external name '_PBMakeFSRefUnicodeSync';
{
 *  PBMakeFSRefUnicodeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBMakeFSRefUnicodeAsync(var paramBlock: FSRefParam); external name '_PBMakeFSRefUnicodeAsync';
{
    CompareFSRefs
    Test whether two FSRefs refer to the same file or directory.
    If they do, noErr is returned.  Otherwise, an appropriate error
    (such as errFSRefsDifferent) is returned.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             A pointer to the first FSRef
    ->  parentRef       A pointer to the second FSRef
}
{
 *  FSCompareFSRefs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCompareFSRefs(const (*var*) ref1: FSRef; const (*var*) ref2: FSRef): OSErr; external name '_FSCompareFSRefs';
{
 *  PBCompareFSRefsSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCompareFSRefsSync(var paramBlock: FSRefParam): OSErr; external name '_PBCompareFSRefsSync';
{
 *  PBCompareFSRefsAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCompareFSRefsAsync(var paramBlock: FSRefParam); external name '_PBCompareFSRefsAsync';
{
    CreateFileUnicode
    Creates a new file.  The input filename is in Unicode.
    You can optionally set catalog info for the file.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The directory where the file is to be created
    ->  whichInfo       Which catalog info fields to set
    ->  catInfo         The values for catalog info fields to set; may be NULL
    ->  nameLength      Number of Unicode characters in the file's name
    ->  name            A pointer to the Unicode name
    <-  spec            A pointer to the FSSpec for the new directory; may be NULL
    <-  newRef          A pointer to the FSRef for the new file; may be NULL
}
{
 *  FSCreateFileUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCreateFileUnicode(const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; whichInfo: FSCatalogInfoBitmap; catalogInfo: {Const}FSCatalogInfoPtr; newRef: FSRefPtr; newSpec: FSSpecPtr): OSErr; external name '_FSCreateFileUnicode';
{
 *  PBCreateFileUnicodeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCreateFileUnicodeSync(var paramBlock: FSRefParam): OSErr; external name '_PBCreateFileUnicodeSync';
{
 *  PBCreateFileUnicodeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCreateFileUnicodeAsync(var paramBlock: FSRefParam); external name '_PBCreateFileUnicodeAsync';
{
    CreateDirectoryUnicode
    Creates a new directory.  The input directory name is in Unicode.
    You can optionally set catalog info for the directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The parent directory where the directory is to be created
    ->  whichInfo       Which catalog info fields to set
    ->  catInfo         The values for catalog info fields to set; may be NULL
    ->  nameLength      Number of Unicode characters in the directory's name
    ->  name            A pointer to the Unicode name
    <-  ioDirID         The DirID of the new directory
    <-  spec            A pointer to the FSSpec for the new directory; may be NULL
    <-  newRef          A pointer to the FSRef for the new directory; may be NULL
}
{
 *  FSCreateDirectoryUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCreateDirectoryUnicode(const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; whichInfo: FSCatalogInfoBitmap; catalogInfo: {Const}FSCatalogInfoPtr; newRef: FSRefPtr; newSpec: FSSpecPtr; newDirID: UInt32Ptr): OSErr; external name '_FSCreateDirectoryUnicode';
{
 *  PBCreateDirectoryUnicodeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCreateDirectoryUnicodeSync(var paramBlock: FSRefParam): OSErr; external name '_PBCreateDirectoryUnicodeSync';
{
 *  PBCreateDirectoryUnicodeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCreateDirectoryUnicodeAsync(var paramBlock: FSRefParam); external name '_PBCreateDirectoryUnicodeAsync';
{
    DeleteObject
    Deletes an existing file or directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory to be deleted
}
{
 *  FSDeleteObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSDeleteObject(const (*var*) ref: FSRef): OSErr; external name '_FSDeleteObject';
{
 *  PBDeleteObjectSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDeleteObjectSync(var paramBlock: FSRefParam): OSErr; external name '_PBDeleteObjectSync';
{
 *  PBDeleteObjectAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBDeleteObjectAsync(var paramBlock: FSRefParam); external name '_PBDeleteObjectAsync';
{
    MoveObject
    Move an existing file or directory into a different directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory to be moved
    ->  parentRef       The file or directory will be moved into this directory
    <-  newRef          A new FSRef for the file or directory in its new location;
                        optional, may be NULL
    NOTE: Moving an object may change its FSRef.  If you want to continue to
    refer to the object, you should pass a non-NULL pointer in newRef and use
    that returned FSRef to access the object after the move.  The FSRef passed
    in "ref" may or may not be usable to access the object after it is moved.
    "newRef" may point to the same storage as "parentRef" or "ref".
}
{
 *  FSMoveObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSMoveObject(const (*var*) ref: FSRef; const (*var*) destDirectory: FSRef; newRef: FSRefPtr): OSErr; external name '_FSMoveObject';
{
 *  PBMoveObjectSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBMoveObjectSync(var paramBlock: FSRefParam): OSErr; external name '_PBMoveObjectSync';
{
 *  PBMoveObjectAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBMoveObjectAsync(var paramBlock: FSRefParam); external name '_PBMoveObjectAsync';
{
    ExchangeObjects
    swap the contents of two files.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The first file 
    ->  parentRef       The second file 
}
{
 *  FSExchangeObjects()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSExchangeObjects(const (*var*) ref: FSRef; const (*var*) destRef: FSRef): OSErr; external name '_FSExchangeObjects';
{
 *  PBExchangeObjectsSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBExchangeObjectsSync(var paramBlock: FSRefParam): OSErr; external name '_PBExchangeObjectsSync';
{
 *  PBExchangeObjectsAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBExchangeObjectsAsync(var paramBlock: FSRefParam); external name '_PBExchangeObjectsAsync';
{
    RenameUnicode
    Change the name of an existing file or directory.  The new name is in
    Unicode.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory to be moved
    ->  nameLength      Number of Unicode characters in the new name
    ->  name            A pointer to the new Unicode name
    ->  textEncodingHint A suggested text encoding to use for the name
    <-  newRef          A new FSRef for the file or directory; may be NULL
    NOTE: Renaming an object may change its FSRef.  If you want to continue to
    refer to the object, you should pass a non-NULL pointer in newRef and use
    that returned FSRef to access the object after the rename.  The FSRef passed
    in "ref" may or may not be usable to access the object after it is renamed.
    "newRef" may point to the same storage as "ref".
}
{
 *  FSRenameUnicode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSRenameUnicode(const (*var*) ref: FSRef; nameLength: UniCharCount; name: UniCharPtr; textEncodingHint: TextEncoding; newRef: FSRefPtr): OSErr; external name '_FSRenameUnicode';
{
 *  PBRenameUnicodeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBRenameUnicodeSync(var paramBlock: FSRefParam): OSErr; external name '_PBRenameUnicodeSync';
{
 *  PBRenameUnicodeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBRenameUnicodeAsync(var paramBlock: FSRefParam); external name '_PBRenameUnicodeAsync';
{
    GetCatalogInfo
    Returns various information about a given file or directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory whose information is to be returned
    ->  whichInfo       Which catalog info fields to get
    <-  catInfo         The returned values of catalog info fields; may be NULL
    <-  spec            A pointer to the FSSpec for the object; may be NULL
    <-  parentRef       A pointer to the FSRef for the object's parent directory; may be NULL
    <-  outName         The Unicode name is returned here.  This pointer may be NULL.
    Note: All of the outputs are optional; if you don't want that particular output, just
    set its pointer to NULL.  This is the call to use to map from an FSRef to an FSSpec.
}
{
 *  FSGetCatalogInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetCatalogInfo(const (*var*) ref: FSRef; whichInfo: FSCatalogInfoBitmap; catalogInfo: FSCatalogInfoPtr; outName: HFSUniStr255Ptr; fsSpec: FSSpecPtr; parentRef: FSRefPtr): OSErr; external name '_FSGetCatalogInfo';
{
 *  PBGetCatalogInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetCatalogInfoSync(var paramBlock: FSRefParam): OSErr; external name '_PBGetCatalogInfoSync';
{
 *  PBGetCatalogInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBGetCatalogInfoAsync(var paramBlock: FSRefParam); external name '_PBGetCatalogInfoAsync';
{
    SetCatalogInfo
    Set catalog information about a given file or directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory whose information is to be changed
    ->  whichInfo       Which catalog info fields to set
    ->  catInfo         The new values of catalog info fields
    Note: Only some of the catalog info fields may be set.  The settable fields
    are given by the constant kFSCatInfoSettableInfo; no other bits may be set in
    whichInfo.
}
{
 *  FSSetCatalogInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSSetCatalogInfo(const (*var*) ref: FSRef; whichInfo: FSCatalogInfoBitmap; const (*var*) catalogInfo: FSCatalogInfo): OSErr; external name '_FSSetCatalogInfo';
{
 *  PBSetCatalogInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetCatalogInfoSync(var paramBlock: FSRefParam): OSErr; external name '_PBSetCatalogInfoSync';
{
 *  PBSetCatalogInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBSetCatalogInfoAsync(var paramBlock: FSRefParam); external name '_PBSetCatalogInfoAsync';
{
    OpenIterator
    Creates an FSIterator to iterate over a directory or subtree.  The
    iterator can then be passed to GetCatalogInfoBulk or CatalogSearch.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    <-  iterator        The returned FSIterator
    ->  iteratorFlags   Controls whether the iterator iterates over subtrees
                        or just the immediate children of the container.
    ->  container       An FSRef for the directory to iterate (or root of
                        the subtree to iterate).
}
{
 *  FSOpenIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSOpenIterator(const (*var*) container: FSRef; iteratorFlags: FSIteratorFlags; var iterator: FSIterator): OSErr; external name '_FSOpenIterator';
{
 *  PBOpenIteratorSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBOpenIteratorSync(var paramBlock: FSCatalogBulkParam): OSErr; external name '_PBOpenIteratorSync';
{
 *  PBOpenIteratorAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBOpenIteratorAsync(var paramBlock: FSCatalogBulkParam); external name '_PBOpenIteratorAsync';
{
    CloseIterator
    Invalidates and disposes an FSIterator.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  iterator        The returned FSIterator
}
{
 *  FSCloseIterator()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCloseIterator(iterator: FSIterator): OSErr; external name '_FSCloseIterator';
{
 *  PBCloseIteratorSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCloseIteratorSync(var paramBlock: FSCatalogBulkParam): OSErr; external name '_PBCloseIteratorSync';
{
 *  PBCloseIteratorAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCloseIteratorAsync(var paramBlock: FSCatalogBulkParam); external name '_PBCloseIteratorAsync';
{
    GetCatalogInfoBulk
    Iterates over catalog objects and returns information about them.
    For now, iterator must have been created with kFSIterateFlat option.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  iterator        The iterator
    ->  maximumItems    The maximum number of items to return
    <-  actualItems     The actual number of items returned
    <-  containerChanged Set to true if the container's contents changed
    ->  whichInfo       The catalog information fields to return for each item
    <-  catalogInfo     An array of catalog information; one for each returned item
    <-  refs            An array of FSRefs; one for each returned item
    <-  specs           An array of FSSpecs; one for each returned item
    <-  names           An array of filenames; one for each returned item
    Note: The catalogInfo, refs, specs, names, and containerChanged are all optional outputs;
    if you don't want that particular output, set its pointer to NULL.
}
{
 *  FSGetCatalogInfoBulk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetCatalogInfoBulk(iterator: FSIterator; maximumObjects: ItemCount; var actualObjects: ItemCount; containerChanged: BooleanPtr; whichInfo: FSCatalogInfoBitmap; catalogInfos: FSCatalogInfoPtr; refs: FSRefPtr; specs: FSSpecPtr; names: HFSUniStr255Ptr): OSErr; external name '_FSGetCatalogInfoBulk';
{
 *  PBGetCatalogInfoBulkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetCatalogInfoBulkSync(var paramBlock: FSCatalogBulkParam): OSErr; external name '_PBGetCatalogInfoBulkSync';
{
 *  PBGetCatalogInfoBulkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBGetCatalogInfoBulkAsync(var paramBlock: FSCatalogBulkParam); external name '_PBGetCatalogInfoBulkAsync';
{
    CatalogSearch
    Iterates over catalog objects, searching for objects that match given
    search criteria.  Returns various information about matching objects.
    For now, iterator must have been created with kFSIterateSubtree option
    and the container must have been the root directory of a volume.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  iterator        The iterator
    ->  maximumItems    The maximum number of items to return
    <-  actualItems     The actual number of items returned
    <-  containerChanged Set to true if the container's contents changed
    ->  whichInfo       The catalog information fields to return for each item
    <-  catalogInfo     An array of catalog information; one for each returned item
    <-  refs            An array of FSRefs; one for each returned item
    <-  specs           An array of FSSpecs; one for each returned item
    <-  names           An array of filenames; one for each returned item
    ->  searchParams    The criteria that controls the matching, including timeout, a bitmap
                        controlling the fields to compare, and the (Unicode) name to compare.
    Note: The catalogInfo, refs, specs, and names are all optional outputs; if you don't want
    that particular output, set its pointer to NULL.
}
{
 *  FSCatalogSearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCatalogSearch(iterator: FSIterator; const (*var*) searchCriteria: FSSearchParams; maximumObjects: ItemCount; var actualObjects: ItemCount; containerChanged: BooleanPtr; whichInfo: FSCatalogInfoBitmap; catalogInfos: FSCatalogInfoPtr; refs: FSRefPtr; specs: FSSpecPtr; names: HFSUniStr255Ptr): OSErr; external name '_FSCatalogSearch';
{
 *  PBCatalogSearchSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCatalogSearchSync(var paramBlock: FSCatalogBulkParam): OSErr; external name '_PBCatalogSearchSync';
{
 *  PBCatalogSearchAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCatalogSearchAsync(var paramBlock: FSCatalogBulkParam); external name '_PBCatalogSearchAsync';
{
    CreateFork
    Create a named fork for a file or directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory
    ->  forkNameLength  The length of the fork name (in Unicode characters)
    ->  forkName        The name of the fork to open (in Unicode)
}
{
 *  FSCreateFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCreateFork(const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr): OSErr; external name '_FSCreateFork';
{
 *  PBCreateForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCreateForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBCreateForkSync';
{
 *  PBCreateForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCreateForkAsync(var paramBlock: FSForkIOParam); external name '_PBCreateForkAsync';
{
    DeleteFork
    Delete a named fork of a file or directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory
    ->  forkNameLength  The length of the fork name (in Unicode characters)
    ->  forkName        The name of the fork to open (in Unicode)
}
{
 *  FSDeleteFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSDeleteFork(const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr): OSErr; external name '_FSDeleteFork';
{
 *  PBDeleteForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBDeleteForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBDeleteForkSync';
{
 *  PBDeleteForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBDeleteForkAsync(var paramBlock: FSForkIOParam); external name '_PBDeleteForkAsync';
{
    IterateForks
    Return the names and sizes of the forks of a file or directory.
    One fork is returned per call.
    ->  ioCompletion    A pointer to a completion routine.
    <-  ioResult        The result code of the function.
    ->  ref             The file or directory containing the forks.
    <-  positionOffset  The length of the fork, in bytes.
    <-  allocationAmount The space allocated to the fork (physical length).
    <-  outForkName     The name of the fork in Unicode.
    <>  forkIterator    Maintains state between calls for a given FSRef.
                        Before the first call, set the initialize field to zero.
}
{
 *  FSIterateForks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSIterateForks(const (*var*) ref: FSRef; var forkIterator: CatPositionRec; forkName: HFSUniStr255Ptr; forkSize: SInt64Ptr; forkPhysicalSize: UInt64Ptr): OSErr; external name '_FSIterateForks';
{
 *  PBIterateForksSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBIterateForksSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBIterateForksSync';
{
 *  PBIterateForksAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBIterateForksAsync(var paramBlock: FSForkIOParam); external name '_PBIterateForksAsync';
{
    OpenFork
    Open a fork for reading and/or writing.  Allows the opened fork
    to grow beyond 2GB in size.  All volumes should support data and
    resource forks.  Other named forks may be supported by some
    volumes.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory containing the fork to open
    ->  forkNameLength  The length of the fork name (in Unicode characters)
    ->  forkName        The name of the fork to open (in Unicode)
    ->  permissions     The access (read and/or write) you want
    <-  forkRefNum      The reference number for accessing the open fork
}
{
 *  FSOpenFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSOpenFork(const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr; permissions: SInt8; var forkRefNum: SInt16): OSErr; external name '_FSOpenFork';
{
 *  PBOpenForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBOpenForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBOpenForkSync';
{
 *  PBOpenForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBOpenForkAsync(var paramBlock: FSForkIOParam); external name '_PBOpenForkAsync';
{
    ReadFork
    Read data from a fork opened via OpenFork.  The first byte to read is
    indicated by a combination of positionMode and positionOffset.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork to read from
    <-  buffer          Pointer to buffer where data will be returned
    ->  requestCount    The number of bytes to read
    <-  actualCount     The number of bytes actually read
    ->  positionMode    The base location for start of read
    ->  positionOffset  The offset from base location for start of read
}
{
 *  FSReadFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSReadFork(forkRefNum: SInt16; positionMode: UInt16; positionOffset: SInt64; requestCount: ByteCount; buffer: UnivPtr; actualCount: ByteCountPtr): OSErr; external name '_FSReadFork';
{
 *  PBReadForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBReadForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBReadForkSync';
{
 *  PBReadForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBReadForkAsync(var paramBlock: FSForkIOParam); external name '_PBReadForkAsync';
{
    WriteFork
    Write data to a fork opened via OpenFork.  The first byte to write is
    indicated by a combination of positionMode and positionOffset.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork to write to
    ->  buffer          Pointer to data to write
    ->  requestCount    The number of bytes to write
    <-  actualCount     The number of bytes actually written
    ->  positionMode    The base location for start of write
    ->  positionOffset  The offset from base location for start of write
}
{
 *  FSWriteFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSWriteFork(forkRefNum: SInt16; positionMode: UInt16; positionOffset: SInt64; requestCount: ByteCount; buffer: UnivPtr; actualCount: ByteCountPtr): OSErr; external name '_FSWriteFork';
{
 *  PBWriteForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBWriteForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBWriteForkSync';
{
 *  PBWriteForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBWriteForkAsync(var paramBlock: FSForkIOParam); external name '_PBWriteForkAsync';
{
    GetForkPosition
    Get the current (default) position of a fork that was
    opened via OpenFork.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork
    <-  positionOffset  The current position of the fork
}
{
 *  FSGetForkPosition()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetForkPosition(forkRefNum: SInt16; var position: SInt64): OSErr; external name '_FSGetForkPosition';
{
 *  PBGetForkPositionSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetForkPositionSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBGetForkPositionSync';
{
 *  PBGetForkPositionAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBGetForkPositionAsync(var paramBlock: FSForkIOParam); external name '_PBGetForkPositionAsync';
{
    SetForkPosition
    Set the current (default) position of a fork that was
    opened via OpenFork.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork
    ->  positionMode    The base location for the new position
    ->  positionOffset  The offset of the new position from the base
}
{
 *  FSSetForkPosition()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSSetForkPosition(forkRefNum: SInt16; positionMode: UInt16; positionOffset: SInt64): OSErr; external name '_FSSetForkPosition';
{
 *  PBSetForkPositionSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetForkPositionSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBSetForkPositionSync';
{
 *  PBSetForkPositionAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBSetForkPositionAsync(var paramBlock: FSForkIOParam); external name '_PBSetForkPositionAsync';
{
    GetForkSize
    Get the current logical size (end-of-file) of an open fork.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork
    <-  positionOffset  The logical size of the fork, in bytes
}
{
 *  FSGetForkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetForkSize(forkRefNum: SInt16; var forkSize: SInt64): OSErr; external name '_FSGetForkSize';
{
 *  PBGetForkSizeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetForkSizeSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBGetForkSizeSync';
{
 *  PBGetForkSizeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBGetForkSizeAsync(var paramBlock: FSForkIOParam); external name '_PBGetForkSizeAsync';
{
    SetForkSize
    Set the logical size (end-of-file) of an open fork.  This
    may cause space to be allocated or deallocated.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork
    ->  positionMode    The base location for the new size
    ->  positionOffset  The offset of the new size from the base
}
{
 *  FSSetForkSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSSetForkSize(forkRefNum: SInt16; positionMode: UInt16; positionOffset: SInt64): OSErr; external name '_FSSetForkSize';
{
 *  PBSetForkSizeSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetForkSizeSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBSetForkSizeSync';
{
 *  PBSetForkSizeAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBSetForkSizeAsync(var paramBlock: FSForkIOParam); external name '_PBSetForkSizeAsync';
{
    AllocateFork
    Allocate space to an open fork.  Typically, the space to be
    allocated is beyond the current size of the fork, to reserve
    space so the file will be able to grow later.  Some volume
    formats are unable to allocate space beyond the logical size
    of the fork.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork
    ->  positionMode    The base location for start of allocation
    ->  positionOffset  The offset of the start of allocation
    ->  allocationFlags Zero or more of the following flags:
        kFSAllocContiguousMask
                Any newly allocated space must be one contiguous piece.
        kFSAllocAllOrNothingMask
                All of the request space must be available, or the call
                will fail.  (If not set, the call may succeed even though
                some of the requested space wasn't allocated.)
        kFSAllocNoRoundUpMask
                Do not allocate additional space.  (If not set, a volume
                may allocate additional space in order to reduce fragmentation.)
    <>  allocationAmount    The number of bytes to allocate
                            On output, the number of bytes actually added
}
{
 *  FSAllocateFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSAllocateFork(forkRefNum: SInt16; flags: FSAllocationFlags; positionMode: UInt16; positionOffset: SInt64; requestCount: UInt64; actualCount: UInt64Ptr): OSErr; external name '_FSAllocateFork';
{
 *  PBAllocateForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBAllocateForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBAllocateForkSync';
{
 *  PBAllocateForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBAllocateForkAsync(var paramBlock: FSForkIOParam); external name '_PBAllocateForkAsync';
{
    FlushFork
    Flush a fork.  Any data written to this fork refnum is flushed to the device.
    The volume's control structures are also flushed to the device.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork to flush
}
{
 *  FSFlushFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSFlushFork(forkRefNum: SInt16): OSErr; external name '_FSFlushFork';
{
 *  PBFlushForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBFlushForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBFlushForkSync';
{
 *  PBFlushForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBFlushForkAsync(var paramBlock: FSForkIOParam); external name '_PBFlushForkAsync';
{
    CloseFork
    Flush and close a fork.  Any data written to this fork refnum is flushed
    to the device.  The volume's control structures are also flushed to the device.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork to close
}
{
 *  FSCloseFork()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSCloseFork(forkRefNum: SInt16): OSErr; external name '_FSCloseFork';
{
 *  PBCloseForkSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBCloseForkSync(var paramBlock: FSForkIOParam): OSErr; external name '_PBCloseForkSync';
{
 *  PBCloseForkAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBCloseForkAsync(var paramBlock: FSForkIOParam); external name '_PBCloseForkAsync';
{
    GetForkCBInfo
    Return information about an open fork.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    <>  desiredRefNum   If non-zero on input, then get information for this refnum;
                        unchanged on output.  If zero on input, iterate over all open
                        forks (possibly limited to a single volume); on output, contains
                        the fork's refnum.
    ->  volumeRefNum    Used when desiredRefNum is zero on input.  Set to 0 to iterate over all
                        volumes, or set to a FSVolumeRefNum to limit iteration to that volume.
    <>  iterator        Used when desiredRefNum is zero on input.  Set to 0 before iterating.
                        Pass the iterator returned by the previous call to continue iterating.
    <-  actualRefNum    The refnum of the open fork.
    <-  ref             The FSRef for the file or directory that contains the fork.
    <-  forkInfo        Various information about the open fork.
    <-  outForkName     The name of the fork
    Note: the foundRefNum, ref, forkInfo, and fork name outputs are all optional; if you don't want
    a particular output, then set its pointer to NULL.  If forkName is NULL, then forkNameLength
    will be undefined.
    Note: Returning the forkInfo generally does not require a disk access.  Returning the
    ref or forkName may cause disk access for some volume formats.
}
{
 *  FSGetForkCBInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetForkCBInfo(desiredRefNum: SInt16; volume: FSVolumeRefNum; iterator: SInt16Ptr; actualRefNum: SInt16Ptr; forkInfo: FSForkInfoPtr; ref: FSRefPtr; outForkName: HFSUniStr255Ptr): OSErr; external name '_FSGetForkCBInfo';
{
 *  PBGetForkCBInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetForkCBInfoSync(var paramBlock: FSForkCBInfoParam): OSErr; external name '_PBGetForkCBInfoSync';
{
 *  PBGetForkCBInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBGetForkCBInfoAsync(var paramBlock: FSForkCBInfoParam); external name '_PBGetForkCBInfoAsync';
{
    GetVolumeInfo
    Returns various information about a given volume, or indexing over all volumes.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    <>  ioVRefNum       On input, the volume reference number or drive number whose
                        information is to be returned (if volumeIndex is 0); same
                        as "volume" input to FSGetVolumeInfo.
                        On output, the actual volume reference number; same as
                        "actualVolume" output of FSGetVolumeInfo.
    ->  volumeIndex     The index of the desired volume, or 0 to use ioVRefNum
    ->  whichInfo       Which volInfo info fields to get
    <-  volumeInfo      The returned values of Volume info fields; may be NULL
    <-  name            The Unicode name is returned here.  This pointer may be NULL.
    Note: All of the outputs are optional; if you don't want that particular output, just
    set its pointer to NULL.
}
{
 *  FSGetVolumeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetVolumeInfo(volume: FSVolumeRefNum; volumeIndex: ItemCount; actualVolume: FSVolumeRefNumPtr; whichInfo: FSVolumeInfoBitmap; info: FSVolumeInfoPtr; volumeName: HFSUniStr255Ptr; rootDirectory: FSRefPtr): OSErr; external name '_FSGetVolumeInfo';
{
 *  PBGetVolumeInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBGetVolumeInfoSync(var paramBlock: FSVolumeInfoParam): OSErr; external name '_PBGetVolumeInfoSync';
{
 *  PBGetVolumeInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBGetVolumeInfoAsync(var paramBlock: FSVolumeInfoParam); external name '_PBGetVolumeInfoAsync';
{
    SetVolumeInfo
    Set information about a given volume.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ioVRefNum       The volume whose information is to be changed
    ->  whichInfo       Which catalog info fields to set
    ->  volumeInfo      The new values of volume info fields
    Note: Only some of the volume info fields may be set.  The settable fields
    are given by the constant kFSVolInfoSettableInfo; no other bits may be set in
    whichInfo.
}
{
 *  FSSetVolumeInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSSetVolumeInfo(volume: FSVolumeRefNum; whichInfo: FSVolumeInfoBitmap; const (*var*) info: FSVolumeInfo): OSErr; external name '_FSSetVolumeInfo';
{
 *  PBSetVolumeInfoSync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PBSetVolumeInfoSync(var paramBlock: FSVolumeInfoParam): OSErr; external name '_PBSetVolumeInfoSync';
{
 *  PBSetVolumeInfoAsync()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PBSetVolumeInfoAsync(var paramBlock: FSVolumeInfoParam); external name '_PBSetVolumeInfoAsync';
{
    FSGetDataForkName
    Returns the constant for the name of the data fork (the empty string)
}
{
 *  FSGetDataForkName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetDataForkName(var dataForkName: HFSUniStr255): OSErr; external name '_FSGetDataForkName';
{
    FSGetResourceForkName
    Returns the constant for the name of the resource fork
    (currently "RESOURCE_FORK").
}
{
 *  FSGetResourceForkName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSGetResourceForkName(var resourceForkName: HFSUniStr255): OSErr; external name '_FSGetResourceForkName';
{
 *  FSRefMakePath()
 *  
 *  Summary:
 *    Converts an FSRef to a pathname.
 *  
 *  Parameters:
 *    
 *    ref:
 *      An FSRef to the file or directory to get the pathname for.
 *    
 *    path:
 *      A pointer to a buffer which FSRefMakePath will fill with a C
 *      string representing the pathname to the file or directory
 *      specified by the ref parameter. The format of the pathname
 *      returned can be determined with the Gestalt selector
 *      gestaltFSAttr's gestaltFSUsesPOSIXPathsForConversion bit.
 *       If the gestaltFSUsesPOSIXPathsForConversion bit is clear, the
 *      pathname is a Mac OS File Manager full pathname in a C string,
 *      and file or directory names in the pathname may be mangled as
 *      returned by the File Manager.
 *      If the gestaltFSUsesPOSIXPathsForConversion bit is set, the
 *      pathname is a UTF8 encoded POSIX absolute pathname in a C
 *      string.
 *      In either case, the pathname returned can be passed back to
 *      FSRefMakePath to create an FSRef to the file or directory.
 *    
 *    maxPathSize:
 *      The size of the path buffer in bytes. If the path buffer is too
 *      small for the pathname string, FSRefMakePath returns
 *      pathTooLongErr or buffersTooSmall.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSRefMakePath(const (*var*) ref: FSRef; path: CStringPtr; maxPathSize: UInt32): OSStatus; external name '_FSRefMakePath';

{
 *  FSPathMakeRef()
 *  
 *  Summary:
 *    Converts a pathname to an FSRef.
 *  
 *  Parameters:
 *    
 *    path:
 *      A pointer to a C String that is the pathname. The format of the
 *      pathname you must supply can be determined with the Gestalt
 *      selector gestaltFSAttr's gestaltFSUsesPOSIXPathsForConversion
 *      bit.
 *      If the gestaltFSUsesPOSIXPathsForConversion bit is clear, the
 *      pathname must be a Mac OS File Manager full pathname in a C
 *      string.
 *      If the gestaltFSUsesPOSIXPathsForConversion bit is set, the
 *      pathname must be a UTF8 encoded POSIX absolute pathname in a C
 *      string.
 *      In either case, the pathname returned by FSRefMakePath can be
 *      passed to FSPathMakeRef.
 *    
 *    ref:
 *      A pointer to an FSRef to fill in.
 *    
 *    isDirectory:
 *      An optional pointer to a Boolean that will be filled in to
 *      indicate if the specified path is a directory (vs. a file).
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSPathMakeRef(path: CStringPtr; var ref: FSRef; isDirectory: BooleanPtr): OSStatus; external name '_FSPathMakeRef';


{
 *  FNMessage
 *  
 *  Discussion:
 *    Messages broadcast about a directory.  If system clients (such as
 *    the Finder) are interested in changes to a directory, they will
 *    receive notifications when application code broadcasts change
 *    messages about that directory.
 }

type
	FNMessage 					= UInt32;
const
	kFNDirectoryModifiedMessage	= 1;

	{
	 *  FNNotify()
	 *  
	 *  Summary:
	 *    Broadcasts notification of changes to the specified directory.
	 *  
	 *  Parameters:
	 *    
	 *    ref:
	 *      The directory for which to broadcast the notification
	 *    
	 *    message:
	 *      An indication of what happened to the target directory
	 *    
	 *    flags:
	 *      Options about delivery of the notification (specify kNilOptions
	 *      for default behaviour)
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.5 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function FNNotify(const (*var*) ref: FSRef; message: FNMessage; flags: OptionBits): OSStatus; external name '_FNNotify';

{
 *  FNNotifyByPath()
 *  
 *  Summary:
 *    Broadcasts notification of changes to the specified directory.
 *  
 *  Parameters:
 *    
 *    path:
 *      Path to the directory for which to broadcast the notification
 *    
 *    message:
 *      An indication of what happened to the target directory
 *    
 *    flags:
 *      Options about delivery of the notification (specify kNilOptions
 *      for default behaviour)
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNNotifyByPath(path: CStringPtr; message: FNMessage; flags: OptionBits): OSStatus; external name '_FNNotifyByPath';

{
 *  FNNotifyAll()
 *  
 *  Discussion:
 *    Broadcasts notification of changes to the filesystem (should only
 *    be used by installers or programs which make lots of changes and
 *    only send one broadcast).
 *  
 *  Parameters:
 *    
 *    message:
 *      An indication of what happened
 *    
 *    flags:
 *      Options about delivery of the notification (specify kNilOptions
 *      for default behaviour)
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.5 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FNNotifyAll(message: FNMessage; flags: OptionBits): OSStatus; external name '_FNNotifyAll';


{
 *  FNSubscriptionRef
 *  
 *  Discussion:
 *    A token associated with a notification subscription.  The
 *    subscriber gets one back when they subscribe to notifications for
 *    a particular directory, and they supply it when the unsubscribe. 
 *    It is also delivered along with the notifications for that
 *    subscription.
 }

type
	FNSubscriptionRef    = ^SInt32; { an opaque 32-bit type }
	FNSubscriptionRefPtr = ^FNSubscriptionRef;  { when a var xx:FNSubscriptionRef parameter can be nil, it is changed to xx: FNSubscriptionRefPtr }

	{
	 *  Discussion:
	 *    Options that can be specified at subscription time.
	 	}

const
	kFNNoImplicitAllSubscription = $01;


	{
	 *  FNSubscriptionProcPtr
	 *  
	 *  Discussion:
	 *    Callback delivered for directory notifications.
	 *  
	 *  Parameters:
	 *    
	 *    message:
	 *      An indication of what happened
	 *    
	 *    flags:
	 *      Options about delivery of the notification (typically
	 *      kNilOptions)
	 *    
	 *    refcon:
	 *      User reference supplied with subscription
	 *    
	 *    subscription:
	 *      Subscription corresponding to this notification
	 	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	FNSubscriptionProcPtr = procedure(message: FNMessage; flags: OptionBits; refcon: UnivPtr; subscription: FNSubscriptionRef);
{$elsec}
	FNSubscriptionProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	FNSubscriptionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	FNSubscriptionUPP = FNSubscriptionProcPtr;
{$endc}	

const
	uppFNSubscriptionProcInfo = $00003FC1;
	{
	 *  NewFNSubscriptionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib on Mac OS X
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewFNSubscriptionUPP(userRoutine: FNSubscriptionProcPtr): FNSubscriptionUPP; external name '_NewFNSubscriptionUPP';
{
 *  DisposeFNSubscriptionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib on Mac OS X
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeFNSubscriptionUPP(userUPP: FNSubscriptionUPP); external name '_DisposeFNSubscriptionUPP';
{
 *  InvokeFNSubscriptionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib on Mac OS X
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeFNSubscriptionUPP(message: FNMessage; flags: OptionBits; refcon: UnivPtr; subscription: FNSubscriptionRef; userRoutine: FNSubscriptionUPP); external name '_InvokeFNSubscriptionUPP';
{
 *  FNSubscribe()
 *  
 *  Summary:
 *    Subscribe to change notifications for the specified directory.
 *  
 *  Parameters:
 *    
 *    directoryRef:
 *      Directory for which the caller wants notifications
 *    
 *    callback:
 *      Function to call back when a notification arrives
 *    
 *    refcon:
 *      User state carried with the subscription
 *    
 *    flags:
 *      Options for future use (specify kNilOptions, or one of the
 *      FNSubscriptionOptions)
 *    
 *    subscription:
 *      Subscription token for subsequent query or unsubscription
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FNSubscribe(const (*var*) directoryRef: FSRef; callback: FNSubscriptionUPP; refcon: UnivPtr; flags: OptionBits; var subscription: FNSubscriptionRef): OSStatus; external name '_FNSubscribe';

{
 *  FNSubscribeByPath()
 *  
 *  Summary:
 *    Subscribe to change notifications for the specified directory.
 *  
 *  Parameters:
 *    
 *    directoryPath:
 *      Directory for which the caller wants notifications
 *    
 *    callback:
 *      Function to call back when a notification arrives
 *    
 *    refcon:
 *      User state carried with the subscription
 *    
 *    flags:
 *      Options for future use (specify kNilOptions, or one of the
 *      FNSubscriptionOptions)
 *    
 *    subscription:
 *      Subscription token for subsequent query or unsubscription
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FNSubscribeByPath(directoryPath: CStringPtr; callback: FNSubscriptionUPP; refcon: UnivPtr; flags: OptionBits; var subscription: FNSubscriptionRef): OSStatus; external name '_FNSubscribeByPath';

{
 *  FNUnsubscribe()
 *  
 *  Summary:
 *    Release a subscription which is no longer needed.
 *  
 *  Parameters:
 *    
 *    subscription:
 *      Subscription previously returned from FNSubscribe or
 *      FNSubscribeForPath
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FNUnsubscribe(subscription: FNSubscriptionRef): OSStatus; external name '_FNUnsubscribe';

{
 *  FNGetDirectoryForSubscription()
 *  
 *  Summary:
 *    Fetch the directory for which this subscription was originally
 *    entered. There is no path variant because paths are fragile, and
 *    the path may have changed.  If the caller does not care about
 *    this subtlety, she can call FSRefMakePath to get a path from the
 *    returned ref.
 *  
 *  Parameters:
 *    
 *    subscription:
 *      Subscription previously returned from FNSubscribe or
 *      FNSubscribeForPath
 *    
 *    ref:
 *      Directory for which this subscription was created
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function FNGetDirectoryForSubscription(subscription: FNSubscriptionRef; var ref: FSRef): OSStatus; external name '_FNGetDirectoryForSubscription';

{$ALIGN MAC68K}


end.
