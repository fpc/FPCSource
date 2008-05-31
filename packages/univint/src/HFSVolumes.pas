{
     File:       HFSVolumes.p
 
     Contains:   On-disk data structures for HFS and HFS Plus volumes.
 
     Version:    Technology: Mac OS 8.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1984-2002 by Apple Computer, Inc.  All rights reserved.
 
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

unit HFSVolumes;
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
uses MacTypes,Files,Finder;


{$ALIGN MAC68K}

{ Signatures used to differentiate between HFS and HFS Plus volumes }

const
	kHFSSigWord					= $4244;						{  'BD' in ASCII  }
	kHFSPlusSigWord				= $482B;						{  'H+' in ASCII  }
	kHFSPlusVersion				= $0004;						{  will change as format changes (version 4 shipped with Mac OS 8.1)  }
	kHFSPlusMountVersion		= FourCharCode('8.10');						{  will change as implementations change ('8.10' in Mac OS 8.1)  }


	{	 CatalogNodeID is used to track catalog objects 	}

type
	HFSCatalogNodeID					= UInt32;

const
	kHFSMaxVolumeNameChars		= 27;
	kHFSMaxFileNameChars		= 31;
	kHFSPlusMaxFileNameChars	= 255;


	{	 Extent overflow file data structures 	}
	{	 HFS Extent key 	}

type
	HFSExtentKeyPtr = ^HFSExtentKey;
	HFSExtentKey = record
		keyLength:				SInt8;									{  length of key, excluding this field  }
		forkType:				SInt8;									{  0 = data fork, FF = resource fork  }
		fileID:					HFSCatalogNodeID;						{  file ID  }
		startBlock:				UInt16;									{  first file allocation block number in this extent  }
	end;

	{	 HFS Plus Extent key 	}
	HFSPlusExtentKeyPtr = ^HFSPlusExtentKey;
	HFSPlusExtentKey = record
		keyLength:				UInt16;									{  length of key, excluding this field  }
		forkType:				SInt8;									{  0 = data fork, FF = resource fork  }
		pad:					SInt8;									{  make the other fields align on 32-bit boundary  }
		fileID:					HFSCatalogNodeID;						{  file ID  }
		startBlock:				UInt32;									{  first file allocation block number in this extent  }
	end;

	{	 Number of extent descriptors per extent record 	}

const
	kHFSExtentDensity			= 3;
	kHFSPlusExtentDensity		= 8;

	{	 HFS extent descriptor 	}

type
	HFSExtentDescriptorPtr = ^HFSExtentDescriptor;
	HFSExtentDescriptor = record
		startBlock:				UInt16;									{  first allocation block  }
		blockCount:				UInt16;									{  number of allocation blocks  }
	end;

	{	 HFS Plus extent descriptor 	}
	HFSPlusExtentDescriptorPtr = ^HFSPlusExtentDescriptor;
	HFSPlusExtentDescriptor = record
		startBlock:				UInt32;									{  first allocation block  }
		blockCount:				UInt32;									{  number of allocation blocks  }
	end;

	{	 HFS extent record 	}
	HFSExtentRecord						= array [0..2] of HFSExtentDescriptor;
	{	 HFS Plus extent record 	}
	HFSPlusExtentRecord					= array [0..7] of HFSPlusExtentDescriptor;

	{	 Fork data info (HFS Plus only) - 80 bytes 	}
	HFSPlusForkDataPtr = ^HFSPlusForkData;
	HFSPlusForkData = record
		logicalSize:			UInt64;									{  fork's logical size in bytes  }
		clumpSize:				UInt32;									{  fork's clump size in bytes  }
		totalBlocks:			UInt32;									{  total blocks used by this fork  }
		extents:				HFSPlusExtentRecord;					{  initial set of extents  }
	end;

	{	 Permissions info (HFS Plus only) - 16 bytes 	}
	HFSPlusPermissionsPtr = ^HFSPlusPermissions;
	HFSPlusPermissions = record
		ownerID:				UInt32;									{  user or group ID of file/folder owner  }
		groupID:				UInt32;									{  additional user of group ID  }
		permissions:			UInt32;									{  permissions (bytes: unused, owner, group, everyone)  }
		specialDevice:			UInt32;									{  UNIX: device for character or block special file  }
	end;

	{	 Catalog file data structures 	}

const
	kHFSRootParentID			= 1;							{  Parent ID of the root folder  }
	kHFSRootFolderID			= 2;							{  Folder ID of the root folder  }
	kHFSExtentsFileID			= 3;							{  File ID of the extents file  }
	kHFSCatalogFileID			= 4;							{  File ID of the catalog file  }
	kHFSBadBlockFileID			= 5;							{  File ID of the bad allocation block file  }
	kHFSAllocationFileID		= 6;							{  File ID of the allocation file (HFS Plus only)  }
	kHFSStartupFileID			= 7;							{  File ID of the startup file (HFS Plus only)  }
	kHFSAttributesFileID		= 8;							{  File ID of the attribute file (HFS Plus only)  }
	kHFSBogusExtentFileID		= 15;							{  Used for exchanging extents in extents file  }
	kHFSFirstUserCatalogNodeID	= 16;


	{	 HFS catalog key 	}

type
	HFSCatalogKeyPtr = ^HFSCatalogKey;
	HFSCatalogKey = record
		keyLength:				SInt8;									{  key length (in bytes)  }
		reserved:				SInt8;									{  reserved (set to zero)  }
		parentID:				HFSCatalogNodeID;						{  parent folder ID  }
		nodeName:				Str31;									{  catalog node name  }
	end;

	{	 HFS Plus catalog key 	}
	HFSPlusCatalogKeyPtr = ^HFSPlusCatalogKey;
	HFSPlusCatalogKey = record
		keyLength:				UInt16;									{  key length (in bytes)  }
		parentID:				HFSCatalogNodeID;						{  parent folder ID  }
		nodeName:				HFSUniStr255;							{  catalog node name  }
	end;


	{	 Catalog record types 	}

const
																{  HFS Catalog Records  }
	kHFSFolderRecord			= $0100;						{  Folder record  }
	kHFSFileRecord				= $0200;						{  File record  }
	kHFSFolderThreadRecord		= $0300;						{  Folder thread record  }
	kHFSFileThreadRecord		= $0400;						{  File thread record  }
																{  HFS Plus Catalog Records  }
	kHFSPlusFolderRecord		= 1;							{  Folder record  }
	kHFSPlusFileRecord			= 2;							{  File record  }
	kHFSPlusFolderThreadRecord	= 3;							{  Folder thread record  }
	kHFSPlusFileThreadRecord	= 4;							{  File thread record  }


	{	 Catalog file record flags 	}
	kHFSFileLockedBit			= $0000;						{  file is locked and cannot be written to  }
	kHFSFileLockedMask			= $0001;
	kHFSThreadExistsBit			= $0001;						{  a file thread record exists for this file  }
	kHFSThreadExistsMask		= $0002;


	{	 HFS catalog folder record - 70 bytes 	}

type
	HFSCatalogFolderPtr = ^HFSCatalogFolder;
	HFSCatalogFolder = record
		recordType:				SInt16;									{  record type  }
		flags:					UInt16;									{  folder flags  }
		valence:				UInt16;									{  folder valence  }
		folderID:				HFSCatalogNodeID;						{  folder ID  }
		createDate:				UInt32;									{  date and time of creation  }
		modifyDate:				UInt32;									{  date and time of last modification  }
		backupDate:				UInt32;									{  date and time of last backup  }
		userInfo:				DInfo;									{  Finder information  }
		finderInfo:				DXInfo;									{  additional Finder information  }
		reserved:				array [0..3] of UInt32;					{  reserved - set to zero  }
	end;

	{	 HFS Plus catalog folder record - 88 bytes 	}
	HFSPlusCatalogFolderPtr = ^HFSPlusCatalogFolder;
	HFSPlusCatalogFolder = record
		recordType:				SInt16;									{  record type = HFS Plus folder record  }
		flags:					UInt16;									{  file flags  }
		valence:				UInt32;									{  folder's valence (limited to 2^16 in Mac OS)  }
		folderID:				HFSCatalogNodeID;						{  folder ID  }
		createDate:				UInt32;									{  date and time of creation  }
		contentModDate:			UInt32;									{  date and time of last content modification  }
		attributeModDate:		UInt32;									{  date and time of last attribute modification  }
		accessDate:				UInt32;									{  date and time of last access (Rhapsody only)  }
		backupDate:				UInt32;									{  date and time of last backup  }
		permissions:			HFSPlusPermissions;						{  permissions (for Rhapsody)  }
		userInfo:				DInfo;									{  Finder information  }
		finderInfo:				DXInfo;									{  additional Finder information  }
		textEncoding:			UInt32;									{  hint for name conversions  }
		reserved:				UInt32;									{  reserved - set to zero  }
	end;

	{	 HFS catalog file record - 102 bytes 	}
	HFSCatalogFilePtr = ^HFSCatalogFile;
	HFSCatalogFile = record
		recordType:				SInt16;									{  record type  }
		flags:					SInt8;									{  file flags  }
		fileType:				SInt8;									{  file type (unused ?)  }
		userInfo:				FInfo;									{  Finder information  }
		fileID:					HFSCatalogNodeID;						{  file ID  }
		dataStartBlock:			UInt16;									{  not used - set to zero  }
		dataLogicalSize:		SInt32;									{  logical EOF of data fork  }
		dataPhysicalSize:		SInt32;									{  physical EOF of data fork  }
		rsrcStartBlock:			UInt16;									{  not used - set to zero  }
		rsrcLogicalSize:		SInt32;									{  logical EOF of resource fork  }
		rsrcPhysicalSize:		SInt32;									{  physical EOF of resource fork  }
		createDate:				UInt32;									{  date and time of creation  }
		modifyDate:				UInt32;									{  date and time of last modification  }
		backupDate:				UInt32;									{  date and time of last backup  }
		finderInfo:				FXInfo;									{  additional Finder information  }
		clumpSize:				UInt16;									{  file clump size (not used)  }
		dataExtents:			HFSExtentRecord;						{  first data fork extent record  }
		rsrcExtents:			HFSExtentRecord;						{  first resource fork extent record  }
		reserved:				UInt32;									{  reserved - set to zero  }
	end;

	{	 HFS Plus catalog file record - 248 bytes 	}
	HFSPlusCatalogFilePtr = ^HFSPlusCatalogFile;
	HFSPlusCatalogFile = record
		recordType:				SInt16;									{  record type = HFS Plus file record  }
		flags:					UInt16;									{  file flags  }
		reserved1:				UInt32;									{  reserved - set to zero  }
		fileID:					HFSCatalogNodeID;						{  file ID  }
		createDate:				UInt32;									{  date and time of creation  }
		contentModDate:			UInt32;									{  date and time of last content modification  }
		attributeModDate:		UInt32;									{  date and time of last attribute modification  }
		accessDate:				UInt32;									{  date and time of last access (Rhapsody only)  }
		backupDate:				UInt32;									{  date and time of last backup  }
		permissions:			HFSPlusPermissions;						{  permissions (for Rhapsody)  }
		userInfo:				FInfo;									{  Finder information  }
		finderInfo:				FXInfo;									{  additional Finder information  }
		textEncoding:			UInt32;									{  hint for name conversions  }
		reserved2:				UInt32;									{  reserved - set to zero  }
																		{  start on double long (64 bit) boundry  }
		dataFork:				HFSPlusForkData;						{  size and block data for data fork  }
		resourceFork:			HFSPlusForkData;						{  size and block data for resource fork  }
	end;

	{	 HFS catalog thread record - 46 bytes 	}
	HFSCatalogThreadPtr = ^HFSCatalogThread;
	HFSCatalogThread = record
		recordType:				SInt16;									{  record type  }
		reserved:				array [0..1] of SInt32;					{  reserved - set to zero  }
		parentID:				HFSCatalogNodeID;						{  parent ID for this catalog node  }
		nodeName:				Str31;									{  name of this catalog node  }
	end;

	{	 HFS Plus catalog thread record -- maximum 520 bytes 	}
	HFSPlusCatalogThreadPtr = ^HFSPlusCatalogThread;
	HFSPlusCatalogThread = record
		recordType:				SInt16;									{  record type  }
		reserved:				SInt16;									{  reserved - set to zero  }
		parentID:				HFSCatalogNodeID;						{  parent ID for this catalog node  }
		nodeName:				HFSUniStr255;							{  name of this catalog node (variable length)  }
	end;


	{
	    These are the types of records in the attribute B-tree.  The values were chosen
	    so that they wouldn't conflict with the catalog record types.
	}

const
	kHFSPlusAttrInlineData		= $10;							{  if size <  kAttrOverflowSize  }
	kHFSPlusAttrForkData		= $20;							{  if size >= kAttrOverflowSize  }
	kHFSPlusAttrExtents			= $30;							{  overflow extents for large attributes  }


	{
	    HFSPlusAttrInlineData
	    For small attributes, whose entire value is stored within this one
	    B-tree record.
	    There would not be any other records for this attribute.
	}

type
	HFSPlusAttrInlineDataPtr = ^HFSPlusAttrInlineData;
	HFSPlusAttrInlineData = record
		recordType:				UInt32;									{     = kHFSPlusAttrInlineData }
		reserved:				UInt32;
		logicalSize:			UInt32;									{     size in bytes of userData }
		userData:				packed array [0..1] of UInt8;			{     variable length; space allocated is a multiple of 2 bytes }
	end;

	{
	    HFSPlusAttrForkData
	    For larger attributes, whose value is stored in allocation blocks.
	    If the attribute has more than 8 extents, there will be additonal
	    records (of type HFSPlusAttrExtents) for this attribute.
	}
	HFSPlusAttrForkDataPtr = ^HFSPlusAttrForkData;
	HFSPlusAttrForkData = record
		recordType:				UInt32;									{     = kHFSPlusAttrForkData }
		reserved:				UInt32;
		theFork:				HFSPlusForkData;						{     size and first extents of value }
	end;

	{
	    HFSPlusAttrExtents
	    This record contains information about overflow extents for large,
	    fragmented attributes.
	}
	HFSPlusAttrExtentsPtr = ^HFSPlusAttrExtents;
	HFSPlusAttrExtents = record
		recordType:				UInt32;									{     = kHFSPlusAttrExtents }
		reserved:				UInt32;
		extents:				HFSPlusExtentRecord;					{     additional extents }
	end;

	{   A generic Attribute Record }
	HFSPlusAttrRecordPtr = ^HFSPlusAttrRecord;
	HFSPlusAttrRecord = record
		case SInt16 of
		0: (
			recordType:			UInt32;
			);
		1: (
			inlineData:			HFSPlusAttrInlineData;
			);
		2: (
			forkData:			HFSPlusAttrForkData;
			);
		3: (
			overflowExtents:	HFSPlusAttrExtents;
			);
	end;

	{	 Key and node lengths 	}

const
	kHFSPlusExtentKeyMaximumLength = 10;
	kHFSExtentKeyMaximumLength	= 7;
	kHFSPlusCatalogKeyMaximumLength = 516;
	kHFSPlusCatalogKeyMinimumLength = 6;
	kHFSCatalogKeyMaximumLength	= 37;
	kHFSCatalogKeyMinimumLength	= 6;
	kHFSPlusCatalogMinNodeSize	= 4096;
	kHFSPlusExtentMinNodeSize	= 512;
	kHFSPlusAttrMinNodeSize		= 4096;


	{	 HFS and HFS Plus volume attribute bits 	}
																{  Bits 0-6 are reserved (always cleared by MountVol call)  }
	kHFSVolumeHardwareLockBit	= 7;							{  volume is locked by hardware  }
	kHFSVolumeUnmountedBit		= 8;							{  volume was successfully unmounted  }
	kHFSVolumeSparedBlocksBit	= 9;							{  volume has bad blocks spared  }
	kHFSVolumeNoCacheRequiredBit = 10;							{  don't cache volume blocks (i.e. RAM or ROM disk)  }
	kHFSBootVolumeInconsistentBit = 11;							{  boot volume is inconsistent (System 7.6 and later)  }
																{  Bits 12-14 are reserved for future use  }
	kHFSVolumeSoftwareLockBit	= 15;							{  volume is locked by software  }
	kHFSVolumeHardwareLockMask	= $80;
	kHFSVolumeUnmountedMask		= $0100;
	kHFSVolumeSparedBlocksMask	= $0200;
	kHFSVolumeNoCacheRequiredMask = $0400;
	kHFSBootVolumeInconsistentMask = $0800;
	kHFSVolumeSoftwareLockMask	= $8000;
	kHFSMDBAttributesMask		= $8380;

	kHFSCatalogNodeIDsReusedBit	= 12;							{  nextCatalogID wrapped around  }
	kHFSCatalogNodeIDsReusedMask = $1000;

	{	 Master Directory Block (HFS only) - 162 bytes 	}
	{	 Stored at sector #2 (3rd sector) 	}

type
	HFSMasterDirectoryBlockPtr = ^HFSMasterDirectoryBlock;
	HFSMasterDirectoryBlock = record
																		{  These first fields are also used by MFS  }
		drSigWord:				UInt16;									{  volume signature  }
		drCrDate:				UInt32;									{  date and time of volume creation  }
		drLsMod:				UInt32;									{  date and time of last modification  }
		drAtrb:					UInt16;									{  volume attributes  }
		drNmFls:				UInt16;									{  number of files in root folder  }
		drVBMSt:				UInt16;									{  first block of volume bitmap  }
		drAllocPtr:				UInt16;									{  start of next allocation search  }
		drNmAlBlks:				UInt16;									{  number of allocation blocks in volume  }
		drAlBlkSiz:				UInt32;									{  size (in bytes) of allocation blocks  }
		drClpSiz:				UInt32;									{  default clump size  }
		drAlBlSt:				UInt16;									{  first allocation block in volume  }
		drNxtCNID:				UInt32;									{  next unused catalog node ID  }
		drFreeBks:				UInt16;									{  number of unused allocation blocks  }
		drVN:					Str27;									{  volume name  }
																		{  Master Directory Block extensions for HFS  }
		drVolBkUp:				UInt32;									{  date and time of last backup  }
		drVSeqNum:				UInt16;									{  volume backup sequence number  }
		drWrCnt:				UInt32;									{  volume write count  }
		drXTClpSiz:				UInt32;									{  clump size for extents overflow file  }
		drCTClpSiz:				UInt32;									{  clump size for catalog file  }
		drNmRtDirs:				UInt16;									{  number of directories in root folder  }
		drFilCnt:				UInt32;									{  number of files in volume  }
		drDirCnt:				UInt32;									{  number of directories in volume  }
		drFndrInfo:				array [0..7] of SInt32;					{  information used by the Finder  }
		drEmbedSigWord:			UInt16;									{  embedded volume signature (formerly drVCSize)  }
		drEmbedExtent:			HFSExtentDescriptor;					{  embedded volume location and size (formerly drVBMCSize and drCtlCSize)  }
		drXTFlSize:				UInt32;									{  size of extents overflow file  }
		drXTExtRec:				HFSExtentRecord;						{  extent record for extents overflow file  }
		drCTFlSize:				UInt32;									{  size of catalog file  }
		drCTExtRec:				HFSExtentRecord;						{  extent record for catalog file  }
	end;

	{	 HFSPlusVolumeHeader (HFS Plus only) - 512 bytes 	}
	{	 Stored at sector #2 (3rd sector) and second-to-last sector. 	}
	HFSPlusVolumeHeaderPtr = ^HFSPlusVolumeHeader;
	HFSPlusVolumeHeader = record
		signature:				UInt16;									{  volume signature == 'H+'  }
		version:				UInt16;									{  current version is kHFSPlusVersion  }
		attributes:				UInt32;									{  volume attributes  }
		lastMountedVersion:		UInt32;									{  implementation version which last mounted volume  }
		reserved:				UInt32;									{  reserved - set to zero  }
		createDate:				UInt32;									{  date and time of volume creation  }
		modifyDate:				UInt32;									{  date and time of last modification  }
		backupDate:				UInt32;									{  date and time of last backup  }
		checkedDate:			UInt32;									{  date and time of last disk check  }
		fileCount:				UInt32;									{  number of files in volume  }
		folderCount:			UInt32;									{  number of directories in volume  }
		blockSize:				UInt32;									{  size (in bytes) of allocation blocks  }
		totalBlocks:			UInt32;									{  number of allocation blocks in volume (includes this header and VBM }
		freeBlocks:				UInt32;									{  number of unused allocation blocks  }
		nextAllocation:			UInt32;									{  start of next allocation search  }
		rsrcClumpSize:			UInt32;									{  default resource fork clump size  }
		dataClumpSize:			UInt32;									{  default data fork clump size  }
		nextCatalogID:			HFSCatalogNodeID;						{  next unused catalog node ID  }
		writeCount:				UInt32;									{  volume write count  }
		encodingsBitmap:		UInt64;									{  which encodings have been use  on this volume  }
		finderInfo:				packed array [0..31] of UInt8;			{  information used by the Finder  }
		allocationFile:			HFSPlusForkData;						{  allocation bitmap file  }
		extentsFile:			HFSPlusForkData;						{  extents B-tree file  }
		catalogFile:			HFSPlusForkData;						{  catalog B-tree file  }
		attributesFile:			HFSPlusForkData;						{  extended attributes B-tree file  }
		startupFile:			HFSPlusForkData;						{  boot file  }
	end;

	{	 ---------- HFS and HFS Plus B-tree structures ---------- 	}
	{	 BTNodeDescriptor -- Every B-tree node starts with these fields. 	}
	BTNodeDescriptorPtr = ^BTNodeDescriptor;
	BTNodeDescriptor = record
		fLink:					UInt32;									{     next node at this level }
		bLink:					UInt32;									{     previous node at this level }
		kind:					SInt8;									{     kind of node (leaf, index, header, map) }
		height:					SInt8;									{     zero for header, map; child is one more than parent }
		numRecords:				UInt16;									{     number of records in this node }
		reserved:				UInt16;									{     reserved; set to zero }
	end;

	{	 Constants for BTNodeDescriptor kind 	}

const
	kBTLeafNode					= -1;
	kBTIndexNode				= 0;
	kBTHeaderNode				= 1;
	kBTMapNode					= 2;

	{	 BTHeaderRec -- The first record of a B-tree header node 	}

type
	BTHeaderRecPtr = ^BTHeaderRec;
	BTHeaderRec = record
		treeDepth:				UInt16;									{     maximum height (usually leaf nodes) }
		rootNode:				UInt32;									{     node number of root node }
		leafRecords:			UInt32;									{     number of leaf records in all leaf nodes }
		firstLeafNode:			UInt32;									{     node number of first leaf node }
		lastLeafNode:			UInt32;									{     node number of last leaf node }
		nodeSize:				UInt16;									{     size of a node, in bytes }
		maxKeyLength:			UInt16;									{     reserved }
		totalNodes:				UInt32;									{     total number of nodes in tree }
		freeNodes:				UInt32;									{     number of unused (free) nodes in tree }
		reserved1:				UInt16;									{     unused }
		clumpSize:				UInt32;									{     reserved }
		btreeType:				SInt8;									{     reserved }
		reserved2:				SInt8;									{     reserved }
		attributes:				UInt32;									{     persistent attributes about the tree }
		reserved3:				array [0..15] of UInt32;				{     reserved }
	end;

	{	 Constants for BTHeaderRec attributes 	}

const
	kBTBadCloseMask				= $00000001;					{     reserved }
	kBTBigKeysMask				= $00000002;					{     key length field is 16 bits }
	kBTVariableIndexKeysMask	= $00000004;					{     keys in index nodes are variable length }

{$ALIGN MAC68K}


end.
