{
     File:       CarbonCore/Files.h
 
     Contains:   File Manager Interfaces.
                 The contents of this header file are deprecated.
                 Use Foundation, CoreFoundation, DiskArbitration, POSIX/BSD, etc. API instead.
 
     Copyright:  © 1985-2011 Apple, Inc. All rights reserved
}
{     Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, June 2018 }

{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit Files;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses acl,MacTypes,MixedMode,OSUtils,TextCommon,UTCUtils,Finder,MacOSXPosix,DADisk,CFBase,CFDate,CFDictionary,CFRunLoop,CFUUID;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{ Finder constants were moved to Finder.Å }


{$ALIGN MAC68K}

{ HFSUniStr255 is the Unicode equivalent of Str255 }
type
	HFSUniStr255Ptr = ^HFSUniStr255;
	HFSUniStr255 = record
		length: UInt16;                 { number of unicode characters }
		unicode: array [0..254] of UniChar;           { unicode characters }
	end;
type
	ConstHFSUniStr255Param = ^HFSUniStr255;

type
	DirIDTypePtr = ^DirIDType;
	DirIDType = SInt32;

const kFSFileSecurityRemoveACL = acl_t(1);

{
    File Permissions

    Do not mix and match the following two sets of File Manager permission
    constants (original model  and AFP model).
    See the Retired Technote FL 37 "You Want Permission To Do What?!!"
    <https://developer.apple.com/legacy/mac/library/#technotes/fl/fl_37.html#//apple_ref/doc/uid/DTS10002463>
    for a detailed discussion of the two separate models
    and how they are related.
}
{    Permissions for File Manager routines which follow the original model.
    These values can be passed in the permissions parameter to FSOpenFork,
    PBOpenForkSync, and PBOpenForkAsync.
 }
const
	fsCurPerm = $00; { open access permissions in ioPermssn }
	fsRdPerm = $01;
	fsWrPerm = $02;
	fsRdWrPerm = $03;
	fsRdWrShPerm = $04;

{    Permissions for deprecated File Manager routines which follow the AFP model
    that is, routines with "OpenDeny" in the name. (These values do not work with
    FSOpenFork, PBOpenForkSync, and PBOpenForkAsync).
    
    The most useful combinations of these are:
        fsRdAccessPerm -> one writer, multiple readers: the readers
        fsRdAccessPerm + fsWrAccessPerm + fsWrDenyPerm -> one writer, multiple readers: the writer
        fsWrDenyPerm + fsRdAccessPerm -> multiple readers, no writers
        fsRdAccessPerm + fsWrAccessPerm -> shared read/write access
        fsRdAccessPerm + fsWrAccessPerm + fsRdDenyPerm + fsWrDenyPerm -> exclusive access
}
const
	fsRdAccessPerm = $01;
	fsWrAccessPerm = $02;
	fsRdDenyPerm = $10;
	fsWrDenyPerm = $20;

const
	fsRtParID = 1;
	fsRtDirID = 2;

const
	fsAtMark = 0;    { positioning modes in ioPosMode }
	fsFromStart = 1;
	fsFromLEOF = 2;
	fsFromMark = 3;

const
{ positionMode (ioPosMode) flags }
	kFSAllowConcurrentAsyncIOBit = 3;    { allow concurrent execution of async calls }
	kFSAllowConcurrentAsyncIOMask = $0008;
	kFSPleaseCacheBit = 4;    { please cache this request }
	kFSPleaseCacheMask = $0010;
	kFSNoCacheBit = 5;    { please don't cache this request }
	kFSNoCacheMask = $0020;
	kFSRdVerifyBit = 6;    { read verify mode }
	kFSRdVerifyMask = $0040;
	kFSForceReadBit = 6;
	kFSForceReadMask = $0040;
	kFSNewLineBit = 7;    { newline mode }
	kFSNewLineMask = $0080;
	kFSNewLineCharMask = $FF00; { newline character }


const
{ CatSearch Search bitmask Constants }
	fsSBPartialName = $01;
	fsSBFullName = $02;
	fsSBFlAttrib = $04;
	fsSBFlFndrInfo = $08;
	fsSBFlLgLen = $20;
	fsSBFlPyLen = $40;
	fsSBFlRLgLen = $80;
	fsSBFlRPyLen = $0100;
	fsSBFlCrDat = $0200;
	fsSBFlMdDat = $0400;
	fsSBFlBkDat = $0800;
	fsSBFlXFndrInfo = $1000;
	fsSBFlParID = $2000;
	fsSBNegate = $4000;
	fsSBDrUsrWds = $08;
	fsSBDrNmFls = $10;
	fsSBDrCrDat = $0200;
	fsSBDrMdDat = $0400;
	fsSBDrBkDat = $0800;
	fsSBDrFndrInfo = $1000;
	fsSBDrParID = $2000;
	fsSBNodeID = $8000;
	fsSBAttributeModDate = $00010000;
	fsSBAccessDate = $00020000;
	fsSBPermissions = $00040000;
	fsSBSkipPackageContents = $00080000;
	fsSBSkipHiddenItems = $00100000;
	fsSBUserID = $00200000;
	fsSBGroupID = $00400000;

const
{ CatSearch Search bit value Constants }
	fsSBPartialNameBit = 0;    {ioFileName points to a substring}
	fsSBFullNameBit = 1;    {ioFileName points to a match string}
	fsSBFlAttribBit = 2;    {search includes file attributes}
	fsSBFlFndrInfoBit = 3;    {search includes finder info}
	fsSBFlLgLenBit = 5;    {search includes data logical length}
	fsSBFlPyLenBit = 6;    {search includes data physical length}
	fsSBFlRLgLenBit = 7;    {search includes resource logical length}
	fsSBFlRPyLenBit = 8;    {search includes resource physical length}
	fsSBFlCrDatBit = 9;    {search includes create date}
	fsSBFlMdDatBit = 10;   {search includes modification date}
	fsSBFlBkDatBit = 11;   {search includes backup date}
	fsSBFlXFndrInfoBit = 12;   {search includes extended finder info}
	fsSBFlParIDBit = 13;   {search includes file's parent ID}
	fsSBNegateBit = 14;   {return all non-matches}
	fsSBDrUsrWdsBit = 3;    {search includes directory finder info}
	fsSBDrNmFlsBit = 4;    {search includes directory valence}
	fsSBDrCrDatBit = 9;    {directory-named version of fsSBFlCrDatBit}
	fsSBDrMdDatBit = 10;   {directory-named version of fsSBFlMdDatBit}
	fsSBDrBkDatBit = 11;   {directory-named version of fsSBFlBkDatBit}
	fsSBDrFndrInfoBit = 12;   {directory-named version of fsSBFlXFndrInfoBit}
	fsSBDrParIDBit = 13;   {directory-named version of fsSBFlParIDBit}
	fsSBNodeIDBit = 15;   { search by range of nodeID }
	fsSBAttributeModDateBit = 16;   { search by range of attributeModDate }
	fsSBAccessDateBit = 17;   { search by range of accessDate [CatalogSearch only] }
	fsSBPermissionsBit = 18;   { search by value/mask of permissions [CatalogSearch only] }
	fsSBSkipPackageContentsBit = 19;   {do not return items inside of packages}
	fsSBSkipHiddenItemsBit = 20;   {do not return items with an invisible element in their path}
	fsSBUserIDBit = 21;   { search by userID in permissions field [CatalogSearch only] }
	fsSBGroupIDBit = 22;    { search by groupID in permissions field [CatalogSearch only] }

const
{ vMAttrib (GetVolParms) bit position constants }
	bLimitFCBs = 31;
	bLocalWList = 30;
	bNoMiniFndr = 29;
	bNoVNEdit = 28;
	bNoLclSync = 27;
	bTrshOffLine = 26;
	bNoSwitchTo = 25;
	bNoDeskItems = 20;
	bNoBootBlks = 19;
	bAccessCntl = 18;
	bNoSysDir = 17;
	bHasExtFSVol = 16;
	bHasOpenDeny = 15;
	bHasCopyFile = 14;
	bHasMoveRename = 13;
	bHasDesktopMgr = 12;
	bHasShortName = 11;
	bHasFolderLock = 10;
	bHasPersonalAccessPrivileges = 9;
	bHasUserGroupList = 8;
	bHasCatSearch = 7;
	bHasFileIDs = 6;
	bHasBTreeMgr = 5;
	bHasBlankAccessPrivileges = 4;
	bSupportsAsyncRequests = 3;    { asynchronous requests to this volume are handled correctly at any time}
	bSupportsTrashVolumeCache = 2;

const
{ vMAttrib (GetVolParms) bit position constants }
	bHasDirectIO = 1;

const
{ vMExtendedAttributes (GetVolParms) bit position constants }
	bIsEjectable = 0;    { volume is in an ejectable disk drive }
	bSupportsHFSPlusAPIs = 1;    { volume supports HFS Plus APIs directly (not through compatibility layer) }
	bSupportsFSCatalogSearch = 2;    { volume supports FSCatalogSearch }
	bSupportsFSExchangeObjects = 3;    { volume supports FSExchangeObjects }
	bSupports2TBFiles = 4;    { volume supports supports 2 terabyte files }
	bSupportsLongNames = 5;    { volume supports file/directory/volume names longer than 31 characters }
	bSupportsMultiScriptNames = 6;    { volume supports file/directory/volume names with characters from multiple script systems }
	bSupportsNamedForks = 7;    { volume supports forks beyond the data and resource forks }
	bSupportsSubtreeIterators = 8;    { volume supports recursive iterators not at the volume root }
	bL2PCanMapFileBlocks = 9;    { volume supports Lg2Phys SPI correctly }
	bParentModDateChanges = 10;   { Changing a file or folder causes its parent's mod date to change }
	bAncestorModDateChanges = 11;   { Changing a file or folder causes all ancestor mod dates to change }
	bSupportsSymbolicLinks = 13;   { volume supports the creation and use of symbolic links (Mac OS X only) }
	bIsAutoMounted = 14;   { volume was mounted automatically (Mac OS X only) }
	bAllowCDiDataHandler = 17;   { allow QuickTime's CDi data handler to examine this volume }
	bSupportsExclusiveLocks = 18;   { volume supports exclusive opens for writing }
	bSupportsJournaling = 19;   { volume supports journal (journal may not be active) }
	bNoVolumeSizes = 20;   { volume is unable to report volume size or free space }
	bIsOnInternalBus = 21;   { device is on an internal bus - see note below }
	bIsCaseSensitive = 22;   { volume is case sensitive }
	bIsCasePreserving = 23;   { volume is case preserving }
	bDoNotDisplay = 24;   { volume should not be displayed in UI }
	bIsRemovable = 25;   { device is removable according to IOKit }
	bNoRootTimes = 26;   { volume does not set reliable times for its root directory }
	bIsOnExternalBus = 27;   { device is on an external bus -- see note below }
	bSupportsExtendedFileSecurity = 28;    { volume supports FSFileSecurity objects }

{    Note: A volume can return one of four states via the bIsInternal and bIsExternal bits.  A volume known
        to be on an internal bus will set bIsInternal and clear bIsExternal.  A volume known to
        be on an external bus will clear bIsInternal and set bIsExternal.  A volume on a bus that
        is indeterminate (could be either) will set both bits.  A volume not on a local bus will, such
        as a network volume, will leave both bits clear. }
const
{ Large Volume Constants }
	kWidePosOffsetBit = 8;
	kUseWidePositioning = 1 shl kWidePosOffsetBit;
	kMaximumBlocksIn4GB = $007FFFFF;

const
{ Foreign Privilege Model Identifiers }
	fsUnixPriv = 1;

const
{ Authentication Constants }
	kNoUserAuthentication = 1;
	kPassword = 2;
	kEncryptPassword = 3;
	kTwoWayEncryptPassword = 6;


{ values of user IDs and group IDs }
const
	knoUser = 0;
	kadministratorUser = 1;

const
	knoGroup = 0;


type
	FSVolumeRefNum = SInt16;
	FSVolumeRefNumPtr = ^FSVolumeRefNum; { when a VAR xx: FSVolumeRefNum parameter can be nil, it is changed to xx: FSVolumeRefNumPtr }
{$ifc TARGET_CPU_64}
type
	FSIORefNum = SInt32;
{$elsec}
type
	FSIORefNum = SInt16;
{$endc}

const
	kFSInvalidVolumeRefNum = 0;

type
	FSRef = record
		hidden: packed array [0..79] of UInt8;             { private to File Manager; ¥¥ need symbolic constant }
	end;
	FSRefPtr = ^FSRef;
type
	FSFileSecurityRef = ^__FSFileSecurity; { an opaque type }
	__FSFileSecurity = record end;
{ Catalog position record }
type
	CatPositionRecPtr = ^CatPositionRec;
	CatPositionRec = record
		initialize: SInt32;
		priv: array [1..6] of SInt16;
	end;

{$ifc TARGET_CPU_64}
type
	FSSpec = record
    { FSSpecs are invalid for 64 bit, but defined in case they appear in shared structs}
    hidden: array [1..70] of UInt8;
	end;
{$elsec}
type
	FSSpec = record
		vRefNum: FSVolumeRefNum;
		parID: SInt32;
		name: StrFileName;                   { a Str63 on MacOS}
	end;
{$endc}
type
	FSSpecPtr = ^FSSpec;
type
	FSSpecHandle = ^FSSpecPtr;
{ pointer to array of FSSpecs }
type
	FSSpecArray = array [0..0] of FSSpec;
	FSSpecArrayPtr = ^FSSpecArray;
{ 
    The only difference between "const FSSpec*" and "ConstFSSpecPtr" is 
    that as a parameter, ConstFSSpecPtr is allowed to be NULL 
}
type
	ConstFSSpecPtr = {const} FSSpecPtr;

type
{$ifc TARGET_CPU_64}
	ParmBlkPtr = UnivPtr;
{$elsec}
	ParmBlkPtr = ^ParamBlockRec;
{$endc}

	IOCompletionProcPtr = procedure( paramBlock: ParmBlkPtr );
	IOCompletionUPP = IOCompletionProcPtr;

{$ifc not TARGET_CPU_64}
	IOParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioRefNum: FSIORefNum;               {refNum for I/O operation}
		ioVersNum: SInt8;              {version number}
		ioPermssn: SInt8;              {Open: permissions (byte)}
		ioMisc: Ptr;                 {Rename: new name (GetEOF,SetEOF: logical end of file) (Open: optional ptr to buffer) (SetFileType: new type)}
		ioBuffer: Ptr;               {data buffer Ptr}
		ioReqCount: SInt32;             {requested byte count; also = ioNewDirID}
		ioActCount: SInt32;             {actual byte count completed}
		ioPosMode: SInt16;              {initial file positioning}
		ioPosOffset: SInt32;            {file position offset}
	end;
	IOParamPtr = ^IOParam;

	FileParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioFRefNum: FSIORefNum;              {reference number}
		ioFVersNum: SInt8;             {version number}
		filler1: SInt8;
		ioFDirIndex: SInt16;            {GetFInfo directory index}
		ioFlAttrib: SInt8;             {GetFInfo: in-use bit=7, lock bit=0}
		ioFlVersNum: SInt8;            {file version number}
		ioFlFndrInfo: FInfo;           {user info}
		ioFlNum: UInt32;                {GetFInfo: file number; TF- ioDirID}
		ioFlStBlk: UInt16;              {start file block (0 if none)}
		ioFlLgLen: SInt32;              {logical length (EOF)}
		ioFlPyLen: SInt32;              {physical length}
		ioFlRStBlk: UInt16;             {start block rsrc fork}
		ioFlRLgLen: SInt32;             {file logical length rsrc fork}
		ioFlRPyLen: SInt32;             {file physical length rsrc fork}
		ioFlCrDat: UInt32;              {file creation date& time (32 bits in secs)}
		ioFlMdDat: UInt32;              {last modified date and time}
	end;
	FileParamPtr = ^FileParam;

	VolumeParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		filler2: UInt32;
		ioVolIndex: SInt16;             {volume index number}
		ioVCrDate: UInt32;              {creation date and time}
		ioVLsBkUp: UInt32;              {last backup date and time}
		ioVAtrb: UInt16;                {volume attrib}
		ioVNmFls: UInt16;               {number of files in directory}
		ioVDirSt: UInt16;               {start block of file directory}
		ioVBlLn: SInt16;                {GetVolInfo: length of dir in blocks}
		ioVNmAlBlks: UInt16;            {for compatibilty ioVNmAlBlks * ioVAlBlkSiz <= 2 GB}
		ioVAlBlkSiz: UInt32;            {for compatibilty ioVAlBlkSiz is <= $0000FE00 (65,024)}
		ioVClpSiz: UInt32;              {GetVolInfo: bytes to allocate at a time}
		ioAlBlSt: UInt16;               {starting disk(512-byte) block in block map}
		ioVNxtFNum: UInt32;             {GetVolInfo: next free file number}
		ioVFrBlk: UInt16;               {GetVolInfo: # free alloc blks for this vol}
	end;
	VolumeParamPtr = ^VolumeParam;

	CntrlParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioCRefNum: FSIORefNum;              {refNum for I/O operation}
		csCode: SInt16;                 {word for control status code}
		csParam: array [0..10] of SInt16;  { operation-defined parameters }
	end;
	CntrlParamPtr = ^CntrlParam;

	SlotDevParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioSRefNum: FSIORefNum;
		ioSVersNum: SInt8;
		ioSPermssn: SInt8;
		ioSMix: Ptr;
		ioSFlags: SInt16;
		ioSlot: SInt8;
		ioID: SInt8;
	end;
	SlotDevParamPtr = ^SlotDevParam;

	MultiDevParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioMRefNum: FSIORefNum;
		ioMVersNum: SInt8;
		ioMPermssn: SInt8;
		ioMMix: Ptr;
		ioMFlags: SInt16;
		ioSEBlkPtr: Ptr;
	end;
	MultiDevParamPtr = ^MultiDevParam;

	ParamBlockRecPtr = ^ParamBlockRec;
	ParamBlockRec = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		case SInt16 of
		0: (
			ioRefNum:			FSIORefNum;								{ refNum for I/O operation }
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
			ioFRefNum:			FSIORefNum;								{ reference number }
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
		ioResult:				{volatile} OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				FSVolumeRefNum;								{ volume refnum (DrvNum for Eject and MountVol) }
		ioFRefNum:				FSIORefNum;
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
			ioFlParID:			UInt32;
			ioFlClpSiz:			SInt32;
		   );
		1: (
			ioDrUsrWds:			DInfo;
			ioDrDirID:			SInt32;
			ioDrNmFls:			UInt16;
			filler3:			array [1..9] of SInt16;
			ioDrCrDat:			UInt32;
			ioDrMdDat:			UInt32;
			ioDrBkDat:			UInt32;
			ioDrFndrInfo:		DXInfo;
			ioDrParID:			SInt32;
		   );
	end;


type
	CInfoPBPtr = CInfoPBRecPtr;
	XCInfoPBRecPtr = ^XCInfoPBRec;
	XCInfoPBRec = record
		qLink: QElemPtr;
		qType: SInt16;
		ioTrap: SInt16;
		ioCmdAddr: Ptr;
		ioCompletion: ProcPtr;           { --> A pointer to a completion routine }
		ioResult: {volatile} OSErr;               { --> The result code of the function }
		ioNamePtr: StringPtr;              { --> Pointer to pathname to object }
		ioVRefNum: FSVolumeRefNum;              { --> A volume specification }
		filler1: SInt32;
		ioShortNamePtr: StringPtr;         { <-> A pointer to the short name string buffer - required! }
		filler2: SInt16;
		ioPDType: SInt16;               { <-- The ProDOS file type }
		ioPDAuxType: SInt32;            { <-- The ProDOS aux type }
		filler3:				array [0..1] of SInt32;
		ioDirID: SInt32;                { --> A directory ID }
	end;
type
	XCInfoPBPtr = XCInfoPBRecPtr;
	{	 
	    The following are structures to be filled out with the _PBGetVolMountInfo call
	    and passed back into the _PBVolumeMount call for external file system mounts. 
		}
	DTPBRecPtr = ^DTPBRec;
	DTPBRec = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioDTRefNum: FSIORefNum;             { desktop refnum }
		ioIndex: SInt16;
		ioTagInfo: SInt32;
		ioDTBuffer: Ptr;
		ioDTReqCount: SInt32;
		ioDTActCount: SInt32;
		ioFiller1: SInt8;
		ioIconType: UInt8;
		ioFiller2: SInt16;
		ioDirID: SInt32;
		ioFileCreator: OSType;
		ioFileType: OSType;
		ioFiller3: SInt32;
		ioDTLgLen: SInt32;
		ioDTPyLen: SInt32;
		ioFiller4: array [1..14] of SInt16;
		ioAPPLParID: SInt32;
	end;
type
	DTPBPtr = DTPBRecPtr;

type
	HIOParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioRefNum: FSIORefNum;
		ioVersNum: SInt8;
		ioPermssn: SInt8;
		ioMisc: Ptr;
		ioBuffer: Ptr;
		ioReqCount: SInt32;
		ioActCount: SInt32;
		ioPosMode: SInt16;
		ioPosOffset: SInt32;
	end;
	HIOParamPtr = ^HIOParam;
type
	HFileParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioFRefNum: FSIORefNum;
		ioFVersNum: SInt8;
		filler1: SInt8;
		ioFDirIndex: SInt16;
		ioFlAttrib: SInt8;
		ioFlVersNum: SInt8;
		ioFlFndrInfo: FInfo;
		ioDirID: SInt32;
		ioFlStBlk: UInt16;
		ioFlLgLen: SInt32;
		ioFlPyLen: SInt32;
		ioFlRStBlk: UInt16;
		ioFlRLgLen: SInt32;
		ioFlRPyLen: SInt32;
		ioFlCrDat: UInt32;
		ioFlMdDat: UInt32;
	end;
	HFileParamPtr = ^HFileParam;
type
	HVolumeParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		filler2: SInt32;
		ioVolIndex: SInt16;
		ioVCrDate: UInt32;
		ioVLsMod: UInt32;
		ioVAtrb: SInt16;
		ioVNmFls: UInt16;
		ioVBitMap: UInt16;
		ioAllocPtr: UInt16;
		ioVNmAlBlks: UInt16;
		ioVAlBlkSiz: UInt32;
		ioVClpSiz: UInt32;
		ioAlBlSt: UInt16;
		ioVNxtCNID: UInt32;
		ioVFrBlk: UInt16;
		ioVSigWord: UInt16;
		ioVDrvInfo: SInt16;
		ioVDRefNum: FSIORefNum;
		ioVFSID: SInt16;
		ioVBkUp: UInt32;
		ioVSeqNum: SInt16;
		ioVWrCnt: UInt32;
		ioVFilCnt: UInt32;
		ioVDirCnt: UInt32;
		ioVFndrInfo:			array [1..8] of SInt32;
	end;
	HVolumeParamPtr = ^HVolumeParam;
type
	XIOParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioRefNum: FSIORefNum;
		ioVersNum: SInt8;
		ioPermssn: SInt8;
		ioMisc: Ptr;
		ioBuffer: Ptr;
		ioReqCount: SInt32;
		ioActCount: SInt32;
		ioPosMode: SInt16;              { must have kUseWidePositioning bit set }
		ioWPosOffset: wide;           { wide positioning offset }
	end;
	XIOParamPtr = ^XIOParam;
type
	XVolumeParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioXVersion: UInt32;             { this XVolumeParam version (0) }
		ioVolIndex: SInt16;
		ioVCrDate: UInt32;
		ioVLsMod: UInt32;
		ioVAtrb: SInt16;
		ioVNmFls: UInt16;
		ioVBitMap: UInt16;
		ioAllocPtr: UInt16;
		ioVNmAlBlks: UInt16;
		ioVAlBlkSiz: UInt32;
		ioVClpSiz: UInt32;
		ioAlBlSt: UInt16;
		ioVNxtCNID: UInt32;
		ioVFrBlk: UInt16;
		ioVSigWord: UInt16;
		ioVDrvInfo: SInt16;
		ioVDRefNum: SInt16;
		ioVFSID: SInt16;
		ioVBkUp: UInt32;
		ioVSeqNum: SInt16;
		ioVWrCnt: UInt32;
		ioVFilCnt: UInt32;
		ioVDirCnt: UInt32;
		ioVFndrInfo:			array [1..8] of SInt32;
		ioVTotalBytes: UInt64;          { total number of bytes on volume }
		ioVFreeBytes: UInt64;           { number of free bytes on volume }
	end;
	XVolumeParamPtr = ^XVolumeParam;
type
	AccessParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioRefNum: FSIORefNum;               { <- , ioRefNum ( use to be filler3 ) }
		ioDenyModes: SInt16;            {access rights data}
		filler4: SInt16;
		filler5: SInt8;
		ioACUser: SInt8;               {access rights for directory only}
		filler6: SInt32;
		ioACOwnerID: SInt32;            {owner ID}
		ioACGroupID: SInt32;            {group ID}
		ioACAccess: SInt32;             {access rights}
		ioDirID: SInt32;
	end;
	AccessParamPtr = ^AccessParam;
type
	ObjParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		filler7: SInt16;
		ioObjType: SInt16;              {function code}
		ioObjNamePtr: StringPtr;           {ptr to returned creator/group name}
		ioObjID: SInt32;                {creator/group ID}
	end;
	ObjParamPtr = ^ObjParam;
type
	CopyParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioDstVRefNum: FSVolumeRefNum;           {destination vol identifier}
		filler8: SInt16;
		ioNewName: StringPtr;              {ptr to destination pathname}
		ioCopyName: StringPtr;             {ptr to optional name}
		ioNewDirID: SInt32;             {destination directory ID}
		filler14: SInt32;
		filler15: SInt32;
		ioDirID: SInt32;
	end;
	CopyParamPtr = ^CopyParam;
type
	WDParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioWDCreated: SInt16;
		ioWDIndex: SInt16;
		ioWDProcID: SInt32;
		ioWDVRefNum: FSVolumeRefNum;
		filler10: SInt16;
		filler11: SInt32;
		filler12: SInt32;
		filler13: SInt32;
		ioWDDirID: SInt32;
	end;
	WDParamPtr = ^WDParam;
type
	FIDParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		filler14: SInt32;
		ioDestNamePtr: StringPtr;          { dest file name }
		filler15: SInt32;
		ioDestDirID: SInt32;            { dest file's directory id }
		filler16: SInt32;
		filler17: SInt32;
		ioSrcDirID: SInt32;             { source file's directory id }
		filler18: SInt16;
		ioFileID: SInt32;               { file ID }
	end;
	FIDParamPtr = ^FIDParam;
type
	ForeignPrivParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioFiller21: SInt32;
		ioFiller22: SInt32;
		ioForeignPrivBuffer: Ptr;
		ioForeignPrivActCount: SInt32;
		ioForeignPrivReqCount: SInt32;
		ioFiller23: SInt32;
		ioForeignPrivDirID: SInt32;
		ioForeignPrivInfo1: SInt32;
		ioForeignPrivInfo2: SInt32;
		ioForeignPrivInfo3: SInt32;
		ioForeignPrivInfo4: SInt32;
	end;
	ForeignPrivParamPtr = ^ForeignPrivParam;
type
	CSParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioMatchPtr: FSSpecPtr;             { match array }
		ioReqMatchCount: SInt32;        { maximum allowable matches }
		ioActMatchCount: SInt32;        { actual match count }
		ioSearchBits: SInt32;           { search criteria selector }
		ioSearchInfo1: CInfoPBPtr;          { search values and range lower bounds }
		ioSearchInfo2: CInfoPBPtr;          { search values and range upper bounds }
		ioSearchTime: SInt32;           { length of time to run search }
		ioCatPosition: CatPositionRec;          { current position in the catalog }
		ioOptBuffer: Ptr;            { optional performance enhancement buffer }
		ioOptBufSize: SInt32;           { size of buffer pointed to by ioOptBuffer }
	end;
	CSParamPtr = ^CSParam;

type
	HParamBlockRecPtr = ^HParamBlockRec;
	HParamBlockRec = record
		qLink:					QElemPtr;								{ queue link in header }
		qType:					SInt16;								{ type byte for safety check }
		ioTrap:					SInt16;								{ FS: the Trap }
		ioCmdAddr:				Ptr;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				FSVolumeRefNum;								{ volume refnum (DrvNum for Eject and MountVol) }
		case SInt16 of
		0: (
			ioRefNum:			FSIORefNum;
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
			ioFRefNum:			FSIORefNum;
			ioFVersNum:			SInt8;
			filler1:			SInt8;
			ioFDirIndex:		SInt16;
			ioFlAttrib:			SInt8;
			ioFlVersNum:		SInt8;
			ioFlFndrInfo:		FInfo;
			ioDirID:			SInt32;
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
			ioVDRefNum:			FSIORefNum;
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
{			  ioDirID:			SInt32; -- since this struct is only defined for 32 bit targets, the ioDirID field from case 1 will be at the same address }
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
			ioNewDirID:			SInt32;								{ destination directory ID }
			{ filler14:				SInt32;
			  filler15:				SInt32;
			  ioDirID:			SInt32; -- since this struct is only defined for 32 bit targets, the ioDirID field from case 1 will be at the same address }
		   );
		6: (
			ioWDCreated:		SInt16;
			ioWDIndex:			SInt16;
			ioWDProcID:			SInt32;
			ioWDVRefNum:		FSVolumeRefNum;
			filler10:			SInt16;
			filler11:			SInt32;
			filler12:			SInt32;
			filler13:			SInt32;
			ioWDDirID:			SInt32;
		   );
		7: (
			filler14:			SInt32;
			ioDestNamePtr:		StringPtr;								{  dest file name  }
			filler15:			SInt32;
			ioDestDirID:		SInt32;							{  dest file's directory id  }
			filler16:			SInt32;
			filler17:			SInt32;
			ioSrcDirID:			SInt32;							{  source file's directory id  }
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
			ioForeignPrivDirID:	SInt32;
			ioForeignPrivInfo1:	SInt32;
			ioForeignPrivInfo2:	SInt32;
			ioForeignPrivInfo3:	SInt32;
			ioForeignPrivInfo4:	SInt32;
		   );
	end;

	HParmBlkPtr							= ^HParamBlockRec;

type
	CMovePBRecPtr = ^CMovePBRec;
	CMovePBRec = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		filler1: SInt32;
		ioNewName: StringPtr;
		filler2: SInt32;
		ioNewDirID: SInt32;
		filler3: array [1..2] of SInt32;
		ioDirID: SInt32;
	end;

	CMovePBPtr = CMovePBRecPtr;
	WDPBRecPtr = ^WDPBRec;
	WDPBRec = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		filler1: SInt16;
		ioWDIndex: SInt16;
		ioWDProcID: SInt32;
		ioWDVRefNum: FSVolumeRefNum;
		filler2: array [1..7] of SInt16;
		ioWDDirID: SInt32;
	end;
type
	WDPBPtr = WDPBRecPtr;
	FCBPBRecPtr = ^FCBPBRec;
	FCBPBRec = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}
		ioRefNum: FSIORefNum;
		filler: SInt16;
		ioFCBIndx: SInt16;
		filler1: SInt16;
		ioFCBFlNm: SInt32;
		ioFCBFlags: SInt16;
		ioFCBStBlk: UInt16;
		ioFCBEOF: SInt32;
		ioFCBPLen: SInt32;
		ioFCBCrPs: SInt32;
		ioFCBVRefNum: FSVolumeRefNum;
		ioFCBClpSiz: SInt32;
		ioFCBParID: SInt32;
	end;
type
	FCBPBPtr = FCBPBRecPtr;
	VCB = record
		qLink: QElemPtr;
		qType: SInt16;
		vcbFlags: SInt16;
		vcbSigWord: UInt16;
		vcbCrDate: UInt32;
		vcbLsMod: UInt32;
		vcbAtrb: SInt16;
		vcbNmFls: UInt16;
		vcbVBMSt: SInt16;
		vcbAllocPtr: SInt16;
		vcbNmAlBlks: UInt16;
		vcbAlBlkSiz: SInt32;
		vcbClpSiz: SInt32;
		vcbAlBlSt: SInt16;
		vcbNxtCNID: SInt32;
		vcbFreeBks: UInt16;
		vcbVN: Str27;
		vcbDrvNum: SInt16;
		vcbDRefNum: FSIORefNum;
		vcbFSID: SInt16;
		vcbVRefNum: FSVolumeRefNum;
		vcbMAdr: Ptr;
		vcbBufAdr: Ptr;
		vcbMLen: SInt16;
		vcbDirIndex: SInt16;
		vcbDirBlk: SInt16;
		vcbVolBkUp: UInt32;
		vcbVSeqNum: UInt16;
		vcbWrCnt: SInt32;
		vcbXTClpSiz: SInt32;
		vcbCTClpSiz: SInt32;
		vcbNmRtDirs: UInt16;
		vcbFilCnt: SInt32;
		vcbDirCnt: SInt32;
		vcbFndrInfo: array [1..8] of SInt32;
		vcbVCSize: UInt16;
		vcbVBMCSiz: UInt16;
		vcbCtlCSiz: UInt16;
		vcbXTAlBlks: UInt16;
		vcbCTAlBlks: UInt16;
		vcbXTRef: SInt16;
		vcbCTRef: SInt16;
		vcbCtlBuf: Ptr;
		vcbDirIDM: SInt32;
		vcbOffsM: SInt16;
	end;
	VCBPtr = ^VCB;
type
	DrvQEl = record
		qLink: QElemPtr;
		qType: SInt16;
		dQDrive: SInt16;
		dQRefNum: SInt16;
		dQFSID: SInt16;
		dQDrvSz: UInt16;
		dQDrvSz2: UInt16;
	end;
	DrvQElPtr = ^DrvQEl;
{$endc} {not TARGET_CPU_64}


{
 *  FSPermissionInfo
 *  
 *  Discussion:
 *    This structure is used when kFSCatInfoPermissions is passed to
 *    the HFSPlus API. On return from GetCatalogInfo and
 *    GetCatalogInfoBulk, the userID, groupID, and mode fields are
 *    returned.  When passed to SetCatalogInfo, only the mode field is
 *    set.  See chmod(2) for details about the mode field. This is
 *    supported on Mac OS X only. NOTE: An FSFileSecurityRef retrieved
 *    via FSGetCatalogInfo is a copy and must be released using
 *    CFRelease() when no longer needed.
 }
type
	FSPermissionInfoPtr = ^FSPermissionInfo;
	FSPermissionInfo = record
		userID: UInt32;
		groupID: UInt32;
		reserved1: UInt8;
		userAccess: UInt8;
		mode: UInt16;
		fileSec: FSFileSecurityRef;
	end;
{  CatalogInfoBitmap describes which fields of the CatalogInfo you wish to get or set.}

type
	FSCatalogInfoBitmap = UInt32;
const
	kFSCatInfoNone = $00000000;
	kFSCatInfoTextEncoding = $00000001;
	kFSCatInfoNodeFlags = $00000002; { Locked (bit 0) and directory (bit 4) only }
	kFSCatInfoVolume = $00000004;
	kFSCatInfoParentDirID = $00000008;
	kFSCatInfoNodeID = $00000010;
	kFSCatInfoCreateDate = $00000020;
	kFSCatInfoContentMod = $00000040;
	kFSCatInfoAttrMod = $00000080;
	kFSCatInfoAccessDate = $00000100; { Note: although included in kFSCatInfoSettableInfo, attempts to set kFSCatInfoAccessDate will do nothing but will not cause an error }
	kFSCatInfoBackupDate = $00000200;
	kFSCatInfoPermissions = $00000400;
	kFSCatInfoFinderInfo = $00000800;
	kFSCatInfoFinderXInfo = $00001000;
	kFSCatInfoValence = $00002000; { Folders only, zero for files }
	kFSCatInfoDataSizes = $00004000; { Data fork logical and physical size }
	kFSCatInfoRsrcSizes = $00008000; { Resource fork logical and physical size }
	kFSCatInfoSharingFlags = $00010000; { sharingFlags: kioFlAttribMountedBit, kioFlAttribSharePointBit }
	kFSCatInfoUserPrivs = $00020000; { userPrivileges }
	kFSCatInfoUserAccess = $00080000; { (OS X only) }
	kFSCatInfoSetOwnership = $00100000; { (OS X only) }
	kFSCatInfoFSFileSecurityRef = $00400000; { FSFileSecurity Object, will show up in the permissions field independent of kFSCatInfoPermissions.  This is also not part of settable, gettable since it requires being released }
	kFSCatInfoAllDates = $000003E0;
	kFSCatInfoGettableInfo = $0003FFFF;
	kFSCatInfoSettableInfo = $00001FE3; { flags, dates, permissions, Finder info, text encoding }
	kFSCatInfoReserved = -262144 ; { SInt32($FFFC0000) -- bits that are currently reserved }

{  Constants for nodeFlags field of FSCatalogInfo }
const
	kFSNodeLockedBit = 0;    { Set if file or directory is locked }
	kFSNodeLockedMask = $0001;
	kFSNodeResOpenBit = 2;    { Set if the resource fork is open }
	kFSNodeResOpenMask = $0004;
	kFSNodeDataOpenBit = 3;    { Set if the data fork is open }
	kFSNodeDataOpenMask = $0008;
	kFSNodeIsDirectoryBit = 4;    { Set if the object is a directory }
	kFSNodeIsDirectoryMask = $0010;
	kFSNodeCopyProtectBit = 6;
	kFSNodeCopyProtectMask = $0040;
	kFSNodeForkOpenBit = 7;    { Set if the file or directory has any open fork }
	kFSNodeForkOpenMask = $0080;
	kFSNodeHardLinkBit = 8;    { Set if the file or directory has a link count > 1 }
	kFSNodeHardLinkMask = $00000100;

{  Constants for sharingFlags field of FSCatalogInfo }
const
	kFSNodeInSharedBit = 2;    { Set if a directory is within a share point }
	kFSNodeInSharedMask = $0004;
	kFSNodeIsMountedBit = 3;    { Set if a directory is a share point currently mounted by some user }
	kFSNodeIsMountedMask = $0008;
	kFSNodeIsSharePointBit = 5;    { Set if a directory is a share point (exported volume) }
	kFSNodeIsSharePointMask = $0020;

{$ifc TARGET_CPU_64}
{
 *  FSCatalogInfo
 *  
 *  Discussion:
 *    For each of the items in this structure, if the given bit is set
 *    in the whichInfo paramater to the FSGetCatalogInfo call then the
 *    field will be filled in on return.  Some fields which are not
 *    asked for my be returned as well, but do not depend on this
 *    behaviour.
 }
type
	FSCatalogInfoPtr = ^FSCatalogInfo;
	FSCatalogInfo = record
{
   * kFSCatInfoNodeFlags / flag bits set if the file is locked, open,
   * is a directory, etc.
   }
		nodeFlags: UInt16;              { node flags }

  {
   * kFSCatInfoVolume / the volume reference of the returned item
   }
		volume: FSVolumeRefNum;                 { object's volume ref }

  {
   * kFSCatInfoParentDirID / the directory id of the parent of the
   * returned item
   }
		parentDirID: UInt32;            { parent directory's ID }

  {
   * kFSCatInfoNodeID / the file id of the returned item
   }
		nodeID: UInt32;                 { file/directory ID }

  {
   * kFSCatInfoSharingFlags / kioFlAttribMountedBit,
   * kioFlAttribSharePointBit
   }
		sharingFlags: UInt8;           { kioFlAttribMountedBit and kioFlAttribSharePointBit }
		userPrivileges: UInt8;         { user's effective AFP privileges (same as ioACUser) }
		reserved1: UInt8;

  {
   * Unused
   }
		reserved2: UInt8;

  {
   * kFSCatInfoCreateDate / date and time of creation
   }
		createDate: UTCDateTime;             { date and time of creation }

  {
   * kFSCatInfoContentMod / date and time of last modification of the
   * content of the returned item
   }
		contentModDate: UTCDateTime;         { date and time of last fork modification }

  {
   * kFSCatInfoAttrMod / date and time of the last modification of the
   * attributes of the returned item
   }
		attributeModDate: UTCDateTime;       { date and time of last attribute modification }

  {
   * kFSCatInfoAccessDate / date and time of the last access to the
   * returned item
   }
		accessDate: UTCDateTime;             { date and time of last access (for Mac OS X) }

  {
   * kFSCatInfoBackupDate / date and time of the last backup for the
   * returned item
   }
		backupDate: UTCDateTime;             { date and time of last backup }

  {
   * kFSCatInfoPermissions / Mac OS X only, file system permissions of
   * the returned item.  Coerce to a FSPermissionInfo to use.
   }
		permissions: FSPermissionInfo;            { permissions (for Mac OS X), as FSPermissionInfo }

  {
   * kFSCatInfoFinderInfo / file type, creator, flags, location. 
   * Coerce to a File/FolderInfo to use.
   }
  finderInfo: packed array [0..15] of UInt8;         { Finder information part 1, as FileInfo or FolderInfo  }

  {
   * kFSCatInfoFinderXInfo / icon, script, et al.  Coerce to a
   * ExtendedFile/FolderInfo to use.
   }
  extFinderInfo: packed array [0..15] of UInt8;      { Finder information part 2, as ExtendedFileInfo or ExtendedFolderInfo }


  {
   * kFSCatInfoDataSizes / the logical size of the data fork of the
   * returned item if a file
   }
		dataLogicalSize: UInt64;        { files only }

  {
   * kFSCatInfoDataSizes / the physical size of the data fork of the
   * returned item if a file
   }
		dataPhysicalSize: UInt64;       { files only }

  {
   * kFSCatInfoRsrcSizes / the logical size of the resource fork of the
   * returned item if a file
   }
		rsrcLogicalSize: UInt64;        { files only }

  {
   * kFSCatInfoRsrcSizes / the physical size of the resource fork of
   * the returned item if a file
   }
		rsrcPhysicalSize: UInt64;       { files only }


  {
   * kFSCatInfoValence / folders only, zero for files.
   }
		valence: UInt32;                { folders only }

  {
   * kFSCatInfoTextEncoding / the text encoding hint for the returned
   * item
   }
		textEncodingHint: TextEncoding;
	end;
{$elsec}
type
	FSCatalogInfoPtr = ^FSCatalogInfo;
	FSCatalogInfo = record
		nodeFlags: UInt16;              { node flags }
		volume: FSVolumeRefNum;                 { object's volume ref }
		parentDirID: UInt32;            { parent directory's ID }
		nodeID: UInt32;                 { file/directory ID }
		sharingFlags: UInt8;           { kioFlAttribMountedBit and kioFlAttribSharePointBit }
		userPrivileges: UInt8;         { user's effective AFP privileges (same as ioACUser) }
		reserved1: UInt8;
		reserved2: UInt8;
		createDate: UTCDateTime;             { date and time of creation }
		contentModDate: UTCDateTime;         { date and time of last fork modification }
		attributeModDate: UTCDateTime;       { date and time of last attribute modification }
		accessDate: UTCDateTime;             { date and time of last access (for Mac OS X) }
		backupDate: UTCDateTime;             { date and time of last backup }
		permissions:			array [0..3] of UInt32;					{  permissions (for Mac OS X)  }
		finderInfo:				packed array [0..15] of UInt8;			{  Finder information part 1  }
		extFinderInfo:			packed array [0..15] of UInt8;			{  Finder information part 2  }

		dataLogicalSize: UInt64;        { files only }
		dataPhysicalSize: UInt64;       { files only }
		rsrcLogicalSize: UInt64;        { files only }
		rsrcPhysicalSize: UInt64;       { files only }

		valence: UInt32;                { folders only }
		textEncodingHint: TextEncoding;
	end;
{$endc} {TARGET_CPU_64}

type
	FSRefParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: ConstStringPtr;              {ptr to Vol:FileName string}
		ioVRefNum: FSVolumeRefNum;              {volume refnum (DrvNum for Eject and MountVol)}

		reserved1: SInt16;              { was ioRefNum }
		reserved2: UInt8;              { was ioVersNum }
		reserved3: UInt8;              { was ioPermssn }

		ref: {const} FSRefPtr;                    { Input ref; the target of the call }
		whichInfo: FSCatalogInfoBitmap;
		catInfo: FSCatalogInfoPtr;
		nameLength: UniCharCount;             { input name length for create/rename }
		name: {const} UniCharPtr;                   { input name for create/rename }
		ioDirID: UInt32;
		spec: FSSpecPtr;
		parentRef: FSRefPtr;              { ref of directory to move another ref to }
		newRef: FSRefPtr;                 { Output ref }
		textEncodingHint: TextEncoding;       { for Rename, MakeFSRefUnicode }
		outName: HFSUniStr255Ptr;                { Output name for GetCatalogInfo }
	end;
	FSRefParamPtr = ^FSRefParam;
{ for use with PBCreateFileAndOpenFork}
type
	FSRefForkIOParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		parentRef: {const} FSRefPtr;              { ref of directory to move another ref to }
		nameLength: UniCharCount;             { input name length for create/rename }
		name: {const} UniCharPtr;                   { input name for create/rename }
		whichInfo: FSCatalogInfoBitmap;
		catInfo: {const} FSCatalogInfoPtr;
		forkNameLength: UniCharCount;         { input; length of fork name }
		forkName: {const} UniCharPtr;               { input; name of fork (NULL indicates data fork) }
		permissions: SInt8;            { desired access to the fork }
		reserved1: UInt8;
		forkRefNum: FSIORefNum;             { Output refNum of newly opened fork }
		newRef: FSRefPtr;                 { Output ref }
	end;
	FSRefForkIOParamPtr = ^FSRefForkIOParam;
type
	FSIterator = ^OpaqueFSIterator; { an opaque type }
	OpaqueFSIterator = record end;
	FSIteratorPtr = ^FSIterator;  { when a var xx:FSIterator parameter can be nil, it is changed to xx: FSIteratorPtr }
const
	kFSIterateFlat = 0;    { Immediate children of container only }
	kFSIterateSubtree = 1;    { Entire subtree rooted at container }
	kFSIterateDelete = 2;
	kFSIterateReserved = -4; { SInt32($FFFFFFFC) }

type
	FSIteratorFlags = OptionBits;
	FSSearchParams = record
		searchTime: Duration;             { a Time Manager duration }
		searchBits: OptionBits;             { which fields to search on }
		searchNameLength: UniCharCount;
		searchName: {const} UniCharPtr;
		searchInfo1: FSCatalogInfoPtr;            { values and lower bounds }
		searchInfo2: FSCatalogInfoPtr;            { masks and upper bounds }
	end;
	FSSearchParamsPtr = ^FSSearchParams;
type
	FSCatalogBulkParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		containerChanged: Boolean;       { true if container changed since last iteration }
		reserved: UInt8;               { make following fields 4-byte aligned }

		iteratorFlags: FSIteratorFlags;
		iterator: FSIterator;
		container: {const} FSRefPtr;              { directory/volume to iterate }
		maximumItems: ItemCount;
		actualItems: ItemCount;
		whichInfo: FSCatalogInfoBitmap;
		catalogInfo: FSCatalogInfoPtr;            { returns an array }
		refs: FSRefPtr;                   { returns an array }
		specs: FSSpecPtr;                  { returns an array }
		names: HFSUniStr255Ptr;                  { returns an array }
		searchParams: {const} FSSearchParamsPtr;
	end;
	FSCatalogBulkParamPtr = ^FSCatalogBulkParam;
type
	FSAllocationFlags = UInt16;
const
	kFSAllocDefaultFlags = $0000; { as much as possible, not contiguous }
	kFSAllocAllOrNothingMask = $0001; { allocate all of the space, or nothing }
	kFSAllocContiguousMask = $0002; { new space must be one contiguous piece }
	kFSAllocNoRoundUpMask = $0004; { don't round up allocation to clump size }
	kFSAllocReservedMask = $FFF8; { these bits are reserved and must not be set }

type
	FSForkIOParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		reserved1: UnivPtr;              { was ioNamePtr }
		reserved2: SInt16;              { was ioVRefNum }
		forkRefNum: FSIORefNum;             { same as ioRefNum }
		reserved3: UInt8;              { was ioVersNum }
		permissions: SInt8;            { desired access to the fork }
		ref: {const} FSRefPtr;                    { which object to open }


		buffer: Ptr;                 {data buffer Ptr}
		requestCount: UInt32;           {requested byte count}
		actualCount: UInt32;            {actual byte count completed}
		positionMode: UInt16;           {initial file positioning}
		positionOffset: SInt64;         {file position offset}

		allocationFlags: FSAllocationFlags;
		allocationAmount: UInt64;

		forkNameLength: UniCharCount;         { input; length of fork name }
		forkName: {const} UniCharPtr;               { input; name of fork }

		forkIterator: CatPositionRec;
		outForkName: HFSUniStr255Ptr;            { output; name of fork }
	end;
	FSForkIOParamPtr = ^FSForkIOParam;
type
	FSForkInfoFlags = UInt8;
	FSForkInfo = record
		flags: FSForkInfoFlags;                  { copy of FCB flags }
		permissions: SInt8;
		volume: FSVolumeRefNum;
		reserved2: UInt32;
		nodeID: UInt32;                 { file or directory ID }
		forkID: UInt32;                 { fork ID }
		currentPosition: UInt64;
		logicalEOF: UInt64;
		physicalEOF: UInt64;
		process: UInt64;                { should be ProcessSerialNumber }
	end;
	FSForkInfoPtr = ^FSForkInfo;
type
	FSForkCBInfoParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		desiredRefNum: FSIORefNum;          { 0 to iterate, non-0 for specific refnum }
		volumeRefNum: FSVolumeRefNum;           { volume to match, or 0 for all volumes }
		iterator: FSIORefNum;               { 0 to start iteration }
		actualRefNum: FSVolumeRefNum;           { actual refnum found }

		ref: FSRefPtr;
		forkInfo: FSForkInfoPtr;
		forkName: HFSUniStr255Ptr;
	end;
	FSForkCBInfoParamPtr = ^FSForkCBInfoParam;
{ Parameter block for use with 64 bit range lock calls.}
type
	FSRangeLockParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		forkRefNum: FSIORefNum;             { fork to operate on }
		requestCount: UInt64;           {requested byte count}
		positionMode: UInt16;           {initial file positioning}
		positionOffset: SInt64;         {file position offset}
		rangeStart: UInt64;             { byte number of first byte (un)locked }
	end;
	FSRangeLockParamPtr = ^FSRangeLockParam;
type
	FSVolumeInfoBitmap = UInt32;
const
	kFSVolInfoNone = $0000;
	kFSVolInfoCreateDate = $0001;
	kFSVolInfoModDate = $0002;
	kFSVolInfoBackupDate = $0004;
	kFSVolInfoCheckedDate = $0008;
	kFSVolInfoFileCount = $0010;
	kFSVolInfoDirCount = $0020;
	kFSVolInfoSizes = $0040; { totalBytes and freeBytes }
	kFSVolInfoBlocks = $0080; { blockSize, totalBlocks, freeBlocks }
	kFSVolInfoNextAlloc = $0100;
	kFSVolInfoRsrcClump = $0200;
	kFSVolInfoDataClump = $0400;
	kFSVolInfoNextID = $0800;
	kFSVolInfoFinderInfo = $1000;
	kFSVolInfoFlags = $2000;
	kFSVolInfoFSInfo = $4000; { filesystemID, signature }
	kFSVolInfoDriveInfo = $8000; { driveNumber, driverRefNum }
	kFSVolInfoGettableInfo = $FFFF; { This seems like it is here just for completeness }
	kFSVolInfoSettableInfo = $3004; { backup date, Finder info, flags }

{ FSVolumeInfo.flags bits.  These are the same as for ioVAtrb, but with nicer names. }
const
	kFSVolFlagDefaultVolumeBit = 5;    { Set if the volume is the default volume }
	kFSVolFlagDefaultVolumeMask = $0020;
	kFSVolFlagFilesOpenBit = 6;    { Set if there are open files or iterators }
	kFSVolFlagFilesOpenMask = $0040;
	kFSVolFlagHardwareLockedBit = 7;    { Set if volume is locked by a hardware setting }
	kFSVolFlagHardwareLockedMask = $0080;
	kFSVolFlagJournalingActiveBit = 14;   { Set if journaling is active on volume }
	kFSVolFlagJournalingActiveMask = $4000;
	kFSVolFlagSoftwareLockedBit = 15;   { Set if volume is locked by software }
	kFSVolFlagSoftwareLockedMask = $8000;


type
	FSVolumeInfo = record
{ Dates -- zero means "never" or "unknown" }
		createDate: UTCDateTime;
		modifyDate: UTCDateTime;
		backupDate: UTCDateTime;
		checkedDate: UTCDateTime;

                                              { File/Folder counts -- return zero if unknown }
		fileCount: UInt32;              { total files on volume }
		folderCount: UInt32;            { total folders on volume }
                                              { Note: no root directory counts }

		totalBytes: UInt64;             { total number of bytes on volume }
		freeBytes: UInt64;              { number of free bytes on volume }

                                              { HFS and HFS Plus specific.  Set fields to zero if not appropriate }
		blockSize: UInt32;              { size (in bytes) of allocation blocks }
		totalBlocks: UInt32;            { number of allocation blocks in volume }
		freeBlocks: UInt32;             { number of unused allocation blocks }
		nextAllocation: UInt32;         { start of next allocation search }
		rsrcClumpSize: UInt32;          { default resource fork clump size }
		dataClumpSize: UInt32;          { default data fork clump size }
		nextCatalogID: UInt32;          { next unused catalog node ID ¥¥¥ OYG ¥¥¥ need to make HFSVolumes.h work Should be HFSCatalogNodeID}
		finderInfo: packed array [0..31] of UInt8;         { information used by Finder }

                                              { Identifying information }
		flags: UInt16;                  { ioVAtrb }
		filesystemID: UInt16;           { ioVFSID }
		signature: UInt16;              { ioVSigWord, unique within an FSID }
		driveNumber: UInt16;            { ioVDrvInfo }
		driverRefNum: FSIORefNum;           { ioVDRefNum }
	end;
	FSVolumeInfoPtr = ^FSVolumeInfo;
type
	FSVolumeInfoParam = record
		qLink: QElemPtr;                  {queue link in header}
		qType: SInt16;                  {type byte for safety check}
		ioTrap: SInt16;                 {FS: the Trap}
		ioCmdAddr: Ptr;              {FS: address to dispatch to}
		ioCompletion: IOCompletionUPP;           {completion routine addr (0 for synch calls)}
		ioResult: {volatile} OSErr;               {result code}
		ioNamePtr: StringPtr;              { unused }
		ioVRefNum: FSVolumeRefNum;              { volume refnum }

		volumeIndex: UInt32;            { index, or 0 to use ioVRefNum }
		whichInfo: FSVolumeInfoBitmap;              { which volumeInfo fields to get/set }
		volumeInfo: FSVolumeInfoPtr;             { information about the volume }
		volumeName: HFSUniStr255Ptr;             { output; pointer to volume name }
		ref: FSRefPtr;                    { volume's FSRef }
	end;
	FSVolumeInfoParamPtr = ^FSVolumeInfoParam;
type
	GetVolParmsInfoBuffer = record
		vMVersion: SInt16;              {version number}
		vMAttrib: SInt32;               {bit vector of attributes (see vMAttrib constants)}
		vMLocalHand: Handle;            {handle to private data}
		vMServerAdr: SInt32;            {AppleTalk server address or zero}
                                              {       vMVersion 1 GetVolParmsInfoBuffer ends here }
		vMVolumeGrade: SInt32;          {approx. speed rating or zero if unrated}
		vMForeignPrivID: SInt16;        {foreign privilege model supported or zero if none}
                                              {       vMVersion 2 GetVolParmsInfoBuffer ends here }
		vMExtendedAttributes: SInt32;   {extended attribute bits (see vMExtendedAttributes constants)}
                                              {       vMVersion 3 GetVolParmsInfoBuffer ends here }
		vMDeviceID: UnivPtr;             { device id name for interoperability with IOKit }
                                              {       vMVersion 4 GetVolParmsInfoBuffer ends here }
		vMMaxNameLength: UniCharCount;
                                              {       vMVersion 5 GetVolParmsInfoBuffer ends here }
	end;
{ 
    The following are structures to be filled out with the _PBGetVolMountInfo call
    and passed back into the _PBVolumeMount call for external file system mounts. 
}
{ the "signature" of the file system }

type
	VolumeType = OSType;
const
{ the signature for AppleShare }
	AppleShareMediaType = FourCharCode('afpm');

{
    VolMount stuff was once in FSM.Å
}
type
	VolMountInfoHeader = record
		length: SInt16;                 { length of location data (including self) }
		media: VolumeType;                  { type of media.  Variable length data follows }
	end;
type
	VolMountInfoPtr = ^VolMountInfoHeader;
{ The new volume mount info record.  The old one is included for compatibility. 
    the new record allows access by foriegn filesystems writers to the flags 
    portion of the record. This portion is now public.  
}
type
	VolumeMountInfoHeader = record
		length: SInt16;                 { length of location data (including self) }
		media: VolumeType;                  { type of media (must be registered with Apple) }
		flags: SInt16;                  { volume mount flags. Variable length data follows }
	end;
	VolumeMountInfoHeaderPtr = ^VolumeMountInfoHeader;
{ volume mount flags }
const
	volMountNoLoginMsgFlagBit = 0;    { Input to VolumeMount: If set, the file system }
	volMountNoLoginMsgFlagMask = $0001; {  should suppresss any log-in message/greeting dialog }
	volMountExtendedFlagsBit = 7;    { Input to VolumeMount: If set, the mount info is a }
	volMountExtendedFlagsMask = $0080; {  AFPXVolMountInfo record for 3.7 AppleShare Client }
	volMountInteractBit = 15;   { Input to VolumeMount: If set, it's OK for the file system }
	volMountInteractMask = $8000; {  to perform user interaction to mount the volume }
	volMountChangedBit = 14;   { Output from VoumeMount: If set, the volume was mounted, but }
	volMountChangedMask = $4000; {  the volume mounting information record needs to be updated. }
	volMountFSReservedMask = $00FF; { bits 0-7 are defined by each file system for its own use }
	volMountSysReservedMask = $FF00; { bits 8-15 are reserved for Apple system use }


type
	AFPVolMountInfo = record
		length: SInt16;                 { length of location data (including self) }
		media: VolumeType;                  { type of media }
		flags: SInt16;                  { bits for no messages, no reconnect }
		nbpInterval: SInt8;            { NBP Interval parameter (IM2, p.322) }
		nbpCount: SInt8;               { NBP Interval parameter (IM2, p.322) }
		uamType: SInt16;                { User Authentication Method }
		zoneNameOffset: SInt16;         { short positive offset from start of struct to Zone Name }
		serverNameOffset: SInt16;       { offset to pascal Server Name string }
		volNameOffset: SInt16;          { offset to pascal Volume Name string }
		userNameOffset: SInt16;         { offset to pascal User Name string }
		userPasswordOffset: SInt16;     { offset to pascal User Password string }
		volPasswordOffset: SInt16;      { offset to pascal Volume Password string }
		AFPData: packed array [1..144] of char;           { variable length data may follow }
	end;
	AFPVolMountInfoPtr = ^AFPVolMountInfo;


{ AFPXVolMountInfo is the new AFP volume mount info record, requires the 3.7 AppleShare Client }
type
	AFPXVolMountInfo = record
		length: SInt16;                 { length of location data (including self) }
		media: VolumeType;                  { type of media }
		flags: SInt16;                  { bits for no messages, no reconnect }
		nbpInterval: SInt8;            { NBP Interval parameter (IM2, p.322) }
		nbpCount: SInt8;               { NBP Interval parameter (IM2, p.322) }
		uamType: SInt16;                { User Authentication Method type }
		zoneNameOffset: SInt16;         { short positive offset from start of struct to Zone Name }
		serverNameOffset: SInt16;       { offset to pascal Server Name string }
		volNameOffset: SInt16;          { offset to pascal Volume Name string }
		userNameOffset: SInt16;         { offset to pascal User Name string }
		userPasswordOffset: SInt16;     { offset to pascal User Password string }
		volPasswordOffset: SInt16;      { offset to pascal Volume Password string }
		extendedFlags: SInt16;          { extended flags word }
		uamNameOffset: SInt16;          { offset to a pascal UAM name string }
		alternateAddressOffset: SInt16; { offset to Alternate Addresses in tagged format }
		AFPData: packed array [1..176] of char;           { variable length data may follow }
	end;
	AFPXVolMountInfoPtr = ^AFPXVolMountInfo;
const
	kAFPExtendedFlagsAlternateAddressMask = 1; {  bit in AFPXVolMountInfo.extendedFlags that means alternateAddressOffset is used}


const
{ constants for use in AFPTagData.fType field}
	kAFPTagTypeIP = $01; { 4 byte IP address (MSB first)            }
	kAFPTagTypeIPPort = $02; { 4 byte IP address, 2 byte port (MSB first)     }
	kAFPTagTypeDDP = $03; { Net,Node,Socket Sent by the server, currently unused by the client }
	kAFPTagTypeDNS = $04;  { DNS name in  address:port format   (total length variable up to 254 chars of dns name)          }


const
{ constants for use in AFPTagData.fLength field}
	kAFPTagLengthIP = $06;
	kAFPTagLengthIPPort = $08;
	kAFPTagLengthDDP = $06;

type
	AFPTagData = packed record
		fLength: UInt8;                { length of this data tag including the fLength field }
		fType: UInt8;
		fData:					packed array [0..0] of UInt8;			{  variable length data  }
	end;
type
	AFPAlternateAddressPtr = ^AFPAlternateAddress;
	AFPAlternateAddress = packed record
{ ¥¥¥ÊNOTE: fVersion was missing in 3.2 Universal Interfaces}
		fVersion: UInt8;               { version of the structure (currently 0x00)}
		fAddressCount: UInt8;
		fAddressList: packed array [0..0] of UInt8;        { actually variable length packed set of AFPTagData }
	end;
const
	kLargeIconSize = 256;
	kLarge4BitIconSize = 512;
	kLarge8BitIconSize = 1024;
	kSmallIconSize = 64;
	kSmall4BitIconSize = 128;
	kSmall8BitIconSize = 256;

{
 *  NewIOCompletionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
{ old name was NewIOCompletionProc }
function NewIOCompletionUPP( userRoutine: IOCompletionProcPtr ): IOCompletionUPP; external name '_NewIOCompletionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeIOCompletionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeIOCompletionUPP( userUPP: IOCompletionUPP ); external name '_DisposeIOCompletionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeIOCompletionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
 { old name was CallIOCompletionProc }
procedure InvokeIOCompletionUPP( paramBlock: ParmBlkPtr; userUPP: IOCompletionUPP ); external name '_InvokeIOCompletionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{#if __MACH__
    #define NewIOCompletionUPP(userRoutine)                     ((IOCompletionUPP)userRoutine)
    #define DisposeIOCompletionUPP(userUPP)
    #define InvokeIOCompletionUPP(paramBlock, userUPP)          (*userUPP)(paramBlock)
#endif}

{
    MakeFSRefUnicode
    Create an FSRef for an existing object specified by 
    Parent FSRef and Unicode name.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             A pointer to the parent directory FSRef
    ->  name            A pointer to Unicode name
    ->  nameLength      The length of the Unicode Name
    ->  textEncodingHint A suggested text encoding to use for the name
    <-  newRef          A pointer to an FSRef
}
{
 *  FSMakeFSRefUnicode()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSMakeFSRefUnicode( const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; textEncodingHint: TextEncoding; var newRef: FSRef ): OSErr; external name '_FSMakeFSRefUnicode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  PBMakeFSRefUnicodeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBMakeFSRefUnicodeSync( var paramBlock: FSRefParam ): OSErr; external name '_PBMakeFSRefUnicodeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBMakeFSRefUnicodeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBMakeFSRefUnicodeAsync( var paramBlock: FSRefParam ); external name '_PBMakeFSRefUnicodeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCompareFSRefs( const (*var*) ref1: FSRef; const (*var*) ref2: FSRef ): OSErr; external name '_FSCompareFSRefs';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCompareFSRefsSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCompareFSRefsSync( var paramBlock: FSRefParam ): OSErr; external name '_PBCompareFSRefsSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCompareFSRefsAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCompareFSRefsAsync( var paramBlock: FSRefParam ); external name '_PBCompareFSRefsAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
    <-  spec            A pointer to the FSSpec for the new directory; may be NULL.  Ignored on 64 bit.
    <-  newRef          A pointer to the FSRef for the new file; may be NULL
}
{
 *  FSCreateFileUnicode()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCreateFileUnicode( const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; whichInfo: FSCatalogInfoBitmap; catalogInfo: {Const}FSCatalogInfoPtr; newRef: FSRefPtr; newSpec: FSSpecPtr ): OSErr; external name '_FSCreateFileUnicode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateFileUnicodeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCreateFileUnicodeSync( var paramBlock: FSRefParam ): OSErr; external name '_PBCreateFileUnicodeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateFileUnicodeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCreateFileUnicodeAsync( var paramBlock: FSRefParam ); external name '_PBCreateFileUnicodeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
    <-  newSpec         A pointer to the FSSpec for the new directory; may be NULL.  Ignored on 64 bit.
    <-  newRef          A pointer to the FSRef for the new directory; may be NULL
}
{
 *  FSCreateDirectoryUnicode()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCreateDirectoryUnicode( const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: UniCharPtr; whichInfo: FSCatalogInfoBitmap; catalogInfo: {Const}FSCatalogInfoPtr; newRef: FSRefPtr; newSpec: FSSpecPtr; newDirID: UInt32Ptr ): OSErr; external name '_FSCreateDirectoryUnicode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateDirectoryUnicodeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCreateDirectoryUnicodeSync( var paramBlock: FSRefParam ): OSErr; external name '_PBCreateDirectoryUnicodeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateDirectoryUnicodeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCreateDirectoryUnicodeAsync( var paramBlock: FSRefParam ); external name '_PBCreateDirectoryUnicodeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSDeleteObject( const (*var*) ref: FSRef ): OSErr; external name '_FSDeleteObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDeleteObjectSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBDeleteObjectSync( var paramBlock: FSRefParam ): OSErr; external name '_PBDeleteObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDeleteObjectAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBDeleteObjectAsync( var paramBlock: FSRefParam ); external name '_PBDeleteObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    UnlinkObject
    Unlinks an existing file or deletes an existing directory.  This call will succeed on an open file, unlike
    DeleteObject.  This call will unlink an archive directory link (where DeleteObject will treat the ADL as a directory).
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory to be deleted
}
{
 *  FSUnlinkObject()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSUnlinkObject( const (*var*) ref: FSRef ): OSErr; external name '_FSUnlinkObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBUnlinkObjectSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function PBUnlinkObjectSync( var paramBlock: FSRefParam ): OSErr; external name '_PBUnlinkObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBUnlinkObjectAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
procedure PBUnlinkObjectAsync( var paramBlock: FSRefParam ); external name '_PBUnlinkObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSMoveObject( const (*var*) ref: FSRef; const (*var*) destDirectory: FSRef; newRef: FSRefPtr ): OSErr; external name '_FSMoveObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBMoveObjectSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBMoveObjectSync( var paramBlock: FSRefParam ): OSErr; external name '_PBMoveObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBMoveObjectAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBMoveObjectAsync( var paramBlock: FSRefParam ); external name '_PBMoveObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSExchangeObjects( const (*var*) ref: FSRef; const (*var*) destRef: FSRef ): OSErr; external name '_FSExchangeObjects';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBExchangeObjectsSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBExchangeObjectsSync( var paramBlock: FSRefParam ): OSErr; external name '_PBExchangeObjectsSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBExchangeObjectsAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBExchangeObjectsAsync( var paramBlock: FSRefParam ); external name '_PBExchangeObjectsAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    FSReplaceObject and FSPathReplaceObject

    You can use the FSReplaceObject and FSPathReplaceObject functions to replace
    one object, the original object, with another, the replacement object.
    FSReplaceObject and FSPathReplaceObject are provided to assist in properly
    preserving metadata during safe save operations. These functions allow files
    to replace files, directories to replace directories, directories to replace
    files and files to replace directories (so a package can replace a file, or
    a file can replace a package). Both objects must reside on the same volume.

    If FSReplaceObject or FSPathReplaceObject are successful, the result object
    will exist at the location of the original object. Nothing will exist at the
    location of the replacement object.  By default FSReplaceObject and  FSPathReplaceObject
    will test the object for write access, and fail if write access is not available.
    Write access in this context is ACL_WRITE_DATA for files and ACL_ADD_FILE for directories.
    This check can be bypassed by passing the kFSReplaceObjectDoNotCheckObjectWriteAccess flag.

    FSReplaceObject and FSPathReplaceObject may need to create a temporary object
    when one of the input objects is a directory, or when the
    kFSReplaceObjectSaveOriginalAsABackup option is specified. The temporary object
    is required since the rename system call does not handle non-empty directory
    replacement or file <-> directory replacement. The file replacing file case
    does not use a temporary object (unless the kFSReplaceObjectSaveOriginalAsABackup
    option is specified). If no temporary name is provided and a temporary object
    is required, then the functions will return an error. Providing a temporary
    name will also allow an application to recover the original object should a
    catastrophic failure (such as a power outage) occur during a replacement
    involving a directory. The temporary object will be created with the temporary
    name either in the temporary directory specified by the temporaryDirectory
    parameter, or in the preferred temporary directory (the preferred temporary
    directory can be obtained with the FSGetTemporaryDirectoryForReplaceObject
    or FSPathGetTemporaryDirectoryForReplaceObject functions). 

    If the kFSReplaceObjectSaveOriginalAsABackup option is specified, the original
    object will be saved in one of two locations: If the temporaryName parameter
    is provided, the original object will be saved with that temporary name in
    the temporary directory. If the temporaryName parameter is not provided and
    the newName parameter is provided, the original object will be saved with
    its original name in the temporary directory.

    The result object will have the data (or directory contents) from the
    replacement object, but will have the original object's creation date and
    backup date. By default, the result object will have extended attributes
    merged from the original object and the replacement object, using the extended
    attributes of the replacement object if the original object has extended
    attributes with the same name. If the kFSReplaceObjectReplaceMetadata option
    flag is passed, then the extended attributes of the original object will be
    completely overwritten by the extended attributes of the replacement object.
    
    The kFSReplaceObjectReplaceMetadata also controls how the label, type, creator,
    hidden extension bit, strong binding, custom icon and custom badge are handled.
    For the custom icon, custom badge, label (where a value of 0 is considered not
    present), and strong binding the replacement object's attributes will be
    used if present, otherwise the original object's attributes will be retained.  For
    the extension hidden bit, type, and creator the original object's attribute is
    preserved.  Setting the kFSReplaceObjectReplaceMetadata flag will use the replacement
    object's attributes for all cases.  Type and creator are only handled in the 
    file to file case.  Strong bindings are not applicable to plain directories.
    
    The default handling of ACL and mode info depends on the types of objects
    involved. If the original object and replacement object are of the same type
    (both files or both directories) then the default behavior is to use the ACL
    and mode of the original object. Setting the kFSReplaceObjectReplacePermissionInfo
    flag will use the ACL and mode from the replacementObject.

    If the objects are different types, then the ACL and mode will be based off
    the original object and will translated as follows:

        Mode:
        file r -> dir rx
        file w -> dir wx
        file x -> dir nothing

        dir r -> file r
        dir w -> file w
        dir x -> file nothing

        ACLs:

        ACL_SEARCH will be added to any allow ACE in the file to directory case.
        ACL_EXECUTE will be removed from any ACE in the file to directory case.
        
        ACL_SEARCH and ACL_DELETE_CHILD will be removed from any ACE in the directory to file case.

        File                Directory
        ACL_READ_DATA   <-> ACL_LIST_DIRECTORY
        ACL_WRITE_DATA  <-> ACL_ADD_FILE
        ACL_APPEND_DATA <-> ACL_ADD_SUBDIRECTORY

        ACL_DELETE , ACL_READ_ATTRIBUTES, ACL_WRITE_ATTRIBUTES, ACL_READ_EXTATTRIBUTES,
        ACL_WRITE_EXTATTRIBUTES, ACL_READ_SECURITY, ACL_WRITE_SECURITY and ACL_CHANGE_OWNER
        are the same for both types and will be left alone.
                        
    This translation behavior can be avoided by passing in either
    kFSReplaceObjectPreservePermissionInfo to use the original object ACL and
    mode info unchanged (except where the bits have different meanings for files
    and directories) or kFSReplaceObjectReplacePermissionInfo to use the ACL and
    mode info of the replacement object (except where the bits have different
    meanings for files and directories). Setting both of these flags is an
    error -- the call will fail with a paramErr. The permissions are only set on
    the top-level object -- the permissions inside a directory are unaffected.
    
    FSReplaceObject may not be atomic -- it may issue multiple system calls to
    accurately replace and preserve the metadata of a file system object.
    
    FSReplaceObject may fail if the source or destination files are open or
    the source or destination objects are directories which contain open files.
}

{
 *  FSReplaceObjectOptions
 *  
 *  Discussion:
 *    Options that can be passed to the FSReplaceObject and
 *    FSPathReplaceObject functions. These are mask values that can be
 *    ORed together to set more than one option.
 }
const
{
   * Use the default behavior (see the discussion of the
   * FSReplaceObject function)
   }
	kFSReplaceObjectDefaultOptions = 0;

  {
   * The result object will only have the metadata from the replacement
   * object. The result object will have no metadata from the original
   * object.
   }
	kFSReplaceObjectReplaceMetadata = $01;

  {
   * The original object will be saved with the temporary name in the
   * temporaryDirectory, or will be saved with the original name (if no
   * temporaryName is provided and a newName is provided) in the
   * temporaryDirectory.
   }
	kFSReplaceObjectSaveOriginalAsABackup = $02;

  {
   * ACL and mode info will come from the replacement object.
   }
	kFSReplaceObjectReplacePermissionInfo = $04;

  {
   * ACL and mode info will come from the original object
   }
	kFSReplaceObjectPreservePermissionInfo = $08;

  {
   * FSReplaceObject does not actually require write permission to
   * perform the replace operation, since no actual writing is done to
   * the original object.  By default ReplaceObject will test the
   * object for write permissions and fail if they are not present
   * (tested using accessx_np).  Write permission in this context is
   * defined as ACL_WRITE_DATA for files and ACL_ADD_FILE for
   * directories.  Pass this flag to skip this check (which will allow
   * the replace to operate on "read only" objects).
   }
	kFSReplaceObjectDoNotCheckObjectWriteAccess = $10;

{
 *  FSReplaceObject()
 *  
 *  Discussion:
 *    This function will replace the object specified by originalObject
 *    with the object specified by replacementObject. The result object
 *    will be in the same parent directory as the original object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    originalObject:
 *      The original object to be replaced.
 *    
 *    replacementObject:
 *      The object which will replace the original object.
 *    
 *    newName:
 *      The new name for the result object. Pass NULL to use the
 *      original object's name.
 *    
 *    temporaryName:
 *      The name of a temporary object should the operation require a
 *      temporary object. The temporary object will be created in the
 *      preferred temporary directory or in the directory specified by
 *      the temporaryDirectory parameter.
 *    
 *    temporaryDirectory:
 *      The directory where the temporary object (if needed) will be
 *      created. The temporary directory must be on the same volume as
 *      the original object. If NULL is passed, then the preferred
 *      temporary directory is used (as per
 *      FSGetTemporaryDirectoryForReplaceObject).
 *    
 *    flags:
 *      A set of options to specify non-default behavior.
 *    
 *    resultObject:
 *      A new FSRef for the result object.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSReplaceObject( const (*var*) originalObject: FSRef; const (*var*) replacementObject: FSRef; newName: CFStringRef; temporaryName: CFStringRef; temporaryDirectory: {const} FSRefPtr; flags: OptionBits; var resultObject: FSRef ): OSStatus; external name '_FSReplaceObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED_MSG(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA, "Use NSFileManager's -replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error: instead. WARNING: FSReplaceObject does not work correctly in sandboxed apps.") *)


{
 *  FSPathReplaceObject()
 *  
 *  Discussion:
 *    This function will replace the object specified by
 *    originalObjectPath with the object specified by
 *    replacementObjectPath. The result object will be in the same
 *    parent directory as the original object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    originalObjectPath:
 *      The path to the original object to be replaced.
 *    
 *    replacementObjectPath:
 *      The path to the object which will replace the original object.
 *    
 *    newName:
 *      The new name for the result object. Pass NULL to use the
 *      original object's name.
 *    
 *    temporaryName:
 *      The name of a temporary object should the operation require a
 *      temporary object. The temporary object will be created in the
 *      preferred temporary directory or in the directory specified by
 *      the temporaryDirectory parameter.
 *    
 *    temporaryDirectoryPath:
 *      The path to the directory where the temporary object (if
 *      needed) will be created. The temporary directory must be on the
 *      same volume as the original object. If NULL is passed, then the
 *      preferred temporary directory is used (as per
 *      FSPathGetTemporaryDirectoryForReplaceObject).
 *    
 *    flags:
 *      A set of options to specify non-default behavior.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSPathReplaceObject( originalObjectPath: ConstCStringPtr; replacementObjectPath: ConstCStringPtr; newName: CFStringRef; temporaryName: CFStringRef; temporaryDirectoryPath: ConstCStringPtr; flags: OptionBits ): OSStatus; external name '_FSPathReplaceObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED_MSG(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA, "Use NSFileManager's -replaceItemAtURL:withItemAtURL:backupItemName:options:resultingItemURL:error: instead. WARNING: FSPathReplaceObject does not work correctly in sandboxed apps.") *)


{
 *  FSGetTemporaryDirectoryForReplaceObject()
 *  
 *  Discussion:
 *    This function will return the preferred directory for use as the
 *    temporaryDirectory by FSReplaceObject. It will return an
 *    appropriate temporary location or the parent directory of the
 *    original object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    originalObject:
 *      The original object to be replaced.
 *    
 *    temporaryDirectory:
 *      The preferred temporary directory.
 *    
 *    flags:
 *      A set of options to specify non-default behavior. Currently no
 *      flags are defined - pass in 0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSGetTemporaryDirectoryForReplaceObject( const (*var*) originalObject: FSRef; var temporaryDirectory: FSRef; flags: OptionBits ): OSStatus; external name '_FSGetTemporaryDirectoryForReplaceObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathGetTemporaryDirectoryForReplaceObject()
 *  
 *  Discussion:
 *    This function will return the preferred directory for use as the
 *    temporaryDirectory by FSPathReplaceObject. It will return an
 *    appropriate temporary location or the parent directory of the
 *    original object.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    originalObjectPath:
 *      The path to the original object to be replaced.
 *    
 *    temporaryDirectoryPath:
 *      The path to the preferred temporary directory.
 *    
 *    maxPathSize:
 *      The size of the buffer pointed to by the temporaryDirectoryPath
 *      parameter.
 *    
 *    flags:
 *      A set of options to specify non-default behavior. Currently no
 *      flags are defined - pass in 0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSPathGetTemporaryDirectoryForReplaceObject( originalObjectPath: ConstCStringPtr; temporaryDirectoryPath: CStringPtr; maxPathSize: UInt32; flags: OptionBits ): OSStatus; external name '_FSPathGetTemporaryDirectoryForReplaceObject';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSRenameUnicode( const (*var*) ref: FSRef; nameLength: UniCharCount; name: UniCharPtr; textEncodingHint: TextEncoding; newRef: FSRefPtr ): OSErr; external name '_FSRenameUnicode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBRenameUnicodeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBRenameUnicodeSync( var paramBlock: FSRefParam ): OSErr; external name '_PBRenameUnicodeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBRenameUnicodeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBRenameUnicodeAsync( var paramBlock: FSRefParam ); external name '_PBRenameUnicodeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    GetCatalogInfo
    Returns various information about a given file or directory.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The file or directory whose information is to be returned
    ->  whichInfo       Which catalog info fields to get
    <-  catInfo         The returned values of catalog info fields; may be NULL
    <-  spec            A pointer to the FSSpec for the object; may be NULL.  Ignored for 64 bit.
    <-  parentRef       A pointer to the FSRef for the object's parent directory; may be NULL
    <-  outName         The Unicode name is returned here.  This pointer may be NULL.
    Note: All of the outputs are optional; if you don't want that particular output, just
    set its pointer to NULL.  This is the call to use to map from an FSRef to an FSSpec.
}
{
 *  FSGetCatalogInfo()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetCatalogInfo( const (*var*) ref: FSRef; whichInfo: FSCatalogInfoBitmap; catalogInfo: FSCatalogInfoPtr; outName: HFSUniStr255Ptr; fsSpec: FSSpecPtr; parentRef: FSRefPtr ): OSErr; external name '_FSGetCatalogInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetCatalogInfoSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBGetCatalogInfoSync( var paramBlock: FSRefParam ): OSErr; external name '_PBGetCatalogInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetCatalogInfoAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBGetCatalogInfoAsync( var paramBlock: FSRefParam ); external name '_PBGetCatalogInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSSetCatalogInfo()
 *  
 *  Summary:
 *    Set catalog information about a given file or directory.
 *  
 *  Discussion:
 *    Set the catalog information for the file or directory represented
 *    by ref.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Parameters:
 *    
 *    ref:
 *      A pointer to an FSRef specifying the file or directory whose
 *      information is to be changed
 *    
 *    whichInfo:
 *      A bitmap specifying which catalog information fields may be
 *      set.  The settable fields are given by the constant
 *      kFSCatInfoSettableInfo; no other bits may be set in whichInfo. 
 *      The one exception to this is that processes running as eid 0 or
 *      euid 0  can change the user ID and group ID of a file or
 *      directory by setting kFSCatInfoSetOwnership in whichInfo and
 *      setting the permissions field of catalogInfo.
 *    
 *    catalogInfo:
 *      A pointer to the structure containing the new catalog
 *      information. Only some of the catalog information fields may be
 *      set.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSSetCatalogInfo( const (*var*) ref: FSRef; whichInfo: FSCatalogInfoBitmap; const (*var*) catalogInfo: FSCatalogInfo ): OSErr; external name '_FSSetCatalogInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetCatalogInfoSync()
 *  
 *  Summary:
 *    Set catalog information about a given file or directory.
 *  
 *  Discussion:
 *    Set the catalog information for the file or directory represented
 *    by ref.  See the description in FSSetCatalogInfo for which items
 *    may be set.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBSetCatalogInfoSync( var paramBlock: FSRefParam ): OSErr; external name '_PBSetCatalogInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetCatalogInfoAsync()
 *  
 *  Summary:
 *    Set catalog information about a given file or directory.
 *  
 *  Discussion:
 *    Set the catalog information for the file or directory represented
 *    by ref.  See the description in FSSetCatalogInfo for which items
 *    may be set.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBSetCatalogInfoAsync( var paramBlock: FSRefParam ); external name '_PBSetCatalogInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSOpenIterator( const (*var*) container: FSRef; iteratorFlags: FSIteratorFlags; var iterator: FSIterator ): OSErr; external name '_FSOpenIterator';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBOpenIteratorSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBOpenIteratorSync( var paramBlock: FSCatalogBulkParam ): OSErr; external name '_PBOpenIteratorSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBOpenIteratorAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBOpenIteratorAsync( var paramBlock: FSCatalogBulkParam ); external name '_PBOpenIteratorAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCloseIterator( iterator: FSIterator ): OSErr; external name '_FSCloseIterator';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCloseIteratorSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCloseIteratorSync( var paramBlock: FSCatalogBulkParam ): OSErr; external name '_PBCloseIteratorSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCloseIteratorAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCloseIteratorAsync( var paramBlock: FSCatalogBulkParam ); external name '_PBCloseIteratorAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
    <-  specs           An array of FSSpecs; one for each returned item.  Ignored on 64 bit.
    <-  names           An array of filenames; one for each returned item
    Note: The catalogInfo, refs, specs, names, and containerChanged are all optional outputs;
    if you don't want that particular output, set its pointer to NULL.
}
{
 *  FSGetCatalogInfoBulk()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetCatalogInfoBulk( iterator: FSIterator; maximumObjects: ItemCount; var actualObjects: ItemCount; containerChanged: BooleanPtr; whichInfo: FSCatalogInfoBitmap; catalogInfos: FSCatalogInfoPtr; refs: FSRefPtr; specs: FSSpecPtr; names: HFSUniStr255Ptr ): OSErr; external name '_FSGetCatalogInfoBulk';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetCatalogInfoBulkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBGetCatalogInfoBulkSync( var paramBlock: FSCatalogBulkParam ): OSErr; external name '_PBGetCatalogInfoBulkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetCatalogInfoBulkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBGetCatalogInfoBulkAsync( var paramBlock: FSCatalogBulkParam ); external name '_PBGetCatalogInfoBulkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
    <-  specs           An array of FSSpecs; one for each returned item.  Ignored on 64 bit.
    <-  names           An array of filenames; one for each returned item
    ->  searchParams    The criteria that controls the matching, including timeout, a bitmap
                        controlling the fields to compare, and the (Unicode) name to compare.
    Note: The catalogInfo, refs, specs, and names are all optional outputs; if you don't want
    that particular output, set its pointer to NULL.
}
{
 *  FSCatalogSearch()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCatalogSearch( iterator: FSIterator; const (*var*) searchCriteria: FSSearchParams; maximumObjects: ItemCount; var actualObjects: ItemCount; containerChanged: BooleanPtr; whichInfo: FSCatalogInfoBitmap; catalogInfos: FSCatalogInfoPtr; refs: FSRefPtr; specs: FSSpecPtr; names: HFSUniStr255Ptr ): OSErr; external name '_FSCatalogSearch';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCatalogSearchSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCatalogSearchSync( var paramBlock: FSCatalogBulkParam ): OSErr; external name '_PBCatalogSearchSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCatalogSearchAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCatalogSearchAsync( var paramBlock: FSCatalogBulkParam ); external name '_PBCatalogSearchAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    CreateFileAndOpenForkUnicode
    Creates a new file and opens the specified fork.  The input filename is in Unicode.
    You can optionally set catalog info for the file.
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  parentRef       The directory where the file is to be created
    ->  whichInfo       Which catalog info fields to set
    ->  catalogInfo     The values for catalog info fields to set; may be NULL
    ->  nameLength      Number of Unicode characters in the file's name
    ->  name            A pointer to the Unicode name
    ->  forkNameLength  The length of the fork name (in Unicode characters).
    ->  forkName        The name of the fork to open (in Unicode).  Passing NULL will open the data fork.
    ->  permissions     The access (read and/or write) you want
    <-  forkRefNum      The reference number for accessing the open fork
    <-  newRef          A pointer to the FSRef for the new file; may be NULL
}
{
 *  FSCreateFileAndOpenForkUnicode()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function FSCreateFileAndOpenForkUnicode( const (*var*) parentRef: FSRef; nameLength: UniCharCount; name: {const} UniCharPtr; whichInfo: FSCatalogInfoBitmap; catalogInfo: FSCatalogInfoPtr; forkNameLength: UniCharCount; forkName: {const} UniCharPtr; permissions: SInt8; var forkRefNum: FSIORefNum; newRef: FSRefPtr ): OSStatus; external name '_FSCreateFileAndOpenForkUnicode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateFileAndOpenForkUnicodeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function PBCreateFileAndOpenForkUnicodeSync( paramBlock: FSRefForkIOParamPtr ): OSStatus; external name '_PBCreateFileAndOpenForkUnicodeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateFileAndOpenForkUnicodeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
procedure PBCreateFileAndOpenForkUnicodeAsync( paramBlock: FSRefForkIOParamPtr ); external name '_PBCreateFileAndOpenForkUnicodeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCreateFork( const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr ): OSErr; external name '_FSCreateFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCreateForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBCreateForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCreateForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCreateForkAsync( var paramBlock: FSForkIOParam ); external name '_PBCreateForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSDeleteFork( const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr ): OSErr; external name '_FSDeleteFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDeleteForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBDeleteForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBDeleteForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDeleteForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBDeleteForkAsync( var paramBlock: FSForkIOParam ); external name '_PBDeleteForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSIterateForks( const (*var*) ref: FSRef; var forkIterator: CatPositionRec; forkName: HFSUniStr255Ptr; forkSize: SInt64Ptr; forkPhysicalSize: UInt64Ptr ): OSErr; external name '_FSIterateForks';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  PBIterateForksSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBIterateForksSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBIterateForksSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBIterateForksAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBIterateForksAsync( var paramBlock: FSForkIOParam ); external name '_PBIterateForksAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSOpenFork( const (*var*) ref: FSRef; forkNameLength: UniCharCount; forkName: UniCharPtr; permissions: SInt8; var forkRefNum: FSIORefNum ): OSErr; external name '_FSOpenFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBOpenForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBOpenForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBOpenForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBOpenForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBOpenForkAsync( var paramBlock: FSForkIOParam ); external name '_PBOpenForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSReadFork( forkRefNum: FSIORefNum; positionMode: UInt16; positionOffset: SInt64; requestCount: ByteCount; buffer: UnivPtr; actualCount: ByteCountPtr ): OSErr; external name '_FSReadFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBReadForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBReadForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBReadForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBReadForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBReadForkAsync( var paramBlock: FSForkIOParam ); external name '_PBReadForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSWriteFork( forkRefNum: FSIORefNum; positionMode: UInt16; positionOffset: SInt64; requestCount: ByteCount; buffer: UnivPtr; actualCount: ByteCountPtr ): OSErr; external name '_FSWriteFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBWriteForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBWriteForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBWriteForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBWriteForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBWriteForkAsync( var paramBlock: FSForkIOParam ); external name '_PBWriteForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetForkPosition( forkRefNum: FSIORefNum; var position: SInt64 ): OSErr; external name '_FSGetForkPosition';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetForkPositionSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBGetForkPositionSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBGetForkPositionSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetForkPositionAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBGetForkPositionAsync( var paramBlock: FSForkIOParam ); external name '_PBGetForkPositionAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSSetForkPosition( forkRefNum: FSIORefNum; positionMode: UInt16; positionOffset: SInt64 ): OSErr; external name '_FSSetForkPosition';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetForkPositionSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBSetForkPositionSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBSetForkPositionSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetForkPositionAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBSetForkPositionAsync( var paramBlock: FSForkIOParam ); external name '_PBSetForkPositionAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetForkSize( forkRefNum: FSIORefNum; var forkSize: SInt64 ): OSErr; external name '_FSGetForkSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetForkSizeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBGetForkSizeSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBGetForkSizeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetForkSizeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBGetForkSizeAsync( var paramBlock: FSForkIOParam ); external name '_PBGetForkSizeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSSetForkSize( forkRefNum: FSIORefNum; positionMode: UInt16; positionOffset: SInt64 ): OSErr; external name '_FSSetForkSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetForkSizeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBSetForkSizeSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBSetForkSizeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetForkSizeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBSetForkSizeAsync( var paramBlock: FSForkIOParam ); external name '_PBSetForkSizeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSAllocateFork( forkRefNum: FSIORefNum; flags: FSAllocationFlags; positionMode: UInt16; positionOffset: SInt64; requestCount: UInt64; actualCount: UInt64Ptr ): OSErr; external name '_FSAllocateFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBAllocateForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBAllocateForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBAllocateForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBAllocateForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBAllocateForkAsync( var paramBlock: FSForkIOParam ); external name '_PBAllocateForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSFlushFork( forkRefNum: FSIORefNum ): OSErr; external name '_FSFlushFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBFlushForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBFlushForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBFlushForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBFlushForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBFlushForkAsync( var paramBlock: FSForkIOParam ); external name '_PBFlushForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSCloseFork( forkRefNum: FSIORefNum ): OSErr; external name '_FSCloseFork';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCloseForkSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBCloseForkSync( var paramBlock: FSForkIOParam ): OSErr; external name '_PBCloseForkSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBCloseForkAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBCloseForkAsync( var paramBlock: FSForkIOParam ); external name '_PBCloseForkAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetForkCBInfo( desiredRefNum: FSIORefNum; volume: FSVolumeRefNum; iterator: SInt16Ptr; actualRefNum: SInt16Ptr; forkInfo: FSForkInfoPtr; ref: FSRefPtr; outForkName: HFSUniStr255Ptr ): OSErr; external name '_FSGetForkCBInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetForkCBInfoSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBGetForkCBInfoSync( var paramBlock: FSForkCBInfoParam ): OSErr; external name '_PBGetForkCBInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetForkCBInfoAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBGetForkCBInfoAsync( var paramBlock: FSForkCBInfoParam ); external name '_PBGetForkCBInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    PBXLockRange
    Lock a range of bytes of the file fork specified by forkRefNum.
    This is only supported on some volume formats. 
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork to read from
    ->  requestCount    The number of bytes to lock
    ->  positionMode    The base location for start of the range
    ->  positionOffset  The offset from base location for start of the range
    <-  rangeStart      Number of the first byte locked
}
{
 *  FSLockRange()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function FSLockRange( forkRefNum: FSIORefNum; positionMode: UInt16; positionOffset: SInt64; requestCount: UInt64; var rangeStart: UInt64 ): OSStatus; external name '_FSLockRange';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBXLockRangeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function PBXLockRangeSync( paramBlock: FSRangeLockParamPtr ): OSStatus; external name '_PBXLockRangeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBXLockRangeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function PBXLockRangeAsync( paramBlock: FSRangeLockParamPtr ): OSStatus; external name '_PBXLockRangeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    PBXUnlockRange
    Unlock a range of bytes of the file fork specified by forkRefNum.
    This is only supported on some volume formats. 
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  forkRefNum      The reference number of the fork to read from
    ->  requestCount    The number of bytes to lock
    ->  positionMode    The base location for start of the range
    ->  positionOffset  The offset from base location for start of the range
    <-  rangeStart      Number of the first byte unlocked
}
{
 *  FSUnlockRange()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function FSUnlockRange( forkRefNum: FSIORefNum; positionMode: UInt16; positionOffset: SInt64; requestCount: UInt64; var rangeStart: UInt64 ): OSStatus; external name '_FSUnlockRange';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBXUnlockRangeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function PBXUnlockRangeSync( paramBlock: FSRangeLockParamPtr ): OSStatus; external name '_PBXUnlockRangeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBXUnlockRangeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function PBXUnlockRangeAsync( paramBlock: FSRangeLockParamPtr ): OSStatus; external name '_PBXUnlockRangeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetVolumeInfo( volume: FSVolumeRefNum; volumeIndex: ItemCount; actualVolume: FSVolumeRefNumPtr; whichInfo: FSVolumeInfoBitmap; info: FSVolumeInfoPtr; volumeName: HFSUniStr255Ptr; rootDirectory: FSRefPtr ): OSErr; external name '_FSGetVolumeInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetVolumeInfoSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBGetVolumeInfoSync( var paramBlock: FSVolumeInfoParam ): OSErr; external name '_PBGetVolumeInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBGetVolumeInfoAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBGetVolumeInfoAsync( var paramBlock: FSVolumeInfoParam ); external name '_PBGetVolumeInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSSetVolumeInfo( volume: FSVolumeRefNum; whichInfo: FSVolumeInfoBitmap; const (*var*) info: FSVolumeInfo ): OSErr; external name '_FSSetVolumeInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetVolumeInfoSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBSetVolumeInfoSync( var paramBlock: FSVolumeInfoParam ): OSErr; external name '_PBSetVolumeInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBSetVolumeInfoAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBSetVolumeInfoAsync( var paramBlock: FSVolumeInfoParam ); external name '_PBSetVolumeInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    FSGetDataForkName
    Returns the constant for the name of the data fork (the empty string)
}
{
 *  FSGetDataForkName()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetDataForkName( var dataForkName: HFSUniStr255 ): OSErr; external name '_FSGetDataForkName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    FSGetResourceForkName
    Returns the constant for the name of the resource fork
    (currently "RESOURCE_FORK").
}
{
 *  FSGetResourceForkName()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSGetResourceForkName( var resourceForkName: HFSUniStr255 ): OSErr; external name '_FSGetResourceForkName';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSRefMakePath()
 *  
 *  Summary:
 *    converts an FSRef to a POSIX path
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Parameters:
 *    
 *    ref:
 *      the file/dir to get the POSIX path for
 *    
 *    path:
 *      a pointer to a buffer which FSRefMakePath will fill with a UTF8
 *      encoded C string representing the path the the specified FSRef
 *    
 *    pathBufferSize:
 *      the size of the path buffer in bytes.  This size should include
 *      the NULL terminator for the returned path string.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function FSRefMakePath( const (*var*) ref: FSRef; path: CStringPtr; pathBufferSize: UInt32 ): OSStatus; external name '_FSRefMakePath';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathMakeRef()
 *  
 *  Summary:
 *    converts a POSIX path to an FSRef
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Parameters:
 *    
 *    path:
 *      a pointer to a UTF8 encoded C String that is a POSIX path
 *    
 *    ref:
 *      a pointer to an FSRef to fill in
 *    
 *    isDirectory:
 *      an optional pointer to a Boolean that will be filled in with
 *      whether the specified path is a directory (vs. a file)
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function FSPathMakeRef( path: CStringPtr; var ref: FSRef; isDirectory: BooleanPtr ): OSStatus; external name '_FSPathMakeRef';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathMakeRefOptions
 *  
 *  Discussion:
 *    Options that can be passed to the FSPathMakeRefWithOptions call.
 }
const
	kFSPathMakeRefDefaultOptions = 0;
	kFSPathMakeRefDoNotFollowLeafSymlink = $01;

{
 *  FSPathMakeRefWithOptions()
 *  
 *  Summary:
 *    converts a POSIX path to an FSRef allowing for options other than
 *    the default
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    path:
 *      a pointer to a UTF8 encoded C String that is a POSIX path
 *    
 *    options:
 *      a set of FSPathMakeRef options
 *    
 *    ref:
 *      a pointer to an FSRef to fill in
 *    
 *    isDirectory:
 *      an optional pointer to a Boolean that will be filled in with
 *      whether the specified path is a directory (vs. a file)
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function FSPathMakeRefWithOptions( path: CStringPtr; options: OptionBits; var ref: FSRef; isDirectory: BooleanPtr ): OSStatus; external name '_FSPathMakeRefWithOptions';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSIsFSRefValid()
 *  
 *  Summary:
 *    Returns true if ref refers to an existing valid file system
 *    object, false otherwise.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    ref:
 *      FSRef to test
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.4 and later
 *    Non-Carbon CFM:   not available
 }
function FSIsFSRefValid( const (*var*) ref: FSRef ): Boolean; external name '_FSIsFSRefValid';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
	FNMessage = UInt32;
const
	kFNDirectoryModifiedMessage = 1;

{
 *  FNNotify()
 *  
 *  Summary:
 *    Broadcasts notification of changes to the specified directory.
 *  
 *  Discussion:
 *    FNNotify is used to notify system clients (such as the Finder) of
 *    modifications to the contents of a directory, specifically
 *    addition or removal of files or folders from the directory. The
 *    Finder and other system clients will refresh their views of the
 *    specified directory when they receive the change notification.
 *    FNNotify is not meant to notify the Finder of changes to a
 *    specific file (for example, changes to a file's type or creator);
 *    for that purpose, use a kAESync AppleEvent sent to the Finder.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
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
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function FNNotify( const (*var*) ref: FSRef; message: FNMessage; flags: OptionBits ): OSStatus; external name '_FNNotify';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FNNotifyByPath()
 *  
 *  Summary:
 *    Broadcasts notification of changes to the specified directory.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
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
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function FNNotifyByPath( path: CStringPtr; message: FNMessage; flags: OptionBits ): OSStatus; external name '_FNNotifyByPath';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FNNotifyAll()
 *  
 *  Discussion:
 *    Broadcasts notification of changes to the filesystem (should only
 *    be used by installers or programs which make lots of changes and
 *    only send one broadcast).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
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
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.0 and later
 *    Non-Carbon CFM:   not available
 }
function FNNotifyAll( message: FNMessage; flags: OptionBits ): OSStatus; external name '_FNNotifyAll';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
	FNSubscriptionRef = ^OpaqueFNSubscriptionRef; { an opaque type }
	OpaqueFNSubscriptionRef = record end;
	FNSubscriptionRefPtr = ^FNSubscriptionRef;  { when a var xx:FNSubscriptionRef parameter can be nil, it is changed to xx: FNSubscriptionRefPtr }

{
 *  Discussion:
 *    Options that can be specified at subscription time.
 }
const
{
   * Specify this option if you do not want to receive notifications on
   * this subscription when FNNotifyAll is called; by default any
   * subscription is also implicitly a subscription to wildcard
   * notifications
   }
	kFNNoImplicitAllSubscription = 1 shl 0;

  {
   * Specify this option if you want to receive notifications on this
   * subscription when your application is in background.  By default
   * notifications will be coalesced and and delivered when your
   * application becomes foreground.
   }
	kFNNotifyInBackground = 1 shl 1;


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
	FNSubscriptionProcPtr = procedure( message: FNMessage; flags: OptionBits; refcon: UnivPtr; subscription: FNSubscriptionRef );

	FNSubscriptionUPP = FNSubscriptionProcPtr;
{
 *  NewFNSubscriptionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewFNSubscriptionUPP( userRoutine: FNSubscriptionProcPtr ): FNSubscriptionUPP; external name '_NewFNSubscriptionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeFNSubscriptionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeFNSubscriptionUPP( userUPP: FNSubscriptionUPP ); external name '_DisposeFNSubscriptionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeFNSubscriptionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure InvokeFNSubscriptionUPP( message: FNMessage; flags: OptionBits; refcon: UnivPtr; subscription: FNSubscriptionRef; userUPP: FNSubscriptionUPP ); external name '_InvokeFNSubscriptionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{#if __MACH__
    #define NewFNSubscriptionUPP(userRoutine)                   ((FNSubscriptionUPP)userRoutine)
    #define DisposeFNSubscriptionUPP(userUPP)
    #define InvokeFNSubscriptionUPP(message, flags, refcon, subscription, userUPP) (*userUPP)(message, flags, refcon, subscription)
#endif}

{
 *  FNSubscribe()
 *  
 *  Summary:
 *    Subscribe to change notifications for the specified directory.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.1
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
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FNSubscribe( const (*var*) directoryRef: FSRef; callback: FNSubscriptionUPP; refcon: UnivPtr; flags: OptionBits; var subscription: FNSubscriptionRef ): OSStatus; external name '_FNSubscribe';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FNSubscribeByPath()
 *  
 *  Summary:
 *    Subscribe to change notifications for the specified directory.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.1
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
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FNSubscribeByPath( directoryPath: CStringPtr; callback: FNSubscriptionUPP; refcon: UnivPtr; flags: OptionBits; var subscription: FNSubscriptionRef ): OSStatus; external name '_FNSubscribeByPath';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FNUnsubscribe()
 *  
 *  Summary:
 *    Release a subscription which is no longer needed.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.1
 *  
 *  Parameters:
 *    
 *    subscription:
 *      Subscription previously returned from FNSubscribe or
 *      FNSubscribeForPath
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FNUnsubscribe( subscription: FNSubscriptionRef ): OSStatus; external name '_FNUnsubscribe';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


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
 *  Mac OS X threading:
 *    Thread safe since version 10.1
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
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Non-Carbon CFM:   not available
 }
function FNGetDirectoryForSubscription( subscription: FNSubscriptionRef; var ref: FSRef ): OSStatus; external name '_FNGetDirectoryForSubscription';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ Async Volume Operation Status return values}
const
	kAsyncMountInProgress = 1;
	kAsyncMountComplete = 2;
	kAsyncUnmountInProgress = 3;
	kAsyncUnmountComplete = 4;
	kAsyncEjectInProgress = 5;
	kAsyncEjectComplete = 6;


type
	FSMountStatus = UInt32;
	FSEjectStatus = UInt32;
	FSUnmountStatus = UInt32;
	FSVolumeOperation = ^OpaqueFSVolumeOperation; { an opaque type }
	OpaqueFSVolumeOperation = record end;
	FSVolumeMountProcPtr = procedure( volumeOp: FSVolumeOperation; clientData: UnivPtr; err: OSStatus; mountedVolumeRefNum: FSVolumeRefNum );
	FSVolumeUnmountProcPtr = procedure( volumeOp: FSVolumeOperation; clientData: UnivPtr; err: OSStatus; volumeRefNum: FSVolumeRefNum; dissenter: pid_t );
	FSVolumeEjectProcPtr = procedure( volumeOp: FSVolumeOperation; clientData: UnivPtr; err: OSStatus; volumeRefNum: FSVolumeRefNum; dissenter: pid_t );
	FSVolumeMountUPP = FSVolumeMountProcPtr;
	FSVolumeUnmountUPP = FSVolumeUnmountProcPtr;
	FSVolumeEjectUPP = FSVolumeEjectProcPtr;
{
 *  NewFSVolumeMountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function NewFSVolumeMountUPP( userRoutine: FSVolumeMountProcPtr ): FSVolumeMountUPP; external name '_NewFSVolumeMountUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewFSVolumeUnmountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function NewFSVolumeUnmountUPP( userRoutine: FSVolumeUnmountProcPtr ): FSVolumeUnmountUPP; external name '_NewFSVolumeUnmountUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewFSVolumeEjectUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
function NewFSVolumeEjectUPP( userRoutine: FSVolumeEjectProcPtr ): FSVolumeEjectUPP; external name '_NewFSVolumeEjectUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeFSVolumeMountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
procedure DisposeFSVolumeMountUPP( userUPP: FSVolumeMountUPP ); external name '_DisposeFSVolumeMountUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeFSVolumeUnmountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
procedure DisposeFSVolumeUnmountUPP( userUPP: FSVolumeUnmountUPP ); external name '_DisposeFSVolumeUnmountUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeFSVolumeEjectUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
procedure DisposeFSVolumeEjectUPP( userUPP: FSVolumeEjectUPP ); external name '_DisposeFSVolumeEjectUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeFSVolumeMountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
procedure InvokeFSVolumeMountUPP( volumeOp: FSVolumeOperation; clientData: UnivPtr; err: OSStatus; mountedVolumeRefNum: FSVolumeRefNum; userUPP: FSVolumeMountUPP ); external name '_InvokeFSVolumeMountUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeFSVolumeUnmountUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
procedure InvokeFSVolumeUnmountUPP( volumeOp: FSVolumeOperation; clientData: UnivPtr; err: OSStatus; volumeRefNum: FSVolumeRefNum; dissenter: pid_t; userUPP: FSVolumeUnmountUPP ); external name '_InvokeFSVolumeUnmountUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeFSVolumeEjectUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.2 and later
 *    Non-Carbon CFM:   not available
 }
procedure InvokeFSVolumeEjectUPP( volumeOp: FSVolumeOperation; clientData: UnivPtr; err: OSStatus; volumeRefNum: FSVolumeRefNum; dissenter: pid_t; userUPP: FSVolumeEjectUPP ); external name '_InvokeFSVolumeEjectUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{#if __MACH__
    #define NewFSVolumeMountUPP(userRoutine)                    ((FSVolumeMountUPP)userRoutine)
    #define NewFSVolumeUnmountUPP(userRoutine)                  ((FSVolumeUnmountUPP)userRoutine)
    #define NewFSVolumeEjectUPP(userRoutine)                    ((FSVolumeEjectUPP)userRoutine)
    #define DisposeFSVolumeMountUPP(userUPP)
    #define DisposeFSVolumeUnmountUPP(userUPP)
    #define DisposeFSVolumeEjectUPP(userUPP)
    #define InvokeFSVolumeMountUPP(volumeOp, clientData, err, mountedVolumeRefNum, userUPP) (*userUPP)(volumeOp, clientData, err, mountedVolumeRefNum)
    #define InvokeFSVolumeUnmountUPP(volumeOp, clientData, err, volumeRefNum, dissenter, userUPP) (*userUPP)(volumeOp, clientData, err, volumeRefNum, dissenter)
    #define InvokeFSVolumeEjectUPP(volumeOp, clientData, err, volumeRefNum, dissenter, userUPP) (*userUPP)(volumeOp, clientData, err, volumeRefNum, dissenter)
#endif}


{
 *  Discussion:
 *    Options that can be passed to the FSMountServerVolumeCalls. These
 *    options are not for use with the local volumes.
 }
const
{
   * Specify this option if you do want the volume displayed as a stand
   * along volume in the UI.
   }
	kFSMountServerMarkDoNotDisplay = 1 shl 0;

  {
   * Specify this option if you want the volume mounted on the mountdir
   * passed in instead of in it.
   }
	kFSMountServerMountOnMountDir = 1 shl 2;

  {
   * Specify this option if you want to suppress connection-time UI
   * when mounting the server volume.
   }
	kFSMountServerSuppressConnectionUI = 1 shl 6;


{
 *  Discussion:
 *    Deprecated options that can be passed to
 *    FSMountServerVolumeCalls.  The options will have no effect on the
 *    OS version they are noted as deprecated on or later.
 }
const
{
   * Specify this option if you do not want other processes notified
   * that this volume has been mounted.  Deprecated on Mac OS X 10.5. 
   * Use kFSMountServerMarkDoNotDisplay instead.
   }
	kFSMountServerMountWithoutNotification = 1 shl 1;


{
 *  Discussion:
 *    Options that can be passed to the FSEjectVolume calls.
 }
const
{
   * Specify this option if you want the volume forcibly ejected. 
   * Force ejecting a volume will very likely result in data loss since
   * the volume will be ejected even if there are open files on it or
   * other volumes that share the same device. This option should be
   * reserved for situations such as the backing store for a volume is
   * gone (so the data is lost regardless).
   }
	kFSEjectVolumeForceEject = 1 shl 0;


{
 *  Discussion:
 *    Options that can be passed to the FSUnmountVolume calls.
 }
const
{
   * Specify this option if you want the volume forcibly unmounted. 
   * Force unmounting a volume will very likely result in data loss
   * since the volume will be ejected even if there are open files on
   * it. This option should be reserved for situations such as the
   * backing store for a volume is gone (so the data is lost
   * regardless).
   }
	kFSUnmountVolumeForceUnmount = 1 shl 0;

{
 *  FSCreateVolumeOperation()
 *  
 *  Discussion:
 *    This routine will return an FSVolumeOperation which can be used
 *    for an async volume operation.  When the operation is completed
 *    the volumeOp should be disposed of to free the memory associated
 *    with the operation using FSDisposeVolumeOperation.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    volumeOp:
 *      The new FSVolumeOperation.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCreateVolumeOperation( var volumeOp: FSVolumeOperation ): OSStatus; external name '_FSCreateVolumeOperation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSDisposeVolumeOperation()
 *  
 *  Discussion:
 *    This routine will release the memory associated with the passed
 *    in volumeOp. It will return paramErr is the volumeOp is in use.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    volumeOp:
 *      The FSVolumeOperation to release.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSDisposeVolumeOperation( volumeOp: FSVolumeOperation ): OSStatus; external name '_FSDisposeVolumeOperation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMountLocalVolumeSync()
 *  
 *  Discussion:
 *    This routine will mount the disk specified by diskID at mountDir
 *    (or the default location if mountDir is NULL).  This routine
 *    returns after the mount is complete.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    diskID:
 *      The disk to mount.
 *    
 *    mountDir:
 *      Pass in NULL (currently only NULL is supported).
 *    
 *    mountedVolumeRefNum:
 *      The volume ref num of the newly mounted volume.
 *    
 *    flags:
 *      Options for future use.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSMountLocalVolumeSync( diskID: CFStringRef; mountDir: CFURLRef; var mountedVolumeRefNum: FSVolumeRefNum; flags: OptionBits ): OSStatus; external name '_FSMountLocalVolumeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMountLocalVolumeAsync()
 *  
 *  Discussion:
 *    This routine will start the process to disk specified by diskID
 *    at mountDir (or the default location if mountDir is NULL).  If a
 *    callback is provided the provided function will be called when
 *    the mount operation is complete.  Once this routine returns noErr
 *    the status of the operation can be found using
 *    FSGetAsyncMountStatus.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    diskID:
 *      The disk to mount.
 *    
 *    mountDir:
 *      Pass in NULL (currently only NULL is supported).
 *    
 *    volumeOp:
 *      An FSVolumeOperation returned by FSCreateVolumeOperation
 *    
 *    clientData:
 *      client data associated with the operation.
 *    
 *    flags:
 *      Options for future use.
 *    
 *    callback:
 *      Function to call when mount is complete.
 *    
 *    runloop:
 *      Runloop to run on.
 *    
 *    runloopMode:
 *      Mode for runloop.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSMountLocalVolumeAsync( diskID: CFStringRef; mountDir: CFURLRef; volumeOp: FSVolumeOperation; clientData: UnivPtr; flags: OptionBits; callback: FSVolumeMountUPP; runloop: CFRunLoopRef; runloopMode: CFStringRef ): OSStatus; external name '_FSMountLocalVolumeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMountServerVolumeSync()
 *  
 *  Discussion:
 *    This routine will mount the server specified by url at mountDir
 *    (or the default location if mountDir is NULL).  An optional user
 *    and password can be passed in for authentication. If no user or
 *    password is provided then the underlying file system will handle
 *    authentication if required.  This routine returns after the mount
 *    is complete.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    url:
 *      The server to mount.
 *    
 *    mountDir:
 *      The directory to mount the server to (default if NULL).
 *    
 *    user:
 *      String to pass as user for authentication.
 *    
 *    password:
 *      String to pass as password for authenticated log in.
 *    
 *    mountedVolumeRefNum:
 *      The volume ref num of the newly mounted volume.
 *    
 *    flags:
 *      Options (such as kFSMountServerMarkDoNotDisplay and
 *      kFSMountServerMountOnMountDir).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSMountServerVolumeSync( url: CFURLRef; mountDir: CFURLRef; user: CFStringRef; password: CFStringRef; var mountedVolumeRefNum: FSVolumeRefNum; flags: OptionBits ): OSStatus; external name '_FSMountServerVolumeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMountServerVolumeAsync()
 *  
 *  Discussion:
 *    This routine will start the process to mount the server specified
 *    by url at mountDir (or the default location if mountDir is NULL).
 *     An optional user and password can be passed in for
 *    authentication. If no user or password is provided then the
 *    underlying file system will handle authentication if required. 
 *    If a callback is provided the provided function will be called
 *    when the mount operation is complete.  Once this routine returns
 *    noErr the status of the operation can be found using
 *    FSGetAsyncMountStatus.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    url:
 *      The server to mount.
 *    
 *    mountDir:
 *      The directory to mount the server to (default if NULL).
 *    
 *    user:
 *      String to pass as user for authentication.
 *    
 *    password:
 *      String to pass as password for authenticated log in.
 *    
 *    volumeOp:
 *      An FSVolumeOperation returned by FSCreateVolumeOperation
 *    
 *    clientData:
 *      client data associated with the operation.
 *    
 *    flags:
 *      Options (such as kFSMountServerMarkDoNotDisplay and
 *      kFSMountServerMountOnMountDir).
 *    
 *    callback:
 *      Function to call when mount is complete.
 *    
 *    runloop:
 *      Runloop run on.
 *    
 *    runloopMode:
 *      Mode for runloop.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSMountServerVolumeAsync( url: CFURLRef; mountDir: CFURLRef; user: CFStringRef; password: CFStringRef; volumeOp: FSVolumeOperation; clientData: UnivPtr; flags: OptionBits; callback: FSVolumeMountUPP; runloop: CFRunLoopRef; runloopMode: CFStringRef ): OSStatus; external name '_FSMountServerVolumeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetAsyncMountStatus()
 *  
 *  Discussion:
 *    This routine returns the current status of an asynchronous mount
 *    operation. A return value of noErr signifies that the status
 *    parameter has been filled with valid information.  If the status
 *    is kAsyncMountComplete then the rest of data returned is valid. 
 *    If the status is anything else then the volumeOpStatus and
 *    mountedVolumeRefNum parameters are invalid (The clientData will
 *    be ok).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    volumeOp:
 *      The async volume operation to get status about.
 *    
 *    status:
 *      The status of the operation.
 *    
 *    volumeOpStatus:
 *      If status is kAsyncMountComplete then this contains the
 *      OSStatus for the operation.
 *    
 *    mountedVolumeRefNum:
 *      If status is kAsyncMountComplete and volumeOpStatus is noErr
 *      then this is the volume ref num for the newly mounted volume.
 *    
 *    clientData:
 *      client data associated with the original
 *      FSMountServerVolumeAsync operation.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetAsyncMountStatus( volumeOp: FSVolumeOperation; var status: FSMountStatus; var volumeOpStatus: OSStatus; var mountedVolumeRefNum: FSVolumeRefNum; clientData: UnivPtrPtr ): OSStatus; external name '_FSGetAsyncMountStatus';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSUnmountVolumeSync()
 *  
 *  Discussion:
 *    This routine unmounts the volume specified by vRefNum.  If the
 *    volume cannot be unmounted the pid of the process which denied
 *    the unmount will be returned in the dissenter parameter.  This
 *    routine returns after the unmount is complete.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      The volume reference number of the volume to unmount.
 *    
 *    flags:
 *      Options (such as kFSUnmountVolumeForceUnmount).
 *    
 *    dissenter:
 *      pid of the process which denied the unmount if the unmount is
 *      denied.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSUnmountVolumeSync( vRefNum: FSVolumeRefNum; flags: OptionBits; var dissenter: pid_t ): OSStatus; external name '_FSUnmountVolumeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSUnmountVolumeAsync()
 *  
 *  Discussion:
 *    This routine starts the process of unmounting the volume
 *    specified by vRefNum. If a callback is provided the provided
 *    function will be called when the unmount operation is complete. 
 *    Once this routine returns noErr the status of the operation can
 *    be found using FSGetAsyncUnmountStatus.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      The volume reference number of the volume to unmount.
 *    
 *    flags:
 *      Options (such as kFSUnmountVolumeForceUnmount).
 *    
 *    volumeOp:
 *      An FSVolumeOperation returned by FSCreateVolumeOperation
 *    
 *    clientData:
 *      client data associated with the operation.
 *    
 *    callback:
 *      Function to call when unmount is complete.
 *    
 *    runloop:
 *      Runloop to run on.
 *    
 *    runloopMode:
 *      Mode for runloop.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSUnmountVolumeAsync( vRefNum: FSVolumeRefNum; flags: OptionBits; volumeOp: FSVolumeOperation; clientData: UnivPtr; callback: FSVolumeUnmountUPP; runloop: CFRunLoopRef; runloopMode: CFStringRef ): OSStatus; external name '_FSUnmountVolumeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetAsyncUnmountStatus()
 *  
 *  Discussion:
 *    This routine returns the current status of an asynchronous
 *    unmount operation. A return value of noErr signifies that the
 *    status parameter has been filled with valid information.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    volumeOp:
 *      The async volume operation to get status about.
 *    
 *    status:
 *      The status of the operation.
 *    
 *    volumeOpStatus:
 *      If status is kAsyncUnmountComplete then this contains the
 *      OSStatus for the operation.
 *    
 *    volumeRefNum:
 *      volume reference number of volume being unmounted.
 *    
 *    dissenter:
 *      pid of the process which denied the unmount if the unmount is
 *      denied.
 *    
 *    clientData:
 *      client data associated with the original
 *      FSMountServerVolumeAsync operation.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetAsyncUnmountStatus( volumeOp: FSVolumeOperation; var status: FSUnmountStatus; var volumeOpStatus: OSStatus; var volumeRefNum: FSVolumeRefNum; var dissenter: pid_t; clientData: UnivPtrPtr ): OSStatus; external name '_FSGetAsyncUnmountStatus';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSCancelVolumeOperation()
 *  
 *  Discussion:
 *    This routine will cancel and outstanding asynchronous volume
 *    mounting operation. It currently is only supported for server
 *    mounts.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    volumeOp:
 *      The async volume operation to cancel.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCancelVolumeOperation( volumeOp: FSVolumeOperation ): OSStatus; external name '_FSCancelVolumeOperation';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSEjectVolumeSync()
 *  
 *  Discussion:
 *    This routine ejects the volume specified by vRefNum.  If the
 *    volume cannot be ejected the pid of the process which denied the
 *    unmount will be returned in the dissenter parameter.  This
 *    routine returns after the eject is complete.  Ejecting a volume
 *    will result in the unmounting of other volumes on the same device.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      The volume reference number of the volume to eject.
 *    
 *    flags:
 *      Options (such as kFSEjectVolumeForceEject).
 *    
 *    dissenter:
 *      pid of the process which denied the unmount if the eject is
 *      denied.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSEjectVolumeSync( vRefNum: FSVolumeRefNum; flags: OptionBits; var dissenter: pid_t ): OSStatus; external name '_FSEjectVolumeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSEjectVolumeAsync()
 *  
 *  Discussion:
 *    This routine starts the process of ejecting the volume specified
 *    by vRefNum. If a callback is provided the provided function will
 *    be called when the eject operation is complete.  Once this
 *    routine returns noErr the status of the operation can be found
 *    using FSGetAsyncEjectStatus.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      The volume reference number of the volume to eject.
 *    
 *    flags:
 *      Options (such as kFSEjectVolumeForceEject).
 *    
 *    volumeOp:
 *      An FSVolumeOperation returned by FSCreateVolumeOperation
 *    
 *    clientData:
 *      client data associated with the operation.
 *    
 *    callback:
 *      Function to call when eject is complete.
 *    
 *    runloop:
 *      Runloop to run on.
 *    
 *    runloopMode:
 *      Mode for runloop.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSEjectVolumeAsync( vRefNum: FSVolumeRefNum; flags: OptionBits; volumeOp: FSVolumeOperation; clientData: UnivPtr; callback: FSVolumeEjectUPP; runloop: CFRunLoopRef; runloopMode: CFStringRef ): OSStatus; external name '_FSEjectVolumeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetAsyncEjectStatus()
 *  
 *  Discussion:
 *    This routine returns the current status of an asynchronous eject
 *    operation. A return value of noErr signifies that the status
 *    parameter has been filled with valid information.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    volumeOp:
 *      The async volume operation to get status about.
 *    
 *    status:
 *      The status of the operation.
 *    
 *    volumeOpStatus:
 *      If status is kAsyncEjectComplete then this contains the
 *      OSStatus for the operation.
 *    
 *    volumeRefNum:
 *      volume reference number of volume being ejected.
 *    
 *    dissenter:
 *      pid of the process which denied the unmount if the eject is
 *      denied.
 *    
 *    clientData:
 *      client data associated with the original
 *      FSMountServerVolumeAsync operation.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetAsyncEjectStatus( volumeOp: FSVolumeOperation; var status: FSEjectStatus; var volumeOpStatus: OSStatus; var volumeRefNum: FSVolumeRefNum; var dissenter: pid_t; clientData: UnivPtrPtr ): OSStatus; external name '_FSGetAsyncEjectStatus';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSCopyDiskIDForVolume()
 *  
 *  Discussion:
 *    This routine returns a copy of the diskID for the passed in
 *    volume.  The caller is responsible for releasing the CFString
 *    later.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      FSVolumeRefNum of the target volume.
 *    
 *    diskID:
 *      The diskID string associated with the target volume.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.2 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCopyDiskIDForVolume( vRefNum: FSVolumeRefNum; var diskID: CFStringRef ): OSStatus; external name '_FSCopyDiskIDForVolume';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_2, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSCopyURLForVolume()
 *  
 *  Discussion:
 *    This routine returns a copy of the url for the passed in volume. 
 *    The caller is responsible for releasing the CFURL later.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      FSVolumeRefNum of the target volume.
 *    
 *    url:
 *      The url associated with the target volume.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCopyURLForVolume( vRefNum: FSVolumeRefNum; var url: CFURLRef ): OSStatus; external name '_FSCopyURLForVolume';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_3, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetVolumeForDiskID()
 *  
 *  Discussion:
 *    This routine returnes the FSVolumeRefNum for a given diskID.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    diskID:
 *      The diskID string associated with the target volume.
 *    
 *    vRefNum:
 *      FSVolumeRefNum of the volume which corresponds to the diskID.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetVolumeForDiskID( diskID: CFStringRef; var vRefNum: FSVolumeRefNum ): OSStatus; external name '_FSGetVolumeForDiskID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSCopyDADiskForVolume()
 *  
 *  Discussion:
 *    This routine returns a copy of the diskID for the passed in
 *    volume.  The caller is responsible for releasing the DADisk
 *    later.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    vRefNum:
 *      FSVolumeRefNum of the target volume.
 *    
 *    disk:
 *      The DADisk associated with the target volume.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCopyDADiskForVolume( vRefNum: FSVolumeRefNum; var disk: DADiskRef ): OSStatus; external name '_FSCopyDADiskForVolume';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetVolumeForDADisk()
 *  
 *  Discussion:
 *    This routine returns the FSVolumeRefNum associated with the
 *    volume referenced by the passed in DADiskRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    disk:
 *      The DADiskRef of the target volume.
 *    
 *    vRefNum:
 *      FSVolumeRefNum of the volume which corresponds to the DADiskRef.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetVolumeForDADisk( disk: DADiskRef; var vRefNum: FSVolumeRefNum ): OSStatus; external name '_FSGetVolumeForDADisk';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ File Operation routines}

type
	FSFileOperationRef = ^__FSFileOperation; { an opaque type }
	__FSFileOperation = record end;
	FSFileOperationStage = UInt32;

{
 *  FSFileOperationClientContext
 *  
 *  Discussion:
 *    Structure containing the user-defined data and callbacks for
 *    FSFileOperation client contextual data.
 }
type
	FSFileOperationClientContext = record
{
   * The version number of the structure type being passed in as a
   * parameter to FSCopyObjectAsync or FSMoveObjectAsync function.
   * Valid version number is currently 0.
   }
		version: CFIndex;

  {
   * An arbitrary pointer to client-defined data, which can be
   * associated with the operation and is passed to the callbacks.
   }
		info: UnivPtr;

  {
   * The callback used to add a retain for the operation on the info
   * pointer for the life of the operation, and may be used for
   * temporary references the operation needs to take. This callback
   * returns the actual info pointer to store in the operation, almost
   * always just the pointer passed as the parameter.
   }
		retain: CFAllocatorRetainCallBack;

  {
   * The callback used to remove a retain previously added for the
   * operation on the info pointer.
   }
		release: CFAllocatorReleaseCallBack;

  {
   * The callback used to create a descriptive string representation of
   * the info pointer (or the data pointed to by the info pointer) for
   * debugging purposes. This is used by the CFCopyDescription()
   * function.
   }
		copyDescription: CFAllocatorCopyDescriptionCallBack;
	end;

{
 *  FSFileOperationStatusProcPtr
 *  
 *  Discussion:
 *    Callback function which is called to return status of an
 *    operation.  It will be called when an operation changes stages
 *    (including failing due to an error) or as updated information is
 *    available limited by the statusChangeInterval of the operation.
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      Operation receiving the callback.
 *    
 *    currentItem:
 *      FSRef to item operation is currently processing.  If the
 *      operation is complete then currentItem refers to the target
 *      item (the new item corresponding to the source item in the
 *      destination directory).
 *    
 *    stage:
 *      Current stage of the operation.
 *    
 *    error:
 *      Either noErr or an error value which caused the operation to
 *      fail.
 *    
 *    statusDictionary:
 *      A CFDictionary with more detailed status information.  The
 *      caller should not release this item.  If the item is needed
 *      beyond the scope of the callback then it needs to be copied.
 *    
 *    info:
 *      The info pointer passed in by the client.
 }
type
	FSFileOperationStatusProcPtr = procedure( fileOp: FSFileOperationRef; const (*var*) currentItem: FSRef; stage: FSFileOperationStage; error: OSStatus; statusDictionary: CFDictionaryRef; info: UnivPtr );

{
 *  FSPathFileOperationStatusProcPtr
 *  
 *  Discussion:
 *    Callback function which is called to return status of an
 *    operation.  It will be called when an operation changes stages
 *    (including failing due to an error) or as updated information is
 *    available limited by the statusChangeInterval of the operation.
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      Operation receiving the callback.
 *    
 *    currentItem:
 *      The UTF-8 path for the item operation is currently processing. 
 *      The caller should not release this item.  If the item is needed
 *      beyond the scope of the callback then it needs to be copied. 
 *      If the operation is complete then currentItem refers to the
 *      target item (the new item corresponding to the source item in
 *      the destination directory).
 *    
 *    stage:
 *      Current stage of the operation.
 *    
 *    error:
 *      Either noErr or an error value which caused the operation to
 *      fail.
 *    
 *    statusDictionary:
 *      A CFDictionary with more detailed status information.  The
 *      caller should not release this item.  If the item is needed
 *      beyond the scope of the callback then it needs to be copied.
 *    
 *    info:
 *      The info pointer passed in by the client.
 }
type
	FSPathFileOperationStatusProcPtr = procedure( fileOp: FSFileOperationRef; currentItem: ConstCStringPtr; stage: FSFileOperationStage; error: OSStatus; statusDictionary: CFDictionaryRef; info: UnivPtr );

{
 *  FSFileOperationOptions
 *  
 *  Discussion:
 *    Set of flags that can be passed into an FSMoveObject(A)sync or
 *    FSCopyObject(A)sync call.
 }
const
{
   * Use the default options - no overwrite, fail if any source item
   * cannot be read, cross volume moves OK.
   }
	kFSFileOperationDefaultOptions = 0;

  {
   * Replace an item in the destDir that has the same name as an item
   * being moved/copied there.
   }
	kFSFileOperationOverwrite = $01;

  {
   * Skip items that cannot be read and continue copying/moving instead
   * of failing the operation.
   }
	kFSFileOperationSkipSourcePermissionErrors = $02;

  {
   * Do not perform a copy/delete to move an item across volume
   * boundries - fail the operation instead.
   }
	kFSFileOperationDoNotMoveAcrossVolumes = $04;

  {
   * Skip the preflight for a directory move/copy.  This will limit the
   * status information that can be returned since the totals will not
   * be calculated.
   }
	kFSFileOperationSkipPreflight = $08;


{
 *  FSFileOperationStage
 *  
 *  Discussion:
 *    The stage corresponds to the state of an asynchronous File
 *    Operation.  The stage is reported in the operation's status
 *    callback or as part of an FSFileOperationCopyStatus call.
 }
const
{
   * Operation has not started yet.
   }
	kFSOperationStageUndefined = 0;

  {
   * Operation is calulating sizes and number of items involved in the
   * operation.
   }
	kFSOperationStagePreflighting = 1;

  {
   * Operation is in progress.
   }
	kFSOperationStageRunning = 2;

  {
   * Operation is done.
   }
	kFSOperationStageComplete = 3;

{ FSFileOperation status dictionary keys}
{
 *  kFSOperationTotalBytesKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the total number of bytes that will be moved/copied by
 *    this operation as a CFNumber.  This value is not available for a
 *    directory operation if kFSFileOperationSkipPreflight was
 *    specified.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationTotalBytesKey: CFStringRef; external name '_kFSOperationTotalBytesKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationBytesCompleteKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the number of bytes that have been moved/copied by this
 *    operation at the time the status call was made as a CFNumber. 
 *    During the preflight stage this value represents the currently
 *    known number of bytes that will be copied/moved.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationBytesCompleteKey: CFStringRef; external name '_kFSOperationBytesCompleteKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationBytesRemainingKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the number of bytes that remain to be moved/copied by
 *    this operation at the time the status call was made as a
 *    CFNumber.  This value is not available for a directory operation
 *    if kFSFileOperationSkipPreflight was specified.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationBytesRemainingKey: CFStringRef; external name '_kFSOperationBytesRemainingKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationTotalObjectsKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the total number of objects that will be moved/copied by
 *    this operation as a CFNumber.  This value is not available for a
 *    directory operation if kFSFileOperationSkipPreflight was
 *    specified.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationTotalObjectsKey: CFStringRef; external name '_kFSOperationTotalObjectsKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationObjectsCompleteKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the number of objects that have been moved/copied by this
 *    operation at the time the status call was made as a CFNumber. 
 *    During the preflight stage this value represents the currently
 *    known number of objects that will be copied/moved.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationObjectsCompleteKey: CFStringRef; external name '_kFSOperationObjectsCompleteKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationObjectsRemainingKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the number of objects that remain to be moved/copied by
 *    this operation at the time the status call was made as a
 *    CFNumber.  This value is not available for a directory operation
 *    if kFSFileOperationSkipPreflight was specified.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationObjectsRemainingKey: CFStringRef; external name '_kFSOperationObjectsRemainingKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationTotalUserVisibleObjectsKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the total number of user visibleobjects that will be
 *    moved/copied by this operation as a CFNumber.  This value is not
 *    available for a directory operation if
 *    kFSFileOperationSkipPreflight was specified.  A packaged
 *    application is one user visible object even though it is made up
 *    of multiple files and directories.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationTotalUserVisibleObjectsKey: CFStringRef; external name '_kFSOperationTotalUserVisibleObjectsKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationUserVisibleObjectsCompleteKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the number of user visible objects that have been
 *    moved/copied by this operation at the time the status call was
 *    made as a CFNumber.  During the preflight stage this value
 *    represents the currently known number of objects that will be
 *    copied/moved.  A packaged application is one user visible object
 *    even though it is made up of multiple files and directories.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationUserVisibleObjectsCompleteKey: CFStringRef; external name '_kFSOperationUserVisibleObjectsCompleteKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationUserVisibleObjectsRemainingKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the number of user visible objects that remain to be
 *    moved/copied by this operation at the time the status call was
 *    made as a CFNumber.  This value is not available for a directory
 *    operation if kFSFileOperationSkipPreflight was specified.  A
 *    packaged application is one user visible object even though it is
 *    made up of multiple files and directories.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationUserVisibleObjectsRemainingKey: CFStringRef; external name '_kFSOperationUserVisibleObjectsRemainingKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  kFSOperationThroughputKey
 *  
 *  Discussion:
 *    This value associated with this key in a status dictionary
 *    returns the current throughput for the operation in bytes per
 *    second as a CFNumber.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
var kFSOperationThroughputKey: CFStringRef; external name '_kFSOperationThroughputKey'; (* attribute const *)
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)
{
 *  FSCopyObjectSync()
 *  
 *  Discussion:
 *    This routine will copy the source object into the destination
 *    directory.  The source object can be a file or directory.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    source:
 *      The source object to copy.
 *    
 *    destDir:
 *      The destination directory for the copy.
 *    
 *    destName:
 *      The name for the new object in the destination directory.  Pass
 *      NULL to use the source object name.
 *    
 *    target:
 *      Upon successful completion a ref to the newly created object. 
 *      If source is a directory then target will be the corresponding
 *      object in the destination directory.
 *    
 *    options:
 *      One or more FSFileOperation flags
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCopyObjectSync( const (*var*) source: FSRef; const (*var*) destDir: FSRef; destName: CFStringRef; var target: FSRef; options: OptionBits ): OSStatus; external name '_FSCopyObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMoveObjectSync()
 *  
 *  Discussion:
 *    This routine will move the source object into the destination
 *    directory.  The source object can be a file or directory.  If a
 *    destName is provided then the object will be renamed as well as
 *    moved.  By default a move across volumes will result in a copy
 *    and deletion of the original source.  The
 *    kFSFileOperationDoNotMoveAcrossVolumes flag will cause cross
 *    volume moves to do nothing and return an error.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    source:
 *      The source object to move.
 *    
 *    destDir:
 *      The destination directory for the move.
 *    
 *    destName:
 *      The name for the object in the destination directory.  Pass
 *      NULL to use the source object name.
 *    
 *    target:
 *      Upon successful completion a ref to the object in its new
 *      location.  If source is a directory then target will be the
 *      corresponding object in the destination directory.
 *    
 *    options:
 *      One or more FSFileOperation flags
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSMoveObjectSync( const (*var*) source: FSRef; const (*var*) destDir: FSRef; destName: CFStringRef; var target: FSRef; options: OptionBits ): OSStatus; external name '_FSMoveObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMoveObjectToTrashSync()
 *  
 *  Discussion:
 *    This routine will move the source object into the trash.  The
 *    source object can be a file or directory.  If the volume the
 *    source object resides on does not support a trash folder then
 *    this call will return an error (this is the same circumstance
 *    that triggers the delete immediately behavior in the Finder).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    source:
 *      The source object to move to the trash.
 *    
 *    target:
 *      Upon successful completion a ref the object in the trash.  If
 *      source is a directory then target will be the corresponding
 *      object in the destination directory.
 *    
 *    options:
 *      One or more FSFileOperation flags
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSMoveObjectToTrashSync( const (*var*) source: FSRef; var target: FSRef; options: OptionBits ): OSStatus; external name '_FSMoveObjectToTrashSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathCopyObjectSync()
 *  
 *  Discussion:
 *    This routine will copy the source object into the destination
 *    directory.  The source object can be a file or directory.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    sourcePath:
 *      The UTF-8 path string of the source object to copy.
 *    
 *    destDirPath:
 *      The UTF-8 path of the destination directory for the copy.
 *    
 *    destName:
 *      The name for the new object in the destination directory.  Pass
 *      NULL to use the source object name.
 *    
 *    targetPath:
 *      A pointer to a char * to allow returning the path to the newly
 *      created object.  The path is allocated using malloc and it is
 *      the caller's responsibility to free.  The pointer will be set
 *      to NULL if the copy failed.
 *    
 *    options:
 *      One or more FSFileOperation flags
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSPathCopyObjectSync( sourcePath: ConstCStringPtr; destDirPath: ConstCStringPtr; destName: CFStringRef; var targetPath: CStringPtr; options: OptionBits ): OSStatus; external name '_FSPathCopyObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathMoveObjectSync()
 *  
 *  Discussion:
 *    This routine will move the source object into the destination
 *    directory.  The source object can be a file or directory.  If a
 *    destName is provided then the object will be renamed as well as
 *    moved.  By default a move across volumes will result in a copy
 *    and deletion of the original source.  The
 *    kFSFileOperationDoNotMoveAcrossVolumes flag will cause cross
 *    volume moves to do nothing and return an error.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    sourcePath:
 *      The UTF-8 path string of the source object to move.
 *    
 *    destDirPath:
 *      The UTF-8 path of the destination directory for the move.
 *    
 *    destName:
 *      The name for the moved object in the destination directory. 
 *      Pass NULL to use the source object name.
 *    
 *    targetPath:
 *      A pointer to a char * to allow returning the path to the newly
 *      created object.  The path is allocated using malloc and it is
 *      the caller's responsibility to free.  The pointer will be set
 *      to NULL if the move failed.
 *    
 *    options:
 *      One or more FSFileOperation flags
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSPathMoveObjectSync( sourcePath: ConstCStringPtr; destDirPath: ConstCStringPtr; destName: CFStringRef; var targetPath: CStringPtr; options: OptionBits ): OSStatus; external name '_FSPathMoveObjectSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathMoveObjectToTrashSync()
 *  
 *  Discussion:
 *    This routine will move the source object into the trash.  The
 *    source object can be a file or directory.  If the volume the
 *    source object resides on does not support a trash folder then
 *    this call will return an error (this is the same circumstance
 *    that triggers the delete immediately behavior in the Finder).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    sourcePath:
 *      The UTF-8 path string of the source object to move.
 *    
 *    targetPath:
 *      A pointer to a char * to allow returning the path to the newly
 *      created object.  The path is allocated using malloc and it is
 *      the caller's responsibility to free.  The pointer will be set
 *      to NULL if the move failed.
 *    
 *    options:
 *      One or more FSFileOperation flags
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSPathMoveObjectToTrashSync( sourcePath: ConstCStringPtr; var targetPath: CStringPtr; options: OptionBits ): OSStatus; external name '_FSPathMoveObjectToTrashSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileOperationGetTypeID()
 *  
 *  Discussion:
 *    This routine will return the CFTypeID for the FSFileOpeation type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Result:
 *    the CFTypeID for the FSFileOperation type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileOperationGetTypeID: CFTypeID; external name '_FSFileOperationGetTypeID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileOperationCreate()
 *  
 *  Discussion:
 *    This routine will create an FSFileOperation for use with either
 *    FSCopyObjectAsync or FSMoveObjectAsync.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator to use.  Pass NULL for the default allocator.
 *  
 *  Result:
 *    A reference to the newly created object or NULL if the creation
 *    failed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileOperationCreate( alloc: CFAllocatorRef ): FSFileOperationRef; external name '_FSFileOperationCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileOperationScheduleWithRunLoop()
 *  
 *  Discussion:
 *    Schedule the given operation on the specified runloop and mode. A
 *    FSFileOperation can be scheduled on multiple runloop/mode
 *    combinations. An opertion must be scheduled on at least one
 *    runloop.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation to schedule.
 *    
 *    runLoop:
 *      The runLoop on which to schedule the operation.  Must be
 *      non-NULL.
 *    
 *    runLoopMode:
 *      The mode on which to schedule the operation.  Must be non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileOperationScheduleWithRunLoop( fileOp: FSFileOperationRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): OSStatus; external name '_FSFileOperationScheduleWithRunLoop';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileOperationUnscheduleFromRunLoop()
 *  
 *  Discussion:
 *    Unschedule the given operation from the specified runloop and
 *    mode.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation to unschedule.
 *    
 *    runLoop:
 *      The runLoop from which to unschedule the operation.  Must be
 *      non-NULL.
 *    
 *    runLoopMode:
 *      The mode from which to unschedule the operation.  Must be
 *      non-NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileOperationUnscheduleFromRunLoop( fileOp: FSFileOperationRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef ): OSStatus; external name '_FSFileOperationUnscheduleFromRunLoop';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSCopyObjectAsync()
 *  
 *  Discussion:
 *    This routine will start an asynchronous copy of the object
 *    specified by source to the directory specified by destDir.  If
 *    destName is provided then the new object will be renamed to
 *    destName.  If destName is not provided then the name of the
 *    source object will be used.  Status callbacks will occur on one
 *    of the runloop/mode combinations that the operation was scheduled
 *    on (and is running).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation object created for this operation.
 *    
 *    source:
 *      The source object to copy.
 *    
 *    destDir:
 *      The destination directory for the copy.
 *    
 *    destName:
 *      The name for the new object in the destination directory.  Pass
 *      NULL to use the source object name.
 *    
 *    flags:
 *      One or more FSFileOperation flags
 *    
 *    callback:
 *      An optional FSFileOperationStatusProcPtr which will be called
 *      with status updates as the copy proceeds.
 *    
 *    statusChangeInterval:
 *      The minimum time between callbacks within a single stage of an
 *      operation.
 *    
 *    clientContext:
 *      Client contextual information to associate with this operation.
 *       The info pointer will be passed to status callbacks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCopyObjectAsync( fileOp: FSFileOperationRef; const (*var*) source: FSRef; const (*var*) destDir: FSRef; destName: CFStringRef; flags: OptionBits; callback: FSFileOperationStatusProcPtr; statusChangeInterval: CFTimeInterval; var clientContext: FSFileOperationClientContext ): OSStatus; external name '_FSCopyObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMoveObjectAsync()
 *  
 *  Discussion:
 *    This routine will start an asynchronous move of the object
 *    specified by source to the directory specified by destDir.  If
 *    destName is provided then the new object will be renamed to
 *    destName.  If destName is not provided then the name of the
 *    source object will be used.  Status callbacks will occur on one
 *    of the runloop/mode combinations that the operation was scheduled
 *    on (and is running).  By default a move across volumes will
 *    result in a copy and deletion of the original source. The
 *    kFSFileOperationDoNotMoveAcrossVolumes flag will cause cross
 *    volume moves to do nothing and return an error.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation object created for this operation.
 *    
 *    source:
 *      The source object to move.
 *    
 *    destDir:
 *      The destination directory for the move.
 *    
 *    destName:
 *      The name for the object in the destination directory.  Pass
 *      NULL to leave the name unchanged.
 *    
 *    flags:
 *      One or more FSFileOperation flags
 *    
 *    callback:
 *      An optional FSFileOperationStatusProcPtr which will be called
 *      with status updates as the copy proceeds.
 *    
 *    statusChangeInterval:
 *      The minimum time between callbacks within a single stage of an
 *      operation.
 *    
 *    clientContext:
 *      Client contextual information to associate with this operation.
 *       The info pointer will be passed to status callbacks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSMoveObjectAsync( fileOp: FSFileOperationRef; const (*var*) source: FSRef; const (*var*) destDir: FSRef; destName: CFStringRef; flags: OptionBits; callback: FSFileOperationStatusProcPtr; statusChangeInterval: CFTimeInterval; var clientContext: FSFileOperationClientContext ): OSStatus; external name '_FSMoveObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSMoveObjectToTrashAsync()
 *  
 *  Discussion:
 *    This routine will start an asynchronous move of the object
 *    specified by source to the trash.  If the volume the source
 *    object resides on does not support a trash folder then the
 *    operation will return an error (this is the same circumstance
 *    that triggers the delete immediately behavior in the Finder).
 *    Status callbacks will occur on one of the runloop/mode
 *    combinations that the operation was scheduled on (and is
 *    running).   Upon successul complettion of the operation the last
 *    currentItem (from either the last status callback or retrieved by
 *    FSFileOperationCopyStatus) will be object in the trash.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation object created for this operation.
 *    
 *    source:
 *      The source object to move.
 *    
 *    flags:
 *      One or more FSFileOperation flags
 *    
 *    callback:
 *      An optional FSFileOperationStatusProcPtr which will be called
 *      with status updates as the move proceeds.
 *    
 *    statusChangeInterval:
 *      The minimum time between callbacks within a single stage of an
 *      operation.
 *    
 *    clientContext:
 *      Client contextual information to associate with this operation.
 *       The info pointer will be passed to status callbacks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSMoveObjectToTrashAsync( fileOp: FSFileOperationRef; const (*var*) source: FSRef; flags: OptionBits; callback: FSFileOperationStatusProcPtr; statusChangeInterval: CFTimeInterval; var clientContext: FSFileOperationClientContext ): OSStatus; external name '_FSMoveObjectToTrashAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathCopyObjectAsync()
 *  
 *  Discussion:
 *    This routine will start an asynchronous copy of the object
 *    specified by source to the directory specified by destDir.  If
 *    destName is provided then the new object will be renamed to
 *    destName.  If destName is not provided then the name of the
 *    source object will be used.  Status callbacks will occur on one
 *    of the runloop/mode combinations that the operation was scheduled
 *    on (and is running).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation object created for this operation.
 *    
 *    sourcePath:
 *      The UTF-8 path string of the source object to copy.
 *    
 *    destDirPath:
 *      The UTF-8 path of the destination directory for the copy.
 *    
 *    destName:
 *      The name for the new object in the destination directory.  Pass
 *      NULL to use the source object name.
 *    
 *    flags:
 *      One or more FSFileOperation flags
 *    
 *    callback:
 *      An optional FSPathFileOperationStatusProcPtr which will be
 *      called with status updates as the copy proceeds.
 *    
 *    statusChangeInterval:
 *      The minimum time between callbacks within a single stage of an
 *      operation.
 *    
 *    clientContext:
 *      Client contextual information to associate with this operation.
 *       The info pointer will be passed to status callbacks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSPathCopyObjectAsync( fileOp: FSFileOperationRef; sourcePath: ConstCStringPtr; destDirPath: ConstCStringPtr; destName: CFStringRef; flags: OptionBits; callback: FSPathFileOperationStatusProcPtr; statusChangeInterval: CFTimeInterval; var clientContext: FSFileOperationClientContext ): OSStatus; external name '_FSPathCopyObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathMoveObjectAsync()
 *  
 *  Discussion:
 *    This routine will start an asynchronous move of the object
 *    specified by source to the directory specified by destDir.  If
 *    destName is provided then the new object will be renamed to
 *    destName.  If destName is not provided then the name of the
 *    source object will be used.  Status callbacks will occur on one
 *    of the runloop/mode combinations that the operation was scheduled
 *    on (and is running).  By default a move across volumes will
 *    result in a copy and deletion of the original source. The
 *    kFSFileOperationDoNotMoveAcrossVolumes flag will cause cross
 *    volume moves to do nothing and return an error.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation object created for this operation.
 *    
 *    sourcePath:
 *      The UTF-8 path string of the source object to move.
 *    
 *    destDirPath:
 *      The UTF-8 path of the destination directory for the move.
 *    
 *    destName:
 *      The name for the object in the destination directory.  Pass
 *      NULL to leave the name unchanged.
 *    
 *    flags:
 *      One or more FSFileOperation flags
 *    
 *    callback:
 *      An optional FSPathFileOperationStatusProcPtr which will be
 *      called with status updates as the move proceeds.
 *    
 *    statusChangeInterval:
 *      The minimum time between callbacks within a single stage of an
 *      operation.
 *    
 *    clientContext:
 *      Client contextual information to associate with this operation.
 *       The info pointer will be passed to status callbacks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSPathMoveObjectAsync( fileOp: FSFileOperationRef; sourcePath: ConstCStringPtr; destDirPath: ConstCStringPtr; destName: CFStringRef; flags: OptionBits; callback: FSPathFileOperationStatusProcPtr; statusChangeInterval: CFTimeInterval; var clientContext: FSFileOperationClientContext ): OSStatus; external name '_FSPathMoveObjectAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathMoveObjectToTrashAsync()
 *  
 *  Discussion:
 *    This routine will start an asynchronous move of the object
 *    specified by source to the trash.  If the volume the source
 *    object resides on does not support a trash folder then this call
 *    will return an error (this is the same circumstance that triggers
 *    the delete immediately behavior in the Finder). Status callbacks
 *    will occur on one of the runloop/mode combinations that the
 *    operation was scheduled on (and is running).  Note that the
 *    object may be renamed when placed in the trash.  Upon successful
 *    completion of the operation the last currentItem (from either the
 *    last status callback or retrieved by
 *    FSPathFileOperationCopyStatus) will be object in the trash.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation object created for this operation.
 *    
 *    sourcePath:
 *      The UTF-8 path string of the source object to move.
 *    
 *    flags:
 *      One or more FSFileOperation flags
 *    
 *    callback:
 *      An optional FSPathFileOperationStatusProcPtr which will be
 *      called with status updates as the move proceeds.
 *    
 *    statusChangeInterval:
 *      The minimum time between callbacks within a single stage of an
 *      operation.
 *    
 *    clientContext:
 *      Client contextual information to associate with this operation.
 *       The info pointer will be passed to status callbacks.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 }
function FSPathMoveObjectToTrashAsync( fileOp: FSFileOperationRef; sourcePath: ConstCStringPtr; flags: OptionBits; callback: FSPathFileOperationStatusProcPtr; statusChangeInterval: CFTimeInterval; var clientContext: FSFileOperationClientContext ): OSStatus; external name '_FSPathMoveObjectToTrashAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileOperationCancel()
 *  
 *  Discussion:
 *    Cancels the specified FSFileOperation. This makes the operation
 *    ineligible to run on any runloop.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation to cancel.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileOperationCancel( fileOp: FSFileOperationRef ): OSStatus; external name '_FSFileOperationCancel';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileOperationCopyStatus()
 *  
 *  Discussion:
 *    This routine returns the current status of an FSFileOperation. 
 *    The status dictionary must be released by the caller.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation from which to retrieve status information.
 *    
 *    currentItem:
 *      FSRef to item operation is currently processing.  If the
 *      operation is complete then currentItem refers to the target
 *      item (the new item corresponding to the source item in the
 *      destination directory).
 *    
 *    stage:
 *      current stage of the operation.
 *    
 *    error:
 *      Either noErr or an error value which caused the operation to
 *      fail.
 *    
 *    statusDictionary:
 *      A CFDictionary with more detailed status information. The
 *      caller is responsible for releasing the object when done with
 *      it.
 *    
 *    info:
 *      The info pointer passed in by the client.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileOperationCopyStatus( fileOp: FSFileOperationRef; var currentItem: FSRef; var stage: FSFileOperationStage; var error: OSStatus; var statusDictionary: CFDictionaryRef; info: UnivPtrPtr ): OSStatus; external name '_FSFileOperationCopyStatus';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSPathFileOperationCopyStatus()
 *  
 *  Discussion:
 *    This routine returns the current status of an FSFileOperation. 
 *    The status dictionary must be released by the caller.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileOp:
 *      The FSFileOperation from which to retrieve status information.
 *    
 *    currentItem:
 *      A path to the item operation is currently processing.  The
 *      caller is responsible for calling free to dispose of the path
 *      string.  If the operation is complete then currentItem refers
 *      to the target item (the new item corresponding to the source
 *      item in the destination directory).
 *    
 *    stage:
 *      current stage of the operation.
 *    
 *    error:
 *      Either noErr or an error value which caused the operation to
 *      fail.
 *    
 *    statusDictionary:
 *      A CFDictionary with more detailed status information.  The
 *      caller is responsible for releasing the object when done with
 *      it.
 *    
 *    info:
 *      The info pointer passed in by the client.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSPathFileOperationCopyStatus( fileOp: FSFileOperationRef; var currentItem: CStringPtr; var stage: FSFileOperationStage; var error: OSStatus; var statusDictionary: CFDictionaryRef; info: UnivPtrPtr ): OSStatus; external name '_FSPathFileOperationCopyStatus';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ String conversion functions}
{
 *  FSCreateStringFromHFSUniStr()
 *  
 *  Discussion:
 *    Creates a CFString from a HFSUniStr255.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator to use.  Pass NULL for the default allocator.
 *    
 *    uniStr:
 *      A HFSUniStr255 to use as the source value for the CFString.
 *  
 *  Result:
 *    A CFStringRef created from the HFSUniStr255 or NULL.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSCreateStringFromHFSUniStr( alloc: CFAllocatorRef; const (*var*) uniStr: HFSUniStr255 ): CFStringRef; external name '_FSCreateStringFromHFSUniStr';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetHFSUniStrFromString()
 *  
 *  Discussion:
 *    Convert a CFString into a HFSUniStr255.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    theString:
 *      The CFString to convert to a HFSUniStr255.
 *    
 *    uniStr:
 *      A pointer to a HFSUniStr255 which will be filled in using the
 *      value of theString.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetHFSUniStrFromString( theString: CFStringRef; var uniStr: {out} HFSUniStr255 ): OSStatus; external name '_FSGetHFSUniStrFromString';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ ACL support}

{
 *  FSFileSecurityGetTypeID()
 *  
 *  Discussion:
 *    This routine will return the CFTypeID for the FSFileSecurity type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Result:
 *    the CFTypeID for the FSFilSecurity type.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityGetTypeID: CFTypeID; external name '_FSFileSecurityGetTypeID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityCreate()
 *  
 *  Discussion:
 *    This routine will create an FSFileSecurity object.  The object
 *    should be released using CFRelease when it is no longer needed.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    alloc:
 *      The CFAllocator to use.  Pass NULL for the default allocator.
 *  
 *  Result:
 *    A reference to the newly created object or NULL if the creation
 *    failed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityCreate( alloc: CFAllocatorRef ): FSFileSecurityRef; external name '_FSFileSecurityCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityCreateWithFSPermissionInfo()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityCreateWithFSPermissionInfo( alloc: CFAllocatorRef; const (*var*) permissions: FSPermissionInfo ): FSFileSecurityRef; external name '_FSFileSecurityCreateWithFSPermissionInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityRefCreateCopy()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityRefCreateCopy( alloc: CFAllocatorRef; fileSec: FSFileSecurityRef ): FSFileSecurityRef; external name '_FSFileSecurityRefCreateCopy';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ get and set properties}

{
 *  FSFileSecurityGetOwnerUUID()
 *  
 *  Discussion:
 *    This routine will get the owner UUID associated with the passed
 *    in FSFileSecurityRef.  In there is no owner UUID property
 *    associated with the FSFileSecurity object then
 *    errFSPropertyNotValid will be returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to get the owner UUID
 *      from.
 *    
 *    owner:
 *      A pointer to storage for the owner UUID associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityGetOwnerUUID( fileSec: FSFileSecurityRef; var owner: CFUUIDBytes ): OSStatus; external name '_FSFileSecurityGetOwnerUUID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecuritySetOwnerUUID()
 *  
 *  Discussion:
 *    This routine will set the owner UUID associated with the passed
 *    in FSFileSecurityRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the owner UUID
 *      for.
 *    
 *    owner:
 *      The UUID to set as the owner UUID associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecuritySetOwnerUUID( fileSec: FSFileSecurityRef; const (*var*) owner: CFUUIDBytes ): OSStatus; external name '_FSFileSecuritySetOwnerUUID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityGetGroupUUID()
 *  
 *  Discussion:
 *    This routine will get the group UUID associated with the passed
 *    in FSFileSecurityRef.  In there is no group UUID property
 *    associated with the FSFileSecurity object then
 *    errFSPropertyNotValid will be returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to get the group UUID
 *      from.
 *    
 *    group:
 *      A pointer to storage for the owner UUID associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityGetGroupUUID( fileSec: FSFileSecurityRef; var group: CFUUIDBytes ): OSStatus; external name '_FSFileSecurityGetGroupUUID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecuritySetGroupUUID()
 *  
 *  Discussion:
 *    This routine will set the group UUID associated with the passed
 *    in FSFileSecurityRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the group UUID
 *      for.
 *    
 *    group:
 *      The UUID to set as the group UUID associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecuritySetGroupUUID( fileSec: FSFileSecurityRef; const (*var*) group: CFUUIDBytes ): OSStatus; external name '_FSFileSecuritySetGroupUUID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityCopyAccessControlList()
 *  
 *  Discussion:
 *    This routine will copy the acl_t associated with the passed in
 *    FSFileSecurityRef.  The accessControlList returned by this
 *    routine is a copy and must be released using acl_free.  The
 *    accessControlList is an acl_t and is meant to be manipulated
 *    using the acl calls defined in <sys/acl.h>.   If there is no acl
 *    property associated with the FSFileSecurity object then
 *    errFSPropertyNotValid will be returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the group for.
 *    
 *    accessControlList:
 *      A pointer to storage for the acl_t associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityCopyAccessControlList( fileSec: FSFileSecurityRef; var accessControlList: acl_t ): OSStatus; external name '_FSFileSecurityCopyAccessControlList';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecuritySetAccessControlList()
 *  
 *  Discussion:
 *    This routine will set the acl associated with the passed in
 *    FSFileSecurityRef. To request removal of an ACL from a filesystem
 *    object pass in kFSFileSecurityRemoveACL as the accessControlList
 *    and set the fileSec on the target object using FSSetCatalogInfo. 
 *    Setting the accessControlList to NULL will result in the property
 *    being unset.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the group for.
 *    
 *    accessControlList:
 *      The acl_t to set as the acl associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecuritySetAccessControlList( fileSec: FSFileSecurityRef; accessControlList: acl_t ): OSStatus; external name '_FSFileSecuritySetAccessControlList';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityGetOwner()
 *  
 *  Discussion:
 *    This routine will get the owner uid associated with the passed in
 *    FSFileSecurityRef.  In there is no owner property associated with
 *    the FSFileSecurity object then errFSPropertyNotValid will be
 *    returned.  Note that this value is not the owner UUID which is
 *    returned by FSFileSecurityGetOwnerUUID.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to get the owner from.
 *    
 *    owner:
 *      A pointer to storage for the owner uid associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityGetOwner( fileSec: FSFileSecurityRef; var owner: UInt32 ): OSStatus; external name '_FSFileSecurityGetOwner';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecuritySetOwner()
 *  
 *  Discussion:
 *    This routine will set the owner uid associated with the passed in
 *    FSFileSecurityRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the owner for.
 *    
 *    owner:
 *      The uid to set as the owner associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecuritySetOwner( fileSec: FSFileSecurityRef; owner: UInt32 ): OSStatus; external name '_FSFileSecuritySetOwner';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityGetGroup()
 *  
 *  Discussion:
 *    This routine will get the group gid associated with the passed in
 *    FSFileSecurityRef.  In there is no group property associated with
 *    the FSFileSecurity object then errFSPropertyNotValid will be
 *    returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to get the owner from.
 *    
 *    group:
 *      A pointer to storage for the group gid associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityGetGroup( fileSec: FSFileSecurityRef; var group: UInt32 ): OSStatus; external name '_FSFileSecurityGetGroup';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecuritySetGroup()
 *  
 *  Discussion:
 *    This routine will set the group gid associated with the passed in
 *    FSFileSecurityRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the group for.
 *    
 *    group:
 *      The gid to set as the group associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecuritySetGroup( fileSec: FSFileSecurityRef; group: UInt32 ): OSStatus; external name '_FSFileSecuritySetGroup';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecurityGetMode()
 *  
 *  Discussion:
 *    This routine will get the mode associated with the passed in
 *    FSFileSecurityRef.  In there is no mode property associated with
 *    the FSFileSecurity object then errFSPropertyNotValid will be
 *    returned.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the group for.
 *    
 *    mode:
 *      A pointer to storage for the mode associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecurityGetMode( fileSec: FSFileSecurityRef; var mode: UInt16 ): OSStatus; external name '_FSFileSecurityGetMode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSFileSecuritySetMode()
 *  
 *  Discussion:
 *    This routine will set the mode associated with the passed in
 *    FSFileSecurityRef.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.4
 *  
 *  Parameters:
 *    
 *    fileSec:
 *      A reference to the FSFileSecurity object to set the group for.
 *    
 *    mode:
 *      The mode to set as the mode associated with fileSec.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.4 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFileSecuritySetMode( fileSec: FSFileSecurityRef; mode: UInt16 ): OSStatus; external name '_FSFileSecuritySetMode';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_4, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


const
{ old names for ioPosMode flags }
	pleaseCacheBit = 4;    { old name of kFSPleaseCacheBit }
	pleaseCacheMask = $0010;
	noCacheBit = 5;    { old name of kFSNoCacheBit  }
	noCacheMask = $0020;
	rdVerifyBit = 6;    { old name of kFSRdVerifyBit }
	rdVerifyMask = $0040;
	rdVerify = 64;   { old name of rdVerifyMask }
	forceReadBit = 6;    { old name of kFSForceReadBit }
	forceReadMask = $0040;
	newLineBit = 7;    { old name of kFSNewLineBit }
	newLineMask = $0080;
	newLineCharMask = $FF00; { old name of kFSNewLineCharMask }


{ mapping codes (ioObjType) for MapName & MapID }
const
	kOwnerID2Name = 1;
	kGroupID2Name = 2;
	kOwnerName2ID = 3;
	kGroupName2ID = 4;    { types of oj object to be returned (ioObjType) for _GetUGEntry }
	kReturnNextUser = 1;
	kReturnNextGroup = 2;
	kReturnNextUG = 3;

{ vcbFlags bits }
const
	kVCBFlagsIdleFlushBit = 3;    { Set if volume should be flushed at idle time }
	kVCBFlagsIdleFlushMask = $0008;
	kVCBFlagsHFSPlusAPIsBit = 4;    { Set if volume implements HFS Plus APIs itself (not via emulation) }
	kVCBFlagsHFSPlusAPIsMask = $0010;
	kVCBFlagsHardwareGoneBit = 5;    { Set if disk driver returned a hardwareGoneErr to Read or Write }
	kVCBFlagsHardwareGoneMask = $0020;
	kVCBFlagsVolumeDirtyBit = 15;   { Set if volume information has changed since the last FlushVol }
	kVCBFlagsVolumeDirtyMask = $8000;

{ ioVAtrb bits returned by PBHGetVInfo and PBXGetVolInfo }
const
	kioVAtrbDefaultVolumeBit = 5;    { Set if the volume is the default volume }
	kioVAtrbDefaultVolumeMask = $0020;
	kioVAtrbFilesOpenBit = 6;    { Set if there are open files or iterators }
	kioVAtrbFilesOpenMask = $0040;
	kioVAtrbHardwareLockedBit = 7;    { Set if volume is locked by a hardware setting }
	kioVAtrbHardwareLockedMask = $0080;
	kioVAtrbSoftwareLockedBit = 15;   { Set if volume is locked by software }
	kioVAtrbSoftwareLockedMask = $8000;

{ ioFlAttrib bits returned by PBGetCatInfo }
const
{ file and directory attributes in ioFlAttrib }
	kioFlAttribLockedBit = 0;    { Set if file or directory is locked }
	kioFlAttribLockedMask = $01;
	kioFlAttribResOpenBit = 2;    { Set if resource fork is open }
	kioFlAttribResOpenMask = $04;
	kioFlAttribDataOpenBit = 3;    { Set if data fork is open }
	kioFlAttribDataOpenMask = $08;
	kioFlAttribDirBit = 4;    { Set if this is a directory }
	kioFlAttribDirMask = $10;
	ioDirFlg = 4;    { Set if this is a directory (old name) }
	ioDirMask = $10;
	kioFlAttribCopyProtBit = 6;    { Set if AppleShare server "copy-protects" the file }
	kioFlAttribCopyProtMask = $40;
	kioFlAttribFileOpenBit = 7;    { Set if file (either fork) is open }
	kioFlAttribFileOpenMask = $80; { ioFlAttrib for directories only }
	kioFlAttribInSharedBit = 2;    { Set if the directory is within a shared area of the directory hierarchy }
	kioFlAttribInSharedMask = $04;
	kioFlAttribMountedBit = 3;    { Set if the directory is a share point that is mounted by some user }
	kioFlAttribMountedMask = $08;
	kioFlAttribSharePointBit = 5;    { Set if the directory is a share point }
	kioFlAttribSharePointMask = $20;

{ ioFCBFlags bits returned by PBGetFCBInfo }
{ IMPORTANT: These ioFCBFlags bit constants are for the SInt16 FCBPBRec.ioFCBFlags field returned }
{ by PBGetFCBInfoSync and PBGetFCBInfoAsync. Do not use them with the FSForkInfo.flags }
{ field returned by the FSGetForkCBInfo, PBGetForkCBInfoSync and PBGetForkCBInfoAsyn functions. }
const
	kioFCBWriteBit = 8;    { Data can be written to this file }
	kioFCBWriteMask = $0100;
	kioFCBResourceBit = 9;    { This file is a resource fork }
	kioFCBResourceMask = $0200;
	kioFCBWriteLockedBit = 10;   { File has a locked byte range }
	kioFCBWriteLockedMask = $0400;
	kioFCBLargeFileBit = 11;   { File may grow beyond 2GB; cache uses file blocks, not bytes }
	kioFCBLargeFileMask = $0800;
	kioFCBSharedWriteBit = 12;   { File is open for shared write access }
	kioFCBSharedWriteMask = $1000;
	kioFCBFileLockedBit = 13;   { File is locked (write-protected) }
	kioFCBFileLockedMask = $2000;
	kioFCBOwnClumpBit = 14;   { File has clump size specified in FCB }
	kioFCBOwnClumpMask = $4000;
	kioFCBModifiedBit = 15;   { File has changed since it was last flushed }
	kioFCBModifiedMask = $8000;

{ IMPORTANT: These FSForkInfoFlags constants are for use with the FSForkInfo.flags }
{ field returned by the FSGetForkCBInfo, PBGetForkCBInfoSync and PBGetForkCBInfoAsyn functions. }
{ Do not use them with the FCBPBRec.ioFCBFlags field returned by PBGetFCBInfoSync and PBGetFCBInfoAsync. }
const
	kForkInfoFlagsWriteBit = kioFCBWriteBit - 8; { Data can be written to this file }
	kForkInfoFlagsWriteMask = 1 shl kForkInfoFlagsWriteBit;
	kForkInfoFlagsResourceBit = kioFCBResourceBit - 8; { This file is a resource fork }
	kForkInfoFlagsResourceMask = 1 shl kForkInfoFlagsResourceBit;
	kForkInfoFlagsWriteLockedBit = kioFCBWriteLockedBit - 8; { File has a locked byte range }
	kForkInfoFlagsWriteLockedMask = 1 shl kForkInfoFlagsWriteLockedBit;
	kForkInfoFlagsLargeFileBit = kioFCBLargeFileBit - 8; { File may grow beyond 2GB; cache uses file blocks, not bytes }
	kForkInfoFlagsLargeFileMask = 1 shl kForkInfoFlagsLargeFileBit;
	kForkInfoFlagsSharedWriteBit = kioFCBSharedWriteBit - 8; { File is open for shared write access }
	kForkInfoFlagsSharedWriteMask = 1 shl kForkInfoFlagsSharedWriteBit;
	kForkInfoFlagsFileLockedBit = kioFCBFileLockedBit - 8; { File is locked (write-protected) }
	kForkInfoFlagsFileLockedMask = 1 shl kForkInfoFlagsFileLockedBit;
	kForkInfoFlagsOwnClumpBit = kioFCBOwnClumpBit - 8; { File has clump size specified in FCB }
	kForkInfoFlagsOwnClumpMask = 1 shl kForkInfoFlagsOwnClumpBit;
	kForkInfoFlagsModifiedBit = kioFCBModifiedBit - 8; { File has changed since it was last flushed }
	kForkInfoFlagsModifiedMask = 1 shl kForkInfoFlagsModifiedBit;

{ ioACUser bits returned by PBGetCatInfo }
{ Note: you must clear ioACUser before calling PBGetCatInfo because some file systems do not use this field }
const
	kioACUserNoSeeFolderBit = 0;    { Set if user does not have See Folder privileges }
	kioACUserNoSeeFolderMask = $01;
	kioACUserNoSeeFilesBit = 1;    { Set if user does not have See Files privileges }
	kioACUserNoSeeFilesMask = $02;
	kioACUserNoMakeChangesBit = 2;    { Set if user does not have Make Changes privileges }
	kioACUserNoMakeChangesMask = $04;
	kioACUserNotOwnerBit = 7;    { Set if user is not owner of the directory }
	kioACUserNotOwnerMask = $80;

{ Folder and File values of access privileges in ioACAccess }
const
	kioACAccessOwnerBit = 31;   { User is owner of directory }
	kioACAccessOwnerMask = -2147483648; { SInt32($80000000) }
	kioACAccessBlankAccessBit = 28;   { Directory has blank access privileges }
	kioACAccessBlankAccessMask = $10000000;
	kioACAccessUserWriteBit = 26;   { User has write privileges }
	kioACAccessUserWriteMask = $04000000;
	kioACAccessUserReadBit = 25;   { User has read privileges }
	kioACAccessUserReadMask = $02000000;
	kioACAccessUserSearchBit = 24;   { User has search privileges }
	kioACAccessUserSearchMask = $01000000;
	kioACAccessEveryoneWriteBit = 18;   { Everyone has write privileges }
	kioACAccessEveryoneWriteMask = $00040000;
	kioACAccessEveryoneReadBit = 17;   { Everyone has read privileges }
	kioACAccessEveryoneReadMask = $00020000;
	kioACAccessEveryoneSearchBit = 16;   { Everyone has search privileges }
	kioACAccessEveryoneSearchMask = $00010000;
	kioACAccessGroupWriteBit = 10;   { Group has write privileges }
	kioACAccessGroupWriteMask = $00000400;
	kioACAccessGroupReadBit = 9;    { Group has read privileges }
	kioACAccessGroupReadMask = $00000200;
	kioACAccessGroupSearchBit = 8;    { Group has search privileges }
	kioACAccessGroupSearchMask = $00000100;
	kioACAccessOwnerWriteBit = 2;    { Owner has write privileges }
	kioACAccessOwnerWriteMask = $00000004;
	kioACAccessOwnerReadBit = 1;    { Owner has read privileges }
	kioACAccessOwnerReadMask = $00000002;
	kioACAccessOwnerSearchBit = 0;    { Owner has search privileges }
	kioACAccessOwnerSearchMask = $00000001;
	kfullPrivileges = $00070007; { all privileges for everybody and owner}
	kownerPrivileges = $00000007; { all privileges for owner only}


{  Volume Characteristics }
{
 *  FSGetVolumeParms()
 *  
 *  Discussion:
 *    Returns information about the specified volume in the passed in
 *    GetVolParmsInfoBuffer.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    volume:
 *      The volume to get the information about.
 *    
 *    buffer:
 *      A GetVolParmsInfoBuffer to fill out
 *    
 *    bufferSize:
 *      the size of the passed in buffer
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetVolumeParms( volume: FSVolumeRefNum; var buffer: GetVolParmsInfoBuffer; bufferSize: ByteCount ): OSStatus; external name '_FSGetVolumeParms';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{  VolumeMount }
{
 *  FSGetVolumeMountInfoSize()
 *  
 *  Discussion:
 *    Returns the size of the MountInfo block associated with the
 *    specified volume.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    volume:
 *      The volume to get the MountInfo size for.
 *    
 *    size:
 *      The size of the buffer required to store the MountInfo data
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetVolumeMountInfoSize( volume: FSVolumeRefNum; var size: ByteCount ): OSStatus; external name '_FSGetVolumeMountInfoSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSGetVolumeMountInfo()
 *  
 *  Discussion:
 *    Returns the MountInfo data for the specified volume.  If the
 *    buffer passed in is too small then errFSBadBuffer is returned and
 *    actualSize will contain the required buffer size to hold the
 *    MountInfo data.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    volume:
 *      The volume to get the MountInfo for.
 *    
 *    buffer:
 *      A pointer to the buffer to copy the MountInfo data into
 *    
 *    bufferSize:
 *      The size of the buffer passed in
 *    
 *    actualSize:
 *      The actual size of the MountInfo data
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSGetVolumeMountInfo( volume: FSVolumeRefNum; buffer: BytePtr; bufferSize: ByteCount; var actualSize: ByteCount ): OSStatus; external name '_FSGetVolumeMountInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FSVolumeMount()
 *  
 *  Discussion:
 *    Will attempt to remount a volume using the passed in MountInfo
 *    buffer
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    buffer:
 *      A pointer to the buffer with the MountInfo data.
 *    
 *    mountedVolume:
 *      the FSVolumeRefNum of the volume that was mounted
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSVolumeMount( buffer: BytePtr; var mountedVolume: FSVolumeRefNum ): OSStatus; external name '_FSVolumeMount';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{  Volume flushing }
{
 *  FSFlushVolume()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSFlushVolume( vRefNum: FSVolumeRefNum ): OSStatus; external name '_FSFlushVolume';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBFlushVolumeSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function PBFlushVolumeSync( paramBlock: FSRefParamPtr ): OSStatus; external name '_PBFlushVolumeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBFlushVolumeAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function PBFlushVolumeAsync( paramBlock: FSRefParamPtr ): OSStatus; external name '_PBFlushVolumeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ CopyFile }
{
    PBFSCopyFile
    Copies a file on a volume that supports the CopyFile system call (which can be
    determined from the bHasCopyFile GetVolParms bit).
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ref             The source item to copy
    ->  parentRef       The destination to copy to
    ->  nameLength      Number of Unicode characters in the optional new name.  Pass in 0 to use the source name
    ->  name            A pointer to the optional new Unicode name; Pass in NULL to use the source name
    <-  newRef          A pointer to the FSRef for the new object; may be NULL
}
{
 *  PBFSCopyFileSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function PBFSCopyFileSync( paramBlock: FSRefParamPtr ): OSStatus; external name '_PBFSCopyFileSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBFSCopyFileAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function PBFSCopyFileAsync( paramBlock: FSRefParamPtr ): OSStatus; external name '_PBFSCopyFileAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ Node ID Resolution }
{
 *  FSResolveNodeID()
 *  
 *  Discussion:
 *    Returns an FSRef to the item on volume with the specified node id.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Parameters:
 *    
 *    volume:
 *      The FSVolumeRefNum of the volume the item is on
 *    
 *    nodeID:
 *      The node id to resolve
 *    
 *    newRef:
 *      A pointer to storage for the FSRef of the item with the
 *      corresponding node id
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function FSResolveNodeID( volume: FSVolumeRefNum; nodeID: UInt32; newRef: FSRefPtr ): OSStatus; external name '_FSResolveNodeID';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
    ->  ioCompletion    A pointer to a completion routine
    <-  ioResult        The result code of the function
    ->  ioVRefNum       The FSVolumeRefNum of the volume the item is on
    ->  ioDirID         The node id to resolve (can be file as well as a directory node id)
    <-  newRef          A pointer to storage for the FSRef of the item with the corresponding node id
}
{
 *  PBFSResolveNodeIDSync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function PBFSResolveNodeIDSync( paramBlock: FSRefParamPtr ): OSStatus; external name '_PBFSResolveNodeIDSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBFSResolveNodeIDAsync()
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.5
 *  
 *  Availability:
 *    Mac OS X:         in version 10.5 and later in CoreServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
function PBFSResolveNodeIDAsync( paramBlock: FSRefParamPtr ): OSStatus; external name '_PBFSResolveNodeIDAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_5, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{ Calls beyond this point are deprecated}

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
   This function is deprecated in Mac OS X 10.5. Use FSMakeFSRefUnicode instead.
}
{$ifc not TARGET_CPU_64}
{
 *  FSpMakeFSRef()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function FSpMakeFSRef( const (*var*) source: FSSpec; var newRef: FSRef ): OSErr; external name '_FSpMakeFSRef';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBMakeFSRefUnicodeSync instead.}
{
 *  PBMakeFSRefSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBMakeFSRefSync( var paramBlock: FSRefParam ): OSErr; external name '_PBMakeFSRefSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBMakeFSRefUnicodeAsync instead.}
{
 *  PBMakeFSRefAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
procedure PBMakeFSRefAsync( var paramBlock: FSRefParam ); external name '_PBMakeFSRefAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBCloseForkSync instead.}
{
 *  PBCloseSync()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCloseSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBCloseSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBCloseForkAsync instead.}
{
 *  PBCloseAsync()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCloseAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBCloseAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBReadForkSync instead.}
{
 *  PBReadSync()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBReadSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBReadSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBReadForkAsync instead.}
{
 *  PBReadAsync()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBReadAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBReadAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBWriteForkSync instead.}
{
 *  PBWriteSync()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBWriteSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBWriteSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBWriteForkAsync instead.}
{
 *  PBWriteAsync()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBWriteAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBWriteAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. It was never implemented on OS X.}
{
 *  PBWaitIOComplete()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 }
function PBWaitIOComplete( paramBlock: ParmBlkPtr; timeout: Duration ): OSErr; external name '_PBWaitIOComplete';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{  Volume Characteristics }
{ This function is deprecated in Mac OS X 10.5. Use FSGetVolumeParms instead.}
{
 *  PBHGetVolParmsSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetVolParmsSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetVolParmsSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetVolumeParms instead.}
{
 *  PBHGetVolParmsAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetVolParmsAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetVolParmsAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{  VolumeMount }
{ This function is deprecated in Mac OS X 10.5. Use FSGetVolumeMountInfoSize instead.}
{
 *  PBGetVolMountInfoSize()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetVolMountInfoSize( paramBlock: ParmBlkPtr ): OSErr; external name '_PBGetVolMountInfoSize';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetVolumeMountInfo instead.}
{
 *  PBGetVolMountInfo()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetVolMountInfo( paramBlock: ParmBlkPtr ): OSErr; external name '_PBGetVolMountInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSVolumeMount instead.}
{
 *  PBVolumeMount()   *** DEPRECATED ***
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBVolumeMount( paramBlock: ParmBlkPtr ): OSErr; external name '_PBVolumeMount';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{  Volume flushing }
{ This function is deprecated in Mac OS X 10.5. Use FSFlushVolume instead.}
{
 *  FlushVol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FlushVol( volName: ConstStr63Param; vRefNum: FSVolumeRefNum ): OSErr; external name '_FlushVol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBFlushVolumeSync instead.}
{
 *  PBFlushVolSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBFlushVolSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBFlushVolSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBFlushVolumeAsync instead.}
{
 *  PBFlushVolAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBFlushVolAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBFlushVolAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBOpenForkSync with deny pos modes instead.}
{
 *  PBHOpenDenySync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenDenySync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenDenySync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBOpenForkAsync with deny pos modes instead.}
{
 *  PBHOpenDenyAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenDenyAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenDenyAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBOpenForkSync with deny pos modes instead.}
{
 *  PBHOpenRFDenySync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenRFDenySync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenRFDenySync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBOpenForkAsync with deny pos modes instead.}
{
 *  PBHOpenRFDenyAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenRFDenyAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenRFDenyAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetCatalogInfo instead.}
{
 *  PBHGetDirAccessSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetDirAccessSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetDirAccessSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetCatalogInfo instead.}
{
 *  PBHGetDirAccessAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetDirAccessAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetDirAccessAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSSetCatalogInfo instead.}
{
 *  PBHSetDirAccessSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetDirAccessSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHSetDirAccessSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSSetCatalogInfo instead.}
{
 *  PBHSetDirAccessAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetDirAccessAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHSetDirAccessAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. See getpwnam() and getpwuid().}
{
 *  PBHMapIDSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHMapIDSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHMapIDSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. See getpwnam() and getpwuid().}
{
 *  PBHMapIDAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHMapIDAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHMapIDAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. See getpwnam() and getpwuid().}
{
 *  PBHMapNameSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHMapNameSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHMapNameSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. See getpwnam() and getpwuid().}
{
 *  PBHMapNameAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHMapNameAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHMapNameAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBFSCopyFileSync instead.}
{
 *  PBHCopyFileSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHCopyFileSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHCopyFileSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use PBFSCopyFileAsync instead.}
{
 *  PBHCopyFileAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHCopyFileAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHCopyFileAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetCatalogInfo to get the node id.}
{
 *  PBCreateFileIDRefSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCreateFileIDRefSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBCreateFileIDRefSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetCatalogInfo to get the node id.}
{
 *  PBCreateFileIDRefAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCreateFileIDRefAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBCreateFileIDRefAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetCatalogInfo to get the node id.}
{
 *  PBResolveFileIDRefSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBResolveFileIDRefSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBResolveFileIDRefSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. Use FSGetCatalogInfo to get the node id.}
{
 *  PBResolveFileIDRefAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBResolveFileIDRefAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBResolveFileIDRefAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. It is obsolete and has no replacement.}
{
 *  PBDeleteFileIDRefSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDeleteFileIDRefSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBDeleteFileIDRefSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.5. It is obsolete and has no replacement.}
{
 *  PBDeleteFileIDRefAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.5
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDeleteFileIDRefAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBDeleteFileIDRefAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_5, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetVolumeInfo instead.}
{
 *  PBXGetVolInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function PBXGetVolInfoSync( paramBlock: XVolumeParamPtr ): OSErr; external name '_PBXGetVolInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetVolumeInfo instead.}
{
 *  PBXGetVolInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function PBXGetVolInfoAsync( paramBlock: XVolumeParamPtr ): OSErr; external name '_PBXGetVolInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBAllocateForkSync instead.}
{
 *  PBAllocateSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBAllocateSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBAllocateSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBAllocateForkAsync instead.}
{
 *  PBAllocateAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBAllocateAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBAllocateAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetForkSizeSync instead.}
{
 *  PBGetEOFSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetEOFSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBGetEOFSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetForkSizeAsync instead.}
{
 *  PBGetEOFAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetEOFAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBGetEOFAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetForkSizeSync instead.}
{
 *  PBSetEOFSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetEOFSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBSetEOFSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetForkSizeAsync instead.}
{
 *  PBSetEOFAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetEOFAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBSetEOFAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetForkPositionSync instead.}
{
 *  PBGetFPosSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetFPosSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBGetFPosSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetForkPositionAsync instead.}
{
 *  PBGetFPosAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetFPosAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBGetFPosAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetForkPositionSync instead.}
{
 *  PBSetFPosSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetFPosSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBSetFPosSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetForkPositionAsync instead.}
{
 *  PBSetFPosAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetFPosAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBSetFPosAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBFlushForkSync instead.}
{
 *  PBFlushFileSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBFlushFileSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBFlushFileSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBFlushForkAsync instead.}
{
 *  PBFlushFileAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBFlushFileAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBFlushFileAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSEjectVolumeSync instead.}
{
 *  PBUnmountVol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBUnmountVol( paramBlock: ParmBlkPtr ): OSErr; external name '_PBUnmountVol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBCatalogSearchSync instead.}
{
 *  PBCatSearchSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCatSearchSync( paramBlock: CSParamPtr ): OSErr; external name '_PBCatSearchSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBCatalogSearchAsync instead.}
{
 *  PBCatSearchAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCatSearchAsync( paramBlock: CSParamPtr ): OSErr; external name '_PBCatSearchAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSEjectVolumeSync instead.}
{
 *  UnmountVol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function UnmountVol( volName: ConstStringPtr { can be NULL }; vRefNum: FSVolumeRefNum ): OSErr; external name '_UnmountVol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. The routines which use the default volume concept have been deprecated.}
{
 *  HSetVol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HSetVol( volName: ConstStringPtr { can be NULL }; vRefNum: FSVolumeRefNum; dirID: SInt32 ): OSErr; external name '_HSetVol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ AddDrive() was moved to Devices.h}

{ This function is deprecated in Mac OS X 10.4. Use FSCloseFork instead.}
{
 *  FSClose()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSClose( refNum: FSIORefNum ): OSErr; external name '_FSClose';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSReadFork instead.}
{
 *  FSRead()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSRead( refNum: FSIORefNum; var count: SInt32; buffPtr: UnivPtr ): OSErr; external name '_FSRead';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSWriteFork instead.}
{
 *  FSWrite()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSWrite( refNum: FSIORefNum; var count: SInt32; buffPtr: {const} UnivPtr ): OSErr; external name '_FSWrite';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSAllocateFork instead.}
{
 *  Allocate()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function Allocate( refNum: FSIORefNum; var count: SInt32 ): OSErr; external name '_Allocate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetForkSize instead.}
{
 *  GetEOF()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetEOF( refNum: FSIORefNum; var logEOF: SInt32 ): OSErr; external name '_GetEOF';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetForkSize instead.}
{
 *  SetEOF()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetEOF( refNum: FSIORefNum; logEOF: SInt32 ): OSErr; external name '_SetEOF';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetForkPosition instead.}
{
 *  GetFPos()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetFPos( refNum: FSIORefNum; var filePos: SInt32 ): OSErr; external name '_GetFPos';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetForkPosition instead.}
{
 *  SetFPos()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function SetFPos( refNum: FSIORefNum; posMode: SInt16; posOff: SInt32 ): OSErr; external name '_SetFPos';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetCatalogInfo instead.}
{
 *  GetVRefNum()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function GetVRefNum( fileRefNum: FSIORefNum; var vRefNum: FSVolumeRefNum ): OSErr; external name '_GetVRefNum';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBXLockRangeSync or FSLockRange instead.}
{
 *  PBLockRangeSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBLockRangeSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBLockRangeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBXLockRangeAsync instead.}
{
 *  PBLockRangeAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBLockRangeAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBLockRangeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBXUnlockRangeSync or FSUnlockRange instead.}
{
 *  PBUnlockRangeSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBUnlockRangeSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBUnlockRangeSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBXUnlockRangeAsync instead.}
{
 *  PBUnlockRangeAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBUnlockRangeAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBUnlockRangeAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. The routines which use the default volume concept have been deprecated.}
{
 *  PBHSetVolSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetVolSync( paramBlock: WDPBPtr ): OSErr; external name '_PBHSetVolSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. The routines which use the default volume concept have been deprecated.}
{
 *  PBHSetVolAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetVolAsync( paramBlock: WDPBPtr ): OSErr; external name '_PBHSetVolAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. The routines which use the default volume concept have been deprecated.}
{
 *  PBHGetVolSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetVolSync( paramBlock: WDPBPtr ): OSErr; external name '_PBHGetVolSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. The routines which use the default volume concept have been deprecated.}
{
 *  PBHGetVolAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetVolAsync( paramBlock: WDPBPtr ): OSErr; external name '_PBHGetVolAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBMoveObjectSync instead.}
{
 *  PBCatMoveSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCatMoveSync( paramBlock: CMovePBPtr ): OSErr; external name '_PBCatMoveSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBMoveObjectAsync instead.}
{
 *  PBCatMoveAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBCatMoveAsync( paramBlock: CMovePBPtr ): OSErr; external name '_PBCatMoveAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBCreateDirectoryUnicodeSync instead.}
{
 *  PBDirCreateSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDirCreateSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBDirCreateSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBCreateDirectoryUnicodeAsync instead.}
{
 *  PBDirCreateAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDirCreateAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBDirCreateAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetForkCBInfoSync instead.}
{
 *  PBGetFCBInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetFCBInfoSync( paramBlock: FCBPBPtr ): OSErr; external name '_PBGetFCBInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetForkCBInfoAsync instead.}
{
 *  PBGetFCBInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetFCBInfoAsync( paramBlock: FCBPBPtr ): OSErr; external name '_PBGetFCBInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetCatalogInfoSync instead.}
{
 *  PBGetCatInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetCatInfoSync( paramBlock: CInfoPBPtr ): OSErr; external name '_PBGetCatInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetCatalogInfoAsync instead.}
{
 *  PBGetCatInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetCatInfoAsync( paramBlock: CInfoPBPtr ): OSErr; external name '_PBGetCatInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoSync instead.}
{
 *  PBSetCatInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetCatInfoSync( paramBlock: CInfoPBPtr ): OSErr; external name '_PBSetCatInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoAsync instead.}
{
 *  PBSetCatInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetCatInfoAsync( paramBlock: CInfoPBPtr ): OSErr; external name '_PBSetCatInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBAllocateForkSync instead.}
{
 *  PBAllocContigSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBAllocContigSync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBAllocContigSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBAllocateForkAsync instead.}
{
 *  PBAllocContigAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBAllocContigAsync( paramBlock: ParmBlkPtr ): OSErr; external name '_PBAllocContigAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetVolumeInfoSync instead.}
{
 *  PBSetVInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetVInfoSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBSetVInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetVolumeInfoAsync instead.}
{
 *  PBSetVInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetVInfoAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBSetVInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetVolumeInfoSync instead.}
{
 *  PBHGetVInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetVInfoSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetVInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetVolumeInfoAsync instead.}
{
 *  PBHGetVInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetVInfoAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetVInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBOpenForkSync instead.}
{
 *  PBHOpenSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBOpenForkAsync instead.}
{
 *  PBHOpenAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBOpenForkSync instead.}
{
 *  PBHOpenRFSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenRFSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenRFSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBOpenForkAsync instead.}
{
 *  PBHOpenRFAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenRFAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenRFAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBOpenForkSync instead.}
{
 *  PBHOpenDFSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenDFSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenDFSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBOpenForkAsync instead.}
{
 *  PBHOpenDFAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHOpenDFAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHOpenDFAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBCreateFileUnicodeSync instead.}
{
 *  PBHCreateSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHCreateSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHCreateSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBCreateFileUnicodeAsync instead.}
{
 *  PBHCreateAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHCreateAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHCreateAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBDeleteObjectSync instead.}
{
 *  PBHDeleteSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHDeleteSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHDeleteSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBDeleteObjectAsync instead.}
{
 *  PBHDeleteAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHDeleteAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHDeleteAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBRenameUnicodeSync instead.}
{
 *  PBHRenameSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHRenameSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHRenameSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBRenameUnicodeAsync instead.}
{
 *  PBHRenameAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHRenameAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHRenameAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoSync instead.}
{
 *  PBHRstFLockSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHRstFLockSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHRstFLockSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoAsync instead.}
{
 *  PBHRstFLockAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHRstFLockAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHRstFLockAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoSync instead.}
{
 *  PBHSetFLockSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetFLockSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHSetFLockSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoAsync instead.}
{
 *  PBHSetFLockAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetFLockAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHSetFLockAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetCatalogInfoSync instead.}
{
 *  PBHGetFInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetFInfoSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetFInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBGetCatalogInfoAsync instead.}
{
 *  PBHGetFInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetFInfoAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetFInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoSync instead.}
{
 *  PBHSetFInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetFInfoSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHSetFInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBSetCatalogInfoAsync instead.}
{
 *  PBHSetFInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHSetFInfoAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHSetFInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBMakeFSRefUnicodeSync instead.}
{
 *  PBMakeFSSpecSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBMakeFSSpecSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBMakeFSSpecSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBMakeFSRefUnicodeAsync instead.}
{
 *  PBMakeFSSpecAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBMakeFSSpecAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBMakeFSSpecAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. The routines which use the default volume concept have been deprecated.}
{
 *  HGetVol()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HGetVol( volName: StringPtr; var vRefNum: FSVolumeRefNum; var dirID: SInt32 ): OSErr; external name '_HGetVol';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSOpenFork instead.}
{
 *  HOpen()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HOpen( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; permission: SInt8; var refNum: FSIORefNum ): OSErr; external name '_HOpen';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSOpenFork instead.}
{
 *  HOpenDF()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HOpenDF( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; permission: SInt8; var refNum: FSIORefNum ): OSErr; external name '_HOpenDF';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSOpenFork instead.}
{
 *  HOpenRF()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HOpenRF( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; permission: SInt8; var refNum: FSIORefNum ): OSErr; external name '_HOpenRF';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSAllocateFork instead.}
{
 *  AllocContig()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function AllocContig( refNum: FSVolumeRefNum; var count: SInt32 ): OSErr; external name '_AllocContig';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSCreateFileUnicode instead.}
{
 *  HCreate()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HCreate( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; creator: OSType; fileType: OSType ): OSErr; external name '_HCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSCreateDirectoryUnicode instead.}
{
 *  DirCreate()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function DirCreate( vRefNum: FSVolumeRefNum; parentDirID: SInt32; const (*var*) directoryName: Str255; var createdDirID: SInt32 ): OSErr; external name '_DirCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSDeleteObject instead.}
{
 *  HDelete()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HDelete( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255 ): OSErr; external name '_HDelete';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetCatalogInfo instead.}
{
 *  HGetFInfo()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HGetFInfo( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; var fndrInfo: FInfo ): OSErr; external name '_HGetFInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetCatalogInfo instead.}
{
 *  HSetFInfo()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HSetFInfo( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; const (*var*) fndrInfo: FInfo ): OSErr; external name '_HSetFInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetCatalogInfo instead.}
{
 *  HSetFLock()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HSetFLock( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255 ): OSErr; external name '_HSetFLock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetCatalogInfo instead.}
{
 *  HRstFLock()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HRstFLock( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255 ): OSErr; external name '_HRstFLock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSRenameUnicode instead.}
{
 *  HRename()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function HRename( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) oldName: Str255; const (*var*) newName: Str255 ): OSErr; external name '_HRename';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSMoveObject instead.}
{
 *  CatMove()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function CatMove( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) oldName: Str255; newDirID: SInt32; const (*var*) newName: Str255 ): OSErr; external name '_CatMove';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBHGetLogInInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetLogInInfoSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetLogInInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBHGetLogInInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHGetLogInInfoAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHGetLogInInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
   This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.
   Use FSMoveObjectSync instead.
}
{
 *  PBHMoveRenameSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHMoveRenameSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHMoveRenameSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
   This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.
   Use FSMoveObjectAsync instead.
}
{
 *  PBHMoveRenameAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBHMoveRenameAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBHMoveRenameAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBGetXCatInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function PBGetXCatInfoSync( paramBlock: XCInfoPBPtr ): OSErr; external name '_PBGetXCatInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBGetXCatInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 }
function PBGetXCatInfoAsync( paramBlock: XCInfoPBPtr ): OSErr; external name '_PBGetXCatInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBExchangeObjectsSync instead.}
{
 *  PBExchangeFilesSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBExchangeFilesSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBExchangeFilesSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use PBExchangeObjectsAsync instead.}
{
 *  PBExchangeFilesAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBExchangeFilesAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBExchangeFilesAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBGetForeignPrivsSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetForeignPrivsSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBGetForeignPrivsSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBGetForeignPrivsAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetForeignPrivsAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBGetForeignPrivsAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBSetForeignPrivsSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetForeignPrivsSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBSetForeignPrivsSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBSetForeignPrivsAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBSetForeignPrivsAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBSetForeignPrivsAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{  Desktop Manager  }
{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
const
{ Desktop Database, ffsGetIconMessage and fsmGetFSIconMessage icon type and size Constants }
	kLargeIcon = 1;
	kLarge4BitIcon = 2;
	kLarge8BitIcon = 3;
	kSmallIcon = 4;
	kSmall4BitIcon = 5;
	kSmall8BitIcon = 6;
	kicnsIconFamily = 239;   { Note: The 'icns' icon family record is variable sized. }

{$endc} {not TARGET_CPU_64}

{$ifc not TARGET_CPU_64}
{
 *  PBDTGetPath()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetPath( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetPath';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTCloseDown()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTCloseDown( paramBlock: DTPBPtr ): OSErr; external name '_PBDTCloseDown';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTAddIconSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTAddIconSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTAddIconSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTAddIconAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTAddIconAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTAddIconAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetIconSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetIconSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetIconSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetIconAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetIconAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetIconAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetIconInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetIconInfoSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetIconInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetIconInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetIconInfoAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetIconInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTAddAPPLSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTAddAPPLSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTAddAPPLSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTAddAPPLAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTAddAPPLAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTAddAPPLAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTRemoveAPPLSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTRemoveAPPLSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTRemoveAPPLSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTRemoveAPPLAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTRemoveAPPLAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTRemoveAPPLAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetAPPLSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetAPPLSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetAPPLSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetAPPLAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetAPPLAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetAPPLAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTSetCommentSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTSetCommentSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTSetCommentSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTSetCommentAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTSetCommentAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTSetCommentAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTRemoveCommentSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTRemoveCommentSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTRemoveCommentSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTRemoveCommentAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTRemoveCommentAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTRemoveCommentAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetCommentSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetCommentSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetCommentSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetCommentAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetCommentAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetCommentAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTFlushSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTFlushSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTFlushSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTFlushAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTFlushAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTFlushAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTResetSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTResetSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTResetSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTResetAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTResetAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTResetAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetInfoSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetInfoSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetInfoSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTGetInfoAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTGetInfoAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTGetInfoAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTOpenInform()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTOpenInform( paramBlock: DTPBPtr ): OSErr; external name '_PBDTOpenInform';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTDeleteSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTDeleteSync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTDeleteSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PBDTDeleteAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.2
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBDTDeleteAsync( paramBlock: DTPBPtr ): OSErr; external name '_PBDTDeleteAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{  FSp traps  }
{ This function is deprecated in Mac OS X 10.4. Use FSMakeFSRefUnicode instead.}
{
 *  FSMakeFSSpec()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSMakeFSSpec( vRefNum: FSVolumeRefNum; dirID: SInt32; const (*var*) fileName: Str255; var spec: FSSpec ): OSErr; external name '_FSMakeFSSpec';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSOpenFork instead.}
{
 *  FSpOpenDF()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpOpenDF( const (*var*) spec: FSSpec; permission: SInt8; var refNum: FSIORefNum ): OSErr; external name '_FSpOpenDF';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSOpenFork instead.}
{
 *  FSpOpenRF()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpOpenRF( const (*var*) spec: FSSpec; permission: SInt8; var refNum: FSIORefNum ): OSErr; external name '_FSpOpenRF';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSCreateFileUnicode instead.}
{
 *  FSpCreate()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpCreate( const (*var*) spec: FSSpec; creator: OSType; fileType: OSType; scriptTag: ScriptCode ): OSErr; external name '_FSpCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSCreateDirectoryUnicode instead.}
{
 *  FSpDirCreate()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpDirCreate( const (*var*) spec: FSSpec; scriptTag: ScriptCode; var createdDirID: SInt32 ): OSErr; external name '_FSpDirCreate';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSDeleteObject instead.}
{
 *  FSpDelete()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpDelete( const (*var*) spec: FSSpec ): OSErr; external name '_FSpDelete';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSGetCatalogInfo instead.}
{
 *  FSpGetFInfo()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpGetFInfo( const (*var*) spec: FSSpec; var fndrInfo: FInfo ): OSErr; external name '_FSpGetFInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetCatalogInfo instead.}
{
 *  FSpSetFInfo()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpSetFInfo( const (*var*) spec: FSSpec; const (*var*) fndrInfo: FInfo ): OSErr; external name '_FSpSetFInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetCatalogInfo instead.}
{
 *  FSpSetFLock()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpSetFLock( const (*var*) spec: FSSpec ): OSErr; external name '_FSpSetFLock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSSetCatalogInfo instead.}
{
 *  FSpRstFLock()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpRstFLock( const (*var*) spec: FSSpec ): OSErr; external name '_FSpRstFLock';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSRenameUnicode instead.}
{
 *  FSpRename()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpRename( const (*var*) spec: FSSpec; const (*var*) newName: Str255 ): OSErr; external name '_FSpRename';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSMoveObject instead.}
{
 *  FSpCatMove()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpCatMove( const (*var*) source: FSSpec; const (*var*) dest: FSSpec ): OSErr; external name '_FSpCatMove';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. Use FSExchangeObjects instead.}
{
 *  FSpExchangeFiles()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function FSpExchangeFiles( const (*var*) source: FSSpec; const (*var*) dest: FSSpec ): OSErr; external name '_FSpExchangeFiles';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBShareSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBShareSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBShareSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBShareAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBShareAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBShareAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBUnshareSync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBUnshareSync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBUnshareSync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBUnshareAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBUnshareAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBUnshareAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBGetUGEntrySync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetUGEntrySync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBGetUGEntrySync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)


{ This function is deprecated in Mac OS X 10.4. It was never implemented on OS X.}
{
 *  PBGetUGEntryAsync()   *** DEPRECATED ***
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.0
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework [32-bit only] but deprecated in 10.4
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 }
function PBGetUGEntryAsync( paramBlock: HParmBlkPtr ): OSErr; external name '_PBGetUGEntryAsync';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_4, __IPHONE_NA, __IPHONE_NA) *)

{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
