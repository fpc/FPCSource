{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 - 2004 by Olle Raab

    A selection of the MacOS API for FreePascal, written
    in the Turbo Pascal dialect.
    It is primarily for internal use in the rtl, please
    do not expect it to remain the same over time,
    it will be subject to changes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Note: Types like Mac_XXX corresponds to the type XXX defined
in MacOS Universal Headers. The prefix is to avoid name clashes
with FPC types.}

{$PACKRECORDS 2}  {Alignment inherited from the m68k days}

{************** from Types.p ***************}
const
  noErr = 0;

type
  SignedByte = shortint;
  SignedBytePtr = ^SignedByte;

	SInt8                               = -128..127;
	SInt16                              = INTEGER;
	SInt32                              = LONGINT;
	UInt8                               = 0..255;
	UInt16                              = WORD;
	UInt32                              = LONGWORD;

  OSErr = Integer;
  OSType = Longword;
  ResType = Longword;
  Mac_Ptr = pointer;
  Mac_Handle = ^Mac_Ptr;
  Str31 = string[31];
  Str32 = string[32];
  Str63 = string[63];
  Str255 = string[255];
  StringPtr = ^Str255;
  ProcPtr = Mac_Ptr;
  UniversalProcPtr = ProcPtr;

  ScriptCode = INTEGER;

  LangCode = INTEGER;

  Point = record
    case INTEGER of
     0: (
       v: INTEGER;
       h: INTEGER;
     );
     1: (
       vh: array[0..1] of INTEGER;
     );
   end;

  PointPtr = ^Point;

  Rect = record
    case INTEGER of
     0: (
       top: INTEGER;
       left: INTEGER;
       bottom: INTEGER;
       right: INTEGER;
     );
     1: (
       topLeft: Point;
       botRight: Point;
     );
   end;

  RectPtr = ^Rect;

procedure Debugger;
external 'InterfaceLib';

procedure DebugStr(s: Str255);
external 'InterfaceLib';

{************** from Memory.p ***************}

type
  Size = Longint;

function NewHandle (byteCount: Size): Mac_Handle;
external 'InterfaceLib';

function NewPtr(logicalSize: Size): Mac_Ptr ;
external 'InterfaceLib';

procedure DisposePtr (p: Mac_Ptr);
external 'InterfaceLib';
function GetPtrSize (p: Mac_Ptr): Size;
external 'InterfaceLib';
procedure SetPtrSize (p: Mac_Ptr; newSize: Size);
external 'InterfaceLib';
procedure DisposeHandle (h: Mac_Handle);
external 'InterfaceLib';
procedure SetHandleSize (h: Mac_Handle; newSize: Size);
external 'InterfaceLib';
function GetHandleSize (h: Mac_Handle): Size;
external 'InterfaceLib';
 
function Mac_FreeMem: Longint;
external 'InterfaceLib' name 'FreeMem';
PROCEDURE MaxApplZone;
external 'InterfaceLib';
PROCEDURE HLock(h: Mac_Handle);
external 'InterfaceLib';
PROCEDURE HUnlock(h: Mac_Handle);
external 'InterfaceLib';
function MemError: OSErr;
external 'InterfaceLib';

{************** from GestaltEqu.p ***************}
FUNCTION Gestalt(selector: OSType; VAR response: LONGINT): OSErr;
external 'InterfaceLib' name 'Gestalt';

const
	{	 Gestalt selector and values for the Appearance Manager 	}
	gestaltAliasMgrAttr			= 'alis';						{  Alias Mgr Attributes  }
	gestaltAliasMgrPresent		= 0;							{  True if the Alias Mgr is present  }
	gestaltAliasMgrSupportsRemoteAppletalk = 1;					{  True if the Alias Mgr knows about Remote Appletalk  }
	gestaltAliasMgrSupportsAOCEKeychain = 2;					{  True if the Alias Mgr knows about the AOCE Keychain  }
	gestaltAliasMgrResolveAliasFileWithMountOptions = 3;		{  True if the Alias Mgr implements gestaltAliasMgrResolveAliasFileWithMountOptions() and IsAliasFile()  }
	gestaltAliasMgrFollowsAliasesWhenResolving = 4;
	gestaltAliasMgrSupportsExtendedCalls = 5;
	gestaltAliasMgrSupportsFSCalls = 6;							{  true if Alias Mgr supports HFS+ Calls  }

	gestaltAppearanceAttr		= 'appr';
	gestaltAppearanceExists		= 0;
	gestaltAppearanceCompatMode	= 1;

	{	 Gestalt selector for determining Appearance Manager version   	}
	{	 If this selector does not exist, but gestaltAppearanceAttr    	}
	{	 does, it indicates that the 1.0 version is installed. This    	}
	{	 gestalt returns a BCD number representing the version of the  	}
	{	 Appearance Manager that is currently running, e.g. 0x0101 for 	}
	{	 version 1.0.1.                                                	}
	gestaltAppearanceVersion	= 'apvr';

	gestaltCFMAttr				= 'cfrg';						{  Selector for information about the Code Fragment Manager  }
	gestaltCFMPresent			= 0;							{  True if the Code Fragment Manager is present  }
	gestaltCFMPresentMask		= $0001;
	gestaltCFM99Present			= 2;							{  True if the CFM-99 features are present.  }
	gestaltCFM99PresentMask		= $0004;

	gestaltAppleEventsAttr		= 'evnt';						{  Apple Events attributes  }
	gestaltAppleEventsPresent	= 0;							{  True if Apple Events present  }
	gestaltScriptingSupport		= 1;
	gestaltOSLInSystem			= 2;							{  OSL is in system so don’t use the one linked in to app  }

	gestaltFindFolderAttr		= 'fold';						{  Folder Mgr attributes  }
	gestaltFindFolderPresent	= 0;							{  True if Folder Mgr present  }
	gestaltFolderDescSupport	= 1;							{  True if Folder Mgr has FolderDesc calls  }
	gestaltFolderMgrFollowsAliasesWhenResolving = 2;			{  True if Folder Mgr follows folder aliases  }
	gestaltFolderMgrSupportsExtendedCalls = 3;					{  True if Folder Mgr supports the Extended calls  }
	gestaltFolderMgrSupportsDomains = 4;						{  True if Folder Mgr supports domains for the first parameter to FindFolder  }
	gestaltFolderMgrSupportsFSCalls = 5;						{  True if FOlder manager supports __FindFolderFSRef & __FindFolderExtendedFSRef  }

	gestaltFPUType				= 'fpu ';						{  fpu type  }
	gestaltNoFPU				= 0;							{  no FPU  }
	gestalt68881				= 1;							{  68881 FPU  }
	gestalt68882				= 2;							{  68882 FPU  }
	gestalt68040FPU				= 3;							{  68040 built-in FPU  }

	gestaltFSAttr				= 'fs  ';						{  file system attributes  }
	gestaltFullExtFSDispatching	= 0;							{  has really cool new HFSDispatch dispatcher  }
	gestaltHasFSSpecCalls		= 1;							{  has FSSpec calls  }
	gestaltHasFileSystemManager	= 2;							{  has a file system manager  }
	gestaltFSMDoesDynamicLoad	= 3;							{  file system manager supports dynamic loading  }
	gestaltFSSupports4GBVols	= 4;							{  file system supports 4 gigabyte volumes  }
	gestaltFSSupports2TBVols	= 5;							{  file system supports 2 terabyte volumes  }
	gestaltHasExtendedDiskInit	= 6;							{  has extended Disk Initialization calls  }
	gestaltDTMgrSupportsFSM		= 7;							{  Desktop Manager support FSM-based foreign file systems  }
	gestaltFSNoMFSVols			= 8;							{  file system doesn't supports MFS volumes  }
	gestaltFSSupportsHFSPlusVols = 9;							{  file system supports HFS Plus volumes  }
	gestaltFSIncompatibleDFA82	= 10;							{  VCB and FCB structures changed; DFA 8.2 is incompatible  }

	gestaltHasHFSPlusAPIs		= 12;							{  file system supports HFS Plus APIs  }
	gestaltMustUseFCBAccessors	= 13;							{  FCBSPtr and FSFCBLen are invalid - must use FSM FCB accessor functions }
	gestaltFSUsesPOSIXPathsForConversion = 14;					{  The path interchange routines operate on POSIX paths instead of HFS paths  }

	gestaltOSAttr				= 'os  ';						{  o/s attributes  }
	gestaltSysZoneGrowable		= 0;							{  system heap is growable  }
	gestaltLaunchCanReturn		= 1;							{  can return from launch  }
	gestaltLaunchFullFileSpec	= 2;							{  can launch from full file spec  }
	gestaltLaunchControl		= 3;							{  launch control support available  }
	gestaltTempMemSupport		= 4;							{  temp memory support  }
	gestaltRealTempMemory		= 5;							{  temp memory handles are real  }
	gestaltTempMemTracked		= 6;							{  temporary memory handles are tracked  }
	gestaltIPCSupport			= 7;							{  IPC support is present  }
	gestaltSysDebuggerSupport	= 8;							{  system debugger support is present  }
	gestaltNativeProcessMgrBit	= 19;							{  the process manager itself is native  }

	gestaltQuickdrawVersion		= 'qd  ';						{  quickdraw version  }
	gestaltOriginalQD			= $0000;						{  original 1-bit QD  }
	gestalt8BitQD				= $0100;						{  8-bit color QD  }
	gestalt32BitQD				= $0200;						{  32-bit color QD  }
	gestalt32BitQD11			= $0201;						{  32-bit color QDv1.1  }
	gestalt32BitQD12			= $0220;						{  32-bit color QDv1.2  }
	gestalt32BitQD13			= $0230;						{  32-bit color QDv1.3  }
	gestaltAllegroQD			= $0250;						{  Allegro QD OS 8.5  }
	gestaltMacOSXQD				= $0300;						{  Mac OS X QD  }
	gestaltScriptMgrVersion		= 'scri';						{  Script Manager version number      }
	gestaltScriptCount			= 'scr#';						{  number of active script systems    }

	gestaltStandardFileAttr		= 'stdf';						{  Standard File attributes  }
	gestaltStandardFile58		= 0;							{  True if selectors 5-8 (StandardPutFile-CustomGetFile) are supported  }
	gestaltStandardFileTranslationAware = 1;					{  True if standard file is translation manager aware  }
	gestaltStandardFileHasColorIcons = 2;						{  True if standard file has 16x16 color icons  }
	gestaltStandardFileUseGenericIcons = 3;						{  Standard file LDEF to use only the system generic icons if true  }
	gestaltStandardFileHasDynamicVolumeAllocation = 4;			{  True if standard file supports more than 20 volumes  }

	gestaltSystemVersion		= 'sysv';						{  system version }

	gestaltThreadMgrAttr		= 'thds';						{  Thread Manager attributes  }
	gestaltThreadMgrPresent		= 0;							{  bit true if Thread Mgr is present  }
	gestaltSpecificMatchSupport	= 1;							{  bit true if Thread Mgr supports exact match creation option  }
	gestaltThreadsLibraryPresent = 2;							{  bit true if Thread Mgr shared library is present  }


{************** from OSUtils.p ***************}

type
  QElemPtr = ^QElem;

  QElem = record
    qLink: QElemPtr;
    qType: INTEGER;
    qData: array[0..0] of INTEGER;
   end;

CONST
	curSysEnvVers				= 2;							{ Updated to equal latest SysEnvirons version }

TYPE
	SysEnvRecPtr = ^SysEnvRec;
	SysEnvRec = RECORD
		environsVersion:		INTEGER;
		machineType:			INTEGER;
		systemVersion:			INTEGER;
		processor:				INTEGER;
		hasFPU:					BOOLEAN;
		hasColorQD:				BOOLEAN;
		keyBoardType:			INTEGER;
		atDrvrVersNum:			INTEGER;
		sysVRefNum:				INTEGER;
	END;

FUNCTION SysEnvirons(versionRequested: INTEGER; VAR theWorld: SysEnvRec): OSErr;
external 'InterfaceLib';

{************** from Finder.p ***************}

 type
  FInfo = record
    fdType: OSType;		{the type of the file}
    fdCreator: OSType;		{file's creator}
    fdFlags: INTEGER;		{flags ex. hasbundle,invisible,locked, etc.}
    fdLocation: Point;		{file's location in folder}
    fdFldr: INTEGER;		{folder containing file}
   end;

  FXInfo = record
    fdIconID: INTEGER;		{Icon ID}
    fdUnused: array[0..2] of INTEGER;	{unused but reserved 6 bytes}
    fdScript: SInt8;		{Script flag and number}
    fdXFlags: SInt8;		{More flag bits}
    fdComment: INTEGER;		{Comment ID}
    fdPutAway: LONGINT;		{Home Dir ID}
   end;

  DInfo = record
    frRect: Rect;		{folder rect}
    frFlags: INTEGER;		{Flags}
    frLocation: Point;		{folder location}
    frView: INTEGER;		{folder view}
   end;

  DXInfo = record
    frScroll: Point;		{scroll position}
    frOpenChain: LONGINT;	{DirID chain of open folders}
    frScript: SInt8;		{Script flag and number}
    frXFlags: SInt8;		{More flag bits}
    frComment: INTEGER;		{comment}
    frPutAway: LONGINT;		{DirID}
   end;

{************** from Files.p ***************}

const
  fsAtMark = 0;
  fsCurPerm = 0;
  fsRdPerm = 1;
  fInvisible = 16384;
  fsWrPerm = 2;
  fsRdWrPerm = 3;
  fsRdWrShPerm = 4;
  fsFromStart = 1;
  fsFromLEOF = 2;
  fsFromMark = 3;
  rdVerify = 64;
  ioMapBuffer = 4;
  ioModeReserved = 8;
  ioDirFlg = 4;			{ see IM IV-125 }
  ioDirMask = $10;
  fsRtParID = 1;
  fsRtDirID = 2;

type
	CatPositionRecPtr = ^CatPositionRec;
	CatPositionRec = RECORD
		initialize:				LONGINT;
		priv:					ARRAY [1..6] OF INTEGER;
	END;

  FSSpec = record
      vRefNum: Integer;
      parID: Longint;
      name: Str63;
   end;
  FSSpecPtr = ^FSSpec;

  IOCompletionUPP = UniversalProcPtr;

  CInfoPBRecPtr = ^CInfoPBRec;
  CInfoPBRec = record
    qLink: QElemPtr;
    qType: INTEGER;
    ioTrap: INTEGER;
    ioCmdAddr: Mac_Ptr;
    ioCompletion: IOCompletionUPP;
    ioResult: OSErr;
    ioNamePtr: StringPtr;
    ioVRefNum: INTEGER;
    ioFRefNum: INTEGER;
    ioFVersNum: SInt8;
    filler1: SInt8;
    ioFDirIndex: INTEGER;
    ioFlAttrib: SInt8;
    ioACUser: SInt8;
    case INTEGER of
     0: (
       ioFlFndrInfo: FInfo;
       ioDirID: LONGINT;
       ioFlStBlk: INTEGER;
       ioFlLgLen: LONGINT;
       ioFlPyLen: LONGINT;
       ioFlRStBlk: INTEGER;
       ioFlRLgLen: LONGINT;
       ioFlRPyLen: LONGINT;
       ioFlCrDat: LONGINT;
       ioFlMdDat: LONGINT;
       ioFlBkDat: LONGINT;
       ioFlXFndrInfo: FXInfo;
       ioFlParID: LONGINT;
       ioFlClpSiz: LONGINT;
     );
     1: (
       ioDrUsrWds: DInfo;
       ioDrDirID: LONGINT;
       ioDrNmFls: INTEGER;
       filler3: array[1..9] of INTEGER;
       ioDrCrDat: LONGINT;
       ioDrMdDat: LONGINT;
       ioDrBkDat: LONGINT;
       ioDrFndrInfo: DXInfo;
       ioDrParID: LONGINT;
     );
   end;

  CInfoPBPtr = ^CInfoPBRec;

	HParamBlockRecPtr = ^HParamBlockRec;
	HParamBlockRec = RECORD
		qLink:					QElemPtr;								{ queue link in header }
		qType:					INTEGER;								{ type byte for safety check }
		ioTrap:					INTEGER;								{ FS: the Trap }
		ioCmdAddr:				pointer;									{ FS: address to dispatch to }
		ioCompletion:			IOCompletionUPP;						{ completion routine addr (0 for synch calls) }
		ioResult:				OSErr;									{ result code }
		ioNamePtr:				StringPtr;								{ ptr to Vol:FileName string }
		ioVRefNum:				INTEGER;								{ volume refnum (DrvNum for Eject and MountVol) }
		CASE INTEGER OF
		0: (
			ioRefNum:			INTEGER;
			ioVersNum:			SInt8;
			ioPermssn:			SInt8;
			ioMisc:				pointer;
			ioBuffer:			pointer;
			ioReqCount:			LONGINT;
			ioActCount:			LONGINT;
			ioPosMode:			INTEGER;
			ioPosOffset:		LONGINT;
		   );
		1: (
			ioFRefNum:			INTEGER;
			ioFVersNum:			SInt8;
			filler1:			SInt8;
			ioFDirIndex:		INTEGER;
			ioFlAttrib:			SInt8;
			ioFlVersNum:		SInt8;
			ioFlFndrInfo:		FInfo;
			ioDirID:			LONGINT;
			ioFlStBlk:			UInt16;
			ioFlLgLen:			LONGINT;
			ioFlPyLen:			LONGINT;
			ioFlRStBlk:			UInt16;
			ioFlRLgLen:			LONGINT;
			ioFlRPyLen:			LONGINT;
			ioFlCrDat:			UInt32;
			ioFlMdDat:			UInt32;
		   );
		2: (
			filler2:			LONGINT;
			ioVolIndex:			INTEGER;
			ioVCrDate:			UInt32;
			ioVLsMod:			UInt32;
			ioVAtrb:			INTEGER;
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
			ioVDrvInfo:			INTEGER;
			ioVDRefNum:			INTEGER;
			ioVFSID:			INTEGER;
			ioVBkUp:			UInt32;
			ioVSeqNum:			UInt16;
			ioVWrCnt:			UInt32;
			ioVFilCnt:			UInt32;
			ioVDirCnt:			UInt32;
			ioVFndrInfo:		ARRAY [1..8] OF LONGINT;
		   );
		3: (
			filler3:			INTEGER;
			ioDenyModes:		INTEGER;								{ access rights data }
			filler4:			INTEGER;
			filler5:			SInt8;
			ioACUser:			SInt8;									{ access rights for directory only }
			filler6:			LONGINT;
			ioACOwnerID:		LONGINT;								{ owner ID }
			ioACGroupID:		LONGINT;								{ group ID }
			ioACAccess:			LONGINT;								{ access rights }
		   );
		4: (
			filler7:			INTEGER;
			ioObjType:			INTEGER;								{ function code }
			ioObjNamePtr:		StringPtr;								{ ptr to returned creator/group name }
			ioObjID:			LONGINT;								{ creator/group ID }
		   );
		5: (
			ioDstVRefNum:		INTEGER;								{ destination vol identifier }
			filler8:			INTEGER;
			ioNewName:			StringPtr;								{ ptr to destination pathname }
			ioCopyName:			StringPtr;								{ ptr to optional name }
			ioNewDirID:			LONGINT;								{ destination directory ID }
		   );
		6: (
			ioWDCreated:		INTEGER;
			ioWDIndex:			INTEGER;
			ioWDProcID:			LONGINT;
			ioWDVRefNum:		INTEGER;
			filler10:			INTEGER;
			filler11:			LONGINT;
			filler12:			LONGINT;
			filler13:			LONGINT;
			ioWDDirID:			LONGINT;
		   );
		7: (
			filler14:			LONGINT;
			ioDestNamePtr:		StringPtr;								{  dest file name  }
			filler15:			LONGINT;
			ioDestDirID:		LONGINT;								{  dest file's directory id  }
			filler16:			LONGINT;
			filler17:			LONGINT;
			ioSrcDirID:			LONGINT;								{  source file's directory id  }
			filler18:			INTEGER;
			ioFileID:			LONGINT;								{  file ID  }
		   );
		8: (
			ioMatchPtr:			FSSpecPtr;								{  match array  }
			ioReqMatchCount:	LONGINT;								{  maximum allowable matches  }
			ioActMatchCount:	LONGINT;								{  actual match count  }
			ioSearchBits:		LONGINT;								{  search criteria selector  }
			ioSearchInfo1:		CInfoPBPtr;								{  search values and range lower bounds  }
			ioSearchInfo2:		CInfoPBPtr;								{  search values and range upper bounds  }
			ioSearchTime:		LONGINT;								{  length of time to run search  }
			ioCatPosition:		CatPositionRec;							{  current position in the catalog  }
			ioOptBuffer:		pointer;									{  optional performance enhancement buffer  }
			ioOptBufSize:		LONGINT;								{  size of buffer pointed to by ioOptBuffer  }
		   );
		9: (
			ioFiller21:			LONGINT;
			ioFiller22:			LONGINT;
			ioForeignPrivBuffer: pointer;
			ioForeignPrivActCount: LONGINT;
			ioForeignPrivReqCount: LONGINT;
			ioFiller23:			LONGINT;
			ioForeignPrivDirID:	LONGINT;
			ioForeignPrivInfo1:	LONGINT;
			ioForeignPrivInfo2:	LONGINT;
			ioForeignPrivInfo3:	LONGINT;
			ioForeignPrivInfo4:	LONGINT;
		   );
	END;

	HParmBlkPtr							= ^HParamBlockRec;


	WDPBRecPtr = ^WDPBRec;
	WDPBRec = RECORD
		qLink:					QElemPtr;
		qType:					INTEGER;
		ioTrap:					INTEGER;
		ioCmdAddr:				pointer;
		ioCompletion:			IOCompletionUPP;
		ioResult:				OSErr;
		ioNamePtr:				StringPtr;
		ioVRefNum:				INTEGER;
		filler1:				INTEGER;
		ioWDIndex:				INTEGER;
		ioWDProcID:				LONGINT;
		ioWDVRefNum:			INTEGER;
		filler2:				ARRAY [1..7] OF INTEGER;
		ioWDDirID:				LONGINT;
	END;

	WDPBPtr								= ^WDPBRec;

FUNCTION GetVol(volName: StringPtr; VAR vRefNum: INTEGER): OSErr;
external 'InterfaceLib';

function FSpOpenDF(spec: FSSpec; permission: SignedByte;
  var refNum: Integer): OSErr;
external 'InterfaceLib';

function FSpCreate(spec: FSSpec; creator, fileType: OSType;
  scriptTag: ScriptCode): OSErr;
external 'InterfaceLib';

function FSpDirCreate(spec: FSSpec; scriptTag: ScriptCode;
  var createdDirID: Longint): OSErr;
external 'InterfaceLib';

function FSpDelete(spec: FSSpec): OSErr;
external 'InterfaceLib';

FUNCTION FSpSetFLock({CONST}VAR spec: FSSpec): OSErr;
external 'InterfaceLib';

FUNCTION FSpRstFLock({CONST}VAR spec: FSSpec): OSErr;
external 'InterfaceLib';

function FSClose(refNum: Integer): OSErr;
external 'InterfaceLib';

function FSRead(refNum: Integer; var count: Longint; buffPtr: Mac_Ptr): OSErr;
external 'InterfaceLib';

function FSWrite(refNum: Integer; var count: Longint; buffPtr: Mac_Ptr): OSErr;
external 'InterfaceLib';

function GetEOF(refNum: Integer; var logEOF: Longint): OSErr;
external 'InterfaceLib';

function SetEOF(refNum: Integer; logEOF: Longint): OSErr;
external 'InterfaceLib';

function GetFPos(refNum: Integer; var filePos: Longint): OSErr;
external 'InterfaceLib';

function SetFPos(refNum: Integer; posMode: Integer; posOff: Longint): OSErr;
external 'InterfaceLib';

FUNCTION PBGetWDInfoSync(paramBlock: WDPBPtr): OSErr;
external 'InterfaceLib';

function PBGetCatInfoSync (paramBlock: CInfoPBPtr): OSErr;
external 'InterfaceLib';

FUNCTION PBSetCatInfoSync(paramBlock: CInfoPBPtr): OSErr;
external 'InterfaceLib';

FUNCTION PBHGetVInfoSync(paramBlock: HParmBlkPtr): OSErr;
external 'InterfaceLib';

function FSMakeFSSpec (vRefNum: Integer; dirID: LongInt;
  fileName: Str255; VAR spec: FSSpec): OSErr;
external 'InterfaceLib';

{************** from Aliases.p ***************}

type
  AliasHandle = Mac_Handle;

function NewAliasMinimalFromFullPath(fullPathLength: Integer;
  fullPath: Mac_Ptr; zoneName: Str32; serverName: Str31;
  var alias: AliasHandle):OSErr;
external 'InterfaceLib';

function ResolveAlias(fromFile: FSSpecPtr; alias: AliasHandle;
  var target: FSSpec; var wasChanged: Boolean):OSErr;
external 'InterfaceLib';


{************** from Folders.p ***************}

CONST
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

	kSystemFolderType			= 'macs';						{  the system folder  }
	kDesktopFolderType			= 'desk';						{  the desktop folder; objects in this folder show on the desk top.  }
	kSystemDesktopFolderType	= 'sdsk';						{  the desktop folder at the root of the hard drive, never the redirected user desktop folder  }
	kTrashFolderType			= 'trsh';						{  the trash folder; objects in this folder show up in the trash  }
	kSystemTrashFolderType		= 'strs';						{  the trash folder at the root of the drive, never the redirected user trash folder  }
	kWhereToEmptyTrashFolderType = 'empt';						{  the "empty trash" folder; Finder starts empty from here down  }
	kPrintMonitorDocsFolderType	= 'prnt';						{  Print Monitor documents  }
	kStartupFolderType			= 'strt';						{  Finder objects (applications, documents, DAs, aliases, to...) to open at startup go here  }
	kShutdownFolderType			= 'shdf';						{  Finder objects (applications, documents, DAs, aliases, to...) to open at shutdown go here  }
	kAppleMenuFolderType		= 'amnu';						{  Finder objects to put into the Apple menu go here  }
	kControlPanelFolderType		= 'ctrl';						{  Control Panels go here (may contain INITs)  }
	kSystemControlPanelFolderType = 'sctl';						{  System control panels folder - never the redirected one, always "Control Panels" inside the System Folder  }
	kExtensionFolderType		= 'extn';						{  System extensions go here  }
	kFontsFolderType			= 'font';						{  Fonts go here  }
	kPreferencesFolderType		= 'pref';						{  preferences for applications go here  }
	kSystemPreferencesFolderType = 'sprf';						{  System-type Preferences go here - this is always the system's preferences folder, never a logged in user's  }
	kTemporaryFolderType		= 'temp';						{  temporary files go here (deleted periodically, but don't rely on it.)  }

FUNCTION FindFolder(vRefNum: INTEGER; folderType: OSType; createFolder: BOOLEAN; VAR foundVRefNum: INTEGER; VAR foundDirID: LONGINT): OSErr;
external 'InterfaceLib';

{************** from Processes.p ***************}

type
  ProcessSerialNumber = record
    highLongOfPSN: LONGINT;
    lowLongOfPSN: LONGINT;
   end;

  ProcessSerialNumberPtr = ^ProcessSerialNumber;
  ProcessInfoRec = record
    processInfoLength: LONGINT;
    processName: StringPtr;
    processNumber: ProcessSerialNumber;
    processType: LONGINT;
    processSignature: OSType;
    processMode: LONGINT;
    processLocation: Mac_Ptr;
    processSize: LONGINT;
    processFreeMem: LONGINT;
    processLauncher: ProcessSerialNumber;
    processLaunchDate: LONGINT;
    processActiveTime: LONGINT;
    processAppSpec: FSSpecPtr;
   end;

  ProcessInfoRecPtr = ^ProcessInfoRec;

const
{ Process identifier - Various reserved process serial numbers }
  kNoProcess = 0;
  kSystemProcess = 1;
  kCurrentProcess = 2;

function GetProcessInformation ({CONST} var PSN: ProcessSerialNumber;
  var info: ProcessInfoRec): OSErr;
external 'InterfaceLib';

{************** from Script.p ***************}

const
  smSystemScript = -1;

{************** from MacErrors.p ***************}

{Contains error codes for all File Manager calls, except
for PBVolumeMount and for File ID Routines (from UI 3.4).
And also memFullErr}

const

  dirFulErr  = -33; { Directory full }
  dskFulErr  = -34; { disk full }
  nsvErr     = -35; { no such volume }
  ioErr      = -36; { I/O error (bummers) }
  bdNamErr   = -37; { there may be no bad names in the final system! }
  fnOpnErr   = -38; { File not open }
  eofErr     = -39; { End of file }
  posErr     = -40; { tried to position to before start of file (r/w) }
  mFulErr    = -41; { memory full (open) or file won't fit (load) }
  tmfoErr    = -42; { too many files open }
  fnfErr     = -43; { File not found }
  wPrErr     = -44; { diskette is write protected. }
  fLckdErr   = -45; { file is locked }

  vLckdErr   = -46; { volume is locked }
  fBsyErr    = -47; { File is busy (delete) }
  dupFNErr   = -48; { duplicate filename (rename) }
  opWrErr    = -49; { file already open with with write permission }
  rfNumErr   = -51; { refnum error }
  gfpErr     = -52; { get file position error }
  volOffLinErr=-53; { volume not on line error (was Ejected) }
  permErr    = -54; { permissions error (on file open) }
  volOnLinErr= -55; { drive volume already on-line at MountVol }
  nsDrvErr   = -56; { no such drive (tried to mount a bad drive num) }
  noMacDskErr= -57; { not a mac diskette (sig bytes are wrong) }
  extFSErr   = -58; { volume in question belongs to an external fs }
  fsRnErr    = -59; { file system internal error:during rename the old entry was deleted but could not be restored. }
  badMDBErr  = -60; { bad master directory block }
  wrPermErr  = -61; { write permissions error }
  dirNFErr   = -120; { Directory not found }
  tmwdoErr   = -121; { No free WDCB available }
  badMovErr  = -122; { Move into offspring error }
  wrgVolTypErr=-123; { Wrong volume type error [operation not supported for MFS] }
  volGoneErr = -124; { Server volume has been disconnected. }
  diffVolErr = -1303; { files on different volumes }
  catChangedErr= -1304; { the catalog has been modified }
  afpAccessDenied= -5000; {  Insufficient access privileges for operation  }
  afpDenyConflict= -5006; {  Specified open/deny modes conflict with current open modes  }
  afpNoMoreLocks= -5015; {  Maximum lock limit reached  }
  afpRangeNotLocked= -5020; {  Tried to unlock range that was not locked by user  }
  afpRangeOverlap= -5021; {  Some or all of range already locked by same user  }
  afpObjectTypeErr= -5025; {  File/Directory specified where Directory/File expected  }
  afpCatalogChanged= -5037;
  afpSameObjectErr= -5038;

  memFullErr = -108; { Not enough room in heap zone }

{************** from Resources ***************}

function GetResource(theType: ResType; theID: Integer): Mac_Handle;
external 'InterfaceLib';

function Get1Resource(theType: ResType; theID: Integer): Mac_Handle;
external 'InterfaceLib';

function GetNamedResource(theType: ResType; name: Str255): Mac_Handle;
external 'InterfaceLib';

function Get1NamedResource(theType: ResType; name: Str255): Mac_Handle;
external 'InterfaceLib';

procedure ReleaseResource(theResource: Mac_Handle);
external 'InterfaceLib';

{************** from DateTimeUtils ***************}

type
	DateTimeRecPtr = ^DateTimeRec;
	DateTimeRec = RECORD
		year:					INTEGER;
		month:					INTEGER;
		day:					INTEGER;
		hour:					INTEGER;
		minute:					INTEGER;
		second:					INTEGER;
		dayOfWeek:				INTEGER;
	END;

PROCEDURE GetTime(var d: DateTimeRec);
external 'InterfaceLib';

PROCEDURE SetTime(const d: DateTimeRec);
external 'InterfaceLib';

PROCEDURE DateToSeconds({CONST}VAR d: DateTimeRec; VAR secs: UInt32);
external 'InterfaceLib';

PROCEDURE SecondsToDate(secs: UInt32; VAR d: DateTimeRec);
external 'InterfaceLib';

{************** from TextUtils ***************}

PROCEDURE UpperString(VAR theString: Str255; diacSensitive: BOOLEAN);
external 'InterfaceLib';

{************** from others ***************}

procedure ExitToShell;
external 'InterfaceLib';

procedure SysBeep(dur: Integer);
external 'InterfaceLib';

function TickCount: Longint;
external 'InterfaceLib';

function Munger (h: Mac_Handle; offset: LONGINT; ptr1: Mac_Ptr;
  len1: LONGINT; ptr2: Mac_Ptr; len2: LONGINT): LONGINT;
external 'InterfaceLib';


{************** misc MPW support routines ***************}

FUNCTION ResolveFolderAliases (volume: INTEGER; directory: LONGINT;
								     path: StringPtr; resolveLeafName: BOOLEAN;
								     VAR theSpec: FSSpec; VAR isFolder, hadAlias,
								     leafIsAlias: BOOLEAN): OSErr;
external 'InterfaceLib'; {??}
{    ...from CIncludes:IntEnv.h }

{$PACKRECORDS NORMAL}

