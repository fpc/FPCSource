{
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

{For a future possiblity of compiling the RTL under Carbon,
 sections containing Carbon only code should be under
 ifdef TARGET_API_MAC_CARBON, at the moment it is always undefinded: }

{$UNDEF TARGET_API_MAC_CARBON }

{$IFDEF TARGET_API_MAC_CARBON }
{$DEFINE OPAQUE_TOOLBOX_STRUCTS }
{$ENDIF}

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

  IntegerPtr              = ^INTEGER;
  LongIntPtr              = ^LONGINT;

  OSErr = Integer;

  FourCharCode = Longword;

  OSType = FourCharCode;
  ResType = FourCharCode;
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
  {   Gestalt selector and values for the Appearance Manager   }
  gestaltAliasMgrAttr      = 'alis';            {  Alias Mgr Attributes  }
  gestaltAliasMgrPresent    = 0;              {  True if the Alias Mgr is present  }
  gestaltAliasMgrSupportsRemoteAppletalk = 1;          {  True if the Alias Mgr knows about Remote Appletalk  }
  gestaltAliasMgrSupportsAOCEKeychain = 2;          {  True if the Alias Mgr knows about the AOCE Keychain  }
  gestaltAliasMgrResolveAliasFileWithMountOptions = 3;    {  True if the Alias Mgr implements gestaltAliasMgrResolveAliasFileWithMountOptions() and IsAliasFile()  }
  gestaltAliasMgrFollowsAliasesWhenResolving = 4;
  gestaltAliasMgrSupportsExtendedCalls = 5;
  gestaltAliasMgrSupportsFSCalls = 6;              {  true if Alias Mgr supports HFS+ Calls  }

  gestaltAppearanceAttr    = 'appr';
  gestaltAppearanceExists    = 0;
  gestaltAppearanceCompatMode  = 1;

  {   Gestalt selector for determining Appearance Manager version     }
  {   If this selector does not exist, but gestaltAppearanceAttr      }
  {   does, it indicates that the 1.0 version is installed. This      }
  {   gestalt returns a BCD number representing the version of the    }
  {   Appearance Manager that is currently running, e.g. 0x0101 for   }
  {   version 1.0.1.                                                  }
  gestaltAppearanceVersion  = 'apvr';

  gestaltCFMAttr        = 'cfrg';            {  Selector for information about the Code Fragment Manager  }
  gestaltCFMPresent      = 0;              {  True if the Code Fragment Manager is present  }
  gestaltCFMPresentMask    = $0001;
  gestaltCFM99Present      = 2;              {  True if the CFM-99 features are present.  }
  gestaltCFM99PresentMask    = $0004;

  gestaltAppleEventsAttr    = 'evnt';            {  Apple Events attributes  }
  gestaltAppleEventsPresent  = 0;              {  True if Apple Events present  }
  gestaltScriptingSupport    = 1;
  gestaltOSLInSystem      = 2;              {  OSL is in system so don’t use the one linked in to app  }

  gestaltFindFolderAttr    = 'fold';            {  Folder Mgr attributes  }
  gestaltFindFolderPresent  = 0;              {  True if Folder Mgr present  }
  gestaltFolderDescSupport  = 1;              {  True if Folder Mgr has FolderDesc calls  }
  gestaltFolderMgrFollowsAliasesWhenResolving = 2;      {  True if Folder Mgr follows folder aliases  }
  gestaltFolderMgrSupportsExtendedCalls = 3;          {  True if Folder Mgr supports the Extended calls  }
  gestaltFolderMgrSupportsDomains = 4;            {  True if Folder Mgr supports domains for the first parameter to FindFolder  }
  gestaltFolderMgrSupportsFSCalls = 5;            {  True if FOlder manager supports __FindFolderFSRef & __FindFolderExtendedFSRef  }

  gestaltFPUType        = 'fpu ';            {  fpu type  }
  gestaltNoFPU        = 0;              {  no FPU  }
  gestalt68881        = 1;              {  68881 FPU  }
  gestalt68882        = 2;              {  68882 FPU  }
  gestalt68040FPU        = 3;              {  68040 built-in FPU  }

  gestaltFSAttr        = 'fs  ';            {  file system attributes  }
  gestaltFullExtFSDispatching  = 0;              {  has really cool new HFSDispatch dispatcher  }
  gestaltHasFSSpecCalls    = 1;              {  has FSSpec calls  }
  gestaltHasFileSystemManager  = 2;              {  has a file system manager  }
  gestaltFSMDoesDynamicLoad  = 3;              {  file system manager supports dynamic loading  }
  gestaltFSSupports4GBVols  = 4;              {  file system supports 4 gigabyte volumes  }
  gestaltFSSupports2TBVols  = 5;              {  file system supports 2 terabyte volumes  }
  gestaltHasExtendedDiskInit  = 6;              {  has extended Disk Initialization calls  }
  gestaltDTMgrSupportsFSM    = 7;              {  Desktop Manager support FSM-based foreign file systems  }
  gestaltFSNoMFSVols      = 8;              {  file system doesn't supports MFS volumes  }
  gestaltFSSupportsHFSPlusVols = 9;              {  file system supports HFS Plus volumes  }
  gestaltFSIncompatibleDFA82  = 10;              {  VCB and FCB structures changed; DFA 8.2 is incompatible  }

  gestaltHasHFSPlusAPIs    = 12;              {  file system supports HFS Plus APIs  }
  gestaltMustUseFCBAccessors  = 13;              {  FCBSPtr and FSFCBLen are invalid - must use FSM FCB accessor functions }
  gestaltFSUsesPOSIXPathsForConversion = 14;          {  The path interchange routines operate on POSIX paths instead of HFS paths  }

  gestaltOSAttr        = 'os  ';            {  o/s attributes  }
  gestaltSysZoneGrowable    = 0;              {  system heap is growable  }
  gestaltLaunchCanReturn    = 1;              {  can return from launch  }
  gestaltLaunchFullFileSpec  = 2;              {  can launch from full file var spec  }
  gestaltLaunchControl    = 3;              {  launch control support available  }
  gestaltTempMemSupport    = 4;              {  temp memory support  }
  gestaltRealTempMemory    = 5;              {  temp memory handles are real  }
  gestaltTempMemTracked    = 6;              {  temporary memory handles are tracked  }
  gestaltIPCSupport      = 7;              {  IPC support is present  }
  gestaltSysDebuggerSupport  = 8;              {  system debugger support is present  }
  gestaltNativeProcessMgrBit  = 19;              {  the process manager itself is native  }

  gestaltQuickdrawVersion    = 'qd  ';            {  quickdraw version  }
  gestaltOriginalQD      = $0000;            {  original 1-bit QD  }
  gestalt8BitQD        = $0100;            {  8-bit color QD  }
  gestalt32BitQD        = $0200;            {  32-bit color QD  }
  gestalt32BitQD11      = $0201;            {  32-bit color QDv1.1  }
  gestalt32BitQD12      = $0220;            {  32-bit color QDv1.2  }
  gestalt32BitQD13      = $0230;            {  32-bit color QDv1.3  }
  gestaltAllegroQD      = $0250;            {  Allegro QD OS 8.5  }
  gestaltMacOSXQD        = $0300;            {  Mac OS X QD  }
  gestaltScriptMgrVersion    = 'scri';            {  Script Manager version number      }
  gestaltScriptCount      = 'scr#';            {  number of active script systems    }

  gestaltStandardFileAttr    = 'stdf';            {  Standard File attributes  }
  gestaltStandardFile58    = 0;              {  True if selectors 5-8 (StandardPutFile-CustomGetFile) are supported  }
  gestaltStandardFileTranslationAware = 1;          {  True if standard file is translation manager aware  }
  gestaltStandardFileHasColorIcons = 2;            {  True if standard file has 16x16 color icons  }
  gestaltStandardFileUseGenericIcons = 3;            {  Standard file LDEF to use only the system generic icons if true  }
  gestaltStandardFileHasDynamicVolumeAllocation = 4;      {  True if standard file supports more than 20 volumes  }

  gestaltSystemVersion    = 'sysv';            {  system version }

  gestaltThreadMgrAttr    = 'thds';            {  Thread Manager attributes  }
  gestaltThreadMgrPresent    = 0;              {  bit true if Thread Mgr is present  }
  gestaltSpecificMatchSupport  = 1;              {  bit true if Thread Mgr supports exact match creation option  }
  gestaltThreadsLibraryPresent = 2;              {  bit true if Thread Mgr shared library is present  }


{************** from OSUtils.p ***************}

type
  QElemPtr = ^QElem;

  QElem = record
    qLink: QElemPtr;
    qType: INTEGER;
    qData: array[0..0] of INTEGER;
   end;

CONST
  curSysEnvVers        = 2;              { Updated to equal latest SysEnvirons version }

TYPE
  SysEnvRecPtr = ^SysEnvRec;
  SysEnvRec = RECORD
    environsVersion:    INTEGER;
    machineType:      INTEGER;
    systemVersion:      INTEGER;
    processor:        INTEGER;
    hasFPU:          BOOLEAN;
    hasColorQD:        BOOLEAN;
    keyBoardType:      INTEGER;
    atDrvrVersNum:      INTEGER;
    sysVRefNum:        INTEGER;
  END;

FUNCTION SysEnvirons(versionRequested: INTEGER; VAR theWorld: SysEnvRec): OSErr;
external 'InterfaceLib';

{************** from Finder.p ***************}

 type
  FInfo = record
    fdType: OSType;    {the type of the file}
    fdCreator: OSType;    {file's creator}
    fdFlags: INTEGER;    {flags ex. hasbundle,invisible,locked, etc.}
    fdLocation: Point;    {file's location in folder}
    fdFldr: INTEGER;    {folder containing file}
   end;

  FXInfo = record
    fdIconID: INTEGER;    {Icon ID}
    fdUnused: array[0..2] of INTEGER;  {unused but reserved 6 bytes}
    fdScript: SInt8;    {Script flag and number}
    fdXFlags: SInt8;    {More flag bits}
    fdComment: INTEGER;    {Comment ID}
    fdPutAway: LONGINT;    {Home Dir ID}
   end;

  DInfo = record
    frRect: Rect;    {folder rect}
    frFlags: INTEGER;    {Flags}
    frLocation: Point;    {folder location}
    frView: INTEGER;    {folder view}
   end;

  DXInfo = record
    frScroll: Point;    {scroll position}
    frOpenChain: LONGINT;  {DirID chain of open folders}
    frScript: SInt8;    {Script flag and number}
    frXFlags: SInt8;    {More flag bits}
    frComment: INTEGER;    {comment}
    frPutAway: LONGINT;    {DirID}
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
  ioDirFlg = 4;      { see IM IV-125 }
  ioDirMask = $10;
  fsRtParID = 1;
  fsRtDirID = 2;

type
  CatPositionRecPtr = ^CatPositionRec;
  CatPositionRec = RECORD
    initialize:        LONGINT;
    priv:          ARRAY [1..6] OF INTEGER;
  END;

  FSSpec = record
      vRefNum: Integer;
      parID: Longint;
      name: Str63;
   end;
  FSSpecPtr = ^FSSpec;

  ParmBlkPtr              = ^ParamBlockRec;

  IOCompletionUPP = UniversalProcPtr;

  ParamBlockRecPtr = ^ParamBlockRec;
  ParamBlockRec = RECORD
    qLink:          QElemPtr;                { queue link in header }
    qType:          INTEGER;                { type byte for safety check }
    ioTrap:          INTEGER;                { FS: the Trap }
    ioCmdAddr:        Mac_Ptr;                  { FS: address to dispatch to }
    ioCompletion:      IOCompletionUPP;            { completion routine addr (0 for synch calls) }
    ioResult:        OSErr;                  { result code }
    ioNamePtr:        StringPtr;                { ptr to Vol:FileName string }
    ioVRefNum:        INTEGER;                { volume refnum (DrvNum for Eject and MountVol) }
    CASE INTEGER OF
    0: (
      ioRefNum:      INTEGER;                { refNum for I/O operation }
      ioVersNum:      SInt8;                  { version number }
      ioPermssn:      SInt8;                  { Open: permissions (byte) }
      ioMisc:        Mac_Ptr;                  { Rename: new name (GetEOF,SetEOF: logical end of file) (Open: optional ptr to buffer) (SetFileType: new type) }
      ioBuffer:      Mac_Ptr;                  { data buffer Ptr }
      ioReqCount:      LONGINT;                { requested byte count; also = ioNewDirID }
      ioActCount:      LONGINT;                { actual byte count completed }
      ioPosMode:      INTEGER;                { initial file positioning }
      ioPosOffset:    LONGINT;                { file position offset }
       );
    1: (
      ioFRefNum:      INTEGER;                { reference number }
      ioFVersNum:      SInt8;                  { version number }
      filler1:      SInt8;
      ioFDirIndex:    INTEGER;                { GetFInfo directory index }
      ioFlAttrib:      SInt8;                  { GetFInfo: in-use bit=7, lock bit=0 }
      ioFlVersNum:    SInt8;                  { file version number }
      ioFlFndrInfo:    FInfo;                  { user info }
      ioFlNum:      UInt32;                  { GetFInfo: file number; TF- ioDirID }
      ioFlStBlk:      UInt16;                  { start file block (0 if none) }
      ioFlLgLen:      LONGINT;                { logical length (EOF) }
      ioFlPyLen:      LONGINT;                { physical length }
      ioFlRStBlk:      UInt16;                  { start block rsrc fork }
      ioFlRLgLen:      LONGINT;                { file logical length rsrc fork }
      ioFlRPyLen:      LONGINT;                { file physical length rsrc fork }
      ioFlCrDat:      UInt32;                  { file creation date& time (32 bits in secs) }
      ioFlMdDat:      UInt32;                  { last modified date and time }
       );
    2: (
      filler2:      LONGINT;
      ioVolIndex:      INTEGER;                { volume index number }
      ioVCrDate:      UInt32;                  { creation date and time }
      ioVLsBkUp:      UInt32;                  { last backup date and time }
      ioVAtrb:      UInt16;                  { volume attrib }
      ioVNmFls:      UInt16;                  { number of files in directory }
      ioVDirSt:      UInt16;                  { start block of file directory }
      ioVBlLn:      INTEGER;                { GetVolInfo: length of dir in blocks }
      ioVNmAlBlks:    UInt16;                  { for compatibilty ioVNmAlBlks * ioVAlBlkSiz <= 2 GB }
      ioVAlBlkSiz:    UInt32;                  { for compatibilty ioVAlBlkSiz is <= $0000FE00 (65,024) }
      ioVClpSiz:      UInt32;                  { GetVolInfo: bytes to allocate at a time }
      ioAlBlSt:      UInt16;                  { starting disk(512-byte) block in block map }
      ioVNxtFNum:      UInt32;                  { GetVolInfo: next free file number }
      ioVFrBlk:      UInt16;                  { GetVolInfo: # free alloc blks for this vol }
       );
    3: (
      ioCRefNum:      INTEGER;                { refNum for I/O operation }
      csCode:        INTEGER;                { word for control status code }
      csParam:      ARRAY [0..10] OF INTEGER;        { operation-defined parameters }
       );
    4: (
      ioSRefNum:      INTEGER;
      ioSVersNum:      SInt8;
      ioSPermssn:      SInt8;
      ioSMix:        Mac_Ptr;
      ioSFlags:      INTEGER;
      ioSlot:        SInt8;
      ioID:        SInt8;
       );
    5: (
      ioMRefNum:      INTEGER;
      ioMVersNum:      SInt8;
      ioMPermssn:      SInt8;
      ioMMix:        Mac_Ptr;
      ioMFlags:      INTEGER;
      ioSEBlkPtr:      Mac_Ptr;
       );
  END;

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

  DTPBRecPtr = ^DTPBRec;
  DTPBRec = RECORD
    qLink:          QElemPtr;                { queue link in header }
    qType:          INTEGER;                { type byte for safety check }
    ioTrap:          INTEGER;                { FS: the Trap }
    ioCmdAddr:        Mac_Ptr;                  { FS: address to dispatch to }
    ioCompletion:      IOCompletionUPP;            { completion routine addr (0 for synch calls) }
    ioResult:        OSErr;                  { result code }
    ioNamePtr:        StringPtr;                { ptr to Vol:FileName string }
    ioVRefNum:        INTEGER;                { volume refnum (DrvNum for Eject and MountVol) }
    ioDTRefNum:        INTEGER;                {  desktop refnum  }
    ioIndex:        INTEGER;
    ioTagInfo:        LONGINT;
    ioDTBuffer:        Mac_Ptr;
    ioDTReqCount:      LONGINT;
    ioDTActCount:      LONGINT;
    ioFiller1:        SInt8;
    ioIconType:        SInt8;
    ioFiller2:        INTEGER;
    ioDirID:        LONGINT;
    ioFileCreator:      OSType;
    ioFileType:        OSType;
    ioFiller3:        LONGINT;
    ioDTLgLen:        LONGINT;
    ioDTPyLen:        LONGINT;
    ioFiller4:        ARRAY [1..14] OF INTEGER;
    ioAPPLParID:      LONGINT;
  END;
  DTPBPtr                = ^DTPBRec;

  HParamBlockRecPtr = ^HParamBlockRec;
  HParamBlockRec = RECORD
    qLink:          QElemPtr;                { queue link in header }
    qType:          INTEGER;                { type byte for safety check }
    ioTrap:          INTEGER;                { FS: the Trap }
    ioCmdAddr:        pointer;                  { FS: address to dispatch to }
    ioCompletion:      IOCompletionUPP;            { completion routine addr (0 for synch calls) }
    ioResult:        OSErr;                  { result code }
    ioNamePtr:        StringPtr;                { ptr to Vol:FileName string }
    ioVRefNum:        INTEGER;                { volume refnum (DrvNum for Eject and MountVol) }
    CASE INTEGER OF
    0: (
      ioRefNum:      INTEGER;
      ioVersNum:      SInt8;
      ioPermssn:      SInt8;
      ioMisc:        pointer;
      ioBuffer:      pointer;
      ioReqCount:      LONGINT;
      ioActCount:      LONGINT;
      ioPosMode:      INTEGER;
      ioPosOffset:    LONGINT;
       );
    1: (
      ioFRefNum:      INTEGER;
      ioFVersNum:      SInt8;
      filler1:      SInt8;
      ioFDirIndex:    INTEGER;
      ioFlAttrib:      SInt8;
      ioFlVersNum:    SInt8;
      ioFlFndrInfo:    FInfo;
      ioDirID:      LONGINT;
      ioFlStBlk:      UInt16;
      ioFlLgLen:      LONGINT;
      ioFlPyLen:      LONGINT;
      ioFlRStBlk:      UInt16;
      ioFlRLgLen:      LONGINT;
      ioFlRPyLen:      LONGINT;
      ioFlCrDat:      UInt32;
      ioFlMdDat:      UInt32;
       );
    2: (
      filler2:      LONGINT;
      ioVolIndex:      INTEGER;
      ioVCrDate:      UInt32;
      ioVLsMod:      UInt32;
      ioVAtrb:      INTEGER;
      ioVNmFls:      UInt16;
      ioVBitMap:      UInt16;
      ioAllocPtr:      UInt16;
      ioVNmAlBlks:    UInt16;
      ioVAlBlkSiz:    UInt32;
      ioVClpSiz:      UInt32;
      ioAlBlSt:      UInt16;
      ioVNxtCNID:      UInt32;
      ioVFrBlk:      UInt16;
      ioVSigWord:      UInt16;
      ioVDrvInfo:      INTEGER;
      ioVDRefNum:      INTEGER;
      ioVFSID:      INTEGER;
      ioVBkUp:      UInt32;
      ioVSeqNum:      UInt16;
      ioVWrCnt:      UInt32;
      ioVFilCnt:      UInt32;
      ioVDirCnt:      UInt32;
      ioVFndrInfo:    ARRAY [1..8] OF LONGINT;
       );
    3: (
      filler3:      INTEGER;
      ioDenyModes:    INTEGER;                { access rights data }
      filler4:      INTEGER;
      filler5:      SInt8;
      ioACUser:      SInt8;                  { access rights for directory only }
      filler6:      LONGINT;
      ioACOwnerID:    LONGINT;                { owner ID }
      ioACGroupID:    LONGINT;                { group ID }
      ioACAccess:      LONGINT;                { access rights }
       );
    4: (
      filler7:      INTEGER;
      ioObjType:      INTEGER;                { function code }
      ioObjNamePtr:    StringPtr;                { ptr to returned creator/group name }
      ioObjID:      LONGINT;                { creator/group ID }
       );
    5: (
      ioDstVRefNum:    INTEGER;                { destination vol identifier }
      filler8:      INTEGER;
      ioNewName:      StringPtr;                { ptr to destination pathname }
      ioCopyName:      StringPtr;                { ptr to optional name }
      ioNewDirID:      LONGINT;                { destination directory ID }
       );
    6: (
      ioWDCreated:    INTEGER;
      ioWDIndex:      INTEGER;
      ioWDProcID:      LONGINT;
      ioWDVRefNum:    INTEGER;
      filler10:      INTEGER;
      filler11:      LONGINT;
      filler12:      LONGINT;
      filler13:      LONGINT;
      ioWDDirID:      LONGINT;
       );
    7: (
      filler14:      LONGINT;
      ioDestNamePtr:    StringPtr;                {  dest file name  }
      filler15:      LONGINT;
      ioDestDirID:    LONGINT;                {  dest file's directory id  }
      filler16:      LONGINT;
      filler17:      LONGINT;
      ioSrcDirID:      LONGINT;                {  source file's directory id  }
      filler18:      INTEGER;
      ioFileID:      LONGINT;                {  file ID  }
       );
    8: (
      ioMatchPtr:      FSSpecPtr;                {  match array  }
      ioReqMatchCount:  LONGINT;                {  maximum allowable matches  }
      ioActMatchCount:  LONGINT;                {  actual match count  }
      ioSearchBits:    LONGINT;                {  search criteria selector  }
      ioSearchInfo1:    CInfoPBPtr;                {  search values and range lower bounds  }
      ioSearchInfo2:    CInfoPBPtr;                {  search values and range upper bounds  }
      ioSearchTime:    LONGINT;                {  length of time to run search  }
      ioCatPosition:    CatPositionRec;              {  current position in the catalog  }
      ioOptBuffer:    pointer;                  {  optional performance enhancement buffer  }
      ioOptBufSize:    LONGINT;                {  size of buffer pointed to by ioOptBuffer  }
       );
    9: (
      ioFiller21:      LONGINT;
      ioFiller22:      LONGINT;
      ioForeignPrivBuffer: pointer;
      ioForeignPrivActCount: LONGINT;
      ioForeignPrivReqCount: LONGINT;
      ioFiller23:      LONGINT;
      ioForeignPrivDirID:  LONGINT;
      ioForeignPrivInfo1:  LONGINT;
      ioForeignPrivInfo2:  LONGINT;
      ioForeignPrivInfo3:  LONGINT;
      ioForeignPrivInfo4:  LONGINT;
       );
  END;

  HParmBlkPtr              = ^HParamBlockRec;


  WDPBRecPtr = ^WDPBRec;
  WDPBRec = RECORD
    qLink:          QElemPtr;
    qType:          INTEGER;
    ioTrap:          INTEGER;
    ioCmdAddr:        pointer;
    ioCompletion:      IOCompletionUPP;
    ioResult:        OSErr;
    ioNamePtr:        StringPtr;
    ioVRefNum:        INTEGER;
    filler1:        INTEGER;
    ioWDIndex:        INTEGER;
    ioWDProcID:        LONGINT;
    ioWDVRefNum:      INTEGER;
    filler2:        ARRAY [1..7] OF INTEGER;
    ioWDDirID:        LONGINT;
  END;

  WDPBPtr                = ^WDPBRec;

FUNCTION PBGetVInfoSync(paramBlock: ParmBlkPtr): OSErr;
external 'InterfaceLib';

FUNCTION GetVol(volName: StringPtr; VAR vRefNum: INTEGER): OSErr;
external 'InterfaceLib';

function FSpOpenDF(var spec: FSSpec; permission: SignedByte;
  var refNum: Integer): OSErr;
external 'InterfaceLib';

function FSpCreate(var spec: FSSpec; creator, fileType: OSType;
  scriptTag: ScriptCode): OSErr;
external 'InterfaceLib';

function FSpDirCreate(var spec: FSSpec; scriptTag: ScriptCode;
  var createdDirID: Longint): OSErr;
external 'InterfaceLib';

function FSpDelete(var spec: FSSpec): OSErr;
external 'InterfaceLib';

FUNCTION FSpGetFInfo(var spec: FSSpec; VAR fndrInfo: FInfo): OSErr;
external 'InterfaceLib';

FUNCTION FSpSetFInfo(var spec: FSSpec; var fndrInfo: FInfo): OSErr;
external 'InterfaceLib';

FUNCTION FSpSetFLock(var spec: FSSpec): OSErr;
external 'InterfaceLib';

FUNCTION FSpRstFLock(var spec: FSSpec): OSErr;
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

FUNCTION HGetFInfo(vRefNum: INTEGER; dirID: LONGINT; fileName: Str255;
  VAR fndrInfo: FInfo): OSErr;
external 'InterfaceLib';

FUNCTION PBDTGetPath(paramBlock: DTPBPtr): OSErr;
external 'InterfaceLib';

FUNCTION PBDTGetAPPLSync(paramBlock: DTPBPtr): OSErr;
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
  kOnSystemDisk        = -32768;            {  previously was 0x8000 but that is an unsigned value whereas vRefNum is signed }
  kOnAppropriateDisk      = -32767;            {  Generally, the same as kOnSystemDisk, but it's clearer that this isn't always the 'boot' disk. }
                                {  Folder Domains - Carbon only.  The constants above can continue to be used, but the folder/volume returned will }
                                {  be from one of the domains below. }
  kSystemDomain        = -32766;            {  Read-only system hierarchy. }
  kLocalDomain        = -32765;            {  All users of a single machine have access to these resources. }
  kNetworkDomain        = -32764;            {  All users configured to use a common network server has access to these resources. }
  kUserDomain          = -32763;            {  Read/write. Resources that are private to the user. }
  kClassicDomain        = -32762;            {  Domain referring to the currently configured Classic System Folder }

  kCreateFolder        = true;
  kDontCreateFolder      = false;

  kSystemFolderType      = 'macs';            {  the system folder  }
  kDesktopFolderType      = 'desk';            {  the desktop folder; objects in this folder show on the desk top.  }
  kSystemDesktopFolderType  = 'sdsk';            {  the desktop folder at the root of the hard drive, never the redirected user desktop folder  }
  kTrashFolderType      = 'trsh';            {  the trash folder; objects in this folder show up in the trash  }
  kSystemTrashFolderType    = 'strs';            {  the trash folder at the root of the drive, never the redirected user trash folder  }
  kWhereToEmptyTrashFolderType = 'empt';            {  the "empty trash" folder; Finder starts empty from here down  }
  kPrintMonitorDocsFolderType  = 'prnt';            {  Print Monitor documents  }
  kStartupFolderType      = 'strt';            {  Finder objects (applications, documents, DAs, aliases, to...) to open at startup go here  }
  kShutdownFolderType      = 'shdf';            {  Finder objects (applications, documents, DAs, aliases, to...) to open at shutdown go here  }
  kAppleMenuFolderType    = 'amnu';            {  Finder objects to put into the Apple menu go here  }
  kControlPanelFolderType    = 'ctrl';            {  Control Panels go here (may contain INITs)  }
  kSystemControlPanelFolderType = 'sctl';            {  System control panels folder - never the redirected one, always "Control Panels" inside the System Folder  }
  kExtensionFolderType    = 'extn';            {  System extensions go here  }
  kFontsFolderType      = 'font';            {  Fonts go here  }
  kPreferencesFolderType    = 'pref';            {  preferences for applications go here  }
  kSystemPreferencesFolderType = 'sprf';            {  System-type Preferences go here - this is always the system's preferences folder, never a logged in user's  }
  kTemporaryFolderType    = 'temp';            {  temporary files go here (deleted periodically, but don't rely on it.)  }

FUNCTION FindFolder(vRefNum: INTEGER; folderType: OSType; createFolder: BOOLEAN; VAR foundVRefNum: INTEGER; VAR foundDirID: LONGINT): OSErr;
external 'InterfaceLib';

{************** from Events.p ***************}

TYPE
  EventKind              = UInt16;
  EventMask              = UInt16;
  EventModifiers            = UInt16;

TYPE
  EventRecordPtr = ^EventRecord;
  EventRecord = RECORD
    what:          EventKind;
    message:        UInt32;
    when:          UInt32;
    where:          Point;
    modifiers:        EventModifiers;
  END;

{************** from Processes.p ***************}

type
  ProcessSerialNumberPtr = ^ProcessSerialNumber;
  ProcessSerialNumber = record
    highLongOfPSN: LONGINT;
    lowLongOfPSN: LONGINT;
   end;

const
{ Process identifier - Various reserved process serial numbers }
  kNoProcess = 0;
  kSystemProcess = 1;
  kCurrentProcess = 2;

TYPE
  LaunchFlags              = UInt16;

CONST
  launchContinue        = $4000;
  launchNoFileFlags      = $0800;
  launchUseMinimum      = $0400;
  launchDontSwitch      = $0200;
  launchAllow24Bit      = $0100;
  launchInhibitDaemon      = $0080;

  {   Format for first AppleEvent to pass to new process.  The size of the overall
    buffer variable: the message body immediately follows the messageLength   }

TYPE
  AppParametersPtr = ^AppParameters;
  AppParameters = RECORD
    theMsgEvent:      EventRecord;
    eventRefCon:      UInt32;
    messageLength:      UInt32;
  END;

  {   Parameter block to _Launch   }
  LaunchParamBlockRecPtr = ^LaunchParamBlockRec;
  LaunchParamBlockRec = RECORD
    reserved1:        UInt32;
    reserved2:        UInt16;
    launchBlockID:      UInt16;
    launchEPBLength:    UInt32;
    launchFileFlags:    UInt16;
    launchControlFlags:    LaunchFlags;
    launchAppSpec:      FSSpecPtr;
    launchProcessSN:    ProcessSerialNumber;
    launchPreferredSize:  UInt32;
    launchMinimumSize:    UInt32;
    launchAvailableSize:  UInt32;
    launchAppParameters:  AppParametersPtr;
  END;

  LaunchPBPtr              = ^LaunchParamBlockRec;

CONST
  extendedBlock        = $4C43;            {  'LC'  }
  extendedBlockLen      = 32;

TYPE
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

FUNCTION LaunchApplication(LaunchParams: LaunchPBPtr): OSErr;
external 'InterfaceLib';

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

                                { Process Manager errors }
  procNotFound        = -600;              { no eligible process with specified descriptor }
  memFragErr          = -601;              { not enough room to launch app w/special requirements }
  appModeErr          = -602;              { memory mode is 32-bit, but app not 32-bit clean }
  protocolErr          = -603;              { app made module calls in improper order }
  hardwareConfigErr      = -604;              { hardware configuration not correct for call }
  appMemFullErr        = -605;              { application SIZE not big enough for launch }
  appIsDaemon          = -606;              { app is BG-only, and launch flags disallow this }
  bufferIsSmall        = -607;              { error returns from Post and Accept  }
  noOutstandingHLE      = -608;
  connectionInvalid      = -609;
  noUserInteractionAllowed  = -610;              {  no user interaction allowed  }

  diffVolErr = -1303; { files on different volumes }
  catChangedErr= -1304; { the catalog has been modified }
  afpAccessDenied= -5000; {  Insufficient access privileges for operation  }
  afpDenyConflict= -5006; {  Specified open/deny modes conflict with current open modes  }
  afpItemNotFound  = -5012;{  Unknown UserName/UserID or missing comment/APPL entry  }
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
    year:          INTEGER;
    month:          INTEGER;
    day:          INTEGER;
    hour:          INTEGER;
    minute:          INTEGER;
    second:          INTEGER;
    dayOfWeek:        INTEGER;
  END;

PROCEDURE GetTime(var d: DateTimeRec);
external 'InterfaceLib';

PROCEDURE SetTime(var d: DateTimeRec);
//PROCEDURE SetTime(const d: DateTimeRec);
external 'InterfaceLib';

PROCEDURE DateToSeconds({CONST}VAR d: DateTimeRec; VAR secs: UInt32);
external 'InterfaceLib';

PROCEDURE SecondsToDate(secs: UInt32; VAR d: DateTimeRec);
external 'InterfaceLib';

{************** from TextUtils ***************}

PROCEDURE UpperString(VAR theString: Str255; diacSensitive: BOOLEAN);
external 'InterfaceLib';

{************** from Quickdraw ***************}

TYPE
  Bits16                = ARRAY [0..15] OF INTEGER;

  PatternPtr = ^Pattern;
  Pattern = RECORD
    pat:          PACKED ARRAY [0..7] OF UInt8;
  END;

  PatPtr                = ^Pattern;
  BitMapPtr = ^BitMap;
  BitMap = RECORD
    baseAddr:        pointer;
    rowBytes:        INTEGER;
    bounds:          Rect;
  END;
  BitMapHandle = ^BitMapPtr;

  CursorPtr = ^Cursor;
  Cursor = RECORD
    data:          Bits16;
    mask:          Bits16;
    hotSpot:        Point;
  END;
  CursPtr = ^Cursor;

  GrafPtr = pointer; //TODO Should actually point to a GrafPort.

  QDGlobalsPtr = ^QDGlobals;
  QDGlobals = RECORD
    privates:        PACKED ARRAY [0..75] OF CHAR;
    randSeed:        LONGINT;                {  in Carbon use GetQDGlobalsRandomSeed }
    screenBits:        BitMap;                  {  in Carbon use GetQDGlobalsScreenBits }
    arrow:          Cursor;                  {  in Carbon use GetQDGlobalsArrow }
    dkGray:          Pattern;                {  in Carbon use GetQDGlobalsDarkGray }
    ltGray:          Pattern;                {  in Carbon use GetQDGlobalsLightGray }
    gray:          Pattern;                {  in Carbon use GetQDGlobalsGray }
    black:          Pattern;                {  in Carbon use GetQDGlobalsBlack }
    white:          Pattern;                {  in Carbon use GetQDGlobalsWhite }
    thePort:        GrafPtr;                {  in Carbon use GetQDGlobalsThePort }
  END;

  QDGlobalsHdl            = ^QDGlobalsPtr;

PROCEDURE InitGraf(globalPtr: Mac_Ptr);
external 'InterfaceLib';

{************** from Fonts ***************}

PROCEDURE SetFScaleDisable(fscaleDisable: BOOLEAN);
external 'InterfaceLib';

{************** from AEDataModel ***************}

{ Apple event descriptor types }

CONST
  typeBoolean          = 'bool';
  typeChar          = 'TEXT';

  {   Preferred numeric Apple event descriptor types   }
  typeSInt16          = 'shor';
  typeSInt32          = 'long';
  typeUInt32          = 'magn';
  typeSInt64          = 'comp';
  typeIEEE32BitFloatingPoint  = 'sing';
  typeIEEE64BitFloatingPoint  = 'doub';
  type128BitFloatingPoint    = 'ldbl';
  typeDecimalStruct      = 'decm';

  {   Non-preferred Apple event descriptor types   }
  typeSMInt          = 'shor';
  typeShortInteger      = 'shor';
  typeInteger          = 'long';
  typeLongInteger        = 'long';
  typeMagnitude        = 'magn';
  typeComp          = 'comp';
  typeSMFloat          = 'sing';
  typeShortFloat        = 'sing';
  typeFloat          = 'doub';
  typeLongFloat        = 'doub';
  typeExtended        = 'exte';
  typeApplSignature      = 'sign';

  {   Constants used creating an AppleEvent   }
                                {  Constant for the returnID param of AECreateAppleEvent  }
  kAutoGenerateReturnID    = -1;              {  AECreateAppleEvent will generate a session-unique ID  }
                                {  Constant for transaction ID’s  }
  kAnyTransactionID      = 0;              {  no transaction is in use  }

  {   Apple event manager data types   }

TYPE
  DescType              = ResType;
  AEKeyword              = FourCharCode;
{$IFDEF OPAQUE_TOOLBOX_STRUCTS }
  AEDataStorage    = ^LONGINT; { an opaque 32-bit type }
  AEDataStoragePtr = ^AEDataStorage;  { when a VAR xx:AEDataStorage parameter can be nil, it is changed to xx: AEDataStoragePtr }
{$ELSE}
  AEDataStorage            = Mac_Handle;
{$ENDIF OPAQUE_TOOLBOX_STRUCTS}

  AEDescPtr = ^AEDesc;
  AEDesc = RECORD
    descriptorType:      DescType;
    dataHandle:        AEDataStorage;
  END;

  AEKeyDescPtr = ^AEKeyDesc;
  AEKeyDesc = RECORD
    descKey:        AEKeyword;
    descContent:      AEDesc;
  END;

{   a list of AEDesc's is a special kind of AEDesc   }
  AEDescList              = AEDesc;
  AEDescListPtr             = ^AEDescList;
  {   AERecord is a list of keyworded AEDesc's   }
  AERecord              = AEDescList;
  AERecordPtr             = ^AERecord;
  {   an AEDesc which contains address data   }
  AEAddressDesc            = AEDesc;
  AEAddressDescPtr           = ^AEAddressDesc;
{   an AERecord that contains an AppleEvent, and related data types   }
  AppleEvent              = AERecord;
  AppleEventPtr             = ^AppleEvent;
  AEReturnID              = SInt16;
  AETransactionID            = SInt32;
  AEEventClass            = FourCharCode;
  AEEventID              = FourCharCode;
  AEArrayType              = SInt8;

TYPE
  AESendPriority         = SInt16;
CONST
  kAENormalPriority      = $00000000;          {  post message at the end of the event queue  }
  kAEHighPriority        = $00000001;          {  post message at the front of the event queue (same as nAttnMsg)  }

TYPE
  AESendMode           = SInt32;

CONST
  kAENoReply          = $00000001;          {  sender doesn't want a reply to event  }
  kAEQueueReply        = $00000002;          {  sender wants a reply but won't wait  }
  kAEWaitReply        = $00000003;          {  sender wants a reply and will wait  }
  kAEDontReconnect      = $00000080;          {  don't reconnect if there is a sessClosedErr from PPCToolbox  }
  kAEWantReceipt        = $00000200;          {  (nReturnReceipt) sender wants a receipt of message  }
  kAENeverInteract      = $00000010;          {  server should not interact with user  }
  kAECanInteract        = $00000020;          {  server may try to interact with user  }
  kAEAlwaysInteract      = $00000030;          {  server should always interact with user where appropriate  }
  kAECanSwitchLayer      = $00000040;          {  interaction may switch layer  }
  kAEDontRecord        = $00001000;          {  don't record this event - available only in vers 1.0.1 and greater  }
  kAEDontExecute        = $00002000;          {  don't send the event for recording - available only in vers 1.0.1 and greater  }
  kAEProcessNonReplyEvents  = $00008000;          {  allow processing of non-reply events while awaiting synchronous AppleEvent reply  }


  {   Constants for timeout durations   }
  kAEDefaultTimeout      = -1;              {  timeout value determined by AEM  }
  kNoTimeOut          = -2;              {  wait until reply comes back, however long it takes  }


FUNCTION AECreateDesc(typeCode: DescType; dataPtr: Mac_Ptr; dataSize: Size; VAR result: AEDesc): OSErr;
external 'InterfaceLib';
FUNCTION AEDisposeDesc(VAR theAEDesc: AEDesc): OSErr;
external 'InterfaceLib';
FUNCTION AEDuplicateDesc(var theAEDesc: AEDesc; VAR result: AEDesc): OSErr;
external 'InterfaceLib';

FUNCTION AECreateAppleEvent(theAEEventClass: AEEventClass; theAEEventID: AEEventID; var target: AEAddressDesc;
    returnID: AEReturnID; transactionID: AETransactionID; VAR result: AppleEvent): OSErr;
external 'InterfaceLib';
FUNCTION AEPutParamPtr(VAR theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; typeCode: DescType; dataPtr: Mac_Ptr; dataSize: Size): OSErr;
external 'InterfaceLib';
FUNCTION AEGetParamDesc(var theAppleEvent: AppleEvent; theAEKeyword: AEKeyword; desiredType: DescType; VAR result: AEDesc): OSErr;
external 'InterfaceLib';

{************** from AEInteraction ***************}

type
  AEIdleUPP = ^LONGINT; { an opaque UPP }
  AEFilterUPP = ^LONGINT; { an opaque UPP }

FUNCTION AESend(var theAppleEvent: AppleEvent; VAR reply: AppleEvent; sendMode: AESendMode; sendPriority: AESendPriority;
    timeOutInTicks: LONGINT; idleProc: AEIdleUPP; filterProc: AEFilterUPP): OSErr;
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

PROCEDURE InitCursorCtl(newCursors: pointer);
external 'PPCToolLib';

PROCEDURE SpinCursor(increment: INTEGER);
external 'PPCToolLib';


{************** API to StdCLib in MacOS ***************}

{$ifdef MACOS_USE_STDCLIB}

{The prefix C_ or c_ is used where names conflicts with pascal
keywords and names. Suffix Ptr is added for pointer to a type.}

type
  size_t = Longint;
  off_t = Longint;
  C_int = Longint;
  C_short = Integer;
  C_long = Longint;
  C_unsigned_int = Cardinal;

var
  errno: C_int; external 'StdCLib' name 'errno';
  MacOSErr: C_short; external 'StdCLib' name 'MacOSErr';

const
  _IOFBF = $00;
  _IOLBF = $40;
  _IONBF = $04;


  O_RDONLY = $00;     // Open for reading only.
  O_WRONLY = $01;     // Open for writing only.
  O_RDWR   = $02;     // Open for reading & writing.
  O_APPEND = $08;     // Write to the end of the file.
  O_RSRC   = $10;     // Open the resource fork.
  O_ALIAS  = $20;     // Open alias file.
  O_CREAT  = $100;    // Open or create a file.
  O_TRUNC  = $200;    // Open and truncate to zero length.
  O_EXCL   = $400;    // Create file only; fail if exists.
  O_BINARY = $800;    // Open as a binary stream.
  O_NRESOLVE = $4000; // Don't resolve any aliases.


  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

  FIOINTERACTIVE = $00006602; // If device is interactive
  FIOBUFSIZE     = $00006603; // Return optimal buffer size
  FIOFNAME       = $00006604; // Return filename
  FIOREFNUM      = $00006605; // Return fs refnum
  FIOSETEOF      = $00006606; // Set file length

  TIOFLUSH = $00007408;       // discard unread input.  arg is ignored

function c_open(path: PChar; oflag: C_int): C_int; cdecl;
  external 'StdCLib' name 'open';

function c_close(filedes: C_int): C_int; cdecl;
  external 'StdCLib' name 'close';

function c_write(filedes: C_int; buf: pointer; nbyte: size_t): size_t; cdecl;
  external 'StdCLib' name 'write';

function c_read(filedes: C_int; buf: pointer; nbyte: size_t): size_t; cdecl;
  external 'StdCLib' name 'read';

function lseek(filedes: C_int; offset: off_t; whence: C_int): off_t; cdecl;
  external 'StdCLib' name 'lseek';

function ioctl(filedes: C_int; cmd: C_unsigned_int; arg: pointer): C_int; cdecl;
  external 'StdCLib' name 'ioctl';

function remove(filename: PChar): C_int; cdecl;
  external 'StdCLib';

function c_rename(old, c_new: PChar): C_int; cdecl;
  external 'StdCLib' name 'rename';

procedure c_exit(status: C_int); cdecl;
  external 'StdCLib' name 'exit';

  {cdecl is actually only needed for m68k}

var
  {Is set to zero for MPWTool, nonzero otherwise.}
  StandAlone: C_int; external name 'StandAlone';

CONST

Sys_EPERM       = 1;    { No permission match }
Sys_ENOENT      = 2;    { No such file or directory }
Sys_ENORSRC     = 3;    { Resource not found *}
Sys_EINTR       = 4;    { System service interrupted *}
Sys_EIO         = 5;    { I/O error }
Sys_ENXIO       = 6;    { No such device or address }
Sys_E2BIG       = 7;    { Insufficient space for return argument * }
Sys_ENOEXEC     = 8;    { File not executable * }
Sys_EBADF       = 9;    { Bad file number }
Sys_ECHILD      = 10;   { No child processes }
Sys_EAGAIN      = 11;   { Resource temporarily unavailable * }
Sys_ENOMEM      = 12;   { Not enough space * }
Sys_EACCES      = 13;   { Permission denied }
Sys_EFAULT      = 14;   { Illegal filename * }
Sys_ENOTBLK     = 15;   { Block device required }
Sys_EBUSY       = 16;   { Device or resource busy }
Sys_EEXIST      = 17;   { File exists }
Sys_EXDEV       = 18;   { Cross-device link }
Sys_ENODEV      = 19;   { No such device }
Sys_ENOTDIR     = 20;   { Not a directory }
Sys_EISDIR      = 21;   { Is a directory }
Sys_EINVAL      = 22;   { Invalid parameter * }
Sys_ENFILE      = 23;   { File table overflow }
Sys_EMFILE      = 24;   { Too many open files }
Sys_ENOTTY      = 25;   { Not a typewriter }
Sys_ETXTBSY     = 26;   { Text file busy. The new process was
                          a pure procedure (shared text) file which was
                          open for writing by another process, or file
                          which was open for writing by another process,
                          or while the pure procedure file was being
                          executed an open(2) call requested write access
                          requested write access.
              (Probably not applicable on macos)}
Sys_EFBIG       = 27;   { File too large }
Sys_ENOSPC      = 28;   { No space left on device }
Sys_ESPIPE      = 29;   { Illegal seek }
Sys_EROFS       = 30;   { Read-only file system }
Sys_EMLINK      = 31;   { Too many links }
Sys_EPIPE       = 32;   { Broken pipe }
Sys_EDOM        = 33;   { Math argument out of domain of func }
Sys_ERANGE      = 34;   { Math result not representable }

{ Note * is slightly different, compared to rtl/sunos/errno.inc}

{$endif}

{$PACKRECORDS NORMAL}

