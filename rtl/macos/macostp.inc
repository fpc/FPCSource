{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by Olle Raab

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

{************** from Types.p ***************}
const
  noErr = 0;

type
  SignedByte = shortint;
  SInt8 = SignedByte;
  SignedBytePtr = ^SignedByte;
  OSErr = Integer;
  OSType = Longint;
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
  Size = LONGINT;

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

{************** from OSUtils.p ***************}

type
  QElemPtr = ^QElem;

  QElem = record
    qLink: QElemPtr;
    qType: INTEGER;
    qData: array[0..0] of INTEGER;
   end;

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
  IOCompletionUPP = UniversalProcPtr;

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

  FSSpec = record
      vRefNum: Integer;
      parID: Longint;
      name: Str63;
   end;
  FSSpecPtr = ^FSSpec;

function MemError: OSErr;
external 'InterfaceLib';

function FSpOpenDF(spec: FSSpec; permission: SignedByte;
  var refNum: Integer): OSErr;
external 'InterfaceLib';

function FSpCreate(spec: FSSpec; creator, fileType: OSType;
  scriptTag: ScriptCode): OSErr;
external 'InterfaceLib';

function FSClose(refNum: Integer): OSErr;
external 'InterfaceLib';

function FSRead(refNum: Integer; var count: Longint; buffPtr: Mac_Ptr): OSErr;
external 'InterfaceLib';

function FSWrite(refNum: Integer; var count: Longint; buffPtr: Mac_Ptr): OSErr;
external 'InterfaceLib';

function GetFPos(refNum: Integer; var filePos: Longint): OSErr;
external 'InterfaceLib';

function SetFPos(refNum: Integer; posMode: Integer; posOff: Longint): OSErr;
external 'InterfaceLib';

function GetEOF(refNum: Integer; var logEOF: Longint): OSErr;
external 'InterfaceLib';

function SetEOF(refNum: Integer; logEOF: Longint): OSErr;
external 'InterfaceLib';

function FSMakeFSSpec (vRefNum: Integer; dirID: LongInt;
  fileName: Str255; VAR spec: FSSpec): OSErr;
external 'InterfaceLib';

function FSpDirCreate(spec: FSSpec; scriptTag: ScriptCode;
  var createdDirID: Longint): OSErr;
external 'InterfaceLib';

function FSpDelete(spec: FSSpec): OSErr;
external 'InterfaceLib';

function PBGetCatInfoSync (paramBlock: CInfoPBPtr): OSErr;
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

{************** from others ***************}

procedure ExitToShell;
external 'InterfaceLib';

procedure SysBeep(dur: Integer);
external 'SysBeep';

function TickCount: Longint;
external 'InterfaceLib';

function Munger (h: Mac_Handle; offset: LONGINT; ptr1: Mac_Ptr;
  len1: LONGINT; ptr2: Mac_Ptr; len2: LONGINT): LONGINT;
external 'InterfaceLib';


