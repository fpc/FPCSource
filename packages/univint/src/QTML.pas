{
     File:       QTML.p
 
     Contains:   QuickTime Cross-platform specific interfaces
 
     Version:    Technology: QuickTime 5.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1997-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit QTML;
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
uses MacTypes,Quickdraw,Events,MacMemory,MacWindows,OSUtils,Files;


{$ALIGN MAC68K}

{$ifc CALL_NOT_IN_CARBON}
{
 *  QTMLYieldCPU()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLYieldCPU; external name '_QTMLYieldCPU';

{
 *  QTMLYieldCPUTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLYieldCPUTime(milliSeconds: SInt32; flags: UInt32); external name '_QTMLYieldCPUTime';

{$endc}  {CALL_NOT_IN_CARBON}


type
	QTMLMutex    = ^SInt32; { an opaque 32-bit type }
	QTMLMutexPtr = ^QTMLMutex;  { when a var xx:QTMLMutex parameter can be nil, it is changed to xx: QTMLMutexPtr }
{$ifc NOT (TARGET_OS_MAC AND TARGET_API_MAC_OS8)}
	QTMLSyncVar    = ^SInt32; { an opaque 32-bit type }
	QTMLSyncVarPtr = ^QTMLSyncVar;  { when a var xx:QTMLSyncVar parameter can be nil, it is changed to xx: QTMLSyncVarPtr }

const
	kInitializeQTMLNoSoundFlag	= $00000001;					{  flag for requesting no sound when calling InitializeQTML }
	kInitializeQTMLUseGDIFlag	= $00000002;					{  flag for requesting GDI when calling InitializeQTML }
	kInitializeQTMLDisableDirectSound = $00000004;				{  disables QTML's use of DirectSound }
	kInitializeQTMLUseExclusiveFullScreenModeFlag = $00000008;	{  later than QTML 3.0: qtml starts up in exclusive full screen mode }
	kInitializeQTMLDisableDDClippers = $00000010;				{  flag for requesting QTML not to use DirectDraw clipper objects; QTML 5.0 and later }

	kQTMLHandlePortEvents		= $00000001;					{  flag for requesting requesting QTML to handle events }
	kQTMLNoIdleEvents			= $00000002;					{  flag for requesting requesting QTML not to send Idle Events }

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  InitializeQTML()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function InitializeQTML(flag: SInt32): OSErr; external name '_InitializeQTML';

{
 *  TerminateQTML()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure TerminateQTML; external name '_TerminateQTML';


{
 *  CreatePortAssociation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function CreatePortAssociation(theWnd: UnivPtr; storage: Ptr; flags: SInt32): GrafPtr; external name '_CreatePortAssociation';

{
 *  DestroyPortAssociation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure DestroyPortAssociation(cgp: CGrafPtr); external name '_DestroyPortAssociation';


{
 *  QTMLGrabMutex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLGrabMutex(mu: QTMLMutex); external name '_QTMLGrabMutex';

{
 *  QTMLTryGrabMutex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function QTMLTryGrabMutex(mu: QTMLMutex): boolean; external name '_QTMLTryGrabMutex';

{
 *  QTMLReturnMutex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLReturnMutex(mu: QTMLMutex); external name '_QTMLReturnMutex';

{
 *  QTMLCreateMutex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLCreateMutex: QTMLMutex; external name '_QTMLCreateMutex';

{
 *  QTMLDestroyMutex()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLDestroyMutex(mu: QTMLMutex); external name '_QTMLDestroyMutex';


{
 *  QTMLCreateSyncVar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLCreateSyncVar: QTMLSyncVarPtr; external name '_QTMLCreateSyncVar';

{
 *  QTMLDestroySyncVar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLDestroySyncVar(p: QTMLSyncVarPtr); external name '_QTMLDestroySyncVar';

{
 *  QTMLTestAndSetSyncVar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLTestAndSetSyncVar(sync: QTMLSyncVarPtr): SInt32; external name '_QTMLTestAndSetSyncVar';

{
 *  QTMLWaitAndSetSyncVar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLWaitAndSetSyncVar(sync: QTMLSyncVarPtr); external name '_QTMLWaitAndSetSyncVar';

{
 *  QTMLResetSyncVar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLResetSyncVar(sync: QTMLSyncVarPtr); external name '_QTMLResetSyncVar';


{
 *  InitializeQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure InitializeQHdr(var qhdr_: QHdr); external name '_InitializeQHdr';

{
 *  TerminateQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure TerminateQHdr(var qhdr_: QHdr); external name '_TerminateQHdr';


{
 *  QTMLAcquireWindowList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLAcquireWindowList; external name '_QTMLAcquireWindowList';

{
 *  QTMLReleaseWindowList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLReleaseWindowList; external name '_QTMLReleaseWindowList';

{
   These routines are here to support "interrupt level" code
      These are dangerous routines, only use if you know what you are doing.
}

{
 *  QTMLRegisterInterruptSafeThread()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLRegisterInterruptSafeThread(threadID: UInt32; threadInfo: UnivPtr): SInt32; external name '_QTMLRegisterInterruptSafeThread';

{
 *  QTMLUnregisterInterruptSafeThread()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLUnregisterInterruptSafeThread(threadID: UInt32): SInt32; external name '_QTMLUnregisterInterruptSafeThread';


{
 *  NativeEventToMacEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function NativeEventToMacEvent(nativeEvent: UnivPtr; var macEvent: EventRecord): SInt32; external name '_NativeEventToMacEvent';

{$endc}  {CALL_NOT_IN_CARBON}
{$ifc TARGET_OS_WIN32}
{$ifc CALL_NOT_IN_CARBON}
{
 *  WinEventToMacEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function WinEventToMacEvent(winMsg: UnivPtr; var macEvent: EventRecord): SInt32; external name '_WinEventToMacEvent';

{
 *  IsTaskBarVisible()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function IsTaskBarVisible: boolean; external name '_IsTaskBarVisible';

{
 *  ShowHideTaskBar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure ShowHideTaskBar(showIt: boolean); external name '_ShowHideTaskBar';

{$endc}  {CALL_NOT_IN_CARBON}

const
	kDDSurfaceLocked			= $00000001;
	kDDSurfaceStatic			= $00000002;

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  QTGetDDObject()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function QTGetDDObject(var lpDDObject: UnivPtr): OSErr; external name '_QTGetDDObject';

{
 *  QTSetDDObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTSetDDObject(lpNewDDObject: UnivPtr): OSErr; external name '_QTSetDDObject';

{
 *  QTSetDDPrimarySurface()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTSetDDPrimarySurface(lpNewDDSurface: UnivPtr; flags: UInt32): OSErr; external name '_QTSetDDPrimarySurface';


{
 *  QTMLGetVolumeRootPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLGetVolumeRootPath(fullPath: CStringPtr; volumeRootPath: CStringPtr; volumeRootLen: UInt32): OSErr; external name '_QTMLGetVolumeRootPath';


{
 *  QTMLSetWindowWndProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLSetWindowWndProc(theWindow: WindowRef; windowProc: UnivPtr); external name '_QTMLSetWindowWndProc';

{
 *  QTMLGetWindowWndProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLGetWindowWndProc(theWindow: WindowRef): Ptr; external name '_QTMLGetWindowWndProc';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {TARGET_OS_WIN32}
{$ifc CALL_NOT_IN_CARBON}
{
 *  QTMLGetCanonicalPathName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLGetCanonicalPathName(inName: CStringPtr; outName: CStringPtr; outLen: UInt32): OSErr; external name '_QTMLGetCanonicalPathName';

{$endc}  {CALL_NOT_IN_CARBON}

const
	kFullNativePath				= 0;
	kFileNameOnly				= $01;
	kDirectoryPathOnly			= $02;
	kUFSFullPathName			= $04;
	kTryVDIMask					= $08;							{     Used in NativePathNameToFSSpec to specify to search VDI mountpoints }
	kFullPathSpecifiedMask		= $10;							{     the passed in name is a fully qualified full path }

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  FSSpecToNativePathName()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function FSSpecToNativePathName(const (*var*) inFile: FSSpec; outName: CStringPtr; outLen: UInt32; flags: SInt32): OSErr; external name '_FSSpecToNativePathName';

{$endc}  {CALL_NOT_IN_CARBON}

const
	kErrorIfFileNotFound		= $80000000;

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NativePathNameToFSSpec()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 *    Windows:          in qtmlClient.lib 3.0 and later
	 	}
function NativePathNameToFSSpec(inName: CStringPtr; var outFile: FSSpec; flags: SInt32): OSErr; external name '_NativePathNameToFSSpec';

{
 *  QTGetAliasInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 *    Windows:          in qtmlClient.lib 5.0 and later
 }
function QTGetAliasInfo(alias: AliasHandle; index: AliasInfoType; outBuf: CStringPtr; bufLen: SInt32; var outLen: SInt32; flags: UInt32): OSErr; external name '_QTGetAliasInfo';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{$ALIGN MAC68K}


end.
