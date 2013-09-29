{
     File:       QuickTime/QTML.h
 
     Contains:   QuickTime Cross-platform specific interfaces
 
     Version:    QuickTime 7.7.1
 
     Copyright:  © 1997-2012 by Apple Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
{  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2012 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit QTML;
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
{$ifc defined(iphonesim)}
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{ QuickTime is not available to 64-bit clients }

{$ifc not TARGET_CPU_64}
{
 *  QTMLYieldCPU()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 3.0 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLYieldCPU; external name '_QTMLYieldCPU';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{ QTMLYieldCPUTime flags}
const
	kQTMLHandlePortEvents = 1 shl 0; { ask for event handling during the yield}

{
 *  QTMLYieldCPUTime()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 3.0 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLYieldCPUTime( milliSeconds: SIGNEDLONG; flags: UNSIGNEDLONG ); external name '_QTMLYieldCPUTime';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


type
	QTMLMutex = ^OpaqueQTMLMutex; { an opaque type }
	OpaqueQTMLMutex = record end;
	QTMLMutexPtr = ^QTMLMutex; { when a var xx:QTMLMutex parameter can be nil, it is changed to xx: QTMLMutexPtr }
	QTMLSyncVar = ^OpaqueQTMLSyncVar; { an opaque type }
	OpaqueQTMLSyncVar = record end;
	QTMLSyncVarPtr = ^QTMLSyncVar;  { when a var xx:QTMLSyncVar parameter can be nil, it is changed to xx: QTMLSyncVarPtr }
{ InitializeQTML flags}
const
	kInitializeQTMLNoSoundFlag = 1 shl 0; { flag for requesting no sound when calling InitializeQTML}
	kInitializeQTMLUseGDIFlag = 1 shl 1; { flag for requesting GDI when calling InitializeQTML}
	kInitializeQTMLDisableDirectSound = 1 shl 2; { disables QTML's use of DirectSound}
	kInitializeQTMLUseExclusiveFullScreenModeFlag = 1 shl 3; { later than QTML 3.0: qtml starts up in exclusive full screen mode}
	kInitializeQTMLDisableDDClippers = 1 shl 4; { flag for requesting QTML not to use DirectDraw clipper objects; QTML 5.0 and later}
	kInitializeQTMLEnableDoubleBufferedSurface = 1 shl 6; { flag for requesting QuickTime use a double-buffered destination surface; QT6.4 and later}

{
 *  InitializeQTML()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  TerminateQTML()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{ CreatePortAssociation flags}
const
	kQTMLNoIdleEvents = 1 shl 1; { ask for a non-auto-idled port to be created}
	kQTMLNoDoubleBufferPort = 1 shl 2; { ask for QTML not to double-buffer this port}

const
	kQTMLIsDoubleBuffered = 'UsesDoubleBuffer';
{
 *  CreatePortAssociation()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  DestroyPortAssociation()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLGrabMutex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 3.0 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLGrabMutex( mu: QTMLMutex ); external name '_QTMLGrabMutex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTMLTryGrabMutex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 4.1 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 4.1 and later
 }
function QTMLTryGrabMutex( mu: QTMLMutex ): Boolean; external name '_QTMLTryGrabMutex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTMLReturnMutex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 3.0 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLReturnMutex( mu: QTMLMutex ); external name '_QTMLReturnMutex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTMLCreateMutex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 3.0 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
function QTMLCreateMutex: QTMLMutex; external name '_QTMLCreateMutex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTMLDestroyMutex()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in QuickTime.framework
 *    CarbonLib:        in CarbonLib 3.0 and later
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }
procedure QTMLDestroyMutex( mu: QTMLMutex ); external name '_QTMLDestroyMutex';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  QTMLCreateSyncVar()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLDestroySyncVar()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLTestAndSetSyncVar()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLWaitAndSetSyncVar()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLResetSyncVar()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  InitializeQHdr()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  TerminateQHdr()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLAcquireWindowList()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLReleaseWindowList()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
   These routines are here to support "interrupt level" code
      These are dangerous routines, only use if you know what you are doing.
}

{
 *  QTMLRegisterInterruptSafeThread()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLUnregisterInterruptSafeThread()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  NativeEventToMacEvent()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{$ifc TARGET_OS_WIN32}
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
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  ShowHideTaskBar()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


const
	kDDSurfaceLocked = 1 shl 0;
	kDDSurfaceStatic = 1 shl 1;

{
 *  QTGetDDObject()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTSetDDObject()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTSetDDPrimarySurface()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLGetVolumeRootPath()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLSetWindowWndProc()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTMLGetWindowWndProc()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{$endc}  { TARGET_OS_WIN32 }

{
 *  QTMLGetCanonicalPathName()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


const
	kFullNativePath = 0;
	kFileNameOnly = 1 shl 0;
	kDirectoryPathOnly = 1 shl 1;
	kUFSFullPathName = 1 shl 2;
	kTryVDIMask = 1 shl 3; {    Used in NativePathNameToFSSpec to specify to search VDI mountpoints}
	kFullPathSpecifiedMask = 1 shl 4; {    the passed in name is a fully qualified full path}

{
 *  FSSpecToNativePathName()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


const
	kErrorIfFileNotFound = 1 shl 31;

{
 *  NativePathNameToFSSpec()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 3.0 and later
 }


{
 *  QTGetAliasInfo()
 *  
 *  Availability:
 *    Mac OS X:         not available
 *    CarbonLib:        not available
 *    Non-Carbon CFM:   not available
 *    Windows:          in qtmlClient.lib 5.0 and later
 }

{$endc} {not TARGET_CPU_64}

{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
