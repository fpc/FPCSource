{
 *  CGError.h
 *  CoreGraphics
 *
 *  Copyright (c) 2000 Apple Computer, Inc. All rights reserved.
 *
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

unit CGErrors;
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
uses MacTypes,CGBase;
{$ALIGN POWER}


{ Types used for error and error handler }
type
	CGError 					= SInt32;
const
	kCGErrorSuccess = 0;
	kCGErrorFirst = 1000;
	kCGErrorFailure = kCGErrorFirst;
	kCGErrorIllegalArgument = 1001;
	kCGErrorInvalidConnection = 1002;
	kCGErrorInvalidContext = 1003;
	kCGErrorCannotComplete = 1004;
	kCGErrorNameTooLong = 1005;
	kCGErrorNotImplemented = 1006;
	kCGErrorRangeCheck = 1007;
	kCGErrorTypeCheck = 1008;
	kCGErrorNoCurrentPoint = 1009;
	kCGErrorInvalidOperation = 1010;
	kCGErrorNoneAvailable = 1011;
    {	internal errors have taken 1012, 1013, and 1014 }

	kCGErrorApplicationRequiresNewerSystem = 1015;
		{	the application being launched says in it's bundle info that it requires a }
		{	newer version of the system than is currently running. }
	
	kCGErrorApplicationNotPermittedToExecute = 1016;
		{	Macintosh Manager is active, and this application is not permitted to run }
	
	kCGErrorApplicationIncorrectExecutableFormatFound = 1023;
		{	the application being launched does not have any executable code for the }
		{	current system. }
	
	kCGErrorApplicationIsLaunching = 1024;
		{ The application is in the process of launching, but hasn't checked in yet.
		   Any launch data provided will be given to the application when it does
		   check in. }
	
	kCGErrorApplicationAlreadyRunning = 1025;
		{	The application being launched was already running ( and had already checked
		    in ) and so any launch data provided can not be delivered to in by CPS }

	kCGErrorApplicationCanOnlyBeRunInOneSessionAtATime = 1026;
		{	The application being launched is incompatible with multiple user sessions,
			and is already running in another session by another user. }
			
	kCGErrorClassicApplicationsMustBeLaunchedByClassic = 1027;
		{	To avoid deadlock, Classic can't launch another Classic application by going
			thru CPS.  This error gets returned in that case, and it signals TruBlueEnvironment
			that it must handle this launch on its own. }

	kCGErrorForkFailed = 1028;
		{	CPS was unable to fork a new process in order to launch an application. }

	kCGErrorLast = kCGErrorForkFailed;


end.
