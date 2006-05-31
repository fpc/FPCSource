{
     File:       OSA.p
 
     Contains:   Open Scripting Architecture Client Interfaces.
 
     Version:    Technology: AppleScript 1.4
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved
 
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

unit OSA;
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
uses MacTypes,AEDataModel,MacErrors,AppleEvents,AEObjects,AEInteraction,Components;


{$ALIGN MAC68K}

{*************************************************************************
    Types and Constants
*************************************************************************}

{    The componenent manager type code for components that
        support the OSA interface defined here. }
{ 0x6f736120 }

const
	kOSAComponentType			= $6F736120 (* 'osa ' *);

	{	 0x73637074 	}
	kOSAGenericScriptingComponentSubtype = $73637074 (* 'scpt' *);

	{	  Type of script document files.  	}
	{	 0x6f736173 	}
	kOSAFileType				= $6F736173 (* 'osas' *);

	{	
	        Suite and event code of the RecordedText event. 
	        (See OSAStartRecording, below.)
	    	}
	{	 0x61736372 	}
	kOSASuite					= $61736372 (* 'ascr' *);

	{	 0x72656364 	}
	kOSARecordedText			= $72656364 (* 'recd' *);

	{	 Selector returns boolean 	}
	{	 0x6d6f6469 	}
	kOSAScriptIsModified		= $6D6F6469 (* 'modi' *);

	{	 Selector returns boolean 	}
	{	 0x63736372 	}
	kOSAScriptIsTypeCompiledScript = $63736372 (* 'cscr' *);

	{	 Selector returns boolean 	}
	{	 0x76616c75 	}
	kOSAScriptIsTypeScriptValue	= $76616C75 (* 'valu' *);

	{	 Selector returns boolean 	}
	{	 0x636e7478 	}
	kOSAScriptIsTypeScriptContext = $636E7478 (* 'cntx' *);

	{	 Selector returns a DescType which may be passed to OSACoerceToDesc 	}
	{	 0x62657374 	}
	kOSAScriptBestType			= $62657374 (* 'best' *);

	{	
	        This selector is used to determine whether a script has source 
	        associated with it that when given to OSAGetSource, the call will not
	        fail.  The selector returns a boolean.
	    	}
	{	 0x67737263 	}
	kOSACanGetSource			= $67737263 (* 'gsrc' *);


	typeOSADialectInfo			= $6469666F (* 'difo' *);						{   0x6469666f    }
	keyOSADialectName			= $646E616D (* 'dnam' *);						{   0x646e616d    }
	keyOSADialectCode			= $64636F64 (* 'dcod' *);						{   0x64636f64    }
	keyOSADialectLangCode		= $646C6364 (* 'dlcd' *);						{   0x646c6364    }
	keyOSADialectScriptCode		= $64736364 (* 'dscd' *);						{   0x64736364    }


type
	OSAError							= ComponentResult;
	{	 Under the Open Scripting Architecture all error results are longs 	}
	OSAID								= UInt32;
	{	
	        OSAIDs allow transparent manipulation of scripts associated with
	        various scripting systems.
	    	}

const
	kOSANullScript				= 0;

	{	 No -script constant. 	}
	kOSANullMode				= 0;							{  sounds better  }
	kOSAModeNull				= 0;							{  tastes consistent  }

	{	
	        Some routines take flags that control their execution.  This constant
	        declares default mode settings are used.
	    	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	OSACreateAppleEventProcPtr = function(theAEEventClass: AEEventClass; theAEEventID: AEEventID; const (*var*) target: AEAddressDesc; returnID: SInt16; transactionID: SInt32; var result: AppleEvent; refCon: SInt32): OSErr;
{$elsec}
	OSACreateAppleEventProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	OSASendProcPtr = function(const (*var*) theAppleEvent: AppleEvent; var reply: AppleEvent; sendMode: AESendMode; sendPriority: AESendPriority; timeOutInTicks: SInt32; idleProc: AEIdleUPP; filterProc: AEFilterUPP; refCon: SInt32): OSErr;
{$elsec}
	OSASendProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	OSACreateAppleEventUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSACreateAppleEventUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	OSASendUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSASendUPP = UniversalProcPtr;
{$endc}	

const
	uppOSACreateAppleEventProcInfo = $000FEFE0;
	uppOSASendProcInfo = $003FEFE0;
	{
	 *  NewOSACreateAppleEventUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewOSACreateAppleEventUPP(userRoutine: OSACreateAppleEventProcPtr): OSACreateAppleEventUPP; external name '_NewOSACreateAppleEventUPP'; { old name was NewOSACreateAppleEventProc }
{
 *  NewOSASendUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewOSASendUPP(userRoutine: OSASendProcPtr): OSASendUPP; external name '_NewOSASendUPP'; { old name was NewOSASendProc }
{
 *  DisposeOSACreateAppleEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSACreateAppleEventUPP(userUPP: OSACreateAppleEventUPP); external name '_DisposeOSACreateAppleEventUPP';
{
 *  DisposeOSASendUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSASendUPP(userUPP: OSASendUPP); external name '_DisposeOSASendUPP';
{
 *  InvokeOSACreateAppleEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSACreateAppleEventUPP(theAEEventClass: AEEventClass; theAEEventID: AEEventID; const (*var*) target: AEAddressDesc; returnID: SInt16; transactionID: SInt32; var result: AppleEvent; refCon: SInt32; userRoutine: OSACreateAppleEventUPP): OSErr; external name '_InvokeOSACreateAppleEventUPP'; { old name was CallOSACreateAppleEventProc }
{
 *  InvokeOSASendUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSASendUPP(const (*var*) theAppleEvent: AppleEvent; var reply: AppleEvent; sendMode: AESendMode; sendPriority: AESendPriority; timeOutInTicks: SInt32; idleProc: AEIdleUPP; filterProc: AEFilterUPP; refCon: SInt32; userRoutine: OSASendUPP): OSErr; external name '_InvokeOSASendUPP'; { old name was CallOSASendProc }
{*************************************************************************
    OSA Interface Descriptions
**************************************************************************
    The OSA Interface is broken down into a required interface, and several
    optional interfaces to support additional functionality.  A given scripting
    component may choose to support only some of the optional interfaces in
    addition to the basic interface.  The OSA Component Flags may be used to 
    query the Component Manager to find a scripting component with a particular
    capability, or determine if a particular scripting component supports a 
    particular capability.
*************************************************************************}
{ OSA Component Flags: }

const
	kOSASupportsCompiling		= $0002;
	kOSASupportsGetSource		= $0004;
	kOSASupportsAECoercion		= $0008;
	kOSASupportsAESending		= $0010;
	kOSASupportsRecording		= $0020;
	kOSASupportsConvenience		= $0040;
	kOSASupportsDialects		= $0080;
	kOSASupportsEventHandling	= $0100;

	{	 Component Selectors: 	}
	kOSASelectLoad				= $0001;
	kOSASelectStore				= $0002;
	kOSASelectExecute			= $0003;
	kOSASelectDisplay			= $0004;
	kOSASelectScriptError		= $0005;
	kOSASelectDispose			= $0006;
	kOSASelectSetScriptInfo		= $0007;
	kOSASelectGetScriptInfo		= $0008;
	kOSASelectSetActiveProc		= $0009;
	kOSASelectGetActiveProc		= $000A;

	{	 Compiling: 	}
	kOSASelectScriptingComponentName = $0102;
	kOSASelectCompile			= $0103;
	kOSASelectCopyID			= $0104;

	kOSASelectCopyScript		= $0105;

	{	 GetSource: 	}
	kOSASelectGetSource			= $0201;

	{	 AECoercion: 	}
	kOSASelectCoerceFromDesc	= $0301;
	kOSASelectCoerceToDesc		= $0302;

	{	 AESending: 	}
	kOSASelectSetSendProc		= $0401;
	kOSASelectGetSendProc		= $0402;
	kOSASelectSetCreateProc		= $0403;
	kOSASelectGetCreateProc		= $0404;
	kOSASelectSetDefaultTarget	= $0405;

	{	 Recording: 	}
	kOSASelectStartRecording	= $0501;
	kOSASelectStopRecording		= $0502;

	{	 Convenience: 	}
	kOSASelectLoadExecute		= $0601;
	kOSASelectCompileExecute	= $0602;
	kOSASelectDoScript			= $0603;

	{	 Dialects: 	}
	kOSASelectSetCurrentDialect	= $0701;
	kOSASelectGetCurrentDialect	= $0702;
	kOSASelectAvailableDialects	= $0703;
	kOSASelectGetDialectInfo	= $0704;
	kOSASelectAvailableDialectCodeList = $0705;

	{	 Event Handling: 	}
	kOSASelectSetResumeDispatchProc = $0801;
	kOSASelectGetResumeDispatchProc = $0802;
	kOSASelectExecuteEvent		= $0803;
	kOSASelectDoEvent			= $0804;
	kOSASelectMakeContext		= $0805;

	{	 Debugging 	}
	kOSADebuggerCreateSession	= $0901;
	kOSADebuggerGetSessionState	= $0902;
	kOSADebuggerSessionStep		= $0903;
	kOSADebuggerDisposeSession	= $0904;
	kOSADebuggerGetStatementRanges = $0905;
	kOSADebuggerGetBreakpoint	= $0910;
	kOSADebuggerSetBreakpoint	= $0911;
	kOSADebuggerGetDefaultBreakpoint = $0912;
	kOSADebuggerGetCurrentCallFrame = $0906;
	kOSADebuggerGetCallFrameState = $0907;
	kOSADebuggerGetVariable		= $0908;
	kOSADebuggerSetVariable		= $0909;
	kOSADebuggerGetPreviousCallFrame = $090A;
	kOSADebuggerDisposeCallFrame = $090B;
	kOSADebuggerCountVariables	= $090C;

	{	 scripting component specific selectors are added beginning with this value  	}
	kOSASelectComponentSpecificStart = $1001;


	{	        Mode Flags:
	
	    Warning: These should not conflict with the AESend mode flags in
	    AppleEvents.h, because we may want to use them as OSA mode flags too.
		}

	{	
	        This mode flag may be passed to OSALoad, OSAStore or OSACompile to
	        instruct the scripting component to not retain the "source" of an
	        expression.  This will cause the OSAGetSource call to return the error
	        errOSASourceNotAvailable if used.  However, some scripting components
	        may not retain the source anyway.  This is mainly used when either space
	        efficiency is desired, or a script is to be "locked" so that its
	        implementation may not be viewed.
	    	}
	kOSAModePreventGetSource	= $00000001;

	{	
	        These mode flags may be passed to OSACompile, OSAExecute, OSALoadExecute
	        OSACompileExecute, OSADoScript, OSAExecuteEvent, or OSADoEvent to
	        indicate whether or not the script may interact with the user, switch
	        layer or reconnect if necessary.  Any AppleEvents will be sent with the
	        corresponding AESend mode supplied.
	    	}
	kOSAModeNeverInteract		= $00000010;
	kOSAModeCanInteract			= $00000020;
	kOSAModeAlwaysInteract		= $00000030;
	kOSAModeDontReconnect		= $00000080;

	{	
	        This mode flag may be passed to OSACompile, OSAExecute, OSALoadExecute
	        OSACompileExecute, OSADoScript, OSAExecuteEvent, or OSADoEvent to
	        indicate whether or not AppleEvents should be sent with the
	        kAECanSwitchLayer mode flag sent or not. NOTE: This flag is exactly the
	        opposite sense of the AppleEvent flag kAECanSwitchLayer.  This is to
	        provide a more convenient default, i.e. not supplying any mode
	        (kOSAModeNull) means to send events with kAECanSwitchLayer.  Supplying
	        the kOSAModeCantSwitchLayer mode flag will cause AESend to be called
	        without kAECanSwitchLayer.
	    	}
	kOSAModeCantSwitchLayer		= $00000040;

	{	
	        This mode flag may be passed to OSACompile, OSAExecute, OSALoadExecute
	        OSACompileExecute, OSADoScript, OSAExecuteEvent, or OSADoEvent to
	        indicate whether or not AppleEvents should be sent with the kAEDontRecord
	        mode flag sent or not. NOTE: This flag is exactly the opposite sense of
	        the AppleEvent flag kAEDontRecord.  This is to provide a more convenient
	        default, i.e. not supplying any mode (kOSAModeNull) means to send events
	        with kAEDontRecord.  Supplying the kOSAModeDoRecord mode flag will 
	        cause AESend to be called without kAEDontRecord.
	    	}
	kOSAModeDoRecord			= $00001000;

	{	
	        This is a mode flag for OSACompile that indicates that a context should
	        be created as the result of compilation. All handler definitions are
	        inserted into the new context, and variables are initialized by
	        evaluating their initial values in a null context (i.e. they must be
	        constant expressions).
	    	}
	kOSAModeCompileIntoContext	= $00000002;

	{	
	        This is a mode flag for OSACompile that indicates that the previous
	        script ID (input to OSACompile) should be augmented with any new
	        definitions in the sourceData rather than replaced with a new script.
	        This means that the previous script ID must designate a context.
	        The presence of this flag causes the kOSAModeCompileIntoContext flag
	        to be implicitly used, causing any new definitions to be initialized
	        in a null context.
	    	}
	kOSAModeAugmentContext		= $00000004;

	{	
	        This mode flag may be passed to OSADisplay or OSADoScript to indicate
	        that output only need be human-readable, not re-compilable by OSACompile.
	        If used, output may be arbitrarily "beautified", e.g. quotes may be left
	        off of string values, long lists may have elipses, etc.
	    	}
	kOSAModeDisplayForHumans	= $00000008;

	{	
	        This mode flag may be passed to OSAStore in the case where the scriptID
	        is a context.  This causes the context to be saved, but not the context's
	        parent context.  When the stored context is loaded back in, the parent
	        will be kOSANullScript.
	    	}
	kOSAModeDontStoreParent		= $00010000;

	{	
	        This mode flag may be passed to OSAExecuteEvent to cause the event to
	        be dispatched to the direct object of the event. The direct object (or
	        subject attribute if the direct object is a non-object specifier) will
	        be resolved, and the resulting script object will be the recipient of
	        the message. The context argument to OSAExecuteEvent will serve as the
	        root of the lookup/resolution process.
	    	}
	kOSAModeDispatchToDirectObject = $00020000;

	{	
	        This mode flag may be passed to OSAExecuteEvent to indicate that
	        components do not have to get the data of object specifier arguments.
	    	}
	kOSAModeDontGetDataForArguments = $00040000;

	{	*************************************************************************
	    OSA Basic Scripting Interface
	**************************************************************************
	    Scripting components must at least support the Basic Scripting interface.
	*************************************************************************	}
	{	        Loading and Storing Scripts:
	
	    These routines allow scripts to be loaded and stored in their internal
	    (possibly compiled, non-text) representation.
		}

	{	 Resource type for scripts 	}
	kOSAScriptResourceType		= $73637074 (* 'scpt' *);

	{	
	        Default type given to OSAStore which creates "generic" loadable script
	        data descriptors.
	    	}
	typeOSAGenericStorage		= $73637074 (* 'scpt' *);

	{
	 *  OSALoad()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function OSALoad(scriptingComponent: ComponentInstance; const (*var*) scriptData: AEDesc; modeFlags: SInt32; var resultingScriptID: OSAID): OSAError; external name '_OSALoad';
{
        OSAComponentFunctionInline(kOSASelectLoad, 12);
    
        Errors:
            badComponentInstance        invalid scripting component instance
            errOSASystemError
            errOSABadStorageType:       scriptData not for this scripting component
            errOSACorruptData:          data seems to be corrupt
            errOSADataFormatObsolete    script data format is no longer supported
            errOSADataFormatTooNew      script data format is from a newer version
        
        ModeFlags:
            kOSAModePreventGetSource
    }
{
 *  OSAStore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAStore(scriptingComponent: ComponentInstance; scriptID: OSAID; desiredType: DescType; modeFlags: SInt32; var resultingScriptData: AEDesc): OSAError; external name '_OSAStore';
{
        OSAComponentFunctionInline(kOSASelectStore, 16);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSABadStorageType:   desiredType not for this scripting component
        
        ModeFlags:
            kOSAModePreventGetSource
            kOSAModeDontStoreParent
    }
{ Executing Scripts: }
{
 *  OSAExecute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAExecute(scriptingComponent: ComponentInstance; compiledScriptID: OSAID; contextID: OSAID; modeFlags: SInt32; var resultingScriptValueID: OSAID): OSAError; external name '_OSAExecute';
{
        OSAComponentFunctionInline(kOSASelectExecute, 16);
        This call runs a script.  The contextID represents the environment
        with which global variables in the script are resolved.  The constant
        kOSANullScript may be used for the contextID if the application wishes
        to not deal with context directly (a default one is associated with each
        scripting component instance).  The resultingScriptValueID is the 
        result of evaluation, and contains a value which may be displayed using
        the OSAGetSource call.  The modeFlags convey scripting component
        specific information.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSAScriptError:      the executing script got an error
    
        ModeFlags:
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{ Displaying results: }
{
 *  OSADisplay()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADisplay(scriptingComponent: ComponentInstance; scriptValueID: OSAID; desiredType: DescType; modeFlags: SInt32; var resultingText: AEDesc): OSAError; external name '_OSADisplay';
{
        OSAComponentFunctionInline(kOSASelectDisplay, 16);
        This call is used to convert results (script value IDs) into displayable
        text. The desiredType should be at least typeChar, and modeFlags are
        scripting system specific flags to control the formatting of the
        resulting text. This call differs from OSAGetSource in that (1) it
        always produces at least typeChar, (2) is only works on script values,
        (3) it may display it's output in non-compilable form (e.g. without
        string quotes, elipses inserted in long and/or circular lists, etc.) and
        (4) it is required by the basic scripting interface.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errAECoercionFail:      desiredType not supported by scripting component
    
        ModeFlags:
            kOSAModeDisplayForHumans
    }
{ Getting Error Information: }
{
 *  OSAScriptError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAScriptError(scriptingComponent: ComponentInstance; selector: OSType; desiredType: DescType; var resultingErrorDescription: AEDesc): OSAError; external name '_OSAScriptError';
{
        OSAComponentFunctionInline(kOSASelectScriptError, 12);
        Whenever script execution returns errOSAExecutionError, this routine
        may be used to get information about that error.  The selector describes
        the type of information desired about the error (various selectors are
        listed below).  The desiredType indicates the data type of the result
        desired for that selector.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSABadSelector:      selector not supported by scripting component
            errAECoercionFail:      desiredType not supported by scripting component
    }
{ OSAScriptError selectors: }
{
        This selector is used to determine the error number of a script error.
        These error numbers may be either system error numbers, or error numbers
        that are scripting component specific.
        Required desiredTypes:  
            typeShortInteger
    }

const
	kOSAErrorNumber				= $6572726E (* 'errn' *);

	{	
	        This selector is used to determine the full error message associated
	        with the error number.  It should include the name of the application
	        which caused the error, as well as the specific error that occurred.
	        This selector is sufficient for simple error reporting (but see
	        kOSAErrorBriefMessage, below).
	        Required desiredTypes:
	            typeChar                    error message string
	    	}
	kOSAErrorMessage			= $65727273 (* 'errs' *);

	{	
	        This selector is used to determine a brief error message associated with
	        the error number.  This message and should not mention the name of the
	        application which caused the error, any partial results or offending
	        object (see kOSAErrorApp, kOSAErrorPartialResult and
	        kOSAErrorOffendingObject, below).
	        Required desiredTypes:
	            typeChar                    brief error message string
	    	}
	{	  0x65727262  	}
	kOSAErrorBriefMessage		= $65727262 (* 'errb' *);

	{	
	        This selector is used to determine which application actually got the
	        error (if it was the result of an AESend), or the current application
	        if ....
	        Required desiredTypes:
	            typeProcessSerialNumber     PSN of the errant application
	            typeChar                    name of the errant application
	    	}
	{	  0x65726170  	}
	kOSAErrorApp				= $65726170 (* 'erap' *);

	{	
	        This selector is used to determine any partial result returned by an 
	        operation. If an AESend call failed, but a partial result was returned,
	        then the partial result may be returned as an AEDesc.
	        Required desiredTypes:
	            typeBest                    AEDesc of any partial result
	    	}
	{	  0x70746c72   	}
	kOSAErrorPartialResult		= $70746C72 (* 'ptlr' *);

	{	
	        This selector is used to determine any object which caused the error
	        that may have been indicated by an application.  The result is an 
	        AEDesc.
	        Required desiredTypes:
	            typeBest                    AEDesc of any offending object
	    	}
	{	  0x65726f62   	}
	kOSAErrorOffendingObject	= $65726F62 (* 'erob' *);

	{	
	        This selector is used to determine the type expected by a coercion 
	        operation if a type error occurred.
	    	}
	{	  0x65727274   	}
	kOSAErrorExpectedType		= $65727274 (* 'errt' *);

	{	
	        This selector is used to determine the source text range (start and 
	        end positions) of where the error occurred.
	        Required desiredTypes:
	            typeOSAErrorRange
	    	}
	{	  0x65726e67  	}
	kOSAErrorRange				= $65726E67 (* 'erng' *);

	{	
	        An AERecord type containing keyOSASourceStart and keyOSASourceEnd fields
	        of type short.
	    	}
	{	  0x65726e67   	}
	typeOSAErrorRange			= $65726E67 (* 'erng' *);

	{	 Field of a typeOSAErrorRange record of typeShortInteger 	}
	{	  0x73726373    	}
	keyOSASourceStart			= $73726373 (* 'srcs' *);

	{	 Field of a typeOSAErrorRange record of typeShortInteger 	}
	{	  0x73726365   	}
	keyOSASourceEnd				= $73726365 (* 'srce' *);

	{	 Disposing Script IDs: 	}
	{
	 *  OSADispose()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function OSADispose(scriptingComponent: ComponentInstance; scriptID: OSAID): OSAError; external name '_OSADispose';
{
        OSAComponentFunctionInline(kOSASelectDispose, 4);
        Disposes a script or context.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
    }
{ Getting and Setting Script Information: }
{
 *  OSASetScriptInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetScriptInfo(scriptingComponent: ComponentInstance; scriptID: OSAID; selector: OSType; value: SInt32): OSAError; external name '_OSASetScriptInfo';
{
        OSAComponentFunctionInline(kOSASelectSetScriptInfo, 12);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSABadSelector:      selector not supported by scripting component
                                    or selector not for this scriptID
    }
{
 *  OSAGetScriptInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetScriptInfo(scriptingComponent: ComponentInstance; scriptID: OSAID; selector: OSType; var result: SInt32): OSAError; external name '_OSAGetScriptInfo';
{
        OSAComponentFunctionInline(kOSASelectGetScriptInfo, 12);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSABadSelector:      selector not supported by scripting component
                                    or selector not for this scriptID
    }
{ Manipulating the ActiveProc:

    Scripting systems will supply default values for these procedures if they
    are not set by the client:
}

type
{$ifc TYPED_FUNCTION_POINTERS}
	OSAActiveProcPtr = function(refCon: SInt32): OSErr;
{$elsec}
	OSAActiveProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	OSAActiveUPP = ^SInt32; { an opaque UPP }
{$elsec}
	OSAActiveUPP = UniversalProcPtr;
{$endc}	

const
	uppOSAActiveProcInfo = $000000E0;
	{
	 *  NewOSAActiveUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewOSAActiveUPP(userRoutine: OSAActiveProcPtr): OSAActiveUPP; external name '_NewOSAActiveUPP'; { old name was NewOSAActiveProc }
{
 *  DisposeOSAActiveUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeOSAActiveUPP(userUPP: OSAActiveUPP); external name '_DisposeOSAActiveUPP';
{
 *  InvokeOSAActiveUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeOSAActiveUPP(refCon: SInt32; userRoutine: OSAActiveUPP): OSErr; external name '_InvokeOSAActiveUPP'; { old name was CallOSAActiveProc }
{
 *  OSASetActiveProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetActiveProc(scriptingComponent: ComponentInstance; activeProc: OSAActiveUPP; refCon: SInt32): OSAError; external name '_OSASetActiveProc';
{
        OSAComponentFunctionInline(kOSASelectSetActiveProc, 8);
        If activeProc is nil, the default activeProc is used.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSAGetActiveProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetActiveProc(scriptingComponent: ComponentInstance; var activeProc: OSAActiveUPP; var refCon: SInt32): OSAError; external name '_OSAGetActiveProc';
{
        OSAComponentFunctionInline(kOSASelectGetActiveProc, 8);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{*************************************************************************
    OSA Optional Compiling Interface
**************************************************************************
    Scripting components that support the Compiling interface have the 
    kOSASupportsCompiling bit set in it's ComponentDescription.
*************************************************************************}
{
 *  OSAScriptingComponentName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAScriptingComponentName(scriptingComponent: ComponentInstance; var resultingScriptingComponentName: AEDesc): OSAError; external name '_OSAScriptingComponentName';
{
        OSAComponentFunctionInline(kOSASelectScriptingComponentName, 4);
        Given a scripting component, this routine returns the name of that
        scripting component in a type that is coercable to text (typeChar).
        The generic scripting component returns the name of the default
        scripting component.  This name should be sufficient to convey to the
        user the kind of script (syntax) he is expected to write.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSACompile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSACompile(scriptingComponent: ComponentInstance; const (*var*) sourceData: AEDesc; modeFlags: SInt32; var previousAndResultingScriptID: OSAID): OSAError; external name '_OSACompile';
{
        OSAComponentFunctionInline(kOSASelectCompile, 12);
        Coerces input desc (possibly text) into a script's internal format.
        Once compiled, the script is ready to run.  The modeFlags convey
        scripting component specific information.  The previous script ID
        (result parameter) is made to refer to the newly compiled script,
        unless it was originally kOSANullScript.  In this case a new script
        ID is created and used.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errAECoercionFail:      sourceData is not compilable
            errOSAScriptError:      sourceData was a bad script (syntax error)
            errOSAInvalidID:        previousAndResultingCompiledScriptID was not
                                    valid on input
    
        ModeFlags:
            kOSAModePreventGetSource
            kOSAModeCompileIntoContext
            kOSAModeAugmentContext
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{
 *  OSACopyID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSACopyID(scriptingComponent: ComponentInstance; fromID: OSAID; var toID: OSAID): OSAError; external name '_OSACopyID';
{
        OSAComponentFunctionInline(kOSASelectCopyID, 8);
        If toID is a reference to kOSANullScript then it is updated to have a
        new scriptID value.  This call can be used to perform undo or revert
        operations on scripts. 
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
    }
{$ifc CALL_NOT_IN_CARBON}
{
 *  OSACopyScript()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function OSACopyScript(scriptingComponent: ComponentInstance; fromID: OSAID; var toID: OSAID): OSAError; external name '_OSACopyScript';
{
        OSAComponentFunctionInline(kOSASelectCopyScript, 8);
        Creates a duplicate copy of the script with the given OSAID and returns
        a new OSAID for it.  Can be used by script editors or debuggers. 
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
    }
{$endc}  {CALL_NOT_IN_CARBON}

{*************************************************************************
    OSA Optional GetSource Interface
**************************************************************************
    Scripting components that support the GetSource interface have the 
    kOSASupportsGetSource bit set in their ComponentDescription.
*************************************************************************}
{
 *  OSAGetSource()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetSource(scriptingComponent: ComponentInstance; scriptID: OSAID; desiredType: DescType; var resultingSourceData: AEDesc): OSAError; external name '_OSAGetSource';
{
        OSAComponentFunctionInline(kOSASelectGetSource, 12);
        This routine causes a compiled script to be output in a form (possibly
        text) such that it is suitable to be passed back to OSACompile.

        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSASourceNotAvailable    can't get source for this scriptID
    }
{*************************************************************************
    OSA Optional AECoercion Interface
**************************************************************************
    Scripting components that support the AECoercion interface have the 
    kOSASupportsAECoercion bit set in their ComponentDescription.
*************************************************************************}
{
 *  OSACoerceFromDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSACoerceFromDesc(scriptingComponent: ComponentInstance; const (*var*) scriptData: AEDesc; modeFlags: SInt32; var resultingScriptID: OSAID): OSAError; external name '_OSACoerceFromDesc';
{
        OSAComponentFunctionInline(kOSASelectCoerceFromDesc, 12);
        This routine causes script data to be coerced into a script value.
        If the scriptData is an AppleEvent, then the resultingScriptID is a
        compiled script ID (mode flags for OSACompile may be used in this case).
        Other scriptData descriptors create script value IDs.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    
        ModeFlags:
            kOSAModePreventGetSource
            kOSAModeCompileIntoContext
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{
 *  OSACoerceToDesc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSACoerceToDesc(scriptingComponent: ComponentInstance; scriptID: OSAID; desiredType: DescType; modeFlags: SInt32; var result: AEDesc): OSAError; external name '_OSACoerceToDesc';
{
        OSAComponentFunctionInline(kOSASelectCoerceToDesc, 16);
        This routine causes a script value to be coerced into any desired form.
        If the scriptID denotes a compiled script, then it may be coerced to 
        typeAppleEvent.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
    }
{*************************************************************************
    OSA Optional AESending Interface
**************************************************************************
    Scripting components that support the AESending interface have the 
    kOSASupportsAESending bit set in their ComponentDescription.
*************************************************************************}
{
    Scripting systems will supply default values for these procedures if they
    are not set by the client:
}
{
 *  OSASetSendProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetSendProc(scriptingComponent: ComponentInstance; sendProc: OSASendUPP; refCon: SInt32): OSAError; external name '_OSASetSendProc';
{
        OSAComponentFunctionInline(kOSASelectSetSendProc, 8);
        If sendProc is nil, the default sendProc is used.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSAGetSendProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetSendProc(scriptingComponent: ComponentInstance; var sendProc: OSASendUPP; var refCon: SInt32): OSAError; external name '_OSAGetSendProc';
{
        OSAComponentFunctionInline(kOSASelectGetSendProc, 8);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSASetCreateProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetCreateProc(scriptingComponent: ComponentInstance; createProc: OSACreateAppleEventUPP; refCon: SInt32): OSAError; external name '_OSASetCreateProc';
{
        OSAComponentFunctionInline(kOSASelectSetCreateProc, 8);
        If createProc is nil, the default createProc is used.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSAGetCreateProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetCreateProc(scriptingComponent: ComponentInstance; var createProc: OSACreateAppleEventUPP; var refCon: SInt32): OSAError; external name '_OSAGetCreateProc';
{
        OSAComponentFunctionInline(kOSASelectGetCreateProc, 8);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSASetDefaultTarget()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetDefaultTarget(scriptingComponent: ComponentInstance; const (*var*) target: AEAddressDesc): OSAError; external name '_OSASetDefaultTarget';
{
        OSAComponentFunctionInline(kOSASelectSetDefaultTarget, 4);
        This routine sets the default target application for AE sending.
        It also establishes the default target from which terminologies come.
        It is effectively like having an AppleScript "tell" statement around
        the entire program.  If this routine is not called, or if the target 
        is a null AEDesc, then the current application is the default target.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{*************************************************************************
    OSA Optional Recording Interface
**************************************************************************
    Scripting components that support the Recording interface have the 
    kOSASupportsRecording bit set in their ComponentDescription.
*************************************************************************}
{
 *  OSAStartRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAStartRecording(scriptingComponent: ComponentInstance; var compiledScriptToModifyID: OSAID): OSAError; external name '_OSAStartRecording';
{
        OSAComponentFunctionInline(kOSASelectStartRecording, 4);
        Starts recording.  If compiledScriptToModifyID is kOSANullScript, a
        new script ID is created and returned.  If the current application has
        a handler for the kOSARecordedText event, then kOSARecordedText events
        are sent to the application containing the text of each AppleEvent 
        recorded.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSARecordingIsAlreadyOn
    }
{
 *  OSAStopRecording()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAStopRecording(scriptingComponent: ComponentInstance; compiledScriptID: OSAID): OSAError; external name '_OSAStopRecording';
{
        OSAComponentFunctionInline(kOSASelectStopRecording, 4);
        If compiledScriptID is not being recorded into or recording is not
        currently on, no error is returned.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
    }
{*************************************************************************
    OSA Optional Convenience Interface
**************************************************************************
    Scripting components that support the Convenience interface have the 
    kOSASupportsConvenience bit set in their ComponentDescription.
*************************************************************************}
{
 *  OSALoadExecute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSALoadExecute(scriptingComponent: ComponentInstance; const (*var*) scriptData: AEDesc; contextID: OSAID; modeFlags: SInt32; var resultingScriptValueID: OSAID): OSAError; external name '_OSALoadExecute';
{
        OSAComponentFunctionInline(kOSASelectLoadExecute, 16);
        This routine is effectively equivalent to calling OSALoad followed by
        OSAExecute.  After execution, the compiled source is disposed.  Only the
        resulting value ID is retained.
    
        Errors:
            badComponentInstance        invalid scripting component instance
            errOSASystemError
            errOSABadStorageType:       scriptData not for this scripting component
            errOSACorruptData:          data seems to be corrupt
            errOSADataFormatObsolete    script data format is no longer supported
            errOSADataFormatTooNew      script data format is from a newer version
            errOSAInvalidID
            errOSAScriptError:          the executing script got an error
    
        ModeFlags:
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{
 *  OSACompileExecute()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSACompileExecute(scriptingComponent: ComponentInstance; const (*var*) sourceData: AEDesc; contextID: OSAID; modeFlags: SInt32; var resultingScriptValueID: OSAID): OSAError; external name '_OSACompileExecute';
{
        OSAComponentFunctionInline(kOSASelectCompileExecute, 16);
        This routine is effectively equivalent to calling OSACompile followed by
        OSAExecute.  After execution, the compiled source is disposed.  Only the
        resulting value ID is retained.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errAECoercionFail:      sourceData is not compilable
            errOSAScriptError:      sourceData was a bad script (syntax error)
            errOSAInvalidID:        previousAndResultingCompiledScriptID was not
                                    valid on input
            errOSAScriptError:      the executing script got an error
    
        ModeFlags:
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{
 *  OSADoScript()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADoScript(scriptingComponent: ComponentInstance; const (*var*) sourceData: AEDesc; contextID: OSAID; desiredType: DescType; modeFlags: SInt32; var resultingText: AEDesc): OSAError; external name '_OSADoScript';
{
        OSAComponentFunctionInline(kOSASelectDoScript, 20);
        This routine is effectively equivalent to calling OSACompile followed by
        OSAExecute and then OSADisplay.  After execution, the compiled source
        and the resulting value are is disposed.  Only the resultingText
        descriptor is retained.  If a script error occur during processing, the 
        resultingText gets the error message of the error, and errOSAScriptError
        is returned.  OSAScriptError may still be used to extract more 
        information about the particular error.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errAECoercionFail:      sourceData is not compilable or 
                                    desiredType not supported by scripting component
            errOSAScriptError:      sourceData was a bad script (syntax error)
            errOSAInvalidID:        previousAndResultingCompiledScriptID was not
                                    valid on input
            errOSAScriptError:      the executing script got an error
    
        ModeFlags:
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
            kOSAModeDisplayForHumans
    }
{*************************************************************************
    OSA Optional Dialects Interface
**************************************************************************
    Scripting components that support the Dialects interface have the 
    kOSASupportsDialects bit set in their ComponentDescription.
*************************************************************************}
{
    These calls allows an scripting component that supports different dialects
    to dynamically switch between those dialects.  Although this interface is
    specified, the particular dialect codes are scripting component dependent.
}
{
 *  OSASetCurrentDialect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetCurrentDialect(scriptingComponent: ComponentInstance; dialectCode: SInt16): OSAError; external name '_OSASetCurrentDialect';
{
        OSAComponentFunctionInline(kOSASelectSetCurrentDialect, 2);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSANoSuchDialect:    invalid dialectCode
    }
{
 *  OSAGetCurrentDialect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetCurrentDialect(scriptingComponent: ComponentInstance; var resultingDialectCode: SInt16): OSAError; external name '_OSAGetCurrentDialect';
{
        OSAComponentFunctionInline(kOSASelectGetCurrentDialect, 4);
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSAAvailableDialects()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAAvailableDialects(scriptingComponent: ComponentInstance; var resultingDialectInfoList: AEDesc): OSAError; external name '_OSAAvailableDialects';
{
        OSAComponentFunctionInline(kOSASelectAvailableDialects, 4);
        This call return an AEList containing information about each of the
        currently available dialects of a scripting component.  Each item
        is an AERecord of typeOSADialectInfo that contains at least the fields
        keyOSADialectName, keyOSADialectCode, KeyOSADialectLangCode and 
        keyOSADialectScriptCode.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSAGetDialectInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetDialectInfo(scriptingComponent: ComponentInstance; dialectCode: SInt16; selector: OSType; var resultingDialectInfo: AEDesc): OSAError; external name '_OSAGetDialectInfo';
{
        OSAComponentFunctionInline(kOSASelectGetDialectInfo, 10);
        This call gives information about the specified dialect of a scripting
        component. It returns an AEDesc whose type depends on the selector 
        specified. Available selectors are the same as the field keys for a
        dialect info record. The type of AEDesc returned is the same as the 
        type of the field that has same key as the selector.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSABadSelector
            errOSANoSuchDialect:    invalid dialectCode
    }
{
 *  OSAAvailableDialectCodeList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAAvailableDialectCodeList(scriptingComponent: ComponentInstance; var resultingDialectCodeList: AEDesc): OSAError; external name '_OSAAvailableDialectCodeList';
{
        OSAComponentFunctionInline(kOSASelectAvailableDialectCodeList, 4);
        This is alternative to OSAGetAvailableDialectCodeList. Use this call
        and  OSAGetDialectInfo to get information on dialects.
        This call return an AEList containing dialect code for each of the
        currently available dialects of a scripting component. Each dialect
        code is a short SInt16 of type typeShortInteger.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError

        Type of a dialect info record containing at least keyOSADialectName
        and keyOSADialectCode fields.

        keys for dialect info record, also used as selectors to OSAGetDialectInfo.

        Field of a typeOSADialectInfo record of typeChar.
        Field of a typeOSADialectInfo record of typeShortInteger.
        Field of a typeOSADialectInfo record of typeShortInteger.
        Field of a typeOSADialectInfo record of typeShortInteger.
    }
{*************************************************************************
    OSA Optional Event Handling Interface
**************************************************************************
    Scripting components that support the Event Handling interface have the 
    kOSASupportsEventHandling bit set in their ComponentDescription.
*************************************************************************}
{
 *  OSASetResumeDispatchProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetResumeDispatchProc(scriptingComponent: ComponentInstance; resumeDispatchProc: AEEventHandlerUPP; refCon: SInt32): OSAError; external name '_OSASetResumeDispatchProc';
{
        OSAComponentFunctionInline(kOSASelectSetResumeDispatchProc, 8);
        This function is used to set the ResumeDispatchProc that will be used
        by OSAExecuteEvent and OSADoEvent if either no event handler can be
        found in the context, or the context event hander "continues" control
        onward. The two constants kOSAUseStandardDispatch and kOSANoDispatch
        may also be passed to this routine indicating that the handler registered
        in the application with AEInstallEventHandler should be used, or no
        dispatch should occur, respectively.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }

const
	kOSAUseStandardDispatch		= $FFFFFFFF;

	{	
	        Special ResumeDispatchProc constant which may be passed to 
	        OSASetResumeDispatchProc indicating that the handler registered
	        in the application with AEInstallEventHandler should be used.
	        
	        NOTE:   Had to remove the cast (AEEventHandlerUPP).  The C compiler
	                doesn't allow pointer types to be assigned to an enum.  All
	                constants must be assigned as enums to translate properly to
	                Pascal.
	    	}
	kOSANoDispatch				= 0;

	{	
	        Special ResumeDispatchProc constant which may be passed to 
	        OSASetResumeDispatchProc indicating that no dispatch should occur.
	        
	        NOTE:   Had to remove the cast (AEEventHandlerUPP).  The C compiler
	                doesn't allow pointer types to be assigned to an enum.  All
	                constants must be assigned as enums to translate properly to
	                Pascal.
	    	}
	kOSADontUsePhac				= $0001;

	{	
	        Special refCon constant that may be given to OSASetResumeDispatchProc
	        only when kOSAUseStandardDispatch is used as the ResumeDispatchProc.
	        This causes the standard dispatch to be performed, except the phac
	        handler is not called.  This is useful during tinkerability, when
	        the phac handler is used to lookup a context associated with an event's 
	        direct parameter, and call OSAExecuteEvent or OSADoEvent.  Failure to
	        bypass the phac handler would result in an infinite loop.
	    	}
	{
	 *  OSAGetResumeDispatchProc()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function OSAGetResumeDispatchProc(scriptingComponent: ComponentInstance; var resumeDispatchProc: AEEventHandlerUPP; var refCon: SInt32): OSAError; external name '_OSAGetResumeDispatchProc';
{
        OSAComponentFunctionInline(kOSASelectGetResumeDispatchProc, 8);
        Returns the registered ResumeDispatchProc.  If no ResumeDispatchProc has
        been registered, then kOSAUseStandardDispatch (the default) is returned.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
    }
{
 *  OSAExecuteEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAExecuteEvent(scriptingComponent: ComponentInstance; const (*var*) theAppleEvent: AppleEvent; contextID: OSAID; modeFlags: SInt32; var resultingScriptValueID: OSAID): OSAError; external name '_OSAExecuteEvent';
{
        OSAComponentFunctionInline(kOSASelectExecuteEvent, 16);
        This call is similar to OSAExecute except the initial command to
        execute comes in the form of an AppleEvent.  If the contextID
        defines any event handlers for that event, they are used to process
        the event.  If no event handler can be found in the context
        errAEEventNotHandled is returned.  If an event handler is found and
        the hander "continues" control onward, the ResumeDispatchProc
        (registered with OSASetResumeDispatchProc, above) is called given the
        AppleEvent.  The result is returned as a scriptValueID.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errOSAScriptError:      the executing script got an error
            errAEEventNotHandled:   no handler for event in contextID
    
        ModeFlags:
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{
 *  OSADoEvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADoEvent(scriptingComponent: ComponentInstance; const (*var*) theAppleEvent: AppleEvent; contextID: OSAID; modeFlags: SInt32; var reply: AppleEvent): OSAError; external name '_OSADoEvent';
{
        OSAComponentFunctionInline(kOSASelectDoEvent, 16);
        This call is similar to OSADoScript except the initial command to
        execute comes in the form of an AppleEvent, and the result is an 
        AppleEvent reply record.  If the contextID defines any event handlers
        for that event, they are used to process the event.  If no event handler
        can be found in the context errAEEventNotHandled is returned.  If an
        event handler is found and the hander "continues" control onward, the
        ResumeDispatchProc (registered with OSASetResumeDispatchProc, above) is
        called given the AppleEvent.  The result is returned in the form of an
        AppleEvent reply descriptor. If at any time the script gets an error, or
        if the ResumeDispatchProc returns a reply event indicating an error,
        then the OSADoEvent call itself returns an error reply (i.e. OSADoEvent
        should never return errOSAScriptError).  Any error result returned by
        the ResumeDispatchProc will be returned by OSADoEvent.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errAEEventNotHandled:   no handler for event in contextID
    
        ModeFlags:
            kOSAModeNeverInteract
            kOSAModeCanInteract
            kOSAModeAlwaysInteract
            kOSAModeCantSwitchLayer
            kOSAModeDontReconnect
            kOSAModeDoRecord
    }
{
 *  OSAMakeContext()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAMakeContext(scriptingComponent: ComponentInstance; const (*var*) contextName: AEDesc; parentContext: OSAID; var resultingContextID: OSAID): OSAError; external name '_OSAMakeContext';
{
        OSAComponentFunctionInline(kOSASelectMakeContext, 12);
        Makes a new empty context which may be passed to OSAExecute or 
        OSAExecuteEvent.  If contextName is typeNull, an unnamed context is
        created. If parentContext is kOSANullScript then the resulting context
        does not inherit bindings from any other context.
    
        Errors:
            badComponentInstance    invalid scripting component instance
            errOSASystemError
            errOSAInvalidID
            errAECoercionFail:      contextName is invalid
    }
{
 * Debugging API
 }
{
 * Types
 }

type
	OSADebugSessionRef					= OSAID;
	OSADebugCallFrameRef				= OSAID;
	{	
	 * Constants
	 	}
	OSAProgramState 			= UInt32;
const
	eNotStarted					= 0;
	eRunnable					= 1;
	eRunning					= 2;
	eStopped					= 3;
	eTerminated					= 4;


type
	OSADebugStepKind 			= UInt32;
const
	eStepOver					= 0;
	eStepIn						= 1;
	eStepOut					= 2;
	eRun						= 3;


type
	OSALocalOrGlobal 			= UInt32;
const
	eLocal						= 0;
	eGlobal						= 1;
	eProperties					= 2;

	{	
	 * Session Information
	 	}
	keyProgramState				= $64737073 (* 'dsps' *);

	{	
	 * Call Frame Information
	 	}

type
	StatementRangePtr = ^StatementRange;
	StatementRange = record
		startPos:				UInt32;
		endPos:					UInt32;
	end;


const
	typeStatementRange			= $73726E67 (* 'srng' *);

	keyProcedureName			= $64666E6D (* 'dfnm' *);						{  typeChar  }
	keyStatementRange			= $64667372 (* 'dfsr' *);						{  typeStatementRange  }
	keyLocalsNames				= $64666C6E (* 'dfln' *);						{  typeAEList of typeChar  }
	keyGlobalsNames				= $6466676E (* 'dfgn' *);						{  typeAEList of typeChar  }
	keyParamsNames				= $6466706E (* 'dfpn' *);						{  typeAEList of typeChar  }

	{	
	 * Sessions
	 	}
	{
	 *  OSADebuggerCreateSession()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function OSADebuggerCreateSession(scriptingComponent: ComponentInstance; inScript: OSAID; inContext: OSAID; var outSession: OSADebugSessionRef): OSAError; external name '_OSADebuggerCreateSession';
{
 *  OSADebuggerGetSessionState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetSessionState(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; var outState: AERecord): OSAError; external name '_OSADebuggerGetSessionState';
{
 *  OSADebuggerSessionStep()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerSessionStep(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; inKind: OSADebugStepKind): OSAError; external name '_OSADebuggerSessionStep';
{
 *  OSADebuggerDisposeSession()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerDisposeSession(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef): OSAError; external name '_OSADebuggerDisposeSession';
{
 *  OSADebuggerGetStatementRanges()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetStatementRanges(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; var outStatementRangeArray: AEDescList): OSAError; external name '_OSADebuggerGetStatementRanges';
{  Returns an array of StatementRange objects. }
{
 *  OSADebuggerGetBreakpoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetBreakpoint(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; inSrcOffset: UInt32; var outBreakpoint: OSAID): OSAError; external name '_OSADebuggerGetBreakpoint';
{
 *  OSADebuggerSetBreakpoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerSetBreakpoint(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; inSrcOffset: UInt32; inBreakpoint: OSAID): OSAError; external name '_OSADebuggerSetBreakpoint';
{
 *  OSADebuggerGetDefaultBreakpoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetDefaultBreakpoint(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; var outBreakpoint: OSAID): OSAError; external name '_OSADebuggerGetDefaultBreakpoint';
{
 * Call Frames
 }
{
 *  OSADebuggerGetCurrentCallFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetCurrentCallFrame(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; var outCallFrame: OSADebugCallFrameRef): OSAError; external name '_OSADebuggerGetCurrentCallFrame';
{
 *  OSADebuggerGetCallFrameState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetCallFrameState(scriptingComponent: ComponentInstance; inCallFrame: OSADebugCallFrameRef; var outState: AERecord): OSAError; external name '_OSADebuggerGetCallFrameState';
{
 *  OSADebuggerGetVariable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetVariable(scriptingComponent: ComponentInstance; inCallFrame: OSADebugCallFrameRef; const (*var*) inVariableName: AEDesc; var outVariable: OSAID): OSAError; external name '_OSADebuggerGetVariable';
{
 *  OSADebuggerSetVariable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerSetVariable(scriptingComponent: ComponentInstance; inCallFrame: OSADebugCallFrameRef; const (*var*) inVariableName: AEDesc; inVariable: OSAID): OSAError; external name '_OSADebuggerSetVariable';
{
 *  OSADebuggerGetPreviousCallFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerGetPreviousCallFrame(scriptingComponent: ComponentInstance; inCurrentFrame: OSADebugCallFrameRef; var outPrevFrame: OSADebugCallFrameRef): OSAError; external name '_OSADebuggerGetPreviousCallFrame';
{
 *  OSADebuggerDisposeCallFrame()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerDisposeCallFrame(scriptingComponent: ComponentInstance; inCallFrame: OSADebugCallFrameRef): OSAError; external name '_OSADebuggerDisposeCallFrame';
{
 *  OSADebuggerCountVariables()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.5 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSADebuggerCountVariables(scriptingComponent: ComponentInstance; inSession: OSADebugSessionRef; inCallFrame: OSADebugCallFrameRef; inWhichSet: OSALocalOrGlobal; var outVariableCount: SInt32): OSAError; external name '_OSADebuggerCountVariables';
{$ALIGN MAC68K}


end.
