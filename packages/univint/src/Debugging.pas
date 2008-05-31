{
     File:       Debugging.p
 
     Contains:   Macros to handle exceptions and assertions.
 
     Version:    Technology: Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1989-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit Debugging;
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
uses MacTypes,Files;

{
  ______________________________________________________________________________________
                                                                                        
    This file defines standard exception handling and assertion macros for              
    system-level programming in C.  Originally used in QuickDraw GX, and heavily        
    modified since, these macros are used extensively throughout Mac OS system          
    software.  Now *you* can look and feel like a system software engineer.             
                                                                                        
    To activate debugger breaks, #define DEBUG to 1 (one) before including this file.   
    Five further levels of debugging are available, selected by #defining one           
    of the following conditionals to 1 after DEBUG is defined to 1.                     
                                                                                        
        DEBUG_INTERNAL      the default; includes file and line number information      
                                                                                        
        DEBUG_EXTERNAL      used for code which must ship to developers outside         
                            your organization; no file or line number information is    
                            included in asserts                                         
                                                                                        
        DEBUG_BREAK_ONLY    where an assertion would normally be sent to the debugger,      
                            send an empty string instead.                               
                                                                                        
        PRODUCTION          used for shipping code; no debugger breaks are emitted      
                                                                                        
        PERFORMANCE         same as PRODUCTION                                          
                                                                                        
    #defining DEBUG to 0 is equivalent to #defining PRODUCTION 1 when DEBUG is 1.       
    (No code for debugger breaks is emitted in either case.)                            
                                                                                        
    Of the multitude of macros, the preferred ones are:                                 
                                                                                        
    debug_string(c-string)                                                              
        If debugging is on, c-string is printed in the debugger.                        
        In production builds, debug_string() does nothing.                              
                                                                                        
    check(expression)                                                                   
    check_noerr(error)                                                                  
        If (expression) evaluates to false, break into the debugger.                    
        In production builds, check() does nothing.                                     
        Code inside check() statements is not compiled into production builds.          
                                                                                        
    require(expression, label)                                                          
    require_noerr(expression, label)                                                    
        If (expression) evaluates to false, announce this fact via the                  
        debugger and then goto label.  In production builds, does not call              
        the debugger but still goes to label if expression is false.                    
                                                                                        
    require_action(expression, label, action)                                           
    require_noerr_action(expression, label, action)                                     
        Same as require, but executes (action) before jumping to label.                 
                                                                                        
    check_string(expression, c-string)                                                  
    require_string(expression, label, c-string)                                         
    require_noerr_string(expression, label, c-string)                                   
        If expression evaluates to false, print string and then proceed as in           
        a normal check/require statement                                                
                                                                                        
    verify(expression)                                                                  
    verify_noerr(error)                                                                 
        If debugging is on, verify is the same as check(expression).                    
        If debugging is off, verify still evaluates (expression)                        
        but ignores the result.  Code inside verify() statements                        
        is executed in both production and debug builds.                                
                                                                                        
    Common usage:                                                                       
                                                                                        
        // my pixmap is not purgeable, so locking it should never fail                  
        verify( LockPixels(myPixMap) );                                                 
        verify_noerr( DisposeThread(myThread, &threadResult, true) );                   
  ______________________________________________________________________________________
}


{
  ______________________________________________________________________________________
                                                                                        
   Before including this file, #define kComponentSignatureString to a C-string          
   containing the name of your client.                                                  
                                                                                        
   example: #define kComponentSignatureString "SurfWriter"                              
  ______________________________________________________________________________________
}


{
  ______________________________________________________________________________________
                                                                                        
    DEBUGASSERTMSG - all error reporting is routed through this macro, which calls the  
    system routine DebugAssert().  If you wish to use your own assertion/debugger break 
    routine, you can override DEBUGASSERTMSG by defining it before including this file. 
  ______________________________________________________________________________________
}


{$ALIGN MAC68K}

{
 *  DebugAssert()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DebugAssert(componentSignature: OSType; options: UInt32; assertionString: ConstCStringPtr; exceptionString: ConstCStringPtr; errorString: ConstCStringPtr; fileName: ConstCStringPtr; lineNumber: SInt32; value: UnivPtr); external name '_DebugAssert';
{
    kBlessedBusErrorBait is an address that will never be mapped
    by Mac OS 8 or 9. It is close to the middle of the 64K range from
    0x68F10000 to 0x68F1FFFF that is unmapped and cannot be accessed 
    without causing an exception. Thus, it's a good value to use for
    filling uninitialized pointers, etc.
}

const
	kBlessedBusErrorBait		= $68F168F1;


	{   TaskLevel masks }
	k68kInterruptLevelMask		= $00000007;
	kInVBLTaskMask				= $00000010;
	kInDeferredTaskMask			= $00000020;
	kInSecondaryIntHandlerMask	= $00000040;
	kInNestedInterruptMask		= $00000080;

	kComponentDebugOption		= 0;							{  optionSelectorNum to turn breaks for component on/off }

	kGetDebugOption				= 1;							{  get current debug option setting }
	kSetDebugOption				= 2;							{  set debug option }

	{
	    DebugComponentCallback
	    DebugComponentCallback is the callback into a component that registers with DebugLib.
	    It is called to get the debug option setting, or to turn a debug option on or off.
	        Inputs:
	            optionSelectorNum   The component debug option to set
	            command             The command:
	                                    kGetDebugOption     - get current debug option setting
	                                    kSetDebugOption     - set debug option
	        Outputs:
	            optionSetting       The current setting if kGetDebugOption;
	                                the new debug option if kSetDebugOption
	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	DebugComponentCallbackProcPtr = procedure(optionSelectorNum: SInt32; command: UInt32; var optionSetting: boolean);
{$elsec}
	DebugComponentCallbackProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	DebugComponentCallbackUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DebugComponentCallbackUPP = UniversalProcPtr;
{$endc}	
	{
	    TaskLevel
	    TaskLevel returns 0 if we're (probably) running at non-interrupt time.
	    There's no way to make this perfect, but this is as close as we can get.
	    If TaskLevel doesn't return 0, then the following masks can be used to learn more:
	        k68kInterruptLevelMask      = 0x00000007
	        kInVBLTaskMask              = 0x00000010
	        kInDeferredTaskMask         = 0x00000020
	        kInSecondaryIntHandlerMask  = 0x00000040
	        kInNestedInterruptMask      = 0x00000080
	}
	{
	 *  TaskLevel()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in DebugLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function TaskLevel: UInt32; external name '_TaskLevel';
{
    NewDebugComponent
    NewDebugComponent registers a component with DebugLib.
        Inputs:
            componentSignature  The unique signature of component
            componentName       The displayable string naming the component
            componentCallback   The callback into component for working with options
        Result:
            noErr                           no error
            memFullErr                      could not allocate memory
            debuggingExecutionContextErr    routine cannot be called at this time
            debuggingDuplicateSignatureErr  componentSignature already registered
            debuggingInvalidNameErr         componentName is invalid (NULL)
}
{
 *  NewDebugComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDebugComponent(componentSignature: OSType; const (*var*) componentName: Str255; componentCallback: DebugComponentCallbackUPP): OSStatus; external name '_NewDebugComponent';
{
    NewDebugOption
    NewDebugOption registers a debug option with DebugLib.
        Inputs:
            componentSignature  The signature of component to register a debug option for
            optionSelectorNum   The selector number of this debug option
            optionName          The displayable string naming this debug option
        Result:
            noErr                           no error
            memFullErr                      could not allocate memory
            debuggingExecutionContextErr    called at interrupt time
            debuggingDuplicateOptionErr     optionSelectorNum already registered
            debuggingInvalidSignatureErr    componentSignature not registered
            debuggingInvalidNameErr         optionName is invalid (NULL)
            debuggingNoCallbackErr          debugging component has no callback
}
{
 *  NewDebugOption()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDebugOption(componentSignature: OSType; optionSelectorNum: SInt32; const (*var*) optionName: Str255): OSStatus; external name '_NewDebugOption';
{
    DisposeDebugComponent
    DisposeDebugComponent removes a component registration and all related debug options from DebugLib.
        Input:
            componentSignature  The unique signature of a component
        Result:
            noErr                           no error
            debuggingExecutionContextErr    called at interrupt time
            debuggingInvalidSignatureErr    componentSignature not registered
}
{
 *  DisposeDebugComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function DisposeDebugComponent(componentSignature: OSType): OSStatus; external name '_DisposeDebugComponent';
{
    GetDebugComponentInfo
    GetDebugComponentInfo returns a component registered with DebugLib.
        Inputs:
            index               The index into the list of registered components (1-based)
        Outputs:
            componentSignature  The unique signature of a component
            componentName       The displayable string naming a component
        Result:
            noErr                           no error
            debuggingNoMatchErr             debugging component not found at this index
}
{
 *  GetDebugComponentInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDebugComponentInfo(index: UInt32; var componentSignature: OSType; var componentName: Str255): OSStatus; external name '_GetDebugComponentInfo';
{
    GetDebugOptionInfo
    GetDebugOptionInfo returns a debug option registered with DebugLib.
        Inputs:
            index               The index into the list of registered debug options (0-based);
                                    0 = kComponentDebugOption 
            componentSignature  The unique signature of a component
        Outputs:
            optionSelectorNum   The selector number of this debug option
            optionName          The displayable string naming this debug option
            optionSetting       The current debug option setting
        Result:
            noErr                           no error
            debuggingInvalidSignatureErr    componentSignature not registered
            debuggingNoMatchErr             option not found at this index
}
{
 *  GetDebugOptionInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetDebugOptionInfo(index: UInt32; componentSignature: OSType; var optionSelectorNum: SInt32; var optionName: Str255; var optionSetting: boolean): OSStatus; external name '_GetDebugOptionInfo';
{
    SetDebugOptionValue
    SetDebugOptionValue sets a debug option registered with DebugLib.
        Inputs:
            componentSignature  The unique signature of a component
            optionSelectorNum   The selector number of this debug option
            newOptionSetting    The new debug option setting
        Result:
            noErr                           no error
            debuggingInvalidSignatureErr    componentSignature not registered
            debuggingInvalidOptionErr       optionSelectorNum is not registered
}
{
 *  SetDebugOptionValue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetDebugOptionValue(componentSignature: OSType; optionSelectorNum: SInt32; newOptionSetting: boolean): OSStatus; external name '_SetDebugOptionValue';
{
    DebugAssertOutputHandler
    DebugAssertOutputHandler is the callback that registers with DebugLib to handle the
    output from DebugAssert.
        Inputs:
            "componentSignature" through "value" are the raw values passed to DebugAssert
                when an exception occurs.
            outputMsg is the string DebugAssert build which would normally be passed to
                DebugStr if a DebugAssertOutputHandler isn't installed.
}

type
{$ifc TYPED_FUNCTION_POINTERS}
	DebugAssertOutputHandlerProcPtr = procedure(componentSignature: OSType; options: UInt32; assertionString: ConstCStringPtr; exceptionString: ConstCStringPtr; errorString: ConstCStringPtr; fileName: ConstCStringPtr; lineNumber: SInt32; value: UnivPtr; outputMsg: Str255);
{$elsec}
	DebugAssertOutputHandlerProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	DebugAssertOutputHandlerUPP = ^SInt32; { an opaque UPP }
{$elsec}
	DebugAssertOutputHandlerUPP = UniversalProcPtr;
{$endc}	
	{
	    InstallDebugAssertOutputHandler
	    InstallDebugAssertOutputHandler installs a DebugAssertOutputHandler which DebugAssert calls
	    instead of DebugStr.
	        Inputs:
	            handler     the DebugAssertOutputHandler to install or NULL to switch back to
	                        the default handler (DebugStr).
	}
	{
	 *  InstallDebugAssertOutputHandler()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in DebugLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure InstallDebugAssertOutputHandler(handler: DebugAssertOutputHandlerUPP); external name '_InstallDebugAssertOutputHandler';
{
    dprintf() takes a variable argument list and 'prints' that to the debugging output
    handler.  Calling dprintf() from anything but C or C++ is tricky.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  dprintf()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure dprintf(format: ConstCStringPtr; ...); external name '_dprintf';


{   vdprintf() takes a va_args list and 'prints' that to the debugging output handler. }
{
 *  vdprintf()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in DebugLib 1.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure vdprintf(format: ConstCStringPtr; va_args_list: CStringPtr); external name '_vdprintf';
{$endc}  {CALL_NOT_IN_CARBON}


const
	uppDebugComponentCallbackProcInfo = $00000FC0;
	uppDebugAssertOutputHandlerProcInfo = $00FFFFC0;
	{
	 *  NewDebugComponentCallbackUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewDebugComponentCallbackUPP(userRoutine: DebugComponentCallbackProcPtr): DebugComponentCallbackUPP; external name '_NewDebugComponentCallbackUPP'; { old name was NewDebugComponentCallbackProc }
{
 *  NewDebugAssertOutputHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewDebugAssertOutputHandlerUPP(userRoutine: DebugAssertOutputHandlerProcPtr): DebugAssertOutputHandlerUPP; external name '_NewDebugAssertOutputHandlerUPP'; { old name was NewDebugAssertOutputHandlerProc }
{
 *  DisposeDebugComponentCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDebugComponentCallbackUPP(userUPP: DebugComponentCallbackUPP); external name '_DisposeDebugComponentCallbackUPP';
{
 *  DisposeDebugAssertOutputHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeDebugAssertOutputHandlerUPP(userUPP: DebugAssertOutputHandlerUPP); external name '_DisposeDebugAssertOutputHandlerUPP';
{
 *  InvokeDebugComponentCallbackUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDebugComponentCallbackUPP(optionSelectorNum: SInt32; command: UInt32; var optionSetting: boolean; userRoutine: DebugComponentCallbackUPP); external name '_InvokeDebugComponentCallbackUPP'; { old name was CallDebugComponentCallbackProc }
{
 *  InvokeDebugAssertOutputHandlerUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeDebugAssertOutputHandlerUPP(componentSignature: OSType; options: UInt32; assertionString: ConstCStringPtr; exceptionString: ConstCStringPtr; errorString: ConstCStringPtr; fileName: ConstCStringPtr; lineNumber: SInt32; value: UnivPtr; const (*var*) outputMsg: Str255; userRoutine: DebugAssertOutputHandlerUPP); external name '_InvokeDebugAssertOutputHandlerUPP'; { old name was CallDebugAssertOutputHandlerProc }
{
  ______________________________________________________________________________________
                                                                                        
    Tech Q&A PLAT-30 says to check bit 5 of the byte at 0xbff to                        
    determine whether MacsBug ( or any other low level debugger )                       
    is installed; I also check that MacJmp ( which points to the                        
    entry point for the debugger ) is not nil and not -1.                               
                                                                                        
    MacJmpFlag:                                                                         
        Bit 5 should be set to indicate the debugger is installed.                      
        Bit 6 should be set to indicate the debugger is initialized.                    
        Bit 7 should be clear to indicate that the debugger is NOT busy                 
                                                                                        
    Dr. MacsBug says to also check that the byte at 0xBFF isn't 0xFF.                   
  ______________________________________________________________________________________
}
{$ifc CALL_NOT_IN_CARBON}
{$endc}  {CALL_NOT_IN_CARBON}

{  no-op asserts for production code }

{______________________________________________________________________________________}
{______________________________________________________________________________________}
{______________________________________________________________________________________}


{______________________________________________________________________________________}
{______________________________________________________________________________________}
{______________________________________________________________________________________}
{______________________________________________________________________________________}
{______________________________________________________________________________________}

{______________________________________________________________________________________}
{______________________________________________________________________________________}

{______________________________________________________________________________________}

{______________________________________________________________________________________}
{______________________________________________________________________________________}
{______________________________________________________________________________________}

{______________________________________________________________________________________}

{______________________________________________________________________________________}


{______________________________________________________________________________________}


{$ALIGN MAC68K}


end.
