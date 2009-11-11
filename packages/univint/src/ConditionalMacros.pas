{
     File:       CarbonCore/ConditionalMacros.h
 
     Contains:   Set up for compiler independent conditionals
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1993-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{    Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit ConditionalMacros;
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
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
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
{$endc} {not MACOSALLINCLUDE}
{$ALIGN POWER}

{***************************************************************************************************
    UNIVERSAL_INTERFACES_VERSION
    
        0x0400 --> version 4.0 (Mac OS X only)
        0x0335 --> version 3.4 
        0x0331 --> version 3.3.1
        0x0330 --> version 3.3
        0x0320 --> version 3.2
        0x0310 --> version 3.1
        0x0301 --> version 3.0.1
        0x0300 --> version 3.0
        0x0210 --> version 2.1
        This conditional did not exist prior to version 2.1
***************************************************************************************************}
// defined in the conversion script as a macro }
// const
//   UNIVERSAL_INTERFACES_VERSION = $0400;
{***************************************************************************************************

    All TARGET_* condtionals are set up by TargetConditionals.h

***************************************************************************************************}


{***************************************************************************************************

    PRAGMA_*
    These conditionals specify whether the compiler supports particular #pragma's
    
        PRAGMA_IMPORT           - Compiler supports: #pragma import on/off/reset
        PRAGMA_ONCE             - Compiler supports: #pragma once
        PRAGMA_STRUCT_ALIGN     - Compiler supports: #pragma options align=mac68k/power/reset
        PRAGMA_STRUCT_PACK      - Compiler supports: #pragma pack(n)
        PRAGMA_STRUCT_PACKPUSH  - Compiler supports: #pragma pack(push, n)/pack(pop)
        PRAGMA_ENUM_PACK        - Compiler supports: #pragma options(!pack_enums)
        PRAGMA_ENUM_ALWAYSINT   - Compiler supports: #pragma enumsalwaysint on/off/reset
        PRAGMA_ENUM_OPTIONS     - Compiler supports: #pragma options enum=int/small/reset


    FOUR_CHAR_CODE
    This conditional is deprecated.  It was used to work around a bug in one obscure compiler that did not pack multiple characters in single quotes rationally.
    It was never intended for endian swapping.

        FourCharCode('abcd')  - Convert a four-char-code to the correct 32-bit value


    TYPE_*
    These conditionals specify whether the compiler supports particular types.

        TYPE_LONGLONG               - Compiler supports "long long" 64-bit integers
        TYPE_BOOL                   - Compiler supports "bool"
        TYPE_EXTENDED               - Compiler supports "extended" 80/96 bit floating point
        TYPE_LONGDOUBLE_IS_DOUBLE   - Compiler implements "long double" same as "double"


    FUNCTION_*
    These conditionals specify whether the compiler supports particular language extensions
    to function prototypes and definitions.

        FUNCTION_PASCAL         - Compiler supports "pascal void Foo()"
        FUNCTION_DECLSPEC       - Compiler supports "__declspec(xxx) void Foo()"
        FUNCTION_WIN32CC        - Compiler supports "void __cdecl Foo()" and "void __stdcall Foo()"

***************************************************************************************************}

{***************************************************************************************************

    Under MacOS, the classic 68k runtime has two calling conventions: pascal or C
    Under Win32, there are two calling conventions: __cdecl or __stdcall
    Headers and implementation files can use the following macros to make their
    source more portable by hiding the calling convention details:

    EXTERN_APIÅ 
    These macros are used to specify the calling convention on a function prototype.

        EXTERN_API              - Classic 68k: pascal, Win32: __cdecl
        EXTERN_API_C            - Classic 68k: C,      Win32: __cdecl
        EXTERN_API_STDCALL      - Classic 68k: pascal, Win32: __stdcall
        EXTERN_API_C_STDCALL    - Classic 68k: C,      Win32: __stdcall


    DEFINE_APIÅ 
    These macros are used to specify the calling convention on a function definition.

        DEFINE_API              - Classic 68k: pascal, Win32: __cdecl
        DEFINE_API_C            - Classic 68k: C,      Win32: __cdecl
        DEFINE_API_STDCALL      - Classic 68k: pascal, Win32: __stdcall
        DEFINE_API_C_STDCALL    - Classic 68k: C,      Win32: __stdcall


    CALLBACK_APIÅ   
    These macros are used to specify the calling convention of a function pointer.

        CALLBACK_API            - Classic 68k: pascal, Win32: __stdcall
        CALLBACK_API_C          - Classic 68k: C,      Win32: __stdcall
        CALLBACK_API_STDCALL    - Classic 68k: pascal, Win32: __cdecl
        CALLBACK_API_C_STDCALL  - Classic 68k: C,      Win32: __cdecl

***************************************************************************************************}

{***************************************************************************************************
    
    Set up TARGET_API_Å_Å values

***************************************************************************************************}

{***************************************************************************************************
    Backward compatibility for clients expecting 2.x version on ConditionalMacros.h

    GENERATINGPOWERPC       - Compiler is generating PowerPC instructions
    GENERATING68K           - Compiler is generating 68k family instructions
    GENERATING68881         - Compiler is generating mc68881 floating point instructions
    GENERATINGCFM           - Code being generated assumes CFM calling conventions
    CFMSYSTEMCALLS          - No A-traps.  Systems calls are made using CFM and UPP's
    PRAGMA_ALIGN_SUPPORTED  - Compiler supports: #pragma options align=mac68k/power/reset
    PRAGMA_IMPORT_SUPPORTED - Compiler supports: #pragma import on/off/reset
    CGLUESUPPORTED          - Clients can use all lowercase toolbox functions that take C strings instead of pascal strings

***************************************************************************************************}


{***************************************************************************************************

    OLDROUTINENAMES         - "Old" names for Macintosh system calls are allowed in source code.
                              (e.g. DisposPtr instead of DisposePtr). The names of system routine
                              are now more sensitive to change because CFM binds by name.  In the 
                              past, system routine names were compiled out to just an A-Trap.  
                              Macros have been added that each map an old name to its new name.  
                              This allows old routine names to be used in existing source files,
                              but the macros only work if OLDROUTINENAMES is true.  This support
                              will be removed in the near future.  Thus, all source code should 
                              be changed to use the new names! You can set OLDROUTINENAMES to false
                              to see if your code has any old names left in it.
    
***************************************************************************************************}


{***************************************************************************************************
 The following macros isolate the use of 68K inlines in function prototypes.
    On the Mac OS under the Classic 68K runtime, function prototypes were followed
 by a list of 68K opcodes which the compiler inserted in the generated code instead
 of a JSR.  Under Classic 68K on the Mac OS, this macro will put the opcodes
    in the right syntax.  For all other OS's and runtimes the macro suppress the opcodes.
  Example:
   
       EXTERN_P void DrawPicture(PicHandle myPicture, const Rect *dstRect)
            ONEWORDINLINE(0xA8F6);
 
***************************************************************************************************}

{***************************************************************************************************

    TARGET_CARBON                   - default: false. Switches all of the above as described.  Overrides all others
                                    - NOTE: If you set TARGET_CARBON to 1, then the other switches will be setup by
                                            ConditionalMacros, and should not be set manually.

    If you wish to do development for pre-Carbon Systems, you can set the following:

    OPAQUE_TOOLBOX_STRUCTS          - default: false. True for Carbon builds, hides struct fields.
    OPAQUE_UPP_TYPES                - default: false. True for Carbon builds, UPP types are unique and opaque.
    ACCESSOR_CALLS_ARE_FUNCTIONS    - default: false. True for Carbon builds, enables accessor functions.
    CALL_NOT_IN_CARBON              - default: true.  False for Carbon builds, hides calls not supported in Carbon.
    
    Specifically, if you are building a non-Carbon application (one that links against InterfaceLib)
    but you wish to use some of the accessor functions, you can set ACCESSOR_CALLS_ARE_FUNCTIONS to 1
    and link with CarbonAccessors.o, which implements just the accessor functions. This will help you
    preserve source compatibility between your Carbon and non-Carbon application targets.
    
    MIXEDMODE_CALLS_ARE_FUNCTIONS   - deprecated.

***************************************************************************************************}

{
     * It's possible to have ACCESSOR_CALLS_ARE_FUNCTIONS set to true and OPAQUE_TOOLBOX_STRUCTS
     * set to false, but not the other way around, so make sure the defines are not set this way.
     }
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
