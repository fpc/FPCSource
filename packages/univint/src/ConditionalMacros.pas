{
     File:       ConditionalMacros.p
 
     Contains:   Set up for compiler independent conditionals
 
     Version:    Technology: Universal Interface Files
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc., all rights reserved
 
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

unit ConditionalMacros;
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
{$ALIGN MAC68K}

{***************************************************************************************************
    UNIVERSAL_INTERFACES_VERSION
    
        0x0400 --> version 4.0 (Mac OS X only)
        0x0341 --> version 3.4.1 
        0x0340 --> version 3.4 
        0x0331 --> version 3.3.1
        0x0330 --> version 3.3
        0x0320 --> version 3.2
        0x0310 --> version 3.1
        0x0301 --> version 3.0.1
        0x0300 --> version 3.0
        0x0210 --> version 2.1
        This conditional did not exist prior to version 2.1
***************************************************************************************************}


{***************************************************************************************************

    TARGET_CPU_Å    
    These conditionals specify which microprocessor instruction set is being
    generated.  At most one of these is true, the rest are false.

        TARGET_CPU_PPC          - Compiler is generating PowerPC instructions
        TARGET_CPU_68K          - Compiler is generating 680x0 instructions
        TARGET_CPU_X86          - Compiler is generating x86 instructions
        TARGET_CPU_MIPS         - Compiler is generating MIPS instructions
        TARGET_CPU_SPARC        - Compiler is generating Sparc instructions
        TARGET_CPU_ALPHA        - Compiler is generating Dec Alpha instructions


    TARGET_OS_Å 
    These conditionals specify in which Operating System the generated code will
    run. At most one of the these is true, the rest are false.

        TARGET_OS_MAC           - Generate code will run under Mac OS
        TARGET_OS_WIN32         - Generate code will run under 32-bit Windows
        TARGET_OS_UNIX          - Generate code will run under some unix 


    TARGET_RT_Å 
    These conditionals specify in which runtime the generated code will
    run. This is needed when the OS and CPU support more than one runtime
    (e.g. MacOS on 68K supports CFM68K and Classic 68k).

        TARGET_RT_LITTLE_ENDIAN - Generated code uses little endian format for integers
        TARGET_RT_BIG_ENDIAN    - Generated code uses big endian format for integers    
        TARGET_RT_MAC_CFM       - TARGET_OS_MAC is true and CFM68K or PowerPC CFM (TVectors) are used
        TARGET_RT_MAC_MACHO     - TARGET_OS_MAC is true and Mach-O style runtime
        TARGET_RT_MAC_68881     - TARGET_OS_MAC is true and 68881 floating point instructions used  


    TARGET__API_Å_Å 
    These conditionals are used to differentiate between sets of API's on the same
    processor under the same OS.  The first section after _API_ is the OS.  The
    second section is the API set.  Unlike TARGET_OS_ and TARGET_CPU_, these
    conditionals are not mutally exclusive. This file will attempt to auto-configure
    all TARGET_API_Å_Å values, but will often need a TARGET_API_Å_Å value predefined
    in order to disambiguate.
    
        TARGET_API_MAC_OS8      - Code is being compiled to run on System 7 through Mac OS 8.x
        TARGET_API_MAC_CARBON   - Code is being compiled to run on Mac OS 8 and Mac OS X via CarbonLib
        TARGET_API_MAC_OSX      - Code is being compiled to run on Mac OS X


    PRAGMA_Å
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
    This conditional does the proper byte swapping to assue that a four character code (e.g. 'TEXT')
    is compiled down to the correct value on all compilers.

        FourCharCode('abcd')  - Convert a four-char-code to the correct 32-bit value


    TYPE_Å
    These conditionals specify whether the compiler supports particular types.

        TYPE_LONGLONG               - Compiler supports "long long" 64-bit integers
        TYPE_BOOL                   - Compiler supports "bool"
        TYPE_EXTENDED               - Compiler supports "extended" 80/96 bit floating point
        TYPE_LONGDOUBLE_IS_DOUBLE   - Compiler implements "long double" same as "double"


    FUNCTION_Å
    These conditionals specify whether the compiler supports particular language extensions
    to function prototypes and definitions.

        FUNCTION_PASCAL         - Compiler supports "pascal void Foo()"
        FUNCTION_DECLSPEC       - Compiler supports "__declspec(xxx) void Foo()"
        FUNCTION_WIN32CC        - Compiler supports "void __cdecl Foo()" and "void __stdcall Foo()"

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


end.
