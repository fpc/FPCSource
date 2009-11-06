{
     File:       CarbonCore/Math64.h
 
     Contains:   64-bit integer math Interfaces.
 
     Version:    CarbonCore-859.2~1
 
     Copyright:  © 1994-2008 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit Math64;
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
	{$setc TARGET_CPU_PPC := TFALSE}
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
uses MacTypes,ConditionalMacros;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{
 *  S64Max()
 *  
 *  Discussion:
 *    Returns largest possible SInt64 value
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Max: SInt64; external name '_S64Max';

{
 *  S64Min()
 *  
 *  Discussion:
 *    Returns smallest possible SInt64 value
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Min: SInt64; external name '_S64Min';


{
 *  S64Add()
 *  
 *  Discussion:
 *    Adds two integers, producing an integer result.  If an overflow
 *    occurs the result is congruent mod (2^64) as if the operands and
 *    result were unsigned.  No overflow is signaled.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Add( left: SInt64; right: SInt64 ): SInt64; external name '_S64Add';


{
 *  S64Subtract()
 *  
 *  Discussion:
 *    Subtracts two integers, producing an integer result.  If an
 *    overflow occurs the result is congruent mod (2^64) as if the
 *    operands and result were unsigned.  No overflow is signaled.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Subtract( left: SInt64; right: SInt64 ): SInt64; external name '_S64Subtract';


{
 *  S64Negate()
 *  
 *  Discussion:
 *    Returns the additive inverse of a signed number (i.e. it returns
 *    0 - the number).  S64Negate (S64Min) is not representable (in
 *    fact, it returns S64Min).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Negate( value: SInt64 ): SInt64; external name '_S64Negate';


{$ifc NOT TYPE_LONGLONG}
{
 *  S64Absolute()
 *  
 *  Discussion:
 *    Returns the absolute value of the number (i.e. the number if it
 *    is positive, or 0 - the number if it is negative). Disabled for
 *    compilers that support long long until llabs() is available
 *    everywhere.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Absolute( value: SInt64 ): SInt64; external name '_S64Absolute';

{$endc}

{
 *  S64Multiply()
 *  
 *  Discussion:
 *    Multiplies two signed numbers, producing a signed result. 
 *    Overflow is ignored and the low-order part of the product is
 *    returned.  The sign of the result is not guaranteed to be correct
 *    if the magnitude of the product is not representable.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Multiply( left: SInt64; right: SInt64 ): SInt64; external name '_S64Multiply';


{$ifc CALL_NOT_IN_CARBON}
{
 *  S64Mod()
 *  
 *  Discussion:
 *    Returns the remainder of divide of dividend by divisor.  The sign
 *    of the remainder is the same as the sign of the dividend (i.e.,
 *    it takes the absolute values of the operands, does the division,
 *    then fixes the sign of the quotient and remainder).
 *  
 *  Availability:
 *    Implemented by client
 }
function S64Mod( dividend: SInt64; divisor: SInt64 ): SInt64; external name '_S64Mod';


{$endc}  {CALL_NOT_IN_CARBON}

{
 *  S64Divide()
 *  
 *  Discussion:
 *    Divides dividend by divisor, returning the quotient.  The
 *    remainder is returned in *remainder if remainder (the pointer) is
 *    non-NULL. The sign of the remainder is the same as the sign of
 *    the dividend (i.e. it takes the absolute values of the operands,
 *    does the division, then fixes the sign of the quotient and
 *    remainder).  If the divisor is zero, then S64Max() will be
 *    returned (or S64Min() if the dividend is negative), and the
 *    remainder will be the dividend; no error is reported.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Divide( dividend: SInt64; divisor: SInt64; remainder: SInt64Ptr ): SInt64; external name '_S64Divide';


{
 *  S64Set()
 *  
 *  Discussion:
 *    Given an SInt32, returns an SInt64 with the same value.  Use this
 *    routine instead of coding 64-bit constants (at least when the
 *    constant will fit in an SInt32).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Set( value: SInt32 ): SInt64; external name '_S64Set';


{
 *  S64SetU()
 *  
 *  Discussion:
 *    Given a UInt32, returns a SInt64 with the same value.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64SetU( value: UInt32 ): SInt64; external name '_S64SetU';

{
 *  S32Set()
 *  
 *  Discussion:
 *    Given an SInt64, returns an SInt32 by discarding the high-order
 *    32 bits.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S32Set( value: SInt64 ): SInt32; external name '_S32Set';


{
 *  S64And()
 *  
 *  Discussion:
 *    Returns one if left and right are non-zero, otherwise returns zero
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64And( left: SInt64; right: SInt64 ): Boolean; external name '_S64And';


{
 *  S64Or()
 *  
 *  Discussion:
 *    Returns one if left or right are non-zero, otherwise returns zero
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Or( left: SInt64; right: SInt64 ): Boolean; external name '_S64Or';


{
 *  S64Eor()
 *  
 *  Discussion:
 *    Returns one if left xor right are non-zero, otherwise returns zero
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Eor( left: SInt64; right: SInt64 ): Boolean; external name '_S64Eor';


{
 *  S64Not()
 *  
 *  Discussion:
 *    Returns one if value is non-zero, otherwisze returns zero.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64Not( value: SInt64 ): Boolean; external name '_S64Not';


{
 *  S64Compare()
 *  
 *  Discussion:
 *    Given two signed numbers, left and right, returns an SInt32 that
 *    compares with zero the same way left compares with right.  If you
 *    wanted to perform a comparison on 64-bit integers of the
 *    form:
 *    operand_1 <operation> operand_2
 *    then you could use an expression of the form:
 *     xxxS64Compare(operand_1,operand_2) <operation> 0
 *    to test for the same condition. CAUTION: DO NOT depend on the
 *    exact value returned by this routine. Only the sign (i.e.
 *    positive, zero, or negative) of the result is guaranteed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function S64Compare( left: SInt64; right: SInt64 ): SInt32; external name '_S64Compare';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  S64BitwiseAnd()
 *  
 *  Discussion:
 *    bitwise AND
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64BitwiseAnd( left: SInt64; right: SInt64 ): SInt64; external name '_S64BitwiseAnd';


{
 *  S64BitwiseOr()
 *  
 *  Discussion:
 *    bitwise OR
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64BitwiseOr( left: SInt64; right: SInt64 ): SInt64; external name '_S64BitwiseOr';


{
 *  S64BitwiseEor()
 *  
 *  Discussion:
 *    bitwise XOR
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64BitwiseEor( left: SInt64; right: SInt64 ): SInt64; external name '_S64BitwiseEor';


{
 *  S64BitwiseNot()
 *  
 *  Discussion:
 *    bitwise negate
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64BitwiseNot( value: SInt64 ): SInt64; external name '_S64BitwiseNot';


{
 *  S64ShiftRight()
 *  
 *  Discussion:
 *    Arithmetic shift of value by the lower 7 bits of the shift.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64ShiftRight( value: SInt64; shift: UInt32 ): SInt64; external name '_S64ShiftRight';


{
 *  S64ShiftLeft()
 *  
 *  Discussion:
 *    Logical shift of value by the lower 7 bits of the shift.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function S64ShiftLeft( value: SInt64; shift: UInt32 ): SInt64; external name '_S64ShiftLeft';


{
 *  U64Max()
 *  
 *  Discussion:
 *    Returns largest possible UInt64 value
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Max: UInt64; external name '_U64Max';

{
 *  U64Add()
 *  
 *  Discussion:
 *    Adds two unsigned integers, producing an integer result.  If an
 *    overflow occurs the result is congruent mod (2^64) as if the
 *    operands and result were unsigned.  No overflow is signaled.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Add( left: UInt64; right: UInt64 ): UInt64; external name '_U64Add';

{
 *  U64Subtract()
 *  
 *  Discussion:
 *    Subtracts two unsigned integers, producing an integer result.  If
 *    an overflow occurs the result is congruent mod (2^64) as if the
 *    operands and result were unsigned.  No overflow is signaled.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Subtract( left: UInt64; right: UInt64 ): UInt64; external name '_U64Subtract';


{
 *  U64Multiply()
 *  
 *  Discussion:
 *    Multiplies two unsigned numbers, producing a signed result. 
 *    Overflow is ignored and the low-order part of the product is
 *    returned.  The sign of the result is not guaranteed to be correct
 *    if the magnitude of the product is not representable.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Multiply( left: UInt64; right: UInt64 ): UInt64; external name '_U64Multiply';


{$ifc CALL_NOT_IN_CARBON}
{
 *  U64Mod()
 *  
 *  Discussion:
 *    Returns the remainder of divide of dividend by divisor.  The sign
 *    of the remainder is the same as the sign of the dividend (i.e.,
 *    it takes the absolute values of the operands, does the division,
 *    then fixes the sign of the quotient and remainder).
 *  
 *  Availability:
 *    Implemented by client
 }
function U64Mod( dividend: UInt64; divisor: UInt64 ): UInt64; external name '_U64Mod';


{$endc}  {CALL_NOT_IN_CARBON}

{
 *  U64Divide()
 *  
 *  Discussion:
 *    Divides dividend by divisor, returning the quotient.  The
 *    remainder is returned in *remainder if remainder (the pointer) is
 *    non-NULL. The sign of the remainder is the same as the sign of
 *    the dividend (i.e. it takes the absolute values of the operands,
 *    does the division, then fixes the sign of the quotient and
 *    remainder).  If the divisor is zero, then U64Max() will be
 *    returned (or U64Min() if the dividend is negative), and the
 *    remainder will be the dividend; no error is reported.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Divide( dividend: UInt64; divisor: UInt64; remainder: UInt64Ptr ): UInt64; external name '_U64Divide';


{
 *  U64Set()
 *  
 *  Discussion:
 *    Given an SInt32, returns an UInt64 with the same value.  Use this
 *    routine instead of coding 64-bit constants (at least when the
 *    constant will fit in an SInt32).
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Set( value: SInt32 ): UInt64; external name '_U64Set';


{
 *  U64SetU()
 *  
 *  Discussion:
 *    Given a UInt32, returns a UInt64 with the same value.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64SetU( value: UInt32 ): UInt64; external name '_U64SetU';

{
 *  U32SetU()
 *  
 *  Discussion:
 *    Given an UInt64, returns an UInt32 by discarding the high-order
 *    32 bits.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U32SetU( value: UInt64 ): UInt32; external name '_U32SetU';


{
 *  U64And()
 *  
 *  Discussion:
 *    Returns one if left and right are non-zero, otherwise returns zero
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64And( left: UInt64; right: UInt64 ): Boolean; external name '_U64And';


{
 *  U64Or()
 *  
 *  Discussion:
 *    Returns one if left or right are non-zero, otherwise returns zero
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Or( left: UInt64; right: UInt64 ): Boolean; external name '_U64Or';


{
 *  U64Eor()
 *  
 *  Discussion:
 *    Returns one if left xor right are non-zero, otherwise returns zero
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Eor( left: UInt64; right: UInt64 ): Boolean; external name '_U64Eor';


{
 *  U64Not()
 *  
 *  Discussion:
 *    Returns one if value is non-zero, otherwisze returns zero.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64Not( value: UInt64 ): Boolean; external name '_U64Not';


{
 *  U64Compare()
 *  
 *  Discussion:
 *    Given two unsigned numbers, left and right, returns an SInt32
 *    that compares with zero the same way left compares with right. 
 *    If you wanted to perform a comparison on 64-bit integers of the
 *    form:
 *    operand_1 <operation> operand_2
 *    then you could use an expression of the form:
 *     xxxU64Compare(operand_1,operand_2) <operation> 0
 *    to test for the same condition. CAUTION: DO NOT depend on the
 *    exact value returned by this routine. Only the sign (i.e.
 *    positive, zero, or negative) of the result is guaranteed.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   not available
 }
function U64Compare( left: UInt64; right: UInt64 ): SInt32; external name '_U64Compare';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


{
 *  U64BitwiseAnd()
 *  
 *  Discussion:
 *    bitwise AND
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64BitwiseAnd( left: UInt64; right: UInt64 ): UInt64; external name '_U64BitwiseAnd';


{
 *  U64BitwiseOr()
 *  
 *  Discussion:
 *    bitwise OR
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64BitwiseOr( left: UInt64; right: UInt64 ): UInt64; external name '_U64BitwiseOr';


{
 *  U64BitwiseEor()
 *  
 *  Discussion:
 *    bitwise XOR
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64BitwiseEor( left: UInt64; right: UInt64 ): UInt64; external name '_U64BitwiseEor';


{
 *  U64BitwiseNot()
 *  
 *  Discussion:
 *    bitwise negate
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64BitwiseNot( value: UInt64 ): UInt64; external name '_U64BitwiseNot';


{
 *  U64ShiftRight()
 *  
 *  Discussion:
 *    Arithmetic shift of value by the lower 7 bits of the shift.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64ShiftRight( value: UInt64; shift: UInt32 ): UInt64; external name '_U64ShiftRight';


{
 *  U64ShiftLeft()
 *  
 *  Discussion:
 *    Logical shift of value by the lower 7 bits of the shift.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function U64ShiftLeft( value: UInt64; shift: UInt32 ): UInt64; external name '_U64ShiftLeft';


{
 *  UInt64ToSInt64()
 *  
 *  Discussion:
 *    converts UInt64 -> SInt64
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function UInt64ToSInt64( value: UInt64 ): SInt64; external name '_UInt64ToSInt64';


{
 *  SInt64ToUInt64()
 *  
 *  Discussion:
 *    converts SInt64 -> UInt64
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function SInt64ToUInt64( value: SInt64 ): UInt64; external name '_SInt64ToUInt64';


{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
