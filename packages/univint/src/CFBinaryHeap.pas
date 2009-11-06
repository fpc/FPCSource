{	CFBinaryHeap.h
	Copyright (c) 1998-2009, Apple Inc. All rights reserved.
}
{   Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, September 2005 }
{	  Pascal Translation Updated:  Jonas Maebe, <jonas@freepascal.org>, October 2009 }
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

unit CFBinaryHeap;
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
uses MacTypes,CFBase;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}

{!
        @header CFBinaryHeap
        CFBinaryHeap implements a container which stores values sorted using
        a binary search algorithm.  CFBinaryHeaps can be useful as priority
	queues.
}


type
	CFBinaryHeapCompareContext = record
		version: CFIndex;
		info: UnivPtr;
		retain: function( info: {const} UnivPtr ): UnivPtr;
		release: procedure( info: {const} UnivPtr );
		copyDescription: function( info: {const} UnivPtr ): CFStringRef;
	end;
	CFBinaryHeapCompareContextPtr = ^CFBinaryHeapCompareContext;

{!
        @typedef CFBinaryHeapCallBacks
	Structure containing the callbacks for values of a CFBinaryHeap.
        @field version The version number of the structure type being passed
                in as a parameter to the CFBinaryHeap creation functions.
                This structure is version 0.
	@field retain The callback used to add a retain for the binary heap
		on values as they are put into the binary heap.
		This callback returns the value to use as the value in the
		binary heap, which is usually the value parameter passed to
		this callback, but may be a different value if a different
		value should be added to the binary heap. The binary heap's
		allocator is passed as the first argument.
	@field release The callback used to remove a retain previously added
		for the binary heap from values as they are removed from
		the binary heap. The binary heap's allocator is passed as the
		first argument.
	@field copyDescription The callback used to create a descriptive
		string representation of each value in the binary heap. This
		is used by the CFCopyDescription() function.
	@field compare The callback used to compare values in the binary heap for
		equality in some operations.
}
type
	CFBinaryHeapCallBacks = record
	  version: CFIndex;
		retain: function( allocator: CFAllocatorRef; info: {const} UnivPtr ): UnivPtr;
		release: procedure( allocator: CFAllocatorRef; info: {const} UnivPtr );
		copyDescription: function( info: {const} UnivPtr ): CFStringRef;
		compare: function( info1, info2: {const} UnivPtr; context: UnivPtr ): CFComparisonResult;
	end;
	CFBinaryHeapCallBacksPtr = ^CFBinaryHeapCallBacks;

{!
	@constant kCFStringBinaryHeapCallBacks
	Predefined CFBinaryHeapCallBacks structure containing a set
	of callbacks appropriate for use when the values in a CFBinaryHeap
	are all CFString types.
}
var kCFStringBinaryHeapCallBacks: CFBinaryHeapCallBacks; external name '_kCFStringBinaryHeapCallBacks'; (* attribute const *)

{!
	@typedef CFBinaryHeapApplierFunction
	Type of the callback function used by the apply functions of
		CFBinaryHeap.
	@param value The current value from the binary heap.
	@param context The user-defined context parameter given to the apply
		function.
}
type
	CFBinaryHeapApplierFunction = procedure( val: {const} UnivPtr; context: UnivPtr );

{!
	@typedef CFBinaryHeapRef
	This is the type of a reference to CFBinaryHeaps.
}
type
	CFBinaryHeapRef = ^SInt32; { an opaque 32-bit type }

{!
	@function CFBinaryHeapGetTypeID
	Returns the type identifier of all CFBinaryHeap instances.
}
function CFBinaryHeapGetTypeID: CFTypeID; external name '_CFBinaryHeapGetTypeID';

{!
	@function CFBinaryHeapCreate
	 Creates a new mutable
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
	 or fixed-mutable
#endif
	 binary heap with the given values.
	@param allocator The CFAllocator which should be used to allocate
		memory for the binary heap and its storage for values. This
		parameter may be NULL in which case the current default
		CFAllocator is used. If this reference is not a valid
		CFAllocator, the behavior is undefined.
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
	@param capacity The maximum number of values that can be contained
	by the CFBinaryHeap. The binary heap starts empty, and can grow to this
		number of values (and it can have less). If this parameter
		is 0, the binary heap's maximum capacity is unlimited (or rather,
		only limited by address space and available memory
		constraints). If this parameter is negative, the behavior is
		undefined.
#else
	@param capacity A hint about the number of values that will be held
		by the CFBinaryHeap. Pass 0 for no hint. The implementation may
		ignore this hint, or may use it to optimize various
		operations. A heap's actual capacity is only limited by 
		address space and available memory constraints). If this 
		parameter is negative, the behavior is undefined.
#endif
	@param callBacks A pointer to a CFBinaryHeapCallBacks structure
		initialized with the callbacks for the binary heap to use on
		each value in the binary heap. A copy of the contents of the
		callbacks structure is made, so that a pointer to a structure
		on the stack can be passed in, or can be reused for multiple
		binary heap creations. If the version field of this callbacks
		structure is not one of the defined ones for CFBinaryHeap, the
		behavior is undefined. The retain field may be NULL, in which
		case the CFBinaryHeap will do nothing to add a retain to values
		as they are put into the binary heap. The release field may be
		NULL, in which case the CFBinaryHeap will do nothing to remove
		the binary heap's retain (if any) on the values when the
		heap is destroyed or a key-value pair is removed. If the
		copyDescription field is NULL, the binary heap will create a
		simple description for a value. If the equal field is NULL, the
		binary heap will use pointer equality to test for equality of
		values. This callbacks parameter itself may be NULL, which is
		treated as if a valid structure of version 0 with all fields
		NULL had been passed in. Otherwise,
		if any of the fields are not valid pointers to functions
		of the correct type, or this parameter is not a valid
		pointer to a CFBinaryHeapCallBacks callbacks structure,
		the behavior is undefined. If any of the values put into the
		binary heap is not one understood by one of the callback functions
		the behavior when that callback function is used is undefined.
  @param compareContext A pointer to a CFBinaryHeapCompareContext structure.
	@result A reference to the new CFBinaryHeap.
}
function CFBinaryHeapCreate( allocator: CFAllocatorRef; capacity: CFIndex; callBacks: CFBinaryHeapCallBacksPtr; const (*var*) compareContext: CFBinaryHeapCompareContext ): CFBinaryHeapRef; external name '_CFBinaryHeapCreate';

{!
	@function CFBinaryHeapCreateCopy
	 Creates a new mutable
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
	 or fixed-mutable
#endif
	 binary heap with the values from the given binary heap.
	@param allocator The CFAllocator which should be used to allocate
		memory for the binary heap and its storage for values. This
		parameter may be NULL in which case the current default
		CFAllocator is used. If this reference is not a valid
		CFAllocator, the behavior is undefined.
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
	@param capacity The maximum number of values that can be contained
		by the CFBinaryHeap. The binary heap starts empty, and can grow to this
		number of values (and it can have less). If this parameter
		is 0, the binary heap's maximum capacity is unlimited (or rather,
		only limited by address space and available memory
		constraints). If this parameter is negative, or less than the number of
    values in the given binary heap, the behavior is undefined.
#else
  @param capacity A hint about the number of values that will be held
		by the CFBinaryHeap. Pass 0 for no hint. The implementation may
		ignore this hint, or may use it to optimize various
		operations. A heap's actual capacity is only limited by
		address space and available memory constraints). 
		This parameter must be greater than or equal
		to the count of the heap which is to be copied, or the
		behavior is undefined. If this parameter is negative, the
		behavior is undefined.
#endif
	@param heap The binary heap which is to be copied. The values from the
		binary heap are copied as pointers into the new binary heap (that is,
		the values themselves are copied, not that which the values
		point to, if anything). However, the values are also
		retained by the new binary heap. The count of the new binary will
		be the same as the given binary heap. The new binary heap uses the same
		callbacks as the binary heap to be copied. If this parameter is
		not a valid CFBinaryHeap, the behavior is undefined.
	@result A reference to the new binary heap.
}
function CFBinaryHeapCreateCopy( allocator: CFAllocatorRef; capacity: CFIndex; heap: CFBinaryHeapRef ): CFBinaryHeapRef; external name '_CFBinaryHeapCreateCopy';

{!
	@function CFBinaryHeapGetCount
	Returns the number of values currently in the binary heap.
	@param heap The binary heap to be queried. If this parameter is not a valid
		CFBinaryHeap, the behavior is undefined.
	@result The number of values in the binary heap.
}
function CFBinaryHeapGetCount( heap: CFBinaryHeapRef ): CFIndex; external name '_CFBinaryHeapGetCount';

{!
	@function CFBinaryHeapGetCountOfValue
	Counts the number of times the given value occurs in the binary heap.
	@param heap The binary heap to be searched. If this parameter is not a
		valid CFBinaryHeap, the behavior is undefined.
	@param value The value for which to find matches in the binary heap. The
		compare() callback provided when the binary heap was created is
		used to compare. If the compare() callback was NULL, pointer
		equality (in C, ==) is used. If value, or any of the values
		in the binary heap, are not understood by the compare() callback,
		the behavior is undefined.
	@result The number of times the given value occurs in the binary heap.
}
function CFBinaryHeapGetCountOfValue( heap: CFBinaryHeapRef; value: {const} UnivPtr ): CFIndex; external name '_CFBinaryHeapGetCountOfValue';

{!
	@function CFBinaryHeapContainsValue
	Reports whether or not the value is in the binary heap.
	@param heap The binary heap to be searched. If this parameter is not a
		valid CFBinaryHeap, the behavior is undefined.
	@param value The value for which to find matches in the binary heap. The
		compare() callback provided when the binary heap was created is
		used to compare. If the compare() callback was NULL, pointer
		equality (in C, ==) is used. If value, or any of the values
		in the binary heap, are not understood by the compare() callback,
		the behavior is undefined.
	@result true, if the value is in the specified binary heap, otherwise false.
}
function CFBinaryHeapContainsValue( heap: CFBinaryHeapRef; value: {const} UnivPtr ): Boolean; external name '_CFBinaryHeapContainsValue';

{!
	@function CFBinaryHeapGetMinimum
	Returns the minimum value is in the binary heap.  If the heap contains several equal
                minimum values, any one may be returned.
	@param heap The binary heap to be searched. If this parameter is not a
		valid CFBinaryHeap, the behavior is undefined.
	@result A reference to the minimum value in the binary heap, or NULL if the
                binary heap contains no values.
}
function CFBinaryHeapGetMinimum( heap: CFBinaryHeapRef ): UnivPtr; external name '_CFBinaryHeapGetMinimum';

{!
	@function CFBinaryHeapGetMinimumIfPresent
	Returns the minimum value is in the binary heap, if present.  If the heap contains several equal
                minimum values, any one may be returned.
	@param heap The binary heap to be searched. If this parameter is not a
		valid CFBinaryHeap, the behavior is undefined.
        @param value A C pointer to pointer-sized storage to be filled with the minimum value in 
                the binary heap.  If this value is not a valid C pointer to a pointer-sized block
                of storage, the result is undefined.  If the result of the function is false, the value
                stored at this address is undefined.
	@result true, if a minimum value was found in the specified binary heap, otherwise false.
}
function CFBinaryHeapGetMinimumIfPresent( heap: CFBinaryHeapRef; {const} value: {variable-size-array} UnivPtrPtr ): Boolean; external name '_CFBinaryHeapGetMinimumIfPresent';

{!
	@function CFBinaryHeapGetValues
	Fills the buffer with values from the binary heap.
	@param heap The binary heap to be queried. If this parameter is not a
		valid CFBinaryHeap, the behavior is undefined.
	@param values A C array of pointer-sized values to be filled with
		values from the binary heap. The values in the C array are ordered
		from least to greatest. If this parameter is not a valid pointer to a 
                C array of at least CFBinaryHeapGetCount() pointers, the behavior is undefined.
}
type 
	CFBinaryHeapValues = array[0..($7F000000 div SizeOf(Ptr))] of Ptr;
	CFBinaryHeapValuesPtr = ^CFBinaryHeapValues;
procedure CFBinaryHeapGetValues( heap: CFBinaryHeapRef; {const} values: {variable-size-array} CFBinaryHeapValuesPtr ); external name '_CFBinaryHeapGetValues';

{!
	@function CFBinaryHeapApplyFunction
	Calls a function once for each value in the binary heap.
	@param heap The binary heap to be operated upon. If this parameter is not a
		valid CFBinaryHeap, the behavior is undefined.
	@param applier The callback function to call once for each value in
		the given binary heap. If this parameter is not a
		pointer to a function of the correct prototype, the behavior
		is undefined. If there are values in the binary heap which the
		applier function does not expect or cannot properly apply
		to, the behavior is undefined. 
	@param context A pointer-sized user-defined value, which is passed
		as the second parameter to the applier function, but is
		otherwise unused by this function. If the context is not
		what is expected by the applier function, the behavior is
		undefined.
}
procedure CFBinaryHeapApplyFunction( heap: CFBinaryHeapRef; applier: CFBinaryHeapApplierFunction; context: UnivPtr ); external name '_CFBinaryHeapApplyFunction';

{!
	@function CFBinaryHeapAddValue
	 Adds the value to the binary heap.
	@param heap The binary heap to which the value is to be added. If this parameter is not a
	 valid mutable CFBinaryHeap, the behavior is undefined.
#if MAC_OS_X_VERSION_MAX_ALLOWED <= MAC_OS_X_VERSION_10_4
	 If the binary heap is a fixed-capacity binary heap and it
	 is full before this operation, the behavior is undefined.
#endif
	@param value The value to add to the binary heap. The value is retained by
		the binary heap using the retain callback provided when the binary heap
		was created. If the value is not of the sort expected by the
		retain callback, the behavior is undefined.
}
procedure CFBinaryHeapAddValue( heap: CFBinaryHeapRef; value: {const} UnivPtr ); external name '_CFBinaryHeapAddValue';

{!
	@function CFBinaryHeapRemoveMinimumValue
	Removes the minimum value from the binary heap.
	@param heap The binary heap from which the minimum value is to be removed. If this 
                parameter is not a valid mutable CFBinaryHeap, the behavior is undefined.
}
procedure CFBinaryHeapRemoveMinimumValue( heap: CFBinaryHeapRef ); external name '_CFBinaryHeapRemoveMinimumValue';

{!
	@function CFBinaryHeapRemoveAllValues
	Removes all the values from the binary heap, making it empty.
	@param heap The binary heap from which all of the values are to be
		removed. If this parameter is not a valid mutable CFBinaryHeap,
		the behavior is undefined.
}
procedure CFBinaryHeapRemoveAllValues( heap: CFBinaryHeapRef ); external name '_CFBinaryHeapRemoveAllValues';

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
