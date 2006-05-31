{
     File:       AVLTree.p
 
     Contains:   Prototypes for routines which create, destroy, allow for
 
     Version:    Universal Interfaces 3.4.2
 
     Copyright:  © 1999-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit AVLTree;
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
uses MacTypes,MixedMode;


{$ALIGN MAC68K}


type
	AVLFlags 					= UInt32;
const
	kAVLFlagUseHandleForDataStorageMask = $00000001;

	{	 The visit stage for AVLWalk() walkProcs 	}

type
	AVLVisitStage 				= UInt16;
const
	kAVLPreOrder				= 0;
	kAVLInOrder					= 1;
	kAVLPostOrder				= 2;

	{	 The order the tree is walked or disposed of. 	}

type
	AVLOrder 					= UInt16;
const
	kAVLLeftToRight				= 0;
	kAVLRightToLeft				= 1;

	{	 The type of the node being passed to a callback proc. 	}

type
	AVLNodeType 				= UInt16;
const
	kAVLIsTree					= 0;
	kAVLIsLeftBranch			= 1;
	kAVLIsRightBranch			= 2;
	kAVLIsLeaf					= 3;
	kAVLNullNode				= 4;

	errItemAlreadyInTree		= -960;
	errNotValidTree				= -961;
	errItemNotFoundInTree		= -962;
	errCanNotInsertWhileWalkProcInProgress = -963;
	errTreeIsLocked				= -964;
	errTreeIsCorrupt			= -965;

	{   The structure of a tree.  It's opaque; don't assume it's 52 bytes in size. }

type
	AVLTreeStructPtr = ^AVLTreeStruct;
	AVLTreeStruct = record
		signature:				OSType;
		privateStuff:			array [0..11] of UInt32;
	end;

	AVLTreePtr							= ^AVLTreeStruct;
	{
	    Every tree must have a function which compares the data for two items and returns < 0, 0, or >0
	    for the items - < 0 if the first item is 'before' the second item according to some criteria,
	    == 0 if the two items are identical according to the criteria, or > 0 if the first item is
	    'after' the second item according to the criteria.  The comparison function is also passed the
	    node type, but most of the time this can be ignored.
	}
{$ifc TYPED_FUNCTION_POINTERS}
	AVLCompareItemsProcPtr = function(tree: AVLTreePtr; i1: UnivPtr; i2: UnivPtr; nd_typ: AVLNodeType): SInt32;
{$elsec}
	AVLCompareItemsProcPtr = ProcPtr;
{$endc}

	{
	    Every tree must have a itemSizeProc; this routine gets passed a pointer to the item's data and
	    returns the size of the data.  If a tree contains records of a fixed size, this function can
	    just return sizeof( that-struct ); otherwise it should calculate the size of the item based on
	    the data for the item.
	}
{$ifc TYPED_FUNCTION_POINTERS}
	AVLItemSizeProcPtr = function(tree: AVLTreePtr; itemPtr: UnivPtr): UInt32;
{$elsec}
	AVLItemSizeProcPtr = ProcPtr;
{$endc}

	{
	    A tree may have an optional disposeItemProc, which gets called whenever an item is removed
	    from the tree ( via AVLRemove() or when AVLDispose() deletes all of the items in the tree ).
	    This might be useful if the nodes in the tree own 'resources'  ( like, open files ) which
	    should be released before the item is removed.
	}
{$ifc TYPED_FUNCTION_POINTERS}
	AVLDisposeItemProcPtr = procedure(tree: AVLTreePtr; dataP: UnivPtr);
{$elsec}
	AVLDisposeItemProcPtr = ProcPtr;
{$endc}

	{
	    The common way to iterate across all of the items in a tree is via AVLWalk(), which takes
	    a walkProcPtr.  This function will get called for every item in the tree three times, as
	    the tree is being walked across.  First, the walkProc will get called with visitStage ==
	    kAVLPreOrder, at which point internally the node of the tree for the given data has just
	    been reached.  Later, this function will get called with visitStage == kAVLInOrder, and
	    lastly this function will get called with visitStage == kAVLPostOrder.
	    The 'minimum' item in the tree will get called with visitStage == kInOrder first, followed
	    by the 'next' item in the tree, up until the last item in the tree structure is called.
	    In general, you'll only care about calls to this function when visitStage == kAVLInOrder.
	}
{$ifc TYPED_FUNCTION_POINTERS}
	AVLWalkProcPtr = function(tree: AVLTreePtr; dataP: UnivPtr; visitStage: AVLVisitStage; node: AVLNodeType; level: UInt32; balance: SInt32; refCon: UnivPtr): OSErr;
{$elsec}
	AVLWalkProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	AVLCompareItemsUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AVLCompareItemsUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	AVLItemSizeUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AVLItemSizeUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	AVLDisposeItemUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AVLDisposeItemUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	AVLWalkUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AVLWalkUPP = UniversalProcPtr;
{$endc}	

const
	uppAVLCompareItemsProcInfo = $00002FF0;
	uppAVLItemSizeProcInfo = $000003F0;
	uppAVLDisposeItemProcInfo = $000003C0;
	uppAVLWalkProcInfo = $000FEBE0;
	{
	 *  NewAVLCompareItemsUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewAVLCompareItemsUPP(userRoutine: AVLCompareItemsProcPtr): AVLCompareItemsUPP; external name '_NewAVLCompareItemsUPP'; { old name was NewAVLCompareItemsProc }
{
 *  NewAVLItemSizeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewAVLItemSizeUPP(userRoutine: AVLItemSizeProcPtr): AVLItemSizeUPP; external name '_NewAVLItemSizeUPP'; { old name was NewAVLItemSizeProc }
{
 *  NewAVLDisposeItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewAVLDisposeItemUPP(userRoutine: AVLDisposeItemProcPtr): AVLDisposeItemUPP; external name '_NewAVLDisposeItemUPP'; { old name was NewAVLDisposeItemProc }
{
 *  NewAVLWalkUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewAVLWalkUPP(userRoutine: AVLWalkProcPtr): AVLWalkUPP; external name '_NewAVLWalkUPP'; { old name was NewAVLWalkProc }
{
 *  DisposeAVLCompareItemsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAVLCompareItemsUPP(userUPP: AVLCompareItemsUPP); external name '_DisposeAVLCompareItemsUPP';
{
 *  DisposeAVLItemSizeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAVLItemSizeUPP(userUPP: AVLItemSizeUPP); external name '_DisposeAVLItemSizeUPP';
{
 *  DisposeAVLDisposeItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAVLDisposeItemUPP(userUPP: AVLDisposeItemUPP); external name '_DisposeAVLDisposeItemUPP';
{
 *  DisposeAVLWalkUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAVLWalkUPP(userUPP: AVLWalkUPP); external name '_DisposeAVLWalkUPP';
{
 *  InvokeAVLCompareItemsUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAVLCompareItemsUPP(tree: AVLTreePtr; i1: UnivPtr; i2: UnivPtr; nd_typ: AVLNodeType; userRoutine: AVLCompareItemsUPP): SInt32; external name '_InvokeAVLCompareItemsUPP'; { old name was CallAVLCompareItemsProc }
{
 *  InvokeAVLItemSizeUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAVLItemSizeUPP(tree: AVLTreePtr; itemPtr: UnivPtr; userRoutine: AVLItemSizeUPP): UInt32; external name '_InvokeAVLItemSizeUPP'; { old name was CallAVLItemSizeProc }
{
 *  InvokeAVLDisposeItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeAVLDisposeItemUPP(tree: AVLTreePtr; dataP: UnivPtr; userRoutine: AVLDisposeItemUPP); external name '_InvokeAVLDisposeItemUPP'; { old name was CallAVLDisposeItemProc }
{
 *  InvokeAVLWalkUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAVLWalkUPP(tree: AVLTreePtr; dataP: UnivPtr; visitStage: AVLVisitStage; node: AVLNodeType; level: UInt32; balance: SInt32; refCon: UnivPtr; userRoutine: AVLWalkUPP): OSErr; external name '_InvokeAVLWalkUPP'; { old name was CallAVLWalkProc }
{
    Create an AVL tree.  The compareItemsProc and the sizeItemProc are required; disposeItemProc is
    optional and can be nil.  The refCon is stored with the list, and is passed back to the
    compareItemsProc, sizeItemProc, and disposeItemsProc calls.  The allocation of the tree ( and all
    nodes later added to the list with AVLInsert ) will be created in what is the current zone at the
    time AVLInit() is called.  Always call AVLDispose() to dispose of a list created with AVLInit().
}
{
 *  AVLInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLInit(flags: UInt32; compareItemsProc: AVLCompareItemsUPP; sizeItemProc: AVLItemSizeUPP; disposeItemProc: AVLDisposeItemUPP; refCon: UnivPtr; var tree: AVLTreePtr): OSErr; external name '_AVLInit';
{
    Dispose of an AVL tree.  This will dispose of each item in the tree in the order specified,
    call the tree's disposeProc proc for each item, and then dispose of the space allocated for
    the tree itself.
}
{
 *  AVLDispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLDispose(var tree: AVLTreePtr; order: AVLOrder): OSErr; external name '_AVLDispose';
{
    Iterate across all of the items in the tree, in the order specified.  kLeftToRight is
    basically lowest-to-highest order, kRightToLeft is highest-to-lowest order.  For each
    node in the tree, it will call the walkProc with three messages ( at the appropriate 
    time ).  First, with kAVLPreOrder when the walking gets to this node in the tree,
    before handling either the left or right subtree, secondly, with kAVLInOrder after
    handling one subtree but before handling the other, and lastly with kAVLPostOrder after
    handling both subtrees.  If you want to handle items in order, then only do something
    if the visit stage is kAVLInOrder.  You can only call AVLRemove() from inside a walkProc
    if visit stage is kAVLPostOrder ( because if you remove a node during the pre or in order
    stages you will corrupt the list ) OR if you return a non-zero result from the walkProc
    call which called AVLRemove() to immediately terminate the walkProc.  Do not call AVLInsert()
    to insert a node into the tree from inside a walkProc.
    The walkProc function gets called with the AVLTreePtr, a pointer to the data for the
    current node ( which you can change in place as long as you do not affect the order within
    the tree ), the visit stage, the type of the current node ( leaf node, right or left branch,
    or full tree ), the level within the tree ( the root is level 1 ), the balance for the
    current node, and the refCon passed to AVLWalk().  This refCon is different from the one passed
    into AVLInit(); use AVLGetRefCon() to get that refCon if you want it inside a walkProc.
    ( Most walkProcs will not care about the values for node type, level, or balance. )
}
{
 *  AVLWalk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLWalk(tree: AVLTreePtr; walkProc: AVLWalkUPP; order: AVLOrder; walkRefCon: UnivPtr): OSErr; external name '_AVLWalk';
{   Return  the number of items in the given tree. }
{
 *  AVLCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLCount(tree: AVLTreePtr; var count: UInt32): OSErr; external name '_AVLCount';
{
    Return the one-based index-th item from the tree by putting it's data at dataPtr
    if dataPtr is non-nil, and it's size into *itemSize if itemSize is non-nil.
    If index is out of range, return errItemNotFoundInTree.  ( Internally, this does
    an AVLWalk(), so the tree can not be modified while this call is in progress ).
}
{
 *  AVLGetIndItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLGetIndItem(tree: AVLTreePtr; index: UInt32; dataPtr: UnivPtr; var itemSize: UInt32): OSErr; external name '_AVLGetIndItem';
{
    Insert the given item into the tree.  This will call the tree's sizeItemProc
    to determine how big the item at data is, and then will make a copy of the
    item and insert it into the tree in the appropriate place.  If an item already
    exists in the tree with the same key ( so that the compareItemsUPP returns 0
    when asked to compare this item to an existing one ), then it will return
    errItemNotFoundInTree.
}
{
 *  AVLInsert()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLInsert(tree: AVLTreePtr; data: UnivPtr): OSErr; external name '_AVLInsert';
{
    Remove any item from the tree with the given key.  If dataPtr != nil, then
    copy the item's data to dataPtr before removing it from the tree.  Before
    removing the item, call the tree's disposeItemProc to let it release anything
    used by the data in the tree.  It is not necessary to fill in a complete
    record for key, only that the compareItemsProc return 0 when asked to compare
    the data at key with the node in the tree to be deleted.  If the item cannot
    be found in the tree, this will return errItemNotFoundInTree.
}
{
 *  AVLRemove()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLRemove(tree: AVLTreePtr; key: UnivPtr; dataPtr: UnivPtr; var itemSize: UInt32): OSErr; external name '_AVLRemove';
{
    Find the item in the tree with the given key, and return it's data in
    dataPtr ( if dataPtr != nil ), and it's size in *itemSize ( if itemSize
    != nil ).  It is not necessary to fill in a complete record for key,
    only that the compareItemsProc return 0 when asked to compare the data
    at key with the node in the tree to be deleted.  If the item cannot
    be found in the tree, this will return errItemNotFoundInTree.
}
{
 *  AVLFind()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLFind(tree: AVLTreePtr; key: UnivPtr; dataPtr: UnivPtr; var itemSize: UInt32): OSErr; external name '_AVLFind';
{
    Get the refCon for the given tree ( set in AVLInit ) and return it.
    If the given tree is invalid, then return nil.
}
{
 *  AVLGetRefcon()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AVLGetRefcon(tree: AVLTreePtr; var refCon: UnivPtr): OSErr; external name '_AVLGetRefcon';
{
    Get the refCon for the given tree ( set in AVLInit ) and return it.
    If the given tree is invalid, then return nil.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  AVLLockTree()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AVLLockTree(tree: AVLTreePtr): OSErr; external name '_AVLLockTree';
{
    Get the refCon for the given tree ( set in AVLInit ) and return it.
    If the given tree is invalid, then return nil.
}
{
 *  AVLUnlockTree()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AVLUnlockTree(tree: AVLTreePtr): OSErr; external name '_AVLUnlockTree';
{
    Get the refCon for the given tree ( set in AVLInit ) and return it.
    If the given tree is invalid, then return nil.
}
{
 *  AVLCheckTree()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.2.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function AVLCheckTree(tree: AVLTreePtr): OSErr; external name '_AVLCheckTree';
{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
