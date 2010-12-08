{ Parsed from Foundation.framework NSEnumerator.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSENUMERATOR_PAS_T}
{$define NSENUMERATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSENUMERATOR_PAS_R}
{$define NSENUMERATOR_PAS_R}

{ Records }
type
  NSFastEnumerationState = record
    state: culong;
    itemsPtr: idPtr;
    mutationsPtr: pculong;
        extra: array[0..4] of culong;
  end;
NSFastEnumerationStatePtr = ^NSFastEnumerationState;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSENUMERATOR_PAS_F}
{$define NSENUMERATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSENUMERATOR_PAS_S}
{$define NSENUMERATOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFastEnumerationProtocol = objcprotocol;
  NSEnumerator = objcclass;
  NSEnumeratorPointer = ^NSEnumerator;
  NSEnumeratorPtr = NSEnumeratorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSENUMERATOR_PAS_C}
{$define NSENUMERATOR_PAS_C}

{ NSEnumerator }
  NSEnumerator = objcclass(NSObject, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSEnumerator; message 'alloc';

    function nextObject: id; message 'nextObject';

    { Adopted Protocols }
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
  end; external;

{ NSExtendedEnumeratorCategory }
  NSExtendedEnumeratorCategory = objccategory(NSEnumerator)
    function allObjects: NSArray; message 'allObjects';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSENUMERATOR_PAS_P}
{$define NSENUMERATOR_PAS_P}
  
{ NSFastEnumeration Protocol }
  NSFastEnumerationProtocol = objcprotocol
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
  end; external name 'NSFastEnumeration';
{$endif}
{$endif}
