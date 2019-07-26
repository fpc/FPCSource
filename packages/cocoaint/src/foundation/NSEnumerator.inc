{ Parsed from Foundation.framework NSEnumerator.h }


{$ifdef TYPES}
type
  NSEnumeratorPtr = ^NSEnumerator;
  NSFastEnumerationProtocolPtr = ^NSFastEnumerationProtocol;
{$endif}

{$ifdef TYPES}
type
  NSFastEnumerationState = record
    state: culong;
    itemsPtr: idPtr;
    mutationsPtr: pculong;
    extra:array[0..4] of culong;
  end;
type
  NSFastEnumerationStatePtr = ^NSFastEnumerationState;
{$endif}

{$ifdef PROTOCOLS}

type
  NSFastEnumerationProtocol = objcprotocol external name 'NSFastEnumeration'
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSEnumerator = objcclass external (NSObject, NSFastEnumerationProtocol)
  public
    function nextObject: id; message 'nextObject';

    { Adopted protocols }
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
  end;


type
  NSExtendedEnumerator = objccategory external (NSEnumerator)
    function allObjects: NSArray; message 'allObjects';
  end;
{$endif}

