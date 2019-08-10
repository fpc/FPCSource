{ Parsed from Foundation.framework NSPointerArray.h }


{ Types from NSPointerArrayConveniences }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSPointerArrayPtr = ^NSPointerArray;
{$endif}

{$ifdef CLASSES}

type
  NSPointerArray = objcclass external (NSObject, NSFastEnumerationProtocol, NSCopyingProtocol, NSCodingProtocol)
  public
    function initWithOptions (options: NSPointerFunctionsOptions): instancetype; message 'initWithOptions:'; { NS_DESIGNATED_INITIALIZER }
    function initWithPointerFunctions (functions: NSPointerFunctions): instancetype; message 'initWithPointerFunctions:'; { NS_DESIGNATED_INITIALIZER }
    class function pointerArrayWithOptions (options: NSPointerFunctionsOptions): NSPointerArray; message 'pointerArrayWithOptions:';
    class function pointerArrayWithPointerFunctions (functions: NSPointerFunctions): NSPointerArray; message 'pointerArrayWithPointerFunctions:';
    function pointerFunctions: NSPointerFunctions; message 'pointerFunctions';
    function pointerAtIndex (index: NSUInteger): pointer; message 'pointerAtIndex:';
    procedure addPointer (pointer_: pointer); message 'addPointer:';
    procedure removePointerAtIndex (index: NSUInteger); message 'removePointerAtIndex:';
    procedure insertPointer_atIndex (item: pointer; index: NSUInteger); message 'insertPointer:atIndex:';
    procedure replacePointerAtIndex_withPointer (index: NSUInteger; item: pointer); message 'replacePointerAtIndex:withPointer:';
    procedure compact; message 'compact';
    procedure setCount(newValue: NSUInteger); message 'setCount:';
    function count: NSUInteger; message 'count';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSPointerArrayConveniences = objccategory external (NSPointerArray)
    class function strongObjectsPointerArray: NSPointerArray; message 'strongObjectsPointerArray'; { available in 10_8, 6_0 }
    class function weakObjectsPointerArray: NSPointerArray; message 'weakObjectsPointerArray'; { available in 10_8, 6_0 }
    function allObjects: NSArray; message 'allObjects';
  end;
{$endif}

