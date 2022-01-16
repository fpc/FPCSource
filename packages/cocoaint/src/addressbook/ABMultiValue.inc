{ Parsed from AddressBook.framework ABMultiValue.h }


{ Types from ABMultiValue }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  ABMultiValuePtr = ^ABMultiValue;
  ABMutableMultiValuePtr = ^ABMutableMultiValue;
{$endif}

{$ifdef CLASSES}

type
  ABMultiValue = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSFastEnumerationProtocol)
  private
    _identifiers: NSMutableArray;
    _labels: NSMutableArray;
    _values: NSMutableArray;
    _primaryIdentifier: NSString;
  public
    function count: NSUInteger; message 'count';
    function valueAtIndex (index: NSUInteger): id; message 'valueAtIndex:';
    function labelAtIndex (index: NSUInteger): NSString; message 'labelAtIndex:';
    function identifierAtIndex (index: NSUInteger): NSString; message 'identifierAtIndex:';
    function indexForIdentifier (identifier: NSString): NSUInteger; message 'indexForIdentifier:';
    function primaryIdentifier: NSString; message 'primaryIdentifier';
    function propertyType: ABPropertyType; message 'propertyType';
    {$if (MAC_OS_X_VERSION_10_5 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
    function valueForIdentifier (identifier: NSString): id; message 'valueForIdentifier:';
    function labelForIdentifier (identifier: NSString): id; message 'labelForIdentifier:';
    {$endif}

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
  end;

type
  ABMutableMultiValue = objcclass external (ABMultiValue)
  public
    function addValue_withLabel (value: id; label_: NSString): NSString; message 'addValue:withLabel:';
    function insertValue_withLabel_atIndex (value: id; label_: NSString; index: NSUInteger): NSString; message 'insertValue:withLabel:atIndex:';
    function removeValueAndLabelAtIndex (index: NSUInteger): ObjCBOOL; message 'removeValueAndLabelAtIndex:';
    function replaceValueAtIndex_withValue (index: NSUInteger; value: id): ObjCBOOL; message 'replaceValueAtIndex:withValue:';
    function replaceLabelAtIndex_withLabel (index: NSUInteger; label_: NSString): ObjCBOOL; message 'replaceLabelAtIndex:withLabel:';
    function setPrimaryIdentifier (identifier: NSString): ObjCBOOL; message 'setPrimaryIdentifier:';
  end;
{$endif}

