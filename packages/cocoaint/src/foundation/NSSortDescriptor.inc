{ Parsed from Foundation.framework NSSortDescriptor.h }


{$ifdef TYPES}
type
  NSSortDescriptorPtr = ^NSSortDescriptor;
{$endif}

{$ifdef CLASSES}

type
  NSSortDescriptor = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _sortDescriptorFlags: NSUInteger;
    _key: NSString;
    _selector: SEL;
    _selectorOrBlock: id;
  public
    class function sortDescriptorWithKey_ascending (key: NSString; ascending: ObjCBOOL): instancetype; message 'sortDescriptorWithKey:ascending:'; { available in 10_6, 4_0 }
    class function sortDescriptorWithKey_ascending_selector (key: NSString; ascending: ObjCBOOL; selector: SEL): instancetype; message 'sortDescriptorWithKey:ascending:selector:'; { available in 10_6, 4_0 }
    function initWithKey_ascending (key: NSString; ascending: ObjCBOOL): instancetype; message 'initWithKey:ascending:';
    function initWithKey_ascending_selector (key: NSString; ascending: ObjCBOOL; selector: SEL): instancetype; message 'initWithKey:ascending:selector:';
    function key: NSString; message 'key';
    function ascending: ObjCBOOL; message 'ascending';
    function selector: SEL; message 'selector';
    procedure allowEvaluation; message 'allowEvaluation'; { available in 10_9, 7_0 }
    class function sortDescriptorWithKey_ascending_comparator (key_: NSString; ascending_: ObjCBOOL; cmptr: NSComparator): instancetype; message 'sortDescriptorWithKey:ascending:comparator:'; { available in 10_6, 4_0 }
    function initWithKey_ascending_comparator (key_: NSString; ascending_: ObjCBOOL; cmptr: NSComparator): instancetype; message 'initWithKey:ascending:comparator:'; { available in 10_6, 4_0 }
    function comparator: NSComparator; message 'comparator';
    function compareObject_toObject (object1: id; object2: id): NSComparisonResult; message 'compareObject:toObject:';
    function reversedSortDescriptor: id; message 'reversedSortDescriptor';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSSet_NSSortDescriptorSorting = objccategory external name 'NSSortDescriptorSorting' (NSSet)
    function sortedArrayUsingDescriptors (sortDescriptors: NSArray): NSArray; message 'sortedArrayUsingDescriptors:'; { available in 10_6, 4_0 }
  end;

type
  NSArray_NSSortDescriptorSorting = objccategory external name 'NSSortDescriptorSorting' (NSArray)
    function sortedArrayUsingDescriptors (sortDescriptors: NSArray): NSArray; message 'sortedArrayUsingDescriptors:';
  end;

type
  NSSortDescriptorSorting = objccategory external (NSMutableArray)
    procedure sortUsingDescriptors (sortDescriptors: NSArray); message 'sortUsingDescriptors:';
  end;

type
  NSOrderedSet_NSKeyValueSorting = objccategory external name 'NSKeyValueSorting' (NSOrderedSet)
    function sortedArrayUsingDescriptors (sortDescriptors: NSArray): NSArray; message 'sortedArrayUsingDescriptors:'; { available in 10_7, 5_0 }
  end;

type
  NSKeyValueSorting = objccategory external (NSMutableOrderedSet)
    procedure sortUsingDescriptors (sortDescriptors: NSArray); message 'sortUsingDescriptors:'; { available in 10_7, 5_0 }
  end;
{$endif}

