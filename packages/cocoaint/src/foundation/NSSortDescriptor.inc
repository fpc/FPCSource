{ Parsed from Foundation.framework NSSortDescriptor.h }

{$ifdef TYPES}
{$ifndef NSSORTDESCRIPTOR_PAS_T}
{$define NSSORTDESCRIPTOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSORTDESCRIPTOR_PAS_R}
{$define NSSORTDESCRIPTOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSORTDESCRIPTOR_PAS_F}
{$define NSSORTDESCRIPTOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSORTDESCRIPTOR_PAS_S}
{$define NSSORTDESCRIPTOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSortDescriptor = objcclass;
  NSSortDescriptorPointer = ^NSSortDescriptor;
  NSSortDescriptorPtr = NSSortDescriptorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSORTDESCRIPTOR_PAS_C}
{$define NSSORTDESCRIPTOR_PAS_C}

{ NSSortDescriptor }
  NSSortDescriptor = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _sortDescriptorFlags: NSUInteger;
    _key: NSString;
    _selector: SEL;
    _selectorOrBlock: id;
    
  public
    class function sortDescriptorWithKey_ascending(key_: NSString; ascending_: Boolean): id; message 'sortDescriptorWithKey:ascending:';
    class function sortDescriptorWithKey_ascending_selector(key_: NSString; ascending_: Boolean; selector_: SEL): id; message 'sortDescriptorWithKey:ascending:selector:';
    function initWithKey_ascending(key_: NSString; ascending_: Boolean): id; message 'initWithKey:ascending:';
    function initWithKey_ascending_selector(key_: NSString; ascending_: Boolean; selector_: SEL): id; message 'initWithKey:ascending:selector:';
    function key: NSString; message 'key';
    function ascending: Boolean; message 'ascending';
    function selector: SEL; message 'selector';
    function compareObject_toObject(object_: id; object_1: id): NSComparisonResult; message 'compareObject:toObject:';
    function reversedSortDescriptor: id; message 'reversedSortDescriptor';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ NSSortDescriptorSortingCategory }
  NSSortDescriptorSortingCategory = objccategory external (NSSet)
    function sortedArrayUsingDescriptors(sortDescriptors: NSArray): NSArray; message 'sortedArrayUsingDescriptors:';
  end;

{ NSSortDescriptorSorting_NSArrayCategory }
  NSSortDescriptorSorting_NSArrayCategory = objccategory external name 'NSSortDescriptorSorting' (NSArray)
    function sortedArrayUsingDescriptors(sortDescriptors: NSArray): NSArray; message 'sortedArrayUsingDescriptors:';
  end;

{ NSSortDescriptorSorting_NSMutableArrayCategory }
  NSSortDescriptorSorting_NSMutableArrayCategory = objccategory external name 'NSSortDescriptorSorting' (NSMutableArray)
    procedure sortUsingDescriptors(sortDescriptors: NSArray); message 'sortUsingDescriptors:';
  end;

{$endif}
{$endif}
