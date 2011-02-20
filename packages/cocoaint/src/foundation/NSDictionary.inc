{ Parsed from Foundation.framework NSDictionary.h }

{$ifdef TYPES}
{$ifndef NSDICTIONARY_PAS_T}
{$define NSDICTIONARY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDICTIONARY_PAS_R}
{$define NSDICTIONARY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDICTIONARY_PAS_F}
{$define NSDICTIONARY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDICTIONARY_PAS_S}
{$define NSDICTIONARY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDictionary = objcclass;
  NSDictionaryPointer = ^NSDictionary;
  NSDictionaryPtr = NSDictionaryPointer;
  NSMutableDictionary = objcclass;
  NSMutableDictionaryPointer = ^NSMutableDictionary;
  NSMutableDictionaryPtr = NSMutableDictionaryPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDICTIONARY_PAS_C}
{$define NSDICTIONARY_PAS_C}

{ NSDictionary }
  NSDictionary = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    function count: NSUInteger; message 'count';
    function objectForKey(aKey: id): id; message 'objectForKey:';
    function keyEnumerator: NSEnumerator; message 'keyEnumerator';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    function mutableCopyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function countByEnumeratingWithState_objects_count(state: NSFastEnumerationStatePtr; stackbuf: idPtr; len: NSUInteger): NSUInteger;
  end;

{ NSMutableDictionary }
  NSMutableDictionary = objcclass external (NSDictionary)
    
  public
    procedure removeObjectForKey(aKey: id); message 'removeObjectForKey:';
    procedure setObject_forKey(anObject: id; aKey: id); message 'setObject:forKey:';
  end;

{ NSExtendedDictionaryCategory }
  NSExtendedDictionaryCategory = objccategory external (NSDictionary)
    function allKeys: NSArray; message 'allKeys';
    function allKeysForObject(anObject: id): NSArray; message 'allKeysForObject:';
    function allValues: NSArray; message 'allValues';
    function description: NSString; message 'description';
    function descriptionInStringsFileFormat: NSString; message 'descriptionInStringsFileFormat';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithLocale_indent(locale: id; level: NSUInteger): NSString; message 'descriptionWithLocale:indent:';
    function isEqualToDictionary(otherDictionary: NSDictionary): Boolean; message 'isEqualToDictionary:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    function objectsForKeys_notFoundMarker(keys: NSArray; marker: id): NSArray; message 'objectsForKeys:notFoundMarker:';
    function writeToFile_atomically(path: NSString; useAuxiliaryFile: Boolean): Boolean; message 'writeToFile:atomically:';
    function writeToURL_atomically(url: NSURL; atomically: Boolean): Boolean; message 'writeToURL:atomically:';
    function keysSortedByValueUsingSelector(comparator: SEL): NSArray; message 'keysSortedByValueUsingSelector:';
    procedure getObjects_andKeys(objects: idPtr; keys: idPtr); message 'getObjects:andKeys:';
  end;

{ NSDictionaryCreationCategory }
  NSDictionaryCreationCategory = objccategory external (NSDictionary)
    class function dictionary: id; message 'dictionary';
    class function dictionaryWithObject_forKey(object_: id; key: id): id; message 'dictionaryWithObject:forKey:';
    class function dictionaryWithObjects_forKeys_count(objects: idPtr; keys: idPtr; cnt: NSUInteger): id; message 'dictionaryWithObjects:forKeys:count:';
    class function dictionaryWithObjectsAndKeys(firstKey: id): id; varargs; message 'dictionaryWithObjectsAndKeys:';
    class function dictionaryWithDictionary(dict: NSDictionary): id; message 'dictionaryWithDictionary:';
    class function dictionaryWithObjects_forKeys(objects: NSArray; keys: NSArray): id; message 'dictionaryWithObjects:forKeys:';
    function initWithObjects_forKeys_count(objects: idPtr; keys: idPtr; cnt: NSUInteger): id; message 'initWithObjects:forKeys:count:';
    function initWithObjectsAndKeys(firstKey: id): id; varargs; message 'initWithObjectsAndKeys:';
    function initWithDictionary(otherDictionary: NSDictionary): id; message 'initWithDictionary:';
    function initWithDictionary_copyItems(otherDictionary: NSDictionary; flag: Boolean): id; message 'initWithDictionary:copyItems:';
    function initWithObjects_forKeys(objects: NSArray; keys: NSArray): id; message 'initWithObjects:forKeys:';
    class function dictionaryWithContentsOfFile(path: NSString): id; message 'dictionaryWithContentsOfFile:';
    class function dictionaryWithContentsOfURL(url: NSURL): id; message 'dictionaryWithContentsOfURL:';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';
  end;

{ NSExtendedMutableDictionaryCategory }
  NSExtendedMutableDictionaryCategory = objccategory external (NSMutableDictionary)
    procedure addEntriesFromDictionary(otherDictionary: NSDictionary); message 'addEntriesFromDictionary:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure removeObjectsForKeys(keyArray: NSArray); message 'removeObjectsForKeys:';
    procedure setDictionary(otherDictionary: NSDictionary); message 'setDictionary:';
  end;

{ NSMutableDictionaryCreationCategory }
  NSMutableDictionaryCreationCategory = objccategory external (NSMutableDictionary)
    class function dictionaryWithCapacity(numItems: NSUInteger): id; message 'dictionaryWithCapacity:';
    function initWithCapacity(numItems: NSUInteger): id; message 'initWithCapacity:';
  end;

{$endif}
{$endif}
