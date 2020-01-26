{ Parsed from Foundation.framework NSDictionary.h }


{ Types from NSDictionary }
{$ifdef TYPES}

{$endif}


{ Types from NSDictionaryCreation }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  NSDictionaryPtr = ^NSDictionary;
  NSMutableDictionaryPtr = ^NSMutableDictionary;
{$endif}

{$ifdef CLASSES}

type
  NSDictionary = objcclass external (NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSSecureCodingProtocol, NSFastEnumerationProtocol)
  public
    function count: NSUInteger; message 'count';
    function objectForKey (aKey: id): id; message 'objectForKey:';
    function keyEnumerator: NSEnumerator; message 'keyEnumerator';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    {$if defined(TARGET_OS_WIN32)}
    function initWithObjects_forKeys_count (objects: idPtr { variable size array of id }; keys: idPtr { variable size array of id }; cnt: NSUInteger): instancetype; message 'initWithObjects:forKeys:count:';
    {$else}
    function initWithObjects_forKeys_count (objects: idPtr { variable size array of id }; keys: NSCopyingProtocol; cnt: NSUInteger): instancetype; message 'initWithObjects:forKeys:count:'; { NS_DESIGNATED_INITIALIZER }
    {$endif}
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    function countByEnumeratingWithState_objects_count (state: NSFastEnumerationStatePtr; buffer: idPtr { variable size array of id }; len: NSUInteger): NSUInteger; message 'countByEnumeratingWithState:objects:count:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function mutableCopyWithZone (zone: NSZonePtr): id; message 'mutableCopyWithZone:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;


type
  NSExtendedDictionary = objccategory external (NSDictionary)
    function allKeys: NSArray; message 'allKeys';
    function allKeysForObject (anObject: id): NSArray; message 'allKeysForObject:';
    function allValues: NSArray; message 'allValues';
    function description: NSString; message 'description';
    function descriptionInStringsFileFormat: NSString; message 'descriptionInStringsFileFormat';
    function descriptionWithLocale (locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithLocale_indent (locale: id; level: NSUInteger): NSString; message 'descriptionWithLocale:indent:';
    function isEqualToDictionary (otherDictionary: NSDictionary): ObjCBOOL; message 'isEqualToDictionary:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    function objectsForKeys_notFoundMarker (keys: NSArray; marker: id): NSArray; message 'objectsForKeys:notFoundMarker:';
    function writeToFile_atomically (path: NSString; useAuxiliaryFile: ObjCBOOL): ObjCBOOL; message 'writeToFile:atomically:';
    function writeToURL_atomically (url: NSURL; atomically: ObjCBOOL): ObjCBOOL; message 'writeToURL:atomically:';
    function keysSortedByValueUsingSelector (comparator: SEL): NSArray; message 'keysSortedByValueUsingSelector:';
    procedure getObjects_andKeys (objects: idPtr { variable size array of id }; keys: idPtr { variable size array of id }); message 'getObjects:andKeys:';
    function objectForKeyedSubscript (key: id): id; message 'objectForKeyedSubscript:'; { available in 10_8, 6_0 }
    procedure enumerateKeysAndObjectsUsingBlock (block: OpaqueCBlock); message 'enumerateKeysAndObjectsUsingBlock:'; { available in 10_6, 4_0 }
    procedure enumerateKeysAndObjectsWithOptions_usingBlock (opts: NSEnumerationOptions; block: OpaqueCBlock); message 'enumerateKeysAndObjectsWithOptions:usingBlock:'; { available in 10_6, 4_0 }
    function keysSortedByValueUsingComparator (cmptr: NSComparator): NSArray; message 'keysSortedByValueUsingComparator:'; { available in 10_6, 4_0 }
    function keysSortedByValueWithOptions_usingComparator (opts: NSSortOptions; cmptr: NSComparator): NSArray; message 'keysSortedByValueWithOptions:usingComparator:'; { available in 10_6, 4_0 }
    function keysOfEntriesPassingTest (predicate: OpaqueCBlock): NSSet; message 'keysOfEntriesPassingTest:'; { available in 10_6, 4_0 }
    function keysOfEntriesWithOptions_passingTest (opts: NSEnumerationOptions; predicate: OpaqueCBlock): NSSet; message 'keysOfEntriesWithOptions:passingTest:'; { available in 10_6, 4_0 }
  end;

type
  NSDictionaryCreation = objccategory external (NSDictionary)
    class function dictionary: instancetype; message 'dictionary';
    class function dictionaryWithObject_forKey (object_: id; key: NSCopyingProtocol): instancetype; message 'dictionaryWithObject:forKey:';
    {$if defined(TARGET_OS_WIN32)}
    class function dictionaryWithObjects_forKeys_count (objects: idPtr { variable size array of id }; keys: idPtr { variable size array of id }; cnt: NSUInteger): instancetype; message 'dictionaryWithObjects:forKeys:count:';
    {$else}
    class function dictionaryWithObjects_forKeys_count (objects: idPtr { variable size array of id }; keys: NSCopyingProtocol; cnt: NSUInteger): instancetype; message 'dictionaryWithObjects:forKeys:count:';
    {$endif}
    class function dictionaryWithObjectsAndKeys (firstObject: id): instancetype; varargs; message 'dictionaryWithObjectsAndKeys:';
    class function dictionaryWithDictionary (dict: NSDictionary): instancetype; message 'dictionaryWithDictionary:';
    class function dictionaryWithObjects_forKeys (objects: NSArray; keys: NSArray): instancetype; message 'dictionaryWithObjects:forKeys:';
    function initWithObjectsAndKeys (firstObject: id): instancetype; varargs; message 'initWithObjectsAndKeys:';
    function initWithDictionary (otherDictionary: NSDictionary): instancetype; message 'initWithDictionary:';
    function initWithDictionary_copyItems (otherDictionary: NSDictionary; flag: ObjCBOOL): instancetype; message 'initWithDictionary:copyItems:';
    function initWithObjects_forKeys (objects: NSArray; keys: NSArray): instancetype; message 'initWithObjects:forKeys:';
    class function dictionaryWithContentsOfFile (path: NSString): NSDictionary; message 'dictionaryWithContentsOfFile:';
    class function dictionaryWithContentsOfURL (url: NSURL): NSDictionary; message 'dictionaryWithContentsOfURL:';
    function initWithContentsOfFile (path: NSString): NSDictionary; message 'initWithContentsOfFile:';
    function initWithContentsOfURL (url: NSURL): NSDictionary; message 'initWithContentsOfURL:';
  end;


type
  NSMutableDictionary = objcclass external (NSDictionary)
  public
    procedure removeObjectForKey (aKey: id); message 'removeObjectForKey:';
    procedure setObject_forKey (anObject: id; aKey: NSCopyingProtocol); message 'setObject:forKey:';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCapacity (numItems: NSUInteger): instancetype; message 'initWithCapacity:'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (aDecoder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
  end;


type
  NSExtendedMutableDictionary = objccategory external (NSMutableDictionary)
    procedure addEntriesFromDictionary (otherDictionary: NSDictionary); message 'addEntriesFromDictionary:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure removeObjectsForKeys (keyArray: NSArray); message 'removeObjectsForKeys:';
    procedure setDictionary (otherDictionary: NSDictionary); message 'setDictionary:';
    procedure setObject_forKeyedSubscript (obj: id; key: NSCopyingProtocol); message 'setObject:forKeyedSubscript:'; { available in 10_8, 6_0 }
  end;

type
  NSMutableDictionaryCreation = objccategory external (NSMutableDictionary)
    class function dictionaryWithCapacity (numItems: NSUInteger): instancetype; message 'dictionaryWithCapacity:';
    class function dictionaryWithContentsOfFile (path: NSString): NSMutableDictionary; message 'dictionaryWithContentsOfFile:';
    class function dictionaryWithContentsOfURL (url: NSURL): NSMutableDictionary; message 'dictionaryWithContentsOfURL:';
    function initWithContentsOfFile (path: NSString): NSMutableDictionary; message 'initWithContentsOfFile:';
    function initWithContentsOfURL (url: NSURL): NSMutableDictionary; message 'initWithContentsOfURL:';
  end;

type
  NSDictionary_NSSharedKeySetDictionary = objccategory external name 'NSSharedKeySetDictionary' (NSDictionary)
    class function sharedKeySetForKeys (keys: NSArray): id; message 'sharedKeySetForKeys:'; { available in 10_8, 6_0 }
  end;

type
  NSSharedKeySetDictionary = objccategory external (NSMutableDictionary)
    class function dictionaryWithSharedKeySet (keyset: id): NSMutableDictionary; message 'dictionaryWithSharedKeySet:'; { available in 10_8, 6_0 }
  end;
{$endif}

