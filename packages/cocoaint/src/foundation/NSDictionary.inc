{ Parsed from Foundation.framework NSDictionary.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDICTIONARY_PAS_H}
{$define NSDICTIONARY_PAS_H}
type
  NSDictionaryPointer = Pointer;
  NSMutableDictionaryPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSDICTIONARY_PAS_T}
{$define NSDICTIONARY_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDictionary = objcclass;
  NSMutableDictionary = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDICTIONARY_PAS_C}
{$define NSDICTIONARY_PAS_C}

{ NSDictionary }
  NSDictionary = objcclass(NSObject, NSCopyingProtocol, NSMutableCopyingProtocol, NSCodingProtocol, NSFastEnumerationProtocol)
    
  public
    class function alloc: NSDictionary; message 'alloc';

    function count: culong; message 'count';
    function objectForKey(aKey: id): id; message 'objectForKey:';
    function keyEnumerator: NSEnumerator; message 'keyEnumerator';

    { Category: NSExtendedDictionary }
    function allKeys: NSArray; message 'allKeys';
    function allKeysForObject(anObject: id): NSArray; message 'allKeysForObject:';
    function allValues: NSArray; message 'allValues';
    function description: NSString; message 'description';
    function descriptionInStringsFileFormat: NSString; message 'descriptionInStringsFileFormat';
    function descriptionWithLocale(locale: id): NSString; message 'descriptionWithLocale:';
    function descriptionWithLocale_indent(locale: id; level: culong): NSString; message 'descriptionWithLocale:indent:';
    function isEqualToDictionary(otherDictionary: NSDictionary): Boolean; message 'isEqualToDictionary:';
    function objectEnumerator: NSEnumerator; message 'objectEnumerator';
    function objectsForKeys_notFoundMarker(keys: NSArray; marker: id): NSArray; message 'objectsForKeys:notFoundMarker:';
    function writeToFile_atomically(path: NSString; useAuxiliaryFile: Boolean): Boolean; message 'writeToFile:atomically:';
    function writeToURL_atomically(url: NSURL; atomically: Boolean): Boolean; message 'writeToURL:atomically:';
    function keysSortedByValueUsingSelector(comparator: SEL): NSArray; message 'keysSortedByValueUsingSelector:';
    procedure getObjects_andKeys(objects: id; keys: id); message 'getObjects:andKeys:';

    { Category: NSDictionaryCreation }
    class function dictionary: id; message 'dictionary';
    class function dictionaryWithObject_forKey(object_: id; key: id): id; message 'dictionaryWithObject:forKey:';
    class function dictionaryWithObjects_forKeys_count(objects: id; keys: id; cnt: culong): id; message 'dictionaryWithObjects:forKeys:count:';
    class function dictionaryWithObjectsAndKeys(firstObject: id; objParams: array of const): id; message 'dictionaryWithObjectsAndKeys:';
    class function dictionaryWithDictionary(dict: NSDictionary): id; message 'dictionaryWithDictionary:';
    class function dictionaryWithObjects_forKeys(objects: NSArray; keys: NSArray): id; message 'dictionaryWithObjects:forKeys:';
    function initWithObjects_forKeys_count(objects: id; keys: id; cnt: culong): id; message 'initWithObjects:forKeys:count:';
    function initWithObjectsAndKeys(firstObject: id; objParams: array of const): id; message 'initWithObjectsAndKeys:';
    function initWithDictionary(otherDictionary: NSDictionary): id; message 'initWithDictionary:';
    function initWithDictionary_copyItems(otherDictionary: NSDictionary; flag: Boolean): id; message 'initWithDictionary:copyItems:';
    function initWithObjects_forKeys(objects: NSArray; keys: NSArray): id; message 'initWithObjects:forKeys:';
    class function dictionaryWithContentsOfFile(path: NSString): id; message 'dictionaryWithContentsOfFile:';
    class function dictionaryWithContentsOfURL(url: NSURL): id; message 'dictionaryWithContentsOfURL:';
    function initWithContentsOfFile(path: NSString): id; message 'initWithContentsOfFile:';
    function initWithContentsOfURL(url: NSURL): id; message 'initWithContentsOfURL:';

    { Category: NSFileAttributes }
    function fileSize: culonglong; message 'fileSize';
    function fileModificationDate: NSDate; message 'fileModificationDate';
    function fileType: NSString; message 'fileType';
    function filePosixPermissions: culong; message 'filePosixPermissions';
    function fileOwnerAccountName: NSString; message 'fileOwnerAccountName';
    function fileGroupOwnerAccountName: NSString; message 'fileGroupOwnerAccountName';
    function fileSystemNumber: clong; message 'fileSystemNumber';
    function fileSystemFileNumber: culong; message 'fileSystemFileNumber';
    function fileExtensionHidden: Boolean; message 'fileExtensionHidden';
    function fileHFSCreatorCode: OSType; message 'fileHFSCreatorCode';
    function fileHFSTypeCode: OSType; message 'fileHFSTypeCode';
    function fileIsImmutable: Boolean; message 'fileIsImmutable';
    function fileIsAppendOnly: Boolean; message 'fileIsAppendOnly';
    function fileCreationDate: NSDate; message 'fileCreationDate';
    function fileOwnerAccountID: NSNumber; message 'fileOwnerAccountID';
    function fileGroupOwnerAccountID: NSNumber; message 'fileGroupOwnerAccountID';

    { Category: NSKeyValueCoding }
    function valueForKey(key: NSString): id; message 'valueForKey:';
  end; external;

{ NSMutableDictionary }
  NSMutableDictionary = objcclass(NSDictionary)
    
  public
    class function alloc: NSMutableDictionary; message 'alloc';

    procedure removeObjectForKey(aKey: id); message 'removeObjectForKey:';
    procedure setObject_forKey(anObject: id; aKey: id); message 'setObject:forKey:';

    { Category: NSExtendedMutableDictionary }
    procedure addEntriesFromDictionary(otherDictionary: NSDictionary); message 'addEntriesFromDictionary:';
    procedure removeAllObjects; message 'removeAllObjects';
    procedure removeObjectsForKeys(keyArray: NSArray); message 'removeObjectsForKeys:';
    procedure setDictionary(otherDictionary: NSDictionary); message 'setDictionary:';

    { Category: NSMutableDictionaryCreation }
    class function dictionaryWithCapacity(numItems: culong): id; message 'dictionaryWithCapacity:';
    function initWithCapacity(numItems: culong): id; message 'initWithCapacity:';

    { Category: NSKeyValueCoding }
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
  end; external;

{$endif}
{$endif}
