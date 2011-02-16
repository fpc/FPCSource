{ Parsed from Foundation.framework NSKeyValueCoding.h }

{$ifdef TYPES}
{$ifndef NSKEYVALUECODING_PAS_T}
{$define NSKEYVALUECODING_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSKEYVALUECODING_PAS_R}
{$define NSKEYVALUECODING_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSKEYVALUECODING_PAS_F}
{$define NSKEYVALUECODING_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSKEYVALUECODING_PAS_S}
{$define NSKEYVALUECODING_PAS_S}

{ External string constants }
var
  NSUndefinedKeyException: NSString; cvar; external;
  NSAverageKeyValueOperator: NSString; cvar; external;
  NSCountKeyValueOperator: NSString; cvar; external;
  NSDistinctUnionOfArraysKeyValueOperator: NSString; cvar; external;
  NSDistinctUnionOfObjectsKeyValueOperator: NSString; cvar; external;
  NSDistinctUnionOfSetsKeyValueOperator: NSString; cvar; external;
  NSMaximumKeyValueOperator: NSString; cvar; external;
  NSMinimumKeyValueOperator: NSString; cvar; external;
  NSSumKeyValueOperator: NSString; cvar; external;
  NSUnionOfArraysKeyValueOperator: NSString; cvar; external;
  NSUnionOfObjectsKeyValueOperator: NSString; cvar; external;
  NSUnionOfSetsKeyValueOperator: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef NSKEYVALUECODING_PAS_C}
{$define NSKEYVALUECODING_PAS_C}

{ NSKeyValueCodingCategory }
  NSKeyValueCodingCategory = objccategory external (NSObject)
    class function accessInstanceVariablesDirectly: Boolean; message 'accessInstanceVariablesDirectly';
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
    function validateValue_forKey_error(ioValue: idPtr; inKey: NSString; outError: NSErrorPointer): Boolean; message 'validateValue:forKey:error:';
    function mutableArrayValueForKey(key: NSString): NSMutableArray; message 'mutableArrayValueForKey:';
    function mutableSetValueForKey(key: NSString): NSMutableSet; message 'mutableSetValueForKey:';
    function valueForKeyPath(keyPath: NSString): id; message 'valueForKeyPath:';
    procedure setValue_forKeyPath(value: id; keyPath: NSString); message 'setValue:forKeyPath:';
    function validateValue_forKeyPath_error(ioValue: idPtr; inKeyPath: NSString; outError: NSErrorPointer): Boolean; message 'validateValue:forKeyPath:error:';
    function mutableArrayValueForKeyPath(keyPath: NSString): NSMutableArray; message 'mutableArrayValueForKeyPath:';
    function mutableSetValueForKeyPath(keyPath: NSString): NSMutableSet; message 'mutableSetValueForKeyPath:';
    function valueForUndefinedKey(key: NSString): id; message 'valueForUndefinedKey:';
    procedure setValue_forUndefinedKey(value: id; key: NSString); message 'setValue:forUndefinedKey:';
    procedure setNilValueForKey(key: NSString); message 'setNilValueForKey:';
    function dictionaryWithValuesForKeys(keys: NSArray): NSDictionary; message 'dictionaryWithValuesForKeys:';
    procedure setValuesForKeysWithDictionary(keyedValues: NSDictionary); message 'setValuesForKeysWithDictionary:';
  end;

{ NSKeyValueCoding_NSArrayCategory }
  NSKeyValueCoding_NSArrayCategory = objccategory external name 'NSKeyValueCoding' (NSArray)
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
  end;

{ NSKeyValueCoding_NSDictionaryCategory }
  NSKeyValueCoding_NSDictionaryCategory = objccategory external name 'NSKeyValueCoding' (NSDictionary)
    function valueForKey(key: NSString): id; message 'valueForKey:';
  end;

{ NSKeyValueCoding_NSMutableDictionaryCategory }
  NSKeyValueCoding_NSMutableDictionaryCategory = objccategory external name 'NSKeyValueCoding' (NSMutableDictionary)
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
  end;

{ NSKeyValueCoding_NSSetCategory }
  NSKeyValueCoding_NSSetCategory = objccategory external name 'NSKeyValueCoding' (NSSet)
    function valueForKey(key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey(value: id; key: NSString); message 'setValue:forKey:';
  end;

{ NSDeprecatedKeyValueCodingCategory }
  NSDeprecatedKeyValueCodingCategory = objccategory external (NSObject)
    class function useStoredAccessor: Boolean; message 'useStoredAccessor'; deprecated 'in Mac OS X 10.4 and later';
    function storedValueForKey(key: NSString): id; message 'storedValueForKey:'; deprecated 'in Mac OS X 10.4 and later';
    procedure takeStoredValue_forKey(value: id; key: NSString); message 'takeStoredValue:forKey:'; deprecated 'in Mac OS X 10.4 and later';
    procedure takeValue_forKey(value: id; key: NSString); message 'takeValue:forKey:'; deprecated 'in Mac OS X 10.3 and later';
    procedure takeValue_forKeyPath(value: id; keyPath: NSString); message 'takeValue:forKeyPath:'; deprecated 'in Mac OS X 10.3 and later';
    function handleQueryWithUnboundKey(key: NSString): id; message 'handleQueryWithUnboundKey:'; deprecated 'in Mac OS X 10.3 and later';
    procedure handleTakeValue_forUnboundKey(value: id; key: NSString); message 'handleTakeValue:forUnboundKey:'; deprecated 'in Mac OS X 10.3 and later';
    procedure unableToSetNilForKey(key: NSString); message 'unableToSetNilForKey:'; deprecated 'in Mac OS X 10.3 and later';
    function valuesForKeys(keys: NSArray): NSDictionary; message 'valuesForKeys:'; deprecated 'in Mac OS X 10.3 and later';
    procedure takeValuesFromDictionary(properties: NSDictionary); message 'takeValuesFromDictionary:'; deprecated 'in Mac OS X 10.3 and later';
  end;

{$endif}
{$endif}
