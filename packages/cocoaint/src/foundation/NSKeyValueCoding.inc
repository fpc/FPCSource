{ Parsed from Foundation.framework NSKeyValueCoding.h }

{$ifdef EXTERNAL_SYMBOLS}
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

{$ifdef CLASSES}

type
  NSObject_NSKeyValueCoding = objccategory external name 'NSKeyValueCoding' (NSObject)
    class function accessInstanceVariablesDirectly: ObjCBOOL; message 'accessInstanceVariablesDirectly';
    function valueForKey (key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:';
    function validateValue_forKey_error (ioValue: idPtr; inKey: NSString; outError: NSErrorPtr): ObjCBOOL; message 'validateValue:forKey:error:';
    function mutableArrayValueForKey (key: NSString): NSMutableArray; message 'mutableArrayValueForKey:';
    function mutableOrderedSetValueForKey (key: NSString): NSMutableOrderedSet; message 'mutableOrderedSetValueForKey:'; { available in 10_7, 5_0 }
    function mutableSetValueForKey (key: NSString): NSMutableSet; message 'mutableSetValueForKey:';
    function valueForKeyPath (keyPath: NSString): id; message 'valueForKeyPath:';
    procedure setValue_forKeyPath (value: id; keyPath: NSString); message 'setValue:forKeyPath:';
    function validateValue_forKeyPath_error (ioValue: idPtr; inKeyPath: NSString; outError: NSErrorPtr): ObjCBOOL; message 'validateValue:forKeyPath:error:';
    function mutableArrayValueForKeyPath (keyPath: NSString): NSMutableArray; message 'mutableArrayValueForKeyPath:';
    function mutableOrderedSetValueForKeyPath (keyPath: NSString): NSMutableOrderedSet; message 'mutableOrderedSetValueForKeyPath:'; { available in 10_7, 5_0 }
    function mutableSetValueForKeyPath (keyPath: NSString): NSMutableSet; message 'mutableSetValueForKeyPath:';
    function valueForUndefinedKey (key: NSString): id; message 'valueForUndefinedKey:';
    procedure setValue_forUndefinedKey (value: id; key: NSString); message 'setValue:forUndefinedKey:';
    procedure setNilValueForKey (key: NSString); message 'setNilValueForKey:';
    function dictionaryWithValuesForKeys (keys: NSArray): NSDictionary; message 'dictionaryWithValuesForKeys:';
    procedure setValuesForKeysWithDictionary (keyedValues: NSDictionary); message 'setValuesForKeysWithDictionary:';
  end;

type
  NSArray_NSKeyValueCoding = objccategory external name 'NSKeyValueCoding' (NSArray)
    function valueForKey (key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:';
  end;

type
  NSDictionary_NSKeyValueCoding = objccategory external name 'NSKeyValueCoding' (NSDictionary)
    function valueForKey (key: NSString): id; message 'valueForKey:';
  end;

type
  NSMutableDictionary_NSKeyValueCoding = objccategory external name 'NSKeyValueCoding' (NSMutableDictionary)
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:';
  end;

type
  NSOrderedSet_NSKeyValueCoding = objccategory external name 'NSKeyValueCoding' (NSOrderedSet)
    function valueForKey (key: NSString): id; message 'valueForKey:'; { available in 10_7, 5_0 }
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:'; { available in 10_7, 5_0 }
  end;

type
  NSKeyValueCoding = objccategory external (NSSet)
    function valueForKey (key: NSString): id; message 'valueForKey:';
    procedure setValue_forKey (value: id; key: NSString); message 'setValue:forKey:';
  end;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef CLASSES}

type
  NSDeprecatedKeyValueCoding = objccategory external (NSObject)
    class function useStoredAccessor: ObjCBOOL; message 'useStoredAccessor'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    function storedValueForKey (key: NSString): id; message 'storedValueForKey:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    procedure takeStoredValue_forKey (value: id; key: NSString); message 'takeStoredValue:forKey:'; deprecated 'in 10_0, 10_4, 2_0, 2_0';
    procedure takeValue_forKey (value: id; key: NSString); message 'takeValue:forKey:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
    procedure takeValue_forKeyPath (value: id; keyPath: NSString); message 'takeValue:forKeyPath:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
    function handleQueryWithUnboundKey (key: NSString): id; message 'handleQueryWithUnboundKey:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
    procedure handleTakeValue_forUnboundKey (value: id; key: NSString); message 'handleTakeValue:forUnboundKey:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
    procedure unableToSetNilForKey (key: NSString); message 'unableToSetNilForKey:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
    function valuesForKeys (keys: NSArray): NSDictionary; message 'valuesForKeys:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
    procedure takeValuesFromDictionary (properties: NSDictionary); message 'takeValuesFromDictionary:'; deprecated 'in 10_0, 10_3, 2_0, 2_0';
  end;
{$endif}

{$endif}
