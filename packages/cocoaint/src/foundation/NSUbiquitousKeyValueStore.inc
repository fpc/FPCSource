{ Parsed from Foundation.framework NSUbiquitousKeyValueStore.h }


{$ifdef TYPES}
type
  NSUbiquitousKeyValueStorePtr = ^NSUbiquitousKeyValueStore;
{$endif}

{$ifdef CLASSES}

type
  NSUbiquitousKeyValueStore = objcclass external (NSObject)
  private
    _private1: id;
    _private2: id;
    _private3: id;
    _private4: pointer;
    _reserved:array[0..2] of pointer;
    _daemonWakeToken: cint;
    {$if defined(DEPLOYMENT_TARGET_MACOSX)}
    _disabledSuddenTermination: ObjCBOOL;
    {$endif}
  public
    class function defaultStore: NSUbiquitousKeyValueStore; message 'defaultStore';
    function objectForKey (aKey: NSString): id; message 'objectForKey:';
    procedure setObject_forKey (anObject: id; aKey: NSString); message 'setObject:forKey:';
    procedure removeObjectForKey (aKey: NSString); message 'removeObjectForKey:';
    function stringForKey (aKey: NSString): NSString; message 'stringForKey:';
    function arrayForKey (aKey: NSString): NSArray; message 'arrayForKey:';
    function dictionaryForKey (aKey: NSString): NSDictionary; message 'dictionaryForKey:';
    function dataForKey (aKey: NSString): NSData; message 'dataForKey:';
    function longLongForKey (aKey: NSString): clonglong; message 'longLongForKey:';
    function doubleForKey (aKey: NSString): double; message 'doubleForKey:';
    function boolForKey (aKey: NSString): ObjCBOOL; message 'boolForKey:';
    procedure setString_forKey (aString: NSString; aKey: NSString); message 'setString:forKey:';
    procedure setData_forKey (aData: NSData; aKey: NSString); message 'setData:forKey:';
    procedure setArray_forKey (anArray: NSArray; aKey: NSString); message 'setArray:forKey:';
    procedure setDictionary_forKey (aDictionary: NSDictionary; aKey: NSString); message 'setDictionary:forKey:';
    procedure setLongLong_forKey (value: clonglong; aKey: NSString); message 'setLongLong:forKey:';
    procedure setDouble_forKey (value: double; aKey: NSString); message 'setDouble:forKey:';
    procedure setBool_forKey (value: ObjCBOOL; aKey: NSString); message 'setBool:forKey:';
    function dictionaryRepresentation: NSDictionary; message 'dictionaryRepresentation';
    function synchronize: ObjCBOOL; message 'synchronize';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUbiquitousKeyValueStoreDidChangeExternallyNotification: NSString { available in 10_7, 5_0 }; cvar; external;
  NSUbiquitousKeyValueStoreChangeReasonKey: NSString { available in 10_7, 5_0 }; cvar; external;
  NSUbiquitousKeyValueStoreChangedKeysKey: NSString { available in 10_7, 5_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSUbiquitousKeyValueStoreServerChange = 0 { available in 10_7, 5_0 };
  NSUbiquitousKeyValueStoreInitialSyncChange = 1 { available in 10_7, 5_0 };
  NSUbiquitousKeyValueStoreQuotaViolationChange = 2 { available in 10_7, 5_0 };
  NSUbiquitousKeyValueStoreAccountChange = 3 { available in 10_8, 6_0 };
{$endif}

