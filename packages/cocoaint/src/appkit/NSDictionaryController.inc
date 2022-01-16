{ Parsed from AppKit.framework NSDictionaryController.h }


{$ifdef TYPES}
type
  NSDictionaryControllerPtr = ^NSDictionaryController;
{$endif}

{$ifdef CLASSES}

type
  NSDictionaryControllerKeyValuePair = objccategory external (NSObject)
    procedure setLocalizedKey (localizedKey: NSString); message 'setLocalizedKey:';
    function localizedKey: NSString; message 'localizedKey';
    procedure setKey (key: NSString); message 'setKey:';
    function key: NSString; message 'key';
    procedure setValue (value: id); message 'setValue:';
    function value: id; message 'value';
    function isExplicitlyIncluded: ObjCBOOL; message 'isExplicitlyIncluded';
  end;


type
  NSDictionaryController = objcclass external (NSArrayController)
  private
    _reserved5: pointer;
    _reserved6: pointer;
    _reserved7: pointer;
    _contentDictionary: id;
    _initialKey: NSString;
    _initialValue: id;
    _minimumInsertionKeyIndex: NSUInteger;
    _localizedKeyStringsFileName: NSString;
    _localizedKeyForKeyDictionary: NSDictionary;
    _keyForLocalizedKeyDictionary: NSDictionary;
    _includedKeys: NSArray;
    _excludedKeys: NSArray;
    _dictionaryControllerFlags: bitpacked record
      case byte of
        0: (_anonBitField__dictionaryControllerFlags0: cuint);
        1: (
          _deepCopiesValues: 0..1;
          _suppressBuildingDictionary: 0..1;
          _reservedDictionaryController: 0..((1 shl 30)-1);
        );
      end;
  public
    function newObject: id; message 'newObject';
    procedure setInitialKey(newValue: NSString); message 'setInitialKey:';
    function initialKey: NSString; message 'initialKey';
    procedure setInitialValue(newValue: id); message 'setInitialValue:';
    function initialValue: id; message 'initialValue';
    procedure setIncludedKeys(newValue: NSArray); message 'setIncludedKeys:';
    function includedKeys: NSArray; message 'includedKeys';
    procedure setExcludedKeys(newValue: NSArray); message 'setExcludedKeys:';
    function excludedKeys: NSArray; message 'excludedKeys';
    procedure setLocalizedKeyDictionary(newValue: NSDictionary); message 'setLocalizedKeyDictionary:';
    function localizedKeyDictionary: NSDictionary; message 'localizedKeyDictionary';
    procedure setLocalizedKeyTable(newValue: NSString); message 'setLocalizedKeyTable:';
    function localizedKeyTable: NSString; message 'localizedKeyTable';
  end;
{$endif}

