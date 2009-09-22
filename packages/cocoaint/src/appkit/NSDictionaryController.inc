{ Parsed from Appkit.framework NSDictionaryController.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSDICTIONARYCONTROLLER_PAS_H}
{$define NSDICTIONARYCONTROLLER_PAS_H}
type
  NSDictionaryControllerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSDICTIONARYCONTROLLER_PAS_T}
{$define NSDICTIONARYCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDICTIONARYCONTROLLER_PAS_R}
{$define NSDICTIONARYCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDICTIONARYCONTROLLER_PAS_F}
{$define NSDICTIONARYCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDICTIONARYCONTROLLER_PAS_T}
{$define NSDICTIONARYCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDictionaryController = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDICTIONARYCONTROLLER_PAS_C}
{$define NSDICTIONARYCONTROLLER_PAS_C}

{ NSDictionaryController }
  NSDictionaryController = objcclass(NSArrayController)
  private
    __reserved5: Pointer;
    __reserved6: Pointer;
    __reserved7: Pointer;
    __contentDictionary: id;
    __initialKey: NSString;
    __initialValue: id;
    __minimumInsertionKeyIndex: NSUInteger;
    __localizedKeyStringsFileName: NSString;
    __localizedKeyForKeyDictionary: NSDictionary;
    __keyForLocalizedKeyDictionary: NSDictionary;
    __includedKeys: NSArray;
    __excludedKeys: NSArray;
    __dictionaryControllerFlags: bitpacked record
        _deepCopiesValues: 0..1;
        _suppressBuildingDictionary: 0..1;
        _reservedDictionaryController: 0..((1 shl 30)-1);
      end;
    
  public
    class function alloc: NSDictionaryController; message 'alloc';

    function newObject: id; message 'newObject';
    procedure setInitialKey(key: NSString); message 'setInitialKey:';
    function initialKey: NSString; message 'initialKey';
    procedure setInitialValue(value: id); message 'setInitialValue:';
    function initialValue: id; message 'initialValue';
    procedure setIncludedKeys(keys: NSArray); message 'setIncludedKeys:';
    function includedKeys: NSArray; message 'includedKeys';
    procedure setExcludedKeys(keys: NSArray); message 'setExcludedKeys:';
    function excludedKeys: NSArray; message 'excludedKeys';
    procedure setLocalizedKeyDictionary(dictionary: NSDictionary); message 'setLocalizedKeyDictionary:';
    function localizedKeyDictionary: NSDictionary; message 'localizedKeyDictionary';
    procedure setLocalizedKeyTable(stringsFileName: NSString); message 'setLocalizedKeyTable:';
    function localizedKeyTable: NSString; message 'localizedKeyTable';
  end; external;

{$endif}
{$endif}
