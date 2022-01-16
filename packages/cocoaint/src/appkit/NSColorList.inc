{ Parsed from AppKit.framework NSColorList.h }


{$ifdef TYPES}
type
  NSColorListPtr = ^NSColorList;
{$endif}

{$ifdef CLASSES}

type
  NSColorList = objcclass external (NSObject, NSCodingProtocol)
  private
    _keyArray: NSMutableArray;
    _colorArray: NSMutableArray;
    _keyToIndexTable: CFMutableDictionaryRef;
    _name: NSString;
    _printerType: NSString;
    _fileName: NSString;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          colorsLoaded: 0..1;
          editable: 0..1;
          hasDeviceSpecificLists: 0..1;
          dirty: 0..1;
          hasFrozen: 0..1;
          notificationsDisabled: 0..1;
          hasAttemptedLoadingBundleForDirectory: 0..1;
          isProfileBased: 0..1;
          int: 0..((1 shl 24)-1);
          {$if defined(__LP64__)}
          int_: 0..((1 shl 32)-1);
          {$endif}
        );
      end;
    _clAuxiliaryStorage: id;
  public
    class function availableColorLists: NSArray; message 'availableColorLists';
    class function colorListNamed (name: NSString): NSColorList; message 'colorListNamed:';
    function initWithName (name: NSString): instancetype; message 'initWithName:';
    function initWithName_fromFile (name: NSString; path: NSString): instancetype; message 'initWithName:fromFile:';
    function name: NSString; message 'name';
    procedure setColor_forKey (color: NSColor; key: NSString); message 'setColor:forKey:';
    procedure insertColor_key_atIndex (color: NSColor; key: NSString; loc: NSUInteger); message 'insertColor:key:atIndex:';
    procedure removeColorWithKey (key: NSString); message 'removeColorWithKey:';
    function colorWithKey (key: NSString): NSColor; message 'colorWithKey:';
    function allKeys: NSArray; message 'allKeys';
    function isEditable: ObjCBOOL; message 'isEditable';
    function writeToFile (path: NSString): ObjCBOOL; message 'writeToFile:';
    procedure removeFile; message 'removeFile';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSColorListDidChangeNotification: NSString; cvar; external;
{$endif}

