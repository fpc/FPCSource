{ Parsed from Appkit.framework NSColorList.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOLORLIST_PAS_H}
{$define NSCOLORLIST_PAS_H}
type
  NSColorListPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOLORLIST_PAS_T}
{$define NSCOLORLIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLORLIST_PAS_R}
{$define NSCOLORLIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLORLIST_PAS_F}
{$define NSCOLORLIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLORLIST_PAS_T}
{$define NSCOLORLIST_PAS_T}

{ External string constants }
var
  NSColorListDidChangeNotification: NSString; external name '_NSColorListDidChangeNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColorList = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLORLIST_PAS_C}
{$define NSCOLORLIST_PAS_C}

{ NSColorList }
  NSColorList = objcclass(NSObject, NSCodingProtocol)
  private
    __keyArray: NSMutableArray;
    __colorArray: NSMutableArray;
    __keyToIndexTable: CFMutableDictionaryRef;
    __name: NSString;
    __printerType: NSString;
    __fileName: NSString;
    __flags: bitpacked record
        colorsLoaded: 0..1;
        editable: 0..1;
        hasDeviceSpecificLists: 0..1;
        dirty: 0..1;
        hasFrozen: 0..1;
        notificationsDisabled: 0..1;
        hasAttemptedLoadingBundleForDirectory: 0..1;
        isProfileBased: 0..1;
        int: 0..((1 shl 24)-1);
        {$ifdef cpu64}
        int: 0..((1 shl 32)-1);
        {$endif}
      end;
    __clAuxiliaryStorage: id;
    
  public
    class function alloc: NSColorList; message 'alloc';

    class function availableColorLists: NSArray; message 'availableColorLists';
    class function colorListNamed(name_: NSString): NSColorList; message 'colorListNamed:';
    function initWithName(name_: NSString): id; message 'initWithName:';
    function initWithName_fromFile(name_: NSString; path: NSString): id; message 'initWithName:fromFile:';
    function name: NSString; message 'name';
    procedure setColor_forKey(color: NSColor; key: NSString); message 'setColor:forKey:';
    procedure insertColor_key_atIndex(color: NSColor; key: NSString; loc: NSUInteger); message 'insertColor:key:atIndex:';
    procedure removeColorWithKey(key: NSString); message 'removeColorWithKey:';
    function colorWithKey(key: NSString): NSColor; message 'colorWithKey:';
    function allKeys: NSArray; message 'allKeys';
    function isEditable: Boolean; message 'isEditable';
    function writeToFile(path: NSString): Boolean; message 'writeToFile:';
    procedure removeFile; message 'removeFile';
  end; external;

{$endif}
{$endif}
