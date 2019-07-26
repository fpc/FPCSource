{ Parsed from AppKit.framework NSAppearance.h }


{$ifdef TYPES}
type
  NSAppearancePtr = ^NSAppearance;
  NSAppearanceCustomizationProtocolPtr = ^NSAppearanceCustomizationProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSAppearance = objcclass external (NSObject, NSCodingProtocol)
  private
    _name: NSString;
    _bundle: NSBundle;
    _private: pointer;
    _reserved: id;
    _auxiliary: id;
    {$if  not defined(__LP64__)}
    _extra:array[0..1] of id;
    {$endif}
  public
    function name: NSString; message 'name';
    class function currentAppearance: NSAppearance; message 'currentAppearance';
    class procedure setCurrentAppearance (appearance: NSAppearance); message 'setCurrentAppearance:';
    class function appearanceNamed (name_: NSString): NSAppearance; message 'appearanceNamed:';
    function initWithAppearanceNamed_bundle (name_: NSString; bundle: NSBundle): instancetype; message 'initWithAppearanceNamed:bundle:';
    function allowsVibrancy: ObjCBOOL; message 'allowsVibrancy';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSAppearanceNameAqua: NSString { available in 10_9 }; cvar; external;
  NSAppearanceNameLightContent: NSString deprecated 'in 10_9, 10_10, "Light content should use the default Aqua apppearance."'; cvar; external;
  NSAppearanceNameVibrantDark: NSString { available in 10_10 }; cvar; external;
  NSAppearanceNameVibrantLight: NSString { available in 10_10 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  NSAppearanceCustomizationProtocol = objcprotocol external name 'NSAppearanceCustomization' (NSObjectProtocol)
  required
    procedure setAppearance(newValue: NSAppearance); message 'setAppearance:';
    function appearance: NSAppearance; message 'appearance';
    function effectiveAppearance: NSAppearance; message 'effectiveAppearance';
  end;
{$endif}

