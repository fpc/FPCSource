{ Parsed from OSAKit.framework OSALanguage.h }


{$ifdef TYPES}
type
  OSALanguagePtr = ^OSALanguage;
{$endif}

{$ifdef TYPES}
type
  OSALanguageFeatures = NSUInteger;
  OSALanguageFeaturesPtr = ^OSALanguageFeatures;

const
  OSASupportsCompiling = $0002;
  OSASupportsGetSource = $0004;
  OSASupportsAECoercion = $0008;
  OSASupportsAESending = $0010;
  OSASupportsRecording = $0020;
  OSASupportsConvenience = $0040;
  OSASupportsDialects = $0080;
  OSASupportsEventHandling = $0100;
{$endif}

{$ifdef CLASSES}

type
  OSALanguage = objcclass external (NSObject)
  private
    _private: OSALanguagePrivate;
  public
    class function availableLanguages: NSArray; message 'availableLanguages';
    class function languageForName (name: NSString): OSALanguage; message 'languageForName:';
    class function languageForScriptDataDescriptor (descriptor: NSAppleEventDescriptor): OSALanguage; message 'languageForScriptDataDescriptor:';
    class function defaultLanguage: OSALanguage; message 'defaultLanguage';
    class procedure setDefaultLanguage (defaultLanguage_: OSALanguage); message 'setDefaultLanguage:';
    function initWithComponent (component_: Component): instancetype; message 'initWithComponent:';
    function sharedLanguageInstance: OSALanguageInstance; message 'sharedLanguageInstance';
    function componentInstance: ComponentInstance; message 'componentInstance';
    function name: NSString; message 'name';
    function info: NSString; message 'info';
    function version: NSString; message 'version';
    function type_: OSType; message 'type';
    function subType: OSType; message 'subType';
    function manufacturer: OSType; message 'manufacturer';
    function features: OSALanguageFeatures; message 'features';
    function isThreadSafe: ObjCBOOL; message 'isThreadSafe';
  end;
{$endif}

