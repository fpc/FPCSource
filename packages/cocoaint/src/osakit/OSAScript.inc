{ Parsed from OSAKit.framework OSAScript.h }


{$ifdef TYPES}
type
  OSAScriptPtr = ^OSAScript;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef EXTERNAL_SYMBOLS}
var
  OSAScriptErrorMessageKey: NSString; cvar; external;
  OSAScriptErrorBriefMessageKey: NSString; cvar; external;
  OSAScriptErrorNumberKey: NSString; cvar; external;
  OSAScriptErrorPartialResultKey: NSString; cvar; external;
  OSAScriptErrorOffendingObjectKey: NSString; cvar; external;
  OSAScriptErrorExpectedTypeKey: NSString; cvar; external;
  OSAScriptErrorAppAddressKey: NSString; cvar; external;
  OSAScriptErrorAppNameKey: NSString; cvar; external;
  OSAScriptErrorRangeKey: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  OSAScriptErrorMessage: NSString; cvar; external;
  OSAScriptErrorNumber: NSString; cvar; external;
  OSAScriptErrorAppName: NSString; cvar; external;
  OSAScriptErrorBriefMessage: NSString; cvar; external;
  OSAScriptErrorRange: NSString; cvar; external;
  OSAStorageScriptType: NSString; cvar; external;
  OSAStorageScriptBundleType: NSString; cvar; external;
  OSAStorageApplicationType: NSString; cvar; external;
  OSAStorageApplicationBundleType: NSString; cvar; external;
  OSAStorageTextType: NSString; cvar; external;
{$endif}

{$ifdef TYPES}
type
  OSAStorageOptions = NSUInteger;
  OSAStorageOptionsPtr = ^OSAStorageOptions;

const
  OSANull = $00000000;
  OSAPreventGetSource = $00000001;
  OSACompileIntoContext = $00000002;
  OSADontSetScriptLocation = $01000000;
  OSAStayOpenApplet = $10000000;
  OSAShowStartupScreen = $20000000;
{$endif}

{$ifdef CLASSES}

type
  OSAScript = objcclass external (NSObject, NSCopyingProtocol)
  private
    _private: OSAScriptPrivate;
    _reserved1: pointer;
    _reserved2: pointer;
    _reserved3: pointer;
    _reserved4: pointer;
    _reserved5: pointer;
    _reserved6: pointer;
  public
    class function scriptDataDescriptorWithContentsOfURL (url: NSURL): NSAppleEventDescriptor; message 'scriptDataDescriptorWithContentsOfURL:';
    function initWithSource (source: NSString): instancetype; message 'initWithSource:';
    function initWithSource_language (source: NSString; language: OSALanguage): instancetype; message 'initWithSource:language:';
    function initWithSource_fromURL_languageInstance_usingStorageOptions (source: NSString; url: NSURL; instance: OSALanguageInstance; storageOptions: OSAStorageOptions): instancetype; message 'initWithSource:fromURL:languageInstance:usingStorageOptions:';
    function initWithContentsOfURL_error (url: NSURL; errorInfo: NSDictionaryPtr): instancetype; message 'initWithContentsOfURL:error:';
    function initWithContentsOfURL_language_error (url: NSURL; language: OSALanguage; errorInfo: NSDictionaryPtr): id; message 'initWithContentsOfURL:language:error:'; deprecated 'in 10.6 and later';
    function initWithContentsOfURL_languageInstance_usingStorageOptions_error (url: NSURL; instance: OSALanguageInstance; storageOptions: OSAStorageOptions; errorInfo: NSErrorPtr): instancetype; message 'initWithContentsOfURL:languageInstance:usingStorageOptions:error:';
    function initWithCompiledData_error (data: NSData; errorInfo: NSDictionaryPtr): id; message 'initWithCompiledData:error:'; deprecated 'in 10.6 and later';
    function initWithCompiledData_fromURL_usingStorageOptions_error (data: NSData; url: NSURL; storageOptions: OSAStorageOptions; errorInfo: NSErrorPtr): instancetype; message 'initWithCompiledData:fromURL:usingStorageOptions:error:';
    function initWithScriptDataDescriptor_fromURL_languageInstance_usingStorageOptions_error (data: NSAppleEventDescriptor; url: NSURL; instance: OSALanguageInstance; storageOptions: OSAStorageOptions; errorInfo: NSErrorPtr): instancetype; message 'initWithScriptDataDescriptor:fromURL:languageInstance:usingStorageOptions:error:';
    function source: NSString; message 'source';
    function url: NSURL; message 'url';
    procedure setLanguage(newValue: OSALanguage); message 'setLanguage:';
    function language: OSALanguage; message 'language';
    procedure setLanguageInstance(newValue: OSALanguageInstance); message 'setLanguageInstance:';
    function languageInstance: OSALanguageInstance; message 'languageInstance';
    function isCompiled: ObjCBOOL; message 'isCompiled';
    function compileAndReturnError (errorInfo: NSDictionaryPtr): ObjCBOOL; message 'compileAndReturnError:';
    function executeAndReturnError (errorInfo: NSDictionaryPtr): NSAppleEventDescriptor; message 'executeAndReturnError:';
    function executeAppleEvent_error (event: NSAppleEventDescriptor; errorInfo: NSDictionaryPtr): NSAppleEventDescriptor; message 'executeAppleEvent:error:';
    function executeAndReturnDisplayValue_error (displayValue: NSAttributedStringPtr; errorInfo: NSDictionaryPtr): NSAppleEventDescriptor; message 'executeAndReturnDisplayValue:error:';
    function executeHandlerWithName_arguments_error (name: NSString; arguments: NSArray; errorInfo: NSDictionaryPtr): NSAppleEventDescriptor; message 'executeHandlerWithName:arguments:error:';
    function richTextSource: NSAttributedString; message 'richTextSource';
    function richTextFromDescriptor (descriptor: NSAppleEventDescriptor): NSAttributedString; message 'richTextFromDescriptor:';
    function writeToURL_ofType_error (url_: NSURL; type_: NSString; errorInfo: NSDictionaryPtr): ObjCBOOL; message 'writeToURL:ofType:error:';
    function writeToURL_ofType_usingStorageOptions_error (url_: NSURL; type_: NSString; storageOptions: OSAStorageOptions; errorInfo: NSDictionaryPtr): ObjCBOOL; message 'writeToURL:ofType:usingStorageOptions:error:';
    function compiledDataForType_usingStorageOptions_error (type_: NSString; storageOptions: OSAStorageOptions; errorInfo: NSDictionaryPtr): NSData; message 'compiledDataForType:usingStorageOptions:error:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

