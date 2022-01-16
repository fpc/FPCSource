{ Parsed from Foundation.framework NSScriptSuiteRegistry.h }


{$ifdef TYPES}
type
  NSScriptSuiteRegistryPtr = ^NSScriptSuiteRegistry;
{$endif}

{$ifdef CLASSES}

type
  NSScriptSuiteRegistry = objcclass external (NSObject)
  private
    _isLoadingSDEFFiles: ObjCBOOL;
    _isLoadingSecurityOverride: ObjCBOOL;
    _hasLoadedIntrinsics: ObjCBOOL;
    _reserved1:array[0..0] of char;
    _seenBundles: NSMutableSet;
    _suiteDescriptionsBeingCollected: NSMutableArray;
    _classDescriptionNeedingRegistration: NSScriptClassDescription;
    _suiteDescriptions: NSMutableArray;
    _commandDescriptionNeedingRegistration: NSScriptCommandDescription;
    _cachedClassDescriptionsByAppleEventCode: NSMutableDictionary;
    _cachedCommandDescriptionsByAppleEventCodes: NSMutableDictionary;
    _cachedSuiteDescriptionsByName: NSDictionary;
    _complexTypeDescriptionsByName: NSMutableDictionary;
    _listTypeDescriptionsByName: NSMutableDictionary;
    _nextComplexTypeAppleEventCode: cuint;
    _reserved2:array[0..3] of pointer;
  public
    class function sharedScriptSuiteRegistry: NSScriptSuiteRegistry; message 'sharedScriptSuiteRegistry';
    class procedure setSharedScriptSuiteRegistry (registry: NSScriptSuiteRegistry); message 'setSharedScriptSuiteRegistry:';
    procedure loadSuitesFromBundle (bundle: NSBundle); message 'loadSuitesFromBundle:';
    procedure loadSuiteWithDictionary_fromBundle (suiteDeclaration: NSDictionary; bundle: NSBundle); message 'loadSuiteWithDictionary:fromBundle:';
    procedure registerClassDescription (classDescription: NSScriptClassDescription); message 'registerClassDescription:';
    procedure registerCommandDescription (commandDescription: NSScriptCommandDescription); message 'registerCommandDescription:';
    function suiteNames: NSArray; message 'suiteNames';
    function appleEventCodeForSuite (suiteName: NSString): FourCharCode; message 'appleEventCodeForSuite:';
    function bundleForSuite (suiteName: NSString): NSBundle; message 'bundleForSuite:';
    function classDescriptionsInSuite (suiteName: NSString): NSDictionary; message 'classDescriptionsInSuite:';
    function commandDescriptionsInSuite (suiteName: NSString): NSDictionary; message 'commandDescriptionsInSuite:';
    function suiteForAppleEventCode (appleEventCode: FourCharCode): NSString; message 'suiteForAppleEventCode:';
    function classDescriptionWithAppleEventCode (appleEventCode: FourCharCode): NSScriptClassDescription; message 'classDescriptionWithAppleEventCode:';
    function commandDescriptionWithAppleEventClass_andAppleEventCode (appleEventClassCode: FourCharCode; appleEventIDCode: FourCharCode): NSScriptCommandDescription; message 'commandDescriptionWithAppleEventClass:andAppleEventCode:';
    function aeteResource (languageName: NSString): NSData; message 'aeteResource:';
  end;
{$endif}

