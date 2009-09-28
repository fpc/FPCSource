{ Parsed from Foundation.framework NSScriptSuiteRegistry.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTSUITEREGISTRY_PAS_H}
{$define NSSCRIPTSUITEREGISTRY_PAS_H}
type
  NSScriptSuiteRegistryPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCRIPTSUITEREGISTRY_PAS_T}
{$define NSSCRIPTSUITEREGISTRY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTSUITEREGISTRY_PAS_R}
{$define NSSCRIPTSUITEREGISTRY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTSUITEREGISTRY_PAS_F}
{$define NSSCRIPTSUITEREGISTRY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTSUITEREGISTRY_PAS_S}
{$define NSSCRIPTSUITEREGISTRY_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptSuiteRegistry = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTSUITEREGISTRY_PAS_C}
{$define NSSCRIPTSUITEREGISTRY_PAS_C}

{ NSScriptSuiteRegistry }
  NSScriptSuiteRegistry = objcclass(NSObject)
  private
    __isLoadingSDEFFiles: Boolean;
        _reserved1: array[0..2] of char;
    __seenBundles: NSMutableSet;
    __suiteDescriptionsBeingCollected: NSMutableArray;
    __classDescriptionNeedingRegistration: NSScriptClassDescription;
    __suiteDescriptions: NSMutableArray;
    __commandDescriptionNeedingRegistration: NSScriptCommandDescription;
    __cachedClassDescriptionsByAppleEventCode: NSMutableDictionary;
    __cachedCommandDescriptionsByAppleEventCodes: NSMutableDictionary;
    __cachedSuiteDescriptionsByName: NSDictionary;
    __complexTypeDescriptionsByName: NSMutableDictionary;
    __listTypeDescriptionsByName: NSMutableDictionary;
    __nextComplexTypeAppleEventCode: cuint;
        _reserved2: array[0..3] of Pointer;
    
  public
    class function alloc: NSScriptSuiteRegistry; message 'alloc';

    class function sharedScriptSuiteRegistry: NSScriptSuiteRegistry; message 'sharedScriptSuiteRegistry';
    class procedure setSharedScriptSuiteRegistry(registry: NSScriptSuiteRegistry); message 'setSharedScriptSuiteRegistry:';
    procedure loadSuitesFromBundle(bundle: NSBundle); message 'loadSuitesFromBundle:';
    procedure loadSuiteWithDictionary_fromBundle(suiteDeclaration: NSDictionary; bundle: NSBundle); message 'loadSuiteWithDictionary:fromBundle:';
    procedure registerClassDescription(classDescription_: NSScriptClassDescription); message 'registerClassDescription:';
    procedure registerCommandDescription(commandDescription: NSScriptCommandDescription); message 'registerCommandDescription:';
    function suiteNames: NSArray; message 'suiteNames';
    function appleEventCodeForSuite(suiteName: NSString): FourCharCode; message 'appleEventCodeForSuite:';
    function bundleForSuite(suiteName: NSString): NSBundle; message 'bundleForSuite:';
    function classDescriptionsInSuite(suiteName: NSString): NSDictionary; message 'classDescriptionsInSuite:';
    function commandDescriptionsInSuite(suiteName: NSString): NSDictionary; message 'commandDescriptionsInSuite:';
    function suiteForAppleEventCode(appleEventCode: FourCharCode): NSString; message 'suiteForAppleEventCode:';
    function classDescriptionWithAppleEventCode(appleEventCode: FourCharCode): NSScriptClassDescription; message 'classDescriptionWithAppleEventCode:';
    function commandDescriptionWithAppleEventClass_andAppleEventCode(appleEventClassCode: FourCharCode; appleEventIDCode: FourCharCode): NSScriptCommandDescription; message 'commandDescriptionWithAppleEventClass:andAppleEventCode:';
    function aeteResource(languageName: NSString): NSData; message 'aeteResource:';
  end; external;

{$endif}
{$endif}
