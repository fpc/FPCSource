{ Parsed from Foundation.framework NSBundle.h }

{$ifdef TYPES}
{$ifndef NSBUNDLE_PAS_T}
{$define NSBUNDLE_PAS_T}

{ Constants }

const
  NSBundleExecutableArchitectureI386 = $00000007;
  NSBundleExecutableArchitecturePPC = $00000012;
  NSBundleExecutableArchitectureX86_64 = $01000007;
  NSBundleExecutableArchitecturePPC64 = $01000012;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBUNDLE_PAS_R}
{$define NSBUNDLE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBUNDLE_PAS_F}
{$define NSBUNDLE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBUNDLE_PAS_S}
{$define NSBUNDLE_PAS_S}

{ External string constants }
var
  NSBundleDidLoadNotification: NSString; cvar; external;
  NSLoadedClasses: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef USER_PATCHES}
{$ifdef INTERFACE}
function NSLocalizedString(key : NSString): NSString;
function NSLocalizedStringFromTable(key : NSString; table : NSString) : NSString;
function NSLocalizedStringFromTableInBundle(key : NSString; table : NSString; bundle : NSBundle) : NSString;
{$endif}

{$ifdef IMPLEMENTATION}

function NSLocalizedString(key : NSString): NSString;
begin
  Result := NSBundle.mainBundle.localizedStringForKey_value_table(key, NSSTR(''), nil);
end;

function NSLocalizedStringFromTable(key : NSString; table : NSString) : NSString;
begin
Result := NSBundle.mainBundle.localizedStringForKey_value_table(key, NSSTR(''), table)
end;

function NSLocalizedStringFromTableInBundle(key : NSString; table : NSString; bundle : NSBundle) : NSString;
begin
Result := bundle.localizedStringForKey_value_table(key, NSSTR(''), table)
end;

{$endif}

{$endif}

{$ifdef FORWARD}
  NSBundle = objcclass;
  NSBundlePointer = ^NSBundle;
  NSBundlePtr = NSBundlePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUNDLE_PAS_C}
{$define NSBUNDLE_PAS_C}

{ NSBundle }
  NSBundle = objcclass external (NSObject)
  private
    _flags: NSUInteger;
    _cfBundle: id;
    _refCount: NSUInteger;
    _principalClass: Pobjc_class;
    _tmp1: id;
    _tmp2: id;
    _reserved1: Pointer;
    _reserved0: Pointer;
    
  public
    class function mainBundle: NSBundle; message 'mainBundle';
    class function bundleWithPath(path: NSString): NSBundle; message 'bundleWithPath:';
    function initWithPath(path: NSString): id; message 'initWithPath:';
    class function bundleWithURL(url: NSURL): NSBundle; message 'bundleWithURL:';
    function initWithURL(url: NSURL): id; message 'initWithURL:';
    class function bundleForClass(aClass: Pobjc_class): NSBundle; message 'bundleForClass:';
    class function bundleWithIdentifier(identifier: NSString): NSBundle; message 'bundleWithIdentifier:';
    class function allBundles: NSArray; message 'allBundles';
    class function allFrameworks: NSArray; message 'allFrameworks';
    function load: Boolean; message 'load';
    function isLoaded: Boolean; message 'isLoaded';
    function unload: Boolean; message 'unload';
    function preflightAndReturnError(error: NSErrorPointer): Boolean; message 'preflightAndReturnError:';
    function loadAndReturnError(error: NSErrorPointer): Boolean; message 'loadAndReturnError:';
    function bundleURL: NSURL; message 'bundleURL';
    function resourceURL: NSURL; message 'resourceURL';
    function executableURL: NSURL; message 'executableURL';
    function URLForAuxiliaryExecutable(executableName: NSString): NSURL; message 'URLForAuxiliaryExecutable:';
    function privateFrameworksURL: NSURL; message 'privateFrameworksURL';
    function sharedFrameworksURL: NSURL; message 'sharedFrameworksURL';
    function sharedSupportURL: NSURL; message 'sharedSupportURL';
    function builtInPlugInsURL: NSURL; message 'builtInPlugInsURL';
    function bundlePath: NSString; message 'bundlePath';
    function resourcePath: NSString; message 'resourcePath';
    function executablePath: NSString; message 'executablePath';
    function pathForAuxiliaryExecutable(executableName: NSString): NSString; message 'pathForAuxiliaryExecutable:';
    function privateFrameworksPath: NSString; message 'privateFrameworksPath';
    function sharedFrameworksPath: NSString; message 'sharedFrameworksPath';
    function sharedSupportPath: NSString; message 'sharedSupportPath';
    function builtInPlugInsPath: NSString; message 'builtInPlugInsPath';
    class function URLForResource_withExtension_subdirectory_inBundleWithURL(name: NSString; ext: NSString; subpath: NSString; bundleURL_: NSURL): NSURL; message 'URLForResource:withExtension:subdirectory:inBundleWithURL:';
    class function URLsForResourcesWithExtension_subdirectory_inBundleWithURL(ext: NSString; subpath: NSString; bundleURL_: NSURL): NSArray; message 'URLsForResourcesWithExtension:subdirectory:inBundleWithURL:';
    function URLForResource_withExtension(name: NSString; ext: NSString): NSURL; message 'URLForResource:withExtension:';
    function URLForResource_withExtension_subdirectory(name: NSString; ext: NSString; subpath: NSString): NSURL; message 'URLForResource:withExtension:subdirectory:';
    function URLForResource_withExtension_subdirectory_localization(name: NSString; ext: NSString; subpath: NSString; localizationName: NSString): NSURL; message 'URLForResource:withExtension:subdirectory:localization:';
    function URLsForResourcesWithExtension_subdirectory(ext: NSString; subpath: NSString): NSArray; message 'URLsForResourcesWithExtension:subdirectory:';
    function URLsForResourcesWithExtension_subdirectory_localization(ext: NSString; subpath: NSString; localizationName: NSString): NSArray; message 'URLsForResourcesWithExtension:subdirectory:localization:';
    class function pathForResource_ofType_inDirectory(name: NSString; ext: NSString; bundlePath_: NSString): NSString; message 'pathForResource:ofType:inDirectory:';
    class function pathsForResourcesOfType_inDirectory(ext: NSString; bundlePath_: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:';
    function pathForResource_ofType(name: NSString; ext: NSString): NSString; message 'pathForResource:ofType:';
    function pathForResource_ofType_inDirectory_forLocalization(name: NSString; ext: NSString; subpath: NSString; localizationName: NSString): NSString; message 'pathForResource:ofType:inDirectory:forLocalization:';
    function pathsForResourcesOfType_inDirectory_forLocalization(ext: NSString; subpath: NSString; localizationName: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:forLocalization:';
    function localizedStringForKey_value_table(key: NSString; value: NSString; tableName: NSString): NSString; message 'localizedStringForKey:value:table:';
    function bundleIdentifier: NSString; message 'bundleIdentifier';
    function infoDictionary: NSDictionary; message 'infoDictionary';
    function localizedInfoDictionary: NSDictionary; message 'localizedInfoDictionary';
    function objectForInfoDictionaryKey(key: NSString): id; message 'objectForInfoDictionaryKey:';
    function classNamed(className: NSString): Pobjc_class; message 'classNamed:';
    function principalClass: Pobjc_class; message 'principalClass';
    function localizations: NSArray; message 'localizations';
    function preferredLocalizations: NSArray; message 'preferredLocalizations';
    function developmentLocalization: NSString; message 'developmentLocalization';
    class function preferredLocalizationsFromArray(localizationsArray: NSArray): NSArray; message 'preferredLocalizationsFromArray:';
    class function preferredLocalizationsFromArray_forPreferences(localizationsArray: NSArray; preferencesArray: NSArray): NSArray; message 'preferredLocalizationsFromArray:forPreferences:';
    function executableArchitectures: NSArray; message 'executableArchitectures';
  end;

{$endif}
{$endif}
