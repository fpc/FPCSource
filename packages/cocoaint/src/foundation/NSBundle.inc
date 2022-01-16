{ Parsed from Foundation.framework NSBundle.h }


{ Types from NSBundle }
{$ifdef TYPES}

const
  NSBundleExecutableArchitectureI386 = $00000007;
  NSBundleExecutableArchitecturePPC = $00000012;
  NSBundleExecutableArchitectureX86_64 = $01000007;
  NSBundleExecutableArchitecturePPC64 = $01000012;
{$endif}


{$ifdef TYPES}
type
  NSBundlePtr = ^NSBundle;
{$endif}

{$ifdef CLASSES}

type
  NSBundle = objcclass external (NSObject)
  private
    _flags: NSUInteger;
    _cfBundle: id;
    _reserved2: NSUInteger;
    _principalClass: pobjc_class;
    _initialPath: id;
    _resolvedPath: id;
    _reserved3: id;
    _lock: id;
  public
    class function mainBundle: NSBundle; message 'mainBundle';
    class function bundleWithPath (path: NSString): instancetype; message 'bundleWithPath:';
    function initWithPath (path: NSString): instancetype; message 'initWithPath:'; { NS_DESIGNATED_INITIALIZER }
    class function bundleWithURL (url: NSURL): instancetype; message 'bundleWithURL:'; { available in 10_6, 4_0 }
    function initWithURL (url: NSURL): instancetype; message 'initWithURL:'; { available in 10_6, 4_0 }
    class function bundleForClass (aClass: pobjc_class): NSBundle; message 'bundleForClass:';
    class function bundleWithIdentifier (identifier: NSString): NSBundle; message 'bundleWithIdentifier:';
    class function allBundles: NSArray; message 'allBundles';
    class function allFrameworks: NSArray; message 'allFrameworks';
    function load: ObjCBOOL; message 'load';
    function isLoaded: ObjCBOOL; message 'isLoaded';
    function unload: ObjCBOOL; message 'unload';
    function preflightAndReturnError (error: NSErrorPtr): ObjCBOOL; message 'preflightAndReturnError:'; { available in 10_5, 2_0 }
    function loadAndReturnError (error: NSErrorPtr): ObjCBOOL; message 'loadAndReturnError:'; { available in 10_5, 2_0 }
    function bundleURL: NSURL; message 'bundleURL';
    function resourceURL: NSURL; message 'resourceURL';
    function executableURL: NSURL; message 'executableURL';
    function URLForAuxiliaryExecutable (executableName: NSString): NSURL; message 'URLForAuxiliaryExecutable:'; { available in 10_6, 4_0 }
    function privateFrameworksURL: NSURL; message 'privateFrameworksURL';
    function sharedFrameworksURL: NSURL; message 'sharedFrameworksURL';
    function sharedSupportURL: NSURL; message 'sharedSupportURL';
    function builtInPlugInsURL: NSURL; message 'builtInPlugInsURL';
    function appStoreReceiptURL: NSURL; message 'appStoreReceiptURL';
    function bundlePath: NSString; message 'bundlePath';
    function resourcePath: NSString; message 'resourcePath';
    function executablePath: NSString; message 'executablePath';
    function pathForAuxiliaryExecutable (executableName: NSString): NSString; message 'pathForAuxiliaryExecutable:';
    function privateFrameworksPath: NSString; message 'privateFrameworksPath';
    function sharedFrameworksPath: NSString; message 'sharedFrameworksPath';
    function sharedSupportPath: NSString; message 'sharedSupportPath';
    function builtInPlugInsPath: NSString; message 'builtInPlugInsPath';
    class function URLForResource_withExtension_subdirectory_inBundleWithURL (name: NSString; ext: NSString; subpath: NSString; bundleURL_: NSURL): NSURL; message 'URLForResource:withExtension:subdirectory:inBundleWithURL:'; { available in 10_6, 4_0 }
    class function URLsForResourcesWithExtension_subdirectory_inBundleWithURL (ext: NSString; subpath: NSString; bundleURL_: NSURL): NSArray; message 'URLsForResourcesWithExtension:subdirectory:inBundleWithURL:'; { available in 10_6, 4_0 }
    function URLForResource_withExtension (name: NSString; ext: NSString): NSURL; message 'URLForResource:withExtension:'; { available in 10_6, 4_0 }
    function URLForResource_withExtension_subdirectory (name: NSString; ext: NSString; subpath: NSString): NSURL; message 'URLForResource:withExtension:subdirectory:'; { available in 10_6, 4_0 }
    function URLForResource_withExtension_subdirectory_localization (name: NSString; ext: NSString; subpath: NSString; localizationName: NSString): NSURL; message 'URLForResource:withExtension:subdirectory:localization:'; { available in 10_6, 4_0 }
    function URLsForResourcesWithExtension_subdirectory (ext: NSString; subpath: NSString): NSArray; message 'URLsForResourcesWithExtension:subdirectory:'; { available in 10_6, 4_0 }
    function URLsForResourcesWithExtension_subdirectory_localization (ext: NSString; subpath: NSString; localizationName: NSString): NSArray; message 'URLsForResourcesWithExtension:subdirectory:localization:'; { available in 10_6, 4_0 }
    class function classPathForResource_ofType_inDirectory (name: NSString; ext: NSString; bundlePath_: NSString): NSString; message 'pathForResource:ofType:inDirectory:';
    class function classPathsForResourcesOfType_inDirectory (ext: NSString; bundlePath_: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:';
    function pathForResource_ofType (name: NSString; ext: NSString): NSString; message 'pathForResource:ofType:';
    function pathForResource_ofType_inDirectory (name: NSString; ext: NSString; subpath: NSString): NSString; message 'pathForResource:ofType:inDirectory:';
    function pathForResource_ofType_inDirectory_forLocalization (name: NSString; ext: NSString; subpath: NSString; localizationName: NSString): NSString; message 'pathForResource:ofType:inDirectory:forLocalization:';
    function pathsForResourcesOfType_inDirectory (ext: NSString; subpath: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:';
    function pathsForResourcesOfType_inDirectory_forLocalization (ext: NSString; subpath: NSString; localizationName: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:forLocalization:';
    function localizedStringForKey_value_table (key: NSString; value: NSString; tableName: NSString): NSString; message 'localizedStringForKey:value:table:';
    function bundleIdentifier: NSString; message 'bundleIdentifier';
    function infoDictionary: NSDictionary; message 'infoDictionary';
    function localizedInfoDictionary: NSDictionary; message 'localizedInfoDictionary';
    function objectForInfoDictionaryKey (key: NSString): id; message 'objectForInfoDictionaryKey:';
    function classNamed (className: NSString): pobjc_class; message 'classNamed:';
    function principalClass: pobjc_class; message 'principalClass';
    function preferredLocalizations: NSArray; message 'preferredLocalizations';
    function localizations: NSArray; message 'localizations';
    function developmentLocalization: NSString; message 'developmentLocalization';
    class function preferredLocalizationsFromArray (localizationsArray: NSArray): NSArray; message 'preferredLocalizationsFromArray:';
    class function preferredLocalizationsFromArray_forPreferences (localizationsArray: NSArray; preferencesArray: NSArray): NSArray; message 'preferredLocalizationsFromArray:forPreferences:';
    function executableArchitectures: NSArray; message 'executableArchitectures';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSBundleDidLoadNotification: NSString; cvar; external;
  NSLoadedClasses: NSString; cvar; external;
{$endif}

