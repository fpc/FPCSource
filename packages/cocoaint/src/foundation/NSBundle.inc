{ Parsed from Foundation.framework NSBundle.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:30:59 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSBUNDLE_PAS_H}
{$define NSBUNDLE_PAS_H}
type
  NSBundlePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSBUNDLE_PAS_T}
{$define NSBUNDLE_PAS_T}

{ Constants }

const
  NSBundleExecutableArchitectureI386 = $00000007;
  NSBundleExecutableArchitecturePPC = $00000012;
  NSBundleExecutableArchitectureX86_64 = $01000007;
  NSBundleExecutableArchitecturePPC64 = $01000012;

{ CFString constants }
var
  NSBundleDidLoadNotification: CFStringRef; external name '_NSBundleDidLoadNotification';
  NSLoadedClasses: CFStringRef; external name '_NSLoadedClasses';

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
{$ifndef NSBUNDLE_PAS_T}
{$define NSBUNDLE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSBundle = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUNDLE_PAS_C}
{$define NSBUNDLE_PAS_C}

{ NSBundle }
  NSBundle = objcclass(NSObject)
  private
    __flags: culong;
    __cfBundle: id;
    __refCount: culong;
    __principalClass: Pobjc_class;
    __tmp1: id;
    __tmp2: id;
    __reserved1: Pointer;
    __reserved0: Pointer;
    
  public
    class function alloc: NSBundle; message 'alloc';

    class function mainBundle: NSBundle; message 'mainBundle';
    class function bundleWithPath(path: NSString): NSBundle; message 'bundleWithPath:';
    function initWithPath(path: NSString): id; message 'initWithPath:';
    class function bundleForClass(aClass: Pobjc_class): NSBundle; message 'bundleForClass:';
    class function bundleWithIdentifier(identifier: NSString): NSBundle; message 'bundleWithIdentifier:';
    class function allBundles: NSArray; message 'allBundles';
    class function allFrameworks: NSArray; message 'allFrameworks';
    function load: Boolean; message 'load';
    function isLoaded: Boolean; message 'isLoaded';
    function unload: Boolean; message 'unload';
    function preflightAndReturnError(var error: NSError): Boolean; message 'preflightAndReturnError:';
    function loadAndReturnError(var error: NSError): Boolean; message 'loadAndReturnError:';
    function bundlePath: NSString; message 'bundlePath';
    function resourcePath: NSString; message 'resourcePath';
    function executablePath: NSString; message 'executablePath';
    function pathForAuxiliaryExecutable(executableName: NSString): NSString; message 'pathForAuxiliaryExecutable:';
    function privateFrameworksPath: NSString; message 'privateFrameworksPath';
    function sharedFrameworksPath: NSString; message 'sharedFrameworksPath';
    function sharedSupportPath: NSString; message 'sharedSupportPath';
    function builtInPlugInsPath: NSString; message 'builtInPlugInsPath';
    function bundleIdentifier: NSString; message 'bundleIdentifier';
    function classNamed(className_: NSString): Pobjc_class; message 'classNamed:';
    function principalClass: Pobjc_class; message 'principalClass';
    class function pathForResource_ofType_inDirectory(name: NSString; ext: NSString; bundlePath_: NSString): NSString; message 'pathForResource:ofType:inDirectory:';
    function pathForResource_ofType(name: NSString; ext: NSString): NSString; message 'pathForResource:ofType:';
    function pathForResource_ofType_inDirectory_forLocalization(name: NSString; ext: NSString; subpath: NSString; localizationName: NSString): NSString; message 'pathForResource:ofType:inDirectory:forLocalization:';
    class function pathsForResourcesOfType_inDirectory(ext: NSString; bundlePath_: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:';
    function pathsForResourcesOfType_inDirectory_forLocalization(ext: NSString; subpath: NSString; localizationName: NSString): NSArray; message 'pathsForResourcesOfType:inDirectory:forLocalization:';
    function localizedStringForKey_value_table(key: NSString; value: NSString; tableName: NSString): NSString; message 'localizedStringForKey:value:table:';
    function infoDictionary: NSDictionary; message 'infoDictionary';
    function localizedInfoDictionary: NSDictionary; message 'localizedInfoDictionary';
    function objectForInfoDictionaryKey(key: NSString): id; message 'objectForInfoDictionaryKey:';
    function localizations: NSArray; message 'localizations';
    function preferredLocalizations: NSArray; message 'preferredLocalizations';
    function developmentLocalization: NSString; message 'developmentLocalization';
    class function preferredLocalizationsFromArray(localizationsArray: NSArray): NSArray; message 'preferredLocalizationsFromArray:';
    class function preferredLocalizationsFromArray_forPreferences(localizationsArray: NSArray; preferencesArray: NSArray): NSArray; message 'preferredLocalizationsFromArray:forPreferences:';
    function executableArchitectures: NSArray; message 'executableArchitectures';

    { Category: NSBundleHelpExtension }
    function contextHelpForKey(key: NSString): NSAttributedString; message 'contextHelpForKey:';

    { Category: NSBundleImageExtension }
    function pathForImageResource(name: NSString): NSString; message 'pathForImageResource:';

    { Category: NSNibLoading }
    class function loadNibFile_externalNameTable_withZone(fileName: NSString; context: NSDictionary; var zone_: NSZone): Boolean; message 'loadNibFile:externalNameTable:withZone:';
    class function loadNibNamed_owner(nibName: NSString; owner: id): Boolean; message 'loadNibNamed:owner:';

    { Category: NSBundleSoundExtensions }
    function pathForSoundResource(name: NSString): NSString; message 'pathForSoundResource:';
  end; external;

{$endif}
{$endif}
