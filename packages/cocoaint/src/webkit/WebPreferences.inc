{ Parsed from Webkit.framework WebPreferences.h }

{$ifdef TYPES}
{$ifndef WEBPREFERENCES_PAS_T}
{$define WEBPREFERENCES_PAS_T}

{ Constants }

const
  WebCacheModelDocumentViewer = 0;
  WebCacheModelDocumentBrowser = 1;
  WebCacheModelPrimaryWebBrowser = 2;

{ Types }
type
  WebCacheModel = WebNSUInteger;
  WebCacheModelPtr = ^WebCacheModel;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBPREFERENCES_PAS_R}
{$define WEBPREFERENCES_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBPREFERENCES_PAS_F}
{$define WEBPREFERENCES_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBPREFERENCES_PAS_S}
{$define WEBPREFERENCES_PAS_S}

{ External string constants }
var
  WebPreferencesChangedNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebPreferences = objcclass;
  WebPreferencesPointer = ^WebPreferences;
  WebPreferencesPtr = WebPreferencesPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBPREFERENCES_PAS_C}
{$define WEBPREFERENCES_PAS_C}

{ WebPreferences }
  WebPreferences = objcclass external (NSObject, NSCodingProtocol)
  private
    _private: WebPreferencesPrivate;
    
  public
    class function standardPreferences: WebPreferences; message 'standardPreferences';
    function initWithIdentifier(anIdentifier: NSString): id; message 'initWithIdentifier:';
    function identifier: NSString; message 'identifier';
    function standardFontFamily: NSString; message 'standardFontFamily';
    procedure setStandardFontFamily(family: NSString); message 'setStandardFontFamily:';
    function fixedFontFamily: NSString; message 'fixedFontFamily';
    procedure setFixedFontFamily(family: NSString); message 'setFixedFontFamily:';
    function serifFontFamily: NSString; message 'serifFontFamily';
    procedure setSerifFontFamily(family: NSString); message 'setSerifFontFamily:';
    function sansSerifFontFamily: NSString; message 'sansSerifFontFamily';
    procedure setSansSerifFontFamily(family: NSString); message 'setSansSerifFontFamily:';
    function cursiveFontFamily: NSString; message 'cursiveFontFamily';
    procedure setCursiveFontFamily(family: NSString); message 'setCursiveFontFamily:';
    function fantasyFontFamily: NSString; message 'fantasyFontFamily';
    procedure setFantasyFontFamily(family: NSString); message 'setFantasyFontFamily:';
    function defaultFontSize: cint; message 'defaultFontSize';
    procedure setDefaultFontSize(size: cint); message 'setDefaultFontSize:';
    function defaultFixedFontSize: cint; message 'defaultFixedFontSize';
    procedure setDefaultFixedFontSize(size: cint); message 'setDefaultFixedFontSize:';
    function minimumFontSize: cint; message 'minimumFontSize';
    procedure setMinimumFontSize(size: cint); message 'setMinimumFontSize:';
    function minimumLogicalFontSize: cint; message 'minimumLogicalFontSize';
    procedure setMinimumLogicalFontSize(size: cint); message 'setMinimumLogicalFontSize:';
    function defaultTextEncodingName: NSString; message 'defaultTextEncodingName';
    procedure setDefaultTextEncodingName(encoding: NSString); message 'setDefaultTextEncodingName:';
    function userStyleSheetEnabled: BOOL; message 'userStyleSheetEnabled';
    procedure setUserStyleSheetEnabled(flag: BOOL); message 'setUserStyleSheetEnabled:';
    function userStyleSheetLocation: NSURL; message 'userStyleSheetLocation';
    procedure setUserStyleSheetLocation(URL: NSURL); message 'setUserStyleSheetLocation:';
    function isJavaEnabled: BOOL; message 'isJavaEnabled';
    procedure setJavaEnabled(flag: BOOL); message 'setJavaEnabled:';
    function isJavaScriptEnabled: BOOL; message 'isJavaScriptEnabled';
    procedure setJavaScriptEnabled(flag: BOOL); message 'setJavaScriptEnabled:';
    function javaScriptCanOpenWindowsAutomatically: BOOL; message 'javaScriptCanOpenWindowsAutomatically';
    procedure setJavaScriptCanOpenWindowsAutomatically(flag: BOOL); message 'setJavaScriptCanOpenWindowsAutomatically:';
    function arePlugInsEnabled: BOOL; message 'arePlugInsEnabled';
    procedure setPlugInsEnabled(flag: BOOL); message 'setPlugInsEnabled:';
    function allowsAnimatedImages: BOOL; message 'allowsAnimatedImages';
    procedure setAllowsAnimatedImages(flag: BOOL); message 'setAllowsAnimatedImages:';
    function allowsAnimatedImageLooping: BOOL; message 'allowsAnimatedImageLooping';
    procedure setAllowsAnimatedImageLooping(flag: BOOL); message 'setAllowsAnimatedImageLooping:';
    procedure setLoadsImagesAutomatically(flag: BOOL); message 'setLoadsImagesAutomatically:';
    function loadsImagesAutomatically: BOOL; message 'loadsImagesAutomatically';
    procedure setAutosaves(flag: BOOL); message 'setAutosaves:';
    function autosaves: BOOL; message 'autosaves';
    procedure setShouldPrintBackgrounds(flag: BOOL); message 'setShouldPrintBackgrounds:';
    function shouldPrintBackgrounds: BOOL; message 'shouldPrintBackgrounds';
    procedure setPrivateBrowsingEnabled(flag: BOOL); message 'setPrivateBrowsingEnabled:';
    function privateBrowsingEnabled: BOOL; message 'privateBrowsingEnabled';
    procedure setTabsToLinks(flag: BOOL); message 'setTabsToLinks:';
    function tabsToLinks: BOOL; message 'tabsToLinks';
    procedure setUsesPageCache(usesPageCache_: BOOL); message 'setUsesPageCache:';
    function usesPageCache: BOOL; message 'usesPageCache';
    procedure setCacheModel(cacheModel_: WebCacheModel); message 'setCacheModel:';
    function cacheModel: WebCacheModel; message 'cacheModel';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{$endif}
{$endif}
