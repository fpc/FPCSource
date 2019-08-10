{ Parsed from WebKit.framework WebPreferences.h }


{$ifdef TYPES}
type
  WebPreferencesPtr = ^WebPreferences;
{$endif}

{$ifdef TYPES}
type
  WebCacheModel = NSUInteger;
  WebCacheModelPtr = ^WebCacheModel;

const
  WebCacheModelDocumentViewer = 0;
  WebCacheModelDocumentBrowser = 1;
  WebCacheModelPrimaryWebBrowser = 2;

type
  WebPreferencesPrivate = OpaqueType;
  WebPreferencesPrivatePtr = ^WebPreferencesPrivate;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  WebPreferencesChangedNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  WebPreferences = objcclass external (NSObject, NSCodingProtocol)
  private
    _private: WebPreferencesPrivatePtr;
  public
    class function standardPreferences: WebPreferences; message 'standardPreferences';
    function initWithIdentifier (anIdentifier: NSString): instancetype; message 'initWithIdentifier:';
    function identifier: NSString; message 'identifier';
    procedure setStandardFontFamily(newValue: NSString); message 'setStandardFontFamily:';
    function standardFontFamily: NSString; message 'standardFontFamily';
    procedure setFixedFontFamily(newValue: NSString); message 'setFixedFontFamily:';
    function fixedFontFamily: NSString; message 'fixedFontFamily';
    procedure setSerifFontFamily(newValue: NSString); message 'setSerifFontFamily:';
    function serifFontFamily: NSString; message 'serifFontFamily';
    procedure setSansSerifFontFamily(newValue: NSString); message 'setSansSerifFontFamily:';
    function sansSerifFontFamily: NSString; message 'sansSerifFontFamily';
    procedure setCursiveFontFamily(newValue: NSString); message 'setCursiveFontFamily:';
    function cursiveFontFamily: NSString; message 'cursiveFontFamily';
    procedure setFantasyFontFamily(newValue: NSString); message 'setFantasyFontFamily:';
    function fantasyFontFamily: NSString; message 'fantasyFontFamily';
    procedure setDefaultFontSize(newValue: cint); message 'setDefaultFontSize:';
    function defaultFontSize: cint; message 'defaultFontSize';
    procedure setDefaultFixedFontSize(newValue: cint); message 'setDefaultFixedFontSize:';
    function defaultFixedFontSize: cint; message 'defaultFixedFontSize';
    procedure setMinimumFontSize(newValue: cint); message 'setMinimumFontSize:';
    function minimumFontSize: cint; message 'minimumFontSize';
    procedure setMinimumLogicalFontSize(newValue: cint); message 'setMinimumLogicalFontSize:';
    function minimumLogicalFontSize: cint; message 'minimumLogicalFontSize';
    procedure setDefaultTextEncodingName(newValue: NSString); message 'setDefaultTextEncodingName:';
    function defaultTextEncodingName: NSString; message 'defaultTextEncodingName';
    procedure setUserStyleSheetEnabled(newValue: ObjCBOOL); message 'setUserStyleSheetEnabled:';
    function userStyleSheetEnabled: ObjCBOOL; message 'userStyleSheetEnabled';
    procedure setUserStyleSheetLocation(newValue: NSURL); message 'setUserStyleSheetLocation:';
    function userStyleSheetLocation: NSURL; message 'userStyleSheetLocation';
    procedure setJavaEnabled(newValue: ObjCBOOL); message 'setJavaEnabled:';
    function isJavaEnabled: ObjCBOOL; message 'isJavaEnabled';
    procedure setJavaScriptEnabled(newValue: ObjCBOOL); message 'setJavaScriptEnabled:';
    function isJavaScriptEnabled: ObjCBOOL; message 'isJavaScriptEnabled';
    procedure setJavaScriptCanOpenWindowsAutomatically(newValue: ObjCBOOL); message 'setJavaScriptCanOpenWindowsAutomatically:';
    function javaScriptCanOpenWindowsAutomatically: ObjCBOOL; message 'javaScriptCanOpenWindowsAutomatically';
    procedure setPlugInsEnabled(newValue: ObjCBOOL); message 'setPlugInsEnabled:';
    function arePlugInsEnabled: ObjCBOOL; message 'arePlugInsEnabled';
    procedure setAllowsAnimatedImages(newValue: ObjCBOOL); message 'setAllowsAnimatedImages:';
    function allowsAnimatedImages: ObjCBOOL; message 'allowsAnimatedImages';
    procedure setAllowsAnimatedImageLooping(newValue: ObjCBOOL); message 'setAllowsAnimatedImageLooping:';
    function allowsAnimatedImageLooping: ObjCBOOL; message 'allowsAnimatedImageLooping';
    procedure setLoadsImagesAutomatically(newValue: ObjCBOOL); message 'setLoadsImagesAutomatically:';
    function loadsImagesAutomatically: ObjCBOOL; message 'loadsImagesAutomatically';
    procedure setAutosaves(newValue: ObjCBOOL); message 'setAutosaves:';
    function autosaves: ObjCBOOL; message 'autosaves';
    procedure setShouldPrintBackgrounds(newValue: ObjCBOOL); message 'setShouldPrintBackgrounds:';
    function shouldPrintBackgrounds: ObjCBOOL; message 'shouldPrintBackgrounds';
    procedure setPrivateBrowsingEnabled(newValue: ObjCBOOL); message 'setPrivateBrowsingEnabled:';
    function privateBrowsingEnabled: ObjCBOOL; message 'privateBrowsingEnabled';
    procedure setTabsToLinks(newValue: ObjCBOOL); message 'setTabsToLinks:';
    function tabsToLinks: ObjCBOOL; message 'tabsToLinks';
    procedure setUsesPageCache(newValue: ObjCBOOL); message 'setUsesPageCache:';
    function usesPageCache: ObjCBOOL; message 'usesPageCache';
    procedure setCacheModel(newValue: WebCacheModel); message 'setCacheModel:';
    function cacheModel: WebCacheModel; message 'cacheModel';
    procedure setSuppressesIncrementalRendering(newValue: ObjCBOOL); message 'setSuppressesIncrementalRendering:';
    function suppressesIncrementalRendering: ObjCBOOL; message 'suppressesIncrementalRendering';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

