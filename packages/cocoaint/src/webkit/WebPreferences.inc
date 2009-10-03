{ Parsed from Webkit.framework WebPreferences.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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

{$endif}
{$endif}

{$ifdef FORWARD}
  WebPreferences = objcclass;
  WebPreferencesPointer = ^WebPreferences;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBPREFERENCES_PAS_C}
{$define WEBPREFERENCES_PAS_C}

{ WebPreferences }
  WebPreferences = objcclass(NSObject, NSCodingProtocol)
  private
    __private: WebPreferencesPrivate;
    
  public
    class function alloc: WebPreferences; message 'alloc';

    class function standardPreferences: WebPreferences; message 'standardPreferences';
    function initWithIdentifier(anIdentifier: NSStringPointer): id; message 'initWithIdentifier:';
    function identifier: NSString; message 'identifier';
    function standardFontFamily: NSString; message 'standardFontFamily';
    procedure setStandardFontFamily(family: NSStringPointer); message 'setStandardFontFamily:';
    function fixedFontFamily: NSString; message 'fixedFontFamily';
    procedure setFixedFontFamily(family: NSStringPointer); message 'setFixedFontFamily:';
    function serifFontFamily: NSString; message 'serifFontFamily';
    procedure setSerifFontFamily(family: NSStringPointer); message 'setSerifFontFamily:';
    function sansSerifFontFamily: NSString; message 'sansSerifFontFamily';
    procedure setSansSerifFontFamily(family: NSStringPointer); message 'setSansSerifFontFamily:';
    function cursiveFontFamily: NSString; message 'cursiveFontFamily';
    procedure setCursiveFontFamily(family: NSStringPointer); message 'setCursiveFontFamily:';
    function fantasyFontFamily: NSString; message 'fantasyFontFamily';
    procedure setFantasyFontFamily(family: NSStringPointer); message 'setFantasyFontFamily:';
    function defaultFontSize: cint; message 'defaultFontSize';
    procedure setDefaultFontSize(size: cint); message 'setDefaultFontSize:';
    function defaultFixedFontSize: cint; message 'defaultFixedFontSize';
    procedure setDefaultFixedFontSize(size: cint); message 'setDefaultFixedFontSize:';
    function minimumFontSize: cint; message 'minimumFontSize';
    procedure setMinimumFontSize(size: cint); message 'setMinimumFontSize:';
    function minimumLogicalFontSize: cint; message 'minimumLogicalFontSize';
    procedure setMinimumLogicalFontSize(size: cint); message 'setMinimumLogicalFontSize:';
    function defaultTextEncodingName: NSString; message 'defaultTextEncodingName';
    procedure setDefaultTextEncodingName(encoding: NSStringPointer); message 'setDefaultTextEncodingName:';
    function userStyleSheetEnabled: Boolean; message 'userStyleSheetEnabled';
    procedure setUserStyleSheetEnabled(flag: Boolean); message 'setUserStyleSheetEnabled:';
    function userStyleSheetLocation: NSURL; message 'userStyleSheetLocation';
    procedure setUserStyleSheetLocation(URL: NSURLPointer); message 'setUserStyleSheetLocation:';
    function isJavaEnabled: Boolean; message 'isJavaEnabled';
    procedure setJavaEnabled(flag: Boolean); message 'setJavaEnabled:';
    function isJavaScriptEnabled: Boolean; message 'isJavaScriptEnabled';
    procedure setJavaScriptEnabled(flag: Boolean); message 'setJavaScriptEnabled:';
    function javaScriptCanOpenWindowsAutomatically: Boolean; message 'javaScriptCanOpenWindowsAutomatically';
    procedure setJavaScriptCanOpenWindowsAutomatically(flag: Boolean); message 'setJavaScriptCanOpenWindowsAutomatically:';
    function arePlugInsEnabled: Boolean; message 'arePlugInsEnabled';
    procedure setPlugInsEnabled(flag: Boolean); message 'setPlugInsEnabled:';
    function allowsAnimatedImages: Boolean; message 'allowsAnimatedImages';
    procedure setAllowsAnimatedImages(flag: Boolean); message 'setAllowsAnimatedImages:';
    function allowsAnimatedImageLooping: Boolean; message 'allowsAnimatedImageLooping';
    procedure setAllowsAnimatedImageLooping(flag: Boolean); message 'setAllowsAnimatedImageLooping:';
    procedure setLoadsImagesAutomatically(flag: Boolean); message 'setLoadsImagesAutomatically:';
    function loadsImagesAutomatically: Boolean; message 'loadsImagesAutomatically';
    procedure setAutosaves(flag: Boolean); message 'setAutosaves:';
    function autosaves: Boolean; message 'autosaves';
    procedure setShouldPrintBackgrounds(flag: Boolean); message 'setShouldPrintBackgrounds:';
    function shouldPrintBackgrounds: Boolean; message 'shouldPrintBackgrounds';
    procedure setPrivateBrowsingEnabled(flag: Boolean); message 'setPrivateBrowsingEnabled:';
    function privateBrowsingEnabled: Boolean; message 'privateBrowsingEnabled';
    procedure setTabsToLinks(flag: Boolean); message 'setTabsToLinks:';
    function tabsToLinks: Boolean; message 'tabsToLinks';
    procedure setUsesPageCache(usesPageCache_: Boolean); message 'setUsesPageCache:';
    function usesPageCache: Boolean; message 'usesPageCache';
    procedure setCacheModel(cacheModel_: WebCacheModel); message 'setCacheModel:';
    function cacheModel: WebCacheModel; message 'cacheModel';
  end; external;

{$endif}
{$endif}
