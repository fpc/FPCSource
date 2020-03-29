{ Parsed from Foundation.framework NSItemProvider.h }


{$ifdef TYPES}
type
  NSItemProviderPtr = ^NSItemProvider;
{$endif}

{$if defined(__OBJC2__)}
{$ifdef TYPES}
type
  NSItemProviderCompletionHandler = OpaqueCBlock;
  NSItemProviderLoadHandler = OpaqueCBlock;
{$endif}

{$ifdef CLASSES}

type
  NSItemProvider = objcclass external (NSObject, NSCopyingProtocol)
  public
    function initWithItem_typeIdentifier (item: NSSecureCodingProtocol; typeIdentifier: NSString): instancetype; message 'initWithItem:typeIdentifier:'; { NS_DESIGNATED_INITIALIZER }
    function initWithContentsOfURL (fileURL: NSURL): instancetype; message 'initWithContentsOfURL:';
    procedure registerItemForTypeIdentifier_loadHandler (typeIdentifier: NSString; loadHandler: NSItemProviderLoadHandler); message 'registerItemForTypeIdentifier:loadHandler:';
    function registeredTypeIdentifiers: NSArray; message 'registeredTypeIdentifiers';
    function hasItemConformingToTypeIdentifier (typeIdentifier: NSString): ObjCBOOL; message 'hasItemConformingToTypeIdentifier:';
    procedure loadItemForTypeIdentifier_options_completionHandler (typeIdentifier: NSString; options: NSDictionary; completionHandler: NSItemProviderCompletionHandler); message 'loadItemForTypeIdentifier:options:completionHandler:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSItemProviderPreferredImageSizeKey: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSPreviewSupport = objccategory external (NSItemProvider)
    procedure setPreviewImageHandler(newValue: NSItemProviderLoadHandler); message 'setPreviewImageHandler:';
    function previewImageHandler: NSItemProviderLoadHandler; message 'previewImageHandler';
    procedure loadPreviewImageWithOptions_completionHandler (options: NSDictionary; completionHandler: NSItemProviderCompletionHandler); message 'loadPreviewImageWithOptions:completionHandler:'; { available in 10_10, 8_0 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSExtensionJavaScriptPreprocessingResultsKey: NSString { available in 10_10, 8_0 }; cvar; external;
  NSExtensionJavaScriptFinalizeArgumentKey: NSString { available in 8_0 }; cvar; external;
  NSItemProviderErrorDomain: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSItemProviderErrorCode = NSInteger;
  NSItemProviderErrorCodePtr = ^NSItemProviderErrorCode;

const
  NSItemProviderUnknownError = -1;
  NSItemProviderItemUnavailableError = -1000;
  NSItemProviderUnexpectedValueClassError = -1100;
{$endif}

{$endif}
