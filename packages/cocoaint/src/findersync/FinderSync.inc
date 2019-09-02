{ Parsed from FinderSync.framework FinderSync.h }


{$ifdef TYPES}
type
  FIFinderSyncControllerPtr = ^FIFinderSyncController;
  FIFinderSyncPtr = ^FIFinderSync;
  FIFinderSyncProtocolPtr = ^FIFinderSyncProtocol;
{$endif}

{$if defined(__OBJC2__)}
{$ifdef CLASSES}

type
  FIFinderSyncController = objcclass external (NSExtensionContext)
  public
    class function defaultController: instancetype; message 'defaultController';
    procedure setDirectoryURLs(newValue: NSSet); message 'setDirectoryURLs:';
    function directoryURLs: NSSet; message 'directoryURLs';
    procedure setBadgeImage_label_forBadgeIdentifier (image: NSImage; label_: NSString; badgeID: NSString); message 'setBadgeImage:label:forBadgeIdentifier:';
    procedure setBadgeIdentifier_forURL (badgeID: NSString; url: NSURL); message 'setBadgeIdentifier:forURL:';
    function targetedURL: NSURL; message 'targetedURL';
    function selectedItemURLs: NSArray; message 'selectedItemURLs';
  end;
{$endif}

{$ifdef TYPES}
type
  FIMenuKind = NSUInteger;
  FIMenuKindPtr = ^FIMenuKind;

const
  FIMenuKindContextualMenuForItems = 0;
  FIMenuKindContextualMenuForContainer = 1;
  FIMenuKindContextualMenuForSidebar = 2;
  FIMenuKindToolbarItemMenu = 3;
{$endif}

{$ifdef PROTOCOLS}

type
  FIFinderSyncProtocol = objcprotocol external name 'FIFinderSync'
  optional
    function menuForMenuKind (menu: FIMenuKind): NSMenu; message 'menuForMenuKind:';
    procedure beginObservingDirectoryAtURL (url: NSURL); message 'beginObservingDirectoryAtURL:';
    procedure endObservingDirectoryAtURL (url: NSURL); message 'endObservingDirectoryAtURL:';
    procedure requestBadgeIdentifierForURL (url: NSURL); message 'requestBadgeIdentifierForURL:';
    function toolbarItemName: NSString; message 'toolbarItemName';
    function toolbarItemImage: NSImage; message 'toolbarItemImage';
    function toolbarItemToolTip: NSString; message 'toolbarItemToolTip';
  end;
{$endif}

{$ifdef CLASSES}

type
  FIFinderSync = objcclass external (NSObject, FIFinderSyncProtocol, NSExtensionRequestHandlingProtocol)

    { Adopted protocols }
    procedure beginObservingDirectoryAtURL (url: NSURL); message 'beginObservingDirectoryAtURL:';
    procedure beginRequestWithExtensionContext (context: NSExtensionContext); message 'beginRequestWithExtensionContext:';
    procedure endObservingDirectoryAtURL (url: NSURL); message 'endObservingDirectoryAtURL:';
    function menuForMenuKind (menu: FIMenuKind): NSMenu; message 'menuForMenuKind:';
    procedure requestBadgeIdentifierForURL (url: NSURL); message 'requestBadgeIdentifierForURL:';
    function toolbarItemImage: NSImage; message 'toolbarItemImage';
    function toolbarItemName: NSString; message 'toolbarItemName';
    function toolbarItemToolTip: NSString; message 'toolbarItemToolTip';
  end;
{$endif}

{$endif}
