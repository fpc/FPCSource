{ Parsed from AppKit.framework NSSharingService.h }


{$ifdef TYPES}
type
  NSSharingServicePtr = ^NSSharingService;
  NSSharingServicePickerPtr = ^NSSharingServicePicker;
  NSSharingServiceDelegateProtocolPtr = ^NSSharingServiceDelegateProtocol;
  NSSharingServicePickerDelegateProtocolPtr = ^NSSharingServicePickerDelegateProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSharingServiceNamePostOnFacebook: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostOnTwitter: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostOnSinaWeibo: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostOnTencentWeibo: NSString { available in 10_9 }; cvar; external;
  NSSharingServiceNamePostOnLinkedIn: NSString { available in 10_9 }; cvar; external;
  NSSharingServiceNameComposeEmail: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameComposeMessage: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameSendViaAirDrop: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameAddToSafariReadingList: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameAddToIPhoto: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameAddToAperture: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameUseAsTwitterProfileImage: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNameUseAsFacebookProfileImage: NSString { available in 10_9 }; cvar; external;
  NSSharingServiceNameUseAsLinkedInProfileImage: NSString { available in 10_9 }; cvar; external;
  NSSharingServiceNameUseAsDesktopPicture: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostImageOnFlickr: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostVideoOnVimeo: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostVideoOnYouku: NSString { available in 10_8 }; cvar; external;
  NSSharingServiceNamePostVideoOnTudou: NSString { available in 10_8 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSSharingService = objcclass external (NSObject)
  private
    _reserved: id;
  public
    procedure setDelegate(newValue: NSSharingServiceDelegateProtocol); message 'setDelegate:';
    function delegate: NSSharingServiceDelegateProtocol; message 'delegate';
    function title: NSString; message 'title';
    function image: NSImage; message 'image';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setMenuItemTitle(newValue: NSString); message 'setMenuItemTitle:';
    function menuItemTitle: NSString; message 'menuItemTitle';
    procedure setRecipients(newValue: NSArray); message 'setRecipients:';
    function recipients: NSArray; message 'recipients';
    procedure setSubject(newValue: NSString); message 'setSubject:';
    function subject: NSString; message 'subject';
    function messageBody: NSString; message 'messageBody';
    function permanentLink: NSURL; message 'permanentLink';
    function accountName: NSString; message 'accountName';
    function attachmentFileURLs: NSArray; message 'attachmentFileURLs';
    class function sharingServicesForItems (items: NSArray): NSArray; message 'sharingServicesForItems:';
    class function sharingServiceNamed (serviceName: NSString): NSSharingService; message 'sharingServiceNamed:';
    function initWithTitle_image_alternateImage_handler (title_: NSString; image_: NSImage; alternateImage_: NSImage; block: OpaqueCBlock): instancetype; message 'initWithTitle:image:alternateImage:handler:';
    function canPerformWithItems (items: NSArray): ObjCBOOL; message 'canPerformWithItems:';
    procedure performWithItems (items: NSArray); message 'performWithItems:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSSharingContentScope = NSInteger;
  NSSharingContentScopePtr = ^NSSharingContentScope;

const
  NSSharingContentScopeItem = 0;
  NSSharingContentScopePartial = 1;
  NSSharingContentScopeFull = 2;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSharingServiceDelegateProtocol = objcprotocol external name 'NSSharingServiceDelegate' (NSObjectProtocol)
  optional
    procedure sharingService_willShareItems (sharingService: NSSharingService; items: NSArray); message 'sharingService:willShareItems:';
    procedure sharingService_didFailToShareItems_error (sharingService: NSSharingService; items: NSArray; error: NSError); message 'sharingService:didFailToShareItems:error:';
    procedure sharingService_didShareItems (sharingService: NSSharingService; items: NSArray); message 'sharingService:didShareItems:';
    function sharingService_sourceFrameOnScreenForShareItem (sharingService: NSSharingService; item: NSPasteboardWritingProtocol): NSRect; message 'sharingService:sourceFrameOnScreenForShareItem:';
    function sharingService_transitionImageForShareItem_contentRect (sharingService: NSSharingService; item: NSPasteboardWritingProtocol; contentRect: NSRectPtr): NSImage; message 'sharingService:transitionImageForShareItem:contentRect:';
    function sharingService_sourceWindowForShareItems_sharingContentScope (sharingService: NSSharingService; items: NSArray; sharingContentScope: NSSharingContentScopePtr): NSWindow; message 'sharingService:sourceWindowForShareItems:sharingContentScope:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSSharingServicePicker = objcclass external (NSObject)
  private
    _reserved: id;
  public
    procedure setDelegate(newValue: NSSharingServicePickerDelegateProtocol); message 'setDelegate:';
    function delegate: NSSharingServicePickerDelegateProtocol; message 'delegate';
    function initWithItems (items: NSArray): instancetype; message 'initWithItems:';
    procedure showRelativeToRect_ofView_preferredEdge (rect: NSRect; view: NSView; preferredEdge: NSRectEdge); message 'showRelativeToRect:ofView:preferredEdge:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSSharingServicePickerDelegateProtocol = objcprotocol external name 'NSSharingServicePickerDelegate' (NSObjectProtocol)
  optional
    function sharingServicePicker_sharingServicesForItems_proposedSharingServices (sharingServicePicker: NSSharingServicePicker; items: NSArray; proposedServices: NSArray): NSArray; message 'sharingServicePicker:sharingServicesForItems:proposedSharingServices:';
    function sharingServicePicker_delegateForSharingService (sharingServicePicker: NSSharingServicePicker; sharingService: NSSharingService): NSSharingServiceDelegateProtocol; message 'sharingServicePicker:delegateForSharingService:';
    procedure sharingServicePicker_didChooseSharingService (sharingServicePicker: NSSharingServicePicker; service: NSSharingService); message 'sharingServicePicker:didChooseSharingService:';
  end;
{$endif}

