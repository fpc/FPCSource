{ Parsed from ImageKit.framework IKSlideshow.h }


{$ifdef TYPES}
type
  IKSlideshowPtr = ^IKSlideshow;
  IKSlideshowDataSourceProtocolPtr = ^IKSlideshowDataSourceProtocol;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef PROTOCOLS}

type
  IKSlideshowDataSourceProtocol = objcprotocol external name 'IKSlideshowDataSource'
  required
    function numberOfSlideshowItems: NSUInteger; message 'numberOfSlideshowItems';
    function slideshowItemAtIndex (index: NSUInteger): id; message 'slideshowItemAtIndex:';
  optional
    function nameOfSlideshowItemAtIndex (index: NSUInteger): NSString; message 'nameOfSlideshowItemAtIndex:';
    function canExportSlideshowItemAtIndex_toApplication (index: NSUInteger; applicationBundleIdentifier: NSString): ObjCBOOL; message 'canExportSlideshowItemAtIndex:toApplication:';
    procedure slideshowWillStart; message 'slideshowWillStart';
    procedure slideshowDidStop; message 'slideshowDidStop';
    procedure slideshowDidChangeCurrentIndex (newIndex: NSUInteger); message 'slideshowDidChangeCurrentIndex:';
  end;
{$endif}

{$ifdef CLASSES}

type
  IKSlideshow = objcclass external (NSObject)
  private
    _privateData: id;
  public
    procedure setAutoPlayDelay(newValue: NSTimeInterval); message 'setAutoPlayDelay:';
    function autoPlayDelay: NSTimeInterval; message 'autoPlayDelay';
    class function sharedSlideshow: IKSlideshow; message 'sharedSlideshow';
    procedure runSlideshowWithDataSource_inMode_options (dataSource: IKSlideshowDataSourceProtocol; slideshowMode: NSString; slideshowOptions: NSDictionary); message 'runSlideshowWithDataSource:inMode:options:';
    procedure stopSlideshow (sender: id); message 'stopSlideshow:';
    procedure reloadData; message 'reloadData';
    procedure reloadSlideshowItemAtIndex (index: NSUInteger); message 'reloadSlideshowItemAtIndex:';
    function indexOfCurrentSlideshowItem: NSUInteger; message 'indexOfCurrentSlideshowItem';
    class function canExportToApplication (applicationBundleIdentifier: NSString): ObjCBOOL; message 'canExportToApplication:';
    class procedure exportSlideshowItem_toApplication (item: id; applicationBundleIdentifier: NSString); message 'exportSlideshowItem:toApplication:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  IKSlideshowModeImages: NSString; cvar; external;
  IKSlideshowModePDF: NSString; cvar; external;
  IKSlideshowModeOther: NSString; cvar; external;
  IKSlideshowWrapAround: NSString; cvar; external;
  IKSlideshowStartPaused: NSString; cvar; external;
  IKSlideshowStartIndex: NSString; cvar; external;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef EXTERNAL_SYMBOLS}
var
  IKSlideshowScreen: NSString; cvar; external;
  IKSlideshowAudioFile: NSString; cvar; external;
{$endif}

{$endif}
{$ifdef EXTERNAL_SYMBOLS}
var
  IKSlideshowPDFDisplayBox: NSString; cvar; external;
  IKSlideshowPDFDisplayMode: NSString; cvar; external;
  IKSlideshowPDFDisplaysAsBook: NSString; cvar; external;
  IK_iPhotoBundleIdentifier: NSString; cvar; external;
  IK_ApertureBundleIdentifier: NSString; cvar; external;
  IK_MailBundleIdentifier: NSString; cvar; external;
{$endif}

{$endif}
