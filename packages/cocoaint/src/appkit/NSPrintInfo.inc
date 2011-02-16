{ Parsed from Appkit.framework NSPrintInfo.h }

{$ifdef TYPES}
{$ifndef NSPRINTINFO_PAS_T}
{$define NSPRINTINFO_PAS_T}

{ Constants }

const
  NSPortraitOrientation = 0;
  NSLandscapeOrientation = 1;

const
  NSAutoPagination = 0;
  NSFitPagination = 1;
  NSClipPagination = 2;

{ Types }
type
  NSPrintingOrientation = NSUInteger;
  NSPrintingOrientationPtr = ^NSPrintingOrientation;
  NSPrintingPaginationMode = NSUInteger;
  NSPrintingPaginationModePtr = ^NSPrintingPaginationMode;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPRINTINFO_PAS_R}
{$define NSPRINTINFO_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPRINTINFO_PAS_F}
{$define NSPRINTINFO_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPRINTINFO_PAS_S}
{$define NSPRINTINFO_PAS_S}

{ External string constants }
var
  NSPrintSpoolJob: NSString; cvar; external;
  NSPrintPreviewJob: NSString; cvar; external;
  NSPrintSaveJob: NSString; cvar; external;
  NSPrintCancelJob: NSString; cvar; external;
  NSPrintPaperName: NSString; cvar; external;
  NSPrintPaperSize: NSString; cvar; external;
  NSPrintOrientation: NSString; cvar; external;
  NSPrintScalingFactor: NSString; cvar; external;
  NSPrintLeftMargin: NSString; cvar; external;
  NSPrintRightMargin: NSString; cvar; external;
  NSPrintTopMargin: NSString; cvar; external;
  NSPrintBottomMargin: NSString; cvar; external;
  NSPrintHorizontallyCentered: NSString; cvar; external;
  NSPrintVerticallyCentered: NSString; cvar; external;
  NSPrintHorizontalPagination: NSString; cvar; external;
  NSPrintVerticalPagination: NSString; cvar; external;
  NSPrintPrinter: NSString; cvar; external;
  NSPrintCopies: NSString; cvar; external;
  NSPrintAllPages: NSString; cvar; external;
  NSPrintFirstPage: NSString; cvar; external;
  NSPrintLastPage: NSString; cvar; external;
  NSPrintMustCollate: NSString; cvar; external;
  NSPrintReversePageOrder: NSString; cvar; external;
  NSPrintJobDisposition: NSString; cvar; external;
  NSPrintPagesAcross: NSString; cvar; external;
  NSPrintPagesDown: NSString; cvar; external;
  NSPrintTime: NSString; cvar; external;
  NSPrintDetailedErrorReporting: NSString; cvar; external;
  NSPrintFaxNumber: NSString; cvar; external;
  NSPrintPrinterName: NSString; cvar; external;
  NSPrintSelectionOnly: NSString; cvar; external;
  NSPrintJobSavingURL: NSString; cvar; external;
  NSPrintJobSavingFileNameExtensionHidden: NSString; cvar; external;
  NSPrintHeaderAndFooter: NSString; cvar; external;
  NSPrintFormName: NSString deprecated 'in Mac OS X 10.2 and later'; cvar; external;
  NSPrintJobFeatures: NSString deprecated 'in Mac OS X 10.2 and later'; cvar; external;
  NSPrintManualFeed: NSString deprecated 'in Mac OS X 10.2 and later'; cvar; external;
  NSPrintPagesPerSheet: NSString deprecated 'in Mac OS X 10.2 and later'; cvar; external;
  NSPrintPaperFeed: NSString deprecated 'in Mac OS X 10.2 and later'; cvar; external;
  NSPrintSavePath: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPrintInfo = objcclass;
  NSPrintInfoPointer = ^NSPrintInfo;
  NSPrintInfoPtr = NSPrintInfoPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPRINTINFO_PAS_C}
{$define NSPRINTINFO_PAS_C}

{ NSPrintInfo }
  NSPrintInfo = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _attributes: NSMutableDictionary;
    _moreVars: id;
    
  public
    class procedure setSharedPrintInfo(printInfo: NSPrintInfo); message 'setSharedPrintInfo:';
    class function sharedPrintInfo: NSPrintInfo; message 'sharedPrintInfo';
    function initWithDictionary(attributes: NSDictionary): id; message 'initWithDictionary:';
    function dictionary: NSMutableDictionary; message 'dictionary';
    procedure setPaperName(name: NSString); message 'setPaperName:';
    procedure setPaperSize(size: NSSize); message 'setPaperSize:';
    procedure setOrientation(orientation_: NSPrintingOrientation); message 'setOrientation:';
    procedure setScalingFactor(scalingFactor_: CGFloat); message 'setScalingFactor:';
    function paperName: NSString; message 'paperName';
    function paperSize: NSSize; message 'paperSize';
    function orientation: NSPrintingOrientation; message 'orientation';
    function scalingFactor: CGFloat; message 'scalingFactor';
    procedure setLeftMargin(margin: CGFloat); message 'setLeftMargin:';
    procedure setRightMargin(margin: CGFloat); message 'setRightMargin:';
    procedure setTopMargin(margin: CGFloat); message 'setTopMargin:';
    procedure setBottomMargin(margin: CGFloat); message 'setBottomMargin:';
    function leftMargin: CGFloat; message 'leftMargin';
    function rightMargin: CGFloat; message 'rightMargin';
    function topMargin: CGFloat; message 'topMargin';
    function bottomMargin: CGFloat; message 'bottomMargin';
    procedure setHorizontallyCentered(flag: Boolean); message 'setHorizontallyCentered:';
    procedure setVerticallyCentered(flag: Boolean); message 'setVerticallyCentered:';
    function isHorizontallyCentered: Boolean; message 'isHorizontallyCentered';
    function isVerticallyCentered: Boolean; message 'isVerticallyCentered';
    procedure setHorizontalPagination(mode: NSPrintingPaginationMode); message 'setHorizontalPagination:';
    procedure setVerticalPagination(mode: NSPrintingPaginationMode); message 'setVerticalPagination:';
    function horizontalPagination: NSPrintingPaginationMode; message 'horizontalPagination';
    function verticalPagination: NSPrintingPaginationMode; message 'verticalPagination';
    procedure setJobDisposition(disposition: NSString); message 'setJobDisposition:';
    function jobDisposition: NSString; message 'jobDisposition';
    procedure setPrinter(printer_: NSPrinter); message 'setPrinter:';
    function printer: NSPrinter; message 'printer';
    procedure setUpPrintOperationDefaultValues; message 'setUpPrintOperationDefaultValues';
    function imageablePageBounds: NSRect; message 'imageablePageBounds';
    function localizedPaperName: NSString; message 'localizedPaperName';
    class function defaultPrinter: NSPrinter; message 'defaultPrinter';
    function printSettings: NSMutableDictionary; message 'printSettings';
    function PMPrintSession: Pointer; message 'PMPrintSession';
    function PMPageFormat: Pointer; message 'PMPageFormat';
    function PMPrintSettings: Pointer; message 'PMPrintSettings';
    procedure updateFromPMPageFormat; message 'updateFromPMPageFormat';
    procedure updateFromPMPrintSettings; message 'updateFromPMPrintSettings';
    procedure setSelectionOnly(selectionOnly: Boolean); message 'setSelectionOnly:';
    function isSelectionOnly: Boolean; message 'isSelectionOnly';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSDeprecated_NSPrintInfoCategory }
  NSDeprecated_NSPrintInfoCategory = objccategory external name 'NSDeprecated' (NSPrintInfo)
    class procedure setDefaultPrinter(printer_: NSPrinter); message 'setDefaultPrinter:'; deprecated 'in Mac OS X 10.2 and later';
    class function sizeForPaperName(name: NSString): NSSize; message 'sizeForPaperName:'; deprecated 'in Mac OS X 10.2 and later';
  end;

{$endif}
{$endif}
