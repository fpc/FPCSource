{ Parsed from Appkit.framework NSPrintInfo.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


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
  NSPrintingPaginationMode = NSUInteger;

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
  NSPrintSpoolJob: NSString; external name '_NSPrintSpoolJob';
  NSPrintPreviewJob: NSString; external name '_NSPrintPreviewJob';
  NSPrintSaveJob: NSString; external name '_NSPrintSaveJob';
  NSPrintCancelJob: NSString; external name '_NSPrintCancelJob';
  NSPrintPaperName: NSString; external name '_NSPrintPaperName';
  NSPrintPaperSize: NSString; external name '_NSPrintPaperSize';
  NSPrintOrientation: NSString; external name '_NSPrintOrientation';
  NSPrintScalingFactor: NSString; external name '_NSPrintScalingFactor';
  NSPrintLeftMargin: NSString; external name '_NSPrintLeftMargin';
  NSPrintRightMargin: NSString; external name '_NSPrintRightMargin';
  NSPrintTopMargin: NSString; external name '_NSPrintTopMargin';
  NSPrintBottomMargin: NSString; external name '_NSPrintBottomMargin';
  NSPrintHorizontallyCentered: NSString; external name '_NSPrintHorizontallyCentered';
  NSPrintVerticallyCentered: NSString; external name '_NSPrintVerticallyCentered';
  NSPrintHorizontalPagination: NSString; external name '_NSPrintHorizontalPagination';
  NSPrintVerticalPagination: NSString; external name '_NSPrintVerticalPagination';
  NSPrintPrinter: NSString; external name '_NSPrintPrinter';
  NSPrintCopies: NSString; external name '_NSPrintCopies';
  NSPrintAllPages: NSString; external name '_NSPrintAllPages';
  NSPrintFirstPage: NSString; external name '_NSPrintFirstPage';
  NSPrintLastPage: NSString; external name '_NSPrintLastPage';
  NSPrintMustCollate: NSString; external name '_NSPrintMustCollate';
  NSPrintReversePageOrder: NSString; external name '_NSPrintReversePageOrder';
  NSPrintJobDisposition: NSString; external name '_NSPrintJobDisposition';
  NSPrintSavePath: NSString; external name '_NSPrintSavePath';
  NSPrintPagesAcross: NSString; external name '_NSPrintPagesAcross';
  NSPrintPagesDown: NSString; external name '_NSPrintPagesDown';
  NSPrintTime: NSString; external name '_NSPrintTime';
  NSPrintDetailedErrorReporting: NSString; external name '_NSPrintDetailedErrorReporting';
  NSPrintFaxNumber: NSString; external name '_NSPrintFaxNumber';
  NSPrintPrinterName: NSString; external name '_NSPrintPrinterName';
  NSPrintHeaderAndFooter: NSString; external name '_NSPrintHeaderAndFooter';
  NSPrintFormName: NSString; external name '_NSPrintFormName';
  NSPrintJobFeatures: NSString; external name '_NSPrintJobFeatures';
  NSPrintManualFeed: NSString; external name '_NSPrintManualFeed';
  NSPrintPagesPerSheet: NSString; external name '_NSPrintPagesPerSheet';
  NSPrintPaperFeed: NSString; external name '_NSPrintPaperFeed';
  NSPrintFaxCoverSheetName: NSString; external name '_NSPrintFaxCoverSheetName';
  NSPrintFaxHighResolution: NSString; external name '_NSPrintFaxHighResolution';
  NSPrintFaxModem: NSString; external name '_NSPrintFaxModem';
  NSPrintFaxReceiverNames: NSString; external name '_NSPrintFaxReceiverNames';
  NSPrintFaxReceiverNumbers: NSString; external name '_NSPrintFaxReceiverNumbers';
  NSPrintFaxReturnReceipt: NSString; external name '_NSPrintFaxReturnReceipt';
  NSPrintFaxSendTime: NSString; external name '_NSPrintFaxSendTime';
  NSPrintFaxTrimPageEnds: NSString; external name '_NSPrintFaxTrimPageEnds';
  NSPrintFaxUseCoverSheet: NSString; external name '_NSPrintFaxUseCoverSheet';
  NSPrintFaxJob: NSString; external name '_NSPrintFaxJob';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPrintInfo = objcclass;
  NSPrintInfoPointer = ^NSPrintInfo;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPRINTINFO_PAS_C}
{$define NSPRINTINFO_PAS_C}

{ NSPrintInfo }
  NSPrintInfo = objcclass(NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    __attributes: NSMutableDictionary;
    __moreVars: id;
    
  public
    class function alloc: NSPrintInfo; message 'alloc';

    class procedure setSharedPrintInfo(printInfo: NSPrintInfo); message 'setSharedPrintInfo:';
    class function sharedPrintInfo: NSPrintInfo; message 'sharedPrintInfo';
    function initWithDictionary(attributes: NSDictionary): id; message 'initWithDictionary:';
    function dictionary: NSMutableDictionary; message 'dictionary';
    procedure setPaperName(name: NSString); message 'setPaperName:';
    procedure setPaperSize(size: NSSize); message 'setPaperSize:';
    procedure setOrientation(orientation_: NSPrintingOrientation); message 'setOrientation:';
    function paperName: NSString; message 'paperName';
    function paperSize: NSSize; message 'paperSize';
    function orientation: NSPrintingOrientation; message 'orientation';
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

    { Category: NSDeprecated }
    class procedure setDefaultPrinter(printer_: NSPrinter); message 'setDefaultPrinter:';
    class function sizeForPaperName(name: NSString): NSSize; message 'sizeForPaperName:';
  end; external;

{$endif}
{$endif}
