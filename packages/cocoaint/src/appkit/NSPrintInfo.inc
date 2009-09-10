{ Parsed from Appkit.framework NSPrintInfo.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSPRINTINFO_PAS_H}
{$define NSPRINTINFO_PAS_H}
type
  NSPrintInfoPointer = Pointer;

{$endif}
{$endif}

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
  NSPrintingOrientation = culong;
  NSPrintingPaginationMode = culong;

{ CFString constants }
var
  NSPrintSpoolJob: CFStringRef; external name '_NSPrintSpoolJob';
  NSPrintPreviewJob: CFStringRef; external name '_NSPrintPreviewJob';
  NSPrintSaveJob: CFStringRef; external name '_NSPrintSaveJob';
  NSPrintCancelJob: CFStringRef; external name '_NSPrintCancelJob';
  NSPrintPaperName: CFStringRef; external name '_NSPrintPaperName';
  NSPrintPaperSize: CFStringRef; external name '_NSPrintPaperSize';
  NSPrintOrientation: CFStringRef; external name '_NSPrintOrientation';
  NSPrintScalingFactor: CFStringRef; external name '_NSPrintScalingFactor';
  NSPrintLeftMargin: CFStringRef; external name '_NSPrintLeftMargin';
  NSPrintRightMargin: CFStringRef; external name '_NSPrintRightMargin';
  NSPrintTopMargin: CFStringRef; external name '_NSPrintTopMargin';
  NSPrintBottomMargin: CFStringRef; external name '_NSPrintBottomMargin';
  NSPrintHorizontallyCentered: CFStringRef; external name '_NSPrintHorizontallyCentered';
  NSPrintVerticallyCentered: CFStringRef; external name '_NSPrintVerticallyCentered';
  NSPrintHorizontalPagination: CFStringRef; external name '_NSPrintHorizontalPagination';
  NSPrintVerticalPagination: CFStringRef; external name '_NSPrintVerticalPagination';
  NSPrintPrinter: CFStringRef; external name '_NSPrintPrinter';
  NSPrintCopies: CFStringRef; external name '_NSPrintCopies';
  NSPrintAllPages: CFStringRef; external name '_NSPrintAllPages';
  NSPrintFirstPage: CFStringRef; external name '_NSPrintFirstPage';
  NSPrintLastPage: CFStringRef; external name '_NSPrintLastPage';
  NSPrintMustCollate: CFStringRef; external name '_NSPrintMustCollate';
  NSPrintReversePageOrder: CFStringRef; external name '_NSPrintReversePageOrder';
  NSPrintJobDisposition: CFStringRef; external name '_NSPrintJobDisposition';
  NSPrintSavePath: CFStringRef; external name '_NSPrintSavePath';
  NSPrintFormName: CFStringRef; external name '_NSPrintFormName';
  NSPrintJobFeatures: CFStringRef; external name '_NSPrintJobFeatures';
  NSPrintManualFeed: CFStringRef; external name '_NSPrintManualFeed';
  NSPrintPagesPerSheet: CFStringRef; external name '_NSPrintPagesPerSheet';
  NSPrintPaperFeed: CFStringRef; external name '_NSPrintPaperFeed';
  NSPrintFaxCoverSheetName: CFStringRef; external name '_NSPrintFaxCoverSheetName';
  NSPrintFaxHighResolution: CFStringRef; external name '_NSPrintFaxHighResolution';
  NSPrintFaxModem: CFStringRef; external name '_NSPrintFaxModem';
  NSPrintFaxReceiverNames: CFStringRef; external name '_NSPrintFaxReceiverNames';
  NSPrintFaxReceiverNumbers: CFStringRef; external name '_NSPrintFaxReceiverNumbers';
  NSPrintFaxReturnReceipt: CFStringRef; external name '_NSPrintFaxReturnReceipt';
  NSPrintFaxSendTime: CFStringRef; external name '_NSPrintFaxSendTime';
  NSPrintFaxTrimPageEnds: CFStringRef; external name '_NSPrintFaxTrimPageEnds';
  NSPrintFaxUseCoverSheet: CFStringRef; external name '_NSPrintFaxUseCoverSheet';
  NSPrintFaxJob: CFStringRef; external name '_NSPrintFaxJob';

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
{$ifndef NSPRINTINFO_PAS_T}
{$define NSPRINTINFO_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPrintInfo = objcclass;

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
