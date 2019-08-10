{ Parsed from AppKit.framework NSPrintInfo.h }


{$ifdef TYPES}
type
  NSPrintInfoPtr = ^NSPrintInfo;
{$endif}

{$ifdef TYPES}
type
  NSPaperOrientation = NSInteger;
  NSPaperOrientationPtr = ^NSPaperOrientation;

const
  NSPaperOrientationPortrait = 0;
  NSPaperOrientationLandscape = 1;

type
  NSPrintingPaginationMode = NSUInteger;
  NSPrintingPaginationModePtr = ^NSPrintingPaginationMode;

const
  NSAutoPagination = 0;
  NSFitPagination = 1;
  NSClipPagination = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
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
  NSPrintSelectionOnly: NSString { available in 10_6 }; cvar; external;
  NSPrintJobSavingURL: NSString { available in 10_6 }; cvar; external;
  NSPrintJobSavingFileNameExtensionHidden: NSString { available in 10_6 }; cvar; external;
  NSPrintHeaderAndFooter: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSPrintInfo = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _attributes: NSMutableDictionary;
    _moreVars: id;
  public
    class procedure setSharedPrintInfo (printInfo: NSPrintInfo); message 'setSharedPrintInfo:';
    class function sharedPrintInfo: NSPrintInfo; message 'sharedPrintInfo';
    function initWithDictionary (attributes: NSDictionary): instancetype; message 'initWithDictionary:';
    function dictionary: NSMutableDictionary; message 'dictionary';
    procedure setPaperName(newValue: NSString); message 'setPaperName:';
    function paperName: NSString; message 'paperName';
    procedure setPaperSize(newValue: NSSize); message 'setPaperSize:';
    function paperSize: NSSize; message 'paperSize';
    procedure setOrientation(newValue: NSPaperOrientation); message 'setOrientation:';
    function orientation: NSPaperOrientation; message 'orientation';
    procedure setScalingFactor(newValue: CGFloat); message 'setScalingFactor:';
    function scalingFactor: CGFloat; message 'scalingFactor';
    procedure setLeftMargin(newValue: CGFloat); message 'setLeftMargin:';
    function leftMargin: CGFloat; message 'leftMargin';
    procedure setRightMargin(newValue: CGFloat); message 'setRightMargin:';
    function rightMargin: CGFloat; message 'rightMargin';
    procedure setTopMargin(newValue: CGFloat); message 'setTopMargin:';
    function topMargin: CGFloat; message 'topMargin';
    procedure setBottomMargin(newValue: CGFloat); message 'setBottomMargin:';
    function bottomMargin: CGFloat; message 'bottomMargin';
    procedure setHorizontallyCentered(newValue: ObjCBOOL); message 'setHorizontallyCentered:';
    function isHorizontallyCentered: ObjCBOOL; message 'isHorizontallyCentered';
    procedure setVerticallyCentered(newValue: ObjCBOOL); message 'setVerticallyCentered:';
    function isVerticallyCentered: ObjCBOOL; message 'isVerticallyCentered';
    procedure setHorizontalPagination(newValue: NSPrintingPaginationMode); message 'setHorizontalPagination:';
    function horizontalPagination: NSPrintingPaginationMode; message 'horizontalPagination';
    procedure setVerticalPagination(newValue: NSPrintingPaginationMode); message 'setVerticalPagination:';
    function verticalPagination: NSPrintingPaginationMode; message 'verticalPagination';
    procedure setJobDisposition(newValue: NSString); message 'setJobDisposition:';
    function jobDisposition: NSString; message 'jobDisposition';
    procedure setPrinter(newValue: NSPrinter); message 'setPrinter:';
    function printer: NSPrinter; message 'printer';
    procedure setUpPrintOperationDefaultValues; message 'setUpPrintOperationDefaultValues';
    function imageablePageBounds: NSRect; message 'imageablePageBounds';
    function localizedPaperName: NSString; message 'localizedPaperName';
    class function defaultPrinter: NSPrinter; message 'defaultPrinter';
    function printSettings: NSMutableDictionary; message 'printSettings';
    function PMPrintSession: pointer; message 'PMPrintSession'; { available in 10_5 }
    function PMPageFormat: pointer; message 'PMPageFormat'; { available in 10_5 }
    function PMPrintSettings: pointer; message 'PMPrintSettings'; { available in 10_5 }
    procedure updateFromPMPageFormat; message 'updateFromPMPageFormat'; { available in 10_5 }
    procedure updateFromPMPrintSettings; message 'updateFromPMPrintSettings'; { available in 10_5 }
    procedure setSelectionOnly(newValue: ObjCBOOL); message 'setSelectionOnly:';
    function isSelectionOnly: ObjCBOOL; message 'isSelectionOnly';
    procedure takeSettingsFromPDFInfo (inPDFInfo: NSPDFInfo); message 'takeSettingsFromPDFInfo:'; { available in 10_9 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  NSPrintInfo_NSDeprecated = objccategory external name 'NSDeprecated' (NSPrintInfo)
    class procedure setDefaultPrinter (printer_: NSPrinter); message 'setDefaultPrinter:'; deprecated 'in 10_0, 10_2';
    class function sizeForPaperName (name: NSString): NSSize; message 'sizeForPaperName:'; deprecated 'in 10_0, 10_2';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSPrintFormName: NSString deprecated 'in 10_0, 10_2'; cvar; external;
  NSPrintJobFeatures: NSString deprecated 'in 10_0, 10_2'; cvar; external;
  NSPrintManualFeed: NSString deprecated 'in 10_0, 10_2'; cvar; external;
  NSPrintPagesPerSheet: NSString deprecated 'in 10_0, 10_2'; cvar; external;
  NSPrintPaperFeed: NSString deprecated 'in 10_0, 10_2'; cvar; external;
  NSPrintSavePath: NSString deprecated 'in 10_0, 10_6, "Use NSPrintJobSavingURL instead"'; cvar; external;
{$endif}

{$ifdef TYPES}
type
  NSPrintingOrientation = NSUInteger;
  NSPrintingOrientationPtr = ^NSPrintingOrientation;

const
  NSPortraitOrientation = 0;
  NSLandscapeOrientation = 1;
{$endif}

