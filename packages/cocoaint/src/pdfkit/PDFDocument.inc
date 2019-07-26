{ Parsed from PDFKit.framework PDFDocument.h }


{ Types from PDFDocument }
{$ifdef TYPES}






{$endif}


{ Types from PDFDocumentDelegate }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  PDFDocumentPtr = ^PDFDocument;
{$endif}

{$ifdef TYPES}
type
  PDFPrintScalingMode = NSInteger;
  PDFPrintScalingModePtr = ^PDFPrintScalingMode;

const
  kPDFPrintPageScaleNone = 0;
  kPDFPrintPageScaleToFit = 1;
  kPDFPrintPageScaleDownToFit = 2;

type
  PDFDocumentPermissions = NSInteger;
  PDFDocumentPermissionsPtr = ^PDFDocumentPermissions;

const
  kPDFDocumentPermissionsNone = 0;
  kPDFDocumentPermissionsUser = 1;
  kPDFDocumentPermissionsOwner = 2;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  PDFDocumentDidUnlockNotification: NSString; cvar; external;
  PDFDocumentDidBeginFindNotification: NSString; cvar; external;
  PDFDocumentDidEndFindNotification: NSString; cvar; external;
  PDFDocumentDidBeginPageFindNotification: NSString; cvar; external;
  PDFDocumentDidEndPageFindNotification: NSString; cvar; external;
  PDFDocumentDidFindMatchNotification: NSString; cvar; external;
  PDFDocumentDidBeginWriteNotification: NSString; cvar; external;
  PDFDocumentDidEndWriteNotification: NSString; cvar; external;
  PDFDocumentDidBeginPageWriteNotification: NSString; cvar; external;
  PDFDocumentDidEndPageWriteNotification: NSString; cvar; external;
  PDFDocumentTitleAttribute: NSString; cvar; external;
  PDFDocumentAuthorAttribute: NSString; cvar; external;
  PDFDocumentSubjectAttribute: NSString; cvar; external;
  PDFDocumentCreatorAttribute: NSString; cvar; external;
  PDFDocumentProducerAttribute: NSString; cvar; external;
  PDFDocumentCreationDateAttribute: NSString; cvar; external;
  PDFDocumentModificationDateAttribute: NSString; cvar; external;
  PDFDocumentKeywordsAttribute: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  PDFDocument = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pdfPriv: PDFDocumentPrivateVars;
  public
    function initWithURL (url: NSURL): id; message 'initWithURL:';
    function initWithData (data: NSData): id; message 'initWithData:';
    function documentURL: NSURL; message 'documentURL';
    function documentRef: CGPDFDocumentRef; message 'documentRef';
    function documentAttributes: NSDictionary; message 'documentAttributes';
    procedure setDocumentAttributes (attributes: NSDictionary); message 'setDocumentAttributes:';
    function majorVersion: cint; message 'majorVersion';
    function minorVersion: cint; message 'minorVersion';
    function isEncrypted: ObjCBOOL; message 'isEncrypted';
    function isLocked: ObjCBOOL; message 'isLocked';
    function unlockWithPassword (password: NSString): ObjCBOOL; message 'unlockWithPassword:';
    function allowsPrinting: ObjCBOOL; message 'allowsPrinting';
    function allowsCopying: ObjCBOOL; message 'allowsCopying';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function permissionsStatus: PDFDocumentPermissions; message 'permissionsStatus';
    {$endif}
    function string_: NSString; message 'string';
    procedure setDelegate (anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function dataRepresentation: NSData; message 'dataRepresentation';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function dataRepresentationWithOptions (options: NSDictionary): NSData; message 'dataRepresentationWithOptions:';
    {$endif}
    function writeToFile (path: NSString): ObjCBOOL; message 'writeToFile:';
    function writeToFile_withOptions (path: NSString; options: NSDictionary): ObjCBOOL; message 'writeToFile:withOptions:';
    function writeToURL (url: NSURL): ObjCBOOL; message 'writeToURL:';
    function writeToURL_withOptions (url: NSURL; options: NSDictionary): ObjCBOOL; message 'writeToURL:withOptions:';
    function outlineRoot: PDFOutline; message 'outlineRoot';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure setOutlineRoot (outline: PDFOutline); message 'setOutlineRoot:';
    {$endif}
    function outlineItemForSelection (selection: PDFSelection): PDFOutline; message 'outlineItemForSelection:';
    function pageCount: NSUInteger; message 'pageCount';
    function pageAtIndex (index: NSUInteger): PDFPage; message 'pageAtIndex:';
    function indexForPage (page: PDFPage): NSUInteger; message 'indexForPage:';
    procedure insertPage_atIndex (page: PDFPage; index: NSUInteger); message 'insertPage:atIndex:';
    procedure removePageAtIndex (index: NSUInteger); message 'removePageAtIndex:';
    procedure exchangePageAtIndex_withPageAtIndex (indexA: NSUInteger; indexB: NSUInteger); message 'exchangePageAtIndex:withPageAtIndex:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function pageClass: pobjc_class; message 'pageClass';
    {$endif}
    function findString_withOptions (string__: NSString; options: NSUInteger): NSArray; message 'findString:withOptions:';
    procedure beginFindString_withOptions (string__: NSString; options: NSUInteger); message 'beginFindString:withOptions:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure beginFindStrings_withOptions (strings: NSArray; options: NSUInteger); message 'beginFindStrings:withOptions:';
    {$endif}
    function findString_fromSelection_withOptions (string__: NSString; selection: PDFSelection; options: NSUInteger): PDFSelection; message 'findString:fromSelection:withOptions:';
    function isFinding: ObjCBOOL; message 'isFinding';
    procedure cancelFindString; message 'cancelFindString';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7)}
    function printOperationForPrintInfo_scalingMode_autoRotate (printInfo: NSPrintInfo; scaleMode: PDFPrintScalingMode; doRotate: ObjCBOOL): NSPrintOperation; message 'printOperationForPrintInfo:scalingMode:autoRotate:';
    {$endif}
    function selectionForEntireDocument: PDFSelection; message 'selectionForEntireDocument';
    function selectionFromPage_atPoint_toPage_atPoint (startPage: PDFPage; startPt: NSPoint; endPage: PDFPage; endPt: NSPoint): PDFSelection; message 'selectionFromPage:atPoint:toPage:atPoint:';
    function selectionFromPage_atCharacterIndex_toPage_atCharacterIndex (startPage: PDFPage; startChar: NSUInteger; endPage: PDFPage; endChar: NSUInteger): PDFSelection; message 'selectionFromPage:atCharacterIndex:toPage:atCharacterIndex:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  PDFDocumentNotifications = objccategory external (NSObject)
    procedure documentDidUnlock (notification: NSNotification); message 'documentDidUnlock:';
    procedure documentDidBeginDocumentFind (notification: NSNotification); message 'documentDidBeginDocumentFind:';
    procedure documentDidEndDocumentFind (notification: NSNotification); message 'documentDidEndDocumentFind:';
    procedure documentDidBeginPageFind (notification: NSNotification); message 'documentDidBeginPageFind:';
    procedure documentDidEndPageFind (notification: NSNotification); message 'documentDidEndPageFind:';
    procedure documentDidFindMatch (notification: NSNotification); message 'documentDidFindMatch:';
  end;

type
  PDFDocumentDelegate = objccategory external (NSObject)
    procedure didMatchString (instance: PDFSelection); message 'didMatchString:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
    function classForPage: pobjc_class; message 'classForPage';
    function classForAnnotationClass (annotationClass: pobjc_class): pobjc_class; message 'classForAnnotationClass:';
    {$endif}
  end;
{$endif}

