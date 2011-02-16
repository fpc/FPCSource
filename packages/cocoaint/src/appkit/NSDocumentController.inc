{ Parsed from Appkit.framework NSDocumentController.h }

{$ifdef TYPES}
{$ifndef NSDOCUMENTCONTROLLER_PAS_T}
{$define NSDOCUMENTCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSDOCUMENTCONTROLLER_PAS_R}
{$define NSDOCUMENTCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSDOCUMENTCONTROLLER_PAS_F}
{$define NSDOCUMENTCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSDOCUMENTCONTROLLER_PAS_S}
{$define NSDOCUMENTCONTROLLER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSDocumentController = objcclass;
  NSDocumentControllerPointer = ^NSDocumentController;
  NSDocumentControllerPtr = NSDocumentControllerPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSDOCUMENTCONTROLLER_PAS_C}
{$define NSDOCUMENTCONTROLLER_PAS_C}

{ NSDocumentController }
  NSDocumentController = objcclass external (NSObject, NSCodingProtocol, NSUserInterfaceValidationsProtocol)
  private
    _documents: id;
    _moreVars: id;
    _cachedTypeDescriptions: NSArray;
    _recents: NSMutableDictionary;
    _recentsLimit: cint;
    
  public
    class function sharedDocumentController: id; message 'sharedDocumentController';
    function init: id; message 'init';
    function documents: NSArray; message 'documents';
    function currentDocument: id; message 'currentDocument';
    function currentDirectory: NSString; message 'currentDirectory';
    function documentForURL(absoluteURL: NSURL): id; message 'documentForURL:';
    function documentForWindow(window: NSWindow): id; message 'documentForWindow:';
    procedure addDocument(document: NSDocument); message 'addDocument:';
    procedure removeDocument(document: NSDocument); message 'removeDocument:';
    procedure newDocument(sender: id); message 'newDocument:';
    function openUntitledDocumentAndDisplay_error(displayDocument: Boolean; outError: NSErrorPointer): id; message 'openUntitledDocumentAndDisplay:error:';
    function makeUntitledDocumentOfType_error(typeName: NSString; outError: NSErrorPointer): id; message 'makeUntitledDocumentOfType:error:';
    procedure openDocument(sender: id); message 'openDocument:';
    function URLsFromRunningOpenPanel: NSArray; message 'URLsFromRunningOpenPanel';
    function runModalOpenPanel_forTypes(openPanel: NSOpenPanel; types: NSArray): NSInteger; message 'runModalOpenPanel:forTypes:';
    function openDocumentWithContentsOfURL_display_error(absoluteURL: NSURL; displayDocument: Boolean; outError: NSErrorPointer): id; message 'openDocumentWithContentsOfURL:display:error:';
    function makeDocumentWithContentsOfURL_ofType_error(absoluteURL: NSURL; typeName: NSString; outError: NSErrorPointer): id; message 'makeDocumentWithContentsOfURL:ofType:error:';
    function reopenDocumentForURL_withContentsOfURL_error(absoluteDocumentURL: NSURL; absoluteDocumentContentsURL: NSURL; outError: NSErrorPointer): Boolean; message 'reopenDocumentForURL:withContentsOfURL:error:';
    function makeDocumentForURL_withContentsOfURL_ofType_error(absoluteDocumentURL: NSURL; absoluteDocumentContentsURL: NSURL; typeName: NSString; outError: NSErrorPointer): id; message 'makeDocumentForURL:withContentsOfURL:ofType:error:';
    procedure setAutosavingDelay(autosavingDelay_: NSTimeInterval); message 'setAutosavingDelay:';
    function autosavingDelay: NSTimeInterval; message 'autosavingDelay';
    procedure saveAllDocuments(sender: id); message 'saveAllDocuments:';
    function hasEditedDocuments: Boolean; message 'hasEditedDocuments';
    procedure reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo(title: NSString; cancellable: Boolean; delegate: id; didReviewAllSelector: SEL; contextInfo: Pointer); message 'reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:';
    procedure closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo(delegate: id; didCloseAllSelector: SEL; contextInfo: Pointer); message 'closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:';
    procedure presentError_modalForWindow_delegate_didPresentSelector_contextInfo(error: NSError; window: NSWindow; delegate: id; didPresentSelector: SEL; contextInfo: Pointer); message 'presentError:modalForWindow:delegate:didPresentSelector:contextInfo:';
    function presentError(error: NSError): Boolean; message 'presentError:';
    function willPresentError(error: NSError): NSError; message 'willPresentError:';
    function maximumRecentDocumentCount: NSUInteger; message 'maximumRecentDocumentCount';
    procedure clearRecentDocuments(sender: id); message 'clearRecentDocuments:';
    procedure noteNewRecentDocument(document: NSDocument); message 'noteNewRecentDocument:';
    procedure noteNewRecentDocumentURL(absoluteURL: NSURL); message 'noteNewRecentDocumentURL:';
    function recentDocumentURLs: NSArray; message 'recentDocumentURLs';
    function defaultType: NSString; message 'defaultType';
    function typeForContentsOfURL_error(inAbsoluteURL: NSURL; outError: NSErrorPointer): NSString; message 'typeForContentsOfURL:error:';
    function documentClassNames: NSArray; message 'documentClassNames';
    function documentClassForType(typeName: NSString): Pobjc_class; message 'documentClassForType:';
    function displayNameForType(typeName: NSString): NSString; message 'displayNameForType:';
    function validateUserInterfaceItem(anItem: NSValidatedUserInterfaceItemProtocol): Boolean; message 'validateUserInterfaceItem:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSDeprecated_NSDocumentControllerCategory }
  NSDeprecated_NSDocumentControllerCategory = objccategory external name 'NSDeprecated' (NSDocumentController)
    function fileExtensionsFromType(typeName: NSString): NSArray; message 'fileExtensionsFromType:'; deprecated 'in Mac OS X 10.5 and later';
    function typeFromFileExtension(fileNameExtensionOrHFSFileType: NSString): NSString; message 'typeFromFileExtension:'; deprecated 'in Mac OS X 10.5 and later';
    function documentForFileName(fileName: NSString): id; message 'documentForFileName:'; deprecated 'in Mac OS X 10.4 and later';
    function fileNamesFromRunningOpenPanel: NSArray; message 'fileNamesFromRunningOpenPanel'; deprecated 'in Mac OS X 10.4 and later';
    function makeDocumentWithContentsOfFile_ofType(fileName: NSString; type_: NSString): id; message 'makeDocumentWithContentsOfFile:ofType:'; deprecated 'in Mac OS X 10.4 and later';
    function makeDocumentWithContentsOfURL_ofType(url: NSURL; type_: NSString): id; message 'makeDocumentWithContentsOfURL:ofType:'; deprecated 'in Mac OS X 10.4 and later';
    function makeUntitledDocumentOfType(type_: NSString): id; message 'makeUntitledDocumentOfType:'; deprecated 'in Mac OS X 10.4 and later';
    function openDocumentWithContentsOfFile_display(fileName: NSString; display: Boolean): id; message 'openDocumentWithContentsOfFile:display:'; deprecated 'in Mac OS X 10.4 and later';
    function openDocumentWithContentsOfURL_display(url: NSURL; display: Boolean): id; message 'openDocumentWithContentsOfURL:display:'; deprecated 'in Mac OS X 10.4 and later';
    function openUntitledDocumentOfType_display(type_: NSString; display: Boolean): id; message 'openUntitledDocumentOfType:display:'; deprecated 'in Mac OS X 10.4 and later';
    procedure setShouldCreateUI(flag: Boolean); message 'setShouldCreateUI:'; deprecated 'in Mac OS X 10.4 and later';
    function shouldCreateUI: Boolean; message 'shouldCreateUI'; deprecated 'in Mac OS X 10.4 and later';
  end;

{$endif}
{$endif}
