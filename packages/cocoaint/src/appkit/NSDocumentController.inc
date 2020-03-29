{ Parsed from AppKit.framework NSDocumentController.h }


{$ifdef TYPES}
type
  NSDocumentControllerPtr = ^NSDocumentController;
{$endif}

{$ifdef CLASSES}

type
  NSDocumentController = objcclass external (NSObject, NSCodingProtocol, NSUserInterfaceValidationsProtocol)
  private
    _documents: id;
    _moreVars: id;
    _cachedTypeDescriptions: NSArray;
    _recents: NSMutableDictionary;
    _recentsLimit: cint;
  public
    class function sharedDocumentController: id; message 'sharedDocumentController';
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    function documents: NSArray; message 'documents';
    function currentDocument: id; message 'currentDocument';
    function currentDirectory: NSString; message 'currentDirectory';
    function documentForURL (url: NSURL): id; message 'documentForURL:';
    function documentForWindow (window: NSWindow): id; message 'documentForWindow:';
    procedure addDocument (document: NSDocument); message 'addDocument:';
    procedure removeDocument (document: NSDocument); message 'removeDocument:';
    procedure newDocument (sender: id); message 'newDocument:';
    function openUntitledDocumentAndDisplay_error (displayDocument: ObjCBOOL; outError: NSErrorPtr): id; message 'openUntitledDocumentAndDisplay:error:';
    function makeUntitledDocumentOfType_error (typeName: NSString; outError: NSErrorPtr): id; message 'makeUntitledDocumentOfType:error:';
    procedure openDocument (sender: id); message 'openDocument:';
    function URLsFromRunningOpenPanel: NSArray; message 'URLsFromRunningOpenPanel';
    function runModalOpenPanel_forTypes (openPanel: NSOpenPanel; types: NSArray): NSInteger; message 'runModalOpenPanel:forTypes:';
    procedure beginOpenPanelWithCompletionHandler (completionHandler: OpaqueCBlock); message 'beginOpenPanelWithCompletionHandler:'; { available in 10_8 }
    procedure beginOpenPanel_forTypes_completionHandler (openPanel: NSOpenPanel; inTypes: NSArray; completionHandler: OpaqueCBlock); message 'beginOpenPanel:forTypes:completionHandler:'; { available in 10_8 }
    procedure openDocumentWithContentsOfURL_display_completionHandler (url: NSURL; displayDocument: ObjCBOOL; completionHandler: OpaqueCBlock); message 'openDocumentWithContentsOfURL:display:completionHandler:'; { available in 10_7 }
    function makeDocumentWithContentsOfURL_ofType_error (url: NSURL; typeName: NSString; outError: NSErrorPtr): id; message 'makeDocumentWithContentsOfURL:ofType:error:';
    procedure reopenDocumentForURL_withContentsOfURL_display_completionHandler (urlOrNil: NSURL; contentsURL: NSURL; displayDocument: ObjCBOOL; completionHandler: OpaqueCBlock); message 'reopenDocumentForURL:withContentsOfURL:display:completionHandler:'; { available in 10_7 }
    function makeDocumentForURL_withContentsOfURL_ofType_error (urlOrNil: NSURL; contentsURL: NSURL; typeName: NSString; outError: NSErrorPtr): id; message 'makeDocumentForURL:withContentsOfURL:ofType:error:';
    procedure setAutosavingDelay(newValue: NSTimeInterval); message 'setAutosavingDelay:';
    function autosavingDelay: NSTimeInterval; message 'autosavingDelay';
    procedure saveAllDocuments (sender: id); message 'saveAllDocuments:';
    function hasEditedDocuments: ObjCBOOL; message 'hasEditedDocuments';
    procedure reviewUnsavedDocumentsWithAlertTitle_cancellable_delegate_didReviewAllSelector_contextInfo (title: NSString; cancellable: ObjCBOOL; delegate: id; didReviewAllSelector: SEL; contextInfo: pointer); message 'reviewUnsavedDocumentsWithAlertTitle:cancellable:delegate:didReviewAllSelector:contextInfo:';
    procedure closeAllDocumentsWithDelegate_didCloseAllSelector_contextInfo (delegate: id; didCloseAllSelector: SEL; contextInfo: pointer); message 'closeAllDocumentsWithDelegate:didCloseAllSelector:contextInfo:';
    function duplicateDocumentWithContentsOfURL_copying_displayName_error (url: NSURL; duplicateByCopying: ObjCBOOL; displayNameOrNil: NSString; outError: NSErrorPtr): NSDocument; message 'duplicateDocumentWithContentsOfURL:copying:displayName:error:'; { available in 10_7 }
    procedure presentError_modalForWindow_delegate_didPresentSelector_contextInfo (error: NSError; window: NSWindow; delegate: id; didPresentSelector: SEL; contextInfo: pointer); message 'presentError:modalForWindow:delegate:didPresentSelector:contextInfo:';
    function presentError (error: NSError): ObjCBOOL; message 'presentError:';
    function willPresentError (error: NSError): NSError; message 'willPresentError:';
    function maximumRecentDocumentCount: NSUInteger; message 'maximumRecentDocumentCount';
    procedure clearRecentDocuments (sender: id); message 'clearRecentDocuments:';
    procedure noteNewRecentDocument (document: NSDocument); message 'noteNewRecentDocument:';
    procedure noteNewRecentDocumentURL (url: NSURL); message 'noteNewRecentDocumentURL:';
    function recentDocumentURLs: NSArray; message 'recentDocumentURLs';
    function defaultType: NSString; message 'defaultType';
    function typeForContentsOfURL_error (url: NSURL; outError: NSErrorPtr): NSString; message 'typeForContentsOfURL:error:';
    function documentClassNames: NSArray; message 'documentClassNames';
    function documentClassForType (typeName: NSString): pobjc_class; message 'documentClassForType:';
    function displayNameForType (typeName: NSString): NSString; message 'displayNameForType:';
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;


type
  NSDocumentController_NSDeprecated = objccategory external name 'NSDeprecated' (NSDocumentController)
    function openDocumentWithContentsOfURL_display_error (url: NSURL; displayDocument: ObjCBOOL; outError: NSErrorPtr): id; message 'openDocumentWithContentsOfURL:display:error:'; deprecated 'in 10_4, 10_7, "Use -openDocumentWithContentsOfURL:display:completionHandler: instead"';
    function reopenDocumentForURL_withContentsOfURL_error (url: NSURL; contentsURL: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'reopenDocumentForURL:withContentsOfURL:error:'; deprecated 'in 10_4, 10_7, "Use -reopenDocumentForURL:withContentsOfURL:display:completionHandler: instead"';
    function fileExtensionsFromType (typeName: NSString): NSArray; message 'fileExtensionsFromType:'; deprecated 'in 10_0, 10_5';
    function typeFromFileExtension (fileNameExtensionOrHFSFileType: NSString): NSString; message 'typeFromFileExtension:'; deprecated 'in 10_0, 10_5';
    function documentForFileName (fileName: NSString): id; message 'documentForFileName:'; deprecated 'in 10_0, 10_4';
    function fileNamesFromRunningOpenPanel: NSArray; message 'fileNamesFromRunningOpenPanel'; deprecated 'in 10_0, 10_4';
    function makeDocumentWithContentsOfFile_ofType (fileName: NSString; type_: NSString): id; message 'makeDocumentWithContentsOfFile:ofType:'; deprecated 'in 10_0, 10_4';
    function makeDocumentWithContentsOfURL_ofType (url: NSURL; type_: NSString): id; message 'makeDocumentWithContentsOfURL:ofType:'; deprecated 'in 10_0, 10_4';
    function makeUntitledDocumentOfType (type_: NSString): id; message 'makeUntitledDocumentOfType:'; deprecated 'in 10_0, 10_4';
    function openDocumentWithContentsOfFile_display (fileName: NSString; display: ObjCBOOL): id; message 'openDocumentWithContentsOfFile:display:'; deprecated 'in 10_0, 10_4';
    function openDocumentWithContentsOfURL_display (url: NSURL; display: ObjCBOOL): id; message 'openDocumentWithContentsOfURL:display:'; deprecated 'in 10_0, 10_4';
    function openUntitledDocumentOfType_display (type_: NSString; display: ObjCBOOL): id; message 'openUntitledDocumentOfType:display:'; deprecated 'in 10_0, 10_4';
    procedure setShouldCreateUI (flag: ObjCBOOL); message 'setShouldCreateUI:'; deprecated 'in 10_0, 10_4';
    function shouldCreateUI: ObjCBOOL; message 'shouldCreateUI'; deprecated 'in 10_0, 10_4';
  end;
{$endif}

