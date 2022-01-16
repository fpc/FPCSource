{ Parsed from AppKit.framework NSDocument.h }


{$ifdef TYPES}
type
  NSDocumentPtr = ^NSDocument;
{$endif}

{$ifdef TYPES}
type
  NSDocumentChangeType = NSUInteger;
  NSDocumentChangeTypePtr = ^NSDocumentChangeType;

const
  NSChangeDone = 0;
  NSChangeUndone = 1;
  NSChangeRedone = 5 { available in 10_5 };
  NSChangeCleared = 2;
  NSChangeReadOtherContents = 3;
  NSChangeAutosaved = 4;
  NSChangeDiscardable = 256 { available in 10_7 };

type
  NSSaveOperationType = NSUInteger;
  NSSaveOperationTypePtr = ^NSSaveOperationType;

const
  NSSaveOperation = 0;
  NSSaveAsOperation = 1;
  NSSaveToOperation = 2;
  NSAutosaveInPlaceOperation = 4 { available in 10_7 };
  NSAutosaveElsewhereOperation = 3 { available in 10_7 };
  NSAutosaveAsOperation = 5 { available in 10_8 };
  NSAutosaveOperation = 3 deprecated 'in 10_4, 10_7, "Use NSAutosaveElsewhereOperation instead"';
{$endif}

{$ifdef CLASSES}

type
  NSDocument = objcclass external (NSObject, NSFilePresenterProtocol, NSUserInterfaceValidationsProtocol)
  private
    _window: NSWindow;
    _windowControllers: id;
    _fileURL: NSURL;
    _fileType: NSString;
    _printInfo: NSPrintInfo;
    _documentReserved: clong;
    savePanelAccessory: NSView;
    _displayName: id;
    _privateData: id;
    _undoManager: NSUndoManager;
    _docFlags: bitpacked record
      case byte of
        0: (_anonBitField__docFlags0: cuint);
        1: (
          inClose: 0..1;
          hasUndoManager: 0..1;
          unused: 0..1;
          reconciledToFileName: 0..1;
          checkingDisplayName: 0..1;
          hasInvalidRestorableState: 0..1;
          hasEverHadInvalidRestorableState: 0..1;
          RESERVED: 0..((1 shl 25)-1);
        );
      end;
    _savePanelSaveType: NSString;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithType_error (typeName: NSString; outError: NSErrorPtr): instancetype; message 'initWithType:error:';
    class function canConcurrentlyReadDocumentsOfType (typeName: NSString): ObjCBOOL; message 'canConcurrentlyReadDocumentsOfType:'; { available in 10_6 }
    function initWithContentsOfURL_ofType_error (url: NSURL; typeName: NSString; outError: NSErrorPtr): instancetype; message 'initWithContentsOfURL:ofType:error:';
    function initForURL_withContentsOfURL_ofType_error (urlOrNil: NSURL; contentsURL: NSURL; typeName: NSString; outError: NSErrorPtr): instancetype; message 'initForURL:withContentsOfURL:ofType:error:';
    procedure setFileType(newValue: NSString); message 'setFileType:';
    function fileType: NSString; message 'fileType';
    procedure setFileURL(newValue: NSURL); message 'setFileURL:';
    function fileURL: NSURL; message 'fileURL';
    procedure setFileModificationDate(newValue: NSDate); message 'setFileModificationDate:';
    function fileModificationDate: NSDate; message 'fileModificationDate';
    procedure setDraft(newValue: ObjCBOOL); message 'setDraft:';
    function isDraft: ObjCBOOL; message 'isDraft';
    procedure performActivityWithSynchronousWaiting_usingBlock (waitSynchronously: ObjCBOOL; block: OpaqueCBlock); message 'performActivityWithSynchronousWaiting:usingBlock:'; { available in 10_7 }
    procedure continueActivityUsingBlock (block: OpaqueCBlock); message 'continueActivityUsingBlock:'; { available in 10_7 }
    procedure continueAsynchronousWorkOnMainThreadUsingBlock (block: OpaqueCBlock); message 'continueAsynchronousWorkOnMainThreadUsingBlock:'; { available in 10_7 }
    procedure performSynchronousFileAccessUsingBlock (block: OpaqueCBlock); message 'performSynchronousFileAccessUsingBlock:'; { available in 10_7 }
    procedure performAsynchronousFileAccessUsingBlock (block: OpaqueCBlock); message 'performAsynchronousFileAccessUsingBlock:'; { available in 10_7 }
    procedure revertDocumentToSaved (sender: id); message 'revertDocumentToSaved:';
    function revertToContentsOfURL_ofType_error (url: NSURL; typeName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'revertToContentsOfURL:ofType:error:';
    function readFromURL_ofType_error (url: NSURL; typeName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'readFromURL:ofType:error:';
    function readFromFileWrapper_ofType_error (fileWrapper: NSFileWrapper; typeName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'readFromFileWrapper:ofType:error:';
    function readFromData_ofType_error (data: NSData; typeName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'readFromData:ofType:error:';
    function isEntireFileLoaded: ObjCBOOL; message 'isEntireFileLoaded';
    function writeToURL_ofType_error (url: NSURL; typeName: NSString; outError: NSErrorPtr): ObjCBOOL; message 'writeToURL:ofType:error:';
    function fileWrapperOfType_error (typeName: NSString; outError: NSErrorPtr): NSFileWrapper; message 'fileWrapperOfType:error:';
    function dataOfType_error (typeName: NSString; outError: NSErrorPtr): NSData; message 'dataOfType:error:';
    procedure unblockUserInteraction; message 'unblockUserInteraction'; { available in 10_7 }
    function autosavingIsImplicitlyCancellable: ObjCBOOL; message 'autosavingIsImplicitlyCancellable';
    function writeSafelyToURL_ofType_forSaveOperation_error (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; outError: NSErrorPtr): ObjCBOOL; message 'writeSafelyToURL:ofType:forSaveOperation:error:';
    function writeToURL_ofType_forSaveOperation_originalContentsURL_error (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; absoluteOriginalContentsURL: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'writeToURL:ofType:forSaveOperation:originalContentsURL:error:';
    function fileAttributesToWriteToURL_ofType_forSaveOperation_originalContentsURL_error (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; absoluteOriginalContentsURL: NSURL; outError: NSErrorPtr): NSDictionary; message 'fileAttributesToWriteToURL:ofType:forSaveOperation:originalContentsURL:error:';
    function keepBackupFile: ObjCBOOL; message 'keepBackupFile';
    function backupFileURL: NSURL; message 'backupFileURL';
    procedure saveDocument (sender: id); message 'saveDocument:';
    procedure saveDocumentAs (sender: id); message 'saveDocumentAs:';
    procedure saveDocumentTo (sender: id); message 'saveDocumentTo:';
    procedure saveDocumentWithDelegate_didSaveSelector_contextInfo (delegate: id; didSaveSelector: SEL; contextInfo: pointer); message 'saveDocumentWithDelegate:didSaveSelector:contextInfo:';
    procedure runModalSavePanelForSaveOperation_delegate_didSaveSelector_contextInfo (saveOperation: NSSaveOperationType; delegate: id; didSaveSelector: SEL; contextInfo: pointer); message 'runModalSavePanelForSaveOperation:delegate:didSaveSelector:contextInfo:';
    function shouldRunSavePanelWithAccessoryView: ObjCBOOL; message 'shouldRunSavePanelWithAccessoryView';
    function prepareSavePanel (savePanel: NSSavePanel): ObjCBOOL; message 'prepareSavePanel:';
    function fileNameExtensionWasHiddenInLastRunSavePanel: ObjCBOOL; message 'fileNameExtensionWasHiddenInLastRunSavePanel';
    function fileTypeFromLastRunSavePanel: NSString; message 'fileTypeFromLastRunSavePanel';
    procedure saveToURL_ofType_forSaveOperation_delegate_didSaveSelector_contextInfo (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; delegate: id; didSaveSelector: SEL; contextInfo: pointer); message 'saveToURL:ofType:forSaveOperation:delegate:didSaveSelector:contextInfo:';
    procedure saveToURL_ofType_forSaveOperation_completionHandler (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; completionHandler: OpaqueCBlock); message 'saveToURL:ofType:forSaveOperation:completionHandler:'; { available in 10_7 }
    function canAsynchronouslyWriteToURL_ofType_forSaveOperation (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType): ObjCBOOL; message 'canAsynchronouslyWriteToURL:ofType:forSaveOperation:'; { available in 10_7 }
    function checkAutosavingSafetyAndReturnError (outError: NSErrorPtr): ObjCBOOL; message 'checkAutosavingSafetyAndReturnError:'; { available in 10_7 }
    procedure scheduleAutosaving; message 'scheduleAutosaving'; { available in 10_7 }
    function hasUnautosavedChanges: ObjCBOOL; message 'hasUnautosavedChanges';
    procedure autosaveDocumentWithDelegate_didAutosaveSelector_contextInfo (delegate: id; didAutosaveSelector: SEL; contextInfo: pointer); message 'autosaveDocumentWithDelegate:didAutosaveSelector:contextInfo:';
    procedure autosaveWithImplicitCancellability_completionHandler (autosavingIsImplicitlyCancellable_: ObjCBOOL; completionHandler: OpaqueCBlock); message 'autosaveWithImplicitCancellability:completionHandler:'; { available in 10_7 }
    class function autosavesInPlace: ObjCBOOL; message 'autosavesInPlace'; { available in 10_7 }
    class function preservesVersions: ObjCBOOL; message 'preservesVersions'; { available in 10_7 }
    procedure browseDocumentVersions (sender: id); message 'browseDocumentVersions:'; { available in 10_8 }
    class function autosavesDrafts: ObjCBOOL; message 'autosavesDrafts'; { available in 10_8 }
    function autosavingFileType: NSString; message 'autosavingFileType';
    procedure setAutosavedContentsFileURL(newValue: NSURL); message 'setAutosavedContentsFileURL:';
    function autosavedContentsFileURL: NSURL; message 'autosavedContentsFileURL';
    procedure canCloseDocumentWithDelegate_shouldCloseSelector_contextInfo (delegate: id; shouldCloseSelector: SEL; contextInfo: pointer); message 'canCloseDocumentWithDelegate:shouldCloseSelector:contextInfo:';
    procedure close; message 'close';
    procedure duplicateDocument (sender: id); message 'duplicateDocument:'; { available in 10_7 }
    procedure duplicateDocumentWithDelegate_didDuplicateSelector_contextInfo (delegate: id; didDuplicateSelector: SEL; contextInfo: pointer); message 'duplicateDocumentWithDelegate:didDuplicateSelector:contextInfo:'; { available in 10_7 }
    function duplicateAndReturnError (outError: NSErrorPtr): NSDocument; message 'duplicateAndReturnError:'; { available in 10_7 }
    procedure renameDocument (sender: id); message 'renameDocument:'; { available in 10_8 }
    procedure moveDocumentToUbiquityContainer (sender: id); message 'moveDocumentToUbiquityContainer:'; { available in 10_8 }
    procedure moveDocument (sender: id); message 'moveDocument:'; { available in 10_8 }
    procedure moveDocumentWithCompletionHandler (completionHandler: OpaqueCBlock); message 'moveDocumentWithCompletionHandler:'; { available in 10_8 }
    procedure moveToURL_completionHandler (url: NSURL; completionHandler: OpaqueCBlock); message 'moveToURL:completionHandler:'; { available in 10_8 }
    procedure lockDocument (sender: id); message 'lockDocument:'; { available in 10_8 }
    procedure unlockDocument (sender: id); message 'unlockDocument:'; { available in 10_8 }
    procedure lockDocumentWithCompletionHandler (completionHandler: OpaqueCBlock); message 'lockDocumentWithCompletionHandler:'; { available in 10_8 }
    procedure lockWithCompletionHandler (completionHandler: OpaqueCBlock); message 'lockWithCompletionHandler:'; { available in 10_8 }
    procedure unlockDocumentWithCompletionHandler (completionHandler: OpaqueCBlock); message 'unlockDocumentWithCompletionHandler:'; { available in 10_8 }
    procedure unlockWithCompletionHandler (completionHandler: OpaqueCBlock); message 'unlockWithCompletionHandler:'; { available in 10_8 }
    function isLocked: ObjCBOOL; message 'isLocked';
    procedure runPageLayout (sender: id); message 'runPageLayout:';
    procedure runModalPageLayoutWithPrintInfo_delegate_didRunSelector_contextInfo (printInfo: NSPrintInfo; delegate: id; didRunSelector: SEL; contextInfo: pointer); message 'runModalPageLayoutWithPrintInfo:delegate:didRunSelector:contextInfo:';
    function preparePageLayout (pageLayout: NSPageLayout): ObjCBOOL; message 'preparePageLayout:';
    function shouldChangePrintInfo (newPrintInfo: NSPrintInfo): ObjCBOOL; message 'shouldChangePrintInfo:';
    procedure setPrintInfo(newValue: NSPrintInfo); message 'setPrintInfo:';
    function printInfo: NSPrintInfo; message 'printInfo';
    procedure printDocument (sender: id); message 'printDocument:';
    procedure printDocumentWithSettings_showPrintPanel_delegate_didPrintSelector_contextInfo (printSettings: NSDictionary; showPrintPanel: ObjCBOOL; delegate: id; didPrintSelector: SEL; contextInfo: pointer); message 'printDocumentWithSettings:showPrintPanel:delegate:didPrintSelector:contextInfo:';
    function printOperationWithSettings_error (printSettings: NSDictionary; outError: NSErrorPtr): NSPrintOperation; message 'printOperationWithSettings:error:';
    procedure runModalPrintOperation_delegate_didRunSelector_contextInfo (printOperation: NSPrintOperation; delegate: id; didRunSelector: SEL; contextInfo: pointer); message 'runModalPrintOperation:delegate:didRunSelector:contextInfo:';
    procedure saveDocumentToPDF (sender: id); message 'saveDocumentToPDF:'; { available in 10_9 }
    function PDFPrintOperation: NSPrintOperation; message 'PDFPrintOperation';
    function isDocumentEdited: ObjCBOOL; message 'isDocumentEdited';
    function isInViewingMode: ObjCBOOL; message 'isInViewingMode';
    procedure updateChangeCount (change: NSDocumentChangeType); message 'updateChangeCount:';
    function changeCountTokenForSaveOperation (saveOperation: NSSaveOperationType): id; message 'changeCountTokenForSaveOperation:'; { available in 10_7 }
    procedure updateChangeCountWithToken_forSaveOperation (changeCountToken: id; saveOperation: NSSaveOperationType); message 'updateChangeCountWithToken:forSaveOperation:'; { available in 10_7 }
    procedure setUndoManager(newValue: NSUndoManager); message 'setUndoManager:';
    function undoManager: NSUndoManager; message 'undoManager';
    procedure setHasUndoManager(newValue: ObjCBOOL); message 'setHasUndoManager:';
    function hasUndoManager: ObjCBOOL; message 'hasUndoManager';
    procedure presentError_modalForWindow_delegate_didPresentSelector_contextInfo (error: NSError; window: NSWindow; delegate: id; didPresentSelector: SEL; contextInfo: pointer); message 'presentError:modalForWindow:delegate:didPresentSelector:contextInfo:';
    function presentError (error: NSError): ObjCBOOL; message 'presentError:';
    function willPresentError (error: NSError): NSError; message 'willPresentError:';
    procedure willNotPresentError (error: NSError); message 'willNotPresentError:'; { available in 10_7 }
    procedure makeWindowControllers; message 'makeWindowControllers';
    function windowNibName: NSString; message 'windowNibName';
    procedure windowControllerWillLoadNib (windowController: NSWindowController); message 'windowControllerWillLoadNib:';
    procedure windowControllerDidLoadNib (windowController: NSWindowController); message 'windowControllerDidLoadNib:';
    procedure setWindow (window: NSWindow); message 'setWindow:';
    procedure addWindowController (windowController: NSWindowController); message 'addWindowController:';
    procedure removeWindowController (windowController: NSWindowController); message 'removeWindowController:';
    procedure showWindows; message 'showWindows';
    function windowControllers: NSArray; message 'windowControllers';
    procedure shouldCloseWindowController_delegate_shouldCloseSelector_contextInfo (windowController: NSWindowController; delegate: id; shouldCloseSelector: SEL; contextInfo: pointer); message 'shouldCloseWindowController:delegate:shouldCloseSelector:contextInfo:';
    procedure setDisplayName (displayNameOrNil: NSString); message 'setDisplayName:'; { available in 10_7 }
    function displayName: NSString; message 'displayName';
    function defaultDraftName: NSString; message 'defaultDraftName'; { available in 10_8 }
    function windowForSheet: NSWindow; message 'windowForSheet';
    class function readableTypes: NSArray; message 'readableTypes';
    class function writableTypes: NSArray; message 'writableTypes';
    class function isNativeType (type_: NSString): ObjCBOOL; message 'isNativeType:';
    function writableTypesForSaveOperation (saveOperation: NSSaveOperationType): NSArray; message 'writableTypesForSaveOperation:';
    function fileNameExtensionForType_saveOperation (typeName: NSString; saveOperation: NSSaveOperationType): NSString; message 'fileNameExtensionForType:saveOperation:'; { available in 10_5 }
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
    class function usesUbiquitousStorage: ObjCBOOL; message 'usesUbiquitousStorage'; { available in 10_8 }

    { Adopted protocols }
    procedure accommodatePresentedItemDeletionWithCompletionHandler (completionHandler: OpaqueCBlock); message 'accommodatePresentedItemDeletionWithCompletionHandler:';
    procedure accommodatePresentedSubitemDeletionAtURL_completionHandler (url: NSURL; completionHandler: OpaqueCBlock); message 'accommodatePresentedSubitemDeletionAtURL:completionHandler:';
    procedure presentedItemDidChange; message 'presentedItemDidChange';
    procedure presentedItemDidGainVersion (version: NSFileVersion); message 'presentedItemDidGainVersion:';
    procedure presentedItemDidLoseVersion (version: NSFileVersion); message 'presentedItemDidLoseVersion:';
    procedure presentedItemDidMoveToURL (newURL: NSURL); message 'presentedItemDidMoveToURL:';
    procedure presentedItemDidResolveConflictVersion (version: NSFileVersion); message 'presentedItemDidResolveConflictVersion:';
    function presentedItemOperationQueue: NSOperationQueue; message 'presentedItemOperationQueue';
    function presentedItemURL: NSURL; message 'presentedItemURL';
    procedure presentedSubitemAtURL_didGainVersion (url: NSURL; version: NSFileVersion); message 'presentedSubitemAtURL:didGainVersion:';
    procedure presentedSubitemAtURL_didLoseVersion (url: NSURL; version: NSFileVersion); message 'presentedSubitemAtURL:didLoseVersion:';
    procedure presentedSubitemAtURL_didMoveToURL (oldURL: NSURL; newURL: NSURL); message 'presentedSubitemAtURL:didMoveToURL:';
    procedure presentedSubitemAtURL_didResolveConflictVersion (url: NSURL; version: NSFileVersion); message 'presentedSubitemAtURL:didResolveConflictVersion:';
    procedure presentedSubitemDidAppearAtURL (url: NSURL); message 'presentedSubitemDidAppearAtURL:';
    procedure presentedSubitemDidChangeAtURL (url: NSURL); message 'presentedSubitemDidChangeAtURL:';
    function primaryPresentedItemURL: NSURL; message 'primaryPresentedItemURL';
    procedure relinquishPresentedItemToReader (reader: OpaqueCBlock); message 'relinquishPresentedItemToReader:';
    procedure relinquishPresentedItemToWriter (writer: OpaqueCBlock); message 'relinquishPresentedItemToWriter:';
    procedure savePresentedItemChangesWithCompletionHandler (completionHandler: OpaqueCBlock); message 'savePresentedItemChangesWithCompletionHandler:';
  end;


type
  NSDocument_NSDeprecated = objccategory external name 'NSDeprecated' (NSDocument)
    function saveToURL_ofType_forSaveOperation_error (url: NSURL; typeName: NSString; saveOperation: NSSaveOperationType; outError: NSErrorPtr): ObjCBOOL; message 'saveToURL:ofType:forSaveOperation:error:'; deprecated 'in 10_4, 10_6, "Use -saveToURL:ofType:forSaveOperation:completionHandler: instead"';
    function dataRepresentationOfType (type_: NSString): NSData; message 'dataRepresentationOfType:'; deprecated 'in 10_0, 10_4';
    function fileAttributesToWriteToFile_ofType_saveOperation (fullDocumentPath: NSString; documentTypeName: NSString; saveOperationType: NSSaveOperationType): NSDictionary; message 'fileAttributesToWriteToFile:ofType:saveOperation:'; deprecated 'in 10_1, 10_4';
    function fileName: NSString; message 'fileName'; deprecated 'in 10_0, 10_4';
    function fileWrapperRepresentationOfType (type_: NSString): NSFileWrapper; message 'fileWrapperRepresentationOfType:'; deprecated 'in 10_0, 10_4';
    function initWithContentsOfFile_ofType (absolutePath: NSString; typeName: NSString): id; message 'initWithContentsOfFile:ofType:'; deprecated 'in 10_0, 10_4';
    function initWithContentsOfURL_ofType (url: NSURL; typeName: NSString): id; message 'initWithContentsOfURL:ofType:'; deprecated 'in 10_0, 10_4';
    function loadDataRepresentation_ofType (data: NSData; type_: NSString): ObjCBOOL; message 'loadDataRepresentation:ofType:'; deprecated 'in 10_0, 10_4';
    function loadFileWrapperRepresentation_ofType (wrapper: NSFileWrapper; type_: NSString): ObjCBOOL; message 'loadFileWrapperRepresentation:ofType:'; deprecated 'in 10_0, 10_4';
    procedure printShowingPrintPanel (flag: ObjCBOOL); message 'printShowingPrintPanel:'; deprecated 'in 10_0, 10_4';
    function readFromFile_ofType (fileName_: NSString; type_: NSString): ObjCBOOL; message 'readFromFile:ofType:'; deprecated 'in 10_0, 10_4';
    function readFromURL_ofType (url: NSURL; type_: NSString): ObjCBOOL; message 'readFromURL:ofType:'; deprecated 'in 10_0, 10_4';
    function revertToSavedFromFile_ofType (fileName_: NSString; type_: NSString): ObjCBOOL; message 'revertToSavedFromFile:ofType:'; deprecated 'in 10_0, 10_4';
    function revertToSavedFromURL_ofType (url: NSURL; type_: NSString): ObjCBOOL; message 'revertToSavedFromURL:ofType:'; deprecated 'in 10_0, 10_4';
    function runModalPageLayoutWithPrintInfo (printInfo_: NSPrintInfo): NSInteger; message 'runModalPageLayoutWithPrintInfo:'; deprecated 'in 10_0, 10_4';
    procedure saveToFile_saveOperation_delegate_didSaveSelector_contextInfo (fileName_: NSString; saveOperation: NSSaveOperationType; delegate: id; didSaveSelector: SEL; contextInfo: pointer); message 'saveToFile:saveOperation:delegate:didSaveSelector:contextInfo:'; deprecated 'in 10_0, 10_4';
    procedure setFileName (fileName_: NSString); message 'setFileName:'; deprecated 'in 10_0, 10_4';
    function writeToFile_ofType (fileName_: NSString; type_: NSString): ObjCBOOL; message 'writeToFile:ofType:'; deprecated 'in 10_0, 10_4';
    function writeToFile_ofType_originalFile_saveOperation (fullDocumentPath: NSString; documentTypeName: NSString; fullOriginalDocumentPath: NSString; saveOperationType: NSSaveOperationType): ObjCBOOL; message 'writeToFile:ofType:originalFile:saveOperation:'; deprecated 'in 10_0, 10_4';
    function writeToURL_ofType (url: NSURL; type_: NSString): ObjCBOOL; message 'writeToURL:ofType:'; deprecated 'in 10_0, 10_4';
    function writeWithBackupToFile_ofType_saveOperation (fullDocumentPath: NSString; documentTypeName: NSString; saveOperationType: NSSaveOperationType): ObjCBOOL; message 'writeWithBackupToFile:ofType:saveOperation:'; deprecated 'in 10_0, 10_4';
  end;
{$endif}

