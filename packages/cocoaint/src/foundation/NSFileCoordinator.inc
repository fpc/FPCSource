{ Parsed from Foundation.framework NSFileCoordinator.h }


{$ifdef TYPES}
type
  NSFileAccessIntentPtr = ^NSFileAccessIntent;
  NSFileCoordinatorPtr = ^NSFileCoordinator;
{$endif}

{$ifdef TYPES}
type
  NSFileCoordinatorReadingOptions = NSUInteger;
  NSFileCoordinatorReadingOptionsPtr = ^NSFileCoordinatorReadingOptions;

const
  NSFileCoordinatorReadingWithoutChanges = 1 shl 0;
  NSFileCoordinatorReadingResolvesSymbolicLink = 1 shl 1;
  NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly = 1 shl 2 { available in 10_10, 8_0 };
  NSFileCoordinatorReadingForUploading = 1 shl 3 { available in 10_10, 8_0 };

type
  NSFileCoordinatorWritingOptions = NSUInteger;
  NSFileCoordinatorWritingOptionsPtr = ^NSFileCoordinatorWritingOptions;

const
  NSFileCoordinatorWritingForDeleting = 1 shl 0;
  NSFileCoordinatorWritingForMoving = 1 shl 1;
  NSFileCoordinatorWritingForMerging = 1 shl 2;
  NSFileCoordinatorWritingForReplacing = 1 shl 3;
  NSFileCoordinatorWritingContentIndependentMetadataOnly = 1 shl 4 { available in 10_10, 8_0 };
{$endif}

{$ifdef CLASSES}

type
  NSFileAccessIntent = objcclass external (NSObject)
  private
    _url: NSURL;
    _isRead: ObjCBOOL;
    _options: NSInteger;
  public
    class function readingIntentWithURL_options (url: NSURL; options: NSFileCoordinatorReadingOptions): instancetype; message 'readingIntentWithURL:options:';
    class function writingIntentWithURL_options (url: NSURL; options: NSFileCoordinatorWritingOptions): instancetype; message 'writingIntentWithURL:options:';
    function URL: NSURL; message 'URL';
  end;

type
  NSFileCoordinator = objcclass external (NSObject)
  private
    _accessArbiter: id;
    _fileReactor: id;
    _purposeID: id;
    _recentFilePresenterURL: NSURL;
    _accessClaimIDOrIDs: id;
    _isCancelled: ObjCBOOL;
    _movedItems: NSMutableDictionary;
  public
    class procedure addFilePresenter (filePresenter: NSFilePresenterProtocol); message 'addFilePresenter:';
    class procedure removeFilePresenter (filePresenter: NSFilePresenterProtocol); message 'removeFilePresenter:';
    class function filePresenters: NSArray; message 'filePresenters';
    function initWithFilePresenter (filePresenterOrNil: NSFilePresenterProtocol): instancetype; message 'initWithFilePresenter:'; { NS_DESIGNATED_INITIALIZER }
    procedure setPurposeIdentifier(newValue: NSString); message 'setPurposeIdentifier:';
    function purposeIdentifier: NSString; message 'purposeIdentifier';
    procedure coordinateAccessWithIntents_queue_byAccessor (intents: NSArray; queue: NSOperationQueue; accessor: OpaqueCBlock); message 'coordinateAccessWithIntents:queue:byAccessor:'; { available in 10_10, 8_0 }
    procedure coordinateReadingItemAtURL_options_error_byAccessor (url: NSURL; options: NSFileCoordinatorReadingOptions; outError: NSErrorPtr; reader: OpaqueCBlock); message 'coordinateReadingItemAtURL:options:error:byAccessor:';
    procedure coordinateWritingItemAtURL_options_error_byAccessor (url: NSURL; options: NSFileCoordinatorWritingOptions; outError: NSErrorPtr; writer: OpaqueCBlock); message 'coordinateWritingItemAtURL:options:error:byAccessor:';
    procedure coordinateReadingItemAtURL_options_writingItemAtURL_options_error_byAccessor (readingURL: NSURL; readingOptions: NSFileCoordinatorReadingOptions; writingURL: NSURL; writingOptions: NSFileCoordinatorWritingOptions; outError: NSErrorPtr; readerWriter: OpaqueCBlock); message 'coordinateReadingItemAtURL:options:writingItemAtURL:options:error:byAccessor:';
    procedure coordinateWritingItemAtURL_options_writingItemAtURL_options_error_byAccessor (url1: NSURL; options1: NSFileCoordinatorWritingOptions; url2: NSURL; options2: NSFileCoordinatorWritingOptions; outError: NSErrorPtr; writer: OpaqueCBlock); message 'coordinateWritingItemAtURL:options:writingItemAtURL:options:error:byAccessor:';
    procedure prepareForReadingItemsAtURLs_options_writingItemsAtURLs_options_error_byAccessor (readingURLs: NSArray; readingOptions: NSFileCoordinatorReadingOptions; writingURLs: NSArray; writingOptions: NSFileCoordinatorWritingOptions; outError: NSErrorPtr; batchAccessor: OpaqueCBlock); message 'prepareForReadingItemsAtURLs:options:writingItemsAtURLs:options:error:byAccessor:';
    procedure itemAtURL_willMoveToURL (oldURL: NSURL; newURL: NSURL); message 'itemAtURL:willMoveToURL:'; { available in 10_8, 6_0 }
    procedure itemAtURL_didMoveToURL (oldURL: NSURL; newURL: NSURL); message 'itemAtURL:didMoveToURL:';
    procedure cancel; message 'cancel';
  end;
{$endif}

