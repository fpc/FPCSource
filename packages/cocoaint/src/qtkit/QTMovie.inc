{ Parsed from QTKit.framework QTMovie.h }


{ Types from QTMovie_Initialization }
{$ifdef TYPES}




{$endif}


{ Types from QTMovie_Image }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_Format }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_Time }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_Editing }
{$ifdef TYPES}


{$endif}


{ Types from QTMovie_VisualSupport }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_VisualContext }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_Threading }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_Chapters }
{$ifdef TYPES}

{$endif}


{ Types from QTMovie_MetadataReading }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  QTMoviePtr = ^QTMovie;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  QTMovieActiveSegmentAttribute: NSString deprecated 'available in 7.0 and later but deprecated in 7.2'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  QTIncludeStillImageTypes = 1 shl 0;
  QTIncludeTranslatableTypes = 1 shl 1;
  QTIncludeAggressiveTypes = 1 shl 2;
  {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
  QTIncludeDynamicTypes = 1 shl 3;
  {$endif}
  QTIncludeCommonTypes = 0;
  QTIncludeAllTypes = $ffff;
type
  QTMovieFileTypeOptions = clong;
const
  {$if defined(__LP64__)}
  QTMovieOperationBeginPhase = 0;
  QTMovieOperationUpdatePercentPhase = 1;
  QTMovieOperationEndPhase = 2;
  {$else}
  QTMovieOperationBeginPhase = movieProgressOpen;
  QTMovieOperationUpdatePercentPhase = movieProgressUpdatePercent;
  QTMovieOperationEndPhase = movieProgressClose;
  {$endif}
type
  QTMovieOperationPhase = clong;
{$endif}

{$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
{$ifdef TYPES}
const
  QTMovieLoadStateError = -1;
  QTMovieLoadStateLoading = 1000;
  QTMovieLoadStateLoaded = 2000;
  QTMovieLoadStatePlayable = 10000;
  QTMovieLoadStatePlaythroughOK = 20000;
  QTMovieLoadStateComplete = 100000;

type
  QTMovieLoadState = NSInteger;
  QTMovieLoadStatePtr = ^QTMovieLoadState;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  NSObject_QTMovie_Delegate = objccategory external name 'QTMovie_Delegate' (NSObject)
    function movie_linkToURL (movie: QTMovie; url: NSURL): ObjCBOOL; message 'movie:linkToURL:';
    function movieShouldLoadData (sender: id): ObjCBOOL; message 'movieShouldLoadData:';
    function movieShouldTask (movie: id): ObjCBOOL; message 'movieShouldTask:';
    function externalMovie (dictionary: NSDictionary): QTMovie; message 'externalMovie:';
    function movie_shouldContinueOperation_withPhase_atPercent_withAttributes (movie: QTMovie; op: NSString; phase: QTMovieOperationPhase; percent: NSNumber; attributes: NSDictionary): ObjCBOOL; message 'movie:shouldContinueOperation:withPhase:atPercent:withAttributes:';
  end;


type
  QTMovie = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    {$if  not defined(__LP64__)}
    _movie: Movie;
    {$endif}
    _state: clong;
    _mediaHelper: QTMovieMediaHelper;
    _initedViaInitWithAttributes: CFBooleanRef;
    _children: QTInvalidationSet;
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
    _reserved4: clong;
    _reserved5: clong;
    _reserved6: clong;
    _reserved7: clong;
    _reserved8: clong;
    _reserved9: clong;
    _reserved10: clong;
    _reserved11: clong;
    _reserved12: clong;
    _reserved13: clong;
    _reserved14: clong;
    _reserved15: clong;
  public
    function duration: QTTime; message 'duration';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;


type
  QTMovie_Initialization = objccategory external (QTMovie)
    class function canInitWithPasteboard (pasteboard: NSPasteboard): ObjCBOOL; message 'canInitWithPasteboard:';
    class function canInitWithFile (fileName: NSString): ObjCBOOL; message 'canInitWithFile:';
    class function canInitWithURL (url: NSURL): ObjCBOOL; message 'canInitWithURL:';
    class function canInitWithDataReference (dataReference: QTDataReference): ObjCBOOL; message 'canInitWithDataReference:';
    class function movieFileTypes (types: QTMovieFileTypeOptions): NSArray; message 'movieFileTypes:';
    class function movieUnfilteredFileTypes: NSArray; message 'movieUnfilteredFileTypes';
    class function movieUnfilteredPasteboardTypes: NSArray; message 'movieUnfilteredPasteboardTypes';
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    class function movieTypesWithOptions (types: QTMovieFileTypeOptions): NSArray; message 'movieTypesWithOptions:';
    {$endif}
    class function movie: id; message 'movie';
    class function movieWithFile_error (fileName: NSString; errorPtr: NSErrorPtr): id; message 'movieWithFile:error:';
    class function movieWithURL_error (url: NSURL; errorPtr: NSErrorPtr): id; message 'movieWithURL:error:';
    class function movieWithDataReference_error (dataReference: QTDataReference; errorPtr: NSErrorPtr): id; message 'movieWithDataReference:error:';
    class function movieWithPasteboard_error (pasteboard: NSPasteboard; errorPtr: NSErrorPtr): id; message 'movieWithPasteboard:error:';
    class function movieWithData_error (data: NSData; errorPtr: NSErrorPtr): id; message 'movieWithData:error:';
    {$if  not defined(__LP64__)}
    class function movieWithQuickTimeMovie_disposeWhenDone_error (movie_: Movie; dispose_: ObjCBOOL; errorPtr: NSErrorPtr): id; message 'movieWithQuickTimeMovie:disposeWhenDone:error:';
    {$endif}
    class function movieWithAttributes_error (attributes: NSDictionary; errorPtr: NSErrorPtr): id; message 'movieWithAttributes:error:';
    class function movieNamed_error (name: NSString; errorPtr: NSErrorPtr): id; message 'movieNamed:error:';
    function initWithFile_error (fileName: NSString; errorPtr: NSErrorPtr): id; message 'initWithFile:error:';
    function initWithURL_error (url: NSURL; errorPtr: NSErrorPtr): id; message 'initWithURL:error:';
    function initWithDataReference_error (dataReference: QTDataReference; errorPtr: NSErrorPtr): id; message 'initWithDataReference:error:';
    function initWithPasteboard_error (pasteboard: NSPasteboard; errorPtr: NSErrorPtr): id; message 'initWithPasteboard:error:';
    function initWithData_error (data: NSData; errorPtr: NSErrorPtr): id; message 'initWithData:error:';
    function initWithMovie_timeRange_error (movie: QTMovie; range: QTTimeRange; errorPtr: NSErrorPtr): id; message 'initWithMovie:timeRange:error:';
    {$if  not defined(__LP64__)}
    function initWithQuickTimeMovie_disposeWhenDone_error (movie_: Movie; dispose_: ObjCBOOL; errorPtr: NSErrorPtr): id; message 'initWithQuickTimeMovie:disposeWhenDone:error:';
    {$endif}
    function initWithAttributes_error (attributes: NSDictionary; errorPtr: NSErrorPtr): id; message 'initWithAttributes:error:';
    function movieWithTimeRange_error (range: QTTimeRange; errorPtr: NSErrorPtr): id; message 'movieWithTimeRange:error:';
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    function initToWritableFile_error (filename: NSString; errorPtr: NSErrorPtr): id; message 'initToWritableFile:error:';
    function initToWritableData_error (data: NSMutableData; errorPtr: NSErrorPtr): id; message 'initToWritableData:error:';
    function initToWritableDataReference_error (dataReference: QTDataReference; errorPtr: NSErrorPtr): id; message 'initToWritableDataReference:error:';
    procedure invalidate; message 'invalidate';
    {$endif}
  end;

type
  QTMovie_Inspection = objccategory external (QTMovie)
    function movieAttributes: NSDictionary; message 'movieAttributes';
    procedure setMovieAttributes (attributes: NSDictionary); message 'setMovieAttributes:';
    function attributeForKey (attributeKey: NSString): id; message 'attributeForKey:';
    procedure setAttribute_forKey (value: id; attributeKey: NSString); message 'setAttribute:forKey:';
    function tracks: NSArray; message 'tracks';
    function tracksOfMediaType (type_: NSString): NSArray; message 'tracksOfMediaType:';
  end;

type
  QTMovie_Image = objccategory external (QTMovie)
    function posterImage: NSImage; message 'posterImage';
    function currentFrameImage: NSImage; message 'currentFrameImage';
    function frameImageAtTime (time: QTTime): NSImage; message 'frameImageAtTime:';
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    function frameImageAtTime_withAttributes_error (time: QTTime; attributes: NSDictionary; errorPtr: NSErrorPtr): pointer; message 'frameImageAtTime:withAttributes:error:';
    {$endif}
  end;

type
  QTMovie_Format = objccategory external (QTMovie)
    function movieFormatRepresentation: NSData; message 'movieFormatRepresentation';
    function writeToFile_withAttributes (fileName: NSString; attributes: NSDictionary): ObjCBOOL; message 'writeToFile:withAttributes:';
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    function writeToFile_withAttributes_error (fileName: NSString; attributes: NSDictionary; errorPtr: NSErrorPtr): ObjCBOOL; message 'writeToFile:withAttributes:error:';
    {$endif}
    function canUpdateMovieFile: ObjCBOOL; message 'canUpdateMovieFile';
    function updateMovieFile: ObjCBOOL; message 'updateMovieFile';
  end;

type
  QTMovie_Time = objccategory external (QTMovie)
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    procedure autoplay; message 'autoplay';
    {$endif}
    procedure play; message 'play';
    procedure stop; message 'stop';
    function currentTime: QTTime; message 'currentTime';
    procedure setCurrentTime (time: QTTime); message 'setCurrentTime:';
    procedure gotoBeginning; message 'gotoBeginning';
    procedure gotoEnd; message 'gotoEnd';
    procedure gotoNextSelectionPoint; message 'gotoNextSelectionPoint';
    procedure gotoPreviousSelectionPoint; message 'gotoPreviousSelectionPoint';
    procedure gotoPosterTime; message 'gotoPosterTime';
    procedure stepForward; message 'stepForward';
    procedure stepBackward; message 'stepBackward';
  end;

type
  QTMovie_PlaybackControl = objccategory external (QTMovie)
    function rate: single; message 'rate';
    procedure setRate (rate: single); message 'setRate:';
    function volume: single; message 'volume';
    procedure setVolume (volume: single); message 'setVolume:';
    function muted: ObjCBOOL; message 'muted';
    procedure setMuted (mute: ObjCBOOL); message 'setMuted:';
  end;

type
  QTMovie_Selection = objccategory external (QTMovie)
    procedure setSelection (selection: QTTimeRange); message 'setSelection:';
    function selectionStart: QTTime; message 'selectionStart';
    function selectionEnd: QTTime; message 'selectionEnd';
    function selectionDuration: QTTime; message 'selectionDuration';
  end;

type
  QTMovie_Editing = objccategory external (QTMovie)
    procedure replaceSelectionWithSelectionFromMovie (movie: id); message 'replaceSelectionWithSelectionFromMovie:';
    procedure appendSelectionFromMovie (movie: id); message 'appendSelectionFromMovie:';
    procedure insertSegmentOfMovie_timeRange_atTime (movie: QTMovie; range: QTTimeRange; time: QTTime); message 'insertSegmentOfMovie:timeRange:atTime:';
    procedure insertSegmentOfMovie_fromRange_scaledToRange (movie: QTMovie; srcRange: QTTimeRange; dstRange: QTTimeRange); message 'insertSegmentOfMovie:fromRange:scaledToRange:';
    procedure insertEmptySegmentAt (range: QTTimeRange); message 'insertEmptySegmentAt:';
    procedure deleteSegment (segment: QTTimeRange); message 'deleteSegment:';
    procedure scaleSegment_newDuration (segment: QTTimeRange; newDuration: QTTime); message 'scaleSegment:newDuration:';
    procedure addImage_forDuration_withAttributes (image: NSImage; duration: QTTime; attributes: NSDictionary); message 'addImage:forDuration:withAttributes:';
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    function insertSegmentOfTrack_timeRange_atTime (track: QTTrack; range: QTTimeRange; time: QTTime): QTTrack; message 'insertSegmentOfTrack:timeRange:atTime:';
    function insertSegmentOfTrack_fromRange_scaledToRange (track: QTTrack; srcRange: QTTimeRange; dstRange: QTTimeRange): QTTrack; message 'insertSegmentOfTrack:fromRange:scaledToRange:';
    procedure removeTrack (track: QTTrack); message 'removeTrack:';
    {$endif}
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_7)}
    function trackByInsertingSegmentOfTrack_timeRange_atTime (track: QTTrack; range: QTTimeRange; time: QTTime): QTTrack; message 'trackByInsertingSegmentOfTrack:timeRange:atTime:';
    function trackByInsertingSegmentOfTrack_fromRange_scaledToRange (track: QTTrack; srcRange: QTTimeRange; dstRange: QTTimeRange): QTTrack; message 'trackByInsertingSegmentOfTrack:fromRange:scaledToRange:';
    {$endif}
  end;

type
  QTMovie_Delegate = objccategory external (QTMovie)
    function delegate: id; message 'delegate';
    procedure setDelegate (delegate: id); message 'setDelegate:';
  end;
{$endif}

{$if  not defined(__LP64__)}
{$ifdef CLASSES}

type
  QTMovie_Primitives = objccategory external (QTMovie)
    function quickTimeMovie: Movie; message 'quickTimeMovie';
    function quickTimeMovieController: MovieController; message 'quickTimeMovieController';
  end;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  QTMovie_VisualSupport = objccategory external (QTMovie)
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    procedure generateApertureModeDimensions; message 'generateApertureModeDimensions';
    procedure removeApertureModeDimensions; message 'removeApertureModeDimensions';
    {$endif}
  end;

type
  QTMovie_VisualContext = objccategory external (QTMovie)
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    {$if  not defined(__LP64__)}
    procedure setVisualContext (visualContext: QTVisualContextRef); message 'setVisualContext:';
    function visualContext: QTVisualContextRef; message 'visualContext';
    {$endif}
    {$endif}
  end;

type
  QTMovie_Threading = objccategory external (QTMovie)
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    class procedure enterQTKitOnThread; message 'enterQTKitOnThread';
    class procedure enterQTKitOnThreadDisablingThreadSafetyProtection; message 'enterQTKitOnThreadDisablingThreadSafetyProtection';
    class procedure exitQTKitOnThread; message 'exitQTKitOnThread';
    function attachToCurrentThread: ObjCBOOL; message 'attachToCurrentThread';
    function detachFromCurrentThread: ObjCBOOL; message 'detachFromCurrentThread';
    procedure setIdling (state: ObjCBOOL); message 'setIdling:';
    function isIdling: ObjCBOOL; message 'isIdling';
    {$endif}
  end;

type
  QTMovie_Chapters = objccategory external (QTMovie)
    {$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
    function hasChapters: ObjCBOOL; message 'hasChapters';
    function chapterCount: NSInteger; message 'chapterCount';
    function chapters: NSArray; message 'chapters';
    procedure addChapters_withAttributes_error (chapters: NSArray; attributes: NSDictionary; errorPtr: NSErrorPtr); message 'addChapters:withAttributes:error:';
    function removeChapters: ObjCBOOL; message 'removeChapters';
    function startTimeOfChapter (chapterIndex: NSInteger): QTTime; message 'startTimeOfChapter:';
    function chapterIndexForTime (time: QTTime): NSInteger; message 'chapterIndexForTime:';
    {$endif}
  end;

type
  QTMovie_MetadataReading = objccategory external (QTMovie)
    {$if ((defined(MAC_OS_X_VERSION_10_7)) and ((MAC_OS_X_VERSION_MIN_REQUIRED >= MAC_OS_X_VERSION_10_7)))}
    function commonMetadata: NSArray; message 'commonMetadata'; deprecated 'available in 10.7 and later but deprecated in 10.9';
    function availableMetadataFormats: NSArray; message 'availableMetadataFormats'; deprecated 'available in 10.7 and later but deprecated in 10.9';
    function metadataForFormat (format: NSString): NSArray; message 'metadataForFormat:'; deprecated 'available in 10.7 and later but deprecated in 10.9';
    {$endif}
  end;
{$endif}

