{ Parsed from AVFoundation.framework AVSampleCursor.h }


{$ifdef TYPES}
type
  AVSampleCursorPtr = ^AVSampleCursor;
{$endif}

{$ifdef TYPES}
type
  AVSampleCursorDependencyInfo = record
    sampleIndicatesWhetherItHasDependentSamples: ObjCBOOL;
    sampleHasDependentSamples: ObjCBOOL;
    sampleIndicatesWhetherItDependsOnOthers: ObjCBOOL;
    sampleDependsOnOthers: ObjCBOOL;
    sampleIndicatesWhetherItHasRedundantCoding: ObjCBOOL;
    sampleHasRedundantCoding: ObjCBOOL;
  end;
type
  AVSampleCursorDependencyInfoPtr = ^AVSampleCursorDependencyInfo;
type
  AVSampleCursorStorageRange = record
    offset: cint64;
    length: cint64;
  end;
type
  AVSampleCursorStorageRangePtr = ^AVSampleCursorStorageRange;
type
  AVSampleCursorSyncInfo = record
    sampleIsFullSync: ObjCBOOL;
    sampleIsPartialSync: ObjCBOOL;
    sampleIsDroppable: ObjCBOOL;
  end;
type
  AVSampleCursorSyncInfoPtr = ^AVSampleCursorSyncInfo;
type
  AVSampleCursorChunkInfo = record
    chunkSampleCount: cint64;
    chunkHasUniformSampleSizes: ObjCBOOL;
    chunkHasUniformSampleDurations: ObjCBOOL;
    chunkHasUniformFormatDescriptions: ObjCBOOL;
  end;
type
  AVSampleCursorChunkInfoPtr = ^AVSampleCursorChunkInfo;
{$endif}

{$if  not defined(TARGET_OS_IPHONE)}
{$ifdef CLASSES}

type
  AVSampleCursor = objcclass external (NSObject, NSCopyingProtocol)
  private
    _sampleCursor: AVSampleCursorInternal;
  public
    function stepInDecodeOrderByCount (stepCount: cint64): cint64; message 'stepInDecodeOrderByCount:';
    function stepInPresentationOrderByCount (stepCount: cint64): cint64; message 'stepInPresentationOrderByCount:';
    function stepByDecodeTime_wasPinned (deltaDecodeTime: CMTime; outWasPinned: pObjCBOOL): CMTime; message 'stepByDecodeTime:wasPinned:';
    function stepByPresentationTime_wasPinned (deltaPresentationTime: CMTime; outWasPinned: pObjCBOOL): CMTime; message 'stepByPresentationTime:wasPinned:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;


type
  AVSampleCursorTemporalPosition = objccategory external (AVSampleCursor)
    function presentationTimeStamp: CMTime; message 'presentationTimeStamp';
    function decodeTimeStamp: CMTime; message 'decodeTimeStamp';
    function comparePositionInDecodeOrderWithPositionOfCursor (cursor: AVSampleCursor): NSComparisonResult; message 'comparePositionInDecodeOrderWithPositionOfCursor:';
    function samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor (cursor: AVSampleCursor): ObjCBOOL; message 'samplesWithEarlierDecodeTimeStampsMayHaveLaterPresentationTimeStampsThanCursor:';
    function samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor (cursor: AVSampleCursor): ObjCBOOL; message 'samplesWithLaterDecodeTimeStampsMayHaveEarlierPresentationTimeStampsThanCursor:';
  end;

type
  AVSampleCursorCurrentSampleInfo = objccategory external (AVSampleCursor)
    function currentSampleDuration: CMTime; message 'currentSampleDuration';
    function copyCurrentSampleFormatDescription: CMFormatDescriptionRef; message 'copyCurrentSampleFormatDescription'; { CF_RETURNS_RETAINED }
    function currentSampleSyncInfo: AVSampleCursorSyncInfo; message 'currentSampleSyncInfo';
    function currentSampleDependencyInfo: AVSampleCursorDependencyInfo; message 'currentSampleDependencyInfo';
  end;

type
  AVSampleCursorSampleStorageInfo = objccategory external (AVSampleCursor)
    function currentChunkStorageURL: NSURL; message 'currentChunkStorageURL';
    function currentChunkStorageRange: AVSampleCursorStorageRange; message 'currentChunkStorageRange';
    function currentChunkInfo: AVSampleCursorChunkInfo; message 'currentChunkInfo';
    function currentSampleIndexInChunk: cint64; message 'currentSampleIndexInChunk';
    function currentSampleStorageRange: AVSampleCursorStorageRange; message 'currentSampleStorageRange';
  end;
{$endif}

{$endif}
