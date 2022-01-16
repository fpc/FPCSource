{ Parsed from AVFoundation.framework AVAssetTrackSegment.h }


{$ifdef TYPES}
type
  AVAssetTrackSegmentPtr = ^AVAssetTrackSegment;
{$endif}

{$ifdef CLASSES}

type
  AVAssetTrackSegment = objcclass external (NSObject)
  private
    _timeMapping: CMTimeMapping;
  public
    function timeMapping: CMTimeMapping; message 'timeMapping';
    function isEmpty: ObjCBOOL; message 'isEmpty';
  end;
{$endif}

