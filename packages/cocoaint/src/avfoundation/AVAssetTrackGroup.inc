{ Parsed from AVFoundation.framework AVAssetTrackGroup.h }


{$ifdef TYPES}
type
  AVAssetTrackGroupPtr = ^AVAssetTrackGroup;
{$endif}

{$ifdef CLASSES}

type
  AVAssetTrackGroup = objcclass external (NSObject, NSCopyingProtocol)
  private
    _assetTrackGroup: AVAssetTrackGroupInternal;
  public
    function trackIDs: NSArray; message 'trackIDs';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

