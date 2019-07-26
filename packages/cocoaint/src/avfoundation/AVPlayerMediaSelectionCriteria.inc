{ Parsed from AVFoundation.framework AVPlayerMediaSelectionCriteria.h }


{$ifdef TYPES}
type
  AVPlayerMediaSelectionCriteriaPtr = ^AVPlayerMediaSelectionCriteria;
{$endif}

{$ifdef CLASSES}

type
  AVPlayerMediaSelectionCriteria = objcclass external (NSObject)
  private
    _criteria: AVPlayerMediaSelectionCriteriaInternal;
  public
    function preferredLanguages: NSArray; message 'preferredLanguages';
    function preferredMediaCharacteristics: NSArray; message 'preferredMediaCharacteristics';
    function initWithPreferredLanguages_preferredMediaCharacteristics (preferredLanguages_: NSArray; preferredMediaCharacteristics_: NSArray): instancetype; message 'initWithPreferredLanguages:preferredMediaCharacteristics:';
  end;
{$endif}

