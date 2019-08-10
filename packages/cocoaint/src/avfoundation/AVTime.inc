{ Parsed from AVFoundation.framework AVTime.h }

{$ifdef CLASSES}

type
  NSValueAVFoundationExtensions = objccategory external (NSValue)
    class function valueWithCMTime (time: CMTime): NSValue; message 'valueWithCMTime:'; { available in 10_7, 4_0 }
    function CMTimeValue_: CMTime; message 'CMTimeValue';
    class function valueWithCMTimeRange (timeRange: CMTimeRange): NSValue; message 'valueWithCMTimeRange:'; { available in 10_7, 4_0 }
    function CMTimeRangeValue: CMTimeRange; message 'CMTimeRangeValue';
    class function valueWithCMTimeMapping (timeMapping: CMTimeMapping): NSValue; message 'valueWithCMTimeMapping:'; { available in 10_7, 4_0 }
    function CMTimeMappingValue: CMTimeMapping; message 'CMTimeMappingValue';
  end;

type
  AVTimeCoding = objccategory external (NSCoder)
    procedure encodeCMTime_forKey (time: CMTime; key: NSString); message 'encodeCMTime:forKey:'; { available in 10_7, 4_0 }
    function decodeCMTimeForKey (key: NSString): CMTime; message 'decodeCMTimeForKey:'; { available in 10_7, 4_0 }
    procedure encodeCMTimeRange_forKey (timeRange: CMTimeRange; key: NSString); message 'encodeCMTimeRange:forKey:'; { available in 10_7, 4_0 }
    function decodeCMTimeRangeForKey (key: NSString): CMTimeRange; message 'decodeCMTimeRangeForKey:'; { available in 10_7, 4_0 }
    procedure encodeCMTimeMapping_forKey (timeMapping: CMTimeMapping; key: NSString); message 'encodeCMTimeMapping:forKey:'; { available in 10_7, 4_0 }
    function decodeCMTimeMappingForKey (key: NSString): CMTimeMapping; message 'decodeCMTimeMappingForKey:'; { available in 10_7, 4_0 }
  end;
{$endif}

