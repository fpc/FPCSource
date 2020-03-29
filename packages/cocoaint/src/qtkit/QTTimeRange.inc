{ Parsed from QTKit.framework QTTimeRange.h }

{$ifdef TYPES}
type
  QTTimeRange = record
    time: QTTime;
    duration: QTTime;
  end;
type
  QTTimeRangePtr = ^QTTimeRange;
{$endif}

{$ifdef CLASSES}

type
  NSValueQTTimeRangeExtensions = objccategory external (NSValue)
    class function valueWithQTTimeRange (range: QTTimeRange): NSValue; message 'valueWithQTTimeRange:';
    function QTTimeRangeValue: QTTimeRange; message 'QTTimeRangeValue';
  end;

type
  NSQTTimeRangeCoding = objccategory external (NSCoder)
    procedure encodeQTTimeRange_forKey (range: QTTimeRange; key: NSString); message 'encodeQTTimeRange:forKey:';
    function decodeQTTimeRangeForKey (key: NSString): QTTimeRange; message 'decodeQTTimeRangeForKey:';
  end;
{$endif}

