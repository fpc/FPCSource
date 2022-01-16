{ Parsed from QTKit.framework QTTime.h }

{$ifdef TYPES}
const
  kQTTimeIsIndefinite = 1 shl 0;

type
  QTTime = record
    timeValue: clonglong;
    timeScale: clong;
    flags: clong;
  end;
type
  QTTimePtr = ^QTTime;
{$endif}

{$ifdef CLASSES}

type
  NSValueQTTimeExtensions = objccategory external (NSValue)
    class function valueWithQTTime (time: QTTime): NSValue; message 'valueWithQTTime:';
    function QTTimeValue: QTTime; message 'QTTimeValue';
  end;

type
  NSQTTimeCoding = objccategory external (NSCoder)
    procedure encodeQTTime_forKey (time: QTTime; key: NSString); message 'encodeQTTime:forKey:';
    function decodeQTTimeForKey (key: NSString): QTTime; message 'decodeQTTimeForKey:';
  end;
{$endif}

{$if (QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)}
{$ifdef CLASSES}

type
  NSValueQTSMPTETimeExtensions = objccategory external (NSValue)
    class function valueWithSMPTETime (time: SMPTETime): NSValue; message 'valueWithSMPTETime:';
    function SMPTETimeValue: SMPTETime; message 'SMPTETimeValue';
  end;

type
  NSCoderQTSMPTETimeCoding = objccategory external (NSCoder)
    procedure encodeSMPTETime_forKey (time: SMPTETime; key: NSString); message 'encodeSMPTETime:forKey:';
    function decodeSMPTETimeForKey (key: NSString): SMPTETime; message 'decodeSMPTETimeForKey:';
  end;
{$endif}

{$endif}
