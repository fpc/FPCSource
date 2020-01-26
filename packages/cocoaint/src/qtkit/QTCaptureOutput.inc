{ Parsed from QTKit.framework QTCaptureOutput.h }


{$ifdef TYPES}
type
  QTCaptureOutputPtr = ^QTCaptureOutput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCaptureOutput = objcclass external (NSObject)
  private
    _session: QTCaptureSession;
    _reserved1: clong;
    _reserved2: clong;
    _reserved3: clong;
  public
    function connections: NSArray; message 'connections';
  end;
{$endif}

{$endif}
