{ Parsed from QTKit.framework QTCaptureDeviceInput.h }


{$ifdef TYPES}
type
  QTCaptureDeviceInputPtr = ^QTCaptureDeviceInput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCaptureDeviceInput = objcclass external (QTCaptureInput)
  private
    _internal: QTCaptureDeviceInputInternal;
    _reserved4: clong;
    _reserved5: clong;
    _reserved6: clong;
  public
    class function deviceInputWithDevice (device: QTCaptureDevice): id; message 'deviceInputWithDevice:';
    function initWithDevice (device: QTCaptureDevice): id; message 'initWithDevice:';
    function device: QTCaptureDevice; message 'device';
  end;
{$endif}

{$endif}
