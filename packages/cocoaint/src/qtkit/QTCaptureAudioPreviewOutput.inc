{ Parsed from QTKit.framework QTCaptureAudioPreviewOutput.h }


{$ifdef TYPES}
type
  QTCaptureAudioPreviewOutputPtr = ^QTCaptureAudioPreviewOutput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCaptureAudioPreviewOutput = objcclass external (QTCaptureOutput)
  private
    _internal: QTCaptureAudioPreviewOutputInternal;
    _reserved4: clong;
    _reserved5: clong;
    _reserved6: clong;
  public
    function outputDeviceUniqueID: NSString; message 'outputDeviceUniqueID';
    procedure setOutputDeviceUniqueID (uniqueID: NSString); message 'setOutputDeviceUniqueID:';
    function volume: single; message 'volume';
    procedure setVolume (volume: single); message 'setVolume:';
  end;
{$endif}

{$endif}
