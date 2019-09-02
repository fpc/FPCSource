{ Parsed from QTKit.framework QTCaptureMovieFileOutput.h }


{$ifdef TYPES}
type
  QTCaptureMovieFileOutputPtr = ^QTCaptureMovieFileOutput;
{$endif}

{$if ((QTKIT_VERSION_MAX_ALLOWED >= QTKIT_VERSION_7_2)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4))}
{$ifdef CLASSES}

type
  QTCaptureMovieFileOutput = objcclass external (QTCaptureFileOutput)
  private
    _internal: QTCaptureMovieFileOutputInternal;
    _reserved8: clong;
  end;
{$endif}

{$endif}
