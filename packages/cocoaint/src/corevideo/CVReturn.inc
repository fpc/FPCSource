{ Parsed from CoreVideo.framework CVReturn.h }

{$if  not (defined(__COREVIDEO_CVRETURN_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVRETURN_H__ = 1;
{$endif}

{$ifdef TYPES}
const
  kCVReturnSuccess = 0;
  kCVReturnFirst = -6660;
  kCVReturnError = kCVReturnFirst;
  kCVReturnInvalidArgument = -6661;
  kCVReturnAllocationFailed = -6662;
  kCVReturnInvalidDisplay = -6670;
  kCVReturnDisplayLinkAlreadyRunning = -6671;
  kCVReturnDisplayLinkNotRunning = -6672;
  kCVReturnDisplayLinkCallbacksNotSet = -6673;
  kCVReturnInvalidPixelFormat = -6680;
  kCVReturnInvalidSize = -6681;
  kCVReturnInvalidPixelBufferAttributes = -6682;
  kCVReturnPixelBufferNotOpenGLCompatible = -6683;
  kCVReturnPixelBufferNotMetalCompatible = -6684;
  kCVReturnWouldExceedAllocationThreshold = -6689;
  kCVReturnPoolAllocationFailed = -6690;
  kCVReturnInvalidPoolAttributes = -6691;
  kCVReturnLast = -6699;

type
  CVReturn = cint32;
  CVReturnPtr = ^CVReturn;
{$endif}

{$endif}
