{ Parsed from CoreVideo.framework CVDisplayLink.h }

{$if  not (defined(__COREVIDEO_CVDISPLAYLINK_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVDISPLAYLINK_H__ = 1;

type
  CVDisplayLinkRef = OpaqueType;
{$endif}

{$ifdef TYPES}
type
  CVDisplayLinkOutputCallback = function (displayLink: CVDisplayLinkRef; inNow: CVTimeStampPtr; inOutputTime: CVTimeStampPtr; flagsIn: CVOptionFlags; var flagsOut: CVOptionFlags; displayLinkContext: pointer): CVReturn; cdecl;
{$endif}

{$ifdef FUNCTIONS}
function CVDisplayLinkGetTypeID: CFTypeID; cdecl; external;
function CVDisplayLinkCreateWithCGDisplays(displayArray: CGDirectDisplayIDPtr; count: CFIndex; var displayLinkOut: CVDisplayLinkRef): CVReturn; cdecl; external;
function CVDisplayLinkCreateWithOpenGLDisplayMask(mask: CGOpenGLDisplayMask; var displayLinkOut: CVDisplayLinkRef): CVReturn; cdecl; external;
function CVDisplayLinkCreateWithCGDisplay(displayID: CGDirectDisplayID; var displayLinkOut: CVDisplayLinkRef): CVReturn; cdecl; external;
function CVDisplayLinkCreateWithActiveCGDisplays(var displayLinkOut: CVDisplayLinkRef): CVReturn; cdecl; external;
function CVDisplayLinkSetCurrentCGDisplay(displayLink: CVDisplayLinkRef; displayID: CGDirectDisplayID): CVReturn; cdecl; external;
function CVDisplayLinkSetCurrentCGDisplayFromOpenGLContext(displayLink: CVDisplayLinkRef; cglContext: CGLContextObj; cglPixelFormat: CGLPixelFormatObj): CVReturn; cdecl; external;
function CVDisplayLinkGetCurrentCGDisplay(displayLink: CVDisplayLinkRef): CGDirectDisplayID; cdecl; external;
function CVDisplayLinkSetOutputCallback(displayLink: CVDisplayLinkRef; callback: CVDisplayLinkOutputCallback; userInfo: pointer): CVReturn; cdecl; external;
function CVDisplayLinkStart(displayLink: CVDisplayLinkRef): CVReturn; cdecl; external;
function CVDisplayLinkStop(displayLink: CVDisplayLinkRef): CVReturn; cdecl; external;
function CVDisplayLinkGetNominalOutputVideoRefreshPeriod(displayLink: CVDisplayLinkRef): CVTime; cdecl; external;
function CVDisplayLinkGetOutputVideoLatency(displayLink: CVDisplayLinkRef): CVTime; cdecl; external;
function CVDisplayLinkGetActualOutputVideoRefreshPeriod(displayLink: CVDisplayLinkRef): double; cdecl; external;
function CVDisplayLinkIsRunning(displayLink: CVDisplayLinkRef): Boolean; cdecl; external;
function CVDisplayLinkGetCurrentTime(displayLink: CVDisplayLinkRef; var outTime: CVTimeStamp): CVReturn; cdecl; external;
function CVDisplayLinkTranslateTime(displayLink: CVDisplayLinkRef; inTime: CVTimeStampPtr; var outTime: CVTimeStamp): CVReturn; cdecl; external;
function CVDisplayLinkRetain(displayLink: CVDisplayLinkRef): CVDisplayLinkRef; cdecl; external;
procedure CVDisplayLinkRelease(displayLink: CVDisplayLinkRef); cdecl; external;
{$endif}

{$endif}
