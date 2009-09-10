type
  __NSAppleEventManagerSuspension = Pointer;
  CGFloat = Float32;
  UTF = UInt32;
  va_list = Pointer; //typedef __darwin_va_list	va_list;
  NSPointerFunctionsOptions = UInt16;
  URefCon = UInt32;
  SRefCon = SInt32;
  IBAction = Pointer;
  CIContext = id;
  CIFilter = id;
  CIImage = id;
  CALayer = id;
  QTMovie = Pointer;
  GLint = integer;
  GLenum = integer;
  GLsizei = integer;
  GLbitfield = integer;
  objc_protocol = protocol;
  
{ Private instance variable types }
type
 _NSImageCellAnimationState = Pointer;
 _CGLPBufferObject = Pointer;
 PATHSEGMENT = Pointer;		{from NSBezierPath.h what is this???}
 
{ Pointer C-Types }
{ Note: the parser appends "Pointer" by default but these may have preferred names in ctypes. }
type
 culongPointer = ^culong;
 UInt32Pointer = Pointer;
 
{ An array of objects }
type
  NSObjectArrayOfObjects = array[0..(high(longint) div sizeof(id))-1] of id;
  NSObjectArrayOfObjectsPtr = ^NSObjectArrayOfObjects;

{ Cocoa types }
const
  NSIntegerMax = high(clong);
  NSIntegerMin = low(clong);
  NSUIntegerMax = high(culong);
 
const
 NX_TABLET_POINTER_UNKNOWN = 0;
 NX_TABLET_POINTER_PEN = 1;
 NX_TABLET_POINTER_CURSOR = 2;
 NX_TABLET_POINTER_ERASER = 3;
 
 NX_SUBTYPE_DEFAULT = 0;
 NX_SUBTYPE_TABLET_POINT = 1;
 NX_SUBTYPE_TABLET_PROXIMITY = 2;
 
 NX_TABLET_BUTTON_PENTIPMASK = $0001;
 NX_TABLET_BUTTON_PENLOWERSIDEMASK = $0002;
 NX_TABLET_BUTTON_PENUPPERSIDEMASK = $0004;
  
{ The CFError API is not in MacOSAll! we need to port this for PasCocoa... } 
type
  CFErrorRef = Pointer;
 
{ Parser hacks - these types should never exist }
type
  char_ = Pointer;
  aeDesc_ = AEDesc;
  CIContext_ = id;
  CIImage_ = id;
  NSRangePointerPointer = Pointer;

{ Parse bugs - these should have been parsed but were not due to errors }
type
  NSHashEnumerator = Pointer; {struct}
  NSMapEnumerator = Pointer; {struct}
  NSUncaughtExceptionHandler = Pointer; {function?}
 
{ NSPointerFunctions - missing from where?? }
const
  NSPointerFunctionsZeroingWeakMemory = 1 shl 0;
  NSPointerFunctionsCopyIn = 1 shl 16;
  NSPointerFunctionsObjectPointerPersonality = 2 shl 8;