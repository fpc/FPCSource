type
  __NSAppleEventManagerSuspension = Pointer;
 {$ifdef cpu64}
  CGFloat = double;
 {$else}
  CGFloat = single;
 {$endif}
  UTF = UInt32;
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

{ Variable argument list - disabled for Pascal }
type
 va_list_rec = record
 end;
 va_list = ^va_list_rec;

{ Private instance variable types }
type
 _NSImageCellAnimationState = Pointer;
 _CGLPBufferObject = Pointer;
 PATHSEGMENT = Pointer;		{from NSBezierPath.h what is this???}
 
{ Pointer C-Types }
type
 NSIntegerPointer = pclong;
 NSUIntegerPointer = pculong;
 NSZonePointer = ^NSZone;
 NSTimeIntervalPointer = ^double;

{ An array of objects }
type
  NSObjectArrayOfObjects = array[0..(high(longint) div sizeof(id))-1] of id;
  NSObjectArrayOfObjectsPtr = ^NSObjectArrayOfObjects;

{ Cocoa types }
const
  NSIntegerMax = high(clong);
  NSIntegerMin = low(clong);
  NSUIntegerMax = high(culong);
  NSNotFound = NSIntegerMax;

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
  aeDesc_Pointer = Pointer;
  __NSAppleEventManagerSuspensionPointer = Pointer;
  
{ Extra pointers for pass by reference arguments }
{ NOTE: many of these were from records parsed that do not contain a pointer pair. Fix this later instead of declaring them here. }
type
 NSPropertyListFormatPointer = Pointer;
 NSStringEncodingPointer = Pointer;
 AppleEventPointer = ^AppleEvent;
 NSGlyphPointer = Pointer;
 CIColorPointer = Pointer;
 NSDecimalPointer = Pointer;
 CIImagePointer = Pointer;
 NSTIFFCompressionPointer = Pointer;
 CIImage_Pointer = Pointer;
 CALayerPointer = Pointer;
 CIFilterPointer = Pointer;
 NSGlyphInscriptionPointer = Pointer;
 NSOpenGLPixelFormatAttributePointer = Pointer;
 GLintPointer = Pointer;
 NSManagedObjectContextPointer = Pointer;
 NSFetchRequestPointer = Pointer;
 NSFastEnumerationStatePointer = Pointer;
 
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