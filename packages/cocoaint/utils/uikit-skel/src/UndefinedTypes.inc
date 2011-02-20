type
  __NSAppleEventManagerSuspension = record end;
  IBAction = Pointer;
  objc_protocol = protocol;
  idptr = ^id;
  idptrptr = ^idptr;
  SELptr = ^SEL;

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
 
{ from CoreData }
type
 NSAttributeType = NSUInteger;

{ Pointer C-Types }

{ An array of objects }
type
  NSObjectArrayOfObjects = array[0..(high(longint) div sizeof(id))-1] of id;
  NSObjectArrayOfObjectsPtr = ^NSObjectArrayOfObjects;

{ Cocoa types }
const
 UINT_MAX = high(culong);
// 7.18.2.4 Limits of integer types capable of holding object pointers
 INTPTR_MIN = low(ptrint);
 INTPTR_MAX = high(ptrint);
 UINTPTR_MAX = high(ptruint);
// 7.18.2.5 Limits of greatest-width integer types
 INTMAX_MIN = low(int64);
 INTMAX_MAX = high(int64);
 UINTMAX_MAX = high(qword);
// 7.18.3 "Other"
 PTRDIFF_MIN = INTPTR_MIN;
 PTRDIFF_MAX = INTPTR_MAX;

{ from IOKit (not yet part of univint) }
const
 NX_TABLET_POINTER_UNKNOWN = 0;
 NX_TABLET_POINTER_PEN = 1;
 NX_TABLET_POINTER_CURSOR = 2;
 NX_TABLET_POINTER_ERASER = 3;
 
 NX_SUBTYPE_DEFAULT = 0;
 NX_SUBTYPE_TABLET_POINT = 1;
 NX_SUBTYPE_TABLET_PROXIMITY = 2;
 NX_SUBTYPE_MOUSE_TOUCH = 3;
 
 NX_TABLET_BUTTON_PENTIPMASK = $0001;
 NX_TABLET_BUTTON_PENLOWERSIDEMASK = $0002;
 NX_TABLET_BUTTON_PENUPPERSIDEMASK = $0004;
  
{ Parser hacks - these types should never exist }
type
  __NSAppleEventManagerSuspensionPtr = Pointer;

  { does not really exist, is in inactive part of ifdef }
  NSURLHandleClientProtocol = objcprotocol external
  end;
