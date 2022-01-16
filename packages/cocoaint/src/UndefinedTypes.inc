
{ Private types }
type
	__NSAppleEventManagerSuspension = OpaqueRecord;
	_NSZone = OpaqueRecord;
	_NSImageCellAnimationState = Pointer;
	_CGLPBufferObject = Pointer;
	PATHSEGMENT = OpaqueRecord;
	PATHSEGMENTPtr = ^PATHSEGMENT;
	_NSModalSession = OpaqueRecord;
	NSThreadPrivate = OpaqueRecord;
	NSScreenAuxiliary = OpaqueRecord;
	__NSProgressIndicatorThreadInfo = OpaqueRecord;
	_NSImageCellAnimationStatePtr = Pointer;
	_CGLPBufferObjectPtr = Pointer;
	__NSOVRowEntryPtr = Pointer;

{ MacOSPosix.pas }
type
	dispatch_queue_t = pointer;
  dispatch_semaphore_t = pointer;
	dispatch_block_t = pointer;
	pthread_mutex_t = pointer;	
	pthread_block_t = pointer;	

{ Security framework - SecureTransport.h }
type
	SSLProtocol = integer;
	
type
	size_tPtr = ^size_t;

// CGWindowLevel.h constants we can't translate to FPC
const
	kCGNormalWindowLevel = -1;
  kCGFloatingWindowLevel = -1;
  kCGTornOffMenuWindowLevel = -1;
  kCGMainMenuWindowLevel = -1;
  kCGStatusWindowLevel = -1;
  kCGDockWindowLevel = -1;
  kCGModalPanelWindowLevel = -1;
  kCGPopUpMenuWindowLevel = -1;
  kCGScreenSaverWindowLevel = -1;

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

	LONG_MAX = high(clong);
	LONG_MIN = low(clong);
	ULONG_MAX = high(culong);
	SHRT_MAX = high(cshort);
	INT_MAX = high(cint);

// ??? what are these???
const
	FLT_MIN = 0;
	FLT_MAX = 1;
	DBL_MAX = high(clong);
	
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

// IOKit types
type
	IOOptionBits = UInt32;
	IOFixed = SInt32;
	IOVersion = UInt32;
	IOItemCount = UInt32;
	IOCacheMode = UInt32;
	IOByteCount = UInt32;
	IOReturn = UInt32;

// OpenGL
type
	GLfloatPtr = pointer;
	
// CarbonCore/Components.h
type
  ComponentRecord = record
    data:array[0..0] of clong;
  end;
  ComponentRecordPtr = ^ComponentRecord;
  Component = ^ComponentRecord;

type
  ComponentInstanceRecord = record
    data:array[0..0] of clong;
  end;
  ComponentInstanceRecordPtr = ^ComponentInstanceRecord;
  ComponentInstance = ^ComponentInstanceRecord;

// OSServices/CSIdentity
type
	CSIdentityAuthorityRef = OpaqueType;
	CSIdentityRef = OpaqueType;

// CoreAudio
type
	AudioFormatListItem = OpaqueRecord;
  AudioFormatListItemPtr = ^AudioFormatListItem;

// OpenGL
type
	CGLContextObjPtr = ^CGLContextObj;
	CGLPixelFormatObjPtr = ^CGLPixelFormatObj;
