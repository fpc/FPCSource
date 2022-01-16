{ Parsed from CoreVideo.framework CVPixelBufferPool.h }

{$if  not (defined(__COREVIDEO__CVPIXELBUFFERPOOL_H__))}
{$ifdef TYPES}
const
  __COREVIDEO__CVPIXELBUFFERPOOL_H__ = 1;
{$endif}

{$ifdef TYPES}
type
  CVPixelBufferPoolRef = OpaqueType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelBufferPoolMinimumBufferCountKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferPoolMaximumBufferAgeKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CVPixelBufferPoolGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferPoolRetain(pixelBufferPool: CVPixelBufferPoolRef): CVPixelBufferPoolRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVPixelBufferPoolRelease(pixelBufferPool: CVPixelBufferPoolRef); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferPoolCreate(allocator: CFAllocatorRef; poolAttributes: CFDictionaryRef; pixelBufferAttributes: CFDictionaryRef; var poolOut: CVPixelBufferPoolRef): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferPoolGetAttributes(pool: CVPixelBufferPoolRef): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferPoolGetPixelBufferAttributes(pool: CVPixelBufferPoolRef): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferPoolCreatePixelBuffer(allocator: CFAllocatorRef; pixelBufferPool: CVPixelBufferPoolRef; var pixelBufferOut: CVPixelBufferRef): CVReturn; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVPixelBufferPoolCreatePixelBufferWithAuxAttributes(allocator: CFAllocatorRef; pixelBufferPool: CVPixelBufferPoolRef; auxAttributes: CFDictionaryRef; pixelBufferOut: CVPixelBufferRefPtr): CVReturn; cdecl; external; { available starting in __MAC_10_7,__IPHONE_4_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVPixelBufferPoolAllocationThresholdKey: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
  kCVPixelBufferPoolFreeBufferNotification: CFStringRef { available starting in __MAC_10_7,__IPHONE_4_0 }; cvar; external;
{$endif}

{$endif}
