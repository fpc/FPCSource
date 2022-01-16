{ Parsed from CoreVideo.framework CVBuffer.h }

{$if  not (defined(__COREVIDEO_CVBUFFER_H__))}
{$ifdef TYPES}
const
  __COREVIDEO_CVBUFFER_H__ = 1;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCVBufferPropagatedAttachmentsKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVBufferNonPropagatedAttachmentsKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVBufferMovieTimeKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVBufferTimeValueKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
  kCVBufferTimeScaleKey: CFStringRef { available starting in __MAC_10_4,__IPHONE_4_0 }; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kCVAttachmentMode_ShouldNotPropagate = 0;
  kCVAttachmentMode_ShouldPropagate = 1;

type
  CVAttachmentMode = cuint32;
  CVAttachmentModePtr = ^CVAttachmentMode;
  CVBufferRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function CVBufferRetain(buffer: CVBufferRef): CVBufferRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVBufferRelease(buffer: CVBufferRef); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVBufferSetAttachment(buffer: CVBufferRef; key: CFStringRef; value: CFTypeRef; attachmentMode: CVAttachmentMode); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVBufferGetAttachment(buffer: CVBufferRef; key: CFStringRef; var attachmentMode: CVAttachmentMode): CFTypeRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVBufferRemoveAttachment(buffer: CVBufferRef; key: CFStringRef); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVBufferRemoveAllAttachments(buffer: CVBufferRef); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
function CVBufferGetAttachments(buffer: CVBufferRef; attachmentMode: CVAttachmentMode): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVBufferSetAttachments(buffer: CVBufferRef; theAttachments: CFDictionaryRef; attachmentMode: CVAttachmentMode); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
procedure CVBufferPropagateAttachments(sourceBuffer: CVBufferRef; destinationBuffer: CVBufferRef); cdecl; external; { available starting in __MAC_10_4,__IPHONE_4_0 }
{$endif}

{$endif}
