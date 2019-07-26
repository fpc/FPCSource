{ Parsed from CoreMedia.framework CMAttachment.h }

{$ifndef CMATTACHMENT_H}

{$ifdef TYPES}
type
  CMAttachmentBearerRef = CFTypeRef;
  CMAttachmentBearerRefPtr = ^CMAttachmentBearerRef;

const
  kCMAttachmentMode_ShouldNotPropagate = 0;
  kCMAttachmentMode_ShouldPropagate = 1;

type
  CMAttachmentMode = cuint32;
  CMAttachmentModePtr = ^CMAttachmentMode;
{$endif}

{$ifdef FUNCTIONS}
procedure CMSetAttachment(target: CMAttachmentBearerRef; key: CFStringRef; value: CFTypeRef; attachmentMode: CMAttachmentMode); cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
function CMGetAttachment(target: CMAttachmentBearerRef; key: CFStringRef; attachmentModeOut: CMAttachmentModePtr): CFTypeRef; cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
procedure CMRemoveAttachment(target: CMAttachmentBearerRef; key: CFStringRef); cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
procedure CMRemoveAllAttachments(target: CMAttachmentBearerRef); cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
function CMCopyDictionaryOfAttachments(allocator: CFAllocatorRef; target: CMAttachmentBearerRef; attachmentMode: CMAttachmentMode): CFDictionaryRef; cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
procedure CMSetAttachments(target: CMAttachmentBearerRef; theAttachments: CFDictionaryRef; attachmentMode: CMAttachmentMode); cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
procedure CMPropagateAttachments(source: CMAttachmentBearerRef; destination: CMAttachmentBearerRef); cdecl; external; { available starting in  __MAC_10_7, __IPHONE_4_0  }
{$endif}

{$endif}
