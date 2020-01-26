{ Parsed from QuickLook.framework QLThumbnail.h }

{$if  not (defined(__QUICKLOOK_QLTHUMBNAIL__))}

{$ifdef TYPES}
type
  QLThumbnailRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function QLThumbnailCreate(allocator: CFAllocatorRef; url: CFURLRef; maxThumbnailSize: CGSize; options: CFDictionaryRef): QLThumbnailRef; cdecl; external;
function QLThumbnailCopyDocumentURL(thumbnail: QLThumbnailRef): CFURLRef; cdecl; external;
function QLThumbnailGetMaximumSize(thumbnail: QLThumbnailRef): CGSize; cdecl; external;
function QLThumbnailCopyOptions(thumbnail: QLThumbnailRef): CFDictionaryRef; cdecl; external;
{$endif}

{$if defined(__BLOCKS__) and (MAC_OS_X_VERSION_10_6 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
{$ifdef FUNCTIONS}
procedure QLThumbnailDispatchAsync(thumbnail: QLThumbnailRef; queue: dispatch_queue_t; completion: dispatch_block_t); cdecl; external;
{$endif}

{$endif}
{$ifdef FUNCTIONS}
function QLThumbnailCopyImage(thumbnail: QLThumbnailRef): CGImageRef; cdecl; external;
function QLThumbnailGetContentRect(thumbnail: QLThumbnailRef): CGRect; cdecl; external;
procedure QLThumbnailCancel(thumbnail: QLThumbnailRef); cdecl; external;
function QLThumbnailIsCancelled(thumbnail: QLThumbnailRef): Boolean; cdecl; external;
{$endif}

{$endif}
