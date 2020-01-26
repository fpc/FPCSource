{ Parsed from QuickLook.framework QLThumbnailImage.h }

{$if  not (defined(__QUICKLOOK_QLTHUMBNAILIMAGE__))}
{$ifdef FUNCTIONS}
function QLThumbnailImageCreate(allocator: CFAllocatorRef; url: CFURLRef; maxThumbnailSize: CGSize; options: CFDictionaryRef): CGImageRef; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kQLThumbnailOptionIconModeKey: CFStringRef; cvar; external;
  kQLThumbnailOptionScaleFactorKey: CFStringRef; cvar; external;
{$endif}

{$endif}
