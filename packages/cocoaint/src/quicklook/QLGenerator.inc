{ Parsed from QuickLook.framework QLGenerator.h }

{$if  not (defined(__QUICKLOOK_QLGENERATOR__))}
{$ifdef TYPES}
const
  kQLReturnMask = $af00;
  kQLReturnNoError = noErr;

type
  QLThumbnailRequestRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function QLThumbnailRequestCopyURL(thumbnail: QLThumbnailRequestRef): CFURLRef; cdecl; external;
function QLThumbnailRequestCopyOptions(thumbnail: QLThumbnailRequestRef): CFDictionaryRef; cdecl; external;
function QLThumbnailRequestCopyContentUTI(thumbnail: QLThumbnailRequestRef): CFStringRef; cdecl; external;
function QLThumbnailRequestGetMaximumSize(thumbnail: QLThumbnailRequestRef): CGSize; cdecl; external;
function QLThumbnailRequestGetGeneratorBundle(thumbnail: QLThumbnailRequestRef): CFBundleRef; cdecl; external;
procedure QLThumbnailRequestSetDocumentObject(thumbnail: QLThumbnailRequestRef; object_: pointer; callbacks: CFArrayCallBacksPtr); cdecl; external;
function QLThumbnailRequestGetDocumentObject(thumbnail: QLThumbnailRequestRef): pointer; cdecl; external;
procedure QLThumbnailRequestSetImage(thumbnail: QLThumbnailRequestRef; image: CGImageRef; properties: CFDictionaryRef); cdecl; external;
procedure QLThumbnailRequestSetImageWithData(thumbnail: QLThumbnailRequestRef; data: CFDataRef; properties: CFDictionaryRef); cdecl; external;
function QLThumbnailRequestCreateContext(thumbnail: QLThumbnailRequestRef; size: CGSize; isBitmap: Boolean; properties: CFDictionaryRef): CGContextRef; cdecl; external;
procedure QLThumbnailRequestFlushContext(thumbnail: QLThumbnailRequestRef; context: CGContextRef); cdecl; external;
procedure QLThumbnailRequestSetImageAtURL(thumbnail: QLThumbnailRequestRef; url: CFURLRef; properties: CFDictionaryRef); cdecl; external;
procedure QLThumbnailRequestSetThumbnailWithDataRepresentation(thumbnail: QLThumbnailRequestRef; data: CFDataRef; contentTypeUTI: CFStringRef; previewProperties: CFDictionaryRef; properties: CFDictionaryRef); cdecl; external;
procedure QLThumbnailRequestSetThumbnailWithURLRepresentation(thumbnail: QLThumbnailRequestRef; url: CFURLRef; contentTypeUTI: CFStringRef; previewProperties: CFDictionaryRef; properties: CFDictionaryRef); cdecl; external;
function QLThumbnailRequestIsCancelled(thumbnail: QLThumbnailRequestRef): Boolean; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kQLThumbnailPropertyExtensionKey: CFStringRef; cvar; external;
  kQLThumbnailPropertyBadgeImageKey: CFStringRef; cvar; external;
  kQLThumbnailPropertyBaseBundlePathKey: CFStringRef; cvar; external;
{$endif}

{$ifdef TYPES}
type
  QLPreviewRequestRef = OpaqueType;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kQLPreviewPropertyDisplayNameKey: CFStringRef; cvar; external;
  kQLPreviewPropertyWidthKey: CFStringRef; cvar; external;
  kQLPreviewPropertyHeightKey: CFStringRef; cvar; external;
  kQLPreviewPropertyBaseBundlePathKey: CFStringRef; cvar; external;
  kQLPreviewPropertyStringEncodingKey: CFStringRef; cvar; external;
{$endif}

{$ifdef TYPES}
const
  kQLPreviewPDFStandardStyle = 0;
  kQLPreviewPDFPagesWithThumbnailsOnRightStyle = 3;
  kQLPreviewPDFPagesWithThumbnailsOnLeftStyle = 4;
type
  QLPreviewPDFStyle = clong;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kQLPreviewPropertyPDFStyleKey: CFStringRef; cvar; external;
  kQLPreviewOptionCursorKey: CFStringRef; cvar; external;
  kQLPreviewPropertyCursorKey: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function QLPreviewRequestCopyURL(preview: QLPreviewRequestRef): CFURLRef; cdecl; external;
function QLPreviewRequestCopyOptions(preview: QLPreviewRequestRef): CFDictionaryRef; cdecl; external;
function QLPreviewRequestCopyContentUTI(preview: QLPreviewRequestRef): CFStringRef; cdecl; external;
function QLPreviewRequestGetGeneratorBundle(preview: QLPreviewRequestRef): CFBundleRef; cdecl; external;
procedure QLPreviewRequestSetDocumentObject(preview: QLPreviewRequestRef; object_: pointer; callbacks: CFArrayCallBacksPtr); cdecl; external;
function QLPreviewRequestGetDocumentObject(preview: QLPreviewRequestRef): pointer; cdecl; external;
function QLPreviewRequestIsCancelled(preview: QLPreviewRequestRef): Boolean; cdecl; external;
procedure QLPreviewRequestSetDataRepresentation(preview: QLPreviewRequestRef; data: CFDataRef; contentTypeUTI: CFStringRef; properties: CFDictionaryRef); cdecl; external;
procedure QLPreviewRequestSetURLRepresentation(preview: QLPreviewRequestRef; url: CFURLRef; contentTypeUTI: CFStringRef; properties: CFDictionaryRef); cdecl; external;
function QLPreviewRequestCreateContext(preview: QLPreviewRequestRef; size: CGSize; isBitmap: Boolean; properties: CFDictionaryRef): CGContextRef; cdecl; external;
function QLPreviewRequestCreatePDFContext(preview: QLPreviewRequestRef; mediaBox: CGRectPtr; auxiliaryInfo: CFDictionaryRef; properties: CFDictionaryRef): CGContextRef; cdecl; external;
procedure QLPreviewRequestFlushContext(preview: QLPreviewRequestRef; context: CGContextRef); cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kQLPreviewPropertyMIMETypeKey: CFStringRef; cvar; external;
  kQLPreviewPropertyTextEncodingNameKey: CFStringRef; cvar; external;
  kQLPreviewPropertyAttachmentDataKey: CFStringRef; cvar; external;
  kQLPreviewPropertyAttachmentsKey: CFStringRef; cvar; external;
  kQLPreviewContentIDScheme: CFStringRef; cvar; external;
{$endif}

{$if (defined(__COREFOUNDATION_CFPLUGINCOM__))}
{$ifdef TYPES}
type
  QLGeneratorInterfaceStruct = record
    GenerateThumbnailForURL: function (thisInterface: pointer; thumbnail: QLThumbnailRequestRef; url: CFURLRef; contentTypeUTI: CFStringRef; options: CFDictionaryRef; maxSize: CGSize): OSStatus; cdecl;
    CancelThumbnailGeneration: procedure (thisInterface: pointer; thumbnail: QLThumbnailRequestRef); cdecl;
    GeneratePreviewForURL: function (thisInterface: pointer; preview: QLPreviewRequestRef; url: CFURLRef; contentTypeUTI: CFStringRef; options: CFDictionaryRef): OSStatus; cdecl;
    CancelPreviewGeneration: procedure (thisInterface: pointer; preview: QLPreviewRequestRef); cdecl;
  end;
type
  QLGeneratorInterfaceStructPtr = ^QLGeneratorInterfaceStruct;
{$endif}

{$endif}
{$endif}
