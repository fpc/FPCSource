{ Parsed from WebKit.framework WKProcessPool.h }


{$ifdef TYPES}
type
  WKProcessPoolPtr = ^WKProcessPool;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKProcessPool = objcclass external (NSObject)
  end;
{$endif}

{$endif}
