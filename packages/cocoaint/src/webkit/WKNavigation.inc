{ Parsed from WebKit.framework WKNavigation.h }


{$ifdef TYPES}
type
  WKNavigationPtr = ^WKNavigation;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKNavigation = objcclass external (NSObject)
  end;
{$endif}

{$endif}
