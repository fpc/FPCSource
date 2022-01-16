{ Parsed from WebKit.framework WKPreferences.h }


{ Types from WKPreferences }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  WKPreferencesPtr = ^WKPreferences;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKPreferences = objcclass external (NSObject)
  public
    procedure setMinimumFontSize(newValue: CGFloat); message 'setMinimumFontSize:';
    function minimumFontSize: CGFloat; message 'minimumFontSize';
    procedure setJavaScriptEnabled(newValue: ObjCBOOL); message 'setJavaScriptEnabled:';
    function javaScriptEnabled: ObjCBOOL; message 'javaScriptEnabled';
    procedure setJavaScriptCanOpenWindowsAutomatically(newValue: ObjCBOOL); message 'setJavaScriptCanOpenWindowsAutomatically:';
    function javaScriptCanOpenWindowsAutomatically: ObjCBOOL; message 'javaScriptCanOpenWindowsAutomatically';
    {$if  not defined(TARGET_OS_IPHONE)}
    procedure setJavaEnabled(newValue: ObjCBOOL); message 'setJavaEnabled:';
    function javaEnabled: ObjCBOOL; message 'javaEnabled';
    procedure setPlugInsEnabled(newValue: ObjCBOOL); message 'setPlugInsEnabled:';
    function plugInsEnabled: ObjCBOOL; message 'plugInsEnabled';
    {$endif}
  end;
{$endif}

{$endif}
