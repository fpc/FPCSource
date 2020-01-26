{ Parsed from WebKit.framework WKWindowFeatures.h }


{$ifdef TYPES}
type
  WKWindowFeaturesPtr = ^WKWindowFeatures;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKWindowFeatures = objcclass external (NSObject)
  public
    function menuBarVisibility: NSNumber; message 'menuBarVisibility';
    function statusBarVisibility: NSNumber; message 'statusBarVisibility';
    function toolbarsVisibility: NSNumber; message 'toolbarsVisibility';
    function allowsResizing: NSNumber; message 'allowsResizing';
    function x: NSNumber; message 'x';
    function y: NSNumber; message 'y';
    function width: NSNumber; message 'width';
    function height: NSNumber; message 'height';
  end;
{$endif}

{$endif}
