{ Parsed from WebKit.framework WKBackForwardListItem.h }


{$ifdef TYPES}
type
  WKBackForwardListItemPtr = ^WKBackForwardListItem;
{$endif}

{$if defined(WK_API_ENABLED)}
{$ifdef CLASSES}

type
  WKBackForwardListItem = objcclass external (NSObject)
  public
    function URL: NSURL; message 'URL';
    function title: NSString; message 'title';
    function initialURL: NSURL; message 'initialURL';
  end;
{$endif}

{$endif}
