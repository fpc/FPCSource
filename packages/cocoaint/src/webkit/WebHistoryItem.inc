{ Parsed from WebKit.framework WebHistoryItem.h }


{$ifdef TYPES}
type
  WebHistoryItemPtr = ^WebHistoryItem;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  WebHistoryItemChangedNotification: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  WebHistoryItem = objcclass external (NSObject, NSCopyingProtocol)
  private
    _private: WebHistoryItemPrivate;
  public
    function initWithURLString_title_lastVisitedTimeInterval (URLString: NSString; title: NSString; time: NSTimeInterval): instancetype; message 'initWithURLString:title:lastVisitedTimeInterval:';
    function originalURLString: NSString; message 'originalURLString';
    function URLString: NSString; message 'URLString';
    function title: NSString; message 'title';
    function lastVisitedTimeInterval: NSTimeInterval; message 'lastVisitedTimeInterval';
    procedure setAlternateTitle(newValue: NSString); message 'setAlternateTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    function icon: NSImage; message 'icon';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

