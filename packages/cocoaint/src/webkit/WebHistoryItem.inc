{ Parsed from Webkit.framework WebHistoryItem.h }

{$ifdef TYPES}
{$ifndef WEBHISTORYITEM_PAS_T}
{$define WEBHISTORYITEM_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBHISTORYITEM_PAS_R}
{$define WEBHISTORYITEM_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBHISTORYITEM_PAS_F}
{$define WEBHISTORYITEM_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBHISTORYITEM_PAS_S}
{$define WEBHISTORYITEM_PAS_S}

{ External string constants }
var
  WebHistoryItemChangedNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebHistoryItem = objcclass;
  WebHistoryItemPointer = ^WebHistoryItem;
  WebHistoryItemPtr = WebHistoryItemPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBHISTORYITEM_PAS_C}
{$define WEBHISTORYITEM_PAS_C}

{ WebHistoryItem }
  WebHistoryItem = objcclass external (NSObject, NSCopyingProtocol)
  private
    _private: WebHistoryItemPrivate;
    
  public
    function initWithURLString_title_lastVisitedTimeInterval(URLString_: NSString; title_: NSString; time: NSTimeInterval): id; message 'initWithURLString:title:lastVisitedTimeInterval:';
    function originalURLString: NSString; message 'originalURLString';
    function URLString: NSString; message 'URLString';
    function title: NSString; message 'title';
    function lastVisitedTimeInterval: NSTimeInterval; message 'lastVisitedTimeInterval';
    procedure setAlternateTitle(alternateTitle_: NSString); message 'setAlternateTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    function icon: NSImage; message 'icon';

    { Adopted Protocols }
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{$endif}
{$endif}
