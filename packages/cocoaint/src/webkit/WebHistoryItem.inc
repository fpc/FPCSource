{ Parsed from Webkit.framework WebHistoryItem.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
  WebHistoryItemChangedNotification: NSString; external name '_WebHistoryItemChangedNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  WebHistoryItem = objcclass;
  WebHistoryItemPointer = ^WebHistoryItem;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBHISTORYITEM_PAS_C}
{$define WEBHISTORYITEM_PAS_C}

{ WebHistoryItem }
  WebHistoryItem = objcclass(NSObject, NSCopyingProtocol)
  private
    __private: WebHistoryItemPrivate;
    
  public
    class function alloc: WebHistoryItem; message 'alloc';

    function initWithURLString_title_lastVisitedTimeInterval(URLString_: NSString; title_: NSString; time: NSTimeInterval): id; message 'initWithURLString:title:lastVisitedTimeInterval:';
    function originalURLString: NSString; message 'originalURLString';
    function URLString: NSString; message 'URLString';
    function title: NSString; message 'title';
    function lastVisitedTimeInterval: NSTimeInterval; message 'lastVisitedTimeInterval';
    procedure setAlternateTitle(alternateTitle_: NSString); message 'setAlternateTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    function icon: NSImage; message 'icon';
  end; external;

{$endif}
{$endif}
