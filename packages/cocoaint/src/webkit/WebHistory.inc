{ Parsed from WebKit.framework WebHistory.h }


{$ifdef TYPES}
type
  WebHistoryPtr = ^WebHistory;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  WebHistoryItemsAddedNotification: NSString; cvar; external;
  WebHistoryItemsRemovedNotification: NSString; cvar; external;
  WebHistoryAllItemsRemovedNotification: NSString; cvar; external;
  WebHistoryLoadedNotification: NSString; cvar; external;
  WebHistorySavedNotification: NSString; cvar; external;
  WebHistoryItemsKey: NSString; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  WebHistory = objcclass external (NSObject)
  private
    _historyPrivate: WebHistoryPrivate;
  public
    class function optionalSharedHistory: WebHistory; message 'optionalSharedHistory';
    class procedure setOptionalSharedHistory (history: WebHistory); message 'setOptionalSharedHistory:';
    function loadFromURL_error (URL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'loadFromURL:error:';
    function saveToURL_error (URL: NSURL; error: NSErrorPtr): ObjCBOOL; message 'saveToURL:error:';
    procedure addItems (newItems: NSArray); message 'addItems:';
    procedure removeItems (items: NSArray); message 'removeItems:';
    procedure removeAllItems; message 'removeAllItems';
    function orderedLastVisitedDays: NSArray; message 'orderedLastVisitedDays';
    function orderedItemsLastVisitedOnDay (calendarDate: NSCalendarDate): NSArray; message 'orderedItemsLastVisitedOnDay:';
    function itemForURL (URL: NSURL): WebHistoryItem; message 'itemForURL:';
    procedure setHistoryItemLimit(newValue: cint); message 'setHistoryItemLimit:';
    function historyItemLimit: cint; message 'historyItemLimit';
    procedure setHistoryAgeInDaysLimit(newValue: cint); message 'setHistoryAgeInDaysLimit:';
    function historyAgeInDaysLimit: cint; message 'historyAgeInDaysLimit';
  end;
{$endif}

