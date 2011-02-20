{ Parsed from Webkit.framework WebHistory.h }

{$ifdef TYPES}
{$ifndef WEBHISTORY_PAS_T}
{$define WEBHISTORY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBHISTORY_PAS_R}
{$define WEBHISTORY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBHISTORY_PAS_F}
{$define WEBHISTORY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBHISTORY_PAS_S}
{$define WEBHISTORY_PAS_S}

{ External string constants }
var
  WebHistoryItemsAddedNotification: NSString; cvar; external;
  WebHistoryItemsRemovedNotification: NSString; cvar; external;
  WebHistoryAllItemsRemovedNotification: NSString; cvar; external;
  WebHistoryLoadedNotification: NSString; cvar; external;
  WebHistorySavedNotification: NSString; cvar; external;
  WebHistoryItemsKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebHistory = objcclass;
  WebHistoryPointer = ^WebHistory;
  WebHistoryPtr = WebHistoryPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBHISTORY_PAS_C}
{$define WEBHISTORY_PAS_C}

{ WebHistory }
  WebHistory = objcclass external (NSObject)
  private
    _historyPrivate: WebHistoryPrivate;
    
  public
    class function optionalSharedHistory: WebHistory; message 'optionalSharedHistory';
    class procedure setOptionalSharedHistory(history: WebHistory); message 'setOptionalSharedHistory:';
    function loadFromURL_error(URL: NSURL; error: NSErrorPointer): Boolean; message 'loadFromURL:error:';
    function saveToURL_error(URL: NSURL; error: NSErrorPointer): Boolean; message 'saveToURL:error:';
    procedure addItems(newItems: NSArray); message 'addItems:';
    procedure removeItems(items: NSArray); message 'removeItems:';
    procedure removeAllItems; message 'removeAllItems';
    function orderedLastVisitedDays: NSArray; message 'orderedLastVisitedDays';
    function orderedItemsLastVisitedOnDay(calendarDate: NSCalendarDate): NSArray; message 'orderedItemsLastVisitedOnDay:';
    function itemForURL(URL: NSURL): WebHistoryItem; message 'itemForURL:';
    procedure setHistoryItemLimit(limit: cint); message 'setHistoryItemLimit:';
    function historyItemLimit: cint; message 'historyItemLimit';
    procedure setHistoryAgeInDaysLimit(limit: cint); message 'setHistoryAgeInDaysLimit:';
    function historyAgeInDaysLimit: cint; message 'historyAgeInDaysLimit';
  end;

{$endif}
{$endif}
