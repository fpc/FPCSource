{ Parsed from Webkit.framework WebHistory.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


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
  WebHistoryItemsAddedNotification: NSString; external name '_WebHistoryItemsAddedNotification';
  WebHistoryItemsRemovedNotification: NSString; external name '_WebHistoryItemsRemovedNotification';
  WebHistoryAllItemsRemovedNotification: NSString; external name '_WebHistoryAllItemsRemovedNotification';
  WebHistoryLoadedNotification: NSString; external name '_WebHistoryLoadedNotification';
  WebHistorySavedNotification: NSString; external name '_WebHistorySavedNotification';
  WebHistoryItemsKey: NSString; external name '_WebHistoryItemsKey';

{$endif}
{$endif}

{$ifdef FORWARD}
  WebHistory = objcclass;
  WebHistoryPointer = ^WebHistory;

{$endif}

{$ifdef CLASSES}
{$ifndef WEBHISTORY_PAS_C}
{$define WEBHISTORY_PAS_C}

{ WebHistory }
  WebHistory = objcclass(NSObject)
  private
    __historyPrivate: WebHistoryPrivate;
    
  public
    class function alloc: WebHistory; message 'alloc';

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
  end; external;

{$endif}
{$endif}
