{ Parsed from Webkit.framework WebHistory.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
    function loadFromURL_error(URL: NSURLPointer; error: NSErrorPointer): Boolean; message 'loadFromURL:error:';
    function saveToURL_error(URL: NSURLPointer; error: NSErrorPointer): Boolean; message 'saveToURL:error:';
    procedure addItems(newItems: NSArrayPointer); message 'addItems:';
    procedure removeItems(items: NSArrayPointer); message 'removeItems:';
    procedure removeAllItems; message 'removeAllItems';
    function orderedLastVisitedDays: NSArray; message 'orderedLastVisitedDays';
    function orderedItemsLastVisitedOnDay(calendarDate: NSCalendarDatePointer): NSArray; message 'orderedItemsLastVisitedOnDay:';
    function itemForURL(URL: NSURLPointer): WebHistoryItem; message 'itemForURL:';
    procedure setHistoryItemLimit(limit: cint); message 'setHistoryItemLimit:';
    function historyItemLimit: cint; message 'historyItemLimit';
    procedure setHistoryAgeInDaysLimit(limit: cint); message 'setHistoryAgeInDaysLimit:';
    function historyAgeInDaysLimit: cint; message 'historyAgeInDaysLimit';
  end; external;

{$endif}
{$endif}
