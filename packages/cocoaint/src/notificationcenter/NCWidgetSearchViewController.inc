{ Parsed from NotificationCenter.framework NCWidgetSearchViewController.h }


{$ifdef TYPES}
type
  NCWidgetSearchViewControllerPtr = ^NCWidgetSearchViewController;
  NCWidgetSearchViewDelegateProtocolPtr = ^NCWidgetSearchViewDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NCWidgetSearchViewController = objcclass external (NSViewController)
  public
    procedure setDelegate(newValue: NCWidgetSearchViewDelegateProtocol); message 'setDelegate:';
    function delegate: NCWidgetSearchViewDelegateProtocol; message 'delegate';
    procedure setSearchResults(newValue: NSArray); message 'setSearchResults:';
    function searchResults: NSArray; message 'searchResults';
    procedure setSearchDescription(newValue: NSString); message 'setSearchDescription:';
    function searchDescription: NSString; message 'searchDescription';
    procedure setSearchResultsPlaceholderString(newValue: NSString); message 'setSearchResultsPlaceholderString:';
    function searchResultsPlaceholderString: NSString; message 'searchResultsPlaceholderString';
    procedure setSearchResultKeyPath(newValue: NSString); message 'setSearchResultKeyPath:';
    function searchResultKeyPath: NSString; message 'searchResultKeyPath';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NCWidgetSearchViewDelegateProtocol = objcprotocol external name 'NCWidgetSearchViewDelegate' (NSObjectProtocol)
    procedure widgetSearch_searchForTerm_maxResults (controller: NCWidgetSearchViewController; searchTerm: NSString; max: NSUInteger); message 'widgetSearch:searchForTerm:maxResults:';
    procedure widgetSearchTermCleared (controller: NCWidgetSearchViewController); message 'widgetSearchTermCleared:';
    procedure widgetSearch_resultSelected (controller: NCWidgetSearchViewController; object_: id); message 'widgetSearch:resultSelected:';
  end;
{$endif}

