{ Parsed from WebKit.framework WebBackForwardList.h }


{$ifdef TYPES}
type
  WebBackForwardListPtr = ^WebBackForwardList;
{$endif}

{$ifdef CLASSES}

type
  WebBackForwardList = objcclass external (NSObject)
  private
    _private: WebBackForwardListPrivate;
  public
    procedure addItem (item: WebHistoryItem); message 'addItem:';
    procedure goBack; message 'goBack';
    procedure goForward; message 'goForward';
    procedure goToItem (item: WebHistoryItem); message 'goToItem:';
    function backItem: WebHistoryItem; message 'backItem';
    function currentItem: WebHistoryItem; message 'currentItem';
    function forwardItem: WebHistoryItem; message 'forwardItem';
    function backListWithLimit (limit: cint): NSArray; message 'backListWithLimit:';
    function forwardListWithLimit (limit: cint): NSArray; message 'forwardListWithLimit:';
    procedure setCapacity(newValue: cint); message 'setCapacity:';
    function capacity: cint; message 'capacity';
    function backListCount: cint; message 'backListCount';
    function forwardListCount: cint; message 'forwardListCount';
    function containsItem (item: WebHistoryItem): ObjCBOOL; message 'containsItem:';
    function itemAtIndex (index: cint): WebHistoryItem; message 'itemAtIndex:';
  end;


type
  WebBackForwardListDeprecated = objccategory external (WebBackForwardList)
    procedure setPageCacheSize (size: NSUInteger); message 'setPageCacheSize:';
    function pageCacheSize: NSUInteger; message 'pageCacheSize';
  end;
{$endif}

