{ Parsed from PubSub.framework PSClient.h }


{$ifdef TYPES}
type
  PSClientPtr = ^PSClient;
{$endif}

{$if (MAC_OS_X_VERSION_10_5 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
{$ifdef CLASSES}

type
  PSClient = objcclass external (NSObject)
  private
    _internal: id;
  public
    class function applicationClient: PSClient; message 'applicationClient';
    class function clientForBundleIdentifier (bundleIdentifier: NSString): PSClient; message 'clientForBundleIdentifier:';
    class function allClientBundleIdentifiers: NSArray; message 'allClientBundleIdentifiers';
    procedure setSettings(newValue: PSFeedSettings); message 'setSettings:';
    function settings: PSFeedSettings; message 'settings';
    function dateLastUpdated: NSDate; message 'dateLastUpdated';
    procedure sendChangesSinceDate (dateLastUpdated_: NSDate); message 'sendChangesSinceDate:';
    function addFeed (feed: PSFeed): ObjCBOOL; message 'addFeed:';
    function addFeedWithURL (url: NSURL): PSFeed; message 'addFeedWithURL:';
    function removeFeed (feed: PSFeed): ObjCBOOL; message 'removeFeed:';
    function feedWithURL (url: NSURL): PSFeed; message 'feedWithURL:';
    function feeds: NSArray; message 'feeds';
    function feedWithIdentifier (identifier: NSString): PSFeed; message 'feedWithIdentifier:';
    function entryWithIdentifier (identifier: NSString): PSEntry; message 'entryWithIdentifier:';
    function signature: NSString; message 'signature';
    procedure setPrivate(newValue: ObjCBOOL); message 'setPrivate:';
    function isPrivate: ObjCBOOL; message 'isPrivate';
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
  end;


type
  PSClientDelegate = objccategory external (NSObject)
    procedure feedDidBeginRefresh (feed: PSFeed); message 'feedDidBeginRefresh:';
    procedure feedDidEndRefresh (feed: PSFeed); message 'feedDidEndRefresh:';
    procedure feed_didAddEntries (feed: PSFeed; entries: NSArray); message 'feed:didAddEntries:';
    procedure feed_didRemoveEntriesWithIdentifiers (feed: PSFeed; identifiers: NSArray); message 'feed:didRemoveEntriesWithIdentifiers:';
    procedure feed_didUpdateEntries (feed: PSFeed; entries: NSArray); message 'feed:didUpdateEntries:';
    procedure feed_didChangeFlagsInEntries (feed: PSFeed; entries: NSArray); message 'feed:didChangeFlagsInEntries:';
    procedure enclosure_downloadStateDidChange (enclosure: PSEnclosure; state: PSEnclosureDownloadState); message 'enclosure:downloadStateDidChange:';
  end;
{$endif}

{$endif}
