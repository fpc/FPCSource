{ Parsed from PubSub.framework PSFeed.h }


{$ifdef TYPES}
type
  PSFeedPtr = ^PSFeed;
{$endif}

{$if (MAC_OS_X_VERSION_10_5 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
{$ifdef EXTERNAL_SYMBOLS}
var
  PSErrorDomain: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  PSInternalError = 1 deprecated 'in 10_5, 10_9, NA, NA';
  PSNotAFeedError = 2 deprecated 'in 10_5, 10_9, NA, NA';
const
  PSUnknownFormat = 0 deprecated 'in 10_5, 10_9, NA, NA';
  PSRSSFormat = 1 deprecated 'in 10_5, 10_9, NA, NA';
  PSAtomFormat = 2 deprecated 'in 10_5, 10_9, NA, NA';
type
  PSFeedFormat = clong;
{$endif}

{$ifdef CLASSES}

type
  PSFeed = objcclass external (NSObject)
  private
    _internal: id;
  public
    function initWithURL (url: NSURL): id; message 'initWithURL:';
    function initWithData_URL (data: NSData; aURL: NSURL): id; message 'initWithData:URL:';
    function client: PSClient; message 'client';
    function identifier: NSString; message 'identifier';
    function title: NSString; message 'title';
    function subtitle: NSString; message 'subtitle';
    function rights: NSString; message 'rights';
    function dateUpdated: NSDate; message 'dateUpdated';
    function timeZoneUpdated: NSTimeZone; message 'timeZoneUpdated';
    function URL: NSURL; message 'URL';
    function alternateURL: NSURL; message 'alternateURL';
    function iconURL: NSURL; message 'iconURL';
    function logoURL: NSURL; message 'logoURL';
    function redirectedURL: NSURL; message 'redirectedURL';
    function localDateUpdated: NSDate; message 'localDateUpdated';
    procedure setLogin(newValue: NSString); message 'setLogin:';
    function login: NSString; message 'login';
    function feedFormat: cint; message 'feedFormat';
    function isRefreshing: ObjCBOOL; message 'isRefreshing';
    function refresh (error: NSErrorPtr): ObjCBOOL; message 'refresh:';
    function lastError: NSError; message 'lastError';
    procedure setSettings(newValue: PSFeedSettings); message 'setSettings:';
    function settings: PSFeedSettings; message 'settings';
    function links: NSArray; message 'links';
    procedure setPassword (password: NSString); message 'setPassword:';
    function entryEnumeratorSortedBy (sortDescriptors: NSArray): NSEnumerator; message 'entryEnumeratorSortedBy:';
    function unreadCount: cint; message 'unreadCount';
    function entries: NSArray; message 'entries';
    function XMLRepresentation: NSXMLElement; message 'XMLRepresentation';
    function XMLRepresentationWithEntries (withEntries: ObjCBOOL): NSXMLElement; message 'XMLRepresentationWithEntries:';
    function extensionXMLElementsUsingNamespace (nameSpace: NSString): NSArray; message 'extensionXMLElementsUsingNamespace:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  PSFeedRefreshingNotification: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
  PSFeedEntriesChangedNotification: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
  PSFeedAddedEntriesKey: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
  PSFeedRemovedEntriesKey: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
  PSFeedUpdatedEntriesKey: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
  PSFeedDidChangeEntryFlagsKey: NSString deprecated 'in 10_5, 10_9, NA, NA'; cvar; external;
{$endif}

{$endif}
