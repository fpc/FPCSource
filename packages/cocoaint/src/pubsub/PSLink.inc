{ Parsed from PubSub.framework PSLink.h }


{$ifdef TYPES}
type
  PSLinkPtr = ^PSLink;
{$endif}

{$if (MAC_OS_X_VERSION_10_5 <= MAC_OS_X_VERSION_MAX_ALLOWED)}
{$ifdef TYPES}
const
  PSLinkToOther = 0 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToRSS = 1 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToAtom = 2 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToAtomService = 3 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToFOAF = 4 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToRSD = 5 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToSelf = 6 deprecated 'in 10_5, 10_9, NA, NA';
  PSLinkToAlternate = 7 deprecated 'in 10_5, 10_9, NA, NA';
type
  PSLinkKind = clong;
{$endif}

{$ifdef CLASSES}

type
  PSLink = objcclass external (NSObject)
  private
    _linkKind: PSLinkKind;
    _relations: NSSet;
    _MIMEType: NSString;
    _title: NSString;
    _href: NSString;
    _baseURL: NSURL;
  public
    class function linksInXML_baseURL (root: NSXMLElement; baseURL: NSURL): NSArray; message 'linksInXML:baseURL:';
    function linkKind: PSLinkKind; message 'linkKind';
    function URL: NSURL; message 'URL';
    function relations: NSSet; message 'relations';
    function title: NSString; message 'title';
    function MIMEType: NSString; message 'MIMEType';
  end;
{$endif}

{$endif}
