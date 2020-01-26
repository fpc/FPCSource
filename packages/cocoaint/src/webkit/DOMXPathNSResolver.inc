{ Parsed from WebKit.framework DOMXPathNSResolver.h }


{$ifdef TYPES}
type
  DOMXPathNSResolverProtocolPtr = ^DOMXPathNSResolverProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  DOMXPathNSResolverProtocol = objcprotocol external name 'DOMXPathNSResolver' (NSObjectProtocol)
    function lookupNamespaceURI (prefix: NSString): NSString; message 'lookupNamespaceURI:';
  end;
{$endif}

