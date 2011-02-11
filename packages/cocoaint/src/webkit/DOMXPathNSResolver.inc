{ Parsed from Webkit.framework DOMXPathNSResolver.h }
{ Version: 2.1.5 - Fri Feb 11 21:46:20 CET 2011 }


{$ifdef TYPES}
{$ifndef DOMXPATHNSRESOLVER_PAS_T}
{$define DOMXPATHNSRESOLVER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMXPATHNSRESOLVER_PAS_R}
{$define DOMXPATHNSRESOLVER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMXPATHNSRESOLVER_PAS_F}
{$define DOMXPATHNSRESOLVER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMXPATHNSRESOLVER_PAS_S}
{$define DOMXPATHNSRESOLVER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMXPathNSResolverProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef DOMXPATHNSRESOLVER_PAS_P}
{$define DOMXPATHNSRESOLVER_PAS_P}
  
{ DOMXPathNSResolver Protocol }
  DOMXPathNSResolverProtocol = objcprotocol external name 'DOMXPathNSResolver'
    function lookupNamespaceURI(prefix: NSString): NSString; message 'lookupNamespaceURI:';
  end;
{$endif}
{$endif}
