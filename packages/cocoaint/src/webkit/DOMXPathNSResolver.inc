{ Parsed from Webkit.framework DOMXPathNSResolver.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


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
  DOMXPathNSResolverProtocol = objcprotocol
    function lookupNamespaceURI(prefix: NSStringPointer): NSString; message 'lookupNamespaceURI:';
  end; external name 'DOMXPathNSResolver';
{$endif}
{$endif}
