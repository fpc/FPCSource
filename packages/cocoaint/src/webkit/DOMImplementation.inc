{ Parsed from Webkit.framework DOMImplementation.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:18:59 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMIMPLEMENTATION_PAS_T}
{$define DOMIMPLEMENTATION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMIMPLEMENTATION_PAS_R}
{$define DOMIMPLEMENTATION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMIMPLEMENTATION_PAS_F}
{$define DOMIMPLEMENTATION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMIMPLEMENTATION_PAS_S}
{$define DOMIMPLEMENTATION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMImplementation = objcclass;
  DOMImplementationPointer = ^DOMImplementation;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMIMPLEMENTATION_PAS_C}
{$define DOMIMPLEMENTATION_PAS_C}

{ DOMImplementation }
  DOMImplementation = objcclass(DOMObject)
    
  public
    class function alloc: DOMImplementation; message 'alloc';

    function hasFeature_version(feature: NSString; version_: NSString): Boolean; message 'hasFeature:version:';
    function createDocumentType_publicId_systemId(qualifiedName: NSString; publicId: NSString; systemId: NSString): DOMDocumentType; message 'createDocumentType:publicId:systemId:';
    function createDocument_qualifiedName_doctype(namespaceURI: NSString; qualifiedName: NSString; doctype: DOMDocumentType): DOMDocument; message 'createDocument:qualifiedName:doctype:';
    function createCSSStyleSheet_media(title: NSString; media: NSString): DOMCSSStyleSheet; message 'createCSSStyleSheet:media:';
    function createHTMLDocument(title: NSString): DOMHTMLDocument; message 'createHTMLDocument:';

    { Category: DOMImplementationDeprecated }
    function hasFeature(feature: NSString; version_: NSString): Boolean; message 'hasFeature:feature:';
    function createDocumentType(qualifiedName: NSString; publicId: NSString; systemId: NSString): DOMDocumentType; message 'createDocumentType:qualifiedName:publicId:';
    function createDocument(namespaceURI: NSString; qualifiedName: NSString; doctype: DOMDocumentType): DOMDocument; message 'createDocument:namespaceURI:qualifiedName:';
    function createCSSStyleSheet(title: NSString; media: NSString): DOMCSSStyleSheet; message 'createCSSStyleSheet:title:';
  end; external;

{$endif}
{$endif}
