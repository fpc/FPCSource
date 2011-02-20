{ Parsed from Webkit.framework DOMImplementation.h }

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
  DOMImplementationPtr = DOMImplementationPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMIMPLEMENTATION_PAS_C}
{$define DOMIMPLEMENTATION_PAS_C}

{ DOMImplementation }
  DOMImplementation = objcclass external (DOMObject)
    
  public
    function hasFeature_version(feature: NSString; version_: NSString): Boolean; message 'hasFeature:version:';
    function createDocumentType_publicId_systemId(qualifiedName: NSString; publicId: NSString; systemId: NSString): DOMDocumentType; message 'createDocumentType:publicId:systemId:';
    function createDocument_qualifiedName_doctype(namespaceURI: NSString; qualifiedName: NSString; doctype: DOMDocumentType): DOMDocument; message 'createDocument:qualifiedName:doctype:';
    function createCSSStyleSheet_media(title: NSString; media: NSString): DOMCSSStyleSheet; message 'createCSSStyleSheet:media:';
    function createHTMLDocument(title: NSString): DOMHTMLDocument; message 'createHTMLDocument:';
  end;

{ DOMImplementationDeprecatedCategory }
  DOMImplementationDeprecatedCategory = objccategory external (DOMImplementation)
    function hasFeature(feature: NSString; version_: NSString): Boolean; message 'hasFeature:feature:'; deprecated 'in_webkit_version_3 and later';
    function createDocumentType(qualifiedName: NSString; publicId: NSString; systemId: NSString): DOMDocumentType; message 'createDocumentType:qualifiedName:publicId:'; deprecated 'in_webkit_version_3 and later';
    function createDocument(namespaceURI: NSString; qualifiedName: NSString; doctype: DOMDocumentType): DOMDocument; message 'createDocument:namespaceURI:qualifiedName:'; deprecated 'in_webkit_version_3 and later';
    function createCSSStyleSheet(title: NSString; media: NSString): DOMCSSStyleSheet; message 'createCSSStyleSheet:title:'; deprecated 'in_webkit_version_3 and later';
  end;

{$endif}
{$endif}
