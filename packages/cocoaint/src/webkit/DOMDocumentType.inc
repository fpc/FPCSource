{ Parsed from Webkit.framework DOMDocumentType.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef DOMDOCUMENTTYPE_PAS_T}
{$define DOMDOCUMENTTYPE_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMDOCUMENTTYPE_PAS_R}
{$define DOMDOCUMENTTYPE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMDOCUMENTTYPE_PAS_F}
{$define DOMDOCUMENTTYPE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMDOCUMENTTYPE_PAS_S}
{$define DOMDOCUMENTTYPE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMDocumentType = objcclass;
  DOMDocumentTypePointer = ^DOMDocumentType;
  DOMDocumentTypePtr = DOMDocumentTypePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMDOCUMENTTYPE_PAS_C}
{$define DOMDOCUMENTTYPE_PAS_C}

{ DOMDocumentType }
  DOMDocumentType = objcclass(DOMNode)
    
  public
    class function alloc: DOMDocumentType; message 'alloc';

    function name: NSString; message 'name';
    function entities: DOMNamedNodeMap; message 'entities';
    function notations: DOMNamedNodeMap; message 'notations';
    function publicId: NSString; message 'publicId';
    function systemId: NSString; message 'systemId';
    function internalSubset: NSString; message 'internalSubset';
  end; external;

{$endif}
{$endif}
