{ Parsed from Webkit.framework DOMHTMLOptionsCollection.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLOPTIONSCOLLECTION_PAS_T}
{$define DOMHTMLOPTIONSCOLLECTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLOPTIONSCOLLECTION_PAS_R}
{$define DOMHTMLOPTIONSCOLLECTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLOPTIONSCOLLECTION_PAS_F}
{$define DOMHTMLOPTIONSCOLLECTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLOPTIONSCOLLECTION_PAS_S}
{$define DOMHTMLOPTIONSCOLLECTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLOptionsCollection = objcclass;
  DOMHTMLOptionsCollectionPointer = ^DOMHTMLOptionsCollection;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOPTIONSCOLLECTION_PAS_C}
{$define DOMHTMLOPTIONSCOLLECTION_PAS_C}

{ DOMHTMLOptionsCollection }
  DOMHTMLOptionsCollection = objcclass(DOMObject)
    
  public
    class function alloc: DOMHTMLOptionsCollection; message 'alloc';

    function item(index: cuint): DOMNode; message 'item:';
    function namedItem(name: NSStringPointer): DOMNode; message 'namedItem:';
  end; external;

{$endif}
{$endif}
