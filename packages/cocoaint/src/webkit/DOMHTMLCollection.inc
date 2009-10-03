{ Parsed from Webkit.framework DOMHTMLCollection.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMHTMLCOLLECTION_PAS_T}
{$define DOMHTMLCOLLECTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMHTMLCOLLECTION_PAS_R}
{$define DOMHTMLCOLLECTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMHTMLCOLLECTION_PAS_F}
{$define DOMHTMLCOLLECTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMHTMLCOLLECTION_PAS_S}
{$define DOMHTMLCOLLECTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMHTMLCollection = objcclass;
  DOMHTMLCollectionPointer = ^DOMHTMLCollection;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLCOLLECTION_PAS_C}
{$define DOMHTMLCOLLECTION_PAS_C}

{ DOMHTMLCollection }
  DOMHTMLCollection = objcclass(DOMObject)
    
  public
    class function alloc: DOMHTMLCollection; message 'alloc';

    function length: cuint; message 'length';
    function item(index: cuint): DOMNode; message 'item:';
    function namedItem(name: NSStringPointer): DOMNode; message 'namedItem:';
  end; external;

{$endif}
{$endif}
