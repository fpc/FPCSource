{ Parsed from Webkit.framework DOMHTMLCollection.h }

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
  DOMHTMLCollectionPtr = DOMHTMLCollectionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLCOLLECTION_PAS_C}
{$define DOMHTMLCOLLECTION_PAS_C}

{ DOMHTMLCollection }
  DOMHTMLCollection = objcclass external (DOMObject)
    
  public
    function length: cuint; message 'length';
    function item(index: cuint): DOMNode; message 'item:';
    function namedItem(name: NSString): DOMNode; message 'namedItem:';
    function tags(name: NSString): DOMNodeList; message 'tags:';
  end;

{$endif}
{$endif}
