{ Parsed from Webkit.framework DOMHTMLOptionsCollection.h }

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
  DOMHTMLOptionsCollectionPtr = DOMHTMLOptionsCollectionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMHTMLOPTIONSCOLLECTION_PAS_C}
{$define DOMHTMLOPTIONSCOLLECTION_PAS_C}

{ DOMHTMLOptionsCollection }
  DOMHTMLOptionsCollection = objcclass external (DOMObject)
    
  public
    procedure setSelectedIndex (newValue: cint); message 'setSelectedIndex:';
    function selectedIndex: cint; message 'selectedIndex';
    procedure setLength (newValue: cuint); message 'setLength:';
    function length: cuint; message 'length';
    procedure add_index(option: DOMHTMLOptionElement; index: cuint); message 'add:index:';
    procedure remove(index: cuint); message 'remove:';
    function item(index: cuint): DOMNode; message 'item:';
    function namedItem(name: NSString): DOMNode; message 'namedItem:';
  end;

{$endif}
{$endif}
