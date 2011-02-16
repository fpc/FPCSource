{ Parsed from Webkit.framework DOMNodeList.h }

{$ifdef TYPES}
{$ifndef DOMNODELIST_PAS_T}
{$define DOMNODELIST_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMNODELIST_PAS_R}
{$define DOMNODELIST_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMNODELIST_PAS_F}
{$define DOMNODELIST_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMNODELIST_PAS_S}
{$define DOMNODELIST_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMNodeList = objcclass;
  DOMNodeListPointer = ^DOMNodeList;
  DOMNodeListPtr = DOMNodeListPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNODELIST_PAS_C}
{$define DOMNODELIST_PAS_C}

{ DOMNodeList }
  DOMNodeList = objcclass external (DOMObject)
    
  public
    function length: cuint; message 'length';
    function item(index: cuint): DOMNode; message 'item:';
  end;

{$endif}
{$endif}
