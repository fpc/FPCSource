{ Parsed from Webkit.framework DOMNodeIterator.h }

{$ifdef TYPES}
{$ifndef DOMNODEITERATOR_PAS_T}
{$define DOMNODEITERATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMNODEITERATOR_PAS_R}
{$define DOMNODEITERATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMNODEITERATOR_PAS_F}
{$define DOMNODEITERATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMNODEITERATOR_PAS_S}
{$define DOMNODEITERATOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMNodeIterator = objcclass;
  DOMNodeIteratorPointer = ^DOMNodeIterator;
  DOMNodeIteratorPtr = DOMNodeIteratorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNODEITERATOR_PAS_C}
{$define DOMNODEITERATOR_PAS_C}

{ DOMNodeIterator }
  DOMNodeIterator = objcclass external (DOMObject)
    
  public
    function root: DOMNode; message 'root';
    function whatToShow: cuint; message 'whatToShow';
    function filter: id; message 'filter';
    function expandEntityReferences: Boolean; message 'expandEntityReferences';
    function referenceNode: DOMNode; message 'referenceNode';
    function pointerBeforeReferenceNode: Boolean; message 'pointerBeforeReferenceNode';
    function nextNode: DOMNode; message 'nextNode';
    function previousNode: DOMNode; message 'previousNode';
    procedure detach; message 'detach';
  end;

{$endif}
{$endif}
