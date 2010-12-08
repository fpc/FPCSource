{ Parsed from Webkit.framework DOMNodeIterator.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  DOMNodeIterator = objcclass(DOMObject)
    
  public
    class function alloc: DOMNodeIterator; message 'alloc';

    function root: DOMNode; message 'root';
    function whatToShow: cuint; message 'whatToShow';
    function filter: id; message 'filter';
    function expandEntityReferences: Boolean; message 'expandEntityReferences';
    function referenceNode: DOMNode; message 'referenceNode';
    function pointerBeforeReferenceNode: Boolean; message 'pointerBeforeReferenceNode';
    function nextNode: DOMNode; message 'nextNode';
    function previousNode: DOMNode; message 'previousNode';
    procedure detach; message 'detach';
  end; external;

{$endif}
{$endif}
