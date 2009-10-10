{ Parsed from Webkit.framework DOMNodeIterator.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMNODEITERATOR_PAS_C}
{$define DOMNODEITERATOR_PAS_C}

{ DOMNodeIterator }
  DOMNodeIterator = objcclass(DOMObject)
  private
    _m_filter: id;
    
  public
    class function alloc: DOMNodeIterator; message 'alloc';

    function root: DOMNode; message 'root';
    function whatToShow: cuint; message 'whatToShow';
    function filter: id; message 'filter';
    function expandEntityReferences: Boolean; message 'expandEntityReferences';
    function nextNode: DOMNode; message 'nextNode';
    function previousNode: DOMNode; message 'previousNode';
    procedure detach; message 'detach';
  end; external;

{$endif}
{$endif}
