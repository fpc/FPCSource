{ Parsed from Webkit.framework DOMTreeWalker.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:52 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMTREEWALKER_PAS_T}
{$define DOMTREEWALKER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMTREEWALKER_PAS_R}
{$define DOMTREEWALKER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMTREEWALKER_PAS_F}
{$define DOMTREEWALKER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMTREEWALKER_PAS_S}
{$define DOMTREEWALKER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMTreeWalker = objcclass;
  DOMTreeWalkerPointer = ^DOMTreeWalker;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMTREEWALKER_PAS_C}
{$define DOMTREEWALKER_PAS_C}

{ DOMTreeWalker }
  DOMTreeWalker = objcclass(DOMObject)
  private
    _m_filter: id;
    
  public
    class function alloc: DOMTreeWalker; message 'alloc';

    function root: DOMNode; message 'root';
    function whatToShow: cuint; message 'whatToShow';
    function filter: id; message 'filter';
    function expandEntityReferences: Boolean; message 'expandEntityReferences';
    procedure setCurrentNode (newValue: DOMNode); message 'setCurrentNode:';
    function currentNode: DOMNode; message 'currentNode';
    function parentNode: DOMNode; message 'parentNode';
    function firstChild: DOMNode; message 'firstChild';
    function lastChild: DOMNode; message 'lastChild';
    function previousSibling: DOMNode; message 'previousSibling';
    function nextSibling: DOMNode; message 'nextSibling';
    function previousNode: DOMNode; message 'previousNode';
    function nextNode: DOMNode; message 'nextNode';
  end; external;

{$endif}
{$endif}
