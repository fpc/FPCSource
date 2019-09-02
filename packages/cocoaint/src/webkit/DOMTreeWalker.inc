{ Parsed from WebKit.framework DOMTreeWalker.h }


{$ifdef TYPES}
type
  DOMTreeWalkerPtr = ^DOMTreeWalker;
{$endif}

{$ifdef CLASSES}

type
  DOMTreeWalker = objcclass external (DOMObject)
  public
    function root: DOMNode; message 'root';
    function whatToShow: cuint; message 'whatToShow';
    function filter: DOMNodeFilterProtocol; message 'filter';
    function expandEntityReferences: ObjCBOOL; message 'expandEntityReferences';
    procedure setCurrentNode(newValue: DOMNode); message 'setCurrentNode:';
    function currentNode: DOMNode; message 'currentNode';
    function parentNode: DOMNode; message 'parentNode';
    function firstChild: DOMNode; message 'firstChild';
    function lastChild: DOMNode; message 'lastChild';
    function previousSibling: DOMNode; message 'previousSibling';
    function nextSibling: DOMNode; message 'nextSibling';
    function previousNode: DOMNode; message 'previousNode';
    function nextNode: DOMNode; message 'nextNode';
  end;
{$endif}

