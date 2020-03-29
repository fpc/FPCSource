{ Parsed from WebKit.framework DOMNodeIterator.h }


{$ifdef TYPES}
type
  DOMNodeIteratorPtr = ^DOMNodeIterator;
{$endif}

{$ifdef CLASSES}

type
  DOMNodeIterator = objcclass external (DOMObject)
  public
    function root: DOMNode; message 'root';
    function whatToShow: cuint; message 'whatToShow';
    function filter: DOMNodeFilterProtocol; message 'filter';
    function expandEntityReferences: ObjCBOOL; message 'expandEntityReferences';
    function referenceNode: DOMNode; message 'referenceNode';
    function pointerBeforeReferenceNode: ObjCBOOL; message 'pointerBeforeReferenceNode';
    function nextNode: DOMNode; message 'nextNode';
    function previousNode: DOMNode; message 'previousNode';
    procedure detach; message 'detach';
  end;
{$endif}

