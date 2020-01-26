{ Parsed from WebKit.framework DOMRange.h }


{$ifdef TYPES}
type
  DOMRangePtr = ^DOMRange;
{$endif}

{$ifdef TYPES}
const
  DOM_START_TO_START = 0;
  DOM_START_TO_END = 1;
  DOM_END_TO_END = 2;
  DOM_END_TO_START = 3;
  DOM_NODE_BEFORE = 0;
  DOM_NODE_AFTER = 1;
  DOM_NODE_BEFORE_AND_AFTER = 2;
  DOM_NODE_INSIDE = 3;
{$endif}

{$ifdef CLASSES}

type
  DOMRange = objcclass external (DOMObject)
  public
    function startContainer: DOMNode; message 'startContainer';
    function startOffset: cint; message 'startOffset';
    function endContainer: DOMNode; message 'endContainer';
    function endOffset: cint; message 'endOffset';
    function collapsed: ObjCBOOL; message 'collapsed';
    function commonAncestorContainer: DOMNode; message 'commonAncestorContainer';
    function text: NSString; message 'text';
    procedure setStart_offset (refNode: DOMNode; offset: cint); message 'setStart:offset:'; { available in 10_5 }
    procedure setEnd_offset (refNode: DOMNode; offset: cint); message 'setEnd:offset:'; { available in 10_5 }
    procedure setStartBefore (refNode: DOMNode); message 'setStartBefore:';
    procedure setStartAfter (refNode: DOMNode); message 'setStartAfter:';
    procedure setEndBefore (refNode: DOMNode); message 'setEndBefore:';
    procedure setEndAfter (refNode: DOMNode); message 'setEndAfter:';
    procedure collapse (toStart: ObjCBOOL); message 'collapse:';
    procedure selectNode (refNode: DOMNode); message 'selectNode:';
    procedure selectNodeContents (refNode: DOMNode); message 'selectNodeContents:';
    function compareBoundaryPoints_sourceRange (how: cushort; sourceRange: DOMRange): cshort; message 'compareBoundaryPoints:sourceRange:'; { available in 10_5 }
    procedure deleteContents; message 'deleteContents';
    function extractContents: DOMDocumentFragment; message 'extractContents';
    function cloneContents: DOMDocumentFragment; message 'cloneContents';
    procedure insertNode (newNode: DOMNode); message 'insertNode:';
    procedure surroundContents (newParent: DOMNode); message 'surroundContents:';
    function cloneRange: DOMRange; message 'cloneRange';
    function toString: NSString; message 'toString';
    procedure detach; message 'detach';
    function createContextualFragment (html: NSString): DOMDocumentFragment; message 'createContextualFragment:'; { available in 10_5 }
    function intersectsNode (refNode: DOMNode): ObjCBOOL; message 'intersectsNode:'; { available in 10_5 }
    function compareNode (refNode: DOMNode): cshort; message 'compareNode:'; { available in 10_5 }
    function comparePoint_offset (refNode: DOMNode; offset: cint): cshort; message 'comparePoint:offset:'; { available in 10_5 }
    function isPointInRange_offset (refNode: DOMNode; offset: cint): ObjCBOOL; message 'isPointInRange:offset:'; { available in 10_5 }
  end;


type
  DOMRangeDeprecated = objccategory external (DOMRange)
    procedure setStart (refNode: DOMNode); message 'setStart:'; deprecated 'in 10_4, 10_5';
    procedure setEnd (refNode: DOMNode); message 'setEnd:'; deprecated 'in 10_4, 10_5';
    function compareBoundaryPoints (how: cushort): cshort; message 'compareBoundaryPoints:'; deprecated 'in 10_4, 10_5';
  end;
{$endif}

