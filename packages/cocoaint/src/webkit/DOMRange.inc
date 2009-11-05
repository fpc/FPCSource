{ Parsed from Webkit.framework DOMRange.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


{$ifdef TYPES}
{$ifndef DOMRANGE_PAS_T}
{$define DOMRANGE_PAS_T}

{ Constants }

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
{$endif}

{$ifdef RECORDS}
{$ifndef DOMRANGE_PAS_R}
{$define DOMRANGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMRANGE_PAS_F}
{$define DOMRANGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMRANGE_PAS_S}
{$define DOMRANGE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMRange = objcclass;
  DOMRangePointer = ^DOMRange;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMRANGE_PAS_C}
{$define DOMRANGE_PAS_C}

{ DOMRange }
  DOMRange = objcclass(DOMObject)
    
  public
    class function alloc: DOMRange; message 'alloc';

    function startContainer: DOMNode; message 'startContainer';
    function startOffset: cint; message 'startOffset';
    function endContainer: DOMNode; message 'endContainer';
    function endOffset: cint; message 'endOffset';
    function collapsed: Boolean; message 'collapsed';
    function commonAncestorContainer: DOMNode; message 'commonAncestorContainer';
    function text: NSString; message 'text';
    procedure setStart_offset(refNode: DOMNode; offset: cint); message 'setStart:offset:';
    procedure setEnd_offset(refNode: DOMNode; offset: cint); message 'setEnd:offset:';
    procedure setStartBefore(refNode: DOMNode); message 'setStartBefore:';
    procedure setStartAfter(refNode: DOMNode); message 'setStartAfter:';
    procedure setEndBefore(refNode: DOMNode); message 'setEndBefore:';
    procedure setEndAfter(refNode: DOMNode); message 'setEndAfter:';
    procedure collapse(toStart: Boolean); message 'collapse:';
    procedure selectNode(refNode: DOMNode); message 'selectNode:';
    procedure selectNodeContents(refNode: DOMNode); message 'selectNodeContents:';
    function compareBoundaryPoints_sourceRange(how: cushort; sourceRange: DOMRange): cshort; message 'compareBoundaryPoints:sourceRange:';
    procedure deleteContents; message 'deleteContents';
    function extractContents: DOMDocumentFragment; message 'extractContents';
    function cloneContents: DOMDocumentFragment; message 'cloneContents';
    procedure insertNode(newNode: DOMNode); message 'insertNode:';
    procedure surroundContents(newParent: DOMNode); message 'surroundContents:';
    function cloneRange: DOMRange; message 'cloneRange';
    function toString: NSString; message 'toString';
    procedure detach; message 'detach';

    { Category: DOMRangeDeprecated }
    procedure setStart(refNode: DOMNode; offset: cint); message 'setStart:refNode:';
    procedure setEnd(refNode: DOMNode; offset: cint); message 'setEnd:refNode:';
    function compareBoundaryPoints(how: cushort; sourceRange: DOMRange): cshort; message 'compareBoundaryPoints:how:';
  end; external;

{$endif}
{$endif}
