{ Parsed from WebKit.framework DOMXPathResult.h }


{$ifdef TYPES}
type
  DOMXPathResultPtr = ^DOMXPathResult;
{$endif}

{$ifdef TYPES}
const
  DOM_ANY_TYPE = 0;
  DOM_NUMBER_TYPE = 1;
  DOM_STRING_TYPE = 2;
  DOM_BOOLEAN_TYPE = 3;
  DOM_UNORDERED_NODE_ITERATOR_TYPE = 4;
  DOM_ORDERED_NODE_ITERATOR_TYPE = 5;
  DOM_UNORDERED_NODE_SNAPSHOT_TYPE = 6;
  DOM_ORDERED_NODE_SNAPSHOT_TYPE = 7;
  DOM_ANY_UNORDERED_NODE_TYPE = 8;
  DOM_FIRST_ORDERED_NODE_TYPE = 9;
{$endif}

{$ifdef CLASSES}

type
  DOMXPathResult = objcclass external (DOMObject)
  public
    function resultType: cushort; message 'resultType';
    function numberValue: double; message 'numberValue';
    function stringValue: NSString; message 'stringValue';
    function ObjCBOOLValue: ObjCBOOL; message 'ObjCBOOLValue';
    function singleNodeValue: DOMNode; message 'singleNodeValue';
    function invalidIteratorState: ObjCBOOL; message 'invalidIteratorState';
    function snapshotLength: cuint; message 'snapshotLength';
    function iterateNext: DOMNode; message 'iterateNext';
    function snapshotItem (index: cuint): DOMNode; message 'snapshotItem:';
  end;
{$endif}

