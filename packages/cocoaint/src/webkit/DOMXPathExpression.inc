{ Parsed from WebKit.framework DOMXPathExpression.h }


{$ifdef TYPES}
type
  DOMXPathExpressionPtr = ^DOMXPathExpression;
{$endif}

{$ifdef CLASSES}

type
  DOMXPathExpression = objcclass external (DOMObject)
  public
    function evaluate_type_inResult (contextNode: DOMNode; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:type:inResult:'; { available in 10_5 }
  end;


type
  DOMXPathExpressionDeprecated = objccategory external (DOMXPathExpression)
    function evaluate_type (contextNode: DOMNode; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:type:'; deprecated 'in 10_5, 10_5';
  end;
{$endif}

