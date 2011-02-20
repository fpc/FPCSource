{ Parsed from Webkit.framework DOMXPathExpression.h }

{$ifdef TYPES}
{$ifndef DOMXPATHEXPRESSION_PAS_T}
{$define DOMXPATHEXPRESSION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef DOMXPATHEXPRESSION_PAS_R}
{$define DOMXPATHEXPRESSION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef DOMXPATHEXPRESSION_PAS_F}
{$define DOMXPATHEXPRESSION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef DOMXPATHEXPRESSION_PAS_S}
{$define DOMXPATHEXPRESSION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  DOMXPathExpression = objcclass;
  DOMXPathExpressionPointer = ^DOMXPathExpression;
  DOMXPathExpressionPtr = DOMXPathExpressionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef DOMXPATHEXPRESSION_PAS_C}
{$define DOMXPATHEXPRESSION_PAS_C}

{ DOMXPathExpression }
  DOMXPathExpression = objcclass external (DOMObject)
    
  public
    function evaluate_type_inResult(contextNode: DOMNode; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:type:inResult:';
  end;

{ DOMXPathExpressionDeprecatedCategory }
  DOMXPathExpressionDeprecatedCategory = objccategory external (DOMXPathExpression)
    function evaluate(contextNode: DOMNode; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:contextNode:type:';
  end;

{$endif}
{$endif}
