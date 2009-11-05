{ Parsed from Webkit.framework DOMXPathExpression.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 10 8:19:00 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef DOMXPATHEXPRESSION_PAS_C}
{$define DOMXPATHEXPRESSION_PAS_C}

{ DOMXPathExpression }
  DOMXPathExpression = objcclass(DOMObject)
    
  public
    class function alloc: DOMXPathExpression; message 'alloc';

    function evaluate_type_inResult(contextNode: DOMNode; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:type:inResult:';

    { Category: DOMXPathExpressionDeprecated }
    function evaluate(contextNode: DOMNode; type_: cushort; inResult: DOMXPathResult): DOMXPathResult; message 'evaluate:contextNode:type:';
  end; external;

{$endif}
{$endif}
