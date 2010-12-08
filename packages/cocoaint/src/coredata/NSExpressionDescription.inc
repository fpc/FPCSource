{ Parsed from Coredata.framework NSExpressionDescription.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSEXPRESSIONDESCRIPTION_PAS_T}
{$define NSEXPRESSIONDESCRIPTION_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSEXPRESSIONDESCRIPTION_PAS_R}
{$define NSEXPRESSIONDESCRIPTION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSEXPRESSIONDESCRIPTION_PAS_F}
{$define NSEXPRESSIONDESCRIPTION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEXPRESSIONDESCRIPTION_PAS_S}
{$define NSEXPRESSIONDESCRIPTION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSExpressionDescription = objcclass;
  NSExpressionDescriptionPointer = ^NSExpressionDescription;
  NSExpressionDescriptionPtr = NSExpressionDescriptionPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEXPRESSIONDESCRIPTION_PAS_C}
{$define NSEXPRESSIONDESCRIPTION_PAS_C}

{ NSExpressionDescription }
  NSExpressionDescription = objcclass(NSPropertyDescription)
  private
    _reservedtype1_1: id;
    _reservedtype1_2: id;
    _reservedtype1_3: NSAttributeType;
    _reservedtype1_4: id;
    _reservedtype1_5: NSUInteger;
    _reservedtype1_6: id;
    _reservedtype2_1: Pointer;
    _reservedtype2_2: Pointer;
    _reservedtype2_3: Pointer;
    _expression: NSExpression;
    _expressionResultType: NSAttributeType;
    
  public
    class function alloc: NSExpressionDescription; message 'alloc';

    function expression: NSExpression; message 'expression';
    procedure setExpression(expression_: NSExpression); message 'setExpression:';
    function expressionResultType: NSAttributeType; message 'expressionResultType';
    procedure setExpressionResultType(type_: NSAttributeType); message 'setExpressionResultType:';
  end; external;

{$endif}
{$endif}
