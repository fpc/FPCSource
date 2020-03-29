{ Parsed from CoreData.framework NSExpressionDescription.h }


{$ifdef TYPES}
type
  NSExpressionDescriptionPtr = ^NSExpressionDescription;
{$endif}

{$ifdef CLASSES}

type
  NSExpressionDescription = objcclass external (NSPropertyDescription)
  private
    _reservedtype1_1: id;
    _reservedtype1_2: id;
    _reservedtype1_3: NSAttributeType;
    _reservedtype1_4: id;
    _reservedtype1_5: NSUInteger;
    _reservedtype1_6: id;
    _reservedtype2_1: pointer;
    _reservedtype2_2: pointer;
    _reservedtype2_3: pointer;
    _expression: NSExpression;
    _expressionResultType: NSAttributeType;
  public
    procedure setExpression(newValue: NSExpression); message 'setExpression:';
    function expression: NSExpression; message 'expression';
    procedure setExpressionResultType(newValue: NSAttributeType); message 'setExpressionResultType:';
    function expressionResultType: NSAttributeType; message 'expressionResultType';
  end;
{$endif}

