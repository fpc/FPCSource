{ Parsed from Foundation.framework NSExpression.h }


{$ifdef TYPES}
type
  NSExpressionPtr = ^NSExpression;
{$endif}

{$ifdef TYPES}
type
  NSExpressionType = NSUInteger;
  NSExpressionTypePtr = ^NSExpressionType;

const
  NSConstantValueExpressionType = 0;
  NSEvaluatedObjectExpressionType = 1;
  NSVariableExpressionType = 2;
  NSKeyPathExpressionType = 3;
  NSFunctionExpressionType = 4;
  NSUnionSetExpressionType = 5 { available in 10_5, 3_0 };
  NSIntersectSetExpressionType = 6 { available in 10_5, 3_0 };
  NSMinusSetExpressionType = 7 { available in 10_5, 3_0 };
  NSSubqueryExpressionType = 13 { available in 10_5, 3_0 };
  NSAggregateExpressionType = 14 { available in 10_5, 3_0 };
  NSAnyKeyExpressionType = 15 { available in 10_9, 7_0 };
  NSBlockExpressionType = 19;
{$endif}

{$ifdef CLASSES}

type
  NSExpression = objcclass external (NSObject, NSSecureCodingProtocol, NSCopyingProtocol)
  private
    _expressionFlags: bitpacked record
      case byte of
        0: (_anonBitField__expressionFlags0: cuint);
        1: (
          _evaluationBlocked: 0..1;
          _reservedExpressionFlags: 0..((1 shl 31)-1);
        );
      end;
    {$ifdef __LP64__}
    reserved: cuint32;
    {$endif}
    _expressionType: NSExpressionType;
  public
    class function expressionWithFormat_argumentArray (expressionFormat: NSString; arguments: NSArray): NSExpression; message 'expressionWithFormat:argumentArray:'; { available in 10_6,4_0 }
    class function expressionWithFormat (expressionFormat: NSString): NSExpression; varargs; message 'expressionWithFormat:'; { available in 10_6,4_0 }
    class function expressionWithFormat_arguments (expressionFormat: NSString; argList: va_list): NSExpression; message 'expressionWithFormat:arguments:'; { available in 10_6,4_0 }
    class function expressionForConstantValue (obj: id): NSExpression; message 'expressionForConstantValue:';
    class function expressionForEvaluatedObject: NSExpression; message 'expressionForEvaluatedObject';
    class function expressionForVariable (string_: NSString): NSExpression; message 'expressionForVariable:';
    class function expressionForKeyPath (keyPath: NSString): NSExpression; message 'expressionForKeyPath:';
    class function expressionForFunction_arguments (name: NSString; parameters: NSArray): NSExpression; message 'expressionForFunction:arguments:';
    class function expressionForAggregate (subexpressions: NSArray): NSExpression; message 'expressionForAggregate:'; { available in 10_5, 3_0 }
    class function expressionForUnionSet_with (left: NSExpression; right: NSExpression): NSExpression; message 'expressionForUnionSet:with:'; { available in 10_5, 3_0 }
    class function expressionForIntersectSet_with (left: NSExpression; right: NSExpression): NSExpression; message 'expressionForIntersectSet:with:'; { available in 10_5, 3_0 }
    class function expressionForMinusSet_with (left: NSExpression; right: NSExpression): NSExpression; message 'expressionForMinusSet:with:'; { available in 10_5, 3_0 }
    class function expressionForSubquery_usingIteratorVariable_predicate (expression: NSExpression; variable: NSString; predicate: id): NSExpression; message 'expressionForSubquery:usingIteratorVariable:predicate:'; { available in 10_5, 3_0 }
    class function expressionForFunction_selectorName_arguments (target: NSExpression; name: NSString; parameters: NSArray): NSExpression; message 'expressionForFunction:selectorName:arguments:'; { available in 10_5, 3_0 }
    class function expressionForAnyKey: NSExpression; message 'expressionForAnyKey'; { available in 10_9, 7_0 }
    class function expressionForBlock_arguments (block: OpaqueCBlock; arguments: NSArray): NSExpression; message 'expressionForBlock:arguments:'; { available in 10_6, 4_0 }
    function initWithExpressionType (type_: NSExpressionType): instancetype; message 'initWithExpressionType:';
    function expressionType: NSExpressionType; message 'expressionType';
    function constantValue: id; message 'constantValue';
    function keyPath: NSString; message 'keyPath';
    function function_: NSString; message 'function';
    function variable: NSString; message 'variable';
    function operand: NSExpression; message 'operand';
    function arguments: NSArray; message 'arguments';
    function collection: id; message 'collection';
    function predicate: NSPredicate; message 'predicate';
    function leftExpression: NSExpression; message 'leftExpression';
    function rightExpression: NSExpression; message 'rightExpression';
    procedure setExpressionBlock(newValue: OpaqueCBlock); message 'setExpressionBlock:';
    function expressionBlock: OpaqueCBlock; message 'expressionBlock';
    function expressionValueWithObject_context (object_: id; context: NSMutableDictionary): id; message 'expressionValueWithObject:context:';
    procedure allowEvaluation; message 'allowEvaluation'; { available in 10_9, 7_0 }

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
  end;
{$endif}

