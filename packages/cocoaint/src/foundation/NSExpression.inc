{ Parsed from Foundation.framework NSExpression.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:52 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSEXPRESSION_PAS_H}
{$define NSEXPRESSION_PAS_H}
type
  NSExpressionPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSEXPRESSION_PAS_T}
{$define NSEXPRESSION_PAS_T}

{ Constants }

const
  NSConstantValueExpressionType = 0;
  NSEvaluatedObjectExpressionType = 0;
  NSVariableExpressionType = 1;
  NSKeyPathExpressionType = 2;
  NSFunctionExpressionType = 3;
  NSUnionSetExpressionType = 4;
  NSIntersectSetExpressionType = 5;
  NSMinusSetExpressionType = 6;
  NSSubqueryExpressionType = 13;
  NSAggregateExpressionType = 7;

{ Types }
type
  NSExpressionType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSEXPRESSION_PAS_R}
{$define NSEXPRESSION_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSEXPRESSION_PAS_F}
{$define NSEXPRESSION_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEXPRESSION_PAS_S}
{$define NSEXPRESSION_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSExpression = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEXPRESSION_PAS_C}
{$define NSEXPRESSION_PAS_C}

{ NSExpression }
  NSExpression = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __reserved: Pointer;
    __expressionType: NSExpressionType;
    
  public
    class function alloc: NSExpression; message 'alloc';

    class function expressionForConstantValue(obj: id): NSExpression; message 'expressionForConstantValue:';
    class function expressionForEvaluatedObject: NSExpression; message 'expressionForEvaluatedObject';
    class function expressionForVariable(string_: NSString): NSExpression; message 'expressionForVariable:';
    class function expressionForKeyPath(keyPath_: NSString): NSExpression; message 'expressionForKeyPath:';
    class function expressionForFunction_arguments(name: NSString; parameters: NSArray): NSExpression; message 'expressionForFunction:arguments:';
    class function expressionForAggregate(subexpressions: NSArray): NSExpression; message 'expressionForAggregate:';
    class function expressionForUnionSet_with(left: NSExpression; right: NSExpression): NSExpression; message 'expressionForUnionSet:with:';
    class function expressionForIntersectSet_with(left: NSExpression; right: NSExpression): NSExpression; message 'expressionForIntersectSet:with:';
    class function expressionForMinusSet_with(left: NSExpression; right: NSExpression): NSExpression; message 'expressionForMinusSet:with:';
    class function expressionForSubquery_usingIteratorVariable_predicate(expression: NSExpression; variable_: NSString; predicate_: id): NSExpression; message 'expressionForSubquery:usingIteratorVariable:predicate:';
    class function expressionForFunction_selectorName_arguments(target: NSExpression; name: NSString; parameters: NSArray): NSExpression; message 'expressionForFunction:selectorName:arguments:';
    function initWithExpressionType(type_: NSExpressionType): id; message 'initWithExpressionType:';
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
    function expressionValueWithObject_context(object_: id; context: NSMutableDictionary): id; message 'expressionValueWithObject:context:';
  end; external;

{$endif}
{$endif}
