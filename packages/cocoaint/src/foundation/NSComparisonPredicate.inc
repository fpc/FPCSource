{ Parsed from Foundation.framework NSComparisonPredicate.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOMPARISONPREDICATE_PAS_H}
{$define NSCOMPARISONPREDICATE_PAS_H}
type
  NSComparisonPredicatePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOMPARISONPREDICATE_PAS_T}
{$define NSCOMPARISONPREDICATE_PAS_T}

{ Constants }

const
  NSCaseInsensitivePredicateOption = $01;
  NSDiacriticInsensitivePredicateOption = $02;

const
  NSMatchesPredicateOperatorType = 0;
  NSLikePredicateOperatorType = 1;
  NSBeginsWithPredicateOperatorType = 2;
  NSEndsWithPredicateOperatorType = 3;
  NSCustomSelectorPredicateOperatorType = 4;
  NSContainsPredicateOperatorType = 99;
  NSBetweenPredicateOperatorType = 5;

{ Types }
type
  NSComparisonPredicateModifier = culong;
  NSPredicateOperatorType = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOMPARISONPREDICATE_PAS_R}
{$define NSCOMPARISONPREDICATE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOMPARISONPREDICATE_PAS_F}
{$define NSCOMPARISONPREDICATE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOMPARISONPREDICATE_PAS_T}
{$define NSCOMPARISONPREDICATE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSComparisonPredicate = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMPARISONPREDICATE_PAS_C}
{$define NSCOMPARISONPREDICATE_PAS_C}

{ NSComparisonPredicate }
  NSComparisonPredicate = objcclass(NSPredicate)
  private
    __reserved2: Pointer;
    __predicateOperator: NSPredicateOperator;
    __lhs: NSExpression;
    __rhs: NSExpression;
    
  public
    class function alloc: NSComparisonPredicate; message 'alloc';

    class function predicateWithLeftExpression_rightExpression_modifier_type_options(lhs: NSExpression; rhs: NSExpression; modifier: NSComparisonPredicateModifier; type_: NSPredicateOperatorType; options_: culong): NSPredicate; message 'predicateWithLeftExpression:rightExpression:modifier:type:options:';
    class function predicateWithLeftExpression_rightExpression_customSelector(lhs: NSExpression; rhs: NSExpression; selector: SEL): NSPredicate; message 'predicateWithLeftExpression:rightExpression:customSelector:';
    function initWithLeftExpression_rightExpression_modifier_type_options(lhs: NSExpression; rhs: NSExpression; modifier: NSComparisonPredicateModifier; type_: NSPredicateOperatorType; options_: culong): id; message 'initWithLeftExpression:rightExpression:modifier:type:options:';
    function initWithLeftExpression_rightExpression_customSelector(lhs: NSExpression; rhs: NSExpression; selector: SEL): id; message 'initWithLeftExpression:rightExpression:customSelector:';
    function predicateOperatorType: NSPredicateOperatorType; message 'predicateOperatorType';
    function comparisonPredicateModifier: NSComparisonPredicateModifier; message 'comparisonPredicateModifier';
    function leftExpression: NSExpression; message 'leftExpression';
    function rightExpression: NSExpression; message 'rightExpression';
    function customSelector: SEL; message 'customSelector';
    function options: culong; message 'options';
  end; external;

{$endif}
{$endif}
