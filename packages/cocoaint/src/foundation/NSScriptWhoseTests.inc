{ Parsed from Foundation.framework NSScriptWhoseTests.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTWHOSETESTS_PAS_H}
{$define NSSCRIPTWHOSETESTS_PAS_H}
type
  NSScriptWhoseTestPointer = Pointer;
  NSLogicalTestPointer = Pointer;
  NSSpecifierTestPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCRIPTWHOSETESTS_PAS_T}
{$define NSSCRIPTWHOSETESTS_PAS_T}

{ Constants }

const
  NSEqualToComparison = 0;
  NSLessThanOrEqualToComparison = 0;
  NSLessThanComparison = 1;
  NSGreaterThanOrEqualToComparison = 2;
  NSGreaterThanComparison = 3;
  NSBeginsWithComparison = 4;
  NSEndsWithComparison = 5;
  NSContainsComparison = 6;

{ Types }
type
  NSTestComparisonOperation = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTWHOSETESTS_PAS_R}
{$define NSSCRIPTWHOSETESTS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTWHOSETESTS_PAS_F}
{$define NSSCRIPTWHOSETESTS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTWHOSETESTS_PAS_T}
{$define NSSCRIPTWHOSETESTS_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptWhoseTest = objcclass;
  NSLogicalTest = objcclass;
  NSSpecifierTest = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTWHOSETESTS_PAS_C}
{$define NSSCRIPTWHOSETESTS_PAS_C}

{ NSScriptWhoseTest }
  NSScriptWhoseTest = objcclass(NSObject, NSCodingProtocol)
    
  public
    class function alloc: NSScriptWhoseTest; message 'alloc';

    function isTrue: Boolean; message 'isTrue';
  end; external;

{ NSLogicalTest }
  NSLogicalTest = objcclass(NSScriptWhoseTest)
  private
    __operator: cint;
    __subTests: id;
    
  public
    class function alloc: NSLogicalTest; message 'alloc';

    function initAndTestWithTests(subTests: NSArray): id; message 'initAndTestWithTests:';
    function initOrTestWithTests(subTests: NSArray): id; message 'initOrTestWithTests:';
    function initNotTestWithTest(subTest: NSScriptWhoseTest): id; message 'initNotTestWithTest:';
  end; external;

{ NSSpecifierTest }
  NSSpecifierTest = objcclass(NSScriptWhoseTest)
  private
    __comparisonOperator: NSTestComparisonOperation;
    __object1: NSScriptObjectSpecifier;
    __object2: id;
    
  public
    class function alloc: NSSpecifierTest; message 'alloc';

    function initWithObjectSpecifier_comparisonOperator_testObject(obj: NSScriptObjectSpecifier; compOp: NSTestComparisonOperation; obj1: id): id; message 'initWithObjectSpecifier:comparisonOperator:testObject:';
  end; external;

{$endif}
{$endif}
