{ Parsed from Foundation.framework NSScriptObjectSpecifiers.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_H}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_H}
type
  NSScriptObjectSpecifierPointer = Pointer;
  NSIndexSpecifierPointer = Pointer;
  NSMiddleSpecifierPointer = Pointer;
  NSNameSpecifierPointer = Pointer;
  NSPositionalSpecifierPointer = Pointer;
  NSPropertySpecifierPointer = Pointer;
  NSRandomSpecifierPointer = Pointer;
  NSRangeSpecifierPointer = Pointer;
  NSRelativeSpecifierPointer = Pointer;
  NSUniqueIDSpecifierPointer = Pointer;
  NSWhoseSpecifierPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_T}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_T}

{ Constants }

const
  NSNoSpecifierError = 0;

const
  NSPositionAfter = 0;
  NSPositionBefore = 1;
  NSPositionBeginning = 2;
  NSPositionEnd = 3;
  NSPositionReplace = 4;

const
  NSRelativeAfter = 0;
  NSRelativeBefore = 0;

const
  NSIndexSubelement = 0;
  NSEverySubelement = 1;
  NSMiddleSubelement = 2;
  NSRandomSubelement = 3;

{ Types }
type
  NSInsertionPosition = NSUInteger;
  NSRelativePosition = NSUInteger;
  NSWhoseSubelementIdentifier = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_R}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_F}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_T}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptObjectSpecifier = objcclass;
  NSIndexSpecifier = objcclass;
  NSMiddleSpecifier = objcclass;
  NSNameSpecifier = objcclass;
  NSPositionalSpecifier = objcclass;
  NSPropertySpecifier = objcclass;
  NSRandomSpecifier = objcclass;
  NSRangeSpecifier = objcclass;
  NSRelativeSpecifier = objcclass;
  NSUniqueIDSpecifier = objcclass;
  NSWhoseSpecifier = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_C}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_C}

{ NSScriptObjectSpecifier }
  NSScriptObjectSpecifier = objcclass(NSObject, NSCodingProtocol)
  private
    __container: NSScriptObjectSpecifier;
    __child: NSScriptObjectSpecifier;
    __key: NSString;
    __containerClassDescription: NSScriptClassDescription;
    __containerIsObjectBeingTested: Boolean;
    __containerIsRangeContainerObject: Boolean;
        _padding: array[0..1] of char;
    __descriptor: NSAppleEventDescriptor;
    __error: NSInteger;
    
  public
    class function alloc: NSScriptObjectSpecifier; message 'alloc';

    class function objectSpecifierWithDescriptor(descriptor_: NSAppleEventDescriptor): NSScriptObjectSpecifier; message 'objectSpecifierWithDescriptor:';
    function initWithContainerSpecifier_key(container: NSScriptObjectSpecifier; property_: NSString): id; message 'initWithContainerSpecifier:key:';
    function initWithContainerClassDescription_containerSpecifier_key(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString): id; message 'initWithContainerClassDescription:containerSpecifier:key:';
    function childSpecifier: NSScriptObjectSpecifier; message 'childSpecifier';
    procedure setChildSpecifier(child: NSScriptObjectSpecifier); message 'setChildSpecifier:';
    function containerSpecifier: NSScriptObjectSpecifier; message 'containerSpecifier';
    procedure setContainerSpecifier(subRef: NSScriptObjectSpecifier); message 'setContainerSpecifier:';
    function containerIsObjectBeingTested: Boolean; message 'containerIsObjectBeingTested';
    procedure setContainerIsObjectBeingTested(flag: Boolean); message 'setContainerIsObjectBeingTested:';
    function containerIsRangeContainerObject: Boolean; message 'containerIsRangeContainerObject';
    procedure setContainerIsRangeContainerObject(flag: Boolean); message 'setContainerIsRangeContainerObject:';
    function key: NSString; message 'key';
    procedure setKey(key_: NSString); message 'setKey:';
    function containerClassDescription: NSScriptClassDescription; message 'containerClassDescription';
    procedure setContainerClassDescription(classDesc: NSScriptClassDescription); message 'setContainerClassDescription:';
    function keyClassDescription: NSScriptClassDescription; message 'keyClassDescription';
    function indicesOfObjectsByEvaluatingWithContainer_count(container: id; count: NSIntegerPointer): NSInteger; message 'indicesOfObjectsByEvaluatingWithContainer:count:';
    function objectsByEvaluatingWithContainers(containers: id): id; message 'objectsByEvaluatingWithContainers:';
    function objectsByEvaluatingSpecifier: id; message 'objectsByEvaluatingSpecifier';
    function evaluationErrorNumber: NSInteger; message 'evaluationErrorNumber';
    procedure setEvaluationErrorNumber(error: NSInteger); message 'setEvaluationErrorNumber:';
    function evaluationErrorSpecifier: NSScriptObjectSpecifier; message 'evaluationErrorSpecifier';
    function descriptor: NSAppleEventDescriptor; message 'descriptor';
  end; external;

{ NSIndexSpecifier }
  NSIndexSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    __index: NSInteger;
    
  public
    class function alloc: NSIndexSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_index(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; index_: NSInteger): id; message 'initWithContainerClassDescription:containerSpecifier:key:index:';
    function index: NSInteger; message 'index';
    procedure setIndex(index_: NSInteger); message 'setIndex:';
  end; external;

{ NSMiddleSpecifier }
  NSMiddleSpecifier = objcclass(NSScriptObjectSpecifier)
    
  public
    class function alloc: NSMiddleSpecifier; message 'alloc';
  end; external;

{ NSNameSpecifier }
  NSNameSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    __name: NSString;
    
  public
    class function alloc: NSNameSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_name(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; name_: NSString): id; message 'initWithContainerClassDescription:containerSpecifier:key:name:';
    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
  end; external;

{ NSPositionalSpecifier }
  NSPositionalSpecifier = objcclass(NSObject)
  private
    __specifier: NSScriptObjectSpecifier;
    __unadjustedPosition: NSInsertionPosition;
    __insertionClassDescription: NSScriptClassDescription;
    __moreVars: id;
    __reserved0: Pointer;
    
  public
    class function alloc: NSPositionalSpecifier; message 'alloc';

    function initWithPosition_objectSpecifier(position_: NSInsertionPosition; specifier: NSScriptObjectSpecifier): id; message 'initWithPosition:objectSpecifier:';
    function position: NSInsertionPosition; message 'position';
    function objectSpecifier: NSScriptObjectSpecifier; message 'objectSpecifier';
    procedure setInsertionClassDescription(classDescription_: NSScriptClassDescription); message 'setInsertionClassDescription:';
    procedure evaluate; message 'evaluate';
    function insertionContainer: id; message 'insertionContainer';
    function insertionKey: NSString; message 'insertionKey';
    function insertionIndex: NSInteger; message 'insertionIndex';
    function insertionReplaces: Boolean; message 'insertionReplaces';
  end; external;

{ NSPropertySpecifier }
  NSPropertySpecifier = objcclass(NSScriptObjectSpecifier)
    
  public
    class function alloc: NSPropertySpecifier; message 'alloc';
  end; external;

{ NSRandomSpecifier }
  NSRandomSpecifier = objcclass(NSScriptObjectSpecifier)
    
  public
    class function alloc: NSRandomSpecifier; message 'alloc';
  end; external;

{ NSRangeSpecifier }
  NSRangeSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    __startSpec: NSScriptObjectSpecifier;
    __endSpec: NSScriptObjectSpecifier;
    
  public
    class function alloc: NSRangeSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_startSpecifier_endSpecifier(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; startSpec: NSScriptObjectSpecifier; endSpec: NSScriptObjectSpecifier): id; message 'initWithContainerClassDescription:containerSpecifier:key:startSpecifier:endSpecifier:';
    function startSpecifier: NSScriptObjectSpecifier; message 'startSpecifier';
    procedure setStartSpecifier(startSpec: NSScriptObjectSpecifier); message 'setStartSpecifier:';
    function endSpecifier: NSScriptObjectSpecifier; message 'endSpecifier';
    procedure setEndSpecifier(endSpec: NSScriptObjectSpecifier); message 'setEndSpecifier:';
  end; external;

{ NSRelativeSpecifier }
  NSRelativeSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    __relativePosition: NSRelativePosition;
    __baseSpecifier: NSScriptObjectSpecifier;
    
  public
    class function alloc: NSRelativeSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_relativePosition_baseSpecifier(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; relPos: NSRelativePosition; baseSpecifier_: NSScriptObjectSpecifier): id; message 'initWithContainerClassDescription:containerSpecifier:key:relativePosition:baseSpecifier:';
    function relativePosition: NSRelativePosition; message 'relativePosition';
    procedure setRelativePosition(relPos: NSRelativePosition); message 'setRelativePosition:';
    function baseSpecifier: NSScriptObjectSpecifier; message 'baseSpecifier';
    procedure setBaseSpecifier(baseSpecifier_: NSScriptObjectSpecifier); message 'setBaseSpecifier:';
  end; external;

{ NSUniqueIDSpecifier }
  NSUniqueIDSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    __uniqueID: id;
    
  public
    class function alloc: NSUniqueIDSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_uniqueID(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; uniqueID_: id): id; message 'initWithContainerClassDescription:containerSpecifier:key:uniqueID:';
    function uniqueID: id; message 'uniqueID';
    procedure setUniqueID(uniqueID_: id); message 'setUniqueID:';
  end; external;

{ NSWhoseSpecifier }
  NSWhoseSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    __test: NSScriptWhoseTest;
    __startSubelementIdentifier: NSWhoseSubelementIdentifier;
    __startSubelementIndex: NSInteger;
    __endSubelementIdentifier: NSWhoseSubelementIdentifier;
    __endSubelementIndex: NSInteger;
    
  public
    class function alloc: NSWhoseSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_test(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; test_: NSScriptWhoseTest): id; message 'initWithContainerClassDescription:containerSpecifier:key:test:';
    function test: NSScriptWhoseTest; message 'test';
    procedure setTest(test_: NSScriptWhoseTest); message 'setTest:';
    function startSubelementIdentifier: NSWhoseSubelementIdentifier; message 'startSubelementIdentifier';
    procedure setStartSubelementIdentifier(subelement: NSWhoseSubelementIdentifier); message 'setStartSubelementIdentifier:';
    function startSubelementIndex: NSInteger; message 'startSubelementIndex';
    procedure setStartSubelementIndex(index: NSInteger); message 'setStartSubelementIndex:';
    function endSubelementIdentifier: NSWhoseSubelementIdentifier; message 'endSubelementIdentifier';
    procedure setEndSubelementIdentifier(subelement: NSWhoseSubelementIdentifier); message 'setEndSubelementIdentifier:';
    function endSubelementIndex: NSInteger; message 'endSubelementIndex';
    procedure setEndSubelementIndex(index: NSInteger); message 'setEndSubelementIndex:';
  end; external;

{$endif}
{$endif}
