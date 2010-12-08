{ Parsed from Foundation.framework NSScriptObjectSpecifiers.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:43 CET 2010 }


{$ifdef TYPES}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_T}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_T}

{ Constants }

const
  NSNoSpecifierError = 0;
  NSNoTopLevelContainersSpecifierError = 0;
  NSContainerSpecifierError = 1;
  NSUnknownKeySpecifierError = 2;
  NSInvalidIndexSpecifierError = 3;
  NSInternalSpecifierError = 4;
  NSOperationNotSupportedForKeySpecifierError = 5;

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
  NSNoSubelement = 4;

{ Types }
type
  NSInsertionPosition = NSUInteger;
  NSInsertionPositionPtr = ^NSInsertionPosition;
  NSRelativePosition = NSUInteger;
  NSRelativePositionPtr = ^NSRelativePosition;
  NSWhoseSubelementIdentifier = NSUInteger;
  NSWhoseSubelementIdentifierPtr = ^NSWhoseSubelementIdentifier;

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
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_S}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptObjectSpecifier = objcclass;
  NSScriptObjectSpecifierPointer = ^NSScriptObjectSpecifier;
  NSScriptObjectSpecifierPtr = NSScriptObjectSpecifierPointer;
  NSIndexSpecifier = objcclass;
  NSIndexSpecifierPointer = ^NSIndexSpecifier;
  NSIndexSpecifierPtr = NSIndexSpecifierPointer;
  NSMiddleSpecifier = objcclass;
  NSMiddleSpecifierPointer = ^NSMiddleSpecifier;
  NSMiddleSpecifierPtr = NSMiddleSpecifierPointer;
  NSNameSpecifier = objcclass;
  NSNameSpecifierPointer = ^NSNameSpecifier;
  NSNameSpecifierPtr = NSNameSpecifierPointer;
  NSPositionalSpecifier = objcclass;
  NSPositionalSpecifierPointer = ^NSPositionalSpecifier;
  NSPositionalSpecifierPtr = NSPositionalSpecifierPointer;
  NSPropertySpecifier = objcclass;
  NSPropertySpecifierPointer = ^NSPropertySpecifier;
  NSPropertySpecifierPtr = NSPropertySpecifierPointer;
  NSRandomSpecifier = objcclass;
  NSRandomSpecifierPointer = ^NSRandomSpecifier;
  NSRandomSpecifierPtr = NSRandomSpecifierPointer;
  NSRangeSpecifier = objcclass;
  NSRangeSpecifierPointer = ^NSRangeSpecifier;
  NSRangeSpecifierPtr = NSRangeSpecifierPointer;
  NSRelativeSpecifier = objcclass;
  NSRelativeSpecifierPointer = ^NSRelativeSpecifier;
  NSRelativeSpecifierPtr = NSRelativeSpecifierPointer;
  NSUniqueIDSpecifier = objcclass;
  NSUniqueIDSpecifierPointer = ^NSUniqueIDSpecifier;
  NSUniqueIDSpecifierPtr = NSUniqueIDSpecifierPointer;
  NSWhoseSpecifier = objcclass;
  NSWhoseSpecifierPointer = ^NSWhoseSpecifier;
  NSWhoseSpecifierPtr = NSWhoseSpecifierPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTOBJECTSPECIFIERS_PAS_C}
{$define NSSCRIPTOBJECTSPECIFIERS_PAS_C}

{ NSScriptObjectSpecifier }
  NSScriptObjectSpecifier = objcclass(NSObject, NSCodingProtocol)
  private
    _container: NSScriptObjectSpecifier;
    _child: NSScriptObjectSpecifier;
    _key: NSString;
    _containerClassDescription: NSScriptClassDescription;
    _containerIsObjectBeingTested: Boolean;
    _containerIsRangeContainerObject: Boolean;
        _padding: array[0..1] of char;
    _descriptor: NSAppleEventDescriptor;
    _error: NSInteger;
    
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
    function indicesOfObjectsByEvaluatingWithContainer_count(container: id; count: NSIntegerPtr): NSIntegerPtr; message 'indicesOfObjectsByEvaluatingWithContainer:count:';
    function objectsByEvaluatingWithContainers(containers: id): id; message 'objectsByEvaluatingWithContainers:';
    function objectsByEvaluatingSpecifier: id; message 'objectsByEvaluatingSpecifier';
    function evaluationErrorNumber: NSInteger; message 'evaluationErrorNumber';
    procedure setEvaluationErrorNumber(error: NSInteger); message 'setEvaluationErrorNumber:';
    function evaluationErrorSpecifier: NSScriptObjectSpecifier; message 'evaluationErrorSpecifier';
    function descriptor: NSAppleEventDescriptor; message 'descriptor';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end; external;

{ NSIndexSpecifier }
  NSIndexSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    _index: NSInteger;
    
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
    _name: NSString;
    
  public
    class function alloc: NSNameSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_name(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; name_: NSString): id; message 'initWithContainerClassDescription:containerSpecifier:key:name:';
    function name: NSString; message 'name';
    procedure setName(name_: NSString); message 'setName:';
  end; external;

{ NSPositionalSpecifier }
  NSPositionalSpecifier = objcclass(NSObject)
  private
    _specifier: NSScriptObjectSpecifier;
    _unadjustedPosition: NSInsertionPosition;
    _insertionClassDescription: NSScriptClassDescription;
    _moreVars: id;
    _reserved0: Pointer;
    
  public
    class function alloc: NSPositionalSpecifier; message 'alloc';

    function initWithPosition_objectSpecifier(position_: NSInsertionPosition; specifier: NSScriptObjectSpecifier): id; message 'initWithPosition:objectSpecifier:';
    function position: NSInsertionPosition; message 'position';
    function objectSpecifier: NSScriptObjectSpecifier; message 'objectSpecifier';
    procedure setInsertionClassDescription(classDescription: NSScriptClassDescription); message 'setInsertionClassDescription:';
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
    _startSpec: NSScriptObjectSpecifier;
    _endSpec: NSScriptObjectSpecifier;
    
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
    _relativePosition: NSRelativePosition;
    _baseSpecifier: NSScriptObjectSpecifier;
    
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
    _uniqueID: id;
    
  public
    class function alloc: NSUniqueIDSpecifier; message 'alloc';

    function initWithContainerClassDescription_containerSpecifier_key_uniqueID(classDesc: NSScriptClassDescription; container: NSScriptObjectSpecifier; property_: NSString; uniqueID_: id): id; message 'initWithContainerClassDescription:containerSpecifier:key:uniqueID:';
    function uniqueID: id; message 'uniqueID';
    procedure setUniqueID(uniqueID_: id); message 'setUniqueID:';
  end; external;

{ NSWhoseSpecifier }
  NSWhoseSpecifier = objcclass(NSScriptObjectSpecifier)
  private
    _test: NSScriptWhoseTest;
    _startSubelementIdentifier: NSWhoseSubelementIdentifier;
    _startSubelementIndex: NSInteger;
    _endSubelementIdentifier: NSWhoseSubelementIdentifier;
    _endSubelementIndex: NSInteger;
    
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

{ NSScriptObjectSpecifiersCategory }
  NSScriptObjectSpecifiersCategory = objccategory(NSObject)
    function objectSpecifier: NSScriptObjectSpecifier; message 'objectSpecifier';
    function indicesOfObjectsByEvaluatingObjectSpecifier(specifier: NSScriptObjectSpecifier): NSArray; message 'indicesOfObjectsByEvaluatingObjectSpecifier:';
  end; external;

{$endif}
{$endif}
