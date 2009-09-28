{ Parsed from Appkit.framework NSRuleEditor.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSRULEEDITOR_PAS_H}
{$define NSRULEEDITOR_PAS_H}
type
  NSRuleEditorPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSRULEEDITOR_PAS_T}
{$define NSRULEEDITOR_PAS_T}

{ Constants }

const
  NSRuleEditorNestingModeSingle = 0;
  NSRuleEditorNestingModeList = 1;
  NSRuleEditorNestingModeCompound = 2;
  NSRuleEditorNestingModeSimple = 3;

const
  NSRuleEditorRowTypeSimple = 0;
  NSRuleEditorRowTypeCompound = 1;

{ Types }
type
  NSRuleEditorNestingMode = NSUInteger;
  NSRuleEditorRowType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRULEEDITOR_PAS_R}
{$define NSRULEEDITOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRULEEDITOR_PAS_F}
{$define NSRULEEDITOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRULEEDITOR_PAS_S}
{$define NSRULEEDITOR_PAS_S}

{ External string constants }
var
  NSRuleEditorPredicateLeftExpression: NSString; external name '_NSRuleEditorPredicateLeftExpression';
  NSRuleEditorPredicateRightExpression: NSString; external name '_NSRuleEditorPredicateRightExpression';
  NSRuleEditorPredicateComparisonModifier: NSString; external name '_NSRuleEditorPredicateComparisonModifier';
  NSRuleEditorPredicateOptions: NSString; external name '_NSRuleEditorPredicateOptions';
  NSRuleEditorPredicateOperatorType: NSString; external name '_NSRuleEditorPredicateOperatorType';
  NSRuleEditorPredicateCustomSelector: NSString; external name '_NSRuleEditorPredicateCustomSelector';
  NSRuleEditorPredicateCompoundType: NSString; external name '_NSRuleEditorPredicateCompoundType';
  NSRuleEditorRowsDidChangeNotification: NSString; external name '_NSRuleEditorRowsDidChangeNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSRuleEditor = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRULEEDITOR_PAS_C}
{$define NSRULEEDITOR_PAS_C}

{ NSRuleEditor }
  NSRuleEditor = objcclass(NSControl)
  private
    __ruleDataSource: id;
    __ruleDelegate: id;
    __draggingRows: NSIndexSet;
    __rowCache: NSMutableArray;
    __slicesHolder: NSView;
    __slices: NSMutableArray;
    __sliceHeight: CGFloat;
    __alignmentGridWidth: CGFloat;
    __subviewIndexOfDropLine: NSInteger;
    __dropLineView: id;
    __currentAnimation: NSViewAnimation;
    __frameTimer: NSTimer;
    __stringsFileName: NSString;
    __standardLocalizer: id;
    __headerLocalizer: id;
    __predicate: NSPredicate;
    __nestingMode: NSInteger;
    __ruleEditorFlags: bitpacked record
        elideUpdating: 0..1;
        lastAlternateKeyValue: 0..1;
        extendedDelegateCalls: 0..1;
        editable: 0..1;
        settingSize: 0..1;
        suppressKeyDown: 0..1;
        dropWasSuccessful: 0..1;
        delegateWantsValidation: 0..1;
        disallowEmpty: 0..1;
        lastDrewWithFRAppearance: 0..1;
        allowsEmptyCompoundRows: 0..1;
        dropChangedRowCount: 0..1;
        reserved: 0..((1 shl 20)-1);
      end;
    __typeKeyPath: NSString;
    __itemsKeyPath: NSString;
    __valuesKeyPath: NSString;
    __subrowsArrayKeyPath: NSString;
    __rowClass: Pobjc_class;
    __boundArrayOwner: id;
    __boundArrayKeyPath: NSString;
    __ruleReserved1: id;
    __lastRow: NSInteger;
    __ruleReserved2: id;
    
  public
    class function alloc: NSRuleEditor; message 'alloc';

    procedure setDelegate(sender: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setFormattingStringsFilename(stringsFilename: NSString); message 'setFormattingStringsFilename:';
    function formattingStringsFilename: NSString; message 'formattingStringsFilename';
    procedure setFormattingDictionary(dictionary: NSDictionary); message 'setFormattingDictionary:';
    function formattingDictionary: NSDictionary; message 'formattingDictionary';
    procedure reloadCriteria; message 'reloadCriteria';
    procedure setNestingMode(mode: NSRuleEditorNestingMode); message 'setNestingMode:';
    function nestingMode: NSRuleEditorNestingMode; message 'nestingMode';
    procedure setRowHeight(height: CGFloat); message 'setRowHeight:';
    function rowHeight: CGFloat; message 'rowHeight';
    procedure setEditable(editable: Boolean); message 'setEditable:';
    function isEditable: Boolean; message 'isEditable';
    procedure setCanRemoveAllRows(val: Boolean); message 'setCanRemoveAllRows:';
    function canRemoveAllRows: Boolean; message 'canRemoveAllRows';
    function predicate: NSPredicate; message 'predicate';
    procedure reloadPredicate; message 'reloadPredicate';
    function predicateForRow(row: NSInteger): NSPredicate; message 'predicateForRow:';
    function numberOfRows: NSInteger; message 'numberOfRows';
    function subrowIndexesForRow(rowIndex: NSInteger): NSIndexSet; message 'subrowIndexesForRow:';
    function criteriaForRow(row: NSInteger): NSArray; message 'criteriaForRow:';
    function displayValuesForRow(row: NSInteger): NSArray; message 'displayValuesForRow:';
    function rowForDisplayValue(displayValue: id): NSInteger; message 'rowForDisplayValue:';
    function rowTypeForRow(rowIndex: NSInteger): NSRuleEditorRowType; message 'rowTypeForRow:';
    function parentRowForRow(rowIndex: NSInteger): NSInteger; message 'parentRowForRow:';
    procedure addRow(sender: id); message 'addRow:';
    procedure insertRowAtIndex_withType_asSubrowOfRow_animate(rowIndex: NSInteger; rowType: NSRuleEditorRowType; parentRow: NSInteger; shouldAnimate: Boolean); message 'insertRowAtIndex:withType:asSubrowOfRow:animate:';
    procedure setCriteria_andDisplayValues_forRowAtIndex(criteria: NSArray; values: NSArray; rowIndex: NSInteger); message 'setCriteria:andDisplayValues:forRowAtIndex:';
    procedure removeRowAtIndex(rowIndex: NSInteger); message 'removeRowAtIndex:';
    procedure removeRowsAtIndexes_includeSubrows(rowIndexes: NSIndexSet; includeSubrows: Boolean); message 'removeRowsAtIndexes:includeSubrows:';
    function selectedRowIndexes: NSIndexSet; message 'selectedRowIndexes';
    procedure selectRowIndexes_byExtendingSelection(indexes: NSIndexSet; extend: Boolean); message 'selectRowIndexes:byExtendingSelection:';
    procedure setRowClass(rowClass_: Pobjc_class); message 'setRowClass:';
    function rowClass: Pobjc_class; message 'rowClass';
    procedure setRowTypeKeyPath(keyPath: NSString); message 'setRowTypeKeyPath:';
    function rowTypeKeyPath: NSString; message 'rowTypeKeyPath';
    procedure setSubrowsKeyPath(keyPath: NSString); message 'setSubrowsKeyPath:';
    function subrowsKeyPath: NSString; message 'subrowsKeyPath';
    procedure setCriteriaKeyPath(keyPath: NSString); message 'setCriteriaKeyPath:';
    function criteriaKeyPath: NSString; message 'criteriaKeyPath';
    procedure setDisplayValuesKeyPath(keyPath: NSString); message 'setDisplayValuesKeyPath:';
    function displayValuesKeyPath: NSString; message 'displayValuesKeyPath';
  end; external;

{$endif}
{$endif}
