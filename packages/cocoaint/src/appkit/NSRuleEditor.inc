{ Parsed from AppKit.framework NSRuleEditor.h }


{$ifdef TYPES}
type
  NSRuleEditorPtr = ^NSRuleEditor;
  NSRuleEditorDelegateProtocolPtr = ^NSRuleEditorDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSRuleEditorNestingMode = NSUInteger;
  NSRuleEditorNestingModePtr = ^NSRuleEditorNestingMode;

const
  NSRuleEditorNestingModeSingle = 0;
  NSRuleEditorNestingModeList = 1;
  NSRuleEditorNestingModeCompound = 2;
  NSRuleEditorNestingModeSimple = 3;

type
  NSRuleEditorRowType = NSUInteger;
  NSRuleEditorRowTypePtr = ^NSRuleEditorRowType;

const
  NSRuleEditorRowTypeSimple = 0;
  NSRuleEditorRowTypeCompound = 1;
{$endif}

{$ifdef CLASSES}

type
  NSRuleEditor = objcclass external (NSControl)
  private
    _ruleDataSource: id;
    _ruleDelegate: id;
    _draggingRows: NSIndexSet;
    _rowCache: NSMutableArray;
    _slicesHolder: NSView;
    _slices: NSMutableArray;
    _sliceHeight: CGFloat;
    _alignmentGridWidth: CGFloat;
    _subviewIndexOfDropLine: NSInteger;
    _dropLineView: id;
    _currentAnimation: NSViewAnimation;
    _unused1: id;
    _stringsFileName: NSString;
    _standardLocalizer: id;
    _headerLocalizer: id;
    _predicate: NSPredicate;
    _nestingMode: NSInteger;
    _ruleEditorFlags: bitpacked record
      case byte of
        0: (_anonBitField__ruleEditorFlags0: cuint);
        1: (
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
        );
      end;
    _typeKeyPath: NSString;
    _itemsKeyPath: NSString;
    _valuesKeyPath: NSString;
    _subrowsArrayKeyPath: NSString;
    _rowClass: pobjc_class;
    _boundArrayOwner: id;
    _boundArrayKeyPath: NSString;
    _ruleReserved1: id;
    _lastRow: NSInteger;
    _ruleReserved2: id;
  public
    procedure setDelegate(newValue: NSRuleEditorDelegateProtocol); message 'setDelegate:';
    function delegate: NSRuleEditorDelegateProtocol; message 'delegate';
    procedure setFormattingStringsFilename(newValue: NSString); message 'setFormattingStringsFilename:';
    function formattingStringsFilename: NSString; message 'formattingStringsFilename';
    procedure setFormattingDictionary(newValue: NSDictionary); message 'setFormattingDictionary:';
    function formattingDictionary: NSDictionary; message 'formattingDictionary';
    procedure reloadCriteria; message 'reloadCriteria';
    procedure setNestingMode(newValue: NSRuleEditorNestingMode); message 'setNestingMode:';
    function nestingMode: NSRuleEditorNestingMode; message 'nestingMode';
    procedure setRowHeight(newValue: CGFloat); message 'setRowHeight:';
    function rowHeight: CGFloat; message 'rowHeight';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setCanRemoveAllRows(newValue: ObjCBOOL); message 'setCanRemoveAllRows:';
    function canRemoveAllRows: ObjCBOOL; message 'canRemoveAllRows';
    function predicate: NSPredicate; message 'predicate';
    procedure reloadPredicate; message 'reloadPredicate';
    function predicateForRow (row: NSInteger): NSPredicate; message 'predicateForRow:';
    function numberOfRows: NSInteger; message 'numberOfRows';
    function subrowIndexesForRow (rowIndex: NSInteger): NSIndexSet; message 'subrowIndexesForRow:';
    function criteriaForRow (row: NSInteger): NSArray; message 'criteriaForRow:';
    function displayValuesForRow (row: NSInteger): NSArray; message 'displayValuesForRow:';
    function rowForDisplayValue (displayValue: id): NSInteger; message 'rowForDisplayValue:';
    function rowTypeForRow (rowIndex: NSInteger): NSRuleEditorRowType; message 'rowTypeForRow:';
    function parentRowForRow (rowIndex: NSInteger): NSInteger; message 'parentRowForRow:';
    procedure addRow (sender: id); message 'addRow:';
    procedure insertRowAtIndex_withType_asSubrowOfRow_animate (rowIndex: NSInteger; rowType: NSRuleEditorRowType; parentRow: NSInteger; shouldAnimate: ObjCBOOL); message 'insertRowAtIndex:withType:asSubrowOfRow:animate:';
    procedure setCriteria_andDisplayValues_forRowAtIndex (criteria: NSArray; values: NSArray; rowIndex: NSInteger); message 'setCriteria:andDisplayValues:forRowAtIndex:';
    procedure removeRowAtIndex (rowIndex: NSInteger); message 'removeRowAtIndex:';
    procedure removeRowsAtIndexes_includeSubrows (rowIndexes: NSIndexSet; includeSubrows: ObjCBOOL); message 'removeRowsAtIndexes:includeSubrows:';
    function selectedRowIndexes: NSIndexSet; message 'selectedRowIndexes';
    procedure selectRowIndexes_byExtendingSelection (indexes: NSIndexSet; extend: ObjCBOOL); message 'selectRowIndexes:byExtendingSelection:';
    procedure setRowClass(newValue: pobjc_class); message 'setRowClass:';
    function rowClass: pobjc_class; message 'rowClass';
    procedure setRowTypeKeyPath(newValue: NSString); message 'setRowTypeKeyPath:';
    function rowTypeKeyPath: NSString; message 'rowTypeKeyPath';
    procedure setSubrowsKeyPath(newValue: NSString); message 'setSubrowsKeyPath:';
    function subrowsKeyPath: NSString; message 'subrowsKeyPath';
    procedure setCriteriaKeyPath(newValue: NSString); message 'setCriteriaKeyPath:';
    function criteriaKeyPath: NSString; message 'criteriaKeyPath';
    procedure setDisplayValuesKeyPath(newValue: NSString); message 'setDisplayValuesKeyPath:';
    function displayValuesKeyPath: NSString; message 'displayValuesKeyPath';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSRuleEditorDelegateProtocol = objcprotocol external name 'NSRuleEditorDelegate' (NSObjectProtocol)
  required
    function ruleEditor_numberOfChildrenForCriterion_withRowType (editor: NSRuleEditor; criterion: id; rowType: NSRuleEditorRowType): NSInteger; message 'ruleEditor:numberOfChildrenForCriterion:withRowType:';
    function ruleEditor_child_forCriterion_withRowType (editor: NSRuleEditor; index: NSInteger; criterion: id; rowType: NSRuleEditorRowType): id; message 'ruleEditor:child:forCriterion:withRowType:';
    function ruleEditor_displayValueForCriterion_inRow (editor: NSRuleEditor; criterion: id; row: NSInteger): id; message 'ruleEditor:displayValueForCriterion:inRow:';
  optional
    function ruleEditor_predicatePartsForCriterion_withDisplayValue_inRow (editor: NSRuleEditor; criterion: id; value: id; row: NSInteger): NSDictionary; message 'ruleEditor:predicatePartsForCriterion:withDisplayValue:inRow:';
    procedure ruleEditorRowsDidChange (notification: NSNotification); message 'ruleEditorRowsDidChange:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSRuleEditorPredicateLeftExpression: NSString; cvar; external;
  NSRuleEditorPredicateRightExpression: NSString; cvar; external;
  NSRuleEditorPredicateComparisonModifier: NSString; cvar; external;
  NSRuleEditorPredicateOptions: NSString; cvar; external;
  NSRuleEditorPredicateOperatorType: NSString; cvar; external;
  NSRuleEditorPredicateCustomSelector: NSString; cvar; external;
  NSRuleEditorPredicateCompoundType: NSString; cvar; external;
  NSRuleEditorRowsDidChangeNotification: NSString; cvar; external;
{$endif}

