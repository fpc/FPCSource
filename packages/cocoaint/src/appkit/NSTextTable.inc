{ Parsed from Appkit.framework NSTextTable.h }

{$ifdef TYPES}
{$ifndef NSTEXTTABLE_PAS_T}
{$define NSTEXTTABLE_PAS_T}

{ Constants }

const
  NSTextBlockAbsoluteValueType = 0;
  NSTextBlockPercentageValueType = 1;

const
  NSTextBlockWidth = 0;
  NSTextBlockMinimumWidth = 1;
  NSTextBlockMaximumWidth = 2;
  NSTextBlockHeight = 4;
  NSTextBlockMinimumHeight = 5;
  NSTextBlockMaximumHeight = 6;

const
  NSTextBlockPadding = -1;
  NSTextBlockBorder = 0;
  NSTextBlockMargin = 1;

const
  NSTextBlockTopAlignment = 0;
  NSTextBlockMiddleAlignment = 1;
  NSTextBlockBottomAlignment = 2;
  NSTextBlockBaselineAlignment = 3;

const
  NSTextTableAutomaticLayoutAlgorithm = 0;
  NSTextTableFixedLayoutAlgorithm = 1;

{ Types }
type
  NSTextBlockValueType = NSUInteger;
  NSTextBlockValueTypePtr = ^NSTextBlockValueType;
  NSTextBlockDimension = NSUInteger;
  NSTextBlockDimensionPtr = ^NSTextBlockDimension;
  NSTextBlockLayer = NSInteger;
  NSTextBlockLayerPtr = ^NSTextBlockLayer;
  NSTextBlockVerticalAlignment = NSUInteger;
  NSTextBlockVerticalAlignmentPtr = ^NSTextBlockVerticalAlignment;
  NSTextTableLayoutAlgorithm = NSUInteger;
  NSTextTableLayoutAlgorithmPtr = ^NSTextTableLayoutAlgorithm;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTTABLE_PAS_R}
{$define NSTEXTTABLE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTTABLE_PAS_F}
{$define NSTEXTTABLE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTTABLE_PAS_S}
{$define NSTEXTTABLE_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextBlock = objcclass;
  NSTextBlockPointer = ^NSTextBlock;
  NSTextBlockPtr = NSTextBlockPointer;
  NSTextTableBlock = objcclass;
  NSTextTableBlockPointer = ^NSTextTableBlock;
  NSTextTableBlockPtr = NSTextTableBlockPointer;
  NSTextTable = objcclass;
  NSTextTablePointer = ^NSTextTable;
  NSTextTablePtr = NSTextTablePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTTABLE_PAS_C}
{$define NSTEXTTABLE_PAS_C}

{ NSTextBlock }
  NSTextBlock = objcclass external (NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    _propVals: Pointer;
    _propMask: NSUInteger;
    _typeMask: NSUInteger;
    _primParamVal: id;
    _otherParamVals: id;
    _blockPrimary: Pointer;
    _blockSecondary: Pointer;
    
  public
    function init: id; message 'init';
    procedure setValue_type_forDimension(val: CGFloat; type_: NSTextBlockValueType; dimension: NSTextBlockDimension); message 'setValue:type:forDimension:';
    function valueForDimension(dimension: NSTextBlockDimension): CGFloat; message 'valueForDimension:';
    function valueTypeForDimension(dimension: NSTextBlockDimension): NSTextBlockValueType; message 'valueTypeForDimension:';
    procedure setContentWidth_type(val: CGFloat; type_: NSTextBlockValueType); message 'setContentWidth:type:';
    function contentWidth: CGFloat; message 'contentWidth';
    function contentWidthValueType: NSTextBlockValueType; message 'contentWidthValueType';
    procedure setWidth_type_forLayer_edge(val: CGFloat; type_: NSTextBlockValueType; layer: NSTextBlockLayer; edge: NSRectEdge); message 'setWidth:type:forLayer:edge:';
    procedure setWidth_type_forLayer(val: CGFloat; type_: NSTextBlockValueType; layer: NSTextBlockLayer); message 'setWidth:type:forLayer:';
    function widthForLayer_edge(layer: NSTextBlockLayer; edge: NSRectEdge): CGFloat; message 'widthForLayer:edge:';
    function widthValueTypeForLayer_edge(layer: NSTextBlockLayer; edge: NSRectEdge): NSTextBlockValueType; message 'widthValueTypeForLayer:edge:';
    procedure setVerticalAlignment(alignment: NSTextBlockVerticalAlignment); message 'setVerticalAlignment:';
    function verticalAlignment: NSTextBlockVerticalAlignment; message 'verticalAlignment';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBorderColor_forEdge(color: NSColor; edge: NSRectEdge); message 'setBorderColor:forEdge:';
    procedure setBorderColor(color: NSColor); message 'setBorderColor:';
    function borderColorForEdge(edge: NSRectEdge): NSColor; message 'borderColorForEdge:';
    function rectForLayoutAtPoint_inRect_textContainer_characterRange(startingPoint: NSPoint; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'rectForLayoutAtPoint:inRect:textContainer:characterRange:';
    function boundsRectForContentRect_inRect_textContainer_characterRange(contentRect: NSRect; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'boundsRectForContentRect:inRect:textContainer:characterRange:';
    procedure drawBackgroundWithFrame_inView_characterRange_layoutManager(frameRect: NSRect; controlView: NSView; charRange: NSRange; layoutManager: NSLayoutManager); message 'drawBackgroundWithFrame:inView:characterRange:layoutManager:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
    function copyWithZone(zone_: NSZonePtr): id;
  end;

{ NSTextTableBlock }
  NSTextTableBlock = objcclass external (NSTextBlock)
  private
    _table: NSTextTable;
    _rowNum: NSInteger;
    _colNum: NSInteger;
    _rowSpan: NSInteger;
    _colSpan: NSInteger;
    _tableBlockPrimary: Pointer;
    _tableBlockSecondary: Pointer;
    
  public
    function initWithTable_startingRow_rowSpan_startingColumn_columnSpan(table_: NSTextTable; row: NSInteger; rowSpan_: NSInteger; col: NSInteger; colSpan: NSInteger): id; message 'initWithTable:startingRow:rowSpan:startingColumn:columnSpan:';
    function table: NSTextTable; message 'table';
    function startingRow: NSInteger; message 'startingRow';
    function rowSpan: NSInteger; message 'rowSpan';
    function startingColumn: NSInteger; message 'startingColumn';
    function columnSpan: NSInteger; message 'columnSpan';
  end;

{ NSTextTable }
  NSTextTable = objcclass external (NSTextBlock)
  private
    _numCols: NSUInteger;
    _tableFlags: NSUInteger;
    _lcache: id;
    _tablePrimary: Pointer;
    _tableSecondary: Pointer;
    
  public
    function numberOfColumns: NSUInteger; message 'numberOfColumns';
    procedure setNumberOfColumns(numCols: NSUInteger); message 'setNumberOfColumns:';
    function layoutAlgorithm: NSTextTableLayoutAlgorithm; message 'layoutAlgorithm';
    procedure setLayoutAlgorithm(algorithm: NSTextTableLayoutAlgorithm); message 'setLayoutAlgorithm:';
    function collapsesBorders: Boolean; message 'collapsesBorders';
    procedure setCollapsesBorders(flag: Boolean); message 'setCollapsesBorders:';
    function hidesEmptyCells: Boolean; message 'hidesEmptyCells';
    procedure setHidesEmptyCells(flag: Boolean); message 'setHidesEmptyCells:';
    function rectForBlock_layoutAtPoint_inRect_textContainer_characterRange(block: NSTextTableBlock; startingPoint: NSPoint; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'rectForBlock:layoutAtPoint:inRect:textContainer:characterRange:';
    function boundsRectForBlock_contentRect_inRect_textContainer_characterRange(block: NSTextTableBlock; contentRect: NSRect; rect: NSRect; textContainer: NSTextContainer; charRange: NSRange): NSRect; message 'boundsRectForBlock:contentRect:inRect:textContainer:characterRange:';
    procedure drawBackgroundForBlock_withFrame_inView_characterRange_layoutManager(block: NSTextTableBlock; frameRect: NSRect; controlView: NSView; charRange: NSRange; layoutManager: NSLayoutManager); message 'drawBackgroundForBlock:withFrame:inView:characterRange:layoutManager:';
  end;

{$endif}
{$endif}
