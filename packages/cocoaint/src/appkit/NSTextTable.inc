{ Parsed from Appkit.framework NSTextTable.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTEXTTABLE_PAS_H}
{$define NSTEXTTABLE_PAS_H}
type
  NSTextBlockPointer = Pointer;
  NSTextTableBlockPointer = Pointer;
  NSTextTablePointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSTEXTTABLE_PAS_T}
{$define NSTEXTTABLE_PAS_T}

{ Types }
type
  NSTextBlockValueType = NSUInteger;
  NSTextBlockDimension = NSUInteger;
  NSTextBlockLayer = NSInteger;
  NSTextBlockVerticalAlignment = NSUInteger;
  NSTextTableLayoutAlgorithm = NSUInteger;

{ Constants }

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
{$ifndef NSTEXTTABLE_PAS_T}
{$define NSTEXTTABLE_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextBlock = objcclass;
  NSTextTableBlock = objcclass;
  NSTextTable = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTTABLE_PAS_C}
{$define NSTEXTTABLE_PAS_C}

{ NSTextBlock }
  NSTextBlock = objcclass(NSObject, NSCodingProtocol, NSCopyingProtocol)
  private
    __propVals: Pointer;
    __propMask: NSUInteger;
    __typeMask: NSUInteger;
    __primParamVal: id;
    __otherParamVals: id;
    __blockPrimary: Pointer;
    __blockSecondary: Pointer;
    
  public
    class function alloc: NSTextBlock; message 'alloc';

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
  end; external;

{ NSTextTableBlock }
  NSTextTableBlock = objcclass(NSTextBlock)
  private
    __table: NSTextTable;
    __rowNum: NSInteger;
    __colNum: NSInteger;
    __rowSpan: NSInteger;
    __colSpan: NSInteger;
    __tableBlockPrimary: Pointer;
    __tableBlockSecondary: Pointer;
    
  public
    class function alloc: NSTextTableBlock; message 'alloc';

    function initWithTable_startingRow_rowSpan_startingColumn_columnSpan(table_: NSTextTable; row: NSInteger; rowSpan_: NSInteger; col: NSInteger; colSpan: NSInteger): id; message 'initWithTable:startingRow:rowSpan:startingColumn:columnSpan:';
    function table: NSTextTable; message 'table';
    function startingRow: NSInteger; message 'startingRow';
    function rowSpan: NSInteger; message 'rowSpan';
    function startingColumn: NSInteger; message 'startingColumn';
    function columnSpan: NSInteger; message 'columnSpan';
  end; external;

{ NSTextTable }
  NSTextTable = objcclass(NSTextBlock)
  private
    __numCols: NSUInteger;
    __tableFlags: NSUInteger;
    __lcache: id;
    __tablePrimary: Pointer;
    __tableSecondary: Pointer;
    
  public
    class function alloc: NSTextTable; message 'alloc';

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
  end; external;

{$endif}
{$endif}
