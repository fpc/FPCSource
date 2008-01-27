(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Table.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines table structures and routines.
 *
 * History:
 *    September 1, 1994 Created by Art Lamb
 *
 *****************************************************************************)

unit table;

interface

uses palmos, coretraps, rect, font, field, control;

//-------------------------------------------------------------------
// Table structures
//-------------------------------------------------------------------

const
  tableDefaultColumnSpacing = 1;
  tableNoteIndicatorWidth   = 7;
  tableNoteIndicatorHeight  = 11;
  tableMaxTextItemSize      = 255; // does not incude terminating null

  tblUnusableRow            = $ffff;

// Display style of a table item
//
type
  tableItemStyles = Enum;

const
  checkboxTableItem = 0;
  customTableItem = Succ(checkboxTableItem);
  dateTableItem = Succ(customTableItem);
  labelTableItem = Succ(dateTableItem);
  numericTableItem = Succ(labelTableItem);
  popupTriggerTableItem = Succ(numericTableItem);
  textTableItem = Succ(popupTriggerTableItem);
  textWithNoteTableItem = Succ(textTableItem);
  timeTableItem = Succ(textWithNoteTableItem);
  narrowTextTableItem = Succ(timeTableItem);
  tallCustomTableItem = Succ(narrowTextTableItem);


type
  TableItemStyleType = tableItemStyles;

  TableItemType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_TABLES} // These fields will not be available in the next OS release!
    itemType: TableItemStyleType;
    fontID: FontID; // font for drawing text
    intValue: Int16;
    ptr: PChar;
  {$endif}
  end;
  TableItemTag = TableItemType;
  TableItemPtr = ^TableItemType;

// Draw item callback routine prototype, used only by customTableItem.
  TableDrawItemFuncType = procedure(tableP: Pointer; row, column: Int16; var bounds: RectangleType);

  TableDrawItemFuncPtr = TableDrawItemFuncType;

// Load data callback routine prototype
  TableLoadDataFuncType = function(tableP: Pointer; row, column: Int16; editable: Boolean;
                                   var dataH: MemHandle; var dataOffset, dataSize: Int16; fld: FieldPtr): Err;

  TableLoadDataFuncPtr = TableLoadDataFuncType;

// Save data callback routine prototype
  TableSaveDataFuncType = function(tableP: Pointer; row, column: Int16): Boolean;

  TableSaveDataFuncPtr = TableSaveDataFuncType;

  TableColumnAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_TABLES} // These fields will not be available in the next OS release!
    width: Coord; // width in pixels
    Bits: UInt16;
{
    UInt16 reserved1       : 5;
    UInt16 masked          : 1;  // if both row + column masked, draw only grey box
    UInt16 editIndicator   : 1;
    UInt16 usable          : 1;
    UInt16 reserved2       : 8;
}
    spacing: Coord; // space after column
    drawCallback: TableDrawItemFuncPtr;
    loadDataCallback: TableLoadDataFuncPtr;
    saveDataCallback: TableSaveDataFuncPtr;
  {$endif}
  end;
  TableColumnAttrTag = TableColumnAttrType;

  TableRowAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_TABLES} // These fields will not be available in the next OS release!
    id: UInt16;
    height: Coord;                 // row height in pixels
    data: UInt32;
    Bits: UInt16;
{
    UInt16 reserved1       : 7;
    UInt16 usable          : 1;
    UInt16 reserved2       : 4;
    UInt16 masked          : 1;  // if both row + column masked, draw only grey box
    UInt16 invalid         : 1;    // true if redraw needed
    UInt16 staticHeight    : 1;  // Set if height does not expands as text is entered
    UInt16 selectable      : 1;
 }
    reserved3: UInt16;
  {$endif}
  end;
  TableRowAttrTag = TableRowAttrType;

  TableAttrType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_TABLES} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
    UInt16 visible:1;          // Set if drawn, used internally
    UInt16 editable:1;         // Set if editable
    UInt16 editing:1;          // Set if in edit mode
    UInt16 selected:1;         // Set if the current item is selected
    UInt16 hasScrollBar:1; // Set if the table has a scroll bar
    UInt16 usable:1;         // Set if in table is visible in the current form
    UInt16 reserved:10;
}
  {$endif}
  end;
  TableAttrTag = TableAttrType;

  TableType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_TABLES} // These fields will not be available in the next OS release!
    id: UInt16;
    bounds: RectangleType;
    attr: TableAttrType;
    numColumns: Int16;
    numRows: Int16;
    currentRow: Int16;
    currentColumn: Int16;
    topRow: Int16;
    columnAttrs: ^TableColumnAttrType;
    rowAttrs: ^TableRowAttrType;
    items: TableItemPtr;
    currentField: FieldType;
  {$endif}
  end;

  TablePtr = ^TableType;

//-------------------------------------------------------------------
// Table routines
//-------------------------------------------------------------------

procedure TblDrawTable(tableP: TablePtr); syscall sysTrapTblDrawTable;

procedure TblRedrawTable(tableP: TablePtr); syscall sysTrapTblRedrawTable;

procedure TblEraseTable(tableP: TablePtr); syscall sysTrapTblEraseTable;

function TblHandleEvent(tableP: TablePtr; event: EventPtr): Boolean; syscall sysTrapTblHandleEvent;

procedure TblGetItemBounds(const tableP: TablePtr; row, column: Int16; var rP: RectangleType); syscall sysTrapTblGetItemBounds;

procedure TblSelectItem(tableP: TablePtr; row, column: Int16); syscall sysTrapTblSelectItem;

function TblGetItemInt(const tableP: TablePtr; row, column: Int16): Int16; syscall sysTrapTblGetItemInt;

procedure TblSetItemInt(tableP: TablePtr; row, column, value: Int16); syscall sysTrapTblSetItemInt;

procedure TblSetItemPtr(tableP: TablePtr; row, column: Int16; value: Pointer); syscall sysTrapTblSetItemPtr;

procedure TblSetItemStyle(tableP: TablePtr; row, column: Int16; type_: TableItemStyleType); syscall sysTrapTblSetItemStyle;

procedure TblUnhighlightSelection(tableP: TablePtr); syscall sysTrapTblUnhighlightSelection;

function TblRowUsable(const tableP: TablePtr; row: Int16): Boolean; syscall sysTrapTblRowUsable;

procedure TblSetRowUsable(tableP: TablePtr; row: Int16; usable: Boolean); syscall sysTrapTblSetRowUsable;

function TblGetLastUsableRow(const tableP: TablePtr): Int16; syscall sysTrapTblGetLastUsableRow;

procedure TblSetColumnUsable(tableP: TablePtr; column: Int16; usable: Boolean); syscall sysTrapTblSetColumnUsable;

procedure TblSetRowSelectable(tableP: TablePtr; row: Int16; selectable: Boolean); syscall sysTrapTblSetRowSelectable;

function TblRowSelectable(const tableP: TablePtr; row: Int16): Boolean; syscall sysTrapTblRowSelectable;

function TblGetNumberOfRows(const tableP: TablePtr): Int16; syscall sysTrapTblGetNumberOfRows;

procedure TblSetCustomDrawProcedure(tableP: TablePtr; column: Int16; drawCallback: TableDrawItemFuncPtr); syscall sysTrapTblSetCustomDrawProcedure;

procedure TblSetLoadDataProcedure(tableP: TablePtr; column: Int16; loadDataCallback: TableLoadDataFuncPtr); syscall sysTrapTblSetLoadDataProcedure;

procedure TblSetSaveDataProcedure(tableP: TablePtr; column: Int16; saveDataCallback: TableSaveDataFuncPtr); syscall sysTrapTblSetSaveDataProcedure;

procedure TblGetBounds(const tableP: TablePtr; var rP: RectangleType); syscall sysTrapTblGetBounds;

procedure TblSetBounds(tableP: TablePtr; {const} var rP: RectangleType); syscall sysTrapTblSetBounds;

function TblGetRowHeight(const tableP: TablePtr; row: Int16): Coord; syscall sysTrapTblGetRowHeight;

procedure TblSetRowHeight(tableP: TablePtr; row: Int16; height: Coord); syscall sysTrapTblSetRowHeight;

function TblGetColumnWidth(const tableP: TablePtr; column: Int16): Coord; syscall sysTrapTblGetColumnWidth;

procedure TblSetColumnWidth(tableP: TablePtr; column: Int16; width: Coord); syscall sysTrapTblSetColumnWidth;

function TblGetColumnSpacing(const tableP: TablePtr; column: Int16): Coord; syscall sysTrapTblGetColumnSpacing;

procedure TblSetColumnSpacing(tableP: TablePtr; column: Int16; spacing: Coord); syscall sysTrapTblSetColumnSpacing;

function TblFindRowID(const tableP: TablePtr; id: UInt16; var rowP: Int16): Boolean; syscall sysTrapTblFindRowID;

function TblFindRowData(const tableP: TablePtr; data: UInt32; var rowP: Int16): Boolean; syscall sysTrapTblFindRowData;

function TblGetRowID(const tableP: TablePtr; row: Int16): UInt16; syscall sysTrapTblGetRowID;

procedure TblSetRowID(tableP: TablePtr; row, id: Int16); syscall sysTrapTblSetRowID;

function TblGetRowData(const tableP: TablePtr; row: Int16): UInt32; syscall sysTrapTblGetRowData;

procedure TblSetRowData(tableP: TablePtr; row: Int16; data: UInt32); syscall sysTrapTblSetRowData;

function TblRowInvalid(const tableP: TablePtr; row: Int16): Boolean; syscall sysTrapTblRowInvalid;

procedure TblMarkRowInvalid(tableP: TablePtr; row: Int16); syscall sysTrapTblMarkRowInvalid;

procedure TblMarkTableInvalid(tableP: TablePtr); syscall sysTrapTblMarkTableInvalid;

function TblGetSelection(const tableP: TablePtr; var rowP, columnP: Int16): Boolean; syscall sysTrapTblGetSelection;

procedure TblInsertRow(tableP: TablePtr; row: Int16); syscall sysTrapTblInsertRow;

procedure TblRemoveRow(tableP: TablePtr; row: Int16); syscall sysTrapTblRemoveRow;

procedure TblReleaseFocus(tableP: TablePtr); syscall sysTrapTblReleaseFocus;

function TblEditing(const tableP: TablePtr): Boolean; syscall sysTrapTblEditing;

function TblGetCurrentField(const tableP: TablePtr): FieldPtr; syscall sysTrapTblGetCurrentField;

procedure TblGrabFocus(tableP: TablePtr; row, column: Int16); syscall sysTrapTblGrabFocus;

procedure TblSetColumnEditIndicator(tableP: TablePtr; column: Int16; editIndicator: Boolean); syscall sysTrapTblSetColumnEditIndicator;

procedure TblSetRowStaticHeight(tableP: TablePtr; row: Int16; staticHeight: Boolean); syscall sysTrapTblSetRowStaticHeight;

procedure TblHasScrollBar(tableP: TablePtr; hasScrollBar: Boolean); syscall sysTrapTblHasScrollBar;

function TblGetItemFont(const tableP: TablePtr; row, column: Int16): FontID; syscall sysTrapTblGetItemFont;

procedure TblSetItemFont(tableP: TablePtr; row, column: Int16; fontID: FontID); syscall sysTrapTblSetItemFont;

function TblGetItemPtr(const tableP: TablePtr; row, column: Int16): Pointer; syscall sysTrapTblGetItemPtr;

function TblRowMasked(const tableP: TablePtr; row: Int16): Boolean; syscall sysTrapTblRowMasked;

procedure TblSetRowMasked(tableP: TablePtr; row: Int16; masked: Boolean); syscall sysTrapTblSetRowMasked;

procedure TblSetColumnMasked(tableP: TablePtr; column: Int16; masked: Boolean); syscall sysTrapTblSetColumnMasked;

function TblGetNumberOfColumns(const tableP: TablePtr): Int16; syscall sysTrapTblGetNumberOfColumns;

function TblGetTopRow(const tableP: TablePtr): Int16; syscall sysTrapTblGetTopRow;

procedure TblSetSelection(tableP: TablePtr; row, column: Int16); syscall sysTrapTblSetSelection;

implementation

end.
