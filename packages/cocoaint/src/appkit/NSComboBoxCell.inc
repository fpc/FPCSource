{ Parsed from Appkit.framework NSComboBoxCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCOMBOBOXCELL_PAS_H}
{$define NSCOMBOBOXCELL_PAS_H}
type
  NSComboBoxCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCOMBOBOXCELL_PAS_T}
{$define NSCOMBOBOXCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOMBOBOXCELL_PAS_R}
{$define NSCOMBOBOXCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOMBOBOXCELL_PAS_F}
{$define NSCOMBOBOXCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOMBOBOXCELL_PAS_S}
{$define NSCOMBOBOXCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSComboBoxCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOMBOBOXCELL_PAS_C}
{$define NSCOMBOBOXCELL_PAS_C}

{ NSComboBoxCell }
  NSComboBoxCell = objcclass(NSTextFieldCell)
  private
    __delegate: id;
    __dataSource: id;
    __cbcFlags: bitpacked record
        usesDataSource: 0..1;
        completes: 0..1;
        buttonBordered: 0..1;
        popUpIsUp: 0..1;
        filteringEvents: 0..1;
        drawing: 0..1;
        synchronizingSelection: 0..1;
        subclassOverridesBoundsForButtonCell: 0..1;
        reserved: 0..((1 shl 8)-1);
        visibleItems: 0..((1 shl 16)-1);
      end;
    __buttonCell: NSButtonCell;
    __tableView: NSTableView;
    __scrollView: NSScrollView;
    __popUp: NSWindow;
    __popUpList: NSMutableArray;
    __cellFrame: NSRect; {garbage collector: __strong }
    __reserved: Pointer;
    
  public
    class function alloc: NSComboBoxCell; message 'alloc';
  end; external;

{$endif}
{$endif}
