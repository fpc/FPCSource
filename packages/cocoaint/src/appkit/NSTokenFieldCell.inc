{ Parsed from AppKit.framework NSTokenFieldCell.h }


{$ifdef TYPES}
type
  NSTokenFieldCellPtr = ^NSTokenFieldCell;
  NSTokenFieldCellDelegateProtocolPtr = ^NSTokenFieldCellDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSTokenStyle = NSUInteger;
  NSTokenStylePtr = ^NSTokenStyle;

const
  NSTokenStyleDefault = 0;
  NSTokenStyleNone = 1;
  NSTokenStyleRounded = 2;
  NSTokenStyleSquared = 3 { available in 10_10 };
  NSTokenStylePlainSquared = 4 { available in 10_10 };
{$endif}

{$ifdef CLASSES}

type
  NSTokenFieldCell = objcclass external (NSTextFieldCell)
  private
    _tokenizingCharacterSet: NSCharacterSet;
    _delegate: id;
    _completionDelay: NSTimeInterval;
    _cache: id;
    _defaultTerminator: id;
    _trackingArea: id;
    _lastCell: id;
    _lastCellFrame: NSRect;
    _autoCompleteCancel: pObjCBOOL;
    _reserved:array[0..5] of id;
    _tfcFlags: bitpacked record
      case byte of
        0: (_anonBitField__tfcFlags0: cuint);
        1: (
          _style: 0..((1 shl 4)-1);
          _invalidCache: 0..1;
          _inDidChange: 0..1;
          _validationDisabled: 0..1;
          _reserved1: 0..1;
          _autoCompleteMode: 0..((1 shl 2)-1);
          _inValidateEditing: 0..1;
          _performingDrop: 0..1;
          _reserved: 0..((1 shl 20)-1);
        );
      end;
  public
    procedure setTokenStyle(newValue: NSTokenStyle); message 'setTokenStyle:';
    function tokenStyle: NSTokenStyle; message 'tokenStyle';
    procedure setCompletionDelay(newValue: NSTimeInterval); message 'setCompletionDelay:';
    function completionDelay: NSTimeInterval; message 'completionDelay';
    class function defaultCompletionDelay: NSTimeInterval; message 'defaultCompletionDelay';
    procedure setTokenizingCharacterSet(newValue: NSCharacterSet); message 'setTokenizingCharacterSet:';
    function tokenizingCharacterSet: NSCharacterSet; message 'tokenizingCharacterSet';
    class function defaultTokenizingCharacterSet: NSCharacterSet; message 'defaultTokenizingCharacterSet';
    procedure setDelegate(newValue: NSTokenFieldCellDelegateProtocol); message 'setDelegate:';
    function delegate: NSTokenFieldCellDelegateProtocol; message 'delegate';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTokenFieldCellDelegateProtocol = objcprotocol external name 'NSTokenFieldCellDelegate' (NSObjectProtocol)
  optional
    function tokenFieldCell_completionsForSubstring_indexOfToken_indexOfSelectedItem (tokenFieldCell: NSTokenFieldCell; substring: NSString; tokenIndex: NSInteger; selectedIndex: NSIntegerPtr): NSArray; message 'tokenFieldCell:completionsForSubstring:indexOfToken:indexOfSelectedItem:';
    function tokenFieldCell_shouldAddObjects_atIndex (tokenFieldCell: NSTokenFieldCell; tokens: NSArray; index: NSUInteger): NSArray; message 'tokenFieldCell:shouldAddObjects:atIndex:';
    function tokenFieldCell_displayStringForRepresentedObject (tokenFieldCell: NSTokenFieldCell; representedObject: id): NSString; message 'tokenFieldCell:displayStringForRepresentedObject:';
    function tokenFieldCell_editingStringForRepresentedObject (tokenFieldCell: NSTokenFieldCell; representedObject: id): NSString; message 'tokenFieldCell:editingStringForRepresentedObject:';
    function tokenFieldCell_representedObjectForEditingString (tokenFieldCell: NSTokenFieldCell; editingString: NSString): id; message 'tokenFieldCell:representedObjectForEditingString:';
    function tokenFieldCell_writeRepresentedObjects_toPasteboard (tokenFieldCell: NSTokenFieldCell; objects: NSArray; pboard: NSPasteboard): ObjCBOOL; message 'tokenFieldCell:writeRepresentedObjects:toPasteboard:';
    function tokenFieldCell_readFromPasteboard (tokenFieldCell: NSTokenFieldCell; pboard: NSPasteboard): NSArray; message 'tokenFieldCell:readFromPasteboard:';
    function tokenFieldCell_menuForRepresentedObject (tokenFieldCell: NSTokenFieldCell; representedObject: id): NSMenu; message 'tokenFieldCell:menuForRepresentedObject:';
    function tokenFieldCell_hasMenuForRepresentedObject (tokenFieldCell: NSTokenFieldCell; representedObject: id): ObjCBOOL; message 'tokenFieldCell:hasMenuForRepresentedObject:';
    function tokenFieldCell_styleForRepresentedObject (tokenFieldCell: NSTokenFieldCell; representedObject: id): NSTokenStyle; message 'tokenFieldCell:styleForRepresentedObject:';
  end;
{$endif}

