{ Parsed from AppKit.framework NSSearchFieldCell.h }


{$ifdef TYPES}
type
  NSSearchFieldCellPtr = ^NSSearchFieldCell;
{$endif}

{$ifdef TYPES}
const
  NSSearchFieldRecentsTitleMenuItemTag = 1000;
  NSSearchFieldRecentsMenuItemTag = 1001;
  NSSearchFieldClearRecentsMenuItemTag = 1002;
  NSSearchFieldNoRecentsMenuItemTag = 1003;
{$endif}

{$ifdef CLASSES}

type
  NSSearchFieldCell = objcclass external (NSTextFieldCell)
  private
    __sfFlags: bitpacked record
      case byte of
        0: (_anonBitField___sfFlags0: cuint);
        1: (
          sendsWholeSearchString: 0..1;
          maximumRecents: 0..((1 shl 8)-1);
          cancelVisible: 0..1;
          reserved2: 0..((1 shl 2)-1);
          disableText: 0..1;
          menuTracking: 0..1;
          deferredUpdate: 0..1;
          sendsImmediately: 0..1;
          centeredLook: 0..1;
          renderingCentered: 0..1;
          becomeTransition: 0..1;
          resignTransition: 0..1;
          reserved: 0..((1 shl 12)-1);
        );
      end;
    _searchButtonCell: NSButtonCell;
    _cancelButtonCell: NSButtonCell;
    _searchMenuTemplate: NSMenu;
    _recentsAutosaveName: NSString;
    _recentSearches: NSMutableArray;
    _searchMenu: NSMenu;
    _partialStringTimer: NSTimer;
    _reserved1: cuint;
    _reserved2: cuint;
    _reserved3: cuint;
    _reserved4: cuint;
  public
    procedure setSearchButtonCell(newValue: NSButtonCell); message 'setSearchButtonCell:';
    function searchButtonCell: NSButtonCell; message 'searchButtonCell';
    procedure setCancelButtonCell(newValue: NSButtonCell); message 'setCancelButtonCell:';
    function cancelButtonCell: NSButtonCell; message 'cancelButtonCell';
    procedure resetSearchButtonCell; message 'resetSearchButtonCell';
    procedure resetCancelButtonCell; message 'resetCancelButtonCell';
    function searchTextRectForBounds (rect: NSRect): NSRect; message 'searchTextRectForBounds:';
    function searchButtonRectForBounds (rect: NSRect): NSRect; message 'searchButtonRectForBounds:';
    function cancelButtonRectForBounds (rect: NSRect): NSRect; message 'cancelButtonRectForBounds:';
    procedure setSearchMenuTemplate(newValue: NSMenu); message 'setSearchMenuTemplate:';
    function searchMenuTemplate: NSMenu; message 'searchMenuTemplate';
    procedure setSendsWholeSearchString(newValue: ObjCBOOL); message 'setSendsWholeSearchString:';
    function sendsWholeSearchString: ObjCBOOL; message 'sendsWholeSearchString';
    procedure setMaximumRecents(newValue: NSInteger); message 'setMaximumRecents:';
    function maximumRecents: NSInteger; message 'maximumRecents';
    procedure setRecentSearches(newValue: NSArray); message 'setRecentSearches:';
    function recentSearches: NSArray; message 'recentSearches';
    procedure setRecentsAutosaveName(newValue: NSString); message 'setRecentsAutosaveName:';
    function recentsAutosaveName: NSString; message 'recentsAutosaveName';
    procedure setSendsSearchStringImmediately(newValue: ObjCBOOL); message 'setSendsSearchStringImmediately:';
    function sendsSearchStringImmediately: ObjCBOOL; message 'sendsSearchStringImmediately';
  end;
{$endif}

