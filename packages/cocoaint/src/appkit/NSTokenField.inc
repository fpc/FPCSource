{ Parsed from AppKit.framework NSTokenField.h }


{$ifdef TYPES}
type
  NSTokenFieldPtr = ^NSTokenField;
  NSTokenFieldDelegateProtocolPtr = ^NSTokenFieldDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSTokenField = objcclass external (NSTextField)
  private
    _reserved1: ObjCBOOL;
    _reserved2: ObjCBOOL;
    _reserved3: ObjCBOOL;
    _reserved4: ObjCBOOL;
    _trackingRectTag: NSTrackingRectTag;
    _reserved5: id;
    _reserved6: id;
    _reserved7: id;
  public
    procedure setDelegate (anObject: NSTokenFieldDelegateProtocol); message 'setDelegate:';
    function delegate_: NSTokenFieldDelegateProtocol; message 'delegate';
    procedure setTokenStyle(newValue: NSTokenStyle); message 'setTokenStyle:';
    function tokenStyle: NSTokenStyle; message 'tokenStyle';
    procedure setCompletionDelay(newValue: NSTimeInterval); message 'setCompletionDelay:';
    function completionDelay: NSTimeInterval; message 'completionDelay';
    class function defaultCompletionDelay: NSTimeInterval; message 'defaultCompletionDelay';
    procedure setTokenizingCharacterSet(newValue: NSCharacterSet); message 'setTokenizingCharacterSet:';
    function tokenizingCharacterSet: NSCharacterSet; message 'tokenizingCharacterSet';
    class function defaultTokenizingCharacterSet: NSCharacterSet; message 'defaultTokenizingCharacterSet';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTokenFieldDelegateProtocol = objcprotocol external name 'NSTokenFieldDelegate' (NSTextFieldDelegateProtocol)
  optional
    function tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem (tokenField: NSTokenField; substring: NSString; tokenIndex: NSInteger; selectedIndex: NSIntegerPtr): NSArray; message 'tokenField:completionsForSubstring:indexOfToken:indexOfSelectedItem:';
    function tokenField_shouldAddObjects_atIndex (tokenField: NSTokenField; tokens: NSArray; index: NSUInteger): NSArray; message 'tokenField:shouldAddObjects:atIndex:';
    function tokenField_displayStringForRepresentedObject (tokenField: NSTokenField; representedObject: id): NSString; message 'tokenField:displayStringForRepresentedObject:';
    function tokenField_editingStringForRepresentedObject (tokenField: NSTokenField; representedObject: id): NSString; message 'tokenField:editingStringForRepresentedObject:';
    function tokenField_representedObjectForEditingString (tokenField: NSTokenField; editingString: NSString): id; message 'tokenField:representedObjectForEditingString:';
    function tokenField_writeRepresentedObjects_toPasteboard (tokenField: NSTokenField; objects: NSArray; pboard: NSPasteboard): ObjCBOOL; message 'tokenField:writeRepresentedObjects:toPasteboard:';
    function tokenField_readFromPasteboard (tokenField: NSTokenField; pboard: NSPasteboard): NSArray; message 'tokenField:readFromPasteboard:';
    function tokenField_menuForRepresentedObject (tokenField: NSTokenField; representedObject: id): NSMenu; message 'tokenField:menuForRepresentedObject:';
    function tokenField_hasMenuForRepresentedObject (tokenField: NSTokenField; representedObject: id): ObjCBOOL; message 'tokenField:hasMenuForRepresentedObject:';
    function tokenField_styleForRepresentedObject (tokenField: NSTokenField; representedObject: id): NSTokenStyle; message 'tokenField:styleForRepresentedObject:';
  end;
{$endif}

