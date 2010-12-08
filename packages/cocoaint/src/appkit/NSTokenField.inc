{ Parsed from Appkit.framework NSTokenField.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSTOKENFIELD_PAS_T}
{$define NSTOKENFIELD_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTOKENFIELD_PAS_R}
{$define NSTOKENFIELD_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTOKENFIELD_PAS_F}
{$define NSTOKENFIELD_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTOKENFIELD_PAS_S}
{$define NSTOKENFIELD_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTokenFieldDelegateProtocol = objcprotocol;
  NSTokenField = objcclass;
  NSTokenFieldPointer = ^NSTokenField;
  NSTokenFieldPtr = NSTokenFieldPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTOKENFIELD_PAS_C}
{$define NSTOKENFIELD_PAS_C}

{ NSTokenField }
  NSTokenField = objcclass(NSTextField)
    
  public
    class function alloc: NSTokenField; message 'alloc';

    procedure setDelegate(anObject: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    procedure setTokenStyle(style: NSTokenStyle); message 'setTokenStyle:';
    function tokenStyle: NSTokenStyle; message 'tokenStyle';
    procedure setCompletionDelay(delay: NSTimeInterval); message 'setCompletionDelay:';
    function completionDelay: NSTimeInterval; message 'completionDelay';
    class function defaultCompletionDelay: NSTimeInterval; message 'defaultCompletionDelay';
    procedure setTokenizingCharacterSet(characterSet: NSCharacterSet); message 'setTokenizingCharacterSet:';
    function tokenizingCharacterSet: NSCharacterSet; message 'tokenizingCharacterSet';
    class function defaultTokenizingCharacterSet: NSCharacterSet; message 'defaultTokenizingCharacterSet';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTOKENFIELD_PAS_P}
{$define NSTOKENFIELD_PAS_P}
  
{ NSTokenFieldDelegate Protocol }
  NSTokenFieldDelegateProtocol = objcprotocol
    function tokenField_completionsForSubstring_indexOfToken_indexOfSelectedItem(tokenField: NSTokenField; substring: NSString; tokenIndex: NSInteger; selectedIndex: NSIntegerPtr): NSArray; message 'tokenField:completionsForSubstring:indexOfToken:indexOfSelectedItem:';
    function tokenField_shouldAddObjects_atIndex(tokenField: NSTokenField; tokens: NSArray; index: NSUInteger): NSArray; message 'tokenField:shouldAddObjects:atIndex:';
    function tokenField_displayStringForRepresentedObject(tokenField: NSTokenField; representedObject: id): NSString; message 'tokenField:displayStringForRepresentedObject:';
    function tokenField_editingStringForRepresentedObject(tokenField: NSTokenField; representedObject: id): NSString; message 'tokenField:editingStringForRepresentedObject:';
    function tokenField_representedObjectForEditingString(tokenField: NSTokenField; editingString: NSString): id; message 'tokenField:representedObjectForEditingString:';
    function tokenField_writeRepresentedObjects_toPasteboard(tokenField: NSTokenField; objects: NSArray; pboard: NSPasteboard): Boolean; message 'tokenField:writeRepresentedObjects:toPasteboard:';
    function tokenField_readFromPasteboard(tokenField: NSTokenField; pboard: NSPasteboard): NSArray; message 'tokenField:readFromPasteboard:';
    function tokenField_menuForRepresentedObject(tokenField: NSTokenField; representedObject: id): NSMenu; message 'tokenField:menuForRepresentedObject:';
    function tokenField_hasMenuForRepresentedObject(tokenField: NSTokenField; representedObject: id): Boolean; message 'tokenField:hasMenuForRepresentedObject:';
    function tokenField_styleForRepresentedObject(tokenField: NSTokenField; representedObject: id): NSTokenStyle; message 'tokenField:styleForRepresentedObject:';
  end; external name 'NSTokenFieldDelegate';
{$endif}
{$endif}
