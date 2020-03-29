{ Parsed from AppKit.framework NSTextStorage.h }


{$ifdef TYPES}
type
  NSTextStoragePtr = ^NSTextStorage;
  NSTextStorageDelegateProtocolPtr = ^NSTextStorageDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSTextStorageEditedOptions = NSUInteger;
  NSTextStorageEditedOptionsPtr = ^NSTextStorageEditedOptions;

const
  NSTextStorageEditedAttributes = 1 shl 0;
  NSTextStorageEditedCharacters = 1 shl 1;
{$endif}

{$ifdef CLASSES}

type
  NSTextStorage = objcclass external (NSMutableAttributedString)
  private
    _editedRange: NSRange;
    _editedDelta: NSInteger;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          editedMask: 0..((1 shl 8)-1);
          int: 0..((1 shl 8)-1);
          disabled: 0..((1 shl 16)-1);
          {$if defined(__LP64__)}
          int_: 0..((1 shl 32)-1);
          {$endif}
        );
      end;
    _layoutManagers: NSMutableArray;
    _sideData: id;
  public
    procedure addLayoutManager (obj: NSLayoutManager); message 'addLayoutManager:';
    procedure removeLayoutManager (obj: NSLayoutManager); message 'removeLayoutManager:';
    function layoutManagers: NSArray; message 'layoutManagers';
    procedure edited_range_changeInLength (editedMask: NSUInteger; range: NSRange; delta: NSInteger); message 'edited:range:changeInLength:';
    procedure processEditing; message 'processEditing';
    procedure invalidateAttributesInRange (range: NSRange); message 'invalidateAttributesInRange:';
    procedure ensureAttributesAreFixedInRange (range: NSRange); message 'ensureAttributesAreFixedInRange:';
    function fixesAttributesLazily: ObjCBOOL; message 'fixesAttributesLazily';
    function editedMask: NSUInteger; message 'editedMask';
    function editedRange: NSRange; message 'editedRange';
    function changeInLength: NSInteger; message 'changeInLength';
    procedure setDelegate(newValue: NSTextStorageDelegateProtocol); message 'setDelegate:';
    function delegate: NSTextStorageDelegateProtocol; message 'delegate';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSTextStorageDelegateProtocol = objcprotocol external name 'NSTextStorageDelegate' (NSObjectProtocol)
  optional
    procedure textStorageWillProcessEditing (notification: NSNotification); message 'textStorageWillProcessEditing:';
    procedure textStorageDidProcessEditing (notification: NSNotification); message 'textStorageDidProcessEditing:';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTextStorageWillProcessEditingNotification: NSString; cvar; external;
  NSTextStorageDidProcessEditingNotification: NSString; cvar; external;
{$endif}

