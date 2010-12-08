{ Parsed from Appkit.framework NSTextStorage.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSTEXTSTORAGE_PAS_T}
{$define NSTEXTSTORAGE_PAS_T}

{ Constants }

const
  NSTextStorageEditedAttributes = 1;
  NSTextStorageEditedCharacters = 2;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTSTORAGE_PAS_R}
{$define NSTEXTSTORAGE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTSTORAGE_PAS_F}
{$define NSTEXTSTORAGE_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTSTORAGE_PAS_S}
{$define NSTEXTSTORAGE_PAS_S}

{ External string constants }
var
  NSTextStorageWillProcessEditingNotification: NSString; cvar; external;
  NSTextStorageDidProcessEditingNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextStorageDelegateProtocol = objcprotocol;
  NSTextStorage = objcclass;
  NSTextStoragePointer = ^NSTextStorage;
  NSTextStoragePtr = NSTextStoragePointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTSTORAGE_PAS_C}
{$define NSTEXTSTORAGE_PAS_C}

{ NSTextStorage }
  NSTextStorage = objcclass(NSMutableAttributedString)
  private
    _editedRange: NSRange;
    _editedDelta: NSInteger;
    __flags: bitpacked record
      case byte of
       0: (_anonbitfield_NSTextStorage2: cuint);
       1: (
        _editedMask: 0..((1 shl 8)-1);
        _anonbitfield_NSTextStorage0: 0..((1 shl 8)-1);
        disabled: 0..((1 shl 16)-1);
        {$ifdef cpu64}
        _anonbitfield_NSTextStorage1: 0..((1 shl 32)-1);
        {$endif}
      );
      end;
    _layoutManagers: NSMutableArray;
    _sideData: id;
    
  public
    class function alloc: NSTextStorage; message 'alloc';

    procedure addLayoutManager(obj: NSLayoutManager); message 'addLayoutManager:';
    procedure removeLayoutManager(obj: NSLayoutManager); message 'removeLayoutManager:';
    function layoutManagers: NSArray; message 'layoutManagers';
    procedure edited_range_changeInLength(editedMask_: NSUInteger; range: NSRange; delta: NSInteger); message 'edited:range:changeInLength:';
    procedure processEditing; message 'processEditing';
    procedure invalidateAttributesInRange(range: NSRange); message 'invalidateAttributesInRange:';
    procedure ensureAttributesAreFixedInRange(range: NSRange); message 'ensureAttributesAreFixedInRange:';
    function fixesAttributesLazily: Boolean; message 'fixesAttributesLazily';
    function editedMask: NSUInteger; message 'editedMask';
    function editedRange: NSRange; message 'editedRange';
    function changeInLength: NSInteger; message 'changeInLength';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTEXTSTORAGE_PAS_P}
{$define NSTEXTSTORAGE_PAS_P}
  
{ NSTextStorageDelegate Protocol }
  NSTextStorageDelegateProtocol = objcprotocol
    procedure textStorageWillProcessEditing(notification: NSNotification); message 'textStorageWillProcessEditing:';
    procedure textStorageDidProcessEditing(notification: NSNotification); message 'textStorageDidProcessEditing:';
  end; external name 'NSTextStorageDelegate';
{$endif}
{$endif}
