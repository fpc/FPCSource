{ Parsed from Appkit.framework NSTextStorage.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSTEXTSTORAGE_PAS_H}
{$define NSTEXTSTORAGE_PAS_H}
type
  NSTextStoragePointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSTEXTSTORAGE_PAS_T}
{$define NSTEXTSTORAGE_PAS_T}

{ External string constants }
var
  NSTextStorageWillProcessEditingNotification: NSString; external name '_NSTextStorageWillProcessEditingNotification';
  NSTextStorageDidProcessEditingNotification: NSString; external name '_NSTextStorageDidProcessEditingNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextStorage = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTSTORAGE_PAS_C}
{$define NSTEXTSTORAGE_PAS_C}

{ NSTextStorage }
  NSTextStorage = objcclass(NSMutableAttributedString)
  private
    __editedRange: NSRange;
    __editedDelta: NSInteger;
    __flags: bitpacked record
        editedMask: 0..((1 shl 8)-1);
        inFSC: 0..1;
        int: 0..((1 shl 7)-1);
        disabled: 0..((1 shl 16)-1);
        {$ifdef cpu64}
        int: 0..((1 shl 32)-1);
        {$endif}
      end;
    __layoutManagers: NSMutableArray;
    __sideData: id;
    
  public
    class function alloc: NSTextStorage; message 'alloc';

    procedure addLayoutManager_setTextStorage(obj: NSLayoutManager); message 'addLayoutManager:';
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

    { Category: Scripting }
    function attributeRuns: NSArray; message 'attributeRuns';
    procedure setAttributeRuns(attributeRuns_: NSArray); message 'setAttributeRuns:';
    function paragraphs: NSArray; message 'paragraphs';
    procedure setParagraphs(paragraphs_: NSArray); message 'setParagraphs:';
    function words: NSArray; message 'words';
    procedure setWords(words_: NSArray); message 'setWords:';
    function characters: NSArray; message 'characters';
    procedure setCharacters(characters_: NSArray); message 'setCharacters:';
    function font: NSFont; message 'font';
    procedure setFont(font_: NSFont); message 'setFont:';
    function foregroundColor: NSColor; message 'foregroundColor';
    procedure setForegroundColor(color: NSColor); message 'setForegroundColor:';
  end; external;

{$endif}
{$endif}
