{ Parsed from Appkit.framework NSTextInputClient.h }

{$ifdef TYPES}
{$ifndef NSTEXTINPUTCLIENT_PAS_T}
{$define NSTEXTINPUTCLIENT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSTEXTINPUTCLIENT_PAS_R}
{$define NSTEXTINPUTCLIENT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSTEXTINPUTCLIENT_PAS_F}
{$define NSTEXTINPUTCLIENT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSTEXTINPUTCLIENT_PAS_S}
{$define NSTEXTINPUTCLIENT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextInputClientProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTEXTINPUTCLIENT_PAS_P}
{$define NSTEXTINPUTCLIENT_PAS_P}
  
{ NSTextInputClient Protocol }
  NSTextInputClientProtocol = objcprotocol external name 'NSTextInputClient'
  required
    procedure insertText_replacementRange(aString: id; replacementRange: NSRange); message 'insertText:replacementRange:';
    procedure setMarkedText_selectedRange_replacementRange(aString: id; selectedRange: NSRange; replacementRange: NSRange); message 'setMarkedText:selectedRange:replacementRange:';
    procedure unmarkText; message 'unmarkText';
    function selectedRange: NSRange; message 'selectedRange';
    function markedRange: NSRange; message 'markedRange';
    function hasMarkedText: Boolean; message 'hasMarkedText';
    function attributedSubstringForProposedRange_actualRange(aRange: NSRange; actualRange: NSRangePointer): NSAttributedString; message 'attributedSubstringForProposedRange:actualRange:';
    function validAttributesForMarkedText: NSArray; message 'validAttributesForMarkedText';
    function firstRectForCharacterRange_actualRange(aRange: NSRange; actualRange: NSRangePointer): NSRect; message 'firstRectForCharacterRange:actualRange:';
    function characterIndexForPoint(aPoint: NSPoint): NSUInteger; message 'characterIndexForPoint:';
  optional
    function attributedString: NSAttributedString; message 'attributedString';
    function fractionOfDistanceThroughGlyphForPoint(aPoint: NSPoint): CGFloat; message 'fractionOfDistanceThroughGlyphForPoint:';
    function baselineDeltaForCharacterAtIndex(anIndex: NSUInteger): CGFloat; message 'baselineDeltaForCharacterAtIndex:';
    function windowLevel: NSInteger; message 'windowLevel';
    function drawsVerticallyForCharacterAtIndex(charIndex: NSUInteger): Boolean; message 'drawsVerticallyForCharacterAtIndex:';
  end;
{$endif}
{$endif}
