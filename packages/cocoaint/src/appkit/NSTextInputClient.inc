{ Parsed from Appkit.framework NSTextInputClient.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:48 ICT 2009 }


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
{$ifndef NSTEXTINPUTCLIENT_PAS_T}
{$define NSTEXTINPUTCLIENT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextInputClientProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSTEXTINPUTCLIENT_PAS_P}
{$define NSTEXTINPUTCLIENT_PAS_P}
  
{ NSTextInputClient Protocol }
  NSTextInputClientProtocol = objcprotocol
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
    function attributedString: NSAttributedString; message 'attributedString';
    function fractionOfDistanceThroughGlyphForPoint(aPoint: NSPoint): CGFloat; message 'fractionOfDistanceThroughGlyphForPoint:';
    function baselineDeltaForCharacterAtIndex(anIndex: NSUInteger): CGFloat; message 'baselineDeltaForCharacterAtIndex:';
    function windowLevel: NSInteger; message 'windowLevel';
  end; external name 'NSTextInputClient';
{$endif}
{$endif}
