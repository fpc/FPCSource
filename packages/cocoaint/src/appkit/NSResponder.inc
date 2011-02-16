{ Parsed from Appkit.framework NSResponder.h }

{$ifdef TYPES}
{$ifndef NSRESPONDER_PAS_T}
{$define NSRESPONDER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSRESPONDER_PAS_R}
{$define NSRESPONDER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSRESPONDER_PAS_F}
{$define NSRESPONDER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSRESPONDER_PAS_S}
{$define NSRESPONDER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSResponder = objcclass;
  NSResponderPointer = ^NSResponder;
  NSResponderPtr = NSResponderPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSRESPONDER_PAS_C}
{$define NSRESPONDER_PAS_C}

{ NSResponder }
  NSResponder = objcclass external (NSObject, NSCodingProtocol)
  private
    _nextResponder: id;
    
  public
    function nextResponder: NSResponder; message 'nextResponder';
    procedure setNextResponder(aResponder: NSResponder); message 'setNextResponder:';
    function tryToPerform_with(anAction: SEL; anObject: id): Boolean; message 'tryToPerform:with:';
    function performKeyEquivalent(theEvent: NSEvent): Boolean; message 'performKeyEquivalent:';
    function validRequestorForSendType_returnType(sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure mouseDown(theEvent: NSEvent); message 'mouseDown:';
    procedure rightMouseDown(theEvent: NSEvent); message 'rightMouseDown:';
    procedure otherMouseDown(theEvent: NSEvent); message 'otherMouseDown:';
    procedure mouseUp(theEvent: NSEvent); message 'mouseUp:';
    procedure rightMouseUp(theEvent: NSEvent); message 'rightMouseUp:';
    procedure otherMouseUp(theEvent: NSEvent); message 'otherMouseUp:';
    procedure mouseMoved(theEvent: NSEvent); message 'mouseMoved:';
    procedure mouseDragged(theEvent: NSEvent); message 'mouseDragged:';
    procedure scrollWheel(theEvent: NSEvent); message 'scrollWheel:';
    procedure rightMouseDragged(theEvent: NSEvent); message 'rightMouseDragged:';
    procedure otherMouseDragged(theEvent: NSEvent); message 'otherMouseDragged:';
    procedure mouseEntered(theEvent: NSEvent); message 'mouseEntered:';
    procedure mouseExited(theEvent: NSEvent); message 'mouseExited:';
    procedure keyDown(theEvent: NSEvent); message 'keyDown:';
    procedure keyUp(theEvent: NSEvent); message 'keyUp:';
    procedure flagsChanged(theEvent: NSEvent); message 'flagsChanged:';
    procedure tabletPoint(theEvent: NSEvent); message 'tabletPoint:';
    procedure tabletProximity(theEvent: NSEvent); message 'tabletProximity:';
    procedure cursorUpdate(event: NSEvent); message 'cursorUpdate:';
    procedure magnifyWithEvent(event: NSEvent); message 'magnifyWithEvent:';
    procedure rotateWithEvent(event: NSEvent); message 'rotateWithEvent:';
    procedure swipeWithEvent(event: NSEvent); message 'swipeWithEvent:';
    procedure beginGestureWithEvent(event: NSEvent); message 'beginGestureWithEvent:';
    procedure endGestureWithEvent(event: NSEvent); message 'endGestureWithEvent:';
    procedure touchesBeganWithEvent(event: NSEvent); message 'touchesBeganWithEvent:';
    procedure touchesMovedWithEvent(event: NSEvent); message 'touchesMovedWithEvent:';
    procedure touchesEndedWithEvent(event: NSEvent); message 'touchesEndedWithEvent:';
    procedure touchesCancelledWithEvent(event: NSEvent); message 'touchesCancelledWithEvent:';
    procedure noResponderFor(eventSelector: SEL); message 'noResponderFor:';
    function acceptsFirstResponder: Boolean; message 'acceptsFirstResponder';
    function becomeFirstResponder: Boolean; message 'becomeFirstResponder';
    function resignFirstResponder: Boolean; message 'resignFirstResponder';
    procedure interpretKeyEvents(eventArray: NSArray); message 'interpretKeyEvents:';
    procedure flushBufferedKeyEvents; message 'flushBufferedKeyEvents';
    procedure setMenu(menu_: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    procedure showContextHelp(sender: id); message 'showContextHelp:';
    procedure helpRequested(eventPtr: NSEvent); message 'helpRequested:';
    function shouldBeTreatedAsInkEvent(theEvent: NSEvent): Boolean; message 'shouldBeTreatedAsInkEvent:';

    { Adopted Protocols }
    procedure encodeWithCoder(aCoder: NSCoder);
    function initWithCoder(aDecoder: NSCoder): id;
  end;

{ NSKeyboardUICategory }
  NSKeyboardUICategory = objccategory external (NSResponder)
    function performMnemonic(theString: NSString): Boolean; message 'performMnemonic:';
  end;

{ NSStandardKeyBindingMethodsCategory }
  NSStandardKeyBindingMethodsCategory = objccategory external (NSResponder)
    procedure insertText(insertString: id); message 'insertText:';
    procedure doCommandBySelector(aSelector: SEL); message 'doCommandBySelector:';
    procedure moveForward(sender: id); message 'moveForward:';
    procedure moveRight(sender: id); message 'moveRight:';
    procedure moveBackward(sender: id); message 'moveBackward:';
    procedure moveLeft(sender: id); message 'moveLeft:';
    procedure moveUp(sender: id); message 'moveUp:';
    procedure moveDown(sender: id); message 'moveDown:';
    procedure moveWordForward(sender: id); message 'moveWordForward:';
    procedure moveWordBackward(sender: id); message 'moveWordBackward:';
    procedure moveToBeginningOfLine(sender: id); message 'moveToBeginningOfLine:';
    procedure moveToEndOfLine(sender: id); message 'moveToEndOfLine:';
    procedure moveToBeginningOfParagraph(sender: id); message 'moveToBeginningOfParagraph:';
    procedure moveToEndOfParagraph(sender: id); message 'moveToEndOfParagraph:';
    procedure moveToEndOfDocument(sender: id); message 'moveToEndOfDocument:';
    procedure moveToBeginningOfDocument(sender: id); message 'moveToBeginningOfDocument:';
    procedure pageDown(sender: id); message 'pageDown:';
    procedure pageUp(sender: id); message 'pageUp:';
    procedure centerSelectionInVisibleArea(sender: id); message 'centerSelectionInVisibleArea:';
    procedure moveBackwardAndModifySelection(sender: id); message 'moveBackwardAndModifySelection:';
    procedure moveForwardAndModifySelection(sender: id); message 'moveForwardAndModifySelection:';
    procedure moveWordForwardAndModifySelection(sender: id); message 'moveWordForwardAndModifySelection:';
    procedure moveWordBackwardAndModifySelection(sender: id); message 'moveWordBackwardAndModifySelection:';
    procedure moveUpAndModifySelection(sender: id); message 'moveUpAndModifySelection:';
    procedure moveDownAndModifySelection(sender: id); message 'moveDownAndModifySelection:';
    procedure moveToBeginningOfLineAndModifySelection(sender: id); message 'moveToBeginningOfLineAndModifySelection:';
    procedure moveToEndOfLineAndModifySelection(sender: id); message 'moveToEndOfLineAndModifySelection:';
    procedure moveToBeginningOfParagraphAndModifySelection(sender: id); message 'moveToBeginningOfParagraphAndModifySelection:';
    procedure moveToEndOfParagraphAndModifySelection(sender: id); message 'moveToEndOfParagraphAndModifySelection:';
    procedure moveToEndOfDocumentAndModifySelection(sender: id); message 'moveToEndOfDocumentAndModifySelection:';
    procedure moveToBeginningOfDocumentAndModifySelection(sender: id); message 'moveToBeginningOfDocumentAndModifySelection:';
    procedure pageDownAndModifySelection(sender: id); message 'pageDownAndModifySelection:';
    procedure pageUpAndModifySelection(sender: id); message 'pageUpAndModifySelection:';
    procedure moveParagraphForwardAndModifySelection(sender: id); message 'moveParagraphForwardAndModifySelection:';
    procedure moveParagraphBackwardAndModifySelection(sender: id); message 'moveParagraphBackwardAndModifySelection:';
    procedure moveWordRight(sender: id); message 'moveWordRight:';
    procedure moveWordLeft(sender: id); message 'moveWordLeft:';
    procedure moveRightAndModifySelection(sender: id); message 'moveRightAndModifySelection:';
    procedure moveLeftAndModifySelection(sender: id); message 'moveLeftAndModifySelection:';
    procedure moveWordRightAndModifySelection(sender: id); message 'moveWordRightAndModifySelection:';
    procedure moveWordLeftAndModifySelection(sender: id); message 'moveWordLeftAndModifySelection:';
    procedure moveToLeftEndOfLine(sender: id); message 'moveToLeftEndOfLine:';
    procedure moveToRightEndOfLine(sender: id); message 'moveToRightEndOfLine:';
    procedure moveToLeftEndOfLineAndModifySelection(sender: id); message 'moveToLeftEndOfLineAndModifySelection:';
    procedure moveToRightEndOfLineAndModifySelection(sender: id); message 'moveToRightEndOfLineAndModifySelection:';
    procedure scrollPageUp(sender: id); message 'scrollPageUp:';
    procedure scrollPageDown(sender: id); message 'scrollPageDown:';
    procedure scrollLineUp(sender: id); message 'scrollLineUp:';
    procedure scrollLineDown(sender: id); message 'scrollLineDown:';
    procedure scrollToBeginningOfDocument(sender: id); message 'scrollToBeginningOfDocument:';
    procedure scrollToEndOfDocument(sender: id); message 'scrollToEndOfDocument:';
    procedure transpose(sender: id); message 'transpose:';
    procedure transposeWords(sender: id); message 'transposeWords:';
    procedure selectAll(sender: id); message 'selectAll:';
    procedure selectParagraph(sender: id); message 'selectParagraph:';
    procedure selectLine(sender: id); message 'selectLine:';
    procedure selectWord(sender: id); message 'selectWord:';
    procedure indent(sender: id); message 'indent:';
    procedure insertTab(sender: id); message 'insertTab:';
    procedure insertBacktab(sender: id); message 'insertBacktab:';
    procedure insertNewline(sender: id); message 'insertNewline:';
    procedure insertParagraphSeparator(sender: id); message 'insertParagraphSeparator:';
    procedure insertNewlineIgnoringFieldEditor(sender: id); message 'insertNewlineIgnoringFieldEditor:';
    procedure insertTabIgnoringFieldEditor(sender: id); message 'insertTabIgnoringFieldEditor:';
    procedure insertLineBreak(sender: id); message 'insertLineBreak:';
    procedure insertContainerBreak(sender: id); message 'insertContainerBreak:';
    procedure insertSingleQuoteIgnoringSubstitution(sender: id); message 'insertSingleQuoteIgnoringSubstitution:';
    procedure insertDoubleQuoteIgnoringSubstitution(sender: id); message 'insertDoubleQuoteIgnoringSubstitution:';
    procedure changeCaseOfLetter(sender: id); message 'changeCaseOfLetter:';
    procedure uppercaseWord(sender: id); message 'uppercaseWord:';
    procedure lowercaseWord(sender: id); message 'lowercaseWord:';
    procedure capitalizeWord(sender: id); message 'capitalizeWord:';
    procedure deleteForward(sender: id); message 'deleteForward:';
    procedure deleteBackward(sender: id); message 'deleteBackward:';
    procedure deleteBackwardByDecomposingPreviousCharacter(sender: id); message 'deleteBackwardByDecomposingPreviousCharacter:';
    procedure deleteWordForward(sender: id); message 'deleteWordForward:';
    procedure deleteWordBackward(sender: id); message 'deleteWordBackward:';
    procedure deleteToBeginningOfLine(sender: id); message 'deleteToBeginningOfLine:';
    procedure deleteToEndOfLine(sender: id); message 'deleteToEndOfLine:';
    procedure deleteToBeginningOfParagraph(sender: id); message 'deleteToBeginningOfParagraph:';
    procedure deleteToEndOfParagraph(sender: id); message 'deleteToEndOfParagraph:';
    procedure yank(sender: id); message 'yank:';
    procedure complete(sender: id); message 'complete:';
    procedure setMark(sender: id); message 'setMark:';
    procedure deleteToMark(sender: id); message 'deleteToMark:';
    procedure selectToMark(sender: id); message 'selectToMark:';
    procedure swapWithMark(sender: id); message 'swapWithMark:';
    procedure cancelOperation(sender: id); message 'cancelOperation:';
    procedure makeBaseWritingDirectionNatural(sender: id); message 'makeBaseWritingDirectionNatural:';
    procedure makeBaseWritingDirectionLeftToRight(sender: id); message 'makeBaseWritingDirectionLeftToRight:';
    procedure makeBaseWritingDirectionRightToLeft(sender: id); message 'makeBaseWritingDirectionRightToLeft:';
    procedure makeTextWritingDirectionNatural(sender: id); message 'makeTextWritingDirectionNatural:';
    procedure makeTextWritingDirectionLeftToRight(sender: id); message 'makeTextWritingDirectionLeftToRight:';
    procedure makeTextWritingDirectionRightToLeft(sender: id); message 'makeTextWritingDirectionRightToLeft:';
  end;

{ NSUndoSupportCategory }
  NSUndoSupportCategory = objccategory external (NSResponder)
    function undoManager: NSUndoManager; message 'undoManager';
  end;

{ NSErrorPresentationCategory }
  NSErrorPresentationCategory = objccategory external (NSResponder)
    procedure presentError_modalForWindow_delegate_didPresentSelector_contextInfo(error: NSError; window: NSWindow; delegate: id; didPresentSelector: SEL; contextInfo: Pointer); message 'presentError:modalForWindow:delegate:didPresentSelector:contextInfo:';
    function presentError(error: NSError): Boolean; message 'presentError:';
    function willPresentError(error: NSError): NSError; message 'willPresentError:';
  end;

{$endif}
{$endif}
