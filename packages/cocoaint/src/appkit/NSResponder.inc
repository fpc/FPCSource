{ Parsed from AppKit.framework NSResponder.h }


{$ifdef TYPES}
type
  NSResponderPtr = ^NSResponder;
{$endif}

{$ifdef CLASSES}

type
  NSResponder = objcclass external (NSObject, NSCodingProtocol)
  private
    _nextResponder: id;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure setNextResponder(newValue: NSResponder); message 'setNextResponder:';
    function nextResponder: NSResponder; message 'nextResponder';
    function tryToPerform_with (anAction: SEL; anObject: id): ObjCBOOL; message 'tryToPerform:with:';
    function performKeyEquivalent (theEvent: NSEvent): ObjCBOOL; message 'performKeyEquivalent:';
    function validRequestorForSendType_returnType (sendType: NSString; returnType: NSString): id; message 'validRequestorForSendType:returnType:';
    procedure mouseDown (theEvent: NSEvent); message 'mouseDown:';
    procedure rightMouseDown (theEvent: NSEvent); message 'rightMouseDown:';
    procedure otherMouseDown (theEvent: NSEvent); message 'otherMouseDown:';
    procedure mouseUp (theEvent: NSEvent); message 'mouseUp:';
    procedure rightMouseUp (theEvent: NSEvent); message 'rightMouseUp:';
    procedure otherMouseUp (theEvent: NSEvent); message 'otherMouseUp:';
    procedure mouseMoved (theEvent: NSEvent); message 'mouseMoved:';
    procedure mouseDragged (theEvent: NSEvent); message 'mouseDragged:';
    procedure scrollWheel (theEvent: NSEvent); message 'scrollWheel:';
    procedure rightMouseDragged (theEvent: NSEvent); message 'rightMouseDragged:';
    procedure otherMouseDragged (theEvent: NSEvent); message 'otherMouseDragged:';
    procedure mouseEntered (theEvent: NSEvent); message 'mouseEntered:';
    procedure mouseExited (theEvent: NSEvent); message 'mouseExited:';
    procedure keyDown (theEvent: NSEvent); message 'keyDown:';
    procedure keyUp (theEvent: NSEvent); message 'keyUp:';
    procedure flagsChanged (theEvent: NSEvent); message 'flagsChanged:';
    procedure tabletPoint (theEvent: NSEvent); message 'tabletPoint:';
    procedure tabletProximity (theEvent: NSEvent); message 'tabletProximity:';
    procedure cursorUpdate (event: NSEvent); message 'cursorUpdate:'; { available in 10_5 }
    procedure magnifyWithEvent (event: NSEvent); message 'magnifyWithEvent:'; { available in 10_5 }
    procedure rotateWithEvent (event: NSEvent); message 'rotateWithEvent:'; { available in 10_5 }
    procedure swipeWithEvent (event: NSEvent); message 'swipeWithEvent:'; { available in 10_5 }
    procedure beginGestureWithEvent (event: NSEvent); message 'beginGestureWithEvent:'; { available in 10_5 }
    procedure endGestureWithEvent (event: NSEvent); message 'endGestureWithEvent:'; { available in 10_5 }
    procedure smartMagnifyWithEvent (event: NSEvent); message 'smartMagnifyWithEvent:'; { available in 10_8 }
    procedure touchesBeganWithEvent (event: NSEvent); message 'touchesBeganWithEvent:'; { available in 10_6 }
    procedure touchesMovedWithEvent (event: NSEvent); message 'touchesMovedWithEvent:'; { available in 10_6 }
    procedure touchesEndedWithEvent (event: NSEvent); message 'touchesEndedWithEvent:'; { available in 10_6 }
    procedure touchesCancelledWithEvent (event: NSEvent); message 'touchesCancelledWithEvent:'; { available in 10_6 }
    procedure quickLookWithEvent (event: NSEvent); message 'quickLookWithEvent:'; { available in 10_8 }
    procedure noResponderFor (eventSelector: SEL); message 'noResponderFor:';
    function acceptsFirstResponder: ObjCBOOL; message 'acceptsFirstResponder';
    function becomeFirstResponder: ObjCBOOL; message 'becomeFirstResponder';
    function resignFirstResponder: ObjCBOOL; message 'resignFirstResponder';
    procedure interpretKeyEvents (eventArray: NSArray); message 'interpretKeyEvents:';
    procedure flushBufferedKeyEvents; message 'flushBufferedKeyEvents';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
    procedure showContextHelp (sender: id); message 'showContextHelp:';
    procedure helpRequested (eventPtr: NSEvent); message 'helpRequested:';
    function shouldBeTreatedAsInkEvent (theEvent: NSEvent): ObjCBOOL; message 'shouldBeTreatedAsInkEvent:';
    function wantsScrollEventsForSwipeTrackingOnAxis (axis: NSEventGestureAxis): ObjCBOOL; message 'wantsScrollEventsForSwipeTrackingOnAxis:'; { available in 10_7 }
    function wantsForwardedScrollEventsForAxis (axis: NSEventGestureAxis): ObjCBOOL; message 'wantsForwardedScrollEventsForAxis:'; { available in 10_7 }
    function supplementalTargetForAction_sender (action: SEL; sender: id): id; message 'supplementalTargetForAction:sender:'; { available in 10_7 }

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;


type
  NSStandardKeyBindingMethods = objccategory external (NSResponder)
    procedure insertText (insertString: id); message 'insertText:';
    procedure doCommandBySelector (aSelector: SEL); message 'doCommandBySelector:';
    procedure moveForward (sender: id); message 'moveForward:';
    procedure moveRight (sender: id); message 'moveRight:';
    procedure moveBackward (sender: id); message 'moveBackward:';
    procedure moveLeft (sender: id); message 'moveLeft:';
    procedure moveUp (sender: id); message 'moveUp:';
    procedure moveDown (sender: id); message 'moveDown:';
    procedure moveWordForward (sender: id); message 'moveWordForward:';
    procedure moveWordBackward (sender: id); message 'moveWordBackward:';
    procedure moveToBeginningOfLine (sender: id); message 'moveToBeginningOfLine:';
    procedure moveToEndOfLine (sender: id); message 'moveToEndOfLine:';
    procedure moveToBeginningOfParagraph (sender: id); message 'moveToBeginningOfParagraph:';
    procedure moveToEndOfParagraph (sender: id); message 'moveToEndOfParagraph:';
    procedure moveToEndOfDocument (sender: id); message 'moveToEndOfDocument:';
    procedure moveToBeginningOfDocument (sender: id); message 'moveToBeginningOfDocument:';
    procedure pageDown (sender: id); message 'pageDown:';
    procedure pageUp (sender: id); message 'pageUp:';
    procedure centerSelectionInVisibleArea (sender: id); message 'centerSelectionInVisibleArea:';
    procedure moveBackwardAndModifySelection (sender: id); message 'moveBackwardAndModifySelection:';
    procedure moveForwardAndModifySelection (sender: id); message 'moveForwardAndModifySelection:';
    procedure moveWordForwardAndModifySelection (sender: id); message 'moveWordForwardAndModifySelection:';
    procedure moveWordBackwardAndModifySelection (sender: id); message 'moveWordBackwardAndModifySelection:';
    procedure moveUpAndModifySelection (sender: id); message 'moveUpAndModifySelection:';
    procedure moveDownAndModifySelection (sender: id); message 'moveDownAndModifySelection:';
    procedure moveToBeginningOfLineAndModifySelection (sender: id); message 'moveToBeginningOfLineAndModifySelection:';
    procedure moveToEndOfLineAndModifySelection (sender: id); message 'moveToEndOfLineAndModifySelection:';
    procedure moveToBeginningOfParagraphAndModifySelection (sender: id); message 'moveToBeginningOfParagraphAndModifySelection:';
    procedure moveToEndOfParagraphAndModifySelection (sender: id); message 'moveToEndOfParagraphAndModifySelection:';
    procedure moveToEndOfDocumentAndModifySelection (sender: id); message 'moveToEndOfDocumentAndModifySelection:';
    procedure moveToBeginningOfDocumentAndModifySelection (sender: id); message 'moveToBeginningOfDocumentAndModifySelection:';
    procedure pageDownAndModifySelection (sender: id); message 'pageDownAndModifySelection:';
    procedure pageUpAndModifySelection (sender: id); message 'pageUpAndModifySelection:';
    procedure moveParagraphForwardAndModifySelection (sender: id); message 'moveParagraphForwardAndModifySelection:';
    procedure moveParagraphBackwardAndModifySelection (sender: id); message 'moveParagraphBackwardAndModifySelection:';
    procedure moveWordRight (sender: id); message 'moveWordRight:';
    procedure moveWordLeft (sender: id); message 'moveWordLeft:';
    procedure moveRightAndModifySelection (sender: id); message 'moveRightAndModifySelection:';
    procedure moveLeftAndModifySelection (sender: id); message 'moveLeftAndModifySelection:';
    procedure moveWordRightAndModifySelection (sender: id); message 'moveWordRightAndModifySelection:';
    procedure moveWordLeftAndModifySelection (sender: id); message 'moveWordLeftAndModifySelection:';
    procedure moveToLeftEndOfLine (sender: id); message 'moveToLeftEndOfLine:'; { available in 10_6 }
    procedure moveToRightEndOfLine (sender: id); message 'moveToRightEndOfLine:'; { available in 10_6 }
    procedure moveToLeftEndOfLineAndModifySelection (sender: id); message 'moveToLeftEndOfLineAndModifySelection:'; { available in 10_6 }
    procedure moveToRightEndOfLineAndModifySelection (sender: id); message 'moveToRightEndOfLineAndModifySelection:'; { available in 10_6 }
    procedure scrollPageUp (sender: id); message 'scrollPageUp:';
    procedure scrollPageDown (sender: id); message 'scrollPageDown:';
    procedure scrollLineUp (sender: id); message 'scrollLineUp:';
    procedure scrollLineDown (sender: id); message 'scrollLineDown:';
    procedure scrollToBeginningOfDocument (sender: id); message 'scrollToBeginningOfDocument:';
    procedure scrollToEndOfDocument (sender: id); message 'scrollToEndOfDocument:';
    procedure transpose (sender: id); message 'transpose:';
    procedure transposeWords (sender: id); message 'transposeWords:';
    procedure selectAll (sender: id); message 'selectAll:';
    procedure selectParagraph (sender: id); message 'selectParagraph:';
    procedure selectLine (sender: id); message 'selectLine:';
    procedure selectWord (sender: id); message 'selectWord:';
    procedure indent (sender: id); message 'indent:';
    procedure insertTab (sender: id); message 'insertTab:';
    procedure insertBacktab (sender: id); message 'insertBacktab:';
    procedure insertNewline (sender: id); message 'insertNewline:';
    procedure insertParagraphSeparator (sender: id); message 'insertParagraphSeparator:';
    procedure insertNewlineIgnoringFieldEditor (sender: id); message 'insertNewlineIgnoringFieldEditor:';
    procedure insertTabIgnoringFieldEditor (sender: id); message 'insertTabIgnoringFieldEditor:';
    procedure insertLineBreak (sender: id); message 'insertLineBreak:';
    procedure insertContainerBreak (sender: id); message 'insertContainerBreak:';
    procedure insertSingleQuoteIgnoringSubstitution (sender: id); message 'insertSingleQuoteIgnoringSubstitution:'; { available in 10_5 }
    procedure insertDoubleQuoteIgnoringSubstitution (sender: id); message 'insertDoubleQuoteIgnoringSubstitution:'; { available in 10_5 }
    procedure changeCaseOfLetter (sender: id); message 'changeCaseOfLetter:';
    procedure uppercaseWord (sender: id); message 'uppercaseWord:';
    procedure lowercaseWord (sender: id); message 'lowercaseWord:';
    procedure capitalizeWord (sender: id); message 'capitalizeWord:';
    procedure deleteForward (sender: id); message 'deleteForward:';
    procedure deleteBackward (sender: id); message 'deleteBackward:';
    procedure deleteBackwardByDecomposingPreviousCharacter (sender: id); message 'deleteBackwardByDecomposingPreviousCharacter:';
    procedure deleteWordForward (sender: id); message 'deleteWordForward:';
    procedure deleteWordBackward (sender: id); message 'deleteWordBackward:';
    procedure deleteToBeginningOfLine (sender: id); message 'deleteToBeginningOfLine:';
    procedure deleteToEndOfLine (sender: id); message 'deleteToEndOfLine:';
    procedure deleteToBeginningOfParagraph (sender: id); message 'deleteToBeginningOfParagraph:';
    procedure deleteToEndOfParagraph (sender: id); message 'deleteToEndOfParagraph:';
    procedure yank (sender: id); message 'yank:';
    procedure complete (sender: id); message 'complete:';
    procedure setMark (sender: id); message 'setMark:';
    procedure deleteToMark (sender: id); message 'deleteToMark:';
    procedure selectToMark (sender: id); message 'selectToMark:';
    procedure swapWithMark (sender: id); message 'swapWithMark:';
    procedure cancelOperation (sender: id); message 'cancelOperation:';
    procedure makeBaseWritingDirectionNatural (sender: id); message 'makeBaseWritingDirectionNatural:'; { available in 10_6 }
    procedure makeBaseWritingDirectionLeftToRight (sender: id); message 'makeBaseWritingDirectionLeftToRight:'; { available in 10_6 }
    procedure makeBaseWritingDirectionRightToLeft (sender: id); message 'makeBaseWritingDirectionRightToLeft:'; { available in 10_6 }
    procedure makeTextWritingDirectionNatural (sender: id); message 'makeTextWritingDirectionNatural:'; { available in 10_6 }
    procedure makeTextWritingDirectionLeftToRight (sender: id); message 'makeTextWritingDirectionLeftToRight:'; { available in 10_6 }
    procedure makeTextWritingDirectionRightToLeft (sender: id); message 'makeTextWritingDirectionRightToLeft:'; { available in 10_6 }
    procedure quickLookPreviewItems (sender: id); message 'quickLookPreviewItems:'; { available in 10_8 }
  end;

type
  NSUndoSupport = objccategory external (NSResponder)
    function undoManager: NSUndoManager; message 'undoManager';
  end;

type
  NSControlEditingSupport = objccategory external (NSResponder)
    function validateProposedFirstResponder_forEvent (responder: NSResponder; event: NSEvent): ObjCBOOL; message 'validateProposedFirstResponder:forEvent:'; { available in 10_7 }
  end;

type
  NSErrorPresentation = objccategory external (NSResponder)
    procedure presentError_modalForWindow_delegate_didPresentSelector_contextInfo (error: NSError; window: NSWindow; delegate: id; didPresentSelector: SEL; contextInfo: pointer); message 'presentError:modalForWindow:delegate:didPresentSelector:contextInfo:';
    function presentError (error: NSError): ObjCBOOL; message 'presentError:';
    function willPresentError (error: NSError): NSError; message 'willPresentError:';
  end;

type
  NSTextFinderSupport = objccategory external (NSResponder)
    procedure performTextFinderAction (sender: id); message 'performTextFinderAction:'; { available in 10_7 }
  end;

type
  NSResponder_NSDeprecated = objccategory external name 'NSDeprecated' (NSResponder)
    function performMnemonic (theString: NSString): ObjCBOOL; message 'performMnemonic:'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

