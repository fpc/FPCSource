{ Parsed from Automator.framework AMWorkflowView.h }


{$ifdef TYPES}
type
  AMWorkflowViewPtr = ^AMWorkflowView;
{$endif}

{$ifdef CLASSES}

type
  AMWorkflowView = objcclass external (NSView)
  private
    _workflowController: AMWorkflowController;
    _actionViewControllers: NSMutableArray;
    _draggedActionViews: NSArray;
    _messageString: NSString;
    _unused: id;
    _variablesEditorController: id;
    _showWhenRunController: id;
    __AMWorkflowViewFlags: bitpacked record
      case byte of
        0: (_anonBitField___AMWorkflowViewFlags0: cuint);
        1: (
          ignoreSubviewFrameChanges: 0..1;
          editingEnabled: 0..1;
          reserved: 0..((1 shl 30)-1);
        );
      end;
    _draggingIndex: NSUInteger;
    _selectionRect: NSRect;
    _future:array[0..3] of id;
  public
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setWorkflowController(newValue: AMWorkflowController); message 'setWorkflowController:';
    function workflowController: AMWorkflowController; message 'workflowController';
  end;
{$endif}

