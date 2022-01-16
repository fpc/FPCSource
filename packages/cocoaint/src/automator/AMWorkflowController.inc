{ Parsed from Automator.framework AMWorkflowController.h }


{$ifdef TYPES}
type
  AMWorkflowControllerPtr = ^AMWorkflowController;
{$endif}

{$ifdef CLASSES}

type
  AMWorkflowController = objcclass external (NSController)
  private
    workflowView: AMWorkflowView;
    _workflow: AMWorkflow;
    _runner: id;
    _runnerInterface: id;
    _eventMonitor: id;
    _delegate: id;
    _flags: bitpacked record
      case byte of
        0: (_anonBitField__flags0: cuint);
        1: (
          shouldRunLocally: 0..1;
          isRunningLocally: 0..1;
          shouldDisplayProgressInMenuBar: 0..1;
          reserved: 0..((1 shl 29)-1);
        );
      end;
    _delegateRespondTo: bitpacked record
      case byte of
        0: (_anonBitField__delegateRespondTo0: cuint);
        1: (
          workflowControllerDidAddWorkflow: 0..1;
          workflowControllerDidRemoveWorkflow: 0..1;
          workflowControllerWillRun: 0..1;
          workflowControllerWillStep: 0..1;
          workflowControllerWillStop: 0..1;
          workflowControllerWillPause: 0..1;
          workflowControllerDidRun: 0..1;
          workflowControllerDidStep: 0..1;
          workflowControllerDidStop: 0..1;
          workflowControllerDidPause: 0..1;
          workflowControllerWillRunAction: 0..1;
          workflowControllerDidRunAction: 0..1;
          workflowControllerDidError: 0..1;
          workflowControllerDidLogMessageOfTypeFromAction: 0..1;
          workflowControllerWillRunConversion: 0..1;
          workflowControllerDidRunConversion: 0..1;
          workflowControllerDidResumeWithAction: 0..1;
          reserved: 0..((1 shl 19)-1);
        );
      end;
    _future: id;
    _future2: id;
    _future3: id;
    _future4: id;
  public
    procedure setWorkflow(newValue: AMWorkflow); message 'setWorkflow:';
    function workflow: AMWorkflow; message 'workflow';
    procedure setWorkflowView(newValue: AMWorkflowView); message 'setWorkflowView:';
    function workflowView_: AMWorkflowView; message 'workflowView';
    procedure setDelegate(newValue: id); message 'setDelegate:';
    function delegate: id; message 'delegate';
    function canRun: ObjCBOOL; message 'canRun';
    function isRunning: ObjCBOOL; message 'isRunning';
    procedure run (sender: id); message 'run:';
    procedure stop (sender: id); message 'stop:';
    function isPaused: ObjCBOOL; message 'isPaused';
    procedure pause (sender: id); message 'pause:';
    procedure step (sender: id); message 'step:';
    procedure reset (sender: id); message 'reset:';
  end;


type
  AMWorkflowControllerDelegate = objccategory external (NSObject)
    procedure workflowControllerWillRun (controller: AMWorkflowController); message 'workflowControllerWillRun:';
    procedure workflowControllerWillStop (controller: AMWorkflowController); message 'workflowControllerWillStop:';
    procedure workflowControllerDidRun (controller: AMWorkflowController); message 'workflowControllerDidRun:';
    procedure workflowControllerDidStop (controller: AMWorkflowController); message 'workflowControllerDidStop:';
    procedure workflowController_willRunAction (controller: AMWorkflowController; action: AMAction); message 'workflowController:willRunAction:';
    procedure workflowController_didRunAction (controller: AMWorkflowController; action: AMAction); message 'workflowController:didRunAction:';
    procedure workflowController_didError (controller: AMWorkflowController; error: NSError); message 'workflowController:didError:';
  end;
{$endif}

