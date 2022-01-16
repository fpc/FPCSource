{ Parsed from GameKit.framework GKDialogController.h }


{$ifdef TYPES}
type
  GKDialogControllerPtr = ^GKDialogController;
  GKViewControllerProtocolPtr = ^GKViewControllerProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  GKViewControllerProtocol = objcprotocol external name 'GKViewController'
  end;
{$endif}

{$ifdef CLASSES}

type
  GKDialogController = objcclass external (NSResponder)
  public
    procedure setParentWindow(newValue: NSWindow); message 'setParentWindow:';
    function parentWindow: NSWindow; message 'parentWindow';
    function presentViewController (viewController: GKViewControllerProtocol): ObjCBOOL; message 'presentViewController:';
    procedure dismiss (sender: id); message 'dismiss:';
  end;


type
  SharedDialogController = objccategory external (GKDialogController)
    class function sharedDialogController: GKDialogController; message 'sharedDialogController';
  end;
{$endif}

