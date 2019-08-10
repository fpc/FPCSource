{ Parsed from QuartzComposer.framework QCPlugInViewController.h }


{$ifdef TYPES}
type
  QCPlugInViewControllerPtr = ^QCPlugInViewController;
{$endif}

{$if (defined(MAC_OS_X_VERSION_10_5)) and ((MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5))}
{$ifdef CLASSES}

type
  QCPlugInViewController = objcclass external (NSViewController)
  private
    _private: pointer;
  public
    function initWithPlugIn_viewNibName (plugIn: QCPlugIn; name: NSString): id; message 'initWithPlugIn:viewNibName:';
    function plugIn: QCPlugIn; message 'plugIn';
  end;


type
  QCPlugIn_QCPlugInViewController = objccategory external name 'QCPlugInViewController' (QCPlugIn)
    function createViewController: QCPlugInViewController; message 'createViewController';
  end;
{$endif}

{$endif}
