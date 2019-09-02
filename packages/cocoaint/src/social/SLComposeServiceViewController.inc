{ Parsed from Social.framework SLComposeServiceViewController.h }


{ Types from SLComposeServiceViewController }
{$ifdef TYPES}
type
  SLComposeServiceViewControllerPtr = ^SLComposeServiceViewController;
{$endif}

{$ifdef CLASSES}

// NOTE: NSTextViewDelegate was omitted from SLComposeServiceViewController to prevent a dependancy

type
  SLComposeServiceViewController = objcclass external (NSViewController)
  public
    procedure presentationAnimationDidFinish; message 'presentationAnimationDidFinish';
    {$if defined(TARGET_OS_IPHONE)}
    function textView: UITextViewPtr; message 'textView';
    {$else}
    function textView: NSTextView; message 'textView';
    {$endif}
    function contentText: NSString; message 'contentText';
    procedure setPlaceholder(newValue: NSString); message 'setPlaceholder:';
    function placeholder: NSString; message 'placeholder';
    procedure didSelectPost; message 'didSelectPost';
    procedure didSelectCancel; message 'didSelectCancel';
    procedure cancel; message 'cancel';
    function isContentValid: ObjCBOOL; message 'isContentValid';
    procedure validateContent; message 'validateContent';
    procedure setCharactersRemaining(newValue: NSNumber); message 'setCharactersRemaining:';
    function charactersRemaining: NSNumber; message 'charactersRemaining';
    {$if defined(TARGET_OS_IPHONE)}
    function configurationItems: NSArray; message 'configurationItems';
    procedure reloadConfigurationItems; message 'reloadConfigurationItems';
    procedure pushConfigurationViewController (viewController: UIViewController); message 'pushConfigurationViewController:';
    procedure popConfigurationViewController; message 'popConfigurationViewController';
    {$endif}
    {$if defined(TARGET_OS_IPHONE)}
    function loadPreviewView: UIViewPtr; message 'loadPreviewView';
    {$endif}
    {$if defined(TARGET_OS_IPHONE)}
    procedure setAutoCompletionViewController(newValue: UIViewController); message 'setAutoCompletionViewController:';
    function autoCompletionViewController: UIViewController; message 'autoCompletionViewController';
    {$endif}
  end;
{$endif}

