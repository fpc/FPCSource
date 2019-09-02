{ Parsed from NotificationCenter.framework NCWidgetProviding.h }


{$ifdef TYPES}
type
  NCWidgetProvidingProtocolPtr = ^NCWidgetProvidingProtocol;
{$endif}

{$ifdef TYPES}
type
  NCUpdateResult = NSUInteger;
  NCUpdateResultPtr = ^NCUpdateResult;

const
  NCUpdateResultNewData = 0;
  NCUpdateResultNoData = 1;
  NCUpdateResultFailed = 2;
{$endif}

{$ifdef PROTOCOLS}

type
  NCWidgetProvidingProtocol = objcprotocol external name 'NCWidgetProviding' (NSExtensionRequestHandlingProtocol)
  optional
    procedure widgetPerformUpdateWithCompletionHandler (completionHandler: OpaqueCBlock); message 'widgetPerformUpdateWithCompletionHandler:';
    function widgetMarginInsetsForProposedMarginInsets (defaultMarginInset: NSEdgeInsets): NSEdgeInsets; message 'widgetMarginInsetsForProposedMarginInsets:';
    function widgetAllowsEditing: ObjCBOOL; message 'widgetAllowsEditing';
    procedure widgetDidBeginEditing; message 'widgetDidBeginEditing';
    procedure widgetDidEndEditing; message 'widgetDidEndEditing';
  end;
{$endif}

{$ifdef CLASSES}

type
  NCWidgetProvidingPresentationStyles = objccategory external (NSViewController)
    procedure presentViewControllerInWidget (viewController: NSViewController); message 'presentViewControllerInWidget:'; { available in 10_10 }
  end;
{$endif}

