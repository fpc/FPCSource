{ Parsed from NotificationCenter.framework NCWidgetController.h }


{$ifdef TYPES}
type
  NCWidgetControllerPtr = ^NCWidgetController;
{$endif}

{$ifdef CLASSES}

type
  NCWidgetController = objcclass external (NSObject)
  public
    class function widgetController: instancetype; message 'widgetController';
    class function defaultWidgetController: NCWidgetController; message 'defaultWidgetController'; deprecated 'in "Use widgetController instead."';
    procedure setHasContent_forWidgetWithBundleIdentifier (flag: ObjCBOOL; bundleID: NSString); message 'setHasContent:forWidgetWithBundleIdentifier:';
  end;
{$endif}

