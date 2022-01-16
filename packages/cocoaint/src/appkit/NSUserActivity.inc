{ Parsed from AppKit.framework NSUserActivity.h }

{$if defined(__OBJC2__)}
{$ifdef TYPES}
const
  NS_USER_ACTIVITY_SUPPORTED = 1;
{$endif}

{$else}
{$ifdef TYPES}
const
  NS_USER_ACTIVITY_SUPPORTED = 0;
{$endif}

{$endif}

{$if defined(NS_USER_ACTIVITY_SUPPORTED)}
{$ifdef CLASSES}

type
  NSResponder_NSUserActivity = objccategory external name 'NSUserActivity' (NSResponder)
    procedure setUserActivity(newValue: NSUserActivity); message 'setUserActivity:';
    function userActivity: NSUserActivity; message 'userActivity';
    procedure updateUserActivityState (userActivity_: NSUserActivity); message 'updateUserActivityState:'; { available in 10_10 }
    procedure restoreUserActivityState (userActivity_: NSUserActivity); message 'restoreUserActivityState:'; { available in 10_10 }
  end;

type
  NSDocument_NSUserActivity = objccategory external name 'NSUserActivity' (NSDocument)
    procedure setUserActivity(newValue: NSUserActivity); message 'setUserActivity:';
    function userActivity: NSUserActivity; message 'userActivity';
    procedure updateUserActivityState (activity: NSUserActivity); message 'updateUserActivityState:'; { available in 10_10 }
    procedure restoreUserActivityState (activity: NSUserActivity); message 'restoreUserActivityState:'; { available in 10_10 }
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSUserActivityDocumentURLKey: NSString { available in 10_10 }; cvar; external;
{$endif}

{$endif}
