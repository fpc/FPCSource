{ Parsed from WebKit.framework WebPluginContainer.h }

{$ifdef CLASSES}

type
  WebPlugInContainer = objccategory external (NSObject)
    procedure webPlugInContainerLoadRequest_inFrame (request: NSURLRequest; target: NSString); message 'webPlugInContainerLoadRequest:inFrame:';
    procedure webPlugInContainerShowStatus (message_: NSString); message 'webPlugInContainerShowStatus:';
    function webPlugInContainerSelectionColor: NSColor; message 'webPlugInContainerSelectionColor';
    function webFrame: WebFrame; message 'webFrame';
  end;
{$endif}

