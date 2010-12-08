{ Parsed from Webkit.framework WebPluginContainer.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


{$ifdef TYPES}
{$ifndef WEBPLUGINCONTAINER_PAS_T}
{$define WEBPLUGINCONTAINER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBPLUGINCONTAINER_PAS_R}
{$define WEBPLUGINCONTAINER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBPLUGINCONTAINER_PAS_F}
{$define WEBPLUGINCONTAINER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBPLUGINCONTAINER_PAS_S}
{$define WEBPLUGINCONTAINER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef WEBPLUGINCONTAINER_PAS_C}
{$define WEBPLUGINCONTAINER_PAS_C}

{ WebPlugInContainerCategory }
  WebPlugInContainerCategory = objccategory(NSObject)
    procedure webPlugInContainerLoadRequest_inFrame(request: NSURLRequest; target: NSString); message 'webPlugInContainerLoadRequest:inFrame:';
    procedure webPlugInContainerShowStatus(message: NSString); message 'webPlugInContainerShowStatus:';
    function webPlugInContainerSelectionColor: NSColor; message 'webPlugInContainerSelectionColor';
    function webFrame: WebFrame; message 'webFrame';
  end; external;

{$endif}
{$endif}
