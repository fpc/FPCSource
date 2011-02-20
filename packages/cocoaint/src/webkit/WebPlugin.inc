{ Parsed from Webkit.framework WebPlugin.h }

{$ifdef TYPES}
{$ifndef WEBPLUGIN_PAS_T}
{$define WEBPLUGIN_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBPLUGIN_PAS_R}
{$define WEBPLUGIN_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBPLUGIN_PAS_F}
{$define WEBPLUGIN_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBPLUGIN_PAS_S}
{$define WEBPLUGIN_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}

{$endif}

{$ifdef CLASSES}
{$ifndef WEBPLUGIN_PAS_C}
{$define WEBPLUGIN_PAS_C}

{ WebPlugInCategory }
  WebPlugInCategory = objccategory external (NSObject)
    procedure webPlugInInitialize; message 'webPlugInInitialize';
    procedure webPlugInStart; message 'webPlugInStart';
    procedure webPlugInStop; message 'webPlugInStop';
    procedure webPlugInDestroy; message 'webPlugInDestroy';
    procedure webPlugInSetIsSelected(isSelected: Boolean); message 'webPlugInSetIsSelected:';
    function objectForWebScript: id; message 'objectForWebScript';
    procedure webPlugInMainResourceDidReceiveResponse(response: NSURLResponse); message 'webPlugInMainResourceDidReceiveResponse:';
    procedure webPlugInMainResourceDidReceiveData(data: NSData); message 'webPlugInMainResourceDidReceiveData:';
    procedure webPlugInMainResourceDidFailWithError(error: NSError); message 'webPlugInMainResourceDidFailWithError:';
    procedure webPlugInMainResourceDidFinishLoading; message 'webPlugInMainResourceDidFinishLoading';
  end;

{$endif}
{$endif}
