{ Parsed from Webkit.framework WebPluginViewFactory.h }

{$ifdef TYPES}
{$ifndef WEBPLUGINVIEWFACTORY_PAS_T}
{$define WEBPLUGINVIEWFACTORY_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef WEBPLUGINVIEWFACTORY_PAS_R}
{$define WEBPLUGINVIEWFACTORY_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef WEBPLUGINVIEWFACTORY_PAS_F}
{$define WEBPLUGINVIEWFACTORY_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef WEBPLUGINVIEWFACTORY_PAS_S}
{$define WEBPLUGINVIEWFACTORY_PAS_S}

{ External string constants }
var
  WebPlugInBaseURLKey: NSString; cvar; external;
  WebPlugInAttributesKey: NSString; cvar; external;
  WebPlugInContainerKey: NSString; cvar; external;
  WebPlugInContainingElementKey: NSString; cvar; external;
  WebPlugInShouldLoadMainResourceKey: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  WebPlugInViewFactoryProtocol = objcprotocol;

{$endif}
{$ifdef PROTOCOLS}
{$ifndef WEBPLUGINVIEWFACTORY_PAS_P}
{$define WEBPLUGINVIEWFACTORY_PAS_P}
  
{ WebPlugInViewFactory Protocol }
  WebPlugInViewFactoryProtocol = objcprotocol external name 'WebPlugInViewFactory'
    class function plugInViewWithArguments(arguments: NSDictionary): NSViewPtr; message 'plugInViewWithArguments:';
  end;
{$endif}
{$endif}
