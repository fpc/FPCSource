{ Parsed from Webkit.framework WebPluginViewFactory.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:27 CET 2010 }


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
  WebPlugInViewFactoryProtocol = objcprotocol
    class function plugInViewWithArguments(arguments: NSDictionary): NSViewPtr; message 'plugInViewWithArguments:';
  end; external name 'WebPlugInViewFactory';
{$endif}
{$endif}
