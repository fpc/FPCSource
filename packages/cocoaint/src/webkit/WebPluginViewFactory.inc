{ Parsed from Webkit.framework WebPluginViewFactory.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Sat Oct 3 15:33:51 ICT 2009 }


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
    class function plugInViewWithArguments(arguments: NSDictionaryPointer): NSView; message 'plugInViewWithArguments:';
  end; external name 'WebPlugInViewFactory';
{$endif}
{$endif}
