{ Parsed from WebKit.framework WebPluginViewFactory.h }


{$ifdef TYPES}
type
  WebPlugInViewFactoryProtocolPtr = ^WebPlugInViewFactoryProtocol;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  WebPlugInBaseURLKey: NSString; cvar; external;
  WebPlugInAttributesKey: NSString; cvar; external;
  WebPlugInContainerKey: NSString; cvar; external;
  WebPlugInContainingElementKey: NSString; cvar; external;
  WebPlugInShouldLoadMainResourceKey: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef PROTOCOLS}

type
  WebPlugInViewFactoryProtocol = objcprotocol external name 'WebPlugInViewFactory' (NSObjectProtocol)
    class function plugInViewWithArguments (arguments: NSDictionary): NSView; message 'plugInViewWithArguments:';
  end;
{$endif}

