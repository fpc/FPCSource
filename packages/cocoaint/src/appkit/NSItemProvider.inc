{ Parsed from AppKit.framework NSItemProvider.h }

{$if defined(__OBJC2__)}
{$ifdef CLASSES}

type
  NSItemSourceInfo = objccategory external (NSItemProvider)
    function sourceFrame: NSRect; message 'sourceFrame';
    function containerFrame: NSRect; message 'containerFrame';
    function preferredPresentationSize: NSSize; message 'preferredPresentationSize';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSTypeIdentifierDateText: NSString { available in 10_10, 8_0 }; cvar; external;
  NSTypeIdentifierAddressText: NSString { available in 10_10, 8_0 }; cvar; external;
  NSTypeIdentifierPhoneNumberText: NSString { available in 10_10, 8_0 }; cvar; external;
  NSTypeIdentifierTransitInformationText: NSString { available in 10_10, 8_0 }; cvar; external;
{$endif}

{$endif}
