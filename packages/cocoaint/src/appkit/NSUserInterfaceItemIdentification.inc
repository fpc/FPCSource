{ Parsed from AppKit.framework NSUserInterfaceItemIdentification.h }


{$ifdef TYPES}
type
  NSUserInterfaceItemIdentificationProtocolPtr = ^NSUserInterfaceItemIdentificationProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSUserInterfaceItemIdentificationProtocol = objcprotocol external name 'NSUserInterfaceItemIdentification'
  required
    procedure setIdentifier(newValue: NSString); message 'setIdentifier:';
    function identifier: NSString; message 'identifier';
  end;
{$endif}

