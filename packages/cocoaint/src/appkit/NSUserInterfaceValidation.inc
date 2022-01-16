{ Parsed from AppKit.framework NSUserInterfaceValidation.h }


{$ifdef TYPES}
type
  NSValidatedUserInterfaceItemProtocolPtr = ^NSValidatedUserInterfaceItemProtocol;
  NSUserInterfaceValidationsProtocolPtr = ^NSUserInterfaceValidationsProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  NSValidatedUserInterfaceItemProtocol = objcprotocol external name 'NSValidatedUserInterfaceItem'
    function action: SEL; message 'action';
    function tag: NSInteger; message 'tag';
  end;

type
  NSUserInterfaceValidationsProtocol = objcprotocol external name 'NSUserInterfaceValidations'
    function validateUserInterfaceItem (anItem: NSValidatedUserInterfaceItemProtocol): ObjCBOOL; message 'validateUserInterfaceItem:';
  end;
{$endif}

