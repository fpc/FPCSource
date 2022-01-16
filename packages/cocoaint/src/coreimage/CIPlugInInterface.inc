{ Parsed from CoreImage.framework CIPlugInInterface.h }


{$ifdef TYPES}
type
  CIPlugInRegistrationProtocolPtr = ^CIPlugInRegistrationProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  CIPlugInRegistrationProtocol = objcprotocol external name 'CIPlugInRegistration'
    function load (host: pointer): ObjCBOOL; message 'load:';
  end;
{$endif}

