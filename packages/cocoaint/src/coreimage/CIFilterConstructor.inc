{ Parsed from CoreImage.framework CIFilterConstructor.h }


{$ifdef TYPES}
type
  CIFilterConstructorProtocolPtr = ^CIFilterConstructorProtocol;
{$endif}

{$ifdef PROTOCOLS}

type
  CIFilterConstructorProtocol = objcprotocol external name 'CIFilterConstructor'
    function filterWithName (name: NSString): CIFilter; message 'filterWithName:';
  end;
{$endif}

