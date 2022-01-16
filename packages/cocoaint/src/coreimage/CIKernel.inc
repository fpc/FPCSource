{ Parsed from CoreImage.framework CIKernel.h }


{$ifdef TYPES}
type
  CIKernelPtr = ^CIKernel;
{$endif}

{$ifdef CLASSES}

type
  CIKernel = objcclass external (NSObject)
  private
    _priv: pointer;
  public
    class function kernelsWithString (s: NSString): NSArray; message 'kernelsWithString:';
    class function kernelWithString (s: NSString): CIKernel; message 'kernelWithString:'; { available starting in __MAC_10_10, __IPHONE_5_0 }
    function name: NSString; message 'name';
    procedure setROISelector (aMethod: SEL); message 'setROISelector:';
  end;
{$endif}

