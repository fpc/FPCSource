{ Parsed from Quartzcore.framework CIKernel.h }

{$ifdef TYPES}
{$ifndef CIKERNEL_PAS_T}
{$define CIKERNEL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIKERNEL_PAS_R}
{$define CIKERNEL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIKERNEL_PAS_F}
{$define CIKERNEL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIKERNEL_PAS_S}
{$define CIKERNEL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CIKernel = objcclass;
  CIKernelPointer = ^CIKernel;
  CIKernelPtr = CIKernelPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CIKERNEL_PAS_C}
{$define CIKERNEL_PAS_C}

{ CIKernel }
  CIKernel = objcclass external (NSObject)
  private
    _priv: Pointer;
    
  public
    class function kernelsWithString(s: NSString): NSArray; message 'kernelsWithString:';
    function name: NSString; message 'name';
    procedure setROISelector(aMethod: SEL); message 'setROISelector:';
  end;

{$endif}
{$endif}
