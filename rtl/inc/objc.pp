
unit objc;

{$ifdef darwin}
{$define targethandled}
{$if defined(iphonesim) or defined(cpuarm) or defined(cpux86_64) or defined(cpupowerpc64)}
{$i objcnf.inc}
{$endif}

{$if defined(cpupowerpc32) or (defined(cpui386) and not defined(iphonesim))}
{$define targethandled}
{$i objc1.inc}
{$endif}
{$endif}


{$ifndef targethandled}
  {$error Target not yet supported for objc.pp unit}
{$endif}
