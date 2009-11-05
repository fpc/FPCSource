
unit objc;

{$ifdef darwin}
{$define targethandled}
{$if defined(cpuarm) or defined(cpux86_64) or defined(cpupowerpc64)}
{$i objcnf.inc}
{$endif}

{$if defined(cpui386) or defined(cpupowerpc32)}
{$define targethandled}
{$i objc1.inc}
{$endif}
{$endif}


{$ifndef targethandled}
  {$error Target not yet supported for objc.pp unit}
{$endif}