{$ifdef freertos}
  {$define cpusmall}
{$endif freertos}
{$ifdef embedded}
  {$define cpusmall}
{$endif embedded}
{$ifdef cpu68k}
  {$define cpusmall}
{$endif}
{$ifdef cpui8086}
  {$define cpusmall}
  {$if defined(FPC_MM_LARGE) or defined(FPC_MM_HUGE)}
    {$hugecode on}
  {$endif}
{$endif}
