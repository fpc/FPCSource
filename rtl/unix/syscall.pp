unit syscall;

interface
{$define FPC_USE_SYSCALL}

{$i sysnr.inc}
{$i syscallh.inc}

implementation
{$ifdef FPC_HAS_SETSYSNR_INC}
{$define FPC_COMPILING_SYSCALL_UNIT}
{$I setsysnr.inc}
{$endif FPC_HAS_SETSYSNR_INC}

{$ifdef FPC_HAS_SETSYSNR_INC}
begin
  SetSyscallNumbers;
{$endif FPC_HAS_SETSYSNR_INC}
end.
