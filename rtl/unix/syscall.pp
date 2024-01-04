{$IFNDEF FPC_DOTTEDUNITS}
unit syscall;
{$ENDIF FPC_DOTTEDUNITS}

interface
{$define FPC_USE_SYSCALL}

{$i sysnr.inc}
{$i syscallh.inc}

implementation
{$ifdef FPC_USE_LIBC}
uses
  unixtype;
{$endif}

{$ifdef FPC_HAS_SETSYSNR_INC}
{$define FPC_COMPILING_SYSCALL_UNIT}
{$I setsysnr.inc}
{$endif FPC_HAS_SETSYSNR_INC}

{$ifdef FPC_USE_LIBC}
procedure seterrno(err:cint); external name 'FPC_SYS_SETERRNO';

{$I syscall.inc}
{$endif}

{$ifdef FPC_HAS_SETSYSNR_INC}
begin
  SetSyscallNumbers;
{$endif FPC_HAS_SETSYSNR_INC}
end.
