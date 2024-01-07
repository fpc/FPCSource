{$IFNDEF FPC_DOTTEDUNITS}
unit syscall;
{$ENDIF FPC_DOTTEDUNITS}

{$ifdef LINUX}
  {$ifdef FPC_USE_LIBC}
    {$mode objfpc}
  {$endif}
{$endif}

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
{$if not declared(seterrno)}
procedure seterrno(err:cint); external name 'FPC_SYS_SETERRNO';
{$endif}
{$I syscall.inc}
{$endif}

{$ifdef FPC_HAS_SETSYSNR_INC}
begin
  SetSyscallNumbers;
{$else}
{$if defined(linux) and defined(i386) and defined(FPC_USE_LIBC)}
begin
  InitSyscallIntf;
{$endif}
{$endif FPC_HAS_SETSYSNR_INC}
end.
