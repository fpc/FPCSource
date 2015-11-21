{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Jonas Maebe, member of the
    Free Pascal development team

    Processor dependent part of strings.pp, that can be shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$if defined(FPC_MM_TINY)}
  {$define FPC_X86_CODE_NEAR}
  {$define FPC_X86_DATA_NEAR}
{$elseif defined(FPC_MM_SMALL)}
  {$define FPC_X86_CODE_NEAR}
  {$define FPC_X86_DATA_NEAR}
{$elseif defined(FPC_MM_MEDIUM)}
  {$define FPC_X86_CODE_FAR}
  {$define FPC_X86_DATA_NEAR}
{$elseif defined(FPC_MM_COMPACT)}
  {$define FPC_X86_CODE_NEAR}
  {$define FPC_X86_DATA_FAR}
{$elseif defined(FPC_MM_LARGE)}
  {$define FPC_X86_CODE_FAR}
  {$define FPC_X86_DATA_FAR}
{$elseif defined(FPC_MM_HUGE)}
  {$define FPC_X86_CODE_FAR}
  {$define FPC_X86_DATA_HUGE}
{$else}
  {$fatal No memory model defined}
{$endif}

