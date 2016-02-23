{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by the Free Pascal development team.

    Processor dependent defines for the ARM runtime library.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$if defined(LINUX) or defined(ANDROID)}
  {$if defined(CPUARMEL) or defined(CPUARMHF)}
    {$define SYSTEM_HAS_KUSER_CMPXCHG} // system has kuser_cmpxchg method
  {$endif}
{$endif}

{$if defined(ANDROID)}
  {$if defined(CPUARMEL) or defined(CPUARMHF)}
    {$define SYSTEM_HAS_KUSER_MEMORY_BARRIER} // system has kuser_memory_barrier method
  {$endif}
{$endif}
