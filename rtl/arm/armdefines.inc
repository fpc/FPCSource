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

{$if not defined(cpuarmv3) and not defined(cpuarmv4)}
{$define CPUARM_HAS_BX} // cpu has BX instruction
{$endif}

{$if not defined(CPUARMV3) and not defined(CPUARMV4) and not defined(CPUARMV5)}
{$define CPUARM_HAS_REV} // cpu has REV instruction
{$endif}

{$if defined(CPUARMV7M) or defined(CPUARMCORTEXM3)}
{$define CPUARM_HAS_IDIV} // cpu has SDIV/UDIV instructions
{$endif}

{$if defined(CPUARMV6) or defined(CPUARMV7) or defined(CPUARMV7M) or defined(CPUARMCORTEXM3)}
{$define CPUARM_HAS_LDREX} // cpu has LDREX/STREX instructions
{$endif}

{$if defined(LINUX) and (defined(CPUARMEL) or defined(CPUARMHF))}
{$define SYSTEM_HAS_KUSER_CMPXCHG} // system has kuser_cmpxchg method
{$endif}
