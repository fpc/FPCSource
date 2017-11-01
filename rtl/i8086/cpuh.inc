{

    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by the Free Pascal development team.

    CPU specific system unit header file

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

 const
   { will be detected at startup }
  { 0=8086/8088/80186/80188/NEC V20/NEC V30, 1=80286, 2=80386 or newer }
  Test8086 : byte = 0; public name '__Test8086';
  { will be detected at startup }
  { 0=NO FPU, 1=8087, 2=80287, 3=80387 or newer }
  Test8087 : byte = 0;
  { will be detected at startup }
  has_sse_support : boolean = false;
  has_mmx_support : boolean = false;

