{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This unit contains basic user mode types.
    Copyright (c) 2010 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  //
  // Basic Types that aren't defined in User-Mode Headers
  //
  CINT = LongInt; // Int64 in 64 bit?
  PCSZ = PChar;
  CLONG = ULONG;
  CSHORT = SmallInt;
  PCSHORT = ^CSHORT;
  PHYSICAL_ADDRESS = LARGE_INTEGER;
  PPHYSICAL_ADDRESS = ^PHYSICAL_ADDRESS;
  KPRIORITY = LONG;

