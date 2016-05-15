{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Implement OS-dependent part of dynamic library loading.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type
  { using PtrInt here is compliant with the other platforms }
  TLibHandle = PtrInt;

Const
  NilHandle = TLibHandle(0);
// these are for easier crossplatform construction of dll names in dynloading libs.
{$if defined(Darwin)}
  SharedSuffix = 'dylib';
{$elseif defined(aix)}
  SharedSuffix = 'a';
{$else}
  SharedSuffix = 'so';
{$endif}

