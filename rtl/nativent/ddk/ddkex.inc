{%MainUnit ddk.pas}
{
    Driver Development Kit for Native NT
    Imports for Executive

    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

function ExAllocatePoolWithTag(PoolType: TPoolType; NumberOfBytes: PtrUInt; Tag: LongWord): Pointer; stdcall; external ntkrnl name 'ExAllocatePoolWithTag';
procedure ExFreePoolWithTag(P: Pointer; Tag: LongWord); stdcall; external ntkrnl name 'ExFreePoolWithTag';

