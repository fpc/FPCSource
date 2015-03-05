{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Maciej Izak

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure InitializeArray(p, typeInfo: Pointer; count: SizeInt);
procedure FinalizeArray(p, typeInfo: Pointer; count: SizeInt);
procedure CopyArray(dest, source, typeInfo: Pointer; count: SizeInt);


