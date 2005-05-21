{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Pierre Muller,
    member of the Free Pascal development team.

    Support unit for working with DXE files for Go32V2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

unit dxetype;

interface

const
   DXE_MAGIC  = $31455844;

type
  dxe_header = record
     magic,
     symbol_offset,
     element_size,
     nrelocs       : cardinal;
  end;

implementation

end.
