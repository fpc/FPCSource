{
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Does the parsing for the i8086 GNU AS styled inline assembler.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
Unit ra8086att;

{$i fpcdefs.inc}

  interface

    uses
      rax86att;

    type
      ti8086attreader = class(tx86attreader)
      end;


  implementation

    uses
      rabase,systems;

const
  asmmode_i8086_att_info : tasmmodeinfo =
          (
            id    : asmmode_i8086_att;
            idtxt : 'ATT';
            casmreader : ti8086attreader;
          );

initialization
  RegisterAsmMode(asmmode_i8086_att_info);
end.
