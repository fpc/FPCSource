{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Basic Processor information

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
Unit CPUInfo;

Interface

Type
{$ifdef FPC}
   AWord = dword;
{$else FPC}
   AWord = Longint;
{$endif FPC}
   TConstExprInt = longint;
   TConstExprUInt = dword;

Const
   { Size of native extended type }
   extended_size = 10;

Implementation

end.
{
  $Log$
  Revision 1.3  2000-08-12 06:45:08  florian
    + type TConstExprInt added

  Revision 1.2  2000/07/13 11:32:39  michael
  + removed logs

}