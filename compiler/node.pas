{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Basic node handling

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
unit node;

  interface

    uses
       globtype,globals,cobjects,aasm,cpubase,symtable,
       tokens;

    {$I nodeh.inc}

  implementation

    uses
       htypechk,ncal,hcodegen,verbose;

    {$I node.inc}

end.
{
  $Log$
  Revision 1.2  2000-09-20 21:52:38  florian
    * removed a lot of errors

  Revision 1.1  2000/08/26 12:27:35  florian
    * initial release
}