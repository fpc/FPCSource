{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Includes the i386 code generator

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
unit cpunode;

{$i defines.inc}

  interface

  implementation

    uses
       ncgbas,
       n386ld,n386add,n386cal,n386con,n386flw,n386mat,n386mem,
       n386set,n386inl,n386opt,
       { this not really a node }
       n386obj;

end.
{
  $Log$
  Revision 1.4  2001-05-18 22:31:06  peter
    * tasmnode.pass_2 is independent of cpu, moved to ncgbas
    * include ncgbas for independent nodes

  Revision 1.3  2001/04/21 13:37:17  peter
    * made tclassheader using class of to implement cpu dependent code

  Revision 1.2  2000/12/31 11:14:11  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.1  2000/10/15 09:39:37  peter
    * moved cpu*.pas to i386/
    * renamed n386 to common cpunode

  Revision 1.1  2000/10/14 10:14:47  peter
    * moehrendorf oct 2000 rewrite

}