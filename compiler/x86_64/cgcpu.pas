{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit implements the code generator for the x86-64.

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
{ This unit implements the code generator for the x86-64.
}
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       cginfo,cgbase,cgobj,cg64f64,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,cpupara,
       node,symconst;

    type
      tcgx86_64 = class(tcgx86)
      end;

  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symdef,symsym,defbase,paramgr,
       rgobj,tgobj,rgcpu;

begin
  cg:=tcgx86_64.create;
  cg64:=tcg64f64.create;
end.
{
  $Log$
  Revision 1.1  2002-07-24 22:38:15  florian
    + initial release of x86-64 target code

}
