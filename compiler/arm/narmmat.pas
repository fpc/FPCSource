{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate ARM assembler for math nodes

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
unit narmmat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat;

    type
      tarmmoddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      tarmshlshrnode = class(tshlshrnode)
         procedure pass_2;override;
      end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,
      aasmbase,aasmcpu,aasmtai,
      defutil,
      cgbase,cgobj,pass_1,pass_2,
      ncon,
      cpubase,cpuinfo,cginfo,
      ncgutil,cgcpu,cg64f32,rgobj;

{*****************************************************************************
                             TARMMODDIVNODE
*****************************************************************************}

    procedure tarmmoddivnode.pass_2;
      begin
      end;

{*****************************************************************************
                             TARMSHLRSHRNODE
*****************************************************************************}

    procedure tarmshlshrnode.pass_2;
      begin
      end;

begin
   cmoddivnode:=tarmmoddivnode;
   cshlshrnode:=tarmshlshrnode;
end.
{
  $Log$
  Revision 1.1  2003-08-21 23:23:59  florian
    * continued to work on the arm skeleton
}

