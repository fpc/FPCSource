{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit implements block, statement nodes etc.

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
unit nstatmnt;

  interface

    uses
       tree;

    type
       pblocknode = ^tblocknode;
       tblocknode = object(tunarynode)
          constructor init(l : pnode);
          procedure det_temp;virtual;
          procedure det_resulttype;virtual;
          procedure secondpass;virtual;
       end;

       pstatementnode = ^tstatementnode;
       tstatementnode = object(tbinarynode)
          constructor init(l,r : pnode);
       end;

  implementation

    uses
       tgobj,globtype,globals,symtable,verbose,cgbase,tgcpu;

{****************************************************************************
                                 TSTAMENTNODE
 ****************************************************************************}

    constructor tstatementnode.init(l,r : pnode);

      begin
         inherited init(l,r);
         treetype:=statementn;
      end;

{****************************************************************************
                                 TBLOCKNODE
 ****************************************************************************}

    constructor tblocknode.init(l : pnode);

      begin
         inherited init(l);
         treetype:=blockn;
      end;

    procedure tblocknode.det_resulttype;

      var
         hp : pstatementnode;

      begin
         hp:=pstatementnode(left);
         while assigned(hp) do
           begin
              if assigned(pstatementnode(hp)^.right) then
                begin
                   tg.cleartempgen;
                   hp^.right^.det_resulttype;
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp^.right^.resulttype) and
                      (hp^.right^.resulttype<>pdef(voiddef)) then
                     CGMessage(cg_e_illegal_expression);
                   if codegenerror then
                     exit;
                end;
              hp:=pstatementnode(hp^.left);
           end;
      end;

    procedure tblocknode.det_temp;

      var
         hp : pstatementnode;

      begin
         hp:=pstatementnode(left);
         while assigned(hp) do
           begin
              if assigned(hp^.right) then
                begin
                   tg.cleartempgen;
                   hp^.right^.det_temp;
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp^.right^.resulttype) and
                      (hp^.right^.resulttype<>pdef(voiddef)) then
                     CGMessage(cg_e_illegal_expression);
                   if codegenerror then
                     exit;
                   hp^.registersint:=hp^.right^.registersint;
                   hp^.registersfpu:=hp^.right^.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp^.registersmmx:=hp^.right^.registersmmx;
                   hp^.registerskni:=hp^.right^.registerskni;
{$endif SUPPORT_MMX}
                end
              else
                hp^.registersint:=0;

              if hp^.registersint>registersint then
                registersint:=hp^.registersint;
              if hp^.registersfpu>registersfpu then
                registersfpu:=hp^.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp^.registersmmx>registersmmx then
                registersmmx:=hp^.registersmmx;
              if hp^.registerskni>registerskni then
                registerskni:=hp^.registerskni;
{$endif}
              hp:=pstatementnode(hp^.left);
           end;
      end;

    procedure tblocknode.secondpass;

      begin
         if assigned(left) then
           left^.secondpass;
      end;


end.
{
  $Log$
  Revision 1.1  2000-07-13 06:30:08  michael
  + Initial import

  Revision 1.5  2000/01/07 01:14:54  peter
    * updated copyright to 2000

  Revision 1.4  1999/08/05 14:58:14  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.3  1999/08/02 17:14:09  florian
    + changed the temp. generator to an object

  Revision 1.2  1999/08/01 23:36:43  florian
    * some changes to compile the new code generator

  Revision 1.1  1999/01/23 23:35:02  florian
    + first versions

}