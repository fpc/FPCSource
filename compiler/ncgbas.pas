{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    This unit implements some basic nodes

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
unit ncgbas;

  interface

    uses
       node;

    type
       tcgnothingnode = class(tnoethingnode)
          procedure pass_2;override;
       end;

       tcgerrornode = class(terrornode)
          procedure pass_2;override;
       end;

       tcgasmnode = class(tasmnode)
          procedure pass_2;override;
       end;

       tcgstatementnode = class(tstatementnode)
          procedure pass_2;override;
       end;

       tcgblocknode = class(tblocknode)
          procedure pass_2;override;
       end;

  implementation

    uses
      globtype,systems,
      cutils,cobjects,verbose,globals,
      aasm,symtable,types,
      htypechk,
      cpubase,cpuasm,
      nflw
{$ifdef newcg}
      ,cgbase
      ,tgcpu
{$else newcg}
      ,hcodegen
  {$ifdef i386}
      ,tgeni386
  {$endif}
  {$ifdef m68k}
      ,tgen68k
  {$endif}
{$endif}
      ;
{*****************************************************************************
                             TFIRSTNOTHING
*****************************************************************************}

    procedure tnothingnode.pass_2;

      begin
      end;

{*****************************************************************************
                             TFIRSTERROR
*****************************************************************************}

{*****************************************************************************
                            TSTATEMENTNODE
*****************************************************************************}

{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    procedure tblocknode.pass_2;
      begin
      { do second pass on left node }
        if assigned(p^.left) then
         secondpass(p^.left);
      end;


begin
   cnothingnode:=tcgnothingnode;
   cerrornode:=tcgerrornode;
   casmnode:=tcgasmnode;
   cstatementnode:=tcgstatementnode;
   cblocknode:=tcgblocknode;
end.
{
  $Log$
  Revision 1.1  2000-10-14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

}