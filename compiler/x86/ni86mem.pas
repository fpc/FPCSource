{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386/i8086 assembler for memory related nodes

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
unit ni86mem;

{$i fpcdefs.inc}

interface
    uses
      globtype,
      cgbase,cpuinfo,cpubase,
      node,nmem,ncgmem,nx86mem;

    type
      ti86addrnode = class(tcgaddrnode)
       protected
        procedure set_absvarsym_resultdef; virtual; abstract;
        function typecheck_non_proc(realsource: tnode; out res: tnode): boolean; override;
      end;

implementation

    uses
      cutils,verbose,
      aasmtai,aasmdata,
      cgutils,cgobj,
      nld,
      symconst,symdef,symcpu;

{*****************************************************************************
                           TI86ADDRNODE
*****************************************************************************}

  function ti86addrnode.typecheck_non_proc(realsource: tnode; out res: tnode): boolean;
    begin
      res:=nil;
      { if we are getting the address of an absolute sym, check whether it's
        a near or a far pointer }
      if (realsource.nodetype=loadn) and
         ((tloadnode(realsource).symtableentry.typ=absolutevarsym) and
         tcpuabsolutevarsym(tloadnode(realsource).symtableentry).absseg) then
        begin
          set_absvarsym_resultdef;
          result:=true;
        end
      else
        result:=inherited;
    end;


end.
