{*****************************************************************************}
{ File                   : ncpucall.pas                                       }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\26\26                                         }
{ Last modification date : 2002\07\01                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate SPARC assembler for in call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************}
UNIT ncpucall;
{$INCLUDE fpcdefs.inc}
interface
uses
  symdef,node,ncal,ncgcal;
type
  TSparccallnode = class(tcgcallnode)
    function pass_1 : tnode;override;
{Under SPARC, the frame pointer is automatically set by the SAVE instruction
which is part of the stardrad calling mechanism. This function will do nothing
else than adding the function prologue, which is in some case loading the
correct value into the frame pointer register!}
    procedure load_framepointer;override;
  end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defbase,paramgr,
{$ifdef GDB}
  {$ifdef delphi}
      sysutils,
  {$else}
      strings,
  {$endif}
      gdb,
{$endif GDB}
      cginfo,cgbase,pass_2,
      cpuinfo,cpubase,aasmbase,aasmtai,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cgobj,tgobj,regvars,rgobj,rgcpu,cg64f32,cgcpu,cpupi;

  function TSparccallnode.pass_1 : tnode;

    begin
       result:=inherited pass_1;
       if assigned(result) then
         exit;
       if procdefinition is tprocdef then
         begin
            if tprocdef(procdefinition).parast.datasize>TSparcprocinfo(procinfo).maxpushedparasize then
              TSparcprocinfo(procinfo).maxpushedparasize:=tprocdef(procdefinition).parast.datasize
         end
       else
         begin
            {!!!!}
         end;
    end;
procedure TSparcCallNode.load_framepointer;
	begin
	  exprasmList.concat(TAiCpu.Op_reg_const_reg(A_SAVE,S_L,stack_pointer_reg,-tprocdef(procdefinition).parast.datasize,stack_pointer_reg));
  end;
begin
   ccallnode:=TSparccallnode;
end.
{
  $Log$
  Revision 1.3  2002-09-30 19:12:14  mazen
  * function prologue fixed

  Revision 1.2  2002/08/30 13:16:23  mazen
  *call parameter handling is now based on the new param manager

  Revision 1.2  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite

  Revision 1.1  2002/08/13 21:40:59  florian
    * more fixes for ppc calling conventions
}

