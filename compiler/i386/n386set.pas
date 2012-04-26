{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for in set/case nodes

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
unit n386set;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      node,nset,pass_1,nx86set;

    type
      ti386casenode = class(tx86casenode)
         procedure optimizevalues(var max_linear_list:aint;var max_dist:aword);override;
      end;


implementation

    uses
      systems,
      verbose,globals,constexp,
      symconst,symdef,defutil,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,procinfo,
      cga,cgutils,cgobj,ncgutil,
      cgx86;


{*****************************************************************************
                            TI386CASENODE
*****************************************************************************}

    procedure ti386casenode.optimizevalues(var max_linear_list:aint;var max_dist:aword);
      begin
        { a jump table crashes the pipeline! }
        if current_settings.optimizecputype=cpu_386 then
          inc(max_linear_list,3)
        else if current_settings.optimizecputype=cpu_Pentium then
          inc(max_linear_list,6)
        else if current_settings.optimizecputype in [cpu_Pentium2,cpu_Pentium3] then
          inc(max_linear_list,9)
        else if current_settings.optimizecputype=cpu_Pentium4 then
          inc(max_linear_list,14);
      end;


begin
   ccasenode:=ti386casenode;
end.
