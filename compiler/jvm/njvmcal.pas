{
    Copyright (c) 2011 by Jonas Maebe

    JVM-specific code for call nodes

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
unit njvmcal;

{$i fpcdefs.inc}

interface

    uses
      symdef,
      ncgcal;

    type

       { tjvmcallnode }

       tjvmcallnode = class(tcgcallnode)
        protected
         procedure set_result_location(realresdef: tstoreddef); override;
         procedure release_unused_return_value_cpu;override;
         procedure extra_post_call_code; override;
       end;


implementation

    uses
      verbose,globtype,
      symtype,defutil,ncal,
      cgbase,cgutils,tgobj,
      cpubase,aasmdata,aasmcpu,
      hlcgobj,hlcgcpu;


{*****************************************************************************
                             TJVMCALLNODE
*****************************************************************************}

    procedure tjvmcallnode.set_result_location(realresdef: tstoreddef);
      begin
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(realresdef),1);
        tg.gettemp(current_asmdata.CurrAsmList,realresdef.size,1,tt_normal,location.reference);
      end;


    procedure tjvmcallnode.release_unused_return_value_cpu;
      begin
        case resultdef.size of
          0:
            ;
          1..4:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop));
              thlcgjvm(hlcg).decstack(1);
            end;
          8:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop2));
              thlcgjvm(hlcg).decstack(2);
            end
          else
            internalerror(2011010305);
        end;
      end;


    procedure tjvmcallnode.extra_post_call_code;
      var
        totalremovesize: longint;
        realresdef: tdef;
      begin
        if not assigned(typedef) then
          realresdef:=tstoreddef(resultdef)
        else
          realresdef:=tstoreddef(typedef);
        totalremovesize:=pushedparasize-realresdef.size;
        { remove parameters from internal evaluation stack counter (in case of
          e.g. no parameters and a result, it can also increase) }
        if totalremovesize>0 then
          thlcgjvm(hlcg).decstack(totalremovesize shr 2)
        else if totalremovesize<0 then
          thlcgjvm(hlcg).incstack((-totalremovesize) shr 2);
      end;


begin
  ccallnode:=tjvmcallnode;
end.
