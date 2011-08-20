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
         procedure extra_pre_call_code; override;
         procedure set_result_location(realresdef: tstoreddef); override;
         procedure do_release_unused_return_value;override;
         procedure extra_post_call_code; override;
       end;


implementation

    uses
      verbose,globtype,
      symconst,symtype,defutil,ncal,
      cgbase,cgutils,tgobj,procinfo,
      cpubase,aasmdata,aasmcpu,
      hlcgobj,hlcgcpu,
      jvmdef;


{*****************************************************************************
                             TJVMCALLNODE
*****************************************************************************}

    procedure tjvmcallnode.extra_pre_call_code;
      begin
        { when calling a constructor, first create a new instance, except
          when calling it from another constructor (because then this has
          already been done before calling the current constructor) }
        if procdefinition.typ<>procdef then
          exit;
        if tprocdef(procdefinition).proctypeoption<>potype_constructor then
          exit;
        if (methodpointer.resultdef.typ<>classrefdef) then
          exit;
        current_asmdata.CurrAsmList.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(tobjectdef(tprocdef(procdefinition).owner.defowner).jvm_full_typename)));
        { the constructor doesn't return anything, so put a duplicate of the
          self pointer on the evaluation stack for use as function result
          after the constructor has run }
        current_asmdata.CurrAsmList.concat(taicpu.op_none(a_dup));
        thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,2);
      end;


    procedure tjvmcallnode.set_result_location(realresdef: tstoreddef);
      begin
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(realresdef),1);
        tg.gettemp(current_asmdata.CurrAsmList,realresdef.size,1,tt_normal,location.reference);
      end;


    procedure tjvmcallnode.do_release_unused_return_value;
      begin
        if (tprocdef(procdefinition).proctypeoption=potype_constructor) and
           (current_procinfo.procdef.proctypeoption=potype_constructor) then
          exit;
        case resultdef.size of
          0:
            ;
          1..4:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,1);
            end;
          8:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_none(a_pop2));
              thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,2);
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
        { a constructor doesn't actually return a value in the jvm }
        if (tprocdef(procdefinition).proctypeoption=potype_constructor) then
          totalremovesize:=pushedparasize
        else
          totalremovesize:=pushedparasize-realresdef.size;
        { remove parameters from internal evaluation stack counter (in case of
          e.g. no parameters and a result, it can also increase) }
        if totalremovesize>0 then
          thlcgjvm(hlcg).decstack(current_asmdata.CurrAsmList,totalremovesize shr 2)
        else if totalremovesize<0 then
          thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,(-totalremovesize) shr 2);
      end;


begin
  ccallnode:=tjvmcallnode;
end.
