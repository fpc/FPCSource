{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    Generate PowerPC assembler for in memory related nodes

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
unit nppcmem;

{$i fpcdefs.inc}

interface

    uses
      node,nmem,ncgmem;

    type
       tppcvecnode = class(tcgvecnode)
          procedure pass_2;override;
       end;

implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtype,symdef,symsym,symtable,defbase,paramgr,
      aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_2,
      pass_1,nld,ncon,nadd,
      cpubase,
      cgobj,tgobj,rgobj,ncgutil;

{*****************************************************************************
                             TPPCVECNODE
*****************************************************************************}

    procedure tppcvecnode.pass_2;

      var
         extraoffset : longint;
         { rl stores the resulttype.def of the left node, this is necessary }
         { to detect if it is an ansistring                          }
         { because in constant nodes which constant index              }
         { the left tree is removed                                  }
         t   : tnode;
         href : treference;
         srsym : tsym;
         pushed : tpushedsaved;
         hightree : tnode;
         isjump  : boolean;
         otl,ofl : tasmlabel;
         newsize : tcgsize;
         pushedregs : tmaybesave;
      begin
         newsize:=def_cgsize(resulttype.def);
         location_reset(location,LOC_REFERENCE,newsize);

         secondpass(left);
         { we load the array reference to location }

         { an ansistring needs to be dereferenced }
         if is_ansistring(left.resulttype.def) or
            is_widestring(left.resulttype.def) then
           begin
              if nf_callunique in flags then
                begin
                   if left.location.loc<>LOC_REFERENCE then
                     begin
                        CGMessage(cg_e_illegal_expression);
                        exit;
                     end;
                   rg.saveusedregisters(exprasmlist,pushed,all_registers);
                   cg.a_paramaddr_ref(exprasmlist,left.location.reference,paramanager.getintparaloc(1));
                   rg.saveregvars(exprasmlist,all_registers);
                   cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_UNIQUE');
                   cg.g_maybe_loadself(exprasmlist);
                   rg.restoreusedregisters(exprasmlist,pushed);
                end;

              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    location_release(exprasmlist,left.location);
                    location.reference.base:=rg.getregisterint(exprasmlist);
                    cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032218);
              end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   rg.saveusedregisters(exprasmlist,pushed,all_registers);
                   cg.a_param_reg(exprasmlist,OS_ADDR,location.reference.base,paramanager.getintparaloc(1));
                   rg.saveregvars(exprasmlist,all_registers);
                   cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_CHECKZERO');
                   cg.g_maybe_loadself(exprasmlist);
                   rg.restoreusedregisters(exprasmlist,pushed);
                end;

              { in ansistrings/widestrings S[1] is p<w>char(S)[0] !! }
              if is_ansistring(left.resulttype.def) then
                dec(location.reference.offset)
              else
                dec(location.reference.offset,2);

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              location_copy(left.location,location);
           end
         else if is_dynamic_array(left.resulttype.def) then
         { ... also a dynamic string }
           begin
              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                     location_release(exprasmlist,left.location);
                     location.reference.base:=rg.getaddressregister(exprasmlist);
                     cg.a_load_ref_reg(exprasmlist,OS_ADDR,
                      left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032219);
              end;

{$warning FIXME}
              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   rg.saveusedregisters(exprasmlist,pushed,all_registers);
                   cg.a_param_reg(exprasmlist,OS_ADDR,location.reference.base,paramanager.getintparaloc(1));
                   rg.saveregvars(exprasmlist,all_registers);
                   cg.a_call_name(exprasmlist,'FPC_ANSISTR_CHECKZERO');
                   cg.g_maybe_loadself(exprasmlist);
                   rg.restoreusedregisters(exprasmlist,pushed);
                end;

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              location_copy(left.location,location);
           end
         else
           location_copy(location,left.location);

         { offset can only differ from 0 if arraydef }
         if (left.resulttype.def.deftype=arraydef) and
           not(is_dynamic_array(left.resulttype.def)) then
           dec(location.reference.offset,
               get_mul_size*tarraydef(left.resulttype.def).lowrange);
         if right.nodetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if (left.resulttype.def.deftype=arraydef) then
                begin
                   if not(is_open_array(left.resulttype.def)) and
                      not(is_array_of_const(left.resulttype.def)) and
                      not(is_dynamic_array(left.resulttype.def)) then
                     begin
                        if (tordconstnode(right).value>tarraydef(left.resulttype.def).highrange) or
                           (tordconstnode(right).value<tarraydef(left.resulttype.def).lowrange) then
                           begin
                          { this should be caught in the resulttypepass! (JM) }
                              if (cs_check_range in aktlocalswitches) then
                                CGMessage(parser_e_range_check_error)
                              else
                                CGMessage(parser_w_range_check_error);
                           end;
                        dec(left.location.reference.offset,
                            get_mul_size*tarraydef(left.resulttype.def).lowrange);
                     end
                   else
                     begin
                        { range checking for open and dynamic arrays !!!! }
{$warning FIXME}
                        {!!!!!!!!!!!!!!!!!}
                     end;
                end
              else if (left.resulttype.def.deftype=stringdef) then
                begin
                   if (tordconstnode(right).value=0) and
                      not(is_shortstring(left.resulttype.def)) then
                    { this should be caught in the resulttypepass! (JM) }
                     CGMessage(cg_e_can_access_element_zero);

                   if (cs_check_range in aktlocalswitches) then
                    begin
                      case tstringdef(left.resulttype.def).string_typ of
                        { it's the same for ansi- and wide strings }
                        st_widestring,
                        st_ansistring:
                          begin
                             rg.saveusedregisters(exprasmlist,pushed,all_registers);
                             cg.a_param_const(exprasmlist,OS_INT,tordconstnode(right).value,paramanager.getintparaloc(2));
                             href:=location.reference;
                             dec(href.offset,7);
                             cg.a_param_ref(exprasmlist,OS_INT,href,paramanager.getintparaloc(1));
                             rg.saveregvars(exprasmlist,all_registers);
                             cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_RANGECHECK');
                             rg.restoreusedregisters(exprasmlist,pushed);
                             cg.g_maybe_loadself(exprasmlist);
                          end;

                        st_shortstring:
                          begin
                             {!!!!!!!!!!!!!!!!!}
                          end;

                        st_longstring:
                          begin
                             {!!!!!!!!!!!!!!!!!}
                          end;
                      end;
                    end;
                end;
              inc(left.location.reference.offset,
                  get_mul_size*tordconstnode(right).value);

              location_copy(location,left.location);
           end
         else
         { not nodetype=ordconstn }
           begin
              if (cs_regalloc in aktglobalswitches) and
                 { if we do range checking, we don't }
                 { need that fancy code (it would be }
                 { buggy)                            }
                 not(cs_check_range in aktlocalswitches) and
                 (left.resulttype.def.deftype=arraydef) then
                begin
                   extraoffset:=0;
                   if (right.nodetype=addn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).right).value;
                             t:=taddnode(right).left;
                             { First pass processed this with the assumption   }
                             { that there was an add node which may require an }
                             { extra register. Fake it or die with IE10 (JM)   }
                             t.registers32 := taddnode(right).registers32;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
                        else if taddnode(right).left.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).left).value;
                             t:=taddnode(right).right;
                             t.registers32 :=  right.registers32;
                             taddnode(right).right:=nil;
                             right.free;
                             right:=t;
                          end;
                     end
                   else if (right.nodetype=subn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
{ this was "extraoffset:=right.right.value;" Looks a bit like
  copy-paste bug :) (JM) }
                             extraoffset:=-tordconstnode(taddnode(right).right).value;
                             t:=taddnode(right).left;
                             t.registers32 :=  right.registers32;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
{ You also have to negate right.right in this case! I can't add an
  unaryminusn without causing a crash, so I've disabled it (JM)
                        else if right.left.nodetype=ordconstn then
                          begin
                             extraoffset:=right.left.value;
                             t:=right.right;
                             t^.registers32 :=  right.registers32;
                             putnode(right);
                             putnode(right.left);
                             right:=t;
                         end;}
                     end;
                   inc(location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if not(location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                { should be internalerror! (JM) }
                CGMessage(cg_e_illegal_expression);
              isjump:=(right.location.loc=LOC_JUMP);
              if isjump then
               begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
               end;
              maybe_save(exprasmlist,right.registers32,location,pushedregs);
              secondpass(right);
              maybe_restore(exprasmlist,location,pushedregs);
              { here we change the location of right
                and the update was forgotten so it
                led to wrong code in emitrangecheck later PM
                so make range check before }

              if cs_check_range in aktlocalswitches then
               begin
                 if left.resulttype.def.deftype=arraydef then
                   begin
                     if is_open_array(left.resulttype.def) or
                        is_array_of_const(left.resulttype.def) then
                      begin
                        tarraydef(left.resulttype.def).genrangecheck;
                        srsym:=searchsymonlyin(tloadnode(left).symtable,
                          'high'+tvarsym(tloadnode(left).symtableentry).name);
                        hightree:=cloadnode.create(tvarsym(srsym),tloadnode(left).symtable);
                        firstpass(hightree);
                        secondpass(hightree);
                        location_release(exprasmlist,hightree.location);
                        reference_reset_symbol(href,newasmsymbol(tarraydef(left.resulttype.def).getrangecheckstring),4);
                        cg.a_load_loc_ref(exprasmlist,hightree.location,href);
                        hightree.free;
                        hightree:=nil;
                      end;
                     cg.g_rangecheck(exprasmlist,right,left.resulttype.def);
                   end;
               end;

              location_force_reg(exprasmlist,right.location,OS_32,false);

              if isjump then
               begin
                 truelabel:=otl;
                 falselabel:=ofl;
               end;

            { produce possible range check code: }
              if cs_check_range in aktlocalswitches then
               begin
                 if left.resulttype.def.deftype=arraydef then
                   begin
                     { done defore (PM) }
                   end
                 else if (left.resulttype.def.deftype=stringdef) then
                   begin
                      case tstringdef(left.resulttype.def).string_typ of
                         { it's the same for ansi- and wide strings }
                         st_widestring,
                         st_ansistring:
                           begin
                              rg.saveusedregisters(exprasmlist,pushed,all_registers);
                              cg.a_param_reg(exprasmlist,OS_INT,right.location.register,paramanager.getintparaloc(1));
                              href:=location.reference;
                              dec(href.offset,7);
                              cg.a_param_ref(exprasmlist,OS_INT,href,paramanager.getintparaloc(1));
                              rg.saveregvars(exprasmlist,all_registers);
                              cg.a_call_name(exprasmlist,'FPC_'+Upper(tstringdef(left.resulttype.def).stringtypname)+'_RANGECHECK');
                              rg.restoreusedregisters(exprasmlist,pushed);
                              cg.g_maybe_loadself(exprasmlist);
                           end;
                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                      end;
                   end;
               end;

              if location.reference.index=R_NO then
               begin
                 location.reference.index:=right.location.register;
                 cg.a_op_const_reg(exprasmlist,OP_IMUL,get_mul_size,
                   right.location.register);
               end
              else
               begin
                 if location.reference.base=R_NO then
                   { this wouldn't make sense for the ppc since there are }
                   { no scalefactors (JM)                                 }
                   internalerror(2002072901)
                 else
                  begin
                    cg.a_loadaddr_ref_reg(exprasmlist,location.reference,
                      location.reference.base);
                    rg.ungetregisterint(exprasmlist,location.reference.index);
                    { the symbol offset is loaded,             }
                    { so release the symbol name and set symbol  }
                    { to nil                                 }
                    location.reference.symbol:=nil;
                    location.reference.offset:=0;
                    cg.a_op_const_reg(exprasmlist,OP_IMUL,
                      get_mul_size,right.location.register);
                    location.reference.index:=right.location.register;
                  end;
               end;

           end;

        location.size:=newsize;
      end;


begin
   cvecnode:=tppcvecnode;
end.
{
  $Log$
  Revision 1.1  2002-07-29 09:21:30  jonas
    + tppcvecnode, almost straight copy of the i386 code, can most likely
      be made generic if all treference type allow a base, index and offset

}
