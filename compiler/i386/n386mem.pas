{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for in memory related nodes

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
unit n386mem;

{$i defines.inc}

interface

    uses
      node,nmem,ncgmem;

    type
       ti386newnode = class(tnewnode)
          procedure pass_2;override;
       end;

       ti386addrnode = class(tcgaddrnode)
          procedure pass_2;override;
       end;

       ti386simplenewdisposenode = class(tsimplenewdisposenode)
          procedure pass_2;override;
       end;

       ti386derefnode = class(tcgderefnode)
          procedure pass_2;override;
       end;

       ti386vecnode = class(tvecnode)
          procedure pass_2;override;
       end;

implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      globtype,systems,
      cutils,verbose,globals,
      symconst,symtype,symdef,symsym,symtable,aasm,types,
      cgbase,temp_gen,pass_2,
      pass_1,nld,ncon,nadd,
      cpubase,cpuasm,
      cgobj,cga,tgcpu,n386util;

{*****************************************************************************
                            TI386NEWNODE
*****************************************************************************}

    procedure ti386newnode.pass_2;
      var
         pushed : tpushed;
         r : preference;
      begin
         if assigned(left) then
           begin
              secondpass(left);
              location.register:=left.location.register;
           end
         else
           begin
              pushusedregisters(pushed,$ff);

              gettempofsizereference(target_info.size_of_pointer,location.reference);

              { determines the size of the mem block }
              push_int(tpointerdef(resulttype.def).pointertype.def.size);
              emit_push_lea_loc(location,false);
              saveregvars($ff);
              emitcall('FPC_GETMEM');

              if tpointerdef(resulttype.def).pointertype.def.needs_inittable then
                begin
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=tstoreddef(tpointerdef(resulttype.def).pointertype.def).get_rtti_label(initrtti);
                   emitpushreferenceaddr(r^);
                   dispose(r);
                   { push pointer we just allocated, we need to initialize the
                     data located at that pointer not the pointer self (PFV) }
                   emit_push_loc(location);
                   emitcall('FPC_INITIALIZE');
                end;
              popusedregisters(pushed);
              { may be load ESI }
              maybe_loadself;
           end;
         if codegenerror then
           exit;
      end;


{*****************************************************************************
                             TI386ADDRNODE
*****************************************************************************}

    procedure ti386addrnode.pass_2;

      begin
        inherited pass_2;
        { for use of other segments }
        if left.location.reference.segment<>R_NO then
          location.segment:=left.location.reference.segment;
      end;

{*****************************************************************************
                         TI386SIMPLENEWDISPOSENODE
*****************************************************************************}

    procedure ti386simplenewdisposenode.pass_2;

      var
         pushed : tpushed;
         r : preference;

      begin
         secondpass(left);
         if codegenerror then
           exit;

         pushusedregisters(pushed,$ff);
         saveregvars($ff);

         { call the mem handling procedures }
         case nodetype of
           simpledisposen:
             begin
                if tpointerdef(left.resulttype.def).pointertype.def.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=tstoreddef(tpointerdef(left.resulttype.def).pointertype.def).get_rtti_label(initrtti);
                     emitpushreferenceaddr(r^);
                     dispose(r);
                     { push pointer adress }
                     emit_push_loc(left.location);
                     emitcall('FPC_FINALIZE');
                  end;
                emit_push_loc(left.location);
                emitcall('FPC_FREEMEM');
             end;
           simplenewn:
             begin
                { determines the size of the mem block }
                push_int(tpointerdef(left.resulttype.def).pointertype.def.size);
                emit_push_lea_loc(left.location,true);
                emitcall('FPC_GETMEM');
                if tpointerdef(left.resulttype.def).pointertype.def.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=tstoreddef(tpointerdef(left.resulttype.def).pointertype.def).get_rtti_label(initrtti);
                     emitpushreferenceaddr(r^);
                     dispose(r);
                     emit_push_loc(left.location);
                     emitcall('FPC_INITIALIZE');
                  end;
             end;
         end;
         popusedregisters(pushed);
         { may be load ESI }
         maybe_loadself;
      end;


{*****************************************************************************
                           TI386DEREFNODE
*****************************************************************************}

    procedure ti386derefnode.pass_2;

      begin
         inherited pass_2;
         if tpointerdef(left.resulttype.def).is_far then
          location.reference.segment:=R_FS;
         if not tpointerdef(left.resulttype.def).is_far and
            (cs_gdb_heaptrc in aktglobalswitches) and
            (cs_checkpointer in aktglobalswitches) then
              begin
                 emit_reg(
                   A_PUSH,S_L,location.reference.base);
                 emitcall('FPC_CHECKPOINTER');
              end;
      end;


{*****************************************************************************
                             TI386VECNODE
*****************************************************************************}

    procedure ti386vecnode.pass_2;
      var
        is_pushed : boolean;
        ind,hr : tregister;
        //_p : tnode;

          function get_mul_size:longint;
          begin
            if nf_memindex in flags then
             get_mul_size:=1
            else
             begin
               if (left.resulttype.def.deftype=arraydef) then
                get_mul_size:=tarraydef(left.resulttype.def).elesize
               else
                get_mul_size:=resulttype.def.size;
             end
          end;

          procedure calc_emit_mul;
          var
             l1,l2 : longint;
          begin
            l1:=get_mul_size;
            case l1 of
             1,2,4,8 : location.reference.scalefactor:=l1;
            else
              begin
                 if ispowerof2(l1,l2) then
                   emit_const_reg(A_SHL,S_L,l2,ind)
                 else
                   emit_const_reg(A_IMUL,S_L,l1,ind);
              end;
            end;
          end;

      var
         extraoffset : longint;
         { rl stores the resulttype.def of the left node, this is necessary }
         { to detect if it is an ansistring                          }
         { because in constant nodes which constant index              }
         { the left tree is removed                                  }
         t   : tnode;
         hp  : preference;
         href : treference;
         tai : Taicpu;
         srsym : tsym;
         pushed : tpushed;
         hightree : tnode;
         hl,otl,ofl : tasmlabel;
      begin
         secondpass(left);
         { we load the array reference to location }

         { an ansistring needs to be dereferenced }
         if is_ansistring(left.resulttype.def) or
           is_widestring(left.resulttype.def) then
           begin
              reset_reference(location.reference);
              if nf_callunique in flags then
                begin
                   if left.location.loc<>LOC_REFERENCE then
                     begin
                        CGMessage(cg_e_illegal_expression);
                        exit;
                     end;
                   pushusedregisters(pushed,$ff);
                   emitpushreferenceaddr(left.location.reference);
                   saveregvars($ff);
                   if is_ansistring(left.resulttype.def) then
                     emitcall('FPC_ANSISTR_UNIQUE')
                   else
                     emitcall('FPC_WIDESTR_UNIQUE');
                   maybe_loadself;
                   popusedregisters(pushed);
                end;

              if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                begin
                   location.reference.base:=left.location.register;
                end
              else
                begin
                   del_reference(left.location.reference);
                   location.reference.base:=getregisterint;
                   emit_ref_reg(A_MOV,S_L,
                     newreference(left.location.reference),
                     location.reference.base);
                end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   pushusedregisters(pushed,$ff);
                   emit_reg(A_PUSH,S_L,location.reference.base);
                   saveregvars($ff);
                   emitcall('FPC_ANSISTR_CHECKZERO');
                   maybe_loadself;
                   popusedregisters(pushed);
                end;

              if is_ansistring(left.resulttype.def) then
                { in ansistrings S[1] is pchar(S)[0] !! }
                dec(location.reference.offset)
              else
                begin
                   { in widestrings S[1] is pwchar(S)[0] !! }
                   dec(location.reference.offset,2);
//                   emit_const_reg(A_SHL,S_L,
//                     1,location.reference.base);
                end;

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              set_location(left.location,location);
           end
         else if is_dynamic_array(left.resulttype.def) then
         { ... also a dynamic string }
           begin
              reset_reference(location.reference);

              if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                begin
                   location.reference.base:=left.location.register;
                end
              else
                begin
                   del_reference(left.location.reference);
                   location.reference.base:=getregisterint;
                   emit_ref_reg(A_MOV,S_L,
                     newreference(left.location.reference),
                     location.reference.base);
                end;
{$warning FIXME}
              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   pushusedregisters(pushed,$ff);
                   emit_reg(A_PUSH,S_L,location.reference.base);
                   saveregvars($ff);
                   emitcall('FPC_ANSISTR_CHECKZERO');
                   maybe_loadself;
                   popusedregisters(pushed);
                end;

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              set_location(left.location,location);
           end
         else
           set_location(location,left.location);

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
                   if (tordconstnode(right).value=0) and not(is_shortstring(left.resulttype.def)) then
                     CGMessage(cg_e_can_access_element_zero);

                   if (cs_check_range in aktlocalswitches) then
                     case tstringdef(left.resulttype.def).string_typ of
                        { it's the same for ansi- and wide strings }
                        st_widestring,
                        st_ansistring:
                          begin
                             pushusedregisters(pushed,$ff);
                             push_int(tordconstnode(right).value);
                             hp:=newreference(location.reference);
                             dec(hp^.offset,7);
                             emit_ref(A_PUSH,S_L,hp);
                             saveregvars($ff);
                             emitcall('FPC_ANSISTR_RANGECHECK');
                             popusedregisters(pushed);
                             maybe_loadself;
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
              inc(left.location.reference.offset,
                  get_mul_size*tordconstnode(right).value);
              if nf_memseg in flags then
                left.location.reference.segment:=R_FS;
              {
              left.resulttype:=resulttype.def;
              disposetree(right);
              _p:=left;
              putnode(p);
              p:=_p;
              }
              set_location(location,left.location);
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
                        else if tordconstnode(taddnode(right).left).nodetype=ordconstn then
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
              if (location.loc<>LOC_REFERENCE) and
                 (location.loc<>LOC_MEM) then
                CGMessage(cg_e_illegal_expression);
              if (right.location.loc=LOC_JUMP) then
               begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
               end;
              is_pushed:=maybe_push(right.registers32,self,false);
              secondpass(right);
              if is_pushed then
                restore(self,false);
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
                        reset_reference(href);
                        tarraydef(left.resulttype.def).genrangecheck;
                        href.symbol:=newasmsymbol(tarraydef(left.resulttype.def).getrangecheckstring);
                        href.offset:=4;
                        srsym:=searchsymonlyin(tloadnode(left).symtable,
                          'high'+tvarsym(tloadnode(left).symtableentry).name);
                        hightree:=cloadnode.create(tvarsym(srsym),tloadnode(left).symtable);
                        firstpass(hightree);
                        secondpass(hightree);
                        emit_mov_loc_ref(hightree.location,href,S_L,true);
                        hightree.free;
                        hightree:=nil;
                      end;
                     cg.g_rangecheck(exprasmlist,right,left.resulttype.def);
                   end;
               end;

              case right.location.loc of
                 LOC_REGISTER:
                   begin
                      ind:=right.location.register;
                      case right.resulttype.def.size of
                         1:
                           begin
                              hr:=reg8toreg32(ind);
                              emit_reg_reg(A_MOVZX,S_BL,ind,hr);
                              ind:=hr;
                           end;
                         2:
                           begin
                              hr:=reg16toreg32(ind);
                              emit_reg_reg(A_MOVZX,S_WL,ind,hr);
                              ind:=hr;
                           end;
                      end;
                   end;
                 LOC_CREGISTER:
                   begin
                      ind:=getregisterint;
                      case right.resulttype.def.size of
                         1:
                           emit_reg_reg(A_MOVZX,S_BL,right.location.register,ind);
                         2:
                           emit_reg_reg(A_MOVZX,S_WL,right.location.register,ind);
                         4:
                           emit_reg_reg(A_MOV,S_L,right.location.register,ind);
                      end;
                   end;
                 LOC_FLAGS:
                   begin
                      ind:=getregisterint;
                      emit_flag2reg(right.location.resflags,reg32toreg8(ind));
                      emit_reg_reg(A_MOVZX,S_BL,reg32toreg8(ind),ind);
                   end;
                 LOC_JUMP :
                   begin
                     ind:=getregisterint;
                     emitlab(truelabel);
                     truelabel:=otl;
                     emit_const_reg(A_MOV,S_L,1,ind);
                     getlabel(hl);
                     emitjmp(C_None,hl);
                     emitlab(falselabel);
                     falselabel:=ofl;
                     emit_reg_reg(A_XOR,S_L,ind,ind);
                     emitlab(hl);
                   end;
                 LOC_REFERENCE,LOC_MEM :
                   begin
                      del_reference(right.location.reference);
                      ind:=getregisterint;
                      { Booleans are stored in an 8 bit memory location, so
                        the use of MOVL is not correct }
                      case right.resulttype.def.size of
                       1 : tai:=Taicpu.Op_ref_reg(A_MOVZX,S_BL,newreference(right.location.reference),ind);
                       2 : tai:=Taicpu.Op_ref_reg(A_MOVZX,S_WL,newreference(right.location.reference),ind);
                       4 : tai:=Taicpu.Op_ref_reg(A_MOV,S_L,newreference(right.location.reference),ind);
                      end;
                      exprasmList.concat(tai);
                   end;
                 else
                   internalerror(5913428);
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
                              pushusedregisters(pushed,$ff);
                              emit_reg(A_PUSH,S_L,ind);
                              hp:=newreference(location.reference);
                              dec(hp^.offset,7);
                              emit_ref(A_PUSH,S_L,hp);
                              saveregvars($ff);
                              emitcall('FPC_ANSISTR_RANGECHECK');
                              popusedregisters(pushed);
                              maybe_loadself;
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
                 location.reference.index:=ind;
                 calc_emit_mul;
               end
              else
               begin
                 if location.reference.base=R_NO then
                  begin
                    case location.reference.scalefactor of
                     2 : emit_const_reg(A_SHL,S_L,1,location.reference.index);
                     4 : emit_const_reg(A_SHL,S_L,2,location.reference.index);
                     8 : emit_const_reg(A_SHL,S_L,3,location.reference.index);
                    end;
                    calc_emit_mul;
                    location.reference.base:=location.reference.index;
                    location.reference.index:=ind;
                  end
                 else
                  begin
                    emit_ref_reg(
                      A_LEA,S_L,newreference(location.reference),
                      location.reference.index);
                    ungetregister32(location.reference.base);
                    { the symbol offset is loaded,             }
                    { so release the symbol name and set symbol  }
                    { to nil                                 }
                    location.reference.symbol:=nil;
                    location.reference.offset:=0;
                    calc_emit_mul;
                    location.reference.base:=location.reference.index;
                    location.reference.index:=ind;
                  end;
               end;

              if nf_memseg in flags then
                location.reference.segment:=R_FS;
           end;
      end;


begin
   cnewnode:=ti386newnode;
   csimplenewdisposenode:=ti386simplenewdisposenode;
   caddrnode:=ti386addrnode;
   cderefnode:=ti386derefnode;
   cvecnode:=ti386vecnode;
end.
{
  $Log$
  Revision 1.19  2001-12-30 17:24:47  jonas
    * range checking is now processor independent (part in cgobj, part in    cg64f32) and should work correctly again (it needed some changes after    the changes of the low and high of tordef's to int64)  * maketojumpbool() is now processor independent (in ncgutil)  * getregister32 is now called getregisterint

  Revision 1.18  2001/12/03 21:48:43  peter
    * freemem change to value parameter
    * torddef low/high range changed to int64

  Revision 1.17  2001/09/30 16:17:17  jonas
    * made most constant and mem handling processor independent

  Revision 1.16  2001/08/30 20:13:57  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.15  2001/08/26 13:37:00  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.14  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.13  2001/04/18 22:02:03  peter
    * registration of targets and assemblers

  Revision 1.12  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.11  2001/04/02 21:20:38  peter
    * resulttype rewrite

  Revision 1.10  2001/03/11 22:58:52  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.9  2001/02/02 22:38:00  peter
    * fixed crash with new(precord), merged

  Revision 1.8  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.7  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.6  2000/11/29 00:30:48  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.5  2000/11/04 14:25:24  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.4  2000/10/31 22:02:57  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/31 14:18:53  jonas
    * merged double deleting of left location when using a temp in
      secondwith (merged from fixes branch). This also fixes web bug1194

  Revision 1.2  2000/10/21 18:16:13  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.1  2000/10/15 09:33:32  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.2  2000/10/14 21:52:54  peter
    * fixed memory leaks

  Revision 1.1  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

}
