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
unit cg386mem;
interface

    uses
      tree;

    procedure secondloadvmt(var p : ptree);
    procedure secondhnewn(var p : ptree);
    procedure secondnewn(var p : ptree);
    procedure secondhdisposen(var p : ptree);
    procedure secondsimplenewdispose(var p : ptree);
    procedure secondaddr(var p : ptree);
    procedure seconddoubleaddr(var p : ptree);
    procedure secondderef(var p : ptree);
    procedure secondsubscriptn(var p : ptree);
    procedure secondvecn(var p : ptree);
    procedure secondselfn(var p : ptree);
    procedure secondwith(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,pass_1,
      cpubase,cpuasm,
      cgai386,tgeni386;

{*****************************************************************************
                             SecondLoadVMT
*****************************************************************************}

    procedure secondloadvmt(var p : ptree);
      begin
         p^.location.register:=getregister32;
         emit_sym_ofs_reg(A_MOV,
            S_L,newasmsymbol(pobjectdef(pclassrefdef(p^.resulttype)^.pointertype.def)^.vmt_mangledname),0,
            p^.location.register);
      end;


{*****************************************************************************
                             SecondHNewN
*****************************************************************************}

    procedure secondhnewn(var p : ptree);
      begin
      end;


{*****************************************************************************
                             SecondNewN
*****************************************************************************}

    procedure secondnewn(var p : ptree);
      var
         pushed : tpushed;
         r : preference;
      begin
         if assigned(p^.left) then
           begin
              secondpass(p^.left);
              p^.location.register:=p^.left^.location.register;
           end
         else
           begin
              pushusedregisters(pushed,$ff);

              gettempofsizereference(target_os.size_of_pointer,p^.location.reference);

              { determines the size of the mem block }
              push_int(ppointerdef(p^.resulttype)^.pointertype.def^.size);
              emit_push_lea_loc(p^.location,false);
              emitcall('FPC_GETMEM');

              if ppointerdef(p^.resulttype)^.pointertype.def^.needs_inittable then
                begin
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=ppointerdef(p^.left^.resulttype)^.pointertype.def^.get_inittable_label;
                   emitpushreferenceaddr(r^);
                   dispose(r);
                   { push pointer we just allocated, we need to initialize the
                     data located at that pointer not the pointer self (PFV) }
                   emit_push_loc(p^.location);
                   emitcall('FPC_INITIALIZE');
                end;
              popusedregisters(pushed);
              { may be load ESI }
              maybe_loadesi;
           end;
         if codegenerror then
           exit;
      end;


{*****************************************************************************
                             SecondDisposeN
*****************************************************************************}

    procedure secondhdisposen(var p : ptree);
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;
         reset_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER:
              p^.location.reference.index:=p^.left^.location.register;
            LOC_CREGISTER:
              begin
                 p^.location.reference.index:=getregister32;
                 emit_reg_reg(A_MOV,S_L,
                   p^.left^.location.register,
                   p^.location.reference.index);
              end;
            LOC_MEM,LOC_REFERENCE :
              begin
                 del_reference(p^.left^.location.reference);
                 p^.location.reference.index:=getregister32;
                 emit_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                   p^.location.reference.index);
              end;
         end;
      end;


{*****************************************************************************
                             SecondNewDispose
*****************************************************************************}

    procedure secondsimplenewdispose(var p : ptree);

      var
         pushed : tpushed;
         r : preference;

      begin
         secondpass(p^.left);
         if codegenerror then
           exit;

         pushusedregisters(pushed,$ff);

         { call the mem handling procedures }
         case p^.treetype of
           simpledisposen:
             begin
                if ppointerdef(p^.left^.resulttype)^.pointertype.def^.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=ppointerdef(p^.left^.resulttype)^.pointertype.def^.get_inittable_label;
                     emitpushreferenceaddr(r^);
                     dispose(r);
                     { push pointer adress }
                     emit_push_loc(p^.left^.location);
                     emitcall('FPC_FINALIZE');
                  end;
                emit_push_lea_loc(p^.left^.location,true);
                emitcall('FPC_FREEMEM');
             end;
           simplenewn:
             begin
                { determines the size of the mem block }
                push_int(ppointerdef(p^.left^.resulttype)^.pointertype.def^.size);
                emit_push_lea_loc(p^.left^.location,true);
                emitcall('FPC_GETMEM');
                if ppointerdef(p^.left^.resulttype)^.pointertype.def^.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=ppointerdef(p^.left^.resulttype)^.pointertype.def^.get_inittable_label;
                     emitpushreferenceaddr(r^);
                     dispose(r);
                     emit_push_loc(p^.left^.location);
                     emitcall('FPC_INITIALIZE');
                  end;
             end;
         end;
         popusedregisters(pushed);
         { may be load ESI }
         maybe_loadesi;
      end;


{*****************************************************************************
                             SecondAddr
*****************************************************************************}

    procedure secondaddr(var p : ptree);
      begin
         secondpass(p^.left);

         { when loading procvar we do nothing with this node, so load the
           location of left }
         if p^.procvarload then
          begin
            set_location(p^.location,p^.left^.location);
            exit;
          end;

         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         {@ on a procvar means returning an address to the procedure that
           is stored in it.}
         { yes but p^.left^.symtableentry can be nil
           for example on @self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (m_tp_procvar in aktmodeswitches) and
           (p^.left^.treetype=loadn) and
           assigned(p^.left^.symtableentry) and
           (p^.left^.symtableentry^.typ=varsym) and
           (pvarsym(p^.left^.symtableentry)^.vartype.def^.deftype=procvardef) then
           emit_ref_reg(A_MOV,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)
         else
           emit_ref_reg(A_LEA,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register);
           { for use of other segments }
           if p^.left^.location.reference.segment<>R_NO then
             p^.location.segment:=p^.left^.location.reference.segment;
      end;


{*****************************************************************************
                             SecondDoubleAddr
*****************************************************************************}

    procedure seconddoubleaddr(var p : ptree);
      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         emit_ref_reg(A_LEA,S_L,
         newreference(p^.left^.location.reference),
           p^.location.register);
      end;


{*****************************************************************************
                             SecondDeRef
*****************************************************************************}

    procedure secondderef(var p : ptree);
      var
         hr : tregister;
      begin
         secondpass(p^.left);
         reset_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER:
              p^.location.reference.base:=p^.left^.location.register;
            LOC_CREGISTER:
              begin
                 { ... and reserve one for the pointer }
                 hr:=getregister32;
                 emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hr);
                 p^.location.reference.base:=hr;
              end;
            else
              begin
                 { free register }
                 del_reference(p^.left^.location.reference);

                 { ...and reserve one for the pointer }
                 hr:=getregister32;
                 emit_ref_reg(
                   A_MOV,S_L,newreference(p^.left^.location.reference),
                   hr);
                 p^.location.reference.base:=hr;
              end;
         end;
         if ppointerdef(p^.left^.resulttype)^.is_far then
          p^.location.reference.segment:=R_FS;
         if not ppointerdef(p^.left^.resulttype)^.is_far and
            (cs_gdb_heaptrc in aktglobalswitches) and
            (cs_checkpointer in aktglobalswitches) then
              begin
                 emit_reg(
                   A_PUSH,S_L,p^.location.reference.base);
                 emitcall('FPC_CHECKPOINTER');
              end;
      end;


{*****************************************************************************
                             SecondSubScriptN
*****************************************************************************}

    procedure secondsubscriptn(var p : ptree);
      var
         hr : tregister;
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;
         { classes must be dereferenced implicit }
         if (p^.left^.resulttype^.deftype=objectdef) and
           pobjectdef(p^.left^.resulttype)^.is_class then
           begin
             reset_reference(p^.location.reference);
             case p^.left^.location.loc of
                LOC_REGISTER:
                  p^.location.reference.base:=p^.left^.location.register;
                LOC_CREGISTER:
                  begin
                     { ... and reserve one for the pointer }
                     hr:=getregister32;
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hr);
                       p^.location.reference.base:=hr;
                  end;
                else
                  begin
                     { free register }
                     del_reference(p^.left^.location.reference);

                     { ... and reserve one for the pointer }
                     hr:=getregister32;
                     emit_ref_reg(
                       A_MOV,S_L,newreference(p^.left^.location.reference),
                       hr);
                     p^.location.reference.base:=hr;
                  end;
             end;
           end
         else
           set_location(p^.location,p^.left^.location);

         inc(p^.location.reference.offset,p^.vs^.address);
      end;


{*****************************************************************************
                               SecondVecN
*****************************************************************************}

    procedure secondvecn(var p : ptree);
      var
        is_pushed : boolean;
        ind,hr : tregister;
        _p : ptree;

          function get_mul_size:longint;
          begin
            if p^.memindex then
             get_mul_size:=1
            else
             get_mul_size:=p^.resulttype^.size;
          end;

          procedure calc_emit_mul;
          var
             l1,l2 : longint;
          begin
            l1:=get_mul_size;
            case l1 of
             1,2,4,8 : p^.location.reference.scalefactor:=l1;
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
         { rl stores the resulttype of the left node, this is necessary }
         { to detect if it is an ansistring                          }
         { because in constant nodes which constant index              }
         { the left tree is removed                                  }
         t   : ptree;
         hp  : preference;
         href : treference;
         tai : Paicpu;
         pushed : tpushed;
         hightree : ptree;
         hl,otl,ofl : pasmlabel;
      begin
         secondpass(p^.left);
         { we load the array reference to p^.location }

         { an ansistring needs to be dereferenced }
         if is_ansistring(p^.left^.resulttype) or
           is_widestring(p^.left^.resulttype) then
           begin
              reset_reference(p^.location.reference);
              if p^.callunique then
                begin
                   if p^.left^.location.loc<>LOC_REFERENCE then
                     begin
                        CGMessage(cg_e_illegal_expression);
                        exit;
                     end;
                   pushusedregisters(pushed,$ff);
                   emitpushreferenceaddr(p^.left^.location.reference);
                   if is_ansistring(p^.left^.resulttype) then
                     emitcall('FPC_ANSISTR_UNIQUE')
                   else
                     emitcall('FPC_WIDESTR_UNIQUE');
                   maybe_loadesi;
                   popusedregisters(pushed);
                end;

              if p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                begin
                   p^.location.reference.base:=p^.left^.location.register;
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   p^.location.reference.base:=getregister32;
                   emit_ref_reg(A_MOV,S_L,
                     newreference(p^.left^.location.reference),
                     p^.location.reference.base);
                end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                   pushusedregisters(pushed,$ff);
                   emit_reg(A_PUSH,S_L,p^.location.reference.base);
                   emitcall('FPC_ANSISTR_CHECKZERO');
                   maybe_loadesi;
                   popusedregisters(pushed);
                end;

              if is_ansistring(p^.left^.resulttype) then
                { in ansistrings S[1] is pchar(S)[0] !! }
                dec(p^.location.reference.offset)
              else
                begin
                   { in widestrings S[1] is pwchar(S)[0] !! }
                   dec(p^.location.reference.offset,2);
                   emit_const_reg(A_SHL,S_L,
                     1,p^.location.reference.base);
                end;

              { we've also to keep left up-to-date, because it is used   }
              { if a constant array index occurs, subject to change (FK) }
              set_location(p^.left^.location,p^.location);
           end
         else
           set_location(p^.location,p^.left^.location);

         { offset can only differ from 0 if arraydef }
         if p^.left^.resulttype^.deftype=arraydef then
           dec(p^.location.reference.offset,
               get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
         if p^.right^.treetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   if not(is_open_array(p^.left^.resulttype)) and
                      not(is_array_of_const(p^.left^.resulttype)) then
                     begin
                        if (p^.right^.value>parraydef(p^.left^.resulttype)^.highrange) or
                           (p^.right^.value<parraydef(p^.left^.resulttype)^.lowrange) then
                           begin
                              if (cs_check_range in aktlocalswitches) then
                                CGMessage(parser_e_range_check_error)
                              else
                                CGMessage(parser_w_range_check_error);
                           end;
                        dec(p^.left^.location.reference.offset,
                            get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
                     end
                   else
                     begin
                        { range checking for open arrays !!!! }
                        {!!!!!!!!!!!!!!!!!}
                     end;
                end
              else if (p^.left^.resulttype^.deftype=stringdef) then
                begin
                   if (p^.right^.value=0) and not(is_shortstring(p^.left^.resulttype)) then
                     CGMessage(cg_e_can_access_element_zero);

                   if (cs_check_range in aktlocalswitches) then
                     case pstringdef(p^.left^.resulttype)^.string_typ of
                        { it's the same for ansi- and wide strings }
                        st_widestring,
                        st_ansistring:
                          begin
                             pushusedregisters(pushed,$ff);
                             push_int(p^.right^.value);
                             hp:=newreference(p^.location.reference);
                             dec(hp^.offset,7);
                             emit_ref(A_PUSH,S_L,hp);
                             emitcall('FPC_ANSISTR_RANGECHECK');
                             popusedregisters(pushed);
                             maybe_loadesi;
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

              inc(p^.left^.location.reference.offset,
                  get_mul_size*p^.right^.value);
              if p^.memseg then
                p^.left^.location.reference.segment:=R_FS;
              p^.left^.resulttype:=p^.resulttype;
              disposetree(p^.right);
              _p:=p^.left;
              putnode(p);
              p:=_p;
           end
         else
         { not treetype=ordconstn }
           begin
              { quick hack, to overcome Delphi 2 }
              if (cs_regalloc in aktglobalswitches) and
                (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   extraoffset:=0;
                   if (p^.right^.treetype=addn) then
                     begin
                        if p^.right^.right^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.right^.value;
                             t:=p^.right^.left;
                             putnode(p^.right^.right);
                             putnode(p^.right);
                             p^.right:=t
                          end
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                             t:=p^.right^.right;
                             putnode(p^.right^.left);
                             putnode(p^.right);
                             p^.right:=t
                          end;
                     end
                   else if (p^.right^.treetype=subn) then
                     begin
                        if p^.right^.right^.treetype=ordconstn then
                          begin
{ this was "extraoffset:=p^.right^.right^.value;" Looks a bit like
  copy-paste bug :) (JM) }
                             extraoffset:=-p^.right^.right^.value;
                             t:=p^.right^.left;
                             putnode(p^.right^.right);
                             putnode(p^.right);
                             p^.right:=t
                          end
{ You also have to negate p^.right^.right in this case! I can't add an
  unaryminusn without causing a crash, so I've disabled it (JM)
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                             t:=p^.right^.right;
                             putnode(p^.right);
                             putnode(p^.right^.left);
                             p^.right:=t;
                          end;}
                     end;
                   inc(p^.location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if (p^.location.loc<>LOC_REFERENCE) and
                 (p^.location.loc<>LOC_MEM) then
                CGMessage(cg_e_illegal_expression);
              if (p^.right^.location.loc=LOC_JUMP) then
               begin
                 otl:=truelabel;
                 getlabel(truelabel);
                 ofl:=falselabel;
                 getlabel(falselabel);
               end;
              is_pushed:=maybe_push(p^.right^.registers32,p,false);
              secondpass(p^.right);
              if is_pushed then
                restore(p,false);
              { here we change the location of p^.right
                and the update was forgotten so it
                led to wrong code in emitrangecheck later PM
                so make range check before }

              if cs_check_range in aktlocalswitches then
               begin
                 if p^.left^.resulttype^.deftype=arraydef then
                   begin
                     if is_open_array(p^.left^.resulttype) or
                        is_array_of_const(p^.left^.resulttype) then
                      begin
                        reset_reference(href);
                        parraydef(p^.left^.resulttype)^.genrangecheck;
                        href.symbol:=newasmsymbol(parraydef(p^.left^.resulttype)^.getrangecheckstring);
                        href.offset:=4;
                        getsymonlyin(p^.left^.symtable,'high'+pvarsym(p^.left^.symtableentry)^.name);
                        hightree:=genloadnode(pvarsym(srsym),p^.left^.symtable);
                        firstpass(hightree);
                        secondpass(hightree);
                        emit_mov_loc_ref(hightree^.location,href,S_L);
                        disposetree(hightree);
                      end;
                     emitrangecheck(p^.right,p^.left^.resulttype);
                   end;
               end;

              case p^.right^.location.loc of
                 LOC_REGISTER:
                   begin
                      ind:=p^.right^.location.register;
                      case p^.right^.resulttype^.size of
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
                      ind:=getregister32;
                      case p^.right^.resulttype^.size of
                         1:
                           emit_reg_reg(A_MOVZX,S_BL,p^.right^.location.register,ind);
                         2:
                           emit_reg_reg(A_MOVZX,S_WL,p^.right^.location.register,ind);
                         4:
                           emit_reg_reg(A_MOV,S_L,p^.right^.location.register,ind);
                      end;
                   end;
                 LOC_FLAGS:
                   begin
                      ind:=getregister32;
                      emit_flag2reg(p^.right^.location.resflags,reg32toreg8(ind));
                      emit_reg_reg(A_MOVZX,S_BL,reg32toreg8(ind),ind);
                   end;
                 LOC_JUMP :
                   begin
                     ind:=getregister32;
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
                      del_reference(p^.right^.location.reference);
                      ind:=getregister32;
                      { Booleans are stored in an 8 bit memory location, so
                        the use of MOVL is not correct }
                      case p^.right^.resulttype^.size of
                       1 : tai:=new(paicpu,op_ref_reg(A_MOVZX,S_BL,newreference(p^.right^.location.reference),ind));
                       2 : tai:=new(Paicpu,op_ref_reg(A_MOVZX,S_WL,newreference(p^.right^.location.reference),ind));
                       4 : tai:=new(Paicpu,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),ind));
                      end;
                      exprasmlist^.concat(tai);
                   end;
                 else
                   internalerror(5913428);
                end;

            { produce possible range check code: }
              if cs_check_range in aktlocalswitches then
               begin
                 if p^.left^.resulttype^.deftype=arraydef then
                   begin
                     { done defore (PM) }
                   end
                 else if (p^.left^.resulttype^.deftype=stringdef) then
                   begin
                      case pstringdef(p^.left^.resulttype)^.string_typ of
                         { it's the same for ansi- and wide strings }
                         st_widestring,
                         st_ansistring:
                           begin
                              pushusedregisters(pushed,$ff);
                              emit_reg(A_PUSH,S_L,ind);
                              hp:=newreference(p^.location.reference);
                              dec(hp^.offset,7);
                              emit_ref(A_PUSH,S_L,hp);
                              emitcall('FPC_ANSISTR_RANGECHECK');
                              popusedregisters(pushed);
                              maybe_loadesi;
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

              if p^.location.reference.index=R_NO then
               begin
                 p^.location.reference.index:=ind;
                 calc_emit_mul;
               end
              else
               begin
                 if p^.location.reference.base=R_NO then
                  begin
                    case p^.location.reference.scalefactor of
                     2 : emit_const_reg(A_SHL,S_L,1,p^.location.reference.index);
                     4 : emit_const_reg(A_SHL,S_L,2,p^.location.reference.index);
                     8 : emit_const_reg(A_SHL,S_L,3,p^.location.reference.index);
                    end;
                    calc_emit_mul;
                    p^.location.reference.base:=p^.location.reference.index;
                    p^.location.reference.index:=ind;
                  end
                 else
                  begin
                    emit_ref_reg(
                      A_LEA,S_L,newreference(p^.location.reference),
                      p^.location.reference.index);
                    ungetregister32(p^.location.reference.base);
                    { the symbol offset is loaded,             }
                    { so release the symbol name and set symbol  }
                    { to nil                                 }
                    p^.location.reference.symbol:=nil;
                    p^.location.reference.offset:=0;
                    calc_emit_mul;
                    p^.location.reference.base:=p^.location.reference.index;
                    p^.location.reference.index:=ind;
                  end;
               end;

              if p^.memseg then
                p^.location.reference.segment:=R_FS;
           end;
      end;

{*****************************************************************************
                               SecondSelfN
*****************************************************************************}

    procedure secondselfn(var p : ptree);
      begin
         reset_reference(p^.location.reference);
         if (p^.resulttype^.deftype=classrefdef) or
           ((p^.resulttype^.deftype=objectdef)
             and pobjectdef(p^.resulttype)^.is_class
           ) then
           p^.location.register:=R_ESI
         else
           p^.location.reference.base:=R_ESI;
      end;


{*****************************************************************************
                               SecondWithN
*****************************************************************************}

    procedure secondwith(var p : ptree);
      var
        usetemp,with_expr_in_temp : boolean;
      begin
         if assigned(p^.left) then
            begin
               secondpass(p^.left);
               if p^.left^.location.reference.segment<>R_NO then
                 message(parser_e_no_with_for_variable_in_other_segments);

               new(p^.withreference);

               usetemp:=false;
               if (p^.left^.treetype=loadn) and
                  (p^.left^.symtable=aktprocsym^.definition^.localst) then
                 begin
                    { for locals use the local storage }
                    p^.withreference^:=p^.left^.location.reference;
                    p^.islocal:=true;
                 end
               else
                { call can happend with a property }
{$ifdef AllocEDI}
                    exprasmlist^.concat(new(pairegalloc,alloc(R_EDI)));
{$endif AllocEDI}
                if { (p^.left^.treetype=calln) and  Don't think that
                      this is necessary (FK) }
                   (p^.left^.resulttype^.deftype=objectdef) and
                   pobjectdef(p^.left^.resulttype)^.is_class then
                 begin
                    emit_mov_loc_reg(p^.left^.location,R_EDI);
                    usetemp:=true;
                 end
               else
                 begin
                   emit_lea_loc_reg(p^.left^.location,R_EDI,false);
                   usetemp:=true;
                 end;

               release_loc(p^.left^.location);

               { if the with expression is stored in a temp    }
               { area we must make it persistent and shouldn't }
               { release it (FK)                               }
               if (p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                 istemp(p^.left^.location.reference) then
                 begin
                    normaltemptopersistant(p^.left^.location.reference.offset);
                    with_expr_in_temp:=true;
                 end
               else
                 with_expr_in_temp:=false;

               { if usetemp is set the value must be in %edi }
               if usetemp then
                begin
                  gettempofsizereference(4,p^.withreference^);
                  normaltemptopersistant(p^.withreference^.offset);
                  { move to temp reference }
                  emit_reg_ref(A_MOV,S_L,
                    R_EDI,newreference(p^.withreference^));
{$ifdef AllocEDI}
                  exprasmlist^.concat(new(pairegalloc,dealloc(R_EDI)));
{$endif AllocEDI}
                  del_reference(p^.left^.location.reference);
                end;

               { p^.right can be optimize out !!! }
               if assigned(p^.right) then
                 secondpass(p^.right);

               if usetemp then
                 ungetpersistanttemp(p^.withreference^.offset);

               if with_expr_in_temp then
                 ungetpersistanttemp(p^.left^.location.reference.offset);

               dispose(p^.withreference);
               p^.withreference:=nil;
            end;
       end;


end.
{
  $Log$
  Revision 1.67  2000-01-09 01:44:20  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.66  2000/01/07 01:14:21  peter
    * updated copyright to 2000

  Revision 1.65  2000/01/04 15:15:50  florian
    + added compiler switch $maxfpuregisters
    + fixed a small problem in secondvecn

  Revision 1.64  2000/01/03 17:10:39  jonas
    * fixed "quick hack, to overcome Delphi 2" :)

  Revision 1.63  1999/12/01 12:42:32  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.62  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.61  1999/11/15 21:54:38  peter
    * LOC_JUMP support for vecn

  Revision 1.60  1999/11/06 14:34:18  peter
    * truncated log to 20 revs

  Revision 1.59  1999/10/30 17:35:26  peter
    * fpc_freemem fpc_getmem new callings updated

  Revision 1.58  1999/09/17 17:14:02  peter
    * @procvar fixes for tp mode
    * @<id>:= gives now an error

  Revision 1.57  1999/09/14 07:59:46  florian
    * finally!? fixed
         with <function with result in temp> do
      My last and also Peter's fix before were wrong :(

  Revision 1.56  1999/09/13 20:49:41  florian
    * hopefully an error in Peter's previous commit fixed

  Revision 1.55  1999/09/10 15:42:50  peter
    * fixed with <calln> do
    * fixed finalize/initialize call for new/dispose

  Revision 1.54  1999/08/25 11:59:46  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.53  1999/08/23 23:49:21  pierre
   * hnewn location corrected

  Revision 1.52  1999/08/19 13:08:52  pierre
   * emit_??? used

  Revision 1.51  1999/08/16 23:20:28  peter
    * range check for array of const

  Revision 1.50  1999/08/14 00:36:05  peter
    * array constructor support

  Revision 1.49  1999/08/04 00:22:53  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.48  1999/08/03 22:02:47  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.47  1999/06/02 10:11:45  florian
    * make cycle fixed i.e. compilation with 0.99.10
    * some fixes for qword
    * start of register calling conventions

  Revision 1.46  1999/05/27 19:44:17  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.45  1999/05/23 18:42:04  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.44  1999/05/21 13:54:53  peter
    * NEWLAB for label as symbol

  Revision 1.43  1999/05/19 16:48:21  florian
    * tdef.typename: returns a now a proper type name for the most types

  Revision 1.42  1999/05/18 22:11:52  pierre
   * checkpointer code was wrong!

  Revision 1.41  1999/05/18 21:58:29  florian
    * fixed some bugs related to temp. ansistrings and functions results
      which return records/objects/arrays which need init/final.

  Revision 1.40  1999/05/18 14:15:26  peter
    * containsself fixes
    * checktypes()

}

