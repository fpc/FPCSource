{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
      cobjects,verbose,globals,systems,
      symtable,aasm,i386,types,
      cgi386,cgai386,temp_gen,tgeni386,hcodegen;

{*****************************************************************************
                             SecondLoadVMT
*****************************************************************************}

    procedure secondloadvmt(var p : ptree);
      begin
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(pai386,op_csymbol_reg(A_MOV,
            S_L,newcsymbol(pobjectdef(pclassrefdef(p^.resulttype)^.definition)^.vmt_mangledname,0),
            p^.location.register)));
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
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;
         p^.location.register:=p^.left^.location.register;
      end;


{*****************************************************************************
                             SecondDisposeN
*****************************************************************************}

    procedure secondhdisposen(var p : ptree);
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;
         clear_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 p^.location.reference.index:=getregister32;
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                   p^.left^.location.register,
                   p^.location.reference.index)));
              end;
            LOC_MEM,LOC_REFERENCE :
              begin
                 del_reference(p^.left^.location.reference);
                 p^.location.reference.index:=getregister32;
                 exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                   p^.location.reference.index)));
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
         { determines the size of the mem block }
         push_int(ppointerdef(p^.left^.resulttype)^.definition^.size);

         { push pointer adress }
         case p^.left^.location.loc of
            LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
              p^.left^.location.register)));
            LOC_REFERENCE:
              emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
         end;

         { call the mem handling procedures }
         case p^.treetype of
           simpledisposen:
             begin
                if ppointerdef(p^.left^.resulttype)^.definition^.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=stringdup(lab2str(ppointerdef(p^.left^.resulttype)^.definition^.get_rtti_label));
                     emitpushreferenceaddr(exprasmlist,r^);
                     { push pointer adress }
                     case p^.left^.location.loc of
                        LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
                          p^.left^.location.register)));
                        LOC_REFERENCE:
                          emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     end;
                     emitcall('FPC_FINALIZE',true);
                  end;
                emitcall('FPC_FREEMEM',true);
             end;
           simplenewn:
             begin
                emitcall('FPC_GETMEM',true);
                if ppointerdef(p^.left^.resulttype)^.definition^.needs_inittable then
                  begin
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=stringdup(lab2str(ppointerdef(p^.left^.resulttype)^.definition^.get_rtti_label));
                     emitpushreferenceaddr(exprasmlist,r^);
                     { push pointer adress }
                     case p^.left^.location.loc of
                        LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
                          p^.left^.location.register)));
                        LOC_REFERENCE:
                          emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     end;
                     emitcall('FPC_INITIALIZE',true);
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
         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         {@ on a procvar means returning an address to the procedure that
           is stored in it.}
         { yes but p^.left^.symtableentry can be nil
           for example on @self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (p^.left^.treetype=loadn) and
           assigned(p^.left^.symtableentry) and
           (p^.left^.symtableentry^.typ=varsym) and
           (pvarsym(p^.left^.symtableentry)^.definition^.deftype=procvardef) then
           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)))
         else
           exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)));
           { for use of other segments }
           if p^.left^.location.reference.segment<>R_DEFAULT_SEG then
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
         exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
         newreference(p^.left^.location.reference),
           p^.location.register)));
      end;


{*****************************************************************************
                             SecondDeRef
*****************************************************************************}

    procedure secondderef(var p : ptree);
      var
         hr : tregister;
      begin
         secondpass(p^.left);
         clear_reference(p^.location.reference);
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
                 exprasmlist^.concat(new(pai386,op_ref_reg(
                   A_MOV,S_L,newreference(p^.left^.location.reference),
                   hr)));
                 p^.location.reference.base:=hr;
              end;
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
           pobjectdef(p^.left^.resulttype)^.isclass then
           begin
             clear_reference(p^.location.reference);
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
                     exprasmlist^.concat(new(pai386,op_ref_reg(
                       A_MOV,S_L,newreference(p^.left^.location.reference),
                       hr)));
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
        pushed : boolean;
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
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,l2,ind)))
                 else
                   exprasmlist^.concat(new(pai386,op_const_reg(A_IMUL,S_L,l1,ind)));
              end;
            end;
          end;

      var
         extraoffset : longint;
         t   : ptree;
         hp  : preference;
         tai : Pai386;

      begin
         secondpass(p^.left);

         { we load the array reference to p^.location }

         { an ansistring needs to be dereferenced }
         if is_ansistring(p^.left^.resulttype) or
           is_widestring(p^.left^.resulttype) then
           begin
              reset_reference(p^.location.reference);
              p^.location.loc:=LOC_REFERENCE;
              del_reference(p^.left^.location.reference);
              p^.location.reference.base:=getregister32;
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                newreference(p^.left^.location.reference),
                p^.location.reference.base)));
              if is_ansistring(p^.left^.resulttype) then
                begin
                   { in ansistrings S[1] is pchar(S)[0] !! }
                   dec(p^.location.reference.offset);
                   { this is necessary for ansistrings with constant index }
                   dec(p^.left^.location.reference.offset);
                end
              else
                begin
                   { in widestrings S[1] is pwchar(S)[0] !! }
                   dec(p^.location.reference.offset,2);
                   { this is necessary for ansistrings with constant index }
                   dec(p^.left^.location.reference.offset,2);
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,
                     2,p^.location.reference.base)));
                end;
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
                   if not(is_open_array(p^.left^.resulttype)) then
                     begin
                        if (p^.right^.value>parraydef(p^.left^.resulttype)^.highrange) or
                           (p^.right^.value<parraydef(p^.left^.resulttype)^.lowrange) then
                          Message(parser_e_range_check_error);

                        dec(p^.left^.location.reference.offset,
                            get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
                     end
                   else
                     begin
                        { range checking for open arrays }
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
                             putnode(p^.right);
                             putnode(p^.right^.right);
                             p^.right:=t
                          end
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                             t:=p^.right^.right;
                             putnode(p^.right);
                             putnode(p^.right^.left);
                             p^.right:=t
                          end;
                     end
                   else if (p^.right^.treetype=subn) then
                     begin
                        if p^.right^.right^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.right^.value;
                             t:=p^.right^.left;
                             putnode(p^.right);
                             putnode(p^.right^.right);
                             p^.right:=t
                          end
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                             t:=p^.right^.right;
                             putnode(p^.right);
                             putnode(p^.right^.left);
                             p^.right:=t
                          end;
                     end;
                   inc(p^.location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if (p^.location.loc<>LOC_REFERENCE) and
                 (p^.location.loc<>LOC_MEM) then
                Message(cg_e_illegal_expression);
              pushed:=maybe_push(p^.right^.registers32,p);
              secondpass(p^.right);
              if pushed then restore(p);
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
                      exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.right^.location.resflags],S_B,reg32toreg8(ind))));
                      emit_reg_reg(A_MOVZX,S_BL,reg32toreg8(ind),ind);
                   end
                 else
                    begin
                       del_reference(p^.right^.location.reference);
                       ind:=getregister32;
                       { Booleans are stored in an 8 bit memory location, so
                         the use of MOVL is not correct }
                       case p^.right^.resulttype^.size of
                         1:
                           tai:=new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.right^.location.reference),ind));
                         2:
                           tai:=new(Pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.right^.location.reference),ind));
                         4:
                           tai:=new(Pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),ind));
                       end;
                       exprasmlist^.concat(tai);
                    end;
              end;
            { produce possible range check code: }
            if cs_check_range in aktlocalswitches then
              begin
                 if p^.left^.resulttype^.deftype=arraydef then
                   begin
                      hp:=new_reference(R_NO,0);
                      parraydef(p^.left^.resulttype)^.genrangecheck;
                      hp^.symbol:=stringdup('R_'+tostr(parraydef(p^.left^.resulttype)^.rangenr));
                      exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,ind,hp)));
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
                         2 : exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,1,p^.location.reference.index)));
                         4 : exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,2,p^.location.reference.index)));
                         8 : exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,3,p^.location.reference.index)));
                      end;
                      calc_emit_mul;
                      p^.location.reference.base:=p^.location.reference.index;
                      p^.location.reference.index:=ind;
                   end
                 else
                   begin
                      exprasmlist^.concat(new(pai386,op_ref_reg(
                        A_LEA,S_L,newreference(p^.location.reference),
                        p^.location.reference.index)));
                      ungetregister32(p^.location.reference.base);
                      { the symbol offset is loaded,               }
                      { so release the symbol name and set symbol  }
                      { to nil                                     }
                      stringdispose(p^.location.reference.symbol);
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
         clear_reference(p^.location.reference);
         if (p^.resulttype^.deftype=classrefdef) or
           ((p^.resulttype^.deftype=objectdef)
             and pobjectdef(p^.resulttype)^.isclass
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
        ref : treference;
        symtable : psymtable;
        i : longint;
      begin
         if assigned(p^.left) then
            begin
               secondpass(p^.left);
               ref.symbol:=nil;
               gettempofsizereference(4,ref);
               exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                 newreference(p^.left^.location.reference),R_EDI)));
               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                 R_EDI,newreference(ref))));
               del_reference(p^.left^.location.reference);
               { the offset relative to (%ebp) is only needed here! }
               symtable:=p^.withsymtable;
               for i:=1 to p^.tablecount do
                 begin
                    symtable^.datasize:=ref.offset;
                    symtable:=symtable^.next;
                 end;

               { p^.right can be optimize out !!! }
               if p^.right<>nil then
                 secondpass(p^.right);
               { clear some stuff }
               ungetiftemp(ref);
            end;
       end;


end.
{
  $Log$
  Revision 1.10  1998-09-14 10:43:52  peter
    * all internal RTL functions start with FPC_

  Revision 1.9  1998/09/03 16:03:15  florian
    + rtti generation
    * init table generation changed

  Revision 1.8  1998/08/23 21:04:34  florian
    + rtti generation for classes added
    + new/dispose do now also a call to INITIALIZE/FINALIZE, if necessaray

  Revision 1.7  1998/08/20 11:27:40  michael
  * Applied Peters Fix

  Revision 1.6  1998/08/10 14:49:49  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.5  1998/07/26 21:58:58  florian
   + better support for switch $H
   + index access to ansi strings added
   + assigment of data (records/arrays) containing ansi strings

  Revision 1.4  1998/07/24 22:16:55  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.3  1998/06/25 08:48:09  florian
    * first version of rtti support

  Revision 1.2  1998/06/08 13:13:35  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:13  peter
    * splitted cgi386

}

