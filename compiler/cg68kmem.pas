{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for in memory related nodes

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
unit cg68kmem;
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
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cga68k,tgen68k;


{*****************************************************************************
                             SecondLoadVMT
*****************************************************************************}

    procedure secondloadvmt(var p : ptree);
      begin
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(paicpu,op_csymbol_reg(A_MOVE,
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

              { code copied from simplenewdispose PM }
              { determines the size of the mem block }
              push_int(ppointerdef(p^.resulttype)^.definition^.size);

              gettempofsizereference(target_os.size_of_pointer,p^.location.reference);
              emitpushreferenceaddr(exprasmlist,p^.location.reference);

              emitcall('FPC_GETMEM',true);
{!!!!!!!}
(*              if ppointerdef(p^.resulttype)^.definition^.needs_inittable then
                begin
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=stringdup(lab2str(ppointerdef(p^.left^.resulttype)^.definition^.get_inittable_label));
                   emitpushreferenceaddr(exprasmlist,r^);
                   { push pointer adress }
                   emitpushreferenceaddr(exprasmlist,p^.location.reference);
                   stringdispose(r^.symbol);
                   dispose(r);
                   emitcall('FPC_INITIALIZE',true);
                end; *)
              popusedregisters(pushed);
              { may be load ESI }
              maybe_loada5;
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
         clear_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER : begin
                               p^.location.reference.base:=getaddressreg;
                               exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
                                 p^.left^.location.register,
                                 p^.location.reference.base)));
                            end;
            LOC_MEM,LOC_REFERENCE :
                            begin
                               del_reference(p^.left^.location.reference);
                               p^.location.reference.base:=getaddressreg;
                               exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                                 p^.location.reference.base)));
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

         pushusedregisters(pushed,$ffff);
         { determines the size of the mem block }
         push_int(ppointerdef(p^.left^.resulttype)^.definition^.size);

         { push pointer adress }
         case p^.left^.location.loc of
            LOC_CREGISTER : exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
              p^.left^.location.register,R_SPPUSH)));
            LOC_REFERENCE:
              emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);

         end;

         { call the mem handling procedures }
         case p^.treetype of
           simpledisposen:
             begin
                if ppointerdef(p^.left^.resulttype)^.definition^.needs_inittable then
                  begin
{!!!!!!!}

(*                     new(r);
                     reset_reference(r^);
                     r^.symbol:=stringdup(lab2str(ppointerdef(p^.left^.resulttype)^.definition^.get_rtti_label));
                     emitpushreferenceaddr(exprasmlist,r^);
                     { push pointer adress }
                     case p^.left^.location.loc of
                        LOC_CREGISTER : exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,
                          p^.left^.location.register)));
                        LOC_REFERENCE:
                          emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     end;
                     emitcall('FPC_FINALIZE',true); *)
                  end;
                emitcall('FPC_FREEMEM',true);
             end;
           simplenewn:
             begin
                emitcall('FPC_GETMEM',true);
                if ppointerdef(p^.left^.resulttype)^.definition^.needs_inittable then
                  begin
{!!!!!!!}

(*                     new(r);
                     reset_reference(r^);
                     r^.symbol:=stringdup(lab2str(ppointerdef(p^.left^.resulttype)^.definition^.get_rtti_label));
                     emitpushreferenceaddr(exprasmlist,r^);
                     { push pointer adress }
                     case p^.left^.location.loc of
                        LOC_CREGISTER : exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,
                          p^.left^.location.register)));
                        LOC_REFERENCE:
                          emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                     end;
                     emitcall('FPC_INITIALIZE',true); *)
                  end;
             end;
         end;
         popusedregisters(pushed);
         { may be load ESI }
         maybe_loada5;
      end;


{*****************************************************************************
                             SecondAddr
*****************************************************************************}

    procedure secondaddr(var p : ptree);
      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         {@ on a procvar means returning an address to the procedure that
          is stored in it.}
       { yes but p^.left^.symtableentry can be nil
       for example on @self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (p^.left^.treetype=loadn) and
          assigned(p^.left^.symtableentry) and
            (p^.left^.symtableentry^.typ=varsym) and
          (Pvarsym(p^.left^.symtableentry)^.definition^.deftype=
           procvardef) then
            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)))
         else
           begin
            exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,
             newreference(p^.left^.location.reference),R_A0)));
            exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
             R_A0,p^.location.register)));
           end;
         { for use of other segments }
         { if p^.left^.location.reference.segment<>R_DEFAULT_SEG then
             p^.location.segment:=p^.left^.location.reference.segment;
         }
         del_reference(p^.left^.location.reference);
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
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,
          newreference(p^.left^.location.reference),R_A0)));
         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
          R_A0,p^.location.register)));
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
            LOC_REGISTER : Begin
                             hr := getaddressreg;
                             emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                             p^.location.reference.base:=hr;
                             ungetregister(p^.left^.location.register);
                           end;
            LOC_CREGISTER : begin
                               { ... and reserve one for the pointer }
                               hr:=getaddressreg;
                               emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                                      p^.location.reference.base:=hr;
                               { LOC_REGISTER indicates that this is a
                               variable register which should not be freed. }
{                               ungetregister(p^.left^.location.register); }
                            end;
            else
              begin
                 { free register }
                 del_reference(p^.left^.location.reference);

                 { ...and reserve one for the pointer }
                 hr:=getaddressreg;
                 exprasmlist^.concat(new(paicpu,op_ref_reg(
                   A_MOVE,S_L,newreference(p^.left^.location.reference),
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
       hr: tregister;

      begin

         secondpass(p^.left);

         if codegenerror then
           exit;
         { classes must be dereferenced implicit }
         if (p^.left^.resulttype^.deftype=objectdef) and
           pobjectdef(p^.left^.resulttype)^.is_class then
           begin
             clear_reference(p^.location.reference);
             case p^.left^.location.loc of
                LOC_REGISTER:
                  begin
                     { move it to an address register...}
                     hr:=getaddressreg;
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                     p^.location.reference.base:=hr;
                     { free register }
                     ungetregister(p^.left^.location.register);
                  end;
                LOC_CREGISTER:
                  begin
                     { ... and reserve one for the pointer }
                     hr:=getaddressreg;
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                       p^.location.reference.base:=hr;
                  end;
                else
                  begin
                     { free register }
                     del_reference(p^.left^.location.reference);

                     { ... and reserve one for the pointer }
                     hr:=getaddressreg;
                     exprasmlist^.concat(new(paicpu,op_ref_reg(
                       A_MOVE,S_L,newreference(p^.left^.location.reference),
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

    { used D0, D1 as scratch (ok) }
    { arrays ...                  }
    { Sets up the array and string }
    { references .                 }
    procedure secondvecn(var p : ptree);

      var
         pushed : boolean;
         ind : tregister;
         _p : ptree;

      procedure calc_emit_mul;

        var
           l1,l2 : longint;

        begin
           l1:=p^.resulttype^.size;
           case l1 of
              1     : p^.location.reference.scalefactor:=l1;
              2 : exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,1,ind)));
              4 : exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,2,ind)));
              8 : exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,3,ind)));
           else
             begin
               if ispowerof2(l1,l2) then
                 exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,l2,ind)))
                   else
                 begin
                   { use normal MC68000 signed multiply }
                   if (l1 >= -32768) and (l1 <= 32767) then
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_MULS,S_W,l1,ind)))
                   else
                   { use long MC68020 long multiply }
                   if (aktoptprocessor = MC68020) then
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_MULS,S_L,l1,ind)))
                   else
                   { MC68000 long multiply }
                     begin
                       exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,l1,R_D0)));
                       exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,ind,R_D1)));
                       emitcall('FPC_LONGMUL',true);
                       exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D0,ind)));
                     end;
                 end;
             end; { else case }
            end; { end case }
        end; { calc_emit_mul }

      var
       extraoffset : longint;
         t : ptree;
         hp : preference;
         tai:paicpu;
       reg: tregister;

      begin
         secondpass(p^.left);
         { RESULT IS IN p^.location.reference }
         set_location(p^.location,p^.left^.location);

         { offset can only differ from 0 if arraydef }
         if p^.left^.resulttype^.deftype=arraydef then
           dec(p^.location.reference.offset,
             p^.resulttype^.size*
             parraydef(p^.left^.resulttype)^.lowrange);

         if p^.right^.treetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   if not(is_open_array(p^.left^.resulttype)) then
                     begin
                        if (p^.right^.value>parraydef(p^.left^.resulttype)^.highrange) or
                           (p^.right^.value<parraydef(p^.left^.resulttype)^.lowrange) then
                          CGMessage(parser_e_range_check_error);

                        dec(p^.left^.location.reference.offset,
                          p^.resulttype^.size*parraydef(p^.left^.resulttype)^.lowrange);
                     end
                   else
                     begin
                        { range checking for open arrays }
                     end;
                end;
              inc(p^.left^.location.reference.offset,
                p^.right^.value*p^.resulttype^.size);
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
                     p^.resulttype^.size*extraoffset);
                end;
              { calculate from left to right }
              if (p^.location.loc<>LOC_REFERENCE) and
                 (p^.location.loc<>LOC_MEM) then
                CGMessage(cg_e_illegal_expression);

              pushed:=maybe_push(p^.right^.registers32,p);
              secondpass(p^.right);
              if pushed then restore(p);
                 case p^.right^.location.loc of
                LOC_REGISTER : begin
                                 ind:=p^.right^.location.register;
                                 case p^.right^.resulttype^.size of
                                 1: exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                                      $ff,ind)));
                                 2: exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                                      $ffff,ind)));
                                 end;
                               end;

                LOC_CREGISTER : begin
                                   ind:=getregister32;
                                   emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,ind);
                                   case p^.right^.resulttype^.size of
                                   1: exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                                      $ff,ind)));
                                   2: exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                                      $ffff,ind)));
                                   end;
                                end;
                   LOC_FLAGS:
                     begin
                        ind:=getregister32;
                        exprasmlist^.concat(new(paicpu,op_reg(flag_2_set[p^.right^.location.resflags],S_B,ind)));
                        exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ff,ind)));
                     end
                else { else outer case }
                   begin
                      del_reference(p^.right^.location.reference);
                           ind:=getregister32;

                      exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                        newreference(p^.right^.location.reference),ind)));

                           {Booleans are stored in an 8 bit memory location, so
                           the use of MOVL is not correct.}
                      case p^.right^.resulttype^.size of
                        1: exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                          $ff,ind)));
                        2: exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                          $ffff,ind)));
                      end; { end case }
                end; { end else begin }
           end;

         { produce possible range check code: }
         if cs_check_range in aktlocalswitches  then
           begin
              if p^.left^.resulttype^.deftype=arraydef then
                begin
                   new(hp);
                   reset_reference(hp^);
                   parraydef(p^.left^.resulttype)^.genrangecheck;
                   hp^.symbol:=stringdup(parraydef(p^.left^.resulttype)^.getrangecheckstring);
                   emit_bounds_check(hp^,ind);
                end;
           end;

         { ------------------------ HANDLE INDEXING ----------------------- }
         { In Motorola 680x0 mode, displacement can only be of 64K max.     }
         { Therefore instead of doing a direct displacement, we must first  }
         { load the new address into an address register. Therefore the     }
         { symbol is not used.                                              }
         if assigned(p^.location.reference.symbol) then
           begin
              if p^.location.reference.base <> R_NO then
                CGMessage(cg_f_secondvecn_base_defined_twice);
              p^.location.reference.base:=getaddressreg;
              exprasmlist^.concat(new(paicpu,op_csymbol_reg(A_LEA,S_L,newcsymbol(p^.location.reference.symbol^,0),
                p^.location.reference.base)));
              stringdispose(p^.location.reference.symbol);
           end;

         if (p^.location.reference.index=R_NO) then
           begin
              p^.location.reference.index:=ind;
              calc_emit_mul;
              { here we must check for the offset      }
              { and if out of bounds for the motorola  }
              { eg: out of signed d8 then reload index }
              { with correct value.                    }
              if p^.location.reference.offset > 127 then
                begin
                   exprasmlist^.concat(new(paicpu,op_const_reg(A_ADD,S_L,p^.location.reference.offset,ind)));
                   p^.location.reference.offset := 0;
                end
              else if p^.location.reference.offset < -128 then
                begin
                   exprasmlist^.concat(new(paicpu,op_const_reg(A_SUB,S_L,-p^.location.reference.offset,ind)));
                   p^.location.reference.offset := 0;
                end;
           end
         { if no index then allways get an address register !! PM }
         else if p^.location.reference.base=R_NO then
           begin
              case p^.location.reference.scalefactor of
                  2 : exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,1,p^.location.reference.index)));
                  4 : exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,2,p^.location.reference.index)));
                  8 : exprasmlist^.concat(new(paicpu,op_const_reg(A_LSL,S_L,3,p^.location.reference.index)));
                end;
              calc_emit_mul;

              { we must use address register to put index in base }
              { compare with cgi386.pas                           }

              reg := getaddressreg;
              p^.location.reference.base := reg;

              emit_reg_reg(A_MOVE,S_L,p^.location.reference.index,reg);
              ungetregister(p^.location.reference.index);

              p^.location.reference.index:=ind;
           end
         else
           begin
              reg := getaddressreg;
              exprasmlist^.concat(new(paicpu,op_ref_reg(
                A_LEA,S_L,newreference(p^.location.reference),
                reg)));

              ungetregister(p^.location.reference.base);
              { the symbol offset is loaded,               }
              { so release the symbol name and set symbol  }
              { to nil                                     }
              stringdispose(p^.location.reference.symbol);
              p^.location.reference.offset:=0;
              calc_emit_mul;
              p^.location.reference.base:=reg;
              ungetregister32(p^.location.reference.index);
              p^.location.reference.index:=ind;
         end;
         end;
      end;


{*****************************************************************************
                               SecondSelfN
*****************************************************************************}

    procedure secondselfn(var p : ptree);
      begin
         clear_reference(p^.location.reference);
         p^.location.reference.base:=R_A5;
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
               exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,
                 newreference(p^.left^.location.reference),R_A0)));
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,
                 R_A0,newreference(ref))));
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
  Revision 1.2  2000-07-13 11:32:37  michael
  + removed logs

}
