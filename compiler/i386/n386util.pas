{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for the i386 code generator

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
unit n386util;

{$i defines.inc}

interface

    uses
      symtype,node,cpubase,cginfo;

    procedure location_force_reg(var l:tlocation;size:TCGSize;maybeconst:boolean);

    function maybe_push(needed : byte;p : tnode;isint64 : boolean) : boolean;
    function maybe_pushfpu(needed : byte;p : tnode) : boolean;
{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;
{$endif TEMPS_NOT_PUSH}
    procedure restore(p : tnode;isint64 : boolean);
{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : tnode;isint64 : boolean);
{$endif TEMPS_NOT_PUSH}
    procedure pushsetelement(p : tnode);
    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                              para_offset:longint;alignment : longint);
    procedure loadshortstring(source,dest : tnode);
    procedure loadlongstring(p:tbinarynode);
    procedure loadansi2short(source,dest : tnode);
    procedure loadwide2short(source,dest : tnode);
    procedure loadinterfacecom(p: tbinarynode);

    procedure emitoverflowcheck(p:tnode);
    procedure firstcomplex(p : tbinarynode);

implementation

    uses
       globtype,globals,systems,verbose,
       cutils,
       aasm,cpuasm,
       symconst,symdef,symsym,symtable,
{$ifdef GDB}
       gdb,
{$endif GDB}
       types,
       ncon,nld,
       pass_1,pass_2,
       cgbase,tgobj,
       cga,regvars,cgobj,cg64f32,rgobj,rgcpu,cgcpu;


    procedure location_force_reg(var l:tlocation;size:TCGSize;maybeconst:boolean);
      var
        hregister,
        hregisterhi : tregister;
        hl : tasmlabel;
      begin
        { release previous location before demanding a new register }
        if (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
         begin
           location_freetemp(exprasmlist,l);
           location_release(exprasmlist,l);
         end;
        { handle transformations to 64bit separate }
        if size in [OS_64,OS_S64] then
         begin
           if not (l.size in [OS_64,OS_S64]) then
            begin
              { load a smaller size to OS_64 }
              if l.loc=LOC_REGISTER then
               hregister:=Changeregsize(l.registerlow,S_L)
              else
               hregister:=rg.getregisterint(exprasmlist);
              { load value in low register }
              case l.loc of
                LOC_FLAGS :
                  cg.g_flags2reg(exprasmlist,l.resflags,hregister);
                LOC_JUMP :
                  begin
                    cg.a_label(exprasmlist,truelabel);
                    cg.a_load_const_reg(exprasmlist,OS_32,1,hregister);
                    getlabel(hl);
                    cg.a_jmp_cond(exprasmlist,OC_NONE,hl);
                    cg.a_label(exprasmlist,falselabel);
                    cg.a_load_const_reg(exprasmlist,OS_32,0,hregister);
                    cg.a_label(exprasmlist,hl);
                  end;
                else
                  cg.a_load_loc_reg(exprasmlist,l,hregister);
              end;
              { reset hi part, take care of the signed bit of the current value }
              hregisterhi:=rg.getregisterint(exprasmlist);
              if (size=OS_S64) and
                 (l.size in [OS_S8,OS_S16,OS_S32]) then
               begin
                 if l.loc=LOC_CONSTANT then
                  begin
                    if (longint(l.value)<0) then
                     cg.a_load_const_reg(exprasmlist,OS_32,$ffffffff,hregisterhi)
                    else
                     cg.a_load_const_reg(exprasmlist,OS_32,0,hregisterhi);
                  end
                 else
                  begin
                    cg.a_load_reg_reg(exprasmlist,OS_32,hregister,hregisterhi);
                    cg.a_op_const_reg(exprasmlist,OP_SAR,31,hregisterhi);
                  end;
               end
              else
               cg.a_load_const_reg(exprasmlist,OS_32,0,hregisterhi);
              location_reset(l,LOC_REGISTER,size);
              l.registerlow:=hregister;
              l.registerhigh:=hregisterhi;
            end
           else
            begin
              { 64bit to 64bit }
              if (l.loc=LOC_REGISTER) or
                 ((l.loc=LOC_CREGISTER) and maybeconst) then
               begin
                 hregister:=l.registerlow;
                 hregisterhi:=l.registerhigh;
               end
              else
               begin
                 hregister:=rg.getregisterint(exprasmlist);
                 hregisterhi:=rg.getregisterint(exprasmlist);
               end;
              { load value in new register }
              tcg64f32(cg).a_load64_loc_reg(exprasmlist,l,hregister,hregisterhi);
              location_reset(l,LOC_REGISTER,size);
              l.registerlow:=hregister;
              l.registerhigh:=hregisterhi;
            end;
         end
        else
         begin
           { transformations to 32bit or smaller }
           if l.loc=LOC_REGISTER then
            begin
              { if the previous was 64bit release the high register }
              if l.size in [OS_64,OS_S64] then
               begin
                 rg.ungetregisterint(exprasmlist,l.registerhigh);
                 l.registerhigh:=R_NO;
               end;
              hregister:=l.register;
            end
           else
            begin
              { get new register }
              if (l.loc=LOC_CREGISTER) and
                 maybeconst and
                 (TCGSize2Size[size]=TCGSize2Size[l.size]) then
               hregister:=l.register
              else
               hregister:=rg.getregisterint(exprasmlist);
            end;
{$ifdef i386}
           hregister:=Changeregsize(hregister,TCGSize2Opsize[size]);
{$endif i386}
           { load value in new register }
           case l.loc of
             LOC_FLAGS :
               cg.g_flags2reg(exprasmlist,l.resflags,hregister);
             LOC_JUMP :
               begin
                 cg.a_label(exprasmlist,truelabel);
                 cg.a_load_const_reg(exprasmlist,size,1,hregister);
                 getlabel(hl);
                 cg.a_jmp_cond(exprasmlist,OC_NONE,hl);
                 cg.a_label(exprasmlist,falselabel);
                 cg.a_load_const_reg(exprasmlist,size,0,hregister);
                 cg.a_label(exprasmlist,hl);
               end;
             else
               begin
                 { load_loc_reg can only handle size >= l.size, when the
                   new size is smaller then we need to adjust the size
                   of the orignal and maybe recalculate l.register for i386 }
                 if (TCGSize2Size[size]<TCGSize2Size[l.size]) then
                  begin
{$ifdef i386}
                    if (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                     l.register:=Changeregsize(l.register,TCGSize2Opsize[size]);
{$endif i386}
                    l.size:=size;
                  end;
                 cg.a_load_loc_reg(exprasmlist,l,hregister);
               end;
           end;
           location_reset(l,LOC_REGISTER,size);
           l.register:=hregister;
         end;
      end;


{*****************************************************************************
                           Emit Push Functions
*****************************************************************************}

    function maybe_push(needed : byte;p : tnode;isint64 : boolean) : boolean;
      var
         pushed : boolean;
         {hregister : tregister; }
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         if p.location.loc = LOC_CREGISTER then
           begin
             maybe_push := true;
             exit;
           end;
         if needed>rg.countunusedregsint then
           begin
              if (p.location.loc=LOC_REGISTER) then
                begin
                   if isint64 then
                     begin
{$ifdef TEMPS_NOT_PUSH}
                        tg.gettempofsizereference(exprasmlist,href,8);
                        p.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmList.concat(Taicpu.Op_reg(A_MOV,S_L,p.location.registerhigh,href));
                        href.offset:=href.offset-4;
{$else TEMPS_NOT_PUSH}
                        exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.location.registerhigh));
{$endif TEMPS_NOT_PUSH}
                        rg.ungetregisterint(exprasmlist,p.location.registerhigh);
                     end
{$ifdef TEMPS_NOT_PUSH}
                   else
                     begin
                        tg.gettempofsizereference(exprasmlist,href,4);
                        p.temp_offset:=href.offset;
                     end
{$endif TEMPS_NOT_PUSH}
                     ;
                   pushed:=true;
{$ifdef TEMPS_NOT_PUSH}
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,p.location.register,href));
{$else TEMPS_NOT_PUSH}
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.location.register));
{$endif TEMPS_NOT_PUSH}
                   rg.ungetregisterint(exprasmlist,p.location.register);
                end
              else if (p.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) and
                      ((p.location.reference.base<>R_NO) or
                       (p.location.reference.index<>R_NO)
                      ) then
                  begin
                     reference_release(exprasmlist,p.location.reference);
                     rg.getexplicitregisterint(exprasmlist,R_EDI);
                     emit_ref_reg(A_LEA,S_L,p.location.reference,R_EDI);
{$ifdef TEMPS_NOT_PUSH}
                     tg.gettempofsizereference(exprasmlist,href,4);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,href));
                     p.temp_offset:=href.offset;
{$else TEMPS_NOT_PUSH}
                     exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
{$endif TEMPS_NOT_PUSH}
                     rg.ungetregisterint(exprasmlist,R_EDI);
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;


     function maybe_pushfpu(needed : byte;p : tnode) : boolean;
       begin
         if needed>=maxfpuregs then
           begin
             if p.location.loc = LOC_FPUREGISTER then
               begin
                 emit_to_mem(p.location,p.resulttype.def);
                 maybe_pushfpu:=true;
               end
             else
               maybe_pushfpu:=false;
           end
         else
           maybe_pushfpu:=false;
       end;


{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;

      var
         pushed : boolean;
         href : treference;

      begin
         if needed>rg.unusedregsint then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64(p^.resulttype.def) then
                     begin
                        tg.gettempofsizereference(exprasmlist,href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmList.concat(Taicpu.Op_reg(A_MOV,S_L,p^.location.registerhigh,href));
                        href.offset:=href.offset-4;
                        rg.ungetregisterint(exprasmlist,p^.location.registerhigh);
                     end
                   else
                     begin
                        tg.gettempofsizereference(exprasmlist,href,4);
                        p^.temp_offset:=href.offset;
                     end;
                   pushed:=true;
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,p^.location.register,href));
                   rg.ungetregisterint(exprasmlist,p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     reference_release(p^.location.reference);
                     rg.getexplicitregisterint(exprasmlist,R_EDI);
                     emit_ref_reg(A_LEA,S_L,reference_copy(p^.location.reference),
                       R_EDI);
                     tg.gettempofsizereference(exprasmlist,href,4);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,href));
                     rg.ungetregisterint(exprasmlist,R_EDI);
                     p^.temp_offset:=href.offset;
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;
{$endif TEMPS_NOT_PUSH}


    procedure restore(p : tnode;isint64 : boolean);
      var
         hregister :  tregister;
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         if p.location.loc = LOC_CREGISTER then
           begin
             load_regvar_reg(exprasmlist,p.location.register);
             exit;
           end;
         hregister:=rg.getregisterint(exprasmlist);
{$ifdef TEMPS_NOT_PUSH}
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
{$else  TEMPS_NOT_PUSH}
         exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,hregister));
{$endif TEMPS_NOT_PUSH}
         if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p.location.register:=hregister;
              if isint64 then
                begin
                   p.location.registerhigh:=rg.getregisterint(exprasmlist);
{$ifdef TEMPS_NOT_PUSH}
                   href.offset:=p.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p.temp_offset;
{$else  TEMPS_NOT_PUSH}
                   exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,p.location.registerhigh));
{$endif TEMPS_NOT_PUSH}
                end;
           end
         else
           begin
              reference_reset(p.location.reference);
              { any reasons why this was moved into the index register ? }
              { normally usage of base register is much better (FK)      }
              p.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p.left
                because otherwise secondload fails !!!
              set_location(p.left^.location,p.location);}
           end;
{$ifdef TEMPS_NOT_PUSH}
         tg.ungetiftemp(exprasmlist,href);
{$endif TEMPS_NOT_PUSH}
      end;


{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : tnode;isint64 : boolean);
      var
         hregister :  tregister;
         href : treference;

      begin
         hregister:=rg.getregisterint(exprasmlist);
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
         if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p.location.register:=hregister;
              if isint64 then
                begin
                   p.location.registerhigh:=rg.getregisterint(exprasmlist);
                   href.offset:=p.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p.temp_offset;
                end;
           end
         else
           begin
              reset_reference(p.location.reference);
              p.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails PM
              set_location(p^.left^.location,p^.location);}
           end;
         tg.ungetiftemp(exprasmlist,href);
      end;
{$endif TEMPS_NOT_PUSH}


    procedure pushsetelement(p : tnode);
      begin
      { copy the element on the stack, slightly complicated }
        if p.nodetype=ordconstn then
         begin
           if aktalignment.paraalign=4 then
             exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,tordconstnode(p).value))
           else
             exprasmList.concat(Taicpu.Op_const(A_PUSH,S_W,tordconstnode(p).value));
         end
        else
         begin
           case p.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER :
               begin
                 if aktalignment.paraalign=4 then
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,changeregsize(p.location.register,S_W)))
                 else
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_W,changeregsize(p.location.register,S_L)));
                 rg.ungetregisterint(exprasmlist,p.location.register);
               end;
           else
             begin
               { you can't push more bytes than the size of the element, }
               { because this may cross a page boundary and you'll get a }
               { sigsegv (JM)                                            }
               emit_push_mem_size(p.location.reference,1);
               reference_release(exprasmlist,p.location.reference);
             end;
           end;
         end;
      end;

    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                                para_offset:longint;alignment : longint);
      var
        tempreference : treference;
        r : treference;
        opsize : topsize;
        hreg : tregister;
        size : longint;
        hlabel : tasmlabel;
        cgsize : tcgsize;
      begin
        case p.location.loc of
           LOC_REGISTER,
           LOC_CREGISTER:
             begin
               cgsize:=def_cgsize(p.resulttype.def);
               if cgsize in [OS_64,OS_S64] then
                begin
                  inc(pushedparasize,8);
                  if inlined then
                   begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     tcg64f32(cg).a_load64_loc_ref(exprasmlist,p.location,r);
                   end
                  else
                   tcg64f32(cg).a_param64_loc(exprasmlist,p.location,-1);
                end
               else
                begin
                  { save old register }
                  hreg:=p.location.register;
                  { update register to use to match alignment }
                  case cgsize of
                    OS_8,OS_S8 :
                      begin
                        if alignment=4 then
                         cgsize:=OS_32
                        else
                         cgsize:=OS_16;
                      end;
                    OS_16,OS_S16 :
                      begin
                        if alignment=4 then
                         cgsize:=OS_32;
                      end;
                  end;
{$ifdef i386}
                  p.location.register:=changeregsize(p.location.register,TCGSize2Opsize[cgsize]);
{$endif i386}
                  inc(pushedparasize,alignment);
                  if inlined then
                   begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     cg.a_load_loc_ref(exprasmlist,p.location,r);
                   end
                  else
                   cg.a_param_loc(exprasmlist,p.location,-1);
                  { restore old register }
                  p.location.register:=hreg;
                end;
               location_release(exprasmlist,p.location);
             end;
           LOC_CONSTANT :
             begin
               cgsize:=def_cgsize(p.resulttype.def);
               if cgsize in [OS_64,OS_S64] then
                begin
                  inc(pushedparasize,8);
                  if inlined then
                   begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     tcg64f32(cg).a_load64_loc_ref(exprasmlist,p.location,r);
                   end
                  else
                   tcg64f32(cg).a_param64_loc(exprasmlist,p.location,-1);
                end
               else
                begin
                  case cgsize of
                    OS_8,OS_S8 :
                      begin
                        if alignment=4 then
                         cgsize:=OS_32
                        else
                         cgsize:=OS_16
                      end;
                    OS_16,OS_S16 :
                      begin
                        if alignment=4 then
                         cgsize:=OS_32;
                      end;
                  end;
                  inc(pushedparasize,alignment);
                  if inlined then
                   begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     cg.a_load_loc_ref(exprasmlist,p.location,r);
                   end
                  else
                   cg.a_param_loc(exprasmlist,p.location,-1);
                end;
               location_release(exprasmlist,p.location);
             end;
           LOC_FPUREGISTER,
           LOC_CFPUREGISTER:
             begin
                size:=align(tfloatdef(p.resulttype.def).size,alignment);
                inc(pushedparasize,size);
                if not inlined then
                 emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmList.first=exprasmList.last) then
                  exprasmList.concat(Tai_force_line.Create);
{$endif GDB}

                { this is the easiest case for inlined !! }
                if inlined then
                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize)
                else
                 reference_reset_base(r,R_ESP,0);

                cg.a_loadfpu_reg_ref(exprasmlist,
                  def_cgsize(p.resulttype.def),p.location.register,r);
             end;
           LOC_REFERENCE,LOC_CREFERENCE:
             begin
                tempreference:=p.location.reference;
                reference_release(exprasmlist,p.location.reference);
                case p.resulttype.def.deftype of
                  enumdef,
                  orddef :
                    begin
                      case p.resulttype.def.size of
                       8 : begin
                             inc(pushedparasize,8);
                             if inlined then
                               begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 inc(tempreference.offset,4);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize+4);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                               end
                             else
                               begin
                                 inc(tempreference.offset,4);
                                 emit_push_mem(tempreference);
                                 dec(tempreference.offset,4);
                                 emit_push_mem(tempreference);
                               end;
                           end;
                       4 : begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                               end
                             else
                               emit_push_mem(tempreference);
                           end;
                     1,2 : begin
                             if alignment=4 then
                              begin
                                opsize:=S_L;
                                hreg:=R_EDI;
                                inc(pushedparasize,4);
                              end
                             else
                              begin
                                opsize:=S_W;
                                hreg:=R_DI;
                                inc(pushedparasize,2);
                              end;
                             if inlined then
                              begin
                                rg.getexplicitregisterint(exprasmlist,R_EDI);
                                emit_ref_reg(A_MOV,opsize,tempreference,hreg);
                                reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                                rg.ungetregisterint(exprasmlist,R_EDI);
                              end
                             else
                              emit_push_mem_size(tempreference,p.resulttype.def.size);
                           end;
                         else
                           internalerror(234231);
                      end;
                    end;
                  floatdef :
                    begin
                      case tfloatdef(p.resulttype.def).typ of
                        s32real :
                          begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                  rg.getexplicitregisterint(exprasmlist,R_EDI);
                                  emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                  reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                  rg.ungetregisterint(exprasmlist,R_EDI);
                               end
                             else
                               emit_push_mem(tempreference);
                          end;
                        s64real,
                        s64comp :
                          begin
                            inc(pushedparasize,4);
                            inc(tempreference.offset,4);
                            if inlined then
                              begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                            inc(pushedparasize,4);
                            dec(tempreference.offset,4);
                            if inlined then
                              begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                          end;
                        s80real :
                          begin
                            inc(pushedparasize,4);
                            if alignment=4 then
                              inc(tempreference.offset,8)
                            else
                              inc(tempreference.offset,6);
                            if inlined then
                              begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                            dec(tempreference.offset,4);
                            inc(pushedparasize,4);
                            if inlined then
                              begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                            if alignment=4 then
                              begin
                                opsize:=S_L;
                                hreg:=R_EDI;
                                inc(pushedparasize,4);
                                dec(tempreference.offset,4);
                              end
                            else
                              begin
                                opsize:=S_W;
                                hreg:=R_DI;
                                inc(pushedparasize,2);
                                dec(tempreference.offset,2);
                              end;
                            if inlined then
                              begin
                                 rg.getexplicitregisterint(exprasmlist,R_EDI);
                                 emit_ref_reg(A_MOV,opsize,tempreference,hreg);
                                 reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                                 rg.ungetregisterint(exprasmlist,R_EDI);
                              end
                            else
                              exprasmList.concat(Taicpu.Op_ref(A_PUSH,opsize,tempreference));
                        end;
                      end;
                    end;
                  pointerdef,
                  procvardef,
                  classrefdef:
                    begin
                       inc(pushedparasize,4);
                       if inlined then
                         begin
                            rg.getexplicitregisterint(exprasmlist,R_EDI);
                            emit_ref_reg(A_MOV,S_L,tempreference,R_EDI);
                            reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                            rg.ungetregisterint(exprasmlist,R_EDI);
                         end
                       else
                         emit_push_mem(tempreference);
                    end;
                  arraydef,
                  recorddef,
                  stringdef,
                  setdef,
                  objectdef :
                    begin
                       { even some structured types are 32 bit }
                       if is_widestring(p.resulttype.def) or
                          is_ansistring(p.resulttype.def) or
                          is_smallset(p.resulttype.def) or
                          ((p.resulttype.def.deftype in [recorddef,arraydef]) and
                           (
                            (p.resulttype.def.deftype<>arraydef) or not
                            (tarraydef(p.resulttype.def).IsConstructor or
                             tarraydef(p.resulttype.def).isArrayOfConst or
                             is_open_array(p.resulttype.def))
                           ) and
                           (p.resulttype.def.size<=4)
                          ) or
                          is_class(p.resulttype.def) or
                          is_interface(p.resulttype.def) then
                         begin
                            if (p.resulttype.def.size>2) or
                               ((alignment=4) and (p.resulttype.def.size>0)) then
                              begin
                                inc(pushedparasize,4);
                                if inlined then
                                  begin
                                    reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                    concatcopy(tempreference,r,4,false,false);
                                  end
                                else
                                  emit_push_mem(tempreference);
                              end
                            else
                              begin
                                if p.resulttype.def.size>0 then
                                  begin
                                    inc(pushedparasize,2);
                                    if inlined then
                                      begin
                                        reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                                        concatcopy(tempreference,r,2,false,false);
                                      end
                                    else
                                      exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_W,tempreference));
                                  end;
                              end;
                         end
                       { call by value open array ? }
                       else if is_cdecl then
                         begin
                           { push on stack }
                           size:=align(p.resulttype.def.size,alignment);
                           inc(pushedparasize,size);
                           emit_const_reg(A_SUB,S_L,size,R_ESP);
                           reference_reset_base(r,R_ESP,0);
                           concatcopy(tempreference,r,size,false,false);
                         end
                       else
                         internalerror(8954);
                    end;
                  else
                    CGMessage(cg_e_illegal_expression);
                end;
             end;
           LOC_JUMP:
             begin
                getlabel(hlabel);
                if alignment=4 then
                 begin
                   opsize:=S_L;
                   inc(pushedparasize,4);
                 end
                else
                 begin
                   opsize:=S_W;
                   inc(pushedparasize,2);
                 end;
                emitlab(truelabel);
                if inlined then
                  begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     emit_const_ref(A_MOV,opsize,1,r);
                  end
                else
                  exprasmList.concat(Taicpu.Op_const(A_PUSH,opsize,1));
                emitjmp(C_None,hlabel);
                emitlab(falselabel);
                if inlined then
                  begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     emit_const_ref(A_MOV,opsize,0,r);
                  end
                else
                  exprasmList.concat(Taicpu.Op_const(A_PUSH,opsize,0));
                emitlab(hlabel);
             end;
           LOC_FLAGS:
             begin
                if alignment=4 then
                 begin
                   opsize:=S_L;
                   hreg:=R_EAX;
                   inc(pushedparasize,4);
                 end
                else
                 begin
                   opsize:=S_W;
                   hreg:=R_AX;
                   inc(pushedparasize,2);
                 end;
                if not(R_EAX in rg.unusedregsint) then
                  begin
                    rg.getexplicitregisterint(exprasmlist,R_EDI);
                    emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                  end;
                cg.g_flags2reg(exprasmlist,p.location.resflags,hreg);
                if inlined then
                  begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                  end
                else
                  exprasmList.concat(Taicpu.Op_reg(A_PUSH,opsize,hreg));
                if not(R_EAX in rg.unusedregsint) then
                  begin
                    emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
                    rg.ungetregisterint(exprasmlist,R_EDI);
                  end;
             end;
{$ifdef SUPPORT_MMX}
           LOC_MMXREGISTER,
           LOC_CMMXREGISTER:
             begin
                inc(pushedparasize,8); { was missing !!! (PM) }
                emit_const_reg(
                  A_SUB,S_L,8,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmList.first=exprasmList.last) then
                  exprasmList.concat(Tai_force_line.Create);
{$endif GDB}
                if inlined then
                  begin
                     reference_reset_base(r,procinfo^.framepointer,para_offset-pushedparasize);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,
                       p.location.register,r));
                  end
                else
                   begin
                      reference_reset_base(r,R_ESP,0);
                      exprasmList.concat(Taicpu.Op_reg_ref(
                        A_MOVQ,S_NO,p.location.register,r));
                   end;
             end;
{$endif SUPPORT_MMX}
        end;
      end;

{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    { produces if necessary overflowcode }
    procedure emitoverflowcheck(p:tnode);
      var
         hl : tasmlabel;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         getlabel(hl);
         if not ((p.resulttype.def.deftype=pointerdef) or
                ((p.resulttype.def.deftype=orddef) and
                 (torddef(p.resulttype.def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                                  bool8bit,bool16bit,bool32bit]))) then
           emitjmp(C_NO,hl)
         else
           emitjmp(C_NB,hl);
         emitcall('FPC_OVERFLOW');
         emitlab(hl);
      end;

   { DO NOT RELY on the fact that the tnode is not yet swaped
     because of inlining code PM }
    procedure firstcomplex(p : tbinarynode);
      var
         hp : tnode;
      begin
         { always calculate boolean AND and OR from left to right }
         if (p.nodetype in [orn,andn]) and
            (p.left.resulttype.def.deftype=orddef) and
            (torddef(p.left.resulttype.def).typ in [bool8bit,bool16bit,bool32bit]) then
           begin
             { p.swaped:=false}
             if nf_swaped in p.flags then
               internalerror(234234);
           end
         else
           if (((p.location.loc=LOC_FPUREGISTER) and
                (p.right.registersfpu > p.left.registersfpu)) or
               ((((p.left.registersfpu = 0) and
                  (p.right.registersfpu = 0)) or
                 (p.location.loc<>LOC_FPUREGISTER)) and
                (p.left.registers32<p.right.registers32))) and
           { the following check is appropriate, because all }
           { 4 registers are rarely used and it is thereby   }
           { achieved that the extra code is being dropped   }
           { by exchanging not commutative operators     }
               (p.right.registers32<=4) then
            begin
              hp:=p.left;
              p.left:=p.right;
              p.right:=hp;
              if nf_swaped in p.flags then
                exclude(p.flags,nf_swaped)
              else
                include(p.flags,nf_swaped);
            end;
         {else
           p.swaped:=false; do not modify }
      end;

{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure push_shortstring_length(p:tnode);
      var
        hightree : tnode;
        srsym    : tsym;
      begin
        if is_open_string(p.resulttype.def) then
         begin
           srsym:=searchsymonlyin(tloadnode(p).symtable,'high'+tvarsym(tloadnode(p).symtableentry).name);
           hightree:=cloadnode.create(tvarsym(srsym),tloadnode(p).symtable);
           firstpass(hightree);
           secondpass(hightree);
           push_value_para(hightree,false,false,0,4);
           hightree.free;
           hightree:=nil;
         end
        else
         begin
           push_int(tstringdef(p.resulttype.def).len);
         end;
      end;

{*****************************************************************************
                           String functions
*****************************************************************************}

    procedure loadshortstring(source,dest : tnode);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
        href: treference;
      begin
         case source.resulttype.def.deftype of
            stringdef:
              begin
                 if (source.nodetype=stringconstn) and
                   (str_length(source)=0) then
                   emit_const_ref(A_MOV,S_B,0,dest.location.reference)
                 else
                   begin
                     emitpushreferenceaddr(dest.location.reference);
                     emitpushreferenceaddr(source.location.reference);
                     push_shortstring_length(dest);
                     emitcall('FPC_SHORTSTR_COPY');
                     maybe_loadself;
                   end;
              end;
            orddef:
              begin
                 if source.nodetype=ordconstn then
                   emit_const_ref(
                      A_MOV,S_W,tordconstnode(source).value*256+1,dest.location.reference)
                 else
                   begin
                      if (source.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                        begin
                           href := dest.location.reference;
                           emit_const_ref(A_MOV,S_B,1,href);
                           inc(href.offset,1);
                           emit_reg_ref(A_MOV,S_B,changeregsize(source.location.register,S_B),href);
                        end
                      else
                      { not so elegant (goes better with extra register    }
                        begin
                           { not "movl", because then we may read past the }
                           { end of the heap! "movw" would be ok too, but  }
                           { I don't think that would be faster (JM)       }
                           rg.getexplicitregisterint(exprasmlist,R_EDI);
                           emit_ref_reg(A_MOVZX,S_BL,source.location.reference,R_EDI);
                           emit_const_reg(A_SHL,S_L,8,R_EDI);
                           emit_const_reg(A_OR,S_L,1,R_EDI);
                           emit_reg_ref(A_MOV,S_W,R_DI,dest.location.reference);
                           rg.ungetregisterint(exprasmlist,R_EDI);
                        end;
                      location_release(exprasmlist,source.location);
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;

    procedure loadlongstring(p:tbinarynode);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
         r : treference;

      begin
         case p.right.resulttype.def.deftype of
            stringdef:
              begin
                 if (p.right.nodetype=stringconstn) and
                   (str_length(p.right)=0) then
                   emit_const_ref(A_MOV,S_L,0,p.left.location.reference)
                 else
                   begin
                     emitpushreferenceaddr(p.left.location.reference);
                     emitpushreferenceaddr(p.right.location.reference);
                     push_shortstring_length(p.left);
                     emitcall('FPC_LONGSTR_COPY');
                     maybe_loadself;
                   end;
              end;
            orddef:
              begin
                 emit_const_ref(A_MOV,S_L,1,p.left.location.reference);

                 r:=p.left.location.reference;
                 inc(r.offset,4);

                 if p.right.nodetype=ordconstn then
                   emit_const_ref(A_MOV,S_B,tordconstnode(p.right).value,r)
                 else
                   begin
                      case p.right.location.loc of
                         LOC_REGISTER,LOC_CREGISTER:
                           emit_reg_ref(A_MOV,S_B,p.right.location.register,r);
                         LOC_CREFERENCE,LOC_REFERENCE:
                           begin
                              if not(R_EAX in rg.unusedregsint) then
                                emit_reg(A_PUSH,S_L,R_EAX);
                              emit_ref_reg(A_MOV,S_B,p.right.location.reference,R_AL);
                              emit_reg_ref(A_MOV,S_B,R_AL,r);
                              if not(R_EAX in rg.unusedregsint) then
                                emit_reg(A_POP,S_L,R_EAX);
                           end
                         else
                           internalerror(20799);
                        end;
                        location_release(exprasmlist,p.right.location);
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;


    procedure loadansi2short(source,dest : tnode);
      var
         pushed : tpushedsaved;
         regs_to_push: tregisterset;
      begin
         { Find out which registers have to be pushed (JM) }
         regs_to_push := all_registers;
         remove_non_regvars_from_loc(source.location,regs_to_push);
         { Push them (JM) }
         rg.saveusedregisters(exprasmlist,pushed,regs_to_push);
         location_freetemp(exprasmlist,source.location);
         location_release(exprasmlist,source.location);
         cg.a_param_loc(exprasmlist,source.location,1);
         push_shortstring_length(dest);
         emitpushreferenceaddr(dest.location.reference);
         rg.saveregvars(exprasmlist,all_registers);
         emitcall('FPC_ANSISTR_TO_SHORTSTR');
         rg.restoreusedregisters(exprasmlist,pushed);
         maybe_loadself;
      end;


    procedure loadwide2short(source,dest : tnode);
      var
         pushed : tpushedsaved;
         regs_to_push: tregisterset;
      begin
         { Find out which registers have to be pushed (JM) }
         regs_to_push := all_registers;
         remove_non_regvars_from_loc(source.location,regs_to_push);
         { Push them (JM) }
         rg.saveusedregisters(exprasmlist,pushed,regs_to_push);
         location_freetemp(exprasmlist,source.location);
         location_release(exprasmlist,source.location);
         cg.a_param_loc(exprasmlist,source.location,1);
         push_shortstring_length(dest);
         emitpushreferenceaddr(dest.location.reference);
         rg.saveregvars(exprasmlist,all_registers);
         emitcall('FPC_WIDESTR_TO_SHORTSTR');
         rg.restoreusedregisters(exprasmlist,pushed);
         maybe_loadself;
      end;


    procedure loadinterfacecom(p: tbinarynode);
    {
      copies an com interface from n.right to n.left, we
      assume, that both sides are com interface, firstassignement have
      to take care of that, an com interface can't be a register variable
    }
      var
         pushed : tpushedsaved;
         ungettemp : boolean;
      begin
         { before pushing any parameter, we have to save all used      }
         { registers, but before that we have to release the       }
         { registers of that node to save uneccessary pushed       }
         { so be careful, if you think you can optimize that code (FK) }

         { nevertheless, this has to be changed, because otherwise the }
         { register is released before it's contents are pushed ->     }
         { problems with the optimizer (JM)                         }
         reference_release(exprasmlist,p.left.location.reference);
         ungettemp:=false;
         case p.right.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 rg.saveusedregisters(exprasmlist,pushed, all_registers - [p.right.location.register]);
                 exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.right.location.register));
              end;
            LOC_REFERENCE,LOC_CREFERENCE:
              begin
                 rg.saveusedregisters(exprasmlist,pushed, all_registers
                   - [p.right.location.reference.base]
                   - [p.right.location.reference.index]);
                 emit_push_mem(p.right.location.reference);
                 ungettemp:=true;
              end;
         end;
         location_release(exprasmlist,p.right.location);
         emitpushreferenceaddr(p.left.location.reference);
         location_release(exprasmlist,p.left.location);
         rg.saveregvars(exprasmlist,all_registers);
         emitcall('FPC_INTF_ASSIGN');
         maybe_loadself;
         rg.restoreusedregisters(exprasmlist,pushed);
         if ungettemp then
           location_release(exprasmlist,p.right.location);
      end;



end.
{
  $Log$
  Revision 1.31  2002-04-15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.30  2002/04/04 19:06:12  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.29  2002/04/04 07:56:15  michael
  * Patch from peter to fix go32v2 cycle

  Revision 1.28  2002/04/02 17:11:37  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.27  2002/03/31 20:26:40  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.26  2002/03/04 19:10:14  peter
    * removed compiler warnings

  Revision 1.25  2001/12/30 17:24:47  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.24  2001/12/03 21:48:43  peter
    * freemem change to value parameter
    * torddef low/high range changed to int64

  Revision 1.23  2001/12/02 16:19:17  jonas
    * less unnecessary regvar loading with if-statements

  Revision 1.22  2001/10/12 13:51:52  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.21  2001/09/17 21:29:14  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.20  2001/08/26 13:37:01  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.19  2001/08/24 12:22:14  jonas
    * fixed memory leak with coping of array-of-consts as valuepara

  Revision 1.18  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.17  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.16  2001/04/18 22:02:03  peter
    * registration of targets and assemblers

  Revision 1.15  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.14  2001/04/02 21:20:39  peter
    * resulttype rewrite

  Revision 1.13  2001/03/11 22:58:52  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.12  2001/03/04 10:26:56  jonas
    * new rangecheck code now handles conversion between signed and cardinal types correctly

  Revision 1.11  2001/03/03 12:41:22  jonas
    * simplified and optimized range checking code, FPC_BOUNDCHECK is no longer necessary

  Revision 1.10  2000/12/31 11:02:12  jonas
    * optimized loadshortstring a bit

  Revision 1.9  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.8  2000/12/11 19:10:19  jonas
    * fixed web bug 1144
    + implemented range checking for 64bit types

  Revision 1.7  2000/12/07 17:19:46  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.6  2000/12/05 11:44:34  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.5  2000/11/29 00:30:49  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/11/13 14:47:46  jonas
    * support for range checking when converting from 64bit to something
      smaller (32bit, 16bit, 8bit)
    * fixed range checking between longint/cardinal and for array indexing
      with cardinal (values > $7fffffff were considered negative)

  Revision 1.3  2000/11/04 14:25:25  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:57  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:32  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.3  2000/10/14 21:52:54  peter
    * fixed memory leaks

  Revision 1.2  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.1  2000/10/01 19:58:40  peter
    * new file

}
