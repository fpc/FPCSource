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

    function maybe_push(needed : byte;p : tnode;isint64 : boolean) : boolean;
    function maybe_pushfpu(needed : byte;p : tnode) : boolean;
{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;
{$endif TEMPS_NOT_PUSH}
    procedure restore(p : tnode;isint64 : boolean);
{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : tnode;isint64 : boolean);
{$endif TEMPS_NOT_PUSH}
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: tregisterset);
    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                              para_offset:longint;alignment : longint);

    procedure emitoverflowcheck(p:tnode);
    procedure firstcomplex(p : tbinarynode);


implementation

    uses
       globtype,globals,systems,verbose,
       cutils,
       aasm,cpuasm,
       symconst,symdef,
{$ifdef GDB}
       gdb,
{$endif GDB}
       types,
       ncgutil,ncon,nld,
       cgbase,tgobj,
       cga,regvars,cgobj,cg64f32,rgobj,rgcpu,cgcpu;


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
                 location_force_mem(exprasmlist,p.location);
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
         href.base:=procinfo^.frame_pointer_reg;
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
         href.base:=procinfo^.frame_pointer_reg;
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

    { only usefull in startup code }
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: tregisterset);
      begin
        case t.loc of
          LOC_REGISTER:
            begin
              { can't be a regvar, since it would be LOC_CREGISTER then }
              exclude(regs,t.register);
              if t.registerhigh <> R_NO then
                exclude(regs,t.registerhigh);
            end;
          LOC_CREFERENCE,LOC_REFERENCE:
            begin
              if not(cs_regalloc in aktglobalswitches) or
                 (t.reference.base in rg.usableregsint) then
                exclude(regs,t.reference.base);
              if not(cs_regalloc in aktglobalswitches) or
                 (t.reference.index in rg.usableregsint) then
              exclude(regs,t.reference.index);
            end;
        end;
      end;


    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                                para_offset:longint;alignment : longint);
      var
        tempreference : treference;
        href : treference;
        hreg : tregister;
        sizetopush,
        size : longint;
        cgsize : tcgsize;
      begin
        { Move flags and jump in register to make it less complex }
        if p.location.loc in [LOC_FLAGS,LOC_JUMP] then
         location_force_reg(exprasmlist,p.location,def_cgsize(p.resulttype.def),false);

        { Handle Floating point types differently }
        if p.resulttype.def.deftype=floatdef then
         begin
           case p.location.loc of
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
                   reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize)
                  else
                   reference_reset_base(href,R_ESP,0);

                  cg.a_loadfpu_reg_ref(exprasmlist,
                    def_cgsize(p.resulttype.def),p.location.register,href);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 sizetopush:=align(p.resulttype.def.size,alignment);
                 tempreference:=p.location.reference;
                 inc(tempreference.offset,sizetopush);
                 while (sizetopush>0) do
                  begin
                    if sizetopush>=4 then
                     begin
                       cgsize:=OS_32;
                       inc(pushedparasize,4);
                       dec(tempreference.offset,4);
                       dec(sizetopush,4);
                     end
                    else
                     begin
                       cgsize:=OS_16;
                       inc(pushedparasize,2);
                       dec(tempreference.offset,2);
                       dec(sizetopush,2);
                     end;
                    if inlined then
                     begin
                       reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
                       cg.a_load_ref_ref(exprasmlist,cgsize,tempreference,href);
                     end
                    else
                     cg.a_param_ref(exprasmlist,cgsize,tempreference,-1);
                  end;
               end;
             else
               internalerror(200204243);
           end;
         end
        else
         begin
           { call by value open array ? }
           if is_cdecl and
              push_addr_param(p.resulttype.def) then
            begin
              if not (p.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                internalerror(200204241);
              { push on stack }
              size:=align(p.resulttype.def.size,alignment);
              inc(pushedparasize,size);
              emit_const_reg(A_SUB,S_L,size,R_ESP);
              reference_reset_base(href,R_ESP,0);
              cg.g_concatcopy(exprasmlist,p.location.reference,href,size,false,false);
            end
           else
            begin
              case p.location.loc of
                LOC_CONSTANT,
                LOC_REGISTER,
                LOC_CREGISTER,
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                    cgsize:=def_cgsize(p.resulttype.def);
                    if cgsize in [OS_64,OS_S64] then
                     begin
                       inc(pushedparasize,8);
                       if inlined then
                        begin
                          reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
                          tcg64f32(cg).a_load64_loc_ref(exprasmlist,p.location,href);
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
                              cgsize:=OS_16;
                           end;
                         OS_16,OS_S16 :
                           begin
                             if alignment=4 then
                              cgsize:=OS_32;
                           end;
                       end;
                       { update register to use to match alignment }
                       if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        begin
                          hreg:=p.location.register;
                          p.location.register:=rg.makeregsize(p.location.register,cgsize);
                        end;
                       inc(pushedparasize,alignment);
                       if inlined then
                        begin
                          reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
                          cg.a_load_loc_ref(exprasmlist,p.location,href);
                        end
                       else
                        cg.a_param_loc(exprasmlist,p.location,-1);
                       { restore old register }
                       if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                         p.location.register:=hreg;
                     end;
                    location_release(exprasmlist,p.location);
                  end;
{$ifdef SUPPORT_MMX}
                LOC_MMXREGISTER,
                LOC_CMMXREGISTER:
                  begin
                     inc(pushedparasize,8);
                     if inlined then
                       begin
                          reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
                          cg.a_loadmm_reg_ref(exprasmlist,p.location.register,href);
                       end
                     else
                      cg.a_parammm_reg(exprasmlist,p.location.register);
                  end;
{$endif SUPPORT_MMX}
                else
                  internalerror(200204241);
              end;
           end;
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
         cg.a_call_name(exprasmlist,'FPC_OVERFLOW');
         cg.a_label(exprasmlist,hl);
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

end.
{
  $Log$
  Revision 1.36  2002-05-12 16:53:18  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.35  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.34  2002/04/21 15:39:41  carl
  * changeregsize -> rg.makeregsize

  Revision 1.33  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.32  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.31  2002/04/15 19:44:21  peter
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
