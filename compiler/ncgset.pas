{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate generic assembler for in set/case nodes

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
unit ncgset;

{$i fpcdefs.inc}

interface

    uses
       node,nset,cpubase,cginfo,cgbase,cgobj,aasmbase,aasmtai;

    type
       tcgsetelementnode = class(tsetelementnode)
          procedure pass_2;override;
       end;

       tcginnode = class(tinnode)
          procedure pass_2;override;
          {# Routine to test bitnumber in bitnumber register on value
             in value register. The __result register should be set
             to one if the bit is set, otherwise __result register
             should be set to zero.

             Should be overriden on processors which have specific
             instructions to do bit tests.
          }

          procedure emit_bit_test_reg_reg(list : taasmoutput; bitnumber : tregister;
             value : tregister; __result :tregister);virtual;
       end;

implementation

    uses
      globtype,systems,
      verbose,globals,
      symconst,symdef,defbase,
      paramgr,
      pass_2,
      ncon,
      cga,tgobj,ncgutil,regvars,rgobj;


{*****************************************************************************
                          TCGSETELEMENTNODE
*****************************************************************************}

    procedure tcgsetelementnode.pass_2;
       var
         pushedregs : tmaybesave;
       begin
       { load first value in 32bit register }
         secondpass(left);
         if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
           location_force_reg(exprasmlist,left.location,OS_32,false);

       { also a second value ? }
         if assigned(right) then
           begin
             maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
             secondpass(right);
             if codegenerror then
               exit;
             maybe_restore(exprasmlist,left.location,pushedregs);
             if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              location_force_reg(exprasmlist,right.location,OS_32,false);
           end;

         { we doesn't modify the left side, we check only the type }
         location_copy(location,left.location);
       end;


{*****************************************************************************
*****************************************************************************}

  {**********************************************************************}
  {  Description: Emit operation to do a bit test, where the bitnumber   }
  {  to test is in the bitnumber register. The value to test against is  }
  {  located in the value register.                                      }
  {   WARNING: Bitnumber register value is DESTROYED!                    }
  {  __Result register is set to 1, if the bit is set otherwise, __Result}
  {   is set to zero. __RESULT register is also used as scratch.         }
  {**********************************************************************}
  procedure tcginnode.emit_bit_test_reg_reg(list : taasmoutput; bitnumber : tregister; value : tregister; __result :tregister);
    begin
      { first make sure that the bit number is modulo 32 }

      { not necessary, since if it's > 31, we have a range error -> will }
      { be caught when range checking is on! (JM)                        }
      { cg.a_op_const_reg(list,OP_AND,31,bitnumber);                     }

      { rotate value register "bitnumber" bits to the right }
      cg.a_op_reg_reg_reg(list,OP_SHR,OS_INT,bitnumber,value,__result);
      { extract the bit we want }
      cg.a_op_const_reg(list,OP_AND,1,__result);
    end;


    procedure tcginnode.pass_2;
       type
         Tsetpart=record
           range : boolean;      {Part is a range.}
           start,stop : byte;    {Start/stop when range; Stop=element when an element.}
         end;
       var
         genjumps,
         use_small,
         ranges     : boolean;
         hr,hr2,hr3,
         pleftreg   : tregister;
         href       : treference;
         opsize     : tcgsize;
         setparts   : array[1..8] of Tsetpart;
         i,numparts : byte;
         adjustment : longint;
         pushedregs : tmaybesave;
         l,l2,l3       : tasmlabel;

         function analizeset(const Aset:Tconstset;is_small:boolean):boolean;
           var
             compares,maxcompares:word;
	     
             i:byte;
           begin
             analizeset:=false;
             ranges:=false;
             numparts:=0;
             compares:=0;
             { Lots of comparisions take a lot of time, so do not allow
               too much comparisions. 8 comparisions are, however, still
               smalller than emitting the set }
             if cs_littlesize in aktglobalswitches then
              maxcompares:=8
             else
              maxcompares:=5;
             { when smallset is possible allow only 3 compares the smallset
               code is for littlesize also smaller when more compares are used }
             if is_small then
              maxcompares:=3;
             for i:=0 to 255 do
              if i in Aset then
               begin
                 if (numparts=0) or (i<>setparts[numparts].stop+1) then
                  begin
                  {Set element is a separate element.}
                    inc(compares);
                    if compares>maxcompares then
                         exit;
                    inc(numparts);
                    setparts[numparts].range:=false;
                    setparts[numparts].stop:=i;
                  end
                 else
                  {Set element is part of a range.}
                  if not setparts[numparts].range then
                   begin
                     {Transform an element into a range.}
                     setparts[numparts].range:=true;
                     setparts[numparts].start:=setparts[numparts].stop;
                     setparts[numparts].stop:=i;
                     ranges := true;
                     { there's only one compare per range anymore. Only a }
                     { sub is added, but that's much faster than a        }
                     { cmp/jcc combo so neglect its effect                }
{                     inc(compares);
                     if compares>maxcompares then
                      exit; }
                   end
                  else
                   begin
                    {Extend a range.}
                    setparts[numparts].stop:=i;
                   end;
              end;
             analizeset:=true;
           end;

       begin
         { We check first if we can generate jumps, this can be done
           because the resulttype.def is already set in firstpass }

         { check if we can use smallset operation using btl which is limited
           to 32 bits, the left side may also not contain higher values !! }
         use_small:=(tsetdef(right.resulttype.def).settype=smallset) and
                    ((left.resulttype.def.deftype=orddef) and (torddef(left.resulttype.def).high<=32) or
                     (left.resulttype.def.deftype=enumdef) and (tenumdef(left.resulttype.def).max<=32));

         { Can we generate jumps? Possible for all types of sets }
         genjumps:=(right.nodetype=setconstn) and
                   analizeset(Tsetconstnode(right).value_set^,use_small);
         { calculate both operators }
         { the complex one first }
         firstcomplex(self);
         secondpass(left);
         { Only process the right if we are not generating jumps }
         if not genjumps then
          begin
            maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
            secondpass(right);
            maybe_restore(exprasmlist,left.location,pushedregs);
          end;
         if codegenerror then
          exit;

         { ofcourse not commutative }
         if nf_swaped in flags then
          swapleftright;

         { location is always LOC_JUMP }
         location_reset(location,LOC_REGISTER,OS_INT);
         { allocate a register for the result }
         location.register := rg.getregisterint(exprasmlist);
         { Get a label to jump to the end }
         getlabel(l);

         if genjumps then
          begin
            { clear the register value, indicating result is FALSE }
            cg.a_load_const_reg(exprasmlist,OS_INT,0,location.register);
            opsize := def_cgsize(left.resulttype.def);
            { If register is used, use only lower 8 bits }
            if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
             begin
               { for ranges we always need a 32bit register, because then we }
               { use the register as base in a reference (JM)                }
               if ranges then
                 begin
                   pleftreg:=rg.makeregsize(left.location.register,OS_INT);
                   cg.a_load_reg_reg(exprasmlist,left.location.size,left.location.register,pleftreg);
                   if opsize <> OS_INT then
                     cg.a_op_const_reg(exprasmlist,OP_AND,255,pleftreg);
                   opsize := OS_INT;
                 end
               else
                 { otherwise simply use the lower 8 bits (no "and" }
                 { necessary this way) (JM)                        }
                 begin
                   pleftreg:=rg.makeregsize(left.location.register,OS_8);
                   opsize := OS_8;
                 end;
             end
            else
             begin
               { load the value in a register }
               pleftreg := cg.get_scratch_reg_int(exprasmlist);
               opsize := OS_INT;
               cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),left.location.reference,pleftreg);
             end;



            { how much have we already substracted from the x in the }
            { "x in [y..z]" expression                               }
            adjustment := 0;
            hr := R_NO;

            for i:=1 to numparts do
             if setparts[i].range then
              { use fact that a <= x <= b <=> cardinal(x-a) <= cardinal(b-a) }
              begin
                { is the range different from all legal values? }
                if (setparts[i].stop-setparts[i].start <> 255) then
                  begin
                    { yes, is the lower bound <> 0? }
                    if (setparts[i].start <> 0) then
                      { we're going to substract from the left register,   }
                      { so in case of a LOC_CREGISTER first move the value }
                      { to edi (not done before because now we can do the  }
                      { move and substract in one instruction with LEA)    }
                      if (left.location.loc = LOC_CREGISTER) and
                         (hr <> pleftreg) then
                        begin
                          hr:=cg.get_scratch_reg_int(exprasmlist);
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,opsize,setparts[i].start,pleftreg,hr);
                          pleftreg:=hr;
                          opsize := OS_INT;
                        end
                      else
                        begin
                          { otherwise, the value is already in a register   }
                          { that can be modified                            }
                          cg.a_op_const_reg(exprasmlist,OP_SUB,
                             setparts[i].start-adjustment,pleftreg)
                        end;
                    { new total value substracted from x:           }
                    { adjustment + (setparts[i].start - adjustment) }
                    adjustment := setparts[i].start;

                    { check if result < b-a+1 (not "result <= b-a", since }
                    { we need a carry in case the element is in the range }
                    { (this will never overflow since we check at the     }
                    { beginning whether stop-start <> 255)                }
                    cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_B,
                      setparts[i].stop-setparts[i].start+1,pleftreg,l);
                  end
                else
                  { if setparts[i].start = 0 and setparts[i].stop = 255,  }
                  { it's always true since "in" is only allowed for bytes }
                  begin
                    cg.a_jmp_always(exprasmlist,l);
                  end;
              end
             else
              begin
                { Emit code to check if left is an element }
                cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_EQ,
                      setparts[i].stop-adjustment,pleftreg,l);
              end;
             { To compensate for not doing a second pass }
             right.location.reference.symbol:=nil;
             getlabel(l3);
             cg.a_jmp_always(exprasmlist,l3);
             { Now place the end label if IN success }
             cg.a_label(exprasmlist,l);
             { result register is 1 }
             cg.a_load_const_reg(exprasmlist,OS_INT,1,location.register);
             { in case value is not found }
             cg.a_label(exprasmlist,l3);
             case left.location.loc of
               LOC_CREGISTER :
                 cg.free_scratch_reg(exprasmlist,pleftreg);
               LOC_REGISTER :
                 rg.ungetregister(exprasmlist,pleftreg);
               else
                 begin
                   reference_release(exprasmlist,left.location.reference);
                   cg.free_scratch_reg(exprasmlist,pleftreg);
                 end;
             end;
          end
         else
         {*****************************************************************}
         {                     NO JUMP TABLE GENERATION                    }
         {*****************************************************************}
          begin
            { We will now generated code to check the set itself, no jmps,
              handle smallsets separate, because it allows faster checks }
            if use_small then
             begin
             {****************************  SMALL SET **********************}
               if left.nodetype=ordconstn then
                begin
                  { clear the register value, indicating result is FALSE }
                  cg.a_load_const_reg(exprasmlist,OS_INT,0,location.register);
                  hr:=cg.get_scratch_reg_int(exprasmlist);
                  case right.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER:
                      begin
                         { load set value into register }
                         cg.a_load_reg_reg(exprasmlist,OS_INT,
                            right.location.register,hr);
                      end;
                    LOC_REFERENCE,
                    LOC_CREFERENCE :
                      begin
                         { load set value into register }
                         cg.a_load_ref_reg(exprasmlist,OS_INT,
                            right.location.reference,hr);
                      end;
                    else
                      internalerror(200203312);
                  end;
                 { then do AND with constant and register }
                 cg.a_op_const_reg(exprasmlist,OP_AND,1 shl
                    (tordconstnode(left).value and 31),hr);
                 { if the value in the AND register is <> 0 then the value is equal. }
                 cg.a_cmp_const_reg_label(exprasmlist,OS_32,OC_EQ,1 shl
                    (tordconstnode(left).value and 31),hr,l);
                 cg.free_scratch_reg(exprasmlist,hr);
                 getlabel(l3);
                 cg.a_jmp_always(exprasmlist,l3);
                 { Now place the end label if IN success }
                 cg.a_label(exprasmlist,l);
                 { result register is 1 : LOC_JUMP }
                 cg.a_load_const_reg(exprasmlist,OS_INT,1,location.register);
                 { in case value is not found }
                 cg.a_label(exprasmlist,l3);
                 location_release(exprasmlist,right.location);
                end
               else
                begin
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr3:=rg.makeregsize(left.location.register,OS_INT);
                          cg.a_load_reg_reg(exprasmlist,left.location.size,left.location.register,hr3);
                          hr:=cg.get_scratch_reg_int(exprasmlist);
                          cg.a_load_reg_reg(exprasmlist,OS_INT,hr3,hr);
                       end;
                  else
                    begin
                      hr:=cg.get_scratch_reg_int(exprasmlist);
                      cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),
                         left.location.reference,hr);
                      location_release(exprasmlist,left.location);
                    end;
                  end;

                  case right.location.loc of
                 LOC_REGISTER,
                LOC_CREGISTER :
                          begin
                            hr2:=right.location.register;
                          end;
                   LOC_CONSTANT :
                       begin
                         hr2:=rg.getregisterint(exprasmlist);
                         cg.a_load_const_reg(exprasmlist,OS_32,
                            right.location.value,hr2);
                       end;
                   LOC_CREFERENCE,
                   LOC_REFERENCE :
                       begin
                         location_release(exprasmlist,right.location);
                         hr2:=rg.getregisterint(exprasmlist);
                         cg.a_load_ref_reg(exprasmlist, OS_32,
                           right.location.reference,hr2);
                       end;
                     else
                       internalerror(2002032210);
                  end;
                  { emit bit test operation }
                  emit_bit_test_reg_reg(exprasmlist,hr,hr2,location.register);
                  { free the resources }
                  case right.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER :
                            rg.ungetregisterint(exprasmlist,right.location.register);
                    LOC_CONSTANT ,
                    LOC_CREFERENCE,
                    LOC_REFERENCE :
                         rg.ungetregisterint(exprasmlist,hr2);
                     else
                       internalerror(2002032210);
                  end;
                  { free bitnumber register }
                  cg.free_scratch_reg(exprasmlist,hr);
                end;
             end
            else
             {************************** NOT SMALL SET ********************}
             begin
               if right.location.loc=LOC_CONSTANT then
                begin
                  { this section has not been tested!    }
                  { can it actually occur currently? CEC }
                  internalerror(20020610);
                  getlabel(l);
                  getlabel(l2);

                  { Is this treated in firstpass ?? }
                  if left.nodetype=ordconstn then
                    begin
                      hr:=rg.getregisterint(exprasmlist);
                      left.location.loc:=LOC_REGISTER;
                      left.location.size:=OS_INT;
                      left.location.register:=hr;
                      cg.a_load_const_reg(exprasmlist,OS_INT,
                            tordconstnode(left).value,hr);
                    end;
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr:=rg.makeregsize(left.location.register,OS_INT);
                          cg.a_load_reg_reg(exprasmlist,left.location.size,left.location.register,hr);
                          cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_BE,31,hr,l);
                        { reset of result register is done in routine entry }
                          cg.a_jmp_always(exprasmlist,l2);
                          cg.a_label(exprasmlist,l);
                        { We have to load the value into a register because
                          btl does not accept values only refs or regs (PFV) }
                          hr2:=rg.getregisterint(exprasmlist);
                          cg.a_load_const_reg(exprasmlist,OS_INT,right.location.value,hr2);
                       end;
                  else
                    begin
                       cg.a_cmp_const_ref_label(exprasmlist,OS_8,OC_BE,31,left.location.reference,l);
                       cg.a_jmp_always(exprasmlist,l2);
                       cg.a_label(exprasmlist,l);
                       location_release(exprasmlist,left.location);
                       hr:=rg.getregisterint(exprasmlist);
                       cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,hr);
                     { We have to load the value into a register because
                       btl does not accept values only refs or regs (PFV) }
                       hr2:=rg.getregisterint(exprasmlist);
                       cg.a_load_const_reg(exprasmlist,OS_INT,
                         right.location.value,hr2);
                    end;
                  end;
                  { emit bit test operation }
                  emit_bit_test_reg_reg(exprasmlist,hr,hr2,location.register);
                  rg.ungetregisterint(exprasmlist,hr2);
                  if not (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                    rg.ungetregisterint(exprasmlist,hr);
                  cg.a_label(exprasmlist,l2);
                end { of right.location.loc=LOC_CONSTANT }
               { do search in a normal set which could have >32 elementsm
                 but also used if the left side contains higher values > 32 }
               else if left.nodetype=ordconstn then
                begin
                  getlabel(l2);
                  getlabel(l);
                  { use location.register as scratch register here }
                  inc(right.location.reference.offset,tordconstnode(left).value shr 3);
                  cg.a_load_ref_reg(exprasmlist, OS_8, right.location.reference, location.register);
                  cg.a_op_const_reg(exprasmlist, OP_AND,1 shl (tordconstnode(left).value and 7),
                     location.register);
                  cg.a_cmp_const_reg_label(exprasmlist,OS_8, OC_NE,0,location.register,l2);
                  cg.a_load_const_reg(exprasmlist, OS_INT,0, location.register);
                  cg.a_jmp_always(exprasmlist,l);
                  cg.a_label(exprasmlist,l2);
                  cg.a_load_const_reg(exprasmlist, OS_INT,1, location.register);
                  {emit_const_ref(A_TEST,S_B,1 shl (tordconstnode(left).value and 7),right.location.reference);}
                  cg.a_label(exprasmlist,l);
                  location_release(exprasmlist,right.location);
                end
               else
                begin
                  if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                    pleftreg:=rg.makeregsize(left.location.register,OS_INT)
                  else
                    pleftreg:=rg.getregisterint(exprasmlist);
                  cg.a_load_loc_reg(exprasmlist,left.location,pleftreg);
                  location_freetemp(exprasmlist,left.location);
                  location_release(exprasmlist,left.location);
                  cg.a_param_reg(exprasmlist,OS_8,pleftreg,paramanager.getintparaloc(2));
                  cg.a_param_ref(exprasmlist,OS_ADDR,right.location.reference,paramanager.getintparaloc(1));
                  cg.a_call_name(exprasmlist,'FPC_SET_IN_BYTE');
                  { result of value is always one full register }
                  cg.a_load_reg_reg(exprasmlist,OS_INT,ACCUMULATOR,location.register);
                  { release the allocated register  }
                  if not (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                    rg.ungetregisterint(exprasmlist,pleftreg);
                  location_release(exprasmlist,right.location);
                end;
             end;
          end;
          location_freetemp(exprasmlist,right.location);
       end;



begin
   csetelementnode:=tcgsetelementnode;
   cinnode:=tcginnode;
end.
{
  $Log$
  Revision 1.8  2002-07-22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.7  2002/07/21 16:58:20  jonas
    * fixed some bugs in tcginnode.pass_2() and optimized the bit test

  Revision 1.6  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.5  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.4  2002/07/07 10:16:29  florian
    * problems with last commit fixed

  Revision 1.3  2002/07/06 20:19:25  carl
  + generic set handling

  Revision 1.2  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.1  2002/06/16 08:14:56  carl
  + generic sets

}
