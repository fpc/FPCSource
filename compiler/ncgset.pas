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
       node,nset,cpubase,cginfo,cgbase,cgobj,aasmbase,aasmtai,globals;

    type
       tcgsetelementnode = class(tsetelementnode)
          procedure pass_2;override;
       end;

       tcginnode = class(tinnode)
          procedure pass_2;override;
       protected
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

       tcgcasenode = class(tcasenode)
          {
            Emits the case node statement. Contrary to the intel
            80x86 version, this version does not emit jump tables,
            because of portability problems.
          }
          procedure pass_2;override;

        protected
          with_sign : boolean;
          opsize : tcgsize;
          jmp_gt,jmp_lt,jmp_le : topcmp;
          { register with case expression }
          hregister,hregister2 : tregister;
          endlabel,elselabel : tasmlabel;

          { true, if we can omit the range check of the jump table }
          jumptable_no_range : boolean;
          { has the implementation jumptable support }
          min_label : tconstexprint;

          procedure optimizevalues(var max_linear_list:longint;var max_dist:cardinal);virtual;
          function  has_jumptable : boolean;virtual;
          procedure genjumptable(hp : pcaserecord;min_,max_ : longint); virtual;
          procedure genlinearlist(hp : pcaserecord); virtual;
          procedure genlinearcmplist(hp : pcaserecord); virtual;
          procedure gentreejmp(p : pcaserecord);
       end;


implementation

    uses
      globtype,systems,
      verbose,
      symconst,symdef,defutil,
      paramgr,
      pass_2,
      nbas,ncon,nflw,
      tgobj,ncgutil,regvars,rgobj,cpuinfo;


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
           {$ifndef newra}
             maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
           {$endif}
             secondpass(right);
             if codegenerror then
               exit;
           {$ifndef newra}
             maybe_restore(exprasmlist,left.location,pushedregs);
           {$endif newra}
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
      cg.a_op_const_reg(list,OP_AND,OS_INT,1,__result);
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
         opsize     : tcgsize;
         setparts   : array[1..8] of Tsetpart;
         i,numparts : byte;
         adjustment : longint;
         pushedregs : tmaybesave;
         l,l2,l3       : tasmlabel;
         r:Tregister;

{$ifdef oldset}
         function analizeset(Aset:Pconstset;is_small:boolean):boolean;
       type
             byteset=set of byte;
{$else}
         function analizeset(const Aset:Tconstset;is_small:boolean):boolean;
{$endif}
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
         {$ifdef oldset}
              if i in byteset(Aset^) then
         {$else}
              if i in Aset then
         {$endif}
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
{$ifdef oldset}
         genjumps:=(right.nodetype=setconstn) and
                   analizeset(Tsetconstnode(right).value_set,use_small);
{$else}
         genjumps:=(right.nodetype=setconstn) and
                   analizeset(Tsetconstnode(right).value_set^,use_small);
{$endif}
         { calculate both operators }
         { the complex one first }
         firstcomplex(self);
         secondpass(left);
         { Only process the right if we are not generating jumps }
         if not genjumps then
          begin
          {$ifndef newra}
            maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
          {$endif}
            secondpass(right);
          {$ifndef newra}
            maybe_restore(exprasmlist,left.location,pushedregs);
          {$endif}
          end;
         if codegenerror then
          exit;

         { ofcourse not commutative }
         if nf_swaped in flags then
          swapleftright;

         { location is always LOC_JUMP }
         location_reset(location,LOC_REGISTER,def_cgsize(resulttype.def));
         { allocate a register for the result }
         location.register := rg.getregisterint(exprasmlist,location.size);

         if genjumps then
          begin
            { Get a label to jump to the end }
            objectlibrary.getlabel(l);

            { clear the register value, indicating result is FALSE }
            cg.a_load_const_reg(exprasmlist,location.size,0,location.register);
            opsize := def_cgsize(left.resulttype.def);
            { If register is used, use only lower 8 bits }
            if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
             begin
               { for ranges we always need a 32bit register, because then we }
               { use the register as base in a reference (JM)                }
               if ranges then
                 begin
                   pleftreg:=rg.makeregsize(left.location.register,OS_INT);
                   cg.a_load_reg_reg(exprasmlist,left.location.size,OS_INT,left.location.register,pleftreg);
                   if opsize <> OS_INT then
                     cg.a_op_const_reg(exprasmlist,OP_AND,OS_INT,255,pleftreg);
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
               opsize := OS_INT;
             {$ifdef newra}
               pleftreg:=rg.getregisterint(exprasmlist,opsize);
             {$else}
               pleftreg := cg.get_scratch_reg_int(exprasmlist,opsize);
             {$endif}
               cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),opsize,left.location.reference,pleftreg);
             end;



            { how much have we already substracted from the x in the }
            { "x in [y..z]" expression                               }
            adjustment := 0;
            hr.enum := R_NO;

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
                         (hr.enum <> pleftreg.enum) then
                        begin
                        {$ifdef newra}
                          hr:=rg.getregisterint(exprasmlist,OS_INT);
                        {$else}
                          hr:=cg.get_scratch_reg_int(exprasmlist,OS_INT);
                        {$endif}
                          cg.a_op_const_reg_reg(exprasmlist,OP_SUB,opsize,setparts[i].start,pleftreg,hr);
                          pleftreg:=hr;
                          opsize := OS_INT;
                        end
                      else
                        begin
                          { otherwise, the value is already in a register   }
                          { that can be modified                            }
                          cg.a_op_const_reg(exprasmlist,OP_SUB,opsize,
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
             objectlibrary.getlabel(l3);
             cg.a_jmp_always(exprasmlist,l3);
             { Now place the end label if IN success }
             cg.a_label(exprasmlist,l);
             { result register is 1 }
             cg.a_load_const_reg(exprasmlist,location.size,1,location.register);
             { in case value is not found }
             cg.a_label(exprasmlist,l3);
             case left.location.loc of
               LOC_CREGISTER :
                {$ifdef newra}
                 rg.ungetregisterint(exprasmlist,pleftreg);
                {$else}
                 cg.free_scratch_reg(exprasmlist,pleftreg);
                {$endif}
               LOC_REGISTER :
                 rg.ungetregisterint(exprasmlist,pleftreg);
               else
                 begin
                   reference_release(exprasmlist,left.location.reference);
                  {$ifdef newra}
                   rg.ungetregisterint(exprasmlist,pleftreg);
                  {$else}
                   cg.free_scratch_reg(exprasmlist,pleftreg);
                  {$endif}
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
                  cg.a_load_const_reg(exprasmlist,location.size,0,location.register);
                  case right.location.loc of
                    LOC_REGISTER:
                      hr:=right.location.register;
                    LOC_CREGISTER:
                      begin
                         hr:=rg.getregisterint(exprasmlist,OS_INT);
                         { load set value into register }
                         cg.a_load_reg_reg(exprasmlist,right.location.size,OS_INT,
                            right.location.register,hr);
                         location_release(exprasmlist,right.location);
                      end;
                    LOC_REFERENCE,
                    LOC_CREFERENCE :
                      begin
                         hr:=rg.getregisterint(exprasmlist,OS_INT);
                         { load set value into register }
                         cg.a_load_ref_reg(exprasmlist,OS_INT,opsize,
                            right.location.reference,hr);
                         location_release(exprasmlist,right.location);
                      end;
                    else
                      internalerror(200203312);
                  end;

                 { then SHR the register }
                 cg.a_op_const_reg(exprasmlist,OP_SHR,OS_INT,
                   tordconstnode(left).value and 31,hr);
                 { then extract the lowest bit }
                 cg.a_op_const_reg(exprasmlist,OP_AND,OS_INT,1,hr);
                 location.register:=hr;
                end
               else
                begin
                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr3:=rg.makeregsize(left.location.register,OS_INT);
                          cg.a_load_reg_reg(exprasmlist,left.location.size,OS_INT,left.location.register,hr3);
                        {$ifdef newra}
                          hr:=rg.getregisterint(exprasmlist,OS_INT);
                        {$else}
                          hr:=cg.get_scratch_reg_int(exprasmlist,OS_INT);
                        {$endif}
                          cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,hr3,hr);
                       end;
                  else
                    begin
                    {$ifdef newra}
                      hr:=rg.getregisterint(exprasmlist,OS_INT);
                    {$else}
                      hr:=cg.get_scratch_reg_int(exprasmlist,OS_INT);
                    {$endif}
                      cg.a_load_ref_reg(exprasmlist,def_cgsize(left.resulttype.def),OS_INT,
                         left.location.reference,hr);
                      location_release(exprasmlist,left.location);
                    end;
                  end;

                  case right.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER :
                      hr2:=right.location.register;
                    LOC_CONSTANT :
                      begin
                        hr2:=rg.getregisterint(exprasmlist,OS_INT);
                        cg.a_load_const_reg(exprasmlist,OS_INT,right.location.value,hr2);
                      end;
                    LOC_CREFERENCE,
                    LOC_REFERENCE :
                      begin
                        location_release(exprasmlist,right.location);
                        hr2:=rg.getregisterint(exprasmlist,OS_INT);
                        cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,right.location.reference,hr2);
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
                {$ifdef newra}
                  rg.ungetregisterint(exprasmlist,hr);
                {$else}
                  cg.free_scratch_reg(exprasmlist,hr);
                {$endif}
                end;
             end
            else
             {************************** NOT SMALL SET ********************}
             begin
               if right.location.loc=LOC_CONSTANT then
                begin
                  { this section has not been tested!    }
                  { can it actually occur currently? CEC }
                  { yes: "if bytevar in [1,3,5,7,9,11,13,15]" (JM) }
                  objectlibrary.getlabel(l);
                  objectlibrary.getlabel(l2);

                  case left.location.loc of
                     LOC_REGISTER,
                     LOC_CREGISTER:
                       begin
                          hr:=rg.makeregsize(left.location.register,OS_INT);
                          cg.a_load_reg_reg(exprasmlist,left.location.size,OS_INT,left.location.register,hr);
                          cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_BE,31,hr,l);
                        { reset of result register is done in routine entry }
                          cg.a_jmp_always(exprasmlist,l2);
                          cg.a_label(exprasmlist,l);
                        { We have to load the value into a register because
                          btl does not accept values only refs or regs (PFV) }
                          hr2:=rg.getregisterint(exprasmlist,OS_INT);
                          cg.a_load_const_reg(exprasmlist,OS_INT,right.location.value,hr2);
                       end;
                     LOC_REFERENCE,
                     LOC_CREFERENCE:
                       begin
                          cg.a_cmp_const_ref_label(exprasmlist,OS_8,OC_BE,31,left.location.reference,l);
                          cg.a_jmp_always(exprasmlist,l2);
                          cg.a_label(exprasmlist,l);
                          location_release(exprasmlist,left.location);
                          hr:=rg.getregisterint(exprasmlist,OS_INT);
                          cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,left.location.reference,hr);
                        { We have to load the value into a register because
                          btl does not accept values only refs or regs (PFV) }
                          hr2:=rg.getregisterint(exprasmlist,OS_INT);
                          cg.a_load_const_reg(exprasmlist,OS_INT,right.location.value,hr2);
                       end;
                     else
                       internalerror(2002081002);
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
                  { use location.register as scratch register here }
                  if (target_info.endian = endian_little) then
                    inc(right.location.reference.offset,tordconstnode(left).value shr 3)
                  else
                    { adjust for endianess differences }
                    inc(right.location.reference.offset,(tordconstnode(left).value shr 3) xor 3);
                  cg.a_load_ref_reg(exprasmlist,OS_8,location.size,right.location.reference, location.register);
                  location_release(exprasmlist,right.location);
                  cg.a_op_const_reg(exprasmlist,OP_SHR,location.size,tordconstnode(left).value and 7,
                    location.register);
                  cg.a_op_const_reg(exprasmlist,OP_AND,location.size,1,location.register);
                end
               else
                begin
                  if (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                    pleftreg:=rg.makeregsize(left.location.register,OS_INT)
                  else
                    pleftreg:=rg.getregisterint(exprasmlist,OS_INT);
                  cg.a_load_loc_reg(exprasmlist,OS_INT,left.location,pleftreg);
                  location_freetemp(exprasmlist,left.location);
                  location_release(exprasmlist,left.location);
                  cg.a_param_reg(exprasmlist,OS_8,pleftreg,paramanager.getintparaloc(2));
                  cg.a_paramaddr_ref(exprasmlist,right.location.reference,paramanager.getintparaloc(1));
                  cg.a_call_name(exprasmlist,'FPC_SET_IN_BYTE');
                  { result of value is always one full register }
                  r.enum:=R_INTREGISTER;
                  r.number:=NR_FUNCTION_RESULT_REG;
                  cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,r,location.register);
                  { release the allocated register  }
                  if not (left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                    rg.ungetregisterint(exprasmlist,pleftreg);
                  location_release(exprasmlist,right.location);
                end;
             end;
          end;
          location_freetemp(exprasmlist,right.location);
       end;

{*****************************************************************************
                            TCGCASENODE
*****************************************************************************}

    procedure tcgcasenode.optimizevalues(var max_linear_list:longint;var max_dist:cardinal);
      begin
        { no changes by default }
      end;


    function tcgcasenode.has_jumptable : boolean;
      begin
        { No jumptable support in the default implementation }
        has_jumptable:=false;
      end;


    procedure tcgcasenode.genjumptable(hp : pcaserecord;min_,max_ : longint);
      begin
        internalerror(200209161);
      end;


    procedure tcgcasenode.genlinearlist(hp : pcaserecord);

      var
         first : boolean;
         last : TConstExprInt;
         scratch_reg: tregister;

      procedure genitem(t : pcaserecord);

          procedure gensub(value:longint);
          begin
            { here, since the sub and cmp are separate we need
              to move the result before subtract to a help
              register.
            }
            cg.a_load_reg_reg(exprasmlist, opsize, opsize, hregister, scratch_reg);
            cg.a_op_const_reg(exprasmlist, OP_SUB, opsize, value, hregister);
          end;

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           { need we to test the first value }
           if first and (t^._low>get_min_value(left.resulttype.def)) then
             begin
                cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_lt,aword(t^._low),hregister,elselabel);
             end;
           if t^._low=t^._high then
             begin
                if t^._low-last=0 then
                  cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_EQ,0,hregister,t^.statement)
                else
                  begin
                      gensub(longint(t^._low-last));
                      cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_EQ,aword(t^._low-last),scratch_reg,t^.statement);
                  end;
                last:=t^._low;
             end
           else
             begin
                { it begins with the smallest label, if the value }
                { is even smaller then jump immediately to the    }
                { ELSE-label                                }
                if first then
                  begin
                     { have we to ajust the first value ? }
                     if (t^._low>get_min_value(left.resulttype.def)) then
                       gensub(longint(t^._low));
                  end
                else
                  begin
                    { if there is no unused label between the last and the }
                    { present label then the lower limit can be checked    }
                    { immediately. else check the range in between:       }
                    gensub(longint(t^._low-last));
                    cg.a_cmp_const_reg_label(exprasmlist, opsize,jmp_lt,aword(t^._low-last),scratch_reg,elselabel);
                  end;
                gensub(longint(t^._high-t^._low));
                cg.a_cmp_const_reg_label(exprasmlist, opsize,jmp_le,aword(t^._high-t^._low),scratch_reg,t^.statement);
                last:=t^._high;
             end;
           first:=false;
           if assigned(t^.greater) then
             genitem(t^.greater);
        end;

      begin
         { do we need to generate cmps? }
         if (with_sign and (min_label<0)) then
           genlinearcmplist(hp)
         else
           begin
              last:=0;
              first:=true;
            {$ifdef newra}
              scratch_reg:=rg.getregisterint(exprasmlist,opsize);
            {$else newra}
              scratch_reg := cg.get_scratch_reg_int(exprasmlist,opsize);
            {$endif}
              genitem(hp);
            {$ifdef newra}
              rg.ungetregisterint(exprasmlist,scratch_reg);
            {$else}
              cg.free_scratch_reg(exprasmlist,scratch_reg);
            {$endif}
              cg.a_jmp_always(exprasmlist,elselabel);
           end;
      end;


    procedure tcgcasenode.genlinearcmplist(hp : pcaserecord);

      var
         first : boolean;
         last : TConstExprInt;

      procedure genitem(t : pcaserecord);

        var
           l1 : tasmlabel;

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           if t^._low=t^._high then
             begin
                if opsize in [OS_S64,OS_64] then
                  begin
                     objectlibrary.getlabel(l1);
{$ifdef Delphi}
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_NE, hi((t^._low)),hregister2,l1);
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_EQ, lo((t^._low)),hregister, t^.statement);
{$else}
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_NE, aword(hi(int64(t^._low))),hregister2,l1);
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_EQ, aword(lo(int64(t^._low))),hregister, t^.statement);
{$endif}
                     cg.a_label(exprasmlist,l1);
                  end
                else
                  begin
                     cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_EQ, aword(t^._low),hregister, t^.statement);
                     last:=t^._low;
                  end;
             end
           else
             begin
                { it begins with the smallest label, if the value }
                { is even smaller then jump immediately to the    }
                { ELSE-label                                }
                if first or (t^._low-last>1) then
                  begin
                     if opsize in [OS_64,OS_S64] then
                       begin
                          objectlibrary.getlabel(l1);
{$ifdef Delphi}
                          cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_lt, aword(hi((t^._low))),
                               hregister2, elselabel);
                          cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_gt, aword(hi((t^._low))),
                               hregister2, l1);
                          { the comparisation of the low dword must be always unsigned! }
                          cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_B, aword(lo((t^._low))), hregister, elselabel);
{$else}
                          cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_lt, aword(hi(int64(t^._low))),
                               hregister2, elselabel);
                          cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_gt, aword(hi(int64(t^._low))),
                               hregister2, l1);
                          { the comparisation of the low dword must be always unsigned! }
                          cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_B, aword(lo(int64(t^._low))), hregister, elselabel);
{$endif}
                          cg.a_label(exprasmlist,l1);
                       end
                     else
                       begin
                        cg.a_cmp_const_reg_label(exprasmlist, opsize, jmp_lt, aword(t^._low), hregister,
                           elselabel);
                       end;
                  end;

                if opsize in [OS_S64,OS_64] then
                  begin
                     objectlibrary.getlabel(l1);
{$ifdef Delphi}
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_lt, aword(hi(t^._high)), hregister2,
                           t^.statement);
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_gt, aword(hi(t^._high)), hregister2,
                           l1);
                    cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_BE, aword(lo(t^._high)), hregister, t^.statement);
{$else}
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_lt, aword(hi(int64(t^._high))), hregister2,
                           t^.statement);
                     cg.a_cmp_const_reg_label(exprasmlist, OS_32, jmp_gt, aword(hi(int64(t^._high))), hregister2,
                           l1);
                    cg.a_cmp_const_reg_label(exprasmlist, OS_32, OC_BE, aword(lo(int64(t^._high))), hregister, t^.statement);
{$endif}
                    cg.a_label(exprasmlist,l1);
                  end
                else
                  begin
                     cg.a_cmp_const_reg_label(exprasmlist, opsize, jmp_le, aword(t^._high), hregister, t^.statement);
                  end;

                last:=t^._high;
             end;
           first:=false;
           if assigned(t^.greater) then
             genitem(t^.greater);
        end;

      begin
         last:=0;
         first:=true;
         genitem(hp);
         cg.a_jmp_always(exprasmlist,elselabel);
      end;


    procedure tcgcasenode.gentreejmp(p : pcaserecord);
      var
         lesslabel,greaterlabel : tasmlabel;
      begin
        cg.a_label(exprasmlist,p^._at);
        { calculate labels for left and right }
        if (p^.less=nil) then
          lesslabel:=elselabel
        else
          lesslabel:=p^.less^._at;
        if (p^.greater=nil) then
          greaterlabel:=elselabel
        else
          greaterlabel:=p^.greater^._at;
        { calculate labels for left and right }
        { no range label: }
        if p^._low=p^._high then
          begin
             if greaterlabel=lesslabel then
               begin
                 cg.a_cmp_const_reg_label(exprasmlist, opsize, OC_NE,p^._low,hregister, lesslabel);
               end
             else
               begin
                 cg.a_cmp_const_reg_label(exprasmlist,opsize, jmp_lt,p^._low,hregister, lesslabel);
                 cg.a_cmp_const_reg_label(exprasmlist,opsize, jmp_gt,p^._low,hregister, greaterlabel);
               end;
             cg.a_jmp_always(exprasmlist,p^.statement);
          end
        else
          begin
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_lt,p^._low, hregister, lesslabel);
             cg.a_cmp_const_reg_label(exprasmlist,opsize,jmp_gt,p^._high,hregister, greaterlabel);
             cg.a_jmp_always(exprasmlist,p^.statement);
          end;
         if assigned(p^.less) then
          gentreejmp(p^.less);
         if assigned(p^.greater) then
          gentreejmp(p^.greater);
      end;


    procedure ReLabel(var p:tasmsymbol);
      begin
        if p.defbind = AB_LOCAL then
         begin
           if not assigned(p.altsymbol) then
             objectlibrary.GenerateAltSymbol(p);
           p:=p.altsymbol;
           p.increfs;
         end;
      end;


    procedure relabelcaserecord(p : pcaserecord);
      begin
         Relabel(p^.statement);
         Relabel(p^._at);
         if assigned(p^.greater) then
           relabelcaserecord(p^.greater);
         if assigned(p^.less) then
           relabelcaserecord(p^.less);
      end;


    procedure tcgcasenode.pass_2;
      var
         lv,hv,
         max_label: tconstexprint;
         labels : longint;
         max_linear_list : longint;
         otl, ofl: tasmlabel;
         isjump : boolean;
         max_dist,
         dist : cardinal;
         hp : tstatementnode;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         { Relabel for inlining? }
         if inlining_procedure and assigned(nodes) then
          begin
            objectlibrary.CreateUsedAsmSymbolList;
            relabelcaserecord(nodes);
          end;

         objectlibrary.getlabel(endlabel);
         objectlibrary.getlabel(elselabel);
         with_sign:=is_signed(left.resulttype.def);
         if with_sign then
           begin
              jmp_gt:=OC_GT;
              jmp_lt:=OC_LT;
              jmp_le:=OC_LTE;
           end
         else
            begin
              jmp_gt:=OC_A;
              jmp_lt:=OC_B;
              jmp_le:=OC_BE;
           end;
        {$ifndef newra}
         rg.cleartempgen;
        {$endif}
         { save current truelabel and falselabel }
         isjump:=false;
         if left.location.loc=LOC_JUMP then
          begin
            otl:=truelabel;
            objectlibrary.getlabel(truelabel);
            ofl:=falselabel;
            objectlibrary.getlabel(falselabel);
            isjump:=true;
          end;
         secondpass(left);
         { determines the size of the operand }
         opsize:=def_cgsize(left.resulttype.def);
         { copy the case expression to a register }
         location_force_reg(exprasmlist,left.location,opsize,false);
         if opsize in [OS_S64,OS_64] then
          begin
            hregister:=left.location.registerlow;
            hregister2:=left.location.registerhigh;
          end
         else
          hregister:=left.location.register;
         if isjump then
          begin
            truelabel:=otl;
            falselabel:=ofl;
          end;

         { we need the min_label always to choose between }
         { cmps and subs/decs                             }
         min_label:=case_get_min(nodes);

         load_all_regvars(exprasmlist);
         { now generate the jumps }
         if opsize in [OS_64,OS_S64] then
           genlinearcmplist(nodes)
         else
           begin
              if cs_optimize in aktglobalswitches then
                begin
                   { procedures are empirically passed on }
                   { consumption can also be calculated   }
                   { but does it pay on the different     }
                   { processors?                       }
                   { moreover can the size only be appro- }
                   { ximated as it is not known if rel8,  }
                   { rel16 or rel32 jumps are used   }
                   max_label:=case_get_max(nodes);
                   labels:=case_count_labels(nodes);
                   { can we omit the range check of the jump table ? }
                   getrange(left.resulttype.def,lv,hv);
                   jumptable_no_range:=(lv=min_label) and (hv=max_label);
                   { hack a little bit, because the range can be greater }
                   { than the positive range of a longint            }

                   if (min_label<0) and (max_label>0) then
                     begin
                        if min_label=TConstExprInt($80000000) then
                          dist:=Cardinal(max_label)+Cardinal($80000000)
                        else
                          dist:=Cardinal(max_label)+Cardinal(-min_label)
                     end
                   else
                     dist:=max_label-min_label;

                   { optimize for size ? }
                   if cs_littlesize in aktglobalswitches  then
                     begin
                       if (has_jumptable) and
                          not((labels<=2) or
                              ((max_label-min_label)<0) or
                              ((max_label-min_label)>3*labels)) then
                         begin
                           { if the labels less or more a continuum then }
                           genjumptable(nodes,min_label,max_label);
                         end
                       else
                         begin
                           { a linear list is always smaller than a jump tree }
                           genlinearlist(nodes);
                         end;
                     end
                   else
                     begin
                        max_dist:=4*cardinal(labels);
                        if jumptable_no_range then
                          max_linear_list:=4
                        else
                          max_linear_list:=2;

                        { allow processor specific values }
                        optimizevalues(max_linear_list,max_dist);

                        if (labels<=max_linear_list) then
                          genlinearlist(nodes)
                        else
                          begin
                            if (has_jumptable) and
                               (dist<max_dist) then
                              genjumptable(nodes,min_label,max_label)
                            else
                              begin
                                 if labels>16 then
                                   gentreejmp(nodes)
                                 else
                                   genlinearlist(nodes);
                              end;
                          end;
                     end;
                end
              else
                { it's always not bad }
                genlinearlist(nodes);
           end;

         rg.ungetregisterint(exprasmlist,hregister);

         { now generate the instructions }
         hp:=tstatementnode(right);
         while assigned(hp) do
           begin
            {$ifndef newra}
              rg.cleartempgen;
            {$endif}
              { relabel when inlining }
              if inlining_procedure then
                begin
                  if hp.left.nodetype<>labeln then
                    internalerror(200211261);
                  Relabel(tlabelnode(hp.left).labelnr);
                end;
              secondpass(hp.left);
              { don't come back to case line }
              aktfilepos:=exprasmList.getlasttaifilepos^;
              load_all_regvars(exprasmlist);
              cg.a_jmp_always(exprasmlist,endlabel);
              hp:=tstatementnode(hp.right);
           end;
         cg.a_label(exprasmlist,elselabel);
         { ...and the else block }
         if assigned(elseblock) then
           begin
            {$ifndef newra}
              rg.cleartempgen;
            {$endif}
              secondpass(elseblock);
              load_all_regvars(exprasmlist);
           end;
         cg.a_label(exprasmlist,endlabel);

         { Remove relabels for inlining }
         if inlining_procedure and
            assigned(nodes) then
          begin
             { restore used symbols }
             objectlibrary.UsedAsmSymbolListResetAltSym;
             objectlibrary.DestroyUsedAsmSymbolList;
          end;
      end;


begin
   csetelementnode:=tcgsetelementnode;
   cinnode:=tcginnode;
   ccasenode:=tcgcasenode;
end.
{
  $Log$
  Revision 1.40  2003-06-03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.39  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.38  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.37  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.36  2003/05/24 19:48:49  jonas
    * fixed tcginnode endian bug again, but correcty this time :)

  Revision 1.35  2003/05/23 21:10:50  florian
    * fixed sparc compiler compilation

  Revision 1.34  2003/05/23 19:52:28  jonas
    * corrected fix for endian differences in tcginnode

  Revision 1.33  2003/05/17 19:17:35  jonas
    * fixed size setting of result location of innodes

  Revision 1.32  2003/05/01 12:26:50  jonas
    * fixed endian issue in inlined in-test for smallsets
    * pass the address of normalsets to fpc_set_in_set_byte instead of the
      contents of the first 4 bytes

  Revision 1.31  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.30  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.29  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.28  2003/04/22 12:45:58  florian
    * fixed generic in operator code
    + added debug code to check if all scratch registers are released

  Revision 1.27  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.26  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.25  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.24  2002/11/27 02:37:13  peter
    * case statement inlining added
    * fixed inlining of write()
    * switched statementnode left and right parts so the statements are
      processed in the correct order when getcopy is used. This is
      required for tempnodes

  Revision 1.23  2002/11/25 17:43:18  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.22  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.21  2002/10/03 21:31:10  carl
    * range check error fixes

  Revision 1.20  2002/09/17 18:54:03  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.19  2002/09/16 18:08:26  peter
    * fix last optimization in genlinearlist, detected by bug tw1066
    * use generic casenode.pass2 routine and override genlinearlist
    * add jumptable support to generic casenode, by default there is
      no jumptable support

  Revision 1.18  2002/08/15 15:11:53  carl
    * oldset define is now correct for all cpu's except i386
    * correct compilation problems because of the above

  Revision 1.17  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.16  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.15  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.14  2002/08/11 11:37:42  jonas
    * genlinear(cmp)list can now be overridden by descendents

  Revision 1.13  2002/08/11 06:14:40  florian
    * fixed powerpc compilation problems

  Revision 1.12  2002/08/10 17:15:12  jonas
    * optimizations and bugfix

  Revision 1.11  2002/07/28 09:24:18  carl
  + generic case node

  Revision 1.10  2002/07/23 14:31:00  daniel
  * Added internal error when asked to generate code for 'if expr in []'

  Revision 1.9  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.8  2002/07/22 11:48:04  daniel
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

