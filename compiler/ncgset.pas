{
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate generic assembler for in set/case labels

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
       globtype,globals,
       node,nset,cpubase,cgbase,cgobj,aasmbase,aasmtai,aasmdata;

    type
       tcgsetelementnode = class(tsetelementnode)
          procedure pass_generate_code;override;
       end;


       Tsetpart=record
         range : boolean;      {Part is a range.}
         start,stop : byte;    {Start/stop when range; Stop=element when an element.}
       end;
       Tsetparts=array[1..8] of Tsetpart;

       tcginnode = class(tinnode)
          function pass_1: tnode;override;
          procedure pass_generate_code;override;
       protected
          {# Routine to test bitnumber in bitnumber register on value
             in value register. The __result register should be set
             to one if the bit is set, otherwise __result register
             should be set to zero.

             Should be overriden on processors which have specific
             instructions to do bit tests.
          }

          procedure emit_bit_test_reg_reg(list : TAsmList;
                                          bitsize: tcgsize; bitnumber,value : tregister;
                                          ressize: tcgsize; res :tregister);virtual;
          function checkgenjumps(out setparts: Tsetparts; out numparts: byte; out use_small: boolean): boolean; virtual;
          function analizeset(const Aset:Tconstset;out setparts: Tsetparts; out numparts: byte;is_small:boolean):boolean;virtual;
       end;

       tcgcasenode = class(tcasenode)
          {
            Emits the case node statement. Contrary to the intel
            80x86 version, this version does not emit jump tables,
            because of portability problems.
          }
          procedure pass_generate_code;override;

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

          function  blocklabel(id:longint):tasmlabel;
          procedure optimizevalues(var max_linear_list:aint;var max_dist:aword);virtual;
          function  has_jumptable : boolean;virtual;
          procedure genjumptable(hp : pcaselabel;min_,max_ : aint); virtual;
          procedure genlinearlist(hp : pcaselabel); virtual;
          procedure genlinearcmplist(hp : pcaselabel); virtual;
       end;


implementation

    uses
      systems,
      verbose,
      symconst,symdef,defutil,
      paramgr,
      procinfo,pass_2,tgobj,
      nbas,ncon,nflw,
      ncgutil,regvars,
      cgutils;


{*****************************************************************************
                          TCGSETELEMENTNODE
*****************************************************************************}

    procedure tcgsetelementnode.pass_generate_code;
       begin
       { load first value in 32bit register }
         secondpass(left);
         if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
           location_force_reg(current_asmdata.CurrAsmList,left.location,OS_32,false);

       { also a second value ? }
         if assigned(right) then
           begin
             secondpass(right);
             if codegenerror then
               exit;
             if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              location_force_reg(current_asmdata.CurrAsmList,right.location,OS_32,false);
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
  procedure tcginnode.emit_bit_test_reg_reg(list : TAsmList;
                                            bitsize: tcgsize; bitnumber,value : tregister;
                                            ressize: tcgsize; res :tregister);
    begin
      { first make sure that the bit number is modulo 32 }

      { not necessary, since if it's > 31, we have a range error -> will }
      { be caught when range checking is on! (JM)                        }
      { cg.a_op_const_reg(list,OP_AND,31,bitnumber);                     }

      if bitsize<>ressize then
        begin
          { FIX ME! We're not allowed to modify the value register here! }

          { shift value register "bitnumber" bits to the right }
          cg.a_op_reg_reg(list,OP_SHR,bitsize,bitnumber,value);
          { extract the bit we want }
          cg.a_op_const_reg(list,OP_AND,bitsize,1,value);
          cg.a_load_reg_reg(list,bitsize,ressize,value,res);
        end
      else
        begin
          { rotate value register "bitnumber" bits to the right }
          cg.a_op_reg_reg_reg(list,OP_SHR,bitsize,bitnumber,value,res);
          { extract the bit we want }
          cg.a_op_const_reg(list,OP_AND,bitsize,1,res);
        end;
    end;


  function tcginnode.analizeset(const Aset:Tconstset; out setparts:tsetparts; out numparts: byte; is_small:boolean):boolean;
    var
      compares,maxcompares:word;
      i:byte;
    begin
      analizeset:=false;
      numparts:=0;
      compares:=0;
      { Lots of comparisions take a lot of time, so do not allow
        too much comparisions. 8 comparisions are, however, still
        smalller than emitting the set }
      if cs_opt_size in current_settings.optimizerswitches then
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
                  { there's only one compare per range anymore. Only a }
                  { sub is added, but that's much faster than a        }
                  { cmp/jcc combo so neglect its effect                }
{                  inc(compares);
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


    function tcginnode.checkgenjumps(out setparts: Tsetparts; out numparts: byte;out use_small: boolean): boolean;
      begin
         { check if we can use smallset operation using btl which is limited
           to 32 bits, the left side may also not contain higher values !! }
         use_small:=(tsetdef(right.resultdef).settype=smallset) and
                    ((left.resultdef.typ=orddef) and (torddef(left.resultdef).high<=32) or
                     (left.resultdef.typ=enumdef) and (tenumdef(left.resultdef).max<=32));

         { Can we generate jumps? Possible for all types of sets }
         checkgenjumps:=(right.nodetype=setconstn) and
                   analizeset(Tsetconstnode(right).value_set^,setparts,numparts,use_small);
      end;


    function tcginnode.pass_1: tnode;
      var
        setparts: Tsetparts;
        numparts: byte;
        use_small: boolean;
      begin
        result := inherited pass_1;
        if not(assigned(result)) and
          checkgenjumps(setparts,numparts,use_small) then
          expectloc := LOC_JUMP;
      end;

    procedure tcginnode.pass_generate_code;
       var
         adjustment : aint;
         href : treference;
         hr,hr2,
         pleftreg   : tregister;
         setparts   : Tsetparts;
         opsize     : tcgsize;
         genjumps,
         use_small  : boolean;
         i,numparts : byte;

       begin
         { We check first if we can generate jumps, this can be done
           because the resultdef is already set in firstpass }

         genjumps := checkgenjumps(setparts,numparts,use_small);

         opsize:=OS_32;

         { calculate both operators }
         { the complex one first }
         firstcomplex(self);
         secondpass(left);
         { Only process the right if we are not generating jumps }
         if not genjumps then
           secondpass(right);
         if codegenerror then
           exit;

         { ofcourse not commutative }
         if nf_swapped in flags then
          swapleftright;

         if genjumps then
          begin
            { location is always LOC_JUMP }
            location_reset(location,LOC_JUMP,OS_NO);

            { If register is used, use only lower 8 bits }
            location_force_reg(current_asmdata.CurrAsmList,left.location,opsize,false);
            pleftreg := left.location.register;

            { how much have we already substracted from the x in the }
            { "x in [y..z]" expression                               }
            adjustment := 0;
            hr:=NR_NO;

            for i:=1 to numparts do
             if setparts[i].range then
              { use fact that a <= x <= b <=> aword(x-a) <= aword(b-a) }
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
                         (hr<>pleftreg) then
                        begin
                          { don't change this back to a_op_const_reg/a_load_reg_reg, since pleftreg must not be modified }
                          hr:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
                          cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,opsize,setparts[i].start,pleftreg,hr);
                          pleftreg:=hr;
                        end
                      else
                        begin
                          { otherwise, the value is already in a register   }
                          { that can be modified                            }
                          cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SUB,opsize,
                             setparts[i].start-adjustment,pleftreg)
                        end;
                    { new total value substracted from x:           }
                    { adjustment + (setparts[i].start - adjustment) }
                    adjustment := setparts[i].start;

                    { check if result < b-a+1 (not "result <= b-a", since }
                    { we need a carry in case the element is in the range }
                    { (this will never overflow since we check at the     }
                    { beginning whether stop-start <> 255)                }
                    cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, OC_B,
                      setparts[i].stop-setparts[i].start+1,pleftreg,current_procinfo.CurrTrueLabel);
                  end
                else
                  { if setparts[i].start = 0 and setparts[i].stop = 255,  }
                  { it's always true since "in" is only allowed for bytes }
                  begin
                    cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
                  end;
              end
             else
              begin
                { Emit code to check if left is an element }
                cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, OC_EQ,
                      setparts[i].stop-adjustment,pleftreg,current_procinfo.CurrTrueLabel);
              end;
             { To compensate for not doing a second pass }
             right.location.reference.symbol:=nil;
             cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
          end
         else
         {*****************************************************************}
         {                     NO JUMP TABLE GENERATION                    }
         {*****************************************************************}
          begin
            { location is always LOC_REGISTER }
            location_reset(location,LOC_REGISTER,def_cgsize(resultdef));

            { We will now generated code to check the set itself, no jmps,
              handle smallsets separate, because it allows faster checks }
            if use_small then
             begin
               {****************************  SMALL SET **********************}
               if left.nodetype=ordconstn then
                begin
                  location_force_reg(current_asmdata.CurrAsmList,right.location,opsize,true);
                  location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
                  { first SHR the register }
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,opsize,tordconstnode(left).value and 31,right.location.register,location.register);
                  { then extract the lowest bit }
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,opsize,1,location.register);
                end
               else
                begin
                  location_force_reg(current_asmdata.CurrAsmList,left.location,opsize,false);
                  location_force_reg(current_asmdata.CurrAsmList,right.location,opsize,false);
                  { allocate a register for the result }
                  location.register:=cg.getintregister(current_asmdata.CurrAsmList,location.size);
                  { emit bit test operation }
                  emit_bit_test_reg_reg(current_asmdata.CurrAsmList,left.location.size,left.location.register,
                      right.location.register,location.size,location.register);
                end;
             end
            else
             {************************** NOT SMALL SET ********************}
             begin
               if right.location.loc=LOC_CONSTANT then
                begin
                  { can it actually occur currently? CEC }
                  { yes: "if bytevar in [1,3,5,7,9,11,13,15]" (JM) }

                  { note: this code assumes that left in [0..255], which is a valid }
                  { assumption (other cases will be caught by range checking) (JM)  }

                  { load left in register }
                  location_force_reg(current_asmdata.CurrAsmList,left.location,opsize,true);
                  if left.location.loc = LOC_CREGISTER then
                    hr := cg.getintregister(current_asmdata.CurrAsmList,opsize)
                  else
                    hr := left.location.register;
                  { load right in register }
                  hr2:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
                  cg.a_load_const_reg(current_asmdata.CurrAsmList,opsize,right.location.value,hr2);

                  { emit bit test operation }
                  emit_bit_test_reg_reg(current_asmdata.CurrAsmList,left.location.size,left.location.register,hr2,opsize,hr2);

                  { if left > 31 then hr := 0 else hr := $ffffffff }
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,opsize,32,left.location.register,hr);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,opsize,31,hr);

                  { if left > 31, then result := 0 else result := result of bit test }
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_AND,opsize,hr,hr2);
                  { allocate a register for the result }
                  location.register := cg.getintregister(current_asmdata.CurrAsmList,location.size);
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,location.size,hr2,location.register);
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
                  { allocate a register for the result }
                  location.register := cg.getintregister(current_asmdata.CurrAsmList,location.size);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_8,location.size,right.location.reference, location.register);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,location.size,tordconstnode(left).value and 7,
                    location.register);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,location.size,1,location.register);
                end
               else
                begin
                  location_force_reg(current_asmdata.CurrAsmList,left.location,OS_INT,true);
                  pleftreg := left.location.register;

                  location_freetemp(current_asmdata.CurrAsmList,left.location);
                  hr := cg.getaddressregister(current_asmdata.CurrAsmList);
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_INT,5,pleftreg,hr);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHL,OS_INT,2,hr);

                  href := right.location.reference;
                  if (href.base = NR_NO) then
                    href.base := hr
                  else if (right.location.reference.index = NR_NO) then
                    href.index := hr
                  else
                    begin
                      hr2 := cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href, hr2);
                      reference_reset_base(href,hr2,0);
                      href.index := hr;
                    end;
                  { allocate a register for the result }
                  location.register := cg.getintregister(current_asmdata.CurrAsmList,opsize);
                  cg.a_load_ref_reg(current_asmdata.CurrAsmList,opsize,opsize,href,location.register);

                  hr := cg.getintregister(current_asmdata.CurrAsmList,opsize);
                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_AND,opsize,31,pleftreg,hr);
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_SHR,opsize,hr,location.register);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,opsize,1,location.register);
                end;
             end;
          end;
          location_freetemp(current_asmdata.CurrAsmList,right.location);
       end;

{*****************************************************************************
                            TCGCASENODE
*****************************************************************************}

    function tcgcasenode.blocklabel(id:longint):tasmlabel;
      begin
        if not assigned(blocks[id]) then
          internalerror(200411301);
        result:=pcaseblock(blocks[id])^.blocklabel;
      end;


    procedure tcgcasenode.optimizevalues(var max_linear_list:aint;var max_dist:aword);
      begin
        { no changes by default }
      end;


    function tcgcasenode.has_jumptable : boolean;
      begin
        { No jumptable support in the default implementation }
        has_jumptable:=false;
      end;


    procedure tcgcasenode.genjumptable(hp : pcaselabel;min_,max_ : aint);
      begin
        internalerror(200209161);
      end;


    procedure tcgcasenode.genlinearlist(hp : pcaselabel);

      var
         first : boolean;
         last : TConstExprInt;
         scratch_reg: tregister;

      procedure genitem(t : pcaselabel);

          procedure gensub(value:aint);
            begin
              { here, since the sub and cmp are separate we need
                to move the result before subtract to help
                the register allocator
              }
              cg.a_load_reg_reg(current_asmdata.CurrAsmList, opsize, opsize, hregister, scratch_reg);
              cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SUB, opsize, value, hregister);
            end;

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           { do we need to test the first value? }
           if first and (t^._low>get_min_value(left.resultdef)) then
             cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,jmp_lt,aint(t^._low),hregister,elselabel);
           if t^._low=t^._high then
             begin
                if t^._low-last=0 then
                  cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,OC_EQ,0,hregister,blocklabel(t^.blockid))
                else
                  begin
                      gensub(aint(t^._low-last));
                      cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,OC_EQ,aint(t^._low-last),scratch_reg,blocklabel(t^.blockid));
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
                     if (t^._low>get_min_value(left.resultdef)) then
                       gensub(aint(t^._low));
                  end
                else
                  begin
                    { if there is no unused label between the last and the }
                    { present label then the lower limit can be checked    }
                    { immediately. else check the range in between:       }
                    gensub(aint(t^._low-last));
                    cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize,jmp_lt,aint(t^._low-last),scratch_reg,elselabel);
                  end;
                gensub(aint(t^._high-t^._low));
                cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,opsize,jmp_le,aint(t^._high-t^._low),scratch_reg,blocklabel(t^.blockid));
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
              scratch_reg:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
              genitem(hp);
              cg.a_jmp_always(current_asmdata.CurrAsmList,elselabel);
           end;
      end;


    procedure tcgcasenode.genlinearcmplist(hp : pcaselabel);

      var
         last : TConstExprInt;
         lastwasrange: boolean;

      procedure genitem(t : pcaselabel);

{$ifndef cpu64bit}
        var
           l1 : tasmlabel;
{$endif cpu64bit}

        begin
           if assigned(t^.less) then
             genitem(t^.less);
           if t^._low=t^._high then
             begin
{$ifndef cpu64bit}
                if opsize in [OS_S64,OS_64] then
                  begin
                     current_asmdata.getjumplabel(l1);
                     cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, OC_NE, aint(hi(int64(t^._low))),hregister2,l1);
                     cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, OC_EQ, aint(lo(int64(t^._low))),hregister, blocklabel(t^.blockid));
                     cg.a_label(current_asmdata.CurrAsmList,l1);
                  end
                else
{$endif cpu64bit}
                  begin
                     cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, OC_EQ, aint(t^._low),hregister, blocklabel(t^.blockid));
                  end;
                { Reset last here, because we've only checked for one value and need to compare
                  for the next range both the lower and upper bound }
                lastwasrange := false;
             end
           else
             begin
                { it begins with the smallest label, if the value }
                { is even smaller then jump immediately to the    }
                { ELSE-label                                }
                if not lastwasrange or (t^._low-last>1) then
                  begin
{$ifndef cpu64bit}
                     if opsize in [OS_64,OS_S64] then
                       begin
                          current_asmdata.getjumplabel(l1);
                          cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, jmp_lt, aint(hi(int64(t^._low))),
                               hregister2, elselabel);
                          cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, jmp_gt, aint(hi(int64(t^._low))),
                               hregister2, l1);
                          { the comparisation of the low dword must be always unsigned! }
                          cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, OC_B, aint(lo(int64(t^._low))), hregister, elselabel);
                          cg.a_label(current_asmdata.CurrAsmList,l1);
                       end
                     else
{$endif cpu64bit}
                       begin
                        cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, jmp_lt, aint(t^._low), hregister,
                           elselabel);
                       end;
                  end;
{$ifndef cpu64bit}
                if opsize in [OS_S64,OS_64] then
                  begin
                     current_asmdata.getjumplabel(l1);
                     cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, jmp_lt, aint(hi(int64(t^._high))), hregister2,
                           blocklabel(t^.blockid));
                     cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, jmp_gt, aint(hi(int64(t^._high))), hregister2,
                           l1);
                    cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, OS_32, OC_BE, aint(lo(int64(t^._high))), hregister, blocklabel(t^.blockid));
                    cg.a_label(current_asmdata.CurrAsmList,l1);
                  end
                else
{$endif cpu64bit}
                  begin
                     cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList, opsize, jmp_le, aint(t^._high), hregister, blocklabel(t^.blockid));
                  end;

                last:=t^._high;
                lastwasrange := true;
             end;
           if assigned(t^.greater) then
             genitem(t^.greater);
        end;

      begin
         last:=0;
         lastwasrange:=false;
         genitem(hp);
         cg.a_jmp_always(current_asmdata.CurrAsmList,elselabel);
      end;


    procedure tcgcasenode.pass_generate_code;
      var
         oldflowcontrol: tflowcontrol;
         i : longint;
         lv,hv,
         max_label: tconstexprint;
         labelcnt : aint;
         max_linear_list : aint;
         otl, ofl: tasmlabel;
         isjump : boolean;
         max_dist,
         dist : aword;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         oldflowcontrol := flowcontrol;
         include(flowcontrol,fc_inflowcontrol);
         { Allocate labels }
         current_asmdata.getjumplabel(endlabel);
         current_asmdata.getjumplabel(elselabel);
         for i:=0 to blocks.count-1 do
           current_asmdata.getjumplabel(pcaseblock(blocks[i])^.blocklabel);

         with_sign:=is_signed(left.resultdef);
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

         { save current current_procinfo.CurrTrueLabel and current_procinfo.CurrFalseLabel }
         isjump:=false;
         if left.expectloc=LOC_JUMP then
          begin
            otl:=current_procinfo.CurrTrueLabel;
            current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
            ofl:=current_procinfo.CurrFalseLabel;
            current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
            isjump:=true;
          end;
         secondpass(left);
         { determines the size of the operand }
         opsize:=def_cgsize(left.resultdef);
         { copy the case expression to a register }
         location_force_reg(current_asmdata.CurrAsmList,left.location,opsize,false);
{$ifndef cpu64bit}
         if opsize in [OS_S64,OS_64] then
           begin
             hregister:=left.location.register64.reglo;
             hregister2:=left.location.register64.reghi;
           end
         else
{$endif cpu64bit}
           hregister:=left.location.register;
         if isjump then
          begin
            current_procinfo.CurrTrueLabel:=otl;
            current_procinfo.CurrFalseLabel:=ofl;
          end
         else
          if (left.location.loc=LOC_JUMP) then
            internalerror(2006050501);

         { we need the min_label always to choose between }
         { cmps and subs/decs                             }
         min_label:=case_get_min(labels);

         { Generate the jumps }
{$ifdef OLDREGVARS}
         load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
{$ifndef cpu64bit}
         if opsize in [OS_64,OS_S64] then
           genlinearcmplist(labels)
         else
{$endif cpu64bit}
           begin
              if cs_opt_level1 in current_settings.optimizerswitches then
                begin
                   { procedures are empirically passed on }
                   { consumption can also be calculated   }
                   { but does it pay on the different     }
                   { processors?                       }
                   { moreover can the size only be appro- }
                   { ximated as it is not known if rel8,  }
                   { rel16 or rel32 jumps are used   }
                   max_label:=case_get_max(labels);
                   labelcnt:=case_count_labels(labels);
                   { can we omit the range check of the jump table ? }
                   getrange(left.resultdef,lv,hv);
                   jumptable_no_range:=(lv=min_label) and (hv=max_label);
                   { hack a little bit, because the range can be greater }
                   { than the positive range of a aint            }

                   if (min_label<0) and (max_label>0) then
                     begin
                        if min_label=TConstExprInt(low(aint)) then
                          dist:=aword(max_label)+aword(low(aint))
                        else
                          dist:=aword(max_label)+aword(-min_label)
                     end
                   else
                     dist:=max_label-min_label;

                   { optimize for size ? }
                   if cs_opt_size in current_settings.optimizerswitches  then
                     begin
                       if has_jumptable and
                          not((labelcnt<=2) or
                              ((max_label-min_label)<0) or
                              ((max_label-min_label)>3*labelcnt)) then
                         begin
                           { if the labels less or more a continuum then }
                           genjumptable(labels,min_label,max_label);
                         end
                       else
                         begin
                           { a linear list is always smaller than a jump tree }
                           genlinearlist(labels);
                         end;
                     end
                   else
                     begin
                        max_dist:=4*labelcnt;
                        if jumptable_no_range then
                          max_linear_list:=4
                        else
                          max_linear_list:=2;

                        { allow processor specific values }
                        optimizevalues(max_linear_list,max_dist);

                        if (labelcnt<=max_linear_list) then
                          genlinearlist(labels)
                        else
                          begin
                            if (has_jumptable) and
                               (dist<max_dist) and
                               (min_label>=low(aint)) and
                               (max_label<=high(aint)) then
                              genjumptable(labels,min_label,max_label)
                            else
                              genlinearlist(labels);
                          end;
                     end;
                end
              else
                { it's always not bad }
                genlinearlist(labels);
           end;

         { generate the instruction blocks }
         for i:=0 to blocks.count-1 do
           begin
              cg.a_label(current_asmdata.CurrAsmList,pcaseblock(blocks[i])^.blocklabel);
              secondpass(pcaseblock(blocks[i])^.statement);
              { don't come back to case line }
              current_filepos:=current_asmdata.CurrAsmList.getlasttaifilepos^;
{$ifdef OLDREGVARS}
              load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
              cg.a_jmp_always(current_asmdata.CurrAsmList,endlabel);
           end;
         { ...and the else block }
         cg.a_label(current_asmdata.CurrAsmList,elselabel);
         if assigned(elseblock) then
           begin
              secondpass(elseblock);
{$ifdef OLDREGVARS}
              load_all_regvars(current_asmdata.CurrAsmList);
{$endif OLDREGVARS}
           end;
         cg.a_label(current_asmdata.CurrAsmList,endlabel);

         { Reset labels }
         for i:=0 to blocks.count-1 do
           pcaseblock(blocks[i])^.blocklabel:=nil;
         flowcontrol := oldflowcontrol + (flowcontrol - [fc_inflowcontrol]);
      end;


begin
   csetelementnode:=tcgsetelementnode;
   cinnode:=tcginnode;
   ccasenode:=tcgcasenode;
end.
