{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for math nodes

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
unit n386mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,nx86mat;

    type
      ti386moddivnode = class(tmoddivnode)
         procedure pass_2;override;
      end;

      ti386shlshrnode = class(tshlshrnode)
         procedure pass_2;override;
         { everything will be handled in pass_2 }
         function first_shlshr64bitint: tnode; override;
      end;

      ti386unaryminusnode = class(tx86unaryminusnode)
      end;

      ti386notnode = class(tx86notnode)
      end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,defutil,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,ncgutil,cgobj,cgutils;

{*****************************************************************************
                             TI386MODDIVNODE
*****************************************************************************}

    function log2(i : dword) : dword;
      begin
        result:=0;
        i:=i shr 1;
        while i<>0 do
          begin
            i:=i shr 1;
            inc(result);
          end;
      end;


   procedure ti386moddivnode.pass_2;
      var
        hreg1,hreg2:Tregister;
        power:longint;
        hl:Tasmlabel;
        op:Tasmop;
        e : longint;
        d,l,s,m,a : dword;
        m_low,m_high,j,k : qword;
      begin
        secondpass(left);
        if codegenerror then
          exit;
        secondpass(right);
        if codegenerror then
          exit;

        if is_64bitint(resulttype.def) then
          { should be handled in pass_1 (JM) }
          internalerror(200109052);
        { put numerator in register }
        location_reset(location,LOC_REGISTER,OS_INT);
        location_force_reg(exprasmlist,left.location,OS_INT,false);
        hreg1:=left.location.register;

        if (nodetype=divn) and (right.nodetype=ordconstn) then
          begin
            if ispowerof2(tordconstnode(right).value,power) then
              begin
                { for signed numbers, the numerator must be adjusted before the
                  shift instruction, but not wih unsigned numbers! Otherwise,
                  "Cardinal($ffffffff) div 16" overflows! (JM) }
                if is_signed(left.resulttype.def) Then
                  begin
                    if (aktOptProcessor <> class386) and
                       not(cs_littlesize in aktglobalswitches) then
                      { use a sequence without jumps, saw this in
                        comp.compilers (JM) }
                      begin
                        { no jumps, but more operations }
                        hreg2:=cg.getintregister(exprasmlist,OS_INT);
                        emit_reg_reg(A_MOV,S_L,hreg1,hreg2);
                        {If the left value is signed, hreg2=$ffffffff, otherwise 0.}
                        emit_const_reg(A_SAR,S_L,31,hreg2);
                        {If signed, hreg2=right value-1, otherwise 0.}
                        emit_const_reg(A_AND,S_L,tordconstnode(right).value-1,hreg2);
                        { add to the left value }
                        emit_reg_reg(A_ADD,S_L,hreg2,hreg1);
                        { do the shift }
                        emit_const_reg(A_SAR,S_L,power,hreg1);
                      end
                    else
                      begin
                        { a jump, but less operations }
                        emit_reg_reg(A_TEST,S_L,hreg1,hreg1);
                        objectlibrary.getjumplabel(hl);
                        cg.a_jmp_flags(exprasmlist,F_NS,hl);
                        if power=1 then
                          emit_reg(A_INC,S_L,hreg1)
                        else
                          emit_const_reg(A_ADD,S_L,tordconstnode(right).value-1,hreg1);
                        cg.a_label(exprasmlist,hl);
                        emit_const_reg(A_SAR,S_L,power,hreg1);
                      end
                  end
                else
                  emit_const_reg(A_SHR,S_L,power,hreg1);
                location.register:=hreg1;
              end
            else
              begin
                e:=tordconstnode(right).value;
                d:=abs(e);
                { Determine algorithm (a), multiplier (m), and shift factor (s) for 32-bit
                  signed integer division. Based on: Granlund, T.; Montgomery, P.L.:
                  "Division by Invariant Integers using Multiplication". SIGPLAN Notices,
                  Vol. 29, June 1994, page 61.
                }

                l:=log2(d);
                j:=qword($80000000) mod qword(d);
                k:=(qword(1) shl (32+l)) div (qword($80000000-j));
                m_low:=((qword(1)) shl (32+l)) div d;
                m_high:=(((qword(1)) shl (32+l)) + k) div d;
                while ((m_low shr 1) < (m_high shr 1)) and (l > 0) do
                  begin
                    m_low:=m_low shr 1;
                    m_high:=m_high shr 1;
                    dec(l);
                  end;
                m:=m_high;
                s:=l;
                if (m_high shr 31)<>0 then
                  a:=1
                else
                  a:=0;
                cg.getcpuregister(exprasmlist,NR_EAX);
                emit_const_reg(A_MOV,S_L,m,NR_EAX);
                cg.getcpuregister(exprasmlist,NR_EDX);
                emit_reg(A_IMUL,S_L,hreg1);
                emit_reg_reg(A_MOV,S_L,hreg1,NR_EAX);
                if a<>0 then
                  begin
                    emit_reg_reg(A_ADD,S_L,NR_EAX,NR_EDX);
                  {
                    printf ("; dividend: memory location or register other than EAX or EDX\n");
                    printf ("\n");
                    printf ("MOV EAX, 0%08LXh\n", m);
                    printf ("IMUL dividend\n");
                    printf ("MOV EAX, dividend\n");
                    printf ("ADD EDX, EAX\n");
                    if (s) printf ("SAR EDX, %d\n", s);
                    printf ("SHR EAX, 31\n");
                    printf ("ADD EDX, EAX\n");
                    if (e < 0) printf ("NEG EDX\n");
                    printf ("\n");
                    printf ("; quotient now in EDX\n");
                  }
                  end;
                {
                  printf ("; dividend: memory location of register other than EAX or EDX\n");
                  printf ("\n");
                  printf ("MOV EAX, 0%08LXh\n", m);
                  printf ("IMUL dividend\n");
                  printf ("MOV EAX, dividend\n");
                  if (s) printf ("SAR EDX, %d\n", s);
                  printf ("SHR EAX, 31\n");
                  printf ("ADD EDX, EAX\n");
                  if (e < 0) printf ("NEG EDX\n");
                  printf ("\n");
                  printf ("; quotient now in EDX\n");
                }
                if s<>0 then
                  emit_const_reg(A_SAR,S_L,s,NR_EDX);
                emit_const_reg(A_SHR,S_L,31,NR_EAX);
                emit_reg_reg(A_ADD,S_L,NR_EAX,NR_EDX);
                if e<0 then
                  emit_reg(A_NEG,S_L,NR_EDX);
                cg.ungetcpuregister(exprasmlist,NR_EDX);
                cg.ungetcpuregister(exprasmlist,NR_EAX);
                location.register:=cg.getintregister(exprasmlist,OS_INT);
                cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,NR_EDX,location.register)
              end;
          end
        else
          begin
            cg.getcpuregister(exprasmlist,NR_EAX);
            emit_reg_reg(A_MOV,S_L,hreg1,NR_EAX);
            cg.getcpuregister(exprasmlist,NR_EDX);
            {Sign extension depends on the left type.}
            if torddef(left.resulttype.def).typ=u32bit then
              emit_reg_reg(A_XOR,S_L,NR_EDX,NR_EDX)
            else
              emit_none(A_CDQ,S_NO);

            {Division depends on the right type.}
            if Torddef(right.resulttype.def).typ=u32bit then
              op:=A_DIV
            else
              op:=A_IDIV;

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              emit_ref(op,S_L,right.location.reference)
            else if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              emit_reg(op,S_L,right.location.register)
            else
              begin
                hreg1:=cg.getintregister(exprasmlist,right.location.size);
                cg.a_load_loc_reg(exprasmlist,OS_32,right.location,hreg1);
                emit_reg(op,S_L,hreg1);
              end;

            {Copy the result into a new register. Release EAX & EDX.}
            cg.ungetcpuregister(exprasmlist,NR_EDX);
            cg.ungetcpuregister(exprasmlist,NR_EAX);
            location.register:=cg.getintregister(exprasmlist,OS_INT);
            if nodetype=divn then
              cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,NR_EAX,location.register)
            else
              cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,NR_EDX,location.register);
          end;
      end;


{*****************************************************************************
                             TI386SHLRSHRNODE
*****************************************************************************}


    function ti386shlshrnode.first_shlshr64bitint: tnode;

    begin
      result := nil;
    end;

    procedure ti386shlshrnode.pass_2;

    var hreg64hi,hreg64lo:Tregister;
        op:Tasmop;
        v : TConstExprInt;
        l1,l2,l3:Tasmlabel;

    begin
      secondpass(left);
      secondpass(right);

      { determine operator }
      if nodetype=shln then
        op:=A_SHL
      else
        op:=A_SHR;

      if is_64bitint(left.resulttype.def) then
        begin
          location_reset(location,LOC_REGISTER,OS_64);

          { load left operator in a register }
          location_force_reg(exprasmlist,left.location,OS_64,false);
          hreg64hi:=left.location.register64.reghi;
          hreg64lo:=left.location.register64.reglo;

          { shifting by a constant directly coded: }
          if (right.nodetype=ordconstn) then
            begin
              v:=Tordconstnode(right).value and 63;
              if v>31 then
                begin
                  if nodetype=shln then
                    begin
                      emit_reg_reg(A_XOR,S_L,hreg64hi,hreg64hi);
                      if ((v and 31) <> 0) then
                        emit_const_reg(A_SHL,S_L,v and 31,hreg64lo);
                    end
                  else
                    begin
                      emit_reg_reg(A_XOR,S_L,hreg64lo,hreg64lo);
                      if ((v and 31) <> 0) then
                        emit_const_reg(A_SHR,S_L,v and 31,hreg64hi);
                    end;
                  location.register64.reghi:=hreg64lo;
                  location.register64.reglo:=hreg64hi;
                end
              else
                begin
                  if nodetype=shln then
                    begin
                      emit_const_reg_reg(A_SHLD,S_L,v and 31,hreg64lo,hreg64hi);
                      emit_const_reg(A_SHL,S_L,v and 31,hreg64lo);
                    end
                  else
                    begin
                      emit_const_reg_reg(A_SHRD,S_L,v and 31,hreg64hi,hreg64lo);
                      emit_const_reg(A_SHR,S_L,v and 31,hreg64hi);
                    end;
                  location.register64.reglo:=hreg64lo;
                  location.register64.reghi:=hreg64hi;
                end;
            end
          else
            begin
              { load right operators in a register }
              cg.getcpuregister(exprasmlist,NR_ECX);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,NR_ECX);

              { left operator is already in a register }
              { hence are both in a register }
              { is it in the case ECX ? }

              { the damned shift instructions work only til a count of 32 }
              { so we've to do some tricks here                           }
              objectlibrary.getjumplabel(l1);
              objectlibrary.getjumplabel(l2);
              objectlibrary.getjumplabel(l3);
              emit_const_reg(A_CMP,S_L,64,NR_ECX);
              cg.a_jmp_flags(exprasmlist,F_L,l1);
              emit_reg_reg(A_XOR,S_L,hreg64lo,hreg64lo);
              emit_reg_reg(A_XOR,S_L,hreg64hi,hreg64hi);
              cg.a_jmp_always(exprasmlist,l3);
              cg.a_label(exprasmlist,l1);
              emit_const_reg(A_CMP,S_L,32,NR_ECX);
              cg.a_jmp_flags(exprasmlist,F_L,l2);
              emit_const_reg(A_SUB,S_L,32,NR_ECX);
              if nodetype=shln then
                begin
                  emit_reg_reg(A_SHL,S_L,NR_CL,hreg64lo);
                  emit_reg_reg(A_MOV,S_L,hreg64lo,hreg64hi);
                  emit_reg_reg(A_XOR,S_L,hreg64lo,hreg64lo);
                  cg.a_jmp_always(exprasmlist,l3);
                  cg.a_label(exprasmlist,l2);
                  emit_reg_reg_reg(A_SHLD,S_L,NR_CL,hreg64lo,hreg64hi);
                  emit_reg_reg(A_SHL,S_L,NR_CL,hreg64lo);
                end
              else
                begin
                  emit_reg_reg(A_SHR,S_L,NR_CL,hreg64hi);
                  emit_reg_reg(A_MOV,S_L,hreg64hi,hreg64lo);
                  emit_reg_reg(A_XOR,S_L,hreg64hi,hreg64hi);
                  cg.a_jmp_always(exprasmlist,l3);
                  cg.a_label(exprasmlist,l2);
                  emit_reg_reg_reg(A_SHRD,S_L,NR_CL,hreg64hi,hreg64lo);
                  emit_reg_reg(A_SHR,S_L,NR_CL,hreg64hi);
                end;
              cg.a_label(exprasmlist,l3);

              cg.ungetcpuregister(exprasmlist,NR_ECX);
              location.register64.reglo:=hreg64lo;
              location.register64.reghi:=hreg64hi;
            end;
        end
      else
        begin
          { load left operators in a register }
          location_copy(location,left.location);
          location_force_reg(exprasmlist,location,OS_INT,false);

          { shifting by a constant directly coded: }
          if (right.nodetype=ordconstn) then
            { l shl 32 should 0 imho, but neither TP nor Delphi do it in this way (FK)}
            emit_const_reg(op,S_L,tordconstnode(right).value and 31,location.register)
          else
            begin
              { load right operators in a ECX }
              cg.getcpuregister(exprasmlist,NR_ECX);
              cg.a_load_loc_reg(exprasmlist,OS_32,right.location,NR_ECX);

              { right operand is in ECX }
              cg.ungetcpuregister(exprasmlist,NR_ECX);
              emit_reg_reg(op,S_L,NR_CL,location.register);
            end;
        end;
    end;


begin
   cunaryminusnode:=ti386unaryminusnode;
   cmoddivnode:=ti386moddivnode;
   cshlshrnode:=ti386shlshrnode;
   cnotnode:=ti386notnode;
end.
