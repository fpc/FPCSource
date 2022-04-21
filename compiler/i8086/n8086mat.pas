{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i8086 assembler for math nodes

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
unit n8086mat;

{$i fpcdefs.inc}

interface

    uses
      node,nmat,ncgmat,nx86mat;

    type
      ti8086moddivnode = class(tmoddivnode)
         function use_moddiv32bit_helper: boolean;
         function first_moddivint: tnode; override;
         procedure pass_generate_code;override;
      end;

      ti8086shlshrnode = class(tx86shlshrnode)
         procedure second_64bit;override;
         function first_shlshr64bitint: tnode; override;
      end;

      ti8086unaryminusnode = class(tx86unaryminusnode)
      end;

      ti8086notnode = class(tx86notnode)
      end;


implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,aasmbase,aasmtai,aasmdata,aasmcpu,defutil,
      cgbase,pass_2,
      ncon,
      cpubase,cpuinfo,
      cga,ncgutil,cgobj,cgutils,
      hlcgobj;

{*****************************************************************************
                             ti8086moddivnode
*****************************************************************************}


    function ti8086moddivnode.use_moddiv32bit_helper: boolean;
      begin
        result:=is_32bit(left.resultdef) or
                is_64bit(left.resultdef) or
                is_32bit(right.resultdef) or
                is_64bit(right.resultdef);
      end;


    function ti8086moddivnode.first_moddivint: tnode;
      begin
        if use_moddiv32bit_helper then
          result:=inherited first_moddivint
        else
          result:=nil;
      end;


    function log2(i : word) : word;
      begin
        result:=0;
        i:=i shr 1;
        while i<>0 do
          begin
            i:=i shr 1;
            inc(result);
          end;
      end;


   procedure ti8086moddivnode.pass_generate_code;
      var
        hreg1,hreg2:Tregister;
        power:longint;
        hl:Tasmlabel;
        op:Tasmop;
        e : smallint;
        d,l,r,s,m,a,n,t : word;
        m_low,m_high,j,k : dword;
        invertsign: Boolean;
      begin
        secondpass(left);
        if codegenerror then
          exit;
        secondpass(right);
        if codegenerror then
          exit;

        if is_64bitint(resultdef) or is_32bitint(resultdef) then
          { should be handled in pass_1 (JM) }
          internalerror(200109052);
        { put numerator in register }
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
        hreg1:=left.location.register;

        if (nodetype=divn) and (right.nodetype=ordconstn) then
          begin
            if isabspowerof2(tordconstnode(right).value,power) then
              begin
                { for signed numbers, the numerator must be adjusted before the
                  shift instruction, but not wih unsigned numbers! Otherwise,
                  "Cardinal($ffffffff) div 16" overflows! (JM) }
                if is_signed(left.resultdef) Then
                  begin
                    invertsign:=tordconstnode(right).value<0;
                    if (current_settings.optimizecputype > cpu_386) and
                       not(cs_opt_size in current_settings.optimizerswitches) then
                      { use a sequence without jumps, saw this in
                        comp.compilers (JM) }
                      begin
                        { no jumps, but more operations }
                        hreg2:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                        emit_reg_reg(A_MOV,S_W,hreg1,hreg2);
                        if power=1 then
                          begin
                            {If the left value is negative, hreg2=(1 shl power)-1=1, otherwise 0.}
                            cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,OS_16,15,hreg2);
                          end
                        else
                          begin
                            {If the left value is negative, hreg2=$ffff, otherwise 0.}
                            cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_16,15,hreg2);
                            {If negative, hreg2=(1 shl power)-1, otherwise 0.}
                            emit_const_reg(A_AND,S_W,(aint(1) shl power)-1,hreg2);
                          end;
                        { add to the left value }
                        emit_reg_reg(A_ADD,S_W,hreg2,hreg1);
                        { do the shift }
                        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_16,power,hreg1);
                      end
                    else
                      begin
                        { a jump, but fewer operations }
                        cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                        emit_reg_reg(A_TEST,S_W,hreg1,hreg1);
                        current_asmdata.getjumplabel(hl);
                        cg.a_jmp_flags(current_asmdata.CurrAsmList,F_NS,hl);
                        cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                        if power=1 then
                          emit_reg(A_INC,S_W,hreg1)
                        else
                          emit_const_reg(A_ADD,S_W,(aint(1) shl power)-1,hreg1);
                        cg.a_label(current_asmdata.CurrAsmList,hl);
                        cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_16,power,hreg1);
                      end;
                    if invertsign then
                      emit_reg(A_NEG,S_W,hreg1);
                  end
                else
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,OS_16,power,hreg1);
                location.register:=hreg1;
              end
            else
              begin
                if is_signed(left.resultdef) then
                  begin
                    e:=tordconstnode(right).value.svalue;
                    d:=abs(e);
                    { Determine algorithm (a), multiplier (m), and shift factor (s) for 16-bit
                      signed integer division. Based on: Granlund, T.; Montgomery, P.L.:
                      "Division by Invariant Integers using Multiplication". SIGPLAN Notices,
                      Vol. 29, June 1994, page 61.
                    }

                    l:=log2(d);
                    j:=dword($8000) mod dword(d);
                    k:=(dword(1) shl (16+l)) div (dword($8000-j));
                    m_low:=((dword(1)) shl (16+l)) div d;
                    m_high:=(((dword(1)) shl (16+l)) + k) div d;
                    while ((m_low shr 1) < (m_high shr 1)) and (l > 0) do
                      begin
                        m_low:=m_low shr 1;
                        m_high:=m_high shr 1;
                        dec(l);
                      end;
                    m:=word(m_high);
                    s:=l;
                    if (m_high shr 15)<>0 then
                      a:=1
                    else
                      a:=0;
                    cg.getcpuregister(current_asmdata.CurrAsmList,NR_AX);
                    emit_const_reg(A_MOV,S_W,aint(m),NR_AX);
                    cg.getcpuregister(current_asmdata.CurrAsmList,NR_DX);
                    emit_reg(A_IMUL,S_W,hreg1);
                    emit_reg_reg(A_MOV,S_W,hreg1,NR_AX);
                    if a<>0 then
                      begin
                        emit_reg_reg(A_ADD,S_W,NR_AX,NR_DX);
                        {
                          printf ("; dividend: memory location or register other than AX or DX\n");
                          printf ("\n");
                          printf ("MOV AX, 0%08LXh\n", m);
                          printf ("IMUL dividend\n");
                          printf ("MOV AX, dividend\n");
                          printf ("ADD DX, AX\n");
                          if (s) printf ("SAR DX, %d\n", s);
                          printf ("SHR AX, 15\n");
                          printf ("ADD DX, AX\n");
                          if (e < 0) printf ("NEG DX\n");
                          printf ("\n");
                          printf ("; quotient now in DX\n");
                        }
                      end;
                      {
                        printf ("; dividend: memory location of register other than AX or DX\n");
                        printf ("\n");
                        printf ("MOV AX, 0%08LXh\n", m);
                        printf ("IMUL dividend\n");
                        printf ("MOV AX, dividend\n");
                        if (s) printf ("SAR DX, %d\n", s);
                        printf ("SHR AX, 15\n");
                        printf ("ADD DX, AX\n");
                        if (e < 0) printf ("NEG DX\n");
                        printf ("\n");
                        printf ("; quotient now in DX\n");
                      }
                    if s<>0 then
                      cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_16,s,NR_DX);
                    cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,OS_16,15,NR_AX);
                    emit_reg_reg(A_ADD,S_W,NR_AX,NR_DX);
                    if e<0 then
                      emit_reg(A_NEG,S_W,NR_DX);
                    cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_DX);
                    cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_AX);
                    location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_DX,location.register)
                  end
                else
                  begin
                    d:=tordconstnode(right).value.svalue;
                    if d>=$8000 then
                      begin
                        cg.a_reg_alloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                        emit_const_reg(A_CMP,S_W,aint(d),hreg1);
                        location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                        emit_const_reg(A_MOV,S_W,0,location.register);
                        emit_const_reg(A_SBB,S_W,-1,location.register);
                        cg.a_reg_dealloc(current_asmdata.CurrAsmList,NR_DEFAULTFLAGS);
                      end
                    else
                      begin
                        { Reduce divisor until it becomes odd }
                        n:=0;
                        t:=d;
                        while (t and 1)=0 do
                          begin
                            t:=t shr 1;
                            inc(n);
                          end;
                        { Generate m, s for algorithm 0. Based on: Granlund, T.; Montgomery,
                        P.L.: "Division by Invariant Integers using Multiplication".
                        SIGPLAN Notices, Vol. 29, June 1994, page 61.
                        }
                        l:=log2(t)+1;
                        j:=dword($ffff) mod dword(t);
                        k:=(dword(1) shl (16+l)) div (dword($ffff-j));
                        m_low:=((dword(1)) shl (16+l)) div t;
                        m_high:=(((dword(1)) shl (16+l)) + k) div t;
                        while ((m_low shr 1) < (m_high shr 1)) and (l>0) do
                          begin
                            m_low:=m_low shr 1;
                            m_high:=m_high shr 1;
                            l:=l-1;
                          end;
                        if (m_high shr 16)=0 then
                          begin
                            m:=word(m_high);
                            s:=l;
                            a:=0;
                          end

                        { Generate m, s for algorithm 1. Based on: Magenheimer, D.J.; et al:
                        "Integer Multiplication and Division on the HP Precision Architecture".
                        IEEE Transactions on Computers, Vol 37, No. 8, August 1988, page 980.
                        }
                        else
                          begin
                            s:=log2(t);
                            m_low:=(dword(1) shl (16+s)) div dword(t);
                            r:=word(((dword(1)) shl (16+s)) mod dword(t));
                            if (r < ((t>>1)+1)) then
                              m:=word(m_low)
                            else
                              m:=word(m_low)+1;
                            a:=1;
                          end;
                        { Reduce multiplier for either algorithm to smallest possible }
                        while (m and 1)=0 do
                          begin
                            m:=m shr 1;
                            dec(s);
                          end;
                        { Adjust multiplier for reduction of even divisors }
                        inc(s,n);
                        cg.getcpuregister(current_asmdata.CurrAsmList,NR_AX);
                        emit_const_reg(A_MOV,S_W,aint(m),NR_AX);
                        cg.getcpuregister(current_asmdata.CurrAsmList,NR_DX);
                        emit_reg(A_MUL,S_W,hreg1);
                        if a<>0 then
                          begin
                            {
                            printf ("; dividend: register other than AX or memory location\n");
                            printf ("\n");
                            printf ("MOV AX, 0%08lXh\n", m);
                            printf ("MUL dividend\n");
                            printf ("ADD AX, 0%08lXh\n", m);
                            printf ("ADC DX, 0\n");
                            if (s) printf ("SHR DX, %d\n", s);
                            printf ("\n");
                            printf ("; quotient now in DX\n");
                            }
                            emit_const_reg(A_ADD,S_W,aint(m),NR_AX);
                            emit_const_reg(A_ADC,S_W,0,NR_DX);
                          end;
                        if s<>0 then
                          cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,OS_16,aint(s),NR_DX);
                        cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_DX);
                        cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_AX);
                        location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_DX,location.register)
                      end;
                  end
              end
          end
        else
          begin
            cg.getcpuregister(current_asmdata.CurrAsmList,NR_AX);
            emit_reg_reg(A_MOV,S_W,hreg1,NR_AX);
            cg.getcpuregister(current_asmdata.CurrAsmList,NR_DX);
            {Sign extension depends on the left type.}
            if torddef(left.resultdef).ordtype=u16bit then
              emit_reg_reg(A_XOR,S_W,NR_DX,NR_DX)
            else
              emit_none(A_CWD,S_NO);

            {Division depends on the right type.}
            if Torddef(right.resultdef).ordtype=u16bit then
              op:=A_DIV
            else
              op:=A_IDIV;

            if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
              emit_ref(op,S_W,right.location.reference)
            else if right.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
              emit_reg(op,S_W,right.location.register)
            else
              begin
                hreg1:=cg.getintregister(current_asmdata.CurrAsmList,right.location.size);
                hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,u16inttype,right.location,hreg1);
                emit_reg(op,S_W,hreg1);
              end;

            {Copy the result into a new register. Release AX & DX.}
            cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_DX);
            cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_AX);
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
            if nodetype=divn then
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_AX,location.register)
            else
              cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,NR_DX,location.register);
          end;
      end;


{*****************************************************************************
                             TI8086SHLRSHRNODE
*****************************************************************************}


    function ti8086shlshrnode.first_shlshr64bitint: tnode;
      begin
        result := nil;
      end;

    procedure ti8086shlshrnode.second_64bit;
      var
        hreg64hi,hreg64lo:Tregister;
        v : TConstExprInt;
        tmpreg64: tregister64;
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));

        { load left operator in a register }
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,resultdef,false);
        hreg64hi:=left.location.register64.reghi;
        hreg64lo:=left.location.register64.reglo;
        location.register64.reglo:=hreg64lo;
        location.register64.reghi:=hreg64hi;

        if right.nodetype=ordconstn then
          begin
            v:=Tordconstnode(right).value and 63;
            location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            if nodetype=shln then
              cg64.a_op64_const_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_64,v,left.location.register64,location.register64)
            else
              cg64.a_op64_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_64,v,left.location.register64,location.register64);
          end
        else
          begin
            { load right operators in a register }
            tmpreg64.reghi:=NR_NO;
            tmpreg64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
            hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,u16inttype,right.location,tmpreg64.reglo);
            if nodetype=shln then
              cg64.a_op64_reg_reg(current_asmdata.CurrAsmList,OP_SHL,OS_64,tmpreg64,location.register64)
            else
              cg64.a_op64_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_64,tmpreg64,location.register64);
          end;
      end;


begin
   cunaryminusnode:=ti8086unaryminusnode;
   cmoddivnode:=ti8086moddivnode;
   cshlshrnode:=ti8086shlshrnode;
   cnotnode:=ti8086notnode;
end.
