{*****************************************************************************}
{ File                   : cgcpu.pas                                          }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\04\26                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
{
    $Id$

    Copyright (c) 1998-2000 by Florian Klaempfl

    This program is free software;you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation;either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program;if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ****************************************************************************}
UNIT cgcpu;
{This unit implements the code generator for the SPARC architecture}
{$INCLUDE fpcdefs.inc}
INTERFACE
USES
  cginfo,cgbase,cgobj,cg64f32,
  aasmbase,aasmtai,aasmcpu,
  cpubase,cpuinfo,cpupara,
  node,symconst;
TYPE
  tcgSPARC=CLASS(tcg)
    FreeParamRegSet:TRegisterSet;
{This method is used to pass a parameter, which is located in a register, to a
routine. It should give the parameter to the routine, as required by the
specific processor ABI. It is overriden for each CPU target.
  Size    : is the size of the operand in the register
  r       : is the register source of the operand
  LocPara : is the location where the parameter will be stored}
    procedure a_param_reg(list:TAasmOutput;size:tcgsize;r:tregister;const LocPara:TParaLocation);override;
    procedure a_param_const(list:TAasmOutput;size:tcgsize;a:aword;CONST LocPara:TParaLocation);override;
    procedure a_param_ref(list:TAasmOutput;size:tcgsize;CONST r:TReference;CONST LocPara:TParaLocation);override;
    procedure a_paramaddr_ref(list:TAasmOutput;CONST r:TReference;CONST LocPara:TParaLocation);override;
    procedure a_call_name(list:TAasmOutput;CONST s:string);override;
    procedure a_call_ref(list:TAasmOutput;CONST ref:TReference);override;
    procedure a_op_const_reg(list:TAasmOutput;Op:TOpCG;a:AWord;reg:TRegister);override;
    procedure a_op_const_ref(list:TAasmOutput;Op:TOpCG;size:TCGSize;a:AWord;CONST ref:TReference);override;
    procedure a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);override;
    procedure a_op_ref_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;CONST ref:TReference;reg:TRegister);override;
    procedure a_op_reg_ref(list:TAasmOutput;Op:TOpCG;size:TCGSize;reg:TRegister;CONST ref:TReference);override;
    procedure a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;a:aword;src, dst:tregister);override;
    procedure a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;size:tcgsize;src1, src2, dst:tregister);override;
        { move instructions }
    procedure a_load_const_reg(list:TAasmOutput;size:tcgsize;a:aword;reg:tregister);override;
    procedure a_load_const_ref(list:TAasmOutput;size:tcgsize;a:aword;CONST ref:TReference);override;
    procedure a_load_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;CONST ref:TReference);override;
    procedure a_load_ref_reg(list:TAasmOutput;size:tcgsize;CONST ref:TReference;reg:tregister);override;
    procedure a_load_reg_reg(list:TAasmOutput;fromsize,tosize:tcgsize;reg1,reg2:tregister);override;
    procedure a_loadaddr_ref_reg(list:TAasmOutput;CONST ref:TReference;r:tregister);override;
        { fpu move instructions }
    procedure a_loadfpu_reg_reg(list:TAasmOutput;reg1, reg2:tregister);override;
    procedure a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;CONST ref:TReference;reg:tregister);override;
    procedure a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;CONST ref:TReference);override;
        { vector register move instructions }
    procedure a_loadmm_reg_reg(list:TAasmOutput;reg1, reg2:tregister);override;
    procedure a_loadmm_ref_reg(list:TAasmOutput;CONST ref:TReference;reg:tregister);override;
    procedure a_loadmm_reg_ref(list:TAasmOutput;reg:tregister;CONST ref:TReference);override;
    procedure a_parammm_reg(list:TAasmOutput;reg:tregister);override;
        {  comparison operations }
    procedure a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;reg:tregister;l:tasmlabel);override;
    procedure a_cmp_const_ref_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;CONST ref:TReference;l:tasmlabel);override;
    procedure a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;reg1,reg2:tregister;l:tasmlabel);override;
    procedure a_cmp_ref_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;CONST ref:TReference;reg:tregister;l:tasmlabel);override;
    procedure a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:tasmlabel);{ override;}
    procedure a_jmp_flags(list:TAasmOutput;CONST f:TResFlags;l:tasmlabel);override;
    procedure g_flags2reg(list:TAasmOutput;Size:TCgSize;CONST f:tresflags;reg:TRegister);override;
    procedure g_stackframe_entry(list:TAasmOutput;localsize:LongInt);override;
    procedure g_restore_frame_pointer(list:TAasmOutput);override;
    procedure g_return_from_proc(list:TAasmOutput;parasize:aword);override;
    procedure g_concatcopy(list:TAasmOutput;CONST source,dest:TReference;len:aword;delsource,loadref:boolean);override;
    class function reg_cgsize(CONST reg:tregister):tcgsize;override;
  PRIVATE
    function IsSimpleRef(const ref:treference):boolean;
    procedure sizes2load(s1:tcgsize;s2:topsize;var op:tasmop;var s3:topsize);
    procedure floatload(list:TAasmOutput;t:tcgsize;CONST ref:TReference);
    procedure floatstore(list:TAasmOutput;t:tcgsize;CONST ref:TReference);
    procedure floatloadops(t:tcgsize;var op:tasmop;var s:topsize);
    procedure floatstoreops(t:tcgsize;var op:tasmop;var s:topsize);
  END;
  TCg64fSPARC=class(tcg64f32)
    procedure a_op64_ref_reg(list:TAasmOutput;op:TOpCG;CONST ref:TReference;reg:TRegister64);override;
    procedure a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);override;
    procedure a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:qWord;regdst:TRegister64);override;
    procedure a_op64_const_ref(list:TAasmOutput;op:TOpCG;value:qWord;CONST ref:TReference);override;
    procedure get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
  END;
CONST
  TOpCG2AsmOp:ARRAY[topcg]OF TAsmOp=(A_NONE,A_ADD,A_AND,A_UDIV,A_SDIV,A_UMUL, A_SMUL, A_NEG,A_NOT,A_OR,A_not,A_not,A_not,A_SUB,A_XOR);
  TOpCmp2AsmCond:ARRAY[topcmp]OF TAsmCond=(C_NONE,C_E,C_G,C_L,C_GE,C_LE,C_NE,C_BE,C_B,C_AE,C_A);
  TCGSize2OpSize:ARRAY[tcgsize]OF TOpSize=(S_NO,S_B,S_W,S_L,S_L,S_B,S_W,S_L,S_L,S_FS,S_FL,S_FX,S_IQ,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_NO);
IMPLEMENTATION
USES
  globtype,globals,verbose,systems,cutils,
  symdef,symsym,defbase,paramgr,
  rgobj,tgobj,rgcpu;
    { we implement the following routines because otherwise we can't }
    { instantiate the class since it's abstract                      }
procedure tcgSPARC.a_param_reg(list:TAasmOutput;size:tcgsize;r:tregister;CONST LocPara:TParaLocation);
  BEGIN
    IF(Size<>OS_32)AND(Size<>OS_S32)
    THEN
      InternalError(2002032212);
		with list,LocPara do
		  case Loc of
			  LOC_REGISTER:
    			if r<>Register
					then
						Concat(taicpu.op_Reg_Reg_Reg(A_OR,S_L,r,R_G0,Register));
				else
				  InternalError(2002101002);
			end;
  end;
procedure tcgSPARC.a_param_const(list:TAasmOutput;size:tcgsize;a:aword;CONST LocPara:TParaLocation);
  BEGIN
    with List do
      case Size of
        OS_32,OS_S32:
          Concat(taicpu.op_const(A_LD,a));
        OS_64,OS_S64:
          Concat(taicpu.op_const(A_LDD,a));
        else
          InternalError(2002032213);
      end;
  END;
procedure tcgSPARC.a_param_ref(list:TAasmOutput;size:tcgsize;const r:TReference;const LocPara:TParaLocation);
  var
    ref: treference;
    tmpreg:TRegister;
  begin
	  with LocPara do
	    case locpara.loc of
  	    LOC_REGISTER,LOC_CREGISTER:
    	    a_load_ref_reg(list,size,r,Register);
      	LOC_REFERENCE:
        	begin
          {Code conventions need the parameters being allocated in %o6+92. See
          comment on g_stack_frame}
          	if locpara.sp_fixup<92
          	then
            	InternalError(2002081104);
          	reference_reset(ref);
          	ref.base:=locpara.reference.index;
          	ref.offset:=locpara.reference.offset;
          	tmpreg := get_scratch_reg_int(list);
          	a_load_ref_reg(list,size,r,tmpreg);
          	a_load_reg_ref(list,size,tmpreg,ref);
          	free_scratch_reg(list,tmpreg);
        	end;
      	LOC_FPUREGISTER,LOC_CFPUREGISTER:
        	case size of
          	OS_32:
            	a_loadfpu_ref_reg(list,OS_F32,r,locpara.register);
	          OS_64:
  	          a_loadfpu_ref_reg(list,OS_F64,r,locpara.register);
    	    else
      	    internalerror(2002072801);
        	end;
	    	else
  	    	internalerror(2002081103);
    	end;
  end;
procedure tcgSPARC.a_paramaddr_ref(list:TAasmOutput;CONST r:TReference;CONST LocPara:TParaLocation);
  VAR
    tmpreg:TRegister;
  BEGIN
    IF r.segment<>R_NO
    THEN
      CGMessage(cg_e_cant_use_far_pointer_there);
    IF(r.base=R_NO)AND(r.index=R_NO)
    THEN
      list.concat(Taicpu.Op_sym_ofs(A_LD,S_L,r.symbol,r.offset))
    ELSE IF(r.base=R_NO)AND(r.index<>R_NO)AND
           (r.offset=0)AND(r.scalefactor=0)AND(r.symbol=nil)
    THEN
      list.concat(Taicpu.Op_reg(A_LD,r.index))
    ELSE IF(r.base<>R_NO)AND(r.index=R_NO)AND
           (r.offset=0)AND(r.symbol=nil)
    THEN
      list.concat(Taicpu.Op_reg(A_LD,r.base))
    ELSE
      BEGIN
        tmpreg:=get_scratch_reg_address(list);
        a_loadaddr_ref_reg(list,r,tmpreg);
        list.concat(taicpu.op_reg(A_LD,tmpreg));
        free_scratch_reg(list,tmpreg);
      END;
  END;
procedure tcgSPARC.a_call_name(list:TAasmOutput;CONST s:string);
  BEGIN
    WITH List,objectlibrary DO
      BEGIN
        concat(taicpu.op_sym(A_CALL,S_L,newasmsymbol(s)));
        concat(taicpu.op_none(A_NOP));
      END;
  END;
procedure tcgSPARC.a_call_ref(list:TAasmOutput;CONST ref:TReference);
  BEGIN
    list.concat(taicpu.op_ref(A_CALL,ref));
    list.concat(taicpu.op_none(A_NOP));
  END;
{********************** load instructions ********************}
procedure tcgSPARC.a_load_const_reg(list:TAasmOutput;size:TCGSize;a:aword;reg:TRegister);
  BEGIN
    WITH List DO
      IF a<>0
      THEN{R_G0 is usually set to zero, so we use it}
        Concat(taicpu.op_reg_const_reg(A_OR,TCGSize2OpSize[size],R_G0,a,reg))
      ELSE{The is no A_MOV in sparc, that's why we use A_OR with help of R_G0}
        Concat(taicpu.op_reg_reg_reg(A_OR,TCGSize2OpSize[size],R_G0,R_G0,reg));
  END;
procedure tcgSPARC.a_load_const_ref(list:TAasmOutput;size:tcgsize;a:aword;CONST ref:TReference);
  BEGIN
    WITH List DO
      IF a=0
      THEN
        Concat(taicpu.op_reg_ref(A_ST,R_G0,ref))
      ELSE
        BEGIN
          a_load_const_reg(list,size,a,R_G1);
          case size of
            OS_32,OS_S32:
              Concat(taicpu.op_reg_ref(A_ST,R_G1,ref));
            OS_64,OS_S64:
              Concat(taicpu.op_reg_ref(A_STD,R_G1,ref));
            else
              InternalError(2002102100);
          end;
        END;
  END;
procedure tcgSPARC.a_load_reg_ref(list:TAasmOutput;size:TCGSize;reg:tregister;CONST ref:TReference);
  BEGIN
    list.concat(taicpu.op_reg_ref(A_LD,reg,ref));
  END;
procedure tcgSPARC.a_load_ref_reg(list:TAasmOutput;size:tcgsize;const ref:TReference;reg:tregister);
  var
    op:tasmop;
    s:topsize;
  begin
    sizes2load(size,S_L,op,s);
    list.concat(taicpu.op_ref_reg(op,ref,reg));
  end;
procedure tcgSPARC.a_load_reg_reg(list:TAasmOutput;fromsize,tosize:tcgsize;reg1,reg2:tregister);
  var
    op:tasmop;
    s:topsize;
  begin
    if(reg1<>reg2)or
      (tcgsize2size[tosize]<tcgsize2size[fromsize])or
      ((tcgsize2size[tosize] = tcgsize2size[fromsize])and
        (tosize <> fromsize)and
        not(fromsize in [OS_32,OS_S32]))
    then
      with list do
        case fromsize of
          OS_8:
            InternalError(2002100800);{concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,reg2,reg1,0,31-8+1,31));}
          OS_S8:
            InternalError(2002100801);{concat(taicpu.op_reg_reg(A_EXTSB,reg2,reg1));}
          OS_16:
            InternalError(2002100802);{concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,reg2,reg1,0,31-16+1,31));}
          OS_S16:
            InternalError(2002100803);{concat(taicpu.op_reg_reg(A_EXTSH,reg2,reg1));}
          OS_32,OS_S32:
            concat(taicpu.op_reg_reg_reg(A_OR,S_L,R_G0,reg1,reg2));
          else internalerror(2002090901);
        end;
  end;
    { all fpu load routines expect that R_ST[0-7] means an fpu regvar and }
    { R_ST means "the current value at the top of the fpu stack" (JM)     }
procedure tcgSPARC.a_loadfpu_reg_reg(list:TAasmOutput;reg1, reg2:tregister);

       begin
{         if NOT (reg1 IN [R_F0..R_F31]) then
           begin
             list.concat(taicpu.op_reg(A_NONE,S_NO,
               trgcpu(rg).correct_fpuregister(reg1,trgcpu(rg).fpuvaroffset)));
             inc(trgcpu(rg).fpuvaroffset);
           end;
         if NOT (reg2 IN [R_F0..R_F31]) then
           begin
             list.concat(taicpu.op_reg(A_JMPL,S_NO,
                 trgcpu(rg).correct_fpuregister(reg2,trgcpu(rg).fpuvaroffset)));
             dec(trgcpu(rg).fpuvaroffset);
           end;}
       end;


    procedure tcgSPARC.a_loadfpu_ref_reg(list:TAasmOutput;size:tcgsize;CONST ref:TReference;reg:tregister);

       begin
         floatload(list,size,ref);
{         if (reg <> R_ST) then
           a_loadfpu_reg_reg(list,R_ST,reg);}
       end;


    procedure tcgSPARC.a_loadfpu_reg_ref(list:TAasmOutput;size:tcgsize;reg:tregister;CONST ref:TReference);

       begin
{         if reg <> R_ST then
           a_loadfpu_reg_reg(list,reg,R_ST);}
         floatstore(list,size,ref);
       end;


    procedure tcgSPARC.a_loadmm_reg_reg(list:TAasmOutput;reg1, reg2:tregister);

       begin
//         list.concat(taicpu.op_reg_reg(A_NONEQ,S_NO,reg1,reg2));
       end;


    procedure tcgSPARC.a_loadmm_ref_reg(list:TAasmOutput;CONST ref:TReference;reg:tregister);

       begin
//         list.concat(taicpu.op_ref_reg(A_NONEQ,S_NO,ref,reg));
       end;


    procedure tcgSPARC.a_loadmm_reg_ref(list:TAasmOutput;reg:tregister;CONST ref:TReference);

       begin
//         list.concat(taicpu.op_reg_ref(A_NONEQ,S_NO,reg,ref));
       end;
procedure tcgSPARC.a_parammm_reg(list:TAasmOutput;reg:tregister);
  VAR
    href:TReference;
  BEGIN
//    list.concat(taicpu.op_const_reg(A_SUB,S_L,8,R_RSP));
//    reference_reset_base(href,R_ESP,0);
//    list.concat(taicpu.op_reg_ref(A_NONEQ,S_NO,reg,href));
  END;
procedure tcgSPARC.a_op_const_reg(list:TAasmOutput;Op:TOpCG;a:AWord;reg:TRegister);

      var
        opcode:tasmop;
        power:LongInt;

      begin
(*       Case Op of
          OP_DIV, OP_IDIV:
            Begin
              if ispowerof2(a,power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_reg(opcode,S_L,power,
                    reg));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109224);
            End;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                begin
                  list.concat(taicpu.op_const_reg(A_SHL,S_L,power,
                    reg));
                  exit;
                end;
              if op = OP_IMUL then
                list.concat(taicpu.op_const_reg(A_IMUL,S_L,
                  a,reg))
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109225);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if not(cs_check_overflow in aktlocalswitches) and
               (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_reg(A_INC,S_L,reg))
              else
                list.concat(taicpu.op_reg(A_DEC,S_L,reg))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                list.concat(taicpu.op_const_reg(A_NONE,S_L,0,reg))
            else if (a = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_reg(A_NONE,S_L,high(aword),reg));
                       OP_XOR:
                         list.concat(taicpu.op_reg(A_NOT,S_L,reg));
                     end
                   end
            else
              list.concat(taicpu.op_const_reg(TOpCG2AsmOp[op],S_L,
                a,reg));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_reg(
                  TOpCG2AsmOp[op],S_L,a and 31,reg));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;*)
      end;


     procedure tcgSPARC.a_op_const_ref(list:TAasmOutput;Op:TOpCG;size:TCGSize;a:AWord;CONST ref:TReference);

      var
        opcode:tasmop;
        power:LongInt;

      begin
(*        Case Op of
          OP_DIV, OP_IDIV:
            Begin
              if ispowerof2(a,power) then
                begin
                  case op of
                    OP_DIV:
                      opcode := A_SHR;
                    OP_IDIV:
                      opcode := A_SAR;
                  end;
                  list.concat(taicpu.op_const_ref(opcode,
                    TCgSize2OpSize[size],power,ref));
                  exit;
                end;
              { the rest should be handled specifically in the code      }
              { generator because of the silly register usage restraints }
              internalerror(200109231);
            End;
          OP_MUL,OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                begin
                  list.concat(taicpu.op_const_ref(A_SHL,TCgSize2OpSize[size],
                    power,ref));
                  exit;
                end;
              { can't multiply a memory location directly with a CONSTant }
              if op = OP_IMUL then
                inherited a_op_const_ref(list,op,size,a,ref)
              else
                { OP_MUL should be handled specifically in the code        }
                { generator because of the silly register usage restraints }
                internalerror(200109232);
            end;
          OP_ADD, OP_AND, OP_OR, OP_SUB, OP_XOR:
            if not(cs_check_overflow in aktlocalswitches) and
               (a = 1) and
               (op in [OP_ADD,OP_SUB]) then
              if op = OP_ADD then
                list.concat(taicpu.op_ref(A_INC,TCgSize2OpSize[size],ref))
              else
                list.concat(taicpu.op_ref(A_DEC,TCgSize2OpSize[size],ref))
            else if (a = 0) then
              if (op <> OP_AND) then
                exit
              else
                a_load_const_ref(list,size,0,ref)
            else if (a = high(aword)) and
                    (op in [OP_AND,OP_OR,OP_XOR]) then
                   begin
                     case op of
                       OP_AND:
                         exit;
                       OP_OR:
                         list.concat(taicpu.op_const_ref(A_NONE,TCgSize2OpSize[size],high(aword),ref));
                       OP_XOR:
                         list.concat(taicpu.op_ref(A_NOT,TCgSize2OpSize[size],ref));
                     end
                   end
            else
              list.concat(taicpu.op_const_ref(TOpCG2AsmOp[op],
                TCgSize2OpSize[size],a,ref));
          OP_SHL,OP_SHR,OP_SAR:
            begin
              if (a and 31) <> 0 Then
                list.concat(taicpu.op_const_ref(
                  TOpCG2AsmOp[op],TCgSize2OpSize[size],a and 31,ref));
              if (a shr 5) <> 0 Then
                internalerror(68991);
            end
          else internalerror(68992);
        end;*)
      end;


     procedure tcgSPARC.a_op_reg_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;src, dst:TRegister);

        var
          regloadsize:tcgsize;
          dstsize:topsize;
          tmpreg:tregister;
          popecx:boolean;

        begin
(*          dstsize := S_Q{makeregsize(dst,size)};
          case op of
            OP_NEG,OP_NOT:
              begin
                if src <> R_NO then
                  internalerror(200112291);
                list.concat(taicpu.op_reg(TOpCG2AsmOp[op],dstsize,dst));
              end;
            OP_MUL,OP_DIV,OP_IDIV:
              { special stuff, needs separate handling inside code }
              { generator                                          }
              internalerror(200109233);
            OP_SHR,OP_SHL,OP_SAR:
              begin
                tmpreg := R_NO;
                { we need cl to hold the shift count, so if the destination }
                { is ecx, save it to a temp for now                         }
                if dst in [R_ECX,R_CX,R_CL] then
                  begin
                    case S_L of
                      S_B:regloadsize := OS_8;
                      S_W:regloadsize := OS_16;
                      else regloadsize := OS_32;
                    end;
                    tmpreg := get_scratch_reg(list);
                    a_load_reg_reg(list,regloadsize,OS_32,src,tmpreg);
                  end;
                if not(src in [R_ECX,R_CX,R_CL]) then
                  begin
                    { is ecx still free (it's also free if it was allocated }
                    { to dst, since we've moved dst somewhere else already) }
                    if not((dst = R_ECX) or
                           ((R_ECX in rg.unusedregsint) and
                            { this will always be true, it's just here to }
                            { allocate ecx                                }
                            (rg.getexplicitregisterint(list,R_ECX) = R_ECX))) then
                      begin
                        list.concat(taicpu.op_reg(A_NONE,S_L,R_ECX));
                        popecx := true;
                      end;
                    a_load_reg_reg(list,OS_8,OS_8,(src),R_CL);
                  end
                else
                  src := R_CL;
                { do the shift }
                if tmpreg = R_NO then
                  list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,
                    R_CL,dst))
                else
                  begin
                    list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],S_L,
                      R_CL,tmpreg));
                    { move result back to the destination }
                    a_load_reg_reg(list,OS_32,OS_32,tmpreg,R_ECX);
                    free_scratch_reg(list,tmpreg);
                  end;
                if popecx then
                  list.concat(taicpu.op_reg(A_POP,S_L,R_ECX))
                else if not (dst in [R_ECX,R_CX,R_CL]) then
                  rg.ungetregisterint(list,R_ECX);
              end;
            else
              begin
                if S_L <> dstsize then
                  internalerror(200109226);
                list.concat(taicpu.op_reg_reg(TOpCG2AsmOp[op],dstsize,
                  src,dst));
              end;
          end;*)
        end;


     procedure tcgSPARC.a_op_ref_reg(list:TAasmOutput;Op:TOpCG;size:TCGSize;CONST ref:TReference;reg:TRegister);

       var
         opsize:topsize;

       begin
(*          case op of
            OP_NEG,OP_NOT,OP_IMUL:
              begin
                inherited a_op_ref_reg(list,op,size,ref,reg);
              end;
            OP_MUL,OP_DIV,OP_IDIV:
              { special stuff, needs separate handling inside code }
              { generator                                          }
              internalerror(200109239);
            else
              begin
                opsize := S_Q{makeregsize(reg,size)};
                list.concat(taicpu.op_ref_reg(TOpCG2AsmOp[op],opsize,ref,reg));
              end;
          end;*)
       end;


     procedure tcgSPARC.a_op_reg_ref(list:TAasmOutput;Op:TOpCG;size:TCGSize;reg:TRegister;CONST ref:TReference);

       var
         opsize:topsize;

       begin
(*         case op of
           OP_NEG,OP_NOT:
             begin
               if reg <> R_NO then
                 internalerror(200109237);
               list.concat(taicpu.op_ref(TOpCG2AsmOp[op],tcgsize2opsize[size],ref));
             end;
           OP_IMUL:
             begin
               { this one needs a load/imul/store, which is the default }
               inherited a_op_ref_reg(list,op,size,ref,reg);
             end;
           OP_MUL,OP_DIV,OP_IDIV:
             { special stuff, needs separate handling inside code }
             { generator                                          }
             internalerror(200109238);
           else
             begin
               opsize := tcgsize2opsize[size];
               list.concat(taicpu.op_reg_ref(TOpCG2AsmOp[op],opsize,reg,ref));
             end;
         end;*)
       end;


    procedure tcgSPARC.a_op_const_reg_reg(list:TAasmOutput;op:TOpCg;
        size:tcgsize;a:aword;src, dst:tregister);
      var
        tmpref:TReference;
        power:LongInt;
        opsize:topsize;
      begin
        opsize := S_L;
        if (opsize <> S_L) or
           not (size in [OS_32,OS_S32]) then
          begin
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        Case Op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR:
            { can't do anything special for these }
            inherited a_op_const_reg_reg(list,op,size,a,src,dst);
          OP_IMUL:
            begin
              if not(cs_check_overflow in aktlocalswitches) and
                 ispowerof2(a,power) then
                { can be done with a shift }
                inherited a_op_const_reg_reg(list,op,size,a,src,dst);
              list.concat(taicpu.op_reg_const_reg(A_SMUL,S_L,src,a,dst));
            end;
          OP_ADD, OP_SUB:
            if (a = 0) then
              a_load_reg_reg(list,size,size,src,dst)
            else
              begin
                reference_reset(tmpref);
                tmpref.base := src;
                tmpref.offset := LongInt(a);
                if op = OP_SUB then
                  tmpref.offset := -tmpref.offset;
                list.concat(taicpu.op_ref_reg(A_NONE,tmpref,dst));
              end
          else internalerror(200112302);
        end;
      end;

    procedure tcgSPARC.a_op_reg_reg_reg(list:TAasmOutput;op:TOpCg;
        size:tcgsize;src1, src2, dst:tregister);
      var
        tmpref:TReference;
        opsize:topsize;
      begin
        opsize := S_L;
        if (opsize <> S_L) or
           (S_L <> S_L) or
           not (size in [OS_32,OS_S32]) then
          begin
            inherited a_op_reg_reg_reg(list,op,size,src1,src2,dst);
            exit;
          end;
        { if we get here, we have to do a 32 bit calculation, guaranteed }
        Case Op of
          OP_DIV, OP_IDIV, OP_MUL, OP_AND, OP_OR, OP_XOR, OP_SHL, OP_SHR,
          OP_SAR,OP_SUB,OP_NOT,OP_NEG:
            { can't do anything special for these }
            inherited a_op_reg_reg_reg(list,op,size,src1,src2,dst);
          OP_IMUL:
            list.concat(taicpu.op_reg_reg_reg(A_SMUL,S_L,src1,src2,dst));
          OP_ADD:
            begin
              reference_reset(tmpref);
              tmpref.base := src1;
              tmpref.index := src2;
              tmpref.scalefactor := 1;
              list.concat(taicpu.op_ref_reg(A_NONE,tmpref,dst));
            end
          else internalerror(200112303);
        end;
      end;

{*************** compare instructructions ****************}

      procedure tcgSPARC.a_cmp_const_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;reg:tregister;
        l:tasmlabel);

        begin
          if (a = 0) then
            list.concat(taicpu.op_reg_reg(A_CMP,reg,reg))
          else
            list.concat(taicpu.op_const_reg(A_CMP,a,reg));
          a_jmp_cond(list,cmp_op,l);
        end;

procedure tcgSPARC.a_cmp_const_ref_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;a:aword;const ref:TReference;l:tasmlabel);
  begin
    with List do
      begin
        Concat(taicpu.op_const(A_LD,a));
        Concat(taicpu.op_ref(A_CMP,ref));
      end;
    a_jmp_cond(list,cmp_op,l);
  end;

      procedure tcgSPARC.a_cmp_reg_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;
        reg1,reg2:tregister;l:tasmlabel);

        begin
 {         if regsize(reg1) <> S_L then
            internalerror(200109226);
          list.concat(taicpu.op_reg_reg(A_CMP,regsize(reg1),reg1,reg2));
          a_jmp_cond(list,cmp_op,l);}
        end;

     procedure tcgSPARC.a_cmp_ref_reg_label(list:TAasmOutput;size:tcgsize;cmp_op:topcmp;CONST ref:TReference;reg:tregister;l:tasmlabel);

        var
          opsize:topsize;

        begin
          opsize := S_Q{makeregsize(reg,size)};
          list.concat(taicpu.op_ref_reg(A_CMP,ref,reg));
          a_jmp_cond(list,cmp_op,l);
        end;

     procedure tcgSPARC.a_jmp_cond(list:TAasmOutput;cond:TOpCmp;l:tasmlabel);

       var
         ai:taicpu;

       begin
         if cond=OC_None then
           ai := Taicpu.Op_sym(A_JMPL,S_NO,l)
         else
           begin
             ai:=Taicpu.Op_sym(A_JMPL,S_NO,l);
             ai.SetCondition(TOpCmp2AsmCond[cond]);
           end;
         ai.is_jmp:=true;
         list.concat(ai);
       end;

     procedure tcgSPARC.a_jmp_flags(list:TAasmOutput;CONST f:TResFlags;l:tasmlabel);
       var
         ai:taicpu;
       begin
         ai := Taicpu.op_sym(A_JMPL,S_NO,l);
         ai.SetCondition(flags_to_cond(f));
         ai.is_jmp := true;
         list.concat(ai);
       end;

procedure tcgSPARC.g_flags2reg(list:TAasmOutput;Size:TCgSize;CONST f:tresflags;reg:TRegister);
  VAR
    ai:taicpu;
    hreg:tregister;
  BEGIN
    hreg := rg.makeregsize(reg,OS_8);
//    ai:=Taicpu.Op_reg(A_Setcc,S_B,hreg);
    ai.SetCondition(flags_to_cond(f));
    list.concat(ai);
    IF hreg<>reg
    THEN
      a_load_reg_reg(list,OS_8,OS_8,hreg,reg);
  END;

{ *********** entry/exit code and address loading ************ }

procedure tcgSPARC.g_stackframe_entry(list:TAasmOutput;localsize:LongInt);
  var
    href:TReference;
    i:integer;
    again:tasmlabel;
  begin
{According the the SPARC ABI the standard stack frame must include :
  *  16 word save for the in and local registers in case of overflow/underflow.
this save area always must exist at the %o6+0,
  *  software conventions requires space for the aggregate return value pointer, even if the word is not used,
  *  althogh the first six words of arguments reside in registers, the standard
stack frame reserves space for them. Arguments beond the sixth reside on the
stack as in the Intel architecture,
  * other areas depend on the compiler and the code being compiled. The
standard calling sequence does not define a maximum stack frame size, nor does
it restrict how a language system uses the "unspecified" areas of the standard
stack frame.}
    Dec(LocalSize,(16+1+5)*4);
{Althogh the SPARC architecture require only word alignment, software
convention and the operating system require every stack frame to be double word
aligned}
    LocalSize:=(LocalSize+3)and $FFFFFFFC;
{Execute the SAVE instruction to get a new register window and get a new stack
frame. In the "SAVE %i6,size,%i6" the first %i6 is related to the state before
execution of the SAVE instrucion so it is the caller %i6, when the %i6 after
execution of that instrucion is the called function stack pointer}
    with list do
      concat(Taicpu.Op_reg_const_reg(A_SAVE,S_L,Stack_Pointer_Reg,localsize,Stack_Pointer_Reg));
  end;
procedure tcgSPARC.g_restore_frame_pointer(list:TAasmOutput);
  begin
{This function intontionally does nothing as frame pointer is restored in the
delay slot of the return instrucion done in g_return_from_proc}
  end;
procedure tcgSPARC.g_return_from_proc(list:TAasmOutput;parasize:aword);
  begin
{According to the SPARC ABI, the stack is cleared using the RESTORE instruction
which is genereted in the g_restore_frame_pointer. Notice that SPARC has no
RETURN instruction and that JMPL is used instead. The JMPL instrucion have one
delay slot, so an inversion is possible such as 
  JMPL  %i7+8,%g0
  RESTORE  %g0,0,%g0
If no inversion we can use just
  RESTORE  %g0,0,%g0
  JMPL  %i7+8,%g0
  NOP}
    with list do
      begin
{Return address is computed by adding 8 to the CALL address saved onto %i6}
        concat(Taicpu.Op_caddr_reg(A_JMPL,R_I7,8,R_G0));
{We use trivial restore in the delay slot of the JMPL instruction, as we
already set result onto %i0}
        concat(Taicpu.Op_reg_const_reg(A_RESTORE,S_L,R_G0,0,R_G0));
      end
  end;
procedure tcgSPARC.a_loadaddr_ref_reg(list:TAasmOutput;CONST ref:TReference;r:tregister);

       begin
//         list.concat(taicpu.op_ref_reg(A_LEA,S_L,ref,r));
       end;
{ ************* 64bit operations ************ }
    procedure TCg64fSPARC.get_64bit_ops(op:TOpCG;var op1,op2:TAsmOp);
      begin
        case op of
          OP_ADD :
            begin
              op1:=A_ADD;
              op2:=A_ADD;
            end;
          OP_SUB :
            begin
              op1:=A_SUB;
              op2:=A_SUB;
            end;
          OP_XOR :
            begin
              op1:=A_XOR;
              op2:=A_XOR;
            end;
          OP_OR :
            begin
              op1:=A_OR;
              op2:=A_OR;
            end;
          OP_AND :
            begin
              op1:=A_AND;
              op2:=A_AND;
            end;
          else
            internalerror(200203241);
        end;
      end;


    procedure TCg64fSPARC.a_op64_ref_reg(list:TAasmOutput;op:TOpCG;CONST ref:TReference;reg:TRegister64);
      var
        op1,op2:TAsmOp;
        tempref:TReference;
      begin
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_ref_reg(op1,ref,reg.reglo));
        tempref:=ref;
        inc(tempref.offset,4);
        list.concat(taicpu.op_ref_reg(op2,tempref,reg.reghi));
      end;


    procedure TCg64fSPARC.a_op64_reg_reg(list:TAasmOutput;op:TOpCG;regsrc,regdst:TRegister64);
      var
        op1,op2:TAsmOp;
      begin
        get_64bit_ops(op,op1,op2);
        list.concat(taicpu.op_reg_reg(op1,regsrc.reglo,regdst.reglo));
        list.concat(taicpu.op_reg_reg(op2,regsrc.reghi,regdst.reghi));
      end;


    procedure TCg64fSPARC.a_op64_const_reg(list:TAasmOutput;op:TOpCG;value:qWord;regdst:TRegister64);
      var
        op1,op2:TAsmOp;
      begin
        case op of
          OP_AND,OP_OR,OP_XOR:
            WITH cg DO
              begin
                a_op_const_reg(list,op,Lo(Value),regdst.reglo);
                a_op_const_reg(list,op,Hi(Value),regdst.reghi);
              end;
          OP_ADD, OP_SUB:
            begin
              // can't use a_op_const_ref because this may use dec/inc
              get_64bit_ops(op,op1,op2);
              list.concat(taicpu.op_const_reg(op1,Lo(Value),regdst.reglo));
              list.concat(taicpu.op_const_reg(op2,Hi(Value),regdst.reghi));
            end;
          else
            internalerror(200204021);
        end;
      end;


procedure TCg64fSPARC.a_op64_const_ref(list:TAasmOutput;op:TOpCG;value:qWord;const ref:TReference);
  var
    op1,op2:TAsmOp;
    tempref:TReference;
  begin
    case op of
      OP_AND,OP_OR,OP_XOR:
      with cg do
        begin
          a_op_const_ref(list,op,OS_32,Lo(Value),ref);
          tempref:=ref;
          inc(tempref.offset,4);
          a_op_const_ref(list,op,OS_32,Hi(Value),tempref);
        end;
      OP_ADD, OP_SUB:
            begin
              get_64bit_ops(op,op1,op2);
              // can't use a_op_const_ref because this may use dec/inc
{              list.concat(taicpu.op_const_ref(op1,Lo(Value),ref));
              tempref:=ref;
              inc(tempref.offset,4);
              list.concat(taicpu.op_const_ref(op2,S_L,Hi(Value),tempref));}
              InternalError(2002102101);
            end;
          else
            internalerror(200204022);
        end;
      end;


{ ************* concatcopy ************ }
procedure TCgSparc.g_concatcopy(list:taasmoutput;const source,dest:treference;len:aword;delsource,loadref:boolean);
  var
    countreg: TRegister;
    src, dst: TReference;
    lab: tasmlabel;
    count, count2: aword;
    orgsrc, orgdst: boolean;
  begin
{$ifdef extdebug}
    if len > high(longint)
    then
      internalerror(2002072704);
{$endif extdebug}
        { make sure short loads are handled as optimally as possible }
        if not loadref then
          if (len <= 8) and
             (byte(len) in [1,2,4,8]) then
            begin
              if len < 8 then
                begin
                  a_load_ref_ref(list,int_cgsize(len),source,dest);
                  if delsource then
                    reference_release(list,source);
                end
              else
                begin
                  a_reg_alloc(list,R_F0);
                  a_loadfpu_ref_reg(list,OS_F64,source,R_F0);
                  if delsource then
                    reference_release(list,source);
                  a_loadfpu_reg_ref(list,OS_F64,R_F0,dest);
                  a_reg_dealloc(list,R_F0);
                end;
              exit;
            end;

        reference_reset(src);
        reference_reset(dst);
        { load the address of source into src.base }
        if loadref then
          begin
            src.base := get_scratch_reg_address(list);
            a_load_ref_reg(list,OS_32,source,src.base);
            orgsrc := false;
          end
        else if not issimpleref(source) or
                ((source.index <> R_NO) and
                 ((source.offset + longint(len)) > high(smallint))) then
          begin
            src.base := get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list,source,src.base);
            orgsrc := false;
          end
        else
          begin
            src := source;
            orgsrc := true;
          end;
        if not orgsrc and delsource then
          reference_release(list,source);
        { load the address of dest into dst.base }
        if not issimpleref(dest) or
           ((dest.index <> R_NO) and
            ((dest.offset + longint(len)) > high(smallint))) then
          begin
            dst.base := get_scratch_reg_address(list);
            a_loadaddr_ref_reg(list,dest,dst.base);
            orgdst := false;
          end
        else
          begin
            dst := dest;
            orgdst := true;
          end;

        count := len div 8;
        if count > 4 then
          { generate a loop }
          begin
            { the offsets are zero after the a_loadaddress_ref_reg and just }
            { have to be set to 8. I put an Inc there so debugging may be   }
            { easier (should offset be different from zero here, it will be }
            { easy to notice in the generated assembler                     }
            inc(dst.offset,8);
            inc(src.offset,8);
            list.concat(taicpu.op_reg_const_reg(A_SUB,S_L,src.base,8,src.base));
            list.concat(taicpu.op_reg_const_reg(A_SUB,S_L,dst.base,8,dst.base));
            countreg := get_scratch_reg_int(list);
            a_load_const_reg(list,OS_32,count,countreg);
            { explicitely allocate R_O0 since it can be used safely here }
            { (for holding date that's being copied)                    }
            a_reg_alloc(list,R_F0);
            objectlibrary.getlabel(lab);
            a_label(list, lab);
            list.concat(taicpu.op_reg_const_reg(A_SUB,S_L,countreg,1,countreg));
            list.concat(taicpu.op_reg_ref(A_LDF,R_F0,src));
            list.concat(taicpu.op_reg_ref(A_STD,R_F0,dst));
            //a_jmp(list,A_BC,C_NE,0,lab);
            free_scratch_reg(list,countreg);
            a_reg_dealloc(list,R_F0);
            len := len mod 8;
          end;

        count := len div 8;
        if count > 0 then
          { unrolled loop }
          begin
            a_reg_alloc(list,R_F0);
            for count2 := 1 to count do
              begin
                a_loadfpu_ref_reg(list,OS_F64,src,R_F0);
                a_loadfpu_reg_ref(list,OS_F64,R_F0,dst);
                inc(src.offset,8);
                inc(dst.offset,8);
              end;
            a_reg_dealloc(list,R_F0);
            len := len mod 8;
          end;

        if (len and 4) <> 0 then
          begin
            a_reg_alloc(list,R_O0);
            a_load_ref_reg(list,OS_32,src,R_O0);
            a_load_reg_ref(list,OS_32,R_O0,dst);
            inc(src.offset,4);
            inc(dst.offset,4);
            a_reg_dealloc(list,R_O0);
          end;
       { copy the leftovers }
       if (len and 2) <> 0 then
         begin
           a_reg_alloc(list,R_O0);
           a_load_ref_reg(list,OS_16,src,R_O0);
           a_load_reg_ref(list,OS_16,R_O0,dst);
           inc(src.offset,2);
           inc(dst.offset,2);
           a_reg_dealloc(list,R_O0);
         end;
       if (len and 1) <> 0 then
         begin
           a_reg_alloc(list,R_O0);
           a_load_ref_reg(list,OS_8,src,R_O0);
           a_load_reg_ref(list,OS_8,R_O0,dst);
           a_reg_dealloc(list,R_O0);
         end;
       if orgsrc then
         begin
           if delsource then
             reference_release(list,source);
         end
       else
         free_scratch_reg(list,src.base);
       if not orgdst then
         free_scratch_reg(list,dst.base);
      end;
function tcgSPARC.reg_cgsize(CONST reg:tregister):tcgsize;
      CONST
        regsize_2_cgsize:array[S_B..S_L] of tcgsize = (OS_8,OS_16,OS_32);
      begin
        result := regsize_2_cgsize[S_L];
      end;


{***************** This is private property, keep out! :) *****************}
function TCgSparc.IsSimpleRef(const ref:treference):boolean;
  begin
    if(ref.base=R_NONE)and(ref.index <> R_NO)
    then
      InternalError(2002100804);
    result :=not(assigned(ref.symbol))and
              (((ref.index = R_NO) and
               (ref.offset >= low(smallint)) and
                (ref.offset <= high(smallint))) or
              ((ref.index <> R_NO) and
              (ref.offset = 0)));
  end;
procedure tcgSPARC.sizes2load(s1:tcgsize;s2:topsize;var op:tasmop;var s3:topsize);
  begin
    case s2 of
      S_B:
        if S1 in [OS_8,OS_S8]
        then
          s3 := S_B
        else
          internalerror(200109221);
      S_W:
        case s1 of
          OS_8,OS_S8:
            s3 := S_BW;
          OS_16,OS_S16:
            s3 := S_W;
        else
          internalerror(200109222);
        end;
      S_L:
        case s1 of
          OS_8,OS_S8:
            s3 := S_BL;
          OS_16,OS_S16:
            s3 := S_WL;
          OS_32,OS_S32:
            s3 := S_L;
          else
            internalerror(200109223);
        end;
      else internalerror(200109227);
    end;
    if s3 in [S_B,S_W,S_L]
    then
      op := A_LD
{    else if s3=S_DW
    then
      op:=A_LDD
    else if s3 in [OS_8,OS_16,OS_32]
    then
      op := A_NONE}
    else
      op := A_NONE;
  end;
procedure tcgSPARC.floatloadops(t:tcgsize;VAR op:tasmop;VAR s:topsize);
  BEGIN
(*         case t of
            OS_F32:begin
                        op:=A_FLD;
                        s:=S_FS;
                     end;
            OS_F64:begin
                        op:=A_FLD;
                        { ???? }
                        s:=S_FL;
                     end;
            OS_F80:begin
                        op:=A_FLD;
                        s:=S_FX;
                     end;
            OS_C64:begin
                        op:=A_FILD;
                        s:=S_IQ;
                     end;
            else internalerror(17);
         end;*)
  END;
procedure tcgSPARC.floatload(list:TAasmOutput;t:tcgsize;CONST ref:TReference);
  VAR
    op:tasmop;
    s:topsize;
  BEGIN
    floatloadops(t,op,s);
    list.concat(Taicpu.Op_ref(op,ref));
{    inc(trgcpu(rg).fpuvaroffset);}
  END;
procedure tcgSPARC.floatstoreops(t:tcgsize;var op:tasmop;var s:topsize);
  BEGIN
{         case t of
            OS_F32:begin
                        op:=A_FSTP;
                        s:=S_FS;
                     end;
            OS_F64:begin
                        op:=A_FSTP;
                        s:=S_FL;
                     end;
            OS_F80:begin
                        op:=A_FSTP;
                        s:=S_FX;
                     end;
            OS_C64:begin
                        op:=A_FISTP;
                        s:=S_IQ;
                     end;
         else
           internalerror(17);
         end;}
      end;
procedure tcgSPARC.floatstore(list:TAasmOutput;t:tcgsize;CONST ref:TReference);
  VAR
    op:tasmop;
    s:topsize;
  BEGIN
    floatstoreops(t,op,s);
    list.concat(Taicpu.Op_ref(op,ref));
{    dec(trgcpu(rg).fpuvaroffset);}
  END;
BEGIN
  cg:=tcgSPARC.create;
END.
{
  $Log$
  Revision 1.18  2002-10-22 13:43:01  mazen
  - cga.pas redueced to an empty unit

  Revision 1.17  2002/10/20 19:01:38  mazen
  + op_raddr_reg and op_caddr_reg added to fix functions prologue

  Revision 1.16  2002/10/13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.15  2002/10/11 13:35:14  mazen
  *** empty log message ***

  Revision 1.14  2002/10/10 19:57:51  mazen
  * Just to update repsitory

  Revision 1.13  2002/10/10 15:10:39  mazen
  * Internal error fixed, but usually i386 parameter model used

  Revision 1.12  2002/10/08 17:17:03  mazen
  *** empty log message ***

  Revision 1.11  2002/10/07 20:33:04  mazen
  word alignement modified in g_stack_frame

  Revision 1.10  2002/10/04 21:57:42  mazen
  * register allocation for parameters now done in cpupara, but InternalError(200109223) in cgcpu.pas:1053 is still not fixed du to location_force problem in ncgutils.pas:419

  Revision 1.9  2002/10/02 22:20:28  mazen
  + out registers allocator for the first 6 scalar parameters which must be passed into %o0..%o5

  Revision 1.8  2002/10/01 21:35:58  mazen
  + procedures exiting prologue added and stack frame now restored in the delay slot of the return (JMPL) instruction

  Revision 1.7  2002/10/01 21:06:29  mazen
  attinst.inc --> strinst.inc

  Revision 1.6  2002/10/01 17:41:50  florian
    * fixed log and id
}
