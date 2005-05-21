{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : second_includeexclude()                          }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondcalln()                                  }
{****************************************************************}
{ DEFINES:                                                       }
{****************************************************************}
{ REMARKS: Tests the include/exclude inline code generation.     }
{****************************************************************}
program tincexc;

{$mode objfpc}

{ Tests we must do :
   - small set : LOC_REGISTER, LOC_REFERENCE
   - large set : LOC_REFERENCE
}

procedure fail;
begin
  WriteLn('Failed!');
  halt(1);
end;

type
       { DO NOT CHANGE THE VALUES OF THESE ENUMERATIONS! }
       tsmallenum = (dA,dB,dC,dd,de,df,dg,dh,di,dj,dk,dl,dm,dn,dop,dp,dq,dr);
       tasmop = (A_ABCD,
         A_ADD,A_ADDA,A_ADDI,A_ADDQ,A_ADDX,A_AND,A_ANDI,
         A_ASL,A_ASR,A_BCC,A_BCS,A_BEQ,A_BGE,A_BGT,A_BHI,
         A_BLE,A_BLS,A_BLT,A_BMI,A_BNE,A_BPL,A_BVC,A_BVS,
         A_BCHG,A_BCLR,A_BRA,A_BSET,A_BSR,A_BTST,A_CHK,
         A_CLR,A_CMP,A_CMPA,A_CMPI,A_CMPM,A_DBCC,A_DBCS,A_DBEQ,A_DBGE,
         A_DBGT,A_DBHI,A_DBLE,A_DBLS,A_DBLT,A_DBMI,A_DBNE,A_DBRA,
         A_DBPL,A_DBT,A_DBVC,A_DBVS,A_DBF,A_DIVS,A_DIVU,
         A_EOR,A_EORI,A_EXG,A_ILLEGAL,A_EXT,A_JMP,A_JSR,
         A_LEA,A_LINK,A_LSL,A_LSR,A_MOVE,A_MOVEA,A_MOVEI,A_MOVEQ,
         A_MOVEM,A_MOVEP,A_MULS,A_MULU,A_NBCD,A_NEG,A_NEGX,
         A_NOP,A_NOT,A_OR,A_ORI,A_PEA,A_ROL,A_ROR,A_ROXL,
         A_ROXR,A_RTR,A_RTS,A_SBCD,A_SCC,A_SCS,A_SEQ,A_SGE,
         A_SGT,A_SHI,A_SLE,A_SLS,A_SLT,A_SMI,A_SNE,
         A_SPL,A_ST,A_SVC,A_SVS,A_SF,A_SUB,A_SUBA,A_SUBI,A_SUBQ,
         A_SUBX,A_SWAP,A_TAS,A_TRAP,A_TRAPV,A_TST,A_UNLK,
         A_RTE,A_RESET,A_STOP,
         { MC68010 instructions }
         A_BKPT,A_MOVEC,A_MOVES,A_RTD,
         { MC68020 instructions }
         A_BFCHG,A_BFCLR,A_BFEXTS,A_BFEXTU,A_BFFFO,
         A_BFINS,A_BFSET,A_BFTST,A_CALLM,A_CAS,A_CAS2,
         A_CHK2,A_CMP2,A_DIVSL,A_DIVUL,A_EXTB,A_PACK,A_RTM,
         A_TRAPCC,A_TRACS,A_TRAPEQ,A_TRAPF,A_TRAPGE,A_TRAPGT,
         A_TRAPHI,A_TRAPLE,A_TRAPLS,A_TRAPLT,A_TRAPMI,A_TRAPNE,
         A_TRAPPL,A_TRAPT,A_TRAPVC,A_TRAPVS,A_UNPK,
         { FPU Processor instructions - directly supported only. }
         { IEEE aware and misc. condition codes not supported   }
         A_FABS,A_FADD,
         A_FBEQ,A_FBNE,A_FBNGT,A_FBGT,A_FBGE,A_FBNGE,
         A_FBLT,A_FBNLT,A_FBLE,A_FBGL,A_FBNGL,A_FBGLE,A_FBNGLE,
         A_FDBEQ,A_FDBNE,A_FDBGT,A_FDBNGT,A_FDBGE,A_FDBNGE,
         A_FDBLT,A_FDBNLT,A_FDBLE,A_FDBGL,A_FDBNGL,A_FDBGLE,A_FBDNGLE,
         A_FSEQ,A_FSNE,A_FSGT,A_FSNGT,A_FSGE,A_FSNGE,
         A_FSLT,A_FSNLT,A_FSLE,A_FSGL,A_FSNGL,A_FSGLE,A_FSNGLE,
         A_FCMP,A_FDIV,A_FMOVE,A_FMOVEM,
         A_FMUL,A_FNEG,A_FNOP,A_FSQRT,A_FSUB,A_FSGLDIV,
         A_FSFLMUL,A_FTST,
         A_FTRAPEQ,A_FTRAPNE,A_FTRAPGT,A_FTRAPNGT,A_FTRAPGE,A_FTRAPNGE,
         A_FTRAPLT,A_FTRAPNLT,A_FTRAPLE,A_FTRAPGL,A_FTRAPNGL,A_FTRAPGLE,A_FTRAPNGLE,
         { Protected instructions }
         A_CPRESTORE,A_CPSAVE,
         { FPU Unit protected instructions                    }
         { and 68030/68851 common MMU instructions            }
         { (this may include 68040 MMU instructions)          }
         A_FRESTORE,A_FSAVE,A_PFLUSH,A_PFLUSHA,A_PLOAD,A_PMOVE,A_PTEST,
         { Useful for assembly langage output }
         A_LABEL,A_NONE);



type
  topset = set of tasmop;
  tsmallset = set of tsmallenum;


 procedure CheckPassed(passed:boolean);
 begin
   if passed then
     WriteLn('Success.')
   else
     begin
       WriteLn('Failure.');
       Halt(1);
     end;
 end;


  Procedure SetTestIncludeOne;
    var
     op : tasmop;
     oplist: set of tasmop;
  Begin
    Write('Include Large Set element testing (LOC_REFERENCE/LOC_REFERENCE)...');
    op:=A_LABEL;
    oplist:=[];
    Include(oplist, op);
    CheckPassed(oplist = [A_LABEL]);
  end;


  function get_large_enum : tasmop;
   begin
     get_large_enum := A_LABEL;
   end;

  Procedure SetTestIncludeTwo;
    var
     oplist: set of tasmop;
  Begin
    Write('Include Large Set element testing (LOC_REFERENCE/LOC_REGISTER)...');
    oplist:=[];
    Include(oplist, get_large_enum);
    CheckPassed(oplist = [A_LABEL]);
  end;


 Procedure SetTestIncludeThree;
  var
   small_enum : tsmallenum;
   small_set : set of tsmallenum;
 begin
   Write('Include Small Set element testing (LOC_REFERENCE/LOC_REFERENCE)...');
   small_enum := dop;
   small_set := [];
   Include(small_set, small_enum);
   CheckPassed(small_set = [DOP]);
 end;


 function get_small_enum : tsmallenum;
  begin
    get_small_enum := dop;
  end;

 Procedure SetTestIncludeFour;
  var
   small_enum : tsmallenum;
   small_set : set of tsmallenum;
 begin
   Write('Include Small Set element testing (LOC_REFERENCE/LOC_REGISTER)...');
   small_enum := dop;
   small_set := [];
   Include(small_set, get_small_enum);
   CheckPassed(small_set = [DOP]);
 end;


  Procedure SetTestExcludeOne;
    var
     op : tasmop;
     oplist: set of tasmop;
  Begin
    Write('Exclude Large Set element testing (LOC_REFERENCE/LOC_REFERENCE)...');
    op:=A_RTE;
    oplist:=[A_RTE,A_LABEL,A_STOP];
    Exclude(oplist, op);
    CheckPassed(oplist = [A_LABEL,A_STOP]);
  end;

 Procedure SetTestExcludeTwo;
  var
   small_enum : tsmallenum;
   small_set : set of tsmallenum;
 begin
   Write('Exclude Small Set element testing (LOC_REFERENCE/LOC_REFERENCE)...');
   small_enum := dop;
   small_set := [dc,dop,dp,dq,dm];
   Exclude(small_set, small_enum);
   CheckPassed(small_set = [dc,dp,dq,dm]);
 end;


 function get_small_enum_two : tsmallenum;
  begin
    get_small_enum_two := dop;
  end;

 Procedure SetTestExcludeThree;
  var
   small_set : set of tsmallenum;
 begin
   Write('Exclude Small Set element testing (LOC_REFERENCE/LOC_REGISTER)...');
   small_set := [dc,dop,dp,dq,dm];
   Exclude(small_set, get_small_enum);
   CheckPassed(small_set = [dc,dp,dq,dm]);
 end;

  function get_large_enum_two : tasmop;
   begin
     get_large_enum_two := A_LABEL;
   end;

  Procedure SetTestExcludeFour;
    var
     oplist: set of tasmop;
  Begin
    Write('Exclude Large Set element testing (LOC_REFERENCE/LOC_REGISTER)...');
    oplist:=[A_LABEL,A_STOP];
    Exclude(oplist, get_large_enum_two);
    CheckPassed(oplist = [A_STOP]);
  end;


Begin
  SetTestIncludeOne;
  SetTestIncludeTwo;
  SetTestIncludeThree;
  SetTestIncludeFour;
  SetTestExcludeOne;
  SetTestExcludeTwo;
  SetTestExcludeThree;
  SetTestExcludeFour;
end.
