{****************************************************************}
{  CODE GENERATOR TEST PROGRAM                                   }
{****************************************************************}
{ NODE TESTED : secondin()                                       }
{****************************************************************}
{ PRE-REQUISITES: secondload()                                   }
{                 secondassign()                                 }
{                 secondtypeconv()                               }
{                 secondadd() for sets                           }
{                 secondsetelement()                             }
{                 secondcalln()                                  }
{                 secondfuncret()                                }
{****************************************************************}
{ DEFINES:                                                       }
{            FPC     = Target is FreePascal compiler             }
{****************************************************************}
{ REMARKS:                                                       }
{                                                                }
{                                                                }
{                                                                }
{****************************************************************}

type
       { DO NOT CHANGE THE VALUES OF THESE ENUMERATIONS! }

       { This will fit into a 32-bit small set }
       tsmallenum = (dA,dB,dC,dd,de,df,dg,dh,di,dj,dk,dl,dm,dn,dop,dp,dq,dr);
       { This will fit into a normal 32-byte set }
       tbigenum = (A_ABCD,
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
         A_BKPT,A_MOVEC,A_MOVES,A_RTD,
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
         A_CPRESTORE,A_CPSAVE,
         A_FRESTORE,A_FSAVE,A_PFLUSH,A_PFLUSHA,A_PLOAD,A_PMOVE,A_PTEST,
         A_LABEL,A_NONE);



type
  tnormalset = set of tbigenum;
  tsmallset = set of tsmallenum;
  
  
  procedure checkpassed(passed : boolean);
   begin
    if passed then
      WriteLn('Passed!')
    else
      begin
        WriteLn('Failure.');
        Halt(1);
      end;
   end;

{ The following cases are possible                                  }
{     jump table usage                                              }
{     small set or normal set                                       }
{     source location : REFERENCE,MEMORY,CONSTANT or REGISTER       }

  { NO GENERATION OF JUMP TABLE }
  { SMALL SET                   }
  procedure smallsettestone;
   var
     op1 : tsmallset;
     op2 : tsmallset;
     op3 : tsmallset;
     op  : tsmallenum;
     passed : boolean;
   begin
     passed := true;
     Write('Small set in operator test (without case table)...');
     { LEFT : LOC_REFERENCE (not a constant node)  }
     { RIGHT : LOC_REFERENCE                       }
     op1 := [DI];
     op2 := [DI];
     op := DI;
     if not (op in op1) then
      passed := false;
     { LEFT : LOC_REFERENCE (a constant node) }
     { RIGHT: LOC_REFERENCE                   }
     op1 := [DL];
     op := DI;
     if not (DL in op1) then
      passed := false;
     { LEFT : LOC_REFERENCE (a constant node) }
     { RIGHT: LOC_REFERENCE (a constant set)  }
     { THIS CAN NEVER HAPPEN - EVALUATED AT COMPILE TIME BY COMPILER }
     op1 := [DB];
     op := DB;
     if not (DB in [DA..DL]) then
      passed := false;
     { LEFT : LOC_REFERENCE (not a constant node) }
     { RIGHT : LOC_REGISTER,LOC_CREGISTER         }
     op := DF;
     op2 := [DB];
     op3 := [DF];
     if not (op in (op2+op3)) then
       passed := false;
     { LEFT : LOC_REGISTER  (a constant node)     }
     { RIGHT : LOC_REGISTER,LOC_CREGISTER         }
     op2 := [DB];
     op3 := [DF];
     if not (DB in (op2+op3)) then
       passed := false;
     checkpassed(passed);  
   end;


  { returns result in register }
   function getsmallop : tsmallenum;
     begin
       getsmallop := DQ;
     end;

  { GENERATION OF JUMP TABLE }
  { SMALL SET                   }
  procedure smallsettesttwo;
   var
     op1 : tsmallset;
     op2 : tsmallset;
     op  : tsmallenum;
     passed : boolean;
   begin
     Write('Small set in operator test (with case table)...');
     passed := true;
     op := DN;
     { LEFT : LOC_REFERENCE }
     { RIGHT: range constant set  (carry flag) }
     if not (op in [DB..DN]) then
      passed := false;
     { LEFT : LOC_REFERENCE }
     { RIGHT: NOT range constant set (zero flag) }
     op := DH;
     if not (op in [DB,DH,DP]) then
      passed := false;
     { LEFT : LOC_REGISTER                        }
     { RIGHT : NOT range constant set (zero flag) }
     op := DH;
     if not (getsmallop in [DA,DB..DN,DQ]) then
      passed := false;
     { LEFT : LOC_REGISTER                        }
     { RIGHT : range constant set (carry flag)    }
     checkpassed(passed);  
   end;

  { returns result in register }
  function getop : tbigenum;
    begin
      getop := A_BFSET;
    end;

   { NO JUMP TABLE }
   { NORMAL SETS   }
   procedure settestone;
   var
     op1 : tnormalset;
     op2 : tnormalset;
     op  : tbigenum;
     passed : boolean;
   begin
     Write('Normal set in operator test (without case table)...');
     passed := true;
     { RIGHT NODE = immediate value in reference field ?? }
     { RIGHT node = ordconstn (how is this possible?) - it goes through }
     {  analizeset!                                                     }
     { Left : LOC_REGISTER               }
     { right : LOC_REFERENCE (call to sys) }
     if not (getop in [A_BFSET,A_MOVE,A_TRAP,A_CMP,A_CMPI,A_FADD,A_LABEL,A_ASL,A_ADDX]) then
       passed := false;

     op := A_MOVE;
     { Left : LOC_REFERENCE              }
     { right : LOC_REFERENCE             }
     if not (op in [A_BFSET,A_MOVE,A_TRAP,A_CMP,A_CMPI,A_FADD,A_LABEL,A_ASL,A_ADDX]) then
       passed := false;
     { Left : ordinal constant           }
     { right : LOC_REFERENCE             }
     op1 := [A_MOVE,A_TRAP];
     if not (A_MOVE in op1) then
       passed := false;

     checkpassed(passed);  
   end;


   { WITH JUMP TABLE }
   { NORMAL SETS   }
   procedure settesttwo;
   var
     op1 : tnormalset;
     op2 : tnormalset;
     op  : tbigenum;
     passed : boolean;
   begin
     Write('Normal set in operator test (with case table)...');
     passed := true;
     { Left : LOC_REGISTER               }
     { right : LOC_REFERENCE with ranges }
     if not (getop in [A_BFSET,A_MOVE,A_ASL..A_BCC]) then
       passed := false;
     { Left : LOC_REGISTER               }
     { right : LOC_REFERENCE no ranges   }

     if not (getop in [A_BFSET,A_MOVE]) then
       passed := false;

     op := A_MOVE;
     { Left : LOC_REFERENCE              }
     { right : LOC_REFERENCE with ranges }
     if not (getop in [A_BFSET,A_MOVE,A_ASL..A_BCC]) then
       passed := false;

     checkpassed(passed);  
   end;


Begin
  smallsettestone;
  smallsettesttwo;
  settestone;
  settesttwo;
end.

{

  $Log$
  Revision 1.2  2002-03-05 21:56:02  carl
  * Adapted for automated testing

  Revision 1.1  2001/06/25 01:34:03  carl
  + secondin() node testing


}