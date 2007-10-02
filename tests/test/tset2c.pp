(*********************************************************************)
(* Copyright (C) 1998, Carl Eric Codere                              *)
(*********************************************************************)
(* FPC (Free Pascal compiler) testsuite: SETS                        *)
(*   Tests the following: in, +, -, *, assignments.                  *)
(*      for small sets amd large sets, both with constants           *)
(*      and variables.                                               *)
(*********************************************************************)

{$packset 1}

const
  failed: boolean = false;

type
       myenum = (dA:=17,dB,dC,dd,dedf,dg,dh,di,dj,dk,dl,dm,dn);
       tasmop = (A_ABCD:=21,
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


Function X(y:myenum): myenum;
Begin
 x:=y;
end;


Procedure SecondInSets;
{ SET_IN_BYTE TESTS }
var
 op,op2 : tasmop;
 oplist: set of tasmop;
Begin
 Write('TESTING SET_IN_BYTE:');
 oplist:=[];
 op:=A_JSR;
 if A_JSR in oplist then
  begin
    WriteLn('A_JSR in [] FAILED.');
    failed := true
  end
 else
  Writeln('A_JSR in [] PASSED.');
 if op in oplist then
  begin
    WriteLn('op(A_JSR) in [] FAILED.');
    failed := true
  end
 else
  Writeln('op (A_JSR) in [] PASSED.');
 op:=A_MOVE;
 oplist:=oplist+[A_MOVE];
 if A_MOVE in oplist then
  WriteLn('A_MOVE in ([]+[A_MOVE]) PASSED.')
 else
  begin
    Writeln('A_MOVE in ([]+[A_MOVE]) FAILED.');
    failed := true;
  end;
 if op in oplist then
  WriteLn('op(A_MOVE) in ([]+[A_MOVE]) PASSED.')
 else
  begin
    Writeln('op(A_MOVE) in ([]+[A_MOVE]) FAILED.');
    failed := true;
  end;
  op:=A_MOVE;
  oplist:=[];
  oplist:=[A_SUB]+[op];
  op2:=A_MOVE;
 if A_MOVE in oplist then
  WriteLn('A_MOVE in ([A_SUB]+[op(A_MOVE)]) PASSED.')
 else
  begin
    Writeln('A_MOVE in ([A_SUB]+[op(A_MOVE)]) FAILED.');
    failed := true
  end;
 if op2 in oplist then
  WriteLn('op2(A_MOVE) in ([A_SUB]+[op(A_MOVE)]) PASSED.')
 else
  begin
    Writeln('op2(A_MOVE) in ([A_SUB]+[op(A_MOVE)]) FAILED.');
    failed := true
  end;
end;

Procedure SetSetByte;
{ SET_SET_BYTE }
var
 op : tasmop;
 oplist: set of tasmop;
Begin
 Write('TESTING SET_SET_BYTE(1):');
 op:=A_LABEL;
 oplist:=[];
 oplist:=oplist+[op];
 if op in oplist then
 Begin
  WriteLn(' PASSED.');
 end
 else
 Begin
  WriteLn(' FAILED.');
  failed := true
 end;
 Write('TESTING INCLUDE:');
 op:=A_RTE;
 include(oplist,op);
 if op in oplist then
 Begin
  WriteLn(' PASSED.');
 end
 else
 Begin
  WriteLn(' FAILED.');
  failed := true;
 end;
end;


Procedure SetAddSets;
{ SET_ADD_SETS }
var
 op2list :set of tasmop;
 oplist: set of tasmop;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[A_MOVE]+[A_JSR];
 op2list:=[A_LABEL];
 oplist:=op2list+oplist;
 if A_MOVE in oplist then
  if A_LABEL in oplist then
   if A_JSR in oplist then
    WriteLn('TESTING SET_ADD_SETS: PASSED.')
   else
    begin
      WriteLn('TESTING SET_ADD_SETS: FAILED.');
      failed := true
    end
  else
    begin
      WriteLn('TESTING SET_ADD_SETS: FAILED.');
      failed := true
    end
 else
  begin
    WriteLn('TESTING SET_ADD_SETS: FAILED.');
    failed := true
  end;
end;

Procedure SetSubsets;
{ SET_SUB_SETS }
var
 op2list :set of tasmop;
 oplist: set of tasmop;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[A_MOVE]+[A_JSR];
 op2list:=[A_MOVE]+[A_JSR];
 oplist:=op2list-oplist;
 if (A_MOVE in oplist) or (A_LABEL in oplist) or (A_JSR in oplist) then
   begin
    WriteLn('TESTING SET_SUB_SETS: FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING SET_SUB_SETS: PASSED.');
 oplist := [A_MOVE,A_RTE];
 exclude(oplist,A_MOVE);
 if (A_MOVE in oplist) then
  begin
    WriteLn('TESTING EXCLUDE: FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING EXCLUDE: PASSED.')
end;

Procedure SetCompSets;
{ SET_COMP_SETS }
var
 op2list :set of tasmop;
 oplist: set of tasmop;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[A_MOVE]+[A_JSR];
 op2list:=[A_MOVE]+[A_JSR];
 if oplist=op2list then
  WriteLn('TESTING SET_COMP_SETS(1): PASSED.')
 else
  begin
    WriteLn('TESTING SET_COMP_SETS(1): FAILED.');
    failed := true
  end;
 oplist:=[A_MOVE];
 if oplist=op2list then
  begin
    WriteLn('TESTING SET_COMP_SETS(2): FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING SET_COMP_SETS(2): PASSED.');
end;

Procedure SetMulSets;
{ SET_COMP_SETS }
var
 op2list :set of tasmop;
 oplist: set of tasmop;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[A_MOVE]+[A_JSR];
 op2list:=[A_MOVE];
 oplist:=oplist*op2list;
 if A_JSR in oplist then
  begin
    WriteLn('TESTING SET_MUL_SETS(1): FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING SET_MUL_SETS(1): PASSED.');
 if A_MOVE in oplist  then
  WriteLn('TESTING SET_MUL_SETS(2): PASSED.')
 else
  begin
    WriteLn('TESTING SET_MUL_SETS(2): FAILED.');
    failed := true
  end;
end;

{------------------------------ TESTS FOR SMALL VALUES ---------------------}
Procedure SmallInSets;
{ SET_IN_BYTE TESTS }
var
 op : myenum;
 oplist: set of myenum;
Begin
 Write('TESTING IN_BYTE:');
 oplist:=[];
 op:=Dn;
 if op in oplist then
  begin
    WriteLn(' FAILED.');
    failed := true
  end;
 op:=dm;
 oplist:=oplist+[Dm];
 if op in oplist then
  WriteLn(' PASSED.')
 else
   begin
    WriteLn(' FAILED.');
    failed := true
  end;
end;

Procedure SmallSetByte;
{ SET_SET_BYTE }
var
 op : myenum;
 oplist: set of myenum;
Begin
 Write('TESTING SET_BYTE(1):');
 op:=DA;
 oplist:=[];
 oplist:=oplist+[op];
 if op in oplist then
 Begin
  WriteLn(' PASSED.');
 end
 else
 Begin
  WriteLn(' FAILED.');
  failed := true;
 end;
end;


Procedure SmallAddSets;
{ SET_ADD_SETS }
var
 op2list :set of myenum;
 oplist: set of myenum;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[DA]+[DC];
 op2list:=[DB];
 oplist:=op2list+oplist;
 if DA in oplist then
  if DC in oplist then
   if DB in oplist then
    WriteLn('TESTING SET_ADD_SETS: PASSED.')
   else
    begin
      WriteLn('TESTING ADD_SETS: FAILED.');
      failed := true
    end
  else
   begin
     WriteLn('TESTING ADD_SETS: FAILED.');
     failed := true
   end
 else
  begin
    WriteLn('TESTING ADD_SETS: FAILED.');
    failed := true
  end;
end;

Procedure SmallSubsets;
{ SET_SUB_SETS }
var
 op2list :set of myenum;
 oplist: set of myenum;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[DA]+[DC];
 op2list:=[DA]+[DC];
 oplist:=op2list-oplist;
 if (DA in oplist) or (DB in oplist) or (DC in oplist) then
  begin
    WriteLn('TESTING SUB_SETS: FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING SUB_SETS: PASSED.')
end;

Procedure SmallCompSets;
{ SET_COMP_SETS }
var
 op2list :set of myenum;
 oplist: set of myenum;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[DA]+[DC];
 op2list:=[DA]+[DC];
 if oplist=op2list then
  WriteLn('TESTING COMP_SETS(1): PASSED.')
 else
  begin
    WriteLn('TESTING COMP_SETS(1): FAILED.');
    failed := true
  end;
 oplist:=[DA];
 if oplist=op2list then
  begin
    WriteLn('TESTING COMP_SETS(2): FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING COMP_SETS(2): PASSED.');
end;

Procedure SmallMulSets;
{ SET_COMP_SETS }
var
 op2list :set of myenum;
 oplist: set of myenum;
Begin
 op2list:=[];
 oplist:=[];
 oplist:=[DA]+[DC];
 op2list:=[DA];
 oplist:=oplist*op2list;
 if DC in oplist then
  begin
    WriteLn('TESTING MUL_SETS(1): FAILED.');
    failed := true
  end
 else
  WriteLn('TESTING MUL_SETS(1): PASSED.');
 if DA in oplist  then
  WriteLn('TESTING MUL_SETS(2): PASSED.')
 else
  begin
    WriteLn('TESTING MUL_SETS(2): FAILED.');
    failed := true
  end;
end;

const
 b: myenum = (dA);
var
 enum: set of myenum;
 oplist: set of tasmop;
 l : word;
Begin
{ small sets }
 enum:=[];
 { add }
 enum:=enum+[da];
 { subtract }
 enum:=enum-[da];
 if DA in enum then
  WriteLn('Found A_LABEL');
 { very large sets       }
 { copy loop test        }
 WRITELN('LARGE SETS:');
 oplist := [A_LABEL];
 { secondin test         }
 if A_LABEL in oplist then
  WriteLn('TESTING SIMPLE SECOND_IN: PASSED.')
 else
   begin
    failed := true
  end;

 { }
 oplist:=[];
 if A_LABEL in oplist then
   begin
    WriteLn('SECOND IN FAILED.');
    failed := true
  end;
 SecondinSets;
 SetSetByte;
 SetAddSets;
 SetSubSets;
 SetCompSets;
 SetMulSets;
 WRITELN('SMALL SETS:');
 SmallInSets;
 SmallAddSets;
 SmallSubSets;
 SmallCompSets;
 SmallMulSets;
 l:=word(A_CPRESTORE);
 if l = word(A_CPRESTORE) then
  Begin
  end
  else failed := true;

  if failed then
    begin
      WriteLn('One or more test failed');
      Halt(1);
    end;
end.
