{
    Copyright (c) 1998-2002 by Florian Klaempfl and Peter Vreman

    Contains the base types for the ARM

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
{# Base unit for processor information. This unit contains
   enumerations of registers, opcodes, sizes, and other
   such things which are processor specific.
}
unit cpubase;

{$define USEINLINE}

{$i fpcdefs.inc}

  interface

    uses
      globtype,globals,
      cpuinfo,
      cgbase
      ;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

    type
      TAsmOp= {$i xtensaop.inc}

      { This should define the array of instructions as string }
      op2strtable=array[tasmop] of string[7];

    const
      { First value of opcode enumeration }
      firstop = low(tasmop);
      { Last value of opcode enumeration  }
      lastop  = high(tasmop);

{*****************************************************************************
                                  Registers
*****************************************************************************}

    type
      { Number of registers used for indexing in tables }
      tregisterindex=0..{$i rxtensanor.inc}-1;

    const
      { Available Superregisters }
      {$i rxtensasup.inc}

      { No Subregisters }
      R_SUBWHOLE = R_SUBNONE;

      { Available Registers }
      {$i rxtensacon.inc}

      { Integer Super registers first and last }
      first_int_supreg = RS_A0;
      first_int_imreg = $10;

      { Float Super register first and last }
      first_fpu_supreg    = RS_F0;
      first_fpu_imreg     = $10;

      { MM Super register first and last }
      first_mm_supreg    = RS_INVALID;
      first_mm_imreg     = $30;

      { firs flag imaginary register }
      first_flag_imreg     = $10;

      regnumber_table : array[tregisterindex] of tregister = (
        {$i rxtensanum.inc}
      );

      regstabs_table : array[tregisterindex] of shortint = (
        {$i rxtensasta.inc}
      );

      regdwarf_table : array[tregisterindex] of shortint = (
        {$i rxtensadwa.inc}
      );

      {*****************************************************************************
                                Instruction post fixes
      *****************************************************************************}
          type
            { Xtensa instructions can have several instruction post fixes }
            TOpPostfix = (PF_None,
              { On big-endian processors, convert encoded immediate value to little-endian.
                For J.L, assembler tries to convert into J if target is within reach, else convert to JX }
              PF_L,
              { Assembler to generate narrow version of instruction if possible }
              PF_N,
              { Opcode operates on single precision floating point register(s)}
              PF_S,
              { Indicate MUL operations involving MAC16 accumulator option }
              PF_AA_LL, PF_AA_HL, PF_AA_LH, PF_AA_HH, PF_AD_LL, PF_AD_HL,
              PF_AD_LH, PF_AD_HH, PF_DA_LL, PF_DA_HL, PF_DA_LH, PF_DA_HH,
              PF_DD_LL, PF_DD_HL, PF_DD_LH, PF_DD_HH,
              PF_DA_LL_LDDEC, PF_DA_HL_LDDEC, PF_DA_LH_LDDEC, PF_DA_HH_LDDEC,
              PF_DA_LL_LDINC, PF_DA_HL_LDINC, PF_DA_LH_LDINC, PF_DA_HH_LDINC,
              PF_DD_LL_LDDEC, PF_DD_HL_LDDEC, PF_DD_LH_LDDEC, PF_DD_HH_LDDEC,
              PF_DD_LL_LDINC, PF_DD_HL_LDINC, PF_DD_LH_LDINC, PF_DD_HH_LDINC,
              { Special registers accessible via RSR, WSR & XSR instructions }
              PF_ACCHI, PF_ACCLO, PF_ATOMCTL, PF_BR, PF_CCOMPARE0, PF_CCOMPARE1,
              PF_CCOMPARE2, PF_CCOUNT, PF_CPENABLE, PF_DBREAKA0, PF_DBREAKA1,
              PF_DBREAKC0, PF_DBREAKC1, PF_DDR, PF_DEBUGCAUSE, PF_DEPC,
              PF_DTLBCFG, PF_EPC1, PF_EPC2, PF_EPC3, PF_EPC4, PF_EPC5, PF_EPC6,
              PF_EPC7, PF_EPS2, PF_EPS3, PF_EPS4, PF_EPS5, PF_EPS6, PF_EPS7,
              PF_EXCCAUSE, PF_EXCSAVE1, PF_EXCSAVE2, PF_EXCSAVE3, PF_EXCSAVE4,
              PF_EXCSAVE5, PF_EXCSAVE6, PF_EXCSAVE7, PF_EXCVADDR, PF_IBREAKA0,
              PF_IBREAKA1, PF_IBREAKENABLE, PF_ICOUNT, PF_ICOUNTLEVEL,
              PF_INTCLEAR, PF_INTENABLE, PF_INTERRUPT, PF_INTSET, PF_ITLBCFG,
              PF_LBEG, PF_LCOUNT, PF_LEND, PF_LITBASE, PF_M0, PF_M1, PF_M2,
              PF_M3, PF_MECR, PF_MEPC, PF_MEPS, PF_MESAVE, PF_MESR, PF_MEVADDR,
              PF_MISC0, PF_MISC1, PF_MISC2, PF_MISC3, PF_MMID, PF_PRID, PF_PS,
              PF_PTEVADDR, PF_RASID, PF_SAR, PF_SCOMPARE1, PF_VECBASE,
              PF_WINDOWBASE, PF_WINDOWSTART);

            TOpPostfixes = set of TOpPostfix;

          const
            oppostfix2str : array[TOpPostfix] of string[12] = ('',
              'l', 'n', 's',
              'aa.ll', 'aa.hl', 'aa.lh', 'aa.hh', 'ad.ll', 'ad.hl',
              'ad.lh', 'ad.hh', 'da.ll', 'da.hl', 'da.lh', 'da.hh',
              'dd.ll', 'dd.hl', 'dd.lh', 'dd.hh', 'da.ll.lddec',
              'da.hl.lddec', 'da.lh.lddec', 'da.hh.lddec', 'da.ll.ldinc',
              'da.hl.ldinc', 'da.lh.ldinc', 'da.hh.ldinc', 'dd.ll.lddec',
              'dd.hl.lddec', 'dd.lh.lddec', 'dd.hh.lddec', 'dd.ll.ldinc',
              'dd.hl.ldinc', 'dd.lh.ldinc', 'dd.hh.ldinc',
              'acchi', 'acclo', 'atomctl', 'br', 'ccompare0', 'ccompare1',
              'ccompare2', 'ccount', 'cpenable', 'dbreaka0', 'dbreaka1',
              'dbreakc0', 'dbreakc1', 'ddr', 'debugcause', 'depc',
              'dtlbcfg', 'epc1', 'epc2', 'epc3', 'epc4', 'epc5', 'epc6',
              'epc7', 'eps2', 'eps3', 'eps4', 'eps5', 'eps6', 'eps7',
              'exccause', 'excsave1', 'excsave2', 'excsave3', 'excsave4',
              'excsave5', 'excsave6', 'excsave7', 'excvaddr', 'ibreaka0',
              'ibreaka1', 'ibreakenable', 'icount', 'icountlevel',
              'intclear', 'intenable', 'interrupt', 'intset', 'itlbcfg',
              'lbeg', 'lcount', 'lend', 'litbase', 'm0', 'm1', 'm2',
              'm3', 'mecr', 'mepc', 'meps', 'mesave', 'mesr', 'mevaddr',
              'misc0', 'misc1', 'misc2', 'misc3', 'mmid', 'prid', 'ps',
              'ptevaddr', 'rasid', 'sar', 'scompare1', 'vecbase',
              'windowbase', 'windowstart');

{*****************************************************************************
                                Conditions
*****************************************************************************}

    type
      TAsmCond=(C_None,
        C_EQ,C_NE,
        C_GE,C_LT,C_GEU,C_LTU,
        C_ANY,C_BNONE,C_ALL,C_NALL,C_BC,C_BS,C_BCI,C_BSI,
        C_EQZ,C_NEZ,C_LTZ,C_GEZ,
        C_EQI,C_NEI,C_LTI,C_GEI,C_LTUI,C_GEUI,
        C_F,C_T
      );

      TAsmConds = set of TAsmCond;

      TResFlagsEnum = (F_Z,F_NZ);

      TResFlags = record
        register: TRegister;
        flag: TResFlagsEnum;
      end;

    const
      cond2str : array[TAsmCond] of string[4]=('',
        'eq','ne',                         
        'ge','lt','geu','ltu',
        'any','none','all','nall','bc','bs','bci','bsi',
        'eqz','nez','ltz','gez',
        'eqi','nei','lti','gei','ltui','geui',
        'f','t'
      );

      uppercond2str : array[TAsmCond] of string[4]=('',
        'EQ','NE',
        'GE','LT','GEU','LTU',
        'ANY','NONE','ALL','NALL','BC','BS', 'BCI','BSI',
        'EQZ','NEZ','LTZ','GEZ',
        'EQI','NEI','LTI','GEI','LTUI','GEUI',
        'F','T'
      );

{*****************************************************************************
                                Operands
*****************************************************************************}


    type
      tupdatereg = (UR_None,UR_Update);

      tcpumodeflag = (mfA, mfI, mfF);
      tcpumodeflags = set of tcpumodeflag;

      tspecialregflag = (srC, srX, srS, srF);
      tspecialregflags = set of tspecialregflag;

{*****************************************************************************
                                 Constants
*****************************************************************************}

    const
      max_operands = 6;

      maxintregs = 15;
      maxfpuregs = 8;
      maxaddrregs = 0;

{*****************************************************************************
                                Operand Sizes
*****************************************************************************}

    type
      topsize = (S_NO,
        S_B,S_W,S_L,S_BW,S_BL,S_WL,
        S_IS,S_IL,S_IQ,
        S_FS,S_FL,S_FX,S_D,S_Q,S_FV,S_FXX
      );


{*****************************************************************************
                          Default generic sizes
*****************************************************************************}
    const
      { Defines the default address size for a processor, }
      OS_ADDR = OS_32;
      { the natural int size for a processor,
        has to match osuinttype/ossinttype as initialized in psystem }
      OS_INT = OS_32;
      OS_SINT = OS_S32;
      { the maximum float size for a processor,           }
      OS_FLOAT = OS_F64;
      { the size of a vector register for a processor     }
      OS_VECTOR = OS_M32;

{*****************************************************************************
                          Generic Register names
*****************************************************************************}

      { Stack pointer register }
      NR_STACK_POINTER_REG = NR_A1;
      RS_STACK_POINTER_REG = RS_A1;
      { Frame pointer register (initialized in tcpuprocinfo.init_framepointer) }
      RS_FRAME_POINTER_REG: tsuperregister = RS_A7;
      NR_FRAME_POINTER_REG: tregister = NR_A7;
      { Register for addressing absolute data in a position independant way,
        such as in PIC code. The exact meaning is ABI specific. For
        further information look at GCC source : PIC_OFFSET_TABLE_REGNUM
      }
       { Results are returned in this register (32-bit values) }
      NR_FUNCTION_RETURN_REG = NR_A2;
      RS_FUNCTION_RETURN_REG = RS_A2;
      { The value returned from a function is available in this register }
      NR_FUNCTION_RESULT_REG = NR_FUNCTION_RETURN_REG;
      RS_FUNCTION_RESULT_REG = RS_FUNCTION_RETURN_REG;

      NR_FPU_RESULT_REG = NR_INVALID;

      NR_MM_RESULT_REG  = NR_INVALID;

      NR_RETURN_ADDRESS_REG = NR_FUNCTION_RETURN_REG;

      { Offset where the parent framepointer is pushed }
      PARENT_FRAMEPOINTER_OFFSET = 0;

      { we consider B0 as the default flag }
      NR_DEFAULTFLAGS = NR_B0;
      RS_DEFAULTFLAGS = RS_B0;

{*****************************************************************************
                       GCC /ABI linking information
*****************************************************************************}

    const
      { Required parameter alignment when calling a routine declared as
        stdcall and cdecl. The alignment value should be the one defined
        by GCC or the target ABI.

        The value of this constant is equal to the constant
        PARM_BOUNDARY / BITS_PER_UNIT in the GCC source.
      }
      std_param_align = 4;


{*****************************************************************************
                                  Helpers
*****************************************************************************}

    { Returns the tcgsize corresponding with the size of reg.}
    function reg_cgsize(const reg: tregister) : tcgsize;
    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
    function is_calljmp(o:tasmop):boolean;{$ifdef USEINLINE}inline;{$endif USEINLINE}
    function findreg_by_number(r:Tregister):tregisterindex;
    function std_regnum_search(const s:string):Tregister;
    function std_regname(r:Tregister):string;

    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}

    function flags_to_cond(const f: TResFlagsEnum) : TAsmCond;

    { Checks if Subset is a subset of c (e.g. "less than" is a subset of "less than or equal" }
    function condition_in(const Subset, c: TAsmCond): Boolean;

    function dwarf_reg(r:tregister):shortint;
    function dwarf_reg_no_error(r:tregister):shortint;
    function eh_return_data_regno(nr: longint): longint;

  implementation

    uses
      systems,rgBase,verbose;


    const
      std_regname_table : TRegNameTable = (
        {$i rxtensastd.inc}
      );

      regnumber_index : array[tregisterindex] of tregisterindex = (
        {$i rxtensarni.inc}
      );

      std_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rxtensasri.inc}
      );


    function cgsize2subreg(regtype: tregistertype; s:Tcgsize):Tsubregister;
      begin
        case regtype of
          R_MMREGISTER:
            begin
              case s of
                { records passed in MM registers }
                OS_32,
                OS_F32:
                  cgsize2subreg:=R_SUBFS;
                OS_64,
                OS_F64:
                  cgsize2subreg:=R_SUBFD;
                else
                  internalerror(2009112701);
              end;
            end;
          else
            cgsize2subreg:=R_SUBWHOLE;
        end;
      end;


    function reg_cgsize(const reg: tregister): tcgsize;
      begin
        case getregtype(reg) of
          R_INTREGISTER :
            reg_cgsize:=OS_32;
          R_FPUREGISTER :
            reg_cgsize:=OS_F32;
          else
            internalerror(2020040501);
          end;
        end;


    function is_calljmp(o:tasmop):boolean;{$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        { This isn't 100% perfect because the arm allows jumps also by writing to PC=R15.
          To overcome this problem we simply forbid that FPC generates jumps by loading R15 }
        is_calljmp:= o in [A_B,A_CALL0,A_CALL4,A_CALL8,A_CALL12,A_CALLX0,A_CALLX4,A_CALLX8,A_CALLX12];
      end;


    function findreg_by_number(r:Tregister):tregisterindex;
      begin
        result:=rgBase.findreg_by_number_table(r,regnumber_index);
      end;


    function std_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,std_regname_table,std_regname_index)];
      end;


    function std_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number_table(r,regnumber_index);
        if p<>0 then
          result:=std_regname_table[p]
        else
          result:=generic_regname(r);
      end;


    function inverse_cond(const c: TAsmCond): TAsmCond; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      const
        inverse: array[TAsmCond] of TAsmCond=(C_None,
          C_NE,C_EQ,
          C_LT,C_GE,C_LTU,C_GEU,
          C_BNONE,C_ANY,C_NALL,C_BNONE,C_BS,C_BC,C_BSI,C_BCI,

          C_NEZ,C_EQZ,C_GEZ,C_LTZ,
          C_NEI,C_EQI,C_GEI,C_LTI,C_GEUI,C_LTUI,
          C_T,C_F
        );
      begin
        result := inverse[c];
      end;


    function flags_to_cond(const f: TResFlagsEnum) : TAsmCond;
      const flags2cond: array[TResFlagsEnum] of tasmcond = (
          C_F,
          C_T);
      begin
        flags_to_cond := flags2cond[f];
      end;


    function conditions_equal(const c1, c2: TAsmCond): boolean; {$ifdef USEINLINE}inline;{$endif USEINLINE}
      begin
        result := c1 = c2;
      end;


    { Checks if Subset is a subset of c (e.g. "less than" is a subset of "less than or equal" }
    function condition_in(const Subset, c: TAsmCond): Boolean;
      begin
        Result := (c = C_None) or conditions_equal(Subset, c);

        { Please update as necessary. [Kit] }
        Result := False;
      end;


    function dwarf_reg(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
        if result=-1 then
          internalerror(200603251);
      end;


    function dwarf_reg_no_error(r:tregister):shortint;
      begin
        result:=regdwarf_table[findreg_by_number(r)];
      end;


    function eh_return_data_regno(nr: longint): longint;
      begin
        if (nr>=0) and (nr<2) then
          result:=nr
        else
          result:=-1;
      end;

end.

