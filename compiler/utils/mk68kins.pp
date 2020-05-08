{
    Copyright (c) 2020 by Karoly Balogh

    Convert m68kins.dat to a set of .inc files for the m68k backend

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program mk68kins;

{$mode objfpc}{$H+}

uses
  SysUtils,StrUtils;

const
  Version = '1.0.0';
  HeaderStr = '{ don''t edit, this file is generated from m68kins.dat; to regenerate, run ''make insdat'' in the compiler directory }';
  max_operands = 6;

type
  TOperandType = (
    OT_DATA,
    OT_ADDR,
    OT_ADDR_INDIR,
    OT_ADDR_INDIR_POSTINC,
    OT_ADDR_INDIR_PREDEC,
    OT_ADDR_DISP16,
    OT_ADDR_IDX_DISP8,
    OT_ABS_SHORT,
    OT_ABS_LONG,
    OT_PC_DISP16,
    OT_PC_IDX_DISP8,
    OT_IMMEDIATE,
    OT_REG_LIST,
    OT_FPUREG_LIST,
    OT_FPUREG,
    OT_SPECIALREG
  );

  TOperandFlags = (
    OF_IMM_QUICK,
    OF_IMM_FLOAT,
    OF_IMM_64BIT,
    OF_SPECREG,
    OF_SPECREG_CCR,
    OF_SPECREG_SR,
    OF_SPECREG_USP,
    OF_SPECREG_FPIAR,
    OF_SPECREG_FPU,
    OF_BITFIELD,
    OF_BRANCH,
    OF_DOUBLE_REG,
    OF_KFACTOR,
    OF_NOSIZE
  );

  TOpSizeFlag = (
    OPS_UNSIZED,
    OPS_SHORT,
    OPS_BYTE,
    OPS_WORD,
    OPS_LONG,
    OPS_QUAD,
    OPS_SINGLE,
    OPS_DOUBLE,
    OPS_EXTENDED,
    OPS_PACKED,
    OPS_COLDFIRE
  );

  TOpSupported = (
    OS_M68000,
    OS_M68000UP,
    OS_M68010UP,
    OS_M68020,
    OS_M68020UP,
    OS_M68030,
    OS_M68040,
    OS_M68040UP,
    OS_M68060,
    OS_M68881,
    OS_M68851,
    OS_CPU32,
    OS_CF,
    OS_CF_ISA_A,
    OS_CF_ISA_APL,
    OS_CF_ISA_B,
    OS_CF_ISA_C,
    OS_CF_HWDIV,
    OS_CF_FPU,
    OS_CF_USP,
    OS_GNU_AS
  );

  TParamType = record
    id: string[32];
    modes: set of TOperandType;
    flags: set of TOperandFlags;
  end;

  TFlagsType = record
    id: string[32];
    flags: set of TOpSizeFlag;
  end;

  TSupportType = record
    id: string[32];
    flag: TOPSupported;
  end;

const
  OpSizes: array[0..16] of TFlagsType = (
    (id: 'UNS';    flags: [OPS_UNSIZED]),
    (id: 'B';      flags: [OPS_BYTE]),
    (id: 'W';      flags: [OPS_WORD]),
    (id: 'L';      flags: [OPS_LONG]),
    (id: 'Q';      flags: [OPS_QUAD]),
    (id: 'BW';     flags: [OPS_BYTE,OPS_WORD]),
    (id: 'BWL';    flags: [OPS_BYTE,OPS_WORD,OPS_LONG]),
    (id: 'WL';     flags: [OPS_WORD,OPS_LONG]),
    (id: 'SBW';    flags: [OPS_SHORT,OPS_BYTE,OPS_WORD]),
    (id: 'SBWL';   flags: [OPS_SHORT,OPS_BYTE,OPS_WORD,OPS_LONG]),
    (id: 'CFWL';   flags: [OPS_WORD,OPS_LONG,OPS_COLDFIRE]),
    (id: 'CFBWL';  flags: [OPS_BYTE,OPS_WORD,OPS_LONG,OPS_COLDFIRE]),
    (id: 'FD';     flags: [OPS_DOUBLE]),
    (id: 'FX';     flags: [OPS_EXTENDED]),
    (id: 'FP';     flags: [OPS_PACKED]),
    (id: 'ANY';    flags: [OPS_BYTE,OPS_WORD,OPS_LONG,OPS_SINGLE,OPS_DOUBLE,OPS_EXTENDED,OPS_PACKED]),
    (id: 'CFANY';  flags: [OPS_BYTE,OPS_WORD,OPS_LONG,OPS_SINGLE,OPS_DOUBLE,OPS_COLDFIRE])
  );

const
  OpSupport: array[0..19] of TSupportType = (
    (id: 'm68000up';   flag: OS_M68000UP),
    (id: 'm68010up';   flag: OS_M68010UP),
    (id: 'm68020';     flag: OS_M68020),
    (id: 'm68020up';   flag: OS_M68020UP),
    (id: 'm68030';     flag: OS_M68030),
    (id: 'm68040';     flag: OS_M68040),
    (id: 'm68040up';   flag: OS_M68040UP),
    (id: 'm68060';     flag: OS_M68060),
    (id: 'm68881';     flag: OS_M68881),
    (id: 'm68851';     flag: OS_M68851),
    (id: 'cpu32';      flag: OS_CPU32),
    (id: 'cf';         flag: OS_CF),
    (id: 'cf_isa_a';   flag: OS_CF_ISA_A),
    (id: 'cf_isa_apl'; flag: OS_CF_ISA_APL),
    (id: 'cf_isa_b';   flag: OS_CF_ISA_B),
    (id: 'cf_isa_c';   flag: OS_CF_ISA_C),
    (id: 'cf_hwdiv';   flag: OS_CF_HWDIV),
    (id: 'cf_fpu';     flag: OS_CF_FPU),
    (id: 'cf_usp';     flag: OS_CF_USP),
    (id: 'gnu_as';     flag: OS_GNU_AS)
  );

const
  ParamTypes: array [0..63] of TParamType = (
    (id: 'void';     modes: []; flags: []),
    (id: '#imm';     modes: [OT_IMMEDIATE]; flags: []),
    (id: '#immq';    modes: [OT_IMMEDIATE]; flags: [OF_NOSIZE,OF_IMM_QUICK]),
    (id: '#immregs'; modes: [OT_IMMEDIATE]; flags: [OF_NOSIZE]), 
    (id: 'Dx';       modes: [OT_DATA]; flags: []),
    (id: 'Dx:Dx';    modes: [OT_DATA]; flags: [OF_DOUBLE_REG]),
    (id: 'Rx';       modes: [OT_DATA, OT_ADDR]; flags: []),
    (id: 'Ax';       modes: [OT_ADDR]; flags: []),
    (id: '(Ax)';     modes: [OT_ADDR_INDIR]; flags: []),
    (id: '-(Ax)';    modes: [OT_ADDR_INDIR_PREDEC]; flags: []),
    (id: '(Ax)+';    modes: [OT_ADDR_INDIR_POSTINC]; flags: []),
    (id: 'd16(Ax)';  modes: [OT_ADDR_DISP16]; flags: []),
    (id: 'Dx-Ax';    modes: [OT_REG_LIST]; flags: []),
    (id: 'FPx';      modes: [OT_FPUREG]; flags: []),
    (id: 'FPx:FPx';  modes: [OT_FPUREG]; flags: [OF_DOUBLE_REG]),
    (id: 'FPx-FPx';  modes: [OT_FPUREG_LIST]; flags: []),
    (id: 'FPspec-list'; modes: [OT_FPUREG_LIST]; flags: [OF_SPECREG, OF_SPECREG_FPU]),

    (id: 'CCR';      modes: [OT_SPECIALREG]; flags: [OF_SPECREG, OF_SPECREG_CCR]),
    (id: 'SR';       modes: [OT_SPECIALREG]; flags: [OF_SPECREG, OF_SPECREG_SR]),
    (id: 'USP';      modes: [OT_SPECIALREG]; flags: [OF_SPECREG, OF_SPECREG_USP]),
    (id: 'CTRL';     modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'FC';       modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'RP_030';   modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'RP_851';   modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'TC';       modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'AC';       modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'M1_B';     modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'BAD';      modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'BAC';      modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'PSR';      modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'PCSR';     modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'TT';       modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'VAL';      modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),
    (id: 'FPIAR';    modes: [OT_SPECIALREG]; flags: [OF_SPECREG, OF_SPECREG_FPIAR]),
    (id: 'FPspec';   modes: [OT_SPECIALREG]; flags: [OF_SPECREG, OF_SPECREG_FPU]),
    (id: '<caches>'; modes: [OT_SPECIALREG]; flags: [OF_SPECREG]),

    (id: '<addr>';   modes: [OT_ABS_LONG]; flags: []),
    (id: '<dest>';   modes: [OT_ABS_LONG]; flags: [OF_BRANCH]),
    (id: '<value>';   modes: [OT_ABS_LONG]; flags: [OF_NOSIZE]),

    (id: '(Rx):(Rx)'; modes: [OT_ADDR_INDIR]; flags: [OF_DOUBLE_REG]),

    (id: '<ea-any>';
     modes: [OT_DATA,OT_ADDR,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8,OT_IMMEDIATE];
     flags: []),
    (id: '<ea-mem>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8,OT_IMMEDIATE];
     flags: []),
    (id: '<ea-mem-alter>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: []),
    (id: '<ea-mem-noimm>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8];
     flags: []),
    (id: '<ea-mem-fpuimm>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8,OT_IMMEDIATE];
     flags: [OF_IMM_FLOAT]),
    (id: '<ea-mem-alter-kf>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: [OF_KFACTOR]),
    (id: '<ea-mem-save>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: []),
    (id: '<ea-mem-restore>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: []),
    (id: '<ea-mem-imm64>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8,OT_IMMEDIATE];
     flags: [OF_IMM_64BIT]),
    (id: '<ea-data>';
     modes: [OT_DATA,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8,OT_IMMEDIATE];
     flags: []),
    (id: '<ea-data-noimm>';
     modes: [OT_DATA,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8];
     flags: []),
    (id: '<ea-data-alter>';
     modes: [OT_DATA,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: []),
    (id: '<ea-data-alter-bf>';
     modes: [OT_DATA,OT_ADDR_INDIR,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: [OF_BITFIELD]),
    (id: '<ea-data-control-bf>';
     modes: [OT_DATA,OT_ADDR_INDIR,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8];
     flags: [OF_BITFIELD]),
    (id: '<ea-control>';
     modes: [OT_ADDR_INDIR,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG,
             OT_PC_DISP16,OT_PC_IDX_DISP8];
     flags: []),
    (id: '<ea-control-alter>';
     modes: [OT_ADDR_INDIR,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: []),
    (id: '<ea-alter>';
     modes: [OT_DATA,OT_ADDR,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_ADDR_IDX_DISP8,OT_ABS_SHORT,OT_ABS_LONG];
     flags: []),
    (id: '<ea-movep>';
     modes: [OT_ADDR_INDIR,OT_ADDR_DISP16];
     flags: []),

    (id: '<cf-ea-mem-alter>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16];
     flags: []),
    (id: '<cf-ea-data-alter>';
     modes: [OT_DATA,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16];
     flags: []),
    (id: '<cf-ea-float>';
     modes: [OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_PC_DISP16];
     flags: []),
    (id: '<cf-ea-data-float>';
     modes: [OT_DATA,OT_ADDR_INDIR,OT_ADDR_INDIR_POSTINC,OT_ADDR_INDIR_PREDEC,
             OT_ADDR_DISP16,OT_PC_DISP16];
     flags: []),
    (id: '<cf-ea-movem>';
     modes: [OT_ADDR_INDIR,OT_ADDR_DISP16];
     flags: []),
    (id: '<cf-ea-fmovem-src>';
     modes: [OT_ADDR_INDIR,OT_ADDR_DISP16,OT_PC_DISP16];
     flags: [])
  );

function OpTypeStr(idx: integer): string;
var
  optyp: TOperandType;
begin
  result:='';
  for optyp in ParamTypes[idx].modes do
    if result='' then
      WriteStr(result,optyp)
    else
      WriteStr(result,result,', ',optyp);
end;

function FlagsToStr(idx: integer): string;
var
  flagtyp: TOperandFlags;
begin
  result:='';
  for flagtyp in ParamTypes[idx].flags do
    if result='' then
      WriteStr(result,flagtyp)
    else
      WriteStr(result,result,', ',flagtyp);
end;

function OpSizeStr(idx: integer): string;
var
  opsizeflag: TOpsizeFlag;
begin
  result:='';
  for opsizeflag in Opsizes[idx].flags do
    if result='' then
      WriteStr(result,opsizeflag)
    else
      WriteStr(result,result,', ',opsizeflag);
end;

function OpSupportStr(const sa: TStringArray): string;
var
  i: integer;
  s: string;
  flag: TOpSupported;
  idx: integer;
begin
  result:='';
  for s in sa do
    begin
      idx:=-1;
      for I:=Low(OpSupport) to High(OpSupport) do
        if OpSupport[I].id=s then
          begin
            idx:=i;
            flag:=OpSupport[i].flag;
            break;
          end;
      if idx < 0 then
        raise Exception.Create('Invalid support type: '''+s+'''');
      if result='' then
        WriteStr(result,flag)
      else
        WriteStr(result,result,', ',flag);
    end;
end;

type

  { T68kInsDatOutputFiles }

  T68kInsDatOutputFiles = class
  public
    OpFile: TextFile;
    NOpFile: TextFile;
    StdOpNames: TextFile;
    InsTabFile: TextFile;

    constructor Create;
    destructor Destroy;override;
  end;

constructor T68kInsDatOutputFiles.Create;
  begin
    AssignFile(OpFile,'m68kop.inc');
    Rewrite(OpFile);
    Writeln(OpFile,HeaderStr);
    Writeln(OpFile,'(');
    AssignFile(NOpFile,'m68knop.inc');
    Rewrite(NOpFile);
    Writeln(NOpFile,HeaderStr);
    AssignFile(StdOpNames,'m68kstd.inc');
    Rewrite(StdOpNames);
    Writeln(StdOpNames,HeaderStr);
    Writeln(StdOpNames,'(');
    AssignFile(InsTabFile,'m68ktab.inc');
    Rewrite(InsTabFile);
    Writeln(InsTabFile,HeaderStr);
    Writeln(InsTabFile,'(');
  end;

destructor T68kInsDatOutputFiles.Destroy;
  begin
    CloseFile(OpFile);
    CloseFile(NOpFile);
    CloseFile(StdOpNames);
    CloseFile(InsTabFile);
    inherited Destroy;
  end;

function FindParamType(const ParamTypeStr: string): Integer;
var
  I: Integer;
begin
  for I:=Low(ParamTypes) to High(ParamTypes) do
    if ParamTypes[I].id=ParamTypeStr then
      exit(I);
  raise Exception.Create('Invalid param type: '''+ParamTypeStr+'''');
end;

function FindOpsize(const SizeStr: string): Integer;
var
  I: Integer;
begin
  for I:=Low(Opsizes) to High(Opsizes) do
    if Opsizes[I].id=SizeStr then
      exit(I);
  raise Exception.Create('Invalid size: '''+SizeStr+'''');
end;

var
  InsDatFile: TextFile;
  OutputFiles: T68kInsDatOutputFiles=nil;
  S, op, ParamsStr: string;
  FirstIns: Boolean=true;
  OpCount: Integer=0;
  S_Split, S_Params, S_Support: TStringArray;
  ParamIdx: Integer;
begin
  writeln('FPC m68k Instruction Table Converter Version ',Version);
  AssignFile(InsDatFile,'./m68kins.dat');
  Reset(InsDatFile);
  try
    OutputFiles:=T68kInsDatOutputFiles.Create;
    while not EoF(InsDatFile) do
      begin
        Readln(InsDatFile,S);
        S:=Trim(S);
        if AnsiStartsStr(';',S) then
          continue
        else if AnsiStartsStr('[',S) then
          begin
            op:=Copy(S,2,Length(S)-2);
            if not FirstIns then
              begin
                Writeln(OutputFiles.OpFile,',');
                Writeln(OutputFiles.StdOpNames,',');
              end;
            FirstIns:=False;
            Write(OutputFiles.OpFile,'A_'+op);
            Write(OutputFiles.StdOpNames,''''+LowerCase(op)+'''');
          end
        else if S<>'' then
          begin
            Inc(OpCount);
            if OpCount<>1 then
              Writeln(OutputFiles.InsTabFile,',');
            S_Split:=S.Split(' ',TStringSplitOptions.ExcludeEmpty);
            S_Params:=S_Split[0].Split(',',TStringSplitOptions.ExcludeEmpty);
            S_Support:=S_Split[4].Split(',',TStringSplitOptions.ExcludeEmpty);
            if (Length(S_Params)=1) and (S_Params[0]='void') then
              SetLength(S_Params,0);
            Writeln(OutputFiles.InsTabFile,'  (');
            Writeln(OutputFiles.InsTabFile,'    opcode  : A_',op,';');
            Writeln(OutputFiles.InsTabFile,'    ops     : ',Length(S_Params),';');
            Write(OutputFiles.InsTabFile,  '    optypes : (');
            if Length(S_Params)>max_operands then
              raise Exception.Create('Too many operands');
            for ParamIdx:=0 to max_operands-1 do
              begin
                if ParamIdx<>0 then
                  Write(OutputFiles.InsTabFile,',');
                if ParamIdx<=High(S_Params) then
                  Write(OutputFiles.InsTabFile,'[',OpTypeStr(FindParamType(S_Params[ParamIdx])),']')
                else
                  Write(OutputFiles.InsTabFile,'[]');
              end;
            Writeln(OutputFiles.InsTabFile, ');');
            Write(OutputFiles.InsTabFile,  '    opflags : (');
            if Length(S_Params)>max_operands then
              raise Exception.Create('Too many operands');
            for ParamIdx:=0 to max_operands-1 do
              begin
                if ParamIdx<>0 then
                  Write(OutputFiles.InsTabFile,',');
                if ParamIdx<=High(S_Params) then
                  Write(OutputFiles.InsTabFile,'[',FlagsToStr(FindParamType(S_Params[ParamIdx])),']')
                else
                  Write(OutputFiles.InsTabFile,'[]');
              end;
            Writeln(OutputFiles.InsTabFile, ');');
            Writeln(OutputFiles.InsTabFile,  '    codelen : ',S_Split[2],';');
            Writeln(OutputFiles.InsTabFile,  '    code    : (',S_Split[1],');');
            Writeln(OutputFiles.InsTabFile,  '    support : [',OpSupportStr(S_Support),'];');
            Writeln(OutputFiles.InsTabFile,  '    sizes   : [',OpsizeStr(FindOpsize(S_Split[3])),'];');
            Write(OutputFiles.InsTabFile,  '  )');
          end;
      end;
    Writeln(OutputFiles.OpFile,');');
    Writeln(OutputFiles.StdOpNames,');');
    Writeln(OutputFiles.NOpFile,OpCount,';');
    Writeln(OutputFiles.InsTabFile);
    Writeln(OutputFiles.InsTabFile,');');
  finally
    FreeAndNil(OutputFiles);
    CloseFile(InsDatFile);
  end;
end.
