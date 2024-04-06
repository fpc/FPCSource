{
    Copyright (c) 2020, 2024 by Nikolay Nikolov

    Convert mos6502ins.dat to a set of .inc files for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program mk6502ins;

{$mode objfpc}{$H+}

uses
  SysUtils;

const
  Version = '1.0.0';
  HeaderStr = '{ don''t edit, this file is generated from mos6502ins.dat; to regenerate, run ''make insdat'' in the compiler directory }';
  max_operands = 2;

  ParamTypes: array [0..12,0..1] of string = (
    ('void',    'AM_IMPLICIT'),
    ('A',       'AM_ACCUMULATOR'),
    ('#',       'AM_IMMEDIATE'),
    ('zpg',     'AM_ZERO_PAGE'),
    ('zpg,X',   'AM_ZERO_PAGE_X'),
    ('zpg,Y',   'AM_ZERO_PAGE_Y'),
    ('rel',     'AM_RELATIVE'),
    ('abs',     'AM_ABSOLUTE'),
    ('abs,X',   'AM_ABSOLUTE_X'),
    ('abs,Y',   'AM_ABSOLUTE_Y'),
    ('(ind)',   'AM_INDIRECT'),
    ('(ind,X)', 'AM_INDEXED_INDIRECT'),
    ('(ind),Y', 'AM_INDIRECT_INDEXED')
  );

type

  { TMOS6502InsDatOutputFiles }

  TMOS6502InsDatOutputFiles = class
  public
    OpFile: TextFile;
    NOpFile: TextFile;
    StdOpNames: TextFile;
    InsTabFile: TextFile;

    constructor Create;
    destructor Destroy;override;
  end;

{ ***************************************************************************
  the routines LeftStr, AnsiStartsStr are copied and reformatted
  from StrUtils and thus covered by the copyright of strutils (see below) as compiler utilities cannot
  depend on packages

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*************************************************************************** }

function LeftStr(const AText: AnsiString; const ACount: SizeInt): AnsiString;
  begin
    Result:=Copy(AText,1,ACount);
  end;


function AnsiStartsStr(const ASubText, AText: string): Boolean;
  begin
    Result := (ASubText = '') or (LeftStr(AText, Length(ASubText)) = ASubText);
  end;

{ ***************************************************************************
 end of StrUtils code
***************************************************************************}

function PasEncode(const S: string): string;
  var
    Ch: Char;
    InQuotes: Boolean;
  begin
    Result:='';
    InQuotes:=False;
    for Ch in S do
      if (Ch>=#32) and (Ch<=#126) then
        begin
          if not InQuotes then
            begin
              Result:=Result+'''';
              InQuotes:=True;
            end;
          if Ch='''' then
            Result:=Result+''''''
          else
            Result:=Result+Ch;
        end
      else
        begin
          if InQuotes then
            begin
              Result:=Result+'''';
              InQuotes:=False;
            end;
          Result:=Result+'#'+IntToStr(Ord(Ch));
        end;
    if InQuotes then
      Result:=Result+'''';
    if Result='' then
      Result:='''''';
  end;

constructor TMOS6502InsDatOutputFiles.Create;
  begin
    AssignFile(OpFile,'mos6502op.inc');
    Rewrite(OpFile);
    Writeln(OpFile,HeaderStr);
    Writeln(OpFile,'(');
    AssignFile(NOpFile,'mos6502nop.inc');
    Rewrite(NOpFile);
    Writeln(NOpFile,HeaderStr);
    AssignFile(StdOpNames,'mos6502stdopnames.inc');
    Rewrite(StdOpNames);
    Writeln(StdOpNames,HeaderStr);
    Writeln(StdOpNames,'(');
    AssignFile(InsTabFile,'mos6502tab.inc');
    Rewrite(InsTabFile);
    Writeln(InsTabFile,HeaderStr);
    Writeln(InsTabFile,'(');
  end;

destructor TMOS6502InsDatOutputFiles.Destroy;
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
    if ParamTypes[I,0]=ParamTypeStr then
      exit(I);
  raise Exception.Create('Invalid param type: '''+ParamTypeStr+'''');
end;

var
  InsDatFile: TextFile;
  OutputFiles: TMOS6502InsDatOutputFiles=nil;
  S, op, ParamsStr, S_AddrMode: string;
  FirstIns: Boolean=true;
  OpCount: Integer=0;
  S_Split: TStringArray;
begin
  writeln('FPC 6502 Instruction Table Converter Version ',Version);
  AssignFile(InsDatFile,'../mos6502/mos6502ins.dat');
  Reset(InsDatFile);
  try
    OutputFiles:=TMOS6502InsDatOutputFiles.Create;
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
            S_AddrMode:=S_Split[0];
            Writeln(OutputFiles.InsTabFile,'  (');
            Writeln(OutputFiles.InsTabFile,'    opcode  : A_',op,';');
            Writeln(OutputFiles.InsTabFile,  '    adrmode : ',ParamTypes[FindParamType(S_AddrMode),1],';');
            Writeln(OutputFiles.InsTabFile,  '    code    : ',S_Split[1],';');
            Writeln(OutputFiles.InsTabFile,  '    flags   : 0');
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

