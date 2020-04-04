{
    Copyright (c) 2020 by Nikolay Nikolov

    Convert z80ins.dat to a set of .inc files for usage with
    the Free pascal compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

program mkz80ins;

{$mode objfpc}{$H+}

uses
  SysUtils,StrUtils;

const
  Version = '1.0.0';
  HeaderStr = '{ don''t edit, this file is generated from z80ins.dat; to regenerate, run ''make insdat'' in the compiler directory }';

type

  { TZ80InsDatOutputFiles }

  TZ80InsDatOutputFiles = class
  public
    OpFile: TextFile;
    NOpFile: TextFile;
    StdOpNames: TextFile;

    constructor Create;
    destructor Destroy;override;
  end;

constructor TZ80InsDatOutputFiles.Create;
  begin
    AssignFile(OpFile,'z80op.inc');
    Rewrite(OpFile);
    Writeln(OpFile,HeaderStr);
    Writeln(OpFile,'(');
    AssignFile(NOpFile,'z80nop.inc');
    Rewrite(NOpFile);
    Writeln(NOpFile,HeaderStr);
    AssignFile(StdOpNames,'z80stdopnames.inc');
    Rewrite(StdOpNames);
    Writeln(StdOpNames,HeaderStr);
    Writeln(StdOpNames,'(');
  end;

destructor TZ80InsDatOutputFiles.Destroy;
  begin
    CloseFile(OpFile);
    CloseFile(NOpFile);
    CloseFile(StdOpNames);
    inherited Destroy;
  end;

var
  InsDatFile: TextFile;
  OutputFiles: TZ80InsDatOutputFiles=nil;
  S, op: string;
  FirstIns: Boolean=true;
  OpCount: Integer=0;
begin
  writeln('FPC Z80 Instruction Table Converter Version ',Version);
  AssignFile(InsDatFile,'../z80/z80ins.dat');
  Reset(InsDatFile);
  try
    OutputFiles:=TZ80InsDatOutputFiles.Create;
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
          end;
      end;
    Writeln(OutputFiles.OpFile,');');
    Writeln(OutputFiles.StdOpNames,');');
    Writeln(OutputFiles.NOpFile,OpCount,';');
  finally
    FreeAndNil(OutputFiles);
    CloseFile(InsDatFile);
  end;
end.

