{
    Copyright (c) 2019 by Nikolay Nikolov

    C structure checker tool. Generates Pascal and C code that prints the
    size of each structure and the size and offset of each of its members.
    Useful for checking ABI compatibility of Pascal bindings to C libraries.

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

program h2paschk;

(* Example input file:

@Pascal uses p_ddraw;
@Pascal begin

@C #include <ddraw.h>
@C #include <stdio.h>
@C #include <stddef.h>
@C #include <tchar.h>
@C int _tmain(int argc, _TCHAR* argv[])
@C {

@record TDDARGB,DDARGB
.blue
.green
.red
.alpha

@record TDDRGBA,DDRGBA
.red
.green
.blue
.alpha

@TYPE size_t
@CONSTANT O_RW

@C   return 0;
@C }

@Pascal end.

*)

{ TODO: Currently the files, describing the records, must be created manually.
  However, it would be extremely useful, if h2pas could also create them
  automatically. }

{$MODE objfpc}{$H+}

uses
  SysUtils;

type
  TLanguage = (lPascal, lC);
  TLanguages = set of TLanguage;
  TIdentifier = array [TLanguage] of string;

const
  DefaultExtension: array [TLanguage] of string = ('pas', 'c');
  CommLangID = lPascal;

type

  { TH2PasCheckerCodeGen }

  TH2PasCheckerCodeGen = class
  private
    FInFileName: string;
    FOpenLanguageOutputs: TLanguages;
    FLangOutput: array [TLanguage] of TextFile;

    FCurrentRecord: TIdentifier;

    procedure Error;
    procedure InitLangOutput(Lang: TLanguage);
    procedure DoneLangOutput(Lang: TLanguage);
    procedure DoneLangOutputs;

    procedure StartRecord(RecordID: TIdentifier);
    procedure HandleType(TypeID: TIdentifier);
    procedure HandleConstant(ConstantID: TIdentifier;PascalType,PascalHexStrSize,CType,CFormat : string);
    procedure HandleConstant(ConstantID: TIdentifier);
    procedure HandleConstantU8(ConstantID: TIdentifier);
    procedure HandleConstantU16(ConstantID: TIdentifier);
    procedure HandleConstantU32(ConstantID: TIdentifier);
    procedure HandleConstantU64(ConstantID: TIdentifier);
    procedure HandleSignedConstant(ConstantID: TIdentifier;PascalType,CType,CFormat : string);
    procedure HandleSignedConstant(ConstantID: TIdentifier);
    procedure HandleConstantS8(ConstantID: TIdentifier);
    procedure HandleConstantS16(ConstantID: TIdentifier);
    procedure HandleConstantS32(ConstantID: TIdentifier);
    procedure HandleConstantS64(ConstantID: TIdentifier);
    procedure HandleFloatConstant(ConstantID: TIdentifier);
    procedure ProcessField(FieldID: TIdentifier);
  public
    destructor Destroy; override;

    procedure ProcessH2PasChk(const InFileName: string);
  end;

procedure TH2PasCheckerCodeGen.Error;
begin
  raise Exception.Create('Error!');
end;

procedure TH2PasCheckerCodeGen.InitLangOutput(Lang: TLanguage);
begin
  AssignFile(FLangOutput[Lang], ChangeFileExt(FInFileName, '.' + DefaultExtension[Lang]));
  ReWrite(FLangOutput[Lang]);
  Include(FOpenLanguageOutputs, Lang);
end;

procedure TH2PasCheckerCodeGen.DoneLangOutput(Lang: TLanguage);
begin
  if Lang in FOpenLanguageOutputs then
  begin
    CloseFile(FLangOutput[Lang]);
    Exclude(FOpenLanguageOutputs, Lang);
  end;
end;

procedure TH2PasCheckerCodeGen.DoneLangOutputs;
var
  Lang: TLanguage;
begin
  for Lang in TLanguage do
    DoneLangOutput(Lang);
end;


procedure TH2PasCheckerCodeGen.HandleType(TypeID: TIdentifier);
begin
  Writeln(FLangOutput[lPascal], '  Writeln(''SizeOf(', TypeID[CommLangID], ')='',SizeOf(', TypeID[lPascal], '));');
  Writeln(FLangOutput[lC], '  printf("SizeOf(', TypeID[CommLangID], ')=%lu\n",sizeof(', TypeID[lC], '));');
end;

procedure TH2PasCheckerCodeGen.HandleConstant(ConstantID: TIdentifier;PascalType,PascalHexStrSize,CType,CFormat : string);
begin
  Writeln(FLangOutput[lPascal], '  Writeln(''Unsigned value Of(', ConstantID[CommLangID], ')=0x'',hexstr(',PascalType,'(', ConstantID[lPascal], '),',PascalHexStrSize,'));');
  Writeln(FLangOutput[lC], '  printf("Unsigned value Of(', ConstantID[CommLangID], ')=0x',CFormat,'\n",(',CType,') ', ConstantID[lC], ');');
end;

procedure TH2PasCheckerCodeGen.HandleSignedConstant(ConstantID: TIdentifier;PascalType,CType,CFormat : string);
begin
  Writeln(FLangOutput[lPascal], '  Writeln(''Signed value Of(', ConstantID[CommLangID], ')='',',PascalType,'(', ConstantID[lPascal],'));');
  Writeln(FLangOutput[lC], '  printf("Signed value Of(', ConstantID[CommLangID], ')=',CFormat,'\n",(',CType,') ', ConstantID[lC],');');
end;

procedure TH2PasCheckerCodeGen.HandleFloatConstant(ConstantID: TIdentifier);
begin
  Writeln(FLangOutput[lPascal], '  Writeln(''Value Of(', ConstantID[CommLangID], ')='',',ConstantID[lPascal],':25:25);');
  Writeln(FLangOutput[lC], '  printf("Value Of(', ConstantID[CommLangID], ')=%0.25f\n",',ConstantID[lC],');');
end;

procedure TH2PasCheckerCodeGen.HandleConstant(ConstantID: TIdentifier);
begin
  HandleConstant(ConstantID,'qword','16','unsigned long long','%016llX');
end;

procedure TH2PasCheckerCodeGen.HandleConstantU8(ConstantID: TIdentifier);
begin
  HandleConstant(ConstantID,'byte','2','unsigned char','%02X');
end;

procedure TH2PasCheckerCodeGen.HandleConstantU16(ConstantID: TIdentifier);
begin
  HandleConstant(ConstantID,'word','4','unsigned short','%04X');
end;

procedure TH2PasCheckerCodeGen.HandleConstantU32(ConstantID: TIdentifier);
begin
  HandleConstant(ConstantID,'dword','8','unsigned int','%08X');
end;

procedure TH2PasCheckerCodeGen.HandleConstantU64(ConstantID: TIdentifier);
begin
  HandleConstant(ConstantID,'qword','16','unsigned int','%016llX');
end;

procedure TH2PasCheckerCodeGen.HandleSignedConstant(ConstantID: TIdentifier);
begin
  HandleSignedConstant(ConstantID,'int64','signed long long','%lld');
end;

procedure TH2PasCheckerCodeGen.HandleConstantS8(ConstantID: TIdentifier);
begin
  HandleSignedConstant(ConstantID,'int8','signed char','%d');
end;

procedure TH2PasCheckerCodeGen.HandleConstantS16(ConstantID: TIdentifier);
begin
  HandleSignedConstant(ConstantID,'int16','signed short','%d');
end;

procedure TH2PasCheckerCodeGen.HandleConstantS32(ConstantID: TIdentifier);
begin
  HandleSignedConstant(ConstantID,'int32','signed int','%d');
end;

procedure TH2PasCheckerCodeGen.HandleConstantS64(ConstantID: TIdentifier);
begin
  HandleSignedConstant(ConstantID,'int64','unsigned int','%lld');
end;


procedure TH2PasCheckerCodeGen.StartRecord(RecordID: TIdentifier);
begin
  FCurrentRecord := RecordID;
  HandleType(RecordID);
end;

procedure TH2PasCheckerCodeGen.ProcessField(FieldID: TIdentifier);
begin
  Writeln(FLangOutput[lPascal], '  Writeln(''SizeOf(', FCurrentRecord[CommLangID], '.', FieldID[CommLangID], ')='',SizeOf(', FCurrentRecord[lPascal], '.', FieldID[lPascal], '));');
  Writeln(FLangOutput[lPascal], '  Writeln(''OffsetOf(', FCurrentRecord[CommLangID], ',', FieldID[CommLangID], ')='',PtrUInt(@', FCurrentRecord[lPascal], '(nil^).', FieldID[lPascal], '));');
  Writeln(FLangOutput[lC], '  printf("SizeOf(', FCurrentRecord[CommLangID], '.', FieldID[CommLangID], ')=%lu\n",sizeof(((', FCurrentRecord[lC], '*)0)->', FieldID[lC], '));');
  Writeln(FLangOutput[lC], '  printf("OffsetOf(', FCurrentRecord[CommLangID], ',', FieldID[CommLangID], ')=%lu\n",offsetof(', FCurrentRecord[lC], ',', FieldID[lC], '));');
end;

destructor TH2PasCheckerCodeGen.Destroy;
begin
  DoneLangOutputs;
  inherited Destroy;
end;

procedure TH2PasCheckerCodeGen.ProcessH2PasChk(const InFileName: string);
var
  InF: TextFile;
  InS: string;
  Command: string;
  I: Integer;
  ID: TIdentifier;
  Lang: TLanguage;

  procedure ReadID;
  begin
    if Pos(',', InS) >= 1 then
    begin
      for Lang in TLanguage do
      begin
        if Pos(',', InS) >= 1 then
        begin
          ID[Lang] := Copy(InS, 1, Pos(',', InS) - 1);
          Delete(InS, 1, Pos(',', InS));
        end
        else
        begin
          ID[Lang] := InS;
          InS := '';
        end;
      end;
    end
    else
      for Lang in TLanguage do
        ID[Lang] := InS;
  end;

begin
  FInFileName := InFileName;
  AssignFile(InF, InFileName);
  Reset(InF);
  try
    InitLangOutput(lPascal);
    InitLangOutput(lC);
    while not EoF(InF) do
    begin
      ReadLn(InF, InS);
      InS := TrimLeft(InS);
      if Length(InS) > 1 then
      begin
        case InS[1] of
          '#':
            begin
              { skip comment; nothing to do... }
            end;
          '@':
            begin
              Command := '@';
              I := 2;
              while (Length(InS) >= I) and (UpCase(InS[I]) in ['A'..'Z']) do
              begin
                Command := Command + UpCase(InS[I]);
                Inc(I);
              end;
              Delete(InS, 1, Length(Command));
              if (Length(InS) >= 1) and (InS[1] = ' ') then
                Delete(InS, 1, 1);

              case Command of
                '@PASCAL':
                  Writeln(FLangOutput[lPascal], InS);
                '@C':
                  Writeln(FLangOutput[lC], InS);
                '@FLOATCONSTANT':
                  begin
                    ReadID;
                    HandleFloatConstant(ID);
                  end;
                '@CONSTANT':
                  begin
                    ReadID;
                    HandleConstant(ID);
                  end;
                '@CONSTANT_U8':
                  begin
                    ReadID;
                    HandleConstantU8(ID);
                  end;
                '@CONSTANT_U16':
                  begin
                    ReadID;
                    HandleConstantU16(ID);
                  end;
                '@CONSTANT_U32':
                  begin
                    ReadID;
                    HandleConstantU32(ID);
                  end;
                '@CONSTANT_U64':
                  begin
                    ReadID;
                    HandleConstantU64(ID);
                  end;
                '@CONSTANT_S':
                  begin
                    ReadID;
                    HandleSignedConstant(ID);
                  end;
                '@CONSTANT_S8':
                  begin
                    ReadID;
                    HandleConstantS8(ID);
                  end;
                '@CONSTANT_S16':
                  begin
                    ReadID;
                    HandleConstantS16(ID);
                  end;
                '@CONSTANT_S32':
                  begin
                    ReadID;
                    HandleConstantU32(ID);
                  end;
                '@CONSTANT_S64':
                  begin
                    ReadID;
                    HandleConstantS64(ID);
                  end;
                '@TYPE':
                  begin
                    ReadID;
                    HandleType(ID);
                  end;
                '@RECORD':
                  begin
                    ReadID;
                    StartRecord(ID);
                  end;
                else
                  Error;
              end;
            end;
          '.':
            begin
              Delete(InS, 1, 1);
              ReadID;
              ProcessField(ID);
            end;
        end;
      end;
    end;
  finally
    CloseFile(InF);
  end;
end;

var
  P: TH2PasCheckerCodeGen;
begin
  if ParamCount = 0 then
  begin
    Writeln('Usage: h2paschk <filename>');
    Halt;
  end;
  P := TH2PasCheckerCodeGen.Create;
  try
    P.ProcessH2PasChk(ParamStr(1));
  finally
    FreeAndNil(P);
  end;
end.

