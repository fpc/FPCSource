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

procedure TH2PasCheckerCodeGen.StartRecord(RecordID: TIdentifier);
begin
  FCurrentRecord := RecordID;
  Writeln(FLangOutput[lPascal], '  Writeln(''SizeOf(', RecordID[CommLangID], ')='',SizeOf(', RecordID[lPascal], '));');
  Writeln(FLangOutput[lC], '  printf("SizeOf(', RecordID[CommLangID], ')=%lu\n",sizeof(', RecordID[lC], '));');
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
                '@RECORD':
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
                    StartRecord(ID);
                  end;
                else
                  Error;
              end;
            end;
          '.':
            begin
              Delete(InS, 1, 1);
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

