{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    Data Dictionary Code Generator Implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpcgcreatedbf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpddCodeGen;
  
Type

  { TDDCreateDBFOptions }

  TDDCreateDBFOptions = Class(TCodeGeneratorOptions)
  private
    FIDent: String;
    FProcName: String;
    FCreateInstance: Boolean;
    FTableName: String;
    procedure SetIdent(const AValue: String);
    procedure SetProcName(const AValue: String);
  Public
    Constructor Create; override;
    Procedure Assign(ASource : TPersistent); override;
  Published
    Property Identifier : String Read FIDent Write SetIdent;
    Property CreateInstance : Boolean Read FCreateInstance Write FCreateInstance default True;
    Property ProcedureName : String Read FProcName Write SetProcName;
    Property TableName : String Read FTableName Write FTableName;
  end;
  
  { TDDCreateDBFGenerator }

  TDDCreateDBFGenerator = Class(TDDCustomCodeGenerator)
  Private
    FFields: TFieldPropDefs;
  Protected
    Function ProcedureDecl : String; virtual;
    Function CreateOptions : TCodeGeneratorOptions; override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
    Procedure DoGenerateInterface(Strings: TStrings); override;
    function GetFieldDefs: TFieldPropDefs; override;
    procedure SetFieldDefs(const AValue: TFieldPropDefs); override;
    Function DBFOptions : TDDCreateDBFOptions;
    Function GetImplementationUsesClause : string; override;
    Function GetInterfaceUsesClause : string; override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function NeedsFieldDefs : Boolean; override;
  end;

implementation

uses db,typinfo;

{ TDDCreateDBFOptions }

procedure TDDCreateDBFOptions.SetIdent(const AValue: String);
begin
  if FIDent=AValue then exit;
  If Not IsValidIdent(AValue) then
    Raise ECodeGenerator.CreateFmt(SErrInvalidIdentifier,[AValue]);
  FIDent:=AValue;
end;

procedure TDDCreateDBFOptions.SetProcName(const AValue: String);
begin
  if FProcName=AValue then exit;
  If Not IsValidIdent(AValue) then
    Raise ECodeGenerator.CreateFmt(SErrInvalidIdentifier,[AValue]);
  FProcName:=AValue;
end;

constructor TDDCreateDBFOptions.Create;
begin
  inherited Create;
  FCreateInstance:=True;
  FIdent:='DBF';
  FTableName:='MyTable';
  FProcName:='CreateDBF';
end;

procedure TDDCreateDBFOptions.Assign(ASource: TPersistent);

Var
  DOP : TDDCreateDBFOptions;

begin
  if ASource is TDDCreateDBFOptions then
    begin
    DOP:=ASource as TDDCreateDBFOptions;
    FCreateInstance:=DOP.FCreateInstance;
    Fident:=DOP.FIdent;
    FProcName:=DOP.FProcName;
    FTableName:=DOP.FTableName;
    end;
  inherited Assign(ASource);
end;

{ TDDCreateDBFGenerator }

function TDDCreateDBFGenerator.ProcedureDecl: String;
begin
  If not DBFOptions.CreateInstance then
    Result:=Format('%s (%s : TDBF)',[DBFoptions.ProcedureName,DBFOptions.Identifier])
  else
    Result:=DBFoptions.ProcedureName;
  Result:=Format('procedure %s;',[Result]);
end;

function TDDCreateDBFGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TDDCreateDBFOptions.Create;
end;

procedure TDDCreateDBFGenerator.DoGenerateImplementation(Strings: TStrings);

Var
  i : integer;
  F : TFieldPropDef;
  S : String;
  N : String;
  
begin
  N:=DBFOptions.Identifier;
  If (DBFoptions.ProcedureName<>'') then
    begin
    BeginMethod(Strings,ProcedureDecl);
    If DBFOptions.CreateInstance then
      begin
      Addln(Strings);
      Addln(Strings,'Var');
      IncIndent;
      Try
        Addln(Strings,'%s : TDBF;',[N]);
      Finally
        DecIndent;
      end;
      end;
    AddLn(Strings,'begin');
    IncIndent;
    end;
  Try
    If DBFOptions.CreateInstance then
      Addln(Strings,'%s:=TDBF.Create(Nil);',[N]);
    Addln(Strings,'With %s do',[N]);
    IncIndent;
    try
      AddLn(Strings,'begin');
      If Not DBFOptions.CreateInstance then
        AddLn(Strings,'Close;');
      AddLn(Strings,'With FieldDefs do');
      IncIndent;
      try
        AddLn(Strings,'begin');
        For I:=0 to Fields.Count-1 do
          begin
          F:=Fields[i];
          If F.Enabled then
            begin
            S:=GetEnumName(TypeInfo(TFieldType),Ord(F.FieldType));
            AddLn(Strings,'Add(''%s'',%s,%d);',[F.FieldName,S,F.PropertySize]);
            end;
          end;
        AddLn(Strings,'end;');
      Finally
        DecIndent;
      end;
      AddLn(Strings,'TableName:=%s;',[CreateString(DBFOptions.TableName)]);
      AddLn(Strings,'CreateTable;');
      AddLn(Strings,'Exclusive:=true;');
      AddLn(Strings,'Open;');
      AddLn(Strings,'end;');
    finally
      DecIndent;
    end;
  Finally
    If (DBFoptions.ProcedureName<>'') then
      begin
      DecIndent;
      EndMethod(Strings,DBFoptions.ProcedureName);
      end;
  end;
end;

procedure TDDCreateDBFGenerator.DoGenerateInterface(Strings: TStrings);
begin
  If (DBFoptions.ProcedureName<>'') then
    BeginMethod(Strings,ProcedureDecl);
end;

function TDDCreateDBFGenerator.GetFieldDefs: TFieldPropDefs;
begin
  Result:=FFields;
end;

procedure TDDCreateDBFGenerator.SetFieldDefs(const AValue: TFieldPropDefs);
begin
  FFields.Assign(AValue);
end;


function TDDCreateDBFGenerator.DBFOptions: TDDCreateDBFOptions;
begin
  Result:=TDDCreateDBFOptions(CodeOptions);
end;

function TDDCreateDBFGenerator.GetImplementationUsesClause: String;
begin
  If DBFOptions.CreateInstance then
    Result:='db, dbf';
end;

function TDDCreateDBFGenerator.GetInterfaceUsesClause: string;
begin
  If Not DBFOptions.CreateInstance then
    Result:='db, dbf';
end;

constructor TDDCreateDBFGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields:=TFieldPropDefs.Create(TFieldPropDef);
end;

destructor TDDCreateDBFGenerator.Destroy;
begin
  FreeAndNil(FFields);
  inherited Destroy;
end;

class function TDDCreateDBFGenerator.NeedsFieldDefs: Boolean;
begin
  Result:=True;
end;

initialization
  RegisterCodeGenerator('DBFCreate','Create DBF file for data',TDDCreateDBFGenerator);
Finalization
  UnRegisterCodeGenerator(TDDCreateDBFGenerator);
end.

