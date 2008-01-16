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
unit fpcgsqlconst;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpddCodeGen;

Type

  { TDDSQLConstOptions }

  TMode = (mConst,mTStrings);
  TDDSQLConstOptions = Class(TCodeGeneratorOptions)
  private
    FIDent: String;
    FMode: TMode;
    procedure SetIdent(const AValue: String);
  Public
    Constructor Create; override;
    Procedure Assign(ASource : TPersistent); override;
  Published
    Property Identifier : String Read FIDent Write SetIdent;
    Property Mode : TMode Read FMode Write FMode;
  end;
  
  
  { TDDSQLConstGenerator }

  TDDSQLConstGenerator = Class(TDDCustomCodeGenerator)
  Private
    FSQL : TStrings;
  Protected
    Function CreateOptions : TCodeGeneratorOptions; override;
    Procedure DoGenerateInterface(Strings: TStrings); override;
    Procedure DoGenerateImplementation(Strings: TStrings); override;
    function GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    Function SQLOptions : TDDSQLConstOptions;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function NeedsSQL : Boolean; override;
    Class Function NeedsFieldDefs : Boolean; override;
  end;

Const
  SSQLConst = 'SQLConst';
  
Resourcestring
  SSQLConstDescr = 'Generate Pascal constant/Stringlist from SQL';

implementation

{ TDDSQLConstOptions }

procedure TDDSQLConstOptions.SetIdent(const AValue: String);
begin
  if FIDent=AValue then exit;
  If Not IsValidIdent(AValue) then
    Raise ECodeGenerator.CreateFmt(SErrInvalidIdentifier,[AValue]);
  FIDent:=AValue;
end;

constructor TDDSQLConstOptions.Create;
begin
  Inherited;
  FIdent:='SQL'; // Do not localize
end;

procedure TDDSQLConstOptions.Assign(ASource: TPersistent);

Var
  CO: TDDSQLConstOptions;
  
begin
  If ASource is TDDSQLConstOptions then
    begin
    CO:=ASource as TDDSQLConstOptions;
    FIDent:=CO.FIdent;
    FMode:=CO.FMode;
    end;
  inherited Assign(ASource);
end;

{ TDDSQLConstGenerator }

function TDDSQLConstGenerator.CreateOptions: TCodeGeneratorOptions;
begin
  Result:=TDDSQLConstOptions.Create;
end;

procedure TDDSQLConstGenerator.DoGenerateInterface(Strings: TStrings);

Var
  S : String;
  I,L : Integer;
  
begin
  If (SQLOptions.Mode=mConst) then
    begin
    Addln(Strings,'Const');
    L:=Length(SQLOPtions.Identifier);
    IncIndent;
    try
      For I:=0 to FSQL.Count-1 do
        begin
        If (I=0) then
          S:=SQLOPtions.Identifier+' = '
        else
          S:=StringOfChar(' ',L)+'   +';
        S:=S+CreateString(FSQL[i]);
        If (I=FSQL.Count-1) then
          S:=S+';'
        else
          S:=S+'+sLineBreak';
        Addln(Strings,S);
        end;
    finally
      DecIndent;
    end;
    end;
end;

procedure TDDSQLConstGenerator.DoGenerateImplementation(Strings: TStrings);

Var
  S : String;
  I,L : Integer;

begin
  If (SQLOptions.Mode=mTStrings) then
    begin
    Addln(Strings,'With %s do',[SQLOPtions.Identifier]);
    IncIndent;
    try
      Addln(Strings,'begin');
      For I:=0 to FSQL.Count-1 do
        Addln(Strings,'Add(%s);',[CreateString(FSQL[i])]);
      Addln(Strings,'end;');
    finally
      DecIndent;
    end;
    end;
end;

function TDDSQLConstGenerator.GetSQL: TStrings;
begin
  Result:=FSQL;
end;

procedure TDDSQLConstGenerator.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

function TDDSQLConstGenerator.SQLOptions: TDDSQLConstOptions;
begin
  Result:=CodeOptions as TDDSQLConstOptions;
end;

constructor TDDSQLConstGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL:=TSTringList.Create;
end;

destructor TDDSQLConstGenerator.Destroy;
begin
  FreeAndNil(FSQL);
  inherited Destroy;
end;

class function TDDSQLConstGenerator.NeedsSQL: Boolean;
begin
  Result:=True;
end;

class function TDDSQLConstGenerator.NeedsFieldDefs: Boolean;
begin
  Result:=False;
end;


Initialization
  RegisterCodeGenerator(SSQLConst, SSQLConstDescr, TDDSQLConstGenerator);
  
Finalization
  UnRegisterCodeGenerator(TDDSQLConstGenerator);

end.

