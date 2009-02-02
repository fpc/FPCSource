unit TestBasics;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  fpcunit, testutils, testregistry, testdecorator,
  Classes, SysUtils;

type

  { TTestBasics }

  TTestBasics = class(TTestCase)
  private
  protected
  published
    procedure TestParseSQL;
    procedure TestInitFielddefsFromFields;
    procedure TestDoubleFieldDef;
    procedure TestFieldDefWithoutDS;
  end;

implementation

uses db, toolsunit;

Type HackedDataset = class(TDataset);

{ TTestBasics }

procedure TTestBasics.TestParseSQL;
var Params  : TParams;
    ReplStr : string;
    pb      : TParamBinding;
begin
  Params := TParams.Create;
  AssertEquals(     'select * from table where id = $1',
    params.ParseSQL('select * from table where id = :id',true,True,True,psPostgreSQL));

  AssertEquals(     'select * from table where id = $1',
    params.ParseSQL('select * from table where id = :id',false,True,True,psPostgreSQL));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $2)',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :2)',true,True,True,psPostgreSQL));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :par3) and (test=''$test'')',true,true,true,psPostgreSQL));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 10=$10 11=$11 12=$5 where (id = $3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id = :par3) and (test=''$test'')',true,true,true,psPostgreSQL));

  AssertEquals(     'select * from table where id = $1',
    params.ParseSQL('select * from table where id = :id',true,true,false,psSimulated,pb,ReplStr));
  AssertEquals('$',ReplStr);

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $2)',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :2)',true,true,false,psSimulated,pb,ReplStr));
  AssertEquals('$',ReplStr);

  AssertEquals(     'update test set 1=$$1 2=$$2 3=$$3 4=$$4 5=$$5 6=$$6 7=$$7 8=$$8 9=$$9 where (id = $$3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :par3) and (test=''$test'')',true,true,false,psSimulated,pb,ReplStr));
  AssertEquals('$$',ReplStr);

  AssertEquals(     'update test set 1=$$1 2=$$2 3=$$3 4=$$4 5=$$5 6=$$6 7=$$7 8=$$8 9=$$9 10=$$10 11=$$11 12=$$5 where (id = $$3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id = :par3) and (test=''$test'')',true,True,True,psSimulated));
  AssertEquals('$$',ReplStr);

  AssertEquals(     'update test set 1=$$$1 2=$$$2 3=$$$3 4=$$$4 5=$$$5 6=$$$6 7=$$$7 8=$$$8 9=$$$9 10=$$$10 11=$$$11 12=$$$5 where (id$$ = $$$3) and (test$=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id$$ = :par3) and (test$=''$test'')',true,true,False,psSimulated,pb,ReplStr));
  AssertEquals('$$$',ReplStr);

  AssertEquals(     'select * from table where id = ?',
    params.ParseSQL('select * from table where id = :id',true,true,true,psInterbase));

// Test bug 10345
  AssertEquals(     'select email from table where upper(email) like ''%''||?||''%''',
    params.ParseSQL('select email from table where upper(email) like ''%''||:email||''%''',true,true,true,psInterbase));

// Test escape-sequences:
  AssertEquals(     'select * from table where ''id '''' = :id''',
    params.ParseSQL('select * from table where ''id '''' = :id''',true,False,True,psPostgreSQL));

  AssertEquals(     'select * from table where "id "" = :id"',
    params.ParseSQL('select * from table where "id "" = :id"',true,False,True,psPostgreSQL));

  AssertEquals(     'select * from table where "id \" = :id"',
    params.ParseSQL('select * from table where "id \" = :id"',true,True,False,psPostgreSQL));

  AssertEquals(     'select * from table where "id \" = $1',
    params.ParseSQL('select * from table where "id \" = :id',true,False,False,psPostgreSQL));

  AssertEquals(     'select * from table where "id  = :id\',
    params.ParseSQL('select * from table where "id  = :id\',true,True,True,psInterbase));

  Params.Free;
end;

procedure TTestBasics.TestInitFielddefsFromFields;

var ds       : TDataset;
    F1,F2,F3 : Tfield;
    
  Procedure CompareFieldAndFieldDef(Fld: TField; FldDef : TFieldDef);
  
  begin
    AssertEquals(Fld.FieldName,FldDef.Name);
    AssertEquals(Fld.Size,FldDef.Size);
    AssertEquals(Fld.Required,FldDef.Required);
    AssertTrue(Fld.DataType=FldDef.DataType);
  end;
    
begin
  ds := TDataset.Create(nil);

  F1:=TStringField.Create(ds);
  F1.Size := 10;
  F1.Name := 'StringFld';
  F1.FieldName := 'FStringFld';
  F1.Required := false;
  F1.Dataset:=ds;

  F2:=TIntegerField.Create(ds);
  F2.Name := 'IntegerFld';
  F2.FieldName := 'FIntegerFld';
  F2.Required := True;
  F2.Dataset:=ds;

  F3:=TBCDField.Create(ds);
  F3.Name := 'BCDFld';
  F3.FieldName := 'FBCDFld';
  F3.Required := false;
  F3.Dataset:=ds;
  (f3 as TBCDField).Precision := 2;
  
  HackedDataset(ds).InitFieldDefsFromfields;
  
  AssertEquals(3,ds.FieldDefs.Count);
  
  CompareFieldAndFieldDef(F1,ds.FieldDefs[0]);
  CompareFieldAndFieldDef(F2,ds.FieldDefs[1]);
  CompareFieldAndFieldDef(F3,ds.FieldDefs[2]);
end;

procedure TTestBasics.TestDoubleFieldDef;
var ds : TDataset;
    PassException : boolean;
begin
  // If a second field with the same name is added to a TFieldDefs, an exception
  // should occur
  ds := TDataset.create(nil);
  ds.FieldDefs.Add('Field1',ftInteger);
  PassException:=False;
  try
    ds.FieldDefs.Add('Field1',ftString,10,false)
  except
    on E: EDatabaseError do PassException := True;
  end;
  AssertTrue(PassException);
end;

procedure TTestBasics.TestFieldDefWithoutDS;
var FieldDefs : TFieldDefs;
begin
  FieldDefs := TFieldDefs.Create(nil);
  FieldDefs.Add('test',ftString);
  FieldDefs.Free;
end;

initialization
  RegisterTest(TTestBasics);
end.
