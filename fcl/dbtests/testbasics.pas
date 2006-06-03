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
  end;

implementation

uses db, toolsunit;


{ TTestBasics }

procedure TTestBasics.TestParseSQL;
var Params : TParams;
begin
  Params := TParams.Create;
  AssertEquals(     'select * from table where id = $1',
    params.ParseSQL('select * from table where id = :id',true,psPostgreSQL));

  AssertEquals(     'select * from table where id = $1',
    params.ParseSQL('select * from table where id = :id',false,psPostgreSQL));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $2)',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :2)',true,psPostgreSQL));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :par3) and (test=''$test'')',true,psPostgreSQL));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 10=$10 11=$11 12=$5 where (id = $3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id = :par3) and (test=''$test'')',true,psPostgreSQL));


  AssertEquals(     'select * from table where id = $1',
    params.ParseSQL('select * from table where id = :id',true,psSimulated));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $2)',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :2)',true,psSimulated));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 where (id = $3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 where (id = :par3) and (test=''$test'')',true,psSimulated));

  AssertEquals(     'update test set 1=$1 2=$2 3=$3 4=$4 5=$5 6=$6 7=$7 8=$8 9=$9 10=$10 11=$11 12=$5 where (id = $3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id = :par3) and (test=''$test'')',true,psSimulated));

  AssertEquals(     'update test set 1=$$1 2=$$2 3=$$3 4=$$4 5=$$5 6=$$6 7=$$7 8=$$8 9=$$9 10=$$10 11=$$11 12=$$5 where (id$ = $$3) and (test=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id$ = :par3) and (test=''$test'')',true,psSimulated));

  AssertEquals(     'update test set 1=$$$1 2=$$$2 3=$$$3 4=$$$4 5=$$$5 6=$$$6 7=$$$7 8=$$$8 9=$$$9 10=$$$10 11=$$$11 12=$$$5 where (id$$ = $$$3) and (test$=''$test'')',
    params.ParseSQL('update test set 1=:1 2=:2 3=:par3 4=:par4 5=:par5 6=:par6 7=:par7 8=:par8 9=:par9 10=:par10 11=:11 12=:par5 where (id$$ = :par3) and (test$=''$test'')',true,psSimulated));

  AssertEquals(     'select * from table where id = ?',
    params.ParseSQL('select * from table where id = :id',true,psInterbase));


  Params.Free;
end;

initialization
  RegisterTest(TTestBasics);
end.
