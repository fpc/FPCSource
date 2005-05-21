unit moneytest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, money, testregistry;

type

  TMoneyTest = class(TTestCase)
  private
    F12CHF: IMoney;
    F14CHF: IMoney;
    F7USD: IMoney;
    F21USD: IMoney;
    FMB1: IMoney;
    FMB2: IMoney;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testBagCreate;
    procedure testBagMultiply;
    procedure testBagNegate;
    procedure testBagSimpleAdd;
    procedure testBagSubtract;
    procedure testBagSumAdd;
    procedure testIsZero;
    procedure testMixedSimpleAdd;
    procedure testBagNotEquals;
    procedure testMoneyBagEquals;
    procedure testMoneyEquals;
    procedure testSimplify;
    procedure testNormalize2;
    procedure testNormalize3;
    procedure testNormalize4;
    procedure testPrint;
    procedure testMoneyBagPrint;
    procedure testSimpleAdd;
    procedure testSimpleBagAdd;
    procedure testSimpleMultiply;
    procedure testSimpleNegate;
    procedure testSimpleSubtract;
    procedure testOperators;
  end;



implementation

{ TMoneyTest }

procedure TMoneyTest.SetUp;
begin
  F12CHF := TMoney.Create(12, 'CHF');
  F14CHF := TMoney.Create(14, 'CHF');
  F7USD := TMoney.Create(7, 'USD');
  F21USD := TMoney.Create(21, 'USD');
  FMB1 := TMoneyBag.CreateWith(F12CHF, F7USD);
  FMB2 := TMoneyBag.CreateWith(F14CHF, F21USD);
end;

procedure TMoneyTest.TearDown;
begin

end;

procedure TMoneyTest.testBagCreate;
begin
  AssertEquals('Wrong number of moneys in bag', 2, FMB1.Count);
end;

procedure TMoneyTest.testBagMultiply;
var
  expected: IMoney;
begin
  expected := TMoneyBag.CreateWith(TMoney.Create(24, 'CHF'), TMoney.Create(14, 'USD'));
  AssertTrue(expected.equals(FMB1.multiply(2)));
  AssertTrue(FMB1.equals(FMB1.multiply(1)));
  AssertTrue('multiplication by zero failed', FMB1.multiply(0).isZero);
end;

procedure TMoneyTest.testBagNegate;
var
  expected: IMoney;
begin
  expected := TMoneyBag.CreateWith(TMoney.Create(-12, 'CHF'), TMoney.Create(-7, 'USD'));
  AssertTrue('expected '+ expected.toString + ' but was ' + FMB1.negate.toString, expected.equals(FMB1.negate));
end;

procedure TMoneyTest.testBagSimpleAdd;
var
  expected: IMoney;
  a, b: IMoney;
begin
  a := TMoney.Create(26, 'CHF');
  b := TMoney.Create(7, 'USD');
  expected := TMoneyBag.CreateWith(a, b);
  AssertTrue('expected ' + expected.toString + ' but was ' + FMB1.add(F14CHF).toString, expected.equals(FMB1.add(F14CHF)));
end;

procedure TMoneyTest.testBagSubtract;
var
  expected: IMoney;
begin
  expected := TMoneyBag.CreateWith(TMoney.Create(-2, 'CHF'), TMoney.Create(-14, 'USD'));
  AssertTrue('expected ' + expected.toString + ' but was ' + FMB1.subtract(FMB2).toString, expected.equals(FMB1.Subtract(FMB2)));
end;

procedure TMoneyTest.testBagSumAdd;
var
  expected: IMoney;
begin
  expected := TMoneyBag.CreateWith(TMoney.Create(26, 'CHF'), TMoney.Create(28, 'USD'));
  AssertTrue('expected ' + expected.toString + ' but was ' + FMB1.add(FMB2).toString, expected.equals(FMB1.add(FMB2)));

end;

procedure TMoneyTest.testIsZero;
var
  F0CHF, F12USD, F0USD, FMB0: IMoney;
begin
  F0CHF := TMoney.Create(0, 'CHF');
  F0USD := TMoney.Create(0, 'USD');
  F12USD := TMoney.Create(12, 'USD');
  AssertTrue('error: [0 CHF] is to be considered zero!', F0CHF.IsZero);
  AssertFalse('error: [12 USD] is not to be considered zero!', F12USD.IsZero);
  AssertTrue(FMB1.subtract(FMB1).isZero);
  FMB0 :=TMoneyBag.CreateWith(F0CHF, F0USD);
  AssertTrue(FMB0.isZero);
end;

procedure TMoneyTest.testMixedSimpleAdd;
var
  expected: IMoney;
begin
  expected := TMoneyBag.CreateWith(F12CHF, F7USD);
  AssertTrue('expected ' + expected.toString + ' but was ' + F12CHF.add(F7USD).toString, expected.equals(F12CHF.add(F7USD)));
end;

procedure TMoneyTest.testBagNotEquals;
var
  expected: IMoney;
  res: IMoney;
begin
  expected := TMoneyBag.CreateWith(F12CHF, F7USD);
  res := TMoney.Create(12, 'CAD').add(F7USD);
  AssertFalse(expected.equals(res));
end;

procedure TMoneyTest.testMoneyBagEquals;
var
  equal: IMoney;
begin
  AssertTrue(not FMB1.equals(nil));
  AssertTrue(FMB1.equals(FMB1));
  equal := TMoneyBag.CreateWith(TMoney.Create(12, 'CHF'), TMoney.Create(7, 'USD'));
  AssertTrue(FMB1.equals(equal));
  AssertTrue(not FMB1.equals(F12CHF));
  AssertTrue(not F12CHF.equals(FMB1));
  AssertTrue(not FMB1.equals(FMB2));
end;

procedure TMoneyTest.testMoneyEquals;
var
  equalMoney: IMoney;
begin
  AssertTrue('error: [12 CHF] does not equal nil', not F12CHF.equals(nil));
  equalMoney := TMoney.Create(12, 'CHF');
  AssertTrue(F12CHF.equals(F12CHF));
  AssertTrue(F12CHF.equals(equalMoney));
  AssertFalse(F12CHF.equals(F14CHF));
end;

procedure TMoneyTest.testSimplify;
var
  money: IMoney;
  F26CHF, F28CHF, F54CHF: IMoney;
begin
  F26CHF := TMoney.Create(26, 'CHF');
  F28CHF := TMoney.Create(28, 'CHF');
  money := TMoneyBag.CreateWith(F26CHF, F28CHF);
  F54CHF := TMoney.Create(54, 'CHF');
  AssertTrue('Expected ' + F54CHF.toString + ' but was '
    + money.toString, F54CHF.equals(money));
end;

procedure TMoneyTest.testNormalize2;
var
  expected: IMoney;
begin
  // {[12 CHF][7 USD]} - [12 CHF] = [7 USD]
  expected := TMoney.Create(7, 'USD');
  AssertTrue('Expected ' + expected.toString + ' but was '
    + FMB1.subtract(F12CHF).toString, expected.equals(FMB1.subtract(F12CHF)));
end;

procedure TMoneyTest.testNormalize3;
var
  ms1: IMoney;
  expected: IMoney;
begin
  // {[12 CHF][7 USD]} - {[12 CHF][3 USD]} = [4 USD]
  ms1 := TMoneyBag.CreateWith(TMoney.Create(12, 'CHF'), TMoney.Create(3, 'USD'));
  expected := TMoney.Create(4, 'USD');
  AssertTrue('Expected ' + expected.toString + ' but was ' + FMB1.subtract(ms1).toString,
    expected.equals(FMB1.subtract(ms1)));
end;

procedure TMoneyTest.testNormalize4;
var
  ms1: IMoney;
  expected: IMoney;
begin
  // [12 CHF] - {[12 CHF][3 USD]} = [-3 USD]
  ms1 := TMoneyBag.CreateWith(TMoney.Create(12, 'CHF'), TMoney.Create(3, 'USD'));
  expected := TMoney.Create(-3, 'USD');
  AssertTrue('Expected ' + expected.toString + ' but was ' + F12CHF.subtract(ms1).toString,
    expected.equals(F12CHF.subtract(ms1)));
end;

procedure TMoneyTest.testPrint;
begin
  AssertEquals('[12 CHF]', F12CHF.ToString);
end;

procedure TMoneyTest.testMoneyBagPrint;
begin
  AssertEquals('{[12 CHF][7 USD]}', FMB1.toString);
end;

procedure TMoneyTest.testSimpleAdd;
var
  expected: IMoney;
  res: IMoney;
begin
  expected := TMoney.Create(26, 'CHF');
  res := F12CHF.add(F14CHF);
  AssertTrue('addition error: [12 CHF] + [14 CHF] was not [26 CHF]', res.equals(expected));
end;

procedure TMoneyTest.testSimpleBagAdd;
var
  expected: IMoney;
begin
  expected := TMoneyBag.CreateWith(TMoney.Create(26, 'CHF'), TMoney.Create(7, 'USD'));
  AssertTrue('expected ' + expected.toString + ' but was ' + F14CHF.add(FMB1).toString, expected.equals(F14CHF.add(FMB1)));
end;

procedure TMoneyTest.testSimpleMultiply;
var
  expected: IMoney;
begin
  expected := TMoney.Create(28, 'CHF');
  AssertTrue('Multiply Error: [14 CHF] * 2 was not equal to [28 CHF]',
    expected.equals(F14CHF.Multiply(2)));
end;

procedure TMoneyTest.testSimpleNegate;
var
  expected: IMoney;
begin
  expected := TMoney.Create(-14, 'CHF');
  AssertTrue('Negate Error: [14 CHF] negate was not equal to [-14 CHF]',
    expected.equals(F14CHF.negate));
end;

procedure TMoneyTest.testSimpleSubtract;
var
  expected: IMoney;
begin
  expected := TMoney.Create(2, 'CHF');
  AssertTrue('Negate Error: [14 CHF] - [12 CHF] was not equal to [2 CHF]',
    expected.equals(F14CHF.subtract(F12CHF)));
end;

procedure TMoneyTest.testOperators;
var
  mb: IMoney;
  ma: IMoney;
begin
  ma := TMoney.Create(2, 'CHF');
  AssertTrue(F14CHF.equals(F12CHF + ma ));
  AssertTrue('expected ' + FMB1.toString +' but was ' +
    (FMB2 - TMoneyBag.CreateWith(TMoney.Create(2, 'CHF'), TMoney.Create(14, 'USD'))).toString,
    FMB1.equals(FMB2 - TMoneyBag.CreateWith(TMoney.Create(2, 'CHF'), TMoney.Create(14, 'USD'))));
  mb := TMoneyBag.CreateWith(TMoney.Create(28, 'CHF'), TMoney.Create(42, 'USD'));
  AssertTrue('expected ' + mb.toString + ' but was ' + (FMB2 *2).toString, (FMB2 * 2).equals(mb));
end;

initialization

  RegisterTests([TMoneyTest]);

end.
