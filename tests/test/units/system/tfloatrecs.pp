uses
  Math;

procedure do_error(i : longint);
  begin
    writeln('Error near ',i);
    halt(1);
  end;

var
{$ifdef FPC_HAS_TYPE_EXTENDED}
  extended_NaN,extended_Inf,extended_NInf,extended_NDenormal,extended_Denormal,extended_Zero,extended_NZero,
  extended_Positive,extended_Negative,extended_InvalidOp : extended;
{$endif FPC_HAS_TYPE_EXTENDED}
{$ifdef FPC_HAS_TYPE_DOUBLE}
  double_NaN,double_Inf,double_NInf,double_NDenormal,double_Denormal,double_Zero,double_NZero,
  double_Positive,double_Negative : double;
{$endif FPC_HAS_TYPE_DOUBLE}
{$ifdef FPC_HAS_TYPE_SINGLE}
  single_NaN,single_Inf,single_NInf,single_NDenormal,single_Denormal,single_Zero,single_NZero,
  single_Positive,single_Negative : single;
{$endif FPC_HAS_TYPE_SINGLE}

begin
{$ifdef FPC_HAS_TYPE_EXTENDED}
  extended_NaN:=NaN;

  extended_Inf:=Infinity;

  extended_NInf:=-Infinity;

  extended_Denormal:=1234.0;
  TExtended80Rec(extended_Denormal).Exp:=0;

  extended_NDenormal:=-1234.0;
  TExtended80Rec(extended_NDenormal).Exp:=0;

  extended_Zero:=0.0;

  extended_NZero:=0.0;
  TExtended80Rec(extended_NZero).Sign:=true;

  extended_Positive:=Pi*10;

  extended_Negative:=-Pi*10;

  extended_InvalidOp:=0;
  TExtended80Rec(extended_InvalidOp).Exp:=$7fff;

  if TExtended80Rec(extended_NaN).SpecialType<>fsNaN then
    do_error(1);

  if TExtended80Rec(extended_Inf).SpecialType<>fsInf then
    do_error(2);

  if TExtended80Rec(extended_NInf).SpecialType<>fsNInf then
    do_error(3);

  if TExtended80Rec(extended_Denormal).SpecialType<>fsDenormal then
    do_error(4);

  if TExtended80Rec(extended_NDenormal).SpecialType<>fsNDenormal then
    do_error(5);

  if TExtended80Rec(extended_Zero).SpecialType<>fsZero then
    do_error(6);

  if TExtended80Rec(extended_NZero).SpecialType<>fsNZero then
    do_error(7);

  if TExtended80Rec(extended_Positive).SpecialType<>fsPositive then
    do_error(8);

  if TExtended80Rec(extended_Negative).SpecialType<>fsNegative then
    do_error(9);

  if TExtended80Rec(extended_InvalidOp).SpecialType<>fsInvalidOp then
    do_error(10);

  if TExtended80Rec(extended_Positive).Mantissa<>$7B53D14AA9C2F2C2 then
    do_error(11);

  if TExtended80Rec(extended_Positive).Fraction<>4.15926535897932384694E-0001 then
    do_error(12);

  if TExtended80Rec(extended_Positive).Exponent<>4 then
    do_error(13);

  if TExtended80Rec(extended_Positive).Sign then
    do_error(14);

  if TExtended80Rec(extended_Positive).Exp<>$4003 then
    do_error(15);

  if TExtended80Rec(extended_Negative).Mantissa<>$7B53D14AA9C2F2C2 then
    do_error(16);

  if TExtended80Rec(extended_Negative).Fraction<>-4.15926535897932384694E-0001 then
    do_error(17);

  if TExtended80Rec(extended_Negative).Exponent<>4 then
    do_error(18);

  if not(TExtended80Rec(extended_Negative).Sign) then
    do_error(19);

  if TExtended80Rec(extended_Negative).Exp<>$4003 then
    do_error(20);
{$endif FPC_HAS_TYPE_EXTENDED}

{$ifdef FPC_HAS_TYPE_DOUBLE}
  double_NaN:=NaN;

  double_Inf:=Infinity;

  double_NInf:=-Infinity;

  double_Denormal:=1234.0;
  TDoubleRec(double_Denormal).Exp:=0;

  double_NDenormal:=-1234.0;
  TDoubleRec(double_NDenormal).Exp:=0;

  double_Zero:=0.0;

  double_NZero:=0.0;
  TDoubleRec(double_NZero).Sign:=true;

  double_Positive:=Pi*10;

  double_Negative:=-Pi*10;

  if TDoubleRec(double_NaN).SpecialType<>fsNaN then
    do_error(101);

  if TDoubleRec(double_Inf).SpecialType<>fsInf then
    do_error(102);

  if TDoubleRec(double_NInf).SpecialType<>fsNInf then
    do_error(103);

  if TDoubleRec(double_Denormal).SpecialType<>fsDenormal then
    do_error(104);

  if TDoubleRec(double_NDenormal).SpecialType<>fsNDenormal then
    do_error(105);

  if TDoubleRec(double_Zero).SpecialType<>fsZero then
    do_error(106);

  if TDoubleRec(double_NZero).SpecialType<>fsNZero then
    do_error(107);

  if TDoubleRec(double_Positive).SpecialType<>fsPositive then
    do_error(108);

  if TDoubleRec(double_Negative).SpecialType<>fsNegative then
    do_error(109);

  if TDoubleRec(double_Positive).Mantissa<>$000F6A7A2955385E then
    do_error(111);

  if TDoubleRec(double_Positive).Fraction<>4.15926535897931159980E-0001 then
    do_error(112);

  if TDoubleRec(double_Positive).Exponent<>4 then
    do_error(113);

  if TDoubleRec(double_Positive).Sign then
    do_error(114);

  if TDoubleRec(double_Positive).Exp<>$403 then
    do_error(115);

  if TDoubleRec(double_Negative).Mantissa<>$000F6A7A2955385E then
    do_error(116);

  if TDoubleRec(double_Negative).Fraction<>-4.15926535897931159980E-0001 then
    do_error(117);

  if TDoubleRec(double_Negative).Exponent<>4 then
    do_error(118);

  if not(TDoubleRec(double_Negative).Sign) then
    do_error(119);

  if TDoubleRec(double_Negative).Exp<>$403 then
    do_error(120);
{$endif FPC_HAS_TYPE_DOUBLE}

{$ifdef FPC_HAS_TYPE_DOUBLE}
  single_NaN:=NaN;

  single_Inf:=Infinity;

  single_NInf:=-Infinity;

  single_Denormal:=1234.0;
  TSingleRec(single_Denormal).Exp:=0;

  single_NDenormal:=-1234.0;
  TSingleRec(single_NDenormal).Exp:=0;

  single_Zero:=0.0;

  single_NZero:=0.0;
  TSingleRec(single_NZero).Sign:=true;

  single_Positive:=Pi*10;

  single_Negative:=-Pi*10;

  if TSingleRec(single_NaN).SpecialType<>fsNaN then
    do_error(201);

  if TSingleRec(single_Inf).SpecialType<>fsInf then
    do_error(202);

  if TSingleRec(single_NInf).SpecialType<>fsNInf then
    do_error(203);

  if TSingleRec(single_Denormal).SpecialType<>fsDenormal then
    do_error(204);

  if TSingleRec(single_NDenormal).SpecialType<>fsNDenormal then
    do_error(205);

  if TSingleRec(single_Zero).SpecialType<>fsZero then
    do_error(206);

  if TSingleRec(single_NZero).SpecialType<>fsNZero then
    do_error(207);

  if TSingleRec(single_Positive).SpecialType<>fsPositive then
    do_error(208);

  if TSingleRec(single_Negative).SpecialType<>fsNegative then
    do_error(209);

  if TSingleRec(single_Positive).Mantissa<>$7b53d1 then
    do_error(211);

  if TSingleRec(single_Positive).Fraction<>4.15925979614257812500E-0001 then
    do_error(212);

  if TSingleRec(single_Positive).Exponent<>4 then
    do_error(213);

  if TSingleRec(single_Positive).Sign then
    do_error(214);

  if TSingleRec(single_Positive).Exp<>$83 then
    do_error(215);

  if TSingleRec(single_Negative).Mantissa<>$7b53d1 then
    do_error(216);

  if TSingleRec(single_Negative).Fraction<>-4.15925979614257812500E-0001 then
    do_error(217);

  if TSingleRec(single_Negative).Exponent<>4 then
    do_error(218);

  if not(TSingleRec(single_Negative).Sign) then
    do_error(219);

  if TSingleRec(single_Negative).Exp<>$83 then
    do_error(220);
{$endif FPC_HAS_TYPE_DOUBLE}

  writeln('ok');
end.

