{ %NORUN }

program tb0670;

const
  Value1 = $06;
  Value2 = $60;
  Value3 = $6000;
  Value4 = $60000000;
  Value5 = $60000000000;

  Value6 = $40;
  Value7 = $4000;
  Value8 = $40000000;
  Value9 = $40000000000;

  ValueNot1 = not Value1;
  ValueNot2 = not Value2;
  ValueNot3 = not Value3;
  ValueNot4 = not Value4;
  ValueNot5 = not Value5;

  ValueOr1 = Value1 or Value2;
  ValueOr2 = Value1 or Value3;
  ValueOr3 = Value1 or Value4;
  ValueOr4 = Value1 or Value5;

  ValueAnd1 = Value2 and Value6;
  ValueAnd2 = Value3 and Value7;
  ValueAnd3 = Value4 and Value8;
  ValueAnd4 = Value5 and Value9;

{ Test "not X" }

{$if not (not Value1 = ValueNot1)}
{$error 'not Value1 = ValueNot1'}
{$endif}

{$if not (not Value2 = ValueNot2)}
{$error 'not Value2 = ValueNot2'}
{$endif}

{$if not (not Value3 = ValueNot3)}
{$error 'not Value3 = ValueNot3'}
{$endif}

{$if not (not Value4 = ValueNot4)}
{$error 'not Value4 = ValueNot4'}
{$endif}

{$if not (not Value5 = ValueNot5)}
{$error 'not Value5 = ValueNot5'}
{$endif}

{ Test "X or Y" }

{$if Value1 or Value2 <> ValueOr1}
{$error 'Value1 or Value2 = ValueOr1'}
{$endif}

{$if Value1 or Value3 <> ValueOr2}
{$error 'Value1 or Value3 = ValueOr2'}
{$endif}

{$if Value1 or Value4 <> ValueOr3}
{$error 'Value1 or Value4 = ValueOr3'}
{$endif}

{$if Value1 or Value5 <> ValueOr4}
{$error 'Value1 or Value5 = ValueOr4'}
{$endif}

{ Test "X and Y" }

{$if Value2 and Value6 <> ValueAnd1 }
{$error 'Value2 and Value6 = ValueAnd1' }
{$endif}

{$if Value3 and Value7 <> ValueAnd2 }
{$error 'Value3 and Value7 = ValueAnd2' }
{$endif}

{$if Value4 and Value8 <> ValueAnd3 }
{$error 'Value4 and Value8 = ValueAnd3' }
{$endif}

{$if Value5 and Value9 <> ValueAnd4 }
{$error 'Value5 and Value9 = ValueAnd4' }
{$endif}

begin
end.
