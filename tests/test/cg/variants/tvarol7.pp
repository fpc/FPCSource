{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp7 = double;
{$else FPC_COMP_IS_INT64}
  comp7 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test7(var a); overload;
  begin
    writeln('formal called instead of smallint');
    writeln('XXX')
  end;

procedure test7(a: smallint); overload;
  begin
    writeln('smallint called instead of formal');
    writeln('YYY')
  end;

var
  x7: longint;

  y7: smallint;
procedure dotest7;
var
  v: variant;

begin
  try
    v := x7;
    test7(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y7;
    test7(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest7;
end. {$endif not bigfile}
