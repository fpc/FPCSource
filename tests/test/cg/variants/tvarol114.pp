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
  comp114 = double;
{$else FPC_COMP_IS_INT64}
  comp114 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test114(a: smallint); overload;
  begin
    writeln('smallint called instead of boolean');
    writeln('XXX')
  end;

procedure test114(a: boolean); overload;
  begin
    writeln('boolean called instead of smallint');
    writeln('YYY')
  end;

var
  x114: smallint;

  y114: boolean;
procedure dotest114;
var
  v: variant;

begin
  try
    v := x114;
    test114(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y114;
    test114(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest114;
end. {$endif not bigfile}
