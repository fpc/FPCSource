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
  comp51 = double;
{$else FPC_COMP_IS_INT64}
  comp51 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test51(a: int64); overload;
  begin
    writeln('int64 called instead of widechar');
    writeln('XXX')
  end;

procedure test51(a: widechar); overload;
  begin
    writeln('widechar called instead of int64');
    writeln('YYY')
  end;

var
  x51: int64;

  y51: widechar;
procedure dotest51;
var
  v: variant;

begin
  try
    v := x51;
    test51(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y51;
    test51(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest51;
end. {$endif not bigfile}
