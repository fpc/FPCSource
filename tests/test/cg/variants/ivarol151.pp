{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp151 = currency;
{$else FPC_COMP_IS_INT64}
  comp151 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test151(a: single); overload;
  begin
    writeln('single called instead of double');
  end;

procedure test151(a: double); overload;
  begin
    writeln('double called instead of single');
    halt(1)
  end;

var
  x151: single;

  y151: double;
procedure dotest151;
var
  v: variant;

begin
  try
    v := x151;
    test151(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y151;
    test151(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest151;
end. {$endif not bigfile}
