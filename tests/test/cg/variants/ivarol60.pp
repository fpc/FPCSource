{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp60 = double;
{$else FPC_COMP_IS_INT64}
  comp60 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test60(a: currency); overload;
  begin
    writeln('currency called instead of single');
    halt(1)
  end;

procedure test60(a: single); overload;
  begin
    writeln('single called instead of currency');
  end;

var
  x60: currency;

  y60: single;
procedure dotest60;
var
  v: variant;

begin
  try
    v := x60;
    test60(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y60;
    test60(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest60;
end. {$endif not bigfile}
