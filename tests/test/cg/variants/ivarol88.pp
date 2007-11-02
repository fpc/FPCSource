{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp88 = currency;
{$else FPC_COMP_IS_INT64}
  comp88 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test88(a: cardinal); overload;
  begin
    writeln('cardinal called instead of double');
  end;

procedure test88(a: double); overload;
  begin
    writeln('double called instead of cardinal');
    halt(1)
  end;

var
  x88: cardinal;

  y88: double;
procedure dotest88;
var
  v: variant;

begin
  try
    v := x88;
    test88(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y88;
    test88(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest88;
end. {$endif not bigfile}
