{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp46 = currency;
{$else FPC_COMP_IS_INT64}
  comp46 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test46(a: int64); overload;
  begin
    writeln('int64 called instead of double');
    halt(1)
  end;

procedure test46(a: double); overload;
  begin
    writeln('double called instead of int64');
  end;

var
  x46: int64;

  y46: double;
procedure dotest46;
var
  v: variant;

begin
  try
    v := x46;
    test46(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y46;
    test46(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest46;
end. {$endif not bigfile}
