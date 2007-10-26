{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp47 = currency;
{$else FPC_COMP_IS_INT64}
  comp47 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test47(a: int64); overload;
  begin
    writeln('int64 called instead of extended');
    halt(1)
  end;

procedure test47(a: extended); overload;
  begin
    writeln('extended called instead of int64');
  end;

var
  x47: int64;

  y47: extended;
procedure dotest47;
var
  v: variant;

begin
  try
    v := x47;
    test47(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y47;
    test47(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest47;
end. {$endif not bigfile}
