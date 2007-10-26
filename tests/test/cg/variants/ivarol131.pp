{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp131 = currency;
{$else FPC_COMP_IS_INT64}
  comp131 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test131(a: shortint); overload;
  begin
    writeln('shortint called instead of extended');
  end;

procedure test131(a: extended); overload;
  begin
    writeln('extended called instead of shortint');
    halt(1)
  end;

var
  x131: shortint;

  y131: extended;
procedure dotest131;
var
  v: variant;

begin
  try
    v := x131;
    test131(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y131;
    test131(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest131;
end. {$endif not bigfile}
