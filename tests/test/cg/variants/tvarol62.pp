{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp62 = currency;
{$else FPC_COMP_IS_INT64}
  comp62 = comp;
{$endif FPC_COMP_IS_INT64}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure test62(a: currency); overload;
  begin
    writeln('currency called instead of extended');
  end;

procedure test62(a: extended); overload;
  begin
    writeln('extended called instead of currency');
    halt(1)
  end;

var
  x62: currency;

  y62: extended;
procedure dotest62;
var
  v: variant;

begin
  try
    v := x62;
    test62(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y62;
    test62(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest62;
end. {$endif not bigfile}
{$else FPC_HAS_TYPE_EXTENDED}
begin
end.
{$endif FPC_HAS_TYPE_EXTENDED}
