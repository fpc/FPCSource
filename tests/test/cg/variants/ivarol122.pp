{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp122 = currency;
{$else FPC_COMP_IS_INT64}
  comp122 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test122(a: byte); overload;
  begin
    writeln('byte called instead of extended');
  end;

procedure test122(a: extended); overload;
  begin
    writeln('extended called instead of byte');
    halt(1)
  end;

var
  x122: byte;

  y122: extended;
procedure dotest122;
var
  v: variant;

begin
  try
    v := x122;
    test122(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y122;
    test122(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest122;
end. {$endif not bigfile}
