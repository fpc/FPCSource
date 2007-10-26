{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp152 = currency;
{$else FPC_COMP_IS_INT64}
  comp152 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test152(a: single); overload;
  begin
    writeln('single called instead of extended');
  end;

procedure test152(a: extended); overload;
  begin
    writeln('extended called instead of single');
    halt(1)
  end;

var
  x152: single;

  y152: extended;
procedure dotest152;
var
  v: variant;

begin
  try
    v := x152;
    test152(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y152;
    test152(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest152;
end. {$endif not bigfile}
