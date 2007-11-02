{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp39 = double;
{$else FPC_COMP_IS_INT64}
  comp39 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test39(a: int64); overload;
  begin
    writeln('int64 called instead of word');
    halt(1)
  end;

procedure test39(a: word); overload;
  begin
    writeln('word called instead of int64');
  end;

var
  x39: int64;

  y39: word;
procedure dotest39;
var
  v: variant;

begin
  try
    v := x39;
    test39(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y39;
    test39(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest39;
end. {$endif not bigfile}
