{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp81 = double;
{$else FPC_COMP_IS_INT64}
  comp81 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test81(a: cardinal); overload;
  begin
    writeln('cardinal called instead of word');
    halt(1)
  end;

procedure test81(a: word); overload;
  begin
    writeln('word called instead of cardinal');
  end;

var
  x81: cardinal;

  y81: word;
procedure dotest81;
var
  v: variant;

begin
  try
    v := x81;
    test81(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y81;
    test81(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest81;
end. {$endif not bigfile}
