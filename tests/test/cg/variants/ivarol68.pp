{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp68 = double;
{$else FPC_COMP_IS_INT64}
  comp68 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test68(a: longint); overload;
  begin
    writeln('longint called instead of word');
    halt(1)
  end;

procedure test68(a: word); overload;
  begin
    writeln('word called instead of longint');
  end;

var
  x68: longint;

  y68: word;
procedure dotest68;
var
  v: variant;

begin
  try
    v := x68;
    test68(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y68;
    test68(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest68;
end. {$endif not bigfile}
