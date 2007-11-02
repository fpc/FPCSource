{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp102 = double;
{$else FPC_COMP_IS_INT64}
  comp102 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test102(a: word); overload;
  begin
    writeln('word called instead of char');
  end;

procedure test102(a: char); overload;
  begin
    writeln('char called instead of word');
    halt(1)
  end;

var
  x102: word;

  y102: char;
procedure dotest102;
var
  v: variant;

begin
  try
    v := x102;
    test102(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y102;
    test102(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest102;
end. {$endif not bigfile}
