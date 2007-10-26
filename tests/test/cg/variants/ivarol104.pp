{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp104 = double;
{$else FPC_COMP_IS_INT64}
  comp104 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test104(a: word); overload;
  begin
    writeln('word called instead of widestring');
  end;

procedure test104(a: widestring); overload;
  begin
    writeln('widestring called instead of word');
    halt(1)
  end;

var
  x104: word;

  y104: widestring;
procedure dotest104;
var
  v: variant;

begin
  try
    v := x104;
    test104(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y104;
    test104(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest104;
end. {$endif not bigfile}
