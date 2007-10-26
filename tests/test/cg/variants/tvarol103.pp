{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp103 = double;
{$else FPC_COMP_IS_INT64}
  comp103 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test103(a: word); overload;
  begin
    writeln('word called instead of boolean');
    writeln('XXX')
  end;

procedure test103(a: boolean); overload;
  begin
    writeln('boolean called instead of word');
    writeln('YYY')
  end;

var
  x103: word;

  y103: boolean;
procedure dotest103;
var
  v: variant;

begin
  try
    v := x103;
    test103(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y103;
    test103(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest103;
end. {$endif not bigfile}
