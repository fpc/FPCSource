{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp121 = currency;
{$else FPC_COMP_IS_INT64}
  comp121 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test121(a: byte); overload;
  begin
    writeln('byte called instead of double');
  end;

procedure test121(a: double); overload;
  begin
    writeln('double called instead of byte');
    halt(1)
  end;

var
  x121: byte;

  y121: double;
procedure dotest121;
var
  v: variant;

begin
  try
    v := x121;
    test121(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y121;
    test121(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest121;
end. {$endif not bigfile}
