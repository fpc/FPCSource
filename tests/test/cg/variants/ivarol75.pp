{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp75 = currency;
{$else FPC_COMP_IS_INT64}
  comp75 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test75(a: longint); overload;
  begin
    writeln('longint called instead of double');
  end;

procedure test75(a: double); overload;
  begin
    writeln('double called instead of longint');
    halt(1)
  end;

var
  x75: longint;

  y75: double;
procedure dotest75;
var
  v: variant;

begin
  try
    v := x75;
    test75(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y75;
    test75(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest75;
end. {$endif not bigfile}
