{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp138 = currency;
{$else FPC_COMP_IS_INT64}
  comp138 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test138(a: shortstring); overload;
  begin
    writeln('shortstring called instead of double');
    halt(1)
  end;

procedure test138(a: double); overload;
  begin
    writeln('double called instead of shortstring');
  end;

var
  x138: shortstring;

  y138: double;
procedure dotest138;
var
  v: variant;

begin
  try
    v := x138;
    test138(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y138;
    test138(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest138;
end. {$endif not bigfile}
