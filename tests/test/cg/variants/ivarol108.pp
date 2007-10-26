{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp108 = double;
{$else FPC_COMP_IS_INT64}
  comp108 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test108(a: smallint); overload;
  begin
    writeln('smallint called instead of shortstring');
  end;

procedure test108(a: shortstring); overload;
  begin
    writeln('shortstring called instead of smallint');
    halt(1)
  end;

var
  x108: smallint;

  y108: shortstring;
procedure dotest108;
var
  v: variant;

begin
  try
    v := x108;
    test108(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y108;
    test108(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest108;
end. {$endif not bigfile}
