{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp34 = double;
{$else FPC_COMP_IS_INT64}
  comp34 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test34(a: comp34); overload;
  begin
    writeln('comp34 called instead of widestring');
  end;

procedure test34(a: widestring); overload;
  begin
    writeln('widestring called instead of comp34');
    halt(1)
  end;

var
  x34: comp34;

  y34: widestring;
procedure dotest34;
var
  v: variant;

begin
  try
    v := x34;
    test34(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y34;
    test34(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest34;
end. {$endif not bigfile}
