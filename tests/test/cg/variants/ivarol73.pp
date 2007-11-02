{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp73 = double;
{$else FPC_COMP_IS_INT64}
  comp73 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test73(a: longint); overload;
  begin
    writeln('longint called instead of ansistring');
  end;

procedure test73(a: ansistring); overload;
  begin
    writeln('ansistring called instead of longint');
    halt(1)
  end;

var
  x73: longint;

  y73: ansistring;
procedure dotest73;
var
  v: variant;

begin
  try
    v := x73;
    test73(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y73;
    test73(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest73;
end. {$endif not bigfile}
