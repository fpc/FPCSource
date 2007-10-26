{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp72 = double;
{$else FPC_COMP_IS_INT64}
  comp72 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test72(a: longint); overload;
  begin
    writeln('longint called instead of shortstring');
  end;

procedure test72(a: shortstring); overload;
  begin
    writeln('shortstring called instead of longint');
    halt(1)
  end;

var
  x72: longint;

  y72: shortstring;
procedure dotest72;
var
  v: variant;

begin
  try
    v := x72;
    test72(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y72;
    test72(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest72;
end. {$endif not bigfile}
