{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp70 = double;
{$else FPC_COMP_IS_INT64}
  comp70 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test70(a: longint); overload;
  begin
    writeln('longint called instead of byte');
    halt(1)
  end;

procedure test70(a: byte); overload;
  begin
    writeln('byte called instead of longint');
  end;

var
  x70: longint;

  y70: byte;
procedure dotest70;
var
  v: variant;

begin
  try
    v := x70;
    test70(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y70;
    test70(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest70;
end. {$endif not bigfile}
