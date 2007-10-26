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
  comp4 = double;
{$else FPC_COMP_IS_INT64}
  comp4 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test4(var a); overload;
  begin
    writeln('formal called instead of longint');
    writeln('XXX')
  end;

procedure test4(a: longint); overload;
  begin
    writeln('longint called instead of formal');
    writeln('YYY')
  end;

var
  x4: longint;

  y4: longint;
procedure dotest4;
var
  v: variant;

begin
  try
    v := x4;
    test4(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y4;
    test4(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest4;
end. {$endif not bigfile}
