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
  comp9 = double;
{$else FPC_COMP_IS_INT64}
  comp9 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test9(var a); overload;
  begin
    writeln('formal called instead of shortint');
    writeln('XXX')
  end;

procedure test9(a: shortint); overload;
  begin
    writeln('shortint called instead of formal');
    writeln('YYY')
  end;

var
  x9: longint;

  y9: shortint;
procedure dotest9;
var
  v: variant;

begin
  try
    v := x9;
    test9(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y9;
    test9(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest9;
end. {$endif not bigfile}
