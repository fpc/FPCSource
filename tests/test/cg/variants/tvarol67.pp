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
  comp67 = double;
{$else FPC_COMP_IS_INT64}
  comp67 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test67(a: longint); overload;
  begin
    writeln('longint called instead of cardinal');
    writeln('XXX')
  end;

procedure test67(a: cardinal); overload;
  begin
    writeln('cardinal called instead of longint');
    writeln('YYY')
  end;

var
  x67: longint;

  y67: cardinal;
procedure dotest67;
var
  v: variant;

begin
  try
    v := x67;
    test67(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y67;
    test67(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest67;
end. {$endif not bigfile}
