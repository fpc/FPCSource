{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp74 = double;
{$else FPC_COMP_IS_INT64}
  comp74 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test74(a: longint); overload;
  begin
    writeln('longint called instead of single');
  end;

procedure test74(a: single); overload;
  begin
    writeln('single called instead of longint');
    halt(1)
  end;

var
  x74: longint;

  y74: single;
procedure dotest74;
var
  v: variant;

begin
  try
    v := x74;
    test74(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y74;
    test74(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest74;
end. {$endif not bigfile}
