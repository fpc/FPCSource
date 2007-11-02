{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp71 = double;
{$else FPC_COMP_IS_INT64}
  comp71 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test71(a: longint); overload;
  begin
    writeln('longint called instead of shortint');
    halt(1)
  end;

procedure test71(a: shortint); overload;
  begin
    writeln('shortint called instead of longint');
  end;

var
  x71: longint;

  y71: shortint;
procedure dotest71;
var
  v: variant;

begin
  try
    v := x71;
    test71(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y71;
    test71(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest71;
end. {$endif not bigfile}
