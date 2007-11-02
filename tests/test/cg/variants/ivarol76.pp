{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp76 = currency;
{$else FPC_COMP_IS_INT64}
  comp76 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test76(a: longint); overload;
  begin
    writeln('longint called instead of extended');
  end;

procedure test76(a: extended); overload;
  begin
    writeln('extended called instead of longint');
    halt(1)
  end;

var
  x76: longint;

  y76: extended;
procedure dotest76;
var
  v: variant;

begin
  try
    v := x76;
    test76(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y76;
    test76(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest76;
end. {$endif not bigfile}
