{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp69 = double;
{$else FPC_COMP_IS_INT64}
  comp69 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test69(a: longint); overload;
  begin
    writeln('longint called instead of smallint');
    halt(1)
  end;

procedure test69(a: smallint); overload;
  begin
    writeln('smallint called instead of longint');
  end;

var
  x69: longint;

  y69: smallint;
procedure dotest69;
var
  v: variant;

begin
  try
    v := x69;
    test69(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y69;
    test69(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest69;
end. {$endif not bigfile}
