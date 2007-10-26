{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp112 = currency;
{$else FPC_COMP_IS_INT64}
  comp112 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test112(a: smallint); overload;
  begin
    writeln('smallint called instead of extended');
  end;

procedure test112(a: extended); overload;
  begin
    writeln('extended called instead of smallint');
    halt(1)
  end;

var
  x112: smallint;

  y112: extended;
procedure dotest112;
var
  v: variant;

begin
  try
    v := x112;
    test112(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y112;
    test112(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest112;
end. {$endif not bigfile}
