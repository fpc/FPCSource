{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp165 = double;
{$else FPC_COMP_IS_INT64}
  comp165 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test165(a: extended); overload;
  begin
    writeln('extended called instead of widechar');
  end;

procedure test165(a: widechar); overload;
  begin
    writeln('widechar called instead of extended');
    halt(1)
  end;

var
  x165: extended;

  y165: widechar;
procedure dotest165;
var
  v: variant;

begin
  try
    v := x165;
    test165(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y165;
    test165(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest165;
end. {$endif not bigfile}
