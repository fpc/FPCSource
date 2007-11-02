{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp89 = currency;
{$else FPC_COMP_IS_INT64}
  comp89 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test89(a: cardinal); overload;
  begin
    writeln('cardinal called instead of extended');
  end;

procedure test89(a: extended); overload;
  begin
    writeln('extended called instead of cardinal');
    halt(1)
  end;

var
  x89: cardinal;

  y89: extended;
procedure dotest89;
var
  v: variant;

begin
  try
    v := x89;
    test89(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y89;
    test89(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest89;
end. {$endif not bigfile}
