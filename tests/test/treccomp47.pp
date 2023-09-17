{ %FAIL }
{ %NORUN }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  generic TComposed<T: TObject> = record
    A: Integer;
    contains T;
    C: Integer;
  end;

  TChildClass = class
  public
    B: Integer;
  end;

var
  c: specialize TComposed<TChildClass>;
begin
end.
