{ %NORUN }
{ %OPT=-Sew }
program record_compose_test;

{$Mode ObjFPC}{$H+}
{$ModeSwitch AdvancedRecords}
{$ModeSwitch RecordComposition}

type
  TChild1 = record
    class operator +(lhs: TChild1; rhs: TChild1): TChild1;
  end;
  TChild2 = record
    class operator +(lhs: TChild2; rhs: TChild2): TChild2;
  end;

  TComposed = record
    contains TChild1;
    contains TChild2;
  end;

class operator TChild1.+(lhs: TChild1; rhs: TChild1): TChild1;
begin
  Result:=Default(TChild1);
end;

class operator TChild2.+(lhs: TChild2; rhs: TChild2): TChild2;
begin
  Result:=Default(TChild2);
end;

begin
end.
