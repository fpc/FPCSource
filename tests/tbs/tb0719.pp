{ %NORUN }
{ %EXPECTMSGS=5024 }
{ %OPT=-vh }

program tb0718;

{$warn 5024 on}

{$push}
{$warn 5024 off}

procedure Test(aArg: LongInt);
begin
  { message 5024 should be hidden for this }
end;

{$pop}

procedure Test2(aArg: LongInt);
begin
  { message 5024 should be visible for this }
end;

begin
  Test(42);
  Test2(42);
end.
