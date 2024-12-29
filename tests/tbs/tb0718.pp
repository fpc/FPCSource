{ %NORUN }
{ %EXPECTMSGS=5024 }
{ %OPT=-vh }

program tb0718;

{$warn 5024 off}

{$warn 5024 on}

procedure Test(aArg: LongInt);
begin
end;

begin
  Test(42);
end.
