{ %fail }
{$mode delphi}

{ manual verification: make sure it complains about a missing overload
  directive rather than saying that overloading is disabled
}

procedure test(a: longint);
begin
end;

procedure test(s: string);
begin
end;

begin
end.
