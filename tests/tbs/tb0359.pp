{ %version=1.1 }

{$ifdef unix}

{$linklib c}

procedure printf(t:pchar);varargs;cdecl;external;

begin
  printf("Test %d",1);
{$else}
begin
  writeln('Unix only test');
{$endif}
end.
