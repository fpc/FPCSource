{ Old file: tbs0277.pp }
{ typecasting with const not possible                  OK 0.99.13 (PFV) }

  program bug0277;
  const test_byte=pchar(1);
  begin
    writeln('Hello world');
  end.
