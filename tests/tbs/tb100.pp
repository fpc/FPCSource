{ Old file: tbs0118.pp }
{ Procedural vars cannot be assigned nil ?              OK 0.99.6 (FK) }

program Test1;

  type
    ExampleProc = procedure;

  var
    Eg: ExampleProc;

  begin
    Eg := nil;  { This produces a compiler error }
  end.
