program Test1;

  type
    ExampleProc = procedure;

  var
    Eg: ExampleProc;

  begin
    Eg := nil;  { This produces a compiler error }
  end.
