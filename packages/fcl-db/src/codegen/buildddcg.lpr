program buildddcg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this }, fpddpopcode, fpcgcreatedbf, fpcgdbcoll,
  fpcgsqlconst, fpcgtiopf, fpddcodegen;

begin
end.

