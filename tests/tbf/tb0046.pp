{ %FAIL }
{ Old file: tbf0234.pp }
{ New with void pointer                                OK 0.99.11 (PM) }

program bug0232;

var p:pointer;

begin
     new(p);
     dispose(p);
end.
