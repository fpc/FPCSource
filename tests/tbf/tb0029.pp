{ %FAIL }
{ Old file: tbf0158.pp }
{ Invalid boolean typecast                              OK 0.99.7 (PFV) }

program tmp;

var
   Molo  :Boolean;

begin
   Molo := 1;     { This should give out a Type mismatch error ! }
end.
