{ %FAIL }
{ Old file: tbf0166.pp }
{ forward type used in declaration crashes instead of error OK 0.99.9 (PFV) }

type
  punknown=^unknown;

  t=object
    procedure p(i:unknown);
  end;

begin
end.
