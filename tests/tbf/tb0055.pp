{ %FAIL }
{ Old file: tbf0284.pp }
{ wrong file position with dup id in other unit        OK 0.99.13 (PFV) }

uses tbs0284b;
{$HINTS ON}
type
  o2=object(o1)
    p : longint;
  end;

begin
end.
