{ %FAIL }
{ Old file: tbf0151.pp }
{ crash when using undeclared variable in withstatement OK 0.99.7 (PFV) }

type tr = record
            l1, l2: longint
          end;

var r: tr;

begin
  with r do
    inc(l)
end.
