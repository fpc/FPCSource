{ %FAIL }
{ Old file: tbf0196.pp }
{ "function a;" is accepted (should require result type) OK 0.99.1 (PM) }

Program bug0195;

function a;
begin
end;

begin
  a
end.
