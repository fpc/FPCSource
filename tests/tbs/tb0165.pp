{ %OPT= -So }

{ Old file: tbs0196.pp }
{ "function a;" is accepted (should require result type) OK 0.99.1 (PM) }

Unit tb0165;
interface

  function a : integer;

implementation
  function a;
begin
  a:=1;
end;

end.
