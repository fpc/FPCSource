{ Old file: tbs0192.pp }
{ can't compare boolean result with true/false, because the boolean result is already in the flags             OK 0.99.11 (PFV) }

var
  k,l : word;
begin
  if (k<>l)=false then
   ;
  if (k<>l)=true then
   ;
end.
