{ Old file: tbs0083.pp }
{  shows missing "dynamic" set constructor               OK 0.99.7 (PFV) }


var
   s1 : set of char;
   c1,c2,c3 : char;

begin
   s1:=[c1..c2,c3];
end.
