{ Old file: tbs0081.pp }
{  Shows incompatibility with borland's 'array of char'. OK 0.99.1 (FK) }

program bug0081;

const
   EOL : array [1..2] of char = #13 + #10;

begin
end.
