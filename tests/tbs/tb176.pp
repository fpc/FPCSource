{ Old file: tbs0203.pp }
{ problem with changed mangledname of procedures after use }

program tbs0203;

uses
{$ifdef go32v2}
  dpmiexcp,
{$endif def go32v2}
  tbs0203a;

begin
   c;
   a;
end.

