{ %FAIL }
{ %OPT=-Sb }
{ -Sb does not skip a "#!" first line in an include file }
program tshebang5;

{$I ishebang.inc}

begin
end.
