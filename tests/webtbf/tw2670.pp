{ %version=1.1 }
{ %fail }
{ Source provided for Free Pascal Bug Report 2670 }
{ Submitted by "marco" on  2003-09-07 }
{ e-mail: marco@freepascal.org }
program bugje;
{$mode delphi}

Uses Classes;

type            theenum = (hello,hi, hoi);
                testme= class(TComponent)
                           field : theenum;
                           end;

begin
end.
