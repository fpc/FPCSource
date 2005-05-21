{ %version=1.1 }

{$mode objfpc}

{ Source provided for Free Pascal Bug Report 1825 }
{ Submitted by "marcov" on  2002-02-19 }
{ e-mail: marco@freepascal.org }
program x;

Type
    IEnumMoniker = Interface (IUnknown)
       ['{00000102-0000-0000-C000-000000000046}']
       End;

begin
end.
