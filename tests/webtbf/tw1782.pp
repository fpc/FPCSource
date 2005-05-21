{ %fail }

{ Source provided for Free Pascal Bug Report 1782 }
{ Submitted by "Aleksey V. Vaneev" on  2002-01-27 }
{ e-mail: picoder@sbis.komi.ru }

{$mode objfpc}

type
        PStruct = ^TStruct;
        TStruct =
                packed record
                        i: LongInt;
                end;

        TCls = class
                protected
                        Fv: array [1..10] of TStruct;

                        function getv (i: LongInt): TStruct;

                public
                        property v [i: LongInt]: TStruct read getv;
                end;

function TCls.getv (i: LongInt): TStruct;
begin
        Result := Fv [i];
end;

var
        sp: PStruct;
        o: TCls;

begin
        o := TCls.create;
        { Should fail with 'variable expected' }
        sp := @o.v [10];
end.
