{ %OPT=-O2 }
{$mode objfpc}

Function SmartPos(Pattern:Char; const Target:AnsiString; SrchBeg,SrchEnd: LongInt): LongInt;
Var
        i       : LongInt;
        l       : LongInt;
        p       : char;
        t       : char;
Begin


        l := Length(Target);

        p := Pattern;

        for i := SrchBeg To l do begin
                t := Target[i];
                if t = p then begin
                        Writeln('Exit value ', i);
                        Result := i;
                        Exit(i);
                end;
        end;

        Writeln('Exit value 0');
        Exit(0);
End;

var
        p1 : Integer;

begin

        p1 := 0;
        writeln(p1);
        p1 := smartpos(':', '20394583245:092834523409:039485', 1, 20);
        writeln(p1);
        if p1<>12 then
         halt(1);
end.
