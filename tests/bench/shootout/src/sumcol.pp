{ Sum a Column of Integers }

program sumcol;

var
    num, tot: longint;
begin
    While Not Eof(input) Do
    begin
        ReadLn(input, num);
        tot := tot + num;
    end;
    WriteLn(tot);
end.
