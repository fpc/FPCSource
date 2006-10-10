{ Matrix Multiplication }

program matrix;
uses SysUtils;

const
    size = 30;

type tMatrix = array[0..size, 0..size] of longint;

procedure mkmatrix( rows, cols : integer; var mx : tMatrix);
var
    R, C : integer;
    count : longint;
begin
    Dec(rows);
    Dec(cols);
    count := 1;
    for R := 0 to rows do
    begin
        for C := 0 to cols do
        begin
            mx[R, C] := count;
            Inc(count);
        end;
    end;
End;

procedure mmult(rows, cols : integer; m1, m2 : tMatrix; var mm : tMatrix );
var
    i, j, k : integer;
    val: longint;
begin
    Dec(rows);
    Dec(cols);
    For i := 0 To rows do
    begin
        For j := 0 To cols do
        begin
            val := 0;
            For k := 0 To cols do
            begin
                Inc(val, m1[i, k] * m2[k, j]);
            end;
            mm[i, j] := val;
        end;
    end;
End;


var NUM, I : integer;
    M1, M2, MM : tMatrix;

begin
    if ParamCount = 0 then
        NUM := 1
    else
        NUM := StrToInt(ParamStr(1));

    if NUM < 1 then NUM := 1;

    mkmatrix(size, size, M1);
    mkmatrix(size, size, M2);

    for I := 0 To NUM do
    begin
        mmult(size, size, M1, M2, MM);
    end;
    WriteLn( IntToStr(MM[0, 0]) + ' ' + IntToStr(MM[2, 3]) + ' ' +
             IntToStr(MM[3, 2]) + ' ' + IntToStr(MM[4, 4]));
end.
