{ Statistical Moments }

Program moments;
uses SysUtils, Classes;

function Power(Base : Real ; Exponent: Integer): Real;
var i : integer;
var pow : real;
begin
    pow := Base;
    For i:= 2 To Exponent do pow := pow * Base;
    Power := pow;
end;

function Compare(A, B : Pointer) : longint;
begin
    if A > B then
        Compare := 1
    else if A < B Then
        Compare := -1
    else
        Compare := 0;
end;


var
    i, N, sum, num, middle : longint;
    list : TList;
    median, mean, deviation,
    average_deviation, standard_deviation,
    variance, skew, kurtosis : real;
begin
    list := TList.Create;
    While Not Eof(input) do
    begin
        Readln(input, num);
        list.Add( Pointer(num) );
    end;
    N := list.Count;
    For i := 0 To N-1 do Inc(sum, longint(list.Items[i]));
    mean := sum / N;
    average_deviation := 0;
    standard_deviation := 0;
    variance := 0;
    skew := 0;
    kurtosis := 0;

    For i := 0 To N-1 do
    begin
        deviation := longint(list.Items[i]) - mean;
        average_deviation := average_deviation + Abs(deviation);
        variance := variance + Power(deviation, 2);
        skew := skew + Power(deviation, 3);
        kurtosis := kurtosis + Power(deviation, 4);

    end;
    average_deviation := average_deviation / N;
    variance := variance / (N-1);
    standard_deviation := Sqrt(variance);


    If variance <> 0 Then
    begin
        skew := skew / (N * variance * standard_deviation);
        kurtosis := kurtosis / (N * variance * variance ) - 3.0;
    end;

    list.Sort(@Compare);


    middle := N Div 2;

    If (N Mod 2) <> 0 Then
        median := longint(list.Items[middle])
    Else
        median := (longint(list.Items[middle]) +
longint(list.Items[middle-1])) / 2;


    WriteLn('n:                  ', N);
    WriteLn('median:             ', median:6:6);
    WriteLn('mean:               ', mean:6:6);
    WriteLn('average_deviation:  ', average_deviation:6:6);
    WriteLn('standard_deviation: ', standard_deviation:6:6);
    WriteLn('variance:           ', variance:6:6);
    WriteLn('skew:               ', skew:6:6);
    WriteLn('kurtosis:           ', kurtosis:6:6);
end.
