Program example80;

{ Example to demonstrate the High and Low functions. }

Type TEnum = ( North, East, South, West );
     TRange = 14..55;
     TArray = Array [2..10] of Longint;

Function Average (Row : Array of Longint) : Real;

Var I : longint;
    Temp : Real;


begin
  Temp := Row[0];
  For I := 1 to High(Row) do
     Temp := Temp + Row[i];
  Average := Temp / (High(Row)+1);
end;

Var A : TEnum;
    B : TRange;
    C : TArray;
    I : longint;

begin
  Writeln ('TEnum  goes from : ',Ord(Low(TEnum)),' to ', Ord(high(TEnum)),'.');
  Writeln ('A      goes from : ',Ord(Low(A)),' to ', Ord(high(A)),'.');
  Writeln ('TRange goes from : ',Ord(Low(TRange)),' to ', Ord(high(TRange)),'.');
  Writeln ('B      goes from : ',Ord(Low(B)),' to ', Ord(high(B)),'.');
  Writeln ('TArray index goes from : ',Ord(Low(TArray)),' to ', Ord(high(TArray)),'.');
  Writeln ('C index      goes from : ',Low(C),' to ', high(C),'.');
  For I:=Low(C) to High(C) do
    C[i]:=I;
  Writeln ('Average :',Average(c));
  Write ('Type of return value is always same as type of argument:');
  Writeln(high(high(word)));
end.
 