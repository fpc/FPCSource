{ List Operations }

Program lists;

uses SysUtils, classes;

const SIZE : longint = 10000;

Function test_lists : integer;
var
    i, len1, len2 : longint;
    Li1, Li2, Li3 : TList;
    lists_equal : Integer;
begin

    Li1 := TList.Create;
    Li1.Capacity := SIZE;
    For i := 0 to SIZE Do
        Li1.Add(Pointer(i));



    Li2 := TList.Create;
    Li2.Capacity := SIZE;
    For i:= 0 to SIZE Do
        Li2.Add(Li1.Items[i]);

    { remove each individual item from left side of Li2 and
      append to right side of Li3 (preserving order) }
    Li3 := TList.Create;
    Li3.Capacity := SIZE;
    For i := 0 to SIZE Do
    begin
        Li3.Add( Li2.First );
        Li2.Remove( Li2.First );
    end;


    { remove each individual item from right side of Li3 and
      append to right side of Li2 (reversing list) }
    For i := 0 To SIZE Do
    begin
        Li2.Add( Li3.Last );
        Li3.Count -= 1;
    end;




    For i := 0 To (SIZE div 2) Do
    begin
        Li1.Exchange( i, SIZE-i );
    end;


    If longint(Li1.first) <> SIZE Then
    begin

        test_lists := 0;
        exit;
    end;


    len1 := Li1.Count - 1;
    len2 := Li2.Count - 1;
    If  len1 <> len2 Then
    begin
        test_lists := 0;
        exit;
    end;

    lists_equal := 1;
    For i := 0 To len1 Do
    begin
        If longint(Li1.items[i]) <> longint(Li2.items[i]) Then
        begin
            lists_equal := 0;
            break;
        end;
    end;

    If lists_equal = 0 Then
    begin
        test_lists := 0;
    end
    else
        test_lists := len1;
end;

var
    ITER, i, result: integer;

begin
    if ParamCount = 0 then
        ITER := 1
    else
        ITER := StrToInt(ParamStr(1));

    if ITER < 1 then ITER := 1;

    For i := 1 To ITER Do result := test_lists();
    Writeln (IntToStr(result));

end.
