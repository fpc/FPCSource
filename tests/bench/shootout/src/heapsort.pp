{ Heapsort }

program heapsort;
uses SysUtils, Classes;

const
    IM = 139968;
    IA =   3877;
    IC =  29573;

var
    ary: TList;
    r : real;
    rr : ^real;
    N, i, LAST : longint;

function gen_random(n : longint) : real;
begin
    LAST := (LAST * IA + IC) mod IM;
    gen_random := n * LAST / IM;
end;

procedure myheapsort(n : longint; var ra : TList);
var
    rr : ^real;
    rra : real;
    i, j, l, ir : longint;
begin
    rra := 0;
    i := 0;
    j := 0;
    l := n shr 1 + 1;
    ir := n;

    while 1 = 1 do
    begin
        if l > 1 then begin
            Dec(l);
            rra := real(ra.Items[l]^);
        end
        else begin
            rra := real(ra.Items[ir]^);



            GetMem(rr, SizeOf(real));
            rr^ := real(ra.Items[1]^);
            ra.items[ir] := rr;


            Dec(ir);
            if ir = 1 then
            begin


                GetMem(rr, SizeOf(real));
                rr^ := rra;
                ra.items[1] := rr;

                exit;
            end;
        end;

        i := l;
        j := l shl 1;



        while j <= ir do begin
            if (j < ir) and (real(ra.items[j]^) < real(ra.items[j+1]^)) then
Inc(j);




            if rra < real(ra.items[j]^) then begin


                GetMem(rr, SizeOf(real));
                rr^ := real(ra.items[j]^);
                ra.items[i] := rr;

                i := j;
                Inc(j, i);
            end
            else begin
                j := ir + 1;
            end;
        end;

        GetMem(rr, SizeOf(real));
        rr^ := rra;
        ra.items[i] := rr;

    end;
end;

begin
    if ParamCount = 0 then
        N := 1
    else
        N := StrToInt(ParamStr(1));
    if N < 1 then N := 1;
    LAST := 42;
    ary := TList.Create;
    ary.Capacity := N;
    r := 0.0;
    GetMem( rr, SizeOf(real) );
    rr^ := r;
    ary.Add( rr );
    for i:= 1 to N do begin
        r := gen_random(1);
        GetMem( rr, SizeOf(real) );
        rr^ := r;

        ary.Add( rr );
    end;
    for i:= 1 to N do begin
        r := real(ary.items[i]^);

    end;
    myheapsort(N, ary);
    r := real(ary.items[N]^);
    WriteLn( r:10:10 );
    ary.Free;
end.
