{ Old file: tbs0225.pp }
{ Sigsegv when run with range checks on open arrays    OK 0.99.11 (PFV) }

 program bug0255;

{$mode objfpc}

{$R+}

  function erwwert(const feld: array of LongInt):extended;
   var i: LongInt;
   begin
    Result:=0;
    for i:=low(feld) to high(feld)
        do begin
            writeln(i);  // gives "0"
            Result:=Result+feld[i];
           end;          //^^^^^^^ there occurs the segfault (216)
                         //        on the first loop
    Result:=Result/(high(feld)-low(feld)+1);
   end;

 var werte: array[0..299] of LongInt;
     i: LongInt;

 begin
  //init the array
  for i:=0 to 299  do
    werte[i]:=random(5);

  //and do something with it
  writeln(erwwert(werte):6:5);
 end.
