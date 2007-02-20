{ based on gpc test pvs1 }
{ FLAG --extended-pascal }

{TEST 6.6.5.4-1, CLASS=CONFORMANCE}

{ This program tests that pack and unpack are
  implemented in this compiler as according to the
  Standard.
  The compiler fails if the program does not compile. }

program t6p6p5p4d1(output);

{$mode macpas}

type
   colourtype = (red,pink,orange,yellow,green,blue);

var
   unone    : array[3..24] of char;
   pacy     : array[1..4] of char;
   pactwo   : packed array[6..7] of colourtype;
   i        : integer;
   colour   : colourtype;
   s: string;

const
   pacone   : packed array[1..4] of char = 'ABCD';
   untwo    : array[4..8] of colourtype = (red,pink,orange,yellow,green);
begin
   pacy:=pacone;
   if pacy <> 'ABCD' then
     halt(1);
   s := pacone;
   unpack(pacone,unone,5);
   if (unone[3] <> #0) or
      (unone[4] <> #0) or
      (unone[5] <> 'A') or
      (unone[6] <> 'B') or
      (unone[7] <> 'C') or
      (unone[8] <> 'D') or
      (unone[9] <> #0) or
      (unone[10] <> #0) or
      (unone[11] <> #0) then
     halt(1);
   colour:=red;
   for i:=4 to 8 do
   begin
      if (untwo[i]<>colour) then
        halt(2);
      colour:=succ(colour)
   end;
   pack(untwo,5,pactwo);
   if (pactwo[6] <> pink) or
      (pactwo[7] <> orange) then
     halt(1);
   writeln('unone[5] = ''', unone[5], ''' = ', ord(unone[5]));
   if unone[5]='A' then
      writeln(' PASS...6.6.5.4-1')
   else
     begin
       writeln(' FAIL...6.6.5.4-1');
       halt(1);
     end;
end.

