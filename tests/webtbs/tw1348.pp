type mitem=array[0..8] of string[16];

const chdiv:array[0..8] of string
      =('Eclipse','Elongation','Occultation','Conjonction',' Lever',
        'Satellites',' Binaire',' Visibilit‚','Courbe');
const ch:array[0..8] of integer=
       (0,1,2,3,4,5,6,7,8);
var
  Error : boolean;

Procedure affmenu(const Rr:array of string);
 var i,j:integer;
 Begin
  i:=0;j:=high(Rr);writeln(j);
  {rr[j div 2]:='Modif';}
   while (i<=j) do
    begin
     writeln(length(RR[i]));
     writeln(Rr[i]);inc(i);
    end;
  if RR[6]<>' Binaire' then
   Error:=true;
End;

Procedure affint(const Rr:array of integer);
 var i,j:integer;
 Begin
  i:=0;j:=high(Rr);writeln(j);
    while (i<=j) do
    begin
    writeln(Rr[i]);inc(i);
    end;
End;


Procedure affm(Rr:string{$ifdef fpc}[16]{$endif});
Begin
  writeln(Rr);
End;

Begin
 affm(chdiv[8]);
 affint(ch);
 writeln('suite');
 affmenu(chdiv);
 writeln('Fin');
 if Error then
  begin
    writeln('ERROR!');
    halt(1);
  end;
End.
