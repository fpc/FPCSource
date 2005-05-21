{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993-98 by Michael Van Canneyt

    Blackbox Game Example

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Program blackbox;

{
  The object of the game is simple : You have a box of 9x9x9 cells.
  you can enter a number of atoms that will be put in the box.
  Then you can start shooting in the box with a laser beam.
  You enter the coordinates where the beam enters the box.
  (this must be on the edges, this means that one of the coordinates
  must be 1 or 9...)
  The beam will bounce off the atoms (using normal bouncing), and you
  will be told where the beam exits.
  From this you must guess where the atoms are...
}

Const MaxSize = 9;
      MaxAtom = 10;

Type TRow   = Array [0..MaxSize+1] of byte;
     TPlane = Array [0..MaxSize+1] of TRow;
     TCube  = Array [0..MaxSize+1] of TPlane;

Var
  Cube                 : TCube;
  Count,Guessed,x,y,z  : Longint;
  ans : string;

Procedure FillCube;

var i,x,y,z : longint;

begin
  randomize;
  for x:=0 to maxsize+1 do
    for y:=0 to maxsize+1 do
      for z:=0 to maxsize+1 do
        Cube[x,y,z]:=0;
  repeat
    Write ('Enter number of atoms (1-',maxatom,') : ');
    readln (count);
    if (count<1) or (count>MaxAtom) then
      writeln ('Invalid value entered. Please try again.');
  until (count>0) and (count<=MaxAtom);
  for I:=1 to count do
     begin
     repeat
       x:=Random(MaxSize)+1;
       y:=Random(MaxSize)+1;
       z:=Random(MaxSize)+1;
     until Cube[x,y,z]=0;
     Cube[x,y,z]:=1;
     end;
end;

Procedure GetCoords (Var X,y,z : longint);

begin
  Write ('X : ');
  readln (x);
  write ('Y : ');
  readln (y);
  write ('z : ');
  readln (z);
end;

Procedure GetStart (Var x,y,z : longint);

Var OK : boolean;

begin
  Writeln ('Please enter beam start coordinates : ');
  Repeat
    GetCoords (x,y,z);
    OK:=((X=1) or (X=MaxSize)) and ((y=1) or (Y=MaxSize)) and
        ((Z=1) or (z=maxsize));
    if Not OK then
      writeln ('The beam should enter at an edge. Please try again');
  until OK;
end;

Function GetGuess : boolean;

Var OK : boolean;
    x,y,z : longint;

begin
  Writeln ('Please enter atom coordinates : ');
  Repeat
    getcoords (x,y,z);
    OK:=((X>=1) and (X<=MaxSize)) and ((y>=1) and (Y<=MaxSize)) and
        ((Z>=1) and (z<=maxsize));
    if Not OK then
      writeln ('These are not valid coordinates. Please try again');
  until OK;
  GetGuess:=False;
  If Cube[x,y,z]<0 then
    Writeln ('You already had this one ! Trying to be clever, eh ?')
  else if Cube[x,y,z]>0 then
    begin
    Writeln ('Correct guess !');
    Cube[x,y,z]:=-Cube[x,y,z];
    getguess:=true;
    end
  else
    Writeln ('Wrong guess !');
end;

Procedure CalcExit (X,Y,Z : longint);

var tx,ty,tz,dx,dy,dz : longint;

begin
  dx:=0;dy:=0;dz:=0;
  if x=1 then dx:=1 else if x=MaxSize then dx:=-1;
  if y=1 then dy:=1 else if y=MaxSize then dy:=-1;
  if z=1 then dz:=1 else if z=MaxSize then dz:=-1;
  writeln ('Direction : ',dx,',',dy,',',dz);
  repeat
  for tx:=-1 to 1 do
    for ty:=-1 to 1 do
      for tz:=-1 to 1 do
        if Cube [X+tx,y+ty,z+tz]<>0 then
          begin
          dx:=dx-tx;
          dy:=dy-ty;
          dz:=dz-tz;
          end;
  if dx<>0 then dx:=dx div abs(dx);
  if dz<>0 then dz:=dz div abs(dz);
  if dy<>0 then dy:=dy div abs(dy);
  x:=x+dx;y:=y+dy;z:=z+dz;
  until ((x=0) or (x=MaxSize+1)) or ((y=0) or (y=maxsize+1)) or
        ((z=0) or (z=maxsize+1));
  Writeln ('Beam exited at : (',x-dx,',',y-dy,',',z-dz,')');
end;

{
Procedure DumpCube ;

Var x,y,z : longint;

begin
  for x:=1 to MaxSize do
    for y:=1 to maxsize do
      for z:=1 to maxsize do
        if Cube[x,y,z]<>0 then
          writeln ('Atom at (',x,',',y,',',z,')');
end;
}

begin
  FillCube;
  Guessed:=0;
  Repeat
    repeat
      Write ('Shoot, guess or quit (s/g/q) : ');
      readln (ans);
      ans[1]:=Upcase(ans[1]);
      if not (ans[1] in ['S','G','Q']) then
        writeln ('Invalid entry. Please try again.');
    until ans[1] in ['S','G','Q'];
    Case ans[1] of
     'S' : begin
           getstart (x,y,z);
           calcexit (x,y,z);
           end;
     'G' : If GetGuess then Inc(Guessed);
    end;
  until (ans[1]='Q') or (guessed=count);
  If Guessed=count then
    Writeln ('Congratulations! All ',Count,' correct !')
  else
    Writeln ('Only ',guessed,' out of ',count,' correct...');
end.
