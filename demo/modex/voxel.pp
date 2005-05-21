{

    This program is part of the FPC demoes.
    Copyright (C) 1999 by Marco van de Voort

    A port of a more "dirty" graphical program, to demonstrate
    some Go32 features. The program displays a landscape in which
    you can move with the cursorkeys

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

The sources for this game was found in SWAG, and was also posted to the
International FIDO Pascal area.
I assume that it is PD (both sources said nothing about the form of copyrights,
but it was contributed to SWAG, which is generally PD)

If there is somebody that claims authorship of these programs,
please mail marco@freepascal.org, and the sources will be removed from our
websites.

------------------------------------------------------------------------
There was no real original, I reconstructed some from several versions.

A voxel source from Swag patched for FPC.

- The original author was unknown. I saw a different version which named
  "Borek" (Marcin Borkowski), 2:480/25  as author.
- Bas van Gaalen donated it to SWAG.
- I, Marco van de Voort made some small FPC adjustments.
- However one problem remained (wrapping of arrays), and Jonas Maebe mailed me
   that glitch to me. This practically meant putting all those WORD()
   typecasts in the array-parameters.

Still BP compatible, Gameunit contains some BP alternatives for Go32
procedures needed.}

PROGRAM voxel;
{$R-}


USES Crt,Dos {$IFDEF FPC}, Go32{$ENDIF};

type lrgarr=array[0..65534] of byte;
const
 pal:array[1..384] of byte=(
  0,0,0,48,48,48,1,0,43,1,3,43,2,5,44,2,7,44,3,9,45,4,11,46,5,13,47,6,15,48,
  7,17,49,8,19,50,9,21,51,10,22,52,11,24,52,12,26,54,13,28,54,14,30,56,15,32,
  56,16,34,58,17,34,58,17,36,58,18,38,60,19,40,60,20,42,62,21,44,62,10,31,0,
  11,31,0,11,31,1,11,32,1,12,32,1,12,32,2,12,33,2,13,33,2,14,33,3,15,33,3,15,
  34,3,15,34,4,15,35,4,16,35,4,16,35,5,16,36,5,17,36,5,17,36,6,18,37,6,18,38,
  7,19,38,8,20,39,8,20,40,9,21,40,10,22,41,10,22,42,11,23,42,12,24,43,12,24,
  44,13,25,44,14,25,45,14,26,46,15,27,46,16,27,47,17,28,47,18,28,48,19,29,49,
  19,30,49,20,30,50,21,31,51,21,32,51,22,32,52,23,33,53,23,34,53,24,34,54,25,
  35,55,25,36,55,26,36,56,27,37,57,27,38,57,27,39,57,27,41,57,27,42,57,27,43,
  57,27,44,57,27,45,57,27,46,57,27,47,57,27,49,57,27,50,57,27,51,57,27,52,57,
  27,53,57,27,55,57,27,56,57,27,57,57,27,58,57,27,58,57,26,58,57,25,58,57,24,
  58,56,23,58,55,22,58,54,20,58,53,19,58,51,18,58,50,17,58,50,16,58,49,15,58,
  48,14,58,47,13,58,46,12,58,45,11,58,44,11,58,44,10,58,43,10,58,42,9,57,41,
  8,57,40,8,56,39,7,56,38,6,55,37,5,55,35,4,54,33,4,54,31,2,32,32,32,63,63,63,
  63,63,63,63,63,63,63,63,63,48,48,48,63,63,63,63,63,63);

VAR
  MP,Scr      : ^lrgarr;
  rng         : array[0..320] of byte;
  dir,i,x,y   : integer;

function ncol(mc,n,dvd:integer):integer;
var loc:integer;
begin
  loc:=(mc+n-random(2*n)) div dvd; ncol:=loc;
  if loc>250 then ncol:=250; if loc<5 then ncol:=5
end;

procedure plasma(x1,y1,x2,y2:word);
var xn,yn,dxy,p1,p2,p3,p4:word;
begin
  if (x2-x1<2) and (y2-y1<2) then
   exit;
  p1:=mp^[WORD(256*y1+x1)]; p2:=mp^[WORD(256*y2+x1)]; p3:=mp^[WORD(256*y1+x2)];
  p4:=mp^[WORD(256*y2+x2)]; xn:=((x2+x1) shr 1) and $ffff; yn:=((y2+y1) shr 1) and $ffff;
  dxy:=5*(x2-x1+y2-y1) div 3;
  if mp^[WORD(256*y1+xn)]=0 then mp^[WORD(256*y1+xn)]:=ncol(p1+p3,dxy,2);
  if mp^[WORD(256*yn+x1)]=0 then mp^[WORD(256*yn+x1)]:=ncol(p1+p2,dxy,2);
  if mp^[WORD(256*yn+x2)]=0 then mp^[WORD(256*yn+x2)]:=ncol(p3+p4,dxy,2);
  if mp^[WORD(256*y2+xn)]=0 then mp^[WORD(256*y2+xn)]:=ncol(p2+p4,dxy,2);
  mp^[WORD(word(256*yn)+xn)]:=ncol(word(p1+p2+p3+p4),word(dxy),4);
  plasma(x1,y1,xn,yn); plasma(xn,y1,x2,yn);
  plasma(x1,yn,xn,y2); plasma(xn,yn,x2,y2);
end;

procedure draw(xp,yp,dir:integer);
var z,zobs,ix,iy,iy1,iyp,ixp,x,y,s,csf,snf,mpc,i,j:integer;
begin
  fillchar(rng,sizeof(rng),200); zobs:=100+mp^[WORD(256*yp+xp)];
  csf:=round(256*cos((dir)/180*pi)); snf:=round(256*sin((dir)/180*pi));
  fillchar(scr^,64000,0);
  for iy:=yp to yp+55 do
   begin
    iy1:=1+2*(iy-yp); s:=4+300 div iy1;
    for ix:=xp+yp-iy to xp-yp+iy do
     begin
      ixp:=xp+((ix-xp)*csf+(iy-yp)*snf) shr 8;
      iyp:=yp+((iy-yp)*csf-(ix-xp)*snf) shr 8;
      x:=160+360*(ix-xp) div iy1;
      if (x>=0) and (x+s<=318) then
       begin
        z:=mp^[WORD(iyp shl 8+ixp)]; mpc:=z shr 1;
        if z<47 then z:=46;  y:=100+(zobs-z)*30 div iy1;
        if (y<=199) and (y>=0) then
         for j:=x to x+s do
          begin
           for i:=y to rng[j] do
            scr^[WORD(320*i+j)]:=mpc;
           if y<rng[WORD(j)] then rng[WORD(j)]:=y
          end;
      end;
    end;
  end;
  {$IFDEF FPC}
   DosMemPut($A000,0,Scr^,64000);
  {$ELSE}
   Move(Scr^,mem[$A000:0],64000);
  {$ENDIF}
end;

VAR Reg : Registers;

begin
  writeln('creating landscape...');
  randomize; x:=0; y:=0; dir:=0; new(mp); fillchar(mp^,65535,0);
  new(scr); mp^[$0000]:=128; plasma(0,0,256,256);
  Reg.ax:=$13;  Intr($10,Reg);
{$IFDEF FPC}
  Outportb($3C8,0);
  for i:=1 to 384 do OutPortb($3c9,pal[i]);
{$ELSE}
  Port[$3C8] := 0;
  for i:=1 to 384 do Port[$3c9] := pal[i];
{$ENDIF}
  repeat
    dir:=dir mod 360;
    draw(x,y,dir);
    case readkey of
      #0:case readkey of
         #75:dec(dir,10);
         #77:inc(dir,10);
         #72:begin
              y:=y+round(5*cos((dir)/180*pi));
              x:=x+round(5*sin((dir)/180*pi));
             end;
         #80:begin
              y:=y-round(5*cos((dir)/180*pi));
              x:=x-round(5*sin((dir)/180*pi));
             end;
          end;
      #27: begin
            Reg.ax:=$3;
            Intr($10,Reg);
            halt
          end
    end
  until false;
end.
