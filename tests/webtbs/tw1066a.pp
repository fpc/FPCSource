{ %CPU=i386 }
{ Source provided for Free Pascal Bug Report 1066 }
{ Submitted by "Fernando Oscar Schmitt" on  2000-07-24 }
{ e-mail: pulp@cpovo.net }

var
 somevar:longint;

{$asmmode intel}
{$inline on}

procedure putpixel(x,y,color:longint);assembler;inline;
asm
mov edi,x
mov eax,y
cmp edi,0
jl @@putpixelend
cmp eax,0
jl @@putpixelend
cmp edi,1023
jg @@putpixelend
cmp eax,767
jg @@putpixelend
shl eax,12
mov ebx,color
add eax,somevar
mov [eax+edi*4],ebx
@@putpixelend:
end ['eax','ebx','edi'];


procedure pixelrow(y,x1,x2,color:longint);assembler;inline;
asm
mov edi,x1
mov ecx,x2
mov eax,y
cmp edi,ecx
jle @@pixelrowdirok
xchg edi,ecx
@@pixelrowdirok:
cmp eax,0
jl @@endpixelrow
cmp eax,767
jg @@endpixelrow
cmp ecx,0
jl @@endpixelrow
cmp edi,1023
jg @@endpixelrow
cmp edi,0
jge @@pixelrowx1ok
mov edi,0
@@pixelrowx1ok:
cmp ecx,1023
jle @@pixelrowx2ok
mov ecx,1023
@@pixelrowx2ok:
sub ecx,edi
shl eax,12
inc ecx
add eax,somevar
cld
lea edi,[eax+4*edi]
mov eax,color
rep stosd
@@endpixelrow:
end ['eax','ecx','edi'];


function str(w:word):string;
var tmp:string;
begin
system.str(w,tmp);
str:=tmp;
end;

function str(l:longint):string;
var tmp:string;
begin
system.str(l,tmp);
str:=tmp;
end;


procedure circle(x0,y0,r,color:longint);
var x,y:longint;
begin
for x:=0 to trunc(r*(sqrt(2)/2))+1 do
 begin
 y:=round(sqrt(r*r-x*x));
 putpixel(x0+x,y0+y,color);
 putpixel(x0-x,y0+y,color);
 putpixel(x0+x,y0-y,color);
 putpixel(x0-x,y0-y,color);
 putpixel(x0+y,y0+x,color);
 putpixel(x0-y,y0+x,color);
 putpixel(x0+y,y0-x,color);
 putpixel(x0-y,y0-x,color);
 end;
end;


procedure circlefill(x0,y0,r,color:longint);
var x,y:longint;
begin
for x:=0 to trunc(r*(sqrt(2)/2))+1 do
 begin
 y:=round(sqrt(r*r-x*x));
 pixelrow(y0+y,x0-x,x0+x,color);
 pixelrow(y0-y,x0-x,x0+x,color);
 pixelrow(y0+x,x0-y,x0+y,color);
 pixelrow(y0-x,x0-y,x0+y,color);
 end;
end;


begin

end.
