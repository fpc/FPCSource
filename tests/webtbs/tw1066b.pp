{ %CPU=i386 }
{ %OPT=-Cg- }
{----------------cut here----------------}

{$asmmode intel}
{$inline on}

var
 somevar:longint;


procedure wastememory(x,y,color:longint);assembler;inline;
asm
mov edi,x
mov eax,y
cmp edi,0
jl @@wastememoryend
cmp eax,0
jl @@wastememoryend
cmp edi,1023
jg @@wastememoryend
cmp eax,767
jg @@wastememoryend
shl eax,12
mov ebx,color
add eax,somevar
mov [eax+edi*4],ebx
@@wastememoryend:
end ['eax','ebx','edi'];


procedure wastememory2(y,x1,x2,color:longint);assembler;inline;
asm
mov edi,x1
mov ecx,x2
mov eax,y
cmp edi,ecx
jle @@wastememory2dirok
xchg edi,ecx
@@wastememory2dirok:
cmp eax,0
jl @@endwastememory2
cmp eax,767
jg @@endwastememory2
cmp ecx,0
jl @@endwastememory2
cmp edi,1023
jg @@endwastememory2
cmp edi,0
jge @@wastememory2x1ok
mov edi,0
@@wastememory2x1ok:
cmp ecx,1023
jle @@wastememory2x2ok
mov ecx,1023
@@wastememory2x2ok:
sub ecx,edi
shl eax,12
inc ecx
add eax,somevar
cld
lea edi,[eax+4*edi]
mov eax,color
rep stosd
@@endwastememory2:
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


procedure testcompiler(x0,y0,r,color:longint);
var x,y:longint;
begin
for x:=0 to trunc(r*(sqrt(2)/2))+1 do
 begin
 y:=round(sqrt(r*r-x*x));
 wastememory(x0+x,y0+y,color);
 wastememory(x0-x,y0+y,color);
 wastememory(x0+x,y0-y,color);
 wastememory(x0-x,y0-y,color);
 wastememory(x0+y,y0+x,color);
 wastememory(x0-y,y0+x,color);
 wastememory(x0+y,y0-x,color);
 wastememory(x0-y,y0-x,color);
 end;
end;


procedure testcompiler2(x0,y0,r,color:longint);
var x,y:longint;
begin
for x:=0 to trunc(r*(sqrt(2)/2))+1 do
 begin
 y:=round(sqrt(r*r-x*x));
 wastememory2(y0+y,x0-x,x0+x,color);
 wastememory2(y0-y,x0-x,x0+x,color);
 wastememory2(y0+x,x0-y,x0+y,color);
 wastememory2(y0-x,x0-y,x0+y,color);
 end;
end;


begin

end.
