unit xobjects;
{
    $Id$
    Copyright (c) 2000 by Daniel Mantione
     member of the Free Pascal development team

    This unit provides an extends the Tobject type with additional methods
    to check the type of an object. It should only be used within
    Turbo Pascal, the Free Pascal objects unit already contains this
    functionality.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
interface

{As TP does not store a link to the parent's VMT in the VMT, a function like
 is_object would be impossible.

 We use a very dirty trick to get it done; in an objects constructor the
 setparent procedure should be called, which stores the link to the parent
 into the DMT link. (!!!)}

uses    objects;

type    Pobject=^Tobject;
        Tobject=object(objects.Tobject)
            function is_object(typ:pointer):boolean;
            procedure setparent(typ:pointer);
        end;

implementation

type    vmt=record
            size,negsize:word;
            dmtlink:pointer;
        end;

function Tobject.is_object(typ:pointer):boolean;assembler;

asm
    les di,self
    mov bx,[es:di]  {Get vmt link.}
    jmp @a3
@a2:
    mov bx,[bx+4]   {Get dmt link, offset.}
    or bx,bx
    mov al,0
    jz @a1
@a3:
    cmp bx,typ.word {Compare with typ.}
    jne @a2
    mov al,1
@a1:
end;

procedure Tobject.setparent(typ:pointer);assembler;

asm
    les di,self
    mov bx,[es:di]  {Get vmt link.}
    mov ax,typ.word
    mov cx,typ+2.word
    mov [bx+4],ax
    mov [bx+6],cx
end;

end.