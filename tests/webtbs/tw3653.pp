{ Source provided for Free Pascal Bug Report 3653 }
{ Submitted by "Marco" on  2005-02-14 }
{ e-mail:  }

{$ifdef fpc}
{$mode delphi}
{$else}
type ptrint = integer;
{$endif}

Type
     arrptr  = array[0..maxint div 4-1] of pointer;
     arrint  = array[0..maxint div 4-1] of integer;
     ppointer= ^arrptr;
     parrint = ^arrint;
     PLightSetElement = ^Pointer;
     TLightSet = Pointer;

     dlightsetiterator = record
                           next : PLightSetElement;
                         end;

function lightstartiter(p:TLightSet):DLightSetIterator;


begin
     result.next:=@ppointer(ptrint(p) and not 3)[0];     // on size.
end;

begin
end.
