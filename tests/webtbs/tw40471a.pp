{ %norun }
{ %needlibrary }
{ %target=linux }
{ %opt=-gl -O- }

library tw40471a;

{$goto on}

// Returns the address of a label inside the library
function LabelAddress: CodePointer; cdecl;

label
  l1;

begin
l1:
  LabelAddress:=@l1;
end;


exports
  LabelAddress;

end.
