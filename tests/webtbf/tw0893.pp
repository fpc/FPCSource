{ %fail }

{ %CPU=i386 }
{$asmmode intel}
type
    BugObject = object
       Fld: word;
       procedure WontCompile;
    end;

procedure BugObject.WontCompile;
begin
  asm
    xor ax, ax
    mov fld, ax
  end;
end;

begin
end.
