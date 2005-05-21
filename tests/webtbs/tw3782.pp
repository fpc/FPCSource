{ %cpu=i386 }
{ %opt=-Ogr }

{$mode delphi}

type
  my_rec = record
    my_field: string;
    end;

var
 my_fyl: my_rec;
 my_str: array[0..99] of char;

function my_func(var f: my_rec):PChar;
begin
  my_func:=@my_str[1];
end;

procedure my_proc(var f: my_rec; const s: String);
var i, len: Integer; wascur: PChar;
begin
  len := length(s);
  wascur := my_func(f);
  for i:=0 to len-1 do
   wascur[i]:=s[i+1];
end;

begin
my_proc(my_fyl,'xxx');
my_str[0]:='y';
writeln(my_str);
end.
{
It's a spilling bug:
# [23] wascur[i]:=s[i+1];
         movl    -8(%ebp),%ireg23d
         # Register %ireg24d allocated
         movl    %ireg16d,%ireg24d
         incl    %ireg24d
         # Register %ireg24d released
         # Register %ireg25d allocated
         movl    %ireg24d,%ireg25d
         # Register %ireg26d allocated
        movl    -12(%ebp),%ireg26d
         # Register %ireg26d released
         # Register %edi allocated
         leal    (%ireg26d,%ireg16d,1),%edi
         # Register %ireg23d,%ireg25d released

becomes

# [23] wascur[i]:=s[i+1];
         movl    -8(%ebp),%edx
         # Register %ecx allocated
         movl    -20(%ebp),%ecx
         incl    %ecx
         # Register %eax allocated
        movl    -12(%ebp),%eax
         # Register %eax released
         # Register %edi,%eax allocated
        movl    -20(%ebp),%eax
         # Register %eax released
         leal    (%eax,%eax,1),%edi
         # Register %edx,%ecx released


ireg16d is spilled to -20(%ebp), so before the leal it must be loaded
into a register. The spilling code picks eax, but that one is already
used at that moment (by ireg26d).
}
