{ %OPT=-O2}
function forms(s: string; len: word): string;
begin
  str(len,forms);
  forms := s + ', ' + forms;
end;

procedure wrt2(s: string);
begin
  if s <> 'e 123, 4' then
    begin
      writeln('bug!');
      halt(1);
    end;
end;

type
  pstring = ^string;
  ta = array[0..254] of pstring;
  tb = array[0..254] of byte;

procedure t(var sel: ta; var selhigh: tb);
var
  ml, i: byte;
begin
  i := 5;
  ml := 8;
  new(sel[i]);
  sel[i]^ := 'testje 123';
  selhigh[i] := 5;
  wrt2(forms(copy(sel[i]^,selhigh[i]+1,255),ml-selhigh[i]+1));
end;

var
  a: ta;
  b: tb;

begin
  t(a,b);
end.
