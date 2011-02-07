var
  b : boolean;
  b8 : ByteBool;
  b16 : WordBool;
  b32 : LongBool;
  b64 : QWordBool;
  s : string;

begin
  b:=false;
  str(b,s);
  if s<>'FALSE' then
    halt(1);

  b8:=false;
  str(b8,s);
  if s<>'FALSE' then
    halt(1);

  b16:=false;
  str(b16,s);
  if s<>'FALSE' then
    halt(1);

  b32:=false;
  str(b32,s);
  if s<>'FALSE' then
    halt(1);

  b64:=false;
  str(b64,s);
  if s<>'FALSE' then
    halt(1);

  writeln('ok');
end.

