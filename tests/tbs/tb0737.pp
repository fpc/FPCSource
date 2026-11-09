{ %OPT=-O3 -vh -Seh }
{ the destination of an assignment passed as hidden var parameter (e.g. as function result
  of a helper) is only written, so it is not a use of the variable: no hints about
  uninitialized variables may be issued here }
program tb0737;

{$mode objfpc}{$h+}

type
  tbyteset = set of byte;

const
  cs : tbyteset = [1,50,220];

var
  gs : string = '12223445';
  gset : tbyteset = [55];

procedure p1;
  var
    s : string = 'abcdef';
  begin
    s:=string(pchar(@s[3]));
    if s<>'cdef' then
      halt(4);
  end;

procedure p2;
  var
    st : tbyteset = [1,2];
  begin
    st:=st+gset;
    if not(55 in st) or not(2 in st) then
      halt(5);
  end;

begin
  gs:=string(pchar(@gs[3]));
  if gs<>'223445' then
    halt(1);
  gs:=copy(gs,2);
  if gs<>'23445' then
    halt(2);
  gset:=gset+cs;
  if not(1 in gset) or not(55 in gset) then
    halt(3);
  p1;
  p2;
  writeln('ok');
end.
