{ Source provided for Free Pascal Bug Report 3360 }
{ Submitted by "Thomas Schatzl" on  2004-10-17 }
{ e-mail:  }
{$R-}
{$Q-}

type
        tdef = record
          f1 : longint;
          f2 : longint;
        end;

        fileof = file of byte;

        enum = (a,b,c,d=10);

        tset1 = set of enum;
        tset2 = set of byte;

        ttdef = type tdef;
        tfile = type file;
        ttext = type text;
        tfileof = type fileof;
        tvariant = type variant;
        //!! tinterfacedobject2 = type tinterfacedobject;
        tenum = type enum;
        ttset1 = type tset1;
        ttset2 = type tset2;

var
  e1 : enum;
  e2 : tenum;
  def1 : tdef;
  def2 : ttdef;

begin
  { enums }
  e1:=a;
  e2:=a;
  e1:=d;
  e2:=d;

  { sets }
  if ord(e1)<>ord(e2) then
    halt(1);
  if sizeof(tset1)<>sizeof(ttset1) then
    halt(2);
  if sizeof(tset2)<>sizeof(ttset2) then
    halt(3);

  { records }
  def1.f1:=$deadbeef;
  def1.f2:=$d0d0d0d0;
  if sizeof(def1)<>sizeof(def2) then
    halt(4);

  move(def1,def2,sizeof(def1));

  if (def1.f1<>def2.f1) or
    (def1.f2<>def2.f2) then
    halt(5);

  writeln('ok');
end.
