{ %VERSION=1.1 }
{ %FAIL }
{ %OPT=-Sew -vw }
{$MODE OBJFPC}
type
  tmyclass = class
   procedure myabstract; virtual; abstract;
  end;

  tmyclass2 = class(tmyclass)
  end;

  tmyclassnode = class of tmyclass;

var
 cla : tmyclass2;
 cla1 : tmyclass;
 classnode : tmyclassnode;
Begin
 cla := tmyclass2.create;
 classnode := tmyclass2;
 cla1 := classnode.create;
end.
