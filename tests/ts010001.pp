type
   tclass = class of tobject;

   tmyclass = class of tmyobject;

   tmyobject = class
   end;

{ only a stupid test routine }
function getanchestor(c : tclass) : tclass;

  var
     l : longint;

  begin
     getanchestor:=tobject;
     l:=l+1;
  end;

var
   classref : tclass;
   myclassref : tmyclass;

begin
   { simple test }
   classref:=classref;
   { more difficult }
   classref:=myclassref;
   classref:=tobject;

   classref:=getanchestor(myclassref);
end.