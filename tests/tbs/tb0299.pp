{$mode objfpc}
type
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

const
   constclassref1 : tclass = tobject;
   constclassref2 : tclass = nil;
   constclassref3 : tclass = tobject;

begin
   { simple test }
   classref:=classref;
   { more difficult }
   classref:=myclassref;
   classref:=tobject;
   myclassref:=tmyobject;

   classref:=getanchestor(myclassref);
   if (upcase(constclassref1.classname)<>'TOBJECT') or
     (constclassref2<>nil) or
     (upcase(myclassref.classname)<>'TMYOBJECT') or
     (upcase(classref.classname)<>'TOBJECT') then
     begin
        writeln('Error');
        halt(1);
     end;
end.
