uses
   dpmiexcp,regexpr;

var
   r : tregexprengine;
   b : array[0..100] of char;
   index,len : longint;

procedure do_error(i : longint);

  begin
     writeln('error near ',i,' index: ',index,' len: ',len);
     halt(1);
  end;

begin
   writeln('*** Testing unit regexpr ***');

   r:=GenerateRegExprEngine('[A-Z]');
   if not(RegExprPos(r,'234578923457823659A38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1000);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z]*');
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
   { is this result correct ??? }
     (index<>0) or (len<>0) then
     do_error(1002);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z]+');
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1003);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z][A-Z]*');
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1004);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z][A-Z]?');
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>2) then
     do_error(1005);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('^\d+');
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1006);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('(nofoo|foo)1234');
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>8) or (len<>7) then
     do_error(1007);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('(nofoo|foo)1234');
   if not(RegExprPos(r,'1234   nofoo1234XXXX',index,len)) or
     (index<>8) or (len<>9) then
     do_error(1008);
   DestroyregExprEngine(r);

   writeln('*** Testing unit regexpr was successful ***');
end.