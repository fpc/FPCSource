{$MODE OBJFPC}
program testreg1;
uses
   regexpr;

var
   r         : tregexprengine;
   index,len : longint;
   S         : String;

procedure do_error(i : longint);

  begin
     writeln('error near ',i,' index: ',index,' len: ',len);
     halt(1);
  end;

begin
   writeln('*** Testing unit regexpr ***');

   { basic tests }

   r:=GenerateRegExprEngine('[A-Z]',[]);
   if not(RegExprPos(r,'234578923457823659GHJK38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1000);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z]*',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
   { is this result correct ??? }
     (index<>0) or (len<>0) then
     do_error(1002);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z]+',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1003);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z][A-Z]*',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1004);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z][A-Z]?',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>2) then
     do_error(1005);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[^\d]+',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>5) then
     do_error(1006);
   DestroyregExprEngine(r);

   { test chaining }

   r:=GenerateRegExprEngine('[A-Z][A-Z]?[A-Z]',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>3) then
     do_error(1007);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z][A-Z]*[0-9]',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>6) then
     do_error(1008);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('[A-Z]+[0-9]',[]);
   if not(RegExprPos(r,'234578923457823659ARTZU38',index,len)) or
     (index<>18) or (len<>6) then
     do_error(1009);
   DestroyregExprEngine(r);

   { case insensitive: }

   r:=GenerateRegExprEngine('[A-Z]',[ref_caseinsensitive]);
   if not(RegExprPos(r,'234578923457823659a38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1100);
   DestroyregExprEngine(r);

   { case insensitive: }
   r:=GenerateRegExprEngine('[a-z]',[ref_caseinsensitive]);
   if not(RegExprPos(r,'234578923457823659A38',index,len)) or
     (index<>18) or (len<>1) then
     do_error(1101);
   DestroyregExprEngine(r);

   { with parenthsis }
   r:=GenerateRegExprEngine('(foo)1234',[]);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1200);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('(((foo)))1234',[]);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1201);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('(foo)(1234)',[]);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1202);
   DestroyregExprEngine(r);

   { test real backtracking }

(*   r:=GenerateRegExprEngine('nofoo|foo',[]);
   if not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>3) then
     do_error(1300);
   DestroyregExprEngine(r);*)

  r := GenerateRegExprEngine('abc\(123\)$',[]);
  if not (RegExprPos(r,'1234 abc(123)', index, len)) or
         (index <> 5) or (len <> 8) then
    do_error (1400);
  DestroyregExprEngine(r);

  r := GenerateRegExprEngine('^\t$',[ref_singleline]);
  if not (RegExprPos(r,#9, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1401);
  DestroyregExprEngine(r);

  r := GenerateRegExprEngine('^\n$',[ref_singleline]);
  if not (RegExprPos(r,#10, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1402);
  DestroyregExprEngine(r);

  r := GenerateRegExprEngine('^\f$',[ref_singleline]);
  if not (RegExprPos(r,#12, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1403);
  DestroyregExprEngine(r);

  r := GenerateRegExprEngine('^\r$',[ref_singleline]);
  if not (RegExprPos(r,#13, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1404);
  DestroyregExprEngine(r);

  r := GenerateRegExprEngine('^\a$',[ref_singleline]);
  if not (RegExprPos(r,#7, index, len)) or
         (index <> 0) or (len <> 1) then
    do_error (1405);
  DestroyregExprEngine(r);

   s := '^Hello World \.  [a-z] \D { } |() ?a*.*\\ 1 $';
   writeln ('Before Escaping: ', s);
   writeln ('Afther Escaping: ', RegExprEscapeStr(s));

   {
   r:=GenerateRegExprEngine('(nofoo|foo)1234',[]);
   if not(RegExprPos(r,'1234   nofoo1234XXXX',index,len)) or
     (index<>8) or (len<>9) then
     do_error(1008);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('(nofoo|foo|anotherfoo)1234',[]);
   if not(RegExprPos(r,'1234   nofoo1234XXXX',index,len)) or
     (index<>8) or (len<>9) then
     do_error(1009);
   DestroyregExprEngine(r);

   r:=GenerateRegExprEngine('nofoo1234|foo1234',[]);
   if (r.data=nil) or not(RegExprPos(r,'1234   foo1234XXXX',index,len)) or
     (index<>7) or (len<>7) then
     do_error(1010);
   DestroyregExprEngine(r);
   }
   writeln('*** Testing unit regexpr was successful ***');
end.
