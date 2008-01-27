program RPNThing;
{
    $ id:                                                       $
    Copyright (c) 2000 by Marco van de Voort(marco@freepascal.org)
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright. (LGPL)

    Much too simplistic program to test some basic features of Symbolic unit.
    It is the very rough skeleton of a symbolic RPN calculator like a HP48.
    Since there are no exception conditions in the parser or evaluator,
     please enter valid expressions.
    Don't use 5E6 notation, it is not implemented yet. You can enter
     symbolic expressions using x, integer constants and half the math
     unit's function.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$ifdef FPC}
 {$Mode ObjFpc}
{$endif}

Uses Symbolic,Crt;

function GetKey:char;

begin
 repeat
  while keypressed DO ;
  result:=ReadKey;
  if result=#0 then      {Make sure control codes are skipped apropiately}
   begin
    result:=readKey;
    result:=#0;
   end;
 until result IN ['X','x','O','o','q','Q',' ','+','-','*','/','^','e','E','d','D','T','t'];
end;


VAR Stack    : array[0..100] of TExpression;
    I,StackPtr : Integer;
    InputC   : Char;
    S        : String;
    Flag     : Boolean;

Procedure Redraw;

var I : Integer;

begin
 for I:=1 to 20 DO
  begin
   GotoXY(1,I);
   Write(' ':79);
   GotoXY(1,I);
   IF (StackPtr>(20-I)) then
    begin
     IF NOT Assigned(stack[20-I]) then
      begin
       gotoXY(1,1); write(' ':50);
       gotoxy(1,1); writeln(I,' ',20-I);
       Writeln(stackptr);
       HALT;
     end;
     Writeln(stack[StackPtr-(21-I)].InfixExpr);
    end
   else
    write('-');
  end;
 GotoXY(1,21);
 Write(' ':80);
end;

begin
 Writeln(' + - / * ^    : perform the RPN operation');
 Writeln(' [space],'#39'    : get a "prompt" to input a number or infix expression');
 Writeln('   E,e        : Try to simplify/evaluate the expression. ');
 Writeln('                For now this is restricted to constant values only');
 Writeln('   D,d        : Drop 1 value from the stack');
 Writeln('   Q,q        : By pressing this key you agree this program is great');
 Writeln('   O,o        : Derive the expression with respect to X');
 Writeln('   T,t        : Taylor polynomal. Also with respect to X, and to 2nd ');
 writeln('                 stacklevel degree');
 Writeln;
 Writeln('Press enter to start calculating');
 ReadLn;
 ClrScr;
 StackPtr:=0;
 repeat
  InputC:=GetKey;
  Case InputC OF
   '+','-','*','/','^'   :   if stackPtr>1 then
                              begin
                               Dec(StackPtr);
                               case InputC of {Double case is ugly but short}
                                 '+' : Stack[StackPtr-1].AddTo(Stack[StackPtr]);
                                 '-' : Stack[StackPtr-1].SubFrom(Stack[StackPtr]);
                                 '*' : Stack[StackPtr-1].Times(Stack[StackPtr]);
                                 '/' : Stack[StackPtr-1].DivBy(Stack[StackPtr]);
                                 '^' : Stack[StackPtr-1].RaiseTo(Stack[StackPtr]);
                                end;
                               Stack[StackPtr].free;
                               Redraw;
                              end;
   'E','e' :  If Stackptr>0 then
               begin
                Stack[StackPtr-1].SimplifyConstants;
                Redraw;
              end;
   'T','t' :  If StackPtr>1 then        {Stackptr-1=function.  Stackptr-2=degree
                                           x is assumed, and x0 is substed}
               begin
                Flag:=True;
                Try
                  i:=Stack[StackPtr-2].ValueAsInteger;
                  except
                    on ENotInt do
                     begin
                      GotoXY(1,1);
                      WritelN('This constant doesn''t evaluate to an integer');
                      Flag:=False;
                     end;
                 end;
                 If I<0 then
                  begin
                   GotoXY(1,1);
                   WritelN('I never heard of negative terms in a Taylor polynomal');
                  end
                 else
                If Flag then
                 begin
                  Stack[StackPtr-2].Free;
                  Stack[StackPtr-2]:=Stack[StackPtr-1];
                  Stack[StackPtr-1]:=Stack[StackPtr-2].Taylor(I,'X','0.0');
                  Redraw;
                 end;
               end;
   'O','o' :  if StackPtr>0 then
               begin
                Stack[StackPtr]:=Stack[StackPtr-1].Derive('X');
                Inc(StackPtr);
                Redraw;
               end;
   'D','d' :  If StackPtr>0 Then
                begin
                 Stack[StackPtr-1].free;
                 Dec(StackPtr);
                 Redraw;
                end;
   ' ',#39   : If Stackptr<100 then
                begin
                 GotoXY(1,1); Writeln(' ':60);
                 gotoxy(1,1); write('Enter expr. : '); readln(s);
                 s:=upcase(S);
                 stack[StackPtr]:=TExpression.Create(S);
                 Stack[StackPtr].Simplificationlevel:=2; {Don't add reals to integer. Only evaluates
                                                        (integer op integer) and (real op real) and
                                                        function(real)}
                 Inc(Stackptr);
                 Redraw;
                end;
   'X','x'  : begin
               ClrScr;
               Writeln(stdout,stack[StackPtr-1].InfixExpr);
               Writeln;
               Writeln(stdout,stack[StackPtr-1].RPNExpr);
               inputC:='q';
              end;
            end;
   until (InputC IN ['q','Q']);

If StackPtr>0 THEN
 For I:=0 To StackPtr-1 Do
  Stack[I].Free;
end.
