{$ifdef FPC}
Uses Math;

{$else not FPC}
function degtorad(deg : extended) : extended;

  begin
     degtorad:=deg*(pi/180.0);
  end;

function radtodeg(rad : extended) : extended;

  begin
     radtodeg:=rad*(180.0/pi);
  end;

 function ArcSin(x : extended) : extended;
   begin
     if abs(x)=1.0 then
       arcsin:=Pi/2
     else
       arcsin:=ArcTan(x/sqrt(1-x*x));
   end;
 function ArcTan2(x,y : extended) : extended;
   begin
     ArcTan2:=ArcTan(x/y);
   end;
{$endif not FPC}

Var
   I : Integer;
   RI,RRI,R0 : extended;

Begin
   For I := -179 To 179 Do
   Begin
      RI:=I;
      WriteLn( RadToDeg(ArcSin(Sin(DegToRad(RI)))):3:18);
   End;
   For I := -89 To 89 Do
   Begin
      RI:=I;
      RRI:=RadToDeg(ArcSin( Sin(DegToRad(RI))));
      WriteLn(RI:3:18,' ',RRI:3:18);
      If RI<>RRI then
        begin
          Writeln('Not exact ',RRI-RI:3:18);
          if I<>0 then
            begin
              Writeln('Percentage error = ',Abs(RRI -RI) *100 / I:3:18);
              if abs((RRI -RI) *100 / I)>0.0001 then
                Begin
                  Writeln('Error too big ');
                  Halt(1);
                end;
            end;
        end;
   End;
   RI:=3;
   RRI:=1;
   R0:=1;
   Writeln(  ArcTan2(ArcTan2(1,1),R0):3:18 , ' should be 0.66577375...');
   if ArcTan2(ArcTan2(1,1),R0)<>ArcTan(ArcTan(1)/R0) then
     begin
       Writeln('There is still a bug in ArcTan2 !');
       Halt(1);
     end;
End.
