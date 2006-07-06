{ %skiptarget=wince }

unit {vidutil}tu2002;
 Interface
 uses
   video;
 Procedure TextOut(X,Y : Word;Const S :
 String);
 Implementation

 Procedure TextOut(X,Y : Word;Const S :
 String);
 Var
  W,P,I,M : Word;
 begin
   P:=((X-1)+(Y-1)*ScreenWidth);
   M:=Length(S);
   If P+M>ScreenWidth*ScreenHeight then
     M:=ScreenWidth*ScreenHeight-P;
   For I:=1 to M do
     VideoBuf^[P+I-1]:=Ord(S[i])+($07 shl 8);
 end;

 end.
