{ Source provided for Free Pascal Bug Report 2233 }
{ Submitted by "Sergey Kosarevsky" on  2002-11-19 }
{ e-mail: netsurfer@au.ru }
Type pGUIView=^tGUIView;
     tGUIView=Object
        Constructor Init;
        Procedure RenderView;Virtual;Abstract;
     End;
Type tGUIWindow=Object(tGUIView)
        Constructor Init;
        Procedure RenderView;Virtual;
     End;
Type tGUICommonControl=Object(tGUIWindow)
        Constructor Init;
        Constructor Init(Param1:Longint);
     End;
Type pGUIRadioGroup=^tGUIRadioGroup;
     tGUIRadioGroup=Object(tGUICommonControl)
        Constructor Init;
        Constructor Init(Param1:Longint);
        Procedure RenderView;Virtual;
     End;
var
  err : boolean;

Constructor tGUIView.Init;
Begin
End;
Constructor tGUIWindow.Init;
Begin
   Inherited Init;
End;
Procedure tGUIWindow.RenderView;
Begin
   WriteLn('tGUIWindow.RenderView()');
End;
Constructor tGUICommonControl.Init;
Begin
   Init(0);
End;
Constructor tGUICommonControl.Init(Param1:Longint);
Begin
   Inherited Init;
End;
Constructor tGUIRadioGroup.Init;
Begin
   Inherited Init;
End;
Constructor tGUIRadioGroup.Init(Param1:Longint);
Begin
   Inherited Init(Param1);
End;
Procedure tGUIRadioGroup.RenderView;
Begin
   Inherited RenderView;
   WriteLn('tGUIRadioGroup.RenderView()');
   err:=false;
End;
Var View:pGUIView;
Begin
   err:=true;
   View:=New(pGUIRadioGroup,Init);
   View^.RenderView;
   if err then
    begin
      writeln('ERROR!');
      halt(1);
    end;
End.
