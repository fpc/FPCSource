Unit uw6767;

{$mode objfpc}{$H+}

Interface

Uses 
Classes,Sysutils;


Type 
  TCheckConnThread = Class(TThread)
    Private 
    Protected
    Procedure Execute;override;
    Public 
    Constructor Create(CreateSuspended : boolean);
  End;



  Implementation


constructor TCheckConnThread.Create(CreateSuspended : boolean);
Begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
End;




Procedure TCheckConnThread.Execute;

Var 
  i : Integer;
Begin
  While (Not Terminated) Do
    Begin
      For i:=1 To 100 Do
        Begin
          If Terminated Then break;
        End;
    End;
End;

End.
