{ Old file: tbs0312.pp }
{ Again the problem of local procs inside methods }

{ Program that showss a problem if
  Self is not reloaded in %esi register
  at entry in local procedure inside method }

uses
  objects;

type
{$ifndef FPC}
  sw_integer = integer;
{$endif not FPC}

  PMYObj = ^TMyObj;

  TMyObj = Object(TObject)
    x : longint;
    Constructor Init(ax : longint);
    procedure display;virtual;
    end;

  PMYObj2 = ^TMyObj2;

  TMyObj2 = Object(TMyObj)
    y : longint;
    Constructor Init(ax,ay : longint);
    procedure display;virtual;
    end;

  PMyCollection = ^TMyCollection;

  TMyCollection = Object(TCollection)
    function At(I : sw_integer) : PMyObj;
    procedure DummyThatShouldNotBeCalled;virtual;
    end;

  { TMy is also a TCollection so that
    ShowMy and DummyThatShouldNotBeCalled are at same position in VMT }
  TMy = Object(TCollection)
    Col : PMyCollection;
    MyObj : PMyObj;
    ShowMyCalled : boolean;
    constructor Init;
    destructor Done;virtual;
    procedure ShowAll;
    procedure AddMyObj(x : longint);
    procedure AddMyObj2(x,y : longint);
    procedure ShowMy;virtual;
    end;

  Constructor TMyObj.Init(ax : longint);
    begin
      Inherited Init;
      x:=ax;
    end;

  Procedure TMyObj.Display;
    begin
      Writeln('x = ',x);
    end;

  Constructor TMyObj2.Init(ax,ay : longint);
    begin
      Inherited Init(ax);
      y:=ay;
    end;

  Procedure TMyObj2.Display;
    begin
      Writeln('x = ',x,' y = ',y);
    end;

  Function TMyCollection.At(I : sw_integer) : PMyObj;
    begin
      At:=Inherited At(I);
    end;

  Procedure TMyCollection.DummyThatShouldNotBeCalled;
    begin
      Writeln('This method should never be called');
      Abstract;
    end;

  Constructor TMy.Init;

    begin
      New(Col,Init(5,5));
      MyObj:=nil;
      ShowMyCalled:=false;
    end;

  Destructor TMy.Done;
    begin
      Dispose(Col,Done);
      Inherited Done;
    end;

  Procedure TMy.ShowAll;

      procedure ShowIt(P : pointer);{$ifdef TP}far;{$endif}
        begin
          ShowMy;
          PMyObj(P)^.Display;
        end;
    begin
      Col^.ForEach(@ShowIt);
    end;

  Procedure TMy.ShowMy;
    begin
      if assigned(MyObj) then
        MyObj^.Display;
      ShowMyCalled:=true;
    end;

  Procedure TMy.AddMyObj(x : longint);

    begin
      MyObj:=New(PMyObj,Init(x));
      Col^.Insert(MyObj);
    end;

  Procedure TMy.AddMyObj2(x,y : longint);
    begin
      MyObj:=New(PMyObj2,Init(x,y));
      Col^.Insert(MyObj);
    end;

var
   My : TMy;
begin
   My.Init;
   My.AddMyObj(5);
   My.AddMyObj2(4,3);
   My.AddMyObj(43);
   { MyObj field is now a PMyObj with value 43 }
   My.ShowAll;
   If not My.ShowMyCalled then
     begin
       Writeln('ShowAll does not work correctly');
       Halt(1);
     end;
   My.Done;

end.
