{ %version=1.1 }
program dumpprops;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

uses
  Classes, TypInfo;

type
  TBaseTest = class(TPersistent)
  private
    FCaption: String;
    FNext: Integer;
    FNext6: Integer;
  protected
  public
    property Caption: String read FCaption write FCaption;
  published
    property Next: Integer read FNext write FNext;
    property Next6: Integer read FNext6 write FNext6;
  end;

  TTest = class(TBaseTest)
  private
    FNext2: Integer;
  protected
  public
  published
    property Caption;
    property Next2: Integer read FNext2 write FNext2;
  end;

var
  p : PPropInfo;
  t : TTest;
begin
  t:=TTest.Create;
  p:=GetPropInfo(t,'Next');
  if (p<>nil) and
     (p^.name='Next') then
   writeln('Success')
  else
   begin
     writeln('ERROR! Got: ',p^.name);
     halt(1);
   end;
end.
