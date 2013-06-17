{ Source provided for Free Pascal Bug Report 2727 }
{ Submitted by "marco (the gory bugs department)" on  2003-10-08 }
{ e-mail:  }

{$mode delphi}


type
  IPersistenceCapable = interface
    ['{A7F3DA50-93BF-4EAF-B40C-8F5020E5D890}']
    function GetSelf: TObject;
    property Self: TObject read GetSelf;
  end;

  TPersistenceCapable = class(TInterfacedObject, IPersistenceCapable)
   function GetSelf: TObject;
   function nonsense:boolean;
  end;

function TPersistenceCapable.GetSelf: TObject;
begin
  result:=nil;
end;

function TPersistenceCapable.nonsense:boolean;
{this works fine if it isn't a method....}

var
  lSource: IPersistenceCapable;

begin
     result:=(lSource.Self is TPersistenceCapable)
end;


begin
end.
