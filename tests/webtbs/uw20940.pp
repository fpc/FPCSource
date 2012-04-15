unit uw20940; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  Storage=class
  type
    Folders=class
    Const
      FLAG_REFRESH = 1;
      FLAG_DELETE  = 2;
    Type
      TItem=record
        ID:Int64;
        Path:string;
      end;
      PItem=^TItem;
      TItems=array of PItem;
      PItems=^TItems;

      TItemsEvent=procedure(Var Items:TItems) of Object;
    end;
    Files=class
    Type
      PItem=^TItem;
      TItem=record
        Name :string;
        Created:TDateTime;
      end;
      TItems=Array of PItem;
      TItemsEvent=procedure(Var Items:TItems) of Object;
    end;
    Resources=class
    Type
      PItem=^TItem;
      TItem=record
        Name :string;
        Description:string;
        Manifest:string;
      end;
      TItems=Array of PItem;
      TItemsEvent=procedure(Var Items:TItems) of Object;
    end;
  end;

implementation

end.

