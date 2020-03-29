unit ub0635;

{$mode objfpc}

INTERFACE

type
  TNode         = Class;
  TSortableNode = Class;
  TAdminNode = Class;
    TGuardNode    = Class;

  TBaseSystem = Class
  strict private
    fEvList       : TSortableNode;
    fGuard        : TGuardNode;
  end;

  TNode = Class
      fOwner     : TBaseSystem;
    end;

  TSortableNode = Class (TNode)
    end;

  TAnalyser = Class (TNode)
    end;

  TStat  = Class (TAnalyser)
    end;


  TAdminNode = Class (TSortableNode)
    end;

  TGuardNode = Class (TAdminNode)
    end;


IMPLEMENTATION

begin
end.

