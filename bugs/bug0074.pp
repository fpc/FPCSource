type
  tmyobject = object
    constructor init;
    procedure callit; virtual;
    destructor done; virtual;
  end;


  constructor tmyobject.init;
  Begin
  end;

  destructor tmyobject.done;
  Begin
  end;

  procedure tmyobject.callit;
  Begin
   WriteLn('Hello...');
  end;

  var
   obj: tmyobject;
  Begin
    obj.init;
    obj.callit;
{    obj.done;}
  end.
