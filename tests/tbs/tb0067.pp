{ Old file: tbs0074.pp }
{  shows MAJOR bugs when trying to compile valid code    OK 0.99.1 (PM/CEC) }

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
