{ %OPT=-O3 }

{ Test triggers incorrect AddMov2Mov optimisation
  (second register not checked for modification) }
program tw39918;

type

  ObjectA = object
    Index: Cardinal;
  end;

  ObjectB = object
    ObjectAArray: array of ObjectA;
    TestString: String; //Needed for crash
  end;

  ObjectC = object
    TestValue: Uint32;
  end;

var

  ObjectBArray: array of ObjectB;
  ObjectCArray: array of ObjectC;
  BIndex: Integer = 0;

begin
  //Init variables
  setlength(ObjectBArray, 1);
  setlength(ObjectBArray[0].ObjectAArray, 1);
  SetLength(ObjectCArray, 1);
  ObjectCArray[0].TestValue := 0;
  ObjectBArray[0].ObjectAArray[0].Index := 0;
  ObjectBArray[0].TestString := 'Needed';

  //Works
  writeln(ObjectCArray[ObjectBArray[BIndex].ObjectAArray[0].Index].TestValue);

  //Crash
  with ObjectBArray[BIndex], ObjectCArray[ObjectAArray[0].Index] do
  begin
    writeln(TestValue);
  end;
  WriteLn('ok');
end.
