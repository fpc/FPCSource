Type TParent = Object
       Procedure SomeProc;
       end;

     TChild = Object(TParent)
       Procedure SomeProc; virtual;
       end;


       Procedure TParent.someproc;
       Begin
       end;


      procedure TChild.Someproc;
      Begin
      end;



Begin
end.