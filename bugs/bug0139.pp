unit bug0139;
 
 interface
 uses
    objpas, bug0139a;
 
 type
    AnotherClass=class(SomeClass)
    protected
    procedure doSomething; override;
    end ;
 
 implementation
 
 procedure AnotherClass.doSomething;
 begin
 inherited doSomething;  // this causes the error: " can not call protected
                         //  method from here " ( or something similar )
 end ;

end.
