unit tbs0139;

{$mode objfpc}

 interface
 uses
    tbs0139a;

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