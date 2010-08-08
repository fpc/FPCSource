{ %opt=-Mdelphi -Sa }
program t_testconvunit;
uses Sysutils, u_testconvunit;

begin
    Randomize();
    try 
        if not testconvunit_test_silent() then
        begin
            //WriteLn('*');
            Halt(1);
        end;
    except on E: Exception do 
        begin
            Halt(2);
        end;
    end;
    Halt(0);
end.
