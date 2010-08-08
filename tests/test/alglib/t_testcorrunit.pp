{ %opt=-Mdelphi -Sa }
program t_testcorrunit;
uses Sysutils, u_testcorrunit;

begin
    Randomize();
    try 
        if not testcorrunit_test_silent() then
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
