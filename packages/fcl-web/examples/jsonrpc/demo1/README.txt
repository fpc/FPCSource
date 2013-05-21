
This is an example of how to use JSON-RPC.

It requires Lazarus to compile.

The various *.in files are input for JSON-RPC requests.

The application can be tested as follows from the command line:

testcgiapp -i demo -p echo/manual < echo.in
testcgiapp -i demo -p echo/dispatch < echobatch.in
testcgiapp -i demo -p echo/registered < echobatch.in
testcgiapp -i demo -p echo/extdirect < extdirect.in
testcgiapp -i demo -p echo/dispatch < notification.in
testcgiapp -i demo -p echo/extdirectapi
testcgiapp -i demo -p echo/content < echobatch.in
testcgiapp -i demo -p echo/module < echobatch.in    

The response is printed on standard output.

The testcgiapp application is located in fcl-web/tests
