Testsuite CGI program

The testsuite program is constructed so it can be compiled in 2 ways:
- as a standalone HTTP server - suitable for testing the code.
- as a CGI script - suitable for production in a low-traffic site

By default, the program is compiled as a .cgi program.

To compile it as a http server, compile with the httpserver define
(-dhttpserver)
