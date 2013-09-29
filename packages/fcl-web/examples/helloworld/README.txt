Hello world example
===================
The simplest "Hello World" example using fcl-web (fpweb) that uses a web action 
called "func1call" in the web module to generate the response page.

Note that the only difference between CGI/FCGI and Apache module is in the 
main project .lpr file and the web server (Apache) configuration.

=====================
1. Compiling
1.a; with FPC
1.b; with Lazarus
2. Setup
2.a; as CGI
2.b; as Apache module
2.c; as FCGI
=====================

1. Compiling:
-------------
We can either use FPC directly, or Lazarus to compile the CGI/FCGI/Apache 
module application. The main project .lpr file, as well as the Lazarus .lpi is 
in the cgi/fcgi/apache directories.

1.a; with FPC
-------------
Go to the directory (cgi/fcgi/apache) that has the .lpr file you wish to 
compile, and then execute the command 

fpc -Fu../webmodule helloworld.lpr

The -Fu parameter shows FPC where to find the web module source code. All 
three web applications share the same web module code.

1.b; with Lazarus
-----------------
It needs the WebLaz Package installed. Open the .lpi file from the choosen 
application directory (cgi/fcgi/apache), and then 

Run -> Build from the menu.


2. Setup:
---------

2.a; as CGI
-----------
http://<WebServer>/cgi-bin/<CGIExecutableName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/cgi-bin/helloworld.exe/func1call

Note: You need to change the URL if "cgi-bin" or "helloworld.exe" changes 
(for example on Linux it is not helloworld.exe).
Also, if your server is listening on port 80 instead of 8080, you can leave 
the :8080 part from the calling URL.


2.b; as Apache module
---------------------
http://<WebServer>/<ApacheLocationName>/func1call should start the 
example if everything is set up properly.
ex: http://127.0.0.1:8080/myapache/func1call
Example Apache configuration file (e.g. httpd.conf) snippet for this:
LoadModule mod_helloworld "<path_to_mod>/helloworld.dll"
<Location /myapache>
    SetHandler mod_helloworld
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the URL if "myapache" changes.
Also, for example on Linux the module can be libhelloworld.so and not 
helloworld.dll

Note: If you recompile an apache module while the module itself is loaded into
the Apache server, the compilation might fail because the file is in use 
(Apache modules stay in the memory). So first, you always need to stop the 
server before you recompile or before you copy over the new version of the 
newly created module.
On Linux, it is enough to simply reload Apache after recompile.


2.c; as FCGI
------------
http://<WebServer>/<ApacheScriptAliasName>/func1call should start the example 
if everything is set up properly.
ex: http://127.0.0.1:8080/myfcgi/func1call
Example Apache configuration file (e.g. httpd.conf) snippet for this:

LoadModule fastcgi_module "<path_to_mod>/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "<path_to_fcgi_app>">
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#External FCGI app, has to start manually, Apache will not do it
  FastCgiExternalServer "<path_to_fcgi_app>/helloworld.exe" -host 127.0.0.1:2015 -idle-timeout 30 -flush
#optionally, to shorten the calling URL and to not display the executable file name (if used, no +ExecCGI is needed above)
  ScriptAlias /myfcgi "<path_to_fcgi_app>/helloworld.exe"
</IfModule>

Note: You need to change the module name if needed. For example on Linux, 
the module is not mod_fastcgi-2.4.6-AP22.dll but mod_fastcgi.so (need to be 
compiled from sources found at http://www.fastcgi.com/dist/ ).
The port (2015 in this example) must match the one set in the project main 
file (helloworld.lpr).
The FCGI application must be running in order for this demo to work (external 
FCGI server setup). Do not forget to restart it after changes and 
recompilation.
Also, mod_fastcgi is not the same as mod_fcgid that the Apache project is 
developing. The latter does not support external FCGI server apps.
There are other ways than external FCGI server apps to handle the FCGI 
protocol and both mod_fastcgi and mod_fcgid supports that. However, external 
FCGI servers are the best for debugging and development, that is why the 
examples do it that way.
