Example that responds with the calling and system parameters
============================================================
This demonstrates how to create a basic fpweb application. It responds to a 
request with a list of received/sent parameters, server settings and variables.

Note: apart from the main project file (echo.lpr), there is not much that 
needs to change with using fpweb, no matter if we create CGI/FCGI applications 
or Apache modules. The web server config is different for each, of course.

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

fpc -Fu../webmodule echo.lpr

The -Fu parameter shows FPC where to find the web module source code. All 
three web applications share the same web module code.

1.b; with Lazarus
-----------------
The example needs the WebLaz Package installed. 
If that is done, open the .lpi file from the choosen application directory 
(cgi/fcgi/apache), and then 

Run -> Build from the menu.


2. Setup:
---------

2.a; as CGI
-----------
http://<WebServer>/cgi-bin/<CGIExecutableName>/ should start the example if 
everything is set up properly.
example: http://127.0.0.1:8080/cgi-bin/echo.exe/

Note: You need to change the CGI application name if needed (for example, on 
Linux it is not echo.exe).
Also, if your server is listening on port 80 instead of 8080, you can leave 
the :8080 part from the calling URL.


2.b; as Apache module
---------------------
http://<WebServer>/<ApacheLocationName>/ should start the example if 
everything is set up properly.
ex: http://127.0.0.1:8080/myapache/

An example for the needed Apache configuration file (example: httpd.conf) snippet:

LoadModule mod_echo "<path_to_mod>/echo.dll"
<Location /myapache>
    SetHandler mod_echo
    Order allow,deny
    Allow from all
</Location>

Note: You need to change the module name if needed. For example on Linux, 
the module can be libecho.so or just simply libecho and not echo.dll .

Note: If you recompile an Apache module while the module itself is loaded into
the Apache server, the compilation will fail, because the file is in use 
(Apache modules stay in memory). 
So first, you always need to stop the Apache server before you recompile 
or before you copy over the new version of the created module.


2.c; as FCGI
------------
http://<WebServer>/<ApacheScriptAliasName>/ should start the example if 
everything is set up properly.
ex: http://127.0.0.1:8080/myfcgi/

An example for the needed Apache configuration file (example: httpd.conf) snippet:

LoadModule fastcgi_module "<path_to_mod>/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "<path_to_fcgi_app>">
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#External FCGI app, has to start manually, Apache will not do it
  FastCgiExternalServer "<path_to_fcgi_app>/echo.exe" -host 127.0.0.1:2015 -idle-timeout 30 -flush
#optionally, to shorten the calling URL and to not display the executable file name (if used, no +ExecCGI is needed above)
  ScriptAlias /myfcgi "<path_to_fcgi_app>/echo.exe"
</IfModule>

Note: You need to change the module name if needed. For example on Linux, 
the module is not mod_fastcgi-2.4.6-AP22.dll but mod_fastcgi.so (needs to be 
compiled from sources found at http://www.fastcgi.com/dist/ ).
The port (2015 in this example) must match the one set in the project main 
file (echo.lpr).
The FCGI application must be running in order for this demo to work (external 
FCGI server setup). Do not forget to restart it after changes and 
recompilation.
Also, mod_fastcgi is not the same as mod_fcgid that the Apache project is 
developing. The latter does not support external FCGI server apps.
There are other ways than external FCGI server apps to handle the FCGI 
protocol and both mod_fastcgi and mod_fcgid supports that. However, external 
FCGI servers are the best for debugging and development, that is why the 
examples do it that way.
