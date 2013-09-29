FCGI-README.txt
Last modified: Attila Borka 05/31/2011
-------------------------------------------------------------------------------
This readme will try to explain and help developers how to be able to 
use the FastCGI protocol in Lazarus (package fpweb/lazweb) and FPC 
(/packages/fcl-web) applications with the Apache web server.

There are two FCGI implementations we are able to use at the moment (FPC 
rev17597 /2.5.1/) with FPC/Lazarus:

1. mod_fcgid maintained by Apache (ASF) http://httpd.apache.org/mod_fcgid/
Easy to set up and use, Apache develops and maintains it. Just needs a 
download/install and it is ready to go, even on Linux.
There are a lot of options and fine tuning that can be used, please refer to 
their documentation at http://httpd.apache.org/mod_fcgid/mod/mod_fcgid.html .

2. mod_fastcgi from http://www.fastcgi.com/ the original FCGI "inventors"
The latest downloads are available from http://www.fastcgi.com/dist/ . Do not 
be alarmed if the latest one is years old. FCGI is a very stable protocol 
since many years (started in 1996).
On Linux, you will need to compile the mod_fastcgi module from the source 
codes, as binaries are only available for Windows as a DLL.
Note, that mod_fastcgi supports the use of External FCGI Servers, which makes 
it possible to run the FCGI application from within Lazarus for example, and 
be able to debug it with the GUI on the fly while it is generating the 
response pages. This can be a good choice for a development environment.
The documentation is at http://www.fastcgi.com/drupal/node/6 .


There are benefits to use both, everyone needs to decide which one fits their 
needs the best for a development environment or a live web server after some 
research on these modules.

Do not forget, that unless you use the External FCGI Server way, you need to 
restart your web server every time you recompile your FCGI program.
In case you use the External FCGI Server, you need to restart the application
itself after every recompile for the changes to take effect in the web server 
responses. The Apache server does not need to be restarted every time in this 
latter case.

===============================================================================
1. mod_fcgid from Apache
1.1 on Windows
1.2 on Linux
2. mod_fastcgi from fastcgi.com
2.1 on Windows
2.1 on Linux
3. NGINX configuration

===============================================================================

1. mod_fcgid from Apache:
=========================

1.1 mod_fcgid from Apache on Windows:
-------------------------------------
Setup Steps:
1.1.1 Download from http://httpd.apache.org/download.cgi#mod_fcgid the latest 
Windows zip file and unpack as it instructs you to, over the installed Apache 
directory. This puts the mod_fcgid.* files under the current /modules 
directory of Apache.

1.1.2 Now, mod_fcgid is ready to be used as soon as we load it with Apache.

1.1.3 If you have not done so yet, compile your FCGI application.

1.1.4 Then, edit the Apache /conf/httpd.conf file and add to the end:
############
LoadModule fcgid_module "modules/mod_fcgid.so"
<IfModule mod_fcgid.c>
  <Directory "<Path_To_Your_FCGI_application>">
    SetHandler fcgid-script
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#optionally, to shorten the URL and to not display the executable file name 
#(if ScriptAlias is used, no +ExecCGI is needed above)
  ScriptAlias /myfcgid "<Path_To_Your_FCGI_application>/<Your_FCGI_application>"
</IfModule>
############
Example:
LoadModule fcgid_module "modules/mod_fcgid.so"
<IfModule mod_fcgid.c>
  <Directory "C:/My Programs/LazarusFCGITest">
    SetHandler fcgid-script
    Order allow,deny
    Allow from all
  </Directory>
  ScriptAlias /myfcgid "C:/My Programs/LazarusFCGITest/helloworld.exe"
</IfModule>

Note, there are many ways to configure the FCGI applications, this is just but 
one example. You can check the Apache and mod_fcgid documentation for 
alternatives.

1.1.5 Start/Restart your Apache server so it will load your FCGI application. 
If everything went according to plan, your FCGI application should be listed 
in the Windows task manager as running.

1.1.6 Open your web browser and try to call your new FCGI application.
Example:
http://127.0.0.1:8080/myfcgid/func1call
or
http://127.0.0.1/myfcgid/func1call
depending on your Apache installation and configuration. "myfcgid" is the 
ScriptAlias name specified for the FCGI application, func1call is the action 
name we want to call within our default web module. If you have multiple web 
modules, you can enter the desired web module name before the action name, for 
example: 
http://127.0.0.1:8080/myfcgid/webmodule1/func1call
or
http://127.0.0.1/myfcgid/webmodule1/func1call

If there is any problem, you can try and check the Apache error.log for clues.


1.2 mod_fcgid from Apache on Linux:
-----------------------------------
Setup Steps:
1.2.1 Install the mod_fcgi module for Apache with your distro's package 
manager. 
Example on Ubuntu:
sudo apt-get install libapache2-mod-fcgid

This downloads, installs and configures mod_fcgid without the need to do 
anything else.

1.2.2 Now, mod_fcgid is ready to be used as soon as we load it with Apache.
mod_fcgid.so should be sitting in the /usr/lib/apache2/modules/ directory (on 
Ubuntu)

1.2.3 If you have not done so yet, compile your FCGI application.

1.2.4 Edit the Apache configuration file (/etc/apache2/apache2.conf on Ubuntu) 
and add to the end:
############
LoadModule fcgid_module "<Path_To_Mod>/mod_fcgid.so"
<IfModule mod_fcgid.c>
  <Directory "<Path_To_Your_FCGI_application>">
    SetHandler fcgid-script
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#optionally, to shorten the URL and to not display the executable file name 
#(if ScriptAlias is used, no +ExecCGI is needed above)
  ScriptAlias /myfcgid "<Path_To_Your_FCGI_application>/<Your_FCGI_application>"
</IfModule>
############
Example:
LoadModule fcgid_module "/usr/lib/apache2/modules/mod_fcgid.so"
<IfModule mod_fcgid.c>
  <Directory "/home/johndoe/LazarusFCGITest">
    SetHandler fcgid-script
    Order allow,deny
    Allow from all
  </Directory>
  ScriptAlias /myfcgid "/home/johndoe/LazarusFCGITest/helloworld"
</IfModule>

(the project was compiled into directory /home/johndoe/LazarusFCGITest/ , and 
the FCGI application is called helloworld with no file extension)

Note, there are many ways to configure the FCGI applications, this is just but 
one example. You can check the Apache and mod_fcgid documentation for 
alternatives.

1.2.5 Start/Restart your Apache server so it will load your FCGI application. 
If everything went according to plan, and you do a "sudo netstat -l" 
(on Ubuntu) from a terminal window, there should be a new line in the result 
list looking something like this: 
... LISTENING   XXXX   /var/lib/apache2/fcgid/sock/... 
indicating, that the Apache mod_fcgid module has loaded your FCGI application.

1.2.6 Open your web browser and try to call your new FCGI application.
Example:
http://127.0.0.1/myfcgid/func1call

"myfcgid" is the ScriptAlias name specified for the FCGI application, and 
func1call is the action name we want to call within our default web module.
If you have multiple web modules, you can enter the desired web module name 
before the action name, for example: 
http://127.0.0.1/myfcgid/webmodule1/func1call .

If there is any problem, you can try and check the Apache error.log for clues.


2. mod_fastcgi from fastcgi.com:
================================
Unlike the Apache developed mod_fcgid, mod_fastcgi has two main operating 
modes for FCGI applications. One is very similar to mod_fcgid (where Apache 
itself loads the FCGI application at startup), and one called External FCGI 
Server mode. With this latter, Apache will not load the FCGI application at 
startup, but it has to be running on its own when a web request arrives 
(either as a system service/daemon, or a simple running application) and 
listening. This makes it possible to run the FCGI application from within a 
debugger interactively, to see/track the request handling like any normal GUI 
application debugging.

2.1 mod_fastcgi from fastcgi.com on Windows:
--------------------------------------------
Setup Steps:
2.1.1 Download the latest SNAP or stable DLL from http://www.fastcgi.com/dist/ 
and put it into the Apache /modules/ directory.

2.1.2 If you have not done so yet, compile your FCGI application.
In case you want to set up an External FCGI Server, then you must specify a 
port number in your main project file (.lpr) before the Application.Run 
instruction (this is the only change needed).
Example:
<...snip...>
  Application.Initialize;
  Application.Port:=9999;//Port the FCGI application is listening on 
  Application.Run;
<...snip...>

2.1.3 Edit the Apache /conf/httpd.conf file and add to the end:

 2.1.3.a External FCGI Server
############
LoadModule fastcgi_module "modules/<mod_fastcgi_DLL_name>"
<IfModule mod_fastcgi.c>
  <Directory "<Path_To_Your_FCGI_application>">
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#External FCGI app, has to be started and running when a request comes in
  FastCgiExternalServer "<Path_To_Your_FCGI_application>/<Your_FCGI_application>" -host 127.0.0.1:<Port> -idle-timeout 30 -flush
#optionally, to shorten the URL and to not display the executable file name (if used, no +ExecCGI is needed above):
  ScriptAlias /myfcgi "<Path_To_Your_FCGI_application>/<Your_FCGI_application>"
</IfModule>
############
Example:
LoadModule fastcgi_module "modules/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "C:/My Programs/LazarusFCGITest">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiExternalServer "C:/My Programs/LazarusFCGITest/helloworld.exe" -host 127.0.0.1:9999 -idle-timeout 30 -flush
  ScriptAlias /myfcgi "C:/My Programs/LazarusFCGITest/helloworld.exe"
</IfModule>

 2.1.3.b Regular FCGI Server
Replace the FastCgiExternalServer line above with
  FastCgiServer "<Path_To_Your_FCGI_application>/<Your_FCGI_application>" -idle-timeout 30

Example:
LoadModule fastcgi_module "modules/mod_fastcgi-2.4.6-AP22.dll"
<IfModule mod_fastcgi.c>
  <Directory "C:/My Programs/LazarusFCGITest">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiServer "C:/My Programs/LazarusFCGITest/helloworld.exe" -idle-timeout 30
  ScriptAlias /myfcgi "C:/My Programs/LazarusFCGITest/helloworld.exe"
</IfModule>

2.1.4 Start/Restart your Apache server.
If you will use the FastCgiExternalServer, then start your application 
manually, so it can start accepting incoming requests from the web server.
If you use the FastCgiServer, then your FCGI application should be launched 
by the Apache server when it starts and should be listed in the Windows task 
manager as running.

2.1.5 Open your web browser and try to call your new FCGI application.
Example:
http://127.0.0.1:8080/myfcgi/func1call
or
http://127.0.0.1/myfcgi/func1call
depending on your Apache installation and configuration.

"myfcgi" is the ScriptAlias name specified for the FCGI application, and 
func1call is the action name we want to call within our default web module.
If you have multiple web modules, you can enter the desired web module name 
before the action name, for example: 
http://127.0.0.1:8080/myfcgi/webmodule1/func1call
or
http://127.0.0.1/myfcgi/webmodule1/func1call

If there is any problem, you can try and check the Apache error.log for clues.


2.2 mod_fastcgi from fastcgi.com on Linux:
------------------------------------------
Setup Steps:
2.2.1 There are no binaries offered for download for Linux, so we need to get 
the mod_fastcgi source codes from http://www.fastcgi.com/dist/ and compile it.

 2.2.1.1. If the Apache development package headers are not installed, then 
we need to get them first:
(on Ubuntu)
>sudo apt-get install apache2-dev

 2.2.1.2. Get the fastcgi module sources (get the latest SNAP or highest 
version stable release from http://www.fastcgi.com/dist/ )

>cd /home/johndoe
>wget http://www.fastcgi.com/dist/mod_fastcgi-xxxxxxxxxxx.tar.gz

Example: wget http://www.fastcgi.com/dist/mod_fastcgi-SNAP-0910052141.tar.gz

 2.2.1.3. Unpack the source code files.
>tar -xzf mod_fastcgi-*.tar.gz

 2.2.1.4. Configure the makefile (we use Apache 2.2, so need the Makefile.AP2).
>cd mod_fastcgi-*
>cp Makefile.AP2 Makefile

 2.2.1.5. Edit the copied Makefile and set top_dir to the proper apache source 
directory created by the Apache development package install. For example, on 
Ubuntu it is /usr/share/apache2 (containing the .mk file).

 2.2.1.6. Compile and install (on Ubuntu)
>make
>sudo make install

2.2.2. Now, we should have a mod_fastcgi.so in /usr/lib/apache2/modules (on 
Ubuntu)

2.2.3. If you have not done so yet, compile your FCGI application.
In case you want to set up an External FCGI Server, then you must specify a 
port number in your main project file (.lpr) before the Application.Run 
instruction (this is the only change needed).
Example:
<...snip...>
  Application.Initialize;
  Application.Port:=1234;//Port the FCGI application is listening on 
  Application.Run;
<...snip...>

2.2.4 Edit the Apache configuration file (/etc/apache2/apache2.conf on Ubuntu) 
and add to the end:
 2.2.4.a External FCGI Server
############
LoadModule fastcgi_module "<Path_To_Mod>/mod_fastcgi.so"
<IfModule mod_fastcgi.c>
  <Directory "<Path_To_Your_FCGI_application>">
#    Options +ExecCGI  <- not needed if ScriptAlias is used below
    Order allow,deny
    Allow from all
  </Directory>
#External FCGI app, has to be manually started and running when a request comes in
  FastCgiExternalServer "<Path_To_Your_FCGI_application>/<Your_FCGI_application>" -host 127.0.0.1:<Port> -idle-timeout 30 -flush
#optionally, to shorten the URL and to not display the executable file name (if used, no +ExecCGI is needed above):
  ScriptAlias /myfcgi "<Path_To_Your_FCGI_application>/<Your_FCGI_application>"
</IfModule>
############
Example:
LoadModule fastcgi_module "/usr/lib/apache2/modules/mod_fastcgi.so"
<IfModule mod_fastcgi.c>
  <Directory "/home/johndoe/LazarusFCGITest">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiExternalServer "/home/johndoe/LazarusFCGITest/helloworld" -host 127.0.0.1:1234 -idle-timeout 30 -flush
  ScriptAlias /myfcgi "/home/johndoe/LazarusFCGITest/helloworld"
</IfModule>

 2.2.4.b Regular FCGI Server
Replace the FastCgiExternalServer line above with
  FastCgiServer "<Path_To_Your_FCGI_application>/<Your_FCGI_application>" -idle-timeout 30

Example:
LoadModule fastcgi_module "/usr/lib/apache2/modules/mod_fastcgi.so"
<IfModule mod_fastcgi.c>
  <Directory "/home/johndoe/LazarusFCGITest">
    Order allow,deny
    Allow from all
  </Directory>
  FastCgiServer "/home/johndoe/LazarusFCGITest/helloworld" -idle-timeout 30
  ScriptAlias /myfcgi "/home/johndoe/LazarusFCGITest/helloworld"
</IfModule>

(the project is compiled into directory /home/johndoe/LazarusFCGITest/ , and 
the FCGI application is called helloworld with no file extension)

Note, there are many ways to configure the FCGI applications, this is just but 
one example. You can check the Apache and mod_fastcgi documentation for 
alternatives. For example, some more information available at
http://www.fastcgi.com/docs/faq.html#typical_httpd.conf

2.2.5 Start/Restart your Apache server. 
If FastCgiExternalServer is used, then start your application manually, so it 
can start accepting incoming requests from the web server.
On the other hand, if FastCgiServer is used and everything went according to 
plan, then the Apache error.log should contain a warning message about your 
FCGI application, saying that it was started. Something like:
[warn] FastCGI: server "/home/johndoe/LazarusFCGITest/helloworld" started...

2.2.6 Open your web browser and try to call your new FCGI application.
Example:
http://127.0.0.1/myfcgi/func1call
"myfcgi" is the ScriptAlias name specified for the FCGI application, and 
func1call is the action name we want to call within our default web module.
If you have multiple web modules, you can enter the desired web module name 
before the action name, for example: 
http://127.0.0.1/myfcgi/webmodule1/func1call

If there is any problem, you can try and check the Apache error.log for clues.

===============================================================================

3. NGINX Configuration

Full configuraion of FastCGI is discussed at:

http://wiki.nginx.org/HttpFastcgiModule

NGINX does not support managing the FastCGI process. The FastCGI process must be
started outside of the NGINX engine, much like the FastCgiExternalServer
mode of Apache. NGINX will just forward the requests to whatever port the
FastCGI process is running on. Note that the fastcgi process does not need
to be running on the same machine as the NGINX process.

This means that in the FastCGI program, you must set the port on which the 
FastCGI process is listening:

Example:
<...snip...>
  Application.Initialize;
  Application.Port:=1234;//Port the FCGI application is listening on 
  Application.Run;
<...snip...>

And the FastCGI process must be started somehow separately. 
On windows, a windows service application is most suitable.
On Unices, a simple process can be put in the system startup scripts.

Then, NGINX must be told to forward all requests to this address.

The following is a sample of a NGINX configuration which sends all requests
to a FastCGI process, listening on port 1234 on the same machine:

{
  include /etc/nginx/fastcgi_params;

  location / { 
     fastcgi_pass 127.0.0.1:1234
     fastcgi_split_path_info ^((?U).+www.mysite.com)(/?.+)$; 
     fastcgi_param  PATH_INFO          $fastcgi_path_info; 
     fastcgi_param  PATH_TRANSLATED    $document_root$fastcgi_path_info; 
     fastcgi_param  SCRIPT_FILENAME    $document_root$fastcgi_script_name; 
  } 
}

The "fastcgi_split_path_info" and fastcgi_param directives are needed, 
so that the FCL-Web environment gets enough information to act on the
request. (PATH_INFO and SCRIPT_FILENAME are needed to work correctly)

This is another example:

location ~ /helloworld/.*) {
  fastcgi_pass    127.0.0.1:1234
  fastcgi_split_path_info ^((?U)./helloworld)(/?.+)$; 
  fastcgi_param   PATH_INFO   $path_info;
  fastcgi_param   SCRIPT_NAME "/helloworld";
}

All urls below 'helloworld' will be passed on to the FastCGI process.
