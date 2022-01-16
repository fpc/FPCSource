
Small demo for simple file module. The server will listen on a specified
port (default 3000) and will serve files starting from the current working
directory.

Just start the server, no options, and point your browser at
http://localhost:3000/

running simpleserver -h will provide the following help:

-d --directory=dir  

  Base directory from which to serve files.
  Default is current working directory: /home/michael/FPC/trunk/packages/fcl-web/examples/simpleserver

-i --indexpage=name 

  Directory index page to use (default: index.html)

-n --noindexpage    

  Do not allow index page.

-p --port=NNNN      

  TCP/IP port to listen on (default is 3000)

-m --mimetypes=file 

  path of mime.types file. Loaded in addition to OS known types.

  A sample mime.types file is provided.

-q --quiet          

  Do not write diagnostic messages

-s --ssl            

  Use SSL. 
  If you set this, the -H or --hostname option must also be used.

-H --hostname=NAME  
  set hostname for self-signed SSL certificate
