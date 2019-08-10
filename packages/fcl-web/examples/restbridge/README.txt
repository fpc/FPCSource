This is a demo for the SQLDB REST Bridge.

It requires a database. The database can be created using the
expenses-DB.sql file (replace DB with the appropriate type) 

Sample data can be inserted with the expenses-data.sql file.

You must edit the program to provide the correct database credentials: 
look for the ExposeDatabase() call, and edit the username/password.

You must also change the name and location of the database.

You can also set the port on which the demo should listen for HTTP requests.
By default it is 3000.

The program can save the connection data to an .ini file, run it with -s
myfile.ini. The connection data and database schema will then be saved.

It can pick up the connection data and schema with the -c myfile.ini
command-line options at a next run.


Enjoy !

Michael.