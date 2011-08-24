
In this directory you can find some examples for SQLdb. They can also be used
to test functionality and new connections.

To use these examples you need a working login to a DB-Server and have the
appropiate client installed.  You have to change 'database.ini' to work with
the right database-engine and login-credentials.  Also check if the format
of various dates matches the format your db expects.

You can check if everything works fine by compiling & running 'alisttables'. If
everything works well, you'll get a list of all tables in the database provided
in database.ini.

I suggest to compile and run the examples in this order:

alisttables.pp      - shows all tables in the current database
bcreatetable.pp     - creates the 'fpdev' table, if it doesn't exist already
cfilltable.pp       - populates the fpdev table with one record
dshowtable.pp       - shows all records in the fpdev table
efilltableparams.pp - add more entries to fpdev, first with and then without
                      using parameters (use dshowtable again to see the results)
fedittable          - edits the first record, and adds another one, without
                      using update/insert-queries. (dshowtable to see results)
gfiltertable        - shows how to use filtering

As you can see all programs start with a letter in alphabetical order.

I think that these examples could be used to write some sort of SQLdb tutorial,
but i don't have time to do that myself. So if someone is interested... Be my
guest.


Joost van der Sluis, 26-sept-2005

