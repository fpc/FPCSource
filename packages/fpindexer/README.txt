This directory contains an implementation of an indexing and search
mechanism.

Architecture:
=============

The indexer and search mechanism design is modular:

  - A storage mechanism
  - An indexer class
  - A search class
  - Text processing classes.

The indexer uses a text processing class and a storage mechanism to create a
search database. The search class uses the same storage mechanism to search
the database. 

Currently, 3 databases are supported:
  - In memory database (plus flat file storage)
  - Firebird database
  - sqlite database.

3 input text processors are supported:
   - Plain text
   - HTML
   - Pas files.
A text processor is selected based on the extension of a file, if a file is
processed.

It is possible to specify a list of words to ignore per language, and a mask for words to
ignore.

On top of the file/stream indexer, a database indexer is implemented.
It can be used to implement full-text search on a database.

Sample programs for all 3 classes (search, index and index DB) are provided
in the examples dir.

Overview of units:
==================
fpindexer:
  The indexer, search and abstract database engine classes. 
  An abstract SQL storage engine class.

ireaderhtml  
  an input engine for HTML files.

ireaderpas  
  an input engine for pascal files.

ireadertxt
  an input engine for plain text files.

masks  
  Copied from the LCL, to implement masks on words.

memindexdb  
  A memory storage engine.

sqldbindexdb  
  An abstract SQLDB storage engine.

fbindexdb  
  A descendent of the SQLDB storage engine which uses a firebird database.

sqliteindexdb
  SQLite database storage engine.

dbindexdb
  Component to index a database.

Overview of classes:
====================

fpindexer:
----------
TFPIndexer: The indexing engine.

TCustomFileReader: abstract input engine.
TFileHandlersManager: factory for file reader classes.
TIgnoreListDef: Word ignore list definition.
TIgnoreLists: Collection of ignore lists

TFPSearch: the search engine.

TCustomIndexDB: Abstract storage engine.
TSQLIndexDB: Abstract SQL-Based storage engine.

ireaderhtml: 
------------

 TIReaderHTML:  HTML input engine.

ireaderpas:
-----------

 TIReaderPAS: pascal input engine.

ireadertxt:  
-----------

  TIReaderTXT: plain text input engine.

memindexdb:
-----------
  TMemIndexDB: In memory storage engine
  TFileIndexDB: Descendent of TMemIndexDB which stores everything in a flat
file using a custom format.

sqldbindexdb:  
-------------
  TSQLDBIndexDB: Abstract class for SQLDB-based storage (descendent of TSQLIndexDB)

sqliteindexdb:
--------------  
  TSQLiteIndexDB: SQLIte based storage engine, descendent of TSQLIndexDB

fbindexdb:  
----------
  TFBIndexDB TSQLDBIndexDB descendent for Firebird.

dbindexdb:
----------
  TDBIndexer: Implements a database indexer, using a second database as the index.
  TIBIndexer: Descendent of TDBIndexer for firebird.
