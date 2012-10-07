:- use_module( library(prosqlite) ).

simple :-
     sqlite_connect( simple, simple, exists(false) ),
     C = 'CREATE TABLE cited_by (pubmed_id bigint(20), ret_date date, citer bigint(20), Primary Key (pubmed_id,citer) );',
     sqlite_query( simple, C, Row ),
     write( create_row(Row) ), nl,
     I1 = 'Insert into cited_by (pubmed_id,ret_date,citer) values (123, "2012/10/06", 321);',
     % I1 = 'Insert into cited_by (pubmed_id,ret_date,citer) values (123, "2012/10/06", 321);',
     sqlite_query( simple, I1, RowI1 ),
     write( insert_row_1(RowI1) ), nl,
     Date = date(2012,10,06),
     Pfx2 = 'Insert into cited_by (pubmed_id,ret_date,citer) values (120, ',
     sqlite_date_sql_atom( Date, SDate ),
     atomic_list_concat( [Pfx2,SDate,', 321);'], I2 ),
     write( i2(I2) ), nl,
     sqlite_query( simple, I2, RowI2 ),
     write( insert_row_2(RowI2) ), nl,
     Sel = 'Select * from cited_by',
     write( sel(Sel) ), nl,
     findall( _, (sqlite_query(simple,Sel,FRow),write(sel:FRow),nl), _ ),
     D = 'Delete From cited_by Where pubmed_id = 120;',
     write( d(D) ), nl,
     sqlite_query( simple, D, DRow ),
     write( del_row(DRow) ), nl,
     findall( _, (sqlite_query(simple,Sel,FRow),write(sel:FRow),nl), _ ),
     sqlite_disconnect( simple ).

