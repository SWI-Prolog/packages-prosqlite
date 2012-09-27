:- module( uniprot, [uniprot/0] ).


:- nl, nl, nl.
:- write( 'To get the example database used in this examples do:' ), nl.
:- write( 'wget http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite' ), nl.
:- nl, nl.

:- use_module( library(prosqlite) ).
/** <module>  uniprot: A complete example for proSQLite.

Look at the sources for the difference predicate calls.

You need the test database from 
               http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite

The output should look like:

==
άμπελος;lib/db% swipl -f none
Welcome to SWI-Prolog (Multi-threaded, 64 bits, Version 6.3.0-28-g53e80e7)
Copyright (c) 1990-2012 University of Amsterdam, VU Amsterdam
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under certain conditions.
Please visit http://www.swi-prolog.org for details.

For help, use ?- help(Topic). or ?- apropos(Word).

?- [uniprot].



To get the example database used in this examples do:
wget http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite


%  library(prosqlite) compiled into prosqlite 0.02 sec, 1,519 clauses
% uniprot compiled 0.02 sec, 1,532 clauses
true.

?- uniprot.
Running on:date(2012,9,27)
Using database at: uniprot.sqlite

table:secondary_accessions
table:identifier_mapping

secondary_accessions/secondary_accession
secondary_accessions/primary_accession
identifier_mapping/uniprot_accession
identifier_mapping/identifier_type
identifier_mapping/target_identifier

secondary_accessions:286525
identifier_mapping:3044651

secondary_accessions+286525
identifier_mapping+3044651

[A0A111,Q10706]-P64943
rows_for(P64943):[row(A0A111),row(Q10706)]

by_findall(P64943,[A0A111,Q10706])
Caution! Deleting db entries for-P64943
sql(Delete from secondary_accessions Where primary_accession='P64943')
Affected rows:2
now(P64943,[])

Not to worry! Adding back the db entries for-P64943
toi(Insert into secondary_accessions (secondary_accession,primary_accession) Values ("A0A111","P64943"))
res(affected(1))
toi(Insert into secondary_accessions (secondary_accession,primary_accession) Values ("Q10706","P64943"))
res(affected(1))
finally(P64943,[A0A111,Q10706])

v:0:0:6
d:date(2012,9,27)

citation
Exploring file based databases via an Sqlite interface. 
 Canisius Sander, Nicos Angelopoulos and Lodewyk Wessels 
 In the ICLP Workshop on Logic-based methods in Programming Environments (WLPE'12),
 p.2-9, 2012. Budapest, Hungary.
true.

?- 

==
     @version 0.0.6, 2012/09/27
     @see latest version at http://bioinformatics.nki.nl/~nicos/sware/prosqlite/uniprot.pl
     @see also http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite

*/


/** uniprot.

Test predicate for prosqlite. Tests all components.

You need the test database from 
               http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite


*/

uniprot :-
     date( Date ), 
     write( 'Running on':Date ), nl,
     sqlite_connect('uniprot.sqlite',uniprot, as_predicates(true) ),  % connect the database
     nl,
     show_tables,
     show_columns,
     show_counts,
     findall_counts,
     T = 'P64943',
     findall( A, secondary_accessions(A,T), As ),
     write( As-T ), nl,
     Q = 'Select secondary_accession From secondary_accessions Where primary_accession="~w"',

     findall( Row, sqlite_format_query( uniprot, Q-'P64943', Row ), Rows ),
     write(rows_for(T):Rows), nl, nl,

     Id = 'P64943',
     findall( S, secondary_accessions(S,Id), SetBef ),
     write( by_findall(Id,SetBef) ), nl,
     write( 'Caution! Deleting db entries for'-Id ), nl,
     sqlite_retractall( uniprot, secondary_accessions(_,Id), Aff ), 
     write( 'Affected rows':Aff ), nl,
     findall( S, secondary_accessions(S,Id), SetAft ),
     write( now(Id,SetAft) ), nl, nl,
     write( 'Not to worry! Adding back the db entries for'-Id ), nl,
     Add1 = 'A0A111',
     sqlite_assert( uniprot, secondary_accessions(Add1,Id) ), 
     Add2 = 'Q10706',
     sqlite_assert( uniprot, secondary_accessions(Add2,Id) ), 
     findall( S, secondary_accessions(S,Id), SetFin ),
     write( finally(Id,SetFin) ), nl, nl,
     % [A0A111,Q10706]-P64943

     sqlite_version( Ver, Date ),
     write( v:Ver ), nl,
     write( d:Date ), nl, nl,
     sqlite_citation( AtmCite, _B ),
     write( citation ), nl,
     write( AtmCite ), nl.


show_tables :-
     sqlite_current_table( uniprot, Table ),
     write( table:Table ), nl, 
     fail.
show_tables :-
     nl.

show_columns :-
     sqlite_table_column( uniprot, Table, Col ),
     write( Table/Col ), nl, 
     fail.
show_columns :-
     nl.

show_counts :-
     sqlite_current_table(uniprot, Table),
     sqlite_table_count(uniprot, Table, Count),
     write(Table:Count), nl, 
     fail.
show_counts :-
     nl.

findall_counts :-
     sqlite_current_table(uniprot, Table, arity(Arity) ),
     length(List, Arity),
     Pred =.. [Table|List],
     findall(1, Pred, Ones),
     length(Ones, Count),
     write( Table+Count ), nl, 
     fail.
findall_counts :-
     nl.
