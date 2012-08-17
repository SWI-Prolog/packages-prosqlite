:- nl, nl, nl.
:- write( 'To get the example database used in this examples do:' ), nl.
:- write( 'wget http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite' ), nl.
:- nl, nl.

:- use_module( library(prosqlite) ).

uniprot :-
     sqlite_connect('uniprot.sqlite',uniprot, as_predicates(true) ),  % connect the database
     show_tables,
     show_columns,
     show_counts,
     findall_counts,
     T = 'P64943',
     findall( A, secondary_accessions(A,T), As ),
     write( As-T ), nl,
     Q = 'Select secondary_accession From secondary_accessions Where primary_accession="~w"',
     findall( Row, sqlite_format_query( uniprot, Q-'P64943', Row ), Rows ),
     write(rows_for(T):Rows), nl.

/** uniprot.

Test predicate for prosqlite.
You need the test database from 

The output should look like : http://bioinformatics.nki.nl/~nicos/sware/sqlite/uniprot.sqlite

==
?- uniprot.
uniprot.sqlite
secondary_accessions
identifier_mapping

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
rows:[row(A0A111),row(Q10706)]
true.


==

*/


show_tables :-
     sqlite_current_table( uniprot, Table ),
     write( Table ), nl, 
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
