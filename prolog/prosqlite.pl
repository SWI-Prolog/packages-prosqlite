:- module( prosqlite,
          [ sqlite_connect/2,           % +FileName, -Conn
            sqlite_connect/3,           % +FileName, -Conn, +Opts
            sqlite_disconnect/1,        % +Conn
            sqlite_current_connection/1,% -Conn
            sqlite_query/2,             % +SQL, -Row
            sqlite_query/3,             % +Conn, +SQL, -Row
            sqlite_format_query/3,      % +Conn, +SQL, -Row
            sqlite_assert/1,            % +Goal
            sqlite_assert/2,            % +Conn, +Goal
            sqlite_retractall/1,        % +Goal
            sqlite_retractall/2,        % +Goal, -Affected
            sqlite_retractall/3,        % +Conn, +Goal, -Affected
            sqlite_current_table/2,     % +Conn, -Table
            sqlite_current_table/3,     % +Conn, ?Table, -Facet
            sqlite_table_column/3,      % +Conn, ?Table, ?Column
            sqlite_table_count/3,       % +Conn, +Table, -Count
            sqlite_default_connection/1,% -Conn
            sqlite_date_sql_atom/2,     % ?Date, ?SqlAtom
            sqlite_version/2,           % -Version, -Date
            sqlite_citation/2           % -Atom, Bibterm
          ]).
:- load_foreign_library(foreign(prosqlite)).

:- dynamic( sqlite_connection/3 ).

/** <module>  proSQLite: a Prolog interface to the SQLite database system.

This library follows the design and borrows code from the ODBC library of SWI-Prolog 
http://www.swi-prolog.org/pldoc/packasqlite_connectge/odbc.html .

The SQLite system is a powerful zero-configuration management systme that interacts
with single-file databases that are cross-platform compatible binaries.

Prosqlite provides three layers of interaction with SQLite databases.
At the lower level is the querrig via SQL statements. A second layer 
allows the interogation of the database dictionary, and the final level
facilitates the viewing of database tables as predicates.
See the publications for further details via sqlite_citation/2.
If you use prosqlite in your research, please consider citing the publications
pointed to by sqlite_citation/2.

The library has been developed and tested for SWI 6.3.0 but it should 
also work on YAP Prolog.

The easiest way to install on SWI is via the package manager. 
If you can compile C-code, then simply do 
==
     ?- pack_install( prosqlite ).
==
And you are good to go.



     @version 0.0.6, 2012/10/06
     @license	Perl Artistic License
     @author Sander Canisius
     @author Nicos Angelopoulos
     @see Sander Canisius, Nicos Angelopoulos and Lodewyk Wessels.  Exploring file based databases via an Sqlite interface.  In the ICLP Workshop on Logic-based methods in Programming Environments, p. 2-9, (2012, Budapest, Hungary).
     @see http://bioinformatics.nki.nl/~nicos/pbs/wlpe2012_sqlite.pdf
     @see http://bioinformatics.nki.nl/~nicos/sware/prosqlite
     @see http://www.sqlite.org/
     @see files in examples/ directory
     @see also available as a SWI pack http://www.swi-prolog.org/pack/list
     @see sources at http://bioinformatics.nki.nl/~nicos/sware/prosqlite/prosqlite-0.0.6.tgz


*/

%-Section interface predicates
%

%% sqlite_version( -Version, -Date ).
%  The current version. Version is a Mj:Mn:Fx term, and date is a date(Y,M,D) term.
%
sqlite_version( 0:0:6, date(2012,10,06) ).

%% sqlite_citation( -Atom, -Bibterm ).
% Succeeds once for each publication related to this library. Atom is the atom representation
% suitable for printing while Bibterm is a bibtex(Type,Key,Pairs) term of the same publication.
% Produces all related publications on backtracking.
sqlite_citation( Atom, bibtex(Type,Key,Pairs) ) :-
     Atom = 'Exploring file based databases via an Sqlite interface. \n Canisius Sander, Nicos Angelopoulos and Lodewyk Wessels \n In the ICLP Workshop on Logic-based methods in Programming Environments (WLPE\'12),\n p.2-9, 2012. Budapest, Hungary.',
     Type = inproceedings,
     Key  = 'CanisiusS+2012',
     Pairs = [
          author = 'Sander Canisius and Nicos Angelopoulos and Lodewyk Wessels',
          title  = 'Exploring file based databases via an Sqlite interface',
          booktitle = 'In ICLP Workshop on Logic-based methods in Programming Environments (WLPE\'12)',
          year = 2012,
          pages= '2-9',
          month = 'September',
          address = 'Budapest, Hungary'
     ].


%% sqlite_connect(+File, ?Connection).
%
%  Open a connection to an sqlite File. If Connection is a variable, an opaque atom
%  is generated, otherwise the opened file is connected to handle Connecction.
%
%  ==
%    sqlite_connect('uniprot.sqlite', uniprot).
%  ==
%
sqlite_connect(File, Conn) :-
     sqlite_connect(File, Conn, []).


%% sqlite_connect(+File, ?Connection, +Options).
%
%  Open a connection to an sqlite File. If Connection is a variable, an opaque atom
%  is generated, otherwise the opened file is connected to handle Connecction.
%  Options is a sinlge term or a list of terms from the following:
%
%         * alias(Atom)     identify the connection as Alias in all transactions
%
%         * as_predicates(Boolean)  if true, create hook predicates that map
%                                   each sqlite table to a prolog predicate.
%                                   These are created in module user, and it is 
%                                   the user's responsibility to be unique in this module.
%
%         * at_module(AtMod)        the module at which the predicates will be asserted at
%                                   (if as_predicates(true)) is also given). Default is =|user|=.
%
%         * exists(Boolean)         do not throw an error if file does not exist and
%                                   Boolean is false. Default is true and an error is
%                                   thrown if the Sqlite file does not exist.
%
sqlite_connect(File, Conn, OptIn) :-
     to_list( OptIn, Opts ),
     sqlite_connect_1(File, Conn, Opts).

sqlite_connect_1(File, _Conn, Opts) :-
     \+ exists_file(File),
     \+ memberchk(exists(false), Opts),
     !,
     open(File, read, _). % just so it throws a nice error
sqlite_connect_1(File1, Conn, Opts) :-
     sqlite_alias(Opts, Conn, Alias),
     \+ var(Alias),
     sqlite_connection(Conn,File2,_),
     !,
     ( File1==File2 -> 
          write( connection_already_open(Conn) ), nl
          ;
          write( connection_alias_in_use(Conn,File2) ), nl
     ),
     fail.
sqlite_connect_1(File, Alias, Opts) :-
     sqlite_alias(Opts, Conn, Alias),
     c_sqlite_connect(File, Conn),
     asserta( sqlite_connection(Alias,File,Conn) ),
     sqlite_establish_predicates(Opts, Conn, Alias).

/*
sqlite_connect(File, Conn, Opts) :-
     c_sqlite_connect(File, Internal),
     !,
     assert( sqlite_connection(Conn,File,Internal) ). 
     */

%% sqlite_disconnect( +Alias ).
%
%  Terminate the connection to a SQLite database file.
%
%  ==
%    sqlite_disconnect(uniprot).
%  ==
%
sqlite_disconnect( Alias ) :-
     once( sqlite_connection(Alias,_,Conn) ),
     c_sqlite_disconnect( Conn ),
     retractall( sqlite_connection(Alias,_,Conn) ).

%% sqlite_current_connection(-Connection).
%
%  Return or interrogate the name of open connection handles.
%
sqlite_current_connection(Conn) :-
     sqlite_connection(Conn,_,_).

%% sqlite_default_connection(-Connection).
%
%  Return or interrogate the name of the default connection. This is the 
%  last connection opened.
%
sqlite_default_connection(Alias) :-
     sqlite_connection(Alias,_,_),
     !.

%% sqlite_query(+Sql, -Row).
%
%  Post an Sql query to default connection and get row result in Row.
%
sqlite_query(Sql, Row) :-
     sqlite_default_connection(Alias),
     sqlite_query(Alias, Sql, Row).

%% sqlite_query(+Connection, +Sql, -Row).
%
%  Post an Sql query to Sqlite Connection and get row result in Row.
%
sqlite_query(Alias, Query, Row) :-
     sqlite_alias_connection(Alias, Connection),
     c_sqlite_query(Connection, Query, Row).

%% sqlite_format_query(+Connection, +FAs, -Row).
%
%  Post a format style Sql query to Sqlite Connection and get row result in Row.
%  FAs is a - pair structure : Format-Arguments.
%
%  ==
%     sqlite_format_query(uniprot, 'PRAGMA table_info(~w)'-Table, row(_, Column, _, _, _, _))
%  ==
%
%
sqlite_format_query(Alias, Format-Arguments, Row) :-
	format(atom(Query), Format, Arguments),
	sqlite_query(Alias, Query, Row).

%% sqlite_current_table(+Connection, -Table).
%
%  Return or interrogate tables in the Sqlite database associated with Connection.
%
sqlite_current_table(Alias, Table) :-
	sqlite_query(Alias, 'SELECT name FROM sqlite_master WHERE type = "table"', row(Table)).

%% sqlite_current_table(+Connection, ?Table, -Facet ).
%
%  Facet is a property of Table found at Connection. Currently only arity(Arity) is
%  delivered.
sqlite_current_table(Connection, Table, Facet ) :-
     sqlite_current_table(Connection, Table),
     sqlite_facet_table( Facet, Connection, Table ).

%% sqlite_table_column(+Connection, ?Table, -Column).
%
%  Return or interrogate tables and columns in the Sqlite database associated with Connection.
%
sqlite_table_column(Alias, Table, Column) :-
     ( var(Table) -> 
          sqlite_current_table(Alias, Table) 
          ;
          true
     ),
	sqlite_format_query(Alias, 'PRAGMA table_info(~w)'-Table, row(_, Column, _, _, _, _)).


%% sqlite_table_count(+Connection, +Table, -Count).
% 
%  True if Count is the number of rows in Sqlite Connection associated Table.
%
sqlite_table_count(Alias, Table, Count) :-
     Sel = 'Select count (*) from ~w',
	sqlite_format_query(Alias, Sel-Table, row(Count)),
     !.

/** sqlite_retractall( Goal ).
     
     Run sqlite_retractall/3 with the default connetion as the first argument,
     and surpression the third argument.
*/
sqlite_retractall( Goal ) :-
     sqlite_default_connection( Conn ),
     sqlite_retractall( Conn, Goal, _Affected ).

/** sqlite_retractall( +Goal, -Affected ).
     
     Apply sqlite_retractall/3 with the default connection as the first argument.

*/
sqlite_retractall( Goal, Aff ) :-
     sqlite_default_connection( Conn ),
     sqlite_retractall( Conn, Goal, Aff ).

/** sqlite_retractall( +Alias, +Goal, -Affected ).

     Remove all rows that correspond to the table from SQLite database identified by Alias
     and is named by Goal's name. The arity of Goal should also match the arity of the table
     to be deleted. Ground arguments are added to the Where part of the DELETE SQL statement
     at their respective column locations.

     ==
          sqlite_retractall( uniprot, secondary_accessions(_,'P64943'), A ).
     ==
      
*/
sqlite_retractall( Conn, Goal, Affected ) :-
     functor( Goal, Name, _Arity ),
     findall( Col, sqlite_table_column(Conn,Name,Col), Cols ),
     findall( C-A, (arg(N,Goal,A),ground(A),nth1(N,Cols,C)), CAs ),
     sql_clm_value_pairs_to_where( CAs, Where ),
     sqlite_retractall_where( Where, Conn, Name, Affected ).

sqlite_retractall_where( '', Conn, Name, 0 ) :-
     !,
     write( user_error, 'Refusing to delete whole connection/table':Conn/Name ), 
     nl( user_error ),
     fail.
sqlite_retractall_where( Where, Conn, Name, Affected ) :-
     Del = 'Delete from',
     atomic_list_concat( [Del,Name,Where], ' ', Sql ),
     sqlite_query( Conn, Sql, Row ),
     Row = affected(Affected).

/** sqlite_assert( Goal ).

     Run =|sqlite( Default, Goal )|= with the Default SQLite connection
*/

sqlite_assert( Goal ) :-
     sqlite_default_connection( Conn ),
     sqlite_assert( Conn, Goal ).
     
/** sqlite_assert( Con, Goal ).

     Assert a single row to the table with the same name as Goal's functor. 
     The arity of Goal should also match the arity of the table.
     Goal should be ground.

==
     sqlite_assert( uniprot, secondary_accessions('Q10706','P64943') ).
==
*/
sqlite_assert( Conn, Goal ) :-
     ground( Goal ),
     functor( Goal, Name, _Arity ),
     findall( Col, sqlite_table_column(Conn,Name,Col), Cols ),
     Ins = 'Insert into ',
     Goal =.. [_|Args],
     maplist( dquote, Args, Qals ),
     atomic_list_concat( Qals, ',', Vals ),
     atomic_list_concat( Cols, ',', Cnms ),
     atomic_list_concat( [Ins,Name,' (',Cnms,') Values ','(',Vals,')'], Insert ),
     sqlite_query( Conn, Insert, _Res ).

/** sqlite_date_sql_atom( Date, Sql ).

Convert between  a Prolog date/3 term and an Sql atom. 
The conversion is bidirectional.
*/
sqlite_date_sql_atom( date(Y,M,D), Sql ) :-
     ground( Sql ), 
     !,
     atomic_list_concat( Consts, '/', Sql ),
     maplist( atom_number, Consts, [Y,M,D] ).
sqlite_date_sql_atom( date(Y,M,D), Sql ) :-
     atomic_list_concat( ['"',Y], Ya ),
     atomic_list_concat( [D,'"'], Da ),
     atomic_list_concat( [Ya,M,Da], '/', Sql ).


%-Section non-interface sqlite specific predicates
%

sqlite_alias(Opts, _Conn, Alias) :-
     memberchk(alias(Alias), Opts),
     !.
sqlite_alias(_Opts, _Conn, Alias ) :-
     atomic( Alias ),
     !.
sqlite_alias(_Opts, Conn, Conn).

sqlite_establish_predicates(Opts, Conn, Alias) :-
     memberchk(as_predicates(true), Opts), 
     !,
     findall(T-C, sqlite_table_column(Alias,T,C), TCs ),
     findall( T, member(T-_,TCs), RepTs ),
     sort( RepTs, Ts ),
     findall( T-Cs, (member(T,Ts),findall(C,member(T-C,TCs),Cs)), TdCs ),
     ( memberchk(at_module(Mod), Opts) -> true; Mod = user ),
     sqlite_establish_tables(TdCs, Conn, Mod ).
sqlite_establish_predicates(_Opts, _Conn, _Alias).

sqlite_establish_tables([], _Conn, _Mod ).
sqlite_establish_tables([Name-Columns|T], Conn, Mod) :-
     length(Columns, Arity),
     functor(Head, Name, Arity),
     Head =..[Name|Args],
     Body = prosqlite:sqlite_holds(Conn,Name,Arity,Columns,Args),
     Mod:assert((Head :- Body)),
     % assert((Head :- Body)),
     sqlite_establish_tables(T, Conn, Mod).
     
sqlite_holds(Conn, Name, _Arity, Columns, Args) :-
     pl_args_column_arg_ground_or_not_pairs( Args, Columns, KnwnClmPrs, UnKnwnCs, UnKnwnAs ),
     % findall(C-V, (nth1(N,Args,V),ground(V),nth1(N,Columns,C)), KnwnClmPrs),
     sql_clm_value_pairs_to_where(KnwnClmPrs, Where),
     % SelStar = 'Select ( from',
     % atomic_list_concat( [SelStar,Name,Where], ' ', Sql ),
     atomic_list_concat( UnKnwnCs, ',', UnC ),
     atomic_list_concat( ['Select ',UnC,'From',Name,Where], ' ', Sql ),
     Row =.. [row|UnKnwnAs],
     c_sqlite_query(Conn, Sql, Row).

sqlite_alias_connection(Alias, Connection) :-
     sqlite_connection(Alias,_,Connection),
     !.

pl_args_column_arg_ground_or_not_pairs( [], [], [], [], [] ).
pl_args_column_arg_ground_or_not_pairs( [A|As], [C|Cs], Knwn, UnCs, UnAs ) :-
     ( ground(A) -> 
          Knwn = [C-A|TKnwn],
          TUnCs = UnCs,
          TUnAs = UnAs
          ;
          TKnwn = Knwn,
          UnCs = [C|TUnCs],
          UnAs = [A|TUnAs]
     ),
     pl_args_column_arg_ground_or_not_pairs( As, Cs, TKnwn, TUnCs, TUnAs ).

%-Section sqlite non-specific auxiliary predicates 
%
to_list(OptIn, Opts) :-
     is_list(OptIn),
     !,
     Opts = OptIn.
to_list(Opt, [Opt] ).

dquote( Val, Quoted ) :-
     number( Val ), 
     !,
     Quoted = Val.
dquote( Val, Quoted ) :-
     atomic_list_concat( ['"',Val,'"'], Quoted ).

sql_clm_value_pairs_to_where(Known, Where) :-
     sql_clm_value_pairs_to_where_conjunction(Known, Conjunction),
     sql_where_conjunction_to_where(Conjunction, Where).
     
sql_where_conjunction_to_where('', '' ) :- !.
sql_where_conjunction_to_where(Conjunction, Where ) :-
     atom_concat( 'Where ', Conjunction, Where ).

sql_clm_value_pairs_to_where_conjunction([], '').
sql_clm_value_pairs_to_where_conjunction([K-V|T], Where) :-
     sql_clm_value_pairs_to_where_conjunction(T, InWhere),
     sql_clm_and_val_to_sql_equals_atom(K, V, KVAtm),
     ( InWhere == '' -> 
          Where = KVAtm
          ;
          atomic_list_concat([KVAtm, ' AND ', InWhere], Where)
     ).

sql_clm_and_val_to_sql_equals_atom(K, V, KVAtm) :-
     ( number(V) -> 
          atom_number(Vatm, V),
          atom_concat('=',Vatm,EqV)
          ;
          atom_concat(V, '\'', VDsh),
          atom_concat('=\'',VDsh,EqV)
     ),
     atom_concat(K, EqV, KVAtm).

sqlite_facet_table( arity(Arity), Connection, Table ) :-
     findall( Column, sqlite_table_column(Connection, Table, Column), Columns ),
     length( Columns, Arity ).
