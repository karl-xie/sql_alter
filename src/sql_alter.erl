-module(sql_alter).

-include("sql_alter.hrl").

-define(TIMEOUT, 10000).
-define(PRINT(F, M), io:format(standard_error, F ++ "~n", M)).

-export([main/1]).


%% escript sql_alter -source=mysql --host=192.168.0.226 --user=root --password=123456 --database=dqdg_local_110001 -target=file --name="F:\work\project-4\main\sql\dqdg.sql"

main(Args) ->
    OptBin = build_opt_binary(Args, <<>>),
    case opt_decoder:decode(OptBin) of
        {ok, Options} ->
            case lists:keyfind(source, #rec_opt.key, Options) of
                #rec_opt{val = SType, sub = SSOpt} ->
                    case get_ddl(erlang:binary_to_atom(SType, utf8), SSOpt) of
                        {ok, SourceDLL} ->
                            case lists:keyfind(target, #rec_opt.key, Options) of
                                #rec_opt{val = TType, sub = TSOpt} ->
                                    case get_ddl(erlang:binary_to_atom(TType, utf8), TSOpt) of
                                        {ok, TargetDLL} ->
                                            case elmorm_compare:string(<<>>, TargetDLL) of
                                                {ok, Bin} ->
                                                    Bin;
                                                {error, Error} ->
                                                    ?PRINT("compare fail with reason ~p", [Error]),
                                                    erlang:halt(7)
                                            end;
                                        {false, Error} ->
                                            ?PRINT("get target dll fail with reason ~p", [Error]),
                                            erlang:halt(6)
                                    end;
                                _ ->
                                    ?PRINT("target not found", []),
                                    erlang:halt(5)
                            end;
                        {false, Error} ->
                            ?PRINT("get source dll fail with reason ~p", [Error]),
                            erlang:halt(4)
                    end;
                _ ->
                    ?PRINT("source not found", []),
                    erlang:halt(3)
            end;
        {false, Error} ->
            ?PRINT("decode args with reason ~p", [Error]),
            erlang:halt(2)
    end.


build_opt_binary([], Result) -> Result;
build_opt_binary([H | T], Result) ->
    build_opt_binary(T, <<Result/binary, (erlang:list_to_binary(H))/binary, " ">>).

get_ddl(mysql, Options) ->
    DefaultOpt = [
        {host, "localhost"},
        {port, 3306},
        {user, "root"},
        {password, ""},
        {database, ""}
    ],
    case get_mysql_options(DefaultOpt, Options, []) of
        {ok, MySqlOpts} ->
            ParentPid = self(),
            F = fun() ->
                case mysql:start_link(MySqlOpts) of
                    {ok, Conn} ->
                        case mysql:query(Conn, <<"SHOW TABLES;">>, ?TIMEOUT) of
                            {ok, _Fields, Rows} ->
                                AllDLL =
                                    lists:foldl(fun([XTableName], InAcc) ->
                                        XSQL = <<"SHOW CREATE TABLE ", XTableName/binary, ";">>,
                                        {ok, _XFields, [[_, XDDL]]} = mysql:query(Conn, XSQL, ?TIMEOUT),
                                        ParentPid ! continue,
                                        [<<"\n\n">>, XDDL | InAcc]
                                    end, [], Rows),
                                ParentPid ! {ok, iolist_to_binary(lists:reverse(AllDLL))};
                            _ ->
                                ParentPid ! {false, db_error}
                        end;
                    {false, Error} ->
                        ParentPid ! {false, Error}
                end
            end,
            erlang:spawn(F),
            wait_mysql_result();
        {false, Error} ->
            {false, Error}
    end;
get_ddl(file, Opts) ->
    case maps:get(name, Opts, undefined) of
        undefined ->
            {false, no_file_name};
        FileName ->
            case file:read_file(FileName) of
                {ok, Binary} ->
                    {ok, Binary};
                {error, Reason} ->
                    {false, Reason}
            end
    end;
get_ddl(Type, _Opts) ->
    {false, {not_support_type, Type}}.

wait_mysql_result() ->
    receive
        continue -> wait_mysql_result();
        {ok, DLL} -> {ok, DLL};
        {false, Error} -> {false, Error}
    after
        2000 ->
            {false, timeout}
    end.

get_mysql_options([], _, Result) -> {ok, Result};
get_mysql_options([{host, Default} | T], Options, Result) ->
    case maps:get(host, Options, undefined) of
        undefined ->
            get_mysql_options(T, Options, [{host, Default} | Result]);
        Val ->
            get_mysql_options(T, Options, [{host, erlang:binary_to_list(Val)} | Result])
    end;
get_mysql_options([{port, Default} | T], Options, Result) ->
    case maps:get(port, Options, undefined) of
        undefined ->
            get_mysql_options(T, Options, [{port, Default} | Result]);
        Val ->
            try
                Port = erlang:binary_to_integer(Val),
                true = (Port >= 0 andalso Port =< 65535),
                get_mysql_options(T, Options, [{port, Port} | Result])
            catch
                _:_ ->
                    {false, port_error}
            end
    end;
get_mysql_options([{user, Default} | T], Options, Result) ->
    case maps:get(user, Options, undefined) of
        undefined ->
            get_mysql_options(T, Options, [{user, Default} | Result]);
        Val ->
            get_mysql_options(T, Options, [{user, erlang:binary_to_list(Val)} | Result])
    end;
get_mysql_options([{password, _} | T], Options, Result) ->
    case maps:get(password, Options, undefined) of
        undefined ->
            {false, no_password};
        Val ->
            get_mysql_options(T, Options, [{password, erlang:binary_to_list(Val)} | Result])
    end;
get_mysql_options([{database, _} | T], Options, Result) ->
    case maps:get(database, Options, undefined) of
        undefined ->
            {false, no_database};
        Val ->
            get_mysql_options(T, Options, [{database, erlang:binary_to_list(Val)} | Result])
    end.