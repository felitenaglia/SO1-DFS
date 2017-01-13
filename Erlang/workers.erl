-module(workers).
-export([anillo/1, cerrarAnillo/1, worker/2]).
-import(lists, [last/1, map/2, keyfind/3, keytake/3, keyfind/3, keydelete/3, keymember/3, member/2, keyreplace/4]).
-import(file, [make_dir/1, list_dir/1, open/2, close/1, pread/3, pwrite/3, position/2, delete/1]).
-define(IF(C,T,F), case C of true -> T; false -> F end).

anillo(N) ->
    case N of
        0 -> error(zero_workers);
        N -> anillo(N,[])
    end.
anillo(0,[H|T]) ->
    last([H|T]) ! {setnext, H}, [H|T];
anillo(N, []) ->
    Pid = spawn(?MODULE, worker, [null,"../filesystem/" ++ integer_to_list(N)]), anillo(N-1,[Pid]);
anillo(N, [H | T]) ->
    Pid = spawn(?MODULE, worker, [H, "../filesystem/" ++ integer_to_list(N)]), anillo(N-1, [Pid | [H | T]]).


cerrarAnillo([]) -> ok;
cerrarAnillo([H | T]) ->
    H ! cerrar,
    cerrarAnillo(T).

worker(null, DirName) ->
    receive 
        {setnext, Pid} -> worker(Pid,DirName)
    end;
worker(PidNext, DirName) -> case make_dir(DirName) of
                               {error,E} when E =/= eexist -> PidNext!cerrar,
                                                         io:format("Error al crear o acceder al directorio ~p~n", [DirName]),
                                                         ok;
                               _ -> case list_dir(DirName) of
                                       {ok, F} -> Files = map(fun(X) -> {X,close} end, F),
                                                          worker(PidNext, DirName, Files, [], []);
                                       {error, _} -> PidNext!cerrar,
                                                     io:format("Error al crear o acceder al directorio ~p~n", [DirName]),
                                                     ok
                                    end
                            end.

worker(PidNext, Dir, Files, FOpen, FReserv) ->
        receive
            cerrar -> exit(ok);
            {W, Cl, IdReq, ok, Op, Res} when W == self() -> Cl ! {Cl, Cl, IdReq, ok, Op, Res};
            {W, Cl, IdReq, err, Op, Res} when W == self() -> Cl ! {Cl, Cl, IdReq, err, Op, Res};
            {W, Cl, IdReq, req, r_fd, {Fd, WD}} when W == self() ->
                        {NewFd, NewOpn} = addFd({r,Fd,WD}, FOpen),
                        Cl ! {Cl, Cl, IdReq, ok, open, NewFd},
                        worker(PidNext, Dir, Files, NewOpn, FReserv);

            {Cl, Cl, IdReq, req, lsd, _} ->
                        PidNext ! {self(), Cl, IdReq, req, w_lsd, map(fun({F,_})-> F end, Files)};
            {W, Cl, IdReq, req, w_lsd, L} when W =/= self()->
                        PidNext ! {W, Cl, IdReq, req, w_lsd, L ++ map(fun({F,_})-> F end, Files)};
            {W, Cl, IdReq, req, w_lsd, L} when W == self() -> Cl ! {Cl, Cl, IdReq, ok, lsd, L};

            {W, Cl, IdReq, req, open, N} when W =/= self() ->
                        case keyfind(N,1,Files) of
                           {_, close} -> case open(Dir ++ "/" ++ N, [read,write]) of
                                            {error, E} -> W ! {W, Cl, IdReq, err, open, E};
                                            {ok, FD} -> {NewFd, NewOpn} = addFd({l,FD,N, 0}, FOpen),
                                                        NewFiles = updateList(N, Files, open),
                                                        case (W == Cl) of
                                                           true -> Cl ! {Cl, Cl, IdReq, ok, open, NewFd};
                                                           false -> W ! {W, Cl, IdReq, req, r_fd, {NewFd, self()}}
                                                        end,
                                                        worker(PidNext, Dir, NewFiles, NewOpn, FReserv)
                                         end;
                           {_, open} -> W ! {W, Cl, IdReq, err, open, eopnfile};
                           false -> PidNext ! {?IF(W==Cl, self(), W), Cl, IdReq, req, open, N}
                        end;
            {W, Cl, IdReq, req, open, _} when W == self() -> Cl ! {Cl, Cl, IdReq, err, open, efilenexist};

            {W, Cl, IdReq, req, close, {OrigFd,Fd}} ->
                        case keytake(Fd,1,FOpen) of
                           {value, {_, {l,IO,N,_}}, NewOpn} -> close(IO),
                                                               NewFiles = updateList(N, Files, close),
                                                               W ! {W, Cl, IdReq, ok, close, OrigFd},
                                                               worker(PidNext, Dir, NewFiles, NewOpn, FReserv);
                           {value, {_, {r,RFd,WF}}, NewOpn} -> WF ! {self(), Cl, IdReq, req, close, {OrigFd,RFd}},
                                                               worker(PidNext, Dir, Files, NewOpn, FReserv);
                           false -> W ! {W, Cl, IdReq, err, close, ebadfd}
                        end;

            {W, Cl, IdReq, req, read, {Fd,Sz}} ->
                        case keyfind(Fd,1,FOpen) of
                           {_, {l,IO,N,P}} -> case pread(IO,P,Sz) of
                                                 {error, E} -> W ! {W, Cl, IdReq, err, read, E};
                                                 {ok, D} -> {ok, NPos} = position(IO, cur),
                                                            NewOpn = updateList(Fd, FOpen, {l,IO,N,NPos}),
                                                            W ! {W, Cl, IdReq, ok, read, D},
                                                            worker(PidNext, Dir, Files, NewOpn, FReserv);
                                                 eof -> {ok, NPos} = position(IO, cur),
                                                        NewOpn = updateList(Fd, FOpen, {l,IO,N,NPos}),
                                                        W ! {W, Cl, IdReq, ok, read, []},
                                                        worker(PidNext, Dir, Files, NewOpn, FReserv)
                                              end;
                           {_, {r,RFd,WF}} -> WF ! {self(), Cl, IdReq, req, read, {RFd,Sz}};
                           false -> W ! {W, Cl, IdReq, err, read, ebadfd}
                        end;

            {W, Cl, IdReq, req, write, {Fd,D}} ->
                        case keyfind(Fd,1,FOpen) of
                           {_, {l,IO,_,_}} -> case pwrite(IO,eof,D) of
                                                 {error, E} -> W ! {W, Cl, IdReq, err, write, E};
                                                 ok -> W ! {W, Cl, IdReq, ok, write, null}
                                              end;
                           {_, {r,RFd,WF}} -> WF ! {self(), Cl, IdReq, req, write, {RFd,D}};
                           false -> W ! {W, Cl, IdReq, err, write, ebadfd}
                        end;

            {W, Cl, IdReq, req, delete, N} when W =/= self() -> 
                        case keyfind(N,1,Files) of
                           {_, close} -> case delete(Dir ++ "/" ++ N) of
                                            {error, E} -> W ! {W, Cl, IdReq, err, delete, E};
                                            ok -> NewFiles = keydelete(N,1,Files),
                                                  W ! {W, Cl, IdReq, ok, delete, null},
                                                  worker(PidNext, Dir, NewFiles, FOpen, FReserv)
                                         end;
                           {_, open} -> W ! {W, Cl, IdReq, err, delete, eopnfile};
                           false -> PidNext ! {?IF(W==Cl, self(), W), Cl, IdReq, req, delete, N}
                        end;
            {W, Cl, IdReq, req, delete, _} when W == self() -> Cl ! {Cl, Cl, IdReq, err, delete, efilenexist};

            {Cl, Cl, IdReq, req, create, N} ->
                        case keymember(N,1,Files) or member(N,FReserv) of
                           true -> Cl ! {Cl, Cl, IdReq, err, create, efileexist};
                           false -> NewRes = [N | FReserv],
                                    PidNext ! {self(), Cl, IdReq, req, create, N},
                                    worker(PidNext, Dir, Files, FOpen, NewRes)
                        end;
            {W, Cl, IdReq, req, create, N} when W =/= self() ->
                        case keymember(N,1,Files) or (member(N,FReserv) andalso W < self()) of
                           false -> NewRes = FReserv -- [N],
                                    PidNext ! {W, Cl, IdReq, req, create, N},
                                    worker(PidNext, Dir, Files, FOpen, NewRes);
                           true -> W ! {W, Cl, IdReq, req, create_exists, N}
                        end;
            {W, Cl, IdReq, req, create, N} when W == self() ->
                        case member(N, FReserv) of
                                true -> NewRes = FReserv -- [N],
                                        case open(Dir ++ "/" ++ N, [write]) of
                                           {error, E} -> Cl ! {Cl, Cl, IdReq, err, create, E},
                                                         NewFiles = Files;
                                           {ok, FD} -> close(FD),
                                                       NewFiles = [{N,close} | Files],
                                                       Cl ! {Cl, Cl, IdReq, ok, create, null}
                                        end,
                                        worker(PidNext, Dir, NewFiles, FOpen, NewRes);
                                false -> Cl ! {Cl, Cl, IdReq, err, create, efileexist}
                        end;
            {W, Cl, IdReq, req, create_exists, N} when W == self()->
                        NewRes = FReserv -- [N],
                        Cl ! {Cl, Cl, IdReq, err, create, efileexist},
                        worker(PidNext, Dir, Files, FOpen, NewRes);

            {Cl, Cl, IdReq, req, bye, L} ->
                        ToClose = map(fun(Fd) -> {self(), Fd} end, L),
                        {NewOpn, NewFiles, ToCloseRes} = closeList(ToClose, FOpen, Files),
                        PidNext ! {self(),Cl, IdReq, req, bye, ToCloseRes},
                        worker(PidNext, Dir, NewFiles, NewOpn, FReserv);
            {W, Cl, IdReq, req, bye, L} when W =/= self() ->
                        {NewOpn, NewFiles, ToCloseRes} = closeList(L, FOpen, Files),
                        PidNext ! {W,Cl, IdReq, req, bye, ToCloseRes},
                        worker(PidNext, Dir, NewFiles, NewOpn, FReserv);
            {W, Cl, IdReq, req, bye, []} when W == self() ->
                        Cl ! {Cl,Cl, IdReq, ok, bye, null};
            {W, Cl, IdReq, req, bye, _} when W == self() ->
                        Cl ! {Cl,Cl, IdReq, err, bye, eclosing}

        end, worker(PidNext, Dir, Files, FOpen, FReserv).

addFd(Fd, []) ->
        {0,[{0,Fd}]};
addFd(Fd,[{Ult,UFd}|T]) ->
        {Ult+1, [{Ult+1,Fd}, {Ult,UFd} | T]}.

updateList(K, L, N) -> keyreplace(K,1,L,{K,N}).

closeList(ListToClose, FOpen, Files) -> closeList(ListToClose,FOpen, Files, []).

closeList([], FOpen, Files, Res) -> {FOpen, Files, Res};
closeList([{Pid,Fd}|T], FOpen, Files, Res) when Pid =/= self() -> closeList(T,FOpen,Files,[{Pid,Fd} | Res]);
closeList([{Pid,Fd}|T], FOpen, Files, Res) when Pid == self() ->
        case keytake(Fd,1,FOpen) of
           {value, {_, {l,IO,N,_}}, NewOpn} -> close(IO),
                                               NewFiles = updateList(N, Files, close),
                                               closeList(T, NewOpn, NewFiles, Res);
           {value, {_, {r,RFd,Wf}}, NewOpn} -> closeList(T, NewOpn, Files,[{Wf,RFd} | Res]);
           false -> closeList(T, FOpen, Files,Res)
        end.

