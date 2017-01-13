-module(server).
-export([start/0, getToken/2, esperaConexion/3]).
-import(gen_tcp, [send/2, listen/2, accept/1, shutdown/2]).
-import(lists, [map/2, all/2, splitwith/2, sublist/2, keydelete/3, keyfind/3, dropwhile/2, member/2]).
-import(workers, [anillo/1, cerrarAnillo/1]).
-define(N_WRKS, 5).
-define(TIMEOUT, 100).
-define(FILENAME_LEN, 50).

start() -> 
    Workers = anillo(?N_WRKS),
    case listen(8000, []) of
        {ok, Socket} -> spawn(?MODULE, esperaConexion, [Socket, Workers, 0]),
                        io:format("El servidor se ejecutara en segundo plano~n");
        {error, Err} -> cerrarAnillo(Workers), io:format("Error al escuchar el puerto (~p)~n", [Err])
    end.

esperaConexion(Socket, [H | T], Id) ->
    case accept(Socket) of
        {ok, ClSocket} -> io:format("~p: Conexion entrante~n", [Id]),
                          spawn(?MODULE, esperaConexion, [Socket, T ++ [H], Id+1]),
                          handleCliente(H, Id, ClSocket, false, [], 0);
        {error, _} -> errorAcceptConnection
    end.

handleCliente(Worker, Id, Socket, Conn, FDList, IdReq) -> 
    receive 
        {tcp_closed, Socket} -> Worker!{self(), self(), IdReq, req, bye, map(fun({_,N})->N end, FDList)},
                                io:format("~p: Se ha desconectado~n", [Id]), exit(ok);
        {tcp_error, Socket, Err} -> Worker!{self(), self(), IdReq, req, bye, map(fun({_,N})->N end, FDList)},
                                    io:format("~p: Error ~p~n", [Id, Err]), exit(ok);
        {_,_,ResPed,ok,open,FDOp} when ResPed < IdReq -> io:format("~p: Descartado: ~p ~p ~p~n", [Id, ok, open, FDOp]),
                                                         {_, NewFDList} = addFd(FDOp,FDList),
                                                         handleCliente(Worker, Id, Socket, Conn, NewFDList, IdReq+1);
        {_,_,ResPed,ok,close,FdCl} when ResPed < IdReq -> io:format("~p: Descartado: ~p ~p ~p~n", [Id, ok, close, FdCl]),
                                                          NewFDList = keydelete(list_to_integer(FdCl),1,FDList), 
                                                          handleCliente(Worker, Id, Socket, Conn, NewFDList, IdReq+1);
        {_,_,ResPed,R,Op,D} when ResPed < IdReq -> io:format("~p: Descartado: ~p ~p ~p~n", [Id, R, Op, D]), ok;
        {tcp, Socket, Data} ->
            case getToken(trim(Data), 5) of
                ["CON" | R] ->
                       case R of
                          [] when Conn == false -> send(Socket, "OK ID " ++ integer_to_list(Id)++"\n"),
                                                   handleCliente(Worker, Id, Socket, true, FDList, IdReq);
                          [] when Conn == true -> send(Socket, errString(ealrcon)),
                                                  handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1);
                          _ -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["BYE" | R] ->
                       case R of
                          [] -> Worker!{self(), self(), IdReq, req, bye, map(fun({_,N})->N end, FDList)};
                          _ -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                _ when Conn == false ->
                       send(Socket, errString(enotcon)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1);

                ["LSD" | R] ->
                       case R of
                          [] -> Worker!{self(), self(), IdReq, req, lsd, null};
                          _ -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["DEL", Nombre | R] ->
                       case (R == []) and checkName(Nombre) of
                          true -> Worker!{self(), self(), IdReq, req, delete, Nombre};
                          false -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["CRE", Nombre | R] ->
                       case (R == []) and checkName(Nombre) and (length(Nombre) =< ?FILENAME_LEN) of
                          true -> Worker!{self(), self(), IdReq, req, create, Nombre};
                          false -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["OPN", Nombre | R] ->
                       case (R == []) and checkName(Nombre) of
                          true -> Worker!{self(), self(), IdReq, req, open, Nombre};
                          false -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["WRT", "FD", Fd, "SIZE", Size, R] -> 
                       case checkNat(Fd) and checkNat(Size) of
                          true -> case keyfind(list_to_integer(Fd),1,FDList) of
                                     {_, ReFd} -> Worker!{self(), self(), IdReq, req, write, {ReFd, sublist(R,list_to_integer(Size))}};
                                     false -> send(Socket, errString(ebadfd)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                                  end;                          
                          false -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["REA", "FD", Fd, "SIZE", Size | R] ->
                       case (R == []) and checkNat(Fd) and checkNat(Size) of
                          true -> case keyfind(list_to_integer(Fd),1,FDList) of
                                     {_, RealFd} -> Worker!{self(), self(), IdReq, req, read, {RealFd, list_to_integer(Size)}};
                                     false -> send(Socket, errString(ebadfd)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                                  end;                     
                          false -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;

                ["CLO", "FD", Fd | R] ->
                       case (R == []) and checkNat(Fd) of
                          true -> case keyfind(list_to_integer(Fd),1,FDList) of
                                     {_, RealFd} -> Worker!{self(), self(), IdReq, req, close, {Fd,RealFd}};
                                     false -> send(Socket, errString(ebadfd)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                                  end;
                          false -> send(Socket, errString(ebadarg)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
                       end;
                [[]] -> handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1);
                _ -> send(Socket, errString(ebadcmd)), handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1)
            end,
            receive
                {_, _, IdReq, ok, bye, _} -> send(Socket, "OK\n"), shutdown(Socket,read_write);
                {_, _, IdReq, ok, lsd, Lista} -> send(Socket, "OK " ++ string:join(Lista," ")++"\n");
                {_, _, IdReq, ok, open, FdOp} -> {NewFd, NewFDList} = addFd(FdOp,FDList),
                                                 send(Socket, "OK FD " ++ integer_to_list(NewFd)++"\n"),
                                                 handleCliente(Worker, Id, Socket, Conn, NewFDList, IdReq+1);
                {_, _, IdReq, ok, read, Buff} -> send(Socket, "OK SIZE " ++ integer_to_list(length(Buff)) ++ " " ++ Buff ++"\n");
                {_, _, IdReq, ok, close, FdCl} -> NewFDList = keydelete(list_to_integer(FdCl),1,FDList), 
                                                  send(Socket, "OK\n"),
                                                  handleCliente(Worker, Id, Socket, Conn, NewFDList, IdReq+1);
                {_, _, IdReq, ok, _, _} -> send(Socket, "OK\n");
                {_, _, IdReq, err, _, E} -> send(Socket, errString(E))
                after ?TIMEOUT -> send(Socket, errString(etime))
            end
    end, handleCliente(Worker, Id, Socket, Conn, FDList, IdReq+1).

isSpace(C) -> member(C, [$\s,  $\t, $\n, $\r, $\f, $\v]).

trim(S) -> dropwhile(fun(C) -> isSpace(C) end, S).

% Devuelve una tupla formada por el primer token y el resto de la cadena. No se utiliza la funcion strings:token ya que esta
% divide a toda la cadena en tokens, y puede afectar los espacios en el comando WRT
% La función con aridad 2 devuelve los N primeros tokens
getToken([]) -> {[],[]};
getToken(S) -> {T, Res} = splitwith(fun(C) -> not isSpace(C) end, S), {T, trim(Res)}.
getToken(S, 1) -> {T, Res} = getToken(S),
                  case Res of [] -> [T];
                              _ -> [T , Res]
                  end;
getToken(S, N) -> {T, Res} = getToken(S),
                  case Res of [] -> [T];
                              _ -> [T | getToken(Res,N-1)]
                  end.

errString(Cod) ->
    case Cod of
        ebadarg -> Res = "EBADARG";
        ebadcmd -> Res = "EBADCMD";
        enotcon -> Res = "ENOTCONNECTED";
        ealrcon -> Res = "EALREADYCONNECTED";
        ebadfd ->  Res = "EBADFD";
        eopnfile -> Res = "EFILEOPENED";
        efileexist -> Res = "EFILEEXIST";
        efilenexist -> Res = "EFILENOTEXIST";
        etime -> Res = "ETIME";
        _ -> Res = "UNKNOWN: " ++ atom_to_list(Cod)
    end, "ERROR " ++ Res ++ "\n".

addFd(Fd, []) ->
        {0,[{0,Fd}]};
addFd(Fd,[{Ult,UFd}|T]) ->
        {Ult+1, [{Ult+1,Fd}, {Ult,UFd} | T]}.

checkName(N) -> all(fun(C) -> (C =< $z andalso C >= $a) or (C =< $Z andalso C >= $A) or (C =< $9 andalso C >= $0) or (C == $.) end, N).

checkNat(N) -> all(fun(C) -> C =< $9 andalso C >= $0 end, N).

