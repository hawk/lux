%% Simple chat server:
%%
%% erl -sname <SERVERNODE> -s chatty server
%% erl -sname <CLIENTNODE> -s chatty client <SERVERNODE>

-module(chatty).

-export([server/0, client/1]).

-define(SERVER, ?MODULE).

display(Format, Args) ->
    Text = ff(Format, Args),
    ok = io:format(Text),
    Text.

ff(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Server side

-record(client,
        {
         nick :: list(),
         pid :: pid(),
         ref :: reference()
        }).

-record(server_state,
        {
         room :: string(),
         log_file :: string(),
         log_fd :: term(),
         clients = [] :: [#client{}]
        }).

server() ->
    Node = node(),
    case split_node(Node) of
        local ->
            display("<ERROR> Local node. Forgot to start erl with -s?\n",
                    []),
            exit(no_network);
        {Name, _Host} ->
            display("Starting server ~s...\n", [Name]),
            timer:sleep(timer:seconds(3)),
            spawn_link(fun() -> server_init(Name) end)
    end.

server_init(Room) ->
    register(?SERVER, self()),
    LogFile = atom_to_list(?MODULE) ++ "_" ++ Room ++ ".log",
    SS = init_log(#server_state{room = Room,
                                log_file = LogFile}),
    log(SS, "Server started.\n", []),
    server_loop(SS).

server_loop(#server_state{clients = Clients} = SS) ->
    receive
        {join, From, Nick} ->
            Ref = monitor(process, From),
            C = #client{nick = Nick, pid = From, ref = Ref},
            log(SS, "Client ~s joined the room.\n", [Nick]),
            Room = SS#server_state.room,
            Greeting = "Welcome to the chat room " ++ Room ++ "!!!",
            From ! {joined, self(), Greeting},
            Info = ff("~s: Client joined.", [Nick]),
            broadcast_display(SS, Info, [From]),
            server_loop(SS#server_state{clients = [C | Clients]});
        {say, From, Nick, Text} ->
            log(SS, "Client ~s said ~s\n", [Nick, Text]),
            OthersText = ff("~s: ~s", [Nick, Text]),
            broadcast_display(SS, OthersText, [From]),
            server_loop(SS);
        {'DOWN', _Ref, process, Pid, Info} ->
            case lists:keyfind(Pid, #client.pid, SS#server_state.clients) of
                false ->
                    log(SS, "<ERROR> Got DOWN from unknown process ~p: ~p\n",
                        [Pid, Info]),
                    server_loop(SS);
                #client{nick = Nick} ->
                    log(SS, "Client ~s died because ~p\n", [Nick, Info]),
                    Alive = lists:keydelete(Pid, #client.pid, Clients),
                    NewSS = SS#server_state{clients = Alive},
                    Text = ff("~s: Client left.", [Nick]),
                    broadcast_display(NewSS, Text, []),
                    server_loop(NewSS)
            end;
        Unexpected ->
            log(SS, "<ERROR> Got unknown message: ~p\n", [Unexpected]),
            server_loop(SS)
    end.

broadcast_display(SS, Text, Except) ->
    broadcast(SS, {display, self(), Text}, Except).

broadcast(#server_state{clients = Clients}, Msg, Except) ->
    [Pid ! Msg || #client{pid = Pid} <- Clients,
                           not lists:member(Pid, Except)].

init_log(#server_state{log_file = LogFile} = SS) ->
    display("Trying to open log file ~s...", [LogFile]),
    case file:open(LogFile, [write]) of
        {ok, Fd} ->
            display("ok.\n", []),
            SS#server_state{log_fd = Fd};
        {error, Reason} ->
            Text = file:format_error(Reason),
            display("<ERROR> Failed to open log file ~p: ~s\n",
                    [LogFile, Text]),
            exit(shutdown)
    end.

log(#server_state{log_fd = Fd}, Format, Args) ->
    Text = ff(Format, Args),
    ok = file:write(Fd, Text),
    Text.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Client side

-record(client_state,
        {
         nick :: string(),
         server_pid :: pid(),
         input_pid :: pid(),
         timeout :: non_neg_integer()
        }).

client([ServerNode]) when is_atom(ServerNode) ->
    display("Trying to join the ~p chat room...\n", [ServerNode]),
    Self = self(),
    {MyNick, ServerPid} = get_name_and_server_pid(ServerNode),
    _ = monitor(process, ServerPid),
    ServerPid ! {join, self(), MyNick},
    {InputPid, _} =
        spawn_monitor(fun() -> input_loop(Self, 1) end),
    CS = #client_state{nick = MyNick,
                       server_pid = ServerPid,
                       input_pid = InputPid},
    client_loop(CS).

client_loop(CS) ->
    receive
        {joined, _From, Greeting} ->
            Extra = "\nEnter text and press enter. Exit chat with ^d.\n",
            reprompt(CS, Greeting ++ Extra),
            client_loop(CS);
        {display, _From, Text} ->
            reprompt(CS, "\n" ++ Text),
            client_loop(CS);
        {input, _From, Text} ->
            Nick = CS#client_state.nick,
            display("~s> ", [Nick]),
            CS#client_state.server_pid ! {say, self(), Nick, Text},
            client_loop(CS);
        {'DOWN', _Ref, process, Pid, Info}
          when Pid =:= CS#client_state.server_pid ->
            display("<ERROR> Server stopped: ~p\n", [Info]),
            exit(shutdown);
        {'DOWN', _Ref, process, Pid, Info}
          when Pid =:= CS#client_state.input_pid ->
            display("<ERROR> No more input: ~p\n", [Info]),
            exit(shutdown);
        Unexpected ->
            reprompt(CS, ff("<ERROR> Got unknown message: ~p\n", [Unexpected])),
            client_loop(CS)
    end.

reprompt(CS, Text) ->
    display("~s\n~s> ", [Text, CS#client_state.nick]).

input_loop(ClientPid, N) ->
    case io:get_line("") of
        eof when N =:= 1 ->
            %% Closed already at startup
            Text =  "<WARNING> No stdin - listen only...\n",
            ClientPid ! {display, self(), Text},
            exit(shutdown);
        eof ->
            Text = "Stdin closed - exiting...\n",
            ClientPid ! {display, self(), Text},
            exit(shutdown);
        {error, Reason} ->
            ReasonStr = file:format_error(Reason),
            Text = ff("<ERROR> Stdin error - ~s...\n", [ReasonStr]),
            ClientPid ! {display, self(), Text},
            exit(shutdown);
        Text ->
            ClientPid ! {input, self(), Text},
            input_loop(ClientPid, N+1)
    end.

get_name_and_server_pid(ServerNameOrNode) ->
    case split_node(node()) of
        local ->
            display("<ERROR> Failed to join chat room ~p. "
                    "Forgot to start erl with -s?\n",
                    [ServerNameOrNode]),
            exit(noconnection);
        {ClientName, ClientHost} ->
            case split_node(ServerNameOrNode) of
                name_only ->
                    ServerName = atom_to_list(ServerNameOrNode),
                    {ClientName, remote_whereis(ServerName, ClientHost)};
                {ServerName, ServerHost} ->
                    {ClientName, remote_whereis(ServerName, ServerHost)}
            end
    end.

split_node(Node) ->
    case string:tokens(atom_to_list(Node), "@") of
        [_Name] ->
            name_only;
        ["nonode","nohost"] ->
            local;
        [Name, Host] ->
            {Name, Host}
    end.

remote_whereis(Name, Host) ->
    Node = list_to_atom(Name ++ "@" ++ Host),
    case rpc:call(Node, erlang, whereis, [?SERVER]) of
        {badrpc,nodedown} ->
            display("<ERROR> Failed to join ~p. Is the node started?\n",
                    [Node]),
            exit(shutdown);
        undefined ->
            display("<ERROR> Failed to join ~p. Is the server started?\n",
                    [Node]),
            exit(shutdown);
        Pid when is_pid(Pid) ->
            Pid
    end.
