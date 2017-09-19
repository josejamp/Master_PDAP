-module(serv_maestro).

-export([init/1, start/0, terminate/2, handle_call/3,
  handle_cast/2, handle_info/2, code_change/3, 
  nueva_partida/5, partidas/0, consulta_puntuaciones/0, 
  actualiza_puntuaciones/1, borra_puntuaciones/0]).

-behaviour(gen_server).
-define(SERVERNAME, ?MODULE).

% Autor: Jose Javier Martinez Pages

% Este modulo representa el servidor maestro, que se encarga de crear servidores de partidas
% y de guardar en una tabla DETS la puntuación de los usuarios de las partidas.
% El estado consiste en una lista de los servidores de juego abiertos, y en una referencia a la tabla DETS.

% Iniciar el servidor no requiere de ningun parametro.
start() -> gen_server:start_link({global, ?SERVERNAME} ,?MODULE, [],[]).

% En la inicializacion se abre la tabla DETS, se guarda como "punt.dets" con
% opciones de claves duplicadas, para que la puntuacion simule la de los juegos arcade.
init(_) ->  
  io:format("Servidor maestro inicializado!~n"),
  {ok,BaseDatos} = dets:open_file(punt, [{file, "punt.dets"},{type, duplicate_bag}]),
  {ok, {[],BaseDatos}}.
  
% Para la llamada de neuva partida se inicializa un servidor de partida con los
% parametros enviados y se añade su PID a la lista de partidas abiertas.
% Si se trata de crear una partida con menos de tres jugadores, devuelve un error.
handle_call({nueva_partida, {N,M,G,P,T}}, {_Pid,_Tag}, {L,BaseDatos}) -> 
	case P >= 3 of
		true ->
			case serv_partida:start(N,M,G,P,T) of
				{ok, Pid} ->
					{reply, {ok,Pid}, {[Pid|L],BaseDatos}};
				_ ->
					{reply, error, {L,BaseDatos}}
			end;
		false ->
			{reply, error, {L,BaseDatos}}
	end;

% Manejador del mensaje para actualizar puntuaciones y acabar la partida.
% Simplementa inserta en la tabla una lista con nombres de usuarios y sus puntuaciones,
% y quita la partida de la listas.
handle_call({actualiza_puntuaciones,Players}, {Pid,_Tag}, {L,BaseDatos}) -> 
	dets:insert(BaseDatos, Players),
	{reply, ok, {[X || X <- L, X =/= Pid],BaseDatos}};
	
% Peticion para conocer los PIDs de las partidas abiertas
handle_call(partidas, {_Pid,_Tag}, {L,BaseDatos}) -> 
	{reply, {ok,L}, {L,BaseDatos}};

% Si el mensaje no cuadra con ninguno, se muestra un mensaje.
handle_call(Request, Info, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.

% Si se pide al servidor consultar las puntuaciones, este acede a la tabla DETS,
% consigue las puntuaciones, las ordena de mayor a menor y las muestra.
handle_cast(consulta_puntuaciones, {L,BaseDatos}) ->
	Consulta = dets:match(BaseDatos, {'$1','$2'}),
	Ordenada = lists:sort( fun([_,P1],[_,P2]) -> (P1 > P2) end, Consulta),
	lists:foreach( fun([N,P]) -> io:format("Jugador: ~s   ~w ~n",[N,P]) end, Ordenada),
	{noreply, {L,BaseDatos}};

% Peticion para borrar la tabla de puntuaciones.
handle_cast(borra_puntuaciones, {L,BaseDatos}) ->
	dets:delete_all_objects(BaseDatos),
	{noreply, {L,BaseDatos}};
	
% Manejador por defecto de cast
handle_cast(Request, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.
	
handle_info(Message, State) ->
  io:format("Unexpected message: ~w~n", [Message]),
  {noreply, State}.

% Cuando el servidor termina cierra la tabla DETS
terminate(Reason, {_, BaseDatos}) ->
  dets:close(BaseDatos),
  io:format("Work server finished.~n"),
  io:format("Reason: ~w~n", [Reason]).

code_change(PreviousVersion, State, _Extra) ->
  io:format("Code change from ~w~n", [PreviousVersion]),
  {ok, State}.
	
% Llamada para crear una nueva partida
nueva_partida(N,M,G,P,T) ->
	gen_server:call({global, ?SERVERNAME}, {nueva_partida,{N,M,G,P,T}}).
	
% Llamada para consultar las partidas abiertas
partidas() ->
	gen_server:call({global, ?SERVERNAME}, partidas).

% Llamada para actualizar las puntuaciones
actualiza_puntuaciones(L) ->
	gen_server:call({global, ?SERVERNAME}, {actualiza_puntuaciones,L}).

% Llamada para consultar las puntuaciones
consulta_puntuaciones() ->
	gen_server:cast({global, ?SERVERNAME}, consulta_puntuaciones).

% Llamada para borrar las puntuaciones
borra_puntuaciones() ->
	gen_server:cast({global, ?SERVERNAME}, borra_puntuaciones).

