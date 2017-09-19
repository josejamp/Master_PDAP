-module(serv_partida).

-export([init/1, start/5, terminate/2, handle_call/3,
  handle_cast/2, handle_info/2, code_change/3, 
  unirse/2, desplaza_cueva/4, habla_cueva/3,
  repara/2, rompe/2, galeria_derrumbada/2,
  cueva_derrumbada/2, tiempo_agotado/1]).

-behaviour(gen_server).

% Autor: Jose Javier Martinez Pages

% Este modulo representa un proceso de partida. Se encarga de aceptar a los jugadores en la partida,
% de controlar cuando se acaba y quien gana; y de mantener cierta información necesaria que los procesos de juego no
% mantienen, tal y como la posición absoluta de un jugador en la cueva, o los jugadores que están presentes en la
% partida.

% Record para representar la información que se guarda para cada jugador que se une
% a la partida: su PID, su nombre, el PID del proceso de juego, la posicion que ocupan
% en el mapa, y su rol
-record(playerInfo, {playerPID, playerName, gamePID, position, rol}).

% Record que representa el estado del servidor. Contiene los parametros de creacion de la partida 
% (dimensiones, numero de aristas, numero de jugadores y tiempo e partida), el grafo que representa
% el mapa de la cueva, el numero de jugadores actualmente unidos a la partida, los roles disponibles
% para los jugadores que se unan, y si la partida ha acabado o no
-record(state, {params, graph, players, numPlayers, availableRols, terminado}).

% La funcion que inicia el servidor necesita las dimensiones de la cueva, el numero de conexiones
% entre cuevas, el numero de jugadores y el tiempo que dura la partida
start(N,M,G,P,T) -> gen_server:start_link(?MODULE, {N,M,G,P,T},[]).

% En la inicializacion se crea la cueva, se modifican las galerias para que esten cerradas, y se
% usa el numero de jugadores para calcular los roles disponibles
init({N,M,G,P,T}) ->  
  case generador_mapa:genera_mapa(N,M,G) of
	error ->
		{stop, normal};
	Mapa ->
		 io:format("Servidor partida inicializado!~n"),
		{ok, #state{params = {N,M,G,P,T},graph = inicializaGaleria(Mapa),players = [], numPlayers = 0, availableRols = inicializaRoles(P), terminado = false}}
  end.
  
% Cuando un jugador quiere unirse a la partida se comprueba si ya se han unido todos los jugadores posibles o si ya hay un jugador unido con su mismo nombre
handle_call({unirse, Nombre}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State ) ->
	NamePlayers = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.playerName == Nombre],
	case (NumPlayers < P) of
		true ->
			case (length(NamePlayers)==0) of
				true ->
					io:format("Posicion del nuevo jugador es: ~w~n", [I]),
					RolJugador = otorgaRol(P, NumPlayers, Rols), % se otorga un rol al jugador
					case (NumPlayers==(P-1)) of %si es el último jugador que puede entrar en la partida
						true ->
							{ok, ProcPID} = proc_juego:start(Nombre, Pid, self(), RolJugador, consigueCueva(I, Aristas), true), % se comienza un proceso de juego para el jugador
							PlayersList = [#playerInfo{playerPID = Pid, playerName = Nombre, gamePID = ProcPID, position = I, rol = RolJugador} | Players], %se añade al jugador a la lista
							ServerPID = self(),
							spawn(fun() -> controlaTiempo(T*1000,ServerPID) end), %se inicia la partida y el cronometro comienza a contar
							AllPlayers = [Player#playerInfo.gamePID || Player <- Players], % se avisa a todos los jugadores de que la partida ha empezado
							lists:foreach( fun(X) -> proc_juego:empieza_partida(X) end, AllPlayers);
						false ->	%si todavia se pueden unir mas jugadores solo se comienza el proceso del juego y se añade al jugador a la lista
							{ok, ProcPID} = proc_juego:start(Nombre, Pid, self(), RolJugador, consigueCueva(I, Aristas), false),
							PlayersList = [#playerInfo{playerPID = Pid, playerName = Nombre, gamePID = ProcPID, position = I, rol = RolJugador} | Players]
					end,
					{reply, {ok,ProcPID}, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = PlayersList, numPlayers = (NumPlayers + 1), availableRols = eliminaRol(RolJugador,Rols), terminado = false}};
				false ->
					{reply, {error,"Ya hay un jugador con ese nombre"}, State}
				end;
		false ->
			{reply, {error,"La partida ya está llena"}, State}
	end;

% Un jugador con un nombre posicionado en una cueva se quiere mover en una direccion, aqui nos encargamos de avisar a los otros jugadores,
% comprobar si la neuva posicion tiene oro, y de darle al jugador su nueva cueva	
handle_call({desplaza_cueva, Nombre, Cueva, Dir}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State) ->
	[{OldPos, RolPlayer}] = [{Player#playerInfo.position, Player#playerInfo.rol} || Player <- Players, Player#playerInfo.gamePID == Pid ],
	{NuevaCueva, NuevaPos} = nuevaCueva(Dir, Aristas, Players, Pid), % conseguimos la nueva cueva para el jugador (la cueva solo tiene informacion de las galerias y de si estan abiertas o cerradas)
	io:format("Nueva posicion es: ~w~n", [NuevaPos]),
	io:format("Nueva cueva es: ~w~n", [NuevaCueva]),
	% Se informa a los jugadores de la antigua cueva y de la neuva del movimiento del jugador
	NamePlayersNuevaCueva = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.position == NuevaPos],
	PlayersNuevaCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == NuevaPos],
	PlayersViejaCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == OldPos, Player#playerInfo.gamePID =/= Pid],
	lists:foreach( fun(X) -> proc_juego:muestra(X,io_lib:format("Ha salido de la cueva el jugador ~s ",[Nombre])) end, PlayersViejaCueva),
	lists:foreach( fun(X) -> proc_juego:muestra(X,io_lib:format("Ha llegado a la cueva el jugador ~s ",[Nombre])) end, PlayersNuevaCueva),
	% Si el jugador acaba en la posicion del oro y no es un saboteador, los buenos ganan
	case ((NuevaPos == GP) and (RolPlayer =/= saboteador)) of 
		true ->
			% Se informa a los jugadores del jugador que ha encontrado el oro y de que la partida ha acabado
			AllPlayers = [Player#playerInfo.gamePID || Player <- Players],
			NotSender = [Player#playerInfo.gamePID || Player <- Players,  Player#playerInfo.gamePID =/= Pid],
			lists:foreach( fun(X) -> proc_juego:muestra(X,io_lib:format("Ha ganado la partida el jugador ~s ",[Nombre])) end, AllPlayers),
			% Se cierran los procesos de todos los jugadores menos del que esta esperando respuesta, este se cerrara cuando conteste al jugador
			lists:foreach( fun(X) -> proc_juego:fin_partida(X) end, NotSender),
			ok = guardaPuntuaciones(Players, minero), % se guardan las puntuaciones
			NewState = #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = actualizaPlayers(Players, NuevaPos, Pid), numPlayers = NumPlayers, availableRols = Rols, terminado = true},
			{reply, fin, NewState};
		false ->
			NewState = #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = actualizaPlayers(Players, NuevaPos, Pid), numPlayers = NumPlayers, availableRols = Rols, terminado = false},
			{reply, {ok, {NuevaCueva, NamePlayersNuevaCueva}}, NewState}
	end;
	
% El jugador quiere enviar un mensaje a los jugadores de su cueva,
% se buscan los jugadores que comparten su posicion, y se les envia el mensaje
handle_call({habla_cueva, Nombre, Mensaje}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = Graph, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State) ->
	[Pos] = [Player#playerInfo.position || Player <- Players, Player#playerInfo.gamePID == Pid ],
	PlayersCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == Pos, Player#playerInfo.gamePID =/= Pid],
	lists:foreach( fun(X) -> proc_juego:muestra(X,io_lib:format("~s : ~s.",[Nombre, Mensaje])) end, PlayersCueva),
	{reply, ok, State};
	
% Si un jugador busca reparar a otro, se ha de comprobar si el otro jugador esta en la cueva
handle_call({repara, Nombre}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = Graph, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State) ->
	[{PosReparador, NombreReparador}] = [ {Player#playerInfo.position, Player#playerInfo.playerName} || Player <- Players, Player#playerInfo.gamePID == Pid ],
	NamePlayersCueva = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.position == PosReparador],
	% Se comprueba si el jugador a reparar esta en la cueva del jugador
	case lists:member(Nombre, NamePlayersCueva) of
		true ->
			[PidVictima] = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.playerName == Nombre ],
			% Se envia un mensaje al que se quiere reparar indicando que va a ser reparado
			case proc_juego:se_reparado(PidVictima, NombreReparador) of
				ok ->
					{reply, ok, State};
				E ->
					{reply, E, State}
			end;
		false ->
			{reply, {error, "El jugador a reparar no se encuentra en la cueva"}, State}
	end;
	
% Romper a un jugador es similar a repararlo, y se comprueba si esta en la cueva que el que lo quiere romper
handle_call({rompe, Nombre}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = Graph, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State) ->
	[{PosReparador, NombreSaboteador}] = [ {Player#playerInfo.position, Player#playerInfo.playerName} || Player <- Players, Player#playerInfo.gamePID == Pid ],
	NamePlayersCueva = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.position == PosReparador],
	case lists:member(Nombre, NamePlayersCueva) of
		true ->
			[PidVictima] = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.playerName == Nombre ],
			case proc_juego:se_roto(PidVictima, NombreSaboteador) of
				ok ->
					{reply, ok, State};
				E ->
					{reply, E, State}
			end;
		false ->
			{reply, {error, "El jugador a reparar no se encuentra en la cueva"}, State}
	end;
	
% Al derrumbar una galeria en una direccion, se ha de actualizar el mapa de la cueva,
% y se ha de informar a los jugadores de las cuevas afectadas
handle_call({galeria_derrumbada, Dir}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State) ->
	% Se coge la posicion de la cueva en la que esta el jugador
	[OldPos] = [Player#playerInfo.position || Player <- Players, Player#playerInfo.gamePID == Pid ],
	% Se coge la posicion de la cueva conectada con la cueva del jugador por la galeria
	{_, NuevaPos} = nuevaCueva(Dir, Aristas, Players, Pid),
	% Se actualiza el mapa de la cueva
	NewState = #state{params = {N,M,G,P,T}, graph = actualizaGaleriaAbirta(OldPos, Dir, {{Vertices, Aristas},{oro, GP},{inicial,I}}), players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false},
	% Se informa a los jugadores de ambaas cuevas del cambio
	PlayersNuevaCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == NuevaPos],
	PlayersViejaCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == OldPos, Player#playerInfo.gamePID =/= Pid],
	lists:foreach( fun(X) -> proc_juego:galeria_actualizada(X,negateDirection(Dir)) end, PlayersNuevaCueva),
	lists:foreach( fun(X) -> proc_juego:galeria_actualizada(X,Dir) end, PlayersViejaCueva),
	{reply, ok, NewState};
	
% Derrumbar una cueva incluye moverse en una dirección, y actualizar el mapa e informar a los jugadores afectados
handle_call({cueva_derrumbada, Dir}, {Pid, _Tag}, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false} = State) ->
	% Al igual que al moverse, que se accede a la informacion de la cueva nueva para pasarsela al jugador
	[{OldPos, RolPlayer}] = [{Player#playerInfo.position, Player#playerInfo.rol} || Player <- Players, Player#playerInfo.gamePID == Pid ],
	[Nombre] = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.gamePID == Pid ],
	{NuevaCueva, NuevaPos} = nuevaCueva(Dir, Aristas, Players, Pid),
	NamePlayersNuevaCueva = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.position == NuevaPos],
	PlayersNuevaCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == NuevaPos],
	PlayersViejaCueva = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == OldPos, Player#playerInfo.gamePID =/= Pid],
	% Si no hay jugadores en la cueva es posible derrumbarla
	case length(PlayersViejaCueva) of
		0 ->
			informaCuevaDerruida(Players, OldPos), % Se informa a los jugadores adyacentes a la cueva de que ha sido derruida
			lists:foreach( fun(X) -> proc_juego:muestra(X,io_lib:format("Ha llegado a la cueva el jugador ~s ",[Nombre])) end, PlayersNuevaCueva),
			% Se comprueba si el jugador al moverse al derrumbar la cueva ha ganado la partida al encontrar el oro
			case ((NuevaPos == GP) and (RolPlayer =/= saboteador)) of
				true ->
					AllPlayers = [Player#playerInfo.gamePID || Player <- Players],
					NotSender = [Player#playerInfo.gamePID || Player <- Players,  Player#playerInfo.gamePID =/= Pid],
					lists:foreach( fun(X) -> proc_juego:muestra(X,io_lib:format("Ha ganado la partida el jugador ~s ",[Nombre])) end, AllPlayers),
					% Se cierran los procesos de todos los jugadores menos del que esta esperando respuesta, este se cerrara cuando conteste al jugador
					lists:foreach( fun(X) -> proc_juego:fin_partida(X) end, NotSender),
					NewState = #state{params = {N,M,G,P,T}, graph = actualizaGaleriaPermanente(OldPos, Dir, {{Vertices, Aristas},{oro, GP},{inicial,I}}), players = actualizaPlayers(Players, NuevaPos, Pid), numPlayers = NumPlayers, availableRols = Rols, terminado = true},
					guardaPuntuaciones(Players, minero),
					{reply, fin, NewState};
				false ->
					NewState = #state{params = {N,M,G,P,T}, graph = actualizaGaleriaPermanente(OldPos, Dir, {{Vertices, Aristas},{oro, GP},{inicial,I}}), players = actualizaPlayers(Players, NuevaPos, Pid), numPlayers = NumPlayers, availableRols = Rols, terminado = false},
					{reply, {ok, {NuevaCueva, NamePlayersNuevaCueva, negateDirection(Dir)}}, NewState}
			end;
		_ ->
			{reply, {error,"No se puede derrumbar la cueva porque hay más jugadores en ella"}, State}
	end;

% Si llega cualquier mensaje cuando la partida ya ha acabado se informa al jugador de que la partida ya ha acabado
handle_call(Request, Info, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = true} = State) ->
  {reply, {error,"Partida ya finalizada"}, State};
  
% Manejador de mensajes por defecto
handle_call(Request, Info, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.
  
% Cuenado se ha acabado el tiempo se informa a los jugadores de que los saboteadores han ganado
handle_cast(tiempo_agotado, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = false}) ->
  AllPlayers = [Player#playerInfo.gamePID || Player <- Players],
  lists:foreach( fun(X) -> proc_juego:muestra(X,"Tiempo finalizado. Los saboteadores ganaron.") end, AllPlayers),
  lists:foreach( fun(X) -> proc_juego:fin_partida(X) end, AllPlayers),
  guardaPuntuaciones(Players, saboteador),
  {stop, normal, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = true}};
  
% Si llega cualquier mensaje cuando la partida ya ha acabado se informa al jugador de que la partida ya ha acabado
handle_cast(Request, #state{params = {N,M,G,P,T}, graph = {{Vertices, Aristas},{oro, GP},{inicial,I}}, players = Players, numPlayers = NumPlayers, availableRols = Rols, terminado = true} = State) ->
  io:format("Partida ya finalizada"),
  {noreply, State};
  
% Manejador de cast por defecto
handle_cast(Request, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.
  
handle_info(Message, State) ->
  io:format("Unexpected message: ~w~n", [Message]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Servidor de partida cerradp.~n"),
  io:format("Motivo: ~w~n", [Reason]).

code_change(PreviousVersion, State, _Extra) ->
  io:format("Code change from ~w~n", [PreviousVersion]),
  {ok, State}.
  

 
%
% Interfaces para mandar mensajes al servidor
% 
unirse(PartidaPID, NombreJugador) ->
	gen_server:call(PartidaPID, {unirse, NombreJugador}).
	
desplaza_cueva(PartidaPID, Nombre, Cueva, Dir) ->
	gen_server:call(PartidaPID, {desplaza_cueva, Nombre, Cueva, Dir}).
	
habla_cueva(PartidaPID, Nombre, Mensaje) ->
	gen_server:call(PartidaPID, {habla_cueva, Nombre, Mensaje}).
	
repara(PartidaPID, Nombre) ->
	gen_server:call(PartidaPID, {repara, Nombre}).
	
rompe(PartidaPID, Nombre) ->
	gen_server:call(PartidaPID, {rompe, Nombre}).
	
galeria_derrumbada(PartidaPID, Dir) ->
	gen_server:call(PartidaPID, {galeria_derrumbada, Dir}).
	
cueva_derrumbada(PartidaPID, DirMove) ->
	gen_server:call(PartidaPID, {cueva_derrumbada, DirMove}).
	
tiempo_agotado(PartidaPID) ->
	gen_server:cast(PartidaPID, tiempo_agotado).


% Funcion que envia las puntuaciones al servidor maestro.
% Cero puntos para los perdedores, y 100 para los ganadores
guardaPuntuaciones(Players, Rol) ->
  PlayersPoints = [Player#playerInfo.playerName || Player <- Players, Player#playerInfo.rol == Rol],
  PlayersNoPoints = [{Player#playerInfo.playerName, 0} || Player <- Players, Player#playerInfo.rol =/= Rol],
  serv_maestro:actualiza_puntuaciones([{PlayerName, 100} || PlayerName <- PlayersPoints] ++ PlayersNoPoints).

% Funcion que informa a los jugadores de que la cueva adyacente a la suya en una direccion ha sido destruida
informaCuevaDerruida(Players, OldPos) ->
	PlayersNorte = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == toVertice(OldPos,norte)],
	PlayersSur = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == toVertice(OldPos,sur)],
	PlayersEste = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == toVertice(OldPos,este)],
	PlayersOeste = [Player#playerInfo.gamePID || Player <- Players, Player#playerInfo.position == toVertice(OldPos,oeste)],
	lists:foreach( fun(X) -> proc_juego:cueva_actualizada(X,sur) end, PlayersNorte),
	lists:foreach( fun(X) -> proc_juego:cueva_actualizada(X,norte) end, PlayersSur),
	lists:foreach( fun(X) -> proc_juego:cueva_actualizada(X,oeste) end, PlayersEste),
	lists:foreach( fun(X) -> proc_juego:cueva_actualizada(X,este) end, PlayersOeste).
  
% Funcion que inicializa los roles en funcion del numero de jugadores
inicializaRoles(P) ->
	[{trunc(P/3), saboteador},{1,tecnico},{P-(trunc(P/3))-1,minero}].
	
% Funcion que asigna un rol a un nuevo jugador de manera aleatoria
% Los elementos de la lista tienen un formato: {numero_disponible, rol}
otorgaRol(P, NumPlayers, [{NS,saboteador},{NT,tecnico},{_,minero}]) ->
	I = rand:uniform(P - NumPlayers),
	case I =< NS of
		true ->
			saboteador;
		false ->
			case I > (NS + NT) of
				true ->
					minero;
				false ->
					tecnico
			end
	end.
	
% Funcion que consume un rol de la lista de roles disponibles
eliminaRol(saboteador, [{NS,saboteador},{NT,tecnico},{NM,minero}]) ->
	[{NS-1,saboteador},{NT,tecnico},{NM,minero}];
eliminaRol(tecnico, [{NS,saboteador},{NT,tecnico},{NM,minero}]) ->
	[{NS,saboteador},{NT-1,tecnico},{NM,minero}];
eliminaRol(minero, [{NS,saboteador},{NT,tecnico},{NM,minero}]) ->
	[{NS,saboteador},{NT,tecnico},{NM-1,minero}].
  
% Dado un vertice y la lista de aristas, consigue una cueva, es decir,
% Busca las aristas en las que esta presente el vertice y las convierte
% en direcciones (norte, sur, este, oeste), asi el jugador no sabe su posicion
% absoluta en el mapa
consigueCueva({X,Y}, Aristas) ->
	io:format("Aristas: ~w~n", [Aristas]),
	[{toDirection({X,Y}, verticeDeArista({X,Y},{{X1,Y1},{X2,Y2}})),Estado}||{{X1,Y1},{X2,Y2},Estado}<-Aristas, verticeEnArista({X,Y},{{X1,Y1},{X2,Y2}})].
	
% Inicializa la lista de aristas a cerradas
inicializaGaleria({{Vert,Aristas},{oro,O},{inicial,I}}) ->
	NuevasAristas = [{{X1,Y1},{X2,Y2},cerrado}||{{X1,Y1},{X2,Y2}}<-Aristas],
	{{Vert,NuevasAristas},{oro,O},{inicial,I}}.
	
% Actualiza el mapa, buscando la arista que se consigue al avanzar desde un
% vertice en una direccion, y marcando esa arista como abierta
actualizaGaleriaAbirta({X,Y}, Dir, {{Vert,Aristas},{oro,O},{inicial,I}}) ->
	NuevaArista = {{X,Y},toVertice({X,Y},Dir),abierto},
	NuevasAristas = [{{X1,Y1},{X2,Y2},E}||{{X1,Y1},{X2,Y2},E}<-Aristas,not(verticeEnArista({X,Y},{{X1,Y1},{X2,Y2}}) and verticeEnArista(toVertice({X,Y},Dir),{{X1,Y1},{X2,Y2}}))],
	{{Vert, [NuevaArista | NuevasAristas]},{oro,O},{inicial,I}}.
	
% Cuando se derrumba una cueva se marcan las galerias como derumbadas de manera permanente
actualizaGaleriaPermanente({X,Y}, Dir, {{Vert,Aristas},{oro,O},{inicial,I}}) ->
	NuevasAristas = [{{X1,Y1},{X2,Y2},permanente}||{{X1,Y1},{X2,Y2},_}<-Aristas,verticeEnArista({X,Y},{{X1,Y1},{X2,Y2}})],
	Antiguas = [ {{X1,Y1},{X2,Y2},E} || {{X1,Y1},{X2,Y2},E}<-Aristas, not(verticeEnArista({X,Y},{{X1,Y1},{X2,Y2}})) ],
	{{Vert, Antiguas ++ NuevasAristas},{oro,O},{inicial,I}}.
	
% Dados dos vertices, dice en que direccion esta
% el segundo respecto del primero
toDirection({X,Y},{X1,Y}) ->
	case X1 == (X + 1) of
		true ->
			sur;
		false ->
			norte
	end;
toDirection({X,Y},{X,Y1}) ->
	case Y1 == (Y + 1) of
		true ->
			este;
		false ->
			oeste
	end.
		
% Dada una direccion, devuelve su contraria
negateDirection(norte) -> sur;
negateDirection(sur) -> norte;
negateDirection(este) -> oeste;
negateDirection(oeste) -> este.
	
% Comprueba si un vertice esta presente en una arista
verticeEnArista({X,Y},{{X,Y},{X1,Y1}}) -> true;
verticeEnArista({X,Y},{{X1,Y1},{X,Y}}) -> true;
verticeEnArista({_,_},{{_,_},{_,_}}) -> false.

% Si un vertice esta en una arista, devuelve el vertice
% qal que esta unido a traves de la arista
verticeDeArista({X,Y},{{X,Y},{X1,Y1}}) -> {X1,Y1};
verticeDeArista({X,Y},{{X1,Y1},{X,Y}}) -> {X1,Y1};
verticeDeArista({_,_},{{_,_},{_,_}}) -> error.

% Dado un vertice y una direccion, devuelve el vertice
% resultante de moverse desde el vertice en la direccion
toVertice({X,Y},norte) -> {X-1,Y};
toVertice({X,Y},sur) -> {X+1,Y};
toVertice({X,Y},este) -> {X,Y+1};
toVertice({X,Y},oeste) -> {X,Y-1}.
	
% COnsigue una cueva para un jugador
nuevaCueva(Dir, Aristas, Players, Pid) ->
	[P] = [PlayerInfo#playerInfo.position || PlayerInfo <- Players, PlayerInfo#playerInfo.gamePID == Pid ],
	NuevaPos = toVertice(P,Dir),
	{consigueCueva(NuevaPos, Aristas), NuevaPos}.
	
% Actualiza la lista de jugadores cuando uno se mueve a una nueva posicion
actualizaPlayers(Players, NuevaPos, Pid) ->
	[P] = [PlayerInfo || PlayerInfo <- Players, PlayerInfo#playerInfo.gamePID == Pid ],
	NuevaLista = [Player || Player <- Players, Player#playerInfo.gamePID =/= Pid],
	[#playerInfo{playerPID = P#playerInfo.playerPID, playerName = P#playerInfo.playerName, gamePID = P#playerInfo.gamePID, position = NuevaPos, rol = P#playerInfo.rol} | NuevaLista ].
	
% Funcion que controla el tiempo que ha pasado en la partida
controlaTiempo(T,ServerPID) ->
	receive 
		_ ->  void
	after
		T ->
			tiempo_agotado(ServerPID)
	end.
	
	
	