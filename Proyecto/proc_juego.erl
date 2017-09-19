-module(proc_juego).

-export([init/1, start/6, terminate/2, handle_call/3,
  handle_cast/2, handle_info/2, code_change/3, 
  mover/2, muestra/2, hablar/2, reparar/2, se_reparado/2,
  romper/2, se_roto/2, derrumbar_galeria/2,
  derrumbar_cueva/2, galeria_actualizada/2, cueva_actualizada/2,
  empieza_partida/1, fin_partida/1]).

-behaviour(gen_server).

% Autor: Jose Javier Martinez Pages

% Modulo que representa el proceso de juego, es decir, al jugador dentro del juego.
% Este modulo se encarga de recibir las ordenes del jugador: moverse en una direccion,
% hablar con los jugadores en su cueva, reparar a un jugador, romper a un jugador,
% romper una galeria y derrumbar una cueva.
% Tambien se encarga de recibir mensajes auxiliares provenientes del servidor de la partida.
% El modulo sigue las restricciones mencionadas en el enunciado, tales como que el jugador
% no conce su posicion absoluta en la cueva, no sabe donde esta el oro, y no conoce
% a los jugadores de la partida, solo le informan de los nombres de los jugadores de su cueva.


% Record para representar la informacion de estado que tiene que guardar el jugador: su nombre, su PID,
% el PID del servidor de partida, su rol, la informacion de las galerias de la cueva que ocupa,
% si esta roto o no, y si la partida ya ha empezado o no
-record(info, {nombre, playerID, serverID, tipo, infoCueva, roto, partidaEmpezada}).

% Para iniciar al jugador se necesita su nombre, los sendos PIDs, su rol, la cueva que ocupa inicialmente
% t¡y si la partida va a empezar justo al unirse o por el contrario todavia falta gente por unirse a la partida
start(Nombre, PlayerPID, ServerPID, Tipo, Cueva, Inicio) -> gen_server:start_link(?MODULE, {Nombre, PlayerPID, ServerPID, Tipo, Cueva, Inicio},[]).

% Al inicializar el jugador se informa del rol que le ha tocado y de su cueva inicial
init({Nombre, PlayerPID, ServerPID, Tipo, Cueva, Inicio}) ->  
  player:muestra(PlayerPID, io_lib:format("Jugador inicializado de tipo ~w ~n",[Tipo])),
  player:muestra(PlayerPID, io_lib:format("Informacion de la cueva inicial ~w ~n",[Cueva])),
  {ok, #info{nombre = Nombre, playerID = PlayerPID, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = false, partidaEmpezada = Inicio}}.
  
% Manejador de la orden de moverse, la partida debe haber emepzado y la galeria debe estar abierta
handle_call({mover, Dir}, {Pid,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
   case galeriaAbierta(Cueva, Dir) of
		true ->
			% se hace uso del servidor de partida para conseguir la nueva cueva y ser
			% informado de los jugadores de la nueva cueva
			case serv_partida:desplaza_cueva(ServerPID, Nombre, Cueva, Dir ) of
				{ok, {NuevaCueva, NuevosPlayers}} ->
					player:muestra(Pid, io_lib:format("Has llegado a la nueva cueva ~w ~n",[NuevaCueva])),
					player:muestra(Pid, io_lib:format("En la nueva cueva se encuentran los jugadores ~s ~n",[NuevosPlayers])),
					{reply, ok, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = NuevaCueva, roto = Roto, partidaEmpezada = true}};
				fin ->
					gen_server:stop(ServerPID), % el ultimo proceso de juego en cerrar cierra el proceso de juego
					{reply, fin, State};
				E->
					{reply, E, State}
			end;
		false ->
			{reply, {error,"No puedes ir allí, la galería está cerrada"}, State}
	end;
	
% Manejador de la orden de hablar con los jugadores de la cueva, como no
% se mantiene una referencia a los jugadores, ha de usarse el servidor de la partida
handle_call({hablar, Mensaje}, {Pid,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case serv_partida:habla_cueva(ServerPID, Nombre, Mensaje) of
		ok ->
			{reply, ok, State};
		E ->
			{reply, E, State}
	end;
	
% Manejador de la orden de reparar, se usa el servidor de partida para ver si
% los jugadores estan en la misma cueva, y ademas se comprueba si el rol
% del jugador es correcto
handle_call({reparar, NombreVictima}, {Pid,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case Tipo of
		tecnico ->
			case serv_partida:repara(ServerPID, NombreVictima) of
				ok ->
					player:muestra(Pid, io_lib:format("Has reparado al jugador ~s ~n",[NombreVictima])),
					{reply, ok, State};
				E ->
					{reply, E, State}
			end;
		_ ->
			{reply, {error,"No puedes reparar, no eres un técnico"}, State}
	end;
	
% Manejador del mensaje que llega para ser reparado, funciona si el jugador
% esta roto
handle_call({se_reparado, NombreTecnico}, {_,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case Roto of
		true ->
			player:muestra(Pid, io_lib:format("Has sido reparado por ~s ~n",[NombreTecnico])),
			{reply, ok, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = false, partidaEmpezada = true}};
		false ->
			{reply, {error,"No puedes reparar, el jugador no está roto"}, State}
	end;
	
% Manejador de la orden de romper, se usa el servidor de partida para ver si
% los jugadores estan en la misma cueva, y ademas se comprueba que el jugador
% no se rompa a si mismo
handle_call({romper, NombreVictima}, {Pid,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case NombreVictima =/= Nombre of
		true ->
			case serv_partida:rompe(ServerPID, NombreVictima) of
				ok ->
					player:muestra(Pid, io_lib:format("Has roto al jugador ~s ~n",[NombreVictima])),
					{reply, ok, State};
				E ->
					{reply, E, State}
			end;
		false ->
			{reply, {error,"No te puedes romper a ti mismo"}, State}
	end;

% Manejador del mensaje que llega para ser roto, funciona si el jugador
% no estaba ya roto
handle_call({se_roto, NombreCausante}, {_,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case Roto of
		false ->
			player:muestra(Pid, io_lib:format("Has sido saboteado por ~s ~n",[NombreCausante])),
			{reply, ok, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = true, partidaEmpezada = true}};
		true ->
			{reply, {error, "No puedes romper el equipo del jugador, ya está roto"}, State}
	end;
	
% Manejador de la orden de derrumbar galeria, tiene efecto si el equipo del jugador
% no esta roto, la galeria no estaba ya abierta y la cueva a la que lleva la galeria 
% no ha sido previamente derruida
handle_call({derrumbar_galeria, Dir}, {Pid,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case (Roto == false) of
		true ->	
			case existeGaleria(Cueva, Dir) of
				true ->
					case (galeriaAbierta(Cueva, Dir) == false) of
						true ->
							case (galeriaBloqueadaPermanente(Cueva, Dir) == false) of
								true ->
									serv_partida:galeria_derrumbada(ServerPID, Dir),
									CuevaAbierta = actualizaCuevaAbierta(Cueva,Dir),
									player:muestra(Pid, io_lib:format("Tras derrumbar la galeria, la cueva es: ~w ~n",[CuevaAbierta])),
									{reply, ok, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = CuevaAbierta, roto = Roto, partidaEmpezada = true}};
								false ->
									{reply, {error,"La cueva que conecta con la galería ha sido derruída"}, State}
							end;
						false ->
							{reply, {error,"La galeria ya está abierta"}, State}
					end;
				false ->
					{reply, {error,"No hay galeria en esa dirección"}, State}
			end;
		false ->
			{reply, {error,"No puedes derrumbar la galería porque estás roto"}, State}
	end;

% Manejador de la orden de derrumbar galeria, tiene efecto si el equipo del jugador
% no esta roto y la galeria por la que quieres escapar esta abierta. El servidor
% de la partida se encarga de ver si hay jugadores en la cuava, de informar de la nueva
% posicion, y de comprobar de si se ha encontrado el oro al moverse
handle_call({derrumbar_cueva, DirMove}, {Pid,_Tag}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	case (Roto == false) of
		true ->		
			case (galeriaAbierta(Cueva, DirMove) == true) of
				true ->
					case serv_partida:cueva_derrumbada(ServerPID, DirMove) of
						{ok, {NuevaCueva, NuevosPlayers, NegDir}} ->
							player:muestra(Pid, io_lib:format("Se ha destruido la cueva. ~n",[])),
							player:muestra(Pid, io_lib:format("Has llegado a la nueva cueva ~w ~n",[NuevaCueva])),
							player:muestra(Pid, io_lib:format("En la nueva cueva se encuentran los jugadores ~w ~n",[NuevosPlayers])),
							{reply, ok, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = actualizaCuevaPermanente(NuevaCueva, NegDir), roto = Roto, partidaEmpezada = true}};
						fin ->
							{reply, fin, State};
						E ->
							{reply, E, State}
					end;
				false ->
					{reply, {error, "No puedes salir por esa dirección, la galería está cerrada"}, State}
			end;
		false ->
			{reply, {error,"No puedes derrumbar la cueva porque estás roto"}, State}
	end;	

% Si llega cualquier mensaje cuando la partida todavia no ha empezado se devuelve un error
handle_call(Request, Info, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = false} = State) ->
  {reply, {error,"La partida todavía no ha empezado"}, State};
  
% Manejador por defecto de call
handle_call(Request, Info, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.
  
% Mensaje para que se muestre una cadena de texto
handle_cast({muestra, Mensaje}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = _} = State) ->
	player:muestra(Pid, io_lib:format("~s~n",[Mensaje])),
	{noreply, State};
	
% Mensaje para actualizar la cueva del jugador al abrirse una galeria
handle_cast({galeria_actualizada, Dir}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true}) ->
	player:muestra(Pid, io_lib:format("Se ha abierto la galeria ~w ~n",[Dir])),
	{noreply, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = actualizaCuevaAbierta(Cueva,Dir), roto = Roto, partidaEmpezada = true}};
	
% Mensaje para actualizar la cueva del jugador al derrumbarse
handle_cast({cueva_actualizada, Dir}, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true}) ->
	player:muestra(Pid, io_lib:format("Se ha derrumbado la cueva en direccion ~w ~n",[Dir])),
	{noreply, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = actualizaCuevaPermanente(Cueva,Dir), roto = Roto, partidaEmpezada = true}};

% Mensaje para informar del inicio de la partida
handle_cast(empieza_partida, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = PE}) ->
	player:muestra(Pid, io_lib:format("¡La partida comienza! ~n",[])),
	{noreply, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true}};
	
% Mensaje para informar del inicio de la partida
handle_cast(fin_partida, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = true} = State) ->
	player:salir(Pid),
	{stop, normal, State};

% Si llega cualquier mensaje cuando la partida todavia no ha empezado se devuelve un error	
handle_cast(Request, #info{nombre = Nombre, playerID = Pid, serverID = ServerPID, tipo = Tipo, infoCueva = Cueva, roto = Roto, partidaEmpezada = false} = State) ->
  player:muestra(Pid, io_lib:format("La partida todavía no ha empezado ~n",[])),
  {noreply, State};

% Manejador por defecto de call
handle_cast(Request, State) ->
  io:format("Unexpected request: ~w~n", [Request]),
  {noreply, State}.
	
handle_info(Message, State) ->
  io:format("Unexpected message: ~w~n", [Message]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("Proceso de juego cerrado~n"),
  io:format("Motivo: ~w~n", [Reason]).

code_change(PreviousVersion, State, _Extra) ->
  io:format("Code change from ~w~n", [PreviousVersion]),
  {ok, State}.
  
  
%
% Interfaces para enviar los mensajes  
%  
mover(Pid, Dir) ->
	gen_server:call(Pid, {mover, Dir}).
  
hablar(Pid, Mensaje) ->
	gen_server:call(Pid, {hablar, Mensaje}).
	
reparar(Pid, Nombre) ->
	gen_server:call(Pid, {reparar, Nombre}).
	
se_reparado(Pid, NombreTecnico) ->
	gen_server:call(Pid, {se_reparado, NombreTecnico}).
	
romper(Pid, Nombre) ->
	gen_server:call(Pid, {romper, Nombre}).
	
se_roto(Pid, NombreCausante) ->
	gen_server:call(Pid, {se_roto, NombreCausante}).
	
derrumbar_galeria(Pid, Dir) ->
	gen_server:call(Pid, {derrumbar_galeria, Dir}).
	
derrumbar_cueva(Pid, DirMove) ->
	gen_server:call(Pid, {derrumbar_cueva, DirMove}).
	
galeria_actualizada(Pid, Dir) ->
	gen_server:cast(Pid, {galeria_actualizada, Dir}).
	
cueva_actualizada(Pid, Dir) ->
	gen_server:cast(Pid, {cueva_actualizada, Dir}).
	
muestra(Pid, Mensaje) ->
	gen_server:cast(Pid, {muestra, Mensaje}).

empieza_partida(Pid) ->
	gen_server:cast(Pid, empieza_partida).
	
fin_partida(Pid) ->
	gen_server:cast(Pid, fin_partida).
	
  
  

% Funcion que devuelve en una cueva existe una
% galeria en una direccion
existeGaleria(Cueva, Dir) ->
	lists:member({Dir,abierto}, Cueva) or lists:member({Dir,cerrado}, Cueva) or lists:member({Dir,permanente}, Cueva).

% Funcion que devuelve si la galeria de una cueva
% en una direccion esta abierta  
galeriaAbierta(Cueva, Dir) ->
	lists:member({Dir,abierto}, Cueva).
	
% Funcion que devuelve si la cueva se ha derrumbado 
galeriaBloqueadaPermanente(Cueva, Dir) ->
	lists:member({Dir,permanente}, Cueva).
	
% Funcion que acualiza una galeria de una cueva para
% que este abierta
actualizaCuevaAbierta(Cueva, Dir) ->
	NuevaCueva = [{Dir1,E} || {Dir1,E} <- Cueva, Dir1 =/= Dir],
	[{Dir, abierto} | NuevaCueva].
	
% Funcion que acualiza una galeria de una cueva para
% que este bloqueada permanentemente, esto ocurre
% cuando la cueva se derrumba
actualizaCuevaPermanente(Cueva,Dir) ->
	NuevaCueva = [{Dir1,E} || {Dir1,E} <- Cueva, Dir1 =/= Dir],
	[{Dir, permanente} | NuevaCueva].
	