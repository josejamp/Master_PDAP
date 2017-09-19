-module(player).

-export([start/2, muestra/2, loop/1, init/2, salir/1]).

% Autor: Jose Javier Martinez Pages

% Modulo que sirve como interfaz entre el proceso de juego y el jugador.
% Su principal funcion es evitar que el jugador tenga que escribir las 
% ordenes con la sintaxis de Erlang, sino que baste con escribirlas
% como entrdas de teclado

% Record que guarda cierta informacion de estado para el jugador,
% como su nombre, el PID del servidor de juego, y el PID de su
% proceso de juego
-record(info, {nombre, serverPID, processPID}).

% Para iniciar al jugador se necesita su nombre y el servidor
% de juego al que quiere conectarse
start(Nombre, ServerPID) -> 
	spawn(player, init, [Nombre, ServerPID]).


% Al iniciarse el juego se manda al propio porceso un mensaje para
% empezar a jugar, y tras esto se inicia la funcion loop que recoge
% los mensajes y actua en consecuencia
init(Nombre, ServerPID) ->
	io:format("Conectando al servidor de juego ~w ~n",[ServerPID]),
	case serv_partida:unirse(ServerPID, Nombre) of
		{ok, ProcessPID} ->
			juega(self()),
			loop(#info{nombre = Nombre, serverPID = ServerPID, processPID = ProcessPID});
		{error, Mensaje} ->
			io:format("Error: ~s~n",[Mensaje])
	end.
	
% Funcion que recibe los mensajes y los trata
loop(#info{nombre = Nombre, serverPID = ServerPID, processPID = ProcessPID} = State) ->
	receive
		 % Si es un mensaje para mostrar una cadena de texo, se muestra
		{muestra, Mensaje} ->
			io:format("~s~n",[Mensaje]),
			loop(State);
		% Si es el mensaje de jugat, se pide la entrada por teclado
		juega ->
			teclado(#info{nombre = Nombre, serverPID = ServerPID, processPID = ProcessPID}),
			loop(State);
		% Se recibe la entrada de teclado y se trata
		{entrada, W} ->
			case respuesta(ProcessPID, W) of
				% Si todo va bien se vuelve a pedir accion
				ok ->
					juega(self()),
					loop(State);
				% Si hay un error se muestra el mensaje
				{error, Msg} ->
					io:format("Error: ~s~n",[Msg]),
					juega(self()),
					loop(State);
				% Si la accion para la partida, se cierra el proceso
				fin ->
					gen_server:stop(ProcessPID),
					io:format("¡Has ganado la partida! ~n", []),
					io:format("Cliente cerrado. Cerrando... ~n", [])
			end;
		% Si se recibe un mensaje stop, se para el proceso
		stop ->
			io:format("Cliente cerrado. Cerrando... ~n", [])
	end.
  
% Interfaz para mandar al jugador mensajes de texto
muestra(Pid, Mensaje) ->
	Pid ! {muestra, Mensaje}.
	
% Interfaz para mandar al jugador la orden de jugar
juega(Pid) ->
	Pid ! juega.

% Interfaz para parar al jugador
salir(Pid) ->
	Pid ! stop.
	
	
% Funcion que pide al usaurio introducir una orden y trata la respuesta,
% si al tratar la respuesta se devuelve algun error, lo muestra por pantalla
teclado(#info{nombre = Nombre, serverPID = ServerPID, processPID = ProcessPID} = State) ->
	Padre = self(),
	spawn(fun() ->
		W1 = io:get_line("> "),
		W2 = string:substr(W1,1,string:len(W1)-1),
		Padre ! {entrada, W2}
	end ).
			
% Funcion que trata la cadena introducida por el usuario
% y la transforma en un mensaje que se envía a su proceso
% de juego
respuesta(ProcessPID, W) ->
	case string:tokens(W, "-") of
		[P1, P2] ->
			Word1 = re:replace(P1, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
			Word2 = re:replace(P2, "(^\\s+)|(\\s+$)", "", [global,{return,list}]),
			case string:to_lower(Word1) of
				"mover" ->
					proc_juego:mover(ProcessPID, list_to_atom(Word2));
				"hablar" ->
					proc_juego:hablar(ProcessPID, Word2);
				"reparar" ->
					proc_juego:reparar(ProcessPID, Word2);
				"romper" ->
					proc_juego:romper(ProcessPID, Word2);
				"derrumbar_g" ->
					proc_juego:derrumbar_galeria(ProcessPID, list_to_atom(Word2));
				"derrumbar_c" ->
					proc_juego:derrumbar_cueva(ProcessPID, list_to_atom(Word2));
				_ ->
					{error, "Orden incorrecta"}
			end;
		_ ->
			{error, "Orden incorrecta"}
	end.

  