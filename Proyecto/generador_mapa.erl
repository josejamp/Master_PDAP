-module(generador_mapa).
-export([genera_mapa/3]).


% Autor: Jose Javier Martinez Pages

% Funcion principal para generar el mapa, devuelve un par, con el grafo, y 
% la posicion que ocupa el oro en el grafo.
% Si el grafo tiene mas o menos aristas de las que puede tener, devuelve el 
% atomo error.
genera_mapa(N,M,G) ->
	case aristasPermitidas(G,N,M) of
		true ->
			{Vertices,Aristas} = crea_grafo(G,N,M),
			{{Vertices,Aristas}, meteOro(M,Vertices),posInicial(Vertices)};
		false ->
			io:format("El numero de aristas es incorrecto ~n"),
			io:format("Para N=~w y M=~w, ~n", [N,M]),
			io:format("el minimo valor permitido para G es ~w, ~n", [minAristasPermitidas(N,M)]),
			io:format("y el maximo valor permitido para G es ~w. ~n", [maxAristasPermitidas(N,M)]),
			error
	end.

% Funcion que crea un grafo NxM usando el algoritmo de Prim, y luego crea G-(N*M -1)
% aristas en los huecos disponibles.
% En esta llamada se generan los vertices posibles.
crea_grafo(G,N,M)-> 
	{Vertices,Aristas} = prim([{X,Y}||X<-lists:seq(1,N),Y<-lists:seq(1,M)],N,M,[],[],[]),
	aleatorios(G-((N*M)-1),N,M,Vertices,Aristas).

% Algoritmo de Prim, inicializa la lista de distancias, de relaciones entre vertices
% y la cola de prioridad, y con ellas calcula la lista de  aristas entre vertices
prim(Vertices,N,M,Distancia_vacia,Padre_vacia,Cola_vacia) ->
	{Dist, Padre, Cola} = inicializar(Vertices,N,M,Distancia_vacia,Padre_vacia,Cola_vacia),
	{Vertices, calcular(Vertices,N,M,Dist,Padre,Cola)}.

% Inicializa las listas de manera recursiva, las inicializa con el caso peor,
% es decir, la distancia con la mayor posible, la lista de padres a null y los
% elementos de la cola con la menor prioridad
inicializar([],_,_,Distancia,Padre,Cola) -> {Distancia,Padre,Cola};
inicializar([{X,Y}|Vertices],N,M,Distancia,Padre,Cola) ->
	Nueva_dist = [{{X,Y},(N*M)+1}| Distancia],
	Nueva_padre = [{{X,Y},null}| Padre],
	Nueva_cola = [{{X,Y},(N*M)+1}| Cola],
	inicializar(Vertices,N,M,Nueva_dist,Nueva_padre,Nueva_cola).

% Genera las aristas entre vertices para el algoritmo de Prim.
% Se consigue la lista de adyacentes al nodo actual, se actualizan las
% listas relativas al nodo y se realiza una llamada recursiva
calcular(_,_,_,_,Padre,[]) -> Padre;
calcular(Vertices,N,M,Distancia,Padre,[{{X,Y},_}|Cola]) ->
	Ady = adyacentes({X,Y},N,M),
	{Nueva_dist, Nueva_padre, Nueva_cola} = trataNodo({X,Y},Ady,Distancia,Padre,Cola),
	calcular(Vertices,N,M,Nueva_dist,Nueva_padre,Nueva_cola).

% Recorre la lista de adyacentes a un nodo y actualiza las listas segun el algoritmo de Prim.
% En este caso la distancia (o peso) de un vertice a otro es 1.
trataNodo(_,[],Distancia,Padre,Cola) -> {Distancia,Padre,Cola};
trataNodo(Nodo,[{X1,Y1}|Adyacentes],Distancia,Padre,Cola) ->
	Peso = peso(Nodo,{X1,Y1}),
	case lists:member({X1,Y1},[{X2,Y2}||{{X2,Y2},D}<-Cola]) and (dameDistancia({X1,Y1},Distancia) > Peso) of
		true ->
			N_padre = [{{X1,Y1},Nodo}|[{{X,Y},P}||{{X,Y},P}<-Padre,{X,Y}=/={X1,Y1}]],
			N_dist = [{{X1,Y1},Peso}|[{{X,Y},D}||{{X,Y},D}<-Distancia,{X,Y}=/={X1,Y1}]],
			N_cola = [{{X1,Y1},Peso}|[{{X,Y},D}||{{X,Y},D}<-Cola,{X,Y}=/={X1,Y1}]],
			trataNodo(Nodo,Adyacentes,N_dist,N_padre,N_cola);
		false ->
			trataNodo(Nodo,Adyacentes,Distancia,Padre,Cola)
	end.

% Funcion que genera G aristas aleatorias.
% Para cada vertice se busca sus vertices adyacentes y se comprueba si no existe ya una arista.
aleatorios(0,_,_,Vertices,Aristas) -> {Vertices,Aristas};
aleatorios(G,N,M,Vertices,Aristas)->
	Disponibles = [{{X1,Y1},{X2,Y2}}||{X1,Y1}<-Vertices,{X2,Y2}<-adyacentes({X1,Y1},N,M),noRepetido({X1,Y1},{X2,Y2},Aristas)],
	I = rand:uniform(length(Disponibles)),
	Nuevo = lists:nth(I,Disponibles),
	aleatorios(G-1,N,M,Vertices,[Nuevo|Aristas]).
	
% Comprueba si la arista entre {X1,Y1} y {X2,Y2} no existe.
noRepetido({X1,Y1},{X2,Y2},Aristas) ->
	length([{{X3,Y3},{X4,Y4}}|| {{X3,Y3},{X4,Y4}} <- Aristas, ((({X1,Y1}=={X3,Y3}) and ({X2,Y2}=={X4,Y4})) or (({X1,Y1}=={X4,Y4}) and ({X2,Y2}=={X3,Y3})))]) == 0.
	
% Funcion que elige aleatoriamente un vertice en la ultima columna y añade el oro.
meteOro(M,Vertices) ->
	Disponibles = [{X1,M}||{X1,_}<-Vertices],
	I = rand:uniform(length(Disponibles)),
	Nuevo = lists:nth(I,Disponibles),
	{oro, Nuevo}.
	
posInicial(Vertices) ->
	Disponibles = [{X1,1}||{X1,_}<-Vertices],
	I = rand:uniform(length(Disponibles)),
	Nuevo = lists:nth(I,Disponibles),
	{inicial, Nuevo}.
	
% Busca la distnacia de un vertice en una lista de {Vertice, Distancia}.
% Si el vertice no esta en la lista, devuelve menos uno.
dameDistancia(_,[]) -> -1;
dameDistancia({X,Y},[{{X,Y},D}|_]) -> D;
dameDistancia({X,Y},[{{_,_},_}|Dist]) -> dameDistancia({X,Y},Dist).
	
% El peso entre dos vertices es 1.
peso({X1,Y1},{X2,Y2}) ->
	1.
	
% Devuelve la lista de vertices adyacentes a uno dado.
adyacentes({X,Y},N,M) ->
	[{X1,Y1} || X1 <- lists:seq(X-1,X+1), Y1 <- lists:seq(Y-1,Y+1), sonAdyacentes({X,Y},{X1,Y1}), X1 =< N, Y1 =< M, X1 > 0, Y1 > 0].

% Comprueba si dos vertices son adyacentes
sonAdyacentes({X1,Y1},{X2,Y1}) ->
	((X2==(X1+1)) or (X2==(X1-1)));
sonAdyacentes({X1,Y1},{X1,Y2}) ->
	((Y2==(Y1+1)) or (Y2==(Y1-1)));
sonAdyacentes({_,_},{_,_}) ->
	false.
	
% Comprueba si G esta entre es un numero posible de aristas permitidas para un grafo NxM
aristasPermitidas(G,N,M) ->
	(G >= minAristasPermitidas(N,M)) and (G =< maxAristasPermitidas(N,M)).
	
% El minimo numero de aristas permitidas es (NxM)-1
minAristasPermitidas(N,M) ->
	(N*M)-1.
	
% El maximo numero de aristas permitidas es (N-1)xM + (M-1)*N dado
% que el grafo es especial y debe representar una cueva
maxAristasPermitidas(N,M) ->
	(N-1)*M + (M-1)*N.
	