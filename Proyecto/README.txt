Autor: José Javier Martínez Pagés

La práctica cuenta con los siguientes archivos:
 - generador_mapa.erl
 - serv_maestro.erl
 - serv_partida.erl
 - proc_juego.erl
 - player.erl
Para ejecutar la práctica es necesario usar el comando de Erlang c(<nombre_modulo>) para los módulos mencionados antes.


La práctica está hecha de tal manera que el nodo maestro puede arrancarse en un nodo y los jugadores en otros. El servidor maestro
se registra globalmente, por lo que los nodos que quieran verle deberán usar net_adm:ping() con el nodo en el que se ha iniciado
el servidor maestro.

Para iniciar el servidor maestro, se usa:
	serv_maestro:start().
Para crear un servidor de partida desde el nodo maestro:
	serv_maestro:nueva_partida( nº filas, nº columnas, nº galerías, nº jugadores, tiempo (segundos) ).

Los jugadores se inician mediante:
	player:start( Nombre en String, PID del servidor de partida).
Para ver las partidas disponibles se puede usar:
	serv_maestro:partidas().
	
Una vez el jugador se haya unido al servidor de partida, podrá introducir acciones, la entrada sigue el formato:
	<accion> -parametro
En particular pueden ser:
	- mover -dirección: mueve al jugador en la dirección especificada (norte, sur, este, oeste):
		Ej: mover -norte
	- hablar -mensaje: muestra un mensaje a los jugadores de la sala:
		Ej: hablar -hola amigos que tal estais
	- reparar -nombre: repara al jugador indicado:
		Ej: reparar -Paco
	- romper -nombre: rompe al jugador indicado:
		Ej: romper -Pepe
	- derrumbar_g -dirección: derrumba la galería indicada en la dirección:
		Ej: derrumbar_g -norte
	- derrumbar_c -dirección: derrumba la cueva en la que está el jugador, y lo desplaza al derrumbar la cueva en la dirección indicada:
		Ej: derrumbar_c -sur
		
Un ejemplo de creación de servidor maestro y de unión de un jugador es el siguiente:
	En un nodo:
		> serv_maestro:start().
		{ok, <0.63.0>}
		> serv_maestro:nueva_partida(3,3,12,3,1000).  %%Partida de cueva 3x3, con 12 galerías, para 3 jugadores, que dura 1000 segundos
		{ok, <0.69.0>}
	En el nodo del jugador:
		> net_adm:ping(node1@MSI).
		pong
		> serv_maestro:partidas().
		{ok, [ <7002.69.0> ]}
		> player:start("Pedro", pid(7002,69,0)).
		Conectando con el servidor de juego <7002.69.0>
		<0.80.0>
		Jugador inicializado de tipo saboteador
		Informacion de la cueva inicial: [{norte,cerrado},{este,cerrado}]
		La partida comienza!                        %%Cuando se hayan unido otros dos jugadores
		> derrumbar_g -este
		Tras derrumbar la galeria, la cueva es: [{norte,cerrado},{este,abierto}]
		...
		...
		...
		
Si el tiempo de la partida acaba, se informará a los jugadores de que los aboteadores han ganado, se cerrarán los procesos y se guardará
la puntuación de los saboteadores del equipo (100 puntos).
Si ganan los buenos, se informará a los jugadores de quién ha sido el jugador que ha encontrado el oro, se cerrarán los procesos y se
dará 100 puntos a los jugadores del equipo ganador.
La puntuación se guarda en el servidor maestro y se puede consultar la puntuación de cualquier jugador que haya participado en cualquier
partida ya acabada. La puntuación se maneja como un juego arcade, por lo que permite nombres de jugador duplicados.
Para consultar la puntuación se usa:
	serv_maestro:consulta_puntuaciones().
Si se quieren borrar, se usa:
	serv_maestro:borra_ountuaciones().
	
Si se quiere parar el servidor maestro, basta con usar stop de la librería gen_server.
		
