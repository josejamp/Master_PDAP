Autor: Jos� Javier Mart�nez Pag�s

La pr�ctica cuenta con los siguientes archivos:
 - generador_mapa.erl
 - serv_maestro.erl
 - serv_partida.erl
 - proc_juego.erl
 - player.erl
Para ejecutar la pr�ctica es necesario usar el comando de Erlang c(<nombre_modulo>) para los m�dulos mencionados antes.


La pr�ctica est� hecha de tal manera que el nodo maestro puede arrancarse en un nodo y los jugadores en otros. El servidor maestro
se registra globalmente, por lo que los nodos que quieran verle deber�n usar net_adm:ping() con el nodo en el que se ha iniciado
el servidor maestro.

Para iniciar el servidor maestro, se usa:
	serv_maestro:start().
Para crear un servidor de partida desde el nodo maestro:
	serv_maestro:nueva_partida( n� filas, n� columnas, n� galer�as, n� jugadores, tiempo (segundos) ).

Los jugadores se inician mediante:
	player:start( Nombre en String, PID del servidor de partida).
Para ver las partidas disponibles se puede usar:
	serv_maestro:partidas().
	
Una vez el jugador se haya unido al servidor de partida, podr� introducir acciones, la entrada sigue el formato:
	<accion> -parametro
En particular pueden ser:
	- mover -direcci�n: mueve al jugador en la direcci�n especificada (norte, sur, este, oeste):
		Ej: mover -norte
	- hablar -mensaje: muestra un mensaje a los jugadores de la sala:
		Ej: hablar -hola amigos que tal estais
	- reparar -nombre: repara al jugador indicado:
		Ej: reparar -Paco
	- romper -nombre: rompe al jugador indicado:
		Ej: romper -Pepe
	- derrumbar_g -direcci�n: derrumba la galer�a indicada en la direcci�n:
		Ej: derrumbar_g -norte
	- derrumbar_c -direcci�n: derrumba la cueva en la que est� el jugador, y lo desplaza al derrumbar la cueva en la direcci�n indicada:
		Ej: derrumbar_c -sur
		
Un ejemplo de creaci�n de servidor maestro y de uni�n de un jugador es el siguiente:
	En un nodo:
		> serv_maestro:start().
		{ok, <0.63.0>}
		> serv_maestro:nueva_partida(3,3,12,3,1000).  %%Partida de cueva 3x3, con 12 galer�as, para 3 jugadores, que dura 1000 segundos
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
		
Si el tiempo de la partida acaba, se informar� a los jugadores de que los aboteadores han ganado, se cerrar�n los procesos y se guardar�
la puntuaci�n de los saboteadores del equipo (100 puntos).
Si ganan los buenos, se informar� a los jugadores de qui�n ha sido el jugador que ha encontrado el oro, se cerrar�n los procesos y se
dar� 100 puntos a los jugadores del equipo ganador.
La puntuaci�n se guarda en el servidor maestro y se puede consultar la puntuaci�n de cualquier jugador que haya participado en cualquier
partida ya acabada. La puntuaci�n se maneja como un juego arcade, por lo que permite nombres de jugador duplicados.
Para consultar la puntuaci�n se usa:
	serv_maestro:consulta_puntuaciones().
Si se quieren borrar, se usa:
	serv_maestro:borra_ountuaciones().
	
Si se quiere parar el servidor maestro, basta con usar stop de la librer�a gen_server.
		
