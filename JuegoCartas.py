from Cartas import Carta,Mazo,LaMona,Mano, ManoPersona


class JuegoCartas:

    manos = []
    def __init__(self):
        self.mazo = Mazo()
        self.mazo.barajar()

        
    def inicializar_partida(self,n_jugadores=2):

        #el primer jugador siempre sera la maquina
        self.manos.append(LaMona('Ordenador'))
        for idxJug in range(1,n_jugadores+1):
            nombreJugador = input("Cual es el nombre del  jugador {0}?".format(idxJug))
            self.manos.append(ManoPersona(nombreJugador))

        
    def jugar_ronda(self):
        self.manos[0].imprimir()
        contOut = self.manos[0].eliminar_parejas()
        print('Se han eliminado {0}'.format(contOut))
        self.manos[0].imprimir()
        for idxJug in range(1,n_jugadores+1):
            contOut = self.manos[idxJug].eliminar_parejas()

if __name__ == "__main__":
    print('Ejecutando como programa principal')
    juego = JuegoCartas()
    n_jugadores = int(input("Cuantos jugadores tiene el juego (sin contar las maquina)?"))
    print(n_jugadores)
    manos = juego.inicializar_partida(n_jugadores)

    juego.mazo.repartir_cartas(juego.manos,40)

    juego.jugar_ronda()

    #########################################################
    #TODO opcional si queréis podeis implementar el juego
    # tal que un jugador pueda decidir
    # a quien le roba una carta y luego se ha de comprobar si forma
    # nueva pareja con la robada
    # la maquina podra almacenar las que le ha robado cada cual
    # de forma que si precisa una carta que le han robado anteriormente pueda
    # recuperarla
    # el juego acabara cuando alguien se quede sin cartas
    # o haya una ronda completa en la que nadie haga parejas
    # en ese caso el que tenga mas cartas en la mano pierde
    ######################################################
