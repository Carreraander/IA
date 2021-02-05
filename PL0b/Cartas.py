import random


class Carta(object):
    """Representa una carta en este caso de la baraja espanola
    
    Attributoss:
      palo: integer 0-3
      rango: integer 1-13
    """

    palo_nombres = ["Bastos", "Oros", "Copas", "Espadas"]
    rango_nombres = [None, "As", "2", "3", "4", "5", "6", "7", "Sota", "Caballo", "Rey"]

    def __init__(self, palo=0, rango=2):
        self.palo = palo
        self.rango = rango

    def __str__(self):
        """Devuelve la carta en modo legible."""
        return '%s de %s' % (Carta.rango_nombres[self.rango],
                             Carta.palo_nombres[self.palo])

    def __cmp__(self, otra):
        """Compara una carta con otra
        Devuelve un positivo si self > otra; negativo si otra > self;
        y 0 si equivalentes.
        """
        t1 = (self.palo, self.rango)
        t2 = (other.palo, other.rango)

        rdo = cmp(t1,t2)
        return rdo

    def __eq__(self, other):
        rdo = NotImplemented
        if isinstance(other, Carta):
            #TODO Anadir el codigo para reescribir el equals
        return rdo

    def __ne__(self, other):
        """self != other"""
        eq = Carta.__eq__(self, other)
        return not eq


class Mazo(object):
    """Representa el mazo de cartas.
    Attributos:
      cartas: una lista de cartas que anadimos una a una.
    """
    
    def __init__(self):
        self.cartas = []
        for palo in range(4):
            for rango in range(1, 11):
                carta = Carta(palo, rango)
                self.cartas.append(carta)

    def __str__(self):
        res = []
        for carta in self.cartas:
            res.append(str(carta))
        return '\n'.join(res)


    def anadir_carta(self, carta):
        """Anade una carta al mazo."""
        self.cartas.append(carta)

    def eliminar_carta(self, carta):
        """Elimina un acarta del mazo."""
        
        if carta in self.cartas:
            self.cartas.remove(carta)
            rdo = True
        else:
            rdo = False
        return rdo

    def esta_carta(self, carta):
        """Comprueba si una carta es mazo.
        True si esta
        False sino"""
        if carta in self.cartas:
            rdo = True
        else:
            rdo = False
        return rdo
    
    def pop_carta(self, i=-1):
        """Saca una carta del mazo.
        i: el indice de la carta a sacar (por defecto -1, es decir, la ultima); 
        """
        return self.cartas.pop(i)

    def barajar(self):
        """Baraja las cartas del mazo."""
        random.shuffle(self.cartas)

    def ordenarAsc(self):
        """Ordena las cartas del mazo en orden ascendente."""
        self.cartas.sort(key = lambda x: x.rango )
        

    def mover_cartas(self, mano, num):
        """Mueve un numero num de cartas desde el mazo a la mano.
        mano: objeto destino perteneciente a la clase Mano
        num:  numero de cartas a desplazar
        """
        for i in range(num):
            self.mano.append(carta)
            self.pop_carta(self,i)
            #TODO Anadir el codigo que falta

    def imprimir(self):
        for idx,carta in enumerate(self.cartas):
            print('  {0} - {1}'.format(idx,carta))

    def esta_vacio(self):
        """Devuelve True si esta vacio:  emplear len"""
        #TODO Anadir el codigo que falta
        if len(mazo) is 0:
            return True
        else
            return False

    def repartir_cartas(self, manos, num_cartas=999):
        num_manos = len(manos)
        i=0
        while i <= num_cartas and not self.esta_vacio():
            #TODO Anadir el codigo           # coger la carta de la cima
            pop_carta(self,-1)
            mano = manos[i % num_manos]  # calcular a quien repartir
            mano.anadir_carta(carta)           # anadir una carta a una mano
            i+=1

class Mano(Mazo):
    """Representa una mano a jugar."""
    
    def __init__(self, jugador=''):
        self.cartas = []
        self.jugador = jugador

    
    def imprimir(self):
        """ Reescribir tal que indique el nombre del jugador propietario de esa mano
        asi como si esta vacia o de no estarlo las cartas que contiene:
        la mano de X esta vacia o la mano de X contiene bla bla.
        emplear el imprimir de la clase madre para imprimir la lista de cartas"""
        printf("%s. ",self.jugador)
        if len(cartas) != 0:
            printf("Tiene la siguiente mano: %s", self.Mazo.imprimir())

        #TODO Anadir el codigo

class ManoPersona(Mano):
    def eliminar_parejas(self):

        cont = 0

        salir = False#chivato para salir cuando la pareja que se pretende eliminar no sea tal

        while not salir:
            super().imprimir()
            print("Que cartas forman pareja? de 0 a {0}".format(len(self.cartas)-1))
            idxCarta,idxPareja = map(int, input().split(','))
            carta = self.cartas[idxCarta]
            posiblePareja = self.cartas[idxPareja]
            print(carta)
            print(posiblePareja)
            pareja = Carta(3 - carta.palo, carta.rango)#

            print(pareja)
            if #TODO Anadir el codigo para comprobar si la que debiera ser pareja es la esperada y si fuese el caso eliminar ambas de la mano
                print("En la mano de {0} forman par {1}  {2}".format(self.jugador, carta, pareja))
                cont += 1
            else:
                salir = True
                
        return cont

class LaMona(Mano):
    ####################################
    # Forman pareja:
    # numX de bastos y numX de espadas
    # numX de copas  y numx de oros
    ####################################
    def eliminar_parejas(self):
        cont = 0#contador de cuantas parejas se eliminan
        originales_cartas = self.cartas[:]
        while originales_cartas is not null:
            mazo.ordenarAsc
        #TODO anadir el codigo para buscar y eliminar parejas automaticamente
        return cont
        
def find_defining_class(obj, method_name):
    """Finds and returns the class object that will provide 
    the definition of method_name (as a string) if it is
    invoked on obj.
    obj: any python object
    method_name: string method name
    """
    for ty in type(obj).mro():
        if method_name in ty.__dict__:
            return ty
    return None


if __name__ == '__main__':
    #ejemplo1
    print("Ejemplo 1 mover cartas del mazo a una mano: ")
    mazo = Mazo()
    mazo.barajar()

    mano = Mano()
    print(find_defining_class(mano, 'barajar'))

    mazo.mover_cartas(mano, 5)
    mano.ordenarAsc()
    mano.imprimir()

    print("Ejemplo 2 repartir cartas entre 2 jugadores (7 cartas): ")
    mazo = Mazo()
    mano1 = ManoPersona('Aitzi')
    mano2 = ManoPersona('Koldo')
    mano3 =ManoPersona('Otro')
    mazo.repartir_cartas([mano1,mano2],7)
    mano1.imprimir()
    mano2.imprimir()
    mano3.imprimir()

    
    

    
