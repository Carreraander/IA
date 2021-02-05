
"""This file contains code for use with "Think Stats",
by Allen B. Downey, available from greenteapress.com
Copyright 2014 Allen B. Downey
License: GNU GPLv3 http://www.gnu.org/licenses/gpl.html
"""

from __future__ import print_function, division

import unittest
from Cartas import Carta, Mazo


class Test(unittest.TestCase):

    def testMazoEliminar(self):
        mazo = Mazo()
        #mazo.imprimir_mazo()
        carta23 = Carta(2, 3)
        mazo.eliminar_carta(carta23)
        #mazo.imprimir_mazo()
        self.assertFalse(mazo.esta_carta(carta23))

if __name__ == "__main__":
    unittest.main()
