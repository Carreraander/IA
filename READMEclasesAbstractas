#Las clases y métodos abstractos no existen en native Python
# se emplean decoradores y el modulo ABS (Abstract Base Classes)

class ClaseAbstracta(object):
     @abc.abstractmethod#este decorador me permite crear métodos abstractos
     def quien_eres(self):
         print "Soy una clase abstracta!"

Así la ClaseAbstracta no es realmente una clase abstracta ya que se puede instanciar (a diferencia de lo que sucede en Java)

poder hacer

claseAbstracta=ClaseAbstracta()
claseAbstracta.quien_soy() 

y esto imprimirá "Soy una clase abstracta"


Para generar una clase abstracta a lo Java hay que emplear en modulo abc, para generar una metaclase

#definición de un método y una clase abstracta
import abc
from abc import ABCMeta
 
class ClaseAbstracta(object):
     __metaclass__ = ABCMeta
     
     @abc.abstractmethod
     def quien_eres(self):
         print "Soy una clase abstracta"
 
 cl = ClaseAbstracta()
 cl.quien_eres()
Traceback (most recent call last):
  File "<pyshell#6>", line 11, in <module>
    cl = ClaseAbstracta()
TypeError: Can't instantiate abstract class claseAbstracta with abstract methods quien_eres

