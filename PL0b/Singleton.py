
class _Singleton:
    _instance = None

    def hello(self):
        print("Hello!")


def Singleton():
    if _Singleton._instance is None:
        _Singleton._instance = _Singleton()
    return _Singleton._instance

#La ejecucion nos da que se puede generar mas de uno, pero en realidad es el mismo


