import unittest
from Singleton import Singleton

class Test(unittest.TestCase):

    def testSingleton(self):
        s1 = Singleton()
        s2 = Singleton()
        print(self.assertTrue(s1 is s2))
        print(self.assertTrue(s1 == s2))

if __name__ == "__main__":
    unittest.main()
