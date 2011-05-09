import unittest
import sys, os

if __name__ == "__main__":
    sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
    from PScheme.tests.test_PScheme import *

    unittest.main()
