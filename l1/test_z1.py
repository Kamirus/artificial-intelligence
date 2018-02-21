from unittest import TestCase

from .z1 import load_position, dump_position


class TestUtil(TestCase):
    def test_load_position(self):
        def aux(pos_str, pos):
            self.assertEqual(pos, load_position(pos_str))
            self.assertEqual(pos_str, dump_position(pos))
            self.assertEqual(pos_str, dump_position(load_position(pos_str)))
            self.assertEqual(pos, load_position(dump_position(pos)))

        aux('a1', (1, 1))
        aux('b3', (2, 3))
        aux('c8', (3, 8))
        aux('h8', (8, 8))
