from unittest import TestCase

from .z4 import opt_dist


class TestOptDist(TestCase):
    def test(self):
        l = [(('0010001000', 5), 3),
             (('0010001000', 4), 4),
             (('0010001000', 3), 3),
             (('0010001000', 2), 2),
             (('0010001000', 1), 1),
             (('0010001000', 0), 2)]

        for args, result in l:
            self.assertEqual(opt_dist(*args), result, args)
