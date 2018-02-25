from unittest import TestCase

from l1.z3 import hand_value


class TestHandsCmp(TestCase):
    def test_layouts(self):
        layouts = [
            [('d', 2), ('c', 9), ('c', 3), ('c', 5), ('c', 8)],  # 1
            [('d', 2), ('c', 2), ('c', 3), ('c', 5), ('c', 8)],  # 2
            [('d', 1), ('c', 2), ('c', 2), ('c', 1), ('c', 3)],  # 2-2
            [('d', 2), ('c', 2), ('c', 3), ('c', 2), ('c', 8)],  # 3
            [('d', 4), ('c', 2), ('c', 3), ('c', 6), ('c', 5)],  # straight
            [('c', 2), ('c', 9), ('c', 3), ('c', 5), ('c', 8)],  # color
            [('d', 2), ('c', 2), ('c', 3), ('c', 2), ('c', 3)],  # 3-2
            [('d', 2), ('c', 2), ('c', 3), ('c', 2), ('c', 2)],  # 4
            [('c', 4), ('c', 2), ('c', 3), ('c', 6), ('c', 5)],  # poker
        ]
        self._assert_hands_in_order(layouts)

    def test_cmp_same_layouts(self):
        hands_weakest_to_strongest = [
            [('d', 10), ('c', 9), ('c', 8), ('c', 7), ('c', 5)],  # 1
            [('d', 10), ('c', 10), ('c', 8), ('c', 7), ('c', 5)],  # 2
            [('d', 'jack'), ('c', 'jack'), ('c', 'ace'), ('c', 'king'), ('c', 'queen')],  # 2
            [('d', 10), ('c', 10), ('c', 9), ('c', 9), ('c', 5)],  # 2-2
            [('d', 'jack'), ('c', 'jack'), ('c', 'queen'), ('c', 'king'), ('c', 'queen')],  # 2-2
            [('d', 10), ('c', 10), ('c', 10), ('c', 7), ('c', 5)],  # 3
            [('d', 'jack'), ('c', 'jack'), ('c', 'jack'), ('c', 'king'), ('c', 'queen')],  # 3
            [('d', 4), ('c', 2), ('c', 3), ('c', 6), ('c', 5)],  # straight
            [('c', 2), ('c', 9), ('c', 3), ('c', 5), ('c', 8)],  # color
            [('d', 2), ('c', 2), ('c', 3), ('c', 2), ('c', 3)],  # 3-2
            [('d', 'jack'), ('c', 'jack'), ('c', 'jack'), ('c', 'queen'), ('c', 'queen')],  # 3-2
            [('d', 2), ('c', 2), ('c', 3), ('c', 2), ('c', 2)],  # 4
            [('d', 'jack'), ('c', 'jack'), ('c', 'jack'), ('c', 'jack'), ('c', 'queen')],  # 4
            [('c', 4), ('c', 2), ('c', 3), ('c', 6), ('c', 5)],  # poker
        ]
        self._assert_hands_in_order(hands_weakest_to_strongest)

    def _assert_hands_in_order(self, layouts):
        for score1, hand1 in enumerate(layouts):
            for score2, hand2 in enumerate(layouts):
                actual = sign(hand_value(hand1) - hand_value(hand2))
                expected = sign(score1 - score2)
                self.assertEqual(actual, expected, f'\nh1: {hand1}\nh2: {hand2}')


def sign(x):
    if x < 0:
        return -1
    if x > 0:
        return 1
    return 0
