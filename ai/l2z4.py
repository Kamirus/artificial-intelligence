from commando import CommandoReducer, CommandoSeeker, split_possible_commandos


def main() -> None:
    with open('zad_output.txt', 'w') as f:
        c = CommandoReducer(debug=False)
        moves = c.reduce()

        board, state = split_possible_commandos(c.full_map)
        res = CommandoSeeker(board, c.targets, state, moves).search_bfs()
        print(res, file=f)


if __name__ == '__main__':
    main()
