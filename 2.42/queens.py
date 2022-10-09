from dataclasses import dataclass

@dataclass
class Posn:
    x: int
    y: int

def attack(posn1, posn2):
    if posn1.x == posn2.x:      # row
        return True
    if posn1.y == posn2.y:      # column
        return True
    if abs(posn1.x - posn2.x) == abs(posn1.y - posn2.y):  # diagnal
        return True
    return False

def queens(board_size):
    def safe(col, positions):
        assert isinstance(positions, list) and len(positions) >= 1
        kth_posn = positions[0]
        assert kth_posn.y == col
        rest_of_queens = positions[1:]
        for posn in rest_of_queens:
            if attack(kth_posn, posn):
                return False
        return True

    def adjoin_position(new_row, col, rest_of_queens):
        return [Posn(new_row, col)] + rest_of_queens

    def queen_cols(k):
        if k == 0:
            return [[]]
        else:
            # Assume that we have already generated the sequence of all possible ways
            # to place k−1 queens in the first k−1 columns of the board.
            placements = queen_cols(k - 1)

            # For each of these ways, generate an extended set of positions by placing
            # a queen in each row of the kth column.
            new_placements = [adjoin_position(new_row, k, a_placement)
                              for a_placement in placements for new_row in range(1, board_size + 1)]

            # Now filter these, keeping only the positions for which the queen in
            # the kth column is safe with respect to the other queens.
            return [a_placement for a_placement in new_placements if safe(k, a_placement)]

    return queen_cols(board_size)


for bd_size in [1, 2, 3, 4, 5, 8]:
    placements = queens(bd_size)
    print(f'board size: {bd_size}, sum of placements: {len(placements)}')
