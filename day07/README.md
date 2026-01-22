# Day 07, Part 1 + Part 2

This problem simulates a beam of light traveling downward through a grid with splitters (`^`). The parser encodes each cell as 2 bits (`.`=0, `S`=1, `^`=2) and sends the grid width as the first byte, followed by the grid data row by row.

The Python reference maintains a `state` array (beam count per column) and processes each cell: when a splitter is hit with `state[j] > 0`, it increments the result, spreads the beam to `j-1` and `j+1` if they're empty (`.`), and clears `state[j]`.

In HardCaml, we process columns left-to-right, which complicates the spread logic. Rightward spread (`j+1`) is easy - we store the beam value in `pending_left` and add it when processing the next column. Leftward spread (`j-1`) requires going back to update an already-processed column, so we use `saved_beam` and extra states (`Wait_for_spread_read`, `Spread_write`) to read and update the previous column.

After processing all rows, part 2 is computed by summing all values in the state RAM, representing the total beam count across all cells.
