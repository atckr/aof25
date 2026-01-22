# Day 01, Part 1 + Part 2

This problem tracks a position on a circular number line (0-100) given a series of left/right movements. The parser packs each move as direction (1 bit) + distance into a 32-bit value sent via `Numeric_shifter.U32`.

The Python reference computes boundary crossings in one shot: `needed` is the distance to the next boundary, and if `dist >= needed`, we add `1 + (dist - needed) // 100` to part 2. Position is updated with modular arithmetic.

In HardCaml, we break this into multiple states. `Calculate_crosses` computes `needed` and checks if we cross. `Process_crosses` loops to count additional full crossings (subtracting 100 each iteration) rather than using division. `Update_count` normalizes the position based on direction, and `Update_part1` checks if we landed exactly at zero for part 1.

This approach avoids division hardware at the cost of extra clock cycles when crossing multiple boundaries.
