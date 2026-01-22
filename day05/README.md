# Day 05, Part 2

Given a list of ranges, we need to compute the total coverage length accounting for overlaps. The approach uses a difference array technique: mark +1 at each range start and -1 at each range end, then compute the running sum to determine coverage at each point.

We store starts and ends in separate dual-port RAMs, then run bubble sort on both arrays in parallel. Once sorted, we iterate through the events in order, accumulating deltas. When coverage transitions from zero to positive, we record the region start; when it drops back to zero, we add the region length to the result.
