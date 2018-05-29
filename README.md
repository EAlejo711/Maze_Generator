# Running

Use the following predicate to generate a maze of `Width` and `Height`. The list `Edges` is filled with edges which represent the whole maze. The list `Path` contains the shortest solution (path from upper left to lower right corner).

```Prolog
maze(+Width, +Height, -Edges, -Path)
```

Moreover, it prints the maze and its variant with highlighted shortest solution.

For example:

```Prolog
maze(39, 5, Edges, Path).

###############################################################################
>...........#...#...#...............#...#.......#...#.#...#.....#.....#...#...#
#.###########.#.#.#.#.#.###.#######.###.#.###.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#.#
#.............#...#.#.#...#.#.....#...#...#.#.#...#.#...#...#.#.#.#.#...#.#.#.#
#.#################.#.###.###.###.###.#.###.#.#####.#.#######.#.#.#.#####.###.#
#.#.#...............#...#.....#...#...#.....#...#...#.#.......#.#.#.....#.....#
#.#.#.###############.###.#####.#####.#.#######.#.#####.#######.#######.#####.#
#.#.#.#.........#.....#.#.#...#.....#.#.#.......#.....#.....#.#.#...#...#...#.#
#.#.#.#.#######.#.#####.###.#.#####.#.###.###########.#####.#.#.#.#.#.###.###.#
#...#.........#.......#.....#.......#.....#.................#.....#...#.......<
###############################################################################

###############################################################################
>@..........#@@@#@@@#@@@@@@@@@@@@@@@#...#....@@@#@@@#.#...#..@@@#..@@@#@@@#...#
#@###########@#@#@#@#@#.###.#######@###.#.###@#@#@#@#.#.#.#.#@#@#.#@#@#@#@#.#.#
#@@@@@@@@@@@@@#@@@#@#@#...#.#.....#@@@#...#.#@#@@@#@#...#...#@#@#.#@#@@@#@#.#.#
#.#################@#@###.###.###.###@#.###.#@#####@#.#######@#@#.#@#####@###.#
#.#.#@@@@@@@@@@@@@@@#@..#.....#...#..@#.....#@@@#@@@#.#@@@@@@@#@#.#@@@@@#@@@@@#
#.#.#@###############@###.#####.#####@#.#######@#@#####@#######@#######@#####@#
#.#.#@#@@@@@@@@@#@@@@@#.#.#...#.....#@#.#@@@@@@@#@@@@@#@@@@@#.#@#@@@#@@@#...#@#
#.#.#@#@#######@#@#####.###.#.#####.#@###@###########@#####@#.#@#@#@#@###.###@#
#...#@@@......#@@@....#.....#.......#@@@@@#..........@@@@@@@#..@@@#@@@#......@<
###############################################################################

Edges = [edge(128, 167), edge(167, 168), edge(168, 169), edge(169, 130), 
	edge(130, 131), edge(131, 170), edge(170, 171), edge(..., ...)|...],
Path = [edge(0, 39), edge(39, 40), edge(40, 41), edge(41, 42), edge(42, 43), 
	edge(43, 44), edge(44, 45), edge(45, 6), edge(..., ...)|...].
```


# Description

An Prolog implementation of the Recursive Backtracker with a simple pseudo-random generator. The resulting maze is represented by a list of edges. Maze fields are represented by a single one-dimensional list. The visited fields are stored using the dynamic predicate `seen(+VisitedField)`.

We use the Lieanr Congruent Generator to obtain pseudo-random numbers. The used parameters are `a=214013`, `c=2531011`, `m=2^32` and we ignore lower 16 bits. The permutation is obtained from the Fisher–Yates Shuffle algorithm.

While printing, the edge list is iterated and fields are associated with particular characters (`.` for corridor or `@` for path). Indices which do not have an associated character are represented by `#`.