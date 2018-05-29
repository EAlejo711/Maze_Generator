%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                      %
%                         Prolog Maze Generator                        %
%                         Recursive Backtracker                        %
%                           Tomas Nekvinda                             %
%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Dynamic memory of unvsited fields
:- dynamic seen/1.



%%%%%%%%%%%%%%%%%%%%%% Predicates for maze printing %%%%%%%%%%%%%%%%%%%%

printMaze(Width, Height, MazeEdges) :-
    printMaze(Width, Height, MazeEdges, []).

% Manage printing of the generated maze, MazeEdges are corridors, 
% PathEdges are used for printing paths in the maze
printMaze(Width, Height, MazeEdges, PathEdges) :-
    % Creation of the list for priting, walls are converted to fields
    PrintWidth  is Width  * 2 + 1,
    PrintHeight is Height * 2 + 1,
    PrintLength is PrintWidth * PrintHeight,
    length(MazeForPrint, PrintLength),
    % Associate vertices and edges with approproate characters
    setEdges(Width, Height, PathEdges, MazeForPrint, '@'),
    setEdges(Width, Height, MazeEdges, MazeForPrint, '.'),
    % The starting posititon in the maze
    nth0(PrintWidth, MazeForPrint, '>'),
    % The goal position in the maze
    End is PrintLength - 1 - PrintWidth,
    nth0(End, MazeForPrint, '<'),
    printBoard(PrintWidth, 0, MazeForPrint).

% Associate field with a charater, if not set already
setCell(CellIndex, Board, Char) :-
    \+ atom(CellIndex),
    nth0(CellIndex, Board, Char).
setCell(_, _, _).

% Sets characters at indices in the Prited list which correspond to 
% vertices and endges which are in the third arg
setEdges(_, _, [], _, _).
setEdges(Width, Height, [edge(From, To) | Edges], Printed, Char) :-
    % Prepare coordiantes x, y in the context of the original maze
    coordinates(Width, From, FromRow, FromCol),
    coordinates(Width, To  , ToRow,   ToCol),
    % Transform coordiantes x, y into the context of the maze to be disaplyed
    PrintFromCol is FromCol * 2 + 1,
    PrintFromRow is FromRow * 2 + 1,
    PrintToCol   is ToCol   * 2 + 1,
    PrintToRow   is ToRow   * 2 + 1,
    % Prepare index of the corridor between two vertices From and To
    PathRow      is (PrintFromRow + PrintToRow) / 2,
    PathCol      is (PrintFromCol + PrintToCol) / 2,
    % Translates souøadnice x, y into the index of maze to be displyed (list)
    PrintFrom    is PrintFromRow * (Width * 2 + 1) + PrintFromCol,
    Path         is PathRow      * (Width * 2 + 1) + PathCol,
    PrintTo      is PrintToRow   * (Width * 2 + 1) + PrintToCol,
    % Sets Char at prepared indices
    setCell(PrintFrom, Printed, Char),
    setCell(Path, Printed, Char),
    setCell(PrintTo, Printed, Char),
    % Recursive call and processing og a next edge
    setEdges(Width, Height, Edges, Printed, Char).

% Display of the created maze
printBoard(_, _, []).
% Print this character if the field was not associated with any other char
printBoard(Max, Curr, [Wall | Printed]) :-
    atom(Wall),!,
    write(Wall),
    NewCurr is (Curr + 1) mod Max,
    % Print new line if at the end of maze row
    printLine(NewCurr),
    printBoard(Max, NewCurr, Printed).
% Otherwise print char which represents wall
printBoard(Max, Curr, [_ | Printed]) :-
    write('#'),
    NewCurr is (Curr + 1) mod Max,
    % Print new line if at the end of maze row
    printLine(NewCurr),
    printBoard(Max, NewCurr, Printed).

% Print new line if arg is zero
printLine(0) :- nl.
printLine(_).

% Transform index of a field which represents rectangle of width W
% into coords of row and column
coordinates(Width, Index, Row, Col) :-
    Row is Index // Width,
    Col is Index mod Width.



%%%%%%%%%%%%%%%%%%%%%%%% Pseudo-random generating %%%%%%%%%%%%%%%%%%%%%%

% Swap items of List at I and J and return new list
swap(List, I, J, New) :-
    % We remove the bigger item first
    ( I > J -> (X,Y) = (J,I) ; (X,Y) = (I,J) ),
    % Remove item from index Y of List and save into A; item saves into Y_out
    nth0(Y, List, Y_out, A),
    % Remove item from index X of A and save into B; item saves into X_out
    nth0(X, A, X_out, B),
    % Add item Y_out to index X and save into C
    nth0(X, C, Y_out, B),
    % Add item X_out to index Y and save into New
    nth0(Y, New, X_out, C).

% Generate permutation of Source and return in Result
shuffle(Source, Result) :-
    length(Source, N),
    shuffle(0, N, Source, Result),!.

% From Range items from I randomly select and swap the item at
% I + Random, recursively repeat - see Fisher–Yates shuffle
shuffle(_, Range, Source, Source) :-
    Range < 2.
shuffle(I, Range, Source, Result) :-
    % Random number from Range
    lcg(Random, Range),
    J is I + Random,
    % swap items if I = J, thus this condition
    ( Random = 0 -> UpdatedSource = Source ;
                    swap(Source, I, J, UpdatedSource) ),
    % Move t the next items
    NewI is I + 1,
    % Reduce Range for item selection
    NewRange is Range - 1,
    shuffle(NewI, NewRange, UpdatedSource, Result).

% Generate integer from the range [0,Range)
lcg(InRangeRnd, Range) :-
    % Load generated item from the global memory
    nb_getval(seed, X),
    % Constants - see Linear Congruential Generator
    A = 214013,
    C = 2531011,
    M = 2147483648,
    Rnd is ((X * A + C) mod M) >> 16,
    InRangeRnd is (Rnd * Range) // 32768,
    % Save new item from the psudo-random series
    nb_setval(seed, Rnd).

% Choose seed and save it
lcgInitialize(Seed) :-
   MasiveChangeSeed is Seed * 1000,
   Value is round(MasiveChangeSeed) mod 2147483648,
   nb_setval(seed, Value).



%%%%%%%%%%%%%%%%%%%%%%%%%%% Maze generation %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generate maze represented by edges of given size Width * Heightstart 
% from the field current
generateMaze(Width, Height, Current, MazeEdges) :-
    % Generates possible steps - list with max 4, min 0 items
    nextSteps(Width, Height, Current, NewSteps),
    % Iterates possible steps and continues recursively, marks  fields
    % as visited and pass edge from the current to the goal field
    findall(
        [Edge | ResursiveMazeEdges],
        ( member(New, NewSteps),
          \+ seen(New),
          assert(seen(New)),
          Edge = edge(Current, New),
          generateMaze(Width, Height, New, ResursiveMazeEdges) ),
    ListOfRecursiveMazeEdgesList),
    % Union of edge lists for steps into a signle list
    append(ListOfRecursiveMazeEdgesList, MazeEdges).

% Generate steps (max 4) and randomly shuffle them
nextSteps(Width, Height, Old, Permutation) :-
    findall(New, nextStep(Width, Height, Old, New), PosibleSteps),
    shuffle(PosibleSteps, Permutation).
nextStep(Width, Height, Old, New) :-
    coordinates(Width, Old, Row, Col),
    ( Row < Height - 1, New is Old + Width;
      Row > 0,          New is Old - Width;
      Col < Width - 1,  New is Old + 1;
      Col > 0,          New is Old - 1 ).



%%%%%%%%%%%%%%%%%%%%%%%%% Shortest path search %%%%%%%%%%%%%%%%%%%%%%%%%

findPath(End, End, _, _, []).
findPath(Start, End, Edges, Memory, [edge(Start, EdgeEnd) | Path]) :-
    ( member(edge(Start, EdgeEnd), Edges) ;
      member(edge(EdgeEnd, Start), Edges) ),
    \+ member(EdgeEnd, Memory),
    findPath(EdgeEnd, End, Edges, [EdgeEnd | Memory], Path).

solution(Start, End, Edges, Path) :-
    findPath(Start, End, Edges, [Start], Path).



%%%%%%%%%%%%%%%%%%%%%%%%%%% Program entry point %%%%%%%%%%%%%%%%%%%%%%%%%

% Generates, prints and saves maze into MazeEdges; Width and Height
% defines size of the maze, Path is the shortes solution (generated
% maze is always acyclic, because it uses Recursive Backtracker,
% but searching for path workds also in the case of cyclic mazes)
maze(Width, Height, MazeEdges, Path) :-
    % Destroy saved dynamic predicates
    retractall(seen(_)),
    % Time is used for incialization of the pseudo-random generator
    get_time(Time),
    lcgInitialize(Time),
    % Size of the maze, selects random beginning field
    MazeSize is  Width * Height - 1,
    lcg(InitPos, MazeSize),
    assert(seen(InitPos)),
    % Maze generation
    generateMaze(Width, Height, InitPos, MazeEdges), nl,
    % Print the created maze
    printMaze(Width, Height, MazeEdges),!, nl,
    % Search the shortest path between fields at indices 0 and MazeSize
    solution(0, MazeSize, MazeEdges, Path),
    % Print maze with highlighted path
    printMaze(Width, Height, MazeEdges, Path),!, nl.





