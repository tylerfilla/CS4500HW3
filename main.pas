{*
 * Tyler Filla
 * March 4, 2019
 * CS 4500-001 :: Intro to Software Profession
 *
 * [ FOREWORD ]
 *
 * For my HW2 submission, I reused most code from my HW1 submission. However,
 * for my HW3 submission, I have decided to rewrite the program in an attempt to
 * apply what I have learned about Free Pascal since. In particular, I tried to
 * improve my encapsulation habits, preferring functions and records to
 * procedures and global state, respectively, and I tried to prefer immutability
 * where possible (e.g. using const references or pass-by-value).
 *
 * [ DESCRIPTION ]
 *
 * This program intends to implement the specified game and program behavior: A
 * strongly-connected digraph is drawn on an imaginary game board, and a marker
 * is randomly moved along the edges until all nodes have been visited. Each
 * node counts the number of times it has been visited, and some statistics are
 * produced after the game completes. This basic game is played three times on
 * boards defined in user-provided text files (the format of these files is
 * defined below, and some examples are provided, as well).
 *
 * [ INTERNALS ]
 *
 * As mentioned in the foreword, this program avoids global variables unlike my
 * my previous submissions. Multiple records are defined to facilitate operation
 * of the game: one record, TBoard, provides the game board; another,
 * TGameResults, tracks the statistics of one game; and another, TSeriesResults,
 * holds the results of all games played in a series. These records are
 * documented in more detail at their respective source code locations.
 *
 * I also simplified the internal representation of the circles and arrows.
 * Previously, I had declared a global dynamic array of circles, and I used the
 * SetLength function to allocate N circle records where each circle record
 * contained a dynamic array of pointers (representing arrows) to other circle
 * records (allocated in that same global array). In HW3, I instead chose a
 * cleaner method: I use a dynamic array of TCircle records in a TBoard record,
 * and each circle has a dynamic array of integer indices.
 *}

program HW3;

{$I+}
{$mode objfpc}

uses sysutils, regexpr;

const
    {*
     * The number of games to play.
     *}
    C_NUM_GAMES = 3;

    {*
     * The name of the output file.
     *}
    C_FILENAME_OUT = 'HW3fillaOutfile.txt';

    {*
     * The maximum number of marks to distribute in a game.
     *}
    C_MAXIMUM_MARKS = 1000000;

type
    {*
     * A circle.
     *}
    TCircle = record
        // Indices of target circles (outgoing arrows).
        arrows: array of integer;

        // The number of checkmarks placed on the circle.
        marks: integer;
    end;

    {*
     * A game board.
     *
     * This most closely represents one game input.
     *
     * A single TBoard record represents the state of a single game board. As
     * the game is played, the board state mutates.
     *}
    TBoard = record
        // The name of the source file from which the board was loaded.
        filename: string;

        // The number of circles on the board.
        numCircles: integer;

        // The number of arrows on the board.
        numArrows: integer;

        // The circles on the board.
        circles: array of TCircle;

        // The total number of checkmarks placed on the board.
        marks: integer;
    end;

    {*
     * Single game results.
     *
     * This most closely represents one game output.
     *
     * As a game is being played on a board defined by a TBoard record, the
     * PlayGame function collects gameplay statistics and results into one of
     * these records. Upon completion, the pair of records, one TBoard and one
     * TGameResults, together represent the game and its simulation.
     *}
    TGameResults = record
        // A copy of the source game board.
        board: TBoard;

        // The maximum number of checks on a circle on the board.
        maxChecks: integer;

        // The average number of checks on a circle on the board.
        avgChecks: real;
    end;

    {*
     * Game series results.
     *
     * This most closely represents the final table.
     *
     * After a number of games have been played, and their respective
     * TGameResults records collected, the ComputeResults function may be used
     * to produce a TSeriesResults record containing the final statistics.
     *}
    TSeriesResults = record
        // Copies of the source game results.
        games: array of TGameResults;

        // The total number of circles over all games.
        totalCircles: integer;

        // The total number of arrows over all games.
        totalArrows: integer;

        // The maximum number of checks on a circle on any game's board.
        overallMaxChecks: integer;

        // The average number of checks on a circle on any game's board.
        overallAvgChecks: real;
    end;

    {*
     * An exception for input file errors.
     *
     * This is a catch-all exception thrown by the input file parser procedure
     * when something doesn't go its way. Its message contains more details.
     *}
    EInputFileException = class(Exception);

    {*
     * An exception for when the maximum mark limit is reached.
     *}
    EMaximumMarksException = class(Exception);

var
    // The output file.
    outputFile: TextFile;

{*
 * Output the given string to the terminal and the output file.
 *}
procedure Out(constref text: string);
begin
    // To the screen
    Write(Output, text);

    // To the output file
    Write(outputFile, text);
end;

{*
 * Output the given string on a new line to the terminal and the output file.
 *}
procedure OutLn(constref text: string);
begin
    // To the screen
    WriteLn(Output, text);

    // To the output file
    WriteLn(outputFile, text);
end;

// Read a file and load it for a game board
function LoadBoardFile(constref filename: string): TBoard;
var
    // The loaded board.
    board: TBoard;

    // The board file.
    boardFile: TextFile;

    // A regular expression to extract arrow details.
    arrowRegExpr: TRegExpr;

    // A temporary arrow definition line.
    tempArrowLine: string;

    // A temporary arrow number.
    tempArrowNum: integer;

    // A temporary arrow destination index.
    tempArrowIdxDst: integer;

    // A temporary arrow source index.
    tempArrowIdxSrc: integer;

begin
    board.filename := filename;
    board.marks := 0;

    // The input file
    AssignFile(boardFile, filename);

    // Create the regular expression for pulling out arrow details
    arrowRegExpr := TRegExpr.Create;
    arrowRegExpr.Expression := '\d+';

    try
        try
            // Open the board file for read
            Reset(boardFile);

            // Try to read variable N (number of circles)
            if Eof(boardFile) then
                raise EInputFileException.create('Failed to read N: Premature EOF');
            try
                ReadLn(boardFile, board.numCircles);
            except
                on E: Exception do
                    raise EInputFileException.create(Format('Failed to read N: %s: %s', [E.ClassName, E.Message]));
            end;

            // Check range of N (from 2 to 20)
            if (board.numCircles < 2) or (board.numCircles > 20) then
                raise EInputFileException.create(Format('N is out of range: %d', [board.numCircles]));

            // Allocate N circles
            SetLength(board.circles, board.numCircles);

            // Try to read variable K (number of arrows)
            if Eof(boardFile) then
                raise EInputFileException.create('Failed to read K: Premature EOF');
            try
                ReadLn(boardFile, board.numArrows);
            except
                on E: Exception do
                    raise EInputFileException.create(Format('Failed to read K: %s: %s', [E.ClassName, E.Message]));
            end;

            // Check range of K (from 2 to 100)
            if (board.numArrows < 2) or (board.numArrows > 100) then
                raise EInputFileException.create(Format('K is out of range: %d', [board.numArrows]));

            // Read in K arrow definitions
            for tempArrowNum := 1 to board.numArrows do
            begin
                // Try to read arrow line
                if Eof(boardFile) then
                    raise EInputFileException.create(Format('Failed to read arrow %d: Premature EOF', [tempArrowNum]));
                try
                    ReadLn(boardFile, tempArrowLine);
                except
                    on E: Exception do
                        raise EInputFileException.create(Format('Failed to read arrow %d: %s: %s', [tempArrowNum, E.ClassName, E.Message]));
                end;

                // Pull source and destination indices from line
                if arrowRegExpr.Exec(tempArrowLine) then
                begin
                    // Pull source index
                    try
                        tempArrowIdxSrc := arrowRegExpr.Match[0].ToInteger;
                    except
                        on E: Exception do
                            raise EInputFileException.create(Format('Failed to read arrow %d source: %s: %s', [tempArrowNum, E.ClassName, E.Message]));
                    end;

                    // Pull destination index
                    arrowRegExpr.ExecNext;
                    try
                        tempArrowIdxDst := arrowRegExpr.Match[0].ToInteger;
                    except
                        on E: Exception do
                            raise EInputFileException.create(Format('Failed to read arrow %d destination: %s: %s', [tempArrowNum, E.ClassName, E.Message]));
                    end;
                end;

                // Check range of arrow source index (from 1 to N)
                if (tempArrowIdxSrc < 1) or (tempArrowIdxSrc > board.numCircles) then
                    raise EInputFileException.create(Format('Source index for arrow %d is out of range: %d', [tempArrowNum, tempArrowIdxSrc]));

                // Check range of arrow destination index (from 1 to N)
                if (tempArrowIdxDst < 1) or (tempArrowIdxDst > board.numCircles) then
                    raise EInputFileException.create(Format('Destination index for arrow %d is out of range: %d', [tempArrowNum, tempArrowIdxDst]));

                { Establish the arrow connection in memory. This increments the
                  length of the source circle's arrow array and appends to it
                  the index of the destination circle. }
                SetLength(board.circles[tempArrowIdxSrc - 1].arrows, Length(board.circles[tempArrowIdxSrc - 1].arrows) + 1);
                board.circles[tempArrowIdxSrc - 1].arrows[High(board.circles[tempArrowIdxSrc - 1].arrows)] := tempArrowIdxDst;
            end;
        except
            // Rethrow miscellaneous I/O errors under catch-all exception
            on E: EInOutError do
                raise EInputFileException.create(Format('Failed to read input file: %s: %s ', [E.ClassName, E.Message]));
        end;
    finally
        // Close the input file
        CloseFile(boardFile);

        // Free arrow regular expression
        arrowRegExpr.Free;
    end;

    // Return loaded board
    LoadBoardFile := board;
end;

{*
 * Determine whether a board is strongly-connected.
 *
 * Verification is performed with an exhaustive search from the perspective of
 * each circle. For each pair of circles, the graph is searched for a path that
 * connects the two circles in the pair.
 *}
function IsBoardStrong(constref board: TBoard): boolean;
var
    // The index of the first circle in the current pair.
    a: integer;

    // The index of the second circle in the current pair.
    b: integer;

    // The set of discovered circles yet to be explored.
    openSet: array of integer;

    // A temporary circle index.
    tempCircle: integer;

    // An iterator index.
    i: integer;

label
    // A label to which jumping scans the next circle.
    nextCircle;

begin
    // Choose pairs of circles
    for a := 1 to board.numCircles do
        for b := 1 to board.numCircles do
        begin
            // Get initialized for the search
            SetLength(openSet, 0);
            tempCircle := 0;

//          OutLn(Format('-> Looking for a path from circle %d to circle %d', [b, a]));

            // Ignore trivial cases where both circles are the same
            if a = b then
                continue;

            // Add circle B to the open set
            SetLength(openSet, Length(openSet) + 1);
            openSet[High(openSet)] := b;

            { Try to find a path from circle B back to circle A. If we cannot, then
              the system is not strongly-connected. }
            while true do
            begin
                { Scan the open set for circle A. If we find circle A in the
                  open set, then we have discovered a path from circle B to
                  circle A. }
                for i := 1 to Length(openSet) do
                begin
                    if openSet[i - 1] = a then
                    begin
//                      OutLn(Format('  -> Found with %d nonterminal(s) remaining', [Length(openSet)]));
                        goto nextCircle;
                    end;
                end;

                { We bail out quickly when we discover paths. So, it follows
                  that if, at any point, the open set is empty, then there is no
                  connection (and the system is not strongly-connected). }
                if Length(openSet) = 0 then
                begin
//                  OutLn('  -> NOT FOUND');
//                  OutLn('');
//                  OutLn(Format('No path from circle %d to circle %d!', [b, a]));
//                  OutLn('The configured graph is not strongly-connected! Bailing out...');
                    IsBoardStrong := false;
                    Exit;
                end;

                // Pop the next circle from the array
                tempCircle := openSet[Low(openSet)];
                for i := 1 to Length(openSet) - 1 do
                    openSet[i - 1] := openSet[i];
                SetLength(openSet, Length(openSet) - 1);

                // Add all newly-reachable circles to open set
                for i := 1 to Length(board.circles[tempCircle - 1].arrows) do
                begin
                    SetLength(openSet, Length(openSet) + 1);
                    openSet[High(openSet)] := board.circles[tempCircle - 1].arrows[i - 1];
                end;
            end;
        nextCircle:
        end;

    OutLn('This board is a strongly-connected digraph.');
    IsBoardStrong := true;
end;

{*
 * Play a single game on the given board. Returns the gameplay results.
 *}
function PlayGame(board: TBoard): TGameResults;
var    
    // The game results.
    results: TGameResults;

    // The index of the current circle.
    currentCircle: integer;

    // The number of unique circles marked.
    numUniqueCirclesMarked: integer;

    // A temporary count of arrows.
    tempNumArrows: integer;

begin
    // Init results
    results.maxChecks := 0;
    results.avgChecks := 0;

    OutLn('Gameplay is about to begin.');

    // Start at first circle
    currentCircle := 1;
    numUniqueCirclesMarked := 0;

    // Core gameplay loop
    while numUniqueCirclesMarked < board.numCircles do
    begin
        // Mark the current circle
        Inc(board.circles[currentCircle - 1].marks);
        Inc(board.marks);

        // Check the maximum mark limit
        if board.marks > C_MAXIMUM_MARKS then
            raise EMaximumMarksException.create(Format('Maximum number of marks reached (%d)', [C_MAXIMUM_MARKS]));

        // Update max check statistics
        if board.circles[currentCircle - 1].marks > results.maxChecks then
        begin
            results.maxChecks := board.circles[currentCircle - 1].marks;
        end;

        // If current circle's count just hit one
        // This would indicate reaching the circle for the first time
        if board.circles[currentCircle - 1].marks = 1 then
        begin
            // Increment the corresponding counter
            Inc(numUniqueCirclesMarked);

            OutLn(Format(' -> Reached %d unique circles out of %d', [numUniqueCirclesMarked, board.numCircles]));
        end;

        // Count the number of arrows pointing *away* from the current circle
        tempNumArrows := Length(board.circles[currentCircle - 1].arrows);

        // If no arrows are available, panic
        // This indicates a bug in the board verifier function
        if tempNumArrows = 0 then
        begin
            OutLn('INTERNAL FAILURE (THIS IS A BUG)');
            OutLn('The strong connection verifier cleared a non-strongly-connected digraph!');
            CloseFile(outputFile);
            Halt;
        end;

        // Move to a randomly-chosen circle pointed to by the current circle
        // The "arrows" array on the current circle contains indices of target circles
        // The act of "moving" is simply that of assigning the index to "currentCircle"
        currentCircle := board.circles[currentCircle - 1].arrows[Random(tempNumArrows)];
    end;

    // Copy updated board through
    results.board := board;

    // Compute average for board
    results.avgChecks := board.marks / board.numCircles;

    // Return game results
    PlayGame := results;
end;

{*
 * Crunch the numbers on the given per-game results to get the series results.
 * This function performs statistics, such as averaging, over all the games.
 *}
function ComputeResults(constref games: array of TGameResults): TSeriesResults;
var
    // The series results.
    results: TSeriesResults;

    // A temporary running total of checks over all games.
    tempTotalChecks: real;

    // An iterator index.
    i: integer;

begin
    tempTotalChecks := 0;

    // Init results
    SetLength(results.games, Length(games));
    results.totalCircles := 0;
    results.totalArrows := 0;
    results.overallMaxChecks := 0;
    results.overallAvgChecks := 0;

    for i := 1 to Length(games) do
    begin
        // Keep running total of checkmarks
        tempTotalChecks := tempTotalChecks + games[i - 1].board.marks;

        // Copy game results through
        results.games[i - 1] := games[i - 1];

        // Sum circles and arrows
        results.totalCircles := results.totalCircles + games[i - 1].board.numCircles;
        results.totalArrows := results.totalArrows + games[i - 1].board.numArrows;

        // Update overall max checks statistic
        if games[i - 1].maxChecks > results.overallMaxChecks then
        begin
            results.overallMaxChecks := games[i - 1].maxChecks;
        end;
    end;

    // Compute overall average
    results.overallAvgChecks := tempTotalChecks / results.totalCircles;

    // Return series results
    ComputeResults := results;
end;

procedure DisplaySeriesResults(constref results: TSeriesResults);
var
    // An iterator index.
    i: integer;

begin
    OutLn(Format('Tabulating results for %d games...', [Length(results.games)]));

    // Table header
    OutLn('+--------+----+----+-------------+-------------+');
    OutLn('| Game # |  N |  K | Max. Checks | Avg. Checks |');
    OutLn('+--------+----+----+-------------+-------------+');

    // Table rows (one per game)
    for i := 1 to C_NUM_GAMES do
    begin
        OutLn(Format('| %6d | %2d | %2d | %11d | %11f |', [i, results.games[i - 1].board.numCircles, results.games[i - 1].board.numArrows, results.games[i - 1].maxChecks, results.games[i - 1].avgChecks]));
        OutLn('+--------+----+----+-------------+-------------+');
    end;

    // Total row
    OutLn(Format('| SERIES | %2d | %2d | %11d | %11f |', [results.totalCircles, results.totalArrows, results.overallMaxChecks, results.overallAvgChecks]));
    OutLn('+--------+----+----+-------------+-------------+');
end;

var
    // Array of results for each played game.
    gameResults: array of TGameResults;

    // Results over all played games.
    seriesResults: TSeriesResults;

    // A temporary filename.
    tempFilename: string;

    // A temporary game board.
    tempBoard: TBoard;

    // An iterator index.
    i: integer;

begin
    // Open the output file for write access
    try
        AssignFile(outputFile, C_FILENAME_OUT);
        Rewrite(OutputFile);
    except
        on E: Exception do
        begin
            WriteLn(Format('Failed to open output file for write: %s', [E.Message]));
            Exit;
        end;
    end;

    Randomize();

    OutLn(Format('Configured for %d games.', [C_NUM_GAMES]));
    OutLn('You will now be prompted to supply board files (relative).');
    OutLn('');

    // Run through games
    SetLength(gameResults, C_NUM_GAMES);
    i := 1;
    while i <= C_NUM_GAMES do
    begin
        // Prompt for board file
        OutLn(Format('Game #%d setup', [i]));
        Out('Input board file: ');
        ReadLn(tempFilename);

        OutLn(Format('Loading board file %s...', [tempFilename]));
        OutLn('');

        // Try to load the board file
        try
            tempBoard := LoadBoardFile(tempFilename);
        except
            on E: Exception do
            begin
                OutLn('There was a problem loading the board.');
                OutLn(Format('Error: %s', [E.Message]));
                OutLn('');

                // Reprompt this game
                Out('(again) ');
                continue;
            end;
        end;

        // Check the board for strong connectedness
        if IsBoardStrong(tempBoard) then
            begin
                OutLn('The board was loaded successfully!');
            end
        else
            begin
                OutLn('The board is not strongly-connected.');
                OutLn('');

                // Reprompt this game
                Out('(again) ');
                continue;
            end;

        OutLn('');

        // Try to play the game on this board and keep the results
        try
            gameResults[i - 1] := PlayGame(tempBoard);
        except
            on E: Exception do
            begin
                OutLn('The gameplay encountered a problem.');
                OutLn(Format('Error: %s', [E.Message]));
                OutLn('');

                // Reprompt this game
                Out('(again) ');
                continue;
            end;
        end;

        OutLn('Gameplay successful! Results have been stored.');
        OutLn('');

        i := i + 1;
    end;

    // Compute series results
    seriesResults := ComputeResults(gameResults);

    // Display series results table
    DisplaySeriesResults(seriesResults);

    OutLn('');
    OutLn(Format('Transcript is available in file %s', [C_FILENAME_OUT]));
    OutLn('');

    // Flush the output file buffer
    // Without this call, the output file is sometimes incomplete (???)
    Flush(outputFile);

    // For platforms with non-persistent consoles (like Windows)
    // This keeps the console window open for grading purposes on such platforms
    WriteLn('Press enter to continue...');
    ReadLn();
end.
