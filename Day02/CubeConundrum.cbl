      * Advent of Code 2023, day 02 - Cube Conundrum
      * GNU COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOCDAY02.

       ENVIRONMENT DIVISION.
      * Specify puzzle input to read. 
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'Day02Input.txt'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * Specification to just parse raw sequential data. 
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(200).
       
       WORKING-STORAGE SECTION.
       01 WS-INPUT-LINE PIC X(200).
       01 WS-EOF PIC A(1).

      * Parsing specific workspace storage.      
       01 WS-PARSE-GAME-DELIMITER PIC X(2).
       01 WS-PARSE-GAME-ID PIC X(8).       
       01 WS-PARSE-GAME-NON-ID PIC X(191).
       01 WS-PARSE-GAME-ID1 PIC A(4).       
       01 WS-PARSE-GAME-ID2 PIC 9(4).
       
       01 WS-GAME-RECORD PIC X(60).
       01 WS-PARSE-GAME PIC X(100).
       01 WS-PARSE-GAME-1 PIC X(100).
       01 WS-PARSE-GAME-2 PIC X(100).
       01 WS-PARSE-GAME-3 PIC X(100).
       01 WS-PARSE-GAME-4 PIC X(100).
       01 WS-PARSE-GAME-5 PIC X(100).
       01 WS-PARSE-GAME-6 PIC X(100).
       
       01 WS-PARSE-ROUND PIC X(20).
       01 WS-PARSE-ROUND-1 PIC X(20).
       01 WS-PARSE-ROUND-2 PIC X(20).
       01 WS-PARSE-ROUND-3 PIC X(20).

       01 WS-NO-CUBES PIC 9(2).
       01 WS-COLOUR PIC A(5).
       01 WS-NO-RED PIC 9(2).
       01 WS-NO-GREEN PIC 9(2).
       01 WS-NO-BLUE PIC 9(2).

       01 WS-NO-RED1 PIC 9(2).
       01 WS-NO-RED2 PIC 9(2).
       01 WS-NO-RED3 PIC 9(2).
       01 WS-NO-RED4 PIC 9(2).
       01 WS-NO-RED5 PIC 9(2).
       01 WS-NO-RED6 PIC 9(2).

       01 WS-NO-GREEN1 PIC 9(2).
       01 WS-NO-GREEN2 PIC 9(2).
       01 WS-NO-GREEN3 PIC 9(2).
       01 WS-NO-GREEN4 PIC 9(2).
       01 WS-NO-GREEN5 PIC 9(2).
       01 WS-NO-GREEN6 PIC 9(2).

       01 WS-NO-BLUE1 PIC 9(2).
       01 WS-NO-BLUE2 PIC 9(2).
       01 WS-NO-BLUE3 PIC 9(2).
       01 WS-NO-BLUE4 PIC 9(2).
       01 WS-NO-BLUE5 PIC 9(2).
       01 WS-NO-BLUE6 PIC 9(2).

      * Processing working storage.
       01 WS-GAME-VALID PIC 9(1).
       01 WS-RED-MIN PIC 9(2) VALUE 0.
       01 WS-GREEN-MIN PIC 9(2) VALUE 0.
       01 WS-BLUE-MIN PIC 9(2) VALUE 0.
       01 WS-POWER PIC 9(4) VALUE 0.
       01 WS-TOTAL1 PIC 9(4) VALUE 0.
       01 WS-TOTAL2 PIC 9(5) VALUE 0.

      * Data type to store all the games.
       01 WS-GAMES.
      *    Stores 100 games.
           03 WS-GAME OCCURS 100 TIMES.
               05 GAME-ID PIC 9(4).
      *        Assumes no more than 6 rounds will be required.               
               05 RED1 PIC 9(2).
               05 GREEN1 PIC 9(2).
               05 BLUE1 PIC 9(2).
               05 RED2 PIC 9(2).
               05 GREEN2 PIC 9(2).
               05 BLUE2 PIC 9(2).
               05 RED3 PIC 9(2).
               05 GREEN3 PIC 9(2).
               05 BLUE3 PIC 9(2).
               05 RED4 PIC 9(2).
               05 GREEN4 PIC 9(2).
               05 BLUE4 PIC 9(2).
               05 RED5 PIC 9(2).
               05 GREEN5 PIC 9(2).
               05 BLUE5 PIC 9(2).
               05 RED6 PIC 9(2).
               05 GREEN6 PIC 9(2).
               05 BLUE6 PIC 9(2).

       01 WS-SUB PIC S9(04) COMP.

      * Read through each line and load into WS-GAMES.
       PROCEDURE DIVISION.      
      *    Initialise subscript to 1.
           MOVE 1 TO WS-SUB
      *    Get row from file, parse it and load into WS-GAMES table.     
           OPEN INPUT INPUT-FILE.
               PERFORM UNTIL WS-EOF='Y'
                   READ INPUT-FILE INTO WS-INPUT-LINE
                       AT END MOVE 'Y' TO WS-EOF
                       NOT AT END PERFORM PARSE-GAME
                   END-READ
               END-PERFORM.
           CLOSE INPUT-FILE.

      *    Process table using subscript.
           PERFORM VARYING WS-SUB FROM 1 BY 1 UNTIL WS-SUB > 100 
               PERFORM IS-GAME-VALID
               IF WS-GAME-VALID = 1 THEN                   
                   COMPUTE WS-TOTAL1 = WS-TOTAL1 + GAME-ID(WS-SUB)
               END-IF               
               PERFORM GAME-POWER
               COMPUTE WS-TOTAL2 = WS-TOTAL2 + WS-POWER
           END-PERFORM.

      *    Final result will be in WS-TOTAL 1 & 2.
           DISPLAY "Part 1 answer: " WS-TOTAL1
           DISPLAY "Part 2 answer: " WS-TOTAL2
           STOP RUN.

      * Parse game.
       PARSE-GAME.
           MOVE ' ' TO WS-PARSE-GAME-1.
           MOVE ' ' TO WS-PARSE-GAME-2.
           MOVE ' ' TO WS-PARSE-GAME-3.
           MOVE ' ' TO WS-PARSE-GAME-4.
           MOVE ' ' TO WS-PARSE-GAME-5.
           MOVE ' ' TO WS-PARSE-GAME-6.

           UNSTRING WS-INPUT-LINE DELIMITED BY ':'
               INTO WS-PARSE-GAME-ID
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-NON-ID
           UNSTRING WS-PARSE-GAME-ID
               INTO WS-PARSE-GAME-ID1
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-ID2
           UNSTRING WS-PARSE-GAME-NON-ID DELIMITED BY ';'
               INTO WS-PARSE-GAME-1
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-2
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-3
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-4
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-5
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-GAME-6.
           
           MOVE WS-PARSE-GAME-1 TO WS-PARSE-GAME.
           PERFORM PARSE-ROUND.
           MOVE WS-NO-RED TO WS-NO-RED1.
           MOVE WS-NO-GREEN TO WS-NO-GREEN1.
           MOVE WS-NO-BLUE TO WS-NO-BLUE1.

           MOVE WS-PARSE-GAME-2 TO WS-PARSE-GAME.
           PERFORM PARSE-ROUND.
           MOVE WS-NO-RED TO WS-NO-RED2.
           MOVE WS-NO-GREEN TO WS-NO-GREEN2.
           MOVE WS-NO-BLUE TO WS-NO-BLUE2.

           MOVE WS-PARSE-GAME-3 TO WS-PARSE-GAME.
           PERFORM PARSE-ROUND.
           MOVE WS-NO-RED TO WS-NO-RED3.
           MOVE WS-NO-GREEN TO WS-NO-GREEN3.
           MOVE WS-NO-BLUE TO WS-NO-BLUE3.

           MOVE WS-PARSE-GAME-4 TO WS-PARSE-GAME.
           PERFORM PARSE-ROUND
           MOVE WS-NO-RED TO WS-NO-RED4.
           MOVE WS-NO-GREEN TO WS-NO-GREEN4.
           MOVE WS-NO-BLUE TO WS-NO-BLUE4.

           MOVE WS-PARSE-GAME-5 TO WS-PARSE-GAME.
           PERFORM PARSE-ROUND.
           MOVE WS-NO-RED TO WS-NO-RED5.
           MOVE WS-NO-GREEN TO WS-NO-GREEN5.
           MOVE WS-NO-BLUE TO WS-NO-BLUE5.

           MOVE WS-PARSE-GAME-6 TO WS-PARSE-GAME.
           PERFORM PARSE-ROUND.
           MOVE WS-NO-RED TO WS-NO-RED6.
           MOVE WS-NO-GREEN TO WS-NO-GREEN6.
           MOVE WS-NO-BLUE TO WS-NO-BLUE6.

           STRING WS-PARSE-GAME-ID2 DELIMITED BY SIZE
               WS-NO-RED1 DELIMITED BY SIZE
               WS-NO-GREEN1 DELIMITED BY SIZE
               WS-NO-BLUE1 DELIMITED BY SIZE
               WS-NO-RED2 DELIMITED BY SIZE
               WS-NO-GREEN2 DELIMITED BY SIZE
               WS-NO-BLUE2 DELIMITED BY SIZE
               WS-NO-RED3 DELIMITED BY SIZE
               WS-NO-GREEN3 DELIMITED BY SIZE
               WS-NO-BLUE3 DELIMITED BY SIZE
               WS-NO-RED4 DELIMITED BY SIZE
               WS-NO-GREEN4 DELIMITED BY SIZE
               WS-NO-BLUE4 DELIMITED BY SIZE
               WS-NO-RED5 DELIMITED BY SIZE
               WS-NO-GREEN5 DELIMITED BY SIZE
               WS-NO-BLUE5 DELIMITED BY SIZE
               WS-NO-RED6 DELIMITED BY SIZE
               WS-NO-GREEN6 DELIMITED BY SIZE
               WS-NO-BLUE6 DELIMITED BY SIZE
           INTO WS-GAME-RECORD.

      *    Load into WS-GAMES table.
           MOVE WS-GAME-RECORD TO WS-GAME(WS-SUB).
           COMPUTE WS-SUB = WS-SUB + 1.

      * Parse round.
       PARSE-ROUND.
           MOVE 0 TO WS-NO-RED.
           MOVE 0 TO WS-NO-GREEN.
           MOVE 0 TO WS-NO-BLUE.
           MOVE ' ' TO WS-PARSE-ROUND-1.
           MOVE ' ' TO WS-PARSE-ROUND-2.
           MOVE ' ' TO WS-PARSE-ROUND-3.
           MOVE FUNCTION TRIM(WS-PARSE-GAME) TO WS-PARSE-GAME.
           
           UNSTRING WS-PARSE-GAME DELIMITED BY ', '
               INTO WS-PARSE-ROUND-1
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-ROUND-2
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-PARSE-ROUND-3
           
           MOVE WS-PARSE-ROUND-1 TO WS-PARSE-ROUND
           PERFORM PARSE-CUBES.
           MOVE WS-PARSE-ROUND-2 TO WS-PARSE-ROUND
           PERFORM PARSE-CUBES.
           MOVE WS-PARSE-ROUND-3 TO WS-PARSE-ROUND
           PERFORM PARSE-CUBES.

      * Parse cubes and colour.
       PARSE-CUBES.
           UNSTRING WS-PARSE-ROUND DELIMITED BY ' '
               INTO WS-NO-CUBES
               DELIMITER IN WS-PARSE-GAME-DELIMITER
               WS-COLOUR           
           
           IF WS-COLOUR = 'red' THEN
               MOVE WS-NO-CUBES TO WS-NO-RED
           END-IF.
           
           IF WS-COLOUR = 'green' THEN
               MOVE WS-NO-CUBES TO WS-NO-GREEN
           END-IF.
           
           IF WS-COLOUR = 'blue' THEN
               MOVE WS-NO-CUBES TO WS-NO-BLUE
           END-IF.
        
      * Determine if game is valid.
       IS-GAME-VALID.
           MOVE 1 TO WS-GAME-VALID.

           IF RED1(WS-SUB)>12 OR RED2(WS-SUB)>12 OR RED3(WS-SUB)>12 
           OR RED4(WS-SUB)>12 OR RED5(WS-SUB)>12 OR RED6(WS-SUB)>12 THEN
               COMPUTE WS-GAME-VALID = WS-GAME-VALID - 1
           END-IF.

           IF WS-GAME-VALID = 1 AND (GREEN1(WS-SUB)>13 OR 
           GREEN2(WS-SUB)>13 OR GREEN3(WS-SUB)>13 OR 
           GREEN4(WS-SUB)>13 OR GREEN5(WS-SUB)>13 OR 
           GREEN6(WS-SUB)>13) THEN
               COMPUTE WS-GAME-VALID = WS-GAME-VALID - 1
           END-IF.

           IF WS-GAME-VALID = 1 AND (BLUE1(WS-SUB)>14 OR 
           BLUE2(WS-SUB)>14 OR BLUE3(WS-SUB)>14 OR 
           BLUE4(WS-SUB)>14 OR BLUE5(WS-SUB)>14 OR 
           BLUE6(WS-SUB)>14) THEN
               COMPUTE WS-GAME-VALID = WS-GAME-VALID - 1
           END-IF.
    
      * Get game power.
       GAME-POWER.
           COMPUTE WS-RED-MIN = FUNCTION MAX(RED1(WS-SUB),RED2(WS-SUB),
           RED3(WS-SUB),RED4(WS-SUB),RED5(WS-SUB),RED6(WS-SUB)).

           COMPUTE WS-GREEN-MIN = FUNCTION MAX(GREEN1(WS-SUB),
           GREEN2(WS-SUB),GREEN3(WS-SUB),GREEN4(WS-SUB),GREEN5(WS-SUB),
           GREEN6(WS-SUB)).

           COMPUTE WS-BLUE-MIN = FUNCTION MAX(BLUE1(WS-SUB),
           BLUE2(WS-SUB),BLUE3(WS-SUB),BLUE4(WS-SUB),BLUE5(WS-SUB),
           BLUE6(WS-SUB)).

           COMPUTE WS-POWER = WS-RED-MIN * WS-GREEN-MIN * WS-BLUE-MIN.
