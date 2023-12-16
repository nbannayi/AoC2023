! Advent of Code 2023, Day 10 - Pipe Maze.
! Fortran.

! Store type info.
MODULE PipeModule
    IMPLICIT NONE

    ! Define a custom type for Pipe segments.
    INTEGER, PARAMETER :: maxRows = 140
    INTEGER, PARAMETER :: maxCols = 140

    TYPE :: Pipe
        CHARACTER(len=1) :: segment
        INTEGER :: distance        
    END TYPE Pipe
END MODULE PipeModule

! Main program
PROGRAM PipeMaze
    USE PipeModule
    IMPLICIT NONE

    CHARACTER(len=50) :: inputFile = 'Day10Input.txt'
    TYPE(Pipe), DIMENSION(maxRows,maxCols) :: pipeGrid
    INTEGER :: farthestDistance, noHoles

    ! Get all data into into 2D array.
    CALL ParseData(inputFile, pipeGrid)        
    
    ! Move all the way round.
    CALL MoveAll(pipeGrid, farthestDistance)

    ! Part 1.    
    PRINT *, 'Part 1 answer: ', farthestDistance

    ! Part 2.
    ! TODO.

    CALL PrintGrid(pipeGrid, 1)

END PROGRAM pipeMaze

! Parse input data into 2D array of Pipe segments.
SUBROUTINE ParseData(inputFile, pipeGrid)
    USE PipeModule
    IMPLICIT NONE

    CHARACTER(len=*), INTENT(IN) :: inputFile
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(OUT) :: pipeGrid
    CHARACTER(len=140) :: line 
    INTEGER :: r, c, ioStatus

    OPEN (1, file = inputFile, status = 'old')

    r = 0
    DO
        READ(1,*, iostat=ioStatus) line
        IF (ioStatus == -1) EXIT
        r = r + 1
        DO c = 1, maxCols            
            pipeGrid(r,c)%segment = line(c:(c+1)) 
            pipeGrid(r,c)%distance = 0
        END DO
    END DO
END SUBROUTINE ParseData

! Print the pipe grid contents.
! Pass 1 for segments, 2 for distance.
SUBROUTINE PrintGrid(pipeGrid, segmentsOrDistance)
    USE PipeModule
    IMPLICIT NONE

    INTEGER :: r, c
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(IN) :: pipeGrid
    INTEGER, INTENT(IN) :: segmentsOrDistance

    DO r = 1, maxRows
        DO c = 1, maxCols
            IF (pipeGrid(r,c)%distance .LT. 0) THEN
                WRITE(*, '(A)', advance='no') 'S'
            ELSE     
                IF (segmentsOrDistance .EQ. 1) THEN
                    WRITE(*, '(I0)', advance='no') pipeGrid(r,c)%distance
                ELSE
                    WRITE(*, '(A)', advance='no') pipeGrid(r,c)%segment
                END IF
            END IF
        END DO          
        IF (IACHAR(pipeGrid(r,1)%segment) .EQ. 0) THEN 
            EXIT
        END IF
        PRINT *, ""    
    END DO
    PRINT *,""
END SUBROUTINE PrintGrid

! Get start pipe.
SUBROUTINE GetStartPipe(pipeGrid, startRow, startCol)
    USE PipeModule
    IMPLICIT NONE

    INTEGER :: r, c
    INTEGER, INTENT(OUT) :: startRow, startCol 
    LOGICAL :: found
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(INOUT) :: pipeGrid
    
    ! -1's mean not found.
    startRow = -1
    startCol = -1
    found = .FALSE.
    r = 1

    DO WHILE (.NOT. found .AND. r <= maxRows)
        DO c = 1, maxCols            
            IF (pipeGrid(r, c)%segment .EQ. 'S') THEN
                startRow = r
                startCol = c
                ! Set to -1 to avoid going back on self later on.
                pipeGrid(r,c)%distance = -1
                found = .TRUE.
                EXIT
            END IF
        END DO
        r = r + 1
    END DO
END SUBROUTINE GetStartPipe

! Move through pipe in both 1 direction and update distances for first move.
SUBROUTINE FirstMove(pipeGrid, r, c)
    USE PipeModule
    IMPLICIT NONE

    INTEGER, INTENT(INOUT) :: r, c
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(INOUT) :: pipeGrid
    CHARACTER(LEN=1) :: N, E, S, W

    ! North segment.
    IF (r-1 .GT. 0) THEN        
        N = pipeGrid(r-1,c)%segment      
        IF (N .EQ. '|' .OR. N .EQ. 'F' .OR. N .EQ. '7') THEN            
            pipeGrid(r-1,c)%distance = 1
            r = r-1
            RETURN
        END IF
    END IF

    ! East segment.
    IF (c+1 .LE. maxCols) THEN
        E = pipeGrid(r,c+1)%segment        
        IF (E .EQ. '-' .OR. E .EQ. 'J' .OR. E .EQ. '7') THEN
            pipeGrid(r,c+1)%distance = 1
            c = c+1
            RETURN            
        END IF
    END IF

    ! South segment.
    IF (r+1 .LE. maxRows) THEN
        S = pipeGrid(r+1,c)%segment       
        IF (S .EQ. '|' .OR. S .EQ. 'L' .OR. S .EQ. 'J') THEN            
            pipeGrid(r+1,c)%distance = 1
            r = r+1
            RETURN
        END IF
    END IF

    ! West segment.
    IF (c-1 .GE. 0) THEN
        W = pipeGrid(r,c-1)%segment        
        IF (W .EQ. '|' .OR. W .EQ. 'F' .OR. W .EQ. 'L') THEN            
            pipeGrid(r,c-1)%distance = 1
            c = c-1
            RETURN
        END IF
    END IF
END SUBROUTINE FirstMove

! Move through pipe in one directions and update distances move.
SUBROUTINE MoveSingle(pipeGrid, r, c, isComplete)
    USE PipeModule
    IMPLICIT NONE

    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(INOUT) :: pipeGrid
    CHARACTER(LEN=1) :: segment
    INTEGER, INTENT(INOUT) :: r, c
    LOGICAL, INTENT(OUT) :: isComplete
    
    isComplete = .FALSE.

    segment = pipeGrid(r,c)%segment

    SELECT CASE (segment)
    CASE ('|')
        IF (pipeGrid(r-1,c)%distance .EQ. 0) THEN
            pipeGrid(r-1,c)%distance = 1
            r = r-1
        ELSE IF (pipeGrid(r+1,c)%distance .EQ. 0) THEN
            pipeGrid(r+1,c)%distance = 1
            r = r+1
        ELSE
            isComplete = .TRUE.
        END IF
    CASE ('-')
        IF (pipeGrid(r,c-1)%distance .EQ. 0) THEN
            pipeGrid(r,c-1)%distance = 1
            c = c-1
        ELSE IF (pipeGrid(r,c+1)%distance .EQ. 0) THEN            
            pipeGrid(r,c+1)%distance = 1
            c = c+1
        ELSE
            isComplete = .TRUE.
        END IF
    CASE ('L')
        IF (pipeGrid(r-1,c)%distance .EQ. 0) THEN
            pipeGrid(r-1,c)%distance = 1
            r = r-1
        ELSE IF (pipeGrid(r,c+1)%distance .EQ. 0) THEN
            pipeGrid(r,c+1)%distance = 1
            c = c+1
        ELSE
            isComplete = .TRUE.
        END IF
    CASE ('J')
        IF (pipeGrid(r-1,c)%distance .EQ. 0) THEN
            pipeGrid(r-1,c)%distance = 1
            r = r-1
        ELSE IF (pipeGrid(r,c-1)%distance .EQ. 0) THEN
            pipeGrid(r,c-1)%distance = 1
            c = c-1
        ELSE
            isComplete = .TRUE.
        END IF
    CASE ('F')
        IF (pipeGrid(r,c+1)%distance .EQ. 0) THEN
            pipeGrid(r,c+1)%distance = 1
            c = c+1
        ELSE IF (pipeGrid(r+1,c)%distance .EQ. 0) THEN
            pipeGrid(r+1,c)%distance = 1
            r = r+1
        ELSE
            isComplete = .TRUE.
        END IF
    CASE ('7')        
        IF (pipeGrid(r,c-1)%distance .EQ. 0) THEN
            pipeGrid(r,c-1)%distance = 1
            c = c-1
        ELSE IF (pipeGrid(r+1,c)%distance .EQ. 0) THEN 

            pipeGrid(r+1,c)%distance = 1
            r = r+1
        ELSE
            isComplete = .TRUE.
        END IF
    END SELECT
END SUBROUTINE MoveSingle

! Move all the way round.
SUBROUTINE MoveAll(pipeGrid, farthestDistance)
    USE PipeModule
    IMPLICIT NONE

    INTEGER :: startRow, startCol, r, c, i
    INTEGER, INTENT(OUT) :: farthestDistance
    TYPE(Pipe), DIMENSION(maxRows,maxCols) :: pipeGrid
    LOGICAL :: isComplete = .FALSE.

    ! Find start location.
    CALL GetStartPipe(pipeGrid, startRow, startCol)    
    r = startRow
    c = startCol

    ! Get first location.
    CALL FirstMove(pipeGrid, r, c)

    ! Navigate round.    
    DO WHILE (.NOT. isComplete)
        CALL MoveSingle(pipeGrid,r,c, isComplete)
    END DO
    
    ! Get farthest distance.
    farthestDistance = 0
    DO r = 1,maxRows
        DO c = 1,maxCols
            IF (pipeGrid(r,c)%distance .NE. 0) THEN
                farthestDistance = farthestDistance + 1
            END IF
        END DO
    END DO
    farthestDistance = farthestDistance/2    
END SUBROUTINE MoveAll

! Fllod fill the pipe maze.
RECURSIVE SUBROUTINE FloodFill(pipeGrid, r, c)
    USE PipeModule
    IMPLICIT NONE
  
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(INOUT) :: pipeGrid    
    INTEGER, INTENT(IN) :: r, c

    ! Check within boundaries.
    IF (r < 1 .OR. r > maxRows .OR. c < 1 .OR. c > maxCols) THEN
        RETURN
    END IF

    ! Check if current pipe segment is a gap.
    IF (pipeGrid(r, c)%distance /= 2) THEN
        RETURN
    END IF

    ! Fill the current cell with a space and make distance 2.
    pipeGrid(r, c)%distance = 3

    ! Now do neighbouring pipes.
    CALL FloodFill(pipeGrid, r+1, c) ! Down
    CALL FloodFill(pipeGrid, r-1, c) ! Up
    CALL FloodFill(pipeGrid, r, c+1) ! Right
    CALL FloodFill(pipeGrid, r, c-1) ! Left

END SUBROUTINE FloodFill

! Get no enclosed holes for part 2, ssumed flood fill has been run.
SUBROUTINE CountHoles(pipeGrid, noHoles)
    USE PipeModule
    IMPLICIT NONE

    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(IN) :: pipeGrid
    INTEGER :: r, c
    INTEGER, INTENT(OUT) :: noHoles

    ! Get farthest distance.    
    noHoles = 0
    DO r = 1,maxRows
        DO c = 1,maxCols
            IF (pipeGrid(r,c)%distance .EQ. 3) THEN
                noHoles = noHoles + 1
            END IF
        END DO
    END DO

END SUBROUTINE CountHoles