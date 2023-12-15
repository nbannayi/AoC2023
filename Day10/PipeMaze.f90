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

    CHARACTER(len=50) :: inputFile = 'Day10InputExample.txt'
    TYPE(Pipe), DIMENSION(maxRows,maxCols) :: pipeGrid
    INTEGER :: startRow, startCol, r1, c1, r2, c2
    CHARACTER(LEN=10) :: myString
    CHARACTER(LEN=10), PARAMETER :: referenceString = 'Example'

    CALL ParseData(inputFile, pipeGrid)
    CALL PrintGrid(pipeGrid)
    CALL GetStartPipe(pipeGrid, startRow, startCol)
    CALL FirstMove(pipeGrid, r1, c1, r2, c2)
    PRINT *, startRow, startCol, r1, c2, r2, c2
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
SUBROUTINE PrintGrid(pipeGrid)
    USE PipeModule
    IMPLICIT NONE

    INTEGER :: r, c
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(IN) :: pipeGrid

    DO r = 1, maxRows
        DO c = 1, maxCols
            WRITE(*, '(A)', advance='no') pipeGrid(r,c)%segment
        END DO          
        IF (IACHAR(pipeGrid(r,1)%segment) .EQ. 0) THEN 
            EXIT
        END IF
        PRINT *, ""    
    END DO
END SUBROUTINE PrintGrid

! Get start pipe.
SUBROUTINE GetStartPipe(pipeGrid, startRow, startCol)
    USE PipeModule
    IMPLICIT NONE

    INTEGER :: r, c
    INTEGER, INTENT(OUT) :: startRow, startCol 
    LOGICAL :: found
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(IN) :: pipeGrid
    
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
                found = .TRUE. 
                EXIT
            END IF
        END DO
        r = r + 1
    END DO
END SUBROUTINE GetStartPipe

! Move through pipe in both directions and update distances .
! First coords are position of anit-clockwise segment, second coords are posiiton of clockwise segmnent.
SUBROUTINE FirstMove(pipeGrid, r1, c1, r2, c2)

    INTEGER, INTENT(INOUT) :: r1, c1, r2, c2
    INTEGER :: r1o, c1o, r2o, c2o
    TYPE(Pipe), DIMENSION(maxRows,maxCols), INTENT(INOUT) :: pipeGrid
    CHARACTER(LEN=1) :: N, E, S, W
    LOGICAL :: foundN, foundE, foundS, foundW
    
    r1o = -1
    c1o = -1
    r2o = -1
    c2o = -1

    ! North segment.
    IF (r1-1 > 0) THEN
        N = pipeGrid(r1-1,c1)%segment
        IF N .EQ. '|' .OR. N .EQ. 'F' .OR. N .EQ. '7' THEN
            pipeGrid(r1-1,c1)%distance = 1
            r1o = r1-1
            c1o = c1
        END IF
    END IF

    ! East segment.
    IF (c1+1 <= maxCols) THEN
        N = pipeGrid(r1,c1+1)%segment
        IF N .EQ. '-' .OR. N .EQ. 'J' .OR. N .EQ. '7' THEN
            foundE = .TRUE.
            pipeGrid(r1,c1+1)%distance = 1
            IF c1o >= 0 THEN
                r2o = r1
                c2o = c1+1
            ELSE
                r1o = r1
                c2o = c1+1
            END IF
        END IF
    END IF

    ! South segment.
    IF (r1+1 <= maxRows) THEN
        N = pipeGrid(r1+1,c1)%segment
        IF N .EQ. '|' .OR. N .EQ. 'L' .OR. N .EQ. 'J' THEN
            foundS = .TRUE.
            pipeGrid(r1+1,c1)%distance = 1
            IF c1o >= 0 THEN
                r2o = r1+1
                c2o = c1
            ELSE
                r1o = r1+1
                c2o = c1
            END IF
        END IF
    END IF

    ! West segment.
    IF (c1-1 > 0) THEN
        N = pipeGrid(r1,c1-1)%segment
        IF N .EQ. '|' .OR. N .EQ. 'F' .OR. N .EQ. 'L' THEN
            foundW = .TRUE.
            pipeGrid(r1,c1-1)%distance = 1
            IF c1o >= 0 THEN
                r2o = r1
                c2o = c1-1
            ELSE
                r1o = r1
                c2o = c1-1
            END IF
        END IF
    END IF

    r1 = r1o
    c1 = c1o
    r2 = r2o
    c2 = c2o

SUBROUTINE FirstMove