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
    INTEGER :: startRow, startCol 
    CHARACTER(LEN=10) :: myString
    CHARACTER(LEN=10), PARAMETER :: referenceString = 'Example'

    CALL ParseData(inputFile, pipeGrid)
    CALL PrintGrid(pipeGrid)
    CALL GetStartPipe(pipeGrid, startRow, startCol)
    PRINT *, startRow, startCol



    myString = 'Example'

    IF (TRIM(myString) .EQ. TRIM(referenceString)) THEN
        PRINT *, 'The string is the reference string.'
    END IF

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
                found  = .TRUE. 
                EXIT
            END IF
        END DO
        r = r + 1
    END DO

END SUBROUTINE GetStartPipe